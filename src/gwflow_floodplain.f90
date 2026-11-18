      subroutine gwflow_floodplain(chan_id) !rtb gwflow

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the water exchange volume between the floodplain and the connected grid cells
!!    (exchange volumes are used in gwflow_simulate, in groundwater balance equations)

      use gwflow_module
      use hydrograph_module, only : ht1,ob,icmd
      use sd_channel_module, only : sd_ch
      use constituent_mass_module

      implicit none

      integer, intent (in) :: chan_id    !       |channel number
      integer :: k = 0                   !       |counter
      integer :: s = 0                   !       |solute counter
      integer :: cell_id = 0             !       |cell in connection with the channel
      integer :: chan_cell = 0           !       |
      integer :: isalt = 0               !       |salt ion counter
      integer :: ics = 0                 !       |constituent counter
      integer :: sol_index = 0
      real :: chan_depth = 0.            !m      |channel depth
      real :: chan_width = 0.            !m      |channel width
      real :: chan_length = 0.           !m      |length of channel, in cell
      real :: bed_elev = 0.              !m      |elevation of channel bed, in cell
      real :: bed_K = 0.                 !m/day  |hydraulic conductivity of channel bed, in cell
      real :: bed_thick = 0.             !m      |thickness of channel bed, in cell
      real :: chan_stage = 0.            !m      |elevation of water in channel, in cell
      real :: flow_area = 0.             !m2     |groundwater flow area of water exchange, in cell
      real :: riv_flow_area = 0.         !m2       |groundwater flow area of water exchange for channel
      real :: gw_head = 0.               !m      |current groundwater head in cell
      real :: Q = 0.                     !m3/day |water exchange flow rate in floodplain, calculated by Darcy's Law
      real :: chan_volume = 0.           !m3     |water volume in channel before groundwater exchange occurs
      real :: chan_csol(100) = 0.        !g/m3   |solute concentration in channel water
      real :: solmass(100) = 0.          !g      |solute mass transferred
      real :: heat_flux = 0.             !J      |heat in water transfer
      real :: chan_heat = 0.             !J      |heat in channel water


      !record starting channel volume (m3)
      chan_volume = ht1%flo

      !only proceed if floodplain option is activated and connection info is loaded
      if(gw_fp_flag == 1 .and. allocated(gw_fpln_info)) then

        !loop through the cells connected to the current channel
        do k=1,gw_fpln_info(chan_id)%ncon

          !cell in connection with the channel
          cell_id = gw_fpln_info(chan_id)%cells(k)

          !only proceed if the cell is active
          if(gw_state(cell_id)%stat.eq.1) then

            !characteristics of channel
            chan_depth = sd_ch(chan_id)%chd !depth (m) of water in channel

            !characteristics of cell
            bed_elev = gw_state(cell_id)%elev
            bed_K = gw_fpln_info(chan_id)%hydc(k)
            flow_area = gw_fpln_info(chan_id)%area(k)

            !derived values
            chan_stage = bed_elev + chan_depth !stage of water in channel (m)

						!adjust flow area, if necessary (if floodplain cell is also a river cell; exchange already calculated for main channel)
            if(gw_fpln_info(chan_id)%mtch(k) > 0) then
              chan_cell = gw_fpln_info(chan_id)%mtch(k)
              riv_flow_area = sd_ch(chan_id)%chw * gw_chan_len(chan_cell) !width * length = river cell exchange flow area
              flow_area = flow_area - riv_flow_area
              if(flow_area.lt.0) then
                flow_area = 0.
              endif
            endif

            !calculate seepage/discharge (m3/day) (assume unit hydraulic gradient)
            Q = 0.
            if(gw_state(cell_id)%head > chan_stage) then
              Q = bed_K * flow_area * (-1) !gw discharge (negative Q: leaving aquifer)
            else
              Q = bed_K * flow_area !seepage (positive Q: entering aquifer)
            endif

            !compare potential Q to available water in cell or channel
            if(Q < 0) then !leaving aquifer
              if((Q*-1) >= gw_state(cell_id)%stor) then !can only remove what is there
                Q = gw_state(cell_id)%stor * (-1)
              endif
            else !entering aquifer
              if(Q > ht1%flo) then !can only remove what is there
                Q = ht1%flo
              endif
            endif

            !store for channel object (positive value = water added to channel)
            ht1%flo = ht1%flo + (Q*(-1))
						ob(icmd)%tsin(1) = ht1%flo

            !add to gwflow source/sink arrays
            gw_hyd_ss(cell_id)%fpln = gw_hyd_ss(cell_id)%fpln + Q
            gw_hyd_ss_yr(cell_id)%fpln = gw_hyd_ss_yr(cell_id)%fpln + Q !store for annual water
            gw_hyd_ss_mo(cell_id)%fpln = gw_hyd_ss_mo(cell_id)%fpln + Q !store for monthly water

            !heat
            if(gw_heat_flag == 1) then
              !current heat in channel
              chan_heat = ht1%temp * gw_rho * gw_cp * ht1%flo !J in channel
              if(Q < 0) then !leaving the cell (aquifer --> channel)
                heat_flux = gwheat_state(cell_id)%temp * gw_rho * gw_cp * Q !J
                if(-heat_flux >= gwheat_state(cell_id)%stor) then
                  heat_flux = -gwheat_state(cell_id)%stor
                endif
              else !entering cell (channel --> aquifer)
                heat_flux = 0.
                if(ht1%temp > 0) then
                  heat_flux = ht1%temp * gw_rho * gw_cp * Q !J
                  if(heat_flux > chan_heat) then
                    heat_flux = chan_heat
                  endif
                endif
              endif
              !update channel heat and temperature
              chan_heat = chan_heat + (heat_flux*(-1))
              if(ht1%flo > 0) then
                ht1%temp = chan_heat / (gw_rho * gw_cp * ht1%flo)
              else
                ht1%temp = 0.
              endif
              !add to heat balance arrays
              gw_heat_ss(cell_id)%fpln = gw_heat_ss(cell_id)%fpln + heat_flux
							gw_heat_ss_mo(cell_id)%fpln = gw_heat_ss_mo(cell_id)%fpln + heat_flux !J
              gw_heat_ss_yr(cell_id)%fpln = gw_heat_ss_yr(cell_id)%fpln + heat_flux !J
            endif

            !calculate solute mass (g/day) transported to/from cell
            if(gw_solute_flag == 1) then
              chan_csol = 0.
              solmass = 0.
              if(Q < 0) then !mass leaving the cell (aquifer --> channel)
                do s=1,gw_nsolute !loop through the solutes
                  solmass(s) = Q * gwsol_state(cell_id)%solute(s)%conc !g
                  if(solmass(s) > gwsol_state(cell_id)%solute(s)%mass) then !can only remove what is there
                    solmass(s) = gwsol_state(cell_id)%solute(s)%mass
                  endif
                  gw_sol_ss(cell_id)%solute(s)%fpln = solmass(s)
									gw_sol_ss_mo(cell_id)%solute(s)%fpln = gw_sol_ss_mo(cell_id)%solute(s)%fpln + solmass(s)
									gw_sol_ss_yr(cell_id)%solute(s)%fpln = gw_sol_ss_yr(cell_id)%solute(s)%fpln + solmass(s)
                enddo
                !add solute to channel
                ht1%no3 = ht1%no3 + (solmass(1)*(-1)/1000.) !kg
                ht1%solp = ht1%solp + (solmass(2)*(-1)/1000.) !kg
                sol_index = 2
                !salts
                if(gwsol_salt == 1) then
                  do isalt=1,cs_db%num_salts
                    sol_index = sol_index + 1
                    ch_water(chan_id)%salt(isalt) = ch_water(chan_id)%salt(isalt) + (solmass(sol_index)*(-1)/1000.) !kg
                  enddo
                endif
                !constituents
                if(gwsol_cons == 1) then
                  do ics=1,cs_db%num_cs
                    sol_index = sol_index + 1
                    ch_water(chan_id)%cs(ics) = ch_water(chan_id)%cs(ics) + (solmass(sol_index)*(-1)/1000.) !kg
                  enddo
                endif
              else !mass entering cell (channel --> aquifer)
                !calculate solute concentrations in channel water
                if(chan_volume > 10.) then
                  chan_csol(1) = (ht1%no3 * 1000.) / chan_volume !g/m3 in channel
                  chan_csol(2) = (ht1%solp * 1000.) / chan_volume !g/m3 in channel
                  sol_index = 2
                  !salts
                  if(gwsol_salt == 1) then
                    do isalt=1,cs_db%num_salts
                      sol_index = sol_index + 1
                      chan_csol(sol_index) = (ch_water(chan_id)%salt(isalt) * 1000.) / chan_volume !g/m3 in channel
                    enddo
                  endif
                  !constituents
                  if(gwsol_cons == 1) then
                    do ics=1,cs_db%num_cs
                      sol_index = sol_index + 1
                      chan_csol(sol_index) = (ch_water(chan_id)%cs(ics) * 1000.) / chan_volume !g/m3 in channel
                    enddo
                  endif
                endif
                !calculate mass (g)
                do s=1,gw_nsolute
                  solmass(s) = Q * chan_csol(s)
                enddo
                !check against available solute mass in the channel
                !no3
                if((solmass(1)/1000.) > ht1%no3) then
                  solmass(1) = ht1%no3 * 1000. !g
                endif
                ht1%no3 = ht1%no3 - (solmass(1)/1000.) !kg
                !p
                if((solmass(2)/1000.) > ht1%solp) then
                  solmass(2) = ht1%solp * 1000. !g
                endif
                ht1%solp = ht1%solp - (solmass(2)/1000.) !kg
                sol_index = 2
                !salts
                if(gwsol_salt == 1) then
                  do isalt=1,cs_db%num_salts
                    sol_index = sol_index + 1
                    if((solmass(sol_index)/1000.) > ch_water(chan_id)%salt(isalt)) then
                      solmass(sol_index) = ch_water(chan_id)%salt(isalt) * 1000. !g
                    endif
                    ch_water(chan_id)%salt(isalt) = ch_water(chan_id)%salt(isalt) - (solmass(sol_index)/1000.) !kg
                  enddo
                endif
                !constituents
                if(gwsol_cons == 1) then
                  do ics=1,cs_db%num_cs
                    sol_index = sol_index + 1
                    if((solmass(sol_index)/1000.) > ch_water(chan_id)%cs(ics)) then
                      solmass(sol_index) = ch_water(chan_id)%cs(ics) * 1000. !g
                    endif
                    ch_water(chan_id)%cs(ics) = ch_water(chan_id)%cs(ics) - (solmass(sol_index)/1000.) !kg
                  enddo
                endif
                !store for mass balance calculations (in gwflow_simulate)
                do s=1,gw_nsolute !loop through the solutes
                  gw_sol_ss(cell_id)%solute(s)%fpln = solmass(s)
									gw_sol_ss_mo(cell_id)%solute(s)%fpln = gw_sol_ss_mo(cell_id)%solute(s)%fpln + solmass(s)
									gw_sol_ss_yr(cell_id)%solute(s)%fpln = gw_sol_ss_yr(cell_id)%solute(s)%fpln + solmass(s)
                enddo
              endif

            endif !end solutes

          endif !check if cell is active

        enddo !go to next cell

      endif


      return
      end subroutine gwflow_floodplain


