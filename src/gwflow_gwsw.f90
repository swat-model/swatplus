      subroutine gwflow_gwsw(chan_id) !rtb gwflow

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the water exchange volume between the channel and the connected grid cells
!!    (exchange volumes are used in gwflow_simulate, in groundwater balance equations)
      
      use gwflow_module
      use hydrograph_module, only : ch_stor
      use sd_channel_module, only : sd_ch
      use constituent_mass_module
      
      implicit none

      integer, intent (in) :: chan_id    !       |channel number
      integer :: k                       !       |counter
      integer :: s                       !       |solute counter
      integer :: cell_id                 !       |cell in connection with the channel
      integer :: isalt                   !       |salt ion counter
      integer :: ics                     !       |constituent counter
      integer :: sol_index               !
      integer :: dum
      real :: chan_depth                 !m      |channel depth
      real :: chan_width                 !m      |channel width
      real :: chan_length                !m      |length of channel, in cell
      real :: bed_elev                   !m      |elevation of channel bed, in cell
      real :: bed_K                      !m/day  |hydraulic conductivity of channel bed, in cell
      real :: bed_thick                  !m      |thickness of channel bed, in cell
      real :: chan_stage                 !m      |elevation of water in channel, in cell
      real :: flow_area                  !m2     |groundwater flow area of water exchange, in cell
      real :: gw_head                    !m      |current groundwater head in cell
      real :: Q                          !m3/day |water exchange flow rate, calculated by Darcy's Law
      real :: head_diff                  !m      |head difference between channel stage and groundwater head
      real :: chan_volume                !m3     |water volume in channel before groundwater exchange occurs
      real :: chan_csol(100)             !g/m3   |solute concentration in channel water
      real :: solmass(100)               !g      |solute mass transferred
      
      
      !current channel storage (m3)
      chan_volume = ch_stor(chan_id)%flo

      !characteristics of channel
      chan_depth = sd_ch(chan_id)%chd !depth (m) of water in channel
      chan_width = sd_ch(chan_id)%chw !width (m) of channel
      
      !loop through the cells connected to the channel
      do k=1,gw_chan_info(chan_id)%ncon
        
        !cell in connection with the channel
        cell_id = gw_chan_info(chan_id)%cells(k)
        
        !only proceed if the cell is active
        if(gw_state(cell_id)%stat == 1) then 
        
          !characteristics of cell
          chan_length = gw_chan_info(chan_id)%leng(k)
          bed_elev = gw_chan_info(chan_id)%elev(k) - gw_bed_change
          bed_K = gw_chan_info(chan_id)%hydc(k)
          bed_thick = gw_chan_info(chan_id)%thck(k)
          
          !derived values
          chan_stage = bed_elev + chan_depth !stage of water in channel (m)
          flow_area = chan_width * chan_length !water exchange flow area (m2)

          !calculate flow exchange rate (m3/day) using Darcy's Law
          !head difference --> head gradient --> flow rate
          gw_head = gw_state(cell_id)%head
          Q = 0.
          if(gw_head < bed_elev) then
            head_diff = chan_depth
            Q =  bed_K * (head_diff / bed_thick) * flow_area !stream leakage (positive Q: entering aquifer)
          elseif (gw_head > chan_stage) then
            head_diff = gw_head - chan_stage
            Q = bed_K * (head_diff / bed_thick) * flow_area * (-1) !gw discharge (negative Q: leaving aquifer)
          elseif (gw_head > bed_elev .and. gw_head < chan_stage) then
            head_diff = chan_stage - gw_head 
            Q = bed_K * (head_diff / bed_thick) * flow_area !stream leakage (positive Q: entering aquifer)
          endif
          
          !store values in gwflow source/sink arrays
          if(Q < 0) then !aquifer --> channel
            !if ((Q*-1 == 1) >= gw_state(cell_id)%stor) then !can only remove what is there
            if (-Q  >= gw_state(cell_id)%stor) then !can only remove what is there
              !Q = gw_state(cell_id)%stor * (-1)
              Q = -gw_state(cell_id)%stor
            endif
            gw_ss(cell_id)%gwsw = gw_ss(cell_id)%gwsw + Q
            gw_state(cell_id)%stor = gw_state(cell_id)%stor + Q !update available groundwater in the cell
          else !channel --> aquifer 
            if(Q > ch_stor(chan_id)%flo) then !can only remove what is there
              Q = ch_stor(chan_id)%flo
            endif
            gw_ss(cell_id)%swgw = gw_ss(cell_id)%swgw + Q
          endif
          gw_ss_sum(cell_id)%gwsw = gw_ss_sum(cell_id)%gwsw + Q !track for annual values
          
          !store for channel object (positive value = water added to channel)
          ch_stor(chan_id)%flo = ch_stor(chan_id)%flo + (Q*(-1))
          
          !calculate solute mass (g/day) transported between cell and channel
          if (gw_solute_flag == 1) then
            chan_csol = 0.
            solmass = 0.
            if(Q < 0) then !mass leaving the cell (aquifer --> channel)
              do s=1,gw_nsolute !loop through the solutes
                solmass(s) = Q * gwsol_state(cell_id)%solute(s)%conc !g
                if(solmass(s) > gwsol_state(cell_id)%solute(s)%mass) then !can only remove what is there
                  solmass(s) = gwsol_state(cell_id)%solute(s)%mass
                endif
                gwsol_ss(cell_id)%solute(s)%gwsw = solmass(s)
                gwsol_ss_sum(cell_id)%solute(s)%gwsw = gwsol_ss_sum(cell_id)%solute(s)%gwsw + solmass(s)
              enddo  
              !add solute mass to channel
              ch_stor(chan_id)%no3 = ch_stor(chan_id)%no3 + (solmass(1)*(-1)/1000.) !kg
              ch_stor(chan_id)%solp = ch_stor(chan_id)%solp + (solmass(2)*(-1)/1000.) !kg
              sol_index = 2
              !salts
              if (gwsol_salt == 1) then
                do isalt=1,cs_db%num_salts
                  sol_index = sol_index + 1
                  ch_water(chan_id)%salt(isalt) = ch_water(chan_id)%salt(isalt) + (solmass(sol_index)*(-1)/1000.) !kg   
                enddo
              endif
              !constituents
              if (gwsol_cons == 1) then
                do ics=1,cs_db%num_cs
                  sol_index = sol_index + 1
                  ch_water(chan_id)%cs(ics) = ch_water(chan_id)%cs(ics) + (solmass(sol_index)*(-1)/1000.) !kg  
                enddo
              endif
            else !mass entering cell (channel --> aquifer)
              !calculate solute concentrations in channel water
              if(chan_volume > 10.) then
                chan_csol(1) = (ch_stor(chan_id)%no3 * 1000.) / chan_volume !no3 g/m3 in channel  
                chan_csol(2) = (ch_stor(chan_id)%solp * 1000.) / chan_volume !p g/m3 in channel  
                sol_index = 2
                !salts
                if (gwsol_salt == 1) then
                  do isalt=1,cs_db%num_salts
                    sol_index = sol_index + 1
                    chan_csol(sol_index) = (ch_water(chan_id)%salt(isalt)*1000.) / chan_volume !g/m3 in channel water        
                  enddo
                endif
                !constituents
                if (gwsol_cons == 1) then
                  do ics=1,cs_db%num_cs
                    sol_index = sol_index + 1
                    chan_csol(sol_index) = (ch_water(chan_id)%cs(ics)*1000.) / chan_volume !g/m3 in channel water
                    if(chan_csol(sol_index) < 0) then
                      dum = 10
                    endif
                  enddo
                endif
              endif
              !calculate mass (g)
              do s=1,gw_nsolute
                solmass(s) = Q * chan_csol(s)
              enddo
              !check against available solute mass in the channel
              !no3
              if((solmass(1)/1000.) > ch_stor(chan_id)%no3) then
                solmass(1) = ch_stor(chan_id)%no3 * 1000. !g
              endif
              ch_stor(chan_id)%no3 = ch_stor(chan_id)%no3 - (solmass(1)/1000.) !kg
              !p
              if((solmass(2)/1000.) > ch_stor(chan_id)%solp) then
                solmass(2) = ch_stor(chan_id)%solp * 1000. !g
              endif
              ch_stor(chan_id)%solp = ch_stor(chan_id)%solp - (solmass(2)/1000.) !kg
              sol_index = 2
              !salts
              if (gwsol_salt == 1) then
                do isalt=1,cs_db%num_salts
                  sol_index = sol_index + 1
                  if((solmass(sol_index)/1000.) > ch_water(chan_id)%salt(isalt)) then
                    solmass(sol_index) = ch_water(chan_id)%salt(isalt) * 1000. !g
                  endif 
                  ch_water(chan_id)%salt(isalt) = ch_water(chan_id)%salt(isalt) - (solmass(sol_index)/1000.) !kg
                enddo
              endif
              !constituents
              if (gwsol_cons == 1) then
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
                gwsol_ss(cell_id)%solute(s)%swgw = solmass(s)
                gwsol_ss_sum(cell_id)%solute(s)%gwsw = gwsol_ss_sum(cell_id)%solute(s)%gwsw + solmass(s)
              enddo
            endif

          endif !end solutes
          
        endif !check if cell is active
      
      enddo !go to next cell 
          
      return
      end subroutine gwflow_gwsw      