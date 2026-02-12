      subroutine gwflow_tile(chan_id) !rtb gwflow

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the water exchange volume between irrigation canals and connected grid cells
!!    (exchange volumes are used in gwflow_simulate, in groundwater balance equations)
      
      use gwflow_module
      use hydrograph_module, only : ch_stor,ch_out_d
      use constituent_mass_module
      
      implicit none

      integer, intent (in) :: chan_id    !       |channel number
      integer :: k = 0                   !       |counter for cells connected to the channel
      integer :: cell_id = 0             !       |id of cell connected to the channel
      integer :: s = 0                   !       |counter of groundwater solutes
      integer :: isalt = 0               !       |salt ion counter
      integer :: ics = 0                 !       |constituent counter
      integer :: sol_index = 0
      real :: chan_volume = 0.           !m3     |water volume in channel before groundwater exchange occurs
      real :: tile_elev = 0.             !m      |elevation of tile drain
      real :: head_diff = 0.             !m      |head difference between groundwater head and tile drain
      real :: Q = 0.                     !m3/day |tile drainage outflow rate, calculated by Darcy's Law
      real :: solmass(100) = 0.          !g      |solute mass transferred from groundwater to channel, via tile
      real :: heat_flux = 0.             !J      |heat in tile drainage
      real :: chan_heat = 0.             !J      |heat in channel water
      
      
      !only proceed if tile drainage is active
      if (gw_tile_flag == 1) then
      
        !record starting channel volume (m3)
        chan_volume = ch_stor(chan_id)%flo
        
        !loop through the cells connected to the channel
        do k=1,gw_tile_info(chan_id)%ncon
        
          !cell in connection with the channel
          cell_id = gw_tile_info(chan_id)%cells(k)
          
          !only proceed if the cell is active
          if(gw_state(cell_id)%stat == 1) then 
          
            !get elevation of the subsurface drain (m)
            tile_elev = gw_state(cell_id)%elev - gw_tile_depth(cell_id)
          
            !only perform calculation if water table is above tile drain            
            if(gw_state(cell_id)%head > tile_elev) then
              
              !calculate tile drainage outflow rate using Darcy's Law
              head_diff = gw_state(cell_id)%head - tile_elev
              Q = gw_tile_drain_area(cell_id) * gw_tile_K(cell_id) * head_diff !m3/day
              
              !check for available groundwater in the cell - can only remove what is there
              if(Q > gw_state(cell_id)%stor) then
                Q = gw_state(cell_id)%stor
              endif
              gw_state(cell_id)%stor = gw_state(cell_id)%stor - Q !update available groundwater in the cell 
              gw_ss(cell_id)%tile = Q * (-1) !leaving aquifer
              gw_ss_sum(cell_id)%tile = gw_ss_sum(cell_id)%tile + (Q*(-1)) !leaving aquifer - store for annual water
              gw_ss_sum_mo(cell_id)%tile = gw_ss_sum_mo(cell_id)%tile + (Q*(-1)) !leaving aquifer - store for monthly water
              
              !add water to channel
              ch_stor(chan_id)%flo = ch_stor(chan_id)%flo + Q
              
              !heat transfer from groundwater to channel
              if(gw_heat_flag) then
                heat_flux = gwheat_state(cell_id)%temp * gw_rho * gw_cp * Q !J
                if(heat_flux >= gwheat_state(cell_id)%stor) then
                  heat_flux = gwheat_state(cell_id)%stor
                endif
                gw_heat_ss(cell_id)%tile = heat_flux * (-1) !leaving aquifer
                gw_heat_ss_sum(cell_id)%tile = gw_heat_ss_sum(cell_id)%tile + (heat_flux*(-1)) !leaving aquifer
                !update temperature in channel
                chan_heat = ch_stor(chan_id)%temp * gw_rho * gw_cp * chan_volume !J in channel
                chan_heat = chan_heat + heat_flux
                if(ch_stor(chan_id)%flo > 0) then
                  ch_stor(chan_id)%temp = chan_heat / (gw_rho * gw_cp * ch_stor(chan_id)%flo)
                else
                  ch_stor(chan_id)%temp = 0.
								endif
								ch_out_d(chan_id)%temp = ch_stor(chan_id)%temp
              endif
              
              !transfer solute mass from groundwater to channel
              if (gw_solute_flag == 1) then
                do s=1,gw_nsolute !loop through the solutes
                  solmass(s) = Q * gwsol_state(cell_id)%solute(s)%conc !g
                  if(solmass(s) > gwsol_state(cell_id)%solute(s)%mass) then !can only remove what is there
                    solmass(s) = gwsol_state(cell_id)%solute(s)%mass
                  endif
                  gwsol_ss(cell_id)%solute(s)%tile = solmass(s) * (-1) !leaving aquifer
                  gwsol_ss_sum(cell_id)%solute(s)%tile = gwsol_ss_sum(cell_id)%solute(s)%tile + (solmass(s)*(-1)) !leaving aquifer
									gwsol_ss_sum_mo(cell_id)%solute(s)%tile = gwsol_ss_sum_mo(cell_id)%solute(s)%tile + (solmass(s)*(-1)) !leaving aquifer
                enddo  
                !add solute to channel
                ch_stor(chan_id)%no3 = ch_stor(chan_id)%no3 + (solmass(1)/1000.) !kg
                ch_stor(chan_id)%solp = ch_stor(chan_id)%solp + (solmass(2)/1000.) !kg
                sol_index = 2
                !salts
                if (gwsol_salt == 1) then
                  do isalt=1,cs_db%num_salts
                    sol_index = sol_index + 1
                    ch_water(chan_id)%salt(isalt) = ch_water(chan_id)%salt(isalt) + (solmass(sol_index)/1000.) !kg   
                  enddo
                endif
                !constituents
                if (gwsol_cons == 1) then
                  do ics=1,cs_db%num_cs
                    sol_index = sol_index + 1
                    ch_water(chan_id)%cs(ics) = ch_water(chan_id)%cs(ics) + (solmass(sol_index)/1000.) !kg  
                  enddo
                endif
              endif !end solutes
              
            endif !check if groundwater head above tile drain
            
          endif !check if cell is active
      
        enddo !go to next tile cell
      
      endif !check if tile drainage is active
      
      return
    end subroutine gwflow_tile      
    