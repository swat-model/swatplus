      subroutine gwflow_tile(chan_id) !rtb gwflow

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the water exchange volume between irrigation canals and connected grid cells
!!    (exchange volumes are used in gwflow_simulate, in groundwater balance equations)
      
      use gwflow_module
      use hydrograph_module, only : ch_stor
      use constituent_mass_module
      
      implicit none

      integer, intent (in) :: chan_id		 !       |channel number
      integer :: k                       !       |counter for cells connected to the channel
      integer :: cell_id                 !			 |id of cell connected to the channel
      integer :: s                       !       |counter of groundwater solutes
      integer :: isalt                   !       |salt ion counter
      integer :: ics                     !       |constituent counter
      integer :: sol_index
      real :: chan_volume                !m3     |water volume in channel before groundwater exchange occurs
      real :: tile_elev                  !m      |elevation of tile drain
      real :: head_diff                  !m      |head difference between groundwater head and tile drain
      real :: Q                          !m3/day |tile drainage outflow rate, calculated by Darcy's Law
      real :: solmass(100)               !g      |solute mass transferred from groundwater to channel, via tile
      
      
      
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
            tile_elev = gw_state(cell_id)%elev - gw_tile_depth 
          
            !only perform calculation if water table is above tile drain            
            if(gw_state(cell_id)%head > tile_elev) then
              
              !calculate tile drainage outflow rate using Darcy's Law
              head_diff = gw_state(cell_id)%head - tile_elev
              Q = gw_tile_drain_area * gw_tile_K * head_diff !m3/day
              
              !check for available groundwater in the cell - can only remove what is there
              if(Q > gw_state(cell_id)%stor) then
                Q = gw_state(cell_id)%stor
              endif
              gw_state(cell_id)%stor = gw_state(cell_id)%stor - Q !update available groundwater in the cell 
              gw_ss(cell_id)%tile = Q * (-1) !leaving aquifer
              gw_ss_sum(cell_id)%tile = gw_ss_sum(cell_id)%tile + (Q*(-1)) !leaving aquifer
              
              !add water to channel
              ch_stor(chan_id)%flo = ch_stor(chan_id)%flo + Q
              
              !transfer solute mass from groundwater to channel
              if (gw_solute_flag == 1) then
                do s=1,gw_nsolute !loop through the solutes
                  solmass(s) = Q * gwsol_state(cell_id)%solute(s)%conc !g
                  if(solmass(s) > gwsol_state(cell_id)%solute(s)%mass) then !can only remove what is there
                    solmass(s) = gwsol_state(cell_id)%solute(s)%mass
                  endif
                  gwsol_ss(cell_id)%solute(s)%tile = solmass(s) * (-1) !leaving aquifer
                  gwsol_ss_sum(cell_id)%solute(s)%tile = gwsol_ss_sum(cell_id)%solute(s)%tile + (solmass(s)*(-1)) !leaving aquifer
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