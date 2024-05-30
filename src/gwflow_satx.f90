      subroutine gwflow_satx(chan_id) !rtb gwflow

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the groundwater volume that enters the channel via saturation excess flow
!!    (exchange volumes are used in gwflow_simulate, in groundwater balance equations)
      
      use gwflow_module
      use hydrograph_module, only : ch_stor
      use constituent_mass_module
      
      implicit none

      integer, intent (in) :: chan_id		 !       |channel id
      integer :: k                       !       |counter
      integer :: s                       !       |solute counter
      integer :: cell_id                 !       |cell in connection with the channel
      integer :: isalt                   !       |salt ion counter
      integer :: ics                     !       |constituent counter
      integer :: sol_index
      real :: satx_depth                 !m			 |height of water table above ground surface
      real :: satx_volume								 !m3     |volume of groundwater above ground surface
      real :: solmass(100)               !g      |solute mass transferred
      

      
      !only proceed if groundwater saturation excess flow is simulation
      if (gw_satx_flag == 1) then
      
        !loop through the cells connected to the channel
        do k=1,gw_satx_info(chan_id)%ncon
          
          !cell in connection with the channel
          cell_id = gw_satx_info(chan_id)%cells(k)
          
          !only proceed if cell is active
          if(gw_state(cell_id)%stat == 1) then
          
            !check if groundwater head is above land surface; if so, gw-->runoff
            if(gw_state(cell_id)%head > gw_state(cell_id)%elev) then  
              
              !calculate saturation excess
              satx_count = satx_count + 1 !track the number of saturated cells (for output)
              satx_depth = gw_state(cell_id)%head - gw_state(cell_id)%elev !height above ground surface
              satx_volume = (gw_state(cell_id)%area * satx_depth) * gw_state(cell_id)%spyd !m3 of groundwater above ground surface 
              
              !store for water balance calculations (in gwflow_simulate)
              gw_ss(cell_id)%satx = satx_volume * (-1) !negative = leaving the aquifer
              gw_ss_sum(cell_id)%satx = gw_ss_sum(cell_id)%satx + (satx_volume * (-1))
              
              !store for channel object (positive value = water added to channel)
              ch_stor(chan_id)%flo = ch_stor(chan_id)%flo + satx_volume
              
              !solutes
              if (gw_solute_flag == 1) then
                !solute mass leaving with saturation excess flow
                do s=1,gw_nsolute
                  solmass(s) = satx_volume * gwsol_state(cell_id)%solute(s)%conc !g
                  if(solmass(s) > gwsol_state(cell_id)%solute(s)%mass) then !can only remove what is there
                    solmass(s) = gwsol_state(cell_id)%solute(s)%mass
                  endif
                  gwsol_ss(cell_id)%solute(s)%satx = solmass(s) * (-1) !negative = leaving the aquifer
                  gwsol_ss_sum(cell_id)%solute(s)%satx = gwsol_ss_sum(cell_id)%solute(s)%satx + (solmass(s)*(-1))
                enddo
                !add solute mass to channel
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
              
            endif !check if saturation occurs
            
          endif !check if cell is active
          
        enddo !go to next connected cell
        
      endif !check if saturation excess flow is being simulated
                            
      return
      end subroutine gwflow_satx      