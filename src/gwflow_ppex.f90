      subroutine gwflow_ppex !rtb gwflow

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine determines the volume of groundwater that is extracted
!!    from gwflow grid cells, for groundwater that is lost to the system
!!    (pumping volume are used in gwflow_simulate, in groundwater balance equations)
      
      use gwflow_module     
      
      implicit none

      integer :: i                          !       |counter
      integer :: j                          !       |counter
      integer :: s                          !       |solute counter
      integer :: cell_id                    !       |gwflow cell
      integer :: pumpex_start_date          !       |starting date of pumping period
      integer :: pumpex_end_date            !       |ending date of pumping period
      real :: Q                             !m3     |pumping rate
      real :: solmass(100)                  !g      |solute mass in pumped groundwater
      
      
      !only proceed if external pumping has been specified
      if (gw_pumpex_flag == 1) then
      
        !loop through pumps and specified pumping periods; apply rate to the associated grid cells
        do i=1,gw_npumpex
          
          !cell where pumping occurs
          cell_id = gw_pumpex_cell(i)
          
          !only proceed if pumping cell is active
          if(gw_state(cell_id)%stat == 1) then
          
            !loop through the pumping periods of the cell
            do j=1,gw_pumpex_nperiods(i)
            
              !determine if the current day of the simulation is within the pumping period; if so, apply the pumping rate to the cell
              pumpex_start_date = gw_pumpex_dates(i,1,j)
              pumpex_end_date = gw_pumpex_dates(i,2,j)
              if(gw_daycount.ge.pumpex_start_date .and. gw_daycount.le.pumpex_end_date) then
                !check to make sure there is enough groundwater to satisfy the pumping rate
                Q = gw_pumpex_rates(i,j)
                if(Q.ge.gw_state(cell_id)%stor) then
                  Q = gw_state(cell_id)%stor
                  gw_state(cell_id)%stor = gw_state(cell_id)%stor - Q
							  endif
                gw_ss(cell_id)%ppex = gw_ss(cell_id)%ppex - Q !negative = leaving the aquifer
                gw_ss_sum(cell_id)%ppex = gw_ss_sum(cell_id)%ppex - Q 
                !if chemical transport simulated, calculate the mass of N and P removed via pumping
                if (gw_solute_flag == 1) then
                  do s=1,gw_nsolute !loop through the solutes
                    solmass(s) = Q * gwsol_state(cell_id)%solute(s)%conc !g
                    gwsol_ss(cell_id)%solute(s)%ppex = gwsol_ss(cell_id)%solute(s)%ppex - solmass(s)
                    gwsol_ss_sum(cell_id)%solute(s)%ppex = gwsol_ss_sum(cell_id)%solute(s)%ppex - solmass(s)
                  enddo
                endif
              endif
              
            enddo !go to next pumping period
          
          endif !check if cell is active
          
        enddo !go to next pump
        
      endif
       
      end subroutine gwflow_ppex     