      subroutine gwflow_phyt() !rtb gwflow

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the water removed from the aquifer via phreatophyte extraction
!!    (extraction volumes are used in gwflow_simulate, in groundwater balance equations)
      
      use gwflow_module
      
      implicit none

      integer :: k = 0                       !       |cell counter (cells with phreatophytes)
			integer :: i = 0                       !       |point counter (segments on depth-ET relationship)
			integer :: cell_id = 0                 !       |cell id
      real :: wt_depth = 0.                   !m      |depth to water table, from the ground surface
      real :: ratio = 0.											 !       |ratio of water table depth to phreatophyte segment depth
			real :: et_rate = 0.                    !m/day  |rate of phreatophyte transpiration from the grid cell
			real :: et_Q = 0.                       !m3/day |volumetric flow rate of phyreatophyte transpiration from the grid cell
			
			
      
      !only proceed if phreatophyte option is activated
      if(gw_phyt_flag == 1) then
      
        !loop through the cells with phreatophytes
			  do k=1,gw_phyt_ncells
				
				  cell_id = gw_phyt_ids(k)
				  
					!determine the rate (m/day) of transpiration
					et_Q = 0.
					wt_depth = gw_state(cell_id)%elev - gw_state(cell_id)%head
					do i=1,gw_phyt_npts-1
					  if(wt_depth >= gw_phyt_dep(i) .and. wt_depth <= gw_phyt_dep(i+1)) then
						  ratio = (wt_depth - gw_phyt_dep(i)) / (gw_phyt_dep(i+1)-gw_phyt_dep(i))  
							et_rate = gw_phyt_rate(i) - (ratio*(gw_phyt_rate(i)-gw_phyt_rate(i+1)))
						  et_Q = et_rate * gw_phyt_area(k)
						endif
					enddo
					
				  !check against available groundawter storage (m3)
					if(et_Q > gw_state(cell_id)%stor) then
					  et_Q = gw_state(cell_id)%stor
					endif
					
					!change sign (negative = removal of groundwater from aquifer)
					et_Q = et_Q * (-1)
					
          !store for gwflow water balance calculations (in gwflow_simulate)
					gw_ss(cell_id)%phyt = gw_ss(cell_id)%phyt + et_Q 
          gw_ss_sum(cell_id)%phyt = gw_ss_sum(cell_id)%phyt + et_Q !store for annual water
          gw_ss_sum_mo(cell_id)%phyt = gw_ss_sum_mo(cell_id)%phyt + et_Q !store for monthly water
					
				enddo !go to the next cell
			  
      endif !check if phreatophyte transpiration is active
      
      
      return
      end subroutine gwflow_phyt
      
           
      