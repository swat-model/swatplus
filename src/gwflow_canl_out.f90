      subroutine gwflow_canl_out !rtb gwflow

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the water exchange volume between irrigation canals and connected grid cells
!!    for canals that originate outside of the model boundary (and hence do not remove water from channels
!!    within the model)
      
      use gwflow_module
      use hydrograph_module, only : ch_stor
      use time_module
      use constituent_mass_module
      
      implicit none

      integer :: i                       !       |cell counter
      integer :: s                       !       |counter of groundwater solutes
      integer :: cell_id                 !       |cell in connection with the canal 
      real :: width                      !m      |canal width
      real :: depth                      !m      |canal depth
      real :: thick                      !m      |canal bed thickness
      real :: length                     !m      |length of canal in the cell
      real :: stage                      !m      |stage of canal in the cell
      real :: bed_K											 !m/day  |hydraulic conductivity of canal bed in the cell
      real :: flow_area                  !m2     |groundwater flow area of water exchange, in cell
      real :: canal_bed                  !m      |canal bed elevation in the cell
      real :: head_diff                  !m      |head difference between canal stage and groundwater head
      real :: Q                          !m3/day |water exchange flow rate, calculated by Darcy's Law
      real :: solmass(100)               !g      |solute mass transferred
      
      
      
      !only proceed if canal-cell exchange is active
      if (gw_canal_flag == 1) then
      
        !loop through the canal cells
        do i=1,gw_canal_ncells_out
       
          !cell id
          cell_id = gw_canl_out_info(i)%cell_id
          
          !only proceed if the cell is active
          if (gw_state(cell_id)%stat == 1) then
            
            !only proceed if canal is "on" (i.e., has water)
            if(time%day.ge.gw_canl_out_info(i)%dayb .and. time%day.le.gw_canl_out_info(i)%daye) then
          
              !attributes of the canal
              width = gw_canl_out_info(i)%wdth
              depth = gw_canl_out_info(i)%dpth
              thick = gw_canl_out_info(i)%thck
              length = gw_canl_out_info(i)%leng
              stage = gw_canl_out_info(i)%elev 
              bed_K = gw_canl_out_info(i)%hydc
              
              !calculate exchange rate Q (m3/day)
              flow_area = length * width !m2 = area of seepage
              canal_bed = stage - depth !m
              Q = 0.
              if(gw_state(cell_id)%head < canal_bed) then
                head_diff = depth
                Q =  bed_K * (head_diff / thick) * flow_area !canal seepage (positive Q: entering aquifer)
              elseif (gw_state(cell_id)%head > stage) then
                head_diff = gw_state(cell_id)%head - stage
                Q = bed_K * (head_diff / thick) * flow_area * (-1) !gw discharge (negative Q: leaving aquifer)
              elseif (gw_state(cell_id)%head > canal_bed .and. gw_state(cell_id)%head < stage) then
                head_diff = stage - gw_state(cell_id)%head
                Q = bed_K * (head_diff / thick) * flow_area !canal seepage (positive Q: entering aquifer)
              endif
              
              !store values in gwflow source/sink arrays
              if(Q < 0) then !groundwater --> canal
                !if((Q*-1 == 1).ge.gw_state(cell_id)%stor) then !can only remove what is there
                if (-Q .ge.gw_state(cell_id)%stor) then !can only remove what is there
                  !Q = gw_state(cell_id)%stor * (-1)
                  Q = -gw_state(cell_id)%stor
                endif
                gw_state(cell_id)%stor = gw_state(cell_id)%stor + Q !update available groundwater in the cell 
              endif  
              gw_ss(cell_id)%canl = gw_ss(cell_id)%canl + Q  
              gw_ss_sum(cell_id)%canl = gw_ss_sum(cell_id)%canl + Q !store for annual water 
              
              !calculate solute mass (g/day) transported between cell and channel
              if (gw_solute_flag == 1) then
                if(Q < 0) then !mass is leaving the cell --> canal
                  do s=1,gw_nsolute !loop through the solutes
                    solmass(s) = Q * gwsol_state(cell_id)%solute(s)%conc !g
                    if(solmass(s) > gwsol_state(cell_id)%solute(s)%mass) then !can only remove what is there
                      solmass(s) = gwsol_state(cell_id)%solute(s)%mass
                    endif
                  enddo
                else !mass entering the cell from the canal (i.e., from the channel that provides the canal water)
                  !calculate mass (g)
                  do s=1,gw_nsolute
                    solmass(s) = Q * canal_out_conc(s)
                  enddo
                endif
                !store in mass balance arrays
                do s=1,gw_nsolute !loop through the solutes
                  gwsol_ss(cell_id)%solute(s)%canl = gwsol_ss(cell_id)%solute(s)%canl + solmass(s)
                  gwsol_ss_sum(cell_id)%solute(s)%canl = gwsol_ss_sum(cell_id)%solute(s)%canl + solmass(s)
                enddo
              endif !end solutes  
                
            endif !check if canal is on
            
          endif !check if cell is active
          
        enddo !go to next canal cells
      
      endif !check if canal-cell exchange is active
       
      return
      end subroutine gwflow_canl_out      