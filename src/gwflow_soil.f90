      subroutine gwflow_soil(hru_id) !rtb gwflow

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the water exchange volume between the aquifer and the soil profile
!!    (exchange volumes are used in gwflow_simulate, in groundwater balance equations)
      
      use gwflow_module
      use soil_module, only : soil
      use hydrograph_module, only : ob
      use hru_module, only : gwsoilq
      
      implicit none

      integer, intent (in) :: hru_id		 !       |hru number
      integer :: k                       !       |counter
      integer :: s                       !       |solute counter
      integer :: jj                      !       |soil layer counter
      integer :: cell_id                 !       |cell in connection with the channel
      real :: hru_Q                      !m3     |volume transferred from cell to the soil profile
      real :: hru_soilz                  !m      |thickness of HRU soil profile
      real :: vadose_z    							 !m			 |thickness of cell vadose zone
      real :: poly_area                  !m2     |area of cell within the HRU
      real :: solmass(100)               !g      |solute mass transferred from cell
      real :: water_depth(100)           !m      |depth of groundwater in each soil layer
      real :: water_depth_tot            !m      |total depth of groundwater in the soil profile
      real :: sol_thick                  !m      |thickness of soil layer
      real :: layer_fraction             !       |fraction of saturated soil profile within the layer
      real :: layer_transfer             !       |amount of water and solute transferred to the soil layer
      real :: hru_area_m2                !m2     |surface area of the hru
      


      !area of the HRU in m2
      hru_area_m2 = ob(hru_id)%area_ha * 10000.    
      
      !only proceed if gw-->soil exchange is active
      if (gw_soil_flag == 1) then
      
        !HRU soil thickness
        hru_soilz = soil(hru_id)%phys(soil(hru_id)%nly)%d / 1000. !m
        
        !loop through the grid cells connected to the HRU -------------------------------------------------------------
        hru_Q = 0.
        do k=1,hru_num_cells(hru_id)
          
          !cell in connection with the HRU
          cell_id = hru_cells(hru_id,k)
        
          !only proceed if the cell is active
          if(gw_state(cell_id)%stat == 1) then 
          
            !calculate the current thickness of the vadose zone (m)
            vadose_z = gw_state(cell_id)%elev - gw_state(cell_id)%head !thickness of vadose zone (m)
            
            !if water table is within the soil profile --> calculate groundwater volume (Q) to transfer; then transfer to soil layer
            hru_Q = 0.
            if(vadose_z < hru_soilz) then !water table is within the soil profile
              poly_area = gw_state(cell_id)%area * cells_fract(hru_id,k) !area of cell within HRU
              hru_Q = (hru_soilz - vadose_z) * poly_area * gw_state(cell_id)%spyd !m3 of groundwater to transfer to the soil profile 

              !store for water balance calculations (in gwflow_simulate)
              gw_ss(cell_id)%soil = gw_ss(cell_id)%soil + (hru_Q*(-1)) !negative = leaving the aquifer
              gw_ss_sum(cell_id)%soil = gw_ss_sum(cell_id)%soil + (hru_Q*(-1))

            !solutes
              if (gw_solute_flag == 1) then
                !solute mass transferred from aquifer to soil
                do s=1,gw_nsolute
                  solmass(s) = hru_Q * gwsol_state(cell_id)%solute(s)%conc !g
                  if(solmass(s) > gwsol_state(cell_id)%solute(s)%mass) then !can only remove what is there
                    solmass(s) = gwsol_state(cell_id)%solute(s)%mass
                  endif
                  gwsol_ss(cell_id)%solute(s)%soil = solmass(s) * (-1) !negative = leaving the aquifer
                  gwsol_ss_sum(cell_id)%solute(s)%soil = gwsol_ss_sum(cell_id)%solute(s)%soil + (solmass(s)*(-1))
                enddo  
              endif !end solutes

              !determine which HRU soil layers are to receive groundwater
              water_depth = 0.
              water_depth_tot = 0.
              do jj=1,soil(hru_id)%nly
                sol_thick = soil(hru_id)%phys(jj)%thick / 1000. !mm --> m
                if((soil(hru_id)%phys(jj)%d/1000.) > vadose_z) then
                  if(jj == 1) then !top layer
                    water_depth(jj) = (soil(hru_id)%phys(jj)%d/1000.) - vadose_z
                  else
                    if(vadose_z > (soil(hru_id)%phys(jj-1)%d/1000.)) then !water table is within this layer
                      water_depth(jj) = (soil(hru_id)%phys(jj)%d/1000.) - vadose_z
                    else !water table is above this layer
                      water_depth(jj) = sol_thick
                    endif
                  endif
                  water_depth_tot = water_depth_tot + water_depth(jj)
                endif  
              enddo !next soil layer

            !transfer the groundwater and the solute mass to the HRU soil layers
              do jj=1,soil(hru_id)%nly
                if(water_depth_tot > 0) then
                  layer_fraction = water_depth(jj) / water_depth_tot
                else
                  layer_fraction = 0.
                endif
                layer_transfer = (hru_Q*layer_fraction) / hru_area_m2 * 1000. !m3 --> mm
                soil(hru_id)%phys(jj)%st = soil(hru_id)%phys(jj)%st + layer_transfer !mm
                gwsoilq(hru_id) = gwsoilq(hru_id) + layer_transfer !store for hru output
              if (gw_solute_flag == 1) then
                  do s=1,gw_nsolute !loop through the solutes
                    layer_transfer = (solmass(s)*layer_fraction) / 1000. / ob(hru_id)%area_ha !g --> kg/ha
                    hru_soil(hru_id,jj,s) = hru_soil(hru_id,jj,s) + layer_transfer !kg/ha (mass added to soil profile in nut_nlch, nut_solp, salt_lch, cs_lch)
                  enddo
                endif
              enddo  

            endif !check for water table in soil profile
                      
          endif !check if cell is active
          
        enddo !go to next connected cell
        
      endif !check if gw-->soil transfer is active
      
      return
      end subroutine gwflow_soil     