      subroutine gwflow_soil(hru_id) !rtb gwflow

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the water exchange volume between the aquifer and the soil profile
!!    (exchange volumes are used in gwflow_simulate, in groundwater balance equations)
      
      use gwflow_module
      use soil_module, only : soil
      use hydrograph_module, only : ob
      use hru_module, only : gwsoilq
      use time_module, only : time
      
      implicit none

      integer, intent (in) :: hru_id     !       |hru number
      integer :: k = 0                   !       |counter
      integer :: s = 0                   !       |solute counter
      integer :: jj = 0                  !       |soil layer counter
      integer :: cell_id = 0             !       |cell in connection with the channel
      real :: hru_Q = 0.                 !m3     |volume transferred from cell to the soil profile
      real :: hru_soilz = 0.             !m      |thickness of HRU soil profile
      real :: vadose_z = 0.                              !m          |thickness of cell vadose zone
      real :: poly_area = 0.             !m2     |area of cell within the HRU
      real :: solmass(100) = 0.          !g      |solute mass transferred from cell
      real :: water_depth(100) = 0.      !m      |depth of groundwater in each soil layer
      real :: water_depth_tot = 0.       !m      |total depth of groundwater in the soil profile
      real :: sol_thick = 0.             !m      |thickness of soil layer
      real :: layer_fraction = 0.        !       |fraction of saturated soil profile within the layer
      real :: layer_transfer = 0.        !       |amount of water and solute transferred to the soil layer
      real :: hru_area_m2 = 0.           !m2     |surface area of the hru
      real :: heat_flux = 0.             !J      |total groundwater heat flux transferred to HRU soils
      real :: soil_volm = 0.             !m3     |volume of soil water in the layer
      real :: soil_heat = 0.             !J      |heat in the soil water of the layer
      


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
              gw_ss_sum(cell_id)%soil = gw_ss_sum(cell_id)%soil + (hru_Q*(-1)) !store for annual water
              gw_ss_sum_mo(cell_id)%soil = gw_ss_sum_mo(cell_id)%soil + (hru_Q*(-1)) !store for monthly water
							
              !heat
              if(gw_heat_flag) then
                heat_flux = gwheat_state(cell_id)%temp * gw_rho * gw_cp * hru_Q !J
                if(heat_flux >= gwheat_state(cell_id)%stor) then
                  heat_flux = gwheat_state(cell_id)%stor
                endif
                gw_heat_ss(cell_id)%soil = gw_heat_ss(cell_id)%soil + (heat_flux*(-1))
                gw_heat_ss_sum(cell_id)%soil = gw_heat_ss_sum(cell_id)%soil + (heat_flux*(-1))
              endif
              
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
                if(gw_heat_flag) then
                  !heat transferred to HRU soil profile
                  layer_transfer = (heat_flux * layer_fraction) !J
                  !update temperature in soil layer
                  soil_volm = (soil(hru_id)%phys(jj)%st/1000.) * hru_area_m2 !m3 of soil water
                  soil_heat = soil(hru_id)%phys(jj)%tmp * gw_rho * gw_cp * soil_volm !J
                  soil_heat = soil_heat + layer_transfer
                  soil(hru_id)%phys(jj)%tmp = soil_heat / (gw_rho * gw_cp * soil_volm)
                endif
              enddo  

            endif !check for water table in soil profile
                      
          endif !check if cell is active
          
        enddo !go to next connected cell
        
      endif !check if gw-->soil transfer is active
      
      return
    end subroutine gwflow_soil     
    