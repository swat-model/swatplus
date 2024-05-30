      subroutine gwflow_ppag(hru_id,hru_demand,extracted,dmd_unmet) !rtb gwflow

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine determines the volume of groundwater that is extracted
!!    from gwflow grid cells
!!    (pumping volume are used in gwflow_simulate, in groundwater balance equations)
      
      use gwflow_module
      use organic_mineral_mass_module, only : soil1
      use hru_module, only : hru,irrn,irrp
      use hydrograph_module, only : wet
      use constituent_mass_module
      use res_salt_module, only : wetsalt_d
      use res_cs_module, only : wetcs_d
      use salt_module, only : hsaltb_d
      use cs_module, only : hcsb_d
      
      
      implicit none

      integer, intent (in) :: hru_id		    !       |hru that has an irrigation water demand
      real, intent (in) :: hru_demand       !m3     |volume of irrigation water demand
      real, intent (inout) :: extracted     !m3     |volume of groundwater extracted from aquifer for irrigation
      real, intent (inout) :: dmd_unmet     !m3     |volume of irrigation demand not met by aquifer
      integer :: i                          !       |counter
      integer :: s                          !       |solute counter
      integer :: cell_id                    !       |gwflow cell
      integer :: wetland                    !       |wetland flag
      integer :: isalt                      !       |salt ion counter
      integer :: ics                        !       |constituent counter
      integer :: sol_index
      real :: cell_demand                   !m3     |volume of irrigation water demand per gwflow cell
      real :: gwvol_avail                   !m3     |groundwater storage in the gwflow cell
      real :: gwvol_removed									!m3     |groundwater removed from the gwflow cell for irrigation demand
      real :: gwvol_unmet										!m3     |groundwater not available to meet irrigation demand
      real :: gw_mass                       !kg     |mass of solute in groundwater of the gwflow cell
      real :: irr_mass(100)                 !kg     |mass of solute removed from aquifer for irrigation
      real :: mass_diff                     !kg     |difference between irrigation mass and actual groundwater mass
      real :: sum_pump                      !m3     |total pumping for the HRU
      
      !zero out the total met and unmet demand
      extracted = 0.
      dmd_unmet = 0.

      !only proceed if the HRU is connected (intersected with) gwflow grid cells
      sum_pump = 0.
      if(hru_num_cells(hru_id).gt.0) then
          
        !groundwater volume to remove from each cell connected to the HRU
        cell_demand = hru_demand / hru_num_cells(hru_id) !groundwater to remove from each cell connected to the HRU
        
        !loop through the cells that are connected to the HRU
        do i=1,hru_num_cells(hru_id)
          
          !id of the current cell
          cell_id = hru_cells(hru_id,i)
          
          !check for available groundwater in the cell
          if(gw_state(cell_id)%head > gw_state(cell_id)%botm) then !if water table is above bedrock
            gwvol_avail = ((gw_state(cell_id)%head-gw_state(cell_id)%botm) * gw_state(cell_id)%area) * gw_state(cell_id)%spyd !m3
				  else
            gwvol_avail = 0.
          endif
          
          !remove irrigation demand from storage; if storage is less than demand, remove all groundwater
          gwvol_removed = 0.
          gwvol_unmet = 0.
          if(gwvol_avail < cell_demand) then
            gwvol_removed = gwvol_avail
            gwvol_unmet = cell_demand - gwvol_avail !amount that is not available for irrigation
          else
            gwvol_removed = cell_demand
          endif
          extracted = extracted + gwvol_removed
          dmd_unmet = dmd_unmet + gwvol_unmet
          
          !save the pumping volume (m3), for use in gwflow_simulate
          gw_ss(cell_id)%ppag = gwvol_removed * (-1) !m3 negative = leaving the aquifer
          gw_ss_sum(cell_id)%ppag = gw_ss_sum(cell_id)%ppag + (gwvol_removed * (-1))
          
          !sum the pumping for the current HRU
          sum_pump = sum_pump + gwvol_removed
          
          !save the unsatisfied pumping volume (m3), for output
          gw_ss(cell_id)%ppdf = gwvol_unmet
          gw_ss_sum(cell_id)%ppdf = gw_ss_sum(cell_id)%ppdf + gwvol_unmet
          
          !add solute mass in irrigation water to HRU soil profile
          !(mass is removed from the aquifer via mass balance equation in gwflow_simulate.f)     
          if (gw_solute_flag == 1) then
            !determine the mass (kg) removed for irrigation water 
            do s=1,gw_nsolute !loop through the solutes
              gw_mass = gwsol_state(cell_id)%solute(s)%mass / 1000. !kg in cell
              irr_mass(s) = (gwsol_state(cell_id)%solute(s)%conc * gwvol_removed) / 1000. !kg removed in irrigation water
              mass_diff = 0.
              if(irr_mass(s).gt.gw_mass) then
                mass_diff = irr_mass(s) - gw_mass
              endif  
              irr_mass(s) = irr_mass(s) - mass_diff
              if(irr_mass(s).lt.0) irr_mass(s) = 0.   
            enddo
            !add solute mass to soil profile of demand object (hru)
            wetland = hru(hru_id)%dbs%surf_stor !check if HRU is a wetland
            if(wetland > 0) then !add to wetland
              !nutrients
              wet(hru_id)%no3 = wet(hru_id)%no3 + irr_mass(1) !kg
              wet(hru_id)%solp = wet(hru_id)%solp + irr_mass(2) !kg
              sol_index = 2
              !salts
              if (gwsol_salt == 1) then
                do isalt=1,cs_db%num_salts
                  sol_index = sol_index + 1
                  wet_water(hru_id)%salt(isalt) = wet_water(hru_id)%salt(isalt) + irr_mass(sol_index) !kg
                  wetsalt_d(hru_id)%salt(isalt)%irrig = wetsalt_d(hru_id)%salt(isalt)%irrig + irr_mass(sol_index) !kg  
                enddo
              endif
              !constituents
              if (gwsol_cons == 1) then
                do ics=1,cs_db%num_cs
                  sol_index = sol_index + 1
                  wet_water(hru_id)%cs(ics) = wet_water(hru_id)%cs(ics) + irr_mass(sol_index) !kg  
                  wetcs_d(hru_id)%cs(ics)%irrig = wetcs_d(hru_id)%cs(ics)%irrig + irr_mass(sol_index) !kg  
                enddo
              endif
            else !add to soil profile
              !nutrients
              soil1(hru_id)%mn(1)%no3 = soil1(hru_id)%mn(1)%no3 + (irr_mass(1)/hru(hru_id)%area_ha) !kg/ha - add to soil layer
              soil1(hru_id)%mp(1)%lab = soil1(hru_id)%mp(1)%lab + (irr_mass(2)/hru(hru_id)%area_ha) !kg/ha - add to soil layer
              sol_index = 2
              !salts
              if (gwsol_salt == 1) then
                do isalt=1,cs_db%num_salts
                  sol_index = sol_index + 1
                  cs_soil(hru_id)%ly(1)%salt(isalt) = cs_soil(hru_id)%ly(1)%salt(isalt) + (irr_mass(sol_index)/hru(hru_id)%area_ha) !kg/ha - add to soil layer
                  hsaltb_d(hru_id)%salt(isalt)%irgw = hsaltb_d(hru_id)%salt(isalt)%irgw + (irr_mass(sol_index)/hru(hru_id)%area_ha) !kg/ha - include in soil salt balance   
                enddo
              endif
              !constituents
              if (gwsol_cons == 1) then
                do ics=1,cs_db%num_cs
                  sol_index = sol_index + 1
                  cs_soil(hru_id)%ly(1)%cs(ics) = cs_soil(hru_id)%ly(1)%cs(ics) + (irr_mass(sol_index)/hru(hru_id)%area_ha) !kg/ha - add to soil layer
                  hcsb_d(hru_id)%cs(ics)%irgw = hcsb_d(hru_id)%cs(ics)%irgw + (irr_mass(sol_index)/hru(hru_id)%area_ha) !kg/ha - include in soil constituent balance   
                enddo
              endif
            endif
            !add to mass balance arrays (to be used in gwflow_simulate)
            do s=1,gw_nsolute !loop through the solutes
              gwsol_ss(cell_id)%solute(s)%ppag = (irr_mass(s)*1000.) * (-1) !g; negative = leaving the aquifer
              gwsol_ss_sum(cell_id)%solute(s)%ppag = gwsol_ss_sum(cell_id)%solute(s)%ppag - (irr_mass(s)*1000.)
            enddo
          endif
          
        enddo !go to next gwflow cell
        
			endif

      !store pumping (m3) for the HRU
      hru_pump(hru_id) = sum_pump
      
      end subroutine gwflow_ppag     