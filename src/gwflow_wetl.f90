      subroutine gwflow_wetl(hru_id) !rtb gwflow

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine determines the volume of groundwater exchanged with wetlands
!!    (exchange volumes are used in gwflow_simulate, in groundwater balance equations)
      
      use gwflow_module     
      use hydrograph_module, only : ob,sp_ob1,wet,wet_in_d
      use hru_module, only : hru
      use water_body_module, only : wet_wat_d
      
      implicit none

      integer, intent (in) :: hru_id	!               |id of the HRU, in which the wetland resides
      integer :: ires                 !               |wetland id
      integer :: s                    !               |solute counter
      integer :: icell                !               |counter for cells connected to the HRU
      integer :: cell_id              !               |gwflow cell
      real :: wt                      !m              |water table elevation within the cell
      real :: wet_stage               !m              |water stage of the wetland
      real :: wet_k                   !m/day          |hydraulic conductivity of the wetland bottom sediments
      real :: wet_area                !m2             |wetland area in connection with grid cell
      real :: wet_seep                !m3             |aggregage seepage from wetland
      real :: gw_inflow               !m3             |groundwater inflow to wetland (for single cell)
      real :: wet_inflow              !m3             |groundwater inflow to wetland (from all connected cells)
      real :: gwvol_avail             !m3             |current groundwater volume stored in grid cell
      real :: mass_transfer           !kg             |solute mass transferred from aquifer to wetland
      real :: gw_mass                 !kg             |solute mass stored in groundwater cell
      real :: wet_inflow_no3          !kg             |groundwater no3 mass to wetland (from all connected cells)
      real :: wet_inflow_solp         !kg             |groundwater p mass to wetland (from all connected cells)
      real :: solmass(100)            !g              |solute mass in cell
      
      
      
      !only proceed if groundwater-wetland exchange is active
      if (gw_wet_flag == 1) then
      
        !wetland id
        ires = hru(hru_id)%dbs%surf_stor
      
        !zero out variables
        wet_seep = 0.
        wet_inflow = 0.
        wet_inflow_no3 = 0.
        wet_inflow_solp = 0.
        gw_inflow = 0.
        
        !only proceed if the HRU is connected to gwflow cells
        if(hru_num_cells(hru_id) > 0) then
        
          !loop through the cells connected to the HRU
          do icell=1,hru_num_cells(hru_id)
          
            !retrieve water table elevation and wetland stage
            cell_id = hru_cells(hru_id,icell)
            wt = gw_state(cell_id)%head !water table elevation (m)
            wet_stage = ob(sp_ob1%hru + hru_id - 1)%elev !hru elevation (m)
            wet_k = hru(hru_id)%wet_hc * 24 / 1000. !mm/hr --> m/day
            wet_area = hru_cells_fract(hru_id,icell) * (wet_wat_d(hru_id)%area_ha * 10000.) !m2
            
            !compute groundwater inflow to wetland, if water table > wetland stage (Darcy's law)
            gw_inflow = 0.
            if(wt > wet_stage) then !groundwater inflow to wetland
              gw_inflow = wet_area * wet_k * ((wt-wet_stage)/wet_thick(ires)) !m3/day
              !check against available groundwater storage (m3) in the grid cell
              if(gw_state(cell_id)%head > gw_state(cell_id)%botm) then !if water table is above bedrock
                gwvol_avail = ((gw_state(cell_id)%head - gw_state(cell_id)%botm) * &
                                gw_state(cell_id)%area) * gw_state(cell_id)%spyd !m3
				      else
                gwvol_avail = 0.
              endif
              !if storage is less than wetland inflow, remove all groundwater
              if(gw_inflow > gwvol_avail) then
                gw_inflow = gwvol_avail
							endif
              !include in groundwater source-sink array (will be removed in gwflow_simulate)
              gw_ss(cell_id)%wetl = gw_ss(cell_id)%wetl + (gw_inflow*(-1)) !m3 negative = leaving the aquifer
              gw_ss_sum(cell_id)%wetl = gw_ss_sum(cell_id)%wetl + (gw_inflow*(-1))
              !add groundwater inflow to wetland; include in wetland water balance
              wet(hru_id)%flo = wet(hru_id)%flo + gw_inflow !m3
              wet_inflow = wet_inflow + gw_inflow !m3 --> track total inflow (for wetland water balance)
              !add solute mass to wetland
              !(mass is removed from the aquifer via mass balance equation in gwflow_simulate.f)
              mass_transfer = 0.
              if (gw_solute_flag == 1) then
                solmass = 0.
                !remove solute mass from gwflow cell
                do s=1,gw_nsolute
                  solmass(s) = gwsol_state(cell_id)%solute(s)%conc * gw_inflow !g/m3 * m3 = g  
                  if(solmass(s) > gwsol_state(cell_id)%solute(s)%mass) then !can only remove what is there
                    solmass(s) = gwsol_state(cell_id)%solute(s)%mass
                  endif
                  gwsol_ss(cell_id)%solute(s)%wetl = solmass(s)
                  gwsol_ss_sum(cell_id)%solute(s)%wetl = gwsol_ss_sum(cell_id)%solute(s)%wetl + solmass(s)
                enddo
                !add solute mass to wetland object
                !no3
                wet(hru_id)%no3 = wet(hru_id)%no3 + (solmass(1)/1000.) !kg
                wet_inflow_no3 = wet_inflow_no3 + (solmass(1)/1000.) !kg
                !p
                wet(hru_id)%solp = wet(hru_id)%solp + (solmass(2)/1000.) !kg
                wet_inflow_solp = wet_inflow_solp + (solmass(2)/1000.) !kg
                !salts
                !constituents
              endif
						else !wetland seepage to soil layers (add for all connected cells)
              wet_seep = wet_seep + (wet_area * wet_k * ((wet_stage-wt)/wet_thick(ires))) !m3/day  
            endif
            
          enddo !go to next cell connected to the HRU
          
        endif !if hru is connected to gwflow grid cells
        
        !copy values into the wetland object
        wet_wat_d(hru_id)%seep = wet_seep
        wet_in_d(hru_id)%flo = wet_in_d(hru_id)%flo + wet_inflow
        if (gw_solute_flag == 1) then
          wet_in_d(hru_id)%no3 = wet_in_d(hru_id)%no3 + wet_inflow_no3
          wet_in_d(hru_id)%solp = wet_in_d(hru_id)%solp + wet_inflow_solp
          !salts
          !cs
        endif
        
      endif !check if groundwater-wetland exchange is active
          
      return
      end subroutine gwflow_wetl    