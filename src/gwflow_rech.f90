      subroutine gwflow_rech !rtb gwflow

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine determines the volume of groundwater that is added to the aquifer via recharge (soil percolation)
!!    (recharge volumes are used in gwflow_simulate, in groundwater balance equations)
      
      use gwflow_module   
      use hydrograph_module, only : ob,sp_ob,sp_ob1
      use maximum_data_module, only : db_mx
      use calibration_data_module, only : lsu_out
      use soil_module, only : soil
      
      implicit none

      integer :: i = 0                !           |counter
      integer :: j = 0                !           |counter for number of HRUs within an LSU
      integer :: k = 0                !           |counter
      integer :: n = 0                !           |counter
      integer :: s = 0                !           |solute counter
      integer :: hru_id = 0           !           |id of the HRU
      integer :: ob_num = 0           !           |object number of the HRU
      integer :: cell_id = 0          !           |id of the gwflow cell
      integer :: cell_count = 0       !           |cell count
      integer :: dum = 0
      real :: recharge = 0.           !mm         |HRU recharge
      real :: recharge_sol = 0.       !kg/ha      |solute mass in recharge water
      real :: hru_recharge = 0.       !m3         |volume of recharge from the HRU
      real :: rech_volume = 0.        !m3         |summation of recharge from multiple HRUs
      real :: cell_rech_volume = 0.   !m3         |volume of recharge to the cell
      real :: rech_solmass(100) = 0.  !g          |summation of solute mass in recharge from multiple HRUs
      real :: cell_rech_solmass(100) = 0. !g      |solute mass in recharge to the cell
      real :: cell_rech_heat = 0.     !J          |heat in recharge to the cell
      real :: hru_total = 0.          !m3         |summation of recharge from multiple HRUs
      real :: hru_cell_total = 0.     !m3         |summation of recharge for multiple cells
      real :: huc12_cell_total = 0.   !m3         |summation of recharge from a huc12 catchment
      real :: sub_recharge = 0.       !m3         |summation of recharge for subbasin
      real :: sub_heat = 0.           !J          |summation of recharge heat for subbasin
      real :: sub_solmass(100) = 0.   !g          |total solute mass in recharge, for the subbasin
      real :: perc_volm = 0.          !m3         |volume of deep percolation
      real :: perc_temp = 0.          !deg C      |temperature of soil water in the lowest soil layer (= deep percolation)
      real :: perc_heat = 0.          !J          |heat of deep percolation water
      real :: recharge_heat = 0.      !J          |heat in recharge water
      

      !calculate recharge and solute mass to the water table
      do k=1,sp_ob%hru
        ob_num = sp_ob1%hru + k - 1
        recharge = gw_rech(k)
        gw_rech(k) = 0.
        gw_rech(k) = ((1.-gw_delay(k))*gwflow_perc(k)) + (gw_delay(k)*recharge)
        if (gw_rech(k) < 1.e-6) gw_rech(k) = 0.
        if(gw_heat_flag) then
          perc_volm = (gwflow_perc(k)/1000.) * (ob(ob_num)%area_ha * 10000.) !m * m2 = m3
          perc_temp = soil(k)%phys(soil(k)%nly)%tmp !deg C
          perc_heat = perc_temp * gw_rho * gw_cp * perc_volm !J
          recharge_heat = gw_rechheat(k)
          gw_rechheat(k) = ((1.-gw_delay(k))*perc_heat) + (gw_delay(k)*recharge_heat)
        endif
        if(gw_solute_flag == 1) then
          do s=1,gw_nsolute !loop through the solutes
            recharge_sol = gw_rechsol(k,s)
            gw_rechsol(k,s) = ((1.-gw_delay(k))*gwflow_percsol(k,s)) + (gw_delay(k)*recharge_sol)
          enddo
        endif
      enddo
			
			
			
      !use hru recharge to calculate recharge (m3) cell values
      if (lsu_cells_link == 1) then !LSU-cell connection
      
        !loop through the landscape units
        do k=1,db_mx%lsu_out
          !sum recharge (m3) for each LSU (based on collection of HRUs within the LSU)
          rech_volume = 0.
          if (gw_solute_flag == 1) then
            rech_solmass = 0.
					endif
          do j=1,lsu_out(k)%num_tot
            hru_id = lsu_out(k)%num(j)
            ob_num = sp_ob1%hru + hru_id - 1
            hru_recharge = (gw_rech(hru_id)/1000.) * (ob(ob_num)%area_ha * 10000.) !m * m2 = m3
            rech_volume = rech_volume + hru_recharge
            if (gw_solute_flag == 1) then
              do s=1,gw_nsolute !loop through the solutes
                rech_solmass(s) = rech_solmass(s) + (gw_rechsol(hru_id,s)*ob(ob_num)%area_ha*1000.) !g
              enddo
			endif
          enddo
          !map recharge from LSU to grid cells connected to the LSU
          do i=1,lsu_num_cells(k)
            cell_id = lsu_cells(k,i)
						if(gw_state(cell_id)%stat == 2) then !if boundary cell, give recharge to nearest active cell
						  cell_id = gw_bound_near(cell_id)
						endif
            cell_rech_volume = rech_volume * lsu_cells_fract(k,i)
            gw_ss(cell_id)%rech = gw_ss(cell_id)%rech + cell_rech_volume
            gw_ss_sum(cell_id)%rech = gw_ss_sum(cell_id)%rech + cell_rech_volume !store for annual water
            gw_ss_sum_mo(cell_id)%rech = gw_ss_sum_mo(cell_id)%rech + cell_rech_volume !store for monthly water
            if(gw_heat_flag) then
              cell_rech_heat = gw_rechheat(k) * lsu_cells_fract(k,i)
              gw_heat_ss(cell_id)%rech = gw_heat_ss(cell_id)%rech + cell_rech_heat
              gw_heat_ss_sum(cell_id)%rech = gw_heat_ss_sum(cell_id)%rech + cell_rech_heat
            endif
            if(gw_solute_flag.eq.1) then
              do s=1,gw_nsolute !loop through the solutes
                cell_rech_solmass(s) = rech_solmass(s) * lsu_cells_fract(k,i)  
                gwsol_ss(cell_id)%solute(s)%rech = gwsol_ss(cell_id)%solute(s)%rech + cell_rech_solmass(s)
                gwsol_ss_sum(cell_id)%solute(s)%rech = gwsol_ss_sum(cell_id)%solute(s)%rech + cell_rech_solmass(s)
								gwsol_ss_sum_mo(cell_id)%solute(s)%rech = gwsol_ss_sum_mo(cell_id)%solute(s)%rech + cell_rech_solmass(s)
              enddo
            endif
          enddo
        enddo !go to next LSU
        
			else !proceed with HRU-cell connection
      
      !map recharge from the HRUs to the grid cells
      if (nat_model) then !national model application
        !loop through the HUC12 subwatersheds
        cell_received = 0
        ob_num = sp_ob1%hru  !object number of first HRU
        hru_total = 0.
        hru_cell_total = 0.
        huc12_cell_total = 0.
        do n=1,sp_ob%outlet
          !loop through the HRUs in the subwatershed - assign recharge to any connected cells
          sub_recharge = 0.
          sub_heat = 0.
          sub_solmass = 0.
          do k=1,huc12_nhru(n)
            hru_id = huc12_hrus(n,k)
            rech_volume = (gw_rech(hru_id)/1000.) * (ob(ob_num)%area_ha * 10000.) !m * m2 = m3
						
						if(rech_volume > 0) then
						  dum = 10
						endif
						
            hru_total = hru_total + rech_volume
            if (gw_solute_flag == 1) then
              do s=1,gw_nsolute !loop through the solutes
                rech_solmass(s) = gw_rechsol(hru_id,s) * ob(ob_num)%area_ha * 1000. !g
              enddo
            endif
            if(hrus_connected(hru_id).eq.1) then
              do i=1,hru_num_cells(hru_id)
                cell_id = hru_cells(hru_id,i)
								if(gw_state(cell_id)%stat == 2) then !if boundary cell, give recharge to nearest active cell
						      cell_id = gw_bound_near(cell_id)
						    endif
                cell_received(cell_id) = 1
                cell_rech_volume = rech_volume * hru_cells_fract(hru_id,i)
                hru_cell_total = hru_cell_total + cell_rech_volume
                gw_ss(cell_id)%rech = gw_ss(cell_id)%rech + cell_rech_volume
                gw_ss_sum(cell_id)%rech = gw_ss_sum(cell_id)%rech + cell_rech_volume !store for annual water
                gw_ss_sum_mo(cell_id)%rech = gw_ss_sum_mo(cell_id)%rech + cell_rech_volume !store for monthly water
                if(gw_heat_flag) then
                  cell_rech_heat = gw_rechheat(hru_id) * hru_cells_fract(hru_id,i)
                  gw_heat_ss(cell_id)%rech = gw_heat_ss(cell_id)%rech + cell_rech_heat
                  gw_heat_ss_sum(cell_id)%rech = gw_heat_ss_sum(cell_id)%rech + cell_rech_heat
                endif
                if (gw_solute_flag == 1) then
                  do s=1,gw_nsolute !loop through the solutes
                    cell_rech_solmass(s) = rech_solmass(s) * hru_cells_fract(hru_id,i)
                    gwsol_ss(cell_id)%solute(s)%rech = gwsol_ss(cell_id)%solute(s)%rech + cell_rech_solmass(s)
                    gwsol_ss_sum(cell_id)%solute(s)%rech = gwsol_ss_sum(cell_id)%solute(s)%rech + cell_rech_solmass(s)
										gwsol_ss_sum_mo(cell_id)%solute(s)%rech = gwsol_ss_sum_mo(cell_id)%solute(s)%rech + cell_rech_solmass(s)
                  enddo
                endif
              enddo      
            else
              sub_recharge = sub_recharge + rech_volume
              if(gw_heat_flag) then
                sub_heat = sub_heat + gw_rechheat(hru_id)
              endif
              if(gw_solute_flag == 1) then
                do s=1,gw_nsolute !loop through the solutes
                  sub_solmass(s) = sub_solmass(s) + rech_solmass(s)  
                enddo
              endif
            endif
            ob_num = ob_num + 1
          enddo
          !loop through the cells in the subwatershed - assign remaining recharge to unconnected cells
          !first: count the number of cells in each subwatershed that did not receive recharge from an HRU
          cell_count = 0
          do k=1,huc12_ncell(n)
            cell_id = huc12_cells(n,k)
						if(gw_state(cell_id)%stat == 2) then !if boundary cell, give recharge to nearest active cell
						  cell_id = gw_bound_near(cell_id)
						endif
            if(cell_received(cell_id).eq.0) then !has not been given recharge from an HRU
              cell_count = cell_count + 1
            endif
          enddo
          !second: calculate the recharge that should go to each grid cell
          !only proceed if there are unconnected cells (i.e. cell_count > 0)
          if(cell_count.gt.0) then
            cell_rech_volume = sub_recharge / cell_count
            if (gw_heat_flag) then
              cell_rech_heat = sub_heat / cell_count
            endif
            if (gw_solute_flag == 1) then
              do s=1,gw_nsolute !loop through the solutes
                cell_rech_solmass(s) = sub_solmass(s) / cell_count
              enddo
						endif
            !third: assign the average cell recharge to each cell
            do k=1,huc12_ncell(n)
              cell_id = huc12_cells(n,k)
							if(gw_state(cell_id)%stat == 2) then !if boundary cell, give recharge to nearest active cell
						    cell_id = gw_bound_near(cell_id)
						  endif
              if(cell_received(cell_id).eq.0) then !has not been given recharge from an HRU
                gw_ss(cell_id)%rech = cell_rech_volume
                huc12_cell_total = huc12_cell_total + cell_rech_volume
                gw_ss_sum(cell_id)%rech = gw_ss_sum(cell_id)%rech + cell_rech_volume !store for annual water
                gw_ss_sum_mo(cell_id)%rech = gw_ss_sum_mo(cell_id)%rech + cell_rech_volume !store for monthly water
                if(gw_heat_flag) then
                  gw_heat_ss(cell_id)%rech = cell_rech_heat
                  gw_heat_ss_sum(cell_id)%rech = gw_heat_ss_sum(cell_id)%rech + cell_rech_heat
                endif
                if (gw_solute_flag == 1) then
                  do s=1,gw_nsolute !loop through the solutes
                    gwsol_ss(cell_id)%solute(s)%rech = cell_rech_solmass(s)
                    gwsol_ss_sum(cell_id)%solute(s)%rech = gwsol_ss_sum(cell_id)%solute(s)%rech + cell_rech_solmass(s)
										gwsol_ss_sum_mo(cell_id)%solute(s)%rech = gwsol_ss_sum_mo(cell_id)%solute(s)%rech + cell_rech_solmass(s)
                  enddo
                endif            
              endif
            enddo
          endif
        enddo
      else !proceed with normal mapping routine
        ob_num = sp_ob1%hru  !object number of first HRU
        do k=1,sp_ob%hru
          rech_volume = (gw_rech(k)/1000.) * (ob(ob_num)%area_ha * 10000.) !m * m2 = m3
          if (gw_solute_flag == 1) then
            do s=1,gw_nsolute !loop through the solutes
              rech_solmass(s) = gw_rechsol(k,s) * ob(ob_num)%area_ha * 1000. !g
            enddo
          endif
          do i=1,hru_num_cells(k)
            cell_id = hru_cells(k,i)
						if(gw_state(cell_id)%stat == 2) then !if boundary cell, give recharge to nearest active cell
						  cell_id = gw_bound_near(cell_id)
						endif
            cell_rech_volume = rech_volume * hru_cells_fract(k,i)
            gw_ss(cell_id)%rech = gw_ss(cell_id)%rech + cell_rech_volume
            gw_ss_sum(cell_id)%rech = gw_ss_sum(cell_id)%rech + cell_rech_volume !store for annual water
            gw_ss_sum_mo(cell_id)%rech = gw_ss_sum_mo(cell_id)%rech + cell_rech_volume !store for monthly water
            if(gw_heat_flag) then
              cell_rech_heat = gw_rechheat(k) * hru_cells_fract(k,i)
              gw_heat_ss(cell_id)%rech = gw_heat_ss(cell_id)%rech + cell_rech_heat
              gw_heat_ss_sum(cell_id)%rech = gw_heat_ss_sum(cell_id)%rech + cell_rech_heat
            endif
            if (gw_solute_flag == 1) then
              do s=1,gw_nsolute !loop through the solutes
                cell_rech_solmass(s) = rech_solmass(s) * hru_cells_fract(k,i)
                gwsol_ss(cell_id)%solute(s)%rech = gwsol_ss(cell_id)%solute(s)%rech + cell_rech_solmass(s)
                gwsol_ss_sum(cell_id)%solute(s)%rech = gwsol_ss_sum(cell_id)%solute(s)%rech + cell_rech_solmass(s)
								gwsol_ss_sum_mo(cell_id)%solute(s)%rech = gwsol_ss_sum_mo(cell_id)%solute(s)%rech + cell_rech_solmass(s)
              enddo
            endif
          enddo
          ob_num = ob_num + 1
        enddo
      endif
      
      endif !check for LSU-cell connection
       
      return
    end subroutine gwflow_rech      
    