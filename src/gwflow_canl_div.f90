      subroutine gwflow_canl_div !rtb gwflow

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the water exchange volume between irrigation canals and connected grid cells
!!    for canals that are connected to a point source diversion (recall object); seepage water should be 
!!    removed from the diverted water volume.
      
      use gwflow_module
      use hydrograph_module, only : irrig
      use time_module
      use constituent_mass_module
			use hru_module, only : hru
			use ch_cs_module, only: div_conc_cs
			use ch_salt_module, only: div_conc_salt
			use res_salt_module, only : wetsalt_d
			use res_cs_module, only : wetcs_d
			use salt_module, only : hsaltb_d
			use cs_module, only : hcsb_d
      
      implicit none

      integer :: i = 0                       !       |cell counter
	  integer :: j = 0
      integer :: s = 0                       !       |counter of groundwater solutes
      integer :: cell_id = 0                 !       |cell in connection with the canal 
	  integer :: irec = 0                    !       |recall ID for canal diversion
	  integer :: sol_index = 0
	  integer :: ics = 0
	  integer :: isalt = 0
	  integer :: hru_id = 0
      integer :: canal_id = 0
			integer :: wetland = 0                 !       |wetland flag
			integer :: dum = 0
      real :: width = 0.                      !m      |canal width
      real :: depth = 0.                      !m      |canal depth
      real :: thick = 0.                      !m      |canal bed thickness
      real :: length = 0.                     !m      |length of canal in the cell
      real :: stage = 0.                      !m      |stage of canal in the cell
      real :: bed_K = 0.											 !m/day  |hydraulic conductivity of canal bed in the cell
			real :: reduc = 0.
			real :: daycount_real = 0.
      real :: flow_area = 0.                  !m2     |groundwater flow area of water exchange, in cell
      real :: canal_bed = 0.                  !m      |canal bed elevation in the cell
      real :: head_diff = 0.                  !m      |head difference between canal stage and groundwater head
      real :: Q = 0.                          !m3/day |water exchange flow rate, calculated by Darcy's Law
      real :: solmass(100) = 0.               !g      |solute mass transferred
      real :: heat_flux = 0.                  !J      |heat in groundawter-canal exchange water
			real :: canal_area = 0.								 !m2     |total area irrigated by canal
			real :: irrig_depth = 0.                !mm     |depth of irrigation water for the HRUs irrigated by a canal
			real :: irrig_volm = 0.
			real :: irrig_conc = 0.
			real :: irrig_mass = 0.
			real :: canal_conc = 0.
			real :: mass_div = 0.
            real :: mass_stor = 0.
            real :: mass_pond = 0.
            real :: mass_seep = 0.
            real :: mass_irrg = 0.
            real :: mass_ret = 0.
			
      

      !only proceed if canal-cell exchange is active
      if (gw_canal_flag == 1) then
      
			
			  !first: calculate seepage from each canal ---------------------------------------------------------------------
			  !(seepage only occurs if the canal has water; based on point diversions)
			  
        !loop through the canal cells
        do i=1,gw_canal_ncells_div
       
				  !canal associated with grid cell
				  canal_id = gw_canl_div_cell(i)%canal_id
				  
					!only proceed if the canal has water
					if(gw_canl_div_info(canal_id)%stor > 0) then
					
            cell_id = gw_canl_div_cell(i)%cell_id
            if (gw_state(cell_id)%stat == 1) then !if cell is active
              !canal attributes
						  width = gw_canl_div_info(canal_id)%width
              depth = gw_canl_div_info(canal_id)%depth
              thick = gw_canl_div_info(canal_id)%thick
							bed_K = gw_canl_div_info(canal_id)%bed_K
						  !cell attributes
              length = gw_canl_div_cell(i)%leng
              canal_bed = gw_canl_div_cell(i)%elev
							stage = canal_bed + depth
              !calculate exchange rate Q (m3/day)
              flow_area = length * width !m2 = area of seepage
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
              !store values in gwflow source/sink arrays;
							!if the seepage is positive, then water is taken from the volume of water already diverted from
							!channels (point source diversion = recall object)
              if(Q < 0) then !groundwater --> canal
                if (-Q .ge.gw_state(cell_id)%stor) then !can only remove what is there
                  Q = -gw_state(cell_id)%stor
                endif
							elseif(Q > 0) then !canal --> groundwater
							  !remove seepage water from the diversion volume
								if(Q > gw_canl_div_info(canal_id)%stor) then
								  Q = gw_canl_div_info(canal_id)%stor
								endif
								gw_canl_div_info(canal_id)%stor = gw_canl_div_info(canal_id)%stor - Q
								gw_canl_div_info(canal_id)%out_seep = gw_canl_div_info(canal_id)%out_seep + Q
							endif
							gw_state(cell_id)%stor = gw_state(cell_id)%stor + Q !update available groundwater in the cell
              gw_ss(cell_id)%canl = gw_ss(cell_id)%canl + Q  
              gw_ss_sum(cell_id)%canl = gw_ss_sum(cell_id)%canl + Q !store for annual water 
              gw_ss_sum_mo(cell_id)%canl = gw_ss_sum_mo(cell_id)%canl + Q !store for monthly water
              
              !heat
              if(gw_heat_flag) then
                if(Q < 0) then
                  heat_flux = gwheat_state(cell_id)%temp * gw_rho * gw_cp * Q !J
                  if(-heat_flux >= gwheat_state(cell_id)%stor) then
                    heat_flux = -gwheat_state(cell_id)%stor
                  endif
                  gw_heat_ss(cell_id)%canl = gw_heat_ss(cell_id)%canl + heat_flux
                  gw_heat_ss_sum(cell_id)%canl = gw_heat_ss_sum(cell_id)%canl + heat_flux
                endif
              endif
              
              !calculate solute mass (g/day) transported between cell and channel
              if (gw_solute_flag == 1) then
                if(Q < 0) then !mass is leaving the cell --> canal
                  do s=1,gw_nsolute !loop through the solutes
                    solmass(s) = Q * gwsol_state(cell_id)%solute(s)%conc !g
                    if(solmass(s) > gwsol_state(cell_id)%solute(s)%mass) then !can only remove what is there
                      solmass(s) = gwsol_state(cell_id)%solute(s)%mass
                    endif
                  enddo
                else !mass entering the cell from the canal (i.e., from the diverted solute mass)
								  !calculate mass (g)
								  solmass(1) = Q * canal_out_conc(1) !no3
									solmass(2) = Q * canal_out_conc(2) !p
									sol_index = 2
                  !salts
                  if (gwsol_salt == 1) then
									  irec = gw_canl_div_info(canal_id)%divr
									  if(irec > 0) then
										  do isalt=1,cs_db%num_salts
                        sol_index = sol_index + 1
                        solmass(sol_index) = Q * div_conc_salt(isalt,irec)
                      enddo
										else
                      do isalt=1,cs_db%num_salts
                        sol_index = sol_index + 1
                        solmass(sol_index) = Q * canal_out_conc(sol_index)
                      enddo
										endif
                  endif
                  !constituents
                  if (gwsol_cons == 1) then
									  irec = gw_canl_div_info(canal_id)%divr
									  if(irec > 0) then
										  do ics=1,cs_db%num_cs
                        sol_index = sol_index + 1
                        solmass(sol_index) = Q * div_conc_cs(ics,irec)
                      enddo
										else
                      do ics=1,cs_db%num_cs
                        sol_index = sol_index + 1
                        solmass(sol_index) = Q * canal_out_conc(sol_index)
                      enddo
										endif
                  endif
                endif
                !store in mass balance arrays
                do s=1,gw_nsolute !loop through the solutes
                  gwsol_ss(cell_id)%solute(s)%canl = gwsol_ss(cell_id)%solute(s)%canl + solmass(s)
                  gwsol_ss_sum(cell_id)%solute(s)%canl = gwsol_ss_sum(cell_id)%solute(s)%canl + solmass(s)
									gwsol_ss_sum_mo(cell_id)%solute(s)%canl = gwsol_ss_sum_mo(cell_id)%solute(s)%canl + solmass(s)
                enddo
              endif !end solutes  
                
            endif !check if cell is active
          endif !check if canal has water
          
        enddo !go to next canal cell
      
				
				!second: calculate irrigation for each HRU --------------------------------------------------------------------
				!(using daily diversion volume: return, pond inflow, and seepage already accounted for)
				
				!loop through each canal; for each canal, calculate daily irrigation and runoff for each
				!HRU within the canal command area
				do i=1,gw_ncanal
				  
				  !calculate irrigation depths for each HRU
				  !first: calculate total command area (m2)
				  canal_area = 0.
				  do j=1,gw_canl_div_info(i)%nhru
				    hru_id = gw_canl_div_info(i)%hrus(j)
					  canal_area = canal_area + (hru(hru_id)%area_ha * 10000.)
					enddo
				  
					!second: calculate irrigation depth (mm); then apply depth and runoff to each HRU
					irrig_depth = 0.
					if(canal_area > 0) then
						irrig_depth = (gw_canl_div_info(i)%stor / canal_area) * 1000.
						do j=1,gw_canl_div_info(i)%nhru
							hru_id = gw_canl_div_info(i)%hrus(j)
							irrig(hru_id)%applied = irrig_depth * (1. - gw_canl_div_info(i)%hru_ro(j))
							irrig(hru_id)%runoff = irrig_depth * gw_canl_div_info(i)%hru_ro(j)
							irrig_volm = (irrig_depth/1000.) * (hru(hru_id)%area_ha * 10000.) !m3
							gw_canl_div_info(i)%out_irrg = gw_canl_div_info(i)%out_irrg + irrig_volm
							gw_canl_div_info(i)%stor = gw_canl_div_info(i)%stor - irrig_volm
						enddo
					endif
					
				  !write out daily volumes and fluxes for the canal
					write(out_canal_bal,100) time%yrc,time%mo,time%day,i,gw_canl_div_info(i)%div, &
					                                                     gw_canl_div_info(i)%stor, &
																															 gw_canl_div_info(i)%out_pond, &
																															 gw_canl_div_info(i)%out_seep, &
																															 gw_canl_div_info(i)%out_irrg, &
																															 gw_canl_div_info(i)%div_ret
						
					!add solute mass to fields (soil layer)
					if (gw_solute_flag == 1) then																										 
					do j=1,gw_canl_div_info(i)%nhru
					  hru_id = gw_canl_div_info(i)%hrus(j)
					  irrig_volm = irrig_depth * (hru(hru_id)%area_ha * 10000.) !m3
						irec = gw_canl_div_info(i)%divr
						sol_index = 2
						!salt ions
						if (gwsol_salt == 1) then
							do isalt=1,cs_db%num_salts
                sol_index = sol_index + 1
                irrig_conc = div_conc_salt(isalt,irec)
								irrig_mass = (irrig_volm * irrig_conc) / 1000. !m3 * g/m3 = g --> kg
								wetland = hru(hru_id)%dbs%surf_stor !check if HRU is a wetland
								if(wetland > 0) then !add to wetland
									wet_water(hru_id)%salt(isalt) = wet_water(hru_id)%salt(isalt) + irrig_mass !kg
									wetsalt_d(hru_id)%salt(isalt)%irrig = wetsalt_d(hru_id)%salt(isalt)%irrig + irrig_mass !kg
								else !add to soil profile
									cs_soil(hru_id)%ly(1)%salt(isalt) = cs_soil(hru_id)%ly(1)%salt(isalt) + (irrig_mass/hru(hru_id)%area_ha) !kg/ha - add to soil layer
									hsaltb_d(hru_id)%salt(isalt)%irsw = hsaltb_d(hru_id)%salt(isalt)%irsw + (irrig_mass/hru(hru_id)%area_ha) !kg/ha - include in soil salt balance  
								endif
								!write out daily solute mass balance
								canal_conc = div_conc_salt(isalt,irec)
								mass_div = (gw_canl_div_info(i)%div * canal_conc) / 1000. !kg
								mass_stor = (gw_canl_div_info(i)%stor * canal_conc) / 1000. !kg 
								mass_pond = (gw_canl_div_info(i)%out_pond * canal_conc) / 1000. !kg
								mass_seep = (gw_canl_div_info(i)%out_seep * canal_conc) / 1000. !kg
								mass_irrg = (gw_canl_div_info(i)%out_irrg * canal_conc) / 1000. !kg
								mass_ret = (gw_canl_div_info(i)%div_ret * canal_conc) / 1000. !kg
								write(out_canal_sol,101) time%yrc,time%mo,time%day,i,gwsol_nm(sol_index),mass_div,mass_stor,mass_pond,mass_seep,mass_irrg,mass_ret
              enddo
            endif
						!constituents
						if (gwsol_cons == 1) then
							do ics=1,cs_db%num_cs
                sol_index = sol_index + 1
                irrig_conc = div_conc_cs(ics,irec)
								irrig_mass = (irrig_volm * irrig_conc) / 1000. !m3 * g/m3 = g --> kg
								wetland = hru(hru_id)%dbs%surf_stor !check if HRU is a wetland
								if(wetland > 0) then !add to wetland
									wet_water(hru_id)%cs(ics) = wet_water(hru_id)%cs(ics) + irrig_mass !kg
									wetcs_d(hru_id)%cs(ics)%irrig = wetcs_d(hru_id)%cs(ics)%irrig + irrig_mass !kg
								else !add to soil profile
									cs_soil(hru_id)%ly(1)%cs(ics) = cs_soil(hru_id)%ly(1)%cs(ics) + (irrig_mass/hru(hru_id)%area_ha) !kg/ha - add to soil layer
									hcsb_d(hru_id)%cs(ics)%irsw = hcsb_d(hru_id)%cs(ics)%irsw + (irrig_mass/hru(hru_id)%area_ha) !kg/ha - include in soil cs balance  
								endif
								!write out daily solute mass balance
								canal_conc = div_conc_cs(ics,irec)
								mass_div = (gw_canl_div_info(i)%div * canal_conc) / 1000. !kg
								mass_stor = (gw_canl_div_info(i)%stor * canal_conc) / 1000. !kg 
								mass_pond = (gw_canl_div_info(i)%out_pond * canal_conc) / 1000. !kg
								mass_seep = (gw_canl_div_info(i)%out_seep * canal_conc) / 1000. !kg
								mass_irrg = (gw_canl_div_info(i)%out_irrg * canal_conc) / 1000. !kg
								mass_ret = (gw_canl_div_info(i)%div_ret * canal_conc) / 1000. !kg
								write(out_canal_sol,101) time%yrc,time%mo,time%day,i,gwsol_nm(sol_index),mass_div,mass_stor,mass_pond,mass_seep,mass_irrg,mass_ret
              enddo
            endif
					enddo
					endif !if solute transport is active
																 
				enddo !go to next canal
				
      endif !check if canal-cell exchange is active
			
			
			
100   format (4i8,10(e15.7))
101   format (4i8,5x,a10,10(e15.7))

      return
      end subroutine gwflow_canl_div
			