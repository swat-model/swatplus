      subroutine gwflow_pond !rtb gwflow

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the volume of seepage from recharge ponds;
!!    removes water from source channel or canal diversion;
!!    writes out recharge pond water balance
	
      
      use gwflow_module
      use hydrograph_module, only : ch_stor
      use time_module
      use constituent_mass_module
      use water_allocation_module
      use climate_module
      use ch_salt_module, only: div_conc_salt
      use ch_cs_module, only: div_conc_cs
			
      implicit none

      integer :: r = 0                       !       |counter for number of recharge ponds
	    integer :: k = 0                       !       |counter for the number of cells connected to the recharge pond
	    integer :: s = 0						 !       |solute counter
	    integer :: year = 0
      integer :: day = 0
      integer :: month = 0
	    integer :: chan_id = 0                 !       |channel id
      integer :: rec_id = 0                  !       |point source if (canal diversion)
      integer :: cell_id = 0				 !       |cell id
      integer :: iwst = 0					 !       |weather station id
      integer :: dum = 0
      integer :: sol_index = 0               !       |solute counter (no3, p, salt ions, constituents)
      integer :: isalt = 0                   !       |salt ion counter
      integer :: ics = 0					 !       |constituent counter
      integer :: canal_id = 0
      real :: div_vol(5000) = 0.			 !m3     |daily water volume diverted to each recharge pond
      real :: chan_volume = 0.               !m3     |starting volume in the source channel
      real :: cell_recharge = 0.             !m3     |recharge from the pond to the aquifer, for a single cell
      real :: pond_recharge = 0.			 !m3     |total recharge from the pond to the aquifer
      real :: div_specified = 0.             !m3     |specified diversion volume in gwflow.ponds file
      real :: div_added = 0.                 !m3     |actual volume added to the recharge pond
      real :: pond_evap = 0.                 !m3     |water evaporated from the pond during the day
      real :: pond_rain = 0.                 !m3     |rainfall added to the pond during the day
      real :: pond_volume = 0.               !m3     |pond volume before recharge occurs
      real :: sol_conc = 0.                  !g/m3   |solute concentration in the source water
      real :: sol_mass = 0.                  !kg     |solute mass removed from the source added --> added to recharge pond
      real :: div_mass(20) = 0.              !kg     |solute mass added to the recharge pond
      real :: rech_mass(20) = 0.             !kg     |solute mass leaching from the pond to the water table
      real :: rech_mass_cell(20) = 0.        !g      |solute mass leaching from the pond to an individual cell
			
      
      
			
      !only proceed if the recharge pond flag has been activated ----------------------------------------------------------------
      if (gw_pond_flag == 1) then
      
			  !read the diverted volumes (m3) for the current day -----------------------------------------------------------
			  read(in_ponds,*) year,month,day,(gw_pond_info(r)%div,r=1,gw_npond)
			  
        !loop through the recharge ponds ------------------------------------------------------------------------------
			  do r=1,gw_npond
			    
				  !zero out values
				  pond_rain = 0.
				  div_added = 0.
					pond_evap = 0.
					pond_recharge = 0.
					gw_pond_info(r)%div_uns = 0.
					div_mass = 0.
					rech_mass = 0.
					
				  !only proceed if the recharge pond is in operation
					if(gw_daycount .ge. gw_pond_info(r)%dy_start) then
					
				  !add diverted water to recharge pond storage (m3) ---------------------------------------
				  div_specified = gw_pond_info(r)%div
					if(div_specified > 0) then
					  dum = 10
					endif
					div_added = 0.
					!channel source
					if(gw_pond_info(r)%chan > 0) then
					  chan_id = gw_pond_info(r)%chan
            chan_volume = ch_stor(chan_id)%flo !current channel storage (m3)
						!determine water volume added to the recharge pond
						div_added = div_specified
						if(div_specified > ch_stor(chan_id)%flo) then
						  div_added = ch_stor(chan_id)%flo
						endif
						gw_pond_info(r)%div_uns = div_specified - div_added !unsatisfied diversion
						!remove from the channel --> add to the recharge pond
	          ch_stor(chan_id)%flo = ch_stor(chan_id)%flo - div_added
						gw_pond_info(r)%stor = gw_pond_info(r)%stor + div_added
						!remove salt mass from the channel --> add to the recharge pond	
						if (gw_solute_flag == 1) then
							if(chan_volume > 10.) then
								!no3
								sol_conc = (ch_stor(chan_id)%no3 * 1000.) / chan_volume !no3 g/m3 in channel 
								sol_mass = (sol_conc * div_added) / 1000. !kg removed from channel
								if(sol_mass > ch_stor(chan_id)%no3) then
								  sol_mass = ch_stor(chan_id)%no3	
								endif
								ch_stor(chan_id)%no3 = ch_stor(chan_id)%no3 - sol_mass
								gw_pond_info(r)%sol_mass(1) = gw_pond_info(r)%sol_mass(1) + sol_mass !kg added to pond
								div_mass(1) = sol_mass
								!p
								sol_conc = (ch_stor(chan_id)%solp * 1000.) / chan_volume !p g/m3 in channel 
								sol_mass = (sol_conc * div_added) / 1000. !kg removed from channel
								if(sol_mass > ch_stor(chan_id)%solp) then
								  sol_mass = ch_stor(chan_id)%solp
								endif
								ch_stor(chan_id)%solp = ch_stor(chan_id)%solp - sol_mass
								gw_pond_info(r)%sol_mass(2) = gw_pond_info(r)%sol_mass(2) + sol_mass !kg added to pond
								div_mass(2) = sol_mass
								!salt
								sol_index = 2
								if (gwsol_salt == 1) then
									do isalt=1,cs_db%num_salts
										sol_index = sol_index + 1
										sol_conc = (ch_water(chan_id)%salt(isalt) * 1000.) / chan_volume !g/m3 in channel water   
										sol_mass = (sol_conc * div_added) / 1000. !kg removed from channel
										if(sol_mass > ch_water(chan_id)%salt(isalt)) then
										  sol_mass = ch_water(chan_id)%salt(isalt)
										endif
										ch_water(chan_id)%salt(isalt) = ch_water(chan_id)%salt(isalt) - sol_mass
										gw_pond_info(r)%sol_mass(sol_index) = gw_pond_info(r)%sol_mass(sol_index) + sol_mass !kg added to pond
										div_mass(sol_index) = sol_mass
									enddo
								endif
								!constituents
								if (gwsol_cons == 1) then
									do ics=1,cs_db%num_cs
										sol_index = sol_index + 1
										sol_conc = (ch_water(chan_id)%cs(ics) * 1000.) / chan_volume !g/m3 in channel water
										sol_mass = (sol_conc * div_added) / 1000. !kg removed from channel
										if(sol_mass > ch_water(chan_id)%cs(ics)) then
										  sol_mass = ch_water(chan_id)%cs(ics)
										endif
										ch_water(chan_id)%cs(ics) = ch_water(chan_id)%cs(ics) - sol_mass
										gw_pond_info(r)%sol_mass(sol_index) = gw_pond_info(r)%sol_mass(sol_index) + sol_mass !kg added to pond
										div_mass(sol_index) = sol_mass
									enddo
								endif
							endif
						endif
					!canal diversion source
					elseif(gw_pond_info(r)%canal > 0) then
					  !determine the canal id
					  canal_id = gw_pond_info(r)%canal
						if(gw_canl_div_info(canal_id)%stor > 0) then
						  div_added = div_specified
						  if(div_specified > gw_canl_div_info(canal_id)%stor) then
							  div_added = gw_canl_div_info(canal_id)%stor
							endif
							gw_pond_info(r)%div_uns = div_specified - div_added !unsatisfied diversion
							!remove from the canal diversion --> add to the recharge pond
							gw_canl_div_info(canal_id)%stor = gw_canl_div_info(canal_id)%stor - div_added
							gw_canl_div_info(canal_id)%out_pond = gw_canl_div_info(canal_id)%out_pond + div_added
						  gw_pond_info(r)%stor = gw_pond_info(r)%stor + div_added
							if (gw_solute_flag == 1) then
							  rec_id = gw_canl_div_info(canal_id)%divr
								!salt
								sol_index = 2
								if (gwsol_salt == 1) then
									do isalt=1,cs_db%num_salts
										sol_index = sol_index + 1  
										sol_mass = (div_conc_salt(isalt,rec_id) * div_added) / 1000. !kg removed from channel
										gw_pond_info(r)%sol_mass(sol_index) = gw_pond_info(r)%sol_mass(sol_index) + sol_mass !kg added to pond
										div_mass(sol_index) = sol_mass
									enddo
								endif
								!constituents
								if (gwsol_cons == 1) then
									do ics=1,cs_db%num_cs
										sol_index = sol_index + 1
										sol_mass = (div_conc_cs(ics,rec_id) * div_added) / 1000. !kg removed from channel
										gw_pond_info(r)%sol_mass(sol_index) = gw_pond_info(r)%sol_mass(sol_index) + sol_mass !kg added to pond
										div_mass(sol_index) = sol_mass
									enddo
								endif
							endif
							
						endif
					!outside water source	
					elseif(gw_pond_info(r)%unl == 1) then
					  gw_pond_info(r)%stor = gw_pond_info(r)%stor + div_specified
						!solute mass
						if (gw_solute_flag == 1) then
						  !no3
						  sol_conc = gw_pond_info(r)%unl_conc(1) !g/m3
							sol_mass = (sol_conc * div_specified) / 1000. !kg
							gw_pond_info(r)%sol_mass(1) = gw_pond_info(r)%sol_mass(1) + sol_mass !kg added to pond
							!p
							sol_conc = gw_pond_info(r)%unl_conc(2) !g/m3
							sol_mass = (sol_conc * div_specified) / 1000. !kg
							gw_pond_info(r)%sol_mass(2) = gw_pond_info(r)%sol_mass(2) + sol_mass !kg added to pond
							!salt
							sol_index = 2
							if (gwsol_salt == 1) then
								do isalt=1,cs_db%num_salts
									sol_index = sol_index + 1  
									sol_conc = gw_pond_info(r)%unl_conc(sol_index) !g/m3
									sol_mass = (sol_conc * div_specified) / 1000. !kg
									gw_pond_info(r)%sol_mass(sol_index) = gw_pond_info(r)%sol_mass(sol_index) + sol_mass !kg added to pond
									div_mass(sol_index) = sol_mass
								enddo
							endif
							!constituents
							if (gwsol_cons == 1) then
								do ics=1,cs_db%num_cs
									sol_index = sol_index + 1
									sol_conc = gw_pond_info(r)%unl_conc(sol_index) !g/m3
									sol_mass = (sol_conc * div_specified) / 1000. !kg
									gw_pond_info(r)%sol_mass(sol_index) = gw_pond_info(r)%sol_mass(sol_index) + sol_mass !kg added to pond
									div_mass(sol_index) = sol_mass
								enddo
							endif
						endif
					else !no water source - no water added to recharge pond
					  goto 10   
					endif
					
					!rainfall
					iwst = gw_pond_info(r)%wsta
					pond_rain = (wst(iwst)%weat%precip/1000.) * gw_pond_info(r)%area !m3
					gw_pond_info(r)%stor = gw_pond_info(r)%stor + pond_rain
					
					!evaporation
					iwst = gw_pond_info(r)%wsta
					pond_evap = gw_pond_info(r)%evap_co * (wst(iwst)%weat%pet/1000.) * gw_pond_info(r)%area !m3
					if(pond_evap > gw_pond_info(r)%stor) then
					  pond_evap = gw_pond_info(r)%stor
					endif
					gw_pond_info(r)%stor = gw_pond_info(r)%stor - pond_evap
					
				  !loop through the number of cells connected to the recharge pond
				  pond_recharge = 0.
				  do k=1,gw_pond_info(r)%ncell
					  !calculate recharge to the cell
					  cell_id = gw_pond_info(r)%cells(k)
						if(gw_state(cell_id)%stat == 1) then !only proceed if the cell is active
							
						  !store initial pond storage volume
						  pond_volume = gw_pond_info(r)%stor !m3
						
						  !calculate pond seepage (recharge)
						  cell_recharge = gw_pond_info(r)%bed_k * gw_pond_info(r)%conn_area(k) !m3/day
							!check against pond volume - can only remove what is available in storage
							if(cell_recharge > gw_pond_info(r)%stor) then
								cell_recharge = gw_pond_info(r)%stor
							endif
							
							!store for water balance and output
							gw_ss(cell_id)%pond = gw_ss(cell_id)%pond + cell_recharge
							gw_ss_sum(cell_id)%pond = gw_ss_sum(cell_id)%pond + cell_recharge
							gw_ss_sum_mo(cell_id)%pond = gw_ss_sum_mo(cell_id)%pond + cell_recharge
							!add to pond recharge --> remove from pond storage
							pond_recharge = pond_recharge + cell_recharge !add to total pond recharge
							gw_pond_info(r)%stor = gw_pond_info(r)%stor - cell_recharge !remove from storage
							
							!solute mass in recharge water (added to cell)
							if(pond_volume> 0) then
							!no3
							if (gw_solute_flag == 1) then
								sol_conc = (gw_pond_info(r)%sol_mass(1)*1000.) / pond_volume !g/m3
								sol_mass = (sol_conc*cell_recharge)/1000. !kg/day
								if(sol_mass > gw_pond_info(r)%sol_mass(1)) then
								  sol_mass = gw_pond_info(r)%sol_mass(1)
								endif
								gw_pond_info(r)%sol_mass(1) = gw_pond_info(r)%sol_mass(1) - sol_mass !kg leached from pond
								rech_mass_cell(1) = sol_mass * 1000. !g
								rech_mass(1) = rech_mass(1) + sol_mass !add to total from pond
								!p
								sol_conc = (gw_pond_info(r)%sol_mass(2)*1000.) / pond_volume !g/m3
								sol_mass = (sol_conc*cell_recharge)/1000. !kg/day
								if(sol_mass > gw_pond_info(r)%sol_mass(2)) then
								  sol_mass = gw_pond_info(r)%sol_mass(2)
								endif
								gw_pond_info(r)%sol_mass(2) = gw_pond_info(r)%sol_mass(2) - sol_mass !kg leached from pond
								rech_mass_cell(2) = sol_mass * 1000. !g
								rech_mass(2) = rech_mass(2) + sol_mass !add to total from pond
								!salt
								sol_index = 2
								if (gwsol_salt == 1) then
									do isalt=1,cs_db%num_salts
									  sol_index = sol_index + 1  
									  sol_conc = (gw_pond_info(r)%sol_mass(sol_index)*1000.) / pond_volume !g/m3
										sol_mass = (sol_conc*cell_recharge)/1000. !kg/day
										if(sol_mass > gw_pond_info(r)%sol_mass(sol_index)) then
											sol_mass = gw_pond_info(r)%sol_mass(sol_index)
										endif
										gw_pond_info(r)%sol_mass(sol_index) = gw_pond_info(r)%sol_mass(sol_index) - sol_mass !kg leached from pond
										rech_mass_cell(sol_index) = sol_mass * 1000. !g
										rech_mass(sol_index) = rech_mass(sol_index) + sol_mass !add to total from pond  	
									enddo
								endif
								!constituents
								if (gwsol_cons == 1) then
									do ics=1,cs_db%num_cs
										sol_index = sol_index + 1  
									  sol_conc = (gw_pond_info(r)%sol_mass(sol_index)*1000.) / pond_volume !g/m3
										sol_mass = (sol_conc*cell_recharge)/1000. !kg/day
										if(sol_mass > gw_pond_info(r)%sol_mass(sol_index)) then
											sol_mass = gw_pond_info(r)%sol_mass(sol_index)
										endif
										gw_pond_info(r)%sol_mass(sol_index) = gw_pond_info(r)%sol_mass(sol_index) - sol_mass !kg leached from pond
										rech_mass_cell(sol_index) = sol_mass * 1000. !g
										rech_mass(sol_index) = rech_mass(sol_index) + sol_mass !add to total from pond  
									enddo
								endif
								!store for mass balance calculations (in gwflow_simulate)
								do s=1,gw_nsolute !loop through the solutes
									gwsol_ss(cell_id)%solute(s)%pond = rech_mass_cell(s) !g/day
									gwsol_ss_sum(cell_id)%solute(s)%pond = gwsol_ss_sum(cell_id)%solute(s)%pond + rech_mass_cell(s) !g/day
									gwsol_ss_sum_mo(cell_id)%solute(s)%pond = gwsol_ss_sum_mo(cell_id)%solute(s)%pond + rech_mass_cell(s) !g/day
								enddo
							endif
							endif !if pond has water

						endif
					enddo !go to next cell
			
					endif !check if recharge pond is in operation
					
					if(div_added < 0) then
					  dum = 10
					endif
					
					!recharge pond water balance - output
					write(out_pond_bal,100) time%yrc,time%mo,time%day,r,gw_pond_info(r)%area, &
					                                                    gw_pond_info(r)%stor, &
																															pond_rain, &
																															div_added, &
																															pond_evap, &
																															pond_recharge, &
																															gw_pond_info(r)%div, &
																															gw_pond_info(r)%div_uns
																															
					!recharge pond solute mass balance - output
					do s=1,gw_nsolute !loop through the solutes
					  write(out_pond_sol,101) time%yrc,time%mo,time%day,r,gw_pond_info(r)%area, &
					                                                    gw_pond_info(r)%stor, &
																															gwsol_nm(s), &
																															gw_pond_info(r)%sol_mass(s), &
																															div_mass(s), &
																															rech_mass(s)
					enddo
																																																									
10  	  enddo !go to next recharge pond
        
        !write out mass (kg) and concentration (g/m3) for each pond
				!no3
				write(out_pond_mass,102) time%yrc,time%mo,time%day,gwsol_nm(1),(gw_pond_info(r)%sol_mass(1),r=1,gw_npond)
				do r=1,gw_npond
					if(gw_pond_info(r)%stor > 0) then
						gw_pond_info(r)%sol_conc(1) = (gw_pond_info(r)%sol_mass(1)*1000.) / gw_pond_info(r)%stor
					else
						gw_pond_info(r)%sol_conc(1) = 0.
					endif
				enddo
				write(out_pond_conc,102) time%yrc,time%mo,time%day,gwsol_nm(1),(gw_pond_info(r)%sol_conc(1),r=1,gw_npond)
				!p
				write(out_pond_mass,102) time%yrc,time%mo,time%day,gwsol_nm(2),(gw_pond_info(r)%sol_mass(2),r=1,gw_npond)
				do r=1,gw_npond
					if(gw_pond_info(r)%stor > 0) then
						gw_pond_info(r)%sol_conc(2) = (gw_pond_info(r)%sol_mass(2)*1000.) / gw_pond_info(r)%stor
					else
						gw_pond_info(r)%sol_conc(2) = 0.
					endif
				enddo
				write(out_pond_conc,102) time%yrc,time%mo,time%day,gwsol_nm(2),(gw_pond_info(r)%sol_conc(2),r=1,gw_npond)
				!salt ions
				sol_index = 2
				if (gwsol_salt == 1) then
					do isalt=1,cs_db%num_salts
						sol_index = sol_index + 1
						write(out_pond_mass,102) time%yrc,time%mo,time%day,gwsol_nm(sol_index),(gw_pond_info(r)%sol_mass(sol_index),r=1,gw_npond)
						do r=1,gw_npond
							if(gw_pond_info(r)%stor > 0) then
								gw_pond_info(r)%sol_conc(sol_index) = (gw_pond_info(r)%sol_mass(sol_index)*1000.) / gw_pond_info(r)%stor
							else
								gw_pond_info(r)%sol_conc(sol_index) = 0.
							endif
						enddo
						write(out_pond_conc,102) time%yrc,time%mo,time%day,gwsol_nm(sol_index),(gw_pond_info(r)%sol_conc(sol_index),r=1,gw_npond)
					enddo
				endif
				!constituents
				if (gwsol_cons == 1) then
					do ics=1,cs_db%num_cs
						sol_index = sol_index + 1  
						write(out_pond_mass,102) time%yrc,time%mo,time%day,gwsol_nm(sol_index),(gw_pond_info(r)%sol_mass(sol_index),r=1,gw_npond)
						do r=1,gw_npond
							if(gw_pond_info(r)%stor > 0) then
								gw_pond_info(r)%sol_conc(sol_index) = (gw_pond_info(r)%sol_mass(sol_index)*1000.) / gw_pond_info(r)%stor
							else
								gw_pond_info(r)%sol_conc(sol_index) = 0.
							endif
						enddo
						write(out_pond_conc,102) time%yrc,time%mo,time%day,gwsol_nm(sol_index),(gw_pond_info(r)%sol_conc(sol_index),r=1,gw_npond)
					enddo
				endif


      endif !check if recharge pond seepage is active

			
100   format (4i8,10(e15.7)) 
101   format (4i8,e15.4,e15.4,5x,a10,e15.4,e15.4,e15.4)
102   format (3i8,5x,a10,5000(e15.4))


      return
      end subroutine gwflow_pond      