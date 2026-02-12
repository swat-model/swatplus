      subroutine salt_lch
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine simulates the loss of salt via surface runoff, 
!!    lateral flow, tile flow, and percolation out of the profile

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    surqsalt(:)  |kg Salt/ha   |amount of salt transported with surface runoff
!!    tilesalt(:)  |kg Salt/ha   |amount of salt transported in tile drainage
!!    latqsalt(:)  |kg Salt/ha   |amount of salt transported with lateral flow	
!!    percsalt(:)  |kg Salt/ha   |amount of salt percolating past bottom of soil profile
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

      use hru_module
      use basin_module
      use constituent_mass_module
      use soil_module
      use gwflow_module, only : gwflow_percsol,gw_solute_flag
      
      implicit none
      
      integer :: j = 0              !               |hru
	  integer :: jj = 0             !               |soil layer counter
      integer :: isalt = 0          !               |salt ion counter
      integer :: dum = 0
      integer :: sol_index = 0      !               |solute index counter
      real :: hru_area_m2 = 0.      !m2             |hru area
      !layer salt ion concentrations
      real :: lay_conc(50,8) = 0.	!g/m3           |salt ion concentration in each soil layer
      real :: water_mm = 0.         !mm				|water in soil layer (including wilting point water)
      real :: water_volume = 0.     !m3             |volume of water in soil layer
      real :: salt_mass_kg = 0.     !kg             |salt ion mass in soil layer
      !salt in surface runoff
      real :: surf_conc = 0.        !g/m3           |salt ion concentration in surface runoff
      real :: ro_vol = 0.           !m3             |volume of runoff water
      real :: ro_mass = 0.          !kg/ha          |salt ion mass in runoff water
      !salt in tile drainage
      real :: tile_vol = 0.         !m3             |volume of tile drainage
      real :: tile_mass = 0.        !kg/ha			|salt ion mass in tile drainage
      !salt in soil lateral flow
      real :: lat_vol = 0.          !m3             |volume of soil lateral flow
      real :: lat_mass = 0.         !kg/ha          |salt ion mass in soil lateral flow
      !salt in percolate water
      real :: perc_vol = 0.         !m3             |volume of percolation
      real :: perc_mass = 0.        !kg/ha          |salt ion mass in percolation
      real :: percsaltlyr = 0.      !kg/ha          |salt ion mass in percolation

      !hru
      j = ihru

			!hru area (m2)
			hru_area_m2 = hru(j)%area_ha * 10000. !ha --> m2
	
	
			!zero out salt flux arrays --------------------------------------------------------------------------------------
			do isalt=1,cs_db%num_salts
				surqsalt(j,isalt) = 0.
				tilesalt(j,isalt) = 0.
				percsalt(j,isalt) = 0.
				latqsalt(j,isalt) = 0.    
			enddo
	
	
			!calculate salt ion concentration in each soil layer ------------------------------------------------------------
			lay_conc = 0.
			do jj=1,soil(j)%nly
				!water volume in soil layer
				if(soil(j)%phys(jj)%st > 1e-6) then
					water_mm = soil(j)%phys(jj)%st + soil(j)%phys(jj)%wpmm  !total soil water
					water_volume = (water_mm/1000.) * hru_area_m2 !m3  
					!loop through the salt ions
					do isalt=1,cs_db%num_salts
						salt_mass_kg = cs_soil(j)%ly(jj)%salt(isalt) * hru(j)%area_ha !kg
						lay_conc(jj,isalt) = (salt_mass_kg * 1000.) / water_volume !g/m3 = mg/L
					enddo
				endif  
			enddo !next soil layer
	
	
			!calculate salt mass in surface runoff --------------------------------------------------------------------------
			if(soil(j)%phys(1)%st > 1e-6) then
				water_mm = soil(j)%phys(1)%st + soil(j)%phys(1)%wpmm  !total soil water
				water_volume = (water_mm/1000.) * hru_area_m2 !m3  
				do isalt=1,cs_db%num_salts
					!salt ion concentration in top soil layer (g/m3)
					salt_mass_kg = cs_soil(j)%ly(1)%salt(isalt) * hru(j)%area_ha !kg
					surf_conc = (salt_mass_kg * 1000.) / water_volume !g/m3 = mg/L
					!volume of surface runoff (m3)
					ro_vol = (surfq(j) / 1000.) * hru_area_m2
					!salt ion mass in surface runoff (kg/ha)
							ro_mass = (ro_vol * surf_conc) / 1000. / hru(j)%area_ha
							if(ro_mass > cs_soil(j)%ly(1)%salt(isalt)) then
						ro_mass = cs_soil(j)%ly(1)%salt(isalt)
					endif
					cs_soil(j)%ly(1)%salt(isalt) = cs_soil(j)%ly(1)%salt(isalt) - ro_mass
					surqsalt(j,isalt) = ro_mass 
				enddo
			endif
	
	    !calculate salt mass in percolation, tile outflow, and soil lateral flow ----------------------------------------
			sol_index = 2
      do isalt=1,cs_db%num_salts
        sol_index = sol_index + 1  
			  
        !loop through the soil layers
				percsaltlyr = 0.
        do jj = 1,soil(j)%nly
          
          !add salt leached from layer above
          cs_soil(j)%ly(jj)%salt(isalt) = cs_soil(j)%ly(jj)%salt(isalt) + percsaltlyr

					!calculate salt in tile flow
          if(hru(j)%lumv%ldrain == jj) then !drainage occurs in the layer
            tile_vol = (qtile / 1000.) * hru_area_m2 !m3 
						tile_mass = (tile_vol * lay_conc(jj,isalt)) / 1000. / hru(j)%area_ha !kg/ha
						if(tile_mass > cs_soil(j)%ly(jj)%salt(isalt)) then
							tile_mass = cs_soil(j)%ly(jj)%salt(isalt)
						endif
						cs_soil(j)%ly(jj)%salt(isalt) = cs_soil(j)%ly(jj)%salt(isalt) - tile_mass
						tilesalt(j,isalt) = tile_mass 
          endif

					!calculate salt in soil lateral flow
					lat_vol =  (soil(j)%ly(jj)%flat / 1000.) * hru_area_m2 !m3 
					lat_mass = (lat_vol * lay_conc(jj,isalt)) / 1000. / hru(j)%area_ha !kg/ha
					if(lat_mass > cs_soil(j)%ly(jj)%salt(isalt)) then
						lat_mass = cs_soil(j)%ly(jj)%salt(isalt)
					endif
					cs_soil(j)%ly(jj)%salt(isalt) = cs_soil(j)%ly(jj)%salt(isalt) - lat_mass
					latqsalt(j,isalt) = latqsalt(j,isalt) + lat_mass !add up for the soil profile
			
					!calculate salt in percolate water
					perc_vol =  (soil(j)%ly(jj)%prk / 1000.) * hru_area_m2 !m3 
					perc_mass = (perc_vol * lay_conc(jj,isalt)) / 1000. / hru(j)%area_ha !kg/ha
					if(perc_mass > cs_soil(j)%ly(jj)%salt(isalt)) then
						perc_mass = cs_soil(j)%ly(jj)%salt(isalt)
					endif
					cs_soil(j)%ly(jj)%salt(isalt) = cs_soil(j)%ly(jj)%salt(isalt) - perc_mass
					percsaltlyr = perc_mass
			
        enddo !go to next soil layer

        !salt leaching from soil profile (i.e., bottom layer)
        percsalt(j,isalt) = percsaltlyr

				!! if gwflow: store in array for use in gwflow_rech
        if(bsn_cc%gwflow == 1 .and. gw_solute_flag == 1) then
          gwflow_percsol(j,sol_index) = percsaltlyr
        endif
				
      enddo !go to next salt ion


      !! calculate concentration (g/m3) of salt in each soil layer ----------------------------------------------------
      do jj = 1,soil(j)%nly
        !loop through the salt ions in the layer
        do isalt=1,cs_db%num_salts
          hru_area_m2 = hru(j)%area_ha * 10000. !ha --> m2
          water_mm = soil(j)%phys(jj)%st + soil(j)%phys(jj)%wpmm  !total soil water
					water_volume = (water_mm/1000.) * hru_area_m2
          if(cs_soil(j)%ly(jj)%salt(isalt).lt.0) then
						cs_soil(j)%ly(jj)%salt(isalt) = 0.
          endif
          salt_mass_kg = cs_soil(j)%ly(jj)%salt(isalt) * hru(j)%area_ha !kg
          !calculate concentration in g/m3
          if(water_volume.gt.0) then
						if(soil(j)%phys(jj)%st > 1e-6) then
							cs_soil(j)%ly(jj)%saltc(isalt) = (salt_mass_kg * 1000.) / water_volume !g/m3 = mg/L
						else
							cs_soil(j)%ly(jj)%saltc(isalt) = 0.
						endif
          else
            cs_soil(j)%ly(jj)%saltc(isalt) = 0.
          endif
        enddo
      enddo

      
      return
      end
			