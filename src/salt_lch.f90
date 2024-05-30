      subroutine salt_lch
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine simulates the loss of salt via surface runoff, 
!!    lateral flow, tile flow, and percolation out of the profile

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    flat(:,:)   |mm H2O        |lateral flow in soil layer on current day
!!    ihru        |none          |HRU number
!!    sol_salt(:,)|kg Salt/ha    |amount of salt stored in solution in each soil layer !rtb salt
!!    sol_prk(:,:)|mm H2O        |percolation from soil layer on current day
!!    sol_ul(:,:) |mm H2O        |amount of water held in the soil layer at
!!                               |saturation
!!    surfq(:)    |mm H2O        |surface runoff generated on day in HRU
!!    sol_csalt   |mg Salt/L     |concentration of salt in solution in each layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    latqsalt(:)  |kg Salt/ha   |amount of salt transported with lateral flow
!!    percsalt(:)  |kg Salt/ha   |amount of salt percolating past bottom of soil profile
!!    gwupsalt(:)  |kg Salt/ha   |amount of salt added to soil profile via groundwater
!!    sol_salt(:,:)|kg Salt/ha   |amount of salt stored in solution in each soil layer
!!    surqsalt(:)  |kg Salt/ha   |amount of salt transported with surface runoff
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    jj          |none          |counter (soil layers)
!!    cosalt      |kg Salt/mm    |concentration of salt in solution
!!    cosurfsalt  |kg Salt/mm    |concentration of salt in surface runoff
!!    percsaltlyr |kg Salt/ha    |salt leached to next lower layer with percolation
!!    ssfsaltlyr  |kg Salt/ha    |salt transported in later flow from layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Max, Min

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use hru_module
      use basin_module
      use constituent_mass_module
      use soil_module
      use basin_module
      use gwflow_module, only : gwflow_percsol,gw_solute_flag,hru_soil
      
      implicit none
      
      integer :: j,jj
      integer :: isalt
      integer :: sol_index,gw_soil_flag
      real :: cosalt,cosurfsalt,percsaltlyr(8),ssfsaltlyr,vsalt
      real :: hru_area_m2,water_volume,salt_mass_kg
      real :: ro_mass,wet_ro_mass,sro,vv,ww

      j = 0
      j = ihru
     
      !rtb gwflow: add salt ion mass transferred to soil profile from the aquifer; store for salt balance
      if (gw_soil_flag == 1 .and. gw_solute_flag == 1) then
        do jj = 1, soil(j)%nly
          sol_index = 2
          do isalt=1,cs_db%num_salts
            sol_index = sol_index + 1
            !add to the soil layer (hru_soil calculated in gwflow_soil)
            cs_soil(j)%ly(jj)%salt(isalt) = cs_soil(j)%ly(jj)%salt(isalt) + &
                                            hru_soil(j,jj,sol_index) !kg/ha
            !store for mass balance output
            gwupsalt(j,isalt) = gwupsalt(j,isalt) + hru_soil(j,jj,sol_index) !kg/ha
          enddo
        enddo
      endif
      
      percsaltlyr = 0.

          
      !loop through the salt ions
      sol_index = 2
      do isalt=1,cs_db%num_salts
        sol_index = sol_index + 1  
      
        !loop through the soil layers
        do jj = 1,soil(j)%nly
          
          !! add salt leached from layer above
          cs_soil(j)%ly(jj)%salt(isalt) = cs_soil(j)%ly(jj)%salt(isalt) + percsaltlyr(isalt)
	    if (cs_soil(j)%ly(jj)%salt(isalt) < 1.e-6) then
            cs_soil(j)%ly(jj)%salt(isalt) = 0.0
          endif

          !! determine concentration of salt in mobile water
          sro = 0.
          vv = 0.
          vsalt = 0.
          cosalt = 0.
          if (jj == 1) then
            sro = surfq(j)
          else
            sro = 0.
          end if
          vv = soil(j)%ly(jj)%prk + sro + soil(j)%ly(jj)%flat + 1.e-10
          if (hru(j)%lumv%ldrain == jj) vv = vv + qtile
          ww = -vv / ((1. - soil(j)%anion_excl) * soil(j)%phys(jj)%ul)
          vsalt = cs_soil(j)%ly(jj)%salt(isalt) * (1. - Exp(ww))
          cosalt = Max(vsalt / vv, 0.)

          !! calculate salt ion mass in surface runoff
          cosurfsalt = bsn_prm%nperco * cosalt
          if (jj == 1) then
            ro_mass = surfq(j) * cosurfsalt
            ro_mass = Min(ro_mass, cs_soil(j)%ly(jj)%salt(isalt))
            cs_soil(j)%ly(jj)%salt(isalt) = cs_soil(j)%ly(jj)%salt(isalt) - ro_mass
            surqsalt(j,isalt) = ro_mass 
          endif
          
          !Daniel 1/2012    
          !! calculate salt in tile flow 
          if (hru(j)%lumv%ldrain == jj) then
            tilesalt(j,isalt) = cosalt * qtile
            tilesalt(j,isalt) = Min(tilesalt(j,isalt), cs_soil(j)%ly(jj)%salt(isalt))
            cs_soil(j)%ly(jj)%salt(isalt) = cs_soil(j)%ly(jj)%salt(isalt) - tilesalt(j,isalt)
          endif
          !Daniel 1/2012 

          !! calculate salt in lateral flow
          ssfsaltlyr = 0.
          if (jj == 1) then
            ssfsaltlyr = cosurfsalt * soil(j)%ly(jj)%flat
          else
            ssfsaltlyr = cosalt * soil(j)%ly(jj)%flat
          end if
          ssfsaltlyr = Min(ssfsaltlyr, cs_soil(j)%ly(jj)%salt(isalt))
          latqsalt(j,isalt) = latqsalt(j,isalt) + ssfsaltlyr
          cs_soil(j)%ly(jj)%salt(isalt)=cs_soil(j)%ly(jj)%salt(isalt) - ssfsaltlyr

          !! calculate salt in percolate water
          percsaltlyr(isalt) = 0.
          percsaltlyr(isalt) = cosalt * soil(j)%ly(jj)%prk
          percsaltlyr(isalt) = Min(percsaltlyr(isalt), cs_soil(j)%ly(jj)%salt(isalt))
          cs_soil(j)%ly(jj)%salt(isalt)=cs_soil(j)%ly(jj)%salt(isalt) - percsaltlyr(isalt)
        
        enddo !go to next soil layer

        !! calculate salt leaching from soil profile
        percsalt(j,isalt) = percsaltlyr(isalt)

        !! if gwflow: store in array for use input_file_module gwflow_rech
        if(bsn_cc%gwflow == 1 .and. gw_solute_flag == 1) then
          gwflow_percsol(j,sol_index) = percsaltlyr(isalt)
        endif
        
      end do !go to next salt ion


      !! calculate concentration (mg/L) of salt in each soil layer
      do jj = 1,soil(j)%nly
        !loop through the salt ions in the layer
        do isalt=1,cs_db%num_salts
          hru_area_m2 = hru(j)%area_ha * 10000. !ha --> m2
          water_volume = (soil(j)%phys(jj)%st/1000.) * hru_area_m2
          if(cs_soil(j)%ly(jj)%salt(isalt).lt.0) then
			  cs_soil(j)%ly(jj)%salt(isalt) = 0.
          endif
          salt_mass_kg = cs_soil(j)%ly(jj)%salt(isalt) * hru(j)%area_ha !kg
          !calculate concentration in mg/L
          if(water_volume.gt.0) then
            cs_soil(j)%ly(jj)%saltc(isalt) = (salt_mass_kg * 1000.) / water_volume !g/m3 = mg/L
          else
            cs_soil(j)%ly(jj)%saltc(isalt) = 0.
			endif
        enddo
      enddo

      return
      end