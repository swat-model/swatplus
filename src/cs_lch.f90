      subroutine cs_lch !rtb cs
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine simulates the loss of constituent mass via surface runoff, 
!!    lateral flow, tile flow, and percolation out of the profile


!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    jj          |none          |counter (soil layers)
!!    cocs        |kg cs/mm      |concentration of constituent in solution
!!    cosurfcs    |kg cs/mm      |concentration of constituent in surface runoff
!!    perccslyr   |kg cs/ha      |constituent mass leached to next lower layer with percolation
!!    gwupcslyr   |kg cs/ha      |constituent mass added to soil profile from groundwater
!!    ssfcslyr    |kg cs/ha      |constituent mass transported in lateral flow from layer
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

      integer :: j,jj,gw_soil_flag
      integer :: ics
      integer :: sol_index
      real :: cocs,cosurfcs,perccslyr(3),ssfcslyr,vcs
      real :: hru_area_m2,water_volume,cs_mass_kg
      real :: ro_mass,sro,vv,ww


      j = 0
      j = ihru

      
      !rtb gwflow: add constituent mass transferred to soil profile from the aquifer; store for cs balance
      if(gw_soil_flag == 1 .and. gw_solute_flag == 1) then
        do jj = 1, soil(j)%nly
          sol_index = 2 + cs_db%num_salts
          do ics=1,cs_db%num_cs
            sol_index = sol_index + 1
            !add mass to soil layer (hru_soil calculated in gwflow_soil)
            cs_soil(j)%ly(jj)%cs(ics) = cs_soil(j)%ly(jj)%cs(ics) + &
                                        hru_soil(j,jj,sol_index) !kg/ha
            !store for mass balance output  
            gwupcs(j,ics) = gwupcs(j,ics) + hru_soil(j,jj,sol_index) !kg/ha
          enddo
		enddo
      endif
      
      perccslyr = 0.

      !loop through the constituents
      sol_index = 2 + cs_db%num_salts
      do ics=1,cs_db%num_cs
        sol_index = sol_index + 1
      
        !loop through the soil layers
        do jj = 1,soil(j)%nly
          
          !! add constituent mass leached from layer above
          cs_soil(j)%ly(jj)%cs(ics) = cs_soil(j)%ly(jj)%cs(ics)      &
                                    + perccslyr(ics)
          
          !! determine concentration of constituent in mobile water
          sro = 0.
          vv = 0.
          vcs = 0.
          cocs = 0.
          if (jj == 1) then
            sro = surfq(j)
          else
            sro = 0.
          end if
          vv = soil(j)%ly(jj)%prk + sro + soil(j)%ly(jj)%flat + 1.e-10
          if (hru(j)%lumv%ldrain == jj) vv = vv + qtile
          ww = -vv / ((1. - soil(j)%anion_excl) * soil(j)%phys(jj)%ul)
          vcs = cs_soil(j)%ly(jj)%cs(ics) * (1. - Exp(ww))
          cocs = Max(vcs / vv, 0.)

          !! calculate constituent mass in surface runoff
          cosurfcs = bsn_prm%nperco * cocs
          if (jj == 1) then
            !surface runoff
            ro_mass = surfq(j) * cosurfcs
            ro_mass = Min(ro_mass, cs_soil(j)%ly(jj)%cs(ics))
            cs_soil(j)%ly(jj)%cs(ics) = cs_soil(j)%ly(jj)%cs(ics) - ro_mass
				surqcs(j,ics) = ro_mass
          endif
          
          !Daniel 1/2012    
          !! calculate constituent mass in tile flow 
          if (hru(j)%lumv%ldrain == jj) then
            tilecs(j,ics) = cocs * qtile
            tilecs(j,ics) = Min(tilecs(j,ics),cs_soil(j)%ly(jj)%cs(ics))
            cs_soil(j)%ly(jj)%cs(ics) = cs_soil(j)%ly(jj)%cs(ics) - tilecs(j,ics)
          endif
          !Daniel 1/2012 

          !! calculate constituent masss in lateral flow
          ssfcslyr = 0.
          if (jj == 1) then
            ssfcslyr = cosurfcs * soil(j)%ly(jj)%flat
          else
            ssfcslyr = cocs * soil(j)%ly(jj)%flat
          end if
          ssfcslyr = Min(ssfcslyr, cs_soil(j)%ly(jj)%cs(ics))
          latqcs(j,ics) = latqcs(j,ics) + ssfcslyr
          cs_soil(j)%ly(jj)%cs(ics) = cs_soil(j)%ly(jj)%cs(ics) - ssfcslyr

          !! calculate constituent mass in percolate water
          perccslyr(ics) = 0.
          perccslyr(ics) = cocs * soil(j)%ly(jj)%prk
          perccslyr(ics) = Min(perccslyr(ics), cs_soil(j)%ly(jj)%cs(ics))
          cs_soil(j)%ly(jj)%cs(ics) = cs_soil(j)%ly(jj)%cs(ics) - perccslyr(ics)
        
        enddo !go to next soil layer

        !! calculate constituent mass leaching from soil profile
        perccs(j,ics) = perccslyr(ics)
        
        !! if gwflow: store in array for use input_file_module gwflow_rech
        if(bsn_cc%gwflow == 1 .and. gw_solute_flag == 1) then
          gwflow_percsol(j,sol_index) = perccslyr(ics)
        endif
        
      end do !go to next constituent


      !! calculate concentration (mg/L) of constituent in each soil layer
      do jj = 1,soil(j)%nly
        !loop through the constituents in the layer
        do ics=1,cs_db%num_cs
          hru_area_m2 = hru(j)%area_ha * 10000. !ha --> m2
          water_volume = (soil(j)%phys(jj)%st/1000.) * hru_area_m2
          if(cs_soil(j)%ly(jj)%cs(ics).lt.0) then
			  cs_soil(j)%ly(jj)%cs(ics) = 0.
          endif
          cs_mass_kg = cs_soil(j)%ly(jj)%cs(ics) * hru(j)%area_ha !kg
          !calculate concentration in mg/L
          if(water_volume.gt.0) then
            cs_soil(j)%ly(jj)%csc(ics) = (cs_mass_kg * 1000.) / water_volume !g/m3 = mg/L
          else
            cs_soil(j)%ly(jj)%csc(ics) = 0.
          endif
        enddo
      enddo

      
      return
      end !cs_lch