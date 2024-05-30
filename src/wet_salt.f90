      subroutine wet_salt(icmd,ihru) !rtb salt

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes the wetland salt ion mass balance
      
      use reservoir_data_module
      use reservoir_module
      use water_body_module
      use hydrograph_module, only : res, resz, ob, ht2, wbody, wet
      use hru_module, only : hru, wetqsalt, wtspsalt
      use constituent_mass_module
      use res_salt_module
      use climate_module
      
      implicit none      
      
      integer, intent (in) :: icmd !command counter (incoming)
      integer, intent (in) :: ihru !HRU number (incoming)
      integer :: iwst              !none          |weather station number
      integer :: isalt             !salt ion counter
      integer :: dum
      real    :: salt_mass_beg,salt_conc_beg,salt_mass_end,salt_conc_end
      real    :: salt_inflow,salt_outflow,salt_seep
      real    :: mass_avail        !track available salt ion mass in the wetland (kg)
      real    :: seep_mass				 !salt ion mass lost via seepage (and added to soil profile) (kg/ha)
      
      !mass balance output (by HRU for wetlands): prepare by setting to 0
      do isalt=1,cs_db%num_salts
        wetsalt_d(ihru)%salt(isalt)%inflow = 0.
        wetsalt_d(ihru)%salt(isalt)%outflow = 0.
        wetsalt_d(ihru)%salt(isalt)%seep = 0.
        wetsalt_d(ihru)%salt(isalt)%mass = 0.
        wetsalt_d(ihru)%salt(isalt)%conc = 0.
      enddo
      
      !loop through the salt ions
      do isalt=1,cs_db%num_salts
          
        !salt ion mass and concentration at beginning of day
        salt_mass_beg = wet_water(ihru)%salt(isalt) !kg
        if(wet(ihru)%flo > 0.) then
          salt_conc_beg = (salt_mass_beg * 1000.) / wet(ihru)%flo !g/m3
				else
          salt_conc_beg = 0.
        endif
        mass_avail = salt_mass_beg
          
        !salt ion mass entering wetland (from surface runon)
        salt_inflow = obcs(icmd)%hin_sur(1)%salt(isalt) !kg
        mass_avail = mass_avail + salt_inflow
          
        !salt ion mass leaving wetland via stream outflow
        salt_outflow = (ht2%flo * salt_conc_beg) / 1000. !m3 * g/m3 = g --> kg
        if(salt_outflow > mass_avail) then
          salt_outflow = mass_avail !take remaining
        endif
        mass_avail = mass_avail - salt_outflow
          
        !salt ion mass leaving wetland via seepage
        salt_seep = (wet_wat_d(ihru)%seep * salt_conc_beg) / 1000. !m3 * g/m3 = g --> kg
        if(salt_seep > mass_avail) then
          salt_seep = mass_avail !take remaining
        endif
        mass_avail = mass_avail - salt_seep
          
        !calculate new salt ion mass and concentration in wetland water at end of day
        salt_mass_end = salt_mass_beg + (salt_inflow - salt_outflow - salt_seep) !kg
        if(wet(ihru)%flo > 0.) then
          salt_conc_end = (salt_mass_end * 1000.) / wet(ihru)%flo !g/m3
				else
          salt_conc_end = 0.
        endif
          
        !store in arrays
        wet_water(ihru)%salt(isalt) = salt_mass_end !kg
        wet_water(ihru)%saltc(isalt) = salt_conc_end !g/m3
          
        !store salt balance terms (kg) for output
        wetsalt_d(ihru)%salt(isalt)%inflow = salt_inflow
        wetsalt_d(ihru)%salt(isalt)%outflow = salt_outflow
        wetsalt_d(ihru)%salt(isalt)%seep = salt_seep
        wetsalt_d(ihru)%salt(isalt)%mass = salt_mass_end
        wetsalt_d(ihru)%salt(isalt)%conc = salt_conc_end
        wetsalt_d(ihru)%salt(isalt)%volm = wet(ihru)%flo
          
        !store salt outflow in runoff term (added to total runoff in hru_hyds)
        wetqsalt(ihru,isalt) = wetsalt_d(ihru)%salt(isalt)%outflow / hru(ihru)%area_ha !kg/ha
          
        !add seeped mass to the top layer of the soil profile; store for mass balance output
        seep_mass = wetsalt_d(ihru)%salt(isalt)%seep / hru(ihru)%area_ha !kg/ha
        cs_soil(ihru)%ly(1)%salt(isalt) = cs_soil(ihru)%ly(1)%salt(isalt) + seep_mass
        wtspsalt(ihru,isalt) = seep_mass
          
      enddo !go to next salt ion
     
      return
      end subroutine wet_salt