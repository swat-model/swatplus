      subroutine wet_cs(icmd, icon, ihru) !rtb cs

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes the wetland constituent mass balance
      
      use reservoir_data_module
      use reservoir_module
      use water_body_module
      use hydrograph_module, only : res, resz, ob, ht2, wbody, wet
      use hru_module, only : hru, wetqcs, wtspcs
      use constituent_mass_module
      use res_cs_module
      use climate_module
      use cs_data_module
      
      implicit none      
      
      integer, intent (in) :: icmd !command counter (incoming)
      integer, intent (in) :: ihru !HRU number (incoming)
      integer :: icon              !none          |counter (incoming)
      integer :: iwst              !none          |weather station number
      integer :: ics               !constituent counter 
      real    :: k_react           !1/day - first-order rate constant, affected by temperature
      real    :: v_settle          !m/day	- settling rate
      real    :: cs_mass_beg,cs_conc_beg,cs_mass_end,cs_conc_end
      real    :: cs_inflow,cs_outflow,cs_seep,cs_settle,cs_rctn,cs_prod
      real    :: seo4_convert      !kg            |mass of seo4 converted to seo3
      real    :: theta             !temperature factor for chemical reactions  
      real    :: mass_avail        !track available constituent mass in the wetland (kg)
      real    :: seep_mass				 !constituent mass lost via seepage (and added to soil profile) (kg/ha)
      

      !mass balance output (by HRU for wetlands): prepare by setting to 0
      do ics=1,cs_db%num_cs
        wetcs_d(ihru)%cs(ics)%inflow = 0.
        wetcs_d(ihru)%cs(ics)%outflow = 0.
        wetcs_d(ihru)%cs(ics)%seep = 0.
        wetcs_d(ihru)%cs(ics)%settle = 0.
        wetcs_d(ihru)%cs(ics)%rctn = 0.
        wetcs_d(ihru)%cs(ics)%irrig = 0.
        wetcs_d(ihru)%cs(ics)%mass = 0.
        wetcs_d(ihru)%cs(ics)%conc = 0.
      enddo
      
      !only proceed if the wetland has more than 1 m3 of water
      if (wet(ihru)%flo > 1.) then
      
        !loop through the constituents
        seo4_convert = 0.
        do ics=1,cs_db%num_cs
          
          !constituent mass and concentration at beginning of day
          cs_mass_beg = wet_water(ihru)%cs(ics) !kg
          if(wet(ihru)%flo > 0.) then
            cs_conc_beg = (cs_mass_beg * 1000.) / wet(ihru)%flo !g/m3
          else
            cs_conc_beg =	0.
          endif
          mass_avail = cs_mass_beg
          
          !constituent mass entering wetland (from surface runon)
          cs_inflow = obcs(icmd)%hin_sur(1)%cs(ics) !kg
          mass_avail = mass_avail + cs_inflow
          
          !constituent mass leaving wetland via stream outflow
          cs_outflow = (ht2%flo * cs_conc_beg) / 1000. !m3 * g/m3 = g --> kg
          if(cs_outflow > mass_avail) then
            cs_outflow = mass_avail !take remaining
          endif
          mass_avail = mass_avail - cs_outflow
          
          !constituent mass leaving wetland via seepage
          cs_seep = (wet_wat_d(ihru)%seep * cs_conc_beg) / 1000. !m3 * g/m3 = g --> kg
          if(cs_seep > mass_avail) then
            cs_seep = mass_avail !take remaining
          endif
          mass_avail = mass_avail - cs_seep
          
          !constituent mass settling to bottom of wetland
          if(ics == 1) then
            v_settle = res_cs_data(icon)%v_seo4
				  elseif(ics == 2) then
            v_settle = res_cs_data(icon)%v_seo3
				  elseif(ics == 3) then
            v_settle = res_cs_data(icon)%v_born
				  endif
          cs_settle = (cs_conc_beg/1000.) * v_settle * (wet_wat_d(ihru)%area_ha*10000.) !kg
          if(cs_settle > mass_avail) then
            cs_settle = mass_avail !take remaining
          endif
          mass_avail = mass_avail - cs_settle
          
          !constituent mass removed via first-order chemical reaction
          !(first-order rate constant dependent on temperature)
          iwst = ob(ihru)%wst
          if(ics == 1) then !seo4
            k_react =  theta(res_cs_data(icon)%k_seo4, res_cs_data(icon)%theta_seo4, wst(iwst)%weat%tave)  
				  elseif(ics == 2) then !seo3
            k_react =  theta(res_cs_data(icon)%k_seo3, res_cs_data(icon)%theta_seo3, wst(iwst)%weat%tave)  
          elseif(ics == 3) then !boron
            k_react =  theta(res_cs_data(icon)%k_born, res_cs_data(icon)%theta_born, wst(iwst)%weat%tave)  
          endif
          cs_rctn = (cs_conc_beg/1000.) * k_react * wet(ihru)%flo !kg
          if(cs_rctn > mass_avail) then
            cs_rctn = mass_avail !take remaining
          endif
          if(ics == 1) then !seo4 - save for seo3 production
            seo4_convert = cs_rctn
          endif
          mass_avail = mass_avail - cs_rctn
          
          !seo3 mass produced via seo4 reduction (seo4-->seo3)
          cs_prod = 0.
          if(ics == 2) then !seo3 (seo4 --> seo3)
            cs_prod = seo4_convert
            mass_avail = mass_avail + cs_prod
          endif
          
          !calculate new seo4 mass and concentration in wetland water at end of day
          cs_mass_end = cs_mass_beg + (cs_inflow - cs_outflow - cs_seep - cs_settle - cs_rctn + cs_prod) !kg
          if(wet(ihru)%flo > 0.) then
            cs_conc_end = (cs_mass_end * 1000.) / wet(ihru)%flo !g/m3
				  else
            cs_conc_end = 0.
          endif
          
          !store in arrays
          wet_water(ihru)%cs(ics) = cs_mass_end !kg
          wet_water(ihru)%csc(ics) = cs_conc_end !g/m3
          
          !store constituent balance terms (kg) for output
          wetcs_d(ihru)%cs(ics)%inflow = cs_inflow
          wetcs_d(ihru)%cs(ics)%outflow = cs_outflow !this is added to constituent mass in surface runoff (in cs_lch)
          wetcs_d(ihru)%cs(ics)%seep = cs_seep !this is input to the top layer in the soil profile (in cs_lch)
          wetcs_d(ihru)%cs(ics)%settle = cs_settle
          wetcs_d(ihru)%cs(ics)%rctn = cs_rctn
          wetcs_d(ihru)%cs(ics)%prod = cs_prod
          wetcs_d(ihru)%cs(ics)%mass = cs_mass_end
          wetcs_d(ihru)%cs(ics)%conc = cs_conc_end
          wetcs_d(ihru)%cs(1)%volm = wet(ihru)%flo
          
          !store salt outflow in runoff term (added to total runoff in hru_hyds)
          wetqcs(ihru,ics) = wetcs_d(ihru)%cs(ics)%outflow / hru(ihru)%area_ha !kg/ha
          
          !add seeped mass to the top layer of the soil profile
          seep_mass = wetcs_d(ihru)%cs(ics)%seep / hru(ihru)%area_ha !kg/ha
          cs_soil(ihru)%ly(1)%cs(ics) = cs_soil(ihru)%ly(1)%cs(ics) + seep_mass
          wtspcs(ihru,ics) = seep_mass
          
        enddo !go to next constituent
      
      endif !check for sufficient wetland volume
      
      return
      end subroutine wet_cs