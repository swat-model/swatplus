      subroutine res_cs(jres, icon, iob) !rtb cs

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes the reservoir constituent mass balance
      
      use reservoir_data_module
      use reservoir_module
      use water_body_module
      use hydrograph_module, only : res, resz, ob, ht2, wbody
      use constituent_mass_module
      use res_cs_module
      use climate_module
      use cs_data_module
      
      implicit none      
      
      integer, intent (in) :: iob
      integer :: jres              !reservoir number (incoming)
      integer :: icon              !none          |counter
      integer :: iwst              !none          |weather station number
      integer :: ics               !constituent counter
      real    :: cs_mass           !mass of constituent in reservoir water (kg)
      real    :: cs_mass_out       !mass of constituent leaving the reservoir via outflow (kg)
      real    :: cs_conc           !concentration of constituent in reservoir water (g/m3 = mg/L)
      integer :: icmd              !none  
      real    :: k_react           !1/day - first-order rate constant, affected by temperature 
      real    :: v_settle          !m/day	- settling rate
      real    :: cs_mass_beg,cs_conc_beg,cs_mass_end,cs_conc_end
      real    :: cs_inflow,cs_outflow,cs_seep,cs_settle,cs_rctn,cs_prod
      real    :: seo4_convert      !kg            |mass of seo4 converted to seo3
      real    :: theta             !temperature factor for chemical reactions  
      real    :: mass_avail        !track available constituent mass in the reservoir (kg)
      
      !object number
      icmd = res_ob(jres)%ob
      
      !mass balance output (by reservoir): prepare by setting to 0
      do ics=1,cs_db%num_cs
        rescs_d(jres)%cs(ics)%inflow = 0.
        rescs_d(jres)%cs(ics)%outflow = 0.
        rescs_d(jres)%cs(ics)%seep = 0.
        rescs_d(jres)%cs(ics)%settle = 0.
        rescs_d(jres)%cs(ics)%rctn = 0.
        rescs_d(jres)%cs(ics)%irrig = 0.
        rescs_d(jres)%cs(ics)%mass = 0.
        rescs_d(jres)%cs(ics)%conc = 0.
      enddo
      
      !only proceed if the reservoir has more than 1 m3 of water
      if (res(jres)%flo > 1.) then
      
        !loop through the constituents
        seo4_convert = 0.
        do ics=1,cs_db%num_cs
        
          !constituent mass and concentration at beginning of day
          cs_mass_beg = res_water(jres)%cs(ics) !kg
          cs_conc_beg = res_water(jres)%csc(ics) !g/m3
          mass_avail = cs_mass_beg
          
          !constituent mass entering reservoir
          cs_inflow = obcs(icmd)%hin(1)%cs(ics) !kg
          mass_avail = mass_avail + cs_inflow
          
          !constituent mass leaving reservoir via stream outflow
          cs_outflow = (ht2%flo * cs_conc_beg) / 1000. !m3 * g/m3 = g --> kg
          if(cs_outflow > mass_avail) then
            cs_outflow = mass_avail !take remaining
          endif
          mass_avail = mass_avail - cs_outflow
          
          !constituent mass leaving reservoir via seepage
          cs_seep = (res_wat_d(jres)%seep * cs_conc_beg) / 1000. !m3 * g/m3 = g --> kg
          if(cs_seep > mass_avail) then
            cs_seep = mass_avail !take remaining
          endif
          mass_avail = mass_avail - cs_seep
          
          !constituent mass settling to bottom of reservoir
          if(ics == 1) then
            v_settle = res_cs_data(icon)%v_seo4
					elseif(ics == 2) then
            v_settle = res_cs_data(icon)%v_seo3
					elseif(ics == 3) then
            v_settle = res_cs_data(icon)%v_born
					endif
          cs_settle = (cs_conc_beg/1000.) * v_settle * (res_wat_d(jres)%area_ha*10000.) !kg
          if(cs_settle > mass_avail) then
            cs_settle = mass_avail !take remaining
          endif
          mass_avail = mass_avail - cs_settle
          
          !constituent mass removed via first-order chemical reaction
          !(first-order rate constant dependent on temperature)
          iwst = ob(iob)%wst
          if(ics == 1) then !seo4
            k_react =  theta(res_cs_data(icon)%k_seo4, res_cs_data(icon)%theta_seo4, wst(iwst)%weat%tave)  
					elseif(ics == 2) then !seo3
            k_react =  theta(res_cs_data(icon)%k_seo3, res_cs_data(icon)%theta_seo3, wst(iwst)%weat%tave)  
          elseif(ics == 3) then !boron
            k_react =  theta(res_cs_data(icon)%k_born, res_cs_data(icon)%theta_born, wst(iwst)%weat%tave)  
          endif
          cs_rctn = (cs_conc_beg/1000.) * k_react * res(jres)%flo !kg
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
          
          !calculate new seo4 mass and concentration in reservoir water at end of day
          cs_mass_end = cs_mass_beg + (cs_inflow - cs_outflow - cs_seep - cs_settle - cs_rctn + cs_prod) !kg
          cs_conc_end = (cs_mass_end * 1000.) / res(jres)%flo !g/m3
          
          !store in arrays
          res_water(jres)%cs(ics) = cs_mass_end !kg
          res_water(jres)%csc(ics) = cs_conc_end !g/m3
          
          !store constituent balance terms (kg) for output
          rescs_d(jres)%cs(ics)%inflow = cs_inflow
          rescs_d(jres)%cs(ics)%outflow = cs_outflow
          rescs_d(jres)%cs(ics)%seep = cs_seep
          rescs_d(jres)%cs(ics)%settle = cs_settle
          rescs_d(jres)%cs(ics)%rctn = cs_rctn
          rescs_d(jres)%cs(ics)%prod = cs_prod
          rescs_d(jres)%cs(ics)%mass = cs_mass_end
          rescs_d(jres)%cs(ics)%conc = cs_conc_end
          rescs_d(jres)%cs(ics)%volm = res(jres)%flo
          
          !set value for outflow (to connect with other objects)
          obcs(icmd)%hd(1)%cs(ics) = cs_outflow 
          
        enddo !go to next constituent
        
      endif
      
      
      return
      end subroutine res_cs