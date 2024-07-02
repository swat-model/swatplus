      subroutine res_salt (jres)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes the reservoir salt ion balance  
      
      use reservoir_data_module
      use reservoir_module
      use water_body_module
      use hydrograph_module, only : res, ht2
      use constituent_mass_module
      use res_salt_module
      
      implicit none      
      
      integer :: jres              !reservoir number (incoming)
      integer :: isalt             !salt ion counter
      integer :: icmd              !none  
      real    :: salt_mass_beg,salt_conc_beg,salt_mass_end,salt_conc_end
      real    :: salt_inflow,salt_outflow,salt_seep
      real    :: mass_avail        !track available salt ion mass in the reservoir (kg)
      
      
      !object number
      icmd = res_ob(jres)%ob
      
      !prepare by setting to 0
      do isalt=1,cs_db%num_salts
        ressalt_d(jres)%salt(isalt)%inflow = 0.
        ressalt_d(jres)%salt(isalt)%outflow = 0.
        ressalt_d(jres)%salt(isalt)%seep = 0.
        ressalt_d(jres)%salt(isalt)%mass = 0.
        ressalt_d(jres)%salt(isalt)%conc = 0.
      enddo
      
      !only proceed if the reservoir has more than 1 m3 of water
      if (res(jres)%flo > 1.) then
        
        !loop through the salt ions
        do isalt=1,cs_db%num_salts
          
          !salt mass and concentration at beginning of day
          salt_mass_beg = res_water(jres)%salt(isalt) !kg
          salt_conc_beg = res_water(jres)%saltc(isalt) !g/m3
          mass_avail = salt_mass_beg
          
          !salt mass entering reservoir
          salt_inflow = obcs(icmd)%hin(1)%salt(isalt) !kg
          mass_avail = mass_avail + salt_inflow
          
          !salt mass leaving reservoir via stream outflow
          salt_outflow = (ht2%flo * salt_conc_beg) / 1000. !m3 * g/m3 = g --> kg
          if(salt_outflow > mass_avail) then
            salt_outflow = mass_avail !take remaining
          endif
          mass_avail = mass_avail - salt_outflow
          
          !salt mass leaving reservoir via seepage
          salt_seep = (res_wat_d(jres)%seep * salt_conc_beg) / 1000. !m3 * g/m3 = g --> kg
          if(salt_seep > mass_avail) then
            salt_seep = mass_avail !take remaining
          endif
          mass_avail = mass_avail - salt_seep
          
          !calculate new salt mass and concentration in reservoir water at end of day
          salt_mass_end = salt_mass_beg + (salt_inflow - salt_outflow - salt_seep) !kg
          salt_conc_end = (salt_mass_end * 1000.) / res(jres)%flo !g/m3
          
          !store in arrays
          res_water(jres)%salt(isalt) = salt_mass_end !kg
          res_water(jres)%saltc(isalt) = salt_conc_end !g/m3
          
          !store salt balance terms (kg) for output
          ressalt_d(jres)%salt(isalt)%inflow = salt_inflow
          ressalt_d(jres)%salt(isalt)%outflow = salt_outflow
          ressalt_d(jres)%salt(isalt)%seep = salt_seep
          ressalt_d(jres)%salt(isalt)%mass = salt_mass_end
          ressalt_d(jres)%salt(isalt)%conc = salt_conc_end
          ressalt_d(jres)%salt(isalt)%volm = res(jres)%flo
          
          !set value for outflow (to connect with other objects)
          obcs(icmd)%hd(1)%salt(isalt) = salt_outflow
          
        enddo !go to next salt ion
        
      endif
         
      return
      end subroutine res_salt