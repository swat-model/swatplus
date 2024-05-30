      subroutine cs_sorb_hru !rtb cs
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine updates constituent concentrations based on sorption in the soil profile

      use hru_module, only : hru,ihru
      use soil_module
      use organic_mineral_mass_module
      use constituent_mass_module
      use cs_module
      use cs_data_module

      implicit none
      integer :: j,jj
      real :: cseo4,cseo3,cborn,ccseo4,ccseo3,ccborn,&        
          hru_area_m2,water_volume,&        
          mass_seo4_sol,mass_seo3_sol,mass_born_sol,&        
          mass_seo4_sorb,mass_seo3_sorb,mass_born_sorb,&				
          sol_thick,volume,&        
          ratio,mass_total,val_num,val_den,val, &        
          cseo4_new,ccseo4_new,&        
          cseo3_new,ccseo3_new,&        
          cborn_new,ccborn_new,&        
          soil_volume,soil_mass
      real :: sorbed_seo4,sorbed_seo3,sorbed_born
      real :: mass_seo4_before,mass_seo4_after,&        
          mass_seo3_before,mass_seo3_after,&        
          mass_born_before,mass_born_after
      
      
      !hru ID
      j = ihru

      !area of the HRU in m2
      hru_area_m2 = hru(j)%area_ha * 10000.

      !first, convert sorbed concentration of kg/ha to mg/kg ----------------------------------------------------------
      do jj = 1,soil(j)%nly
        
        !get thickness of the soil layer
        sol_thick = soil(j)%phys(jj)%thick

        !get the mass (kg) of the soil in the current layer
        soil_volume = hru_area_m2 * (sol_thick/1000.) !m3 of soil
        soil_mass = soil_volume * (soil(j)%phys(jj)%bd*1000.) !kg of soil
        
        !get the mass (mg) of Se in the current layer
        sorbed_seo4 = cs_soil(j)%ly(jj)%cs_sorb(1) !kg/ha
        sorbed_seo3 = cs_soil(j)%ly(jj)%cs_sorb(2) !kg/ha
        sorbed_born = cs_soil(j)%ly(jj)%cs_sorb(3) !kg/ha
        mass_seo4_sorb = sorbed_seo4 * 1.e6 * hru(j)%area_ha !mg
        mass_seo3_sorb = sorbed_seo3 * 1.e6 * hru(j)%area_ha !mg
        mass_born_sorb = sorbed_born * 1.e6 * hru(j)%area_ha !mg
        
        !compute mass concentration (mg of Se per kg of soil)
        cs_soil(j)%ly(jj)%csc_sorb(1) = mass_seo4_sorb / soil_mass !mg/kg
        cs_soil(j)%ly(jj)%csc_sorb(2) = mass_seo3_sorb / soil_mass !mg/kg
        cs_soil(j)%ly(jj)%csc_sorb(3) = mass_born_sorb / soil_mass !mg/kg

      enddo
      

      !calculate mass transfer of selenium, for each layer ------------------------------------------------------------
      mass_seo4_before = 0.
      mass_seo4_after = 0.
      mass_seo3_before = 0.
      mass_seo3_after = 0.
      mass_born_before = 0.
      mass_born_after = 0.
      do jj = 1,soil(j)%nly
        
        !retrieve the current (daily) constituent solution concentrations (mg/L)
        cseo4 = cs_soil(j)%ly(jj)%csc(1) !mg/L
        cseo3 = cs_soil(j)%ly(jj)%csc(2) !mg/L
        cborn = cs_soil(j)%ly(jj)%csc(3) !mg/L

        !retrieve the sorbed constituent mass concentration (mg/kg)
        ccseo4 = cs_soil(j)%ly(jj)%csc_sorb(1) !mg/kg
        ccseo3 = cs_soil(j)%ly(jj)%csc_sorb(2) !mg/kg
        ccborn = cs_soil(j)%ly(jj)%csc_sorb(3) !mg/kg

        !find total mass of constituent in solution (mg) in the soil layer
        water_volume = (soil(j)%phys(jj)%st/1000.) * hru_area_m2 !m * m2 = m3
        mass_seo4_sol = (cseo4 * 1000) * water_volume !mg of seo4
        mass_seo3_sol = (cseo3 * 1000) * water_volume !mg of seo3
        mass_born_sol = (cborn * 1000) * water_volume !mg of boron
        
        !find total constituent mass sorbed to soil (mg) in the soil layer
        sol_thick = soil(j)%phys(jj)%thick
        volume = hru_area_m2 * (sol_thick/1000.) !m3 of soil
        mass_seo4_sorb = ccseo4 * volume * (soil(j)%phys(jj)%bd*1000) !mg of seo4
        mass_seo3_sorb = ccseo3 * volume * (soil(j)%phys(jj)%bd*1000) !mg of seo3
        mass_born_sorb = ccborn * volume * (soil(j)%phys(jj)%bd*1000) !mg of boron

        !seo4
        !compute new values of solution mass and sorbed mass, given 1) Kd and 2) total mass is conserved
        !(two equations, two unknowns)
        mass_seo4_before = mass_seo4_before + mass_seo4_sol
        mass_total = mass_seo4_sol + mass_seo4_sorb 
        val_num = (cs_rct_soil(j)%kd_seo4 * volume * soil(j)%phys(jj)%bd*1000)!total mass, which is conserved
        val_den = (water_volume*1000)
        val = (val_num / val_den) + 1
        mass_seo4_sol = mass_total / val
        mass_seo4_sorb = mass_total - mass_seo4_sol
        mass_seo4_after = mass_seo4_after + mass_seo4_sol
        
        !convert to solute concentration (mg/L) and sorbed concentration (mg/kg)
        if(water_volume > 0) then
          cseo4_new = (mass_seo4_sol/water_volume) / 1000. !mg/L
        else
          cseo4_new = 0.
        endif
        ccseo4_new = (mass_seo4_sorb/volume)/(soil(j)%phys(jj)%bd*1000) !mg/kg
        ratio = ccseo4_new / cseo4_new !this should equal Kd

        !store in global arrays
        cs_soil(j)%ly(jj)%csc(1) = cseo4_new !mg/L
        cs_soil(j)%ly(jj)%csc_sorb(1) = ccseo4_new !mg/kg

        !convert soil water concentration to kg/ha
        water_volume = (soil(j)%phys(jj)%st/1000.) * hru_area_m2 !m * m2 = m3
        cs_soil(j)%ly(jj)%cs(1) = (cs_soil(j)%ly(jj)%csc(1)/1000.) * water_volume / hru(j)%area_ha !kg of Se per ha
        
        !seo3
        !compute new values of solution mass and sorbed mass, given 1) Kd and 2) total mass is conserved
        !(two equations, two unknowns)
        mass_seo3_before = mass_seo3_before + mass_seo3_sol
        mass_total = mass_seo3_sol + mass_seo3_sorb !total mass, which is conserved
        val_num = (cs_rct_soil(j)%kd_seo3 * volume * soil(j)%phys(jj)%bd*1000)
        val_den = (water_volume*1000)
        val = (val_num / val_den) + 1
        mass_seo3_sol = mass_total / val
        mass_seo3_sorb = mass_total - mass_seo3_sol
        mass_seo3_after = mass_seo3_after + mass_seo3_sol

        !convert to solute concentration (mg/L) and sorbed concentration (mg/kg)
        if(water_volume > 0) then
          cseo3_new = (mass_seo3_sol/water_volume) / 1000. !mg/L
        else
          cseo3_new = 0.
        endif
        ccseo3_new = (mass_seo3_sorb/volume)/(soil(j)%phys(jj)%bd*1000) !mg/kg
        ratio = ccseo3_new / cseo3_new !this should equal Kd

        !store in global arrays
        cs_soil(j)%ly(jj)%csc(2) = cseo3_new !mg/L
        cs_soil(j)%ly(jj)%csc_sorb(2) = ccseo3_new !mg/kg
        
        !convert soil water concentration to kg/ha
        water_volume = (soil(j)%phys(jj)%st/1000.) * hru_area_m2 !m * m2 = m3
        cs_soil(j)%ly(jj)%cs(2) = (cs_soil(j)%ly(jj)%csc(2)/1000.) * water_volume / hru(j)%area_ha !kg of Se per ha
     
        !boron
        !compute new values of solution mass and sorbed mass, given 1) Kd and 2) total mass is conserved
        !(two equations, two unknowns)
        mass_born_before = mass_born_before + mass_born_sol
        mass_total = mass_born_sol + mass_born_sorb !total mass, which is conserved
        val_num = (cs_rct_soil(j)%kd_born * volume * soil(j)%phys(jj)%bd*1000)
        val_den = (water_volume*1000)
        val = (val_num / val_den) + 1
        mass_born_sol = mass_total / val
        mass_born_sorb = mass_total - mass_born_sol
        mass_born_after = mass_born_after + mass_born_sol

        !convert to solute concentration (mg/L) and sorbed concentration (mg/kg)
        if(water_volume > 0) then
          cborn_new = (mass_born_sol/water_volume) / 1000. !mg/L
        else
          cborn_new = 0.
        endif
        ccborn_new = (mass_born_sorb/volume)/(soil(j)%phys(jj)%bd*1000) !mg/kg
        ratio = ccborn_new / cborn_new !this should equal Kd

        !store in global arrays
        cs_soil(j)%ly(jj)%csc(3) = cborn_new !mg/L
        cs_soil(j)%ly(jj)%csc_sorb(3) = ccborn_new !mg/kg
        
        !convert soil water concentration to kg/ha
        water_volume = (soil(j)%phys(jj)%st/1000.) * hru_area_m2 !m * m2 = m3
        cs_soil(j)%ly(jj)%cs(3) = (cs_soil(j)%ly(jj)%csc(3)/1000.) * water_volume / hru(j)%area_ha !kg of Boron per ha
     
      enddo

      
      !store: mass transferred by sorption (seo4, seo3, boron)
      hcsb_d(j)%cs(1)%sorb = ((mass_seo4_before-mass_seo4_after)/1.e6) / hru(j)%area_ha !kg/ha
      hcsb_d(j)%cs(2)%sorb = ((mass_seo3_before-mass_seo3_after)/1.e6) / hru(j)%area_ha !kg/ha
      hcsb_d(j)%cs(3)%sorb = ((mass_born_before-mass_born_after)/1.e6) / hru(j)%area_ha !kg/ha
      
      !convert sorbed mg/kg to kg/ha, for basic HRU transport calculations (se_sed subroutine) ------------------------
      do jj = 1,soil(j)%nly
        
        !get thickness of the soil layer
        sol_thick = soil(j)%phys(jj)%thick

        !get the mass (kg) of the soil in the current layer
        soil_volume = hru_area_m2 * (sol_thick/1000.) !m3 of soil
        soil_mass = soil_volume * (soil(j)%phys(jj)%bd*1000) !kg of soil

        !get the mass (kg) of constituent in the current layer
        mass_seo4_sorb = (cs_soil(j)%ly(jj)%csc_sorb(1)*soil_mass)/1.e6 !mg/kg * kg / 1.e6 = kg of seo4
        mass_seo3_sorb = (cs_soil(j)%ly(jj)%csc_sorb(2)*soil_mass)/1.e6
        mass_born_sorb = (cs_soil(j)%ly(jj)%csc_sorb(3)*soil_mass)/1.e6

        !compute kg of sorbed constituent per ha of land
        cs_soil(j)%ly(jj)%cs_sorb(1) = mass_seo4_sorb / hru(j)%area_ha !kg/ha
        cs_soil(j)%ly(jj)%cs_sorb(2) = mass_seo3_sorb / hru(j)%area_ha !kg/ha
        cs_soil(j)%ly(jj)%cs_sorb(3) = mass_born_sorb / hru(j)%area_ha !kg/ha
        
      enddo


      return
      end !cs_sorb_hru
