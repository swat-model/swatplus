      subroutine cs_sorb_aqu !rtb cs
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine updates constituent concentrations based on sorption in the aquifer

      use hydrograph_module, only : ob,icmd
      use aquifer_module
      use organic_mineral_mass_module
      use constituent_mass_module
      use cs_aquifer
      use cs_data_module
      
      implicit none
      integer :: iaq,iaqdb
      real :: mass_seo4_sol,mass_seo3_sol,mass_born_sol,&        
          mass_seo4_sorb,mass_seo3_sorb,mass_born_sorb, &        
          ratio,mass_total,val_num,val_den,val,         &        
          cseo4_new,ccseo4_new,                         &        
          cseo3_new,ccseo3_new,cborn_new,ccborn_new,    &        
          gw_volume,aqu_volume,aqu_bd,aqu_mass
      real :: sorbed_seo4,sorbed_seo3,sorbed_born
      real :: mass_seo4_before,mass_seo4_after,&        
          mass_seo3_before,mass_seo3_after,&				
          mass_born_before,mass_born_after
      
      
      !aquifer ID
      iaq = ob(icmd)%num
      
      !volume of groundwater in the aquifer
      gw_volume = (aqu_d(iaq)%stor/1000.)*(ob(icmd)%area_ha*10000.) !m3 of groundwater

      !get database of the aquifer object
      iaqdb = ob(icmd)%props
      
      
      !convert sorbed concentration of kg/ha to mg/kg -----------------------------------------------------------------
      !mass of aquifer material
      aqu_volume = (ob(icmd)%area_ha*10000.) * aqudb(iaqdb)%dep_bot * (1-aqu_dat(iaq)%spyld) !m3 of aquifer material
      aqu_bd = 2000. !kg/m3
      aqu_mass = aqu_volume * aqu_bd !m3 * kg/m3 --> kg
      
      !get the mass (mg) of constituent in the current layer
      sorbed_seo4 = cs_aqu(iaq)%cs_sorb(1) !kg/ha
      sorbed_seo3 = cs_aqu(iaq)%cs_sorb(2) !kg/ha
      sorbed_born = cs_aqu(iaq)%cs_sorb(3) !kg/ha
      mass_seo4_sorb = sorbed_seo4 * 1.e6 * ob(icmd)%area_ha !mg
      mass_seo3_sorb = sorbed_seo3 * 1.e6 * ob(icmd)%area_ha !mg
      mass_born_sorb = sorbed_born * 1.e6 * ob(icmd)%area_ha !mg

      !compute mass concentration (mg of constituent per kg of soil)
      cs_aqu(iaq)%csc_sorb(1) = mass_seo4_sorb / aqu_mass !mg/kg
      cs_aqu(iaq)%csc_sorb(2) = mass_seo3_sorb / aqu_mass !mg/kg
      cs_aqu(iaq)%csc_sorb(3) = mass_born_sorb / aqu_mass !mg/kg

      
      !calculate mass transfer of constituent, for each layer ------------------------------------------------------------
      mass_seo4_before = 0.
      mass_seo4_after = 0.
      mass_seo3_before = 0.
      mass_seo3_after = 0.
      mass_born_before = 0.
      mass_born_after = 0.

      !total mass of constituent in solution (mg) in the aquifer
      mass_seo4_sol = (cs_aqu(iaq)%csc(1) * 1000.) * gw_volume !g/m3 * (1000 mg/g) * m3 = mg of seo4
      mass_seo3_sol = (cs_aqu(iaq)%csc(2) * 1000.) * gw_volume !mg of seo3
      mass_born_sol = (cs_aqu(iaq)%csc(3) * 1000.) * gw_volume !mg of boron
      
      !find total mass of constituent sorbed to soil (mg) in the soil layer
      mass_seo4_sorb = cs_aqu(iaq)%csc_sorb(1) * aqu_mass !mg/kg * kg = mg
      mass_seo3_sorb = cs_aqu(iaq)%csc_sorb(2) * aqu_mass !mg/kg * kg = mg
      mass_born_sorb = cs_aqu(iaq)%csc_sorb(3) * aqu_mass !mg/kg * kg = mg

      !seo4
      !compute new values of solution mass and sorbed mass, given 1) Kd and 2) total mass is conserved
      !(two equations, two unknowns)
      mass_seo4_before = mass_seo4_sol
      mass_total = mass_seo4_sol + mass_seo4_sorb !total mass, which is conserved
      val_num = cs_rct_aqu(iaq)%kd_seo4 * aqu_volume * aqu_bd
      val_den = gw_volume * 1000
      val = (val_num / val_den) + 1
      mass_seo4_sol = mass_total / val
      mass_seo4_sorb = mass_total - mass_seo4_sol
      mass_seo4_after = mass_seo4_sol
        
      !convert to solute concentration (mg/L) and sorbed concentration (mg/kg)
      if(gw_volume > 0) then
        cseo4_new = (mass_seo4_sol/gw_volume) / 1000. !g/m3
      else
        cseo4_new = 0.
      endif
      ccseo4_new = (mass_seo4_sorb/aqu_volume) / aqu_bd  !mg/kg
      ratio = ccseo4_new / cseo4_new !this should be equal to Kd_seo4

      !store in global arrays
      cs_aqu(iaq)%csc(1) = cseo4_new !g/m3
      cs_aqu(iaq)%csc_sorb(1) = ccseo4_new !mg/kg

      !convert groundwater concentration to kg
      cs_aqu(iaq)%cs(1) = (cs_aqu(iaq)%csc(1)/1000.) * gw_volume
      
      !seo3
      !compute new values of solution mass and sorbed mass, given 1) Kd and 2) total mass is conserved
      !(two equations, two unknowns)
      mass_seo3_before = mass_seo3_sol
      mass_total = mass_seo3_sol + mass_seo3_sorb !total mass, which is conserved
      val_num = cs_rct_aqu(iaq)%kd_seo3 * aqu_volume * aqu_bd
      val_den = gw_volume * 1000
      val = (val_num / val_den) + 1
      mass_seo3_sol = mass_total / val
      mass_seo3_sorb = mass_total - mass_seo3_sol
      mass_seo3_after = mass_seo3_sol
        
      !convert to solute concentration (mg/L) and sorbed concentration (mg/kg)
      if(gw_volume > 0) then
        cseo3_new = (mass_seo3_sol/gw_volume) / 1000. !g/m3
      else
        cseo3_new = 0.
      endif
      ccseo3_new = (mass_seo3_sorb/aqu_volume) / aqu_bd  !mg/kg
      ratio = ccseo3_new / cseo3_new !this should be equal to Kd_seo3

      !store in global arrays
      cs_aqu(iaq)%csc(2) = cseo3_new !g/m3
      cs_aqu(iaq)%csc_sorb(2) = ccseo3_new !mg/kg

      !convert groundwater concentration to kg
      cs_aqu(iaq)%cs(2) = (cs_aqu(iaq)%csc(2)/1000.) * gw_volume
      
      !boron
      !compute new values of solution mass and sorbed mass, given 1) Kd and 2) total mass is conserved
      !(two equations, two unknowns)
      mass_born_before = mass_born_sol
      mass_total = mass_born_sol + mass_born_sorb !total mass, which is conserved
      val_num = cs_rct_aqu(iaq)%kd_born * aqu_volume * aqu_bd
      val_den = gw_volume * 1000
      val = (val_num / val_den) + 1
      mass_born_sol = mass_total / val
      mass_born_sorb = mass_total - mass_born_sol
      mass_born_after = mass_born_sol
        
      !convert to solute concentration (mg/L) and sorbed concentration (mg/kg)
      if(gw_volume > 0) then
        cborn_new = (mass_born_sol/gw_volume) / 1000. !g/m3
      else
        cborn_new = 0.
      endif
      ccborn_new = (mass_born_sorb/aqu_volume) / aqu_bd  !mg/kg
      ratio = ccborn_new / cborn_new !this should be equal to Kd_born

      !store in global arrays
      cs_aqu(iaq)%csc(3) = cborn_new !g/m3
      cs_aqu(iaq)%csc_sorb(3) = ccborn_new !mg/kg

      !convert groundwater concentration to kg
      cs_aqu(iaq)%cs(3) = (cs_aqu(iaq)%csc(3)/1000.) * gw_volume
      
      
      !store: mass transferred by sorption (seo4, seo3, boron) --------------------------------------------------------
      acsb_d(iaq)%cs(1)%sorb = ((mass_seo4_before-mass_seo4_after)/1.e6) !kg
      acsb_d(iaq)%cs(2)%sorb = ((mass_seo3_before-mass_seo3_after)/1.e6) !kg
      acsb_d(iaq)%cs(3)%sorb = ((mass_born_before-mass_born_after)/1.e6) !kg
      
      
      !convert sorbed mg/kg to kg/ha ----------------------------------------------------------------------------------
      mass_seo4_sorb = (cs_aqu(iaq)%csc_sorb(1)*aqu_mass)/1.e6 !mg/kg * kg / 1.e6 = kg of seo4
      cs_aqu(iaq)%cs_sorb(1) = mass_seo4_sorb / ob(icmd)%area_ha
      
      mass_seo3_sorb = (cs_aqu(iaq)%csc_sorb(2)*aqu_mass)/1.e6 !mg/kg * kg / 1.e6 = kg of seo3
      cs_aqu(iaq)%cs_sorb(2) = mass_seo3_sorb / ob(icmd)%area_ha
      
      mass_born_sorb = (cs_aqu(iaq)%csc_sorb(3)*aqu_mass)/1.e6 !mg/kg * kg / 1.e6 = kg of boron
      cs_aqu(iaq)%cs_sorb(3) = mass_born_sorb / ob(icmd)%area_ha
      
      !store for mass balance output
      acsb_d(iaq)%cs(1)%srbd = mass_seo4_sorb
      acsb_d(iaq)%cs(2)%srbd = mass_seo3_sorb
      acsb_d(iaq)%cs(3)%srbd = mass_born_sorb
      

      return
      end !cs_sorb_aqu
