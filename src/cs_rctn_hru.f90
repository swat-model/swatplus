      subroutine cs_rctn_hru !rtb cs
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine updates constituent concentrations based on chemical reactions and sorption in the soil profile

      use hru_module, only : hru,ihru
      use constituent_mass_module
      use cs_data_module
      use soil_module
      use organic_mineral_mass_module
      use cs_module

      implicit none

      integer :: j,jj,n,dum
      real :: cseo4,cseo3
      real :: conc_old,conc_new,conc_rg,k_rg,phi_value, &
              hru_area_m2,water_volume,gw_volume, &
              mass_seo4_before,mass_seo3_before, &
              mass_seo4_after,mass_seo3_after
      real :: cs_mass_kg,seo4_conc,seo3_conc,no3_conc
      dimension conc_old(3),conc_new(3),conc_rg(3),k_rg(4,3),phi_value(3)
      
      
      !hru ID
      j = ihru

      !area of the HRU in m2
      hru_area_m2 = hru(j)%area_ha * 10000.

      !chemical reactions (soil profile) for selenium
      !calculate change in concentration due to chemical reactions, for each soil layer
      mass_seo4_before = 0.
      mass_seo4_after = 0.
      mass_seo3_before = 0.
      mass_seo3_after = 0.
      do jj = 1,soil(j)%nly
          
        !volume of water in the soil layer (m3)
        water_volume = (soil(j)%phys(jj)%st/1000.) * hru_area_m2

        !solute concentration in soil water (g/m3)
        if(water_volume > 0) then
          cs_mass_kg = cs_soil(j)%ly(jj)%cs(1) * hru(j)%area_ha !kg/ha * ha --> kg
          seo4_conc = (cs_mass_kg * 1000.) / water_volume !g/m3 = mg/L
          cs_mass_kg = cs_soil(j)%ly(jj)%cs(2) * hru(j)%area_ha
          seo3_conc = (cs_mass_kg * 1000.) / water_volume
          cs_mass_kg = soil1(j)%mn(jj)%no3 * hru(j)%area_ha
          no3_conc = (cs_mass_kg * 1000.) / water_volume
        else
          seo4_conc = 0.
          seo3_conc = 0.
          no3_conc = 0.
        endif

        !retrieve the current (daily) selenium solution concentrations (mg/L)
        conc_old(1) = seo4_conc 
        conc_old(2) = seo3_conc 
        conc_old(3) = no3_conc 
       
        !retrieve the current mass for seo4 and seo3 (kg/ha)
        mass_seo4_before = mass_seo4_before + cs_soil(j)%ly(jj)%cs(1)
        mass_seo3_before = mass_seo3_before + cs_soil(j)%ly(jj)%cs(2)
       
        !calculate the change in species concentrations using the 4th-order Runge-Kutta scheme.
        !for each slope, the Runge-Kutta slopes will be calculated using the R-K concentrations.
        !K1 (first slope)
        conc_rg(1) = conc_old(1)
        conc_rg(2) = conc_old(2)
        conc_rg(3) = conc_old(3)
        call se_reactions_soil(j,jj,conc_rg,k_rg,1)
        !K2 (second slope)
        conc_rg(1) = conc_old(1) + (0.5*1*k_rg(1,1))
        conc_rg(2) = conc_old(2) + (0.5*1*k_rg(1,2))
        conc_rg(3) = conc_old(3) + (0.5*1*k_rg(1,3))
        call se_reactions_soil(j,jj,conc_rg,k_rg,2)
        !K3 (third slope)
        conc_rg(1) = conc_old(1) + (0.5*1*k_rg(2,1))
        conc_rg(2) = conc_old(2) + (0.5*1*k_rg(2,2))
        conc_rg(3) = conc_old(3) + (0.5*1*k_rg(2,3))
        call se_reactions_soil(j,jj,conc_rg,k_rg,3)
        !K4 (fourth slope)
        conc_rg(1) = conc_old(1) + (1*k_rg(3,1))
        conc_rg(2) = conc_old(2) + (1*k_rg(3,2))
        conc_rg(3) = conc_old(3) + (1*k_rg(3,3))
        call se_reactions_soil(j,jj,conc_rg,k_rg,4)
       
        !calculate new concentration
        do n=1,3
          !calculate the increment, then the new concentration
          phi_value(n) = (1./6.) * (k_rg(1,n) + (2*k_rg(2,n)) + (2*k_rg(3,n)) + k_rg(4,n))
          conc_new(n) = conc_old(n) + (phi_value(n)*1)
        enddo
       
        !store new concentration values
        cs_soil(j)%ly(jj)%csc(1) = conc_new(1)
        cs_soil(j)%ly(jj)%csc(2) = conc_new(2)
          
        !convert to kg/ha, for regular SWAT routines
        cs_soil(j)%ly(jj)%cs(1) = (cs_soil(j)%ly(jj)%csc(1)/1000.)*water_volume / hru(j)%area_ha !kg of seo4 per ha
        cs_soil(j)%ly(jj)%cs(2) = (cs_soil(j)%ly(jj)%csc(2)/1000.)*water_volume / hru(j)%area_ha !kg of seo3 per ha
        soil1(j)%mn(jj)%no3 = (conc_new(3)/1000.)*water_volume / hru(j)%area_ha !kg of no3-n per ha
       
        !check mass after chemical reactions
        mass_seo4_after = mass_seo4_after + cs_soil(j)%ly(jj)%cs(1)
        mass_seo3_after = mass_seo3_after + cs_soil(j)%ly(jj)%cs(2)
       
      enddo

      !store mass balance terms
      hcsb_d(j)%cs(1)%rctn = mass_seo4_after - mass_seo4_before !kg/ha
      hcsb_d(j)%cs(2)%rctn = mass_seo3_after - mass_seo3_before !kg/ha
      

      return
      end !cs_rctn_hru

      
      
      

      !rate laws for Se chemical reduction (seo4 --> seo3) --------------------------------------------------------------------------------
      subroutine se_reactions_soil(j,jj,conc_rg,k_rg,k_slope)

      use cs_data_module

      implicit none

      integer :: j,jj,k_slope,kk
      real :: conc_rg,conc_old,k_rg,cseo4,cseo3, &
              no3inhib,hru_area_m2,no3_mass_kg,no3_conc, &
              seo4red,seo3red,dseo4,dseo3,dno3,cno3, &
              o2,o2red,no3red,yseo4_o2,yseo4_no3,se_prod_o2,se_prod_no3, &
              ko2a,kno3,sseratio
      dimension conc_rg(3),conc_old(3),k_rg(4,3)

      !get concentration of SeO4 and SeO3
      cseo4 = conc_rg(1)
      cseo3 = conc_rg(2)
      cno3 = conc_rg(3)

      !concentration of dissolved oxygen (O2) (specified in selenium input file)
      o2 = cs_rct_soil(j)%oxy_soil
       
      !rate law for selenate reduction
      no3inhib = cs_rct_soil(j)%se_ino3 / (cs_rct_soil(j)%se_ino3 + cno3)
      seo4red = cs_rct_soil(j)%kseo4 * cseo4 * no3inhib
      
      !rate law for selenite reduction
      seo3red = cs_rct_soil(j)%kseo3 * cseo3 * no3inhib
      
      !rate law for oxygen reduction and nitrate reduction, in the presence of shale
      yseo4_o2 = 315.84 / 224.0
      yseo4_no3 = 789.6 / 196.0
      no3red = 0.
      se_prod_o2 = 0.
      se_prod_no3 = 0.
      do kk=1,num_geol_shale
        !reduction of o2
        ko2a = cs_rct_soil(j)%ko2a(kk)
        o2red = ko2a * o2 * cs_rct_soil(j)%shale(kk)
        !reduction of no3
        kno3 = cs_rct_soil(j)%kno3a(kk)
        no3red = no3red + (kno3 * cno3 * cs_rct_soil(j)%shale(kk))
        !total selenium released from the shale (via oxidation)
        sseratio = cs_rct_soil(j)%sseratio(kk)
        se_prod_o2 = se_prod_o2 + (o2red * yseo4_o2 * (1/sseratio))
        se_prod_no3 = se_prod_no3 + (no3red * yseo4_no3 * (1/sseratio))
      enddo
       
      !change in seo4 and seo3
      dseo4 = (se_prod_o2 + se_prod_no3) - seo4red
      dseo3 = seo4red - seo3red !all of reduced seo4 becomes seo3
      dno3 = no3red * (-1)
       
      !store change in concentrations
      k_rg(k_slope,1) = dseo4
      k_rg(k_slope,2) = dseo3
      k_rg(k_slope,3) = dno3

      return 
      end !se_reactions_soil

