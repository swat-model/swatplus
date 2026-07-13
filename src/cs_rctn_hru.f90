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
      
      external :: se_reactions_soil

      integer :: j = 0
      integer :: jj = 0
      integer :: n = 0
      real :: conc_old
      real :: conc_new
      real :: conc_rg
      real :: k_rg
      real :: phi_value
      real :: hru_area_m2 = 0.
      real :: water_volume = 0.
      real :: mass_seo4_before = 0.
      real :: mass_seo3_before = 0.
      real :: mass_seo4_after = 0.
      real :: mass_seo3_after = 0.
      real :: cs_mass_kg = 0.
      real :: seo4_conc = 0.
      real :: seo3_conc = 0.
      real :: no3_conc = 0.
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
      
      end subroutine cs_rctn_hru 