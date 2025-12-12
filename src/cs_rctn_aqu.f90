      subroutine cs_rctn_aqu !rtb cs
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine updates constituent concentrations based on chemical reactions in groundwater

      use hydrograph_module, only : ob,icmd
      use aquifer_module
      use constituent_mass_module
      use cs_data_module
      use organic_mineral_mass_module
      use cs_module
      use cs_aquifer

      implicit none
      
      external :: se_reactions_aquifer

      integer :: n = 0
      integer :: iaq = 0
      real :: conc_old
      real :: conc_new
      real :: conc_rg
      real :: k_rg
      real :: phi_value
      real :: gw_volume = 0.
      real :: mass_seo4_before = 0.
      real :: mass_seo3_before = 0.
      real :: mass_seo4_after = 0.
      real :: mass_seo3_after = 0.
      real :: cs_mass_kg = 0.
      dimension conc_old(3),conc_new(3),conc_rg(3),k_rg(4,3),phi_value(3)
        
      !aquifer ID
      iaq = ob(icmd)%num
      
      !volume of groundwater in the aquifer
      gw_volume = (aqu_d(iaq)%stor/1000.)*(ob(icmd)%area_ha*10000.) !m3 of groundwater
      
      mass_seo4_before = 0.
      mass_seo4_after = 0.
      mass_seo3_before = 0.
      mass_seo3_after = 0.
      
      !retrieve the current (daily) selenium groundwater concentration
      conc_old(1) = cs_aqu(iaq)%csc(1) !mg/L
      conc_old(2) = cs_aqu(iaq)%csc(2) !mg/L
      cs_mass_kg = aqu_d(iaq)%no3_st * ob(icmd)%area_ha !kg
      if(gw_volume > 0) then
        conc_old(3) = (cs_mass_kg * 1000.) / gw_volume !g/m3 = mg/L
      else
        conc_old(3) = 0.
      endif
      
      !retrieve the current mass concentrations
      mass_seo4_before = cs_aqu(iaq)%cs(1) !kg
      mass_seo3_before = cs_aqu(iaq)%cs(2) !kg

      !calculate the change in species concentrations using the 4th-order Runge-Kutta scheme.
      !for each slope, the Runge-Kutta slopes will be calculated using the R-K concentrations.
        
      !K1 (first slope)
      conc_rg(1) = conc_old(1)
      conc_rg(2) = conc_old(2)
      conc_rg(3) = conc_old(3)
      call se_reactions_aquifer(iaq,conc_rg,k_rg,1)

      !K2 (second slope)
      conc_rg(1) = conc_old(1) + (0.5*1*k_rg(1,1))
      conc_rg(2) = conc_old(2) + (0.5*1*k_rg(1,2))
      conc_rg(3) = conc_old(3) + (0.5*1*k_rg(1,3))
      call se_reactions_aquifer(iaq,conc_rg,k_rg,2)

      !K3 (third slope)
      conc_rg(1) = conc_old(1) + (0.5*1*k_rg(2,1))
      conc_rg(2) = conc_old(2) + (0.5*1*k_rg(2,2))
      conc_rg(3) = conc_old(3) + (0.5*1*k_rg(2,3))
      call se_reactions_aquifer(iaq,conc_rg,k_rg,3)

      !K4 (fourth slope)
      conc_rg(1) = conc_old(1) + (1*k_rg(3,1))
      conc_rg(2) = conc_old(2) + (1*k_rg(3,2))
      conc_rg(3) = conc_old(3) + (1*k_rg(3,3))
      call se_reactions_aquifer(iaq,conc_rg,k_rg,4)

      !calculate new concentration
      do n=1,3
        !calculate the increment, then the new concentration
        phi_value(n) = (1./6.) * (k_rg(1,n) + (2*k_rg(2,n)) + (2*k_rg(3,n)) +   k_rg(4,n))
        conc_new(n) = conc_old(n) + (phi_value(n)*1)
      enddo

      !store new concentration values
      cs_aqu(iaq)%csc(1) = conc_new(1)
      cs_aqu(iaq)%csc(2) = conc_new(2)
      aqu_d(iaq)%no3_st = (conc_new(3)/1000.)*gw_volume / ob(icmd)%area_ha !kg of no3-n per ha

      !convert to kg
      cs_aqu(iaq)%cs(1) = (cs_aqu(iaq)%csc(1)*gw_volume) / 1000.
      cs_aqu(iaq)%cs(2) = (cs_aqu(iaq)%csc(2)*gw_volume) / 1000.

      !check mass after chemical reactions
      mass_seo4_after = cs_aqu(iaq)%cs(1) !kg
      mass_seo3_after = cs_aqu(iaq)%cs(2) !kg

      !store mass balance terms
      acsb_d(iaq)%cs(1)%rctn = mass_seo4_after - mass_seo4_before !kg
      acsb_d(iaq)%cs(2)%rctn = mass_seo3_after - mass_seo3_before !kg    
      
      return
      
      end subroutine cs_rctn_aqu