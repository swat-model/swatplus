      subroutine swr_subwq
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes HRU loadings of chlorophyll-a, CBOD, 
!!    and dissolved oxygen to the main channel

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    enratio     |none          |enrichment ratio calculated for day in HRU
!!    hru_km(:)   |km^2          |area of HRU in square kilometers
!!    sedorgn(:)  |kg N/ha       |amount of organic nitrogen in surface runoff
!!                               |in HRU for the day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cbodu(:)    |mg/L          |carbonaceous biological oxygen demand of 
!!                               |surface runoff on current day in HRU
!!    chl_a(:)    |microgram/L   |chlorophyll-a concentration in water yield
!!                               |on current day in HRU
!!    doxq(:)     |mg/L          |dissolved oxygen concentration in the surface
!!                               |runoff on current day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module
      use hru_module, only : hru, ihru, qdr, sedorgn, surqno3, cbodu, doxq, chl_a, sedyld, enratio
      use soil_module
      use organic_mineral_mass_module
      use carbon_module
      use climate_module
      
      implicit none

      integer :: j             !none          |HRU number
      real :: soxy             !mg/L          |dissolved oxygen saturation concentration 
      real :: tp               !kmoles P      |kilomoles of phosphorus in nutrient loading to
                               !              |main channel
      real :: org_c            !kg            |organic carbon content of surface runoff on
                               !              |day in HRU
      real :: wtmp             !deg K         |temperature of surface runoff
      real :: ww               !none          |variable to hold intermediate calculation
                               !              |result
      real :: xx               !none          |variable to hold intermediate calculation
                               !              |result
      real :: yy               !none          |variable to hold intermediate calculation
                               !              |result
      real :: zz               !none          |variable to hold intermediate calculation
                               !              |result
      
      j = ihru

        !! calculcate water temperature
        !! Stefan and Preudhomme. 1993.  Stream temperature estimation
        !!from air temperature.  Water Res. Bull. p. 27-45
        !! SWAT manual 2.3.13
        wtmp = 5.0 + 0.75 * w%tave
        if (wtmp <= 0.1) wtmp = 0.1
        wtmp = wtmp + 273.15    !! deg C to deg K
      
        if (qdr(j) > 1.e-4) then
          tp = 100. * (sedorgn(j) + surqno3(j)) / qdr(j)    !100*kg/ha/mm = ppm 
          chl_a(j) = .1 * tp                                ! assume chlorophyll a is 0.01 total p
 
          !! calculate organic carbon loading to main channel
          org_c = 0.
          org_c = (soil1(j)%cbn(1) / 100.) * enratio * sedyld(j) * 1000.
          
          !!add by zhang
          !!========================
          if (bsn_cc%cswat == 2) then
            org_c = hsc_d(j)%sed_c * hru(j)%area_ha
          end if
          !!add by zhang
          !!========================
          
                  
          !! calculate carbonaceous biological oxygen demand (ppm or mg/L)
          cbodu(j) = 2.7 * org_c / (qdr(j) * hru(j)%km) / 10000.  !JAEHAK 2016

          !! calculate dissolved oxygen saturation concentration
          !! QUAL2E equation III-29
          ww = -139.34410 + (1.575701E05 / wtmp)
          xx = 6.642308E07 / (wtmp**2)
          yy = 1.243800E10 / (wtmp**3)
          zz = 8.621949E11 / (wtmp**4)
          soxy = Exp(ww - xx + yy - zz)
          if (soxy < 0.) soxy = 0.

          !! calculate actual dissolved oxygen concentration
          doxq(j) = soxy * exp(-0.1 * cbodu(j))
          if (doxq(j) < 0.0) doxq(j) = 0.0
          if (doxq(j) > soxy) doxq(j) = soxy
        else
          chl_a(j) = 0.
          cbodu(j) = 0.
          doxq(j) = 0.
        end if

      return
      end subroutine swr_subwq