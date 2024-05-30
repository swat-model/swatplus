      subroutine et_act
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates potential plant transpiration for Priestley-
!!    Taylor and Hargreaves ET methods, and potential and actual soil
!!    evaporation. NO3 movement into surface soil layer due to evaporation
!!    is also calculated.


!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    canstor(:)   |mm H2O        |amount of water held in canopy storage
!!    ep_max       |mm H2O        |maximum amount of transpiration (plant et)
!!                                |that can occur on current day in HRU 
!!    esco(:)      |none          |soil evaporation compensation factor
!!    ihru         |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    canev        |mm H2O        |amount of water evaporated from canopy
!!                                |storage
!!    ep_max       |mm H2O        |maximum amount of transpiration (plant et)
!!                                |that can occur on current day in HRU
!!    es_day       |mm H2O        |actual amount of evaporation (soil et) that
!!                                |occurs on day in HRU
!!    snoev        |mm H2O        |amount of water in snow lost through
!!                                |sublimation on current day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Min, Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
 
      use basin_module
      use organic_mineral_mass_module
      use hru_module, only : hru, canstor, ihru, canev, ep_max,  &
         es_day, pet_day, snoev
      use soil_module
      use plant_module
      use climate_module
      use hydrograph_module
      use water_body_module
      use reservoir_data_module
      
      implicit none

      integer :: j               !none          |HRU number
!!    real, parameter :: esd = 500., etco = 0.80, effnup = 0.1
      real :: esd                !mm            |maximum soil depth from which evaporation
                                 !              |is allowed to occur
      real :: etco               !              |
      real :: effnup             !              ! 
      real :: no3up              !kg N/ha       |amount of nitrate moving upward in profile 
      real :: es_max             !mm H2O        |maximum amount of evaporation (soil et)
                                 !              |that can occur on current day in HRU
      real :: eos1               !none          |variable to hold intermediate calculation
                                 !              |result
      real :: xx                 !none          |variable to hold intermediate calculation 
                                 !              |result
      real :: cej                !              |
      real :: eaj                !none          |weighting factor to adjust PET for impact of
                                 !              |plant cover    
      real :: pet                !mm H2O        |amount of PET remaining after water stored
                                 !              |in canopy is evaporated
      real :: esleft             !mm H2O        |potenial soil evap that is still available
      real :: evzp               !              |
      real :: eosl               !mm H2O        |maximum amount of evaporation that can occur
                                 !              |from soil profile
      real :: dep                !mm            |soil depth from which evaporation will occur
                                 !              |in current soil layer
      real :: evz                !              | 
      real :: sev                !mm H2O        |amount of evaporation from soil layer
      real :: sev_st             !mm H2O        |evaporation / soil water for no3 flux from layer 1 -> 2
      real :: cover              !kg/ha         |soil cover
      real :: wetvol_mm          !mm            |wetland water volume - average depth over hru
      integer :: ly              !none          |counter     
      integer:: ires,ihyd !Jaehak 2022

      j = ihru
      pet = pet_day
!!    added statements for test of real statement above
	esd = 500.  !soil(j)%zmx
	etco = 0.80
	effnup = 0.05
      ires= hru(j)%dbs%surf_stor !Jaehak 2022


!! evaporate canopy storage first
!! canopy storage is calculated by the model only if the Green & Ampt
!! method is used to calculate surface runoff. The curve number methods
!! take canopy effects into account in the equations. For either of the
!! CN methods, canstor will always equal zero.
      canev = 0.
      pet = pet - canstor(j)
      if (pet < 0.) then
        canstor(j) = -pet
        canev = pet_day
        pet = 0.
        ep_max = 0.
        es_max = 0.
      else
        canev = canstor(j)
        canstor(j) = 0.
      endif

      if (pet > 1.0e-6) then

        !! compute potential plant evap for methods other that Penman-Monteith
        !if (bsn_cc%pet /= 1) then
          if (pcom(j)%lai_sum <= 3.0) then
            ep_max = pcom(j)%lai_sum * pet / 3.
          else
            ep_max = pet
          end if
          if (ep_max < 0.) ep_max = 0.
        !end if

        !! compute potential soil evaporation
        cej = -5.e-5
        eaj = 0.
        es_max = 0.
        eos1 = 0.
        cover = pl_mass(j)%ab_gr_com%m + rsd1(j)%tot_com%m
        if (hru(j)%sno_mm >= 0.5) then
          eaj = 0.5
        else
          eaj = Exp(cej * (cover + 0.1))
        end if
        es_max = pet * eaj
        eos1 = pet / (es_max + ep_max + 1.e-10)
        eos1 = es_max * eos1
        es_max = Min(es_max, eos1)
        es_max = Max(es_max, 0.)

        if (wet(j)%flo > 0.) then !wetlands water evaporation reduced by canopy Jaehak 2022
        
          if (pcom(j)%lai_sum <= 4.0) then 
            ihyd = wet_dat(ires)%hyd
            es_max = wet_hyd(ihyd)%evrsv * (1.-pcom(j)%lai_sum / 4.) * pet !adapted from Sakaguchi et al. 2014
          else
            es_max = 0.
          endif

        else  
        
          !! make sure maximum plant and soil ET doesn't exceed potential ET
          if (pet_day < es_max + ep_max) then
            es_max = pet_day - ep_max
            if (pet < es_max + ep_max) then
              es_max = pet * es_max / (es_max + ep_max)
              ep_max = pet * ep_max / (es_max + ep_max)
            end if
          end if
        end if
        
        !! adjust es_max and ep_max for impervous urban cover
        !es_max = 0.5 * es_max
        !ep_max = 0.5 * ep_max
          
        !! initialize soil evaporation variables
        esleft = es_max

        !! compute sublimation
        if (w%tave > 0.) then
          if (hru(j)%sno_mm >= esleft) then
            !! take all soil evap from snow cover
            hru(j)%sno_mm = hru(j)%sno_mm - esleft
            snoev = snoev + esleft
            esleft = 0.
          else
            !! take all soil evap from snow cover before taking from soil
            esleft = esleft - hru(j)%sno_mm
            snoev = snoev + hru(j)%sno_mm
            hru(j)%sno_mm = 0.
          endif
        endif

        !! compute evaporation from ponded water
        wet_wat_d(j)%evap = 0.
        if (wet(j)%flo > 0.) then
          wetvol_mm = wet(j)%flo / (10. *  hru(j)%area_ha)    !mm=m3/(10.*ha)
          !! take all soil evap from wetland storage before taking from soil
          if (wetvol_mm >= esleft) then
            wetvol_mm = wetvol_mm - esleft
            wet_wat_d(j)%evap = esleft * (10. *  hru(j)%area_ha)
            esleft = 0.
          else
            esleft = esleft - wetvol_mm
            wet_wat_d(j)%evap = wetvol_mm * (10. *  hru(j)%area_ha)
            wetvol_mm = 0.
          endif
          wet(j)%flo = 10. * wetvol_mm * hru(j)%area_ha
          hru(j)%water_evap = wet_wat_d(j)%evap / (10. * hru(j)%area_ha)  !mm=m3/(10*ha)
        endif

!! take soil evap from each soil layer
      evzp = 0.
      eosl = esleft
      do ly = 1, soil(j)%nly

        !! depth exceeds max depth for soil evap (esd)
        dep = 0.
        if (ly == 1) then
          dep = soil(j)%phys(1)%d
        else
          dep = soil(j)%phys(ly-1)%d
        endif
        
        if (dep < esd) then
          !! calculate evaporation from soil layer
          evz = eosl * soil(j)%phys(ly)%d / (soil(j)%phys(ly)%d +        &
             Exp(2.374 - .00713 * soil(j)%phys(ly)%d))
          sev = evz - evzp * (1. - hru(j)%hyd%esco)
          evzp = evz
          if (soil(j)%phys(ly)%st < soil(j)%phys(ly)%fc) then
            xx =  2.5 * (soil(j)%phys(ly)%st - soil(j)%phys(ly)%fc) /    &
             soil(j)%phys(ly)%fc
            sev = sev * exp(xx)
          end if
          sev = Min(sev, soil(j)%phys(ly)%st * etco)

          if (sev < 0.) sev = 0.
          if (sev > esleft) sev = esleft

          !! adjust soil storage, potential evap
          if (soil(j)%phys(ly)%st > sev) then
            esleft = esleft - sev
            soil(j)%phys(ly)%st = Max(1.e-6, soil(j)%phys(ly)%st - sev)
          else
            esleft = esleft - soil(j)%phys(ly)%st
            sev = soil(j)%phys(ly)%st
            soil(j)%phys(ly)%st = 0.
          endif
        endif

        !! compute no3 flux from layer 2 to 1 by soil evaporation
        if (ly == 2) then
          if (soil(j)%phys(2)%st > 1.e-3) then
            sev_st = sev / (soil(j)%phys(2)%st)
          else
            sev_st = 0.
          end if
          sev_st = amin1 (1., sev_st)
          no3up = effnup * sev_st * soil1(j)%mn(2)%no3
          no3up = Min(no3up, soil1(j)%mn(2)%no3)
          soil1(j)%mn(2)%no3 = max(0.0001,soil1(j)%mn(2)%no3 - no3up)
          soil1(j)%mn(1)%no3 = max(0.0001,soil1(j)%mn(1)%no3 + no3up)
        endif

      end do    !layer loop

      !! update total soil water content
      soil(j)%sw = 0.
      do ly = 1, soil(j)%nly
        soil(j)%sw = soil(j)%sw + soil(j)%phys(ly)%st
      end do

      !! calculate actual amount of evaporation from soil
      es_day = es_max - esleft
      if (es_day < 0.) es_day = 0.

      end if

      return
      end subroutine et_act