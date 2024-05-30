      subroutine cli_initwgn(iwgn)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine initializes the HRU weather generator parameters from the 
!!    .wgn file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    i           |none          |HRU number
!!    ndays(:)    |julian date   |julian date for last day of preceding
!!                               |month (where the array location is the
!!                               |number of the month) The dates are for
!!                               |leap years
!!    rndseed(:,:)|none          |random number generator seeds
!!    rnmd1       |none          |random number between 0.0 and 1.0
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    daylmn(:)   |hr            |shortest daylength occurring during the year
!!    dewpt(:,:)  |deg C         |average dew point temperature for the month
!!    iregion(:)     |none          |precipitation category:
!!                               |  1 precipitation <= 508 mm/yr
!!                               |  2 precipitation > 508 and <= 1016 mm/yr
!!                               |  3 precipitation > 1016 mm/yr
!!    latcos(:)   |none          |Cos(Latitude)
!!    latsin(:)   |none          |Sin(Latitude)
!!    phutot(:)   |heat unit     |total potential heat units for year (used
!!                               |when no crop is growing)
!!    pr_w(1,:,:) |none          |probability of wet day after dry day in month
!!    pr_w(2,:,:) |none          |probability of wet day after wet day in month
!!    pr_w(3,:,:) |none          |proportion of wet days in the month
!!    tmp_an(:)   |deg C         |average annual air temperature
!!    tmpmn(:,:)  |deg C         |avg monthly minimum air temperature
!!    tmpmx(:,:)  |deg C         |avg monthly maximum air temperature
!!    tmpstdmn(:,:)|deg C        |standard deviation for avg monthly minimum air
!!                               |temperature
!!    tmpstdmx(:,:)|deg C        |standard deviation for avg monthly maximum air
!!                               |temperature
!!    welev(:)    |m             |elevation of weather station used to compile
!!                               |data
!!    wlat(:)     |degrees       |latitude of weather station used to compile
!!                               |data
!!    wndav(:,:) |m/s            |average wind speed for the month
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    pcpmm(:)    |mm            |amount of precipitation in month
!!    pcpd(:)     |days          |average number of days of precipitation
!!                               |in the month
!!    rainhhmx(:) |mm            |maximum 0.5 hour rainfall in month
!!                               |for entire period of record
!!    rain_yrs    |none          |number of years of recorded maximum 0.5h 
!!                               |rainfall used to calculate values for 
!!                               |rainhhmx(:)
!!    titldum     |NA            |title line of .wgn file (not used elsewhere)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Sin, Cos, Tan, Abs, Acos, Log, Exp, MaxVal
!!    SWAT: Aunif, Dstn1

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module
      use climate_module
      use time_module
      
      implicit none

      real :: xx                            !varies        |variable to hold calculation results
      real :: lattan                        !none          |Tan(Latitude)
      real :: x1                            !none          |variable to hold calculation results
      real :: x2                            !none          |variable to hold calculation results
      real :: x3                            !none          |variable to hold calculation results
      real :: tav                           !deg C         |average monthly temperature
      real :: tmin                          !deg C         |minimum average monthly temperature
      real :: tmax                          !deg C         |maximum average monthly temperature
      real :: tk                            !deg K         |maximum average monthly temperature degrees Kelvin
      real :: alb                           !none          |albedo
      real :: d                             !              |
      real :: gma                           !              |
      real :: ho                            !              |
      real :: aph                           !              |
      integer :: inext                      !none          |counter for days for running sum
      real :: sum                           !none          |variable to hold summation results
      real :: summm_p                       !mm            |sum of precipitation over year
      real :: summm_pet                     !mm            |sum of potential ET over year
      real :: summn_t                       !deg C         |sum of mimimum temp values over year
      real :: summx_t                       !deg C         |sum of maximum temp values over year
      real :: rnm2                          !none          |random number between 0.0 and 1.0
      real :: r6                            !none          |variable to hold calculation result
      real :: xlv                           !none          |variable to hold calculation results
      real, dimension (12) :: rain_hhsm     !mm            |smoothed values for maximum 0.5 hour rainfall 
      real :: rndm1                         !none          |random number between 0.0 and 1.0
      real :: dl                            !hour          |time threshold used to define dormant
                                            !              |period for plant (when daylength is within
                                            !              |the time specified by dl from the minimum
                                            !              |daylength for the area, the plant will go
                                            !              |dormant)     
      integer :: mon                        !none          |monthly counter
      integer :: mdays                      !none          |number of days in the month
      integer :: j                          !none          |counter
      integer :: m1                         !none          |array location (see definition of ndays)
      integer ::  nda                       !julian date   |julian date of last day in the month
      real :: cli_dstn1                     !              |
      real :: pcp_gen                       !mm H2O        |generated precipitation
      real :: aunif                         !              |
      integer :: xrnd                       !              |
      integer :: iwgn                       !              |
      integer :: mo_ppet                    !              |

      !! variables needed for radiation calcs.
      x1 = 0.0
      x2 = 0.0
      xx = wgn(iwgn)%lat / 57.296     !!convert degrees to radians (2pi/360=1/57.296)
      wgn_pms(iwgn)%latsin = Sin(xx)
      wgn_pms(iwgn)%latcos = Cos(xx)
      lattan = Tan(xx)
      !! calculate minimum daylength -> daylength=2*acos(-tan(sd)*tan(lat))/omega
      !! where solar declination, sd, = -23.5 degrees for minimum daylength in northern hemisphere and -tan(sd) = .4348
      !! absolute value is taken of tan(lat) to convert southern hemisphere values to northern hemisphere
      !! the angular velocity of the earth's rotation, omega, = 15 deg/hr or 0.2618 rad/hr and 2/0.2618 = 7.6394
      x1 = .4348 * Abs(lattan)      
      if (x1 < 1.) x2 = Acos(x1) 
      wgn_pms(iwgn)%daylmn = 7.6394 * x2

      !! calculate day length threshold for dormancy
      if (bsn_prm%dorm_hr < 1.e-6) then
        dl = 0.
         if (Abs(wgn(iwgn)%lat) > 40.) then
          dl = 1.
         else if (Abs(wgn(iwgn)%lat) < 20.) then
          dl = -1.
         else
         dl = (Abs(wgn(iwgn)%lat) - 20.) / 20.
         end if
      else
         dl = bsn_prm%dorm_hr
      end if
      wgn_pms(iwgn)%daylth = dl

      !! calculate smoothed maximum 0.5hr rainfall amounts
      rain_hhsm = 0.
      rain_hhsm(1) = (wgn(iwgn)%rainhmx(12) + wgn(iwgn)%rainhmx(1) + wgn(iwgn)%rainhmx(2)) / 3.
      do mon = 2, 11
        rain_hhsm(mon) = (wgn(iwgn)%rainhmx(mon-1) + wgn(iwgn)%rainhmx(mon) + wgn(iwgn)%rainhmx(mon+1)) / 3.
      end do
      rain_hhsm(12) = (wgn(iwgn)%rainhmx(11) + wgn(iwgn)%rainhmx(12) + wgn(iwgn)%rainhmx(1)) / 3.

      !! calculate missing values and additional parameters
      summx_t = 0.
      summn_t = 0.
      summm_p = 0.
      summm_pet = 0.
      tmin = 100.
      tmax = 0.
      do mon = 1, 12
        mdays = 0
        tav = 0.
        mdays = ndays(mon+1) - ndays(mon)
        tav = (wgn(iwgn)%tmpmx(mon) + wgn(iwgn)%tmpmn(mon)) / 2.
        if (tav > tmax) tmax = tav
        if (tav < tmin) tmin = tav
        summx_t = summx_t + wgn(iwgn)%tmpmx(mon)
        summn_t = summn_t + wgn(iwgn)%tmpmn(mon)

        !! calculate total potential heat units
        if (tav > 0.) wgn_pms(iwgn)%phutot = wgn_pms(iwgn)%phutot + tav * mdays

        !! calculate values for pr_w if missing or bad
        if (wgn(iwgn)%pr_ww(mon) <= wgn(iwgn)%pr_wd(mon).or.                    &
                                      wgn(iwgn)%pr_wd(mon) <= 0.) then
          if (wgn(iwgn)%pcpd(mon) < .1) wgn(iwgn)%pcpd(mon) = 0.1
          wgn(iwgn)%pr_wd(mon) = .75 * wgn(iwgn)%pcpd(mon) / mdays
          wgn(iwgn)%pr_ww(mon) = .25 + wgn(iwgn)%pr_wd(mon)
        else
        !! if pr_w values good, use calculated pcpd based on these values
        !! using first order Markov chain
        wgn(iwgn)%pcpd(mon) = mdays * wgn(iwgn)%pr_wd(mon) /                  &               
                       (1. - wgn(iwgn)%pr_ww(mon) + wgn(iwgn)%pr_wd(mon))
    
        end if

        !! calculate precipitation-related values
        if (wgn(iwgn)%pcpd(mon) <= 0.) wgn(iwgn)%pcpd(mon) = .001
        wgn_pms(iwgn)%pr_wdays(mon) = wgn(iwgn)%pcpd(mon) / mdays
        wgn_pms(iwgn)%pcpmean(mon) = wgn(iwgn)%pcpmm(mon) / wgn(iwgn)%pcpd(mon)
        if (wgn(iwgn)%pcpskw(mon) < 0.2) wgn(iwgn)%pcpskw(mon) = 0.2
        summm_p = summm_p + wgn(iwgn)%pcpmm(mon)
        wgn_pms(iwgn)%pcpdays = wgn_pms(iwgn)%pcpdays + wgn(iwgn)%pcpd(mon)

        !! compute potential et with Preistley-Taylor Method
        tav  = (wgn(iwgn)%tmpmx(mon) + wgn(iwgn)%tmpmn(mon)) / 2.
        tk = tav  + 273.
        alb = .15   !tropical rainforests (0.05-0.15)
        d = EXP(21.255 - 5304. / tk) * 5304. / tk ** 2
        gma = d / (d +.68)
        ho = 23.9 * wgn(iwgn)%solarav(mon) * (1. - alb) / 58.3
        aph = 1.28
        wgn_pms(iwgn)%pet(mon) = aph * ho * gma * (ndays(mon+1) - ndays(mon))
        summm_pet = summm_pet + wgn_pms(iwgn)%pet(mon)
      end do

      !! idewpt=0 if dew point or 1 if relative humidity
      wgn_pms(iwgn)%idewpt = 1
      do mon = 1, 12
        if (wgn(iwgn)%dewpt(mon) > 1. .or. wgn(iwgn)%dewpt(mon) < 0.) then
          wgn_pms(iwgn)%idewpt = 0
          exit
        end if
      end do
              
      !! initialize arrays for precip divided by pet moving sum
      ppet_ndays = 30
      allocate (wgn_pms(iwgn)%mne_ppet(ppet_ndays))
      allocate (wgn_pms(iwgn)%precip_mce(ppet_ndays))
      allocate (wgn_pms(iwgn)%pet_mce(ppet_ndays))
      !! initialize my next element array for the linked list
      do inext = 1, ppet_ndays
        wgn_pms(iwgn)%mne_ppet(inext) = inext
      end do
      !! use average monthly precip and pet from wgn
      if (time%mo == 1) then
        mo_ppet = 12
      else
        mo_ppet = time%mo - 1
      end if
      
      wgn_pms(iwgn)%precip_sum = 0.
      wgn_pms(iwgn)%pet_sum = 0.
      do inext = 1, ppet_ndays
        wgn_pms(iwgn)%precip_mce(inext) = wgn(iwgn)%pcpmm(mo_ppet) / (ndays(mo_ppet+1) - ndays(mo_ppet))
        wgn_pms(iwgn)%pet_mce(inext) = wgn_pms(iwgn)%pet(mo_ppet) / (ndays(mo_ppet+1) - ndays(mo_ppet))
        wgn_pms(iwgn)%precip_sum = wgn_pms(iwgn)%precip_sum + wgn_pms(iwgn)%precip_mce(inext)
        wgn_pms(iwgn)%pet_sum = wgn_pms(iwgn)%pet_sum + wgn_pms(iwgn)%pet_mce(inext)
      end do

      wgn_pms(iwgn)%pcp_an = summm_p
      wgn_pms(iwgn)%ppet_an = summm_p / summm_pet
      wgn_pms(iwgn)%tmp_an = (summx_t + summn_t) / 24.

      !! calculate initial temperature of soil layers
      if (time%day_start > ndays(2)) then
        do mon = 2, 12
          m1 = 0
          nda = 0
          m1 = mon + 1
          nda = ndays(m1) - 1
          if (time%day_start <= nda) exit
        end do
      else
        mon = 1
      end if

      xrnd = rndseed(idg(3),iwgn)
      rndm1 = Aunif(xrnd)
      do mon = 1, 12
        !! calculate precipitation correction factor for pcp generator
        r6 = wgn(iwgn)%pcpskw(mon) / 6.
        sum = 0.
        do j = 1, 1000
          rnm2 = Aunif(xrnd)
          xlv = (cli_Dstn1(rndm1,rnm2) - r6) * r6 + 1
          rndm1 = rnm2
          xlv = (xlv**3 - 1.) * 2 / wgn(iwgn)%pcpskw(mon)
          pcp_gen = xlv * wgn(iwgn)%pcpstd(mon) + wgn_pms(iwgn)%pcpmean(mon)
          if (pcp_gen < 0.01) pcp_gen = 0.01
          sum = sum + pcp_gen
        end do
        if (sum > 0.) then
          wgn_pms(iwgn)%pcf(mon) = 1000. * wgn_pms(iwgn)%pcpmean(mon) / sum
        else
          wgn_pms(iwgn)%pcf(mon) = 1.
        end if

        !! calculate or estimate amp_r values
        if (wgn(iwgn)%rain_yrs < 1.0) wgn(iwgn)%rain_yrs = 10.
        x1 = .5 / wgn(iwgn)%rain_yrs 
        x2 = x1 / wgn(iwgn)%pcpd(mon)
        x3 = rain_hhsm(mon) / Log(x2)
        if (wgn_pms(iwgn)%pcpmean(mon) > 1.e-4) then
          wgn_pms(iwgn)%amp_r(mon) = bsn_prm%adj_pkr * (1. - Exp(x3 /       &
                                       wgn_pms(iwgn)%pcpmean(mon)))
        else
          wgn_pms(iwgn)%amp_r(mon) = 0.95
        end if
        if (wgn_pms(iwgn)%amp_r(mon) < .1) wgn_pms(iwgn)%amp_r(mon) = .1
        if (wgn_pms(iwgn)%amp_r(mon) > .95) wgn_pms(iwgn)%amp_r(mon) = .95
      end do

      !! determine precipitation category (ireg initialized to category 1)
      if (summm_p < 508.) then
        wgn_pms(iwgn)%ireg = 1
      else if (summm_p >= 508. .and. summm_p < 1016.) then
        wgn_pms(iwgn)%ireg = 2
      else if (summm_p >= 1016.) then
        wgn_pms(iwgn)%ireg = 3
      end if

      return
      end subroutine cli_initwgn