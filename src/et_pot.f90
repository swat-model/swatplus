      subroutine et_pot
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates potential evapotranspiration using one
!!    of three methods. If Penman-Monteith is being used, potential plant
!!    transpiration is also calculated.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name       |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    albday     |none           |albedo for the day in HRU
!!    gsi(:)     |m/s            |maximum stomatal conductance
!!    ihru       |none           |HRU number
!!    vpd2(:)    |(m/s)*(1/kPa)  |rate of decline in stomatal conductance per
!!               |               |unit increase in vapor pressure deficit
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ep_max      |mm H2O        |maximum amount of transpiration (plant et) 
!!                               |that can occur on current day in HRU
!!    pet_day     |mm H2O        |potential evapotranspiration on current day in
!!                               |HRU
!!    vpd         |kPa           |vapor pressure deficit
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Log, Sqrt, Max, Min
!!    SWAT: Ee

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use plant_data_module
      use basin_module
      use hydrograph_module
      use climate_module
      use hru_module, only : hru, ihru, albday, ipl, pet_day, vpd, ep_max
      use plant_module
      
      implicit none
      
      integer :: j                 !none          |HRU number            
      integer :: idp               !              |
      integer :: iob               !              |
      real :: tk                   !deg K         |average air temperature on current day for HRU
      real :: pb                   !kPa           |mean atmospheric pressure
      real :: gma                  !kPa/deg C     |psychrometric constant
      real :: xl                   !MJ/kg         |latent heat of vaporization
      real :: ea                   !kPa           |saturated vapor pressure
      real :: ed                   !              |
      real :: dlt                  !kPa/deg C     |slope of the saturation vapor pressure-
                                   !              |temperature curve 
      real :: ramm                 !MJ/m2         |extraterrestrial radiation
      real :: ralb1                !MJ/m2         |net incoming radiation
      real :: ralb                 !MJ/m2         |net incoming radiation for PET
      real :: xx                   !kPa           |difference between vpd and vpthreshold
      real :: rbo                  !none          |net emissivity
      real :: rto                  !none          |cloud cover factor 
      real :: rn                   !MJ/m2         |net radiation
      real :: uzz                  !m/s           |wind speed at height zz
      real :: zz                   !cm            |height at which wind speed is determined
      real :: zom                  !cm            |roughness length for momentum transfer
      real :: zov                  !cm            |roughness length for vapor transfer
      real :: rv                   !s/m           |aerodynamic resistance to sensible heat and
                                   !              |vapor transfer
      real :: rn_pet               !MJ/m2         |net radiation for continuous crop cover 
      real :: fvpd                 !kPa           |amount of vapor pressure deficit over 
                                   !              |threshold value
      real :: rc                   !s/m           |canopy resistance
      real :: rho                  !MJ/(m3*kPa)   |K1*0.622*xl*rho/pb
      real :: rout                 !MJ/m2         |outgoing radiation
      real :: d                    !cm            |displacement height for plant type
      real :: chz                  !cm            |vegetation height
      real :: gsi_adj              !              |
      real :: pet_alpha            !none          |alpha factor in Priestley-Taylor PET equation
      real :: ee                   !              | 
      real ::  gsi_wav             !              | 
      integer :: igrocom           !              | 

      !! initialize local variables
      j = ihru

      tk = w%tave + 273.15

      !! calculate mean barometric pressure
      pb = 101.3 - hru(j)%topo%elev * (0.01152 - 0.544e-6 * hru(j)%topo%elev)

      !! calculate latent heat of vaporization
      xl = 2.501 - 2.361e-3 * w%tave

      !! calculate psychrometric constant
      gma = 1.013e-3 * pb / (0.622 * xl)

      !! calculate saturation vapor pressure, actual vapor pressure and
      !! vapor pressure deficit
      ea = Ee(w%tave)
      ed = ea * w%rhum
      vpd = ea - ed

      !!calculate the slope of the saturation vapor pressure curve
      dlt = 4098. * ea / (w%tave + 237.3)**2
	
!! DETERMINE POTENTIAL ET

      select case (bsn_cc%pet)

       case (0)   !! PRIESTLEY-TAYLOR POTENTIAL EVAPOTRANSPIRATION METHOD
     
       !! net radiation
         !! calculate net short-wave radiation for PET
          if (hru(j)%sno_mm <= .5) then
            ralb = w%solrad * (1.0 - 0.23)
          else
            ralb = w%solrad * (1.0 - 0.8)
          end if

        !! calculate net long-wave radiation

          !! net emissivity  equation 2.2.20 in SWAT manual
          rbo = -(0.34 - 0.139 * Sqrt(ed))

          !! cloud cover factor equation 2.2.19
            if (w%solradmx < 1.e-4) then
		    rto = 0.
            else
              rto = 0.9 * (w%solrad / w%solradmx) + 0.1
            end if

          !! net long-wave radiation equation 2.2.21
          rout = rbo * rto * 4.9e-9 * (tk**4)

          !! calculate net radiation
          rn_pet = ralb + rout
          
          !! net radiation
          pet_alpha = 1.28
          pet_day = pet_alpha * (dlt / (dlt + gma)) * rn_pet / xl
          pet_day = Max(0., pet_day)

       case (1)   !! PENMAN-MONTEITH POTENTIAL EVAPOTRANSPIRATION METHOD

       !! net radiation
         !! calculate net short-wave radiation for PET
          if (hru(j)%sno_mm <= .5) then
            ralb = w%solrad * (1.0 - 0.23) 
          else
            ralb = w%solrad * (1.0 - 0.8) 
          end if
         !! calculate net short-wave radiation for max plant ET
          ralb1 = w%solrad * (1.0 - albday) 

         !! calculate net long-wave radiation
          !! net emissivity  equation 2.2.20 in SWAT manual
          rbo = -(0.34 - 0.139 * Sqrt(ed))

          !! cloud cover factor equation 2.2.19
          if (w%solradmx  < 1.e-4) then
            rto = 0.
          else
            rto = 0.9 * (w%solrad / w%solradmx) + 0.1
          end if

          !! net long-wave radiation equation 2.2.21
          rout = rbo * rto * 4.9e-9 * (tk**4)

          !! calculate net radiation
          rn = ralb1 + rout
          rn_pet = ralb + rout
          !! net radiation

          rho = 1710. - 6.85 * w%tave

          if (w%windsp < 0.01) w%windsp = 0.01

           !! potential ET: reference crop alfalfa at 40 cm height
           rv = 114. / (w%windsp * (170./1000.)**0.2)
           rc = 49. / (1.4 - 0.4 * co2y(time%yrs) / 330.)
           pet_day = (dlt * rn_pet + gma * rho * vpd / rv) / (xl * (dlt + gma * (1. + rc / rv)))
           pet_day = Max(0., pet_day)
 
        !! maximum plant ET
          igrocom = 0
          do ipl = 1, pcom(j)%npl
            if (pcom(j)%plcur(ipl)%gro == "y") igrocom = 1
          end do
          if (igrocom <= 0) then
            ep_max = 0.0
          else
            !! determine wind speed and height of wind speed measurement
            !! adjust to 100 cm (1 m) above canopy if necessary
            if (pcom(j)%cht_mx <= 1.0) then
              zz = 170.0
            else
              zz = pcom(j)%cht_mx * 100. + 100.
            end if
            uzz = w%windsp * (zz/1000.)**0.2

            !! calculate canopy height in cm
            if (pcom(j)%cht_mx < 0.01) then
              chz = 1.
            else
              chz = pcom(j)%cht_mx * 100.
            end if

            !! calculate roughness length for momentum transfer
            if (chz <= 200.) then
              zom = 0.123 * chz
            else
              zom = 0.058 * chz**1.19
            end if
 
            !! calculate roughness length for vapor transfer
            zov = 0.1 * zom

            !! calculate zero-plane displacement of wind profile
            d = 0.667 * chz

            !! calculate aerodynamic resistance
            rv = Log((zz - d) / zom) * Log((zz - d) / zov)
            rv = rv / ((0.41)**2 * uzz)

            !! adjust stomatal conductivity for low vapor pressure
            !! this adjustment will lower maximum plant ET for plants
            !! sensitive to very low vapor pressure
            gsi_wav = 0.
            do ipl = 1, pcom(j)%npl
              idp = pcom(j)%plcur(ipl)%idplt
              rto = pcom(j)%plg(ipl)%lai / (pcom(j)%lai_sum + 0.01)
              xx = vpd - 1.
              if (xx > 0.0) then
                fvpd = Max(0.1,1.0 - plcp(idp)%vpd2 * xx)
              else
                fvpd = 1.0
              end if
              gsi_adj = pldb(idp)%gsi * fvpd
              gsi_wav = gsi_wav + gsi_adj * rto
            end do
          
            !! calculate canopy resistance
            rc = 1. / (gsi_adj + 0.01)           !single leaf resistance
            rc = rc / (0.5 * (pcom(j)%lai_sum + 0.01) * (1.4 - 0.4 * co2y(time%yrs) / 330.))

            !! calculate maximum plant ET
            ep_max = (dlt * rn + gma * rho * vpd / rv) / (xl * (dlt + gma * (1. + rc / rv)))
            if (ep_max < 0.) ep_max = 0.
            ep_max = Min(ep_max, pet_day)
          end if
       
       case (2)   !! HARGREAVES POTENTIAL EVAPOTRANSPIRATION METHOD

        !! extraterrestrial radiation
        !! 37.59 is coefficient in equation 2.2.6 !!extraterrestrial
        !! 30.00 is coefficient in equation 2.2.7 !!max at surface
        ramm = w%solradmx  * 37.59 / 30. 

        if (w%tmax > w%tmin) then
         pet_day = 0.0023 * (ramm / xl) * (w%tave + 17.8) * (w%tmax - w%tmin)**0.5
         pet_day = Max(0., pet_day)
        else
          pet_day = 0.
        endif
      
       case (3)  !! READ IN PET VALUES
        iob = hru(j)%obj_no
        iwst = ob(iob)%wst
        pet_day = wst(iwst)%weat%pet
  
      end select
       
      pet_day = hru(j)%hyd%pet_co * pet_day

      return
      end subroutine et_pot