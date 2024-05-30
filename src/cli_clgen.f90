      subroutine cli_clgen(iwgn)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the daylength, distribution of 
!!    radiation throughout the day and maximum radiation for day

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    j           |none          |HRU number
!!    latcos(:)   |none          |Cos(Latitude) for HRU
!!    latsin(:)   |none          |Sin(Latitude) for HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    frad(:,:)   |none          |fraction of solar radiation occuring during 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sdlat       |none          |(-Tan(sd)*Tan(lat))
!!    cosrho(:)   |none          |Cos(zenith angle for hour)
!!    dd          |none          |relative distance of the earth from the sun
!!    h           |none          |Acos(-Tan(sd)*Tan(lat))
!!    ii          |none          |counter
!!    sd          |radians       |solar declination: latitude at which the sun
!!                               |is directly overhead at noon
!!    totrho      |none          |sum of cosrho values for all hours of day
!!    yc          |none          |Cos(sd)*Cos(lat)
!!    ys          |none          |Sin(sd)*Sin(lat)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Sin, Tan, Acos, Cos, Real
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module
      use climate_module
      use time_module
      use hydrograph_module
      
      implicit none

      integer :: ii               !none          |counter
      integer :: iwgn             !units         |description 
      real :: sd                  !radians       |solar declination: latitude at which the sun
                                  !              |is directly overhead at noon
      real :: sdlat               !none          |(-Tan(sd)*Tan(lat))
      real :: h                   !none          |Acos(-Tan(sd)*Tan(lat))
      real :: ys                  !none          |Sin(sd)*Sin(lat) 
      real :: yc                  !none          |Cos(sd)*Cos(lat)
      real :: dd                  !none          |relative distance of the earth from the sun
      real :: cosrho(time%step)   !none          |Cos(zenith angle for hour)
      real :: totrho              !none          |sum of cosrho values for all hours of day
      real :: hr_angle            !none          |hour angle

      !! Reset prior day category for precipitation     
      if (wst(iwst)%weat%precip >= 0.1) then
        wst(iwst)%weat%precip_prior_day = "wet"
      else
        wst(iwst)%weat%precip_prior_day = "dry"
      end if

      !! Calculate Daylength
      !! calculate solar declination: equation 2.1.2 in SWAT manual
      sd = Asin(.4 * Sin((Real(time%day) - 82.) / 58.09))  !!365/2pi = 58.09

      !! calculate the relative distance of the earth from the sun the eccentricity of the orbit
      dd = 1.0 + 0.033 * Cos(Real(time%day) / 58.09)

      !!daylength = 2 * Acos(-Tan(sd) * Tan(lat)) / omega
      !!where the angular velocity of the earth"s rotation, omega, is equal
      !! to 15 deg/hr or 0.2618 rad/hr and 2/0.2618 = 7.6374

      sdlat = -wgn_pms(iwgn)%latsin * Tan(sd) / wgn_pms(iwgn)%latcos
      
      if (sdlat > 1.) then    !! sdlat will be >= 1. if latitude exceeds +/- 66.5 deg in winter
        h = 0.
      elseif (sdlat >= -1.) then
        h = Acos(sdlat)
      else
        h = 3.1416         !! latitude exceeds +/- 66.5 deg in summer
      endif 
      wst(iwst)%weat%daylength = 7.6394 * h
          
      !! Calculate Potential (maximum) Radiation !!
      !! equation 2.2.7 in SWAT manual
      ys = wgn_pms(iwgn)%latsin * Sin(sd)
      yc = wgn_pms(iwgn)%latcos * Cos(sd)
      wst(iwst)%weat%solradmx = 30. * dd * (h * ys + yc * Sin(h))

      !! Calculate fraction of radiation recieved during each hour in day
      !! this calculation assumes solar noon (when the angle between the
      !! observer on the earth to the sun and a line normal to the earth"s
      !! at that position is at a minimum) falls at 12 noon in day.
      !! equation 2.2.10 in SWAT manual
      
      cosrho = 0.
      totrho = 0.
      do ii = 1, time%step
        !!angular velocity times hour away from solar noon. To calculate
        !!radiation for an hour, the hour angle for the midpoint of the 
        !!time period is used. time = 0. at solar noon with positive values
        !! in the morning and negative in the evening
        hr_angle = (12.5 - Real(ii)) * 0.2618 * time%dtm / 60.   !!0.2618 rad/hr
        cosrho(ii) = ys + yc * Cos(hr_angle)
        if (cosrho(ii) <= 0.) cosrho(ii) = 0.
        totrho = totrho + cosrho(ii)
      end do
      if (totrho > 0.001) then
        do ii = 1, time%step
          frad(iwgn,ii) = cosrho(ii) / totrho
        end do
      end if

      return
      end subroutine cli_clgen