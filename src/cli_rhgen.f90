      subroutine cli_rhgen(iwgn)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine generates weather relative humidity

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    dewpt(:,:)  |deg C         |average dew point temperature for the month
!!    idg(:)      |none          |array location of random number seed used
!!                               |for a given process
!!    j           |none          |HRU number
!!    pr_w(3,:,:) |none          |proportion of wet days in a month
!!    rndseed(:,:)|none          |random number seeds
!!    tmpmn(:,:)  |deg C         |avg monthly minimum air temperature
!!    tmpmx(:,:)  |deg C         |avg monthly maximum air temperature
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp
!!    SWAT: Atri, Ee

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use climate_module
      use hydrograph_module
      use time_module

      implicit none
      
      real :: vv                  !none          |variable to hold intermediate calculation 
      real :: rhm                 !none          |mean monthly relative humidity adjusted for
                                  !              |wet or dry condiditions
      real :: yy                  !none          |variable to hold intermediate calculation
      real :: uplm                !none          |highest relative humidity value allowed for
                                  !              |any day in month
      real :: blm                 !none          |lowest relative humidity value allowed for
                                  !              |any day in month
      real :: rhmo                !none          |mean monthly relative humidity
      real :: tmpmean             !deg C         |average temperature for the month in HRU
      real :: atri                !none          |daily value generated for distribution
      real :: ee                  !              |
      integer :: iwgn             !              |
      

      !! Climate Paramenters required for Penman-Monteith
      
      !! convert dewpoint to relative humidity (idewpt == 0)
      if (wgn_pms(iwgn)%idewpt == 0) then
        tmpmean = (wgn(iwgn)%tmpmx(time%mo) + wgn(iwgn)%tmpmn(time%mo)) / 2.
        rhmo = Ee(wgn(iwgn)%dewpt(time%mo)) / Ee(tmpmean)
      else
        rhmo = wgn(iwgn)%dewpt(time%mo)
      end if

      yy = 0.9 * wgn_pms(iwgn)%pr_wdays(time%mo)
      rhm = (rhmo - yy) / (1.0 - yy)
      if (rhm < 0.05) rhm = 0.5 * rhmo
      if (wst(iwst)%weat%precip > 0.0) rhm = rhm * 0.1 + 0.9
      vv = rhm - 1.
      uplm = rhm - vv * Exp(vv)
      blm = rhm * (1.0 - Exp(-rhm))
      wst(iwst)%weat%rhum = Atri(blm,rhm,uplm,rndseed(idg(7),iwgn))

      return
      end subroutine cli_rhgen