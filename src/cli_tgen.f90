      subroutine cli_tgen(iwgn)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine generates temperature data when the user chooses to 
!!    simulate or when data is missing for particular days in the
!!    weather file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    j           |none          |HRU number
!!    pr_w(3,:,:) |none          |proportion of wet days in month
!!    tmpmn(:,:)  |deg C         |avg monthly minimum air temperature
!!    tmpmx(:,:)  |deg C         |avg monthly maximum air temperature
!!    tmpstdmn(:,:)|deg C        |standard deviation for avg monthly minimum air
!!                               |temperature
!!    tmpstdmx(:,:)|deg C        |standard deviation for avg monthly maximum air
!!                               |temperature
!!    wgncur(1,:) |none          |parameter which predicts impact of precip on
!!                               |daily maximum air temperature
!!    wgncur(2,:) |none          |parameter which predicts impact of precip on
!!                               |daily minimum air temperature
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Abs

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use climate_module
      use hydrograph_module
      use time_module
      
      implicit none

      real :: tmxg               !deg C         |generated maximum temperature for the day
      real :: tmng               !deg C         |generated minimum temperature for the day
      real :: tamp               !deg C         |difference between mean monthly air temperature
                                 !              |and monthly max or min temperature
      real :: txxm               !deg C         |modified monthly maximum temperature
      integer :: iwgn            !              |

      tamp = .5 * (wgn(iwgn)%tmpmx(time%mo) - wgn(iwgn)%tmpmn(time%mo))
      txxm = wgn(iwgn)%tmpmx(time%mo) + tamp * wgn_pms(iwgn)%pr_wdays(time%mo)
      
      if (wst(iwst)%weat%precip > 0.0) txxm = txxm - tamp

      tmxg = txxm + wgn(iwgn)%tmpstdmx(time%mo) * wgncur(1,iwgn)
      tmng = (wgn(iwgn)%tmpmn(time%mo)) + wgn(iwgn)%tmpstdmn(time%mo) *  wgncur(2,iwgn)
      if (tmng > tmxg) tmng = tmxg - .2 * Abs(tmxg)

      wst(iwst)%weat%tmax = tmxg
      wst(iwst)%weat%tmin = tmng

      return
      end subroutine cli_tgen