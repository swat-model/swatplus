      subroutine cli_slrgen(iwgn)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine generates solar radiation

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    j           |none          |HRU number
!!    pr_w(3,:,:) |none          |proportion of wet days in a month
!!    wgncur(3,:) |none          |parameter which predicts impact of precip on
!!                               |daily solar radiation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use hydrograph_module
      use climate_module
      
      implicit none

      real :: rx                     !none          |variable to hold intermediate calculation
      real ::  rav                   !MJ/m^2        |modified monthly average solar radiation
      integer :: iwgn                !              |

      rav = wgn(iwgn)%solarav(time%mo) / (1. - 0.5 * wgn_pms(iwgn)%pr_wdays(time%mo))
      if (wst(iwst)%weat%precip > 0.0) rav = 0.5 * rav
      rx = wst(iwst)%weat%solradmx - rav
      wst(iwst)%weat%solrad = rav + wgncur(3,iwgn) * rx / 4.
      if (wst(iwst)%weat%solrad <= 0.) wst(iwst)%weat%solrad = .05 * wst(iwst)%weat%solradmx
      
      wst(iwst)%weat%solrad = wgn(iwgn)%solarav(time%mo)
      return
      end subroutine cli_slrgen