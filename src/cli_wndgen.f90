      subroutine cli_wndgen(iwgn)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine generates wind speed

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    idg(:)      |none          |array location of random number seed used
!!                               |for a given process
!!    j           |none          |HRU number
!!    rndseed(:,:)|none          |random number seeds
!!    wndav(:,:)  |m/s           |average wind speed for the month in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Log
!!    SWAT: Aunif
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use hydrograph_module
      use climate_module
      use time_module
      
      implicit none
      
      real :: v6                  !none          |random number between 0.0 and 1.0
      real :: v7                  !              |
      real :: rdir2               !              |
      real :: pi2                 !              |
      integer :: idir             !none          |counter
      integer :: idir1            !              |
      integer :: idir2            !              |
      integer :: mo               !              |
      integer :: iwgn             !              | 
      integer :: iwndir           !              |
      real :: aunif               !              |
      real :: g                   !              |

      pi2 = 6.283185
      mo = time%mo
      
      !! Generate wind speed !!
      v6 = Aunif(rndseed(idg(5),iwgn))
      wst(iwst)%weat%windsp = wgn(iwgn)%windav(time%mo) * (-Log(v6)) ** 0.3
      
      !! Generate wind direction !!
      !!! set to zero, no longer attempt to read in
      iwndir = 0
      if (iwndir > 0) then
      idir1 = 16
      idir2 = 15
      v7 = Aunif(rndseed(idg(5),iwgn))
      do idir = 1, 16
        if (wnd_dir(iwndir)%dir(mo,idir) > v7) then
          idir1 = idir
          idir2 = idir - 1
          exit
        end if
      end do
      if (idir1 == 1) then
        g = v7 / wnd_dir(iwndir)%dir(mo,idir1)
      else
        g = (v7 - wnd_dir(iwndir)%dir(mo,idir2)) /                        &
           (wnd_dir(iwndir)%dir(mo,idir1) - wnd_dir(iwndir)%dir(mo,idir2))
      end if

      rdir2 = float (idir2)
      wst(iwst)%weat%wndir = pi2 * (g + rdir2 - .5) / 16.
      wst(iwst)%weat%wndir = pi2 + wst(iwst)%weat%wndir
      end if
      
      return
      end subroutine cli_wndgen