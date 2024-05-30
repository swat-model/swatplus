      subroutine zero2

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine zeros all array values

      use hru_module, only : clayld, lagyld, ovrlnd, sagyld, sanyld,  &
          sedyld, silyld, smx,surf_bs, twash,wrt, bss_ex

      implicit none

      real :: cklsp                 !                 |

      real :: zdb                   !mm               |division term from net pesticide equation

      cklsp = 0.

      ovrlnd = 0.

	  sedyld = 0.
	  sanyld = 0.
	  silyld = 0.
	  clayld = 0.
	  sagyld = 0.
	  lagyld = 0.
      smx = 0.
      surf_bs = 0.
      twash = 0.
      wrt = 0.
      zdb = 0.
      bss_ex = 0. !rtb gwflow
      
      return
      end