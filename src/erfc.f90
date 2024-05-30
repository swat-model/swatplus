      function hru_erfc(xx)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    erfc is the complementary error function; 
!!    erf(B) is the error function for B
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Abs

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      implicit none

      real, parameter :: c1 = .19684             !              |
      real, parameter :: c2 = .115194            !              |
      real, parameter :: c3 = .00034             !              |
      real, parameter :: c4 = .019527            !              |
      real, intent (in) :: xx                    !              |
      real :: x                                  !none          |variable to hold intermediate 
                                                 !              |calculation result
      real :: erf                                !              |
      real :: hru_erfc                           !              |

      x = 0.
      erf = 0.
      hru_erfc = 0.

      x = Abs(1.4142 * xx)
      erf = 1. - (1. + c1 * x + c2 * x * x + c3 * x**3 + c4 * x**4) **  (-4)
      if (xx < 0.) erf = -erf

      hru_erfc = 1. - erf

      return
      end function