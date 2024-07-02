      function atri(at1,at2,at3,at4i)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this function generates a random number from a triangular distribution
!!    given X axis points at start, end, and peak Y value

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Sqrt
!!    SWAT: Aunif

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      
      implicit none

      real, intent (in) :: at1           !none          |lower limit for distribution
      real, intent (in) :: at2           !none          |monthly mean for distribution
      real, intent (in) :: at3           !none          |upper limit for distribution
      integer, intent (in out) :: at4i   !none          |random number seed 
      real :: u3                         !              | 
      real :: rn                         !none          |random number between 0.0 and 1.0
      real :: y                          !              | 
      real :: b1                         !              | 
      real :: b2                         !              | 
      real :: x1                         !              | 
      real :: xx                         !              | 
      real :: yy                         !              | 
      real :: amn                        !              | 
      real :: atri                       !none          |daily value generated for distribution
      real :: aunif                      !              | 

      u3 = 0.
      rn = 0.
      y = 0.
      b1 = 0.
      b2 = 0.
      x1 = 0.
      xx = 0.
      yy = 0.
      amn = 0.

      u3 = at2 - at1
      rn = Aunif(at4i)
      y = 2.0 / (at3 - at1)
      b2 = at3 - at2
      b1 = rn / y
      x1 = y * u3 / 2.0

      if (rn <= x1) then
        xx = 2.0 * b1 * u3
        if (xx <= 0.) then
          yy = 0.
        else
          yy = Sqrt(xx)
        end if
        atri = yy + at1
      else
        xx = b2 * b2 - 2.0 * b2 * (b1 - 0.5 * u3)
        if (xx <= 0.) then
          yy = 0.
        else
          yy = Sqrt(xx)
        end if
        atri = at3 - yy
      end if

      amn = (at3 + at2 + at1) / 3.0
      atri = atri * at2 / amn

      if (atri >= 1.0) atri = 0.99
      if (atri <= 0.0) atri = 0.001

      return
      end