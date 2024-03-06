      function qman(x1,x2,x3,x4)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates flow rate or flow velocity using Manning"s
!!    equation. If x1 is set to 1, the velocity is calculated. If x1 is set to
!!    cross-sectional area of flow, the flow rate is calculated.
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Sqrt

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      implicit none 

      real, intent (in) :: x1       !m^2 or none   |cross-sectional flow area or 1.
      real, intent (in) :: x2       !m             |hydraulic radius
      real, intent (in) :: x3       !none          |Manning"s "n" value for channel
      real, intent (in) :: x4       !m/m           |average slope of channel
      real :: qman                  !m^3/s or m/s  |flow rate or flow velocity

      qman = x1 * x2 ** .6666 * Sqrt(x4) / (x3 + .001)

      return
      end