      function ee(tk)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This function calculates saturation vapor pressure at a given 
!!    air temperature. 
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      implicit none

      real, intent (in) :: tk        !deg C         |mean air temperature
      real :: ee                     !kPa           |saturation vapor pressure

      ee = 0.
      if (tk + 237.3 /= 0.) then
        ee = (16.78 * tk - 116.9) / (tk + 237.3)
        ee = Exp(ee)
      end if

      return
      end                                               