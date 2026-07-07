      real function expo (xx)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This function checks the argument against upper and lower 
!!    boundary values prior to taking the Exponential

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    expo        |none          |Exp(xx)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp
 
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      implicit none
 
      real :: xx                 !none          |Exponential argument
      real :: yy                 !              |   

      yy = xx

      if (yy < -20.) yy = -20.
      if (yy > 20.) yy = 20.

      expo = 0.
      expo = Exp(yy)       

      return
      end function