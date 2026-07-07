      function cli_dstn1(rn1,rn2)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this function computes the distance from the mean of a normal 
!!    distribution with mean = 0 and standard deviation = 1, given two
!!    random numbers

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name       |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    rn1        |none          |first random number
!!    rn2        |none          |second random number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name       |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    dstn1      |              |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Sqrt, Log, Cos

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      implicit none
      
      real, intent (in) :: rn1     !none          |first random number
      real, intent (in) :: rn2     !none          |second random number 
      real :: cli_dstn1            !              |
      
      cli_dstn1 = Sqrt(-2. * Log(rn1)) * Cos(6.283185 * rn2)

      return
      end function