      subroutine sq_dailycn

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    Calculates curve number for the day in the HRU 

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    smx(:)      |none          |retention coefficient for cn method based on
!!                               |soil moisture
!!    wrt(1,:)    |none          |1st shape parameter for calculation of
!!                               |water retention
!!    wrt(2,:)    |none          |2nd shape parameter for calculation of
!!                               |water retention
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cnday(:)    |none          |curve number for current day, HRU and at 
!!                               |current soil moisture
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp


!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module
      use hru_module, only : cnday, wrt, smx, ihru
      use soil_module
      
      implicit none
 
      integer :: j       !none          |HRU number
      real :: r2         !none          |retention parameter in CN equation
      real :: sw_fac     !none          |variable used to store intermediate value of soil water factor

      j = ihru

      sw_fac = wrt(1,j) - wrt(2,j) * soil(j)%sw
      if (sw_fac < -20.) sw_fac = -20.
      if (sw_fac > 20.) sw_fac = 20.

      !! traditional CN method (function of soil water)
      if ((soil(j)%sw + Exp(sw_fac)) > 0.001) then
        r2 = smx(j) * (1. - soil(j)%sw / (soil(j)%sw + Exp(sw_fac)))
      else
        r2 = smx(j)
      end if

      if (soil(j)%phys(2)%tmp <= 0.) r2 = smx(j) * (1. - Exp(- bsn_prm%cn_froz * r2))
      r2 = Max(3.,r2)

      cnday(j) = 25400. / (r2 + 254.)
      
      return
      end subroutine sq_dailycn