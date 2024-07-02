      subroutine curno(cnn,h)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine determines the curve numbers for moisture conditions
!!    I and III and calculates coefficents and shape parameters for the 
!!    water retention curve
!!    the coefficents and shape parameters are calculated by one of two methods:
!!    the default method is to make them a function of soil water, the 
!!    alternative method (labeled new) is to make them a function of 
!!    accumulated PET, precipitation and surface runoff.
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    smx(:)      |none          |retention coefficient for cn method based on
!!                               |soil moisture
!!    wrt(1,:)    |none          |1st shape parameter for calculation of 
!!                               |water retention
!!    wrt(2,:)    |none          |2nd shape parameter for calculation of 
!!                               |water retention
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition   
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Max
!!    SWAT: ascrv

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use time_module
      use hru_module, only : cn2, hru, smx, wrt
      use soil_module
      
      implicit none
   
      integer, intent (in) :: h            !none          |HRU number
      real, intent (in) :: cnn             !cnn           |none          |SCS runoff curve number for moisture condition II
      real :: c2                           !none          |variable used to hold calculated value 
      real :: cn1                          !none          |SCS runoff curve number for moisture condition I
      real :: cn3                          !none          |SCS runoff curve number for moisture condition III
      real :: s3                           !none          |retention parameter for CN3
      real :: rto3                         !none          |fraction difference between CN3 and CN1 retention parameters
      real :: rtos                         !none          |fraction difference between CN=99 and CN1 retention parameters
      real :: smxold                       !              | 
      real :: sumul                        !mm H2O        |amount of water held in soil profile at saturation
      real :: sumfc                        !mm H2O        |amount of water held in the soil profile at field capacity
      real :: max                        !              |
      real :: amin1                        !              |
      
      cn2(h) = cnn
      
      !! calculate moisture condition I and III curve numbers
      c2 = 100. - cnn
      cn1 = cnn - 20. * c2 / (c2 + Exp(2.533 - 0.0636 * c2))
      cn1 = Max(cn1, .4 * cnn)
      smxold = 254.* (100. / cn1 - 1.)
      cn3 = cnn * Exp(.006729 * c2)

      !! calculate maximum retention parameter value
      smx(h) = 254. * (100. / cn1 - 1.)

      !! calculate retention parameter value for CN3
      s3 = 254. * (100. / cn3 - 1.)

      !! calculate fraction difference in retention parameters
      rto3 = 1. - s3 / smx(h)
      rtos = 1. - 2.54 / smx(h)
      sumul = soil(h)%sumul
      sumfc = soil(h)%sumfc + hru(h)%hyd%cn3_swf * (soil(h)%sumul - soil(h)%sumfc)
      if (sumfc < 0.) then  !***jga
        sumfc = 10.
      end if
      sumfc = max (.05, sumfc)
      sumfc = amin1 (sumul-.05, sumfc)
      !! calculate shape parameters
      call ascrv(rto3, rtos, sumfc, sumul, wrt(1,h), wrt(2,h))

      return
      end subroutine curno