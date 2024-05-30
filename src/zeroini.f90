      subroutine zeroini

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine zeros values for single array variables

      use hru_module, only : snocov1, volcrmin    
      use soil_module
      use time_module
      
      implicit none

      real :: snocov2             !none          |2nd shape parameter for snow cover equation
                                  !              |This parameter is determined by solving the
                                  !              |equation for 95% snow cover
      snocov1 = 0.
      snocov2 = 0.
      volcrmin = 0.
      return
      end subroutine zeroini