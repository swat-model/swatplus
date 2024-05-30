      subroutine soil_text_init (isol)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine initializes soil parameters based on awc

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ddrain(:)     |mm            |depth to the sub-surface drain
!!    i             |none          |HRU number
!!    rock(:)       |%             |percent of rock fragments in soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    rock(:)       |none          |exponential value that is a function of
!!                                 |percent rock
!!    sol_st(:,:)   |mm H2O        |amount of water stored in the soil layer
!!                                 |on any given day (less wp water)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Sqrt
!!    SWAT: Curno

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use soil_module

      implicit none

      integer :: isol         !              |
      real :: sa              !ha            |surface area of impounded water body
      real :: cl              !              |
      real :: si              !m/n           |slope of main channel

      sa = soil(isol)%phys(1)%sand / 100.
      cl = soil(isol)%phys(1)%clay  / 100.
      si = soil(isol)%phys(1)%silt / 100.
      !! determine detached sediment size distribution
      !! typical for mid-western soils in USA (Foster et al., 1980 0 Based on SWRRB
       soil(isol)%det_san = sa * (1. - cl)** 2.49   !! Sand fraction
       soil(isol)%det_sil = 0.13 * si               !! Silt fraction
       soil(isol)%det_cla = 0.20 * cl               !! Clay fraction   
       if (cl < .25) then
         soil(isol)%det_sag = 2.0 * cl              !! Small aggregate fraction                    
       else if (cl > .5) then
         soil(isol)%det_sag = .57
       else
         soil(isol)%det_sag = .28 * (cl - .25) + .5
       end if

       soil(isol)%det_lag = 1. - soil(isol)%det_san -                 &                
          soil(isol)%det_sil - soil(isol)%det_cla - soil(isol)%det_sag  !! Large Aggregate fraction

      !! Error check. May happen for soils with more sand
      !! Soil not typical of mid-western USA The fraction wont add upto 1.0
	  if (soil(isol)%det_lag < 0.) then
	    soil(isol)%det_san = soil(isol)%det_san/(1 - soil(isol)%det_lag) 
	    soil(isol)%det_sil = soil(isol)%det_sil/(1 - soil(isol)%det_lag) 
	    soil(isol)%det_cla = soil(isol)%det_cla/(1 - soil(isol)%det_lag) 
	    soil(isol)%det_sag = soil(isol)%det_sag/(1 - soil(isol)%det_lag) 
	    soil(isol)%det_lag = 0.
      end if

      return
      end subroutine soil_text_init