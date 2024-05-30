      subroutine surface

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine models surface hydrology at any desired time step

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ovrlnd(:)   |mm H2O        |overland flow onto HRU from upstream
!!                               |routing unit
!!    qp_cms      |m3/sec        |peak runoff rate
!!    surfq(:)    |mm H2O        |surface runoff generated in HRU during
!!                               |the day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max
!!    SWAT: canopyint, snom, crackvol, dailycn, volq, crackflow, surfst_h2o,
!!    SWAT: alph, pkq, tran, eiusle, ysed

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      
      use basin_module
      use time_module
      use hydrograph_module
      use hru_module, only : hru, surfq, ihru,    &
        qp_cms, precip_eff, qday                  !rtb gwflow
      use soil_module
      use urban_data_module
      use output_landscape_module
      
      implicit none

      integer :: j                !none          |HRU number 
      real :: ulu                 !              |
      real :: hruirrday           !              |
      integer :: irmmdt           !              |

      j = ihru
      ulu = hru(j)%luse%urb_lu
      hruirrday = 0.
      irmmdt = 0.

      !!calculate subdaily curve number value
      call sq_dailycn

      !! compute runoff - surfq in mm H2O
      if (precip_eff > 0.1) then
         call sq_volq 

        !! adjust runoff for loss into crack volume
         if (surfq(j) > 0. .and. bsn_cc%crk == 1) call sq_crackflow
      end if

      !! add irrigation runoff and surface runon runoff
      surfq(j) = surfq(j) + irrig(j)%runoff
      irrig(j)%runoff = 0.

      !! calculate amount of surface runoff reaching main channel during day
      !! (qday) and store the remainder
      !call sq_surfst
      qday =  surfq(j)

      if (qday > 1.e-6) then
        !! compute peak rate - qp_cms in m3/s  
        call ero_pkq 
      end if  

      if (qday > 1.e-6 .and. qp_cms > 1.e-6) then
        call ero_eiusle

	!! calculate sediment erosion by rainfall and overland flow
		call ero_ovrsed
      end if

      call ero_cfactor
      if (surfq(j) > 1.e-6 .and. qp_cms > 1.e-6) call ero_ysed

      if (qday < 0.) qday = 0.

1010  format (2(i4,1x),a5,a4,1x,10f8.3)
      return
      end subroutine surface