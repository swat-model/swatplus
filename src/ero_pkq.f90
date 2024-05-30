      subroutine ero_pkq
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes the peak runoff rate for each HRU
!!    and the entire subbasin using a modification of the rational formula

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hru_km(:)    |km^2          |area of HRU in square kilometers
!!    ihru         |none          |HRU number
!!    tconc(:)     |hr            |time of concentration for HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    qp_cms       |m^3/s         |peak runoff rate
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Log, Exp

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use hru_module, only: hru, tconc, ihru, qp_cms, qday
      use hydrograph_module
      use climate_module
      use basin_module
      
      implicit none

      integer :: j      !none          |HRU number
      real :: altc      !              |
      real :: qp_cfs    !ft3/s         |peak flow rate    
      integer :: iob    !              | 
      real :: xx
      
      j = ihru
      iob = hru(j)%obj_no
      iwst = ob(iob)%wst
      
      xx = (2. * tconc(j) * Log(1. - wst(iwst)%weat%precip_half_hr))
      altc = 1. - exp(xx)
      qp_cms = altc * qday / tconc(j)           !! mm/h
      qp_cms = qp_cms * hru(j)%km / 3.6          !! m^3/s
      
      !! NRCS dimensionless hydrograph with PRF
      !! convert ha-mm * mi2/259ha * in/25.4mm to mi2-in --> 1/6578.6
      qp_cfs = bsn_prm%prf / 6578.6 * hru(j)%area_ha * qday / tconc(j)
      qp_cms = qp_cfs / 35.3

      return
      end subroutine ero_pkq