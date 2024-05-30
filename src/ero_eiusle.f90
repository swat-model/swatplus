      subroutine ero_eiusle
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine computes the USLE erosion index (EI)

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    idg(:)      |none        |array location of random number seed
!!                             |used for a given process
!!    ihru        |none        |HRU number
!!    rndseed(:,:)|none        |random number generator seed
!!    snomlt      |mm H2O      |amount of snow melt in HRU on current day
!!    tconc(:)    |hr          |time of concentration
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units                  |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    usle_ei     |100(ft-tn in)/(acre-hr)|USLE rainfall erosion index
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    xa          |none        |fraction of daily rainfall occuring during
!!                             |half-hour of max intensity rain
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Log, Log10

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use climate_module
      use hydrograph_module
      use hru_module, only : hru, usle_ei, usle_eifac, ihru,   &
        usle_ei, usle_eifac
      
      implicit none

      integer :: j         !none        |HRU number
      real :: xb           !none        |intermediate calculation
      real ::pkrf          !none        |intermediate calculation
      real ::pkrf30        !mm/hr       |maximum 30-min. storm intensity
      integer :: iob       !            |
     

      j = ihru
      iob = hru(j)%obj_no
      iwst = ob(iob)%wst

      if (w%precip > 1.e-4) then
        xb = -2. * Log(1. - wst(iwst)%weat%precip_half_hr)
        pkrf30 = 2. * w%precip * wst(iwst)%weat%precip_half_hr
        pkrf = xb * w%precip
        usle_ei = w%precip * (12.1 + 8.9 * (Log10(pkrf) - .4343)) * pkrf30 / 1000.
        if (usle_ei < 1.e-4) usle_ei = 0.
        usle_eifac(j) = usle_ei
      endif

      return
      end subroutine ero_eiusle