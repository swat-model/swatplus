      subroutine gcycl

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine initializes the random number seeds. If the user
!!    desires a different set of random numbers for each simulation run,
!!    the random number generator is used to reset the values of the 
!!    seeds.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    idg(:)      |none          |array location of random number seed used
!!                               |for a given process
!!    rndseed(:,:)|none          |random number seeds. The seeds in the array
!!                               |are used to generate random numbers for the
!!                               |following purposes
!!                               |(1) wet/dry day probability
!!                               |(2) solar radiation
!!                               |(3) precipitation
!!                               |(4) USLE rainfall erosion index
!!                               |(5) wind speed
!!                               |(6) 0.5 hr rainfall fraction
!!                               |(7) relative humidity
!!                               |(8) maximum temperature
!!                               |(9) minimum temperature
!!                               |(10) generate new random numbers
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: Aunif

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use climate_module
      use basin_module
      use maximum_data_module
      use conditional_module
      
      implicit none 

      real :: xx           !none          |dummy variable to accept function value
                           !              |which is then discarded
      real :: rn           !none          |random number between 0.0 and 1.0
      integer :: ii        !none          |variable to hold calculated value 
      integer :: j         !none          |counter
      integer :: k         !none          |counter, and variable
      integer :: rndseed10 !none          |seed for random number generator that is 
                           !              |used to reset other random number seeds 
      integer :: iwgn      !none          |counter 
      real :: aunif        !              |
      
      
!!    initialize random number array locator
      idg = (/1,2,3,4,5,6,7,8,9/)

!!    initialize random number seeds
       
      do iwgn = 1, db_mx%wgnsta
        rndseed(1,iwgn) = 748932582
        rndseed(2,iwgn) = 1985072130
        rndseed(3,iwgn) = 1631331038
        rndseed(4,iwgn) = 67377721
        rndseed(5,iwgn) = 366304404
        rndseed(6,iwgn) = 1094585182
        rndseed(7,iwgn) = 1767585417
        rndseed(8,iwgn) = 1980520317
        rndseed(9,iwgn) = 392682216
      end do
      rndseed10 = 64298628

      if (bsn_prm%igen /= 0) then
        !! assign new random number seeds
        do j = 1, 9
           rn = 0.
           ii = 0
           rn = Aunif(rndseed10)
           ii = 100 * bsn_prm%igen * rn
           do k = 1, ii
             xx = Aunif(rndseed10)
           end do  
           rndseed(j,1) = rndseed10
        end do
       
        !! shuffle random number for conditional probablility
        !! assign random number for decision table conditional
        rndseed_cond = rndseed10
      
        !! shuffle seeds randomly (Bratley, Fox, Schrage, p34)
        do j = 9, 1, -1
          ii = 0
          rn = 0.
          ii = idg(j)
          rn = Aunif(rndseed10)
          k = j * rn + 1
          idg(j) = idg(k)
          idg(k) = ii
        end do
      end if

      !! assign half-hour maximum rainfall seed to second array location for use
      !! in sub-daily pcp generator
      do iwgn = 1, db_mx%wgnsta
        rndseed(10,iwgn) = rndseed(idg(6),iwgn)
      end do

      do iwgn = 1, db_mx%wgnsta
        rnd2(iwgn) = Aunif(rndseed(idg(2),iwgn))
        rnd3(iwgn) = Aunif(rndseed(idg(3),iwgn))
        rnd8(iwgn) = Aunif(rndseed(idg(8),iwgn))
        rnd9(iwgn) = Aunif(rndseed(idg(9),iwgn))
      end do

      return
      end subroutine gcycl