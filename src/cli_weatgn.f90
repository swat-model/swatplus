      subroutine cli_weatgn(iwgn)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine generates weather parameters used to simulate the impact
!!    of precipitation on the other climatic processes

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    rnd2(:)     |none          |random number between 0.0 and 1.0
!!    rnd8(:)     |none          |random number between 0.0 and 1.0
!!    rnd9(:)     |none          |random number between 0.0 and 1.0
!!    rndseed(:,:)|none          |random number generator seeds
!!    wgnold(:,:) |none          |previous value of wgncur(:)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    rnd2(:)     |none          |random number between 0.0 and 1.0
!!    rnd8(:)     |none          |random number between 0.0 and 1.0
!!    rnd9(:)     |none          |random number between 0.0 and 1.0
!!    wgncur(:,:) |none          |parameter to predict the impact of precip on
!!                               |other weather attributes
!!    wgnold(:,:) |none          |previous value of wgncur(:,:)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: Aunif, Dstn1

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use climate_module
      
      implicit none

      integer, dimension (2) :: zshape    !none          |array shape parameters
      integer :: n                        !none          |counter                                       
      integer :: l                        !none          |counter
      integer :: iwgn
      real, dimension (3,3) :: a          !none          |3 x 3 matrix whose elements are defined such
                                          !              |that the new sequences of max temp, min temp, 
                                          !              |and radiation have the desired serial-
                                          !              |correlation and cross-correlation coefficients
      real, dimension (3,3) :: b          !none          |3 x 3 matrix whose elements are defined such
                                          !              |that the new sequences of max temp, min temp, 
                                          !              |and radiation have the desired serial-
                                          !              |correlation and cross-correlation coefficients
      real, dimension (3) :: xx           !none          |variable to hold calculation value 
      real, dimension (3) :: e            !none          |3 x 1 matrix of independent random components
      real :: v2                          !none          |random number between 0.0 and 1.0
      real :: aunif                       !              |
      real :: cli_dstn1                   !              |
 
      zshape = (/3, 3/)
      a = Reshape((/.567, .253, -.006, .086, .504, -.039, -.002, -.050,     &
                  .244/), zshape)
      b = Reshape((/.781, .328, .238, 0., .637, -.341, 0., 0., .873/),      & 
                  zshape)
      xx = 0.
      e = 0.

!!    set random number array values
        v2 = 0.
        v2 = Aunif(rndseed(idg(8),iwgn))
        e(1) = cli_Dstn1(rnd8(iwgn),v2)    !! for max temp
        rnd8(iwgn) = v2
        v2 = 0.
        v2 = Aunif(rndseed(idg(9),iwgn))
        e(2) = cli_Dstn1(rnd9(iwgn),v2)    !! for min temp
        rnd9(iwgn) = v2
        v2 = 0.
        v2 = Aunif(rndseed(idg(2),iwgn))
        e(3) = cli_Dstn1(rnd2(iwgn),v2)    !! for radiation
        rnd2(iwgn) = v2

      do n = 1, 3
        wgncur(n,iwgn) = 0.
        do l = 1, 3
          wgncur(n,iwgn) = wgncur(n,iwgn) + b(n,l) * e(l)
          xx(n) = xx(n) + a(n,l) * wgnold(l,iwgn)
        end do
      end do

      do n = 1, 3
        wgncur(n,iwgn) = wgncur(n,iwgn) + xx(n)
        if (wgncur(n,iwgn) > 1.) wgncur(n,iwgn) = 1.
        if (wgncur(n,iwgn) < -1.) wgncur(n,iwgn) = -1.
        wgnold(n,iwgn) = wgncur(n,iwgn)
      end do

      return
      end subroutine cli_weatgn