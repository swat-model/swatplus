      subroutine soil_awc_init (isol)

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
      use basin_module
      use time_module
      
      implicit none

      integer :: ly           !none          |soil layer counter
      integer :: nly          !none          |number of soil layers
      real :: sumpor          !mm            |porosity of profile
      real :: pormm           !mm            |porosity in mm depth
      integer :: isol         !              |   
      real :: drpor           !              |
      real :: depth_prev      !              |
      
      !! reset soil parameters based on awc
      nly = soil(isol)%nly
      do ly = 1, nly
        if (soil(isol)%phys(ly)%awc <= 1.e-6) soil(isol)%phys(ly)%awc = .005
        if (soil(isol)%phys(ly)%awc >= .8) soil(isol)%phys(ly)%awc = .8
        
        !! calculate water content of soil at -1.5 MPa and -0.033 MPa
        soil(isol)%phys(ly)%wp = 0.4 * soil(isol)%phys(ly)%clay * soil(isol)%phys(ly)%bd / 100.
        if (soil(isol)%phys(ly)%wp <= 0.) soil(isol)%phys(ly)%wp = .005
        soil(isol)%phys(ly)%up = soil(isol)%phys(ly)%wp + soil(isol)%phys(ly)%awc
        soil(isol)%phys(ly)%por = 1. - soil(isol)%phys(ly)%bd / 2.65
        if (soil(isol)%phys(ly)%up >= soil(isol)%phys(ly)%por) then
           soil(isol)%phys(ly)%up = soil(isol)%phys(ly)%por - .05
           soil(isol)%phys(ly)%wp = soil(isol)%phys(ly)%up - soil(isol)%phys(ly)%awc
          if (soil(isol)%phys(ly)%wp <= 0.) then
            soil(isol)%phys(ly)%up = soil(isol)%phys(ly)%por * .75
            soil(isol)%phys(ly)%wp = soil(isol)%phys(ly)%por * .25
          end if
        end if
        !! compute drainable porosity and variable water table factor - Daniel
        drpor = soil(isol)%phys(ly)%por - soil(isol)%phys(ly)%up
        soil(isol)%ly(ly)%vwt = (437.13*drpor**2)-(95.08 * drpor)+8.257
      end do

      !! initialize water/drainage coefs for each soil layer
      depth_prev = 0.
      sumpor = 0.
      soil(isol)%sumfc = 0.
      soil(isol)%sumul = 0.
      soil(isol)%sw = 0.
      soil(isol)%sumwp = 0.
      
      do ly = 1, nly
        soil(isol)%phys(ly)%thick = soil(isol)%phys(ly)%d - depth_prev
        pormm = soil(isol)%phys(ly)%por * soil(isol)%phys(ly)%thick
        sumpor = sumpor + pormm
        soil(isol)%phys(ly)%ul = (soil(isol)%phys(ly)%por - soil(isol)%phys(ly)%wp) * soil(isol)%phys(ly)%thick
        soil(isol)%sumul = soil(isol)%sumul + soil(isol)%phys(ly)%ul
        soil(isol)%phys(ly)%fc = soil(isol)%phys(ly)%thick * (soil(isol)%phys(ly)%up - soil(isol)%phys(ly)%wp)
        soil(isol)%sumfc = soil(isol)%sumfc + soil(isol)%phys(ly)%fc
        soil(isol)%phys(ly)%st = soil(isol)%phys(ly)%fc * soil(isol)%ffc
        soil(isol)%phys(ly)%hk = (soil(isol)%phys(ly)%ul - soil(isol)%phys(ly)%fc) / soil(isol)%phys(ly)%k
        if (soil(isol)%phys(ly)%hk < 1.) soil(isol)%phys(ly)%hk = 1.
        soil(isol)%sw = soil(isol)%sw + soil(isol)%phys(ly)%st
        soil(isol)%phys(ly)%wpmm = soil(isol)%phys(ly)%wp * soil(isol)%phys(ly)%thick
        soil(isol)%sumwp = soil(isol)%sumwp + soil(isol)%phys(ly)%wpmm
        soil(isol)%phys(ly)%crdep = soil(isol)%crk * 0.916 * Exp(-.0012 * soil(isol)%phys(ly)%d) * soil(isol)%phys(ly)%thick
        soil(isol)%ly(ly)%volcr = soil(isol)%phys(ly)%crdep * (soil(isol)%phys(ly)%fc - soil(isol)%phys(ly)%st) / &
            (soil(isol)%phys(ly)%fc)
        depth_prev = soil(isol)%phys(ly)%d
      end do
      !! initialize water table depth and soil water for Daniel
      soil(isol)%swpwt = soil(isol)%sw
      if (soil(isol)%ffc > 1.) then
        soil(isol)%wat_tbl = (soil(isol)%det_lag - soil(isol)%ffc *   &
          soil(isol)%sumfc) / soil(isol)%phys(nly)%d
      else
        soil(isol)%wat_tbl = 0.
      end if
      soil(isol)%avpor = sumpor / soil(isol)%phys(nly)%d
      soil(isol)%avbd = 2.65 * (1. - soil(isol)%avpor)

      return
      end subroutine soil_awc_init