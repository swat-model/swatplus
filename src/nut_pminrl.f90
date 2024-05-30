      subroutine nut_pminrl
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes p flux between the labile, active mineral
!!    and stable mineral p pools.     
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module
      use organic_mineral_mass_module
      use hru_module, only : ihru
      use soil_module
      use output_landscape_module, only : hnb_d
      
      implicit none

      real, parameter :: bk = 0.01      !              |was .0006
      integer :: j                      !none          |HRU number
      integer :: l                      !none          |counter 
      real :: rto                       !              |
      real :: rmp1                      !kg P/ha       |amount of phosphorus moving from the solution
                                        !              |mineral to the active mineral pool in the soil layer
      real :: roc                       !kg P/ha       |amount of phosphorus moving from the active
                                        !              |mineral to the stable mineral pool in the soil layer

      j = ihru

      hnb_d(j)%lab_min_p = 0.
      hnb_d(j)%act_sta_p = 0.

      rto = bsn_prm%psp / (1. - bsn_prm%psp)

      do l = 1, soil(j)%nly
        rmp1 = (soil1(j)%mp(l)%lab - soil1(j)%mp(l)%act * rto)
        !! mike changed/added per isabelle beaudin"s email from 01/21/09
        if (rmp1 > 0.) rmp1 = rmp1 * 0.1
        if (rmp1 < 0.) rmp1 = rmp1 * 0.6
        !! mike changed/added per isabelle beaudin"s email from 01/21/09
        rmp1 = Min(rmp1, soil1(j)%mp(l)%lab)

        roc = bk * (4. * soil1(j)%mp(l)%act - soil1(j)%mp(l)%sta)
        if (roc < 0.) roc = roc * .1
        roc = Min(roc, soil1(j)%mp(l)%act)

        soil1(j)%mp(l)%sta = soil1(j)%mp(l)%sta + roc
        if (soil1(j)%mp(l)%sta < 0.) soil1(j)%mp(l)%sta = 0.

        soil1(j)%mp(l)%act = soil1(j)%mp(l)%act - roc + rmp1
        if (soil1(j)%mp(l)%act < 0.) soil1(j)%mp(l)%act = 0.

        soil1(j)%mp(l)%lab = soil1(j)%mp(l)%lab - rmp1
        if (soil1(j)%mp(l)%lab < 0.) soil1(j)%mp(l)%lab = 0.

        hnb_d(j)%lab_min_p = hnb_d(j)%lab_min_p + rmp1
        hnb_d(j)%act_sta_p = hnb_d(j)%act_sta_p + roc
      end do

      return
      end subroutine nut_pminrl