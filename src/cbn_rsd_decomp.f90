      subroutine cbn_rsd_decomp

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine estimates daily nitrogen and phosphorus
!!    mineralization and immobilization considering fresh organic
!!    material (plant residue) and active and stable humus material

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru          |none          |HRU number
!!    rsdco_pl(:)   |none          |plant residue decomposition coefficient. The
!!                                 |fraction of residue which will decompose in
!!                                 |a day assuming optimal moisture,
!!                                 |temperature, C:N ratio, and C:P ratio
!!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max, Exp, Sqrt, Min, Abs

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use septic_data_module
      use basin_module
      use organic_mineral_mass_module
      use hru_module, only : rsdco_plcom, i_sep, ihru, isep 
      use soil_module
      use plant_module
      use output_landscape_module, only : hnb_d
      
      implicit none 

      integer :: j = 0      !none          |HRU number
      integer :: k = 0      !none          |counter (soil layer)
      ! integer :: kk = 0     !none          |soil layer used to compute soil water and
      !                       !              |soil temperature factors
      !integer :: idp = 0
      real :: rmn1 = 0.     !kg N/ha       |amount of nitrogen moving from fresh organic
                            !              |to nitrate(80%) and active organic(20%)
                            !              |pools in layer
      real :: rmp = 0.      !              |to labile(80%) and organic(20%) pools in layer
      real :: xx = 0.       !varies        |variable to hold intermediate calculation result
      real :: csf = 0.      !none          |combined temperature/soil water factor
      real :: rwn = 0.      !kg N/ha       |amount of nitrogen moving from active organic
                            !              |to stable organic pool in layer
      real :: hmn = 0.      !kg N/ha       |amount of nitrogen moving from active organic
                            !              |nitrogen pool to nitrate pool in layer
      real :: hmp = 0.      !kg P/ha       |amount of phosphorus moving from the organic
                            !              |pool to the labile pool in layer
      real :: cnr = 0.      !              |carbon nitrogen ratio
      real :: cnrf = 0.     !              |carbon nitrogen ratio factor 
      real :: cpr = 0.      !              |carbon phosphorus ratio
      real :: cprf = 0.     !              |carbon phosphorus ratio factor
      real :: ca = 0.       !              |
      real :: decr = 0.     !              |
      !real :: rdc = 0.      !              |
      real :: wdn = 0.      !kg N/ha       |amount of nitrogen lost from nitrate pool in
                            !              |layer due to denitrification
      real :: cdg = 0.      !none          |soil temperature factor
      real :: sut = 0.      !none          |soil water factor
      real :: nactfr = 0.   !none          |nitrogen active pool fraction. The fraction
                            !              |of organic nitrogen in the active pool. 

      j = ihru
      nactfr = .02
      !zero transformations for summing layers
      hnb_d(j)%act_nit_n = 0.
      hnb_d(j)%org_lab_p = 0.
      hnb_d(j)%act_sta_n = 0.
      hnb_d(j)%denit = 0.
      hnb_d(j)%rsd_nitorg_n = 0.
      hnb_d(j)%rsd_laborg_p = 0.

      !! compute humus mineralization of organic soil pools 
      do k = 1, soil(j)%nly

        !! mineralization can occur only if temp above 0 deg
        if (soil(j)%phys(k)%tmp > 0.) then
          !! compute soil water factor
          sut = .1 + .9 * Sqrt(soil(j)%phys(k)%st / soil(j)%phys(k)%fc)
          sut = Max(.05, sut)

          !!compute soil temperature factor
          xx = soil(j)%phys(k)%tmp
          cdg = .9 * xx / (xx + Exp(9.93 - .312 * xx)) + .1
          cdg = Max(.1, cdg)

          !! compute combined factor
          xx = cdg * sut
          if (xx < 0.) xx = 0.
          if (xx > 1.e6) xx = 1.e6
          csf = Sqrt(xx)

          !! compute residue decomp and mineralization of 
          !! fresh organic n and p (upper two layers only)
          rmn1 = 0.
          rmp = 0.
          if (soil1(j)%rsd(k)%n > 1.e-4) then
            cnr = soil1(j)%rsd(k)%c / soil1(j)%rsd(k)%n
            if (cnr > 500.) cnr = 500.
            cnrf = Exp(-.693 * (cnr - 25.) / 25.)
          else
            cnrf = 1.
          end if
            
          if (soil1(j)%rsd(k)%p > 1.e-4) then
            cpr = soil1(j)%rsd(k)%c / soil1(j)%rsd(k)%p
            if (cpr > 5000.) cpr = 5000.
            cprf = Exp(-.693 * (cpr - 200.) / 200.)
          else
            cprf = 1.
          end if

          ca = Min(cnrf, cprf, 1.)
            
          !! compute root and incorporated residue decomposition
          !! all plant residue in soil is mixed - don't track individual plant residue in soil
              
          if (pcom(j)%npl > 0) then
            decr = rsdco_plcom(j) / pcom(j)%npl * ca * csf
          else
            decr = 0.05
          end if
          decr = Max(bsn_prm%decr_min, decr)
          decr = Min(decr, 1.)
          decomp = decr * soil1(j)%rsd(k)
          soil1(j)%rsd(k) = soil1(j)%rsd(k) - decomp

          ! The following if statements are to prevent runtime underflow errors with gfortran 
          if (soil1(j)%rsd(k)%m < 1.e-10) soil1(j)%rsd(k)%m = 0.0 
          if (soil1(j)%rsd(k)%c < 1.e-10) soil1(j)%rsd(k)%c = 0.0 
          if (soil1(j)%rsd(k)%n < 1.e-10) soil1(j)%rsd(k)%n = 0.0 
          if (soil1(j)%rsd(k)%p < 1.e-10) soil1(j)%rsd(k)%p = 0.0 

          soil1(j)%meta(k) = soil1(j)%meta(k) + meta_frac * decomp
          soil1(j)%str(k) = soil1(j)%str(k) + str_frac * decomp
          soil1(j)%lig(k) = soil1(j)%lig(k) + lig_frac * decomp

        end if
      end do        ! k = 1, soil(j)%nly

      return
      end subroutine