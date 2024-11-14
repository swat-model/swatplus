      subroutine rsd_decomp

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

      use plant_data_module
      use basin_module
      use organic_mineral_mass_module
      use hru_module, only : rsdco_plcom, i_sep, ihru, ipl, isep 
      use soil_module
      use plant_module
      use output_landscape_module, only : hnb_d
      use carbon_module, only : hrc_d
      
      implicit none 

      integer :: j = 0      !none          |HRU number
      integer :: k = 0      !none          |counter (soil layer)
      integer :: kk = 0     !none          |soil layer used to compute soil water and
                            !              |soil temperature factors
      integer :: idp = 0
      real :: rmn1 = 0.     !kg N/ha       |amount of nitrogen moving from fresh organic
                            !              |to nitrate(80%) and active organic(20%)
                            !              |pools in layer
      real :: rmp = 0.      !              |to labile(80%) and organic(20%) pools in layer
      real :: xx = 0.       !varies        |variable to hold intermediate calculation result
      real :: csf = 0.      !none          |combined temperature/soil water factor
      real :: cnr = 0.      !              |carbon nitrogen ratio
      real :: cnrf = 0.     !              |carbon nitrogen ratio factor 
      real :: cpr = 0.      !              |carbon phosphorus ratio
      real :: cprf = 0.     !              |carbon phosphorus ratio factor
      real :: ca = 0.       !              |
      real :: decr = 0.     !              |
      real :: rdc = 0.      !              |
      real :: cdg = 0.      !none          |soil temperature factor
      real :: sut = 0.      !none          |soil water factor
      
      j = ihru
      
      !zero transformations for summing layers
      hnb_d(j)%rsd_nitorg_n = 0.
      hnb_d(j)%rsd_laborg_p = 0.

      !! mineralization can occur only if temp above 0 deg
      if (soil(j)%phys(1)%tmp > 0.) then
        rsd1(j)%tot_com = orgz
        if (bsn_cc%cswat == 2) then
          rsd1(j)%tot_meta = orgz
          rsd1(j)%tot_str = orgz
          rsd1(j)%tot_lignin = orgz
        end if
          
      !! compute residue decomp and mineralization of fresh organic n and p of flat residue
        do ipl = 1, pcom(j)%npl        !! we need to decompose each plant
          rmn1 = 0.
          rmp = 0.
          if (rsd1(j)%tot(ipl)%n > 1.e-4) then
            cnr = rsd1(j)%tot(ipl)%c / rsd1(j)%tot(ipl)%n
            if (cnr > 500.) cnr = 500.
            cnrf = Exp(-.693 * (cnr - 25.) / 25.)
          else
            cnrf = 1.
          end if
            
          if (rsd1(j)%tot(ipl)%p > 1.e-4) then
            cpr = rsd1(j)%tot(ipl)%c / rsd1(j)%tot(ipl)%p
            if (cpr > 5000.) cpr = 5000.
            cprf = Exp(-.693 * (cpr - 200.) / 200.)
          else
            cprf = 1.
          end if
        
          !! compute soil water factor
          if (soil(j)%phys(1)%st < 0.) soil(j)%phys(1)%st = .0000001
          sut = .1 + .9 * Sqrt(soil(j)%phys(1)%st / soil(j)%phys(1)%fc)
          sut = Max(.05, sut)

          !!compute soil temperature factor
          xx = soil(j)%phys(1)%tmp
          cdg = .9 * xx / (xx + Exp(9.93 - .312 * xx)) + .1
          cdg = Max(.1, cdg)

          !! compute combined factor
          xx = cdg * sut
          if (xx < 0.) xx = 0.
          if (xx > 1.e6) xx = 1.e6
          csf = Sqrt(xx)
          ca = Min(cnrf, cprf, 1.)
          !! compute residue decomp and mineralization for each plant
          if (pcom(j)%npl > 0) then
            idp = pcom(j)%plcur(ipl)%idplt
            decr = pldb(idp)%rsdco_pl * ca * csf
          else
            decr = 0.05 * ca * csf
          end if
          decr = Max(bsn_prm%decr_min, decr)
          decr = Min(decr, 1.)
          
          !! mineralization of mass and carbon
          rsd1(j)%tot(ipl)%m = Max(1.e-6, rsd1(j)%tot(ipl)%m)
          rdc = decr * rsd1(j)%tot(ipl)%m
          rsd1(j)%tot(ipl)%m = rsd1(j)%tot(ipl)%m - rdc
          if (rsd1(j)%tot(ipl)%m < 0.) rsd1(j)%tot(ipl)%m = 0.
          
          !! apply decay to total carbon pool
          rsd1(j)%tot(ipl)%c = (1. - decr) * rsd1(j)%tot(ipl)%c
          if (rsd1(j)%tot(ipl)%c < 0.) rsd1(j)%tot(ipl)%c = 0.
          soil1(j)%hact(1)%c = soil1(j)%hact(1)%c + decr * rsd1(j)%tot(ipl)%c
          
          !! apply decay to all carbon pools
          if (bsn_cc%cswat == 2) then
            decomp = decr * rsd1(j)%meta(ipl)
            rsd1(j)%meta(ipl) = rsd1(j)%meta(ipl) - decomp
            soil1(j)%meta(ipl) = rsd1(j)%meta(ipl) + decomp
            decomp = decr * rsd1(j)%str(ipl)
            rsd1(j)%str(ipl) = rsd1(j)%str(ipl) - decomp
            soil1(j)%str(ipl) = rsd1(j)%str(ipl) + decomp
            decomp = decr * rsd1(j)%lignin(ipl)
            rsd1(j)%lignin(ipl) = rsd1(j)%lignin(ipl) - decomp
            soil1(j)%lig(ipl) = rsd1(j)%lignin(ipl) + decomp
          end if
      
          !! mineralization of residue n and p
          rmn1 = decr * rsd1(j)%tot(ipl)%n 
          rsd1(j)%tot(ipl)%n = Max(1.e-6, rsd1(j)%tot(ipl)%n)
          rsd1(j)%tot(ipl)%n = rsd1(j)%tot(ipl)%n - rmn1
          soil1(j)%mn(1)%no3 = soil1(j)%mn(1)%no3 + .8 * rmn1
          soil1(j)%hact(1)%n = soil1(j)%hact(1)%n + .2 * rmn1
          
          rsd1(j)%tot(ipl)%p = Max(1.e-6, rsd1(j)%tot(ipl)%p)
          rmp = decr * rsd1(j)%tot(ipl)%p
          rsd1(j)%tot(ipl)%p = rsd1(j)%tot(ipl)%p - rmp
          soil1(j)%mp(1)%lab = soil1(j)%mp(1)%lab + .8 * rmp
          soil1(j)%hact(1)%p = soil1(j)%hact(1)%p + .2 * rmp
          
          hnb_d(j)%rsd_nitorg_n = hnb_d(j)%rsd_nitorg_n + .8 * rmn1
          hnb_d(j)%rsd_laborg_p = hnb_d(j)%rsd_laborg_p + .8 * rmp
          
          !! sum total residue pools
          rsd1(j)%tot_com = rsd1(j)%tot_com + rsd1(j)%tot(ipl)
          if (bsn_cc%cswat == 2) then
            rsd1(j)%tot_meta = rsd1(j)%tot_meta + rsd1(j)%meta(ipl)
            rsd1(j)%tot_str = rsd1(j)%tot_str + rsd1(j)%str(ipl)
            rsd1(j)%tot_lignin = rsd1(j)%tot_lignin + rsd1(j)%lignin(ipl)
          end if
          
        end do      ! ipl = 1, pcom(j)%npl
      end if        ! soil temperature > 0.
      
      return
      end subroutine rsd_decomp