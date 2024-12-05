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
      integer :: ly = 0     !none          |soil layer used to compute soil water and |soil temperature factors
      real :: xx = 0.       !varies        |variable to hold intermediate calculation result
      real :: csf = 0.      !none          |combined temperature/soil water factor
      real :: cnr = 0.      !              |carbon nitrogen ratio
      real :: cnrf = 0.     !              |carbon nitrogen ratio factor 
      real :: cpr = 0.      !              |carbon phosphorus ratio
      real :: cprf = 0.     !              |carbon phosphorus ratio factor
      real :: ca = 0.       !              |
      real :: decr = 0.     !              |
      real :: cdg = 0.      !none          |soil temperature factor
      real :: sut = 0.      !none          |soil water factor
      
      j = ihru
      
      !! zero transformations for summing layers
      !hrc_d(j)%rsd_surfdecay_c = 0.
      !hrc_d(j)%rsd_rootdecay_c = 0.
      hnb_d(j)%rsd_nitorg_n = 0.
      hnb_d(j)%rsd_laborg_p = 0.

      !! compute residue decomp and mineralization of fresh organic n and p of each layer
      do ly = 1, soil(j)%nly    !! decompose residue in each layer (layer 1 = surface residue)
        !! mineralization can occur only if temp above 0 deg
        if (soil(j)%phys(ly)%tmp > 0.) then
          
          if (soil1(j)%rsd(ly)%n > 1.e-4) then
            cnr = soil1(j)%rsd(ly)%c / soil1(j)%rsd(ly)%n
            if (cnr > 500.) cnr = 500.
            cnrf = Exp(-.693 * (cnr - 25.) / 25.)
          else
            cnrf = 1.
          end if
            
          if (soil1(j)%rsd(ly)%p > 1.e-4) then
            cpr = soil1(j)%rsd(ly)%c / soil1(j)%rsd(ly)%p
            if (cpr > 5000.) cpr = 5000.
            cprf = Exp(-.693 * (cpr - 200.) / 200.)
          else
            cprf = 1.
          end if
        
          !! compute soil water factor
          if (soil(j)%phys(ly)%st < 0.) soil(j)%phys(ly)%st = .0000001
          sut = .1 + .9 * Sqrt(soil(j)%phys(ly)%st / soil(j)%phys(ly)%fc)
          sut = Max(.05, sut)

          !!compute soil temperature factor
          xx = soil(j)%phys(ly)%tmp
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
            decr = rsdco_plcom(j) * ca * csf
          else
            decr = 0.05 * ca * csf
          end if
          decr = Max(bsn_prm%decr_min, decr)
          decr = Min(decr, 1.)
          
          !! apply decay to total carbon pool for both C models
          decomp = decr * soil1(j)%rsd(ly)
          soil1(j)%rsd(ly) = soil1(j)%rsd(ly) - decomp
          soil1(j)%mn(ly)%no3 = soil1(j)%mn(ly)%no3 + .8 * decomp%n
          soil1(j)%hact(ly)%n = soil1(j)%hact(ly)%n + .2 * decomp%n
          soil1(j)%mp(ly)%lab = soil1(j)%mp(ly)%lab + .8 * decomp%p
          soil1(j)%hact(ly)%p = soil1(j)%hact(ly)%p + .2 * decomp%p
          
          if (ly == 1) then
            hrc_d(j)%rsd_surfdecay_c = hrc_d(j)%rsd_surfdecay_c + 0.42 * decomp%c
          else
            hrc_d(j)%rsd_rootdecay_c = hrc_d(j)%rsd_rootdecay_c + 0.42 * decomp%c
          end if
          hnb_d(j)%rsd_nitorg_n = hnb_d(j)%rsd_nitorg_n + .8 * decomp%n
          hnb_d(j)%rsd_laborg_p = hnb_d(j)%rsd_laborg_p + .8 * decomp%p
            
        end if        ! soil temperature > 0.
      end do      ! ly = 1, soil(j)%nly
      
      return
      end subroutine rsd_decomp