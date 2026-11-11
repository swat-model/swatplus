      subroutine cbn_surfrsd_decomp

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
      use hru_module, only : ihru 
      use soil_module
      use plant_module
      use plant_data_module
      use carbon_module, only : cnr_cap, cnr_ref, cpr_cap, cpr_ref
      use output_landscape_module, only : hnb_d
      
      implicit none 
       
      integer :: j = 0      !none          |HRU number
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
      integer :: ipl = 0    !none          |plant number in plant community
      integer :: idp = 0    !none          |plant number in plant data module
      real :: cdg = 0.      !none          |soil temperature factor
      real :: sut = 0.      !none          |soil water factor
      j = ihru
      !zero transformations for summing layers
      hnb_d(j)%act_nit_n = 0.
      hnb_d(j)%org_lab_p = 0.
      hnb_d(j)%act_sta_n = 0.
      hnb_d(j)%denit = 0.
      hnb_d(j)%rsd_nitorg_n = 0.
      hnb_d(j)%rsd_laborg_p = 0.

      !! compute root and incorporated residue decomposition
      !! compute humus mineralization of organic soil pools 
        do ipl = 1, pcom(j)%npl
          ! mineralization can occur only if temp above 0 deg
          photo_decomp = photo_degrade_factor * pl_mass(j)%abg_rsd(ipl) 
          pl_mass(j)%abg_rsd(ipl) = pl_mass(j)%abg_rsd(ipl) - photo_decomp
          pl_mass(j)%abg_rsd_tot = pl_mass(j)%abg_rsd_tot - photo_decomp
          if (soil(j)%phys(1)%tmp > 0.) then
            !! compute soil water factor
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

            !! compute residue decomp and mineralization of surface residue
            rmn1 = 0.
            rmp = 0.
            if (pl_mass(j)%abg_rsd(ipl)%n > 1.e-4) then
              cnr = pl_mass(j)%abg_rsd(ipl)%c / pl_mass(j)%abg_rsd(ipl)%n
              if (cnr > cnr_cap) cnr = cnr_cap
              cnrf = Exp(-.693 * (cnr - cnr_ref) / cnr_ref)
            else
              cnrf = 1.
            end if
            
            if (pl_mass(j)%abg_rsd(ipl)%p > 1.e-4) then
              cpr = pl_mass(j)%abg_rsd(ipl)%c / pl_mass(j)%abg_rsd(ipl)%p
              if (cpr > cpr_cap) cpr = cpr_cap
              cprf = Exp(-.693 * (cpr - cpr_ref) / cpr_ref)
            else
              cprf = 1.
            end if

            ca = Min(cnrf, cprf, 1.)
            
            idp = pcom(j)%plcur(ipl)%idplt 
            decr = pldb(idp)%rsdco_pl * ca * csf
            decr = Max(bsn_prm%decr_min, decr)
            decr = Min(decr, 1.)
            decomp = decr * pl_mass(j)%abg_rsd(ipl)
            pl_mass(j)%abg_rsd(ipl) = pl_mass(j)%abg_rsd(ipl) - decomp
            pl_mass(j)%abg_rsd_tot = pl_mass(j)%abg_rsd_tot - decomp
            soil1(j)%pl(ipl)%rsd(1)%abg = soil1(j)%pl(ipl)%rsd(1)%abg + decomp

            !! flush the surface residue pool to zero once it drifts below 1.e-10.
            !! abg_rsd is decremented by `decomp` every day, so without this it decays
            !! asymptotically into denormal territory and gfortran raises underflow.
            if (pl_mass(j)%abg_rsd(ipl)%m < 1.e-10) pl_mass(j)%abg_rsd(ipl)%m = 0.0
            if (pl_mass(j)%abg_rsd(ipl)%c < 1.e-10) pl_mass(j)%abg_rsd(ipl)%c = 0.0
            if (pl_mass(j)%abg_rsd(ipl)%n < 1.e-10) pl_mass(j)%abg_rsd(ipl)%n = 0.0
            if (pl_mass(j)%abg_rsd(ipl)%p < 1.e-10) pl_mass(j)%abg_rsd(ipl)%p = 0.0

          end if    ! soil temp > 0
          
        end do      ! ipl = 1, pcom(j)%npl

      return
      end subroutine cbn_surfrsd_decomp
