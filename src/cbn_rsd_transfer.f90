      subroutine cbn_rsd_transfer

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
      use output_landscape_module, only : hnb_d
      
      implicit none 

      integer :: j = 0      !none          |HRU number
      integer :: k = 0      !none          |counter (soil layer)
      real :: decr = 0.     !              |
      integer :: ipl = 0      !              |plant number in plant community
      real :: idp = 0.      !              |plant number in plant data module
      real :: nactfr = 0.   !none          |nitrogen active pool fraction. The fraction
                            !              |of organic nitrogen in the active pool.
      real :: clg = 0.      !none          |lignin fraction of the residue
      real :: rln = 0.      !none          |lignin-to-nitrogen ratio of the residue
      real :: lmf = 0.      !none          |metabolic fraction of residue (CENTURY: 0.85-0.018*RLN)
      real :: lsf = 0.      !none          |structural fraction of residue (1-LMF)
      real :: rlr = 0.      !none          |lignin fraction going to structural (<= LSF)

      j = ihru
      nactfr = .02
      !zero transformations for summing layers
      hnb_d(j)%act_nit_n = 0.
      hnb_d(j)%org_lab_p = 0.
      hnb_d(j)%act_sta_n = 0.
      hnb_d(j)%denit = 0.
      hnb_d(j)%rsd_nitorg_n = 0.
      hnb_d(j)%rsd_laborg_p = 0.

      !! compute root and incorporated residue decomposition
      do k = 1, soil(j)%nly

        do ipl = 1, pcom(j)%npl
          !! mineralization can occur only if temp above 0 deg
          if (soil(j)%phys(k)%tmp > 0.) then
            idp = pcom(j)%plcur(ipl)%idplt
            !! move ALL soil residue (decr = 1.0) into the CENTURY pools, then zero the
            !! residue pool. Partition the above- and below-ground components separately so
            !! each uses its own lignin fraction (lig_frac_abg vs lig_frac_blg).
            transfer = soil1(j)%pl(ipl)%rsd(k)
            soil1(j)%rsd_tot(k) = soil1(j)%rsd_tot(k) - sum_origin(transfer)
            soil1(j)%pl(ipl)%rsd(k) = rsd_originz

            call partition_rsd (transfer%abg, res_part_fracs(idp)%lig_frac_abg)
            call partition_rsd (transfer%blg, res_part_fracs(idp)%lig_frac_blg)

          end if    ! soil temp > 0

        end do      ! ipl = 1, pcom(j)%npl
      end do        ! k = 1, soil(j)%nly

      return

      contains

      !! CENTURY metabolic/structural split (item B, option 3), ported from SWAT-C
      !! (dormant_change.f90): the metabolic fraction is derived from the residue
      !! lignin-to-N ratio, INDEPENDENTLY of the lignin fraction, then all residue
      !! lignin is placed in the structural pool. This replaces str_frac = lig_frac/0.80
      !! (which forced lignin to 80% of structural and could drive meta_frac negative).
      !! Applied once per origin (t) with that origin's lignin fraction (clg); j, k and
      !! rln/lmf/lsf/rlr are host-associated.
      subroutine partition_rsd (t, clg_o)
        type (organic_mass), intent (in) :: t     ! one origin's residue (abg or blg)
        real, intent (in) :: clg_o                ! that origin's lignin fraction
        rln = clg_o * t%m / (t%n + 1.e-5)                       ! lignin mass / N mass
        lmf = 0.85 - 0.018 * rln
        lmf = max(0.01, min(0.7, lmf))
        lsf = 1.0 - lmf
        rlr = min(0.8, clg_o)                                   ! lignin fraction (capped)
        rlr = min(rlr, lsf)                                     ! lignin <= structural (nonlig >= 0)
        soil1(j)%meta(k) = soil1(j)%meta(k) + lmf * t
        soil1(j)%str(k)  = soil1(j)%str(k)  + lsf * t           ! str%c reconstituted in cbn_zhang2; feeds str%m/%n/%p
        soil1(j)%lig(k)  = soil1(j)%lig(k)  + rlr * t           ! all residue lignin -> structural lignin
        soil1(j)%nonlig(k)%c = soil1(j)%nonlig(k)%c + (lsf - rlr) * t%c
      end subroutine partition_rsd

      end subroutine cbn_rsd_transfer
