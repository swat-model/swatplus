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
            decr = 1.0 ! added by fg to move all soil rsd into soil meta, str, lig pools
            transfer = decr * soil1(j)%pl(ipl)%rsd(k)
            soil1(j)%pl(ipl)%rsd(k) = soil1(j)%pl(ipl)%rsd(k) - transfer
            soil1(j)%rsd_tot(k) = soil1(j)%rsd_tot(k) - transfer

            ! The following if statements are to prevent runtime underflow errors with gfortran 
            if (soil1(j)%pl(ipl)%rsd(k)%m < 1.e-10) soil1(j)%pl(ipl)%rsd(k)%m = 0.0 
            if (soil1(j)%pl(ipl)%rsd(k)%c < 1.e-10) soil1(j)%pl(ipl)%rsd(k)%c = 0.0 
            if (soil1(j)%pl(ipl)%rsd(k)%n < 1.e-10) soil1(j)%pl(ipl)%rsd(k)%n = 0.0 
            if (soil1(j)%pl(ipl)%rsd(k)%p < 1.e-10) soil1(j)%pl(ipl)%rsd(k)%p = 0.0 

            !! CENTURY metabolic/structural split (item B, option 3), ported from
            !! SWAT-C (dormant_change.f90): the metabolic fraction is derived from
            !! the residue lignin-to-N ratio, INDEPENDENTLY of the lignin fraction,
            !! then all residue lignin is placed in the structural pool. This
            !! replaces str_frac = lig_frac/0.80 (which forced lignin to 80% of
            !! structural and could drive meta_frac negative). High-lignin residue
            !! now gets a larger structural fraction, so lignin is a sensible,
            !! quality-dependent fraction of it and meta_frac stays valid.
            clg = cswat_1_part_fracs(idp)%lig_frac_blg               ! residue lignin fraction
            rln = clg * transfer%m / (transfer%n + 1.e-5)           ! lignin mass / N mass
            lmf = 0.85 - 0.018 * rln
            lmf = max(0.01, min(0.7, lmf))
            lsf = 1.0 - lmf
            rlr = min(0.8, clg)                                      ! lignin fraction (capped)
            rlr = min(rlr, lsf)                                      ! lignin <= structural (nonlig >= 0)
            soil1(j)%meta(k) = soil1(j)%meta(k) + lmf * transfer
            soil1(j)%str(k)  = soil1(j)%str(k)  + lsf * transfer     ! str%c reconstituted in cbn_zhang2; feeds str%m/%n/%p
            soil1(j)%lig(k)  = soil1(j)%lig(k)  + rlr * transfer     ! all residue lignin -> structural lignin
            soil1(j)%nonlig(k)%c = soil1(j)%nonlig(k)%c + (lsf - rlr) * transfer%c

          end if    ! soil temp > 0
          
        end do      ! ipl = 1, pcom(j)%npl
      end do        ! k = 1, soil(j)%nly

      return
      end subroutine cbn_rsd_transfer
