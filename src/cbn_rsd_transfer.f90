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
      real :: ipl = 0.      !              |plant number in plant community
      real :: idp = 0.      !              |plant number in plant data module
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

      !! compute root and incorporated residue decomposition
      !! compute humus mineralization of organic soil pools 
      do k = 1, soil(j)%nly

        do ipl = 1, pcom(j)%npl
          !! mineralization can occur only if temp above 0 deg
          if (soil(j)%phys(k)%tmp > 0.) then
            idp = pcom(j)%plcur(ipl)%idplt 
            decr = 1.0 ! added by fg to move all soil rsd into soil meta, str, lig pools
            transfer = decr * soil1(j)%pl(ipl)%rsd(k)
            soil1(j)%pl(ipl)%rsd(k) = soil1(j)%pl(ipl)%rsd(k) - transfer

            ! The following if statements are to prevent runtime underflow errors with gfortran 
            if (soil1(j)%pl(ipl)%rsd(k)%m < 1.e-10) soil1(j)%pl(ipl)%rsd(k)%m = 0.0 
            if (soil1(j)%pl(ipl)%rsd(k)%c < 1.e-10) soil1(j)%pl(ipl)%rsd(k)%c = 0.0 
            if (soil1(j)%pl(ipl)%rsd(k)%n < 1.e-10) soil1(j)%pl(ipl)%rsd(k)%n = 0.0 
            if (soil1(j)%pl(ipl)%rsd(k)%p < 1.e-10) soil1(j)%pl(ipl)%rsd(k)%p = 0.0 

            !! add mass and carbon to soil organic pools
            ! soil1(j)%meta(k)%m = soil1(j)%meta(k)%m + pldb(idp)%res_part_fracs%meta_frac * transfer%m
            ! soil1(j)%str(k)%m = soil1(j)%str(k)%m + pldb(idp)%res_part_fracs%str_frac * transfer%m
            ! soil1(j)%lig(k)%m = soil1(j)%lig(k)%m + pldb(idp)%res_part_fracs%lig_frac * transfer%m
            ! soil1(j)%meta(k)%c = soil1(j)%meta(k)%c + pldb(idp)%res_part_fracs%meta_frac * transfer%c
            ! soil1(j)%str(k)%c = soil1(j)%str(k)%c + pldb(idp)%res_part_fracs%str_frac * transfer%c
            ! soil1(j)%lig(k)%c = soil1(j)%lig(k)%c + pldb(idp)%res_part_fracs%lig_frac * transfer%c
            
            soil1(j)%meta(k)%m = soil1(j)%meta(k)%m + cswat_3_part_fracs(idp)%meta_frac_blg * transfer%m
            soil1(j)%str(k)%m  = soil1(j)%str(k)%m  + cswat_3_part_fracs(idp)%str_frac_blg  * transfer%m
            soil1(j)%lig(k)%m  = soil1(j)%lig(k)%m  + cswat_3_part_fracs(idp)%lig_frac_blg  * transfer%m
            soil1(j)%meta(k)%c = soil1(j)%meta(k)%c + cswat_3_part_fracs(idp)%meta_frac_blg * transfer%c
            soil1(j)%str(k)%c  = soil1(j)%str(k)%c  + cswat_3_part_fracs(idp)%str_frac_blg  * transfer%c
            soil1(j)%lig(k)%c  = soil1(j)%lig(k)%c  + cswat_3_part_fracs(idp)%lig_frac_blg  * transfer%c
            
            !! add nitrogen and phosphorus to soil organic pools - assume c/n and c/p ratios
            !! c/n=10 for metabolic and 150 for structural; c/p=100 for metabolic and 1500 for structural
            !! solve ntot = nmeta + nstr  &  nmet = 15.* nstr * cmet/cstr
            if (soil1(j)%meta(k)%c > 1.e-6) then
              rsd_meta%n = transfer%n - soil1(j)%str(k)%c / (15. * soil1(j)%meta(k)%c)
            else
              rsd_meta%n = 0. 
            end if
            soil1(j)%meta(k)%n = soil1(j)%meta(k)%n + rsd_meta%n
            rsd_str%n = transfer%n - rsd_meta%n
            soil1(j)%str(k)%n = soil1(j)%str(k)%n + rsd_str%n
            soil1(j)%lig(k)%n = soil1(j)%lig(k)%n + lig_frac * rsd_str%n
            
            if (soil1(j)%meta(k)%c > 1.e-6) then
              rsd_meta%p = transfer%p - soil1(j)%str(k)%c / (15. * soil1(j)%meta(k)%c)
            else
              rsd_meta%p = 0.  
            end if
            soil1(j)%meta(k)%p = soil1(j)%meta(k)%p + rsd_meta%p
            rsd_str%p = transfer%p - rsd_meta%p
            soil1(j)%str(k)%p = soil1(j)%str(k)%p + rsd_str%p
            soil1(j)%lig(k)%p = soil1(j)%lig(k)%p + lig_frac * rsd_str%p
            
          end if    ! soil temp > 0
          
        end do      ! ipl = 1, pcom(j)%npl
      end do        ! k = 1, soil(j)%nly

      return
      end subroutine cbn_rsd_transfer