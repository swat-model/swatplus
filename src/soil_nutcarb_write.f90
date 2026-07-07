      subroutine soil_nutcarb_write

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes daily HRU output to the output.hru file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    etday         |mm H2O        |actual amount of evapotranspiration that
!!                                 |occurs on day in HRU
!!    gw_q(:)       |mm H2O        |groundwater contribution to streamflow from
!!                                 |HRU on current day
!!    hru_ha(:)     |ha            |area of HRU in hectares
!!    hru_km(:)     |km^2          |area of HRU in square kilometers
!!    ihru          |none          |HRU number
!!    rchrg(:)      |mm H2O        |amount of water recharging both aquifers on
!!                                 |current day in HRU
!!    surfq(:)      |mm H2O        |surface runoff generated on day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |counter
!!    j           |none          |HRU number
!!    sb          |none          |subbasin number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use hru_module, only : etday, ihru, tillage_factor
      use soil_module
      use time_module
      use basin_module
      use organic_mineral_mass_module
      use hydrograph_module
      use calibration_data_module
      use output_landscape_module
      
      implicit none
      
      integer :: ly             !none        |counter
      real :: const             !none        |counter
      integer :: iihru          !none        |counter
      integer :: j              !none        |counter
      integer :: iob
      real :: soilp_prev = 0.
      real :: plantp_prev = 0. 
      real :: rsdp_prev = 0.
      real :: bal0
      
      !! basin output - zero daily basin outputs before summing
      bsn_org_soil = soil_org_z
      bsn_org_pl = soil_org_z
      bsn_org_rsd = soil_org_z
      bsn_mn = 0.
      bsn_mp = 0.
        
      !! sum the output for the entire soil profile
      do j = 1, sp_ob%hru
        soil1(j)%tot_org = soil_org_z
        soil_prof_hact = soil_org_z
        soil_prof_hsta = soil_org_z
        soil_prof_mn = soil_mn_z
        soil_prof_mp = soil_mp_z
        do ly = 1, soil(j)%nly
          soil_prof_mn = soil_prof_mn + soil1(j)%mn(ly)
          soil_prof_mp = soil_prof_mp + soil1(j)%mp(ly)
          soil1(j)%tot_org = soil1(j)%tot_org + soil1(j)%tot(ly)
          soil_prof_hact = soil_prof_hact + soil1(j)%hact(ly)
          soil_prof_hsta = soil_prof_hsta + soil1(j)%hsta(ly)
          soil_prof_str = soil_prof_str + soil1(j)%str(ly)
          soil_prof_lig = soil_prof_lig + soil1(j)%lig(ly)
          soil_prof_meta = soil_prof_meta + soil1(j)%meta(ly)
          soil_prof_man = soil_prof_man + soil1(j)%man(ly)
          soil_prof_hs = soil_prof_hs + soil1(j)%hs(ly)
          soil_prof_hp = soil_prof_hp + soil1(j)%hp(ly)
          soil_prof_microb = soil_prof_microb + soil1(j)%microb(ly)
          soil_prof_water = soil_prof_water + soil1(j)%water(ly)
        end do
        soil1(j)%tot_mn = soil_prof_mn%no3 + soil_prof_mn%nh4
        !soil1(j)%tot_mp = soil_prof_mp%wsol + soil_prof_mp%lab + soil_prof_mp%act + soil_prof_mp%sta
        soil1(j)%tot_mp = soil_prof_mp%lab

        ! write all carbon, organic n and p, and mineral n and p for the soil profile
        !write (2610,*) time%day, time%yrc, ihru, soil_prof_mn, soil_prof_mp, soil_prof_tot, soil_prof_str,  &
        !  soil_prof_lig, soil_prof_meta, soil_prof_man, soil_prof_hs, soil_prof_hp, soil_prof_microb,       &
        !  soil_prof_water
        
        ! write all carbon, organic n and p, and mineral n and p for the soil profile, plants, and residue
        !write (2610,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(j)%gis_id, ob(j)%name,soil1(j)%tot_mn, &
        !  soil1(j)%tot_mp, soil1(j)%tot_org%c, soil1(j)%tot_org%n, soil1(j)%tot_org%p, pl_mass(j)%tot_com%c,    &
        !  pl_mass(j)%tot_com%n, pl_mass(j)%tot_com%p, rsd1(j)%tot_com%c, rsd1(j)%tot_com%n, rsd1(j)%tot_com%p
    
        ! write all carbon, organic n and p, and mineral n and p for the soil profile, plants, and residue
        !!write (2610,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(j)%gis_id, ob(j)%name, soil1(j)%tot_mn, &
        !!  soil_prof_hact%n, soil_prof_hsta%n, pl_mass(j)%tot_com%n, rsd1(j)%tot_com%n, soil1(j)%tot_mp,          &
        !!  pl_mass(j)%tot_com%p, rsd1(j)%tot_com%p
    
      !! labile p balance - labp gain from rsd = 1/(1+0.2)*rsd_laborg_p
      bal0 = (soilp_prev - soil_prof_mp%lab) + (plantp_prev - pl_mass(j)%tot_com%p) +               &
        .8 * (rsdp_prev - rsd1(j)%tot_com%p) +                                                      &
        hnb_d(j)%fertp + hnb_d(j)%org_lab_p - .8 * hnb_d(j)%rsd_laborg_p - hnb_d(j)%puptake -       &
        hnb_d(j)%lab_min_p - hls_d(j)%lchlabp + hls_d(j)%tilelabp - hls_d(j)%surqsolp
      soilp_prev = soil_prof_mp%lab
      rsdp_prev = rsd1(j)%tot_com%p
      plantp_prev = pl_mass(j)%tot_com%p
      
      end do    !! hru loop
      
      !! summing hru output for the basin
      do j = 1, sp_ob%hru
        iihru = lsu_elem(j)%obtypno
        if (lsu_elem(iihru)%bsn_frac > 1.e-12) then
          const = lsu_elem(iihru)%bsn_frac
          if (lsu_elem(iihru)%obtyp == "hru") then
            bsn_org_soil = bsn_org_soil + const * soil1(iihru)%tot_org
            bsn_org_pl = bsn_org_pl + const * pl_mass(iihru)%tot_com
            bsn_org_rsd = bsn_org_rsd + const * rsd1(iihru)%tot_com
            bsn_mn = bsn_mn + const * soil1(iihru)%tot_mn
            bsn_mp = bsn_mp + const * soil1(iihru)%tot_mp
          end if
        end if
      end do
              
      !! write all carbon, organic n and p, and mineral n and p for the soil profile, plants, and residue
      !write (2610,*) time%day, time%yrc, ihru, bsn_mn, bsn_mp, bsn_org_soil%c,      &
      !  bsn_org_soil%n, bsn_org_soil%p, bsn_org_pl%c, bsn_org_pl%n, bsn_org_pl%p,   &
      !  bsn_org_rsd%c, bsn_org_rsd%n, bsn_org_rsd%p
      
100   format (6i12,5x,a12,11f16.3)
    
      return

      end subroutine soil_nutcarb_write