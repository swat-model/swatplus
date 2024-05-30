      subroutine soil_nutcarb_write

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes daily HRU output to the output.hru file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
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

      use soil_module
      use organic_mineral_mass_module
      use hydrograph_module
      use calibration_data_module
      
      implicit none
      
      integer :: ly             !none        |counter
      real :: const             !none        |counter
      integer :: iihru          !none        |counter
      integer :: j              !none        |counter
      integer :: iob
           
      !! basin output - zero daily basin outputs before summing
      bsn_org_soil = soil_org_z
      bsn_org_pl = soil_org_z
      bsn_org_rsd = soil_org_z
        
      !! sum the output for the entire soil profile
      do j = 1, sp_ob%hru
        iob = sp_ob1%hru + j - 1   !!!!!! added for new output write  !!nbs
        soil1(j)%tot_org = soil_org_z
        soil_prof_hact = soil_org_z
        soil_prof_hsta = soil_org_z
        do ly = 1, soil(j)%nly
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
        soil1(j)%tot_org = soil_prof_hact + soil_prof_hsta + soil_prof_microb
        
        !write all organic carbon for the plant community
        write (4560,*) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
            pl_mass(j)%tot_com%c, pl_mass(j)%ab_gr_com%c, pl_mass(j)%leaf_com%c,                  &
            pl_mass(j)%stem_com%c, pl_mass(j)%seed_com%c, pl_mass(j)%root_com%c
        
        !write all organic carbon for the residue (all plants)
        write (4561,*) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
            rsd1(j)%tot_com%c, rsd1(j)%tot_meta%c, rsd1(j)%tot_str%c, rsd1(j)%tot_lignin%c
        
        !write all organic carbon for the soil profile
        write (4562,*) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
            soil1(j)%tot_org%c, soil_prof_str%c, soil_prof_lig%c, soil_prof_meta%c,               &
            soil_prof_man%c, soil_prof_hs%c, soil_prof_hp%c, soil_prof_microb%c
      
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
      write (4566,*) time%day, time%yrc, ' basin ', bsn_org_soil%c, bsn_org_pl%c, bsn_org_rsd%c
          
      return

      end subroutine soil_nutcarb_write