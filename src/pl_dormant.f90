      subroutine pl_dormant

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine checks the dormant status of the different plant types

      use climate_module
      use hydrograph_module
      use plant_data_module
      use organic_mineral_mass_module
      use hru_module, only : hru, dormhr, ipl, ihru
      use plant_module

      implicit none

      integer :: j = 0              !none          |HRU number
      integer :: idp = 0            !              |
      integer :: iob = 0            !              |
      integer :: iwgn = 0           !              |
      integer :: ly = 0             !              |soil layer number
      real :: rto = 0.              !              |
      real :: lai_init = 0.         !
      real :: lai_drop = 0.

      j = ihru
      idp = pcom(j)%plcur(ipl)%idplt
      iob = hru(j)%obj_no
      iwst = ob(iob)%wst
      iwgn = wst(iwst)%wco%wgn

      !! check for beginning of dormant season
      if (pcom(j)%plcur(ipl)%idorm == "n" .and. wst(iwst)%weat%daylength - dormhr(j) < wgn_pms(iwgn)%daylmn) then

        !! beginning of temperature based perennial dormant period - above ground senescence
        if (pldb(idp)%typ == "perennial") then
          pcom(j)%plcur(ipl)%idorm = "y"
          !! add dead stem mass to residue pool
          rto = 0. !***jga  pldb(idp)%bm_dieoff
          stem_drop = rto * pl_mass(j)%stem(ipl)
          !! lower lai by same ratio
          lai_init = pcom(j)%plg(ipl)%lai
          pcom(j)%plg(ipl)%lai = rto * lai_init
          !! compute leaf biomass drop
          leaf_drop%m = rto * pl_mass(j)%leaf(ipl)%m
          leaf_drop%n = leaf_drop%m * pcom(j)%plm(ipl)%n_fr
          leaf_drop%n = max (0., leaf_drop%n)
          leaf_drop%p = leaf_drop%m * pcom(j)%plm(ipl)%p_fr
          leaf_drop%p = max (0., leaf_drop%p)
          !! add all seed/fruit mass to residue pool
          seed_drop = pl_mass(j)%seed(ipl)
          abgr_drop = stem_drop + leaf_drop + seed_drop

          !! add all seed/fruit mass to residue pool
          pl_mass(j)%tot(ipl) = pl_mass(j)%tot(ipl) - abgr_drop
          if (pl_mass(j)%tot_com%m < 0.) then
            pl_mass(j)%tot_com%m = 0.
          end if
          pl_mass(j)%ab_gr(ipl) = pl_mass(j)%ab_gr(ipl) - abgr_drop
          pl_mass(j)%stem(ipl) = pl_mass(j)%stem(ipl) - stem_drop
          pl_mass(j)%leaf(ipl) = pl_mass(j)%leaf(ipl) - leaf_drop
          pl_mass(j)%seed(ipl) = plt_mass_z
          
          soil1(j)%rsd(1) = soil1(j)%rsd(1) + abgr_drop
          !! This allocation if cswat == 2 is done in cbn_rsd_decomp subroutine
          ! if (bsn_cc%cswat == 2) then
          !   soil1(j)%meta(ly) = soil1(j)%meta(ly) + 0.85 * abgr_drop
          !   soil1(j)%str(ly) = soil1(j)%str(ly) + 0.15 * abgr_drop
          !   soil1(j)%lig(ly) = soil1(j)%lig(ly) + 0.12 * abgr_drop  ! 0.12 = 0.8 * 0.15 -> lig = 80%str
          ! end if
          
          pcom(j)%plcur(ipl)%idorm = "y"
          pcom(j)%plcur(ipl)%phuacc = 0.
          pcom(j)%plstr(ipl)%strsw = 1.
        end if   ! beginning of dormancy for perennial

        !! beginning of cool season annual dormant period
        if (pldb(idp)%typ == "cold_annual") then
          if (pcom(j)%plcur(ipl)%phuacc < 0.75) then
            pcom(j)%plcur(ipl)%idorm = "y"
            pcom(j)%plstr(ipl)%strsw = 1.
          end if 
        end if  ! beginning of cool season annual dormant period
        
      end if    ! beginning of dormancy 

      !! check if end of dormant period
      if (pcom(j)%plcur(ipl)%idorm == "y" .and. wst(iwst)%weat%daylength - dormhr(j) >=   &
                                                                wgn_pms(iwgn)%daylmn) then
        if (pldb(idp)%typ == "perennial") then
          !! end of perennial dormant period
          pcom(j)%plcur(ipl)%idorm = "n"
          pcom(j)%plcur(ipl)%phuacc = 0.
          pcom(j)%plg(ipl)%d_senes = 0
        end if

        !! end of cool season annual dormant period
        if (pldb(idp)%typ == "cold_annual") then
          pcom(j)%plcur(ipl)%idorm = "n"
          pcom(j)%plcur(ipl)%phuacc = 0.
        end if

      end if    ! end of dormant period

      return
      end subroutine pl_dormant