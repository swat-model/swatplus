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

      integer :: j                  !none          |HRU number
      integer :: idp                !              |
      integer :: iob                !              |
      integer :: iwgn               !              |
      real :: rto                   !              |
      real :: lai_init              !
      real :: lai_drop

      j = ihru
      idp = pcom(j)%plcur(ipl)%idplt
      iob = hru(j)%obj_no
      iwst = ob(iob)%wst
      iwgn = wst(iwst)%wco%wgn

      !! check for beginning of dormant season
      if (pcom(j)%plcur(ipl)%idorm == "n" .and. wst(iwst)%weat%daylength - dormhr(j) < wgn_pms(iwgn)%daylmn) then

        !! beginning of temperature based perennial dormant period - leaf drop
        !if (pldb(idp)%typ == "perennial") then
        !  pcom(j)%plcur(ipl)%idorm = "y"
        !  !! add leaf mass to residue pool
        !  rsd1(j)%tot(1) = pl_mass(j)%leaf(ipl) + rsd1(j)%tot(1)
        !end if

        !! beginning of temperature based perennial dormant period - above ground senescence
        if (pldb(idp)%typ == "perennial") then
          pcom(j)%plcur(ipl)%idorm = "y"
          !! add dead stem mass to residue pool
          rto = pldb(idp)%bm_dieoff
          abgr_drop = rto * pl_mass(j)%stem(ipl)
          !! drop lai to minimum if not already
          lai_init = pcom(j)%plg(ipl)%lai
          pcom(j)%plg(ipl)%lai = pldb(idp)%alai_min
          !! compute leaf biomass drop
          if (lai_init > 0.001) then
            lai_drop = (lai_init - pcom(j)%plg(ipl)%lai) / lai_init
          else
            lai_drop = 0.
          end if
          lai_drop = max (0., lai_drop)
          lai_drop = amin1 (1., lai_drop)
          leaf_drop%m = lai_drop * pl_mass(j)%leaf(ipl)%m
          leaf_drop%n = leaf_drop%m * pcom(j)%plm(ipl)%n_fr
          leaf_drop%n = max (0., leaf_drop%n)
          leaf_drop%p = leaf_drop%m * pcom(j)%plm(ipl)%p_fr
          leaf_drop%p = max (0., leaf_drop%p)

          pl_mass(j)%tot(ipl) = pl_mass(j)%tot(ipl) - abgr_drop - leaf_drop
          pl_mass(j)%ab_gr(ipl) = pl_mass(j)%ab_gr(ipl) - abgr_drop - leaf_drop
          pl_mass(j)%stem(ipl) = pl_mass(j)%stem(ipl) - abgr_drop
          pl_mass(j)%leaf(ipl) = pl_mass(j)%leaf(ipl) - leaf_drop
          rsd1(j)%tot(1) = rsd1(j)%tot(1) + abgr_drop + leaf_drop
          !! add all seed/fruit mass to residue poolpool
          abgr_drop = pl_mass(j)%seed(ipl)
          pl_mass(j)%tot(ipl) = pl_mass(j)%tot(ipl) - abgr_drop
          pl_mass(j)%ab_gr(ipl) = pl_mass(j)%ab_gr(ipl) - abgr_drop
          pl_mass(j)%seed(ipl) = plt_mass_z
          rsd1(j)%tot(1) = rsd1(j)%tot(1) + abgr_drop
        end if

        !! beginning of cool season annual dormant period
        if (pldb(idp)%typ == "cold_annual") then
          if (pcom(j)%plcur(ipl)%phuacc < 0.75) then
            pcom(j)%plcur(ipl)%idorm = "y"
            pcom(j)%plstr(ipl)%strsw = 1.
          end if 
        end if
      end if

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

      end if

      return
      end subroutine pl_dormant