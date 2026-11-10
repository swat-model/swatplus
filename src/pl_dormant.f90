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
      real :: rto = 0.              !              |
      real :: lai_init = 0.         !

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
          
          !! subtract seed/fruit mass from pools
          seed_drop = pl_mass(j)%seed(ipl)
          pl_mass(j)%seed(ipl) = plt_mass_z
          pl_mass(j)%ab_gr(ipl) = pl_mass(j)%ab_gr(ipl) - seed_drop
          pl_mass(j)%tot(ipl) = pl_mass(j)%tot(ipl) - seed_drop
          pl_mass(j)%ab_gr_com = pl_mass(j)%ab_gr_com - seed_drop
          pl_mass(j)%tot_com = pl_mass(j)%tot_com - seed_drop
          
          !! add seed/fruit mass to residue pool
          pl_mass(j)%abg_rsd(ipl) = pl_mass(j)%abg_rsd(ipl) + seed_drop
          pl_mass(j)%abg_rsd_tot = pl_mass(j)%abg_rsd_tot + seed_drop
          
          pcom(j)%plcur(ipl)%idorm = "y"
          pcom(j)%plcur(ipl)%phuacc = 0.
          pcom(j)%plstr(ipl)%strsw = 1.
        end if   ! beginning of dormancy for perennial
        
        !! beginning of temperature based perennial dormant period - above ground senescence
        if (pldb(idp)%typ == "herb_perennial") then
          pcom(j)%plcur(ipl)%idorm = "y"
          
          !! all above ground biomass dies off - add to residue pool
          !! 60% of n and p in above ground biomass is translocated to roots, 40% goes to residue pool
          pl_mass(j)%root(ipl)%n = pl_mass(j)%root(ipl)%n + 0.6 * pl_mass(j)%ab_gr(ipl)%n
          pl_mass(j)%root_com%n = pl_mass(j)%root_com%n + 0.6 * pl_mass(j)%ab_gr(ipl)%n
          pl_mass(j)%root(ipl)%p = pl_mass(j)%root(ipl)%p + 0.6 * pl_mass(j)%ab_gr(ipl)%p
          pl_mass(j)%root_com%p = pl_mass(j)%root_com%p + 0.6 * pl_mass(j)%ab_gr(ipl)%p
          
          !! 40% of n and p in above ground biomass goes to residue pool
          pl_mass(j)%abg_rsd(ipl)%n = pl_mass(j)%abg_rsd(ipl)%n + 0.4 * pl_mass(j)%ab_gr(ipl)%n
          pl_mass(j)%abg_rsd_tot%n = pl_mass(j)%abg_rsd_tot%n + 0.4 * pl_mass(j)%ab_gr(ipl)%n
          pl_mass(j)%abg_rsd(ipl)%p = pl_mass(j)%abg_rsd(ipl)%p + 0.4 * pl_mass(j)%ab_gr(ipl)%p
          pl_mass(j)%abg_rsd_tot%p = pl_mass(j)%abg_rsd_tot%p + 0.4 * pl_mass(j)%ab_gr(ipl)%p
          
          !! 100% of m and c in above ground biomass goes to residue pool
          pl_mass(j)%abg_rsd(ipl)%m = pl_mass(j)%abg_rsd(ipl)%m + pl_mass(j)%ab_gr(ipl)%m
          pl_mass(j)%abg_rsd_tot%m = pl_mass(j)%abg_rsd_tot%m + pl_mass(j)%ab_gr(ipl)%m
          pl_mass(j)%abg_rsd(ipl)%c = pl_mass(j)%abg_rsd(ipl)%c + pl_mass(j)%ab_gr(ipl)%c
          pl_mass(j)%abg_rsd_tot%c = pl_mass(j)%abg_rsd_tot%c + pl_mass(j)%ab_gr(ipl)%c
          
          pcom(j)%plcur(ipl)%idorm = "y"
          pcom(j)%plcur(ipl)%phuacc = 0.
          pcom(j)%plstr(ipl)%strsw = 1.
          pcom(j)%plg(ipl)%lai = 0.
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
        if (pldb(idp)%typ == "perennial" .or. pldb(idp)%typ == "herb_perennial") then
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