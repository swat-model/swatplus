      subroutine mgt_harvresidue (jj, harveff, iharvop)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs the harvest residue operation 

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

      use plant_module
      use carbon_module
      use mgt_operations_module
      use organic_mineral_mass_module
      
      implicit none
 
      integer, intent (in) :: jj        !none           |hru number
      real, intent (in) :: harveff      !0-1            |harvest efficiency
      type (organic_mass) :: rsd_removed
      real              :: eff          !0-1            |local scoope harvest efficiency
      real    :: harv_idx               !none           |harvest index 
      real    :: net_eff                !none           |equipment efficiency times harvest efficiency 
      real    :: reduction_frac         !               |fractional adjustment if residue remaining is than bm_min
      real    :: bm_min                 !               |minimum biomass that must remain after residue removal 
      integer, intent (in) :: iharvop   !               |harvest operation type
      integer :: ipl = 0                !none           |sequential plant number
      integer :: j = 0

      
      j = jj
      !! prevent the harvest efficiency from being too small
      if (harveff < .00001) then
            eff = harvop_db(iharvop)%eff
      else
            eff = harveff
      endif
      harv_idx = harvop_db(iharvop)%hi_ovr
      bm_min = harvop_db(iharvop)%bm_min
      net_eff = eff * harv_idx
      
      !! zero stover harvest
      hrc_d(j)%harv_stov_c = 0.
      
      ! pl_mass(j)%rsd_tot = orgz
      !! harvest plant surface residue
      rsd_removed = orgz
      do ipl = 1, pcom(j)%npl
      !   pl_mass(j)%rsd(ipl) = (1. - eff) * pl_mass(j)%rsd(ipl) 
      !   pl_mass(j)%rsd_tot = pl_mass(j)%rsd_tot + pl_mass(j)%rsd(ipl)
      !!  compute carbon in harvested residue
      !   hrc_d(j)%harv_stov_c = (pl_mass(j)%rsd_tot%c / eff) - pl_mass(j)%rsd_tot%c

        rsd_removed = net_eff * pl_mass(j)%rsd(ipl)

        ! Check to see if more residue was removed than allowed
        if ((pl_mass(j)%rsd(ipl)%m - rsd_removed%m) < bm_min) then
            reduction_frac = (pl_mass(j)%rsd(ipl)%m - bm_min) / pl_mass(j)%rsd(ipl)%m
            rsd_removed = reduction_frac * pl_mass(j)%rsd(ipl)
        endif

        pl_mass(j)%rsd(ipl) = pl_mass(j)%rsd(ipl) - rsd_removed 
        pl_mass(j)%rsd_tot = pl_mass(j)%rsd_tot - rsd_removed
        if (pl_mass(j)%rsd_tot%m < 1.e-6) then
          pl_mass(j)%rsd_tot = orgz
        endif

        hrc_d(j)%harv_stov_c = rsd_removed%c


      end do
      
      return
      end  subroutine mgt_harvresidue