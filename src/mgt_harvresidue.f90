      subroutine mgt_harvresidue (jj, iplant, harveff, iharvop)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs the harvest residue operation

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    jj          |none           |hru number
!!    iplant      |none           |sequential plant number within the community
!!    harveff     |0-1            |harvest efficiency
!!    iharvop     |none           |harvest operation type

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

      use plant_module
      use carbon_module
      use mgt_operations_module
      use organic_mineral_mass_module

      implicit none

      integer, intent (in) :: jj        !none           |hru number
      integer, intent (in) :: iplant    !none           |sequential plant number
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
      !! operate on the targeted plant only. the caller (mgt_sched / actions) has
      !! already matched the operation's crop name against the community; looping
      !! over every plant here discarded that targeting and let a residue harvest
      !! named for one crop remove -- or, below bm_min, create -- another crop's residue.
      ipl = iplant
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

      !! harvest plant surface residue
      rsd_removed = orgz

      !! nothing to harvest, and the bm_min correction below divides by this mass
      if (pl_mass(j)%abg_rsd(ipl)%m > 1.e-6) then

        rsd_removed = net_eff * pl_mass(j)%abg_rsd(ipl)

        ! Check to see if more residue was removed than allowed
        if ((pl_mass(j)%abg_rsd(ipl)%m - rsd_removed%m) < bm_min) then
            !! floor at zero: residue already below bm_min gives a negative fraction,
            !! which would ADD mass rather than remove it and pin the pool at bm_min
            reduction_frac = max (0., (pl_mass(j)%abg_rsd(ipl)%m - bm_min) /            &
                                       pl_mass(j)%abg_rsd(ipl)%m)
            rsd_removed = reduction_frac * pl_mass(j)%abg_rsd(ipl)
        endif

        pl_mass(j)%abg_rsd(ipl) = pl_mass(j)%abg_rsd(ipl) - rsd_removed
        pl_mass(j)%abg_rsd_tot = pl_mass(j)%abg_rsd_tot - rsd_removed
        if (pl_mass(j)%abg_rsd_tot%m < 1.e-6) then
          pl_mass(j)%abg_rsd_tot = orgz
        endif

        hrc_d(j)%harv_stov_c = rsd_removed%c

      endif

      return
      end  subroutine mgt_harvresidue
