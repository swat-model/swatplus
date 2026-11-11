      subroutine pl_burnop (jj, iburn)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs all management operations             

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ibrn        |none          |counter in readmgt 
!!    phub        |              |heat units to schedule burning
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module
      use mgt_operations_module
      use organic_mineral_mass_module
      use hru_module, only : cn2, ipl
      use soil_module
      use plant_module
      use carbon_module
      
      implicit none      
      
      external :: curno
   
      integer :: j = 0                       !none          |counter
      integer, intent (in) :: jj             !none          |counter  
      integer, intent (in) :: iburn          !julian date   |date of burning
      real :: cnop = 0.                      !              |updated cn after fire
      real :: burn_p                         !kg P/ha       |burned (non-volatilized) P accumulated for return to stable humus (reset in body, no implicit SAVE)

      j = jj

      !!update curve number
      cnop = cn2(j) + fire_db(iburn)%cn2_upd
      if (cnop > 98.0) then
        cnop = 98.0
      end if
      call curno(cnop, j)
      
      !! zero total community masses
      pl_mass(j)%tot_com = plt_mass_z
      pl_mass(j)%ab_gr_com = plt_mass_z
      pl_mass(j)%leaf_com = plt_mass_z
      pl_mass(j)%stem_com = plt_mass_z
      pl_mass(j)%seed_com = plt_mass_z
      burn_p = 0.                            ! explicit reset each call (do not rely on implicit SAVE)

      !! burn surface (layer 1) humus once (not per plant). Burned P is not volatilized;
      !! it is accumulated in burn_p and returned to the stable humus P pool once, after
      !! the plant loop, so there is a single place that updates soil1(j)%hp(1)%p.
      pl_burn = fire_db(iburn)%fr_burn * (soil1(j)%hs(1) + soil1(j)%hp(1))
      soil1(j)%hs(1) = (1. - fire_db(iburn)%fr_burn) * soil1(j)%hs(1)
      soil1(j)%hp(1) = (1. - fire_db(iburn)%fr_burn) * soil1(j)%hp(1)
      burn_p = burn_p + pl_burn%p                          ! + humus P
      hsc_d(j)%emit_c = hsc_d(j)%emit_c + pl_burn%c        ! burned humus C -> soil carbon emission

      !!burn biomass and residue for each plant
      do ipl = 1, pcom(j)%npl
              
        !! burn this plant's surface residue; C -> residue emission, P returns to humus
        pl_burn = fire_db(iburn)%fr_burn * pl_mass(j)%abg_rsd(ipl)
        pl_mass(j)%abg_rsd(ipl) = pl_mass(j)%abg_rsd(ipl) - pl_burn
        pl_mass(j)%abg_rsd_tot = pl_mass(j)%abg_rsd_tot - pl_burn      ! decrement total by burned (was: overwrite with one plant)
        hrc_d(j)%emit_c = hrc_d(j)%emit_c + pl_burn%c          ! burned residue C -> residue carbon emission
        burn_p = burn_p + pl_burn%p                            ! + surface residue P
      
        !! burn all above ground plant components
        pl_burn = fire_db(iburn)%fr_burn * pl_mass(j)%ab_gr(ipl)
        pl_mass(j)%ab_gr(ipl) = (1. - fire_db(iburn)%fr_burn) * pl_mass(j)%ab_gr(ipl)
        pl_mass(j)%stem(ipl) = (1. - fire_db(iburn)%fr_burn) * pl_mass(j)%stem(ipl)
        pl_mass(j)%leaf(ipl) = (1. - fire_db(iburn)%fr_burn) * pl_mass(j)%leaf(ipl)
        pl_mass(j)%seed(ipl) = (1. - fire_db(iburn)%fr_burn) * pl_mass(j)%seed(ipl)
        !! burned above-ground plant C -> plant carbon emission
        hpc_d(j)%emit_c = hpc_d(j)%emit_c + pl_burn%c

        !! sum total community masses
        pl_mass(j)%tot_com = pl_mass(j)%tot_com + pl_mass(j)%tot(ipl)
        pl_mass(j)%ab_gr_com = pl_mass(j)%ab_gr_com + pl_mass(j)%ab_gr(ipl)
        pl_mass(j)%leaf_com = pl_mass(j)%leaf_com + pl_mass(j)%leaf(ipl)
        pl_mass(j)%stem_com = pl_mass(j)%stem_com + pl_mass(j)%stem(ipl)
        pl_mass(j)%seed_com = pl_mass(j)%seed_com + pl_mass(j)%seed(ipl)
        
      end do     ! npl loop

      !! return all burned (non-volatilized) P to the stable humus P pool - single update point
      soil1(j)%hp(1)%p = soil1(j)%hp(1)%p + burn_p

      return
      end subroutine pl_burnop