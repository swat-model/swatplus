      subroutine pest_apply (jj, ipest, pest_kg, pestop)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine applies pesticide

      use mgt_operations_module
      use basin_module
      use soil_module
      use plant_module
      use output_ls_pesticide_module
      use constituent_mass_module
      
      implicit none
      
      integer :: j = 0                   !none          |HRU number
      integer, intent (in) :: jj         !none          |HRU number
      integer :: ipl = 0                 !none          |plant number
      real :: gc = 0.                    !none          |fraction of ground covered by plant foliage
      integer, intent (in) :: ipest      !none          |sequential hru pesticide number
      integer, intent (in) :: pestop     !              | 
      real, intent (in) :: pest_kg       !kg/ha         |amount of pesticide applied 
      real :: surf_frac = 0.             !kg/ha         |amount added to soil surface layer - rest to second layer 
      real :: pl_frac = 0.               !0-1           |fraction of pesticide applied to each plant
      !! BUG FIX: local lai_sum recomputed from current per-plant LAI
      real :: lai_sum_local = 0.         !m2/m2         |recomputed lai_sum for this HRU

      j = jj

      !! BUG FIX: Recompute lai_sum from current per-plant LAI values.
      !! pcom%lai_sum is set by pl_community (hru_control line 376), which runs
      !! AFTER pest_apply (called via actions/mgt_operatn at lines 160/275).
      !! Management operations (plant, grow_init, kill, harvest) in the same
      !! daily loop can change individual plg%lai without updating lai_sum,
      !! making sum(plg%lai / lai_sum) != 1. This caused foliage to receive
      !! different mass than apply_f records.
      lai_sum_local = 0.
      do ipl = 1, pcom(j)%npl
        lai_sum_local = lai_sum_local + pcom(j)%plg(ipl)%lai
      end do

      !! calculate ground cover using fresh lai_sum
      gc = (1.99532 - erfc(1.333 * lai_sum_local - 2.)) / 2.1
      if (gc < 0.) gc = 0.

      !! update pesticide levels on ground and foliage
      if (lai_sum_local > 1.e-6) then
        do ipl = 1, pcom(j)%npl
          pl_frac = pcom(j)%plg(ipl)%lai / lai_sum_local
          cs_pl(j)%pl_on(ipl)%pest(ipest) = cs_pl(j)%pl_on(ipl)%pest(ipest) + gc * pl_frac * pest_kg
        end do
      end if
      surf_frac = chemapp_db(pestop)%surf_frac
      cs_soil(j)%ly(1)%pest(ipest) = cs_soil(j)%ly(1)%pest(ipest) + (1. - gc) * surf_frac * pest_kg
      cs_soil(j)%ly(2)%pest(ipest) = cs_soil(j)%ly(2)%pest(ipest) + (1. - gc) * (1. - surf_frac) * pest_kg
      
      !! BUG FIX: use += instead of = to correctly accumulate when
      !! multiple application events (DTL + scheduled) target the same
      !! pesticide on the same HRU on the same day
      hpestb_d(j)%pest(ipest)%apply_f = hpestb_d(j)%pest(ipest)%apply_f + gc * pest_kg
      hpestb_d(j)%pest(ipest)%apply_s = hpestb_d(j)%pest(ipest)%apply_s + (1. - gc) * pest_kg

      return
      end subroutine pest_apply