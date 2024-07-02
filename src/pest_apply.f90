      subroutine pest_apply (jj, ipest, pest_kg, pestop)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine applies pesticide

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units            |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ap_ef(:)     |none             |application efficiency (0-1)
!!    drift(:)     |kg               |amount of pesticide drifting onto main 
!!                                   |channel in subbasin
!!    hru_km(:)    |km**2            |area of HRU in square kilometers
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    drift(:)    |kg            |amount of pesticide drifting onto main 
!!                               |channel in subbasin
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: Erfc

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use mgt_operations_module
      use basin_module
      use soil_module
      use plant_module
      use output_ls_pesticide_module
      use constituent_mass_module
      
      implicit none
      
      integer :: j                       !none          |HRU number
      integer, intent (in) :: jj         !none          |HRU number
      integer :: ipl                     !none          |plant number
      real :: gc                         !none          |fraction of ground covered by plant foliage
      integer, intent (in) :: ipest      !none          |sequential hru pesticide number
      integer, intent (in) :: pestop     !              | 
      real, intent (in) :: pest_kg       !kg/ha         |amount of pesticide applied 
      real :: surf_frac                  !kg/ha         |amount added to soil surface layer - rest to second layer 
      real :: pl_frac                    !0-1           |fraction of pesticide applied to each plant

      j = jj

      !! calculate ground cover
      gc = (1.99532 - erfc(1.333 * pcom(j)%lai_sum - 2.)) / 2.1
      if (gc < 0.) gc = 0.

      !! update pesticide levels on ground and foliage
      if (pcom(j)%lai_sum > 1.e-6) then
        do ipl = 1, pcom(j)%npl
          pl_frac = pcom(j)%plg(ipl)%lai / pcom(j)%lai_sum
          cs_pl(j)%pl_on(ipl)%pest(ipest) = cs_pl(j)%pl_on(ipl)%pest(ipest) + gc * pl_frac * pest_kg
        end do
      end if
      surf_frac = chemapp_db(pestop)%surf_frac
      cs_soil(j)%ly(1)%pest(ipest) = cs_soil(j)%ly(1)%pest(ipest) + (1. - gc) * surf_frac * pest_kg
      cs_soil(j)%ly(2)%pest(ipest) = cs_soil(j)%ly(2)%pest(ipest) + (1. - gc) * (1. - surf_frac) * pest_kg
      
      hpestb_d(j)%pest(ipest)%apply_f = gc * pest_kg
      hpestb_d(j)%pest(ipest)%apply_s = (1. - gc) * pest_kg

      return
      end subroutine pest_apply