       subroutine path_apply (frt_kg)
          
      !! calculate ground cover
      !! graze only if adequate biomass in HRU
            
!!    this subroutine applies pathogens leached to the plants and soil 

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!         ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    fert_kg      |kg/ha         |manure applied

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use soil_module
      use plant_module
      use pathogen_data_module
      use output_ls_pathogen_module
      use constituent_mass_module

      implicit none
            
      real, intent (in)  :: frt_kg
      real :: frt_t        !          |
      real :: gc           !none      |fraction of ground covered by plant foliage
      real :: gc1          !          | 
      real :: pl_frac      !0-1       |fraction of pesticide applied to each plant
      integer :: ipath     !none      |counter
      integer :: ipath_db  !          |pathogen type from pathogens.pth data input file
      integer :: j         !          |
      integer :: ipl       !none      |plant number
      
      !! add pathogens - #cfu/g * t(manure)/ha * 1.e6 g/t * ha/10,000 m^2 = 100.  **should be conc in manure
      gc = (1.99532 - erfc(1.333 * pcom(j)%lai_sum - 2.)) / 2.1
      if (gc < 0.) gc = 0.
      gc1 = 1. - gc
      do ipath = 1, cs_db%num_paths
        ipath_db = cs_db%path_num(ipath)
        frt_t = path_db(ipath_db)%fr_manure * frt_kg / 1000.
        !! update pathogen levels on foliage
        if (pcom(j)%lai_sum > 1.e-6) then
          do ipl = 1, pcom(j)%npl
            pl_frac = pcom(j)%plg(ipl)%lai / pcom(j)%lai_sum
            cs_pl(j)%pl_on(ipl)%path(ipath) = cs_pl(j)%pl_on(ipl)%pest(ipath) + gc * pl_frac * frt_kg
            hpath_bal(j)%path(ipath)%apply_plt = hpath_bal(j)%path(ipath)%apply_plt + gc * pl_frac * frt_kg
          end do
        end if
        !! update pathogen levels on ground
        hpath_bal(j)%path(ipath)%apply_sol =  gc1 * frt_t * 100.
        cs_soil(j)%ly(1)%path(ipath) = cs_soil(j)%ly(1)%path(ipath) + gc1 * frt_t * 100.
      end do

      return
      
      end subroutine path_apply