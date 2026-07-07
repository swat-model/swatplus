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
      integer :: ipath     !none      |counter
      integer :: ipath_db  !          |pathogen type from pathogens.pth data input file
      integer :: j         !          |
      
      gc = (1.99532 - erfc(1.333 * pcom(j)%lai_sum - 2.)) / 2.1
      if (gc < 0.) gc = 0.
      gc1 = 1. - gc
      do ipath = 1, cs_db%num_paths
        ipath_db = cs_db%path_num(ipath)
        frt_t = path_db(ipath_db)%swf * frt_kg / 1000.
        !! add pathogens - #cfu/g * t(manure)/ha * 1.e6 g/t * ha/10,000 m^2 = 100.  **should be conc in manure
        hpath_bal(j)%path(ipath)%apply_sol =  gc1 * cs_soil(j)%ly(1)%path(ipath) * frt_t * 100. 
        hpath_bal(j)%path(ipath)%apply_plt = gc * cs_soil(j)%ly(1)%path(ipath) * frt_t * 100.
        cs_soil(j)%ly(1)%path(ipath) = cs_soil(j)%ly(1)%path(ipath) + hpath_bal(j)%path(ipath)%apply_sol
        cs_pl(j)%path(ipath) = cs_pl(j)%path(ipath) + hpath_bal(j)%path(ipath)%apply_plt
      end do

      return
      
      end subroutine path_apply