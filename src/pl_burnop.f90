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
   
      integer :: j = 0                       !none          |counter
      integer, intent (in) :: jj             !none          |counter  
      integer, intent (in) :: iburn          !julian date   |date of burning
      real :: cnop = 0.                      !              |updated cn after fire
      real :: fr_burn = 0.                   !              |fraction burned
      real :: pburn = 0.                     !              |amount of phosphorus that burns - removed from plant
                                             !              |phosphorus and added to soil organic phosphorus 

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
      
      !!burn biomass and residue for each plant
      do ipl = 1, pcom(j)%npl
        
        !! burn all above ground plant components
        pl_burn = fire_db(iburn)%fr_burn * pl_mass(j)%ab_gr(ipl)
        pl_mass(j)%ab_gr(ipl) = (1. - fire_db(iburn)%fr_burn) * pl_mass(j)%ab_gr(ipl)
        pl_mass(j)%stem(ipl) = (1. - fire_db(iburn)%fr_burn) * pl_mass(j)%stem(ipl)
        pl_mass(j)%leaf(ipl) = (1. - fire_db(iburn)%fr_burn) * pl_mass(j)%leaf(ipl)
        pl_mass(j)%seed(ipl) = (1. - fire_db(iburn)%fr_burn) * pl_mass(j)%seed(ipl)
        !! carbon in co2 emitted during burning
        hrc_d(j)%emit_c = hrc_d(j)%emit_c + pl_burn%c
        hpc_d(j)%emit_c = hpc_d(j)%emit_c + pl_burn%c
        
        !! burn all surface (layer 1) residue and humus components
        if (bsn_cc%cswat == 1) then
          !! const carbon (bsn_cc%cswat == 1)
          pl_burn = fire_db(iburn)%fr_burn * (soil1(j)%hact(1) + soil1(j)%hsta(1))
          soil1(j)%rsd(1) = (1. - fire_db(iburn)%fr_burn) * soil1(j)%rsd(1)
          soil1(j)%hact(1) = (1. - fire_db(iburn)%fr_burn) * soil1(j)%hact(1)
          soil1(j)%hsta(1) = (1. - fire_db(iburn)%fr_burn) * soil1(j)%hsta(1)
          !! add plant p burn to stable humus pool for constant carbon
          soil1(j)%hsta(1)%p = soil1(j)%hsta(1)%p + pl_burn%p
        else
          !! dynamic carbon (bsn_cc%cswat == 2)
          pl_burn = fire_db(iburn)%fr_burn * (soil1(j)%hs(1) + soil1(j)%hp(1) + soil1(j)%rsd(ipl))
          soil1(j)%rsd(1) = (1. - fire_db(iburn)%fr_burn) * soil1(j)%rsd(1)
          soil1(j)%hs(1) = (1. - fire_db(iburn)%fr_burn) * soil1(j)%hs(1)
          soil1(j)%hp(1) = (1. - fire_db(iburn)%fr_burn) * soil1(j)%hp(1)
          soil1(j)%meta(1) = (1. - fire_db(iburn)%fr_burn) * soil1(j)%meta(1)
          soil1(j)%str(1) = (1. - fire_db(iburn)%fr_burn) * soil1(j)%str(1)
          soil1(j)%lig(1) = (1. - fire_db(iburn)%fr_burn) * soil1(j)%lig(1)
          !! add plant p burn to stable humus pool for constant carbon
          soil1(j)%hp(1)%p = soil1(j)%hp(1)%p + pl_burn%p
        end if
       
        !! sum total community masses
        pl_mass(j)%tot_com = pl_mass(j)%tot_com + pl_mass(j)%tot(ipl)
        pl_mass(j)%ab_gr_com = pl_mass(j)%ab_gr_com + pl_mass(j)%ab_gr(ipl)
        pl_mass(j)%leaf_com = pl_mass(j)%leaf_com + pl_mass(j)%leaf(ipl)
        pl_mass(j)%stem_com = pl_mass(j)%stem_com + pl_mass(j)%stem(ipl)
        pl_mass(j)%seed_com = pl_mass(j)%seed_com + pl_mass(j)%seed(ipl)
        
       
      end do     ! npl loop

      return
      end subroutine pl_burnop