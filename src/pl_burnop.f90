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
   
      integer :: j                           !none          |counter
      integer, intent (in) :: jj             !none          |counter  
      integer, intent (in) :: iburn          !julian date   |date of burning
      real :: cnop                           !              |updated cn after fire
      real :: fr_burn                        !              |fraction burned
      real :: pburn                          !              |amount of phosphorus that burns - removed from plant
                                             !              |phosphorus and added to soil organic phosphorus 

      j = jj

      !!update curve number
      cnop = cn2(j) + fire_db(iburn)%cn2_upd
      call curno(cnop,j)
      
      !!burn biomass and residue
      fr_burn = fire_db(iburn)%fr_burn
      pl_mass(j)%tot(ipl)%m = pl_mass(j)%tot(ipl)%m * fr_burn
      pl_mass(j)%tot(ipl)%n = pl_mass(j)%tot(ipl)%n * fr_burn
      pburn = pl_mass(j)%tot(ipl)%p * fr_burn
      soil1(j)%hsta(1)%p = soil1(j)%hsta(1)%p + pburn
      pl_mass(j)%tot(ipl)%p = pl_mass(j)%tot(ipl)%p - pburn
      rsd1(j)%tot_com%m = rsd1(j)%tot_com%m * fr_burn
      rsd1(j)%tot(1)%n = rsd1(j)%tot(1)%n * fr_burn
      soil1(j)%hact(1)%n = soil1(j)%hact(1)%n * fr_burn
      soil1(j)%hsta(1)%n = soil1(j)%hsta(1)%n* fr_burn

      !!insert new biomss by zhang	  
      !!=================================
      if (bsn_cc%cswat == 2) then
          rsd1(j)%tot_meta%m = rsd1(j)%tot_meta%m * fr_burn
          rsd1(j)%tot_str%m = rsd1(j)%tot_str%m * fr_burn
          rsd1(j)%tot_str%c = rsd1(j)%tot_str%c * fr_burn
          rsd1(j)%tot_str%n = rsd1(j)%tot_str%n * fr_burn
          rsd1(j)%tot_meta%c = rsd1(j)%tot_meta%c * fr_burn
          rsd1(j)%tot_meta%n = rsd1(j)%tot_meta%n * fr_burn
          rsd1(j)%tot_lignin%c = rsd1(j)%tot_lignin%c * fr_burn

          hpc_d(j)%emit_c = hpc_d(j)%emit_c + pl_mass(j)%tot(ipl)%m * (1. - fr_burn)
          hrc_d(j)%emit_c = hrc_d(j)%emit_c + rsd1(j)%tot_com%m * (1. - fr_burn)  
      end if 
      !!insert new biomss by zhang
      !!=================================

      return
      end subroutine pl_burnop