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
 
      integer :: j = 0                  !none           |HRU number
      integer, intent (in) :: jj        !none           |hru number
      real, intent (in) :: harveff      !0-1            |harvest efficiency
      real              :: eff          !0-1            |local scoope harvest efficiency
      integer, intent (in) :: iharvop   !               |harvest operation type
      
      j = jj
      !! prevent the harvest efficiency from being too small
      if (harveff < .00001) then
            eff = harvop_db(iharvop)%eff
      else
            eff = harveff
      endif
      
      !! zero stover harvest
      hrc_d(j)%harv_stov_c = 0.
      
      !! harvest plant surface residue
      soil1(j)%rsd(1) = (1. - eff) * soil1(j)%rsd(1)
      !! the partitioning is handleed in the cbn_rds_decomp subroutine
      ! soil1(j)%meta(1) = (1. - harveff) * soil1(j)%meta(1)
      ! soil1(j)%str(1) = (1. - harveff) * soil1(j)%str(1)
      ! soil1(j)%lig(1) = (1. - harveff) * soil1(j)%lig(1)
      
      !! compute carbon in harvested residue
      ! hrc_d(j)%harv_stov_c = hrc_d(j)%harv_stov_c + 0.42 * (1. - harveff) * soil1(j)%rsd(1)%c
      !! carbon residue is not accumulated for harvested residue 
      hrc_d(j)%harv_stov_c = 0.42 * soil1(j)%rsd(1)%c
      
      return
      end  subroutine mgt_harvresidue