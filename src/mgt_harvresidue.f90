      subroutine mgt_harvresidue (jj, harveff)

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
      use organic_mineral_mass_module
      
      implicit none
 
      integer :: j = 0                  !none           |HRU number
      integer, intent (in) :: jj        !none           |hru number
      real, intent (in) :: harveff      !0-1            |harvest efficiency
      
      j = jj
      
      !! zero stover harvest
      hrc_d(j)%harv_stov_c = 0.
      
      !! harvest plant surface residue
      soil1(j)%rsd(1) = (1. - harveff) * soil1(j)%rsd(1)
      soil1(j)%meta(1) = (1. - harveff) * soil1(j)%meta(1)
      soil1(j)%str(1) = (1. - harveff) * soil1(j)%str(1)
      soil1(j)%lig(1) = (1. - harveff) * soil1(j)%lig(1)
      
      !! compute carbon in harvested residue
      hrc_d(j)%harv_stov_c = hrc_d(j)%harv_stov_c + 0.42 * (1. - harveff) * soil1(j)%rsd(1)%c
      
      return
      end  subroutine mgt_harvresidue