      subroutine mgt_harvresidue (jj, harveff)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs the harvest residue operation 

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

      !use basin_module
      !use hru_module, only : ipl
      use plant_module
      !use plant_data_module
      !use mgt_operations_module
      !use carbon_module
      use organic_mineral_mass_module
      
      implicit none
 
      integer :: j                      !none           |HRU number
      integer, intent (in) :: jj        !none           |hru number
      real, intent (in) :: harveff      !0-1            |harvest efficiency
      integer :: ipl                    !none           |sequential plant number in community
      
      j = jj
      
      do ipl = 1, pcom(j)%npl        !! harvest each plant residue
        rsd1(j)%tot(ipl) = harveff * rsd1(j)%tot(ipl)
        rsd1(j)%meta(ipl) = harveff * rsd1(j)%meta(ipl)
        rsd1(j)%str(ipl) = harveff * rsd1(j)%str(ipl)
        rsd1(j)%lignin(ipl) = harveff * rsd1(j)%lignin(ipl)
      end do
      
      !! harvest total residue
      rsd1(j)%tot_com = harveff * rsd1(j)%tot_com
      rsd1(j)%tot_meta = harveff * rsd1(j)%tot_meta
      rsd1(j)%tot_str = harveff * rsd1(j)%tot_str
      rsd1(j)%tot_lignin = harveff * rsd1(j)%tot_lignin
      
      return
      end  subroutine mgt_harvresidue