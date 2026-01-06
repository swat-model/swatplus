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
      integer :: ipl = 0                !none           |sequential plant number
      
      j = jj
      !! prevent the harvest efficiency from being too small
      if (harveff < .00001) then
            eff = harvop_db(iharvop)%eff
      else
            eff = harveff
      endif
      
      !! zero stover harvest
      hrc_d(j)%harv_stov_c = 0.
      
      pl_mass(j)%rsd_tot = orgz
      !! harvest plant surface residue
      do ipl = 1, pcom(j)%npl
        pl_mass(j)%rsd(ipl) = (1. - eff) * pl_mass(j)%rsd(ipl) 
        pl_mass(j)%rsd_tot = pl_mass(j)%rsd_tot + pl_mass(j)%rsd(ipl)
        !! compute carbon in harvested residue
        hrc_d(j)%harv_stov_c = pl_mass(j)%rsd_tot%c / eff - pl_mass(j)%rsd_tot%c
      end do
      
      return
      end  subroutine mgt_harvresidue