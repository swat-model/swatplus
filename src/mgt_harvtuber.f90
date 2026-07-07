      subroutine mgt_harvtuber (jj, iplant, iharvop)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs the harvest grain only operation 

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

      use basin_module
      use hru_module, only : ipl
      use plant_module
      use plant_data_module
      use mgt_operations_module
      use carbon_module
      use organic_mineral_mass_module
      
      implicit none
 
      integer :: j                      !none           |HRU number
      integer, intent (in) :: jj        !none           |hru number
      integer, intent (in) :: iplant    !               |plant number from plant community
      integer, intent (in) :: iharvop   !               |harvest operation type
      real :: harveff                   !0-1            |harvest efficiency
      integer :: idp                    !none           |plant number from plants.plt
      real :: harveff1                  !0-1            |1.-harveff
      real :: remain                    !0-1            |
      real :: hvst                      !0-1            |
      
      j = jj
      ipl = iplant
      idp = pcom(j)%plcur(ipl)%idplt
      harveff = harvop_db(iharvop)%eff
      
      !! hi is tuber mass -- yield = 1 - 1/(hi+1)
      remain = 1. / (pcom(j)%plcur(ipl)%harv_idx + 1.)
      hvst = 1. - remain
      pl_mass(j)%tot(ipl) = remain * pl_mass(j)%tot(ipl)
      pl_yield = hvst * pl_yield
      
      !! multiply by harvest efficiency
      pl_yield = harveff * pl_mass(j)%seed(ipl)
            
      !! apply pest stress to harvest index - mass lost due to pests - don't add to residue
      pl_yield = (1. - pcom(j)%plcur(ipl)%pest_stress) * pl_yield
      
      !! add remaining tuber (seed) mass to slow humus pool of soil - to preserve balances
      harveff1 = 1. - harveff
      soil1(j)%hact(1) = harveff1 * pl_mass(j)%seed(ipl) + soil1(j)%hact(1)
      
      !! set other masses
      pl_mass(j)%ab_gr(ipl) = pl_mass(j)%tot(ipl)
      pl_mass(j)%seed(ipl) = plt_mass_z
      pl_mass(j)%root(ipl) = plt_mass_z
      pl_mass(j)%leaf(ipl) = plt_mass_z
      pl_mass(j)%stem(ipl) = plt_mass_z

      return
      end  subroutine mgt_harvtuber