      subroutine mgt_harvgrain (jj, iplant, iharvop)

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
      use constituent_mass_module
      
      implicit none
 
      integer :: j                      !none           |HRU number
      integer :: k                      !none           |pesticide number
      integer, intent (in) :: jj        !none           |hru number
      integer, intent (in) :: iplant    !               |plant number from plant community
      integer, intent (in) :: iharvop   !               |harvest operation type
      real :: harveff                   !0-1            |harvest efficiency
      integer :: idp                    !none           |plant number from plants.plt
      real :: harveff1                  !0-1            |1.-harveff
      real :: yld_rto                   !0-1            |yield to total biomass ratio
      real :: yldpst                    !kg pst/ha          |pesticide removed in yield
      
      j = jj
      ipl = iplant
      idp = pcom(j)%plcur(ipl)%idplt
      harveff = harvop_db(iharvop)%eff

      !! check for minimum harvest index
      pcom(j)%plcur(ipl)%harv_idx = max (pcom(j)%plcur(ipl)%harv_idx, pldb(idp)%wsyf)
        
      !! remove seed mass from total plant mass and calculate yield
      pl_mass(j)%tot(ipl) = pl_mass(j)%tot(ipl) - pl_mass(j)%seed(ipl)
      pl_mass(j)%ab_gr(ipl) = pl_mass(j)%ab_gr(ipl) - pl_mass(j)%seed(ipl)
      pl_yield = harveff * pl_mass(j)%seed(ipl)
      
      !! apply pest stress to harvest index - mass lost due to pests - don't add to residue
      pl_yield = (1. - pcom(j)%plcur(ipl)%pest_stress) * pl_yield
      !! add plant carbon for printing
      hpc_d(j)%harv_c = hpc_d(j)%harv_c + pl_yield%c
      
      !! add seed mass from harveff to slow humus pool of soil - to preserve balances
      harveff1 = 1. - harveff
      soil1(j)%hact(1) = harveff1 * pl_mass(j)%seed(ipl) + soil1(j)%hact(1)
      
      !! zero seed mass
      pl_mass(j)%seed(ipl) = plt_mass_z
      
	  !! adjust foliar and internal pesticide for grain removal
      do k = 1, cs_db%num_pests
        !! calculate amount of pesticide removed with yield
        yld_rto = pl_yield%m / pl_mass(j)%tot(ipl)%m
        yldpst = yld_rto * (cs_pl(j)%pl_in(ipl)%pest(k) + cs_pl(j)%pl_on(ipl)%pest(k))
        cs_pl(j)%pl_in(ipl)%pest(k) = cs_pl(j)%pl_in(ipl)%pest(k) - (1. - yld_rto) *    &
                                                           cs_pl(j)%pl_in(ipl)%pest(k)
        cs_pl(j)%pl_in(ipl)%pest(k) = Max (0., cs_pl(j)%pl_in(ipl)%pest(k))
        cs_pl(j)%pl_on(ipl)%pest(k) = cs_pl(j)%pl_on(ipl)%pest(k) - (1. - yld_rto) *    &
                                                           cs_pl(j)%pl_on(ipl)%pest(k)
        cs_pl(j)%pl_on(ipl)%pest(k) = Max (0., cs_pl(j)%pl_on(ipl)%pest(k))
      end do   

      return
      end subroutine mgt_harvgrain