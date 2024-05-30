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
      use soil_module
      use constituent_mass_module
      
      implicit none
 
      integer :: j                      !none           |HRU number
      integer :: k                      !none           |pesticide number
      integer :: ly                     !none           |soil layer number
      integer, intent (in) :: jj        !none           |hru number
      integer, intent (in) :: iplant    !               |plant number from plant community
      integer, intent (in) :: iharvop   !               |harvest operation type
      real :: harveff                   !0-1            |harvest efficiency
      integer :: idp                    !none           |plant number from plants.plt
      real :: yld_rto                   !0-1            |yield to total biomass ratio
      real :: yldpst                    !kg pst/ha          |pesticide removed in yield
      j = jj
      ipl = iplant
      idp = pcom(j)%plcur(ipl)%idplt
      harveff = harvop_db(iharvop)%eff
      
      !! multiply root biomass by harvest efficiency
      pl_yield = harveff * pl_mass(j)%root(ipl)
      
      !! remove harvested root mass from existing root mass
      pl_mass(j)%root(ipl) = pl_mass(j)%root(ipl) - pl_yield
      
      !! update root fractions in each layer
      call pl_rootfr
      
      !! allocate remaining dead roots, N, P to soil layers
	  do ly = 1, soil(j)%nly
        soil1(j)%rsd(ly) = soil(j)%ly(ly)%rtfr * pl_mass(j)%root(ipl) + soil1(j)%rsd(ly)
      end do
      
      !! apply pest stress to harvest index - mass lost due to pests - don't add to residue
      pl_yield = (1. - pcom(j)%plcur(ipl)%pest_stress) * pl_yield
      
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

      !! add above ground mass to residue pool
      rsd1(j)%tot(1) = rsd1(j)%tot(1) + pl_mass(j)%ab_gr(ipl)

      !! zero all plant components - assume tuber harvest kills plant
      pl_mass(j)%tot(ipl) = plt_mass_z
      pl_mass(j)%ab_gr(ipl) = plt_mass_z
      pl_mass(j)%leaf(ipl) = plt_mass_z
      pl_mass(j)%stem(ipl) = plt_mass_z
      pl_mass(j)%seed(ipl) = plt_mass_z
      pl_mass(j)%root(ipl) = plt_mass_z

      return
      end subroutine mgt_harvtuber