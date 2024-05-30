      subroutine mgt_harvbiomass (jj, iplant, iharvop)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs the harvest operation for above ground biomass (no kill)

      use organic_mineral_mass_module
      use soil_module
      use plant_module
      use plant_data_module
      use mgt_operations_module
      use constituent_mass_module
      use carbon_module
      
      implicit none
     
      integer :: j                      !none               |HRU number
      integer :: k                      !none               |pesticide counter
      integer :: idp                    !                   |
      integer, intent (in) :: jj        !none               |counter
      integer, intent (in) :: iplant    !                   |plant number xwalked from hlt_db()%plant and plants.plt
      integer, intent (in) :: iharvop   !                   |harvest operation type
      integer :: ipl                    !none               |counter
      real :: clippst                   !kg pst/ha          |pesticide in clippings
      real :: yldpst                    !kg pst/ha          |pesticide removed in yield
      real :: hi_tot                    !kg/ha)/(kg/ha)     |total harvest index = hi_ovr * harveff
      real :: hi_ovr                    !kg/ha)/(kg/ha)     |harvest index target specified at harvest
      real :: harveff                   !0-1                |harvest efficiency
      real :: clip                      !0-1                |1.-harveff
      real :: yld_rto                   !0-1            |yield to total biomass ratio

      j = jj
      ipl = iplant
            
      idp = pcom(j)%plcur(ipl)%idplt
      hi_ovr = harvop_db(iharvop)%hi_ovr
      harveff = harvop_db(iharvop)%eff

      !! remove yield from seed, leaf, and stem - using hi_ovr for all parts
      hi_tot = hi_ovr * harveff
      harv_seed = hi_tot * pl_mass(j)%seed(ipl)
      harv_leaf = hi_tot * pl_mass(j)%leaf(ipl)
      harv_stem = hi_tot * pl_mass(j)%stem(ipl)
      pl_yield = harv_seed + harv_leaf
      pl_yield = pl_yield + harv_stem
            
      !! apply pest stress to harvest index - mass lost due to pests - don't add to residue
      pl_yield = (1. - pcom(j)%plcur(ipl)%pest_stress) * pl_yield
      !! add plant carbon for printing
      hrc_d(j)%plant_c = hrc_d(j)%plant_c + pl_yield%c
      hpc_d(j)%drop_c = hpc_d(j)%drop_c + pl_yield%c
      
	  !! adjust foliar and internal pesticide for plant removal
      do k = 1, cs_db%num_pests
        !! calculate amount of pesticide removed with yield and clippings
        yld_rto = (hi_tot * pl_mass(j)%ab_gr(ipl)%m) / pl_mass(j)%tot(ipl)%m
        yldpst = yld_rto * (cs_pl(j)%pl_in(ipl)%pest(k) + cs_pl(j)%pl_on(ipl)%pest(k))
        cs_pl(j)%pl_in(ipl)%pest(k) = cs_pl(j)%pl_in(ipl)%pest(k) - (1. - yld_rto) *    &
                                                           cs_pl(j)%pl_in(ipl)%pest(k)
        cs_pl(j)%pl_in(ipl)%pest(k) = Max (0., cs_pl(j)%pl_in(ipl)%pest(k))
        cs_pl(j)%pl_on(ipl)%pest(k) = cs_pl(j)%pl_on(ipl)%pest(k) - (1. - yld_rto) *    &
                                                           cs_pl(j)%pl_on(ipl)%pest(k)
        cs_pl(j)%pl_on(ipl)%pest(k) = Max (0., cs_pl(j)%pl_on(ipl)%pest(k))

        clippst = (1. - harveff) * (cs_pl(j)%pl_in(ipl)%pest(k) + cs_pl(j)%pl_on(ipl)%pest(k))
        if (clippst < 0.) clippst = 0.
        !! add pesticide in clippings to soil surface
        cs_soil(j)%ly(1)%pest(k) = cs_soil(j)%ly(1)%pest(k) + clippst
      end do   
      
      !! update remaining plant organic pools
      pl_mass(j)%seed(ipl) = pl_mass(j)%seed(ipl) - harv_seed
      pl_mass(j)%leaf(ipl) = pl_mass(j)%leaf(ipl) - harv_leaf
      pl_mass(j)%stem(ipl) = pl_mass(j)%stem(ipl) - harv_stem
      pl_mass(j)%tot(ipl) = pl_mass(j)%tot(ipl) - pl_yield
      pl_mass(j)%ab_gr(ipl) = pl_mass(j)%ab_gr(ipl) - pl_yield

      !! add clippings (biomass left behind) to slow humus pool of soil
      clip = 1. - harveff
      harv_left = clip * pl_yield
      rsd1(j)%tot(1) = harv_left + rsd1(j)%tot(1)

	  !! calculation for dead roots allocations, resetting phenology, updating other pools
      !! reset leaf area index and fraction of growing season
      if (pl_mass(j)%tot(ipl)%m > 0.001) then
        !! assume the lai biomass relationship - 0.5 lai decline for biomass removal
        pcom(j)%plg(ipl)%lai = pcom(j)%plg(ipl)%lai * (1. - 0.5 * hi_tot)
        pcom(j)%plcur(ipl)%phuacc = pcom(j)%plcur(ipl)%phuacc * (1. - 0.5 * hi_tot)
        pcom(j)%plg(ipl)%root_frac = pl_mass(j)%root(ipl)%m / pl_mass(j)%tot(ipl)%m
      else
        pl_mass(j)%tot(ipl)%m = 0.
        pcom(j)%plg(ipl)%lai = 0.
        pcom(j)%plcur(ipl)%phuacc = 0.
      end if

      return
      end subroutine mgt_harvbiomass