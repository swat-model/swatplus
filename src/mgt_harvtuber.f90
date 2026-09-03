      subroutine mgt_harvtuber (jj, iplant, iharvop)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs the tuber harvest operation

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
      use pesticide_data_module, only : pestdb
      use constituent_mass_module
      use output_ls_pesticide_module
      
      implicit none
      
      external :: pl_rootfr
 
      integer :: j = 0                  !none           |HRU number
      integer :: k = 0                  !none           |pesticide number
      integer :: ly = 0                 !none           |soil layer number
      integer, intent (in) :: jj        !none           |hru number
      integer, intent (in) :: iplant    !               |plant number from plant community
      integer, intent (in) :: iharvop   !               |harvest operation type
      real :: harveff = 0.              !0-1            |harvest efficiency
      integer :: idp = 0                !none           |plant number from plants.plt
      real :: yld_rto = 0.              !0-1            |yield to total biomass ratio
      real :: yldpst = 0.               !kg pst/ha          |pesticide removed in yield
      j = jj
      ipl = iplant
      idp = pcom(j)%plcur(ipl)%idplt
      harveff = harvop_db(iharvop)%eff
      
      !! multiply root biomass by harvest efficiency
      pl_yield = harveff * pl_mass(j)%root(ipl)
      
      !! remove harvested root mass from existing root mass
      pl_mass(j)%root(ipl) = pl_mass(j)%root(ipl) - pl_yield
      
      !! update root fractions in each layer
      call pl_rootfr(j)
      
      !! allocate remaining dead roots, N, P to soil layers
      do ly = 1, soil(j)%nly
        soil1(j)%pl(ipl)%rsd(ly) = soil1(j)%pl(ipl)%rsd(ly) + pcom(j)%plg(ipl)%rtfr(ly) *  &
                                                                      pl_mass(j)%root(ipl)
      end do
      
      !! apply pest stress to harvest index - mass lost due to pests - don't add to residue
      pl_yield = (1. - pcom(j)%plcur(ipl)%pest_stress) * pl_yield
      
      !! adjust foliar and internal pesticide for tuber removal
      do k = 1, cs_db%num_pests
        if (pl_mass(j)%tot(ipl)%m > 1.e-6) then
          yld_rto = pl_yield%m / pl_mass(j)%tot(ipl)%m
          yld_rto = Min(yld_rto, 1.)
        else
          yld_rto = 0.
        end if
        yldpst = yld_rto * (cs_pl(j)%pl_in(ipl)%pest(k) + cs_pl(j)%pl_on(ipl)%pest(k))
        hpestb_d(j)%pest(k)%harv_export = hpestb_d(j)%pest(k)%harv_export + yldpst
        !! BUG FIX: original code kept yld_rto fraction instead of (1-yld_rto)
        cs_pl(j)%pl_in(ipl)%pest(k) = (1. - yld_rto) * cs_pl(j)%pl_in(ipl)%pest(k)
        cs_pl(j)%pl_in(ipl)%pest(k) = Max (0., cs_pl(j)%pl_in(ipl)%pest(k))
        cs_pl(j)%pl_on(ipl)%pest(k) = (1. - yld_rto) * cs_pl(j)%pl_on(ipl)%pest(k)
        cs_pl(j)%pl_on(ipl)%pest(k) = Max (0., cs_pl(j)%pl_on(ipl)%pest(k))
      end do   

      !! tuber harvest is terminal for the current plant (plant mass is zeroed below)
      !! route remaining pesticide before zeroing pools:
      !! - pl_on always returns to soil surface
      !! - pl_in returns to soil only for non-sink pesticides (tracked as kill_ret)
      !! - for sink option, pl_in is terminally removed and tracked as harvest export
      do k = 1, cs_db%num_pests
        cs_soil(j)%ly(1)%pest(k) = cs_soil(j)%ly(1)%pest(k) + cs_pl(j)%pl_on(ipl)%pest(k)
        cs_pl(j)%pl_on(ipl)%pest(k) = 0.
        if (pestdb(cs_db%pest_num(k))%harvest_sink < 0.5) then
          hpestb_d(j)%pest(k)%kill_ret = hpestb_d(j)%pest(k)%kill_ret + cs_pl(j)%pl_in(ipl)%pest(k)
          cs_soil(j)%ly(1)%pest(k) = cs_soil(j)%ly(1)%pest(k) + cs_pl(j)%pl_in(ipl)%pest(k)
        else
          hpestb_d(j)%pest(k)%harv_export = hpestb_d(j)%pest(k)%harv_export + cs_pl(j)%pl_in(ipl)%pest(k)
        end if
        cs_pl(j)%pl_in(ipl)%pest(k) = 0.
      end do

      !! add above ground mass to residue pool
      pl_mass(j)%rsd_tot = pl_mass(j)%rsd_tot + pl_mass(j)%ab_gr(ipl)
      pl_mass(j)%rsd(ipl) = pl_mass(j)%rsd(ipl) + pl_mass(j)%ab_gr(ipl)

      !! zero all plant components - assume tuber harvest kills plant
      pl_mass(j)%tot(ipl) = plt_mass_z
      pl_mass(j)%ab_gr(ipl) = plt_mass_z
      pl_mass(j)%leaf(ipl) = plt_mass_z
      pl_mass(j)%stem(ipl) = plt_mass_z
      pl_mass(j)%seed(ipl) = plt_mass_z
      pl_mass(j)%root(ipl) = plt_mass_z

      return
      end subroutine mgt_harvtuber