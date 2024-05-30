      subroutine pl_pup

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates plant phosphorus uptake

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    pup1(:)   |none           |1st shape parameter for plant P uptake
!!                                |equation
!!    pup2(:)   |none           |2st shape parameter for plant P uptake
!!                                |equation
!!    ihru        |none           |HRU number
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~     
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    uapd        |kg P/ha       |plant demand of phosphorus
!!    up2         |kg P/ha       |optimal plant phosphorus content
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Min
!!    SWAT: nuts

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module
      use organic_mineral_mass_module
      use hru_module, only : uapd, up2, pplnt, ihru, ipl, rto_solp, uptake
      use soil_module
      use plant_module
      use output_landscape_module

      implicit none

      integer :: j           !none      |hru number
      integer :: l           !none      |counter (soil layer)
      real :: root_depth     !mm        |root depth
      real :: soil_depth     !mm        |soil layer depth
      real :: uapl           !kg P/ha   |amount of phosphorus removed from layer
      real :: upmx           !kg P/ha   |maximum amount of phosphorus that can be
                             !          |removed from the soil layer
     
      j = ihru

      pcom(j)%plstr(ipl)%strsp = 1.
      hnb_d(j)%puptake = 0.
      if (uapd(ipl) < 1.e-6) return

      !! find depth of soil layer the roots are into
      root_depth = max (10.1, pcom(j)%plg(ipl)%root_dep)
      soil_depth = 0.
      do l = 1, soil(j)%nly
        soil_depth = soil(j)%phys(l)%d
        if (root_depth < soil_depth) then
          root_depth = soil(j)%phys(l)%d
          exit
        end if
      end do
        
      do l = 1, soil(j)%nly
        soil_depth = soil(j)%phys(l)%d
        if (root_depth < soil_depth) exit
        upmx = uapd(ipl) * rto_solp * (1. - Exp(-bsn_prm%p_updis * soil_depth / root_depth)) / uptake%p_norm
        uapl = Min(upmx - pplnt(j), soil1(j)%mp(l)%lab)
        pplnt(j) = pplnt(j) + uapl
        soil1(j)%mp(l)%lab = soil1(j)%mp(l)%lab - uapl
      end do
      if (pplnt(j) < 0.) pplnt(j) = 0.

      pl_mass(j)%tot(ipl)%p = pl_mass(j)%tot(ipl)%p + pplnt(j)
      pl_mass(j)%ab_gr(ipl)%p = pl_mass(j)%ab_gr(ipl)%p + pplnt(j) * (1. - pcom(j)%plg(ipl)%root_frac)
      pl_mass(j)%root(ipl)%p = pl_mass(j)%root(ipl)%p + pplnt(j) * pcom(j)%plg(ipl)%root_frac
      pl_mass_up%p = pplnt(j)
      hnb_d(j)%puptake = hnb_d(j)%puptake + pplnt(j)

      !! compute phosphorus stress
      !call nuts(pl_mass(j)%tot(ipl)%p, up2(ipl), pcom(j)%plstr(ipl)%strsp)
      
      !***jga
      call nuts(pl_mass(j)%ab_gr(ipl)%p, up2(ipl), pcom(j)%plstr(ipl)%strsp)

      return
      end subroutine pl_pup