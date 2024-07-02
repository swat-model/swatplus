      subroutine pl_pupd
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates plant phosphorus demand

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bio_p1(:)   |none           |1st shape parameter for plant P uptake
!!                                |equation
!!    bio_p2(:)   |none           |2st shape parameter for plant P uptake
!!                                |equation
!!    ihru        |none           |HRU number
!!                                |fraction of P in crop biomass at maturity
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~    
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    icrop       |none          |land cover code
!!    j           |none          |HRU number
!!    uapd        |kg P/ha       |plant demand of phosphorus
!!    up2         |kg P/ha       |optimal plant phosphorus content
!!    upmx        |kg P/ha       |maximum amount of phosphorus that can be
!!                               |removed from the soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Min
!!    SWAT: nuts

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use plant_data_module
      use hru_module, only : up2, uapd, ihru, ipl
      use plant_module
      use organic_mineral_mass_module

      implicit none

      integer :: idp
      integer :: j           !none      |hru number
      real :: matur_frac     !frac      |fraction to maturity - use hu for annuals and years to maturity for perennials

      j = ihru

      idp = pcom(j)%plcur(ipl)%idplt
      
      !! set fraction to maturity for annuals and perennials
      if (pldb(idp)%typ == "perennial") then
        matur_frac = float(pcom(j)%plcur(ipl)%curyr_mat) / float(pldb(idp)%mat_yrs)
      else  !annuals
        matur_frac = pcom(j)%plcur(ipl)%phuacc
      end if
      
      pcom(j)%plm(ipl)%p_fr = (pldb(idp)%pltpfr1 - pldb(idp)%pltpfr3) *             &  
        (1. - matur_frac / (matur_frac + Exp(plcp(idp)%pup1 - plcp(idp)%pup2 *      &
        matur_frac))) + pldb(idp)%pltpfr3

      up2(ipl) = pcom(j)%plm(ipl)%p_fr * pl_mass(j)%tot(ipl)%m
      if (up2(ipl) < pl_mass(j)%tot(ipl)%p) up2(ipl) = pl_mass(j)%tot(ipl)%p
      uapd(ipl) = up2(ipl) - pl_mass(j)%tot(ipl)%p
      uapd(ipl) = 1.5 * uapd(ipl)                     !! luxury p uptake
 
      return
      end subroutine pl_pupd