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
!!    gx          |mm            |lowest depth in layer from which phosphorus
!!                               |may be removed
!!    icrop       |none          |land cover code
!!    ir          |none          |flag for bottom of root zone
!!    j           |none          |HRU number
!!    l           |none          |counter (soil layers)
!!    uapd        |kg P/ha       |plant demand of phosphorus
!!    uapl        |kg P/ha       |amount of phosphorus removed from layer
!!    up2         |kg P/ha       |optimal plant phosphorus content
!!    upmx        |kg P/ha       |maximum amount of phosphorus that can be
!!                               |removed from the soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Min
!!    SWAT: nuts

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use plant_data_module
      use hru_module, only : up2, uapd, ihru, ipl, uptake
      use plant_module
      use organic_mineral_mass_module

      implicit none

      integer :: idp
      integer :: icrop       !none      |land cover code
      integer :: j           !none      |hru number
      integer :: l           !none      |counter (soil layer)
      integer :: ir          !none      |flag to denote bottom of root zone reached
      real :: uapl           !kg P/ha   |amount of phosphorus removed from layer
      real :: gx             !mm        |lowest depth in layer from which nitrogen
                             !          |may be removed

      j = ihru

      idp = pcom(j)%plcur(ipl)%idplt
      
      pcom(j)%plm(ipl)%p_fr = (pldb(idp)%pltpfr1 - pldb(idp)%pltpfr3) * &  
        (1. - pcom(j)%plcur(ipl)%phuacc / (pcom(j)%plcur(ipl)%phuacc +  &       
        Exp(plcp(idp)%pup1 - plcp(idp)%pup2 *                           &
        pcom(j)%plcur(ipl)%phuacc))) + pldb(idp)%pltpfr3

      up2(ipl) = pcom(j)%plm(ipl)%p_fr * pl_mass(j)%tot(ipl)%m
      if (up2(ipl) < pl_mass(j)%tot(ipl)%p) up2(ipl) = pl_mass(j)%tot(ipl)%p
      uapd(ipl) = up2(ipl) - pl_mass(j)%tot(ipl)%p
      uapd(ipl) = 1.5 * uapd(ipl)                     !! luxury p uptake
 
      return
      end subroutine pl_pupd