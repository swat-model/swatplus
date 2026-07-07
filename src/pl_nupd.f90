      subroutine pl_nupd
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine calculates plant nitrogen demand

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bio_n1(:)   |none           |1st shape parameter for plant N uptake equation
!!    bio_n2(:)   |none           |2nd shape parameter for plant N uptake equation
!!    ihru        |none           |HRU number
!!    pltnfr(1,:) |kg N/kg biomass|nitrogen uptake parameter #1: normal fraction
!!                                |of N in crop biomass at emergence
!!    pltnfr(3,:) |kg N/kg biomass|nitrogen uptake parameter #3: normal fraction
!!                                |of N in crop biomass at maturity
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    uno3d       |kg N/ha       |plant nitrogen deficiency for day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use plant_data_module
      use hru_module, only : un2, uno3d, ihru, ipl
      use plant_module
      use organic_mineral_mass_module
      
      implicit none      
      
      integer :: icrop       !none      |land cover code
      integer :: j           !none      |hru number
      integer :: l           !none      |counter (soil layer)
      real :: uno3l          !kg N/ha   |plant nitrogen demand
      integer :: ir          !none      |flag to denote bottom of root zone reached
      integer :: idp         !          |       
      real :: gx             !mm        |lowest depth in layer from which nitrogen may be removed

      j = ihru

      idp = pcom(j)%plcur(ipl)%idplt
      
      pcom(j)%plm(ipl)%n_fr = (pldb(idp)%pltnfr1 - pldb(idp)%pltnfr3) *  &
          (1. -pcom(j)%plcur(ipl)%phuacc / (pcom(j)%plcur(ipl)%phuacc +  &      
          Exp(plcp(idp)%nup1 - plcp(idp)%nup2 *                          &
          pcom(j)%plcur(ipl)%phuacc))) + pldb(idp)%pltnfr3

      un2(ipl) = pcom(j)%plm(ipl)%n_fr * pl_mass(j)%tot(ipl)%m
      if (un2(ipl) < pl_mass(j)%tot(ipl)%n) un2(ipl) = pl_mass(j)%tot(ipl)%n
      uno3d(ipl) = un2(ipl) - pl_mass(j)%tot(ipl)%n
      
      return 
      end subroutine pl_nupd