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
      
      integer :: j           !none      |hru number
      integer :: idp         !          |       
      real :: matur_frac     !frac      |fraction to maturity - use hu for annuals and years to maturity for perennials

      j = ihru

      idp = pcom(j)%plcur(ipl)%idplt
      
      !! set fraction to maturity for annuals and perennials
      if (pldb(idp)%typ == "perennial") then
        matur_frac = float(pcom(j)%plcur(ipl)%curyr_mat) / float(pldb(idp)%mat_yrs)
      else  !annuals
        matur_frac = pcom(j)%plcur(ipl)%phuacc
      end if
      
      pcom(j)%plm(ipl)%n_fr = (pldb(idp)%pltnfr1 - pldb(idp)%pltnfr3) *             &
          (1. - matur_frac / (matur_frac + Exp(plcp(idp)%nup1 - plcp(idp)%nup2 *    &
          matur_frac))) + pldb(idp)%pltnfr3

      un2(ipl) = pcom(j)%plm(ipl)%n_fr * pl_mass(j)%tot(ipl)%m
      if (un2(ipl) < pl_mass(j)%tot(ipl)%n) un2(ipl) = pl_mass(j)%tot(ipl)%n
      uno3d(ipl) = un2(ipl) - pl_mass(j)%tot(ipl)%n
      
      return 
      end subroutine pl_nupd