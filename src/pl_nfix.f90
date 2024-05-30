      subroutine pl_nfix
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine estimates nitrogen fixation by legumes

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    uno3d       |kg N/ha       |plant nitrogen deficiency for day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max, Min

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module
      use organic_mineral_mass_module
      use hru_module, only : uno3d, nplnt, ihru, fixn, ipl 
      use soil_module
      use plant_module
      use plant_data_module
      
      implicit none

      integer :: j           !none      |hru number
      integer :: l           !none      |counter (soil layer)
      integer :: idp         !none      |plant number from plants.plt
      real :: uno3l          !kg N/ha   |plant nitrogen demand
      real :: fxw            !          | 
      real :: sumn           !kg N/ha   |total amount of nitrate stored in soil profile
      real :: fxn            !          |
      real :: fxg            !          |
      real :: fxr            !          |

      j = ihru
      idp = pcom(j)%plcur(ipl)%idplt
 
!! compute the difference between supply and demand
      if (uno3d(ipl) > nplnt(j)) then
        uno3l = uno3d(ipl) - nplnt(j)
      else
        !! if supply is being met, fixation=0 and return
        fixn = 0.
        return
      endif

!! compute fixation as a function of no3, soil water, and growth stage

      !! compute soil water factor
      fxw = soil(j)%sw / (.85 * soil(j)%sumfc)

      !! compute no3 factor
      sumn = 0.
      fxn = 0.
      do l = 1, soil(j)%nly
        sumn = sumn + soil1(j)%mn(l)%no3
      end do
      if (sumn > 300.) fxn = 0.
      if (sumn > 100. .and. sumn <= 300.) fxn = 1.5 - .005 * sumn
      if (sumn <= 100.) fxn = 1.

      !! compute growth stage factor
      fxg = 0.
      if (pcom(j)%plcur(ipl)%phuacc > .15 .and. pcom(j)%plcur(ipl)%phuacc <= .30) then
          fxg = 6.67 * pcom(j)%plcur(ipl)%phuacc - 1.
      endif
      if(pcom(j)%plcur(ipl)%phuacc > .30 .and. pcom(j)%plcur(ipl)%phuacc <= .55) fxg = 1.
      if (pcom(j)%plcur(ipl)%phuacc > .55 .and. pcom(j)%plcur(ipl)%phuacc <= .75) then
         fxg = 3.75 - 5. * pcom(j)%plcur(ipl)%phuacc
      endif

      fxr = Min(1., fxw, fxn) * fxg
      fxr = Max(0., fxr)

      fixn = Min(6., fxr * uno3l)
      fixn = pldb(idp)%nfix_co * fixn + (1. - pldb(idp)%nfix_co) * uno3l
      fixn = Min(fixn, uno3l)
      fixn = Min(bsn_prm%nfixmx, fixn)

      return
      end subroutine pl_nfix