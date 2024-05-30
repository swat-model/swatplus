      subroutine nut_orgnc

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the amount of organic nitrogen removed in
!!    surface runoff 

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    enratio       |none         |enrichment ratio calculated for day in HRU
!!    erorgn(:)     |none         |organic N enrichment ratio, if left blank
!!                                |the model will calculate for every event
!!    ihru          |none         |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use organic_mineral_mass_module
      use hru_module, only : hru, ihru, sedorgn, sedyld, enratio 
      use soil_module
      
      implicit none

      integer :: j        !none          |HRU number
      real :: xx          !kg N/ha       |amount of organic N in first soil layer
      real :: wt1         !none          |conversion factor (mg/kg => kg/ha)
      real :: er          !none          |enrichment ratio
      real :: conc        !              |concentration of organic N in soil
      real :: xx1         !              |
 
      j = ihru

      !! HRU calculations
      xx = soil1(j)%tot(1)%n + rsd1(j)%tot(1)%n + rsd1(j)%man%n
      wt1 = soil(j)%phys(1)%bd * soil(j)%phys(1)%d / 100.

      if (hru(j)%hyd%erorgn > .001) then
        er = hru(j)%hyd%erorgn
      else
        er = enratio
      end if

      conc = xx * er / wt1
      sedorgn(j) = .001 * conc * sedyld(j) / hru(j)%area_ha

	  !! update soil nitrogen pools only for HRU calculations
      if (xx > 1.e-6) then
        xx1 = (1. - sedorgn(j) / xx)
		soil1(j)%tot(1)%n = soil1(j)%tot(1)%n * xx1
		rsd1(j)%tot(1)%n = rsd1(j)%tot(1)%n * xx1
		rsd1(j)%man%n = rsd1(j)%man%n * xx1
      end if

      return
      end subroutine nut_orgnc