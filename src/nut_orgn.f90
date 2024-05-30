      subroutine nut_orgn

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
      use hru_module, only : hru, sedorgn, sedyld, ihru, enratio
      use soil_module
      use plant_module
      
      implicit none

      integer :: j             !none          |HRU number
      real :: orgn_kgha        !kg N/ha       |amount of organic N in first soil layer
      real :: wt1              !kg/ha         |weight of upper soil layer
      real :: er               !none          |enrichment ratio           
      real :: frac             !              |fraction of organic N in soil

      j = ihru

      !! HRU calculations
      orgn_kgha = soil1(j)%hsta(1)%n + soil1(j)%hact(1)%n
      !! kg/ha = t/m3 * mm * 10,000 m2/ha * m/1000 mm * 1000 kg/t
      wt1 = 10000. * soil(j)%phys(1)%bd * soil(j)%phys(1)%d

      if (hru(j)%hyd%erorgn > .001) then
        er = hru(j)%hyd%erorgn
      else
        er = enratio
      end if

      frac = orgn_kgha * er / wt1

      !! HRU calculations
      !! kg/ha = t / ha * 1000. kg/t
      sedorgn(j) = 1000. * frac * sedyld(j) / hru(j)%area_ha

	  !! update soil nitrogen pools only for HRU calculations
      if (orgn_kgha > 1.e-6) then
       soil1(j)%hact(1)%n = soil1(j)%hact(1)%n - sedorgn(j) * (soil1(j)%hact(1)%n / orgn_kgha)
       soil1(j)%hsta(1)%n = soil1(j)%hsta(1)%n - sedorgn(j) * (soil1(j)%hsta(1)%n / orgn_kgha)

       if (soil1(j)%hact(1)%n < 0.) then
         sedorgn(j) = sedorgn(j) + soil1(j)%hact(1)%n
         soil1(j)%hact(1)%n = 0.
       end if

       if (soil1(j)%hsta(1)%n < 0.) then
         sedorgn(j) = sedorgn(j) + soil1(j)%hsta(1)%n
         soil1(j)%hsta(1)%n = 0.
       end if

      end if

      return
      end subroutine nut_orgn