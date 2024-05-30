      subroutine cs_sed !rtb cs
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the amount of selenium attached to sediment in surface runoff

      use hru_module, only : hru, sedorgn, sedyld, ihru, enratio, sedmcs
      use basin_module
      use constituent_mass_module
      use soil_module

      implicit none
      integer :: j
      real :: xx,xxseo4,xxseo3,wt1,conc,sedse

      !hru number
      j = ihru

      !total sorbed selenium in the first layer of soil (top 10 mm: thus, available for removal by erosion)
      xx = 0.
      xxseo4 = 0.
      xxseo3 = 0.
      xx = cs_soil(j)%ly(1)%cs_sorb(1) + cs_soil(j)%ly(1)%cs_sorb(2) !only from first layer
      if (xx > 1.e-6) then
        xxseo4 = cs_soil(j)%ly(1)%cs_sorb(1) / xx !fraction that is seo4
        xxseo3 = cs_soil(j)%ly(1)%cs_sorb(2) / xx !fraction that is seo3
      endif

      !calculate conversion factor
      wt1 = 0.
      wt1 = soil(j)%phys(1)%bd * soil(j)%phys(1)%thick / 100.

      !concentration of selenium attached to sediment in the top 10 mm
      !use same enrichment ratio as used for N and P
      conc = 0.
      conc = xx * enratio / wt1

      !amount of selenium transported with sediment to the main channel in surface runoff
      sedse = 0.
      sedse = .001 * conc * sedyld(j) / hru(j)%area_ha
      sedmcs(j,1) = sedse * xxseo4
      sedmcs(j,2) = sedse * xxseo3

      !perform check (can't have se sorbed concentration go negative)
      if(sedmcs(j,1) > cs_soil(j)%ly(1)%cs_sorb(1)) then
        sedmcs(j,1) = cs_soil(j)%ly(1)%cs_sorb(1)
        cs_soil(j)%ly(1)%cs_sorb(1) = 0.
      endif
      if(sedmcs(j,2) > cs_soil(j)%ly(1)%cs_sorb(2)) then
        sedmcs(j,2) = cs_soil(j)%ly(1)%cs_sorb(2)
        cs_soil(j)%ly(1)%cs_sorb(2) = 0.
      endif
      
      !modify selenium pools (remove selenium that has been taken by erosion)
      cs_soil(j)%ly(1)%cs_sorb(1) = cs_soil(j)%ly(1)%cs_sorb(1) - sedmcs(j,1)
      cs_soil(j)%ly(1)%cs_sorb(2) = cs_soil(j)%ly(1)%cs_sorb(2) - sedmcs(j,2)

      
      return
      end !cs_sed