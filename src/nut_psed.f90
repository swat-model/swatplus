      subroutine nut_psed

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the amount of organic and mineral phosphorus
!!    attached to sediment in surface runoff

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    enratio       |none         |enrichment ratio calculated for day in HRU
!!    erorgp(:)     |none         |organic P enrichment ratio, if left blank
!!                                |the model will calculate for every event
!!    ihru          |none         |HRU number
!!    sedyld(:)     |metric tons  |daily soil loss caused by water erosion in
!!                                |HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sedminpa(:)  |kg P/ha       |amount of active mineral phosphorus sorbed to
!!                                |sediment in surface runoff in HRU for day
!!    sedminps(:)  |kg P/ha       |amount of stable mineral phosphorus sorbed to
!!                                |sediment in surface runoff in HRU for day
!!    sedorgp(:)   |kg P/ha       |amount of organic phosphorus in surface
!!                                |runoff in HRU for the day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

        use hru_module, only : hru, sedyld, sedorgp, sedminpa, sedminps, ihru, enratio,  &
          ihru 
        use soil_module
        use plant_module
        use organic_mineral_mass_module
      
        implicit none       

        integer :: j                !none           |HRU number
        real :: sedp_attach         !kg P/ha        |amount of phosphorus attached to sediment 
                                    !               |in soil
        real :: wt1                 !kg/ha          |weight of upper soil layer
        real :: er                  !none           |enrichment ratio
        real :: frac                !none           |fraction of organic P in soil
        real :: sedp                !kg P/ha        |total amount of P removed in sediment erosion 
        real :: sed_orgp            !kg P/ha        |total amount of P in organic pools
        real :: sed_hump            !kg P/ha        |amount of P in humus pool
        real :: sed_manp            !kg P/ha        |amount of P in manure soil pool
        real :: sed_rsd_manp        !kg P/ha        |maount of P in residue manure pool
        real :: fr_orgp             !kg P/ha        |fraction of organic phosphorus in soil (humus + manure in soil + manure in residue)
        real :: fr_actmin           !kg P/ha        |fraction of active mineral phosphorus in soil
        real :: fr_stamin           !kg P/ha        |fraction of stable mineral phosphorus in soil

        j = ihru

        !! HRU sediment calculations
        sedp_attach = soil1(j)%hsta(1)%p + soil1(j)%man(1)%p + rsd1(j)%man%p + soil1(j)%mp(1)%sta + soil1(j)%mp(1)%act
        if (sedp_attach > 1.e-9) then
          fr_orgp = (soil1(j)%hsta(1)%p + soil1(j)%man(1)%p  + rsd1(j)%man%p) / sedp_attach
          fr_actmin = soil1(j)%mp(1)%sta / sedp_attach
          fr_stamin = soil1(j)%mp(1)%act / sedp_attach
        end if

        !! kg/ha = t/m3 * mm * 10,000 m2/ha * m/1000 mm * 1000 kg/t
        wt1 = 10000. * soil(j)%phys(1)%bd * soil(j)%phys(1)%d

        if (hru(j)%hyd%erorgp > .001) then
          er = hru(j)%hyd%erorgp
        else
          er = enratio
        end if
      
        frac = sedp_attach * er / wt1
        !! kg/ha = t / ha * 1000. kg/t
        sedp = 1000. * frac * sedyld(j) / hru(j)%area_ha
        
        if (sedp > 1.e-9) then
          sedorgp(j) = sedp * fr_orgp
          sedminpa(j) = sedp * fr_actmin
          sedminpa(j) = amin1 (sedminpa(j), soil1(j)%mp(1)%act)
          soil1(j)%mp(1)%act = soil1(j)%mp(1)%act - sedminpa(j)
          sedminps(j) = sedp * fr_stamin
          sedminps(j) = amin1 (sedminps(j), soil1(j)%mp(1)%sta)
          soil1(j)%mp(1)%sta = soil1(j)%mp(1)%sta - sedminps(j)
        
          sed_orgp = soil1(j)%hsta(1)%p + soil1(j)%man(1)%p  + rsd1(j)%man%p
          if (sed_orgp > 1.e-6) then
            sed_hump = sedorgp(j) * (soil1(j)%hsta(1)%p / sed_orgp)
            sed_hump = amin1 (sed_hump, soil1(j)%hsta(1)%p)
            soil1(j)%hsta(1)%p = soil1(j)%hsta(1)%p - sed_hump
        
            sed_manp = sedorgp(j) * (soil1(j)%man(1)%p / sed_orgp)
            sed_manp = amin1 (sed_manp, soil1(j)%man(1)%p)
            soil1(j)%man(1)%p = soil1(j)%man(1)%p - sed_manp
        
            sed_rsd_manp = sedorgp(j) * (rsd1(j)%man%p / sed_orgp)
            sed_rsd_manp = amin1 (sed_rsd_manp, rsd1(j)%man%p)
            rsd1(j)%man%p = rsd1(j)%man%p - sed_rsd_manp
          end if
        else
          sedorgp(j) = 0.
          sedminpa(j) = 0.
          sedminpa(j) = 0.
          sedminps(j) = 0.
          sedminps(j) = 0.
        end if 

      return
      end subroutine nut_psed