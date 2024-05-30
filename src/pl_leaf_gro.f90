      subroutine pl_leaf_gro
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine adjusts plant biomass, leaf area index, and canopy height
!!    taking into account the effect of water, temperature and nutrient stresses
!!    on the plant

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units            |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    blai(:)     |none             |maximum (potential) leaf area index
!!    bio_e(:)    |(kg/ha)/(MJ/m**2)|biomass-energy ratio
!!                                  |The potential (un stressed) growth rate per
!!                                  |unit of intercepted photosynthetically
!!                                  |active radiation.
!!    dlai(:)     |none             |fraction of growing season when leaf
!!                                  |area declines
!!    ep_day      |mm H2O           |actual amount of transpiration that occurs
!!                                  |on day in HRU
!!    es_day      |mm H2O           |actual amount of evaporation (soil et) that
!!                                  |occurs on day in HRU
!!    hvsti(:)    |(kg/ha)/(kg/ha)  |harvest index: crop yield/aboveground
!!                                  |biomass
!!    ihru        |none             |HRU number
!!    leaf1(:)    |none             |1st shape parameter for leaf area
!!                                  |development equation.
!!    leaf2(:)    |none             |2nd shape parameter for leaf area
!!                                  |development equation.
!!    pet_day     |mm H2O           |potential evapotranspiration on current day
!!                                  |in HRU
!!    t_base(:)   |deg C            |minimum temperature for plant growth
!!    vpd         |kPa              |vapor pressure deficit
!!    ruc1(:)    |none             |1st shape parameter for radiation use
!!                                  |efficiency equation.
!!    ruc2(:)    |none             |2nd shape parameter for radiation use
!!                                  |efficiency equation.
!!    wavp(:)     |none             |Rate of decline in radiation use efficiency
!!                                  |as a function of vapor pressure deficit
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bioday      |kg            |biomass generated on current day in HRU
!!    rsr1c(:)    |              |initial root to shoot ratio at beg of growing season
!!    rsr2c(:)    |              |root to shoot ratio at end of growing season
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units            |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    par         |MJ/m^2           |photosynthetically active radiation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Max, Min, Sqrt
!!    SWAT: tstr, nup, npup, anfert

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use plant_data_module
      use basin_module
      use hru_module, only : ihru, ipl
      use plant_module
      use carbon_module
      use organic_mineral_mass_module
      
      implicit none 
      
      integer :: j              !none               |HRU number
      integer :: idp            !                   |
      real :: f                 !none               |fraction of plant's maximum lai corresponding to a given fraction of phu (annual)
      real :: f_p               !none               |fraction of plant's maximum lai corresponding to a given fraction of phu (perennial)
      real :: ff                !                   |
      real :: deltalai          !                   |
      real :: laimax            !none               |maximum leaf area index
      real :: lai_exp           !                   |
      real :: rto_lin           !none               |ratio of current years of growth:years to maturity of perennial
      real :: rto               !none               |ratio of current years of growth:years to maturity of perennial
      real :: sumlaiht          !                   |
      integer :: jpl            !none               |counter
      
      j = ihru
      idp = pcom(j)%plcur(ipl)%idplt
      
          f = pcom(j)%plcur(ipl)%phuacc / (pcom(j)%plcur(ipl)%phuacc +                  &
              Exp(plcp(idp)%leaf1 - plcp(idp)%leaf2 * pcom(j)%plcur(ipl)%phuacc))
          pcom(j)%plg(ipl)%laimxfr = amin1 (f, pcom(j)%plg(ipl)%laimxfr)    !dormancy and grazing lower phuacc
          ff = f - pcom(j)%plg(ipl)%laimxfr
          pcom(j)%plg(ipl)%laimxfr = f
          
          f_p = pcom(j)%plcur(ipl)%phuacc_p / (pcom(j)%plcur(ipl)%phuacc_p +            &
              Exp(plcp(idp)%leaf1 - plcp(idp)%leaf2 * pcom(j)%plcur(ipl)%phuacc_p))
          !pcom(j)%plg(ipl)%laimxfr_p = amin1 (f_p, pcom(j)%plg(ipl)%laimxfr_p)
          !ff_p = f_p - pcom(j)%plg(ipl)%laimxfr_p
          !pcom(j)%plg(ipl)%laimxfr = f_p

          !! calculate new leaf area index when phuacc < dlai
          if (pcom(j)%plcur(ipl)%phuacc < pldb(idp)%dlai) then
            laimax = 0.
            deltalai = 0.
            if (pldb(idp)%typ == "perennial") then
              if (pcom(j)%plcur(ipl)%curyr_mat < 1) pcom(j)%plcur(ipl)%curyr_mat = 1
              rto_lin = float(pcom(j)%plcur(ipl)%curyr_mat) / float(pldb(idp)%mat_yrs)
              rto = alog10 (rto_lin)
              lai_exp = rto * pldb(idp)%laixco_tree
              laimax = pcom(j)%plcur(ipl)%lai_pot * 10. ** lai_exp
              laimax = Min (laimax, pcom(j)%plcur(ipl)%lai_pot)
            else
              laimax = pcom(j)%plcur(ipl)%lai_pot
            end if
            
            !! calculate new canopy height
            if (pldb(idp)%typ == "perennial") then
              pcom(j)%plg(ipl)%cht = pldb(idp)%chtmx * Sqrt(f_p)
            else
              pcom(j)%plg(ipl)%cht = pldb(idp)%chtmx * Sqrt(f)
            end if

            !! calculate fraction of above ground tree biomass that is leaf
            if (pldb(idp)%typ == "perennial") then
              pcom(j)%plg(ipl)%leaf_frac = 0.03     !!***needs to be a plants.plt parameter
            end if
            
            if (pcom(j)%plg(ipl)%lai > laimax) pcom(j)%plg(ipl)%lai = laimax
            !! only apply water stress to lai            
            deltalai = ff * laimax * (1.0 - Exp(5.0 * (pcom(j)%plg(ipl)%lai - laimax))) * Sqrt(pcom(j)%plstr(ipl)%strsw)
            !! adjust lai increment for plant competition
            sumlaiht = 0.
            do jpl = 1, pcom(j)%npl
              sumlaiht = sumlaiht + pcom(j)%plg(ipl)%lai * pcom(j)%plg(jpl)%cht
            end do
            if (sumlaiht > 1.e-6) then
              rto = (pcom(j)%plg(ipl)%lai * pcom(j)%plg(ipl)%cht) / sumlaiht
            else
              rto = 1.
            end if
            deltalai = deltalai * rto
            pcom(j)%plg(ipl)%lai = pcom(j)%plg(ipl)%lai + deltalai
            
            if (pcom(j)%plg(ipl)%lai > laimax) pcom(j)%plg(ipl)%lai = laimax
            pcom(j)%plg(ipl)%olai = pcom(j)%plg(ipl)%lai
            pcom(j)%plg(ipl)%dphu = pcom(j)%plcur(ipl)%phuacc
          end if    ! phu < dlai
          
      return
      end subroutine pl_leaf_gro