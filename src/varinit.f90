      subroutine varinit
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine initializes variables for the daily simulation of the 
!!    land phase of the hydrologic cycle (the subbasin command loop)

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    albday      |none          |albedo for the day in HRU
!!    bioday      |kg            |biomass generated on current day in HRU
!!    bsprev      |mm H2O        |surface runoff lagged from prior day of
!!                               |simulation
!!    canev       |mm H2O        |amount of water evaporated from canopy storage
!!    ep_day      |mm H2O        |actual amount of transpiration that occurs on
!!                               |day in HRU
!!    ep_max      |mm H2O        |maximum amount of transpiration (plant et)
!!                               |that can occur on day in HRU
!!    es_day      |mm H2O        |actual amount of evaporation (from soil) that
!!                               |occurs on day in HRU
!!    hhqday(:)   |mm H2O        |surface runoff from HRU for every hour in day
!!    inflpcp     |mm H2O        |amount of precipitation that infiltrates into
!!                               |soil (enters soil)
!!    lat_pst(:)  |kg pst/ha     |amount of pesticide in lateral flow in HRU for
!!                               |the day
!!    qp_cms      |m^3/s         |peak runoff rate for the day in HRU
!!    pet_day     |mm H2O        |potential evapotranspiration for day in HRU
!!    qtile       |mm H2O        |drainage tile flow for day in HRU
!!    sepday      |mm H2O        |percolation from bottom of the soil layer on
!!                               |day in HRU
!!    snoev       |mm H2O        |amount of water in snow lost through 
!!                               |sublimation on current day in HRU
!!    snofall     |mm H2O        |amount of precipitation falling as freezing 
!!                               |rain/snow on day in HRU
!!    snomlt      |mm H2O        |amount of water in snow melt for the day in 
!!                               |HRU
!!    sw_excess   |mm H2O        |amount of water in soil that exceeds field 
!!                               |capacity (gravity drained water)
!!    uno3d       |kg N/ha       |plant nitrogen deficiency for day in HRU
!!    usle_ei     |none          |USLE erodibility index on day for HRU
!!    vpd         |kPa           |vapor pressure deficit
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 


!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use time_module
      use hru_module, only : hhqday, ihru, albday,                                      &
        bioday, bsprev, canev, ep_day, ep_max, es_day, fertn, fertp, grazn, grazp,      &
        hhsedy, inflpcp, latqrunon, ls_overq, lyrtile, qp_cms,                          &
        pet_day, qday, qtile, sepday, snoev, snofall, snomlt,                           &
        sw_excess, ubnrunoff, ubntss, uno3d, usle, usle_ei, voltot, vpd, fixn 
      use soil_module
      use hydrograph_module
      
      implicit none

      integer :: j              !none          |HRU number
      integer :: ly             !none          |counter
      real :: crk               !mm H2O        |percolation due to crack flow
      real :: enratio           !none          |enrichment ratio calculated for day in HRU
      real :: etday             !mm H2O        |actual amount of evapotranspiration that 
                                !              |occurs on day in HRU
      real :: over_flow         !              |
      real :: sedprev           !              | 
      integer :: irmmdt         !              | 

      j = ihru

      !! initialize hru variables - modular code
      do ly = 1, soil(j)%nly
        soil(j)%ly(ly)%prk = 0.
        soil(j)%ly(ly)%flat = 0.
      end do
      
      !!initialize variables NUBS - all these need to be checked
        albday = 0.
        bioday = 0.
        bsprev = 0.
        canev = 0.
        crk = 0.
        enratio = 0.
        ep_day = 0.
        ep_max = 0.
        es_day = 0.
        etday = 0.
        fertn = 0.
        fertp = 0.
        fixn = 0.
        grazn = 0.
        grazp = 0.
        if (time%step > 1)  hhqday(j,:) = 0.
        inflpcp = 0.
        lyrtile = 0.
        qp_cms = 0.
        pet_day = 0.
        qday = 0.
        qtile = 0.
        ls_overq = 0.
        over_flow = 0.
        latqrunon = 0.
        sepday = 0.
        snoev = 0.
        snofall = 0.
        snomlt = 0.
        sw_excess = 0.
        uno3d = 0.
        usle = 0.
        usle_ei = 0.
        vpd = 0.
        voltot = 0.

	!! urban modeling by J.Jeong
	  sedprev = 0.
	  ubnrunoff = 0.
	  irmmdt = 0.
        hhsedy = 0.
        ubntss = 0.
        wet_seep_day(j)%no3 = 0
        wet_seep_day(j)%nh3 = 0
        wet_seep_day(j)%orgn =0
        wet_seep_day(j)%solp =0
        wet_seep_day(j)%sedp =0

       return
       end subroutine varinit