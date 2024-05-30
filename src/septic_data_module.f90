      module septic_data_module
    
      implicit none
                          
      type septic_db
          character(len=20) :: sepnm
          real :: qs = 0.               !! m3/d          |flow rate of the septic tank effluent per capita (sptq)
          real :: bodconcs = 0.         !! mg/l          |biological oxygen demand of the septic tank effluent
          real :: tssconcs = 0.         !! mg/l          |concentration of total suspended solid in the septic tank effluent
          real :: nh4concs = 0.         !! mg/l          |concentration of total phosphorus in the septic tank effluent
          real :: no3concs = 0.         !! mg/l          |concentration of nitrate in the septic tank effluent
          real :: no2concs = 0.         !! mg/l          |concentration of nitrite in the septic tank effluent
          real :: orgnconcs = 0.        !! mg/l          |concentration of organic nitrogen in the septic tank effluent
          real :: minps = 0.            !! mg/l          |concentration of mineral phosphorus in the septic tank effluent    
          real :: orgps = 0.            !! mg/l          |concentration of organic phosphorus in the septic tank effluent
          real :: fcolis = 0.           !! mg/l          |concentration of fecal coliform in the septic tank effluent
      end type septic_db
      type (septic_db), dimension (:), allocatable :: sepdb
      
     type septic_system
        character(len=13) :: name = "default"
        integer :: typ = 0      !! none            |septic system type
        integer :: yr = 0       !!                 |year the septic system became operational
        integer :: opt = 0      !!none             |Septic system operation flag (1=active,2=failing,0=not operated)
        real :: cap  = 0.       !!none             |Number of permanent residents in the house
        real :: area = 0.       !!m^2              |average area of drainfield of individual septic systems 
        integer :: tfail  = 0   !!days             |time until falling systems gets fixed
        real :: z  = 0.         !!mm               |depth to the top of the biozone layer from the ground surface
        real :: thk = 0.        !!mm               |thickness of biozone layer        
        real :: strm_dist = 0.  !!km               |distance to the stream from the septic
        real :: density = 0.    !!                 |number of septic systems per square kilometer
        real :: bd  = 0.        !!kg/m^3           |density of biomass 
        real :: bod_dc = 0.     !!m^3/day          |BOD decay rate coefficient
        real :: bod_conv        !!                 |a conversion factor representing the proportion of mass
        !!                                           bacterial growth and mass BOD degraded in the STE.
        real :: fc1 = 0.        !!none             |Linear coefficient for calculation of field capacity in the biozone
        real :: fc2 = 0.        !!none             |Exponential coefficient for calculation of field capacity in the biozone  
        real :: fecal = 0.      !!m^3/day          |fecal coliform bacteria decay rate coefficient  
        real :: plq = 0.        !!none             |conversion factor for plaque from TDS 
        real :: mrt = 0.        !!none             |mortality rate coefficient   
        real :: rsp = 0.        !!none             |respiration rate coefficient  
        real :: slg1 = 0.       !!none             |slough-off calibration parameter
        real :: slg2 = 0.       !!none             |slough-off calibration parameter
        real :: nitr = 0.       !!none             |nitrification rate coefficient
        real :: denitr = 0.     !!none             |denitrification rate coefficient
        real :: pdistrb = 0.    !!(L/kg)           |Linear P sorption distribution coefficient
        real :: psorpmax = 0.   !!(mg P/kg Soil)   |Maximum P sorption capacity 
        real :: solpslp = 0.    !!                 |Slope of the linear effluent soluble P equation
        real :: solpintc = 0.   !!                 |Intercept of the linear effluent soluble P equation
      end type septic_system
      type (septic_system), dimension (:), allocatable :: sep
           
      end module septic_data_module 