      module plant_data_module
    
      implicit none
      
      character(len=16), dimension (:), allocatable :: plts_bsn     !none      |plant names simulated in current run
      character(len=16), dimension (:), allocatable :: plants_bsn   !none      |plant names simulated in current run - final
     
      type plant_db
        character(len=40) :: plantnm     !none              |crop name
        character(len=18) :: typ         !none              |plant category
                                         !                  |warm_annual
                                         !                  |cold_annual
                                         !                  |warm_annual_tuber
                                         !                  |cold_annual_tuber
                                         !                  |perennial
        character(len=18) :: trig        !none              |phenology trigger
                                         !                  |moisture_gro
                                         !                  |temp_gro
        real :: nfix_co                  !none              |n fixation coefficient (0.5 legume; 0 non-legume)
        integer :: days_mat = 110        !days              |days to maturity - if zero use hu for entire growing season
        real :: bio_e = 15.0             !(kg/ha/(MJ/m**2)  |biomass-energy ratio
        real :: hvsti = 0.76             !(kg/ha)/(kg/ha)   |harvest index: crop yield/aboveground biomass
        real :: blai = 5.0               !none              |max (potential) leaf area index
        real :: frgrw1 = 0.05            !none              |fraction of the growing season corresponding to the
                                         !                  |  1st point on optimal leaf area development curve
        real :: laimx1 = 0.05            !none              |frac of max leaf area index corresponding to the 
                                         !                  |  1st point on optimal leaf area development curve
        real :: frgrw2 = 0.4             !none              |fraction of the growing season corresponding to the 
                                         !                  |  2nd point on optimal leaf area development curve 
        real :: laimx2 = 0.95            !none              |fraction of max leaf area index corresponding to the
                                         !                  |  2nd point on optimal leaf area development curve
        real :: dlai = 0.99              !none              |frac of growing season when leaf are declines
        real :: dlai_rate = 1.           !none              |exponent that governs lai decline rate
        real :: chtmx = 6.0              !m                 |maximum canopy height
        real :: rdmx = 3.5               !m                 |maximum root depth
        real :: t_opt = 30.              !deg C             |optimal temp for plant growth
        real :: t_base = 10.             !deg C             |minimum temp for plant growth
        real :: cnyld = 0.0015           !kg N/kg yld       |frac of nitrogen in yield
        real :: cpyld = 0.0003           !kg P/kg yld       |frac of phosphorus in yield
        real :: pltnfr1 = 0.006          !kg N/kg biomass   |nitrogen uptake parm #1
        real :: pltnfr2 = 0.002          !kg N/kg biomass   |nitrogen uptake parm #2 
        real :: pltnfr3 = 0.0015         !kg N/kg/biomass   |nitrogen uptake parm #3
        real :: pltpfr1 = 0.0007         !kg P/kg/biomass   |phoshorus uptake parm #1
        real :: pltpfr2 = 0.0004         !kg P/kg/biomass   |phoshorus uptake parm #2
        real :: pltpfr3 = 0.0003         !kg P/kg/biomass   |phoshorus uptake parm #3
        real :: wsyf = 0.01              !(kg/ha)/(kg/ha)   |value of harvest index bet 0 and HVSTI
        real :: usle_c = 0.001           !none              |minimum value of the USLE C factor for water erosion
        real :: gsi = 0.002              !m/s               |maximum stomatal conductance
        real :: vpdfr = 4.               !kPa               |vapor pressure deficit at which GMAXFR is valid
        real :: gmaxfr = 0.75            !none              |fraction of max stomatal conductance that is 
                                         !                    achieved at the vapor pressue deficit defined by VPDFR
        real :: wavp = 8.                !none              |rate of decline in radiation use efficiency
        real :: co2hi = 660.             !uL CO2/L air      |CO2 concentration higher than the ambient corresponding
                                         !                    to the 2nd point on radiation use efficiency curve             
        real :: bioehi = 16.             !(kg/ha)/(MJ/m**2) |biomass-energy ratio when plant is in an environment with 
                                         !                    CO2 level equal to the value of CO2HI.
        real :: rsdco_pl = 0.05          !none              |plant residue decomposition coeff
        real :: alai_min = 0.75          !m**2/m**2         |min LAI during winter dormant period
        real :: laixco_tree = 0.3        !none              |coefficient to estimate max lai during tree growth
        integer :: mat_yrs = 10          !years             |years to maturity  
        real :: bmx_peren = 1000.        !metric tons/ha    |max biomass for forest
        real :: ext_coef = 0.65          !                  |light extinction coefficient
        real :: leaf_tov_min = 12.       !months            |perennial leaf turnover rate with minimum stress (complete turnover in 12 mon)
        real :: leaf_tov_max = 3.        !months            |perennial leaf turnover rate with maximum stress (complete turnover in 3 mon)
        real :: bm_dieoff = 0.           !frac              |above ground biomass that dies off at dormancy
        !real :: leaf_frac_mx             !frac              |max fraction of above ground biomass that is leaf (assume constant over life of perennial)
        real :: rsr1 = 0.                !frac              |initial root to shoot ratio at the beg of growing season
        real :: rsr2 = 0.                !frac              |root to shoot ratio at the end of the growing season
        real :: pop1 = 0.                !plants/m^2        |plant population corresponding to the 1st point on the
                                         !                             population lai curve
        real :: frlai1 = 0.              !frac              |frac of max leaf area index corresponding to the 1st 
                                         !                     point on the leaf area development curve
        real :: pop2 = 0.                !plants/m^2        |plant population corresponding to the 2nd point on the
                                         !                             population lai curve
        real :: frlai2 = 0.              !frac              |frac of max leaf area index corresponding to the 2nd 
                                         !                     point on the leaf area development curve
        real :: frsw_gro = .5            !frac              |30 day sum of P-PET to initiate growth of tropical 
                                         !                     plants during monsoon season - pcom()%plcur()%iseason
        real :: aeration = 0.2           !                  |aeration stress factor
        real :: rsd_pctcov = 0.          !                  |residue factor for percent cover equation
        real :: rsd_covfac = 0.          !                  |residue factor for surface cover (C factor) equation
        !character(len=45) :: desc = "unknown"
      end type plant_db
      type (plant_db), dimension(:),allocatable, target, save :: pldb
      type (plant_db), pointer :: pl_db
         
      type plant_cp
        real :: popsc1 = 0.
        real :: popsc2 = 0.
        real :: leaf1 = 0.       !none      |1st shape parameter for leaf area
        real :: leaf2 = 0.       !none      |2nd leaf parameter for leaf area
        real :: ruc1 = 0.        !none      |1st shape parameter for radiation use efficiency equation
        real :: ruc2 = 0.        !none      |2nd shape parameter for radiation use efficiency equation
        real :: nup1 = 0.        !none      |1st shape parameter for plant N uptake equation      
        real :: nup2 = 0.        !none      |2nd shape parameter for plant N uptake equation
        real :: pup1 = 0.        !none      |1st shape parameter for plant P uptake equation
        real :: pup2 = 0.        !none      |2nd shape parameter for plant P uptake equation
        real :: gmaxfr = 0.      !none      |fraction of max stomatal conductance that is 
                                 !            achieved at the vapor pressue deficit defined by VPDFR
        real :: vpdfr = 0.       !kPa       |vapor pressure deficit at which GMAXFR is valid
        real :: cvm = 0.         !frac      |fraction of the maximum leaf area index corresponding
                                 !            to the second point of the optimal leaf area dev curve
        real :: vpd2 = 0.        !kPa       |vapor pressure deficit corresponding to the second point
                                 !            on the stomatal conductance curve
      end type plant_cp
      type (plant_cp), dimension(:),allocatable, target, save :: plcp
      type (plant_cp), pointer :: pl_cp
            
      type plant_init_db
        character(len=40) :: cpnm = "frsd"
        integer :: db_num = 1               !           |plant object
        character(len=1) :: igro = "y"      !           |land cover status
                                            !           |n = no land cover growing
                                            !           |y = land cover growing
        real :: lai = 0.                    !m**2/m**2  |leaf area index
        real :: bioms = 0.                  !kg/ha      |land cover/crop biomass
        real :: phuacc = 0.                 !           |frac of plant heat unit acc.
        real :: pop = 0.
        real :: fr_yrmat = 1.               !years      |fraction of current year of growth to years to maturity 
        real :: rsdin = 10000.              !kg/ha      |initial residue cover
      end type plant_init_db
      
      type plant_community_db   
        character(len=40) :: name = "frsd_frsd"
        integer :: plants_com = 1
        integer :: rot_yr_ini = 1
        type (plant_init_db), dimension(:), allocatable :: pl
      end type plant_community_db
      type (plant_community_db), dimension(:), allocatable :: pcomdb
                          
      type plant_transplant_db
        character(len=40) :: name = "frsd"
        real :: lai = 0.                    !m**2/m**2      |leaf area index
        real :: bioms = 0.                  !kg/ha          |land cover/crop biomass
        real :: phuacc = 0.                 !frac           |frac of plant heat unit acc.
        real :: fr_yrmat = 0.05             !years          |fraction of current year of growth to years to maturity 
        real :: pop                         !plants/m^2     |plant population 
      end type plant_transplant_db
      type (plant_transplant_db), dimension(:), allocatable :: transpl
    
      end module plant_data_module 