      module hru_lte_module

      real, dimension(12) :: awct = 0.
      real, dimension(12) :: port = 0.
      real, dimension(12) :: scon = 0.
        
      type swatdeg_hru_data
        character(len=16) :: name
        real :: dakm2 = 0.          !km^2          |drainage area
        real :: cn2 = 0.            !none          |condition II curve number             
        real :: cn3_swf = 0.        !none          |soil water factor for cn3 (used in calibration)
                                    !              |0 = fc; 1 = saturation (porosity)
        real :: tc = 0.             !min           |time of concentration
        real :: soildep = 0.        !mm            |soil profile depth
        real :: perco = 0.          !              |soil percolation coefficient
        real :: slope = 0.          !m/m           |land surface slope
        real :: slopelen = 0.       !m             |land surface slope length
        real :: etco = 0.           !              |et coefficient - use with pet and aet
        real :: sy = 0.             !mm            |specific yld of the shallow aquifer
        real :: abf = 0.            !              |alpha factor groundwater
        real :: revapc = 0.         !              |revap coefficient amt of et from shallow aquifer
        real :: percc = 0.          !              |percolation coeff from shallow to deep
        real :: sw = 0.             !frac          |initial soil water (frac of awc)
        real :: gw = 0.             !mm            |initial shallow aquifer storage
        real :: gwflow = 0.         !mm            |initial shallow aquifer flow
        real :: gwdeep = 0.         !mm            |initital deep aquifer flow
        real :: snow = 0.           !mm            |initial snow water equivalent
        real :: xlat = 0.           !              |latitude
        character(len=16) :: text   !              |soil texture
                                    !              |1=sand 2=loamy_sand 3=sandy_loam 4=loam
                                    !              |5=silt_loam 6=silt 7=silty_clay 8=clay_loam
                                    !              |9=sandy_clay_loam 10=sandy_clay 
                                    !              |11=silty_clay 12=clay 
        character(len=16) ::  tropical !           |(0)="non_trop" (1)="trop"
        character(len=16) :: igrow1 !              |start of growing season for non-tropical (pl_grow_sum)
                                    !              |start of monsoon initialization period for tropical 
        character(len=16) :: igrow2 !              |end of growing season for non-tropical (pl_end_sum)
                                    !              |end of monsoon initialization period for tropical
        character(len=16) :: plant  !              |plant type (as listed in plants.plt)
        real :: stress = 0.         !frac          |plant stress - pest, root restriction, soil quality, nutrient, (non water, temp)
        character(len=16) :: ipet = "harg"  !      |potential ET method (0="harg"; 1="p_t")
        character(len=16) :: irr = "no_irr" !      |irrigation code 0="no_irr";  1="irr"
        character(len=16) :: irrsrc = "outside_bsn" !irrigation source 0="outside_bsn"; 1="shal_aqu" 2="deep_aqu"
        real :: tdrain = 0.         !hr            |design subsurface tile drain time
        real :: uslek = 0.          !              |usle soil erodibility factor
        real :: uslec = 0.          !              |usle cover factor
        real :: uslep = 0.          !none          |USLE equation support practice (P) factor
        real :: uslels = 0.         !none          |USLE equation length slope (LS) factor
      end type swatdeg_hru_data
      type (swatdeg_hru_data), dimension (:), allocatable :: hlt_db
      
      type swatdeg_hru_dynamic
        character(len=16) :: name
        integer :: props
        integer :: obj_no
        character(len=16) :: lsu             !              |landscape unit - character
        character(len=16) :: region          !              |region - character
        character(len=16) :: plant           !              |plant type (as listed in plants.plt)
        !integer :: iplant = 1                !              |plant number xwalked from hlt_db()%plant and plants.plt
        integer :: iplant = 0                !              |plant number xwalked from hlt_db()%plant and plants.plt
        real :: km2 = 0.                     !km^2          |drainage area
        real :: cn2 = 0.                     !              |condition II curve number (used in calibration)
        real :: cn3_swf = 0.                 !none          |soil water factor for cn3 (used in calibration)
                                             !              |0 = fc; 1 = saturation (porosity)
        real :: soildep = 0.                 !mm            |soil profile depth
        real :: etco = 0.                    !              |et coefficient - use with pet and aet (used in calibration)
        real :: revapc = 0.                  !m/m           |revap from aquifer (used in calibration)
        real :: perco = 0.                   !              |soil percolation coefficient (used in calibration)
        real :: tdrain = 0.                  !hr            |design subsurface tile drain time (used in calibration)
        real :: stress = 0.                  !frac          |plant stress - pest, root restriction, soil quality, nutrient, 
                                             !              |(non water, temp) (used in calibration)
        real :: uslefac = 0.                 !              |USLE slope length factor
        real :: wrt1 = 0.
        real :: wrt2 = 0.
        real :: smx = 0.
        real :: hk = 0.
        real :: yls = 0.
        real :: ylc = 0.
        real :: awc = 0.                     !mm/mm        |available water capacity of soil 
        real :: g = 0.
        real :: hufh = 0.
        real :: phu = 0.     
        real :: por = 0.
        real :: sc = 0.
        real :: sw = 0.                      !mm/mm         |initial soil water storage
        real :: gw = 0.                      !mm            |initial shallow aquifer storage
        real :: snow = 0.                    !mm            |initial water content of snow
        real :: gwflow = 0.                  !mm            |initial groundwater flow
        character(len=1) :: gro = "n"        !              |y=plant growing; n=not growing;
        real :: dm = 0.                      !t/ha          |plant biomass
        real :: alai = 0.                    !              |leaf area index
        real :: yield = 0.                   !t/ha          |plant yield
        real :: npp = 0.                     !t/ha          |net primary productivity
        real :: lai_mx = 0.                  !              |maximum leaf area index
        real :: gwdeep = 0.                  !mm            |deep aquifer storage
        real :: aet = 0.                     !mm            |sum of actual et during growing season (for hi water stress)
        real :: pet = 0.                     !mm            |sum of potential et during growing season (for hi water stress)
        integer :: start = 0
        integer :: end = 0
      end type swatdeg_hru_dynamic
      type (swatdeg_hru_dynamic), dimension (:), allocatable :: hlt
      type (swatdeg_hru_dynamic), dimension (:), allocatable :: hlt_init
               
      contains

      end module hru_lte_module