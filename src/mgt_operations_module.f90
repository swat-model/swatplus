      module mgt_operations_module
    
      implicit none
      
     type irrigation_operation
        character (len=40) :: name
        real :: amt_mm = 25.4           !! mm     |irrigation application amount
        real :: eff = 0.                !!        |irrigation in-field efficiency
        real :: surq = 0.               !! frac   |surface runoff ratio
        real :: dep_mm = 0.             !! mm     |depth of application for subsurface irrigation
        real :: salt = 0.               !! mg/kg  |concentration of total salt in irrigation
        real :: no3 = 0.                !! mg/kg  |concentration of nitrate in irrigation
        real :: po4 = 0.                !! mg/kg  |concentration of phosphate in irrigation
      end type irrigation_operation
      type (irrigation_operation), dimension(:), allocatable :: irrop_db

     type puddle_operation
        character (len=40) :: name
        real :: wet_hc = 0.             !! mm/h     |hydraulic conductivity of upper layer of soil after puddling
        real :: sed = 0.                !! ppm      |sediment concentration after puddling
        real :: orgn = 0.               !! ppm      |organic N concentration after puddling
        real :: sedp = 0.               !! ppm      |organic P concentration after puddling
        real :: no3 = 0.                !! ppm      |NO3-N concentration after puddling
        real :: solp = 0.               !! ppm      |mineral (soluble P) concentration after puddling
        real :: nh3 = 0.                !! ppm      |NH3 concentration after puddling
        real :: no2 = 0.                !! ppm      |NO2 concentration after puddling
      end type puddle_operation
      type (puddle_operation), dimension(:), allocatable :: pudl_db

      type filtstrip_operation
        character (len=40) :: name
        integer :: vfsi = 0             !       |on/off flag for vegetative filter strip
        real :: vfsratio = 0.           !       |contouring USLE P factor
        real :: vfscon                  !       |fraction of the total runoff from the entire field
        real :: vfsch                   !       |fraction of flow entering the most concentrated 10% of the VFS.
                                        !          which is fully channelized
      end type filtstrip_operation
      type (filtstrip_operation), dimension(:), allocatable :: filtstrip_db

      type fire_operation
        character (len=40) :: name
        real :: cn2_upd = 0.            !       |change in SCS curve number II value
        real :: fr_burn = 0.            !       |fraction burned
      end type fire_operation
      type (fire_operation),dimension(:), allocatable :: fire_db
      
      type grwaterway_operation
        character (len=40) :: name
        integer :: grwat_i = 0.     !none          |On/off Flag for waterway simulation
        real :: grwat_n = 0.        !none          |Mannings"s n for grassed waterway
        real :: grwat_spcon = 0.    !none          |sediment transport coefficant defined by user
        real :: grwat_d = 0.        !m             |depth of Grassed waterway
        real :: grwat_w = 0.        !none          |width of grass waterway
        real :: grwat_l = 0.        !km            |length of Grass Waterway
        real :: grwat_s = 0.        !m/m           |slope of grass waterway
      end type grwaterway_operation
      type (grwaterway_operation),dimension(:), allocatable :: grwaterway_db

      type bmpuser_operation  
        character (len=40) :: name
        integer :: bmp_flag = 0
        real :: bmp_sed = 0.       !%              | Sediment removal by BMP       
        real :: bmp_pp = 0.        !%              | Particulate P removal by BMP
        real :: bmp_sp = 0.        !%              | Soluble P removal by BMP
        real :: bmp_pn = 0.        !%              | Particulate N removal by BMP 
        real :: bmp_sn = 0.        !%              | Soluble N removal by BMP  
        real :: bmp_bac = 0.       !%              | Bacteria removal by BMP
      end type bmpuser_operation 
      
      type bmpuser_operation1  
        character (len=40) :: name
        integer :: bmp_flag = 0
        real :: surf_flo = 0.       !%              | Surface Flow removal by BMP  
        real :: surf_sed = 0.       !%              | Surface Sediment removal by BMP       
        real :: surf_pp = 0.        !%              | Surface Particulate P removal by BMP
        real :: surf_sp = 0.        !%              | Surface Soluble P removal by BMP
        real :: surf_pn = 0.        !%              | Surface Particulate N removal by BMP 
        real :: surf_sn = 0.        !%              | Surface Soluble N removal by BMP  
        real :: surf_bac = 0.       !%              | Surface Bacteria removal by BMP
        real :: sub_flo = 0.        !%              | Subsurface Flow removal by BMP  
        real :: sub_sed = 0.        !%              | Subsurface Sediment removal by BMP       
        real :: sub_pp = 0.         !%              | Subsurface Particulate P removal by BMP
        real :: sub_sp = 0.         !%              | Subsurface Soluble P removal by BMP
        real :: sub_pn = 0.         !%              | Subsurface Particulate N removal by BMP 
        real :: sub_sn = 0.         !%              | Subsurface Soluble N removal by BMP  
        real :: sub_bac = 0.        !%              | Subsurface Bacteria removal by BMP 
        real :: tile_flo = 0.       !%              | Tile Flow removal by BMP 
        real :: tile_sed = 0.       !%              | Tile Sediment removal by BMP       
        real :: tile_pp = 0.        !%              | Tile Particulate P removal by BMP
        real :: tile_sp = 0.        !%              | Tile Soluble P removal by BMP
        real :: tile_pn = 0.        !%              | Tile Particulate N removal by BMP 
        real :: tile_sn = 0.        !%              | Tile Soluble N removal by BMP  
        real :: tile_bac = 0.       !%              | Tile Bacteria removal by BMP 
      end type bmpuser_operation1
      type (bmpuser_operation),dimension(:), allocatable :: bmpuser_db
      
      type chemical_application_operation
        character (len=40) :: name
        character (len=40) :: form = " "        !           |solid; liquid
        character (len=40) :: op_typ = " "      !           |operation type-spread; spray; inject; direct
        real :: app_eff = 0.                    !           |application efficiency
        real :: foliar_eff = 0.                 !           |foliar efficiency
        real :: inject_dep = 0.                 !mm         |injection depth
        real :: surf_frac = 0.                  !           |surface fraction-amount in upper 10 mm
        real :: drift_pot = 0.                  !           |drift potential
        real :: aerial_unif = 0.                !           |aerial uniformity
      end type chemical_application_operation
      type (chemical_application_operation),dimension(:), allocatable :: chemapp_db

      type harvest_operation
        character (len=40) :: name
        character (len=40) :: typ   !none              |grain;biomass;residue;tree;tuber
        real :: hi_ovr = 0.         !(kg/ha)/(kg/ha)   |harvest index target specified at harvest
        real :: eff = 0.            !none              |harvest efficiency: fraction of harvested yield that is removed 
                                                       !the remainder becomes residue on the soil surface
        real :: bm_min = 0          !kg/ha             |minimum biomass to allow harvest
      end type harvest_operation
      type (harvest_operation), dimension(:), allocatable :: harvop_db
      type (harvest_operation) :: harvop
      type (harvest_operation) :: hkop
      
      type grazing_operation
        character (len=40) :: name
        character (len=40) :: fertnm = " "
        integer :: manure_id                             !fertilizer number from fertilizer.frt
        real :: eat = 0.              !!(kg/ha)/day      |dry weight of biomass removed by grazing daily
        real :: tramp = 0.            !!(kg/ha)/day      |dry weight of biomass removed by trampling daily
        real :: manure = 0.           !!(kg/ha)/day      |dry weight of manure deposited
        real :: biomin = 0.           !!kg/ha            |minimum plant biomass for grazing
      end type grazing_operation
      type (grazing_operation), dimension(:), allocatable :: grazeop_db
      type (grazing_operation) :: graze
      
      type streetsweep_operation
        character (len=40) :: name
        real :: eff = 0.               !!none             |removal efficiency of sweeping operation
        real :: fr_curb = 0.           !!none             |availability factor, the fraction of the
                                       !!                    curb length that is sweepable
      end type streetsweep_operation
      type (streetsweep_operation), dimension(:), allocatable :: sweepop_db
      type (streetsweep_operation) :: sweepop
      
      type management_ops1
        character(len=40) :: name
        character(len=40) :: op  
        !! operation code 4-digit char name
        !!  1 pcom - establish plant community  
        !!  2 plnt - plant  
        !!  3 harv - harvest only  
        !!  4 kill - Kill  
        !!  5 hvkl - Harvest and kill   
        !!  6 till - Tillage
        !!  7 irrm - Irrigation manual
        !!  8 irra - Irrigation auto
        !!  9 rel     ??  REMOVE?
        !! 10 fert - Fertilizer
        !! 11 frta - Fertilizer auto
        !! 12 frtc - Fertilizer continuous 
        !! 13 pest - Pesticide application 
        !! 14 pstc - Pesticide continuous
        !! 15 graz - Grazing  
        !! 16 burn - Burn  
        !! 17 swep - Street Sweep  
        !! 18 prtp - Print plant vars
        !! 19 mons - ?? REMOVE ??
        !! 20 skip - Skip to end of the year
        integer :: mon = 0
        integer :: day = 0
        integer :: jday = 0
        integer :: year = 0
        real :: husc = 0.
        character(len=40) :: op_char
        character (len=40) :: op_plant
        integer :: op1 = 0
        integer :: op2 = 0              !! |none          |plant number in community for hu scheduling
        real :: op3 = 0                 !! |none          |application amount (mm or kg/ha)
        integer :: op4 = 0              !! |none          |
      end type management_ops1
      
      type management_ops
        character(len=40) :: name
        character(len=40) :: op  
        !! operation code 4-digit char name
        !! plnt; autoplnt - plant
        !! harv; autoharv - harvest only
        !! kill; autokill - kill
        !! hvkl; autohk - harvest and kill
        !! till; autotill - tillage
        !! irr; autoirr - irrigation
        !! fert; autofert - fertlizer
        !! pest; pestauto - pesticide application
        !! graz; autograz - grazing
        !! burn; autoburn - burn
        !! swep; autoswep - street sweep
        !! prtp - print plant vars
        !! skip - skip to end of the year
        integer :: mon = 0
        integer :: day = 0
        integer :: jday = 0
        integer :: year = 0
        real :: husc = 0.
        character(len=40) :: op_char
        character (len=40) :: op_plant
        integer :: op1 = 0
        integer :: op2 = 0              !! |none          |plant number in community for hu scheduling
        real :: op3 = 0                 !! |none          |application amount (mm or kg/ha)
        integer :: op4 = 0              !! |none          |fert and pest type-point to fert and pest db
      end type management_ops
      type (management_ops) :: mgt
      type (management_ops) :: mgt1
      type (management_ops), dimension(1) :: mgt2
      
      type management_schedule
        character(len=40) :: name
        integer :: num_ops = 0
        integer :: num_autos = 0
        integer :: first_op = 0
        type (management_ops), dimension (:), allocatable :: mgt_ops
        character(len=40), dimension (:), allocatable :: auto_name
        character(len=40), dimension (:), allocatable :: auto_crop
        integer :: auto_crop_num
        integer, dimension (:), allocatable :: num_db
        integer :: irr
      end type management_schedule
      type (management_schedule), dimension (:), allocatable :: sched
      
      end module mgt_operations_module 