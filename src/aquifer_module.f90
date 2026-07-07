      module aquifer_module
    
      implicit none

      type aquifer_database
        character(len=16) :: aqunm = ""         !aquifer name
        character(len=16) :: aqu_ini = ""       !initial aquifer data- points to name in initial.aqu
        real :: flo = 0.05          !mm         |flow from aquifer in current time step 
        real :: dep_bot = 0.        !m          |depth - mid-slope surface to bottom of aquifer 
        real :: dep_wt = 0.         !m          |depth - mid-slope surface to water table (initial)
        real :: no3 = 0.            !ppm NO3-N  |nitrate-N concentration in aquifer (initial)
        real :: minp = 0.           !kg         |mineral phosphorus in aquifer (initial)
        real :: cbn = .5            !percent    |organic carbon in aquifer (initial)
        real :: flo_dist = 50.      !m          |average flow distance to stream or object
        real :: bf_max = 0.         !mm         |maximum daily baseflow - when all channels are contributing
        real :: alpha = 0.          !1/days     |lag factor for groundwater recession curve
        real :: revap_co = 0.       !           |revap oefficient - evap=pet*revap_co
        real :: seep = 0.           !frac       |fraction of recharge that seeps from aquifer
        real :: spyld = 0.          !m^3/m^3    |specific yield of aquifer
        real :: hlife_n = 0.        !days       |half-life of nitrogen in groundwater
        real :: flo_min = 0.        !m          |water table depth for return flow to occur
        real :: revap_min = 0.      !m          |water table depth for revap to occur 
      end type aquifer_database
      type (aquifer_database), dimension(:), allocatable :: aqudb 
      
      type aquifer_data_parameters
        real :: area_ha = 0.     !ha         |surface area of aquifer
        real :: alpha = 0.       !1/days     |lag factor for groundwater recession curve
        real :: bf_max = 0.      !m          |maximum daily baseflow - when all channels are contributing
        real :: flo_min          !m          |water table depth for flow to occur
        real :: revap_co         !0-1 frac   |fraction of pet to calculate revap
        real :: revap_min = 0.   !m          |water table for revap to occur
        real :: seep = 0.        !frac       |fraction of recharge that seeps from aquifer
        real :: spyld = 0.       !m^3/m^3    |specific yield of aquifer
        real :: alpha_e = 0.     !days       |Exp(-alpha)
        real :: nloss = 0.       !frac       |nloss based on half life
        real :: rchrg_prev = 0.  !m^3        |previous days recharge
        real :: rchrgn_prev = 0. !m^3        |previous days n recharge
      end type aquifer_data_parameters
      type (aquifer_data_parameters), dimension(:), allocatable :: aqu_prm 

      type aquifer_dynamic
        real :: flo = 0.        !mm         |lateral flow from aquifer      
        real :: dep_wt = 0.     !m          |average depth from average surface elevation to water table
        real :: stor = 0.       !mm         |average water storage in aquifer in timestep
        real :: rchrg = 0.      !mm         |recharge entering aquifer from other objects
        real :: seep = 0.       !kg N/ha    |seepage from bottom of aquifer
        real :: revap = 0.      !mm         |plant water uptake and evaporation
        real :: no3 = 0.        !ppm NO3-N  |average NO3-N flowing into aquifer from another object
        real :: minp = 0.       !kg         |mineral phosphorus transported in return (lateral) flow 
        real :: cbn = 0.        !percent    |organic carbon in aquifer - currently static
        real :: orgn = 0.       !kg/ha      |organic nitrogen in aquifer - currently static
        real :: rchrg_n = 0.    !kg/NO3-N   |nitrate NO3-N flowing into aquifer from another object  
        real :: nloss = 0.      !kg/ha      |nitrate NO3-N loss
        real :: no3gw           !kg N/ha    |nitrate loading to reach in groundwater
        real :: seepno3 = 0.    !kg         |seepage of no3 to next object
        real :: flo_cha = 0.    !mm H2O     |surface runoff flowing into channels
        real :: flo_res = 0.    !mm H2O     |surface runoff flowing into reservoirs
        real :: flo_ls = 0.     !mm H2O     |surface runoff flowing into a landscape element (hru or ru)
      end type aquifer_dynamic
      type (aquifer_dynamic), dimension(:), allocatable :: aqu_om_init
      type (aquifer_dynamic), dimension(:), allocatable :: aqu_d
      type (aquifer_dynamic), dimension(:), allocatable :: aqu_m
      type (aquifer_dynamic), dimension(:), allocatable :: aqu_y
      type (aquifer_dynamic), dimension(:), allocatable :: aqu_a
      type (aquifer_dynamic), dimension(:), allocatable :: saqu_d
      type (aquifer_dynamic), dimension(:), allocatable :: saqu_m
      type (aquifer_dynamic), dimension(:), allocatable :: saqu_y
      type (aquifer_dynamic), dimension(:), allocatable :: saqu_a
      !type (aquifer_dynamic), dimension(:), allocatable :: raqu_d
      !type (aquifer_dynamic), dimension(:), allocatable :: raqu_m
      !type (aquifer_dynamic), dimension(:), allocatable :: raqu_y
      !type (aquifer_dynamic), dimension(:), allocatable :: raqu_a
      type (aquifer_dynamic) :: baqu_d
      type (aquifer_dynamic) :: baqu_m
      type (aquifer_dynamic) :: baqu_y
      type (aquifer_dynamic) :: baqu_a
      type (aquifer_dynamic) :: aquz

       type aquifer_init_data_char
        character (len=16) :: name                 !xwalk with aqudb(iaqu)%aqu_ini 
        character (len=16) :: org_min              !points to initial organic-mineral input file
        character (len=16) :: pest                 !points to initial pesticide input file
        character (len=16) :: path                 !points to initial pathogen input file
        character (len=16) :: hmet                 !points to initial heavy metals input file
        character (len=16) :: salt                 !points to initial salt input file
      end type aquifer_init_data_char
      type (aquifer_init_data_char), dimension(:), allocatable :: aqu_init_dat_c
      
      type aquifer_init_data
        integer :: org_min = 1              !points to initial organic-mineral input file
        integer :: pest = 1                 !points to initial pesticide input file
        integer :: path = 1                 !points to initial pathogen input file
        integer :: hmet = 1                 !points to initial heavy metals input file
        integer :: salt = 1                 !points to initial salt input file
      end type aquifer_init_data
      type (aquifer_init_data), dimension(:), allocatable :: aqu_init
      
      type aqu_header
          character (len=6) :: day      =      "  jday"
          character (len=6) :: mo       =      "   mon"
          character (len=6) :: day_mo   =      "   day"
          character (len=6) :: yrc      =      "    yr"
          character (len=8) :: isd      =      "   unit "                                            
          character (len=8) :: id       =      " gis_id "           
          character (len=16) :: name    =      " name              "          
          character(len=16) :: flo      =      "            flo"        ! (mm)
          character(len=16) :: dep_wt   =      "         dep_wt"        ! (m)
          character(len=15) :: stor     =      "           stor"        ! (mm)
          character(len=15) :: rchrg    =      "          rchrg"        ! (mm)
          character(len=15) :: seep     =      "           seep"        ! (mm)
          character(len=15) :: revap    =      "          revap"        ! (mm)
          character(len=15) :: no3_st   =      "         no3_st"        ! (kg/ha N)
          character(len=15) :: minp     =      "           minp"        ! (kg)
          character(len=15) :: orgn     =      "           orgn"        ! (kg/ha N)
          character(len=15) :: orgp     =      "           orgp"        ! (kg/ha P)
          character(len=15) :: rchrgn   =      "         rchrgn"        ! (kg/ha N)
          character(len=15) :: nloss    =      "          nloss"        ! (kg/ha N)
          character(len=15) :: no3gw    =      "          no3gw"        ! (kg N/ha)
          character(len=15) :: seep_no3 =      "        seepno3"        ! (kg)
          character(len=15) :: flo_cha  =      "        flo_cha"        ! (mm)
          character(len=15) :: flo_res  =      "        flo_res"        ! (mm)
          character(len=15) :: flo_ls   =      "         flo_ls"        ! (mm)
      end type aqu_header
      type (aqu_header) :: aqu_hdr
      
      type aqu_header_units
          character (len=6) :: day      =  "      "
          character (len=6) :: mo       =  "      "
          character (len=6) :: day_mo   =  "      "
          character (len=6) :: yrc      =  "      "
          character (len=8) :: isd      =  "        "                                            
          character (len=8) :: id       =  "        "           
          character (len=16) :: name    =  "                   "          
          character(len=16) :: flo      =  "              mm"         ! (mm)
          character (len=16) :: depwt   =  "              m "         ! (m)
          character(len=15) :: stor     =  "             mm"          ! (mm)
          character(len=15) :: rchrg    =  "             mm"          ! (mm)
          character(len=15) :: seep     =  "             mm"          ! (mm)
          character(len=15) :: revap    =  "             mm"          ! (mm)
          character(len=15) :: no3_st   =  "        kg/ha_N"          ! (kg/ha N)
          character(len=14) :: minp     =  "            kg"           ! (kg)
          character(len=15) :: orgn     =  "        kg/ha_N"          ! (kg/ha N)
          character(len=15) :: orgp     =  "        kg/ha_P"          ! (kg/ha P)
          character(len=15) :: rchrgn   =  "        kg/ha_N"          ! (kg/ha N)
          character(len=15) :: nloss    =  "        kg/ha_N"          ! (kg/ha N)
          character(len=15) :: no3gw    =  "        kg/ha_N"          ! (kg N/ha)
          character(len=15) :: seep_no3 =  "             kg"          ! (kg)
          character(len=15) :: flo_cha  =  "             mm"          ! (mm)
          character(len=15) :: flo_res  =  "             mm"          ! (mm)
          character(len=15) :: flo_ls   =  "             mm"          ! (mm)
      end type aqu_header_units
      type (aqu_header_units) :: aqu_hdr_units
      
      interface operator (+)
        module procedure aqu_add
      end interface
      
      interface operator (/)
        module procedure aqu_div
      end interface
        
      contains
            
      function aqu_add(aqo1,aqo2) result (aqo3)
      type (aquifer_dynamic),  intent (in) :: aqo1
      type (aquifer_dynamic),  intent (in) :: aqo2
      type (aquifer_dynamic) :: aqo3
       aqo3%flo = aqo1%flo + aqo2%flo
       aqo3%dep_wt = aqo1%dep_wt + aqo2%dep_wt
       aqo3%stor = aqo1%stor + aqo2%stor
       aqo3%no3 = aqo1%no3 + aqo2%no3   
       aqo3%minp = aqo1%minp + aqo2%minp  
       aqo3%cbn = aqo1%cbn + aqo2%cbn
       aqo3%orgn = aqo1%orgn + aqo2%orgn
       aqo3%rchrg = aqo1%rchrg + aqo2%rchrg     
       aqo3%rchrg_n = aqo1%rchrg_n + aqo2%rchrg_n         
       aqo3%nloss = aqo1%nloss + aqo2%nloss
       aqo3%seep = aqo1%seep + aqo2%seep
       aqo3%revap = aqo1%revap + aqo2%revap
       aqo3%no3gw = aqo1%no3gw + aqo2%no3gw
       aqo3%seepno3 = aqo1%seepno3 + aqo2%seepno3
       aqo3%flo_cha = aqo1%flo_cha + aqo2%flo_cha
       aqo3%flo_res = aqo1%flo_res + aqo2%flo_res
       aqo3%flo_ls = aqo1%flo_ls + aqo2%flo_ls
      end function aqu_add
      
      function aqu_div (aq1,const) result (aq2)
        type (aquifer_dynamic), intent (in) :: aq1
        real, intent (in) :: const
        type (aquifer_dynamic) :: aq2
        aq2%flo = aq1%flo / const
        aq2%dep_wt = aq1%dep_wt / const
        aq2%stor = aq1%stor / const
        aq2%no3 = aq1%no3 / const
        aq2%minp = aq1%minp / const
        aq2%cbn = aq1%cbn / const
        aq2%orgn = aq1%orgn / const
        aq2%rchrg = aq1%rchrg / const
        aq2%rchrg_n = aq1%rchrg_n / const
        aq2%nloss = aq1%nloss / const
        aq2%seep = aq1%seep / const
        aq2%revap = aq1%revap / const
        aq2%no3gw = aq1%no3gw / const
        aq2%seepno3 = aq1%seepno3 / const
        aq2%flo_cha = aq1%flo_cha / const
        aq2%flo_res = aq1%flo_res / const
        aq2%flo_ls = aq1%flo_ls / const
      end function aqu_div
        
      end module aquifer_module