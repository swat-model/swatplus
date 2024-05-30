      module reservoir_module
    
      implicit none

      real :: reactw                  !mg pst        |amount of pesticide in reach that is lost through reactions
      real :: volatpst                !mg pst        |amount of pesticide lost from reach by volatilization
      real :: setlpst                 !mg pst        |amount of pesticide moving from water to sediment due to settling
      real :: resuspst                !mg pst        |amount of pesticide moving from sediment to reach due to resuspension
      real :: difus                   !mg pst        |diffusion of pesticide from sediment to reach
      real :: reactb                  !mg pst        |amount of pesticide in sediment that is lost through reactions
      real :: bury                    !mg pst        |loss of pesticide from active sediment layer by burial

      type reservoir
        character(len=13) :: name = "default"
        integer :: ob = 0                           !object number if reservoir object; hru number if hru object
        integer :: props = 0                        !points to res_dat
        integer :: iweir                    !       !weir ID Jaehak 2023 
        character (len=1) :: rel_tbl = "d"          !d == decision table, c == conditions table
        real :: psa = 0.                    !ha     |res surface area when res is filled to princ spillway
        real :: pvol = 0.                   !ha-m   |vol of water needed to fill the res to the princ spillway (read in as ha-m and converted to m^3)
        real :: esa = 0.                    !ha     |res surface area when res is filled to emerg spillway 
        real :: evol = 0.                   !ha-m   |vol of water needed to fill the res to the emerg spillway (read in as ha-m and converted to m^3)
        real :: br1 = 0.                    !none   |vol-surface area coefficient for reservoirs (model estimates if zero)
                                            !       |vol-depth coefficient for hru impoundment
        real :: br2 = 0.                    !none   |vol-surface area coefficient for reservoirs (model estimates if zero)
                                            !       |vol-depth coefficient for hru impoundment
        real :: depth = 0                   !m      !average depth of water
        real :: weir_hgt = 0                !m      !height of weir above the bottom
        real :: weir_wid = 0                !m      !width of weir above the bottom  Jaehak 2022
        real :: seci = 0                    !m      !seci depth
        real :: prev_flo = 0                !m3     !previous days flow to smooth outflows
        real :: lag_up = 0                  !       !lag parameter for increasing outflow - prevents sudden jumps
        real :: lag_down = 0                !       !lag parameter for decreasing outflow - prevents sudden drops
        real, dimension (:), allocatable :: kd      !           |aquatic mixing velocity (diffusion/dispersion)-using mol_wt
        real, dimension (:), allocatable :: aq_mix  ! m/day     |aquatic mixing velocity (diffusion/dispersion)-using mol_wt
      end type reservoir          
      type (reservoir), dimension(:),allocatable :: res_ob
      
      type wetland
        integer :: iweir                    !       !weir ID   Jaehak 2022
        real :: psa = 0.                    !ha     |res surface area when res is filled to princ spillway
        real :: pvol = 0.                   !m^3    |vol of water needed to fill the res to the princ spillway (read in as ha-m and converted to m^3)
        real :: esa = 0.                    !ha     |res surface area when res is filled to emerg spillway 
        real :: evol = 0.                   !m^3    |vol of water needed to fill the res to the emerg spillway (read in as ha-m and converted to m^3)
        real :: area_ha = 0                 !ha     !reservoir surface area
        real :: depth = 0                   !m      !average depth of water
        real :: weir_hgt = 0                !m      !height of weir above the bottom
        real :: weir_wid = 0                !m      !width of weir   Jaehak 2022
        real :: seci = 0                    !m      !seci depth
      end type wetland          
      type (wetland), dimension(:),allocatable :: wet_ob

      type reservoir_pest_processes
        real :: react = 0.              ! kg       !pesticide lost through reactions in water layer
        real :: volat = 0.              ! kg       !pesticide lost through volatilization
        real :: settle = 0.             ! kg       !pesticide settling to benthic layer
        real :: resus = 0.              ! kg       !pesticide resuspended into lake water
        real :: difus = 0.              ! kg       !pesticide diffusing from benthic sediment to water
        real :: react_ben = 0.          ! kg       !pesticide lost from benthic by reactions
        real :: bury = 0.               ! Kg       |pesticide lost from benthic by burial
      end type reservoir_pest_processes
      type (reservoir_pest_processes), dimension(:),allocatable :: res_pest_d
      type (reservoir_pest_processes), dimension(:),allocatable :: res_pest_m
      type (reservoir_pest_processes), dimension(:),allocatable :: res_pest_y
      type (reservoir_pest_processes), dimension(:),allocatable :: res_pest_a
      type (reservoir_pest_processes), dimension(:),allocatable :: wet_pest_d
      type (reservoir_pest_processes), dimension(:),allocatable :: wet_pest_m
      type (reservoir_pest_processes), dimension(:),allocatable :: wet_pest_y
      type (reservoir_pest_processes), dimension(:),allocatable :: wet_pest_a
      
      type res_header
          ! first part of header for res_in
          character (len=5) :: day    =   " jday"
          character (len=6) :: mo     =   "   mon"
          character (len=6) :: day_mo =   "   day"
          character (len=6) :: yrc    =   "    yr"
          character (len=8) :: j      =   "  resnum "
          character (len=9) :: id     =   "  gis_id "        
          character (len=16) :: name  =   " name               " 
          character (len=13) :: flo   =   "        flo"     !! ha-m         |volume of water
          character (len=12) :: sed   =   "       sed"      !! metric tons  |sediment 
          character (len=10) :: orgn  =   "    orgn"        !! kg N         |organic N
          character (len=10) :: sedp  =   "    sedp"        !! kg P         |organic P
          character (len=10) :: no3   =   "     no3"        !! kg N         |NO3-N
          character (len=10) :: solp  =   "    solp"        !! kg P         |mineral (soluble P)
          character (len=10) :: chla  =   "    chla"        !! kg           |chlorophyll-a
          character (len=10) :: nh3   =   "     nh3"        !! kg N         |NH3
          character (len=10) :: no2   =   "     no2"        !! kg N         |NO2
          character (len=10) :: cbod  =   "    cbod"        !! kg           |carbonaceous biological oxygen demand
          character (len=10) :: dox   =   "     dox"        !! kg           |dissolved oxygen
          character (len=10) :: san   =   "     san"        !! tons         |detached sand
          character (len=10) :: sil   =   "     sil"        !! tons         |detached silt
          character (len=10) :: cla   =   "     cla"        !! tons         |detached clay
          character (len=10) :: sag   =   "     sag"        !! tons         |detached small ag
          character (len=10) :: lag   =   "     lag"        !! tons         |detached large ag
          character (len=10) :: grv   =   "     grv"        !! tons         |gravel
          character (len=10) :: temp  =   "    temp"        !! deg c        |temperature
          end type res_header
       type (res_header) :: res_hdr
      
      type res_header1
          !! this one for res_out
          character (len=8) :: flo    = "     flo"          !! ha-m         |volume of water
          character (len=10) :: sed   = "       sed"        !! metric tons  |sediment 
          character (len=10) :: orgn  = "      orgn"        !! kg N         |organic N
          character (len=10) :: sedp  = "      sedp"        !! kg P         |organic P
          character (len=10) :: no3   = "       no3"        !! kg N         |NO3-N
          character (len=10) :: solp  = "      solp"        !! kg P         |mineral (soluble P)
          character (len=10) :: chla  = "      chla"        !! kg           |chlorophyll-a
          character (len=10) :: nh3   = "       nh3"        !! kg N         |NH3
          character (len=10) :: no2   = "       no2"        !! kg N         |NO2
          character (len=10) :: cbod  = "      cbod"        !! kg           |carbonaceous biological oxygen demand
          character (len=10) :: dox   = "       dox"        !! kg           |dissolved oxygen
          character (len=10) :: san   = "       san"        !! tons         |detached sand
          character (len=10) :: sil   = "       sil"        !! tons         |detached silt
          character (len=10) :: cla   = "       cla"        !! tons         |detached clay
          character (len=10) :: sag   = "       sag"        !! tons         |detached small ag
          character (len=10) :: lag   = "       lag"        !! tons         |detached large ag
          character (len=10) :: grv   = "       grv"        !! tons         |gravel
          character (len=10) :: temp  = "      temp"        !! deg c        |temperature
          end type res_header1
       type (res_header1) :: res_hdr1
       
       type reservoir_hdr
           !! last part of header for res_om
        character (len=10) :: area_ha    = "   area_ha"
        character (len=10) :: evap       = "      evap"             !mm     |evaporation from res surface area
        character (len=10) :: seep       = "      seep"             !mm     |seepage from res bottom
        character (len=10) :: sed_setl   = " sed_setlp"             !t      |sediment settling
        character (len=10) :: seci       = "      seci"             !m      !seci depth
        character (len=10) :: solp_loss  = " solp_loss"             !kg     |soluble phosphorus loss
        character (len=10) :: sedp_loss  = " sedp_loss"             !kg     |sediment attached phosphorus loss
        character (len=10) :: orgn_loss  = " orgn_loss"             !kg     |organic nitrogen loss
        character (len=10) :: no3_loss   = "  no3_loss"             !kg     |nitrate loss
        character (len=10) :: nh3_loss   = "  nh3_loss"             !kg     |ammonium nitrogen loss
        character (len=10) :: no2_loss   = "  no2_loss"             !kg     |nitrite loss
      end type reservoir_hdr
      type (reservoir_hdr) :: res_hdr2
      
      type res_headerbsn
          ! this one used for the reservoir_???.bsn.txt files
          character (len=8) :: flo    = "     flo"            !! ha-m         |volume of water
          character (len=12) :: sed   = "         sed"        !! metric tons  |sediment 
          character (len=12) :: orgn  = "        orgn"        !! kg N         |organic N
          character (len=12) :: sedp  = "        sedp"        !! kg P         |organic P
          character (len=12) :: no3   = "         no3"        !! kg N         |NO3-N
          character (len=12) :: solp  = "        solp"        !! kg P         |mineral (soluble P)
          character (len=12) :: chla  = "        chla"        !! kg           |chlorophyll-a
          character (len=12) :: nh3   = "         nh3"        !! kg N         |NH3
          character (len=12) :: no2   = "         no2"        !! kg N         |NO2
          character (len=12) :: cbod  = "        cbod"        !! kg           |carbonaceous biological oxygen demand
          character (len=12) :: dox   = "         dox"        !! kg           |dissolved oxygen
          character (len=12) :: san   = "         san"        !! tons         |detached sand
          character (len=12) :: sil   = "         sil"        !! tons         |detached silt
          character (len=12) :: cla   = "         cla"        !! tons         |detached clay
          character (len=12) :: sag   = "         sag"        !! tons         |detached small ag
          character (len=12) :: lag   = "         lag"        !! tons         |detached large ag
          character (len=12) :: grv   = "         grv"        !! tons         |gravel
          character (len=12) :: temp  = "        temp"        !! deg c        |temperature
          end type res_headerbsn
       type (res_headerbsn) :: res_hdrbsn
       
       type res_header_unit
       !! is this correct for res_out ??? also uses hy_output ??? gsm 9/2018
          character (len=5) :: day    = "     "
          character (len=6) :: mo     = "      "
          character (len=6) :: day_mo = "      "
          character (len=6) :: yrc    = "      "
          character (len=8) :: j      = "         "
          character (len=9) :: id     = "         "        
          character (len=16) :: name  = "                   " 
          character (len=13) :: flo   = "        ha-m"    !! ha-m         |volume of water
          character (len=12) :: sed   = "   met_tons"     !! metric tons  |sediment 
          character (len=10) :: orgn  = "    kg N"        !! kg N         |organic N
          character (len=10) :: sedp  = "    kg P"        !! kg P         |organic P
          character (len=10) :: no3   = "    kg N"        !! kg N         |NO3-N
          character (len=10) :: solp  = "    kg P"        !! kg P         |mineral (soluble P)
          character (len=10) :: chla  = "      kg"        !! kg           |chlorophyll-a
          character (len=10) :: nh3   = "    kg N"        !! kg N         |NH3
          character (len=10) :: no2   = "    kg N"        !! kg N         |NO2
          character (len=10) :: cbod  = "      kg"        !! kg           |carbonaceous biological oxygen demand
          character (len=10) :: dox   = "      kg"        !! kg           |dissolved oxygen
          character (len=10) :: san   = "    tons"        !! tons         |detached sand
          character (len=10) :: sil   = "    tons"        !! tons         |detached silt
          character (len=10) :: cla   = "    tons"        !! tons         |detached clay
          character (len=10) :: sag   = "    tons"        !! tons         |detached small ag
          character (len=10) :: lag   = "    tons"        !! tons         |detached large ag
          character (len=10) :: grv   = "    tons"        !! tons         |gravel
          character (len=10) :: temp  = "   deg c"        !! deg c        |temperature 
          end type res_header_unit
       type (res_header_unit) :: res_hdr_unt
          
        type res_header_unit1
       !! is this correct for Units res_out ??? also uses hy_output ??? gsm 9/2018
          character (len=10) :: flo   = "     ha-m"         !! ha-m         |volume of water
          character (len=8) :: sed    = "met_tons"          !! metric tons  |sediment 
          character (len=10) :: orgn  = "      kg_N"        !! kg N         |organic N
          character (len=10) :: sedp  = "      kg_P"        !! kg P         |organic P
          character (len=10) :: no3   = "      kg_N"        !! kg N         |NO3-N
          character (len=10) :: solp  = "      kg_P"        !! kg P         |mineral (soluble P)
          character (len=10) :: chla  = "        kg"        !! kg           |chlorophyll-a
          character (len=10) :: nh3   = "      kg N"        !! kg N         |NH3
          character (len=10) :: no2   = "      kg N"        !! kg N         |NO2
          character (len=10) :: cbod  = "        kg"        !! kg           |carbonaceous biological oxygen demand
          character (len=10) :: dox   = "        kg"        !! kg           |dissolved oxygen
          character (len=10) :: san   = "      tons"        !! tons         |detached sand
          character (len=10) :: sil   = "      tons"        !! tons         |detached silt
          character (len=10) :: cla   = "      tons"        !! tons         |detached clay
          character (len=10) :: sag   = "      tons"        !! tons         |detached small ag
          character (len=10) :: lag   = "      tons"        !! tons         |detached large ag
          character (len=10) :: grv   = "      tons"        !! tons         |gravel
          character (len=10) :: temp  = "     deg_c"        !! deg c        |temperature 
          end type res_header_unit1
       type (res_header_unit1) :: res_hdr_unt1
       
       type res_header_unit2
           !! last part of units 
        character (len=10) :: area_ha   =    "        ha"  
        character (len=10) :: evap      =    "        mm"
        character (len=10) :: seep      =    "        mm"        !mm     |seepage from res bottom
        character (len=10) :: sed_setl  =    "         t"        !t      |sediment settling
        character (len=10) :: seci      =    "         m"        !m      !seci depth 
        character (len=10) :: solp_loss =    "        kg"        !kg     |soluble phosphorus loss  
        character (len=10) :: sedp_loss =    "        kg"        !kg     |sediment attached phosphorus loss 
        character (len=10) :: orgn_loss =    "        kg"        !kg     |organic nitrogen loss 
        character (len=10) :: no3_loss  =    "        kg"        !kg     |nitrate loss
        character (len=10) :: nh3_loss  =    "        kg"        !kg     |ammonium nitrogen loss
        character (len=10) :: no2_loss  =    "        kg"        !kg     |nitrite loss
      end type res_header_unit2
type (res_header_unit2) res_hdr_unt2

type res_header_unitbsn
       !! is this correct for Units res_out ??? also uses hy_output ??? gsm 9/2018
          character (len=10) :: flo   = "     m^3"        !! m^3          |volume of water
          character (len=12) :: sed   = "  met_tons"        !! metric tons  |sediment 
          character (len=10) :: orgn  = "      kg_N"        !! kg N         |organic N
          character (len=12) :: sedp  = "        kg_P"        !! kg P         |organic P
          character (len=12) :: no3   = "        kg_N"        !! kg N         |NO3-N
          character (len=12) :: solp  = "        kg_P"        !! kg P         |mineral (soluble P)
          character (len=12) :: chla  = "          kg"        !! kg           |chlorophyll-a
          character (len=12) :: nh3   = "        kg_N"        !! kg N         |NH3
          character (len=12) :: no2   = "        kg_N"        !! kg N         |NO2
          character (len=12) :: cbod  = "          kg"        !! kg           |carbonaceous biological oxygen demand
          character (len=12) :: dox   = "          kg"        !! kg           |dissolved oxygen
          character (len=12) :: san   = "        tons"        !! tons         |detached sand
          character (len=12) :: sil   = "        tons"        !! tons         |detached silt
          character (len=12) :: cla   = "        tons"        !! tons         |detached clay
          character (len=12) :: sag   = "        tons"        !! tons         |detached small ag
          character (len=12) :: lag   = "        tons"        !! tons         |detached large ag
          character (len=12) :: grv   = "        tons"        !! tons         |gravel
          character (len=12) :: temp  = "       deg_c"        !! deg c        |temperature 
          end type res_header_unitbsn
       type (res_header_unitbsn) :: res_hdr_untbsn

      end module reservoir_module