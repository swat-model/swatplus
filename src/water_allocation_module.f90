    module water_allocation_module
    
      implicit none
            
      integer :: wuses = 0     !number of water use objects in simulation
      integer :: wtps = 0      !number of water treatment plant objects in simulation
      real :: trans_m3 = 0.
      real :: dmd_m3 = 0.                   !m3     |demand
      real, dimension(6) :: trn_fr = 0.     !frac   |transfer fraction for each source object (up to 6)
      character (len=25) :: wallo_name = ""         !name of water allocation object
      
      !! point of diversion objects (POD) for each place of use (POU)
      type pou_points_of_delivery
        character (len=25) :: name = ""         !name of POD
        integer :: num = 0                      !POD number
        character (len=10) :: typ = ""          !type of POD - channel, reservoir, aquifer, canal, etc
        integer :: typ_num = 0                  !type number
        character (len=10) :: conv_typ = ""     !conveyance type - pipe or pump
        integer :: conv_num = 0                 !number of the conveyance object
        character (len=25) :: dtbl_min = ""     !decision table name to set minimum level of POD for withdrawal
        integer :: dtbl_min_num = 0             !decision table number to set minimum level of POD for withdrawal
        real :: const_min = 0.                  !fixed min daily level - if dtble is not used
        character (len=25) :: dtbl_wdraw = ""   !decision table name to set maximum withdrawal
        integer :: dtbl_wdraw_num = 0           !decision table number to set maximum POD withdrawal
        real :: ann_max = 0.                    !annual maximim withdrawal (m3/s) - if dtbl not used
        real :: wdraw_max = 0                   !maximum withdrawal from POD during the time period
        real :: wdraw_cur = 0                   !current withdrawal from POD during the time period
        real :: frac = 0.                       !fraction of daily right from the POD (m3/s)
        real :: duty = 0.                       !annual maximim withdrawal (m3/s)
        real :: deliv = 0.                      !fraction of daily right from the POD (m3/s)
        character (len=1) :: comp = ""          !compensate if unmet (y/n)
        character (len=1) :: fin = ""           !water taken from all POD in the POU (y/n)
      end type pou_points_of_delivery
        
      type pou_points_of_return
        character (len=25) :: name = ""         !name of POR
        character (len=10) :: typ = ""          !type of POR
        integer :: num = 0                      !POR number
        character (len=10) :: conv_typ = ""     !conveyance type - pipe or pump
        integer :: conv_num = 0                 !number of the conveyance object
        character (len=25) :: dtbl_max = ""     !decision table name to set maximum level of POR for return
        real :: const_max = 0.                  !fixed max daily level - if dtble is not used
        real :: ann_max = 0.                    !annual maximim inflow (m3/s)
        real :: frac = 0.                       !fraction of POU outflow to each POR (m3/s)
      end type pou_points_of_return
        
      !! irrigation amount and irrigation operations number (irr.ops) if POU type is irr
      type pou_irrigation
        integer :: hru_num = 0                  !number of nrus in the farm/district that are irrigated
        integer, dimension(:), allocatable :: hru                          !hru number
        character (len=25), dimension(:), allocatable :: dtbl_lum          !decision table name to set daily irrigation demand
        integer, dimension(:), allocatable :: dtbl_num                     !decision table number to set daily irrigation demand
      end type pou_irrigation
      
      !! hru number and decision table for hrus that are irrigated
      type hru_irrigation
        character (len=25) :: name = ""         !name of irrigation useruser
        integer :: hrus = 0                     !number of irrigation operations from irr.ops
        real :: irr_dmd = 0.                    !irrigation demand of the irrigation object (m3/s)
        integer, dimension(:), allocatable :: hru_num                      !hru number
        character (len=25), dimension(:), allocatable :: dtbl_lum          !decision table name to set daily irrigation demand
      end type hru_irrigation
      type (hru_irrigation), dimension(:), allocatable :: hruirr_db        !POU data for the water allocation
      
      !! place of use objects (POU)
      type place_of_use
        character (len=25) :: name = ""         !name of POU
        character (len=10) :: typ = ""          !type of POU
        integer :: typ_num = 0                  !POU number
        integer :: pods = 0                     !number of sources or points of diversion (PODs) for the POU
        integer :: pors = 0                     !number of points of return (PORs) for the POU
        integer :: num_pods = 0                 !number of PODs for the POU
        character (len=25) :: dtbl_mx = ""      !decision table name to set max daily right or duty
        integer :: dtbl_mx_num = 0              !decision table number to set max daily right or duty
        real :: rate_max = 0.                   !fixed max daily right or duty (m3/s) - if dtble is not used
        real :: demand = 0.                     !irrigation demand of all hrus in the POU (m3/s)
        character (len=25) :: dtbl_pod_fr = ""  !decision table name to set fractions from each POD - if null use constant fraction
        integer :: dtbl_pod_fr_num = 0          !decision table name to set fractions from each POD - if null use constant fraction
        character (len=25) :: dtbl_por_fr = ""  !decision table name to set fractions to each POR - if null use constant fraction
        integer :: dtbl_por_fr_num = 0          !decision table name to set fractions to each POR - if null use constant fraction
        character (len=1) :: fin = ""           !water taken from all POD in the POU (y/n)
        type (pou_irrigation) :: irr             !irrigation hru and dtbl if POU type is irr
        type (pou_points_of_delivery), dimension(:), allocatable :: pod     !POD data for the POU
        type (pou_points_of_return), dimension(:), allocatable :: por       !POR data for the POU
      end type place_of_use
      type (place_of_use), dimension(:), allocatable :: pou     !POU data for the water allocation
        
      !! place of use objects (POU) for each point of diversion (POD)
      type pod_points_of_use
        integer :: num = 0                      !POU number
        character (len=25) :: name = ""         !name of POU
        integer :: pod_num = 0                  !POD number in POU
        character (len=10) :: typ = ""          !type of POD
        integer :: typ_num = 0                  !POD type number
        character (len=25) :: right = ""        !water right
      end type pod_points_of_use
        
      !! point of diversion objects (POD)
      type point_of_delivery
        integer :: num = 0                      !POD number
        character (len=25) :: name = ""         !name of POD
        character (len=10) :: typ = ""          !type of POD
        integer :: typ_num = 0                  !POD type number
        integer :: pous = 0                     !number of places of use (POUs) for the POD
        character (len=1) :: fin = ""           !water taken from all POD in the POU (y/n)
        type (pod_points_of_use), dimension(:), allocatable :: pou      !POU of the POD
      end type point_of_delivery
      type (point_of_delivery), dimension(:), allocatable :: pod     !POD data for the water allocation
      
      !! duty and delivery the POU and for each POD for outputting
      type duty_delivered
        real :: duty = 0.                       !ha-m       |duty or demand from the POD and total duty for the POU
        real :: deliv  = 0.                     !ha-m       |delivered from the POD and total delivered for the POU
      end type duty_delivered
      type (duty_delivered) :: duty_delivz
      
      type pou_duty_delivered
        type (duty_delivered) :: duty_tot                           !ha-m       !total pou duty or demand
        type (duty_delivered), dimension(:), allocatable :: pod     !ha-m       |duty or demand from each POD
      end type pou_duty_delivered
      type (pou_duty_delivered), dimension(:), allocatable :: poud_met     !daily duty and delivery
      type (pou_duty_delivered), dimension(:), allocatable :: poum_met     !monthly duty and delivery
      type (pou_duty_delivered), dimension(:), allocatable :: pouy_met     !yearly duty and delivery
      type (pou_duty_delivered), dimension(:), allocatable :: poua_met     !average annual duty and delivery
      
      !! ?????NOT SURE????? counters for outside basin source objects
      type outside_basin_objects
        integer :: daymoyr = 0              !recall file number - recall_db - daily, monthly or yearly
        integer :: aa = 0                   !exco number in exco_db - ave annual constant
      end type outside_basin_objects
        
      !! water treatment concentration adjustment for each treatment level
      type concentration_levels
        character (len=6) :: org_min_typ = ""       !const, dtbl, recall
        character (len=6) :: pests_typ = ""         !const, dtbl, recall
        character (len=6) :: paths_typ = ""         !const, dtbl, recall
        character (len=6) :: hmets_typ = ""         !const, dtbl, recall      
        character (len=6) :: salts_typ = ""         !const, dtbl, recall
        character (len=6) :: constit_typ = ""       !const, dtbl, recall
        character (len=25) :: org_min_name = ""     !sediment, carbon, and nutrients
        character (len=25) :: pests_name = ""       !pesticides - ppm
        character (len=25) :: paths_name = ""       !pathogens - cfu
        character (len=25) :: hmets_name = ""       !heavy metals - ppm
        character (len=25) :: salts_name = ""       !salt ions - ppm
        character (len=25) :: constit_name = ""     !other constituents - ppm
        integer :: om_num = 0              !id for sediment, carbon, and nutrients
        integer :: pest_num = 0            !id for pesticides
        integer :: path_num = 0            !id for pathogens
        integer :: hmet_num = 0            !id for heavy metals
        integer :: salt_num = 0            !id for salt ions
        integer :: constit_num = 0         !id for other constituents
        character (len=80) :: descrip = ""      !concentration level description
      end type concentration_levels
        
      !! water treatment and use data
      type water_treatment_use_data
        character (len=25) :: name = ""         !name of the water treatment plant
        integer :: db_num = 0                   !data file pointer
        integer :: wallo_pod = 0                !POD (point of diversion) number for water allocation - 0 if not POD
        real :: stor_mx                   !m3   !maximum storage in plant
        real :: lag_days                  !days !treatment time - lag outflow
        real :: loss_fr                         !water loss during treament
        integer :: num_treats = 0               !number of treatment levels for the water treatment plant
        type (concentration_levels), dimension(:), allocatable :: conc
        character (len=80) :: descrip = ""      !treatment plant description
      end type water_treatment_use_data        
      type (water_treatment_use_data), dimension(:), allocatable :: wtp
      type (water_treatment_use_data), dimension(:), allocatable :: wuse
      type (water_treatment_use_data), dimension(:), allocatable :: osrc
      
      !! outside basin receivng object data
      type outside_basin_receive
        character (len=25) :: name = ""         !name of outside basin receiving object
        character (len=25) :: filename = ""     !name of outside basin receiving object
      end type outside_basin_receive        
      type (outside_basin_receive), dimension(:), allocatable :: orcv
      
      !! water_transfer_data
      type water_transfer_data
        character (len=25) :: name = ""         !name of the water tower or pipe
        integer :: wallo_pod = 0                !POD (point of diversion) number for water allocation - 0 if not POD
        character (len=25) :: init = ""         !name of the initial concentrations
        real :: stor_mx                   !m3   !maximum storage in plant
        real :: ddown_days                !days !days to drawdown the storage to zero
        real :: loss_fr                         !water loss during treament
        integer :: num_aqu                      !number of aquifers
        real, dimension(:), allocatable :: aqu_loss_fr
      end type water_transfer_data
      type (water_transfer_data), dimension(:), allocatable :: wtow        
      type (water_transfer_data), dimension(:), allocatable :: pipe
      
      !! canal data
      type water_canal_data
        character (len=25) :: name = ""         !name of the canal
        character (len=25) :: w_sta = ""        !name of nearby weather station
        character (len=25) :: init = ""         !name of the initial concentrations in canal
        character (len=25) :: dtbl = ""         !name of decision table to determine canal outflow
        integer :: wallo_pod = 0                !POD (point of diversion) number for water allocation - 0 if not POD
        real :: ddown_days                !days !days to drawdown the storage to zero
        real :: w                         !m    !bottom width of canal
        real :: d                         !m    !depth of canal
        real :: l                         !km   !length of canal
        real :: s                         !m    !slope of canal
        real :: stor_mx                  !m    !maximum storage in canal
        real :: ss                        !m/m  !side slope of trapezoidal canal (horizontal/vertical)
        real :: evap_co                   !     !evap coef to compute evaporation loss
        real :: sat_con                   !mm/d !to compute percolation from canal to groundwater
        real :: loss_fr                         !water loss - seepage to aquifer
        real :: bed_thick = 0.            !m    !bed sediment thickness for Darcy seepage (gwflow; 0 if not used)
        integer :: div_id = 0                   !recall diversion ID (gwflow; 0 if wallo-routed)
        integer :: day_beg = 0                  !Julian day canal begins operation (gwflow external; 0 otherwise)
        integer :: day_end = 0                  !Julian day canal ends operation (gwflow external; 0 otherwise)
        integer :: num_aqu                      !number of aquifers
        real, dimension(:), allocatable :: aqu_loss_fr
      end type water_canal_data    
      type (water_canal_data), dimension(:), allocatable :: canal
      
      character(len=16), dimension(:), allocatable :: om_init_name
      character(len=16), dimension(:), allocatable :: om_treat_name
      character(len=16), dimension(:), allocatable :: om_use_name
      character(len=16), dimension(:), allocatable :: om_osrc_name
      
      type wallo_header            
        character(len=6) :: day      =   "  jday"
        character(len=6) :: mo       =   "   mon"
        character(len=6) :: day_mo   =   " day "
        character(len=6) :: yrc      =   " yr  "
        character(len=8) :: itrn     =   " unit   "
        character(len=16) :: trn_typ  =  "trn_typ         "
        character(len=16) :: trn_num =   "    trn_num     "
        character(len=17) :: rcv_typ  =  "drcv_typ         "
        character(len=16) :: rcv_num =   "    rcv_num     "
        character(len=12) :: src1_obj =  "   src1_obj "
        character(len=12) :: src1_typ =  " src1_typ   "
        character(len=12)  :: src1_num = " src1_num   "
        character(len=15) :: trn1  =     "    demand     "      !! ha-m     |demand - muni or irrigation       
        character(len=15) :: s1out  =   "src1_withdraw  "       !! ha-m     |withdrawal from source 1
        character(len=12) :: s1un =    "  src1_unmet"          !! ha-m     |unmet from source 1 
        character(len=12) :: src2_typ =  " src2_typ   "
        character(len=12)  :: src2_num = " src2_num   "
        character(len=15) :: trn2  =     "    demand     "      !! ha-m     |demand - muni or irrigation       
        character(len=15) :: s2out  =   "src2_withdraw  "       !! ha-m     |withdrawal from source 2
        character(len=12) :: s2un =    "  src2_unmet"          !! ha-m     |unmet from source 2           
        character(len=12) :: src3_typ =  " src3_typ   "
        character(len=12)  :: src3_num = " src3_num   "
        character(len=15) :: trn3  =     "    demand     "      !! ha-m     |demand - muni or irrigation       
        character(len=15) :: s3out  =   "src3_withdraw  "       !! ha-m     |withdrawal from source 3
        character(len=12) :: s3un =    "  src3_unmet"          !! ha-m     |unmet from source 3      

        end type wallo_header
      type (wallo_header) :: wallo_hdr

      type wallo_header_units         
        character (len=8) :: day      =  "        "
        character (len=8) :: mo       =  "        "
        character (len=8) :: day_mo   =  "        "
        character (len=8) :: yrc      =  "        "
        character (len=8) :: itrn     =  "        "
        character (len=16) :: trn_typ  =  "                "
        character (len=16) :: trn_num  =  "                "
        character (len=16) :: rcv_typ  =  "                "
        character (len=16) :: rcv_num  =  "                "
        character (len=12) :: src1_obj =  "            "
        character (len=12) :: src1_typ =  "            "
        character (len=8) :: src1_num =  "        "
        character (len=15) :: trn1 =      "m^3            "            !! ha-m    |demand - muni or irrigation
        character (len=15) :: s1out =     "m^3            "            !! ha-m    |withdrawal from source 1       
        character (len=9) :: s1un =      "m^3      "                   !! ha-m    |unmet from source 1 
        character (len=15) :: src2_typ =  "               "
        character (len=15) :: src2_num =  "               "
        character (len=15) :: trn2 =      "m^3            "        !! ha-m    |demand - muni or irrigation
        character (len=15) :: s2out =     "m^3            "        !! ha-m    |withdrawal from source 2       
        character (len=15) :: s2un =      "m^3            "        !! ha-m    |unmet from source 2        
        character (len=15) :: src3_typ =  "               "
        character (len=15) :: src3_num =  "               "
        character (len=15) :: trn3 =      "m^3            "        !! ha-m    |demand - muni or irrigation
        character (len=15) :: s3out =     "m^3            "        !! ha-m    |withdrawal from source 3       
        character (len=15) :: s3un =      "m^3            "        !! ha-m    |unmet from source 3   

        end type wallo_header_units
      type (wallo_header_units) :: wallo_hdr_units 
      
!! wallo USE headers
      type wallo_use_header       
        character(len=11) :: day      =   "       jday"
        character(len=12) :: mo       =   "         mon"
        character(len=12) :: day_mo   =   "         day"
        character(len=12) :: yrc      =   "          yr"         
        character(len=17) :: name     =   "     name        "
        character(len=12) :: iuse     =   "         use"       
        character (len=15) :: flo     =   "            flo"      !! ha-m         |volume of water
        character (len=15) :: sed     =   "            sed"        !! metric tons  |sediment
        character (len=15) :: orgn    =   "           orgn"        !! kg N         |organic N
        character (len=15) :: sedp    =   "           sedp"        !! kg P         |organic P
        character (len=15) :: no3     =   "            no3"        !! kg N         |NO3-N
        character (len=15) :: solp    =   "           solp"        !! kg P         |mineral (soluble P)
        character (len=15) :: chla    =   "           chla"        !! kg           |chlorophyll-a
        character (len=15) :: nh3     =   "            nh3"        !! kg N         |NH3
        character (len=15) :: no2     =   "            no2"        !! kg N         |NO2
        character (len=15) :: cbod    =   "           cbod"        !! kg           |carbonaceous biological oxygen demand
        character (len=15) :: dox     =   "            dox"        !! kg           |dissolved oxygen
        character (len=15) :: san     =   "            san"        !! tons         |detached sand
        character (len=15) :: sil     =   "            sil"        !! tons         |detached silt
        character (len=15) :: cla     =   "            cla"        !! tons         |detached clay
        character (len=15) :: sag     =   "            sag"        !! tons         |detached small ag
        character (len=15) :: lag     =   "            lag"        !! tons         |detached large ag
        character (len=15) :: grv     =   "            grv"        !! tons         |gravel
        character (len=15) :: temp    =   "           null"        !! deg c        |temperature
        end type wallo_use_header
      type (wallo_use_header) :: wallo_use_hdr

   type wallo_use_header_units      
    character (len=11) :: day      =  "           "
    character (len=12) :: mo       =  "           "
    character (len=12) :: day_mo   =  "           "
    character (len=12) :: yrc      =  "           "   
    character(len=17) :: name     =   "                "
    character(len=12) :: iuse     =   "           "
    character (len=15) :: flo    =  "          m^3/s"        !! m^3/s        |volume of water
    character (len=15) :: sed    =  "           tons"        !! metric tons  |sediment
    character (len=15) :: orgn   =  "            kgN"        !! kg N         |organic N
    character (len=15) :: sedp   =  "            kgP"        !! kg P         |organic P
    character (len=15) :: no3    =  "            kgN"        !! kg N         |NO3-N
    character (len=15) :: solp   =  "            kgP"        !! kg P         |mineral (soluble P)
    character (len=15) :: chla   =  "             kg"        !! kg           |chlorophyll-a
    character (len=15) :: nh3    =  "            kgN"        !! kg N         |NH3
    character (len=15) :: no2    =  "            kgN"        !! kg N         |NO2
    character (len=15) :: cbod   =  "             kg"        !! kg           |carbonaceous biological oxygen demand
    character (len=15) :: dox    =  "             kg"        !! kg           |dissolved oxygen
    character (len=15) :: san    =  "           tons"        !! tons         |detached sand
    character (len=15) :: sil    =  "           tons"        !! tons         |detached silt
    character (len=15) :: cla    =  "           tons"        !! tons         |detached clay
    character (len=15) :: sag    =  "           tons"        !! tons         |detached small ag
    character (len=15) :: lag    =  "           tons"        !! tons         |detached large ag
    character (len=15) :: grv    =  "           tons"        !! tons         |gravel
    character (len=15) :: temp   =  "               "        !! deg c        |temperature   
  end type wallo_use_header_units
  type (wallo_use_header_units) :: wallo_use_hdr_units 

    end module water_allocation_module