    module water_allocation_module
    
      implicit none
            
      real :: trans_m3 = 0.
      real :: trn_m3 = 0.                   !m3     |demand
      
      !! transfer source objects
      type transfer_source_objects
        character (len=10) :: typ = ""          !source object type
        integer :: num = 0                      !number of the source object
        character (len=10) :: conv_typ = ""     !conveyance type - pipe or pump
        integer :: conv_num = 0                 !number of the conveyance object
        character (len=25) :: dtbl_lim = ""     !decision table name to set withdrawal limit of the source object
        real :: wdraw_lim = 0.                  !actual withdrawal limit of source object (res-frac principal, aqu-max depth (m); cha-min flow (m3/s))
        real :: frac = 0.                       !fraction of transfer supplied by the source
        character (len=1) :: comp = ""          !compensate if other source objects are past withdrawal threshold (y/n)
      end type transfer_source_objects
        
      !! source and receiving objects
      type transfer_receiving_objects
        character (len=10) :: typ = ""          !receiving object type
        integer :: num = 0                      !number of the receiving object
        !character (len=25) :: dtbl_rob = ""     !decision table name to set fraction to each receiving object
        integer :: frac = 0.                    !soil layer to receive incoming tile flow
      end type transfer_receiving_objects
        
      !! counters for outside basin source objects
      type outside_basin_objects
        integer :: daymoyr = 0              !recall file number - recall_db - daily, monthly or yearly
        integer :: aa = 0                   !exco number in exco_db - ave annual constant
      end type outside_basin_objects
        
      !! water transfer objects
      type water_transfer_objects
        integer :: num = 0                      !transfer object number
        integer :: ch_src = 0                   !channel number in transfer object (0 if no channel)
        character (len=10) :: trn_typ = ""      !transfer type - decision table, recall, ave daily
        character (len=40) :: trn_typ_name = "" !transfer type name of table or recall
        integer :: dtbl_num = 0                 !number of decision table for demand amount (if used)
        integer :: dtbl_lum = 0                 !number of decision table for demand amount for irrigation (if used)
        integer :: rec_num = 0                  !number of recall file for demand amount (if used)
        real :: amount = 0.                     !m3 per day for urban objects and mm for hru
        character (len=2) :: right = ""         !water right (sr -senior or jr - junior right)
        integer :: src_num = 0                  !number of source objects
        character (len=25) :: dtbl_src = ""     !decision table name to allocate sources
        integer :: dtbl_src_num = 0             !number of source allocation decision table
        type (transfer_source_objects), dimension(:), allocatable :: src      !sequential source objects as listed in wallo object
        type (outside_basin_objects), dimension(:), allocatable :: osrc      !number of outside basin source object - recall_db.rec file
        integer :: rcv_num = 0                  !number of receiving objects
        !character (len=25) :: dtbl_rcv = ""     !decision table name to allocate receiving objects
        type (transfer_receiving_objects) :: rcv  !receiving object
        real :: unmet_m3 = 0.                   !m3     |unmet demand for the object
        real :: withdr_tot = 0.                 !m3     |total withdrawal of demand object from all sources
        real :: irr_eff = 0.                    !irrigation in-field efficiency
        real :: surq = 0.                       !surface runoff ratio
        !type (hyd_output) :: hd
      end type water_transfer_objects

      !! source output
      type source_output
        real :: demand = 0.                     !ha-m       !demand
        real :: withdr = 0.                     !ha-m       |amoount withdrawn from the source
        real :: unmet  = 0.                     !ha-m       |unmet demand
      end type source_output
      type (source_output) :: walloz
      
      !water allocation
      type water_allocation
        character (len=25) :: name = ""         !name of the water allocation object
        character (len=25) :: rule_typ = ""     !rule type to allocate water
        integer :: trn_cur = 1                  !current transfer object
        integer :: trn_obs = 0                  !number of transfer objects
        type (source_output) :: tot             !total demand, withdrawal and unmet for entire allocation object
        type (water_transfer_objects), dimension(:), allocatable :: trn     !dimension by transfer objects
      end type water_allocation
      type (water_allocation), dimension(:), allocatable :: wallo           !dimension by water allocation objects
      type (water_allocation), pointer :: wal
      
      !! water treatment and use data
      type water_treatment_use_data
        character (len=25) :: name = ""         !name of the water treatment plant
        !character (len=25) :: init = ""         !name of the intitial concentrations in wtp storage
        real :: stor_mx                   !m3   !maximum storage in plant
        real :: lag_days                  !days !treatement time - lag outflow
        real :: loss_fr                         !water loss during treament
        character (len=25) :: org_min = ""      !sediment, carbon, and nutrients
        character (len=25) :: pests = ""        !pesticides - ppm
        character (len=25) :: paths = ""        !pathogens - cfu
        character (len=25) :: hmets = ""        !heavy metals - ppm
        character (len=25) :: salts = ""        !salt ions - ppm
        character (len=25) :: constit = ""      !other constituents - ppm
        character (len=80) :: descrip = ""      !description
        integer :: iorg_min = 0                 !sediment, carbon, and nutrients - pointer to om_use.wal
        integer :: ipests = 0                   !pesticides
        integer :: ipaths = 0                   !pathogens
        integer :: isalts = 0                   !salt ions
        integer :: iconstit = 0                 !other constituents
      end type water_treatment_use_data        
      type (water_treatment_use_data), dimension(:), allocatable :: wtp
      type (water_treatment_use_data), dimension(:), allocatable :: wuse
      
      !! outside basin source object data
      type outside_basin_source
        character (len=25) :: name = ""         !name of outside basin source
        real :: stor_mx                   !m3   !maximum storage in plant
        real :: lag_days                  !days !treatement time - lag outflow
        real :: loss_fr                         !water loss during treament
        integer :: iorg_min = 0                 !sediment, carbon, and nutrients - pointer to om_use.wal
        integer :: ipests = 0                   !pesticides
        integer :: ipaths = 0                   !pathogens
        integer :: isalts = 0                   !salt ions
        integer :: iconstit = 0                 !other constituents
      end type outside_basin_source        
      type (outside_basin_source), dimension(:), allocatable :: osrc
      
      !! outside basin receivng object data
      type outside_basin_receive
        character (len=25) :: name = ""         !name of outside basin receiving object
        character (len=25) :: filename = ""     !name of outside basin receiving object
      end type outside_basin_receive        
      type (outside_basin_receive), dimension(:), allocatable :: orcv
      
      type aquifer_loss
        integer :: aqu_num                      !aquifer number
        real :: frac                            !fraction of loss in specific aquifer
      end type aquifer_loss
      
      !! water_transfer_data
      type water_transfer_data
        character (len=25) :: name = ""         !name of the water tower or pipe
        character (len=25) :: init = ""         !name of the intitial concentrations
        real :: stor_mx                   !m3   !maximum storage in plant
        real :: ddown_days                !days !days to drawdown the storage to zero
        real :: loss_fr                         !water loss during treament
        integer :: num_aqu                      !number of aquifers
        type (aquifer_loss), dimension(:), allocatable :: aqu_loss
      end type water_transfer_data
      type (water_transfer_data), dimension(:), allocatable :: wtow        
      type (water_transfer_data), dimension(:), allocatable :: pipe
      
      !! canal data
      type water_canal_data
        character (len=25) :: name = ""         !name of the canal
        character (len=25) :: w_sta = ""        !name of nearby weather station
        character (len=25) :: init = ""         !name of the intitial concentrations in canal
        character (len=25) :: dtbl = ""         !name of decision table to determine canal outflow
        real :: ddown_days                !days !days to drawdown the storage to zero
        real :: w                         !m    !top width of canal
        real :: d                         !m    !depth of canal
        real :: s                         !m    !slope of canal
        real :: ss                        !m/m  !side slope of trapezoidal canal
        real :: sat_con                         !to compute percolation from canal to groundwater
        real :: loss_fr                         !water loss during treament
        real :: bed_thick = 0.            !m    !bed sediment thickness for Darcy seepage (gwflow; 0 if not used)
        integer :: div_id = 0                   !recall diversion ID (gwflow; 0 if wallo-routed)
        integer :: day_beg = 0                  !Julian day canal begins operation (gwflow external; 0 otherwise)
        integer :: day_end = 0                  !Julian day canal ends operation (gwflow external; 0 otherwise)
        integer :: num_aqu                      !number of aquifers
        type (aquifer_loss), dimension(:), allocatable :: aqu_loss
      end type water_canal_data    
      type (water_canal_data), dimension(:), allocatable :: canal
      
      character(len=16), dimension(:), allocatable :: om_init_name
      character(len=16), dimension(:), allocatable :: om_treat_name
      character(len=16), dimension(:), allocatable :: om_use_name
      character(len=16), dimension(:), allocatable :: om_osrc_name
      
      !transfer object output
      type transfer_object_output
        real :: trn_flo = 0.            !m3     |total transfer of the transfer object
        type (source_output), dimension(:), allocatable :: src
      end type transfer_object_output
      
      !water allocation output
      type water_allocation_output
        type (transfer_object_output), dimension(:), allocatable :: trn
      end type water_allocation_output
      type (water_allocation_output), dimension(:), allocatable :: wallod_out     !dimension by transfer objects
      type (water_allocation_output), dimension(:), allocatable :: wallom_out     !dimension by transfer objects
      type (water_allocation_output), dimension(:), allocatable :: walloy_out     !dimension by transfer objects
      type (water_allocation_output), dimension(:), allocatable :: walloa_out     !dimension by transfer objects
      
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
      
      interface operator (+)
        module procedure wallout_add
      end interface

      interface operator (/)
        module procedure wallo_div_const
      end interface   

      contains

      !! routines for hydrograph module
      function wallout_add (wallo1, wallo2) result (wallo3)
        type (source_output), intent (in) :: wallo1
        type (source_output), intent (in) :: wallo2
        type (source_output) :: wallo3
        wallo3%demand = wallo1%demand + wallo2%demand
        wallo3%withdr = wallo1%withdr + wallo2%withdr
        wallo3%unmet = wallo1%unmet + wallo2%unmet
      end function wallout_add

      function wallo_div_const (wallo1, const) result (wallo2)
        type (source_output), intent (in) :: wallo1
        real, intent (in) :: const
        type (source_output) :: wallo2
        wallo2%demand = wallo1%demand / const
        wallo2%withdr = wallo1%withdr / const
        wallo2%unmet = wallo1%unmet / const
      end function wallo_div_const

    end module water_allocation_module