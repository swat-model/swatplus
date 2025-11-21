      module water_allocation_module
    
      implicit none
            
      real :: trans_m3 = 0.
      real :: trn_m3 = 0.                   !m3     |demand
      
      !! water source objects
      type water_source_objects
        integer :: num = 0                      !source object number
        character (len=6) :: ob_typ = ""        !channel(cha), reservoir(res), aquifer(aqu), unlimited source(unl)
        integer :: ob_num = 0                   !number of the object type
        character (len=10) :: lim_typ = ""      !selecting how to determine available water - decision table (dtbl), recall file (rec), or monthly limit (mon_lim)
        character (len=25) :: lim_name = ""     !name of decision table or recall file
        integer :: dtbl_num = 0                 !number of decision table for available water (if used)
        integer :: rec_num = 0                  !number of recall file for available water (if used)
        real, dimension (12) :: limit_mon = 0.  !min chan flow(m3/s), min res level(frac prinicpal), max aqu depth(m)
      end type water_source_objects
      
      !! transfer source objects
      type transfer_source_objects
        character (len=10) :: typ = ""          !source object type
        integer :: num = 0                      !number of the source object
        character (len=10) :: conv_typ = ""     !conveyance type - pipe or pump
        integer :: conv_num = 0                 !number of the conveyance object
        real :: frac = 0.                       !fraction of transfer supplied by the source
        character (len=1) :: comp = ""          !compensate if other source objects are past withdrawal threshold (y/n)
      end type transfer_source_objects
        
      !! source and receiving objects
      type transfer_receiving_objects
        character (len=10) :: typ = ""          !receiving object type
        integer :: num = 0                      !number of the receiving object
      end type transfer_receiving_objects
        
      !! water transfer objects
      type water_transfer_objects
        integer :: num = 0                      !transfer object number
        character (len=10) :: trn_typ = ""      !transfer type - decision table, recall, ave daily
        character (len=10) :: trn_typ_name = "" !transfer type name of table or recall
        integer :: dtbl_num = 0                 !number of decision table for demand amount (if used)
        integer :: rec_num = 0                  !number of recall file for demand amount (if used)
        real :: amount = 0.                     !m3 per day for urban objects and mm for hru
        character (len=2) :: right = ""         !water right (sr -senior or jr - junior right)
        integer :: src_num = 0                  !number of source objects
        character (len=25) :: dtbl_src = ""     !decision table name to allocate sources
        integer :: dtbl_src_num = 0             !number of source allocation decision table
        type (transfer_source_objects), dimension(:), allocatable :: src      !sequential source objects as listed in wallo object
        integer, dimension(:), allocatable :: src_wal
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
        integer :: src_obs = 0                  !number of source objects
        integer :: trn_obs = 0                  !number of transfer objects
        integer :: out_src = 0                  !number of source objects outside the basin
        integer :: out_rcv = 0                  !number of receiving objects outside the basin
        integer :: wtp = 0                      !number of water treatment objects
        integer :: uses = 0                     !number of water use objects (domestic, industrial, municipal)
        integer :: stor = 0                     !number of urban storage objects (water towers)
        integer :: pipe = 0                     !number of pipe objects
        integer :: canal = 0                    !number of canal objects
        integer :: pump = 0                     !number of pump objects
        character (len=1) :: cha_ob = ""        !y-yes there is a channel object; n-no channel object (only one per water allocation object)
        type (source_output) :: tot             !total demand, withdrawal and unmet for entire allocation object
        type (water_source_objects), dimension(:), allocatable :: src       !dimension by source objects
        type (water_transfer_objects), dimension(:), allocatable :: trn     !dimension by demand objects
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
      
      type aquifer_loss
        real :: aqu_num                         !aquifer number
        real :: frac                            !fraction of loss in specific aquifer
      end type aquifer_loss
      
      !! water_transfer_data
      type water_transfer_data
        character (len=25) :: name = ""         !name of the water treatment plant
        character (len=25) :: init = ""         !name of the intitial concentrations in wtp storage
        real :: stor_mx                   !m3   !maximum storage in plant
        real :: lag_days                  !days !treatement time - lag outflow
        real :: loss_fr                         !water loss during treament
        integer :: num_aqu                      !number of aquifers
        type (aquifer_loss), dimension(:), allocatable :: aqu_loss
      end type water_transfer_data
      type (water_transfer_data), dimension(:), allocatable :: wtow        
      type (water_transfer_data), dimension(:), allocatable :: pipe        
      type (water_transfer_data), dimension(:), allocatable :: canal
      
      character(len=16), dimension(:), allocatable :: om_init_name
      character(len=16), dimension(:), allocatable :: om_treat_name
      character(len=16), dimension(:), allocatable :: om_use_name
      
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