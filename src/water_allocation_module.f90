      module water_allocation_module
    
      use hydrograph_module, only : hyd_output
    
      implicit none
            
      real :: trans_m3
      real :: dmd_m3                        !m3     |demand
      character(len=16), dimension(:), allocatable :: trt_om_name    !treatment name in treatment.trt
      
      !water source objects
      type water_source_objects
        integer :: num                          !demand object number
        character (len=3) :: ob_typ             !channel(cha), reservoir(res), aquifer(aqu), unlimited source(unl)
        integer :: ob_num                       !number of the object type
        real, dimension (12) :: limit_mon       !min chan flow(m3/s), min res level(frac prinicpal), max aqu depth(m)
        character (len=25) :: div_rec
        integer :: rec_num
        real :: div_vol
      end type water_source_objects

      !demand source objects
      type water_demand_sources
        integer :: src                          !sequential source number as listed in wallo object
        real :: frac                            !fraction of demand supplied by the source
        character (len=1) :: comp               !compensate from source if other sources are limiting (y/n)
      end type water_demand_sources
      
      !canal diversion source object (rtb)
      real, dimension (:), allocatable :: div_volume_daily   !daily volume of canal water added to total
      real, dimension (:), allocatable :: div_volume_total   !volume of canal water available for irrigation
      real, dimension (:), allocatable :: div_volume_used    !volume of canal water used for irrigation
      real :: div_delay                                      !number of days that diverted irrigation water can be used
      
          
      !demand source objects
      type water_demand_source_objects
        character (len=10) :: ob_typ            !hru (for irrigation) or muni (municipal) or divert (interbasin diversion)
        integer :: ob_num                       !number of the object type
      end type water_demand_source_objects
          
      !water demand objects
      type water_demand_objects
        integer :: num                          !demand object number
        character (len=10) :: ob_typ            !hru (for irrigation) or muni (municipal) or divert (interbasin diversion)
        integer :: ob_num                       !number of the object type
        character (len=25) :: withdr            !withdrawal type - ave_day or recall for muni and divert - irrig for hru
        real :: amount                          !m3 per day for muni and mm for hru
        character (len=2) :: right              !water right (sr -senior or jr - junior right)
        character (len=25) :: treat_typ         !recall for inputting a recall object and treat for a treatment object
        character (len=25) :: treatment         !pointer to the recall or dr file
        character (len=10) :: rcv_ob            !receiving object (channel, reservoir, aquifer) - no dtl - all return to this object
        integer :: rcv_num                      !receiving object number
        character (len=10) :: rcv_dtl           !receiving object decision table - to condition water transfers and diversions
        integer :: rec_num                      !recall number when using recall for muni or divert demands
        integer :: trt_num                      !treatment database number when treating the withdrawn water
        integer :: dmd_src_obs                  !number of source objects available for the demand object
        real :: unmet_m3                        !m3     |unmet demand for the object
        real :: withdr_tot                      !m3     |total withdrawal of demand object from all souces
        real :: irr_eff                         !irrigation in-field efficiency
        real :: surq                            !surface runoff ratio
        type (hyd_output) :: hd
        type (hyd_output) :: trt
        type (water_demand_sources), dimension(:), allocatable :: src               !sequential source objects as listed in wallo object
        type (water_demand_source_objects), dimension(:), allocatable :: src_ob     !type and number of each source object
      end type water_demand_objects

      !source output
      type source_output
        real :: demand = 0.                     !ha-m       !demand
        real :: withdr = 0.                     !ha-m       |amoount withdrawn from the source
        real :: unmet  = 0.                     !ha-m       |unmet demand
      end type source_output
      type (source_output) :: walloz
      
      !water allocation
      type water_allocation
        character (len=25) :: name              !name of the water allocation object
        character (len=25) :: rule_typ          !rule type to allocate water
        integer :: src_obs                      !number of source objects
        integer :: dmd_obs                      !number of demand objects
        character (len=1) :: cha_ob             !y-yes there is a channel object; n-no channel object (only one per water allocation object)
        integer :: cha                          !channel number
        type (source_output) :: tot             !total demand, withdrawal and unmet for entire allocation object
        type (water_source_objects), dimension(:), allocatable :: src        !dimension by source objects
        type (water_demand_objects), dimension(:), allocatable :: dmd        !dimension by demand objects
      end type water_allocation
      type (water_allocation), dimension(:), allocatable :: wallo            !dimension by water allocation objects

      !demand object output
      type demand_object_output
        real :: dmd_tot                 !m3     |total demand of the demand object
        type (source_output), dimension(:), allocatable :: src
      end type demand_object_output
      
      !water allocation output
      type water_allocation_output
        type (demand_object_output), dimension(:), allocatable :: dmd
      end type water_allocation_output
      type (water_allocation_output), dimension(:), allocatable :: wallod_out     !dimension by demand objects
      type (water_allocation_output), dimension(:), allocatable :: wallom_out     !dimension by demand objects
      type (water_allocation_output), dimension(:), allocatable :: walloy_out     !dimension by demand objects
      type (water_allocation_output), dimension(:), allocatable :: walloa_out     !dimension by demand objects
      
      type wallo_header            
		character(len=6) :: day      =	 "  jday"       
		character(len=6) :: mo       =	 "	 mon"
		character(len=6) :: day_mo   =	 " day "
		character(len=6) :: yrc      =	 " yr  "        
		character(len=8) :: idmd	 =	 " unit   "      
		character(len=16) :: dmd_typ  =  "dmd_typ         "
		character(len=16) :: dmd_num =	 "    dmd_num     "     
		character(len=16) :: rcv_typ  =  "drcv_typ         "
		character(len=16) :: rcv_num =	 "    rcv_num     "   
        character(len=12) :: src1_obj =  "   src1_obj "
		character(len=12) :: src1_typ =	 " src1_typ   " 
        character(len=12)  :: src1_num = " src1_num	  "                                      
        character(len=15) :: dmd1  =     "	  demand     "      !! ha-m     |demand - muni or irrigation       
        character(len=15) :: s1out  =   "src1_withdraw  "       !! ha-m     |withdrawal from source 1
        character(len=12) :: s1un =    "  src1_unmet"          !! ha-m     |unmet from source 1 
		character(len=12) :: src2_typ =	 " src2_typ   " 
        character(len=12)  :: src2_num = " src2_num	  "                                      
        character(len=15) :: dmd2  =     "	  demand     "      !! ha-m     |demand - muni or irrigation       
        character(len=15) :: s2out  =   "src2_withdraw  "       !! ha-m     |withdrawal from source 2
        character(len=12) :: s2un =    "  src2_unmet"          !! ha-m     |unmet from source 2           
		character(len=12) :: src3_typ =	 " src3_typ   " 
        character(len=12)  :: src3_num = " src3_num	  "                                      
        character(len=15) :: dmd3  =     "	  demand     "      !! ha-m     |demand - muni or irrigation       
        character(len=15) :: s3out  =   "src3_withdraw  "       !! ha-m     |withdrawal from source 3
        character(len=12) :: s3un =    "  src3_unmet"          !! ha-m     |unmet from source 3      

        end type wallo_header
      type (wallo_header) :: wallo_hdr

      type wallo_header_units         
		character (len=8) :: day	  =  "	      "
		character (len=8) :: mo       =  "	      "
		character (len=8) :: day_mo   =  "	      "       
		character (len=8) :: yrc      =  "	      "       
		character (len=8) :: idmd	  =  "	      "     
		character (len=16) :: dmd_typ  =  "	               "
		character (len=16) :: dmd_num  =  "                "      
		character (len=16) :: rcv_typ  =  "	               "
		character (len=16) :: rcv_num  =  "                " 
        character (len=12) :: src1_obj =  "            "
		character (len=12) :: src1_typ =  "	           "       
		character (len=8) ::  src1_num =  "        "     
        character (len=15) :: dmd1 =      "m^3            "            !! ha-m    |demand - muni or irrigation
        character (len=15) :: s1out =	  "m^3            "            !! ha-m    |withdrawal from source 1       
        character (len=9) ::  s1un =      "m^3      "                  !! ha-m    |unmet from source 1 
		character (len=15) :: src2_typ =  "               "        
		character (len=15) :: src2_num =  "               "        
        character (len=15) :: dmd2 =      "m^3            "        !! ha-m    |demand - muni or irrigation
        character (len=15) :: s2out =	  "m^3            "        !! ha-m    |withdrawal from source 2       
        character (len=10) :: s2un =      "m^3            "        !! ha-m    |unmet from source 2        
		character (len=15) :: src3_typ =  "               "        
		character (len=15) :: src3_num =  "               "        
        character (len=15) :: dmd3 =      "m^3            "        !! ha-m    |demand - muni or irrigation
        character (len=15) :: s3out =	  "m^3            "        !! ha-m    |withdrawal from source 3       
        character (len=10) :: s3un =      "m^3            "        !! ha-m    |unmet from source 3   

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