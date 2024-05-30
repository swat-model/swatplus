      module manure_allocation_module
    
      use fertilizer_data_module
    
      implicit none
            
      !manure demand source and amount
      type manure_demand_amount
        integer :: mallo_obj = 0
        integer :: src_obj = 0
        real :: app_t_ha = 0.
        integer :: app_method = 0
      end type manure_demand_amount
      type (manure_demand_amount) :: manure_amtz
      
      !manure source balance - storage, produced, withdrawan of the allocation object
      type source_manure_output
        real :: stor = 0.               !current manure stored - tons
        real :: prod = 0.               !mannure produced - tons
        real :: withdr = 0.             !manure withdrawal from all demand objects - tons
      end type source_manure_output
      type (source_manure_output) :: malloz

      !manure source objects
      type manure_source_objects
        integer :: num                          !source object number
        character (len=3) :: mois_typ           !wet or dry
        character (len=25) :: manure_typ        !points to fertilizer.frt
        real :: lat                             !latitude
        real :: long                            !longitude
        real :: stor_init                       !initial storage - tons
        real :: stor_max                        !maximum storage - tons
        real, dimension (12) :: prod_mon        !average monthly manure produced - tons/month
        integer :: fertdb                       !fertilizer database number (fertilizer.frt)
        type (source_manure_output) :: bal_d    !daily amount - storage, produced, withdrawn from the source - tons
        type (source_manure_output) :: bal_m    !monthly amount - storage, produced, withdrawn from the source - tons
        type (source_manure_output) :: bal_y    !yearly amount - storage, produced, withdrawn from the source - tons
        type (source_manure_output) :: bal_a    !ave annual amount - storage, produced, withdrawn from the source - tons
      end type manure_source_objects

      !manure demand objects
      type manure_demand_objects
        integer :: num                          !demand object number
        character (len=10) :: ob_typ            !hru (for application) or muni (treatmentb) or divert (interbasin diversion)
        integer :: ob_num                       !number of the object type
        character (len=25) :: dtbl              !decision table name for manure/fert application
        character (len=2) :: right              !manure right (sr -senior or jr - junior right
        integer :: dtbl_num
        type (manure_demand_amount) :: manure_amt
        real, dimension(:), allocatable :: withdr       !daily amount withdrawn from each source
        real, dimension(:), allocatable :: withdr_m     !amount withdrawn from each source
        real, dimension(:), allocatable :: withdr_y     !amount withdrawn from each source
        real, dimension(:), allocatable :: withdr_a     !amount withdrawn from each source
      end type manure_demand_objects

      !manure allocation object
      type manure_allocation
        character (len=25) :: name              !name of the water allocation object
        character (len=25) :: rule_typ          !rule type to allocate water
        integer :: src_obs                      !number of source objects
        integer :: dmd_obs                      !number of demand objects
        type (source_manure_output) :: tot            !total demand, withdrawal and unmet for entire allocation object
        type (manure_source_objects), dimension(:), allocatable :: src        !dimension by source objects
        type (manure_demand_objects), dimension(:), allocatable :: dmd        !dimension by demand objects
      end type manure_allocation
      type (manure_allocation), dimension(:), allocatable :: mallo            !dimension by water allocation objects

      type mallo_header            
		character(len=6) :: day      =	 "  jday"       
		character(len=6) :: mo       =	 "	 mon"
		character(len=6) :: day_mo   =	 " day "
		character(len=6) :: yrc      =	 " yr  "        
		character(len=8) :: idmd	 =	 " unit   "      
		character(len=16) :: dmd_typ  =  "dmd_typ         "
		character(len=16) :: dmd_num =	 "    dmd_num     "        
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
        end type mallo_header
      type (mallo_header) :: mallo_hdr

      type mallo_header_units         
		character (len=8) :: day	  =  "	      "
		character (len=8) :: mo       =  "	      "
		character (len=8) :: day_mo   =  "	      "       
		character (len=8) :: yrc      =  "	      "       
		character (len=8) :: idmd	  =  "	      "     
		character (len=16) :: dmd_typ  =  "	               "
		character (len=16) :: dmd_num  =  "                " 
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
        end type mallo_header_units
      type (mallo_header_units) :: mallo_hdr_units 
      
      interface operator (+)
        module procedure mallout_add
      end interface

      interface operator (/)
        module procedure mallo_div_const
      end interface   

      contains

      !! routines for hydrograph module
      function mallout_add (mallo1, mallo2) result (mallo3)
        type (source_manure_output), intent (in) :: mallo1
        type (source_manure_output), intent (in) :: mallo2
        type (source_manure_output) :: mallo3
        mallo3%stor = mallo1%stor
        mallo3%prod = mallo1%prod + mallo2%prod
        mallo3%withdr = mallo1%withdr + mallo2%withdr
      end function mallout_add

      function mallo_div_const (mallo1, const) result (mallo2)
        type (source_manure_output), intent (in) :: mallo1
        real, intent (in) :: const
        type (source_manure_output) :: mallo2
        mallo2%stor = mallo1%stor
        mallo2%prod = mallo1%prod / const
        mallo2%withdr = mallo1%withdr / const
      end function mallo_div_const

      end module manure_allocation_module