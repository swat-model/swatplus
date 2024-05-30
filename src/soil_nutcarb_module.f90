     module soil_nutcarb_module
    
     implicit none
     
     type organic_carbon_header
        character (len=12) :: day_mo    =  "        day"
        character (len=12) :: yrc       =  "         yr"        
        character (len=12) :: hru       =  "       unit"      
        character (len=18) :: str_c     =  "          str_c"
        character (len=15) :: lig_c     =  "          lig_c"
        character (len=15) :: meta_c    =  "         meta_c"        
        character (len=15) :: man_c     =  "          man_c"        
        character (len=15) :: hum_c     =  "          hum_c"
        character (len=15) :: phum_c    =  "         phum_c"       
        character (len=12) :: mb_c      =  "       mb_c "
      end type organic_carbon_header      
      type (organic_carbon_header) :: orgc_hdr
      
      type total_carbon_header 
        character (len=12) :: day    = "       jday "
        character (len=12) :: yrc    = "         yr "
        character (len=12) :: isd    = "       unit "      
        character (len=14) :: soil_org_c   =  "   soil_tot_c"
        character (len=14) :: plm_com_c    =  "  plant_tot_c"
        character (len=14) :: rsd_com_c    =  "    rsd_tot_c"
      end type total_carbon_header      
      type (total_carbon_header) :: totc_hdr
          
     type organic_carbon_units
        character (len=12) :: day    = "         -- "
        character (len=12) :: yrc    = "         -- "
        character (len=12) :: isd    = "         -- "    
        character (len=18) :: str_c     =  "          kg/ha"
        character (len=15) :: lig_c     =  "          kg/ha"
        character (len=15) :: meta_c    =  "          kg/ha"        
        character (len=15) :: man_c     =  "          kg/ha"        
        character (len=15) :: hum_c     =  "          kg/ha"
        character (len=15) :: phum_c    =  "          kg/ha"       
        character (len=12) :: mb_c      =  "          kg/ha"
      end type organic_carbon_units      
      type (organic_carbon_units) :: orgc_units
      
      type total_carbon_units 
        character (len=12) :: day    = "         -- "
        character (len=12) :: yrc    = "         -- "
        character (len=12) :: isd    = "         -- "      
        character (len=14) :: soil_org_c   =  "         kg/ha"
        character (len=14) :: plm_com_c    =  "         kg/ha"
        character (len=14) :: rsd_com_c    =  "         kg/ha"
      end type total_carbon_units      
      type (total_carbon_units) :: totc_units
      
     end module soil_nutcarb_module