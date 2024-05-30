      module ch_cs_module !rtb cs
      
      use constituent_mass_module, only : cs_db
      
      implicit none
                   
      type ch_cs_balance
        real :: tot_in = 0.             ! kg        !total constituent entering the channel
        real :: gw_in = 0.              ! kg        !total constituent entering the channel from groundwater
        real :: tot_out = 0.            ! kg        !total constituent leaving the channel
        real :: seep = 0.               ! kg        !constituent mass leaving the channel via seepage
        real :: irr = 0.                ! kg        !constituent mass leaving the channel via irrigation
        real :: div = 0.                ! kg        !constituent mass added to or removed from the channel via diversion
        real :: water = 0.              ! kg        !total constituent in water at end of day
        real :: conc = 0.               ! mg/L      !constituent concentration in channel water at end of day
      end type ch_cs_balance
      
      type ch_cs_output
        type (ch_cs_balance), dimension (:), allocatable :: cs         !cs hydrographs
      end type ch_cs_output
      type (ch_cs_balance) :: ch_csbz
           
      type (ch_cs_output), dimension(:), allocatable, save :: chcs_d
      type (ch_cs_output), dimension(:), allocatable, save :: chcs_m
      type (ch_cs_output), dimension(:), allocatable, save :: chcs_y
      type (ch_cs_output), dimension(:), allocatable, save :: chcs_a
                 
      type ch_cs_header
          character (len=6) :: day =        "  jday"
          character (len=6) :: mo =         "   mon"
          character (len=6) :: day_mo =     "   day"
          character (len=6) :: yrc =        "    yr"
          character (len=8) :: isd =        "   unit "
          character (len=12) :: id =        " gis_id "           
          character(len=18) :: seo4in =     "seo4_in "
          character(len=18) :: seo3in =     "seo3_in  "
          character(len=18) :: bornin =     "born_in  "
          character(len=18) :: seo4gw =     "seo4_gw "
          character(len=18) :: seo3gw =     "seo3_gw  "
          character(len=18) :: borngw =     "born_gw  "
          character(len=18) :: seo4out =    "seo4_out "
          character(len=18) :: seo3out =    "seo3_out  "
          character(len=18) :: bornout =    "born_out  "
          character(len=18) :: seo4seep =   "seo4_seep "
          character(len=18) :: seo3seep =   "seo3_seep "
          character(len=18) :: bornseep =   "born_seep "
          character(len=18) :: seo4irr =    "seo4_irr "
          character(len=18) :: seo3irr =    "seo3_irr  "
          character(len=18) :: bornirr =    "born_irr  "
          character(len=18) :: seo4div =    "seo4_div "
          character(len=18) :: seo3div =    "seo3_div  "
          character(len=18) :: borndiv =    "born_div  "
          character(len=18) :: seo4 =       "seo4_mass "
          character(len=18) :: seo3 =       "seo3_mass  "
          character(len=18) :: born =       "born_mass  "
          character(len=18) :: seo4c =      "seo4_conc"
          character(len=18) :: seo3c =      "seo3_conc"
          character(len=18) :: bornc =      "born_conc"
      end type ch_cs_header
      type (ch_cs_header) :: chcs_hdr
      

      end module ch_cs_module