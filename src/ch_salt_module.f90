      module ch_salt_module
    
      use constituent_mass_module, only : cs_db
      
      implicit none
                   
      type ch_salt_balance
        real :: tot_in = 0.             ! kg        !total salt ion entering the channel
        real :: gw_in = 0.              ! kg        !total salt ion entering the channel from groundwater
        real :: tot_out = 0.            ! kg        !total salt ion leaving the channel
        real :: seep = 0.               ! kg        !total salt ion leaving the channel via seepage
        real :: irr = 0.                ! kg        !salt ion mass leaving the channel via irrigation
        real :: div = 0.                ! kg        !salt ion mass added to or removed from the channel via diversion
        real :: water = 0.              ! kg        !total salt ion in water at end of day
        real :: conc = 0.               ! mg/L      !salt ion concentration in channel water at end of day
      end type ch_salt_balance
      
      type ch_salt_output
        type (ch_salt_balance), dimension (:), allocatable :: salt         !salt hydrographs
      end type ch_salt_output
      type (ch_salt_balance) :: ch_saltbz
           
      type (ch_salt_output), dimension(:), allocatable, save :: chsalt_d
      type (ch_salt_output), dimension(:), allocatable, save :: chsalt_m
      type (ch_salt_output), dimension(:), allocatable, save :: chsalt_y
      type (ch_salt_output), dimension(:), allocatable, save :: chsalt_a
                 
      type ch_salt_header
          character (len=6) :: day =        "  jday"
          character (len=6) :: mo =         "   mon"
          character (len=6) :: day_mo =     "   day"
          character (len=6) :: yrc =        "    yr"
          character (len=8) :: isd =        "   unit "
          character (len=12) :: id =        " gis_id "           
          character(len=18) :: so4in =      "so4_in "
          character(len=18) :: cain =       "ca_in  "
          character(len=18) :: mgin =       "mg_in  "
          character(len=18) :: nain =       "na_in  "
          character(len=18) :: kin =        "k_in   "
          character(len=18) :: clin =       "cl_in  "
          character(len=18) :: co3in =      "co3_in "
          character(len=18) :: hco3in =     "hco3_in"
          character(len=18) :: so4gw =      "so4_gw "
          character(len=18) :: cagw =       "ca_gw  "
          character(len=18) :: mggw =       "mg_gw  "
          character(len=18) :: nagw =       "na_gw  "
          character(len=18) :: kgw =        "k_gw   "
          character(len=18) :: clgw =       "cl_gw  "
          character(len=18) :: co3gw =      "co3_gw "
          character(len=18) :: hco3gw =     "hco3_gw"
          character(len=18) :: so4out =     "so4_out "
          character(len=18) :: caout =      "ca_out  "
          character(len=18) :: mgout =      "mg_out  "
          character(len=18) :: naout =      "na_out  "
          character(len=18) :: kout =       "k_out   "
          character(len=18) :: clout =      "cl_out  "
          character(len=18) :: co3out =     "co3_out "
          character(len=18) :: hco3out =    "hco3_out"
          character(len=18) :: so4seep =    "so4_seep "
          character(len=18) :: caseep =     "ca_seep  "
          character(len=18) :: mgseep =     "mg_seep  "
          character(len=18) :: naseep =     "na_seep  "
          character(len=18) :: kseep =      "k_seep   "
          character(len=18) :: clseep =     "cl_seep  "
          character(len=18) :: co3seep =    "co3_seep "
          character(len=18) :: hco3seep =   "hco3_seep"
          character(len=18) :: so4irr =     "so4_irr "
          character(len=18) :: cairr =      "ca_irr  "
          character(len=18) :: mgirr =      "mg_irr  "
          character(len=18) :: nairr =      "na_irr  "
          character(len=18) :: kirr =       "k_irr   "
          character(len=18) :: clirr =      "cl_irr  "
          character(len=18) :: co3irr =     "co3_irr "
          character(len=18) :: hco3irr =    "hco3_irr"
          character(len=18) :: so4div =     "so4_div "
          character(len=18) :: cadiv =      "ca_div  "
          character(len=18) :: mgdiv =      "mg_div  "
          character(len=18) :: nadiv =      "na_div  "
          character(len=18) :: kdiv =       "k_div   "
          character(len=18) :: cldiv =      "cl_div  "
          character(len=18) :: co3div =     "co3_div "
          character(len=18) :: hco3div =    "hco3_div"
          character(len=18) :: so4 =        "so4_mass "
          character(len=18) :: ca =         "ca_mass  "
          character(len=18) :: mg =         "mg_mass  "
          character(len=18) :: na =         "na_mass  "
          character(len=18) :: k =          "k_mass   "
          character(len=18) :: cl =         "cl_mass  " 
          character(len=18) :: co3 =        "co3_mass "
          character(len=18) :: hco3 =       "hco3_mass"
          character(len=18) :: so4c =       "so4_conc"
          character(len=18) :: cac =        "ca_conc"
          character(len=18) :: mgc =        "mg_conc"
          character(len=18) :: nac =        "na_conc"
          character(len=18) :: kc =         "k_conc"
          character(len=18) :: clc =        "cl_conc"
          character(len=18) :: co3c =       "co3_conc"
          character(len=18) :: hco3c =      "hco3_conc"
      end type ch_salt_header
      type (ch_salt_header) :: chsalt_hdr
      

      end module ch_salt_module