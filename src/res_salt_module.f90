      module res_salt_module !rtb salt

      implicit none
              
      type res_salt_balance
        real :: inflow = 0.             ! kg        !salt entering the reservoir via streamflow
        real :: outflow = 0.            ! kg        !salt leaving the reservoir via streamflow
        real :: seep = 0.               ! kg        !salt leaving the reservoir via seepage to aquifer
        real :: fert = 0.								! kg        !salt added to reservoir (wetland) via fertilizer
        real :: irrig = 0.              ! kg        !salt removed from the reservoir via irrigation diversion
        real :: div = 0.                ! kg        !salt mass removed or added via diversion
        real :: mass = 0.               ! kg        !salt in reservoir water at end of day
        real :: conc = 0.               ! g/m3      !salt concentration in reservoir at end of day
        real :: volm = 0.               ! m3        !volume of water in the reservoir
      end type res_salt_balance
      
      type res_salt_output
        type (res_salt_balance), dimension (:), allocatable :: salt         !salt hydrographs
      end type res_salt_output
      type (res_salt_balance) :: res_saltbz
           
      !arrays for reservoir mass balance output
      type (res_salt_output), dimension(:), allocatable, save :: ressalt_d
      type (res_salt_output), dimension(:), allocatable, save :: ressalt_m
      type (res_salt_output), dimension(:), allocatable, save :: ressalt_y
      type (res_salt_output), dimension(:), allocatable, save :: ressalt_a
			
      !arrays for wetland mass balance output
      type (res_salt_output), dimension(:), allocatable, save :: wetsalt_d
      type (res_salt_output), dimension(:), allocatable, save :: wetsalt_m
      type (res_salt_output), dimension(:), allocatable, save :: wetsalt_y
      type (res_salt_output), dimension(:), allocatable, save :: wetsalt_a
      
      !reservoir constituent parameters
      type reservoir_salt_data
        character(len=25) :: name
        real, dimension (:), allocatable :: c_init    !g/m3       |initial concentration of each salt ion
      end type reservoir_salt_data
      type (reservoir_salt_data), dimension(:), allocatable :: res_salt_data
      
      !output file headers
      type res_salt_header
          character (len=6) :: day =        "  jday"
          character (len=6) :: mo =         "   mon"
          character (len=6) :: day_mo =     "   day"
          character (len=6) :: yrc =        "    yr"
          character (len=8) :: isd =        "   unit "
          character (len=12) :: id =         " gis_id "           
          character(len=15) :: so4in =      "so4_in "
          character(len=15) :: cain =       "ca_in  "
          character(len=15) :: mgin =       "mg_in  "
          character(len=15) :: nain =       "na_in  "
          character(len=15) :: kin =        "k_in   "
          character(len=15) :: clin =       "cl_in  "
          character(len=15) :: co3in =      "co3_in "
          character(len=15) :: hco3in =     "hco3_in"
          character(len=15) :: so4out =     "so4_out "
          character(len=15) :: caout =      "ca_out  "
          character(len=15) :: mgout =      "mg_out  "
          character(len=15) :: naout =      "na_out  "
          character(len=15) :: kout =       "k_out   "
          character(len=15) :: clout =      "cl_out  "
          character(len=15) :: co3out =     "co3_out "
          character(len=15) :: hco3out =    "hco3_out"
          character(len=15) :: so4seep =    "so4_seep "
          character(len=15) :: caseep =     "ca_seep  "
          character(len=15) :: mgseep =     "mg_seep  "
          character(len=15) :: naseep =     "na_seep  "
          character(len=15) :: kseep =      "k_seep   "
          character(len=15) :: clseep =     "cl_seep  "
          character(len=15) :: co3seep =    "co3_seep "
          character(len=15) :: hco3seep =   "hco3_seep"
          character(len=15) :: so4fert =    "so4_fert "
          character(len=15) :: cafert =     "ca_fert  "
          character(len=15) :: mgfert =     "mg_fert  "
          character(len=15) :: nafert =     "na_fert  "
          character(len=15) :: kfert =      "k_fert   "
          character(len=15) :: clfert =     "cl_fert  "
          character(len=15) :: co3fert =    "co3_fert "
          character(len=15) :: hco3fert =   "hco3_fert"
          character(len=15) :: so4irr =     "so4_irrg "
          character(len=15) :: cairr =      "ca_irrg  "
          character(len=15) :: mgirr =      "mg_irrg  "
          character(len=15) :: nairr =      "na_irrg  "
          character(len=15) :: kirr =       "k_irrg   "
          character(len=15) :: clirr =      "cl_irrg  "
          character(len=15) :: co3irr =     "co3_irrg "
          character(len=15) :: hco3irr =    "hco3_irrg"
          character(len=15) :: so4div =     "so4_div "
          character(len=15) :: cadiv =      "ca_div  "
          character(len=15) :: mgdiv =      "mg_div  "
          character(len=15) :: nadiv =      "na_div  "
          character(len=15) :: kdiv =       "k_div   "
          character(len=15) :: cldiv =      "cl_div  "
          character(len=15) :: co3div =     "co3_div "
          character(len=15) :: hco3div =    "hco3_div"
          character(len=15) :: so4 =        "so4_mass "
          character(len=15) :: ca =         "ca_mass  "
          character(len=15) :: mg =         "mg_mass  "
          character(len=15) :: na =         "na_mass  "
          character(len=15) :: k =          "k_mass   "
          character(len=15) :: cl =         "cl_mass  " 
          character(len=15) :: co3 =        "co3_mass "
          character(len=15) :: hco3 =       "hco3_mass"
          character(len=15) :: so4c =       "so4_conc"
          character(len=15) :: cac =        "ca_conc"
          character(len=15) :: mgc =        "mg_conc"
          character(len=15) :: nac =        "na_conc"
          character(len=15) :: kc =         "k_conc"
          character(len=15) :: clc =        "cl_conc"
          character(len=15) :: co3c =       "co3_conc"
          character(len=15) :: hco3c =      "hco3_conc"
          character(len=15) :: volm =       "vol_m3"
      end type res_salt_header
      type (res_salt_header) :: ressalt_hdr

      end module res_salt_module