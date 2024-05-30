      module channel_module
    
      implicit none
    
      integer :: jhyd    !units         |description    
      integer :: jsed    !units         |description 
      integer :: jnut    !units         |description
      real :: rttime     !hr            |reach travel time
      real :: ben_area   !m2            |benthic area (bottom sediments)
      real :: rchdep     !m             |depth of flow on day
      real :: rtevp      !m^3 H2O       |evaporation from reach on day
      real :: rttlc      !m^3 H2O       |transmission losses from reach on day
      real :: pet_ch     ! mm           |potential evaporation from reach on day
      real, dimension (:), allocatable :: hrtwtr     !m^3 H2O       |water leaving reach
      real, dimension (:), allocatable :: hharea     !m^2           |cross-sectional area of flow
      real, dimension (:), allocatable :: hdepth     !m             |depth of flow
      real, dimension (:), allocatable :: rhy        !m H2O         |main channel hydraulic radius
      real, dimension (:), allocatable :: hsdti      !m^3/s         |flow rate in reach for hour
      real, dimension (:), allocatable :: hhtime     !hr            |flow travel time for hour
      real, dimension (:), allocatable :: hrttlc     !m^3 H2O       |transmission losses from reach during time step
      real, dimension (:), allocatable :: hrtevp     !m^3 H2O       |evaporation from reach during time step
      real, dimension (:), allocatable :: hhstor     !m^3 H2O       |water stored in reach at end of hour
      real, dimension (:), allocatable :: hrchwtr    !m^3 H2O       |water stored at beginning of day
      real, dimension (:), allocatable :: halgae     !mg alg/L      |algal biomass concentration in reach
      real, dimension (:), allocatable :: hbactlp    !# cfu/100mL   |less persistent bacteria in reach/outflow during hour
      real, dimension (:), allocatable :: hbactp     !# cfu/100mL   |persistent bacteria in reach/outflow during hour
      real, dimension (:), allocatable :: hbod       !mg O2/L       |carbonaceous biochemical oxygen demand inreach at end of hour
      real, dimension (:), allocatable :: hchla      !mg chl-a/L    |chlorophyll-a concentration in reach at end of hour
      real, dimension (:), allocatable :: hdisox     !mg O2/L       |dissolved oxygen concentration in reach at end of hour
      real, dimension (:), allocatable :: hnh4       !mg N/L        |ammonia concentration in reach at end of hour
      real, dimension (:), allocatable :: hno2       !mg N/L        |nitrite concentration in reach at end of hour
      real, dimension (:), allocatable :: hno3       !mg N/L        |nitrate concentration in reach at end of hour 
      real, dimension (:), allocatable :: horgn      !mg N/L        |organic nitrogen concentration in reach at end of hour
      real, dimension (:), allocatable :: horgp      !mg P/L        |organic phosphorus concentration in reach at end of hour
      real, dimension (:), allocatable :: hsedst     !metric tons   |amount of sediment stored in reach at the end of hour    
      real, dimension (:), allocatable :: hsedyld    !metric tons   |sediment transported out of reach during hour
      real, dimension (:), allocatable :: hsolp      !mg P/L        |dissolved phosphorus concentration in reach at end of hour
      real, dimension (:), allocatable :: hsolpst    !mg pst/m^3    |soluble pesticide concentration in outflow on day
      real, dimension (:), allocatable :: hsorpst    !mg pst/m^3    |sorbed pesticide concentration in outflow on day
      real, dimension (:), allocatable :: rchsep     !
      
      real :: peakr, rcharea, sdti
      real :: bnkrte              !
      real :: degrte              !
      real :: sedrch              !metric tons       |sediment transported out of reach on day
      real :: rch_san             !
      real :: rch_sil             !
      real :: rch_cla             !
      real :: rch_sag
      real :: rtwtr_d             !m^3 H2O           |water leaving reach during day
      real :: rt_delt             ! calculation time step in days
      real :: rch_lag             !
      real :: rch_gra             !
      real :: rtwtr               !m^3 H2O           |water leaving reach on day
      real :: wtrin               !m^3               |water entering reach during day
      integer:: sed_ch 
      
      
      type channel
          real :: algae = 0.     ! mg alg/L      |algal biomass concentration in reach
          real :: ammonian = 0.  ! mg N/L        |ammonia concentration in reach
          real :: bankst = 0.    ! m^3 H2O       |bank storage 
          real :: li = 0.        ! km            |initial length of main channel
          real :: orgn = 0.      !               |organic nitrogen contribution from channel erosion 
          real :: orgp = 0.      !               |organic phosphorus contribution from channel erosion 
          real :: si = 0.        !(m/n)          |slope of main channel
          real :: wi = 0.        !(m)            |width of main channel at top of bank
          real :: di = 0.        !(m)            |depth of main channel from top of bank to bottom
          real :: chlora = 0.    ! mg chl-a/L    |chlorophyll-a concentration in reach
          real :: pst_conc =0.   ! mg/(m**3)     |initial pesticide concentration in reach
          real :: dep_chan =0.   ! m             |average daily water depth in channel
          real :: disolvp = 0.   ! mg P/L        |dissolved P concentration in reach
          real :: drift = 0.     ! kg            |amount of pesticide drifting onto main channel in subbasin
          real :: flwin = 0.     ! m^3 H2O       |flow into reach on previous day
          real :: flwout = 0.    ! m^3 H2O       |flow out of reach on previous day
          real :: nitraten = 0.  ! mg N/L        |nitrate concentration in reach
          real :: nitriten = 0.  ! mg N/L        |nitrite concentration in reach
          real :: organicn = 0.  ! mg N/L        |organic nitrogen concentration in reach
          real :: organicp = 0.  ! mg P/L        |organic phosphorus concentration in reach
          real :: rch_bactlp= 0. ! # cfu/100ml   |less persistent bacteria stored in reach
          real :: rch_bactp = 0. ! # cfu/100ml   |persistent bacteria stored in reach
          real :: rch_cbod = 0.  ! mg O2/L       |carbonaceous biochemical oxygen demand in reach 
          real :: rch_dox = 0.   ! mg O2/L       |dissolved oxygen concentration in reach
          real :: rchstor = 0.   ! m^3 H2O       |water stored in reach
          real :: sedst = 0.     ! metric tons   |amount of sediment stored in reach
          real :: vel_chan = 0.  ! m/s           |average flow velocity in channel
          real :: bed_san = 0.
          real :: bed_sil = 0.
          real :: bed_cla = 0.
          real :: bed_gra = 0.
          real :: bnk_san = 0.
          real :: bnk_sil = 0.
          real :: bnk_cla = 0.
          real :: bnk_gra = 0.
          real :: depfp = 0.
          real :: depprfp = 0.
          real :: depsilfp = 0.
          real :: depclafp = 0.
          real :: depch = 0.
          real :: depprch = 0.
          real :: depsanch = 0.
          real :: depsilch = 0.
          real :: depclach = 0.
          real :: depsagch= 0.
          real :: deplagch = 0.
          real :: depgrach = 0.
          real :: sanst = 0.
          real :: silst = 0.
          real :: clast = 0.
          real :: sagst = 0.
          real :: lagst = 0.
          real :: grast = 0.
          real :: wattemp = 0.         
          real :: bactp = 0.
          real :: chfloodvol = 0.
          real :: bactlp = 0.
      end type channel
      type (channel), dimension(:), allocatable :: ch 

      type ch_output
          real :: flo_in = 0.                  ! (ha-m)     |streamflow into reach during time step 
          real :: flo_out = 0.                 ! (ha-m)     |streamflow out of reach during time step
          real :: evap = 0.                    ! (m^3/s)    |daily rate of water loss from reach by evaporation
          real :: tloss = 0.                   ! (m^3/s)    |rate of water loss from reach by transmission through the streambed   
          real :: sed_in = 0.                  ! (tons)     |sediment transported with water into reach 
          real :: sed_out = 0.                 ! (tons)     |sediment transported with water out of reach
          real :: sed_conc = 0.                ! (mg/L)     |concentration of sediment in reach
          real :: orgn_in = 0.                 ! (kg N)     |organic nitrogen transported with water into reach
          real :: orgn_out = 0.                ! (kg N)     |organic nitrogen transported with water out of reach
          real :: orgp_in = 0.                 ! (kg P)     |organic phosphorus transported with water into reach
          real :: orgp_out = 0.                ! (kg P)     |organic phosphorus transported with water out of reach
          real :: no3_in = 0.                  ! (kg N)     |nitrate transported with water into reach   
          real :: no3_out = 0.                 ! (kg N)     |nitrate transported with water out of reach
          real :: nh4_in = 0.                  ! (kg)       |ammonium transported with water into reach
          real :: nh4_out = 0.                 ! (kg)       |ammonium transported with water out of reach
          real :: no2_in = 0.                  ! (kg)       |nitrite transported with water into reach
          real :: no2_out = 0.                 ! (kg)       |nitrite transported with water out of reach
          real :: solp_in = 0.                 ! (kg P)     |soluble pesticide transported with water into reach
          real :: solp_out = 0.                ! (kg P)     |soluble pesticide transported with water out of reach
          real :: chla_in = 0.                 ! (kg)       |amount of chlorophyll a transported into reach      
          real :: chla_out = 0.                ! (kg)       |amount of chlorophyll a transported out of reach      
          real :: cbod_in = 0.                 ! (kg)       |carbonaceous biochemical oxygen demand of material transported into reach
          real :: cbod_out = 0.                ! (kg)       |carbonaceous biochemical oxygen demand of material transported out of reach
          real :: dis_in = 0.                  ! (kg)       |amount of dissolved oxygen transported into reach
          real :: dis_out = 0.                 ! (kg)       |amount of dissolved oxygen transported out of reach
          real :: solpst_in = 0.               ! (mg pst)   |soluble pesticide transported with water into reach	
          real :: solpst_out = 0.              ! (mg pst)   |soluble pesticide transported with water out of reach
          real :: sorbpst_in = 0.              ! (mg pst)   |pesticide sorbed to sediment transported with water into reach
          real :: sorbpst_out = 0.             ! (mg pst)   |pesticide sorbed to sediment transported with water out of reach
          real :: react = 0.                   ! (mg pst)   |loss of pesticide from water from reaction 
          real :: volat = 0.                   ! (mg)       |loss of pesticide from water by volatilization 
          real :: setlpst = 0.                 ! (mg pst)   |transfer of pesticide from water to river bed sediment by settling
          real :: resuspst = 0.                ! (mg)       |transfer of pesticide from river bed sediment to water by resuspension
          real :: difus = 0.                   ! mg         |transfer of pesticide from water to river bed sediment by diffusion                               
          real :: reactb = 0.                  ! (mg)       |loss of pesticide from river bed sediment by reaction
          real :: bury = 0.                    ! (mg)       |loss of pesticide from river bed sediment by burial
          real :: sedpest = 0.                 ! mg         |pesticide in river bed sediment 
          real :: bacp = 0.                    ! # cfu/100mL  |number of persistent bacteria transported out of reach
          real :: baclp = 0.                   ! # cfu/100mL  |number of less persistent bacteria transported out of reach
          real :: met1 = 0.                    ! kg         |conservative metal #1 transported out of reach  
          real :: met2 = 0.                    ! kg         |conservative metal #2 transported out of reach  
          real :: met3 = 0.                    ! kg         |conservative metal #3 transported out of reach  
          real :: sand_in = 0.                 ! tons       |sand in 
          real :: sand_out = 0.                ! tons       |sand out
          real :: silt_in = 0.                 ! tons       |silt_in
          real :: silt_out = 0.                ! tons       |silt_out
          real :: clay_in = 0.                 ! tons       |clay_in
          real :: clay_out = 0.                ! tons       |clay_out
          real :: smag_in = 0.                 ! tons       |small aggregates transported into reach  
          real :: smag_out = 0.                ! tons       |small aggregates transported out of reach
          real :: lag_in = 0.                  ! tons       |large aggregates transported into reachlg ag in
          real :: lag_out = 0.                 ! tons       |large aggregates transported out of reach
          real :: grvl_in = 0.                 ! tons       |gravel in
          real :: grvl_out = 0.                ! tons       |gravel out           
          real :: bnk_ero = 0.                 ! tons       |bank erosion
          real :: ch_deg = 0.                  ! tons       |channel degradation
          real :: ch_dep = 0.                  ! tons       |channel deposition
          real :: fp_dep = 0.                  ! tons       |flood deposition
          real :: tot_ssed = 0.                ! mg/L       |total suspended sediments
      end type ch_output
      
      type regional_output_channel
        type (ch_output), dimension (:), allocatable :: ord
      end type regional_output_channel
      type (regional_output_channel), dimension (:), allocatable :: rch_d
      type (regional_output_channel), dimension (:), allocatable :: rch_m
      type (regional_output_channel), dimension (:), allocatable :: rch_y
      type (regional_output_channel), dimension (:), allocatable :: rch_a
      
      type (ch_output), dimension(:), allocatable, save :: ch_d
      type (ch_output), dimension(:), allocatable, save :: ch_m
      type (ch_output), dimension(:), allocatable, save :: ch_y
      type (ch_output), dimension(:), allocatable, save :: ch_a
      type (ch_output) :: bch_d
      type (ch_output) :: bch_m
      type (ch_output) :: bch_y
      type (ch_output) :: bch_a
      type (ch_output) :: chz
      
      type ch_header
          character (len=6) :: day =          " jday"
          character (len=6) :: mo =           "   mon"
          character (len=6) :: day_mo =       "   day"                                          
          character (len=5) :: yrc =          "   yr"
          character (len=9) :: isd =          "    unit "
          character (len=8) :: id =           " gis_id "           
          character (len=16) :: name =        " name              "           
          character(len=16) :: flo_in =       "      flo_in    "        ! (ha-m)
          character(len=15) :: flo_out =      "    flo_out    "         ! (ha-m)
          character(len=15) :: evap =         "        evap     "       ! (ha-m)
          character(len=15) :: tloss =        "     tloss     "         ! (ha-m)
          character(len=15) :: sed_in =       "      sed_in    "        ! (tons)
          character(len=15) :: sed_out=       "     sed_out    "        ! (tons)
          character(len=15) :: sed_conc =     "    sed_conc    "        ! (mg/L)
          character(len=15) :: orgn_in =      "    orgn_in   "          ! (kg N)
          character(len=15) :: orgn_out =     "    orgn_out    "        ! (kg N)
          character(len=15) :: orgp_in =      "      orgp_in      "     ! (kg P)
          character(len=15) :: orgp_out =     "      orgp_out  "        ! (kg P)
          character(len=15) :: no3_in =       "       no3_in   "        ! (kg N)
          character(len=15) :: no3_out =      "      no3_out   "        ! (kg N)
          character(len=15) :: nh4_in =       "        nh4_in  "        ! (kg)
          character(len=15) :: nh4_out=       "       nh4_out  "        ! (kg)
          character(len=15) :: no2_in =       "        no2_in  "        ! (kg)
          character(len=15) :: no2_out =      "       no2_out  "        ! (kg)
          character(len=15) :: solp_in =      "      solp_in   "        ! (kg P)
          character(len=15) :: solp_out =     "     solp_out   "        ! (kg P)
          character(len=15) :: chla_in =      "       chla_in  "        ! (kg)
          character(len=15) :: chla_out =     "      chla_out  "        ! (kg)
          character(len=15) :: cbod_in =      "       cbod_in  "        ! (kg)
          character(len=15) :: cbod_out =     "      cbod_out  "        ! (kg)
          character(len=15) :: dis_in =       "        dis_in  "        ! (kg)
          character(len=15) :: dis_out =      "       dis_out  "        ! (kg)
          character(len=15) :: solpst_in =    "   solpst_in    "        ! (mg pst)
          character(len=15) :: solpst_out =   "   solpst_out   "        ! (mg pst)
          character(len=15) :: sorbpst_in =   "  sorbpst_in    "        ! (mg pst)
          character(len=15) :: sorbpst_out=   "  sorbpst_out   "        ! (mg pst)
          character(len=15) :: react =        "     react      "        ! (mg pst)
          character(len=15) :: volat =        "        volat   "        ! (mg)
          character(len=15) :: setlpst =      "   setlpst      "        ! (mg pst)
          character(len=15) :: resuspst =     "     resuspst   "        ! (mg)
          character(len=15) :: difus =        "     difus      "        ! (mg pst)              
          character(len=15) :: reactb =       "    reactb      "        ! pst/sed (mg)
          character(len=15) :: bury =         "     bury       "        ! pst bury (mg)
          character(len=15) :: sedpest =      "      sedpest   "        ! pst in rivbed sed mg
          character(len=15) :: bacp =         "           bacp"        ! persistent bact out
          character(len=15) :: baclp =        "          baclp"        ! lpersistent bact out
          character(len=15) :: met1 =         "           met1"        ! cmetal #1  
          character(len=15) :: met2 =         "           met2"        ! cmetal #2
          character(len=15) :: met3 =         "           met3"        ! cmetal #3
          character(len=15) :: sand_in =      "        sand_in"        ! sand in 
          character(len=15) :: sand_out =     "       sand_out"        ! sand out
          character(len=15) :: silt_in =      "        silt_in"        ! silt_in
          character(len=15) :: silt_out =     "       silt_out"        ! silt_out
          character(len=15) :: clay_in =      "        clay_in"        ! clay_in
          character(len=15) :: clay_out =     "       clay_out"        ! clay_out
          character(len=15) :: smag_in =      "        smag_in"        ! sm ag in  
          character(len=15) :: smag_out =     "       smag_out"        ! sm ag out
          character(len=15) :: lag_in =       "         lag_in"        ! lg ag in
          character(len=15) :: lag_out =      "        lag_out"        ! lg ag out
          character(len=15) :: grvl_in =      "        grvl_in"        ! gravel in
          character(len=15) :: grvl_out =     "       grvl_out"        ! gravel out
          character(len=15) :: bnk_ero =      "        bnk_ero"        ! bank erosion
          character(len=15) :: ch_deg =       "         ch_deg"        ! channel degradation
          character(len=15) :: ch_dep =       "         ch_dep"        ! channel deposition
          character(len=15) :: fp_dep =       "         fp_dep"        ! flood deposition         
          character(len=15) :: tot_ssed =     "       tot_ssed"        ! total suspended sediments       
      end type ch_header
      type (ch_header) :: ch_hdr
      
      type ch_header_units
          character (len=6) :: day         =   "     "  
          character (len=6) :: mo          =   "     "
          character (len=6) :: day_mo      =   "      "                                          
          character (len=5) :: yrc         =   "     "
          character (len=9) :: isd         =   "         "
          character (len=8) :: id          =   "        "           
          character (len=16) :: name       =   "               "           
          character(len=16) :: flo_in      =   "        ha-m"            ! (ha-m)
          character(len=15) :: flo_out     =   "       ha-m"             ! (ha-m)
          character(len=15) :: evap        =   "        ha-m"            ! (ha-m)
          character(len=15) :: tloss       =   "      ha-m"              ! (ha-m)
          character(len=15) :: sed_in      =   "        tons"            ! (tons)
          character(len=15) :: sed_out     =   "        tons"            ! (tons)
          character(len=15) :: sed_conc    =   "        mg/L"            ! (mg/L)
          character(len=15) :: orgn_in     =   "        kgN"             ! (kg N)
          character(len=15) :: orgn_out    =   "         kgN"            ! (kg N)
          character(len=15) :: orgp_in     =   "          kgP"           ! (kg P)
          character(len=15) :: orgp_out    =   "           kgP"          ! (kg P)
          character(len=15) :: no3_in      =   "          kgN"           ! (kg N)
          character(len=15) :: no3_out     =   "          kgN"           ! (kg N)
          character(len=15) :: nh4_in      =   "            kg"          ! (kg)
          character(len=15) :: nh4_out     =   "            kg"          ! (kg)
          character(len=15) :: no2_in      =   "            kg"          ! (kg)
          character(len=15) :: no2_out     =   "            kg"          ! (kg)
          character(len=15) :: solp_in     =   "          kgP"           ! (kg P)
          character(len=15) :: solp_out    =   "          kgP"           ! (kg P)
          character(len=15) :: chla_in     =   "            kg"          ! (kg)
          character(len=15) :: chla_out    =   "            kg"          ! (kg)
          character(len=15) :: cbod_in     =   "            kg"           ! (kg)
          character(len=15) :: cbod_out    =   "            kg"           ! (kg)
          character(len=15) :: dis_in      =   "            kg"           ! (kg)
          character(len=15) :: dis_out     =   "            kg"           ! (kg)
          character(len=15) :: solpst_in   =   "      mg_pst"           ! (mg pst)
          character(len=15) :: solpst_out  =   "       mg_pst"           ! (mg pst)
          character(len=15) :: sorbpst_in  =   "      mg_pst"           ! (mg pst)
          character(len=15) :: sorbpst_out =   "       mg_pst"           ! (mg pst)
          character(len=15) :: react       =   "    mg_pst"           ! (mg pst)
          character(len=15) :: volat       =   "           mg"           ! (mg)
          character(len=15) :: setlpst     =   "    mg_pst"           ! (mg pst)
          character(len=15) :: resuspst    =   "           mg"           ! (mg)
          character(len=15) :: difus       =   "    mg_pst"           ! (mg pst)              
          character(len=15) :: reactb      =   "        mg"           ! pst/sed (mg)
          character(len=15) :: bury        =   "       mg"           ! pst bury (mg)
          character(len=15) :: sedpest     =   "           mg"           ! pst in rivbed sed mg
          character(len=15) :: bacp        =   "            ----"        ! persistent bact out
          character(len=15) :: baclp       =   "            ----"        ! lpersistent bact out
          character(len=15) :: met1        =   "            ----"        ! cmetal #1  
          character(len=15) :: met2        =   "            ----"        ! cmetal #2
          character(len=15) :: met3        =   "            ----"        ! cmetal #3
          character(len=15) :: sand_in     =   "            ----"        ! sand in 
          character(len=15) :: sand_out    =   "            ----"        ! sand out
          character(len=15) :: silt_in     =   "            ----"        ! silt_in
          character(len=15) :: silt_out    =   "            ----"        ! silt_out
          character(len=15) :: clay_in     =   "            ----"        ! clay_in
          character(len=15) :: clay_out    =   "            ----"        ! clay_out
          character(len=15) :: smag_in     =   "            ----"        ! sm ag in  
          character(len=15) :: smag_out    =   "            ----"        ! sm ag out
          character(len=15) :: lag_in      =   "            ----"        ! lg ag in
          character(len=15) :: lag_out     =   "            ----"        ! lg ag out
          character(len=15) :: grvl_in     =   "            ----"        ! gravel in
          character(len=15) :: grvl_out    =   "            ----"        ! gravel out
          character(len=15) :: bnk_ero     =   "            ----"        ! bank erosion
          character(len=15) :: ch_deg      =   "            ----"        ! channel degradation
          character(len=15) :: ch_dep      =   "            ----"        ! channel deposition
          character(len=15) :: fp_dep      =   "            ----"        ! flood deposition         
          character(len=15) :: tot_ssed    =   "            ----"        ! total suspended sediments       
      end type ch_header_units
      type (ch_header_units) :: ch_hdr_units
      
      interface operator (+)
        module procedure ch_add
      end interface
      
      interface operator (/)
        module procedure ch_div
      end interface
        
      interface operator (*)
        module procedure ch_mult
      end interface 
             
      contains
             
      function ch_add(cho1,cho2) result (cho3)
      type (ch_output),  intent (in) :: cho1
      type (ch_output),  intent (in) :: cho2
      type (ch_output) :: cho3
       cho3%flo_in = cho1%flo_in + cho2%flo_in
       cho3%flo_out = cho1%flo_out + cho2%flo_out
       cho3%evap = cho1%evap + cho2%evap   
       cho3%tloss = cho1%tloss + cho2%tloss  
       cho3%sed_in = cho1%sed_in + cho2%sed_in
       cho3%sed_out = cho1%sed_out + cho2%sed_out
       cho3%sed_conc = cho1%sed_conc + cho2%sed_conc     
       cho3%orgn_in =cho1%orgn_in + cho2%orgn_in
       cho3%orgn_out = cho1%orgn_out + cho2%orgn_out            
       cho3%orgp_in = cho1%orgp_in + cho2%orgp_in
       cho3%orgp_out = cho1%orgp_out + cho2%orgp_out
       cho3%no3_in = cho1%no3_in + cho2%no3_in
       cho3%no3_out = cho1%no3_out + cho2%no3_out
       cho3%nh4_in = cho1%nh4_in + cho2%nh4_in
       cho3%nh4_out = cho1%nh4_out + cho2%nh4_out
       cho3%no2_in = cho1%no2_in + cho2%no2_in
       cho3%no2_out = cho1%no2_out + cho2%no2_out
       cho3%solp_in = cho1%solp_in + cho2%solp_in
       cho3%solp_out = cho1%solp_out + cho2%solp_out
       cho3%chla_in = cho1%chla_in + cho2%chla_in
       cho3%chla_out = cho1%chla_out + cho2%chla_out
       cho3%cbod_in = cho1%cbod_in + cho2%cbod_in
       cho3%cbod_out = cho1%cbod_out + cho2%cbod_out
       cho3%dis_in = cho1%dis_in + cho2%dis_in
       cho3%dis_out = cho1%dis_out + cho2%dis_out
       cho3%solpst_in = cho1%solpst_in + cho2%solpst_in
       cho3%solpst_out = cho1%solpst_out + cho2%solpst_out
       cho3%sorbpst_in = cho1%sorbpst_in + cho2%sorbpst_in
       cho3%sorbpst_out = cho1%sorbpst_out + cho2%sorbpst_out         
       cho3%react = cho1%react + cho2%react
       cho3%volat = cho1%volat + cho2%volat
       cho3%setlpst = cho1%setlpst + cho2%setlpst
       cho3%resuspst = cho1%resuspst + cho2%resuspst
       cho3%difus = cho1%difus + cho2%difus
       cho3%reactb = cho1%reactb + cho2%reactb
       cho3%bury = cho1%bury + cho2%bury
       cho3%sedpest = cho1%sedpest + cho2%sedpest
       cho3%bacp = cho1%bacp + cho2%bacp
       cho3%baclp = cho1%baclp + cho2%baclp
       cho3%met1 = cho1%met1 + cho2%met1
       cho3%met2 = cho1%met2 + cho2%met2
       cho3%met3 = cho1%met3 + cho2%met3
       cho3%sand_in = cho1%sand_in + cho2%sand_in           
       cho3%sand_out = cho1%sand_out + cho2%sand_out
       cho3%silt_in = cho1%silt_in + cho2%silt_in
       cho3%silt_out = cho1%silt_out + cho2%silt_out
       cho3%clay_in = cho1%clay_in + cho2%clay_in
       cho3%clay_out = cho1%clay_out + cho2%clay_out
       cho3%smag_in = cho1%smag_in + cho2%smag_in
       cho3%smag_out = cho1%smag_out + cho2%smag_out
       cho3%lag_in = cho1%lag_in + cho2%lag_in
       cho3%lag_out = cho1%lag_out + cho2%lag_out
       cho3%grvl_in = cho1%grvl_in + cho2%grvl_in
       cho3%grvl_out = cho1%grvl_out + cho2%grvl_out
       cho3%bnk_ero = cho1%bnk_ero + cho2%bnk_ero
       cho3%ch_deg = cho1%ch_deg + cho2%ch_deg
       cho3%fp_dep = cho1%fp_dep + cho2%fp_dep
       cho3%tot_ssed = cho1%tot_ssed + cho2%tot_ssed
      end function
      
      function ch_div (ch1,const) result (ch2)
        type (ch_output), intent (in) :: ch1
        real, intent (in) :: const
        type (ch_output) :: ch2
        ch2%flo_in = ch1%flo_in / const
        ch2%flo_out = ch1%flo_out / const
        ch2%evap = ch1%evap / const  
        ch2%tloss = ch1%tloss / const  
        ch2%sed_in = ch1%sed_in / const 
        ch2%sed_out = ch1%sed_out / const           
        ch2%sed_conc = ch1%sed_conc / const            
        ch2%orgn_in = ch1%orgn_in / const  
        ch2%orgn_out = ch1%orgn_out / const              
        ch2%orgp_in = ch1%orgp_in / const  
        ch2%orgp_out = ch1%orgp_out / const            
        ch2%no3_in = ch1%no3_in / const
        ch2%no3_out = ch1%no3_out / const                    
        ch2%nh4_in = ch1%nh4_in / const  
        ch2%nh4_out = ch1%nh4_out / const               
        ch2%no2_in = ch1%no2_in / const 
        ch2%no2_out = ch1%no2_out / const                      
        ch2%solp_in = ch1%solp_in / const        
        ch2%solp_out = ch1%solp_out / const                   
        ch2%chla_in = ch1%chla_in / const   
        ch2%chla_out = ch1%chla_out / const                  
        ch2%cbod_in = ch1%cbod_in / const    
        ch2%cbod_out = ch1%cbod_out / const                   
        ch2%dis_in = ch1%dis_in / const      
        ch2%dis_out = ch1%dis_out / const                    
        ch2%solpst_in = ch1%solpst_in / const    
        ch2%solpst_out = ch1%solpst_out / const                
        ch2%sorbpst_in = ch1%sorbpst_in / const   
        ch2%sorbpst_out = ch1%sorbpst_out / const                  
        ch2%react = ch1%react / const                                
        ch2%volat = ch1%volat / const                         
        ch2%setlpst = ch1%setlpst / const                            
        ch2%resuspst = ch1%resuspst / const                          
        ch2%difus = ch1%difus / const                               
        ch2%reactb = ch1%reactb / const                               
        ch2%bury = ch1%bury / const                                   
        ch2%sedpest = ch1%sedpest / const
        ch2%bacp = ch1%bacp / const                        
        ch2%baclp = ch1%baclp / const                       
        ch2%met1 = ch1%met1 / const                         
        ch2%met2 = ch1%met2 / const                         
        ch2%met3 = ch1%met3 / const                         
        ch2%sand_in = ch1%sand_in / const          
        ch2%sand_out = ch1%sand_out / const                         
        ch2%silt_in = ch1%silt_in / const          
        ch2%silt_out = ch1%silt_out / const                       
        ch2%clay_in = ch1%clay_in / const             
        ch2%clay_out = ch1%clay_out / const                        
        ch2%smag_in = ch1%smag_in / const            
        ch2%smag_out = ch1%smag_out / const                       
        ch2%lag_in = ch1%lag_in / const          
        ch2%lag_out = ch1%lag_out / const                      
        ch2%grvl_in = ch1%grvl_in / const        
        ch2%grvl_out = ch1%grvl_out / const                      
        ch2%bnk_ero = ch1%bnk_ero / const
        ch2%ch_deg = ch1%ch_deg / const
        ch2%fp_dep = ch1%fp_dep / const
        ch2%tot_ssed = ch1%tot_ssed / const
      end function ch_div
      
      function ch_mult (const, chn1) result (chn2)
        type (ch_output), intent (in) :: chn1
        real, intent (in) :: const
        type (ch_output) :: chn2
        chn2%flo_in = const * chn1%flo_in      
        chn2%flo_out = const * chn1%flo_out
        chn2%evap = const * chn1%evap
        chn2%tloss = const * chn1%tloss
        chn2%sed_in = const * chn1%sed_in
        chn2%sed_out = const * chn1%sed_out
        chn2%sed_conc = const * chn1%sed_conc      
        chn2%orgn_in = const * chn1%orgn_in
        chn2%orgn_out = const * chn1%orgn_out
        chn2%orgp_in = const * chn1%orgp_in
        chn2%orgp_out = const * chn1%orgp_out
        chn2%no3_in = const * chn1%no3_in
        chn2%no3_out = const * chn1%no3_out    
        chn2%nh4_in = const * chn1%nh4_in
        chn2%nh4_out = const * chn1%nh4_out
        chn2%no2_in = const * chn1%no2_in
        chn2%no2_out = const * chn1%no2_out
        chn2%solp_in = const * chn1%solp_in
        chn2%solp_out = const * chn1%solp_out      
        chn2%chla_in = const * chn1%chla_in
        chn2%chla_out = const * chn1%chla_out
        chn2%cbod_in = const * chn1%cbod_in
        chn2%cbod_out = const * chn1%cbod_out
        chn2%dis_in = const * chn1%dis_in
        chn2%dis_out = const * chn1%dis_out     
        chn2%solpst_in = const * chn1%solpst_in
        chn2%solpst_out = const * chn1%solpst_out
        chn2%sorbpst_in = const * chn1%sorbpst_in
        chn2%sorbpst_out = const * chn1%sorbpst_out
        chn2%react = const * chn1%react
        chn2%bury = const * chn1%bury      
        chn2%sedpest = const * chn1%sedpest
        chn2%bacp = const * chn1%bacp
        chn2%baclp = const * chn1%baclp
        chn2%met1 = const * chn1%met1
        chn2%met2 = const * chn1%met2
        chn2%met3 = const * chn1%met3     
        chn2%sand_in = const * chn1%sand_in
        chn2%sand_out = const * chn1%sand_out
        chn2%silt_in = const * chn1%silt_in
        chn2%silt_out = const * chn1%silt_out
        chn2%clay_in = const * chn1%clay_in
        chn2%clay_out = const * chn1%clay_out      
        chn2%smag_in = const * chn1%smag_in
        chn2%smag_out = const * chn1%smag_out
        chn2%lag_in = const * chn1%lag_in
        chn2%lag_out = const * chn1%lag_out
        chn2%grvl_in = const * chn1%grvl_in
        chn2%grvl_out = const * chn1%grvl_out   
        chn2%bnk_ero = const * chn1%bnk_ero
        chn2%ch_deg = const * chn1%ch_deg
        chn2%fp_dep = const * chn1%fp_dep
        chn2%tot_ssed = const * chn1%tot_ssed     
      end function ch_mult
               
      end module channel_module