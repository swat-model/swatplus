      module salt_module
    
      implicit none

      !soil -------------------------------------------------------------------------------------------------------------------------------
      
      type salt_balance
        !salt ions = so4,ca,mg,na,k,cl,co3,hco3 
        real :: soil = 0.            !! |kg/ha       |total salt ion mass in the soil profile
        real :: diss = 0.            !! |kg/ha       |salt ion mass transferred from sorbed phase to dissolved phase
        real :: surq = 0.            !! |kg/ha       |salt ion mass lost in surface runoff in HRU
        real :: latq = 0.            !! |kg/ha       |salt ion mass in lateral flow in HRU
        real :: urbq = 0.            !! |kg/ha			 |salt ion mass in urban runoff
        real :: wetq = 0.            !! |kg/ha       |salt ion mass in wetland runoff
        real :: tile = 0.            !! |kg/ha       |salt ion mass in tile flow in HRU
        real :: perc = 0.            !! |kg/ha       |salt ion mass leached past bottom of soil
        real :: gwup = 0.            !! |kg/ha       |salt ion mass from groundwater (to soil profile)
        real :: wtsp = 0.            !! |kg/ha       |salt ion mass in wetland seepage (to soil profile)
        real :: irsw = 0.            !! |kg/ha       |salt ion mass applied on soil via surface water irrigation
        real :: irgw = 0.            !! |kg/ha       |salt ion mass applied on soil via groundwater irrigation
        real :: irwo = 0.            !! |kg/ha       |salt ion mass applied on soil via girrigation from without (wo) the watershed
        real :: rain = 0.            !! |kg/ha       |salt ion mass added to soil via rainfall
        real :: dryd = 0.            !! |kg/ha       |salt ion mass added to soil via dry atmospheric deposition  
        real :: road = 0.            !! |kg/ha       |salt ion mass added to soil via applied road salt
        real :: fert = 0.            !! |kg/ha       |salt ion mass added to soil via fertilizer 
        real :: amnd = 0.            !! |kg/ha       |salt ion mass added to soil via salt amendments
        real :: uptk = 0.            !! |kg/ha       |salt ion mass taken up by crop roots
        real :: conc = 0.						 !! |mg/L        |salt ion concentration in soil water (averaged over all soil layers)
      end type salt_balance
      
      type object_salt_balance
        type (salt_balance), dimension (:), allocatable :: salt
      end type object_salt_balance
      
      !soil system fluxes
      type (object_salt_balance), dimension (:), allocatable :: hsaltb_d
      type (object_salt_balance), dimension (:), allocatable :: hsaltb_m
      type (object_salt_balance), dimension (:), allocatable :: hsaltb_y
      type (object_salt_balance), dimension (:), allocatable :: hsaltb_a
      
      !routing unit fluxes (summation of all HRU inputs)
      type (object_salt_balance), dimension (:), allocatable :: ru_hru_saltb_d
      type (object_salt_balance), dimension (:), allocatable :: ru_hru_saltb_m
      type (object_salt_balance), dimension (:), allocatable :: ru_hru_saltb_y
      type (object_salt_balance), dimension (:), allocatable :: ru_hru_saltb_a
      
      !salt balance arrays
      real :: salt_basin_mo(28) = 0.
      real :: salt_basin_yr(28) = 0.
      real :: salt_basin_aa(28) = 0.
      
      !salt fertilizer
      type fert_db_salt
        character(len=16) :: fertnm = " "
        real :: so4 = 0.           !! kg so4/ha      |fertilizer load of so4 (kg/ha)
        real :: ca = 0.            !! kg ca/ha       |fertilizer load of ca (kg/ha)
        real :: mg = 0.            !! kg mg/ha       |fertilizer load of mg (kg/ha)
        real :: na = 0.            !! kg na/ha       |fertilizer load of na (kg/ha)
        real :: k = 0.             !! kg k/ha        |fertilizer load of k (kg/ha)
        real :: cl = 0.            !! kg cl/ha       |fertilizer load of cl (kg/ha)
        real :: co3 = 0.           !! kg co3/ha      |fertilizer load of co3 (kg/ha)
        real :: hco3 = 0.          !! kg hco3/ha     |fertilizer load of hco3 (kg/ha)
      end type fert_db_salt
      type (fert_db_salt), dimension(:),allocatable, save :: fert_salt
      integer :: fert_salt_flag = 0
      
      !salt uptake
      real, dimension(:,:), allocatable :: salt_uptake_kg       !specified daily salt mass taken up by crop roots (kg/ha)
      integer :: salt_uptake_on                                 !flag for simulating salt uptake
      
      !urban salt concentrations
      real, dimension(:,:), allocatable :: salt_urban_conc      !salt ion conc in suspended solid load from imp areas (mg salt / kg sed)
      
      !header for daily (basin-wide) salt balance output
      type output_saltbal_header
         character (len=8) :: yrc =        "      yr"
         character (len=8) :: mon =        "      mo"
         character (len=8) :: day =        "    jday"  
         character(len=16) :: lat =        "     lat_kg"
         character(len=16) :: gw =         "      gw_kg"
         character(len=16) :: sur =        "     sur_kg"
         character(len=16) :: urb =        "     urb_kg"
         character(len=16) :: wet =        "     wet_kg"
         character(len=16) :: tile =       "    tile_kg"
         character(len=16) :: perc =       "    perc_kg"
         character(len=16) :: gwup =       "    gwup_kg"
         character(len=16) :: wtsp =       "    wtsp_kg"
         character(len=16) :: irsw =       "    irsw_kg"
         character(len=16) :: irgw =       "    irgw_kg"
         character(len=16) :: irwo =       "    irwo_kg"
         character(len=16) :: rain =       "    rain_kg"
         character(len=16) :: dryd =       "    dryd_kg"
         character(len=16) :: road =       "    road_kg"
         character(len=16) :: fert =       "    fert_kg"
         character(len=16) :: amnd =       "    amnd_kg"
         character(len=16) :: uptk =       "    uptk_kg"
         character(len=16) :: ptso =       "    ptso_kg"
         character(len=16) :: pout =       "   ptout_kg"
         character(len=16) :: rchg =       "    rchg_kg"
         character(len=16) :: seep =       "    seep_kg"
         character(len=16) :: dssl =       "    dssl_kg"
         character(len=16) :: dsaq =       "    dsaq_kg"
         character(len=16) :: slds =       "  soilds_kg"
         character(len=16) :: slmn =       "  soilmn_kg"
         character(len=16) :: aqds =       "   aquds_kg"
         character(len=16) :: aqmn =       "   aqumn_kg"
      end type output_saltbal_header      
      type (output_saltbal_header) :: saltb_hdr
      
      !header for daily, monthly, yearly, average annual HRU salt output
      type output_salt_hdr_hru
         character (len=6) :: day =        "  jday"
         character (len=6) :: mo =         "   mon"
         character (len=6) :: day_mo =     "   day"
         character (len=6) :: yrc =        "    yr"
         character (len=8) :: isd =        "   unit "
         character (len=12) :: id =        " gis_id "
         !total salt in soil profile
         character(len=15) :: so4sl =      "soil_so4"
         character(len=15) :: casl =       "soil_ca"
         character(len=15) :: mgsl =       "soil_mg"
         character(len=15) :: nasl =       "soil_na"
         character(len=15) :: ksl =        "soil_k"
         character(len=15) :: clsl =       "soil_cl"
         character(len=15) :: co3sl =      "soil_co3"
         character(len=15) :: hco3sl =     "soil_hco3"
         !surface runoff
         character(len=15) :: so4sq =      "surq_so4"
         character(len=15) :: casq =       "surq_ca"
         character(len=15) :: mgsq =       "surq_mg"
         character(len=15) :: nasq =       "surq_na"
         character(len=15) :: ksq =        "surq_k"
         character(len=15) :: clsq =       "surq_cl"
         character(len=15) :: co3sq =      "surq_co3"
         character(len=15) :: hco3sq =     "surq_hco3"
         !lateral flow
         character(len=15) :: so4lq =      "latq_so4"
         character(len=15) :: calq =       "latq_ca"
         character(len=15) :: mglq =       "latq_mg"
         character(len=15) :: nalq =       "latq_na"
         character(len=15) :: klq =        "latq_k"
         character(len=15) :: cllq =       "latq_cl"
         character(len=15) :: co3lq =      "latq_co3"
         character(len=15) :: hco3lq =     "latq_hco3"
         !urban runoff
         character(len=15) :: so4uq =      "urbq_so4"
         character(len=15) :: cauq =       "urbq_ca"
         character(len=15) :: mguq =       "urbq_mg"
         character(len=15) :: nauq =       "urbq_na"
         character(len=15) :: kuq =        "urbq_k"
         character(len=15) :: cluq =       "urbq_cl"
         character(len=15) :: co3uq =      "urbq_co3"
         character(len=15) :: hco3uq =     "urbq_hco3"
         !wetland runoff
         character(len=15) :: so4wt =      "wetq_so4"
         character(len=15) :: cawt =       "wetq_ca"
         character(len=15) :: mgwt =       "wetq_mg"
         character(len=15) :: nawt =       "wetq_na"
         character(len=15) :: kwt =        "wetq_k"
         character(len=15) :: clwt =       "wetq_cl"
         character(len=15) :: co3wt =      "wetq_co3"
         character(len=15) :: hco3wt =     "wetq_hco3"
         !tile flow
         character(len=15) :: so4tq =      "tile_so4"
         character(len=15) :: catq =       "tile_ca"
         character(len=15) :: mgtq =       "tile_mg"
         character(len=15) :: natq =       "tile_na"
         character(len=15) :: ktq =        "tile_k"
         character(len=15) :: cltq =       "tile_cl"
         character(len=15) :: co3tq =      "tile_co3"
         character(len=15) :: hco3tq =     "tile_hco3"
         !percolation
         character(len=15) :: so4pc =      "perc_so4"
         character(len=15) :: capc =       "perc_ca"
         character(len=15) :: mgpc =       "perc_mg"
         character(len=15) :: napc =       "perc_na"
         character(len=15) :: kpc =        "perc_k"
         character(len=15) :: clpc =       "perc_cl"
         character(len=15) :: co3pc =      "perc_co3"
         character(len=15) :: hco3pc =     "perc_hco3"
         !groundwater transfer
         character(len=15) :: so4gt =      "gwup_so4"
         character(len=15) :: cagt =       "gwup_ca"
         character(len=15) :: mggt =       "gwup_mg"
         character(len=15) :: nagt =       "gwup_na"
         character(len=15) :: kgt =        "gwup_k"
         character(len=15) :: clgt =       "gwup_cl"
         character(len=15) :: co3gt =      "gwup_co3"
         character(len=15) :: hco3gt =     "gwup_hco3"
         !wetland seepage to soil
         character(len=15) :: so4ws =      "wtsp_so4"
         character(len=15) :: caws =       "wtsp_ca"
         character(len=15) :: mgws =       "wtsp_mg"
         character(len=15) :: naws =       "wtsp_na"
         character(len=15) :: kws =        "wtsp_k"
         character(len=15) :: clws =       "wtsp_cl"
         character(len=15) :: co3ws =      "wtsp_co3"
         character(len=15) :: hco3ws =     "wtsp_hco3"
         !irrigation (surface water)
         character(len=15) :: so4is =      "irsw_so4"
         character(len=15) :: cais =       "irsw_ca"
         character(len=15) :: mgis =       "irsw_mg"
         character(len=15) :: nais =       "irsw_na"
         character(len=15) :: kis =        "irsw_k"
         character(len=15) :: clis =       "irsw_cl"
         character(len=15) :: co3is =      "irsw_co3"
         character(len=15) :: hco3is =     "irsw_hco3"
         !irrigation (groundwater)
         character(len=15) :: so4ig =      "irgw_so4"
         character(len=15) :: caig =       "irgw_ca"
         character(len=15) :: mgig =       "irgw_mg"
         character(len=15) :: naig =       "irgw_na"
         character(len=15) :: kig =        "irgw_k"
         character(len=15) :: clig =       "irgw_cl"
         character(len=15) :: co3ig =      "irgw_co3"
         character(len=15) :: hco3ig =     "irgw_hco3"
         !irrigation (outside watershed)
         character(len=15) :: so4io =      "irwo_so4"
         character(len=15) :: caio =       "irwo_ca"
         character(len=15) :: mgio =       "irwo_mg"
         character(len=15) :: naio =       "irwo_na"
         character(len=15) :: kio =        "irwo_k"
         character(len=15) :: clio =       "irwo_cl"
         character(len=15) :: co3io =      "irwo_co3"
         character(len=15) :: hco3io =     "irwo_hco3"
         !rainfall (wet deposition)
         character(len=15) :: so4rn =      "rain_so4"
         character(len=15) :: carn =       "rain_ca"
         character(len=15) :: mgrn =       "rain_mg"
         character(len=15) :: narn =       "rain_na"
         character(len=15) :: krn =        "rain_k"
         character(len=15) :: clrn =       "rain_cl"
         character(len=15) :: co3rn =      "rain_co3"
         character(len=15) :: hco3rn =     "rain_hco3"
         !dry deposition
         character(len=15) :: so4dd =      "dryd_so4"
         character(len=15) :: cadd =       "dryd_ca"
         character(len=15) :: mgdd =       "dryd_mg"
         character(len=15) :: nadd =       "dryd_na"
         character(len=15) :: kdd =        "dryd_k"
         character(len=15) :: cldd =       "dryd_cl"
         character(len=15) :: co3dd =      "dryd_co3"
         character(len=15) :: hco3dd =     "dryd_hco3"
         !road salt application
         character(len=15) :: so4rd =      "road_so4"
         character(len=15) :: card =       "road_ca"
         character(len=15) :: mgrd =       "road_mg"
         character(len=15) :: nard =       "road_na"
         character(len=15) :: krd =        "road_k"
         character(len=15) :: clrd =       "road_cl"
         character(len=15) :: co3rd =      "road_co3"
         character(len=15) :: hco3rd =     "road_hco3"
         !fertilizer application
         character(len=15) :: so4fz =      "fert_so4"
         character(len=15) :: cafz =       "fert_ca"
         character(len=15) :: mgfz =       "fert_mg"
         character(len=15) :: nafz =       "fert_na"
         character(len=15) :: kfz =        "fert_k"
         character(len=15) :: clfz =       "fert_cl"
         character(len=15) :: co3fz =      "fert_co3"
         character(len=15) :: hco3fz =     "fert_hco3"
         !amnd salt application
         character(len=15) :: so4am =      "amnd_so4"
         character(len=15) :: caam =       "amnd_ca"
         character(len=15) :: mgam =       "amnd_mg"
         character(len=15) :: naam =       "amnd_na"
         character(len=15) :: kam =        "amnd_k"
         character(len=15) :: clam =       "amnd_cl"
         character(len=15) :: co3am =      "amnd_co3"
         character(len=15) :: hco3am =     "amnd_hco3"
         !salt uptake
         character(len=15) :: so4up =      "uptk_so4"
         character(len=15) :: caup =       "uptk_ca"
         character(len=15) :: mgup =       "uptk_mg"
         character(len=15) :: naup =       "uptk_na"
         character(len=15) :: kup =        "uptk_k"
         character(len=15) :: clup =       "uptk_cl"
         character(len=15) :: co3up =      "uptk_co3"
         character(len=15) :: hco3up =     "uptk_hco3"
         !soil water concentration (averaged over layers)
         character(len=15) :: so4c =       "conc_so4"
         character(len=15) :: cac =        "conc_ca"
         character(len=15) :: mgc =        "conc_mg"
         character(len=15) :: nac =        "conc_na"
         character(len=15) :: kc =         "conc_k"
         character(len=15) :: clc =        "conc_cl"
         character(len=15) :: co3c =       "conc_co3"
         character(len=15) :: hco3c =      "conc_hco3"
         !salt mineral dissolution and precipitation
         character(len=15) :: dssl =       "dssl_total"
      end type output_salt_hdr_hru      
      type (output_salt_hdr_hru) :: salt_hdr_hru
            
      end module