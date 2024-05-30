      module constituent_mass_module

      implicit none

      character(len=16), dimension(:), allocatable :: pest_init_name
      character(len=16), dimension(:), allocatable :: path_init_name
      character(len=16), dimension(:), allocatable :: hmet_init_name
      character(len=16), dimension(:), allocatable :: salt_init_name
      character(len=16), dimension(:), allocatable :: cs_init_name !rtb cs

      
      type constituents
        integer :: num_tot = 0                                      !number of total constituents simulated
        integer :: num_pests = 0                                    !number of pesticides simulated
        character (len=16), dimension(:), allocatable :: pests      !name of the pesticides- points to pesticide database
        !!need to crosswalk pests to get pest_num for database - use sequential for object
        integer, dimension(:), allocatable :: pest_num              !number of the pesticides- points to pesticide database
        integer :: num_paths = 0                                    !number of pathogens simulated
        character (len=16), dimension(:), allocatable :: paths      !name of the pathogens- points to pathogens database
        integer, dimension(:), allocatable :: path_num              !number of the pathogens- points to pathogens database
        integer :: num_metals = 0                                   !number of heavy metals simulated
        character (len=16), dimension(:), allocatable :: metals     !name of the heavy metals- points to heavy metals database
        integer, dimension(:), allocatable :: metals_num            !number of the heavy metals- points to heavy metals database
        integer :: num_salts = 0                                    !number of salt ions simulated
        character (len=16), dimension(:), allocatable :: salts      !name of the salts - points to salts database
        integer, dimension(:), allocatable :: salts_num             !number of the salts - points to salts database
        integer :: num_cs = 0                                       !number of other constituents simulated
        character (len=16), dimension(:), allocatable :: cs         !name of the constituents - points to cs database
        integer, dimension(:), allocatable :: cs_num                !number of the constituents - points to salts database
      end type constituents
      type (constituents) :: cs_db

      type exco_pesticide
        real, dimension (:), allocatable :: pest         !pesticide hydrographs
      end type exco_pesticide
      type (exco_pesticide), dimension (:), allocatable :: exco_pest        !export coefficients
      
      type dr_pesticide
        real, dimension (:), allocatable :: pest         !pesticide delivery
      end type dr_pesticide
      type (dr_pesticide), dimension (:), allocatable :: dr_pest            !delivery ratios
      
      type exco_pathogens
        real, dimension (:), allocatable :: path         !pesticide hydrographs
      end type exco_pathogens
      type (exco_pathogens), dimension (:), allocatable :: exco_path        !export coefficients
      
      type dr_pathogens
        real, dimension (:), allocatable :: path         !pathogen delivery
      end type dr_pathogens
      type (dr_pathogens), dimension (:), allocatable :: dr_path            !delivery ratios
      
      type exco_heavy_metals
        real, dimension (:), allocatable :: hmet                            !heavy metals hydrographs
      end type exco_heavy_metals
      type (exco_heavy_metals), dimension (:), allocatable :: exco_hmet     !export coefficients
      
      type dr_heavy_metals
        real, dimension (:), allocatable :: hmet                            !heavy metals delivery
      end type dr_heavy_metals
      type (dr_heavy_metals), dimension (:), allocatable :: dr_hmet         !delivery ratios
      
      type exco_salts
        real, dimension (:), allocatable :: salt                            !salts hydrographs
      end type exco_salts
      type (exco_salts), dimension (:), allocatable :: exco_salt            !export coefficients
      
      type dr_salts
        real, dimension (:), allocatable :: salt                            !salts delivery
      end type dr_salts
      type (dr_salts), dimension (:), allocatable :: dr_salt                !delivery ratios
      
      type salt_solids_soil
        real, dimension (:), allocatable :: solid                               !salt solid by soil layer
      end type salt_solids_soil
      type (salt_solids_soil), dimension (:), allocatable :: sol_salt_solid     !salt solid by hru

      ! constituent mass - soil, plant, aquifer, and channels
      type constituent_mass
        real, dimension (:), allocatable :: pest        !kg/ha          |pesticide in soil layer
        real, dimension (:), allocatable :: path        !pathogen hydrographs
        real, dimension (:), allocatable :: hmet        !heavy metal hydrographs 
        real, dimension (:), allocatable :: salt        !salt ion mass (kg/ha)
        real, dimension (:), allocatable :: salt_min    !salt mineral hydrographs
        real, dimension (:), allocatable :: saltc       !salt ion concentrations (mg/L)
        real, dimension (:), allocatable :: cs          !constituent mass (kg/ha)
        real, dimension (:), allocatable :: csc         !constituent concentration (mg/L)
        real, dimension (:), allocatable :: cs_sorb     !sorbed constituent mass (kg/ha)
        real, dimension (:), allocatable :: csc_sorb    !sorbed constituent concentration (mg/kg)
      end type constituent_mass
      
      ! irrigation water constituent mass - dimensioned by hru
      type (constituent_mass), dimension (:), allocatable :: cs_irr
      
      ! soil constituent mass - dimensioned by hru
      type soil_constituent_mass
        type (constituent_mass), dimension (:), allocatable :: ly
      end type soil_constituent_mass
      type (soil_constituent_mass), dimension (:), allocatable :: cs_soil

      ! plant constituent mass - dimensioned by hru
      type plant_constituent_mass
        type (constituent_mass), dimension (:), allocatable :: pl_in    !constituent in plant
        type (constituent_mass), dimension (:), allocatable :: pl_on    !constituent on plant
        type (constituent_mass), dimension (:), allocatable :: pl_up    !constituent uptake by plant
      end type plant_constituent_mass
      
      type (plant_constituent_mass), dimension (:), allocatable :: cs_pl

      ! aquifer constituent mass
      type (constituent_mass), dimension (:), allocatable :: cs_aqu
      type (constituent_mass), dimension (:), allocatable :: cs_aqu_init

      ! storing water and benthic constituents in channel
      type (constituent_mass), dimension (:), allocatable :: ch_water
      type (constituent_mass), dimension (:), allocatable :: ch_benthic
      type (constituent_mass), dimension (:), allocatable :: ch_water_init
      type (constituent_mass), dimension (:), allocatable :: ch_benthic_init
      
      ! constituent mass - reservoirs
      type constituent_mass_res
        real, dimension (:), allocatable :: pest
        real, dimension (:), allocatable :: path
        real, dimension (:), allocatable :: hmet
        real, dimension (:), allocatable :: salt        !salt ion mass (kg)
        real, dimension (:), allocatable :: saltc       !salt ion concentrations (g/m3)
        real, dimension (:), allocatable :: cs          !constituent mass (kg)
        real, dimension (:), allocatable :: csc         !constituent concentration (g/m3)
      end type constituent_mass_res
      
      ! storing salt and constituent mass in reservoirs
      type (constituent_mass), dimension (:), allocatable :: res_water
      type (constituent_mass), dimension (:), allocatable :: res_benthic
      
      ! storing salt and constituent mass in wetlands
      type (constituent_mass), dimension (:), allocatable :: wet_water
      
      ! hydrographs used in command for adding incoming hyds
      type (constituent_mass) :: hcs1, hcs2, hcs3
      ! set zero constituent hydrograph
      type (constituent_mass) :: hin_csz
      
      ! hydrographs for all constituents - dimension to number of each constituent
      type all_constituent_hydrograph
        type (constituent_mass), dimension (:), allocatable :: hd
        type (constituent_mass), dimension (:), allocatable :: hin
        type (constituent_mass), dimension (:), allocatable :: hin_sur
        type (constituent_mass), dimension (:), allocatable :: hin_lat
        type (constituent_mass), dimension (:), allocatable :: hin_til
        type (constituent_mass), dimension (:), allocatable :: hin_aqu
        type (constituent_mass), dimension(:), allocatable :: hcsin_d
        type (constituent_mass), dimension(:), allocatable :: hcsin_m
        type (constituent_mass), dimension(:), allocatable :: hcsin_y
        type (constituent_mass), dimension(:), allocatable :: hcsin_a
        type (constituent_mass), dimension(:), allocatable :: hcsout_m
        type (constituent_mass), dimension(:), allocatable :: hcsout_y
        type (constituent_mass), dimension(:), allocatable :: hcsout_a
      end type all_constituent_hydrograph
      type (all_constituent_hydrograph), dimension (:), allocatable :: obcs
      
      ! array for indicating if object has obcs allocated
      integer, dimension (:), allocatable :: obcs_alloc
      
      ! hydrographs for groundwater loading to streams
      type gw_load_hydrograph
        type (constituent_mass), dimension (:), allocatable :: hd
      end type gw_load_hydrograph
      type (gw_load_hydrograph), dimension (:), allocatable :: aq_chcs
      
      ! set zero all constituent hydrograph
      type (all_constituent_hydrograph) :: hcsz
      
      !routing unit salt mass (rtb salt)
      type (all_constituent_hydrograph), dimension (:), allocatable :: rusaltb_d
      type (all_constituent_hydrograph), dimension (:), allocatable :: rusaltb_m
      type (all_constituent_hydrograph), dimension (:), allocatable :: rusaltb_y
      type (all_constituent_hydrograph), dimension (:), allocatable :: rusaltb_a
      
      !routing unit constituent mass (rtb cs)
      type (all_constituent_hydrograph), dimension (:), allocatable :: rucsb_d
      type (all_constituent_hydrograph), dimension (:), allocatable :: rucsb_m
      type (all_constituent_hydrograph), dimension (:), allocatable :: rucsb_y
      type (all_constituent_hydrograph), dimension (:), allocatable :: rucsb_a
      
      !recall salinity inputs (rtb salt)
      type recall_salt_inputs
         character (len=16) :: name
         integer :: typ                        !recall type - 1=day, 2=mon, 3=year
         character(len=30) :: filename         !filename
         integer :: start_yr                   !start year of point source file
         integer :: end_yr                     !end year of point source file
         integer :: pts_type                   !1 = within watershed; 2 = from outside watershed
         type (constituent_mass), dimension (:,:), allocatable :: hd_salt
      end type recall_salt_inputs
      type (recall_salt_inputs), dimension(:),allocatable:: rec_salt
      
      !recall constituent inputs (rtb cs)
      type recall_cs_inputs
         character (len=16) :: name
         integer :: typ                        !recall type - 1=day, 2=mon, 3=year
         character(len=30) :: filename         !filename
         integer :: start_yr                   !start year of point source file
         integer :: end_yr                     !end year of point source file
         integer :: pts_type                   !1 = within watershed; 2 = from outside watershed
         type (constituent_mass), dimension (:,:), allocatable :: hd_cs
      end type recall_cs_inputs
      type (recall_cs_inputs), dimension(:),allocatable:: rec_cs
      
      !salt balance arrays (rtb salt)
      !point sources originating from within the watershed (e.g. WWTP effluent)
      type (constituent_mass), dimension(:),allocatable:: recsaltb_d
      type (constituent_mass), dimension(:),allocatable:: recsaltb_m
      type (constituent_mass), dimension(:),allocatable:: recsaltb_y
      type (constituent_mass), dimension(:),allocatable:: recsaltb_a
      !point sources originating from outside the watershed (e.g. inflow from upstream watersheds)
      type (constituent_mass), dimension(:),allocatable:: recoutsaltb_d
      type (constituent_mass), dimension(:),allocatable:: recoutsaltb_m
      type (constituent_mass), dimension(:),allocatable:: recoutsaltb_y
      type (constituent_mass), dimension(:),allocatable:: recoutsaltb_a
      
      !constituent balance arrays (rtb cs)
      !point sources originating from within the watershed (e.g. WWTP effluent)
      type (constituent_mass), dimension(:),allocatable:: reccsb_d
      type (constituent_mass), dimension(:),allocatable:: reccsb_m
      type (constituent_mass), dimension(:),allocatable:: reccsb_y
      type (constituent_mass), dimension(:),allocatable:: reccsb_a
      !point sources originating from outside the watershed (e.g. inflow from upstream watersheds)
      type (constituent_mass), dimension(:),allocatable:: recoutcsb_d
      type (constituent_mass), dimension(:),allocatable:: recoutcsb_m
      type (constituent_mass), dimension(:),allocatable:: recoutcsb_y
      type (constituent_mass), dimension(:),allocatable:: recoutcsb_a
      
      !recall pesticide inputs
      type recall_pesticide_inputs
         character (len=16) :: name
         integer :: num = 0                    !number of elements
         integer :: typ                        !recall type - 1=day, 2=mon, 3=year
         character(len=13) :: filename         !filename
         !hyd_output units are in cms and mg/L
         type (constituent_mass), dimension (:,:), allocatable :: hd_pest
      end type recall_pesticide_inputs
      type (recall_pesticide_inputs), dimension(:),allocatable:: rec_pest
      
      ! intial constituent soil-plant concentrations for hrus
      type cs_soil_init_concentrations
        character (len=16) :: name                  !! name of the constituent - points to constituent database
        real, dimension (:), allocatable :: soil    !! ppm                  |amount of constituent in soil at start of simulation
        real, dimension (:), allocatable :: plt     !! ppm or #cfu/m^2      |amount of constituent on plant at start of simulation
      end type cs_soil_init_concentrations
      type (cs_soil_init_concentrations), dimension(:), allocatable:: pest_soil_ini
      type (cs_soil_init_concentrations), dimension(:), allocatable:: path_soil_ini
      type (cs_soil_init_concentrations), dimension(:), allocatable:: hmet_soil_ini
      !! first 8 values of soil and plt are salt ion concentrations and next 5 are salt mineral fractions
      type (cs_soil_init_concentrations), dimension(:), allocatable:: salt_soil_ini
      type (cs_soil_init_concentrations), dimension(:), allocatable:: cs_soil_ini !rtb cs
      
      ! intial salt ion groundwater concentrations and mineral fractions for aquifers
      type salt_aqu_init_concentrations
        character (len=16) :: name                  !! name of the constituent - points to constituent database
        real, dimension (:), allocatable :: conc    !! g/m3                 |salt ion concentration at start of simulation
        real, dimension (:), allocatable :: frac    !! fractions            |salt mineral fractions at start of simulation
      end type salt_aqu_init_concentrations
      type (salt_aqu_init_concentrations), dimension(:), allocatable:: salt_aqu_ini
      
      ! intial constituent groundwater concentrations for aquifers
      type cs_aqu_init_concentrations
        character (len=16) :: name                  !! name of the constituent - points to constituent database
        real, dimension (:), allocatable :: aqu     !! ppm                  |concentration, sorbed mass at start of simulation
      end type cs_aqu_init_concentrations
      type (cs_aqu_init_concentrations), dimension(:), allocatable:: cs_aqu_ini
      
      !initial salt ion water concentrations for channels
      type salt_cha_init_concentrations
        character (len=16) :: name                  !! name of the constituent - points to salt ion database
        real, dimension (:), allocatable :: conc    !! g/m3                 |salt ion concentration at start of simulation
      end type salt_cha_init_concentrations
      type (salt_cha_init_concentrations), dimension(:), allocatable:: salt_cha_ini
      
      !initial constituent water concentrations for channels
      type cs_cha_init_concentrations
        character (len=16) :: name                  !! name of the constituent - points to salt ion database
        real, dimension (:), allocatable :: conc    !! g/m3                 |constituent concentration at start of simulation
      end type cs_cha_init_concentrations
      type (cs_cha_init_concentrations), dimension(:), allocatable:: cs_cha_ini
      
      ! intial constituent water-benthic concentrations for reservoirs and channels
      type cs_water_init_concentrations
        character (len=16) :: name                    !! name of the constituent - points to constituent database
        real, dimension (:), allocatable :: water     !! ppm,fracitons        |amount of constituents (dissolved, salt minerals) in aquifer at start of simulation
        real, dimension (:), allocatable :: benthic   !! ppm or #cfu/m^2      |amount of constituent in benthic at start of simulation
        real, dimension (:), allocatable :: reservoir !! ppm                  |amount of constituent in reservoir water at start of simulation
      end type cs_water_init_concentrations
      type (cs_water_init_concentrations), dimension(:),allocatable:: pest_water_ini
      type (cs_water_init_concentrations), dimension(:),allocatable:: path_water_ini
      type (cs_water_init_concentrations), dimension(:),allocatable:: hmet_water_ini

      ! concentration in irrigation water (outside source)
      type cs_irrigation_concentrations
        character (len=16) :: name                  !! name of the constituent - points to constituent database
        real, dimension (:), allocatable :: water   !! ppm                  |amount of constituent in water at start of simulation
      end type cs_irrigation_concentrations
      type (cs_irrigation_concentrations), dimension(:),allocatable:: salt_water_irr
      type (cs_irrigation_concentrations), dimension(:),allocatable:: cs_water_irr
      
      !daily output for constituents                
      !logical :: cs_obs_file                               !               |flag: file for channels with daily output
      integer :: cs_obs_file                                !               |flag: file for channels with daily output      
      integer :: cs_str_nobs                                !                |number of channels for daily output
      integer, dimension (:), allocatable :: cs_str_obs     !                |list of channels for daily output
      
      
      !header for routing unit salt balance output
      type output_rusaltb_header
         character(len=6) :: day =        "  jday"
         character(len=6) :: mo =         "   mon"
         character(len=6) :: day_mo =     "   day"
         character(len=6) :: yrc =        "    yr"
         character(len=8) :: isd =        "   unit "
         character(len=12) :: id =        " gis_id "
         !total salt out (surq + latq + tile) --> see hru_hyds subroutine
         character(len=15) :: so4tot =    "total_so4"
         character(len=15) :: castot =    "total_ca"
         character(len=15) :: mgstot =    "total_mg"
         character(len=15) :: nastot =    "total_na"
         character(len=15) :: kstot =     "total_k"
         character(len=15) :: clstot =    "total_cl"
         character(len=15) :: co3stot =   "total_co3"
         character(len=15) :: hco3stot =  "total_hco3"
         !percolation
         character(len=15) :: so4pc =      "perc_so4"
         character(len=15) :: capc =       "perc_ca"
         character(len=15) :: mgpc =       "perc_mg"
         character(len=15) :: napc =       "perc_na"
         character(len=15) :: kpc =        "perc_k"
         character(len=15) :: clpc =       "perc_cl"
         character(len=15) :: co3pc =      "perc_co3"
         character(len=15) :: hco3pc =     "perc_hco3"
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
         !tile flow
         character(len=15) :: so4tq =      "tile_so4"
         character(len=15) :: catq =       "tile_ca"
         character(len=15) :: mgtq =       "tile_mg"
         character(len=15) :: natq =       "tile_na"
         character(len=15) :: ktq =        "tile_k"
         character(len=15) :: cltq =       "tile_cl"
         character(len=15) :: co3tq =      "tile_co3"
         character(len=15) :: hco3tq =     "tile_hco3"
         !wetland seepage to soil profile
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
         !soil salt amendments
         character(len=15) :: so4am =      "amnd_so4"
         character(len=15) :: caam =       "amnd_ca"
         character(len=15) :: mgam =       "amnd_mg"
         character(len=15) :: naam =       "amnd_na"
         character(len=15) :: kam =        "amnd_k"
         character(len=15) :: clam =       "amnd_cl"
         character(len=15) :: co3am =      "amnd_co3"
         character(len=15) :: hco3am =     "amnd_hco3"
         !plant salt uptake
         character(len=15) :: so4up =      "uptk_so4"
         character(len=15) :: caup =       "uptk_ca"
         character(len=15) :: mgup =       "uptk_mg"
         character(len=15) :: naup =       "uptk_na"
         character(len=15) :: kup =        "uptk_k"
         character(len=15) :: clup =       "uptk_cl"
         character(len=15) :: co3up =      "uptk_co3"
         character(len=15) :: hco3up =     "uptk_hco3"
         !salt mineral dissolution and precipitation
         character(len=15) :: dssl =       "dssl_total"
      end type output_rusaltb_header      
      type (output_rusaltb_header) :: rusaltb_hdr
      
      !header for routing unit constituent balance output
      type output_rucsb_header
         character(len=6) :: day =        "  jday"
         character(len=6) :: mo =         "   mon"
         character(len=6) :: day_mo =     "   day"
         character(len=6) :: yrc =        "    yr"
         character(len=8) :: isd =        "   unit "
         character(len=12) :: id =        " gis_id "
         !total cs out (surq + latq + tile) --> see hru_hyds subroutine
         character(len=15) :: seo4tot =   "total_seo4"
         character(len=15) :: seo3tot =   "total_seo3"
         character(len=15) :: borntot =   "total_born"
         !percolation
         character(len=15) :: seo4pc =    "perc_seo4"
         character(len=15) :: seo3pc =    "perc_seo3"
         character(len=15) :: bornpc =    "perc_born"
         !surface runoff
         character(len=15) :: seo4sq =    "surq_seo4"
         character(len=15) :: seo3sq =    "surq_seo3"
         character(len=15) :: bornsq =    "surq_born"
         !lateral flow
         character(len=15) :: seo4lq =    "latq_seo4"
         character(len=15) :: seo3lq =    "latq_seo3"
         character(len=15) :: bornlq =    "latq_born"
         !tile flow
         character(len=15) :: seo4tq =    "tile_seo4"
         character(len=15) :: seo3tq =    "tile_seo3"
         character(len=15) :: borntq =    "tile_born"
         !sediment runoff
         character(len=15) :: seo4sd =    "sedm_seo4"
         character(len=15) :: seo3sd =    "sedm_seo3"
         character(len=15) :: bornsd =    "sedm_born"
         !wetland seepage to soil profile
         character(len=15) :: seo4ws =    "wtsp_seo4"
         character(len=15) :: seo3ws =    "wtsp_seo3"
         character(len=15) :: bornws =    "wtsp_born"
         !irrigation (surface water)
         character(len=15) :: seo4is =    "irsw_seo4"
         character(len=15) :: seo3is =    "irsw_seo3"
         character(len=15) :: bornis =    "irsw_born"
         !irrigation (groundwater)
         character(len=15) :: seo4ig =    "irgw_seo4"
         character(len=15) :: seo3ig =    "irgw_seo3"
         character(len=15) :: bornig =    "irgw_born"
         !irrigation (outside watershed)
         character(len=15) :: seo4io =    "irwo_seo4"
         character(len=15) :: seo3io =    "irwo_seo3"
         character(len=15) :: bornio =    "irwo_born"
         !rainfall (wet deposition)
         character(len=15) :: seo4rn =    "rain_seo4"
         character(len=15) :: seo3rn =    "rain_seo3"
         character(len=15) :: bornrn =    "rain_born"
         !dry deposition
         character(len=15) :: seo4dd =    "dryd_seo4"
         character(len=15) :: seo3dd =    "dryd_seo3"
         character(len=15) :: borndd =    "dryd_born"
         !fertilizer
         character(len=15) :: seo4fz =    "fert_seo4"
         character(len=15) :: seo3fz =    "fert_seo3"
         character(len=15) :: bornfz =    "fert_born"
         !plant selenium uptake
         character(len=15) :: seo4up =    "uptk_seo4"
         character(len=15) :: seo3up =    "uptk_seo3"
         character(len=15) :: bornup =    "uptk_born"
         !chemical reactions
         character(len=15) :: seo4rc =    "rctn_seo4"
         character(len=15) :: seo3rc =    "rctn_seo3"
         character(len=15) :: bornrc =    "rctn_born"
         !mass transfer from sorption
         character(len=15) :: seo4sb =    "sorb_seo4"
         character(len=15) :: seo3sb =    "sorb_seo3"
         character(len=15) :: bornsb =    "sorb_born"
      end type output_rucsb_header      
      type (output_rucsb_header) :: rucsb_hdr
      
     type constituents_header_in          
        character (len=11) :: day      = "       jday "
        character (len=12) :: mo       = "         mon"
        character (len=12) :: day_mo   = "         day"
        character (len=12) :: yrc      = "          yr"
        character (len=12) :: name     = "         iob"
        character (len=12) :: otype    = "     gis_id "
        character (len=12) :: type     = "        type"
        character (len=12) :: num      = "         num"
        character (len=12) :: obout    = "     obtypin"
        character (len=12) :: obno_out = "  obtyp_noin"
        character (len=12) :: htyp_out = "     htyp_in"
        character (len=12) :: frac     = "     frac_in"
        character (len=12) :: sol      = "      sol_in"
        character (len=12) :: sor      = "      sor_in"
      end type constituents_header_in
      type (constituents_header_in) :: csin_hyd_hdr
          
      type constituents_header_out          
        character (len=11) :: day      = "       jday "
        character (len=12) :: mo       = "         mon"
        character (len=12) :: day_mo   = "         day"
        character (len=12) :: yrc      = "          yr"
        character (len=12) :: name     = "         iob"
        character (len=12) :: otype    = "     gis_id "
        character (len=12) :: type     = "        type"
        character (len=12) :: num      = "         num"
        character (len=12) :: obout    = "    obtypout"
        character (len=12) :: obno_out = " obtyp_noout"
        character (len=12) :: htyp_out = "    htyp_out"
        character (len=12) :: frac     = "    frac_out"
      end type constituents_header_out
      type (constituents_header_out) :: csout_hyd_hdr
      
      type sol_sor
        character (len=12) :: sol =      "     sol_out"
        character (len=12) :: sor =      "     sor_out"
      end type sol_sor
      type (sol_sor), dimension (:), allocatable :: cs_pest_solsor
      type (sol_sor), dimension (:), allocatable :: cs_path_solsor
      type (sol_sor), dimension (:), allocatable :: cs_hmet_solsor
      type (sol_sor), dimension (:), allocatable :: cs_salt_solsor
     
      interface operator (+)
        module procedure hydcsout_add
      end interface
      
      interface operator (*)
        module procedure hydcsout_mult_const
      end interface

      contains
      
      function hydcsout_add (hydcs1, hydcs2) result (hydcs3)
        type (constituent_mass), intent (in) :: hydcs1
        type (constituent_mass), intent (in) :: hydcs2
        type (constituent_mass) :: hydcs3
        integer :: ipest, ipath, ihmet, isalt, ics
        allocate (hydcs3%pest(cs_db%num_pests))
        allocate (hydcs3%path(cs_db%num_paths))
        allocate (hydcs3%hmet(cs_db%num_metals))
        allocate (hydcs3%salt(cs_db%num_salts))
        allocate (hydcs3%cs(cs_db%num_cs))

        do ipest = 1, cs_db%num_pests
          hydcs3%pest(ipest) =  hydcs2%pest(ipest) + hydcs1%pest(ipest)
        end do
        do ipath = 1, cs_db%num_paths
          hydcs3%path(ipath) =  hydcs2%path(ipath) + hydcs1%path(ipath)
        end do
        do ihmet = 1, cs_db%num_metals
          hydcs3%hmet(ihmet) =  hydcs2%hmet(ihmet) + hydcs1%hmet(ihmet)
        end do
        do isalt = 1, cs_db%num_salts
          hydcs3%salt(isalt) =  hydcs2%salt(isalt) + hydcs1%salt(isalt)
        end do !rtb cs
        do ics = 1, cs_db%num_cs
          hydcs3%cs(ics) =  hydcs2%cs(ics) + hydcs1%cs(ics)
        end do
      return
      end function hydcsout_add
      
      function hydcsout_mult_const (const, hydcs1) result (hydcs2)
        type (constituent_mass), intent (in) :: hydcs1
        type (constituent_mass) :: hydcs2
        real, intent (in) :: const
        integer :: ipest, ipath, ihmet, isalt, ics
        allocate (hydcs2%pest(cs_db%num_pests))
        allocate (hydcs2%path(cs_db%num_paths))
        allocate (hydcs2%hmet(cs_db%num_metals))
        allocate (hydcs2%salt(cs_db%num_salts))
        allocate (hydcs2%cs(cs_db%num_cs)) !rtb cs

        do ipest = 1, cs_db%num_pests
          hydcs2%pest(ipest) =  const * hydcs1%pest(ipest)
        end do
        do ipath = 1, cs_db%num_paths
          hydcs2%path(ipath) =  const * hydcs1%path(ipath)
        end do
        do ihmet = 1, cs_db%num_metals
          hydcs2%hmet(ihmet) =  const * hydcs1%hmet(ihmet)
        end do
        do isalt = 1, cs_db%num_salts
          hydcs2%salt(isalt) =  const * hydcs1%salt(isalt)
        end do
        do ics = 1, cs_db%num_cs !rtb cs
          hydcs2%cs(ics) =  const * hydcs1%cs(ics)
        end do
        return
      end function hydcsout_mult_const
      
      end module constituent_mass_module