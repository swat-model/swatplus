      module organic_mineral_mass_module
    
      use carbon_module, only: organic_flux, organic_controls, organic_allocations, organic_transformations
      use carbon_module
      implicit none 

      real :: meta_frac = 0.85  !none       |fraction of that is metabolic
      real :: str_frac = 0.15   !none       |fraction of that is structural
      real :: lig_frac = 0.12   !none       |fraction of that is lignin
      
      type organic_mass
        real :: m = 0.              !kg/ha      |total object mass
        real :: c = 0.              !kg/ha      |carbon mass
        real :: n = 0.              !kg/ha      |organic nitrogen mass
        real :: p = 0.              !kg/ha      |organic phosphorus mass
      end type organic_mass
      type (organic_mass) :: orgz

      type organic_mixing_mass
        type (organic_mass) :: tot       !       |total organic pool
        type (organic_mass) :: rsd       !       |fresh residue-all plants in one pool - layer 1 = surface residue
        !! humus pools for old mineralization model (static carbon)
        type (organic_mass) :: hact      !       |active humus for old mineralization model
        type (organic_mass) :: hsta      !       |stable humus for old mineralization model
        !! organic pools used in CENTURY model
        type (organic_mass) :: hs        !       |slow humus
        type (organic_mass) :: hp        !       |passive humus
        type (organic_mass) :: microb    !       |microbial biomass
        type (organic_mass) :: str       !       |structural litter pool
        type (organic_mass) :: lig       !       |lignin pool
        type (organic_mass) :: meta      !       |metabolic litter pool
        type (organic_mass) :: man       !       |manure pool
        type (organic_mass) :: water     !       |water soluble
      end type organic_mixing_mass
      type (organic_mixing_mass) :: mix_org

      type clay_mass
        real :: m = 0.              !kg or kg/ha      |total object mass
        real :: nh4 = 0.            !kg or kg/ha      |ammonium mass
      end type clay_mass
      
      type sediment
        real :: m = 0.              !kg or kg/ha      |total object mass
        real :: sand = 0.           !kg or kg/ha      |sand mass
        real :: silt = 0.           !kg or kg/ha      |silt mass
        type (clay_mass) :: clay    !kg or kg/ha      |clay mass
        real :: gravel = 0.         !kg or kg/ha      |gravel mass
      end type sediment
      
      type mineral_nitrogen
        real :: no3 = 0.            !kg/ha  |nitrate dimensioned by layer
        real :: nh4 = 0.            !kg/ha  |ammonium dimensioned by layer
      end type mineral_nitrogen
      type (mineral_nitrogen) :: mnz
      type (mineral_nitrogen) :: mix_mn    !       |mineral n pool used in tillage mixing
            
      type mineral_phosphorus
        real :: wsol = 0.           !kg/ha  |water soluble p dimensioned by layer
        real :: lab = 0.            !kg/ha  |labile p dimensioned by layer
        real :: act = 0.            !kg/ha  |active mineral p dimensioned by layer
        real :: sta = 0.            !kg/ha  |stable mineral p dimensioned by layer
      end type mineral_phosphorus
      type (mineral_phosphorus) :: mpz
      type (mineral_phosphorus) :: mix_mp    !       |mineral p pool used in tillage mixing
      
      type soil_profile_mass
        character (len=16) :: name = ""
        real :: tot_mn = 0.                                         !       |total mineral n pool (no3+nh4) in soil profile
        real :: tot_mp = 0.                                         !       |mineral p pool (wsol+lab+act+sta) in soil profile
        real :: salt = 0.                                           !       |total salt amount (kg/ha) in soil profile
        type (organic_mass) :: tot_org                              !       |total organics in soil profile
        type (organic_mass) :: seq_org                              !       |sequestered organics in soil profile wich does not include the surface layer
        type (organic_mass) :: surf_org                             !       |soil surface layer soil soil profile
        real, dimension(:), allocatable :: sw                       !mm     |soil water dimensioned by layer
        real, dimension(:), allocatable :: cbn                      !%      |percent carbon
        type (sediment), dimension(:), allocatable :: sed           !       |sediment dimensioned by layer
        type (mineral_nitrogen), dimension(:), allocatable :: mn    !       |mineral n pool dimensioned by layer
        type (mineral_phosphorus), dimension(:), allocatable :: mp  !       |mineral p humus pool dimensioned by layer
        !! tot and rsd used for both carbon methods
        type (organic_mass), dimension(:), allocatable :: tot       !       |total organic pool dimensioned by layer
        type (organic_mass), dimension(:), allocatable :: seq       !       |total sequestered organic pool dimensioned by layer, surface layer = 0.0
        type (organic_mass), dimension(:), allocatable :: rsd       !       |fresh residue-all plants in one pool by layer - layer 1 = surface residue
        !! humus pools for old mineralization model (static carbon)
        type (organic_controls),    dimension(:), allocatable :: org_con_lr  !      |organic contral variables by layer
        type (organic_allocations), dimension(:), allocatable :: org_allo_lr !      |organic allocation variables by layer
        type (organic_ratio), dimension(:), allocatable :: org_ratio_lr      !      |organic nitrogen carbon ratios layer
        type (organic_transformations), dimension(:), allocatable :: org_tran_lr !  |portential organic transformations layer
        type (organic_flux)                            :: org_flx_tot !      |total organic flux for soil profile
        type (organic_flux), dimension(:), allocatable :: org_flx_lr  !      |organic flux by layer
        type (organic_flux), dimension(:), allocatable :: org_flx_cum_lr !  |cumulative organic flux by layer
        type (organic_mass), dimension(:), allocatable :: hact      !       |active humus for old mineralization model dimensioned by layer
        type (organic_mass), dimension(:), allocatable :: hsta      !       |stable humus for old mineralization model dimensioned by layer
        !! organic pools used in CENTURY model
        type (organic_mass), dimension(:), allocatable :: hs        !       |slow humus dimensioned by layer
        type (organic_mass), dimension(:), allocatable :: hp        !       |passive humus dimensioned by layer
		
        !! rest are used in CENTURY model
        type (organic_mass), dimension(:), allocatable :: microb    !       |microbial biomass
        type (organic_mass), dimension(:), allocatable :: str       !       |structural litter pool dimensioned by layer
        type (organic_mass), dimension(:), allocatable :: lig       !       |lignin pool dimensioned by layer
        type (organic_mass), dimension(:), allocatable :: meta      !       |metabolic litter pool dimensioned by layer
        type (organic_mass), dimension(:), allocatable :: man       !       |manure pool dimensioned by layer
        type (organic_mass), dimension(:), allocatable :: water     !       |water soluble
      end type soil_profile_mass
      
      !soil profile object - dimensioned to number of hrus, using the hru pointer
      type (soil_profile_mass), dimension(:), allocatable, target :: soil1
      type (soil_profile_mass), dimension(:), allocatable :: soil1_init
      type (organic_mass) :: soil_prof_tot                          !       |total organic pool for profile (summed by layer)
      type (organic_mass) :: soil_prof_rsd                          !       |total fresh organic residue pool for profile (summed by lower layers)
      type (organic_mass) :: soil_prof_srsd                         !       |total fresh organic residue pool for surface
      type (organic_mass) :: soil_prof_hact                         !       |total active humus pool for profile (summed by layer)
      type (organic_mass) :: soil_prof_hsta                         !       |total stable huumus pool for profile (summed by layer)
      type (organic_mass) :: soil_prof_hs                           !       |total slow humus pool for profile (summed by layer)
      type (organic_mass) :: soil_prof_hp                           !       |total passive humus pool for profile (summed by layer)
      type (organic_mass) :: soil_prof_microb                       !       |total microbial pool for profile (summed by layer)
      type (organic_mass) :: soil_prof_seq_hs                       !       |sequestered slow humus pool for profile summed up by layer excluding layer 1
      type (organic_mass) :: soil_prof_seq_hp                       !       |sequestered passive humus pool for profile summed up by layer excluding layer 1
      type (organic_mass) :: soil_prof_seq_microb                   !       |sequestered microbial pool for profile summed up by layer excluding layer 1
      type (organic_mass) :: soil_prof_str                          !       |total structural pool for profile (summed by layer)
      type (organic_mass) :: soil_prof_lig                          !       |total lignin pool for profile (summed by layer)
      type (organic_mass) :: soil_prof_meta                         !       |total metabolic pool for profile (summed by layer)
      type (organic_mass) :: soil_prof_sstr                         !       |total structural pool for surface (summed by lower layers)
      type (organic_mass) :: soil_prof_slig                         !       |total lignin pool for surface (summed by lower layers)
      type (organic_mass) :: soil_prof_smeta                         !       |total metabolic pool for profile (summed by layer)
      type (organic_mass) :: soil_prof_man                          !       |total manure pool for profile (summed by layer)
      type (organic_mass) :: soil_prof_water                        !       |total dissolved pool for profile (summed by layer)
      type (organic_mass) :: soil_org_z                             !       |used to zero organic objects
      type (organic_mass) :: soil_prof_somc
      type (mineral_nitrogen) :: soil_prof_mn                       !       |mineral n pool (summed by layer)
      type (mineral_phosphorus) :: soil_prof_mp                     !       |mineral p  pool (summed by layer)
      type (mineral_nitrogen) :: soil_mn_z
      type (mineral_phosphorus) :: soil_mp_z
      type (organic_mass) :: bsn_org_soil                           !       |total soil organics in basin
      type (organic_mass) :: bsn_org_pl                             !       |total plant organics in basin
      type (organic_mass) :: bsn_org_rsd                            !       |total residue organics in basin
      real :: bsn_mn = 0.                                           !       |total mineral n pool (no3+nh4) in basin
      real :: bsn_mp = 0.                                           !       |mineral p pool (wsol+lab+act+sta) in basin
      type (organic_mass) :: decomp                                 !       |temporary storage for residue decomp
      type (organic_mass) :: pl_burn                                !       |residue and plant mass burned in fire
      type (organic_mass) :: rsd_meta                               !       |temporary storage for initial metabolic litter
      type (organic_mass) :: rsd_str                                !       |temporary storage for initial structural litter
      
      type plant_community_mass
       character(len=4) :: name = ""
       type (organic_mass), dimension(:), allocatable :: tot        !kg/ha      |total biomass for individual plant in community
       type (organic_mass), dimension(:), allocatable :: ab_gr      !kg/ha      |above ground biomass for individual plant in community
       type (organic_mass), dimension(:), allocatable :: leaf       !kg/ha      |leaf mass for individual plant in community
       type (organic_mass), dimension(:), allocatable :: stem       !kg/ha      |wood/stalk mass for individual plant in community
       type (organic_mass), dimension(:), allocatable :: root       !kg/ha      |root mass for individual plant in community (by soil layer)
       type (organic_mass), dimension(:), allocatable :: seed       !kg/ha      |seed (grain) mass for individual plant in community
       type (organic_mass), dimension(:), allocatable :: yield_tot  !kg/ha      |running total sum of yield at harvest -  ave annual print
       type (organic_mass), dimension(:), allocatable :: yield_yr   !kg/ha      |running yearly sum of yield at harvest - yearly print
       type (organic_mass) :: tot_com                               !kg/ha      |total biomass for entire community
       type (organic_mass) :: ab_gr_com                             !kg/ha      |above ground mass for entire community
       type (organic_mass) :: leaf_com                              !kg/ha      |leaf mass for entire community
       type (organic_mass) :: stem_com                              !kg/ha      |wood/stalk mass for entire community
       type (organic_mass) :: root_com                              !kg/ha      |root mass for entire community
       type (organic_mass) :: seed_com                              !kg/ha      |seed (grain) mass for entire community
      end type plant_community_mass
      
      type (plant_community_mass), dimension (:), allocatable :: pl_mass
      type (plant_community_mass), dimension (:), allocatable :: pl_mass_init
      type (organic_mass) :: pl_yield                               !kg/ha      |crop yield
      type (organic_mass) :: pl_mass_up                             !kg/ha      |daily biomass and c increase; n and p uptake
      type (organic_mass) :: pl_residue
      type (organic_mass) :: harv_seed, harv_leaf, harv_stem, harv_left
      type (organic_mass) :: graz_plant, graz_seed, graz_leaf, graz_stem
      type (organic_mass) :: leaf_drop                              !kg/ha      |organic mass of falling leaves
      type (organic_mass) :: abgr_drop                              !kg/ha      |above ground that dies at dormancy
      type (organic_mass) :: stem_drop                              !kg/ha      |stem that dies at dormancy
      type (organic_mass) :: seed_drop                              !kg/ha      |seed that dies at dormancy
      type (organic_mass) :: plt_mass_z

      type mineral_mass
        real :: m = 0.          !kg or kg/ha      |total object mass
        real :: no3 = 0.        !kg or kg/ha      |nitrate mass
        real :: no2 = 0.        !kg or kg/ha      |nitrite mass
        real :: nh4 = 0.        !kg or kg/ha      |ammonium mass  
        real :: po4 = 0.        !kg or kg/ha      |phosphate mass 
      end type mineral_mass

      type organic_mineral_mass
        real :: vol = 0.
        type (organic_mass) :: hum
        type (organic_mass) :: hum_act
        type (mineral_mass) :: min
      end type organic_mineral_mass
      
      !hru will point directly to herds - managed in schedule_ops and ultimately can be managed in conditional subroutine
      !herds are different from soil and plant in that they can move from hru to hru
      type animal_herds
        character(len=16) :: name = ""                                      !           |herd name (small_dairy, )
        integer :: num_tot = 0                                              !           |total number of animals in the herd
        type (organic_mass) :: herd_mass                                    !kg         |total mass of herd
        character(len=16), dimension(:), allocatable :: typ                 !           |animal type (points to animal.hrd)
        integer, dimension(:), allocatable :: num                           !           |number of each type of animal
        type (organic_mass), dimension(:), allocatable :: mass              !           |mass of each type of animal
        type (organic_mass), dimension(:), allocatable :: eat               !           |biomass eaten by each type of animal
        type (organic_mineral_mass), dimension(:), allocatable :: manure    !           |manure from each type of animal
      end type animal_herds
      
      !fertilizer object      
      type fertilizer_mass
        character (len=16) :: name = ""
        type (mineral_mass) :: org       !soil matrix dimensioned by layer
        type (organic_mass) :: min       !soil water dimensioned by layer
      end type fertilizer_mass
      !fertilizer object should be used as database input from fert.dat
      type (fertilizer_mass), dimension(:), allocatable :: fert         !dimension to number of fertilzers in database
      
      type (organic_mass) :: org_frt  !dimension to number of manures in database
      
      !manure object should be used as database input from manure.dat
      type (organic_mass), dimension(:), allocatable :: manure  !dimension to number of manures in database
      
      type organic_mineral_hydrograph
        real :: flo = 0.               !! m^3          |volume of water
        real :: sed = 0.               !! metric tons  |sediment
        type (organic_mass) :: org
        type (mineral_mass) :: min
        real :: chla = 0.              !! kg           |chlorophyll-a
        real :: cbod = 0.              !! kg           |carbonaceous biological oxygen demand
        real :: dox = 0.               !! kg           |dissolved oxygen
        real :: temp = 0.              !! deg c        |temperature
        real :: san = 0.               !! tons         |detached sand
        real :: sil = 0.               !! tons         |detached silt
        real :: cla = 0.               !! tons         |detached clay
        real :: sag = 0.               !! tons         |detached small ag
        real :: lag = 0.               !! tons         |detached large ag
        real :: grv = 0.               !! tons         |gravel
      end type organic_mineral_hydrograph
      
      type spatial_object_hydrographs
        character (len=16) :: name = ""                                 !should match the object_connectivity object
        !water and soluble components
        type (organic_mineral_hydrograph) :: hin                                 !inflow hydrograph for surface runon - sum of all inflow hyds
        type (organic_mineral_hydrograph) :: hin_sur                             !inflow hydrograph for surface flow - sum of all surface inflow hyds
        type (organic_mineral_hydrograph) :: hin_lat                             !inflow hydrograph for lateral soil flow - sum of all lateral inflow hyds
        type (organic_mineral_hydrograph) :: hin_til                             !inflow hydrograph for tile flow - sum of all tile inflow hyds
        type (organic_mineral_hydrograph) :: hin_aqu                             !inflow hydrograph for aquifer flow - sum of all aquifer inflow hyds
        type (organic_mineral_hydrograph), dimension(:),allocatable :: hd        !generated hydrograph (ie 1=tot, 2= recharge, 3=surf, etc)
        type (organic_mineral_hydrograph), dimension(:,:),allocatable :: ts      !subdaily hydrographs
        type (organic_mineral_hydrograph), dimension(:),allocatable :: tsin      !inflow subdaily hydrograph
        !sediment (sorbed) in the water components
        type (organic_mineral_hydrograph) :: hins                                 !inflow hydrograph for surface runon - sum of all inflow hyds
        type (organic_mineral_hydrograph) :: hin_ssur                             !inflow hydrograph for surface flow - sum of all surface inflow hyds
        type (organic_mineral_hydrograph) :: hin_slat                             !inflow hydrograph for lateral soil flow - sum of all lateral inflow hyds
        type (organic_mineral_hydrograph) :: hin_stil                             !inflow hydrograph for tile flow - sum of all tile inflow hyds
        type (organic_mineral_hydrograph), dimension(:),allocatable :: hds        !generated hydrograph (ie 1=tot, 2= recharge, 3=surf, etc)
        type (organic_mineral_hydrograph), dimension(:,:),allocatable :: tss      !subdaily hydrographs
        type (organic_mineral_hydrograph), dimension(:),allocatable :: tsins      !inflow subdaily hydrograph
        !hydrograph output variables
        type (organic_mineral_hydrograph), dimension(:),allocatable :: hin_d
        type (organic_mineral_hydrograph), dimension(:),allocatable :: hin_m
        type (organic_mineral_hydrograph), dimension(:),allocatable :: hin_y
        type (organic_mineral_hydrograph), dimension(:),allocatable :: hin_a
        type (organic_mineral_hydrograph), dimension(:),allocatable :: hout_m
        type (organic_mineral_hydrograph), dimension(:),allocatable :: hout_y
        type (organic_mineral_hydrograph), dimension(:),allocatable :: hout_a
        type (organic_mineral_hydrograph) :: hdep_m
        type (organic_mineral_hydrograph) :: hdep_y
        type (organic_mineral_hydrograph) :: hdep_a
      end type spatial_object_hydrographs
      !track spatial_object_hydrographs with ob - use same pointer
      type (spatial_object_hydrographs), dimension(:),allocatable :: obom
      
      !recall organic-mineral inputs
      type recall_organic_mineral_inputs
         character (len=16) :: name = ""
         integer :: num = 0                    !number of elements
         integer :: typ = 0                    !recall type - 1=day, 2=mon, 3=year
         character(len=13) :: filename = ""    !filename
         !hyd_output units are in cms and mg/L
         type (organic_mineral_hydrograph), dimension (:,:), allocatable :: hd_om     !export coefficients
      end type recall_organic_mineral_inputs
      type (recall_organic_mineral_inputs),dimension(:),allocatable:: rec_om

      !export coefficient and delivery ratio pesticides
      type (organic_mineral_hydrograph), dimension(:,:), allocatable :: exco_om
      
      !export coefficient and delivery ratio pesticides
      type (organic_mineral_hydrograph), dimension(:,:), allocatable :: dr_om
      
      type routing_unit_elements_hydrographs
        character (len=16) :: name = ""                              !should match the object_connectivity object
        type (organic_mineral_mass), dimension(:), allocatable :: hd
      end type routing_unit_elements_hydrographs
      !point to subbasin element objects - same as sub_elem
      type (routing_unit_elements_hydrographs), dimension(:), allocatable :: sub_e_hd
      
      type channel_surface_elements_hydrographs
        character (len=16) :: name = ""                              !should match the channel_surface_elements object
        type (organic_mineral_mass), dimension(:), allocatable :: hd
      end type channel_surface_elements_hydrographs
      !point to channel-surface objects - same as ch_sur
      type (channel_surface_elements_hydrographs), dimension(:), allocatable :: ch_sur_hd
      
      !objects needed for operators
      type (organic_mineral_mass) :: o_m1, o_m2, o_m3
      type (mineral_phosphorus) :: pmin_m1, pmin_m2, pmin_m3
      type (mineral_nitrogen) :: nmin_m1, nmin_m2, nmin_m3

      !we may also need operators for organic and mineral operations
      
      interface operator (+)
        module procedure om_add1
      end interface
      
      interface operator (+)
        module procedure org_flux_add1
      end interface
      
      interface operator (-)
        module procedure om_subtract
      end interface
            
      interface operator (*)
        module procedure om_mult_const
      end interface 
                         
      interface operator (/)
        module procedure om_divide
      end interface 
                   
      interface operator (+)
        module procedure pmin_add
      end interface 
                  
      interface operator (*)
        module procedure nmin_mult_const
      end interface 
                  
      interface operator (+)
        module procedure nmin_add
      end interface 

      interface operator (*)
        module procedure pmin_mult_const
      end interface 

    contains

      !! add mineral n
      function nmin_add (nmin_m1, nmin_m2) result (nmin_m3)
        type (mineral_nitrogen), intent (in) :: nmin_m1
        type (mineral_nitrogen), intent (in) :: nmin_m2
        type (mineral_nitrogen) :: nmin_m3
        nmin_m3%no3 = nmin_m1%no3 + nmin_m2%no3
        nmin_m3%nh4 = nmin_m1%nh4 + nmin_m2%nh4
      end function nmin_add
      
      !! multiply mineral n by a constant
      function nmin_mult_const (const, nmin_m1) result (nmin_m2)
        real, intent (in) :: const
        type (mineral_nitrogen), intent (in) :: nmin_m1
        type (mineral_nitrogen) :: nmin_m2
        nmin_m2%no3 = const * nmin_m1%no3
        nmin_m2%nh4 = const * nmin_m1%nh4
      end function nmin_mult_const
                          
      function pmin_add (pmin_m1, pmin_m2) result (pmin_m3)
        type (mineral_phosphorus), intent (in) :: pmin_m1
        type (mineral_phosphorus), intent (in) :: pmin_m2
        type (mineral_phosphorus) :: pmin_m3
        pmin_m3%wsol = pmin_m1%wsol + pmin_m2%wsol
        pmin_m3%lab = pmin_m1%lab + pmin_m2%lab
        pmin_m3%act = pmin_m1%act + pmin_m2%act
        pmin_m3%sta = pmin_m1%sta + pmin_m2%sta
      end function pmin_add

      !! multiply mineral n by a constant
      function pmin_mult_const (const, pmin_m1) result (pmin_m2)
        real, intent (in) :: const
        type (mineral_phosphorus), intent (in) :: pmin_m1
        type (mineral_phosphorus) :: pmin_m2
        pmin_m2%wsol = const * pmin_m1%wsol
        pmin_m2%lab = const * pmin_m1%lab
        pmin_m2%act = const * pmin_m1%act
        pmin_m2%sta = const * pmin_m1%sta
      end function pmin_mult_const
                          
      !! add organic mass
      function om_add1 (o_m1, o_m2) result (o_m3)
        type (organic_mass), intent (in) :: o_m1
        type (organic_mass), intent (in) :: o_m2
        type (organic_mass) :: o_m3
        o_m3%m = o_m1%m + o_m2%m
        o_m3%c = o_m1%c + o_m2%c
        o_m3%n = o_m1%n + o_m2%n
        o_m3%p = o_m1%p + o_m2%p
      end function om_add1
            
      !! subtract organic mass
      function om_subtract (o_m1, o_m2) result (o_m3)
        type (organic_mass), intent (in) :: o_m1
        type (organic_mass), intent (in) :: o_m2
        type (organic_mass) :: o_m3
        o_m3%m = o_m1%m - o_m2%m
        o_m3%c = o_m1%c - o_m2%c
        o_m3%n = o_m1%n - o_m2%n
        o_m3%p = o_m1%p - o_m2%p
      end function om_subtract
                           
      !! multiply organic mass by a constant
      function om_mult_const (const, o_m1) result (o_m2)
        real, intent (in) :: const
        type (organic_mass), intent (in) :: o_m1
        type (organic_mass) :: o_m2
        o_m2%m = const * o_m1%m
        o_m2%c = const * o_m1%c
        o_m2%n = const * o_m1%n
        o_m2%p = const * o_m1%p
      end function om_mult_const
                          
      !! divide organic mass by a constant
      function om_divide (o_m1, const) result (o_m2)
        type (organic_mass), intent (in) :: o_m1
        real, intent (in) :: const 
        type (organic_mass) :: o_m2
        o_m2%m = o_m1%m / const
        o_m2%c = o_m1%c / const
        o_m2%n = o_m1%n / const
        o_m2%p = o_m1%p / const
      end function om_divide
      
      !! add org_flux
      function org_flux_add1 (org_flux1, org_flux2) result (org_flux3)
        type (organic_flux), intent (in) :: org_flux1
        type (organic_flux), intent (in) :: org_flux2
        type (organic_flux) :: org_flux3
        org_flux3%cfmets1 = org_flux1%cfmets1 + org_flux2%cfmets1
        org_flux3%cfstrs1 = org_flux1%cfstrs1 + org_flux2%cfstrs1
        org_flux3%cfstrs2 = org_flux1%cfstrs2 + org_flux2%cfstrs2
        org_flux3%efmets1 = org_flux1%efmets1 + org_flux2%efmets1
        org_flux3%efstrs1 = org_flux1%efstrs1 + org_flux2%efstrs1
        org_flux3%efstrs2 = org_flux1%efstrs2 + org_flux2%efstrs2
        org_flux3%immmets1 = org_flux1%immmets1 + org_flux2%immmets1
        org_flux3%immstrs1 = org_flux1%immstrs1 + org_flux2%immstrs1
        org_flux3%immstrs2 = org_flux1%immstrs2 + org_flux2%immstrs2
        org_flux3%mnrmets1 = org_flux1%mnrmets1 + org_flux2%mnrmets1
        org_flux3%mnrstrs1 = org_flux1%mnrstrs1 + org_flux2%mnrstrs1
        org_flux3%mnrstrs1 = org_flux1%mnrstrs2 + org_flux2%mnrstrs2
        org_flux3%co2fmet = org_flux1%co2fmet + org_flux2%co2fmet
        org_flux3%co2fstr = org_flux1%co2fstr + org_flux2%co2fstr
        org_flux3%cfs1s2 = org_flux1%cfs1s2 + org_flux2%cfs1s2
        org_flux3%cfs1s3 = org_flux1%cfs1s3 + org_flux2%cfs1s3
        org_flux3%cfs2s1 = org_flux1%cfs2s1 + org_flux2%cfs2s1
        org_flux3%cfs2s3 = org_flux1%cfs2s3 + org_flux2%cfs2s3
        org_flux3%cfs3s1 = org_flux1%cfs3s1 + org_flux2%cfs3s1
        org_flux3%efs1s2 = org_flux1%efs1s2 + org_flux2%efs1s2
        org_flux3%efs1s3 = org_flux1%efs1s3 + org_flux2%efs1s3
        org_flux3%efs2s1 = org_flux1%efs2s1 + org_flux2%efs2s1
        org_flux3%efs2s3 = org_flux1%efs2s3 + org_flux2%efs2s3
        org_flux3%efs3s1 = org_flux1%efs3s1 + org_flux2%efs3s1
        org_flux3%imms1s2 = org_flux1%imms1s2 + org_flux2%imms1s2
        org_flux3%imms1s3 = org_flux1%imms1s3 + org_flux2%imms1s3
        org_flux3%imms2s1 = org_flux1%imms2s1 + org_flux2%imms2s1
        org_flux3%imms2s3 = org_flux1%imms2s3 + org_flux2%imms2s3
        org_flux3%imms3s1 = org_flux1%imms3s1 + org_flux2%imms3s1
        org_flux3%mnrs1s2 = org_flux1%mnrs1s2 + org_flux2%mnrs1s2
        org_flux3%mnrs1s3 = org_flux1%mnrs1s3 + org_flux2%mnrs1s3
        org_flux3%mnrs2s1 = org_flux1%mnrs2s1 + org_flux2%mnrs2s1
        org_flux3%mnrs2s3 = org_flux1%mnrs2s3 + org_flux2%mnrs2s3
        org_flux3%mnrs3s1 = org_flux1%mnrs3s1 + org_flux2%mnrs3s1
        org_flux3%co2fs1 = org_flux1%co2fs1 + org_flux2%co2fs1
        org_flux3%co2fs2 = org_flux1%co2fs2 + org_flux2%co2fs2
        org_flux3%co2fs3 = org_flux1%co2fs3 + org_flux2%co2fs3
      end function org_flux_add1
      
      end module organic_mineral_mass_module 