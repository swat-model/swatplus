      module organic_mineral_mass_module
    
      implicit none 

      type organic_mass
        real :: m = 0.              !kg or kg/ha      |total object mass
        real :: c = 0.              !kg or kg/ha      |carbon mass
        real :: n = 0.              !kg or kg/ha      |organic nitrogen mass
        real :: p = 0.              !kg or kg/ha      |organic phosphorus mass
      end type organic_mass
      type (organic_mass) :: orgz

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
            
      type mineral_phosphorus
        real :: wsol = 0.           !kg/ha  |water soluble p dimensioned by layer
        real :: lab = 0.            !kg/ha  |labile p dimensioned by layer
        real :: act = 0.            !kg/ha  |active mineral p dimensioned by layer
        real :: sta = 0.            !kg/ha  |stable mineral p dimensioned by layer
      end type mineral_phosphorus
      
      type soil_profile_mass1
        character (len=16) :: name
        real :: tot_mn                                              !       |total mineral n pool (no3+nh4) in soil profile
        real :: tot_mp                                              !       |mineral p pool (wsol+lab+act+sta) in soil profile
        type (organic_mass) :: tot_org                              !       |total organics in soil profile
        real, dimension(:), allocatable :: sw                       !mm     |soil water dimensioned by layer
        real, dimension(:), allocatable :: cbn                      !%      |percent carbon
        type (sediment), dimension(:), allocatable :: sed           !       |sediment dimensioned by layer
        type (mineral_nitrogen), dimension(:), allocatable :: mn    !       |mineral n pool dimensioned by layer
        type (mineral_phosphorus), dimension(:), allocatable :: mp  !       |mineral p humus pool dimensioned by layer
        type (organic_mass), dimension(:), allocatable :: tot       !       |total organic pool dimensioned by layer
        type (organic_mass), dimension(:), allocatable :: hact      !       |active humus for old mineralization model dimensioned by layer
        type (organic_mass), dimension(:), allocatable :: hsta      !       |stable humus for old mineralization model dimensioned by layer
        type (organic_mass), dimension(:), allocatable :: hs        !       |slow humus dimensioned by layer
        type (organic_mass), dimension(:), allocatable :: hp        !       |passive humus dimensioned by layer
        type (organic_mass), dimension(:), allocatable :: rsd       !       |fresh residue-all plants in one pool by layer
        !! rest are used in CENTURY model
        type (organic_mass), dimension(:), allocatable :: microb    !       |microbial biomass
        type (organic_mass), dimension(:), allocatable :: str       !       |structural litter pool dimensioned by layer
        type (organic_mass), dimension(:), allocatable :: lig       !       |lignin pool dimensioned by layer
        type (organic_mass), dimension(:), allocatable :: meta      !       |metabolic litter pool dimensioned by layer
        type (organic_mass), dimension(:), allocatable :: man       !       |manure pool dimensioned by layer
        type (organic_mass), dimension(:), allocatable :: water     !       |water soluble
      end type soil_profile_mass1
      
      !soil profile object - dimensioned to number of hrus, using the hru pointer
      type (soil_profile_mass1), dimension(:), allocatable, target :: soil1
      type (soil_profile_mass1), dimension(:), allocatable :: soil1_init
      type (soil_profile_mass1), pointer :: s1
      type (organic_mass) :: soil_prof_tot                          !       |total litter pool
      type (organic_mass) :: soil_prof_hact                         !       |total litter pool
      type (organic_mass) :: soil_prof_hsta                         !       |total litter pool
      type (organic_mass) :: soil_prof_str                          !       |total litter pool
      type (organic_mass) :: soil_prof_lig                          !       |total litter pool
      type (organic_mass) :: soil_prof_meta                         !       |total litter pool
      type (organic_mass) :: soil_prof_man                          !       |total litter pool
      type (organic_mass) :: soil_prof_hs                           !       |total litter pool
      type (organic_mass) :: soil_prof_hp                           !       |total litter pool
      type (organic_mass) :: soil_prof_microb                       !       |total litter pool
      type (organic_mass) :: soil_prof_water                        !       |total litter pool
      type (organic_mass) :: soil_org_z                             !       |total litter pool
      type (mineral_nitrogen) :: soil_prof_mn                       !       |stable humus pool
      type (mineral_phosphorus) :: soil_prof_mp                     !       |active humus pool
      type (mineral_nitrogen) :: soil_mn_z
      type (mineral_phosphorus) :: soil_mp_z
      type (organic_mass) :: bsn_org_soil                           !       |total soil organics in basin
      type (organic_mass) :: bsn_org_pl                             !       |total plant organics in basin
      type (organic_mass) :: bsn_org_rsd                            !       |total residue organics in basin
      real :: bsn_mn                                                !       |total mineral n pool (no3+nh4) in soil profile
      real :: bsn_mp                                                !       |mineral p pool (wsol+lab+act+sta) in soil profile

      type residue_mass1
        character (len=16) :: name
        type (organic_mass), dimension(:), allocatable :: tot       !       |total mass surface residue litter pool-dimensioned by plant
        type (organic_mass), dimension(:), allocatable :: ls        !       |structural litter pool-dimensioned by plant
        type (organic_mass), dimension(:), allocatable :: lignin    !       |lignin pool-dimensioned by plant
        type (organic_mass), dimension(:), allocatable :: lm        !       |metabolic litter pool-dimensioned by plant
        type (organic_mass) :: tot_com                              !kg/ha  |total
        type (organic_mass) :: meta                                 !       |
        type (organic_mass) :: str                                  !       |
        type (organic_mass) :: lig                                  !       |
        type (organic_mass) :: bm                                   !       |microbial biomass pool
        type (organic_mass) :: man                                  !       |manure pool
      end type residue_mass1
      !soil profile object - dimensioned to number of hrus, using the hru pointer
      type (residue_mass1), dimension(:), allocatable :: rsd1
      type (residue_mass1), dimension(:), allocatable :: rsd1_init

      type plant_community_mass
       character(len=4) :: name
       type (organic_mass), dimension(:), allocatable :: tot        !kg/ha      |total biomass for individual plant in community
       type (organic_mass), dimension(:), allocatable :: ab_gr      !kg/ha      |above ground biomass for individual plant in community
       type (organic_mass), dimension(:), allocatable :: leaf       !kg/ha      |leaf mass for individual plant in community
       type (organic_mass), dimension(:), allocatable :: stem       !kg/ha      |wood/stalk mass for individual plant in community
       type (organic_mass), dimension(:), allocatable :: root       !kg/ha      |root mass for individual plant in community (by soil layer)
       type (organic_mass), dimension(:), allocatable :: seed       !kg/ha      |seed (grain) mass for individual plant in community
       type (organic_mass), dimension(:), allocatable :: yield_tot  !kg/ha      |running sum of yield at harvest
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
      type (organic_mass) :: leaf_drop
      type (organic_mass) :: plt_mass_z

      type organic_mineral_hydrograph1
        real :: vol = 0.                    ! m^3           |volume of water
        type (sediment) :: sed              !               |sediment
        type (organic_mass) :: algae        !               |algae
        type (organic_mass) :: biofilm      !               |biofilm
        real :: chla = 0.                   ! kg            |chlorophyll-a
        real :: cbod = 0.                   ! kg            |carbonaceous biological oxygen demand
        real :: dox = 0.                    ! kg            |dissolved oxygen
        real :: temp = 0.                   ! deg c         |temperature
      end type organic_mineral_hydrograph1
      
      !!end of new stuff
      
      
      type mineral_mass
        real :: m               !kg or kg/ha      |total object mass
        real :: no3             !kg or kg/ha      |nitrate mass
        real :: no2             !kg or kg/ha      |nitrite mass
        real :: nh4             !kg or kg/ha      |ammonium mass  
        real :: po4             !kg or kg/ha      |phosphate mass 
      end type mineral_mass

      type organic_mineral_mass
        real :: vol
        type (organic_mass) :: hum
        type (organic_mass) :: hum_act
        type (mineral_mass) :: min
      end type organic_mineral_mass
      
      type soil_profile_mass
        character (len=16) :: name
        type (organic_mineral_mass), dimension(:), allocatable :: sol       !soil matrix dimensioned by layer
        type (organic_mineral_mass), dimension(:), allocatable :: sw        !soil water dimensioned by layer
      end type soil_profile_mass
      !soil profile object - dimensioned to number of hrus, using the hru pointer
      !type (soil_profile_mass), dimension(:), allocatable :: soil
      
      !type plant_community_mass
      !  character (len=4) :: name                                !                 |same as plant_community object
      !  !live biomass
      !  type (organic_mass), dimension(:), allocatable :: tot    !kg/ha            |total biomass for individual plant in community
      !  type (organic_mass), dimension(:), allocatable :: veg    !kg/ha            |vegetative mass for individual plant in community
      !  type (organic_mass), dimension(:,:), allocatable :: root !kg/ha            |root mass for individual plant in community (by soil layer)
      !  type (organic_mass), dimension(:), allocatable :: grain  !kg/ha            |grain mass for individual plant in community
      !  type (organic_mass) :: tot_com                           !kg/ha            |total biomass for entire community
      !  type (organic_mass) :: veg_com                           !kg/ha            |vegetative mass for entire community
      !  type (organic_mass) :: root_com                          !kg/ha            |root mass for entire community
      !  type (organic_mass) :: grain_com                         !kg/ha            |grain mass for entire community
      !  !dead biomass - residue
      !  type (organic_mass), dimension(:,:), allocatable :: rsd  !kg/ha            |flat residue for individual plant in community (by soil layer)
      !  type (organic_mass), dimension(:), allocatable :: rsd_st !kg/ha            |standing residue for individual plant in community
      !end type plant_community_mass
      !plant community object - dimensioned to number of hrus, using the hru pointer
      !type (plant_community_mass), dimension(:), allocatable :: plnt

      !hru will point diretly to herds - managed in schedule_ops and ultimately can be managed in conditional subroutine
      !herds are different from soil and plant in that they can move from hru to hru
      type animal_herds
        character(len=16) :: name                                           !           |herd name (small_dairy, )
        integer :: num_tot                                                  !           |total number of animals in the herd
        type (organic_mass) :: herd_mass                                    !kg         |total mass of herd
        character(len=16), dimension(:), allocatable :: typ                 !           |animal type (points to animal.hrd)
        integer, dimension(:), allocatable :: num                           !           |number of each type of animal
        type (organic_mass), dimension(:), allocatable :: mass              !           |mass of each type of animal
        type (organic_mass), dimension(:), allocatable :: eat               !           |biomass eaten by each type of animal
        type (organic_mineral_mass), dimension(:), allocatable :: manure    !           |manure from each type of animal
      end type animal_herds
      
      !fertilizer object      
      type fertilizer_mass
        character (len=16) :: name
        type (mineral_mass) :: org       !soil matrix dimensioned by layer
        type (organic_mass) :: min       !soil water dimensioned by layer
      end type fertilizer_mass
      !fertilizer object should be used as database input from fert.dat
      type (fertilizer_mass), dimension(:), allocatable :: fert         !dimension to number of fertilzers in database
      
      !manure object should be used as database input from manure.dat
      type (organic_mineral_mass), dimension(:), allocatable :: manure  !dimension to number of manures in database
      
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
        character (len=16) :: name                                      !should match the object_connectivity object
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
         character (len=16) :: name
         integer :: num = 0                    !number of elements
         integer :: typ                        !recall type - 1=day, 2=mon, 3=year
         character(len=13) :: filename         !filename
         !hyd_output units are in cms and mg/L
         type (organic_mineral_hydrograph), dimension (:,:), allocatable :: hd_om     !export coefficients
      end type recall_organic_mineral_inputs
      type (recall_organic_mineral_inputs),dimension(:),allocatable:: rec_om

      !export coefficient and delivery ratio pesticides
      type (organic_mineral_hydrograph), dimension(:,:), allocatable :: exco_om
      
      !export coefficient and delivery ratio pesticides
      type (organic_mineral_hydrograph), dimension(:,:), allocatable :: dr_om
      
      type routing_unit_elements_hydrographs
        character (len=16) :: name                                   !should match the object_connectivity object
        type (organic_mineral_mass), dimension(:), allocatable :: hd
      end type routing_unit_elements_hydrographs
      !point to subbasin element objects - same as sub_elem
      type (routing_unit_elements_hydrographs), dimension(:), allocatable :: sub_e_hd
      
      type channel_surface_elements_hydrographs
        character (len=16) :: name                                   !should match the channel_surface_elements object
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
                  
      interface operator (+)
        module procedure nmin_add
        end interface 

    contains

      
      function nmin_add (nmin_m1, nmin_m2) result (nmin_m3)
        type (mineral_nitrogen), intent (in) :: nmin_m1
        type (mineral_nitrogen), intent (in) :: nmin_m2
        type (mineral_nitrogen) :: nmin_m3
        nmin_m3%no3 = nmin_m1%no3 + nmin_m2%no3
        nmin_m3%nh4 = nmin_m1%nh4 + nmin_m2%nh4
      end function nmin_add
      
      function pmin_add (pmin_m1, pmin_m2) result (pmin_m3)
        type (mineral_phosphorus), intent (in) :: pmin_m1
        type (mineral_phosphorus), intent (in) :: pmin_m2
        type (mineral_phosphorus) :: pmin_m3
        pmin_m3%wsol = pmin_m1%wsol + pmin_m2%wsol
        pmin_m3%lab = pmin_m1%lab + pmin_m2%lab
        pmin_m3%act = pmin_m1%act + pmin_m2%act
        pmin_m3%sta = pmin_m1%sta + pmin_m2%sta
      end function pmin_add

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
      

      
      end module organic_mineral_mass_module 