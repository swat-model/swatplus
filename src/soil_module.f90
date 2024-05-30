     module soil_module
    
     implicit none
     
       type soilayer
        real :: ec = 0.
        real :: cal = 0.
        real :: ph = 0.
        real :: alb = 0.         !! none          albedo when soil is moist
        real :: usle_k = 0.      !!               USLE equation soil erodibility (K) factor 
        real ::conk = 0.         !! mm/hr          lateral saturated hydraulic conductivity for each profile layer in a give HRU. 
        real ::flat = 0.         !! mm H2O         lateral flow storage array
        real :: prk = 0.         !! mm H2O         percolation from soil layer on current day
        real :: volcr = 0.       !! mm             crack volume for soil layer 
        real :: tillagef = 0. 
        real :: rtfr = 0.        !! none           root fraction
        real :: watp = 0.
        integer :: a_days = 0
        integer :: b_days = 0
        real :: psp_store = 0.
        real :: ssp_store = 0.    
        real :: percc = 0.       !!
        real :: latc = 0.        !!
        real :: vwt = 0.         !!
      end type soilayer
      type (soilayer), dimension(:), allocatable :: layer1
      
      type soil_physical_properties
        real :: d = 0.              !! mm           ! depth to bottom of soil layer
        real :: thick = 0.          !! mm           ! thichness of soil layer
        real :: bd = 0.             !! Mg/m**3      ! bulk density of the soil
        real :: k = 0.              !! mm/hr        ! saturated hydraulic conductivity of soil layer. Index:(layer,HRU)
        real :: clay = 0.           !! none         ! fraction clay content in soil material (UNIT CHANGE!)
        real :: silt = 0.           !! %            ! percent silt content in soil material
        real :: sand = 0.           !! none         ! fraction of sand in soil material
        real :: rock = 0.           !! %            ! percent of rock fragments in soil layer 
        real :: conv_wt = 0.        !! none         ! factor which converts kg/kg to kg/ha
        real :: crdep = 0.          !! mm           ! maximum or potential crack volume
        real :: awc = 0.            !! mm H20/mm    | soil available water capacity of soil layer
        real :: fc = 0.             !! mm H2O       | amount of water available to plants in soil layer at field capacity (fc - wp),Index:(layer,HRU)
        real :: hk = 0.             !! none         ! beta coefficent to calculate hydraulic conductivity
        real :: por = 0.            !! none         ! total porosity of soil layer expressed as a fraction of the total volume, Index:(layer,HRU)
        real :: st = 0.             !! mm H2O       ! amount of water stored in the soil layer on any given day (less wp water)
        real :: tmp = 0.            !! deg C        ! daily average temperature of second soil layer
        real :: ul = 0.             !! mm H2O       ! amount of water held in the soil layer at saturation (sat - wp water)
        real :: up = 0.             !! mm H2O/mm    ! soil water content of soil at -0.033 MPa (field capacity)
        real :: wp = 0.             !! mm H20/mm    ! soil water content of soil at -1.5 MPa (wilting point)
        real :: wpmm = 0.           !! mm H20       ! water content of soil at -1.5 MPa (wilting point)
      end type soil_physical_properties
      type (soil_physical_properties),dimension (:), allocatable:: phys1

      type soil_profile
        character(len=16) :: snam = ""     !! NA            soil series name  
        character(len=16) :: hydgrp = ""    !! NA            hydrologic soil group
        character(len=16) :: texture = ""
        integer ::  nly  = 0               !! none          number of soil layers 
        type (soil_physical_properties),dimension (:), allocatable:: phys
        type (soilayer), dimension (:), allocatable :: ly
        real, dimension(:),allocatable :: pest              !! kg/ha    total pesticide in the soil profile
        real :: zmx = 0.                   !! mm            maximum rooting depth in soil
        real :: anion_excl = 0.            !! none          fraction of porosity from which anions are excluded
        real :: crk = 0.                   !! none          crack volume potential of soil
        real :: alb = 0.                   !! none          albedo when soil is moist
        real :: usle_k = 0.                !!               USLE equation soil erodibility (K) factor 
        real :: det_san = 0.
        real :: det_sil = 0.
        real :: det_cla = 0.
        real :: det_sag = 0.
        real :: det_lag = 0.
        real :: sumul = 0.                 !! mm H2O         amount of water held in soil profile at saturation
        real :: sumfc = 0.                 !! mm H2O         amount of water held in the soil profile at field capacity                  
        real :: sw = 0.                    !! mm H2O         amount of water stored in soil profile
        real :: sw_300 = 0.                !! mm H2O         amount of water stored to 300 mm
        real :: sumwp = 0.                 !!
        real :: swpwt = 0.                 !!
        real :: ffc = 0.                   !! none           initial HRU soil water content expressed as fraction of field capacity
        real :: wat_tbl = 0.               !! 
        real :: avpor = 0.                 !! none           average porosity for entire soil profile
        real :: avbd = 0.                  !! Mg/m^3         average bulk density for soil profile
      end type soil_profile
      type (soil_profile), dimension(:), allocatable :: soil
      type (soil_profile), dimension(:), allocatable :: soil_init
      
      type soil_hru_database
         character(len=16) :: snam = ""     !! NA            soil series name  
         character(len=16) :: hydgrp = ""    !! NA            hydrologic soil group
         character(len=16) :: texture = ""
         type (soil_profile) :: s
         type (soil_physical_properties),dimension(:), allocatable::phys
         type (soilayer), dimension(:), allocatable :: ly
      end type soil_hru_database
      type (soil_hru_database), dimension(:), allocatable :: sol
      
     end module soil_module