      module soil_data_module
    
      implicit none
      
      type soil_lte_database
       character(len=16) :: texture
       real :: awc
       real :: por
       real :: scon
      end type soil_lte_database
      type (soil_lte_database), dimension(:), allocatable :: soil_lte
    
       type soiltest_db
        character(len=16) :: name = "default"
        real :: exp_co = .001         !	       |depth coefficient to adjust concentrations for depth
        real :: lab_p = 5.            !ppm     |labile P in soil surface
        real :: nitrate = 7.          !ppm     |nitrate N in soil surface
        real :: fr_hum_act = .02      !0-1     |fraction of soil humus that is active
        real :: hum_c_n = 10.         !ratio   |humus C:N ratio (range 8-12)
        real :: hum_c_p = 80.         !ratio   |humus C:P ratio (range 70-90)
        real :: inorgp = 0.5          !ppm     |inorganic P in soil surface - not currently used
        real :: watersol_p = .15      !ppm     |water soluble P in soil surface - not currently used  
        real :: h3a_p = .25           !ppm     |h3a P in soil surface - not currently used
        real :: mehlich_p = 1.2       !ppm     |Mehlich P in soil surface - not currently used
        real :: bray_strong_p = .85   !ppm     |Bray P in soil surface - not currently used
      end type soiltest_db
      type (soiltest_db), dimension (:), allocatable :: solt_db
          
!!!!!! OLD type 
      type soiltest_db_old
        character(len=16) :: name = "default"
        real :: exp_co = .001         !	       |depth coefficient to adjust concentrations for depth
        real :: totaln = 13.          !ppm     |total N in soil
        real :: inorgn = 6.           !ppm     |inorganic N in soil surface
        real :: orgn = 3.             !ppm     |organic N in soil surface
        real :: totalp = 3.           !ppm     |total P in soil surface
        real :: inorgp = 3.5          !ppm     |inorganic P in soil surface
        real :: orgp = .4             !ppm     |organic P in soil surface
        real :: watersol_p = .15      !ppm     |water soluble P in soil surface    
        real :: h3a_p = .25           !ppm     |h3a P in soil surface        
        real :: mehlich_p = 1.2       !ppm     |Mehlich P in soil surface
        real :: bray_strong_p = .85   !ppm     |Bray P in soil surface
      end type soiltest_db_old
!!!!!! OLD type 
      
    type soilayer_db
        real :: z = 1500.           !! mm             |depth to bottom of soil layer
        real :: bd = 1.3            !! Mg/m**3        |bulk density of the soil
        real :: awc = 0.2           !! mm H20/mm soil |available water capacity of soil layer
        real :: k = 10.0            !! mm/hr          |saturated hydraulic conductivity of soil layer. Index:(layer,HRU)
        real :: cbn = 2.0           !! %              |percent organic carbon in soil layer
        real :: clay = 10.          !! none           |fraction clay content in soil material (UNIT CHANGE!)
        real :: silt = 60.          !! %              |percent silt content in soil material 
        real :: sand = 30.          !! none           |fraction of sand in soil material
        real :: rock = 0.           !! %              |percent of rock fragments in soil layer      
        real :: alb = 0.1           !! none           |albedo when soil is moist
        real :: usle_k = 0.2        !!                |USLE equation soil erodibility (K) factor 
        real :: ec = 0.             !! dS/m           |electrical conductivity of soil layer
        real :: cal = 0.            !! %              |soil CaCo3
        real :: ph = 0.             !!                |soil Ph
      end type soilayer_db
      
      type soil_profile_db
        character(len=20) :: snam = " "       !! NA            |soil series name 
        integer ::  nly  = 1                  !! none          |number of soil layers  
        character(len=16) :: hydgrp = "A"     !! NA            |hydrologic soil group
        real :: zmx = 1500.                   !! mm            |maximum rooting depth
        real :: anion_excl = 0.5              !! none          |fraction of porosity from which anions are excluded
        real :: crk = 0.01                    !! none          |crack volume potential of soil
        character(len=16) :: texture = " "    !!               |texture of soil
      end type soil_profile_db
      
      type soil_database
       type (soil_profile_db) :: s
       type (soilayer_db), dimension(:), allocatable :: ly
      end type soil_database
      type (soil_database), dimension(:), allocatable :: soildb
                
      end module soil_data_module