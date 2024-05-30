      !module for constituent data parameters (rtb cs)
      module cs_data_module
      
      implicit none
      
      ! constituent reactions (sorption, kinetics) - soils and aquifers
      type constituent_rct
        real :: kd_seo4 = 0.                                !! |           |sorption partition coefficient for seo4
        real :: kd_seo3 = 0.                                !! |           |sorption partition coefficient for seo3
        real :: kd_born = 0.                                !! |           |sorption partition coefficient for boron
        real :: kseo4 = 0.                                  !! |1/day      |first-order rate constant for seo4 reduction to seo3
        real :: kseo3 = 0.                                  !! |1/day      |first-order rate constant for seo3 reduction to elemental se
        real :: se_ino3                                     !! |           |selenium reduction inhibition factor
        real :: oxy_soil                                    !! |mg/L       |oxygen concentration in soil water
        real :: oxy_aqu                                     !! |mg/L       |oxygen concentration in groundwater
        real, dimension(:), allocatable :: shale            !! |           |fraction of object area that is occupied by shale formations (source of se)
        real, dimension(:), allocatable :: sseratio         !! |           |sulfur/se ratio in shale material
        real, dimension(:), allocatable :: ko2a             !! |1/day      |first-order rate constant for autotrophic reduction of dissolved oxygen
        real, dimension(:), allocatable :: kno3a            !! |1/day      |first-order rate constant for autotrophic reduction of no3
      end type constituent_rct
      
      !arrays for reaction parameters in soils (by HRU object) and aquifer (by aquifer object)
      type (constituent_rct), dimension (:), allocatable :: cs_rct_soil
      type (constituent_rct), dimension (:), allocatable :: cs_rct_aqu
      
      !arrays to hold reaction parameters
      real, dimension(:,:), allocatable :: rct       !reaction parameters
      real, dimension(:,:), allocatable :: rct_shale !reaction parameters for shale
      
      !number of geologic formations with shale
      integer :: num_geol_shale
      
      !plant boron tolerance parameters
      integer :: bor_tol_sim                                           !      |flag to simulate boron effect on plant growth
      real, dimension (:), allocatable :: bor_stress_a,bor_stress_b    !      |a and b parameters in boron relative yield equations
      
      end module cs_data_module 