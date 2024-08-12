      module wind_data_module
    
      implicit none
 
      type wind_erosion_factors
        real :: bare = 0.        !!t/d      |erosion from bare soil
        real :: veg = 0.         !!         |vegetation factor
        real :: rough = 0.       !!         |roughness factor
        real :: unshelt = 0.     !!         |unsheltered distance factor
        real :: erod = 0.        !!         |wind erodibility factor
      end type wind_erosion_factors
      type (wind_erosion_factors) :: wind_factors
            
      end module wind_data_module 