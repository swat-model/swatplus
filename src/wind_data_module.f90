      module wind_data_module
    
      implicit none
 
      type wind_erosion_factors
        real :: bare             !!t/d      |erosion from bare soil
        real :: veg              !!         |vegetation factor
        real :: rough            !!         |roughness factor
        real :: unshelt          !!         |unsheltered distance factor
        real :: erod             !!         |wind erodibility factor
      end type wind_erosion_factors
      type (wind_erosion_factors) :: wind_factors
            
      end module wind_data_module 