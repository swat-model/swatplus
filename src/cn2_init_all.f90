      subroutine cn2_init_all

      use soil_module
      use maximum_data_module
      use landuse_data_module
      use hydrograph_module, only : sp_ob
      
      implicit none
 
      integer :: j                   !none       |counter 
      
      !!assign topography and hyd parameters
      do j = 1, sp_ob%hru
        call cn2_init (j)
      end do
      
      return
      end subroutine cn2_init_all