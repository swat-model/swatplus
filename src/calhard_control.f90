      subroutine calhard_control
    
      use aquifer_module
      use maximum_data_module 
      use hydrograph_module
      
      implicit none

      !! re-initialize all objects
      call re_initialize

      !! rerun model
      cal_sim = " hard calibration simulation "
      call time_control

      return
      end subroutine calhard_control