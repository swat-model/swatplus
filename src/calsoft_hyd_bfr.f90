      subroutine calsoft_hyd_bfr

      use soil_module
      use plant_module
      use hydrograph_module
      use ru_module
      use aquifer_module
      use channel_module
      use hru_lte_module
      use sd_channel_module
      use basin_module
      use maximum_data_module
      use calibration_data_module
      use conditional_module
      use reservoir_module
      use organic_mineral_mass_module
      use time_module
      
      implicit none
      
      integer :: iter_all      !none      |counter
      integer :: iterall       !none      |counter

      ! calibrate hydrology
      iter_all = 1
        
      do iterall = 1, iter_all

        ! calibrate harg_pet for potential ET
        call calsoft_hyd_bfr_et

        ! calibrate cn3_swf for surface runoff
        call calsoft_hyd_bfr_surq
        
        ! calibrate latq_co for lateral soil flow
        call calsoft_hyd_bfr_latq
        
        ! calibrate perco for percolation
        call calsoft_hyd_bfr_perc

        ! calibrate cn3_swf for surface runoff
        call calsoft_hyd_bfr_surq
        
      end do    ! iter_all loop
        
      !cal_codes%hyd_hru = "n"
      
	  return
      end subroutine calsoft_hyd_bfr