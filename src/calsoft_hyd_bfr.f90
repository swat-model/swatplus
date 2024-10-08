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
      
      integer :: iter_all = 0  !none      |counter
      integer :: iterall = 0   !none      |counter

      ! calibrate hydrology
      iter_all = 1
        
      do iterall = 1, iter_all

        ! calibrate petco for actual ET
        ! start with half the range
        ls_prms(4)%neg = ls_prms(4)%neg / 2.
        ls_prms(4)%pos = ls_prms(4)%pos / 2.
        ls_prms(4)%lo = (1. - ls_prms(4)%lo) / 2. + ls_prms(4)%lo
        ls_prms(4)%up = ls_prms(4)%up - (ls_prms(4)%up - 1.) / 2.
        call calsoft_hyd_bfr_pet
        ! calibrate esco for actual ET
        call calsoft_hyd_bfr_et
        ! calibrate petco for actual ET
        ! allow full range
        ls_prms(4)%neg = 2. * ls_prms(4)%neg
        ls_prms(4)%pos = 2. * ls_prms(4)%pos
        ls_prms(4)%lo = ls_prms(4)%lo - (1. - ls_prms(4)%lo)
        ls_prms(4)%up = ls_prms(4)%up + (ls_prms(4)%up - 1.)
        call calsoft_hyd_bfr_pet

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