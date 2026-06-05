      subroutine proc_open

      use basin_module
      use netcdf_output_module, only: netcdf_output_init
      implicit none
      
      external :: header_aquifer, header_channel, header_const, header_hyd, header_lu_change, header_mgt, &
                  header_path, header_pest, header_reservoir, header_salt, header_sd_channel, header_snutc, &
                  header_water_allocation, header_wetland, header_write, header_yield, &
                  output_landscape_init, search

      !! write headers in output files (swatplus_perf: netcdf)
      if (pco%cdfout == "y") then
        call netcdf_output_init
        !! fort-leak-fix: suppress text-only outputs with no NetCDF backend (avoid fort.<N>)
        pco%mgtout = "n"
        pco%crop_yld = "n"
        pco%hydcon = "n"
        pco%fdcout = "n"
        pco%nb_bsn%d = "n"; pco%nb_bsn%m = "n"; pco%nb_bsn%y = "n"; pco%nb_bsn%a = "n"
        pco%ls_bsn%d = "n"; pco%ls_bsn%m = "n"; pco%ls_bsn%y = "n"; pco%ls_bsn%a = "n"
        pco%pw_bsn%d = "n"; pco%pw_bsn%m = "n"; pco%pw_bsn%y = "n"; pco%pw_bsn%a = "n"
        pco%aqu_bsn%d = "n"; pco%aqu_bsn%m = "n"; pco%aqu_bsn%y = "n"; pco%aqu_bsn%a = "n"
        pco%sd_chan_bsn%d = "n"; pco%sd_chan_bsn%m = "n"; pco%sd_chan_bsn%y = "n"; pco%sd_chan_bsn%a = "n"
        pco%nb_lsu%d = "n"; pco%nb_lsu%m = "n"; pco%nb_lsu%y = "n"; pco%nb_lsu%a = "n"
        pco%ls_lsu%d = "n"; pco%ls_lsu%m = "n"; pco%ls_lsu%y = "n"; pco%ls_lsu%a = "n"
        pco%pw_lsu%d = "n"; pco%pw_lsu%m = "n"; pco%pw_lsu%y = "n"; pco%pw_lsu%a = "n"
        pco%res%d = "n"; pco%res%m = "n"; pco%res%y = "n"; pco%res%a = "n"
        pco%hyd%d = "n"; pco%hyd%m = "n"; pco%hyd%y = "n"; pco%hyd%a = "n"
        pco%ru%d = "n"; pco%ru%m = "n"; pco%ru%y = "n"; pco%ru%a = "n"
      else
      call output_landscape_init
      call header_channel
      call header_aquifer
      call header_sd_channel
      call header_mgt
      call header_lu_change
      call header_yield
      call header_hyd
      call header_reservoir
      call header_wetland
      ! call header_snutc
      call header_water_allocation
      
      call header_pest
      call header_path
      call header_salt !rtb salt
      call header_const !rtb cs

      call header_write
      end if
           
      return
      
      end subroutine proc_open