      subroutine proc_read
     
      use input_file_module
      
      implicit none

      integer :: date_time(8) = 0       !              |
      character*10 b(3)                 !              |

             
      call ch_read_temp
      call cli_read_atmodep

      ! Read traditional climate data files first (if not using NetCDF)
      if (in_cli%weat_sta /= "netcdf.ncw") then
            call DATE_AND_TIME (b(1), b(2), b(3), date_time)
            write (*,111) "reading from precipitation file    ", date_time(5), date_time(6), date_time(7)
            write (9003,111) "reading from precipitation file    ", date_time(5), date_time(6), date_time(7)
            call cli_pmeas      ! Reads precipitation files and sets db_mx%pcpfiles

            call DATE_AND_TIME (b(1), b(2), b(3), date_time)
            write (*,111) "reading from temperature file      ", date_time(5), date_time(6), date_time(7)
            write (9003,111) "reading from temperature file      ", date_time(5), date_time(6), date_time(7)
            call cli_tmeas      ! Reads temperature files and sets db_mx%tmpfiles  

            call DATE_AND_TIME (b(1), b(2), b(3), date_time)
            write (*,111) "reading from solar radiation file  ", date_time(5), date_time(6), date_time(7)
            write (9003,111) "reading from solar radiation file  ", date_time(5), date_time(6), date_time(7)
            call cli_smeas      ! Reads solar radiation files and sets db_mx%slrfiles

            call DATE_AND_TIME (b(1), b(2), b(3), date_time)    
            write (*,111) "reading from relative humidity file", date_time(5), date_time(6), date_time(7)
            write (9003,111) "reading from relative humidity file", date_time(5), date_time(6), date_time(7)
            call cli_hmeas      ! Reads humidity files and sets db_mx%rhfiles

            call DATE_AND_TIME (b(1), b(2), b(3), date_time)
            write (*,111) "reading from wind file             ", date_time(5), date_time(6), date_time(7)
            write (9003,111) "reading from wind file             ", date_time(5), date_time(6), date_time(7)
            call cli_wmeas      ! Reads wind files and sets db_mx%wndfiles

            call cli_petmeas    ! Reads PET files and sets db_mx%petfiles
            call cli_staread    ! Traditional: reads file names, uses search() to map to indices
      111   format (1x,a, 25x,"Time",2x,i2,":",i2,":",i2)
      else
          ! NetCDF path: read stations first, then climate data to set up arrays properly
          call cli_staread    ! NetCDF: reads scale factors, sets up wco_c%*gage names
          call cli_ncdf_meas  ! NetCDF: reads NetCDF data, populates climate arrays
      end if


      call constit_db_read
      call pest_metabolite_read     !! read pesticide metabolites
      call soil_plant_init
      call soil_plant_init_cs
      call solt_db_read
      call pest_hru_aqu_read
      call path_hru_aqu_read
      call hmet_hru_aqu_read
      
      !rtb salt
      call salt_hru_read
      call salt_aqu_read
      call salt_irr_read
      call salt_plant_read
      call cli_read_atmodep_salt
      call salt_roadsalt_read
      call salt_uptake_read
      call salt_urban_read
      call salt_fert_read

      !rtb cs
      call cs_hru_read
      call cs_aqu_read
      call cli_read_atmodep_cs
      call cs_irr_read
      call cs_plant_read
      call cs_uptake_read
      call cs_reactions_read
      call cs_urban_read
      call cs_fert_read
      
      call topo_read
      call field_read
      call hydrol_read
      
      call snowdb_read
      call soil_db_read
      call soil_lte_db_read
      
      return
      
      end subroutine proc_read