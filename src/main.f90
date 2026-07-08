      program main

      use time_module
      use hydrograph_module
      use maximum_data_module
      use calibration_data_module

      implicit none
      
      integer :: date_time(8)           !              |
      character*10 b(3)                 !              |
      
      prog = " SWAT+ Apr 8 2022    MODULAR Rev 2020.60.5.4"

      write (*,1000)
      open (9003,file='simulation.out')
      write (9003,1000)
 1000 format(1x,"                  SWAT+               ",/,             &
     &          "             Revision 60.5.4          ",/,             &
     &          "      Soil & Water Assessment Tool    ",/,             &
     &          "               PC Version             ",/,             &
     &          "    Program reading . . . executing",/)

      call proc_bsn   
      call proc_date_time
      call proc_db
      call proc_read

      call hyd_connect
      call exco_db_read
      call dr_db_read
      
      call cli_lapse
      call object_read_output
      call water_rights_read

      call om_water_init
      call pest_cha_res_read
      call path_cha_res_read
      call salt_cha_res_read

      call lsu_read_elements        !defining landscape units by hru

      call proc_hru
      call proc_cha
      call proc_aqu
      
      !! read decision table data for conditional management
      call dtbl_lum_read
 
      call hru_lte_read

      call proc_cond
      call dtbl_res_read
      call dtbl_scen_read
      ! input scenarios used in simulation
      call cal_cond_read
            
      call dtbl_flocon_read
      call hru_dtbl_actions_init
            
      ! read reservoir and wetland data
      call proc_res
      call wet_read_hyd
      call wet_read
      if (db_mx%wet_dat > 0) call wet_initial
      if (bsn_cc%i_fpwet == 2) call wet_fp_init
      
      ! read water allocation file
      call water_allocation_read
      
      call proc_cal
      
      call proc_open
      
      ! compute unit hydrograph parameters for subdaily runoff
      call unit_hyd_ru_hru

      call dr_ru
        
      call hyd_connect_out
      
      ! save initial time settings for soft calibration runs
      time_init = time

      !! simulate watershed processes
      if (time%step < 0) then
        !! export coefficient - average annual
        time%end_sim = 1
        call command
      else
        call time_control
      end if
      
      if (cal_soft == "y") call calsoft_control
      
      if (cal_hard == "y") then
        deallocate (cal_upd)
        call cal_parmchg_read
        call calhard_control
      end if
           
      !! write successful completion to screen and file
      write (*,1001)
      write (9003,1001)
      open (107,file="success.fin")
  
      call DATE_AND_TIME (b(1), b(2), b(3), date_time)
      write (*,1234) date_time(2), date_time(3), date_time(1), date_time(5), date_time(6), date_time(7)
      write (9003,1234) date_time(2), date_time(3), date_time(1), date_time(5), date_time(6), date_time(7)
1234  format(/,"  Date of Sim", 2x,i2,"/",i2,"/",i4, " Time",2x,i2,":",i2,":",i2)
            
      write (107,1001)     
 1001 format (/," Execution successfully completed ")

	  stop      
      end