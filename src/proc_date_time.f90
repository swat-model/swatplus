      subroutine proc_date_time
     
      !use time_module, only : sim_start
       use time_module
       use input_file_module
  
      implicit none
     
      integer :: date_time(8) = 0       !              |
      character*10 b(3)                 !              |
  
      !call cpu_time(sim_start)
      call DATE_AND_TIME (b(1), b(2), b(3), date_time)
      write (*,1234) date_time(2), date_time(3), date_time(1), date_time(5), date_time(6), date_time(7) 
      write (9003,1234) date_time(2), date_time(3), date_time(1), date_time(5), date_time(6), date_time(7) 
1234  format(/," Date of Sim", 2x,i2,"/",i2,"/",i4, " Time",2x,i2,":",i2,":",i2)
     
      if (in_cli%weat_sta == "netcdf.ncw") then
            write(*,*) "simulation will use netcdf climate inputs"
            write(9003,*) "simulation will use netcdf climate inputs"

            write (*,*) "reading from wgn file              "
            write (9003,*) "reading from wgn file              "
            call DATE_AND_TIME (b(1), b(2), b(3), date_time)
            call cli_wgnread
      else
            ! climate data files are read in proc_read.f90 for traditional setup
            write (*,111) "reading from wgn file              ", date_time(5), date_time(6), date_time(7)
            write (9003,111) "reading from wgn file              ", date_time(5), date_time(6), date_time(7)
            call DATE_AND_TIME (b(1), b(2), b(3), date_time)
            call cli_wgnread
            write (*,111) "reading from wx station file       ", date_time(5), date_time(6), date_time(7)
            write (9003,111) "reading from wx station file       ", date_time(5), date_time(6), date_time(7)
            call DATE_AND_TIME (b(1), b(2), b(3), date_time)
            
      111   format (1x,a, 25x,"Time",2x,i2,":",i2,":",i2)
      endif
      return
      
      end subroutine proc_date_time