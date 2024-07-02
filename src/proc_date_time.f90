      subroutine proc_date_time
     
      !use time_module, only : sim_start
       use time_module
  
      implicit none
     
      integer :: date_time(8)           !              |
      character*10 b(3)                 !              |
  
      !call cpu_time(sim_start)
      call DATE_AND_TIME (b(1), b(2), b(3), date_time)
      write (*,1234) date_time(2), date_time(3), date_time(1), date_time(5), date_time(6), date_time(7) 
      write (9003,1234) date_time(2), date_time(3), date_time(1), date_time(5), date_time(6), date_time(7) 
1234  format(/,"  Date of Sim", 2x,i2,"/",i2,"/",i4, " Time",2x,i2,":",i2,":",i2)
     
      write (*,111) "reading from pet file              ", date_time(5), date_time(6), date_time(7)
      write (9003,111) "reading from pet file              ", date_time(5), date_time(6), date_time(7)
      call DATE_AND_TIME (b(1), b(2), b(3), date_time)
      call cli_petmeas
      
      write (*,111) "reading from precipitation file    ", date_time(5), date_time(6), date_time(7)
      write (9003,111) "reading from precipitation file    ", date_time(5), date_time(6), date_time(7)
      call cli_pmeas
      write (*,111) "reading from temperature file      ", date_time(5), date_time(6), date_time(7)
      write (9003,111) "reading from temperature file      ", date_time(5), date_time(6), date_time(7)
      call DATE_AND_TIME (b(1), b(2), b(3), date_time)
      call cli_tmeas
      write (*,111) "reading from solar radiation file  ", date_time(5), date_time(6), date_time(7)
      write (9003,111) "reading from solar radiation file  ", date_time(5), date_time(6), date_time(7)
      call DATE_AND_TIME (b(1), b(2), b(3), date_time)
      call cli_smeas
      write (*,111) "reading from relative humidity file", date_time(5), date_time(6), date_time(7)
      write (9003,111) "reading from relative humidity file", date_time(5), date_time(6), date_time(7)
      call DATE_AND_TIME (b(1), b(2), b(3), date_time)
      call cli_hmeas
      write (*,111) "reading from wind file             ", date_time(5), date_time(6), date_time(7)
      write (9003,111) "reading from wind file             ", date_time(5), date_time(6), date_time(7)
      call DATE_AND_TIME (b(1), b(2), b(3), date_time)
      call cli_wmeas
      write (*,111) "reading from wgn file              ", date_time(5), date_time(6), date_time(7)
      write (9003,111) "reading from wgn file              ", date_time(5), date_time(6), date_time(7)
      call DATE_AND_TIME (b(1), b(2), b(3), date_time)
      call cli_wgnread
      write (*,111) "reading from wx station file       ", date_time(5), date_time(6), date_time(7)
      write (9003,111) "reading from wx station file       ", date_time(5), date_time(6), date_time(7)
      call DATE_AND_TIME (b(1), b(2), b(3), date_time)
      
111   format (1x,a, 5x,"Time",2x,i2,":",i2,":",i2)
      
      return
      
      end subroutine proc_date_time