      subroutine proc_bsn

      use time_module
      
      implicit none
      
!!!  open file to print all output files that are written
      open (9000,file="files_out.out")
      write (9000,*) "files_out.out - OUTPUT FILES WRITTEN"      
!!!  open diagnostics.out file to print problems with various files
     open (9001,file="diagnostics.out", recl=8000)  !!ext to 8000 recl per email 8/2/21 - Kai-Uwe
     write (9001,*) "DIAGNOSTICS.OUT FILE" 
!!!  open drainage areas output file
     open (9004,file="area_calc.out", recl=8000)
        		
      call basin_read_cc
      call basin_read_objs
      call time_read
      
      !if (time%step > 0) then
        time%dtm = 1440. / time%step
      !end if
      
      call readcio_read
             
      call basin_read_prm
      call basin_prm_default
      call basin_print_codes_read
      call co2_read
   
	  return
      
      end subroutine proc_bsn