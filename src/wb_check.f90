      subroutine wb_check
    
      use basin_module
      use hydrograph_module
      use hru_module
      use soil_module
      
      integer :: j = 0                  !none		   |counter
      
      !! checker.out file    
      open (4000,file = "checker.out",recl=1200)
      write (4000,*) bsn%name, prog
      write (4000,*) chk_hdr
      write (4000,*) chk_unit
      write (9000,*) "CHK                       checker.out"

      do j = 1, sp_ob%hru
         write (4000,100) soil(j)%snam, soil(j)%hydgrp, soil(j)%zmx, soil(j)%usle_k, soil(j)%sumfc,  &
            soil(j)%sumul, hru(j)%lumv%usle_p, hru(j)%lumv%usle_ls, hru(j)%hyd%esco, hru(j)%hyd%epco,      &
            hru(j)%hyd%cn3_swf, hru(j)%hyd%perco, hru(j)%hyd%latq_co, hru(j)%tiledrain  
      end do
    !! checker.out file
      
100   format(2a16,11f12.4,i12)
      
	  return      
      end