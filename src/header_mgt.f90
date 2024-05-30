     subroutine header_mgt
    
     use basin_module
     
     implicit none 
!!   open mgt.out file 
      if (pco%mgtout == "y") then
        open (2612,file="mgt_out.txt",recl=800)
        write (2612,*) bsn%name, prog
        write (2612,*) mgt_hdr
        write (2612,*) mgt_hdr_unt1
        write (9000,*) "MGT                       mgt_out.txt"
      end if
          
      return
      end subroutine header_mgt  