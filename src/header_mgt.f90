     subroutine header_mgt
    
     use basin_module
     use output_path_module
     
     implicit none 
!!   open mgt.out file 
      if (pco%mgtout == "y") then
        call open_output_file(2612, "mgt_out.txt", 800)
        write (2612,*) bsn%name, prog
        write (2612,*) mgt_hdr
        write (2612,*) mgt_hdr_unt1
        write (9000,*) "MGT                       mgt_out.txt"
      end if
          
      return
      end subroutine header_mgt