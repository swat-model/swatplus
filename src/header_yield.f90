     subroutine header_yield
    
     use basin_module
     use hydrograph_module
     use output_path_module
     
     implicit none 
    
!!  yield biomass file
      if (pco%mgtout == "y") then
        call open_output_file(4700, "yield.out", 800)
        write (9000,*) "YLD                       yield.out"
        if (pco%csvout == "y") then
          call open_output_file(4701, "yield.csv", 800)
          write (9000,*) "YLD                       yield.csv"
        end if
      end if  
      
      
!!! BASIN CROP YIELDS
      if (sp_ob%hru > 0 .and. (pco%crop_yld == "y" .or. pco%crop_yld == "b")) then
        call open_output_file(5100, "basin_crop_yld_yr.txt", 800)
        write (5100,*) bsn%name, prog
        write (5100,*) bsn_yld_hdr
        write (9000,*) "BASIN_CROP_YLD            basin_crop_yld_yr.txt"
        call open_output_file(5101, "basin_crop_yld_aa.txt", 800)
        write (5101,*) bsn%name, prog
        write (5101,*) bsn_yld_hdr
        write (9000,*) "BASIN_CROP_YLD            basin_crop_yld_aa.txt"
      end if
      
      return
      end subroutine header_yield  