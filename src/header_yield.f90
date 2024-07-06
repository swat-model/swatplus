     subroutine header_yield
    
     use basin_module
     use hydrograph_module
     
     implicit none 
    
!!  yield biomass file
      if (pco%mgtout == "y") then
        open (4700,file="yield.out", recl=800)
        write (9000,*) "YLD                       yield.out"
        if (pco%csvout == "y") then
          open (4701,file="yield.csv", recl=800)
          write (9000,*) "YLD                       yield.csv"
        end if
      end if  
      
      
!!! BASIN CROP YIELDS
      if (sp_ob%hru > 0) then
        open (5100,file="basin_crop_yld_yr.txt", recl=800)
        write (5100,*) bsn%name, prog
        write (5100,*) bsn_yld_hdr
        write (9000,*) "BASIN_CROP_YLD            basin_crop_yld_yr.txt"
        open (5101,file="basin_crop_yld_aa.txt", recl=800)
        write (5101,*) bsn%name, prog
        write (5101,*) bsn_yld_hdr
        write (9000,*) "BASIN_CROP_YLD            basin_crop_yld_aa.txt"
      end if
      
      return
      end subroutine header_yield  