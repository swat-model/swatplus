     subroutine header_reservoir
    
     use basin_module
     use reservoir_module
     !use hydrograph_module, only : res, sp_ob
     use hydrograph_module
     
     implicit none 

        open (7777,file="reservoir_sed.txt",recl=1500)
        
    !! RESERVOIR
      if (pco%res%d == "y" .and. sp_ob%res > 0 ) then
        open (2540,file="reservoir_day.txt",recl=1500)
        write (2540,*) bsn%name, prog
        write (9000,*) "RES                       reservoir_day.txt"
        write (2540,*) ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
        write (2540,*) ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
          if (pco%csvout == "y") then
            open (2544,file="reservoir_day.csv",recl=1500)
            write (2544,*) bsn%name, prog
            write (2544,'(*(G0.3,:","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
            write (2544,'(*(G0.3,:","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
            write (9000,*) "RES                       reservoir_day.csv"
          end if
      end if
      
     if (pco%res%m == "y" .and. sp_ob%res > 0 ) then
        open (2541,file="reservoir_mon.txt",recl=1500)
        write (2541,*) bsn%name, prog
        write (9000,*) "RES                       reservoir_mon.txt"
        write (2541,*) ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
        write (2541,*) ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
           if (pco%csvout == "y") then
            open (2545,file="reservoir_mon.csv",recl=1500)
            write (2545,*) bsn%name, prog
            write (2545,'(*(G0.3,:","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
            write (2545,'(*(G0.3,:","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
            write (2545,*) "RES                       reservoir_mon.csv"
          end if
     end if
     
     if (pco%res%y == "y" .and. sp_ob%res > 0 ) then
        open (2542,file="reservoir_yr.txt",recl=1500)
        write (2542,*) bsn%name, prog
        write (9000,*) "RES                       reservoir_yr.txt"
        write (2542,*) ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
        write (2542,*) ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
          if (pco%csvout == "y") then
            open (2546,file="reservoir_yr.csv",recl=1500)
            write (2546,*) bsn%name, prog
            write (2546,'(*(G0.3,:","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
            write (2546,'(*(G0.3,:","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
            write (9000,*) "RES                       reservoir_yr.csv"
          end if
      end if
      
      if (pco%res%a == "y" .and. sp_ob%res > 0) then
        open (2543,file="reservoir_aa.txt",recl = 1500)
        write (2543,*) bsn%name, prog
        write (2543,*) ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
        write (2543,*) ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
        write (9000,*) "RES                       reservoir_aa.txt"
          if (pco%csvout == "y") then
            open (2547,file="reservoir_aa.csv",recl=1500)
            write (2547,*) bsn%name, prog
            write (2547,'(*(G0.3,:","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
            write (2547,'(*(G0.3,:","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
            write (9000,*) "RES                       reservoir_aa.csv"
          end if
      end if
    
      return
      end subroutine header_reservoir  