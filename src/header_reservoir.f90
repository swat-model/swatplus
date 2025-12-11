     subroutine header_reservoir
    
     use basin_module
     use reservoir_module
     !use hydrograph_module, only : res, sp_ob
     use hydrograph_module
     use output_path_module
     
     implicit none 

    !call open_output_file(7777, "reservoir_sed.txt", 1500)
        
    !! RESERVOIR
      if (pco%res%d == "y" .and. sp_ob%res > 0 ) then
        call open_output_file(2540, "reservoir_day.txt", 1500)
        write (2540,*) bsn%name, prog
        write (9000,*) "RES                       reservoir_day.txt"
        write (2540,*) ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
        write (2540,*) ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
          if (pco%csvout == "y") then
            call open_output_file(2544, "reservoir_day.csv", 1500)
            write (2544,*) bsn%name, prog
            write (2544,'(*(G0.3,:","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
            write (2544,'(*(G0.3,:","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
            write (9000,*) "RES                       reservoir_day.csv"
          end if
      end if
      
     if (pco%res%m == "y" .and. sp_ob%res > 0 ) then
        call open_output_file(2541, "reservoir_mon.txt", 1500)
        write (2541,*) bsn%name, prog
        write (9000,*) "RES                       reservoir_mon.txt"
        write (2541,*) ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
        write (2541,*) ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
           if (pco%csvout == "y") then
            call open_output_file(2545, "reservoir_mon.csv", 1500)
            write (2545,*) bsn%name, prog
            write (2545,'(*(G0.3,:","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
            write (2545,'(*(G0.3,:","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
            write (2545,*) "RES                       reservoir_mon.csv"
          end if
     end if
     
     if (pco%res%y == "y" .and. sp_ob%res > 0 ) then
        call open_output_file(2542, "reservoir_yr.txt", 1500)
        write (2542,*) bsn%name, prog
        write (9000,*) "RES                       reservoir_yr.txt"
        write (2542,*) ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
        write (2542,*) ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
          if (pco%csvout == "y") then
            call open_output_file(2546, "reservoir_yr.csv", 1500)
            write (2546,*) bsn%name, prog
            write (2546,'(*(G0.3,:","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
            write (2546,'(*(G0.3,:","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
            write (9000,*) "RES                       reservoir_yr.csv"
          end if
      end if
      
      if (pco%res%a == "y" .and. sp_ob%res > 0) then
        call open_output_file(2543, "reservoir_aa.txt", 1500)
        write (2543,*) bsn%name, prog
        write (2543,*) ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
        write (2543,*) ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
        write (9000,*) "RES                       reservoir_aa.txt"
          if (pco%csvout == "y") then
            call open_output_file(2547, "reservoir_aa.csv", 1500)
            write (2547,*) bsn%name, prog
            write (2547,'(*(G0.3,:","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
            write (2547,'(*(G0.3,:","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
            write (9000,*) "RES                       reservoir_aa.csv"
          end if
      end if
    
      return
      end subroutine header_reservoir  