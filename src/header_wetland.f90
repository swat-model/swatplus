     subroutine header_wetland
    
     use basin_module
     use reservoir_module
     use hydrograph_module
     use output_path_module
     
     implicit none

    !! RESERVOIR/WETLAND - DAILY
      if (pco%res%d == "y") then
        call open_output_file(2548, "wetland_day.txt", 1500)
        write (2548,*) bsn%name, prog
        write (9000,*) "RES_WET                   wetland_day.txt"
        write (2548,*) ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
        write (2548,*) ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
          if (pco%csvout == "y") then
            call open_output_file(2552, "wetland_day.csv", 1500)
            write (2552,*) bsn%name, prog
            write (2552,'(*(G0.3,:","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
            write (2552,'(*(G0.3,:","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
            write (9000,*) "RES_WET                   wetland_day.csv"
          end if
      end if
      
    !! RESERVOIR/WETLAND - MONTHLY
      if (pco%res%m == "y") then
        call open_output_file(2549, "wetland_mon.txt", 1500)
        write (2549,*) bsn%name, prog
        write (9000,*) "RES_WET                   wetland_mon.txt"
        write (2549,*) ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
        write (2549,*) ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
          if (pco%csvout == "y") then
            call open_output_file(2553, "wetland_mon.csv", 1500)
            write (2553,*) bsn%name, prog
            write (2553,'(*(G0.3,:","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
            write (2553,'(*(G0.3,:","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3 
            write (9000,*) "RES_WET                   wetland_mon.csv"
          end if
      end if
      
   !! RESERVOIR/WETLAND YEARLY
     if (pco%res%y == "y") then
        call open_output_file(2550, "wetland_yr.txt", 1500)
        write (2550,*) bsn%name, prog
        write (9000,*) "RES_WET                   wetland_yr.txt"
        write (2550,*) ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
        write (2550,*) ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
          if (pco%csvout == "y") then
            call open_output_file(2554, "wetland_yr.csv", 1500)
            write (2554,*) bsn%name, prog
            write (2554,'(*(G0.3,:","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
            write (2554,'(*(G0.3,:","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
            write (9000,*) "RES_WET                   wetland_yr.csv"
          end if
     end if
     
    
    !! RESERVOIR/WETLAND - AVERAGE ANNUAL   
      if (pco%res%a == "y") then
        call open_output_file(2551, "wetland_aa.txt", 1500)
        write (2551,*) bsn%name, prog
        write (2551,*) ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
        write (2551,*) ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3 
        write (9000,*) "RES_WET                   wetland_aa.txt"
          if (pco%csvout == "y") then
            call open_output_file(2555, "wetland_aa.csv", 1500)
            write (2555,*) bsn%name, prog
            write (2555,'(*(G0.3,:","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
            write (2555,'(*(G0.3,:","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
            !write (2555,'(*(G0.3,:","))') res_hdr_add
            write (9000,*) "RES_WET                   wetland_aa.csv"
          end if
      end if
    
      return
     end subroutine header_wetland