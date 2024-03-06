     subroutine header_wetland
    
     use basin_module
     use reservoir_module
     use hydrograph_module
     
     implicit none

    !! RESERVOIR/WETLAND - DAILY
      if (pco%res%d == "y") then
        open (2548,file="wetland_day.txt",recl=1500)
        write (2548,*) bsn%name, prog
        write (9000,*) "RES_WET                   wetland_day.txt"
        write (2548,*) ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
        write (2548,*) ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
          if (pco%csvout == "y") then
            open (2552,file="wetland_day.csv",recl=1500)
            write (2552,*) bsn%name, prog
            write (2552,'(*(G0.3,:","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
            write (2552,'(*(G0.3,:","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
            write (9000,*) "RES_WET                   wetland_day.csv"
          end if
      end if
      
    !! RESERVOIR/WETLAND - MONTHLY
      if (pco%res%m == "y") then
        open (2549,file="wetland_mon.txt",recl=1500)
        write (2549,*) bsn%name, prog
        write (9000,*) "RES_WET                   wetland_mon.txt"
        write (2549,*) ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
        write (2549,*) ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
          if (pco%csvout == "y") then
            open (2553,file="wetland_mon.csv",recl=1500)
            write (2553,*) bsn%name, prog
            write (2553,'(*(G0.3,:","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
            write (2553,'(*(G0.3,:","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3 
            write (9000,*) "RES_WET                   wetland_mon.csv"
          end if
      end if
      
   !! RESERVOIR/WETLAND YEARLY
     if (pco%res%y == "y") then
        open (2550,file="wetland_yr.txt",recl=1500)
        write (2550,*) bsn%name, prog
        write (9000,*) "RES_WET                   wetland_yr.txt"
        write (2550,*) ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
        write (2550,*) ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
          if (pco%csvout == "y") then
            open (2554,file="wetland_yr.csv",recl=1500)
            write (2554,*) bsn%name, prog
            write (2554,'(*(G0.3,:","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
            write (2554,'(*(G0.3,:","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
            write (9000,*) "RES_WET                   wetland_yr.csv"
          end if
     end if
     
    
    !! RESERVOIR/WETLAND - AVERAGE ANNUAL   
      if (pco%res%a == "y") then
        open (2551,file="wetland_aa.txt",recl=1500)
        write (2551,*) bsn%name, prog
        write (2551,*) ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
        write (2551,*) ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3 
        write (9000,*) "RES_WET                   wetland_aa.txt"
          if (pco%csvout == "y") then
            open (2555,file="wetland_aa.csv",recl=1500)
            write (2555,*) bsn%name, prog
            write (2555,'(*(G0.3,:","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
            write (2555,'(*(G0.3,:","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
            !write (2555,'(*(G0.3,:","))') res_hdr_add
            write (9000,*) "RES_WET                   wetland_aa.csv"
          end if
      end if
    
      return
     end subroutine header_wetland