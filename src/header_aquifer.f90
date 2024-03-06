      subroutine header_aquifer

      use aquifer_module
      use basin_module
      use hydrograph_module, only : sp_ob
      implicit none 
         
!!!  AQUIFER
       if (sp_ob%aqu > 0) then
        if (pco%aqu%d == "y") then
          open (2520,file="aquifer_day.txt",recl = 1500)
          write (2520,*) bsn%name, prog
          write (2520,*) aqu_hdr !! aquifer
          write (2520,*) aqu_hdr_units
          write (9000,*) "AQUIFER                   aquifer_day.txt"
         if (pco%csvout == "y") then
            open (2524,file="aquifer_day.csv",recl = 1500)
            write (2524,*) bsn%name, prog
            write (2524,'(*(G0.3,:,","))') aqu_hdr   !! aquifer csv
            write (2524,'(*(G0.3,:,","))') aqu_hdr_units
            write (9000,*) "AQUIFER                   aquifer_day.csv"
         end if
        endif
       endif
       
        if (sp_ob%aqu > 0) then
         if (pco%aqu%m == "y") then
          open (2521,file="aquifer_mon.txt",recl = 1500)
          write (2521,*) bsn%name, prog
          write (2521,*) aqu_hdr   !! aquifer
          write (2521,*) aqu_hdr_units
          write (9000,*) "AQUIFER                   aquifer_mon.txt"
          if (pco%csvout == "y") then
            open (2525,file="aquifer_mon.csv",recl = 1500)
            write (2525,*) bsn%name, prog
            write (2525,'(*(G0.3,:,","))') aqu_hdr   !! aquifer csv
            write (2525,'(*(G0.3,:,","))') aqu_hdr_units
            write (9000,*) "AQUIFER                   aquifer_mon.csv"
          end if
         end if
        end if

       if (sp_ob%aqu > 0) then
        if (pco%aqu%y == "y") then
          open (2522,file="aquifer_yr.txt",recl = 1500)
          write (2522,*) bsn%name, prog
          write (2522,*) aqu_hdr !! aquifer
          write (2522,*) aqu_hdr_units
          write (9000,*) "AQUIFER                   aquifer_yr.txt"
         if (pco%csvout == "y") then
            open (2526,file="aquifer_yr.csv",recl = 1500)
            write (2526,*) bsn%name, prog
            write (2526,'(*(G0.3,:,","))') aqu_hdr   !! aquifer csv
            write (2526,'(*(G0.3,:,","))') aqu_hdr_units
            write (9000,*) "AQUIFER                   aquifer_yr.csv"
         end if
        endif
       endif
       
        if (sp_ob%aqu > 0) then
         if (pco%aqu%a == "y") then
          open (2523,file="aquifer_aa.txt",recl = 1500)
          write (2523,*) bsn%name, prog
          write (2523,*) aqu_hdr   !! aquifer
          write (2523,*) aqu_hdr_units
          write (9000,*) "AQUIFER                   aquifer_aa.txt"
          if (pco%csvout == "y") then
            open (2527,file="aquifer_aa.csv",recl = 1500)
            write (2527,*) bsn%name, prog
            write (2527,'(*(G0.3,:,","))') aqu_hdr   !! aquifer csv
            write (2527,'(*(G0.3,:,","))') aqu_hdr_units
            write (9000,*) "AQUIFER                   aquifer_aa.csv"
          end if
         end if 
        end if 
                        
      return
      end subroutine header_aquifer