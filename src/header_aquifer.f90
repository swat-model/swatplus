      subroutine header_aquifer

      use aquifer_module
      use basin_module
      use hydrograph_module, only : sp_ob
      use output_path_module
      
      implicit none 
         
!!!  AQUIFER
       if (sp_ob%aqu > 0) then
        if (pco%aqu%d == "y") then
          call open_output_file(2520, "aquifer_day.txt", 1500)
          write (2520,*) bsn%name, prog
          write (2520,*) aqu_hdr !! aquifer
          write (2520,*) aqu_hdr_units
          write (9000,*) "AQUIFER                   aquifer_day.txt"
         if (pco%csvout == "y") then
            call open_output_file(2524, "aquifer_day.csv", 1500)
            write (2524,*) bsn%name, prog
            write (2524,'(*(G0.3,:,","))') aqu_hdr   !! aquifer csv
            write (2524,'(*(G0.3,:,","))') aqu_hdr_units
            write (9000,*) "AQUIFER                   aquifer_day.csv"
         end if
        endif
       endif
       
        if (sp_ob%aqu > 0) then
         if (pco%aqu%m == "y") then
          call open_output_file(2521, "aquifer_mon.txt", 1500)
          write (2521,*) bsn%name, prog
          write (2521,*) aqu_hdr   !! aquifer
          write (2521,*) aqu_hdr_units
          write (9000,*) "AQUIFER                   aquifer_mon.txt"
          if (pco%csvout == "y") then
            call open_output_file(2525, "aquifer_mon.csv", 1500)
            write (2525,*) bsn%name, prog
            write (2525,'(*(G0.3,:,","))') aqu_hdr   !! aquifer csv
            write (2525,'(*(G0.3,:,","))') aqu_hdr_units
            write (9000,*) "AQUIFER                   aquifer_mon.csv"
          end if
         end if
        end if

       if (sp_ob%aqu > 0) then
        if (pco%aqu%y == "y") then
          call open_output_file(2522, "aquifer_yr.txt", 1500)
          write (2522,*) bsn%name, prog
          write (2522,*) aqu_hdr !! aquifer
          write (2522,*) aqu_hdr_units
          write (9000,*) "AQUIFER                   aquifer_yr.txt"
         if (pco%csvout == "y") then
            call open_output_file(2526, "aquifer_yr.csv", 1500)
            write (2526,*) bsn%name, prog
            write (2526,'(*(G0.3,:,","))') aqu_hdr   !! aquifer csv
            write (2526,'(*(G0.3,:,","))') aqu_hdr_units
            write (9000,*) "AQUIFER                   aquifer_yr.csv"
         end if
        endif
       endif
       
        if (sp_ob%aqu > 0) then
         if (pco%aqu%a == "y") then
          call open_output_file(2523, "aquifer_aa.txt", 1500)
          write (2523,*) bsn%name, prog
          write (2523,*) aqu_hdr   !! aquifer
          write (2523,*) aqu_hdr_units
          write (9000,*) "AQUIFER                   aquifer_aa.txt"
          if (pco%csvout == "y") then
            call open_output_file(2527, "aquifer_aa.csv", 1500)
            write (2527,*) bsn%name, prog
            write (2527,'(*(G0.3,:,","))') aqu_hdr   !! aquifer csv
            write (2527,'(*(G0.3,:,","))') aqu_hdr_units
            write (9000,*) "AQUIFER                   aquifer_aa.csv"
          end if
         end if 
        end if 
                        
      return
      end subroutine header_aquifer