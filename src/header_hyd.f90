     subroutine header_hyd
    
     use basin_module   
     use hydrograph_module
     use output_path_module
     
     implicit none 
      !! HYDCON (no headers)    
      if (pco%hydcon == "y") then
        call open_output_file(7000, "hydcon.out")
          write (9000,*) "HYDCON                    hydcon.out"
          if (pco%csvout == "y") then
            call open_output_file(7001, "hydcon.csv")
            write (9000,*) "HYDCON                    hydcon.csv"
          end if
      end if

      !! HYDOUT  
      if (pco%hyd%d == "y") then
        call open_output_file(2580, "hydout_day.txt", 800)
        write (2580,*) bsn%name, prog
        write (2580,*) hyd_hdr_time, hyd_hdr_obj, hyd_hdr
        write (2580,*) hyd_hdr_units2
        write (9000,*) "HYDOUT                    hydout_day.txt"
          if (pco%csvout == "y") then
            call open_output_file(2584, "hydout_day.csv", 800)
            write (2584,*) bsn%name, prog
            write (2584,'(*(G0.3,:","))') hyd_hdr_time, hyd_hdr_obj, hyd_hdr
            write (2584,'(*(G0.3,:","))') hyd_hdr_units2
            write (9000,*) "HYDOUT                    hydout_day.csv"
          end if
      end if
      
     if (pco%hyd%m == "y") then
        call open_output_file(2581, "hydout_mon.txt", 800)
        write (2581,*) bsn%name, prog
        write (2581,*) hyd_hdr_time, hyd_hdr_obj, hyd_hdr
        write (2581,*) hyd_hdr_units2
        write (9000,*) "HYDOUT                    hydout_mon.txt"
          if (pco%csvout == "y") then
            call open_output_file(2585, "hydout_mon.csv", 800)
            write (2585,*) bsn%name, prog
            write (2585,'(*(G0.3,:","))') hyd_hdr_time, hyd_hdr_obj, hyd_hdr
            write (2585,'(*(G0.3,:","))') hyd_hdr_units2
            write (9000,*) "HYDOUT                    hydout_mon.csv"
          end if
     end if
     
     if (pco%hyd%y == "y") then
        call open_output_file(2582, "hydout_yr.txt", 800)
        write (2582,*) bsn%name, prog
        write (2582,*) hyd_hdr_time, hyd_hdr_obj, hyd_hdr
        write (2582,*) hyd_hdr_units2
        write (9000,*) "HYDOUT                    hydout_yr.txt"
          if (pco%csvout == "y") then
            call open_output_file(2586, "hydout_yr.csv", 800)
            write (2586,*) bsn%name, prog
            write (2586,'(*(G0.3,:","))') hyd_hdr_time, hyd_hdr_obj, hyd_hdr
            write (2586,'(*(G0.3,:","))') hyd_hdr_units2
            write (9000,*)   "HYDOUT                    hydout_yr.csv"
          end if
     end if
     
     if (pco%hyd%a == "y") then
        call open_output_file(2583, "hydout_aa.txt", 800)
        write (2583,*) bsn%name, prog
        write (2583,*) hyd_hdr_time, hyd_hdr_obj, hyd_hdr
        write (2583,*) hyd_hdr_units2
        write (9000,*) "HYDOUT                    hydout_aa.txt"
          if (pco%csvout == "y") then
            call open_output_file(2587, "hydout_aa.csv", 800)
            write (2587,*) bsn%name, prog
            write (2587,'(*(G0.3,:","))') hyd_hdr_time, hyd_hdr_obj, hyd_hdr
            write (2587,'(*(G0.3,:","))') hyd_hdr_units2
            write (9000,*)   "HYDOUT                    hydout_aa.csv"
          end if
       end if
        

     !! HYDIN 
       if (pco%hyd%d == "y") then
        call open_output_file(2560, "hydin_day.txt", 800)
        write (2560,*) bsn%name, prog
        write (2560,*) hyd_hdr_time, hyd_hdr_obj, hyd_hdr
        write (2560,*) hyd_hdr_units2
        write (9000,*) "HYDIN                     hydin_day.txt"
          if (pco%csvout == "y") then
            call open_output_file(2564, "hydin_day.csv", 800)
            write (2564,*) bsn%name, prog
            write (2564,'(*(G0.3,:","))') hyd_hdr_time, hyd_hdr_obj, hyd_hdr
            write (2564,'(*(G0.3,:","))') hyd_hdr_units2
            write (9000,*) "HYDIN                     hydin_day.csv"
          end if
       endif
       
      if (pco%hyd%m == "y") then
        call open_output_file(2561, "hydin_mon.txt", 800)
        write (2561,*) bsn%name, prog
        write (2561,*) hyd_hdr_time, hyd_hdr_obj, hyd_hdr
        write (2561,*) hyd_hdr_units2
        write (9000,*) "HYDIN                     hydin_mon.txt"
          if (pco%csvout == "y") then
            call open_output_file(2565, "hydin_mon.csv", 800)
            write (2565,*) bsn%name, prog
            write (2565,'(*(G0.3,:","))') hyd_hdr_time, hyd_hdr_obj, hyd_hdr
            write (2565,'(*(G0.3,:","))') hyd_hdr_units2
            write (9000,*) "HYDIN                     hydin_mon.csv"
          end if
      endif
      
      if (pco%hyd%y == "y") then
        call open_output_file(2562, "hydin_yr.txt", 800)
        write (2562,*) bsn%name, prog
        write (2562,*) hyd_hdr_time, hyd_hdr_obj, hyd_hdr
        write (2562,*) hyd_hdr_units2
        write (9000,*) "HYDIN                     hydin_yr.txt"
          if (pco%csvout == "y") then
            call open_output_file(2566, "hydin_yr.csv", 800)
            write (2566,*) bsn%name, prog
            write (2566,'(*(G0.3,:","))') hyd_hdr_time, hyd_hdr_obj, hyd_hdr
            write (2566,'(*(G0.3,:","))') hyd_hdr_units2
            write (9000,*) "HYDIN                     hydin_yr.csv"
          end if
      endif
      
      if (pco%hyd%a == "y") then
        call open_output_file(2563, "hydin_aa.txt", 800)
        write (2563,*) bsn%name, prog
        write (2563,*) hyd_hdr_time, hyd_hdr_obj, hyd_hdr
        write (2563,*) hyd_hdr_units2
        write (9000,*) "HYDIN                     hydin_aa.txt"
          if (pco%csvout == "y") then
            call open_output_file(2567, "hydin_aa.csv", 800)
            write (2567,*) bsn%name, prog
            write (2567,'(*(G0.3,:","))') hyd_hdr_time, hyd_hdr_obj, hyd_hdr
            write (2567,'(*(G0.3,:","))') hyd_hdr_units2
            write (9000,*) "HYDIN                     hydin_aa.csv"
          end if
      endif
      
      !! hydrograph deposition - DAILY
      !if (pco%hyd%d == "y" .and. ob(icmd)%rcv_tot > 0) then
       if (pco%hyd%d == "y") then
        call open_output_file(2700, "deposition_day.txt", 800)
        write (2700,*) bsn%name, prog
        write (2700,*) hyd_hdr_time, hyd_hdr
        write (2700,*) hyd_hdr_units
        write (9000,*) "DEPO                      deposition_day.txt"
          if (pco%csvout == "y") then
            call open_output_file(2704, "deposition_day.csv", 800)
            write (2704,*) bsn%name, prog
            write (2704,'(*(G0.3,:","))') hyd_hdr_time, hyd_hdr
            write (2704,'(*(G0.3,:","))') hyd_hdr_units
            write (9000,*) "DEPO                      deposition_day.csv"
          end if
      end if
       
      !! hydrograph deposition - MONTHLY
      if (pco%hyd%m == "y") then
        call open_output_file(2701, "deposition_mon.txt", 800)
        write (2701,*) bsn%name, prog
        write (2701,*) hyd_hdr_time, hyd_hdr
        write (2701,*) hyd_hdr_units
        write (9000,*) "DEPO                      deposition_mon.txt"
          if (pco%csvout == "y") then
            call open_output_file(2705, "deposition_mon.csv", 800)
            write (2705,*) bsn%name, prog
            write (2705,'(*(G0.3,:","))') hyd_hdr_time, hyd_hdr
            write (2705,'(*(G0.3,:","))') hyd_hdr_units
            write (9000,*) "DEPO                      deposition_mon.csv"
          end if
       end if
       
      !! hydrograph deposition - YEARLY
       if (pco%hyd%y == "y") then
        call open_output_file(2702, "deposition_yr.txt", 800)
        write (2702,*) bsn%name, prog
        write (2702,*) hyd_hdr_time, hyd_hdr
        write (2702,*) hyd_hdr_units
        write (9000,*) "DEPO                      deposition_yr.txt"
          if (pco%csvout == "y") then
            call open_output_file(2706, "deposition_yr.csv", 800)
            write (2706,*) bsn%name, prog
            write (2706,'(*(G0.3,:","))') hyd_hdr_time, hyd_hdr
            write (2706,'(*(G0.3,:","))') hyd_hdr_units
            write (9000,*) "DEPO                      deposition_yr.csv"
          end if
       end if
       
      !! hydrograph deposition - ANNUAL
       if (pco%hyd%a == "y") then
        call open_output_file(2703, "deposition_aa.txt", 800)
        write (2703,*) bsn%name, prog
        write (2703,*) hyd_hdr_time, hyd_hdr
        write (2703,*) hyd_hdr_units
        write (9000,*) "DEPO                      deposition_aa.txt"
          if (pco%csvout == "y") then
            call open_output_file(2707, "deposition_aa.csv", 800)
            write (2707,*) bsn%name, prog
            write (2707,'(*(G0.3,:","))') hyd_hdr_time, hyd_hdr
            write (2707,'(*(G0.3,:","))') hyd_hdr_units
            write (9000,*) "DEPO                      deposition_aa.csv"
          end if
       end if
  
      return
      end subroutine header_hyd 