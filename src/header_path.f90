     subroutine header_path
    
     use basin_module
     use reservoir_module
     use output_ls_pathogen_module
     use constituent_mass_module
     use output_path_module
     
     implicit none 

    !! HRU_PATHOGEN - daily
      if (pco%wb_hru%d == "y" .and. cs_db%num_tot > 0) then
        call open_output_file(2790, "hru_path_day.txt", 800)
        write (2790,*) bsn%name, prog
        write (9000,*) "HRU_PATH                  hru_path_day.txt"
        write (2790,*) pathb_hdr

          if (pco%csvout == "y") then
            call open_output_file(2794, "hru_path_day.csv", 800)
            write (2794,*) bsn%name, prog
            write (2794,'(*(G0.3,:","))') pathb_hdr
            write (9000,*) "HRU_PATH                  hru_path_day.csv"
          end if
      end if
      
!! HRU_PATHOGEN - monthly
      if (pco%wb_hru%m == "y" .and. cs_db%num_tot > 0) then
        call open_output_file(2791, "hru_path_mon.txt", 800)
        write (2791,*) bsn%name, prog
        write (9000,*) "HRU_PATH                  hru_path_mon.txt"
        write (2791,*) pathb_hdr

          if (pco%csvout == "y") then
            call open_output_file(2795, "hru_path_mon.csv", 800)
            write (2795,*) bsn%name, prog
            write (2795,'(*(G0.3,:","))') pathb_hdr
            write (9000,*) "HRU_PATH                  hru_path_mon.csv"
          end if
      end if
      
!! HRU_PATHOGEN - yearly
      if (pco%wb_hru%y == "y" .and. cs_db%num_tot > 0) then
        call open_output_file(2792, "hru_path_yr.txt", 800)
        write (2792,*) bsn%name, prog
        write (9000,*) "HRU_PATH                  hru_path_yr.txt"
        write (2792,*) pathb_hdr

          if (pco%csvout == "y") then
            call open_output_file(2796, "hru_path_yr.csv", 800)
            write (2796,*) bsn%name, prog
            write (2796,'(*(G0.3,:","))') pathb_hdr
            write (9000,*) "HRU_PATH                  hru_path_yr.csv"
          end if
      end if
      
!! HRU_PATHOGEN - ave annual
      if (pco%wb_hru%a == "y" .and. cs_db%num_tot > 0) then
        call open_output_file(2793, "hru_path_aa.txt", 800)
        write (2793,*) bsn%name, prog
        write (9000,*) "HRU_PATH                  hru_path_aa.txt"
        write (2793,*) pathb_hdr

          if (pco%csvout == "y") then
            call open_output_file(2797, "hru_path_aa.csv", 800)
            write (2797,*) bsn%name, prog
            write (2797,'(*(G0.3,:","))') pathb_hdr
            write (9000,*) "HRU_PATH                  hru_path_aa.csv"
          end if
      end if
    
      return
     end subroutine header_path