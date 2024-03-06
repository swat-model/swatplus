     subroutine header_path
    
     use basin_module
     use reservoir_module
     use output_ls_pathogen_module
     use constituent_mass_module
     implicit none 

    !! HRU_PATHOGEN - daily
      if (pco%wb_hru%d == "y" .and. cs_db%num_tot > 0) then
        open (2790,file="hru_path_day.txt",recl=800)
        write (2790,*) bsn%name, prog
        write (9000,*) "HRU_PATH                  hru_path_day.txt"
        write (2790,*) pathb_hdr

          if (pco%csvout == "y") then
            open (2794,file="hru_path_day.csv",recl=800)
            write (2794,*) bsn%name, prog
            write (2794,'(*(G0.3,:","))') pathb_hdr
            write (9000,*) "HRU_PATH                  hru_path_day.csv"
          end if
      end if
      
!! HRU_PATHOGEN - monthly
      if (pco%wb_hru%m == "y" .and. cs_db%num_tot > 0) then
        open (2791,file="hru_path_mon.txt",recl=800)
        write (2791,*) bsn%name, prog
        write (9000,*) "HRU_PATH                  hru_path_mon.txt"
        write (2791,*) pathb_hdr

          if (pco%csvout == "y") then
            open (2795,file="hru_path_mon.csv",recl=800)
            write (2795,*) bsn%name, prog
            write (2795,'(*(G0.3,:","))') pathb_hdr
            write (9000,*) "HRU_PATH                  hru_path_mon.csv"
          end if
      end if
      
!! HRU_PATHOGEN - yearly
      if (pco%wb_hru%y == "y" .and. cs_db%num_tot > 0) then
        open (2792,file="hru_path_yr.txt",recl=800)
        write (2792,*) bsn%name, prog
        write (9000,*) "HRU_PATH                  hru_path_yr.txt"
        write (2792,*) pathb_hdr

          if (pco%csvout == "y") then
            open (2796,file="hru_path_yr.csv",recl=800)
            write (2796,*) bsn%name, prog
            write (2796,'(*(G0.3,:","))') pathb_hdr
            write (9000,*) "HRU_PATH                  hru_path_yr.csv"
          end if
      end if
      
!! HRU_PATHOGEN - ave annual
      if (pco%wb_hru%a == "y" .and. cs_db%num_tot > 0) then
        open (2793,file="hru_path_aa.txt",recl=800)
        write (2793,*) bsn%name, prog
        write (9000,*) "HRU_PATH                  hru_path_aa.txt"
        write (2793,*) pathb_hdr

          if (pco%csvout == "y") then
            open (2797,file="hru_path_aa.csv",recl=800)
            write (2797,*) bsn%name, prog
            write (2797,'(*(G0.3,:","))') pathb_hdr
            write (9000,*) "HRU_PATH                  hru_path_aa.csv"
          end if
      end if
    
      return
     end subroutine header_path