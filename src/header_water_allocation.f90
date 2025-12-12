      subroutine header_water_allocation

      use maximum_data_module
      use water_allocation_module
      use basin_module
      use output_path_module
      
      implicit none 

!!!  Water Allocation Output
      if (db_mx%wallo_db > 0) then
        if (pco%water_allo%d == "y") then
          call open_output_file(3110, "water_allo_day.txt", 1500)
          write (3110,*) bsn%name, prog
          write (3110,*) wallo_hdr
          write (3110,*) wallo_hdr_units
          write (9000,*) "WATER_ALLOCATION          water_allo_day.txt"
          if (pco%csvout == "y") then
            call open_output_file(3114, "water_allo_day.csv", 1500)
            write (3114,*) bsn%name, prog
            write (3114,'(*(G0.3,:,","))') wallo_hdr
            write (3114,'(*(G0.3,:,","))') wallo_hdr_units
            write (9000,*) "WATER_ALLOCATION          water_allo_day.csv"
          end if
        endif
      endif
      
        if (db_mx%wallo_db > 0) then
          if (pco%water_allo%m == "y") then  
          call open_output_file(3111, "water_allo_mon.txt", 1500)
          write (3111,*) bsn%name, prog
          write (3111,*) wallo_hdr
          write (3111,*) wallo_hdr_units
          write (9000,*) "WATER_ALLOCATION          water_allo_mon.txt"
          if (pco%csvout == "y") then
            call open_output_file(3115, "water_allo_mon.csv", 1500)
            write (3115,*) bsn%name, prog
            write (3115,'(*(G0.3,:,","))') wallo_hdr
            write (3115,'(*(G0.3,:,","))') wallo_hdr_units
            write (9000,*) "WATER_ALLOCATION          water_allo_mon.csv"
          end if
          end if
         end if 
        
      if (db_mx%wallo_db > 0) then
        if (pco%water_allo%y == "y") then
          call open_output_file(3112, "water_allo_yr.txt", 1500)
          write (3112,*) bsn%name, prog
          write (3112,*) wallo_hdr
          write (3112,*) wallo_hdr_units
          write (9000,*) "WATER_ALLOCATION          water_allo_yr.txt"
          if (pco%csvout == "y") then
            call open_output_file(3116, "water_allo_yr.csv", 1500)
            write (3116,*) bsn%name, prog
            write (3116,'(*(G0.3,:,","))') wallo_hdr
            write (3116,'(*(G0.3,:,","))') wallo_hdr_units
            write (9000,*) "WATER_ALLOCATION          water_allo_yr.csv"
          end if
        endif
      endif
      
        if (db_mx%wallo_db > 0) then
          if (pco%water_allo%a == "y") then
          call open_output_file(3113, "water_allo_aa.txt", 1500)
          write (3113,*) bsn%name, prog
          write (3113,*) wallo_hdr
          write (3113,*) wallo_hdr_units
          write (9000,*) "WATER_ALLOCATION          water_allo_aa.txt"
          if (pco%csvout == "y") then
            call open_output_file(3117, "water_allo_aa.csv", 1500)
            write (3117,*) bsn%name, prog
            write (3117,'(*(G0.3,:,","))') wallo_hdr
            write (3117,'(*(G0.3,:,","))') wallo_hdr_units
            write (9000,*) "WATER_ALLOCATION          water_allo_aa.csv"
          end if
          end if
         end if 
       
      return
      end subroutine header_water_allocation