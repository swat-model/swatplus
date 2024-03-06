      subroutine header_water_allocation

      use maximum_data_module
      use water_allocation_module
      use basin_module
      
      implicit none 

!!!  SWAT-DEG CHANNEL
      if (db_mx%wallo_db > 0) then
        if (pco%water_allo%d == "y") then
          open (3110,file="water_allo_day.txt",recl = 1500)
          write (3110,*) bsn%name, prog
          write (3110,*) wallo_hdr
          write (3110,*) wallo_hdr_units
          write (9000,*) "WATER_ALLOCATION          water_allo_day.txt"
          if (pco%csvout == "y") then
            open (3114,file="water_allo_day.csv",recl = 1500)
            write (3114,*) bsn%name, prog
            write (3114,'(*(G0.3,:,","))') wallo_hdr
            write (3114,'(*(G0.3,:,","))') wallo_hdr_units
            write (9000,*) "WATER_ALLOCATION          water_allo_day.csv"
          end if
        endif
      endif
      
        if (db_mx%wallo_db > 0) then
          if (pco%water_allo%m == "y") then  
          open (3111,file="water_allo_mon.txt",recl = 1500)
          write (3111,*) bsn%name, prog
          write (3111,*) wallo_hdr
          write (3111,*) wallo_hdr_units
          write (9000,*) "WATER_ALLOCATION          water_allo_mon.txt"
          if (pco%csvout == "y") then
            open (3115,file="water_allo_mon.csv",recl = 1500)
            write (3115,*) bsn%name, prog
            write (3115,'(*(G0.3,:,","))') wallo_hdr
            write (3115,'(*(G0.3,:,","))') wallo_hdr_units
            write (9000,*) "WATER_ALLOCATION          water_allo_mon.csv"
          end if
          end if
         end if 
        
      if (db_mx%wallo_db > 0) then
        if (pco%water_allo%y == "y") then
          open (3112,file="water_allo_yr.txt",recl = 1500)
          write (3112,*) bsn%name, prog
          write (3112,*) wallo_hdr
          write (3112,*) wallo_hdr_units
          write (9000,*) "WATER_ALLOCATION          water_allo_yr.txt"
          if (pco%csvout == "y") then
            open (3116,file="water_allo_yr.csv",recl = 1500)
            write (3116,*) bsn%name, prog
            write (3116,'(*(G0.3,:,","))') wallo_hdr
            write (3116,'(*(G0.3,:,","))') wallo_hdr_units
            write (9000,*) "WATER_ALLOCATION          water_allo_yr.csv"
          end if
        endif
      endif
      
        if (db_mx%wallo_db > 0) then
          if (pco%water_allo%a == "y") then
          open (3113,file="water_allo_aa.txt",recl = 1500)
          write (3113,*) bsn%name, prog
          write (3113,*) wallo_hdr
          write (3113,*) wallo_hdr_units
          write (9000,*) "WATER_ALLOCATION          water_allo_aa.txt"
          if (pco%csvout == "y") then
            open (3117,file="water_allo_aa.csv",recl = 1500)
            write (3117,*) bsn%name, prog
            write (3117,'(*(G0.3,:,","))') wallo_hdr
            write (3117,'(*(G0.3,:,","))') wallo_hdr_units
            write (9000,*) "WATER_ALLOCATION          water_allo_aa.csv"
          end if
          end if
         end if 
       
      return
      end subroutine header_water_allocation