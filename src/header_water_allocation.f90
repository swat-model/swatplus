      subroutine header_water_allocation

      use maximum_data_module
      use water_allocation_module
      use basin_module
      use output_path_module
      
      implicit none 

!!!  Water Allocation Output
      if (pco%water_allo%d == "y") then
        call open_output_file(3110, "water_allo_day.txt", 1500)
        write (3110,*) bsn%name, prog
        write (3110,*) wallo_hdr
        write (3110,*) wallo_hdr_units
        write (9000,*) "WATER_ALLOCATION          water_allo_day.txt"
        if (pco%csvout == "y") then
          call open_output_file(3114, "water_allo_day.csv", 1500)
          write (3114,*) bsn%name, prog
          write (3114,'(*(G0.6,:,","))') wallo_hdr
          write (3114,'(*(G0.6,:,","))') wallo_hdr_units
          write (9000,*) "WATER_ALLOCATION          water_allo_day.csv"
        end if
      endif
      
      if (pco%water_allo%m == "y") then  
        call open_output_file(3111, "water_allo_mon.txt", 1500)
        write (3111,*) bsn%name, prog
        write (3111,*) wallo_hdr
        write (3111,*) wallo_hdr_units
        write (9000,*) "WATER_ALLOCATION          water_allo_mon.txt"
        if (pco%csvout == "y") then
          call open_output_file(3115, "water_allo_mon.csv", 1500)
          write (3115,*) bsn%name, prog
          write (3115,'(*(G0.6,:,","))') wallo_hdr
          write (3115,'(*(G0.6,:,","))') wallo_hdr_units
          write (9000,*) "WATER_ALLOCATION          water_allo_mon.csv"
        end if
      end if
        
      if (pco%water_allo%y == "y") then
        call open_output_file(3112, "water_allo_yr.txt", 1500)
        write (3112,*) bsn%name, prog
        write (3112,*) wallo_hdr
        write (3112,*) wallo_hdr_units
        write (9000,*) "WATER_ALLOCATION          water_allo_yr.txt"
        if (pco%csvout == "y") then
          call open_output_file(3116, "water_allo_yr.csv", 1500)
          write (3116,*) bsn%name, prog
          write (3116,'(*(G0.6,:,","))') wallo_hdr
          write (3116,'(*(G0.6,:,","))') wallo_hdr_units
          write (9000,*) "WATER_ALLOCATION          water_allo_yr.csv"
        end if
      endif
      
      if (pco%water_allo%a == "y") then
        call open_output_file(3113, "water_allo_aa.txt", 1500)
        write (3113,*) bsn%name, prog
        write (3113,*) wallo_hdr
        write (3113,*) wallo_hdr_units
        write (9000,*) "WATER_ALLOCATION          water_allo_aa.txt"
        if (pco%csvout == "y") then
          call open_output_file(3117, "water_allo_aa.csv", 1500)
          write (3117,*) bsn%name, prog
          write (3117,'(*(G0.6,:,","))') wallo_hdr
          write (3117,'(*(G0.6,:,","))') wallo_hdr_units
          write (9000,*) "WATER_ALLOCATION          water_allo_aa.csv"
        end if
      end if
      
!!!  Water Allocation Use Output
      if (pco%water_allo%d == "y") then
        call open_output_file(3118, "wallo_use_day.txt", 1500)
        write (3118,*) bsn%name, prog
        write (3118,*) wallo_use_hdr
        write (3118,*) wallo_use_hdr_units
        write (9000,*) "WATER_ALLOCATION          wallo_use_day.txt"
        if (pco%csvout == "y") then
          call open_output_file(3122, "wallo_use_day.csv", 1500)
          write (3122,*) bsn%name, prog
          write (3122,'(*(G0.6,:,","))') wallo_use_hdr
          write (3122,'(*(G0.6,:,","))') wallo_use_hdr_units
          write (9000,*) "WATER_ALLOCATION          wallo_use_day.csv"
        end if
      endif      
       
       if (pco%water_allo%m == "y") then
        call open_output_file(3119, "wallo_use_mon.txt", 1500)
        write (3119,*) bsn%name, prog
        write (3119,*) wallo_use_hdr
        write (3119,*) wallo_use_hdr_units
        write (9000,*) "WATER_ALLOCATION          wallo_use_mon.txt"
        if (pco%csvout == "y") then
          call open_output_file(3122, "wallo_use_mon.csv", 1500)
          write (3123,*) bsn%name, prog
          write (3123,'(*(G0.6,:,","))') wallo_use_hdr
          write (3123,'(*(G0.6,:,","))') wallo_use_hdr_units
          write (9000,*) "WATER_ALLOCATION          wallo_use_mon.csv"
        end if
      endif      
       
      if (pco%water_allo%y == "y") then
        call open_output_file(3120, "wallo_use_yr.txt", 1500)
        write (3120,*) bsn%name, prog
        write (3120,*) wallo_use_hdr
        write (3120,*) wallo_use_hdr_units
        write (9000,*) "WATER_ALLOCATION          wallo_use_yr.txt"
        if (pco%csvout == "y") then
          call open_output_file(3122, "wallo_use_yr.csv", 1500)
          write (3124,*) bsn%name, prog
          write (3124,'(*(G0.6,:,","))') wallo_use_hdr
          write (3124,'(*(G0.6,:,","))') wallo_use_hdr_units
          write (9000,*) "WATER_ALLOCATION          wallo_use_yr.csv"
        end if
      endif      
       
      if (pco%water_allo%a == "y") then
        call open_output_file(3121, "wallo_use_aa.txt", 1500)
        write (3121,*) bsn%name, prog
        write (3121,*) wallo_use_hdr
        write (3121,*) wallo_use_hdr_units
        write (9000,*) "WATER_ALLOCATION          wallo_use_aa.txt"
        if (pco%csvout == "y") then
          call open_output_file(3122, "wallo_use_aa.csv", 1500)
          write (3125,*) bsn%name, prog
          write (3125,'(*(G0.6,:,","))') wallo_use_hdr
          write (3125,'(*(G0.6,:,","))') wallo_use_hdr_units
          write (9000,*) "WATER_ALLOCATION          wallo_use_aa.csv"
        end if
      endif      
      
  !!!  Water Allocation Treat Output
      if (pco%water_allo%d == "y") then
        call open_output_file(3130, "wallo_treat_day.txt", 1500)
        write (3130,*) bsn%name, prog
        write (3130,*) wallo_use_hdr
        write (3130,*) wallo_use_hdr_units
        write (9000,*) "WATER_ALLOCATION          wallo_treat_day.txt"
        if (pco%csvout == "y") then
          call open_output_file(3134, "wallo_treat_day.csv", 1500)
          write (3134,*) bsn%name, prog
          write (3134,'(*(G0.6,:,","))') wallo_use_hdr
          write (3134,'(*(G0.6,:,","))') wallo_use_hdr_units
          write (9000,*) "WATER_ALLOCATION          wallo_treat_day.csv"
        end if
      endif      
       
       if (pco%water_allo%m == "y") then
        call open_output_file(3131, "wallo_treat_mon.txt", 1500)
        write (3131,*) bsn%name, prog
        write (3131,*) wallo_use_hdr
        write (3131,*) wallo_use_hdr_units
        write (9000,*) "WATER_ALLOCATION          wallo_treat_mon.txt"
        if (pco%csvout == "y") then
          call open_output_file(3135, "wallo_treat_mon.csv", 1500)
          write (3135,*) bsn%name, prog
          write (3135,'(*(G0.6,:,","))') wallo_use_hdr
          write (3135,'(*(G0.6,:,","))') wallo_use_hdr_units
          write (9000,*) "WATER_ALLOCATION          wallo_treat_mon.csv"
        end if
      endif      
       
      if (pco%water_allo%y == "y") then
        call open_output_file(3132, "wallo_treat_yr.txt", 1500)
        write (3132,*) bsn%name, prog
        write (3132,*) wallo_use_hdr
        write (3132,*) wallo_use_hdr_units
        write (9000,*) "WATER_ALLOCATION          wallo_treat_yr.txt"
        if (pco%csvout == "y") then
          call open_output_file(3126, "wallo_treat_yr.csv", 1500)
          write (3136,*) bsn%name, prog
          write (3136,'(*(G0.6,:,","))') wallo_use_hdr
          write (3136,'(*(G0.6,:,","))') wallo_use_hdr_units
          write (9000,*) "WATER_ALLOCATION          wallo_treat_yr.csv"
        end if
      endif      
       
      if (pco%water_allo%a == "y") then
        call open_output_file(3133, "wallo_treat_aa.txt", 1500)
        write (3133,*) bsn%name, prog
        write (3133,*) wallo_use_hdr
        write (3133,*) wallo_use_hdr_units
        write (9000,*) "WATER_ALLOCATION          wallo_treat_aa.txt"
        if (pco%csvout == "y") then
          call open_output_file(3137, "wallo_treat_aa.csv", 1500)
          write (3137,*) bsn%name, prog
          write (3137,'(*(G0.6,:,","))') wallo_use_hdr
          write (3137,'(*(G0.6,:,","))') wallo_use_hdr_units
          write (9000,*) "WATER_ALLOCATION          wallo_treat_aa.csv"
        end if
      endif          
       
      return
      end subroutine header_water_allocation