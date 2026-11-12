      subroutine header_water_allocation

      use maximum_data_module
      use water_allocation_module
      use basin_module
      use output_path_module
      
      implicit none 

!!!  Water Allocation MET Output
      if (pco%water_allo%d == "y") then
        call open_output_file(3410, "wallo_met_day.txt", 1500)
        write (3410,*) bsn%name, prog
        write (3410,*) wallo_met_hdr
        write (3410,*) wallo_met_hdr_units
        write (9000,*) "WATER_ALLOCATION          wallo_met_day.txt"
        if (pco%csvout == "y") then
          call open_output_file(3414, "wallo_met_day.csv", 1500)
          write (3414,*) bsn%name, prog
          write (3414,'(*(G0.6,:,","))') wallo_met_hdr
          write (3414,'(*(G0.6,:,","))') wallo_met_hdr_units
          write (9000,*) "WATER_ALLOCATION          wallo_met_day.csv"
        end if
      endif
      
      if (pco%water_allo%m == "y") then  
        call open_output_file(3411, "wallo_met_mon.txt", 1500)
        write (3411,*) bsn%name, prog
        write (3411,*) wallo_met_hdr
        write (3411,*) wallo_met_hdr_units
        write (9000,*) "WATER_ALLOCATION          wallo_met_mon.txt"
        if (pco%csvout == "y") then
          call open_output_file(3415, "wallo_met_mon.csv", 1500)
          write (3415,*) bsn%name, prog
          write (3415,'(*(G0.6,:,","))') wallo_met_hdr
          write (3415,'(*(G0.6,:,","))') wallo_met_hdr_units
          write (9000,*) "WATER_ALLOCATION          wallo_met_mon.csv"
        end if
      end if
        
      if (pco%water_allo%y == "y") then
        call open_output_file(3412, "wallo_met_yr.txt", 1500)
        write (3412,*) bsn%name, prog
        write (3412,*) wallo_met_hdr
        write (3412,*) wallo_met_hdr_units
        write (9000,*) "WATER_ALLOCATION          wallo_met_yr.txt"
        if (pco%csvout == "y") then
          call open_output_file(3416, "wallo_met_yr.csv", 1500)
          write (3416,*) bsn%name, prog
          write (3416,'(*(G0.6,:,","))') wallo_met_hdr
          write (3416,'(*(G0.6,:,","))') wallo_met_hdr_units
          write (9000,*) "WATER_ALLOCATION          wallo_met_yr.csv"
        end if
      endif
      
      if (pco%water_allo%a == "y") then
        call open_output_file(3413, "wallo_met_aa.txt", 1500)
        write (3413,*) bsn%name, prog
        write (3413,*) wallo_met_hdr
        write (3413,*) wallo_met_hdr_units
        write (9000,*) "WATER_ALLOCATION          wallo_met_aa.txt"
        if (pco%csvout == "y") then
          call open_output_file(3417, "wallo_met_aa.csv", 1500)
          write (3417,*) bsn%name, prog
          write (3417,'(*(G0.6,:,","))') wallo_met_hdr
          write (3417,'(*(G0.6,:,","))') wallo_met_hdr_units
          write (9000,*) "WATER_ALLOCATION          wallo_met_aa.csv"
        end if
      end if
!!!  Water Allocation MET Output
      
!!!  Water Allocation POU POD Output
      if (pco%water_allo%d == "y") then
        call open_output_file(3418, "wallo_pou_pods_day.txt", 1500)
        write (3418,*) bsn%name, prog
        write (3418,*) wallo_poupod_hdr
        write (3418,*) wallo_poupod_hdr_units
        write (9000,*) "WATER_ALLOCATION          wallo_pou_pods_day.txt"
        if (pco%csvout == "y") then
          call open_output_file(3422, "wallo_pou_pods_day.csv", 1500)
          write (3422,*) bsn%name, prog
          write (3422,'(*(G0.6,:,","))') wallo_poupod_hdr
          write (3422,'(*(G0.6,:,","))') wallo_poupod_hdr_units
          write (9000,*) "WATER_ALLOCATION          wallo_pou_pods_day.csv"
        end if
      endif
      
      if (pco%water_allo%m == "y") then  
        call open_output_file(3419, "wallo_pou_pods_mon.txt", 1500)
        write (3419,*) bsn%name, prog
        write (3419,*) wallo_poupod_hdr
        write (3419,*) wallo_poupod_hdr_units
        write (9000,*) "WATER_ALLOCATION          wallo_pou_pods_mon.txt"
        if (pco%csvout == "y") then
          call open_output_file(3423, "wallo_pou_pods_mon.csv", 1500)
          write (3423,*) bsn%name, prog
          write (3423,'(*(G0.6,:,","))') wallo_poupod_hdr
          write (3423,'(*(G0.6,:,","))') wallo_poupod_hdr_units
          write (9000,*) "WATER_ALLOCATION          wallo_pou_pods_mon.csv"
        end if
      end if
        
      if (pco%water_allo%y == "y") then
        call open_output_file(3420, "wallo_pou_pods_yr.txt", 1500)
        write (3420,*) bsn%name, prog
        write (3420,*) wallo_poupod_hdr
        write (3420,*) wallo_poupod_hdr_units
        write (9000,*) "WATER_ALLOCATION          wallo_pou_pods_yr.txt"
        if (pco%csvout == "y") then
          call open_output_file(3424, "wallo_pou_pods_yr.csv", 1500)
          write (3424,*) bsn%name, prog
          write (3424,'(*(G0.6,:,","))') wallo_poupod_hdr
          write (3424,'(*(G0.6,:,","))') wallo_poupod_hdr_units
          write (9000,*) "WATER_ALLOCATION          wallo_pou_pods_yr.csv"
        end if
      endif
      
      if (pco%water_allo%a == "y") then
        call open_output_file(3421, "wallo_pou_pods_aa.txt", 1500)
        write (3421,*) bsn%name, prog
        write (3421,*) wallo_poupod_hdr
        write (3421,*) wallo_poupod_hdr_units
        write (9000,*) "WATER_ALLOCATION          wallo_pou_pods_aa.txt"
        if (pco%csvout == "y") then
          call open_output_file(3425, "wallo_pou_pods_aa.csv", 1500)
          write (3425,*) bsn%name, prog
          write (3425,'(*(G0.6,:,","))') wallo_poupod_hdr
          write (3425,'(*(G0.6,:,","))') wallo_poupod_hdr_units
          write (9000,*) "WATER_ALLOCATION          wallo_pou_pods_aa.csv"
        end if
      end if
!!!  Water Allocation POU POD Output
      
!!!  Water Allocation POU POR Output
      if (pco%water_allo%d == "y") then
        call open_output_file(3426, "wallo_pou_pors_day.txt", 1500)
        write (3426,*) bsn%name, prog
        write (3426,*) wallo_poupor_hdr
        write (3426,*) wallo_poupor_hdr_units
        write (9000,*) "WATER_ALLOCATION          wallo_pou_pors_day.txt"
        if (pco%csvout == "y") then
          call open_output_file(3430, "wallo_pou_pors_day.csv", 1500)
          write (3430,*) bsn%name, prog
          write (3430,'(*(G0.6,:,","))') wallo_poupor_hdr
          write (3430,'(*(G0.6,:,","))') wallo_poupor_hdr_units
          write (9000,*) "WATER_ALLOCATION          wallo_pou_pors_day.csv"
        end if
      endif
      
      if (pco%water_allo%m == "y") then  
        call open_output_file(3427, "wallo_pou_pors_mon.txt", 1500)
        write (3427,*) bsn%name, prog
        write (3427,*) wallo_poupor_hdr
        write (3427,*) wallo_poupor_hdr_units
        write (9000,*) "WATER_ALLOCATION          wallo_pou_pors_mon.txt"
        if (pco%csvout == "y") then
          call open_output_file(3431, "wallo_pou_pors_mon.csv", 1500)
          write (3431,*) bsn%name, prog
          write (3431,'(*(G0.6,:,","))') wallo_poupor_hdr
          write (3431,'(*(G0.6,:,","))') wallo_poupor_hdr_units
          write (9000,*) "WATER_ALLOCATION          wallo_pou_pors_mon.csv"
        end if
      end if
        
      if (pco%water_allo%y == "y") then
        call open_output_file(3428, "wallo_pou_pors_yr.txt", 1500)
        write (3428,*) bsn%name, prog
        write (3428,*) wallo_poupor_hdr
        write (3428,*) wallo_poupor_hdr_units
        write (9000,*) "WATER_ALLOCATION          wallo_pou_pors_yr.txt"
        if (pco%csvout == "y") then
          call open_output_file(3432, "wallo_pou_pors_yr.csv", 1500)
          write (3432,*) bsn%name, prog
          write (3432,'(*(G0.6,:,","))') wallo_poupor_hdr
          write (3432,'(*(G0.6,:,","))') wallo_poupor_hdr_units
          write (9000,*) "WATER_ALLOCATION          wallo_pou_pors_yr.csv"
        end if
      endif
      
      if (pco%water_allo%a == "y") then
        call open_output_file(3429, "wallo_pou_pors_aa.txt", 1500)
        write (3429,*) bsn%name, prog
        write (3429,*) wallo_poupor_hdr
        write (3429,*) wallo_poupor_hdr_units
        write (9000,*) "WATER_ALLOCATION          wallo_pou_pors_aa.txt"
        if (pco%csvout == "y") then
          call open_output_file(3433, "wallo_pou_pors_aa.csv", 1500)
          write (3433,*) bsn%name, prog
          write (3433,'(*(G0.6,:,","))') wallo_poupor_hdr
          write (3433,'(*(G0.6,:,","))') wallo_poupor_hdr_units
          write (9000,*) "WATER_ALLOCATION          wallo_pou_pors_aa.csv"
        end if
      end if
!!!  Water Allocation POU POR Output
     
      
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