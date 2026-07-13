     subroutine header_cs
   
     use hydrograph_module
     use constituent_mass_module
     use output_path_module
     
     implicit none 
     
     integer :: ipest = 0
     integer :: ipath = 0
     integer :: imet = 0
     integer :: isalt = 0
           
     !! daily - HYDIN
     if (pco%hyd%d == "y") then   
        if (cs_db%num_pests > 0) then
          call open_output_file(2708, "hydin_pests_day.txt", 800)
             write (9000,*) "HYDIN_PESTS               hydin_pests_day.txt"
             write (2708,*) bsn%name, prog
             write (2708,*) csin_hyd_hdr, (cs_pest_solsor(ipest), ipest = 1, cs_db%num_pests)
            if (pco%csvout == "y") then
              call open_output_file(2724, "hydin_pests_day.csv", 800)
                 write (9000,*) "HYDIN_PESTS               hydin_pests_day.csv"
                 write (2724,*) bsn%name, prog
                 write (2724,*) csin_hyd_hdr, (cs_pest_solsor(ipest), ipest = 1, cs_db%num_pests)
            end if
        end if

        if (cs_db%num_paths > 0) then
          call open_output_file(2712, "hydin_paths_day.txt", 800)
             write (9000,*) "HYDIN_PATHS               hydin_paths_day.txt"
             write (2712,*) bsn%name, prog
             write (2712,*) csin_hyd_hdr, (cs_path_solsor(ipath), ipath = 1, cs_db%num_paths)
            if (pco%csvout == "y") then
              call open_output_file(2728, "hydin_paths_day.csv", 800)
                 write (9000,*) "HYDIN_PATHS               hydin_paths_day.csv"
                 write (2728,*) bsn%name, prog
                 write (2728,*) csin_hyd_hdr, (cs_path_solsor(ipath), ipath = 1, cs_db%num_paths)
            end if
        end if
        
        if (cs_db%num_metals > 0) then
          call open_output_file(2716, "hydin_metals_day.txt", 800)
             write (9000,*) "HYDIN_METALS              hydin_metals_day.txt"
             write (2716,*) bsn%name, prog
             write (2716,*) csin_hyd_hdr, (cs_hmet_solsor(imet), imet = 1, cs_db%num_metals)
            if (pco%csvout == "y") then
              call open_output_file(2732, "hydin_metals_day.csv", 800)
                write (9000,*) "HYDIN_METALS              hydin_metals_day.csv"
                write (2732,*) bsn%name, prog
               write (2732,*) csin_hyd_hdr, (cs_hmet_solsor(imet), imet = 1, cs_db%num_metals)
            end if
        end if
        
        if (cs_db%num_salts > 0) then
          call open_output_file(2720, "hydin_salts_day.txt", 800)
             write (9000,*) "HYDIN_SALTS               hydin_salts_day.txt"
             write (2720,*) bsn%name, prog
             write (2720,*) csin_hyd_hdr, (cs_salt_solsor(isalt), isalt = 1, cs_db%num_salts)
            if (pco%csvout == "y") then
              call open_output_file(2736, "hydin_salts_day.csv", 800)
                write (9000,*) "HYDIN_SALTS               hydin_salts_day.csv"
                write (2736,*) bsn%name, prog
                write (2736,*) csin_hyd_hdr, (cs_salt_solsor(isalt), isalt = 1, cs_db%num_salts)
            end if
        end if
     end if      !! end daily - HYDIN

     !! monthly - HYDIN
     if (pco%hyd%m == "y") then   
        if (cs_db%num_pests > 0) then
          call open_output_file(2709, "hydin_pests_mon.txt", 800)
              write (9000,*) "HYDIN_PESTS               hydin_pests_mon.txt"
              write (2709,*) bsn%name, prog
              write (2709,*) csin_hyd_hdr, (cs_pest_solsor(ipest), ipest = 1, cs_db%num_pests)
            if (pco%csvout == "y") then
              call open_output_file(2725, "hydin_pests_mon.csv", 800)
              write (9000,*) "HYDIN_PESTS               hydin_pests_mon.csv"
              write (2725,*) bsn%name, prog
              write (2725,*) csin_hyd_hdr, (cs_pest_solsor(ipest), ipest = 1, cs_db%num_pests)
              
            end if
        end if

        if (cs_db%num_paths > 0) then
          call open_output_file(2713, "hydin_paths_mon.txt", 800)
             write (9000,*) "HYDIN_PATHS               hydin_paths_mon.txt"
             write (2713,*) bsn%name, prog
             write (2713,*) csin_hyd_hdr, (cs_path_solsor(ipath), ipath = 1, cs_db%num_paths) 
            if (pco%csvout == "y") then
              call open_output_file(2729, "hydin_paths_mon.csv", 800)
              write (9000,*) "HYDIN_PATHS               hydin_paths_mon.csv"
              write (2729,*) bsn%name, prog
              write (2729,*) csin_hyd_hdr, (cs_path_solsor(ipath), ipath = 1, cs_db%num_paths)
            end if
        end if
        
        if (cs_db%num_metals > 0) then
          call open_output_file(2717, "hydin_metals_mon.txt", 800)
             write (9000,*) "HYDIN_METALS              hydin_metals_mon.txt"
             write (2717,*) bsn%name, prog
             write (2717,*) csin_hyd_hdr, (cs_hmet_solsor(imet), imet = 1, cs_db%num_metals)
            if (pco%csvout == "y") then
              call open_output_file(2733, "hydin_metals_mon.csv", 800)
              write (9000,*) "HYDIN_METALS              hydin_metals_mon.csv"
              write (2733,*) bsn%name, prog
              write (2733,*) csin_hyd_hdr, (cs_hmet_solsor(imet), imet = 1, cs_db%num_metals)
            end if
        end if
        
        if (cs_db%num_salts > 0) then
          call open_output_file(2721, "hydin_salts_mon.txt", 800)
             write (9000,*) "HYDIN_SALTS               hydin_salts_mon.txt"
             write (2721,*) bsn%name, prog
             write (2721,*) csin_hyd_hdr, (cs_salt_solsor(isalt), isalt = 1, cs_db%num_salts)
            if (pco%csvout == "y") then
              call open_output_file(2737, "hydin_salts_mon.csv", 800)
              write (9000,*) "HYDIN_SALTS               hydin_salts_mon.csv"
              write (2737,*) bsn%name, prog
              write (2737,*) csin_hyd_hdr, (cs_salt_solsor(isalt), isalt = 1, cs_db%num_salts)
            end if
        end if
     end if         !! end monthly - HYDIN

     
     !! yearly - HYDIN
     if (pco%hyd%y == "y") then   
        if (cs_db%num_pests > 0) then
          call open_output_file(2710, "hydin_pests_yr.txt", 800)
             write (9000,*) "HYDIN_PESTS               hydin_pests_yr.txt"
             write (2710,*) bsn%name, prog
             write (2710,*) csin_hyd_hdr,(cs_pest_solsor(ipest), ipest = 1, cs_db%num_pests) 
            if (pco%csvout == "y") then
              call open_output_file(2726, "hydin_pests_yr.csv", 800)
              write (9000,*) "HYDIN_PESTS               hydin_pests_yr.csv"
              write (2726,*) bsn%name, prog
              write (2726,*) csin_hyd_hdr, (cs_pest_solsor(ipest), ipest = 1, cs_db%num_pests)
            end if
        end if

        if (cs_db%num_paths > 0) then
          call open_output_file(2714, "hydin_paths_yr.txt", 800)
             write (9000,*) "HYDIN_PATHS               hydin_paths_yr.txt"
             write (2714,*) bsn%name, prog
             write (2714,*) csin_hyd_hdr, (cs_path_solsor(ipath), ipath = 1, cs_db%num_paths)
            if (pco%csvout == "y") then
              call open_output_file(2730, "hydin_paths_yr.csv", 800)
              write (9000,*) "HYDIN_PATHS               hydin_paths_yr.csv"
              write (2730,*) bsn%name, prog
              write (2730,*) csin_hyd_hdr, (cs_path_solsor(ipath), ipath = 1, cs_db%num_paths)
            end if
        end if
        
        if (cs_db%num_metals > 0) then
          call open_output_file(2718, "hydin_metals_yr.txt", 800)
             write (9000,*) "HYDIN_METALS              hydin_metals_yr.txt"
             write (2718,*) bsn%name, prog
             write (2718,*) csin_hyd_hdr, (cs_hmet_solsor(imet), imet = 1, cs_db%num_metals)
            if (pco%csvout == "y") then
              call open_output_file(2734, "hydin_metals_yr.csv", 800)
              write (9000,*) "HYDIN_METALS              hydin_metals_yr.csv"
              write (2718,*) bsn%name, prog
              write (2718,*) csin_hyd_hdr, (cs_hmet_solsor(imet), imet = 1, cs_db%num_metals)
            end if
        end if
        
        if (cs_db%num_salts > 0) then
          call open_output_file(2722, "hydin_salts_yr.txt", 800)
             write (9000,*) "HYDIN_SALTS               hydin_salts_yr.txt"
             write (2722,*) bsn%name, prog
             write (2722,*) csin_hyd_hdr, (cs_salt_solsor(isalt), isalt = 1, cs_db%num_salts)
            if (pco%csvout == "y") then
              call open_output_file(2738, "hydin_salts_yr.csv", 800)
              write (9000,*) "HYDIN_SALTS               hydin_salts_yr.csv"
              write (2738,*) bsn%name, prog
              write (2738,*) csin_hyd_hdr, (cs_salt_solsor(isalt), isalt = 1, cs_db%num_salts)
            end if
        end if
     end if         !! end yearly - HYDIN

     !! average annual - HYDIN
     if (pco%hyd%a == "y") then   
        if (cs_db%num_pests > 0) then
          call open_output_file(2711, "hydin_pests_aa.txt", 800)
             write (9000,*) "HYDIN_PESTS               hydin_pests_aa.txt"
             write (2711,*) bsn%name, prog
             write (2711,*) csin_hyd_hdr, (cs_pest_solsor(ipest), ipest = 1, cs_db%num_pests)
            if (pco%csvout == "y") then
              call open_output_file(2727, "hydin_pests_aa.csv", 800)
              write (9000,*) "HYDIN_PESTS               hydin_pests_aa.csv"
              write (2727,*) bsn%name, prog
              write (2727,*) csin_hyd_hdr, (cs_pest_solsor(ipest), ipest = 1, cs_db%num_pests)
            end if
        end if

        if (cs_db%num_paths > 0) then
          call open_output_file(2715, "hydin_paths_aa.txt", 800)
             write (9000,*) "HYDIN_PATHS               hydin_paths_aa.txt"
             write (2715,*) bsn%name, prog
             write (2715,*) csin_hyd_hdr, (cs_path_solsor(ipath), ipath = 1, cs_db%num_paths)
            if (pco%csvout == "y") then
              call open_output_file(2731, "hydin_paths_aa.csv", 800)
              write (9000,*) "HYDIN_PATHS               hydin_paths_aa.csv"
              write (2731,*) bsn%name, prog
              write (2731,*) csin_hyd_hdr, (cs_path_solsor(ipath), ipath = 1, cs_db%num_paths)
            end if
        end if
        
        if (cs_db%num_metals > 0) then
          call open_output_file(2719, "hydin_metals_aa.txt", 800)
             write (9000,*) "HYDIN_METALS              hydin_metals_aa.txt"
             write (2719,*) bsn%name, prog
             write (2719,*) csin_hyd_hdr, (cs_hmet_solsor(imet), imet = 1, cs_db%num_metals)
            if (pco%csvout == "y") then
              call open_output_file(2735, "hydin_metals_aa.csv", 800)
              write (9000,*) "HYDIN_METALS              hydin_metals_aa.csv"
              write (2735,*) bsn%name, prog
              write (2735,*) csin_hyd_hdr,(cs_hmet_solsor(imet), imet = 1, cs_db%num_metals) 
            end if
        end if
        
        if (cs_db%num_salts > 0) then
          call open_output_file(2723, "hydin_salts_aa.txt", 800)
             write (9000,*) "HYDIN_SALTS               hydin_salts_aa.txt"
             write (2723,*) bsn%name, prog
             write (2723,*) csin_hyd_hdr, (cs_salt_solsor(isalt), isalt = 1, cs_db%num_salts)
            if (pco%csvout == "y") then
              call open_output_file(2739, "hydin_salts_aa.csv", 800)
              write (9000,*) "HYDIN_SALTS               hydin_salts_aa.csv"
              write (2739,*) bsn%name, prog
              write (2739,*) csin_hyd_hdr, (cs_salt_solsor(isalt), isalt = 1, cs_db%num_salts)
            end if
        end if
     end if         !! end average annual - HYDIN
          
     !! daily - HYDOUT
     if (pco%hyd%d == "y") then   
        if (cs_db%num_pests > 0) then
          call open_output_file(2740, "hydout_pests_day.txt", 800)
             write (2740,*) bsn%name, prog
             write (2740,*) csout_hyd_hdr, (cs_pest_solsor(ipest), ipest = 1, cs_db%num_pests)
             write (9000,*) "HYDOUT_PESTS              hydout_pests_day.txt"
            if (pco%csvout == "y") then
              call open_output_file(2756, "hydout_pests_day.csv")
                 write (2756,*) bsn%name, prog
                 write (2756,*) csout_hyd_hdr, (cs_pest_solsor(ipest), ipest = 1, cs_db%num_pests)
              write (9000,*) "HYDOUT_PESTS              hydout_pests_day.csv"
            end if
        end if

        if (cs_db%num_paths > 0) then
          call open_output_file(2744, "hydout_paths_day.txt", 800)
            write (9000,*) "HYDOUT_PATHS              hydout_paths_day.txt"
             write (2744,*) bsn%name, prog
             write (2744,*) csout_hyd_hdr, (cs_path_solsor(ipath), ipath = 1, cs_db%num_paths)
            if (pco%csvout == "y") then
              call open_output_file(2760, "hydout_paths_day.csv")
                 write (2760,*) bsn%name, prog
                 write (2760,*) csout_hyd_hdr, (cs_path_solsor(ipath), ipath = 1, cs_db%num_paths)
              write (9000,*) "HYDOUT_PATHS              hydout_paths_day.csv"
            end if
        end if
        
        if (cs_db%num_metals > 0) then
          call open_output_file(2748, "hydout_metals_day.txt", 800)
             write (9000,*) "HYDOUT_METALS             hydout_metals_day.txt"
             write (2748,*) bsn%name, prog
             write (2748,*) csout_hyd_hdr, (cs_hmet_solsor(imet), imet = 1, cs_db%num_metals)
            if (pco%csvout == "y") then
              call open_output_file(2764, "hydout_metals_day.csv")
                 write (9000,*) "HYDOUT_METALS             hydout_metals_day.csv"
                 write (2764,*) bsn%name, prog
                 write (2764,*) csout_hyd_hdr, (cs_hmet_solsor(imet), imet = 1, cs_db%num_metals)
            end if
        end if
        
        if (cs_db%num_salts > 0) then
          call open_output_file(2752, "hydout_salts_day.txt", 800)
             write (9000,*) "HYDOUT_SALTS              hydout_salts_day.txt"
             write (2752,*) bsn%name, prog
             write (2752,*) csout_hyd_hdr, (cs_salt_solsor(isalt), isalt = 1, cs_db%num_salts)
            if (pco%csvout == "y") then
              call open_output_file(2768, "hydout_salts_day.csv")
                 write (9000,*) "HYDOUT_SALTS              hydout_salts_day.csv"
                 write (2768,*) bsn%name, prog
                 write (2768,*) csout_hyd_hdr, (cs_salt_solsor(isalt), isalt = 1, cs_db%num_salts)
            end if
        end if
     end if      !! end daily - HYDOUT

     !! monthly - HYDOUT
     if (pco%hyd%m == "y") then   
        if (cs_db%num_pests > 0) then
          call open_output_file(2741, "hydout_pests_mon.txt", 800)
             write (9000,*) "HYDOUT_PESTS              hydout_pests_mon.txt"
             write (2741,*) bsn%name, prog
             write (2741,*) csout_hyd_hdr, (cs_pest_solsor(ipest), ipest = 1, cs_db%num_pests)
            if (pco%csvout == "y") then
              call open_output_file(2757, "hydout_pests_mon.csv", 800)
                 write (9000,*) "HYDOUT_PESTS              hydout_pests_mon.csv"
                 write (2757,*) bsn%name, prog
                 write (2757,*) csout_hyd_hdr, (cs_pest_solsor(ipest), ipest = 1, cs_db%num_pests)
            end if
        end if

        if (cs_db%num_paths > 0) then
          call open_output_file(2745, "hydout_paths_mon.txt", 800)
             write (9000,*) "HYDOUT_PATHS              hydout_paths_mon.txt"
             write (2745,*) bsn%name, prog
             write (2745,*) csout_hyd_hdr, (cs_path_solsor(ipath), ipath = 1, cs_db%num_paths)
            if (pco%csvout == "y") then
              call open_output_file(2761, "hydout_paths_mon.csv")
                 write (9000,*) "HYDOUT_PATHS              hydout_paths_mon.csv"
                 write (2761,*) bsn%name, prog
                 write (2761,*) csout_hyd_hdr, (cs_path_solsor(ipath), ipath = 1, cs_db%num_paths)
            end if
        end if
        
        if (cs_db%num_metals > 0) then
          call open_output_file(2749, "hydout_metals_mon.txt", 800)
             write (9000,*) "HYDOUT_METALS             hydout_metals_mon.txt"
             write (2749,*) bsn%name, prog
             write (2749,*) csout_hyd_hdr, (cs_hmet_solsor(imet), imet = 1, cs_db%num_metals)
            if (pco%csvout == "y") then
              call open_output_file(2765, "hydout_metals_mon.csv")
                write (9000,*) "HYDOUT_METALS             hydout_metals_mon.csv"
                write (2765,*) bsn%name, prog
                write (2765,*) csout_hyd_hdr, (cs_hmet_solsor(imet), imet = 1, cs_db%num_metals)
            end if
        end if
        
        if (cs_db%num_salts > 0) then
          call open_output_file(2753, "hydout_salts_mon.txt", 800)
             write (9000,*) "HYDOUT_SALTS              hydout_salts_mon.txt"
             write (2753,*) bsn%name, prog
             write (2753,*) csout_hyd_hdr, (cs_salt_solsor(isalt), isalt = 1, cs_db%num_salts)
            if (pco%csvout == "y") then
              call open_output_file(2769, "hydout_salts_mon.csv", 800)
                write (9000,*) "HYDOUT_SALTS              hydout_salts_mon.csv"
                write (2769,*) bsn%name, prog
                write (2769,*) csout_hyd_hdr, (cs_salt_solsor(isalt), isalt = 1, cs_db%num_salts)
            end if
        end if
     end if         !! end monthly - HYDOUT

     
     !! yearly - HYDOUT
     if (pco%hyd%y == "y") then   
        if (cs_db%num_pests > 0) then
          call open_output_file(2742, "hydout_pests_yr.txt", 800)
             write (9000,*) "HYDOUT_PESTS              hydout_pests_yr.txt"
             write (2742,*) bsn%name, prog
             write (2742,*) csout_hyd_hdr, (cs_pest_solsor(ipest), ipest = 1, cs_db%num_pests)
            if (pco%csvout == "y") then
              call open_output_file(2758, "hydout_pests_yr.csv", 800)
                write (9000,*) "HYDOUT_PESTS              hydout_pests_yr.csv"
                write (2758,*) bsn%name, prog
                write (2758,*) csout_hyd_hdr, (cs_pest_solsor(ipest), ipest = 1, cs_db%num_pests)
            end if
        end if

        if (cs_db%num_paths > 0) then
          call open_output_file(2746, "hydout_paths_yr.txt", 800)
             write (9000,*) "HYDOUT_PATHS              hydout_paths_yr.txt"
             write (2746,*) bsn%name, prog
             write (2746,*) csout_hyd_hdr, (cs_pest_solsor(ipath), ipath = 1, cs_db%num_paths)
            if (pco%csvout == "y") then
              call open_output_file(2762, "hydout_paths_yr.csv", 800)
                write (9000,*) "HYDOUT_PATHS              hydout_paths_yr.csv"
                write (2762,*) bsn%name, prog
                write (2762,*) csout_hyd_hdr, (cs_pest_solsor(ipath), ipath = 1, cs_db%num_paths)
            end if
        end if
        
        if (cs_db%num_metals > 0) then
          call open_output_file(2750, "hydout_metals_yr.txt", 800)
             write (9000,*) "HYDOUT_METALS             hydout_metals_yr.txt"
             write (2750,*) bsn%name, prog
             write (2750,*) csout_hyd_hdr, (cs_hmet_solsor(imet), imet = 1, cs_db%num_metals)
            if (pco%csvout == "y") then
              call open_output_file(2766, "hydout_metals_yr.csv", 800)
                write (9000,*) "HYDOUT_METALS             hydout_metals_yr.csv"
                write (2766,*) bsn%name, prog
                write (2766,*) csout_hyd_hdr, (cs_hmet_solsor(imet), imet = 1, cs_db%num_metals)
            end if
        end if
        
        if (cs_db%num_salts > 0) then
          call open_output_file(2754, "hydout_salts_yr.txt", 800)
             write (9000,*) "HYDOUT_SALTS              hydout_salts_yr.txt"
             write (2754,*) bsn%name, prog
             write (2754,*) csout_hyd_hdr, (cs_salt_solsor(isalt), isalt = 1, cs_db%num_salts)
            if (pco%csvout == "y") then
              call open_output_file(2770, "hydout_salts_yr.csv", 800)
                write (9000,*) "HYDOUT_SALTS              hydout_salts_yr.csv"
                write (2770,*) bsn%name, prog
                write (2770,*) csout_hyd_hdr, (cs_salt_solsor(isalt), isalt = 1, cs_db%num_salts)
            end if
        end if
     end if         !! end yearly - HYDOUT

     !! average annual - HYDOUT
     if (pco%hyd%a == "y") then   
        if (cs_db%num_pests > 0) then
          call open_output_file(2743, "hydout_pests_aa.txt", 800)
             write (9000,*) "HYDOUT_PESTS              hydout_pests_aa.txt"
             write (2743,*) bsn%name, prog
             write (2743,*) csout_hyd_hdr, (cs_pest_solsor(ipest), ipest = 1, cs_db%num_pests)
            if (pco%csvout == "y") then
              call open_output_file(2759, "hydout_pests_aa.csv", 800)
                write (9000,*) "HYDOUT_PESTS              hydout_pests_aa.csv"
                write (2759,*) bsn%name, prog
                write (2759,*) csout_hyd_hdr, (cs_pest_solsor(ipest), ipest = 1, cs_db%num_pests)
            end if
        end if

        if (cs_db%num_paths > 0) then
          call open_output_file(2747, "hydout_paths_aa.txt", 800)
             write (9000,*) "HYDOUT_PATHS              hydout_paths_aa.txt"
             write (2747,*) bsn%name, prog
             write (2747,*) csout_hyd_hdr, (cs_path_solsor(ipest), ipath = 1, cs_db%num_paths)
            if (pco%csvout == "y") then
              call open_output_file(2763, "hydout_paths_aa.csv", 800)
                write (9000,*) "HYDOUT_PATHS              hydout_paths_aa.csv"
                write (2763,*) bsn%name, prog
                write (2763,*) csout_hyd_hdr, (cs_path_solsor(ipest), ipath = 1, cs_db%num_paths)
            end if
        end if
        
        if (cs_db%num_metals > 0) then
          call open_output_file(2751, "hydout_metals_aa.txt", 800)
             write (9000,*) "HYDOUT_METALS             hydout_metals_aa.txt"
             write (2751,*) bsn%name, prog
             write (2751,*) csout_hyd_hdr, (cs_hmet_solsor(imet), imet = 1, cs_db%num_metals)
            if (pco%csvout == "y") then
              call open_output_file(2767, "hydout_metals_aa.csv", 800)
                write (9000,*) "HYDOUT_METALS             hydout_metals_aa.csv"
                write (2767,*) bsn%name, prog
                write (2767,*) csout_hyd_hdr, (cs_hmet_solsor(ipest), ipest = 1, cs_db%num_metals)
            end if
        end if
        
        if (cs_db%num_salts > 0) then
          call open_output_file(2755, "hydout_salts_aa.txt", 800)
             write (9000,*) "HYDOUT_SALTS              hydout_salts_aa.txt"
             write (2755,*) bsn%name, prog
             write (2755,*) csout_hyd_hdr, (cs_salt_solsor(isalt), isalt = 1, cs_db%num_salts)
            if (pco%csvout == "y") then
              call open_output_file(2771, "hydout_salts_aa.csv", 800)
                write (9000,*) "HYDOUT_SALTS              hydout_salts_aa.csv"
                write (2771,*) bsn%name, prog
                write (2771,*) csout_hyd_hdr, (cs_salt_solsor(isalt), isalt = 1, cs_db%num_salts)
            end if
        end if
      end if         !! end average annual - HYDOUT     
     
      return
      end subroutine header_cs