     subroutine header_cs
   
     use hydrograph_module
     use constituent_mass_module
   
     implicit none 
     
     integer ipest
     integer ipath
     integer imet
     integer isalt
           
     !! daily - HYDIN
     if (pco%hyd%d == "y") then   
        if (cs_db%num_pests > 0) then
          open (2708,file="hydin_pests_day.txt",recl=800)
             write (9000,*) "HYDIN_PESTS               hydin_pests_day.txt"
             write (2708,*) bsn%name, prog
             write (2708,*) csin_hyd_hdr, (cs_pest_solsor(ipest), ipest = 1, cs_db%num_pests)
            if (pco%csvout == "y") then
              open (2724,file="hydin_pests_day.csv",recl=800)
                 write (9000,*) "HYDIN_PESTS               hydin_pests_day.csv"
                 write (2724,*) bsn%name, prog
                 write (2724,*) csin_hyd_hdr, (cs_pest_solsor(ipest), ipest = 1, cs_db%num_pests)
            end if
        end if

        if (cs_db%num_paths > 0) then
          open (2712,file="hydin_paths_day.txt",recl=800)
             write (9000,*) "HYDIN_PATHS               hydin_paths_day.txt"
             write (2712,*) bsn%name, prog
             write (2712,*) csin_hyd_hdr, (cs_path_solsor(ipath), ipath = 1, cs_db%num_paths)
            if (pco%csvout == "y") then
              open (2728,file="hydin_paths_day.csv",recl=800)
                 write (9000,*) "HYDIN_PATHS               hydin_paths_day.csv"
                 write (2728,*) bsn%name, prog
                 write (2728,*) csin_hyd_hdr, (cs_path_solsor(ipath), ipath = 1, cs_db%num_paths)
            end if
        end if
        
        if (cs_db%num_metals > 0) then
          open (2716,file="hydin_metals_day.txt",recl=800)
             write (9000,*) "HYDIN_METALS              hydin_metals_day.txt"
             write (2716,*) bsn%name, prog
             write (2716,*) csin_hyd_hdr, (cs_hmet_solsor(imet), imet = 1, cs_db%num_metals)
            if (pco%csvout == "y") then
              open (2732,file="hydin_metals_day.csv",recl=800)
                write (9000,*) "HYDIN_METALS              hydin_metals_day.csv"
                write (2732,*) bsn%name, prog
               write (2732,*) csin_hyd_hdr, (cs_hmet_solsor(imet), imet = 1, cs_db%num_metals)
            end if
        end if
        
        if (cs_db%num_salts > 0) then
          open (2720,file="hydin_salts_day.txt",recl=800)
             write (9000,*) "HYDIN_SALTS               hydin_salts_day.txt"
             write (2720,*) bsn%name, prog
             write (2720,*) csin_hyd_hdr, (cs_salt_solsor(isalt), isalt = 1, cs_db%num_salts)
            if (pco%csvout == "y") then
              open (2736,file="hydin_salts_day.csv",recl=800)
                write (9000,*) "HYDIN_SALTS               hydin_salts_day.csv"
                write (2736,*) bsn%name, prog
                write (2736,*) csin_hyd_hdr, (cs_salt_solsor(isalt), isalt = 1, cs_db%num_salts)
            end if
        end if
     end if      !! end daily - HYDIN

     !! monthly - HYDIN
     if (pco%hyd%m == "y") then   
        if (cs_db%num_pests > 0) then
          open (2709,file="hydin_pests_mon.txt",recl=800)
              write (9000,*) "HYDIN_PESTS               hydin_pests_mon.txt"
              write (2709,*) bsn%name, prog
              write (2709,*) csin_hyd_hdr, (cs_pest_solsor(ipest), ipest = 1, cs_db%num_pests)
            if (pco%csvout == "y") then
              open (2725,file="hydin_pests_mon.csv",recl=800)
              write (9000,*) "HYDIN_PESTS               hydin_pests_mon.csv"
              write (2725,*) bsn%name, prog
              write (2725,*) csin_hyd_hdr, (cs_pest_solsor(ipest), ipest = 1, cs_db%num_pests)
              
            end if
        end if

        if (cs_db%num_paths > 0) then
          open (2713,file="hydin_paths_mon.txt",recl=800)
             write (9000,*) "HYDIN_PATHS               hydin_paths_mon.txt"
             write (2713,*) bsn%name, prog
             write (2713,*) csin_hyd_hdr, (cs_path_solsor(ipath), ipath = 1, cs_db%num_paths) 
            if (pco%csvout == "y") then
              open (2729,file="hydin_paths_mon.csv",recl=800)
              write (9000,*) "HYDIN_PATHS               hydin_paths_mon.csv"
              write (2729,*) bsn%name, prog
              write (2729,*) csin_hyd_hdr, (cs_path_solsor(ipath), ipath = 1, cs_db%num_paths)
            end if
        end if
        
        if (cs_db%num_metals > 0) then
          open (2717,file="hydin_metals_mon.txt",recl=800)
             write (9000,*) "HYDIN_METALS              hydin_metals_mon.txt"
             write (2717,*) bsn%name, prog
             write (2717,*) csin_hyd_hdr, (cs_hmet_solsor(imet), imet = 1, cs_db%num_metals)
            if (pco%csvout == "y") then
              open (2733,file="hydin_metals_mon.csv",recl=800)
              write (9000,*) "HYDIN_METALS              hydin_metals_mon.csv"
              write (2733,*) bsn%name, prog
              write (2733,*) csin_hyd_hdr, (cs_hmet_solsor(imet), imet = 1, cs_db%num_metals)
            end if
        end if
        
        if (cs_db%num_salts > 0) then
          open (2721,file="hydin_salts_mon.txt",recl=800)
             write (9000,*) "HYDIN_SALTS               hydin_salts_mon.txt"
             write (2721,*) bsn%name, prog
             write (2721,*) csin_hyd_hdr, (cs_salt_solsor(isalt), isalt = 1, cs_db%num_salts)
            if (pco%csvout == "y") then
              open (2737,file="hydin_salts_mon.csv",recl=800)
              write (9000,*) "HYDIN_SALTS               hydin_salts_mon.csv"
              write (2737,*) bsn%name, prog
              write (2737,*) csin_hyd_hdr, (cs_salt_solsor(isalt), isalt = 1, cs_db%num_salts)
            end if
        end if
     end if         !! end monthly - HYDIN

     
     !! yearly - HYDIN
     if (pco%hyd%y == "y") then   
        if (cs_db%num_pests > 0) then
          open (2710,file="hydin_pests_yr.txt",recl=800)
             write (9000,*) "HYDIN_PESTS               hydin_pests_yr.txt"
             write (2710,*) bsn%name, prog
             write (2710,*) csin_hyd_hdr,(cs_pest_solsor(ipest), ipest = 1, cs_db%num_pests) 
            if (pco%csvout == "y") then
              open (2726,file="hydin_pests_yr.csv",recl=800)
              write (9000,*) "HYDIN_PESTS               hydin_pests_yr.csv"
              write (2726,*) bsn%name, prog
              write (2726,*) csin_hyd_hdr, (cs_pest_solsor(ipest), ipest = 1, cs_db%num_pests)
            end if
        end if

        if (cs_db%num_paths > 0) then
          open (2714,file="hydin_paths_yr.txt",recl=800)
             write (9000,*) "HYDIN_PATHS               hydin_paths_yr.txt"
             write (2714,*) bsn%name, prog
             write (2714,*) csin_hyd_hdr, (cs_path_solsor(ipath), ipath = 1, cs_db%num_paths)
            if (pco%csvout == "y") then
              open (2730,file="hydin_paths_yr.csv",recl=800)
              write (9000,*) "HYDIN_PATHS               hydin_paths_yr.csv"
              write (2730,*) bsn%name, prog
              write (2730,*) csin_hyd_hdr, (cs_path_solsor(ipath), ipath = 1, cs_db%num_paths)
            end if
        end if
        
        if (cs_db%num_metals > 0) then
          open (2718,file="hydin_metals_yr.txt",recl=800)
             write (9000,*) "HYDIN_METALS              hydin_metals_yr.txt"
             write (2718,*) bsn%name, prog
             write (2718,*) csin_hyd_hdr, (cs_hmet_solsor(imet), imet = 1, cs_db%num_metals)
            if (pco%csvout == "y") then
              open (2734,file="hydin_metals_yr.csv",recl=800)
              write (9000,*) "HYDIN_METALS              hydin_metals_yr.csv"
              write (2718,*) bsn%name, prog
              write (2718,*) csin_hyd_hdr, (cs_hmet_solsor(imet), imet = 1, cs_db%num_metals)
            end if
        end if
        
        if (cs_db%num_salts > 0) then
          open (2722,file="hydin_salts_yr.txt",recl=800)
             write (9000,*) "HYDIN_SALTS               hydin_salts_yr.txt"
             write (2722,*) bsn%name, prog
             write (2722,*) csin_hyd_hdr, (cs_salt_solsor(isalt), isalt = 1, cs_db%num_salts)
            if (pco%csvout == "y") then
              open (2738,file="hydin_salts_yr.csv",recl=800)
              write (9000,*) "HYDIN_SALTS               hydin_salts_yr.csv"
              write (2738,*) bsn%name, prog
              write (2738,*) csin_hyd_hdr, (cs_salt_solsor(isalt), isalt = 1, cs_db%num_salts)
            end if
        end if
     end if         !! end yearly - HYDIN

     !! average annual - HYDIN
     if (pco%hyd%a == "y") then   
        if (cs_db%num_pests > 0) then
          open (2711,file="hydin_pests_aa.txt",recl=800)
             write (9000,*) "HYDIN_PESTS               hydin_pests_aa.txt"
             write (2711,*) bsn%name, prog
             write (2711,*) csin_hyd_hdr, (cs_pest_solsor(ipest), ipest = 1, cs_db%num_pests)
            if (pco%csvout == "y") then
              open (2727,file="hydin_pests_aa.csv",recl=800)
              write (9000,*) "HYDIN_PESTS               hydin_pests_aa.csv"
              write (2727,*) bsn%name, prog
              write (2727,*) csin_hyd_hdr, (cs_pest_solsor(ipest), ipest = 1, cs_db%num_pests)
            end if
        end if

        if (cs_db%num_paths > 0) then
          open (2715,file="hydin_paths_aa.txt",recl=800)
             write (9000,*) "HYDIN_PATHS               hydin_paths_aa.txt"
             write (2715,*) bsn%name, prog
             write (2715,*) csin_hyd_hdr, (cs_path_solsor(ipath), ipath = 1, cs_db%num_paths)
            if (pco%csvout == "y") then
              open (2731,file="hydin_paths_aa.csv",recl=800)
              write (9000,*) "HYDIN_PATHS               hydin_paths_aa.csv"
              write (2731,*) bsn%name, prog
              write (2731,*) csin_hyd_hdr, (cs_path_solsor(ipath), ipath = 1, cs_db%num_paths)
            end if
        end if
        
        if (cs_db%num_metals > 0) then
          open (2719,file="hydin_metals_aa.txt",recl=800)
             write (9000,*) "HYDIN_METALS              hydin_metals_aa.txt"
             write (2719,*) bsn%name, prog
             write (2719,*) csin_hyd_hdr, (cs_hmet_solsor(imet), imet = 1, cs_db%num_metals)
            if (pco%csvout == "y") then
              open (2735,file="hydin_metals_aa.csv",recl=800)
              write (9000,*) "HYDIN_METALS              hydin_metals_aa.csv"
              write (2735,*) bsn%name, prog
              write (2735,*) csin_hyd_hdr,(cs_hmet_solsor(imet), imet = 1, cs_db%num_metals) 
            end if
        end if
        
        if (cs_db%num_salts > 0) then
          open (2723,file="hydin_salts_aa.txt",recl=800)
             write (9000,*) "HYDIN_SALTS               hydin_salts_aa.txt"
             write (2723,*) bsn%name, prog
             write (2723,*) csin_hyd_hdr, (cs_salt_solsor(isalt), isalt = 1, cs_db%num_salts)
            if (pco%csvout == "y") then
              open (2739,file="hydin_salts_aa.csv",recl=800)
              write (9000,*) "HYDIN_SALTS               hydin_salts_aa.csv"
              write (2739,*) bsn%name, prog
              write (2739,*) csin_hyd_hdr, (cs_salt_solsor(isalt), isalt = 1, cs_db%num_salts)
            end if
        end if
     end if         !! end average annual - HYDIN
          
     !! daily - HYDOUT
     if (pco%hyd%d == "y") then   
        if (cs_db%num_pests > 0) then
          open (2740,file="hydout_pests_day.txt",recl=800)
             write (2740,*) bsn%name, prog
             write (2740,*) csout_hyd_hdr, (cs_pest_solsor(ipest), ipest = 1, cs_db%num_pests)
             write (9000,*) "HYDOUT_PESTS              hydout_pests_day.txt"
            if (pco%csvout == "y") then
              open (2756,file="hydout_pests_day.csv")
                 write (2756,*) bsn%name, prog
                 write (2756,*) csout_hyd_hdr, (cs_pest_solsor(ipest), ipest = 1, cs_db%num_pests)
              write (9000,*) "HYDOUT_PESTS              hydout_pests_day.csv"
            end if
        end if

        if (cs_db%num_paths > 0) then
          open (2744,file="hydout_paths_day.txt",recl=800)
            write (9000,*) "HYDOUT_PATHS              hydout_paths_day.txt"
             write (2744,*) bsn%name, prog
             write (2744,*) csout_hyd_hdr, (cs_path_solsor(ipath), ipath = 1, cs_db%num_paths)
            if (pco%csvout == "y") then
              open (2760,file="hydout_paths_day.csv")
                 write (2760,*) bsn%name, prog
                 write (2760,*) csout_hyd_hdr, (cs_path_solsor(ipath), ipath = 1, cs_db%num_paths)
              write (9000,*) "HYDOUT_PATHS              hydout_paths_day.csv"
            end if
        end if
        
        if (cs_db%num_metals > 0) then
          open (2748,file="hydout_metals_day.txt",recl=800)
             write (9000,*) "HYDOUT_METALS             hydout_metals_day.txt"
             write (2748,*) bsn%name, prog
             write (2748,*) csout_hyd_hdr, (cs_hmet_solsor(imet), imet = 1, cs_db%num_metals)
            if (pco%csvout == "y") then
              open (2764,file="hydout_metals_day.csv")
                 write (9000,*) "HYDOUT_METALS             hydout_metals_day.csv"
                 write (2764,*) bsn%name, prog
                 write (2764,*) csout_hyd_hdr, (cs_hmet_solsor(imet), imet = 1, cs_db%num_metals)
            end if
        end if
        
        if (cs_db%num_salts > 0) then
          open (2752,file="hydout_salts_day.txt",recl=800)
             write (9000,*) "HYDOUT_SALTS              hydout_salts_day.txt"
             write (2752,*) bsn%name, prog
             write (2752,*) csout_hyd_hdr, (cs_salt_solsor(isalt), isalt = 1, cs_db%num_salts)
            if (pco%csvout == "y") then
              open (2768,file="hydout_salts_day.csv")
                 write (9000,*) "HYDOUT_SALTS              hydout_salts_day.csv"
                 write (2768,*) bsn%name, prog
                 write (2768,*) csout_hyd_hdr, (cs_salt_solsor(isalt), isalt = 1, cs_db%num_salts)
            end if
        end if
     end if      !! end daily - HYDOUT

     !! monthly - HYDOUT
     if (pco%hyd%m == "y") then   
        if (cs_db%num_pests > 0) then
          open (2741,file="hydout_pests_mon.txt",recl=800)
             write (9000,*) "HYDOUT_PESTS              hydout_pests_mon.txt"
             write (2741,*) bsn%name, prog
             write (2741,*) csout_hyd_hdr, (cs_pest_solsor(ipest), ipest = 1, cs_db%num_pests)
            if (pco%csvout == "y") then
              open (2757,file="hydout_pests_mon.csv",recl=800)
                 write (9000,*) "HYDOUT_PESTS              hydout_pests_mon.csv"
                 write (2757,*) bsn%name, prog
                 write (2757,*) csout_hyd_hdr, (cs_pest_solsor(ipest), ipest = 1, cs_db%num_pests)
            end if
        end if

        if (cs_db%num_paths > 0) then
          open (2745,file="hydout_paths_mon.txt",recl=800)
             write (9000,*) "HYDOUT_PATHS              hydout_paths_mon.txt"
             write (2745,*) bsn%name, prog
             write (2745,*) csout_hyd_hdr, (cs_path_solsor(ipath), ipath = 1, cs_db%num_paths)
            if (pco%csvout == "y") then
              open (2761,file="hydout_paths_mon.csv")
                 write (9000,*) "HYDOUT_PATHS              hydout_paths_mon.csv"
                 write (2761,*) bsn%name, prog
                 write (2761,*) csout_hyd_hdr, (cs_path_solsor(ipath), ipath = 1, cs_db%num_paths)
            end if
        end if
        
        if (cs_db%num_metals > 0) then
          open (2749,file="hydout_metals_mon.txt",recl=800)
             write (9000,*) "HYDOUT_METALS             hydout_metals_mon.txt"
             write (2749,*) bsn%name, prog
             write (2749,*) csout_hyd_hdr, (cs_hmet_solsor(imet), imet = 1, cs_db%num_metals)
            if (pco%csvout == "y") then
              open (2765,file="hydout_metals_mon.csv")
                write (9000,*) "HYDOUT_METALS             hydout_metals_mon.csv"
                write (2765,*) bsn%name, prog
                write (2765,*) csout_hyd_hdr, (cs_hmet_solsor(imet), imet = 1, cs_db%num_metals)
            end if
        end if
        
        if (cs_db%num_salts > 0) then
          open (2753,file="hydout_salts_mon.txt",recl=800)
             write (9000,*) "HYDOUT_SALTS              hydout_salts_mon.txt"
             write (2753,*) bsn%name, prog
             write (2753,*) csout_hyd_hdr, (cs_salt_solsor(isalt), isalt = 1, cs_db%num_salts)
            if (pco%csvout == "y") then
              open (2769,file="hydout_salts_mon.csv",recl=800)
                write (9000,*) "HYDOUT_SALTS              hydout_salts_mon.csv"
                write (2769,*) bsn%name, prog
                write (2769,*) csout_hyd_hdr, (cs_salt_solsor(isalt), isalt = 1, cs_db%num_salts)
            end if
        end if
     end if         !! end monthly - HYDOUT

     
     !! yearly - HYDOUT
     if (pco%hyd%y == "y") then   
        if (cs_db%num_pests > 0) then
          open (2742,file="hydout_pests_yr.txt",recl=800)
             write (9000,*) "HYDOUT_PESTS              hydout_pests_yr.txt"
             write (2742,*) bsn%name, prog
             write (2742,*) csout_hyd_hdr, (cs_pest_solsor(ipest), ipest = 1, cs_db%num_pests)
            if (pco%csvout == "y") then
              open (2758,file="hydout_pests_yr.csv",recl=800)
                write (9000,*) "HYDOUT_PESTS              hydout_pests_yr.csv"
                write (2758,*) bsn%name, prog
                write (2758,*) csout_hyd_hdr, (cs_pest_solsor(ipest), ipest = 1, cs_db%num_pests)
            end if
        end if

        if (cs_db%num_paths > 0) then
          open (2746,file="hydout_paths_yr.txt",recl=800)
             write (9000,*) "HYDOUT_PATHS              hydout_paths_yr.txt"
             write (2746,*) bsn%name, prog
             write (2746,*) csout_hyd_hdr, (cs_pest_solsor(ipath), ipath = 1, cs_db%num_paths)
            if (pco%csvout == "y") then
              open (2762,file="hydout_paths_yr.csv",recl=800)
                write (9000,*) "HYDOUT_PATHS              hydout_paths_yr.csv"
                write (2762,*) bsn%name, prog
                write (2762,*) csout_hyd_hdr, (cs_pest_solsor(ipath), ipath = 1, cs_db%num_paths)
            end if
        end if
        
        if (cs_db%num_metals > 0) then
          open (2750,file="hydout_metals_yr.txt",recl=800)
             write (9000,*) "HYDOUT_METALS             hydout_metals_yr.txt"
             write (2750,*) bsn%name, prog
             write (2750,*) csout_hyd_hdr, (cs_hmet_solsor(imet), imet = 1, cs_db%num_metals)
            if (pco%csvout == "y") then
              open (2766,file="hydout_metals_yr.csv",recl=800)
                write (9000,*) "HYDOUT_METALS             hydout_metals_yr.csv"
                write (2766,*) bsn%name, prog
                write (2766,*) csout_hyd_hdr, (cs_hmet_solsor(imet), imet = 1, cs_db%num_metals)
            end if
        end if
        
        if (cs_db%num_salts > 0) then
          open (2754,file="hydout_salts_yr.txt",recl=800)
             write (9000,*) "HYDOUT_SALTS              hydout_salts_yr.txt"
             write (2754,*) bsn%name, prog
             write (2754,*) csout_hyd_hdr, (cs_salt_solsor(isalt), isalt = 1, cs_db%num_salts)
            if (pco%csvout == "y") then
              open (2770,file="hydout_salts_yr.csv",recl=800)
                write (9000,*) "HYDOUT_SALTS              hydout_salts_yr.csv"
                write (2770,*) bsn%name, prog
                write (2770,*) csout_hyd_hdr, (cs_salt_solsor(isalt), isalt = 1, cs_db%num_salts)
            end if
        end if
     end if         !! end yearly - HYDOUT

     !! average annual - HYDOUT
     if (pco%hyd%a == "y") then   
        if (cs_db%num_pests > 0) then
          open (2743,file="hydout_pests_aa.txt",recl=800)
             write (9000,*) "HYDOUT_PESTS              hydout_pests_aa.txt"
             write (2743,*) bsn%name, prog
             write (2743,*) csout_hyd_hdr, (cs_pest_solsor(ipest), ipest = 1, cs_db%num_pests)
            if (pco%csvout == "y") then
              open (2759,file="hydout_pests_aa.csv",recl=800)
                write (9000,*) "HYDOUT_PESTS              hydout_pests_aa.csv"
                write (2759,*) bsn%name, prog
                write (2759,*) csout_hyd_hdr, (cs_pest_solsor(ipest), ipest = 1, cs_db%num_pests)
            end if
        end if

        if (cs_db%num_paths > 0) then
          open (2747,file="hydout_paths_aa.txt",recl=800)
             write (9000,*) "HYDOUT_PATHS              hydout_paths_aa.txt"
             write (2747,*) bsn%name, prog
             write (2747,*) csout_hyd_hdr, (cs_path_solsor(ipest), ipath = 1, cs_db%num_paths)
            if (pco%csvout == "y") then
              open (2763,file="hydout_paths_aa.csv",recl=800)
                write (9000,*) "HYDOUT_PATHS              hydout_paths_aa.csv"
                write (2763,*) bsn%name, prog
                write (2763,*) csout_hyd_hdr, (cs_path_solsor(ipest), ipath = 1, cs_db%num_paths)
            end if
        end if
        
        if (cs_db%num_metals > 0) then
          open (2751,file="hydout_metals_aa.txt",recl=800)
             write (9000,*) "HYDOUT_METALS             hydout_metals_aa.txt"
             write (2751,*) bsn%name, prog
             write (2751,*) csout_hyd_hdr, (cs_hmet_solsor(imet), imet = 1, cs_db%num_metals)
            if (pco%csvout == "y") then
              open (2767,file="hydout_metals_aa.csv",recl=800)
                write (9000,*) "HYDOUT_METALS             hydout_metals_aa.csv"
                write (2767,*) bsn%name, prog
                write (2767,*) csout_hyd_hdr, (cs_hmet_solsor(ipest), ipest = 1, cs_db%num_metals)
            end if
        end if
        
        if (cs_db%num_salts > 0) then
          open (2755,file="hydout_salts_aa.txt",recl=800)
             write (9000,*) "HYDOUT_SALTS              hydout_salts_aa.txt"
             write (2755,*) bsn%name, prog
             write (2755,*) csout_hyd_hdr, (cs_salt_solsor(isalt), isalt = 1, cs_db%num_salts)
            if (pco%csvout == "y") then
              open (2771,file="hydout_salts_aa.csv",recl=800)
                write (9000,*) "HYDOUT_SALTS              hydout_salts_aa.csv"
                write (2771,*) bsn%name, prog
                write (2771,*) csout_hyd_hdr, (cs_salt_solsor(isalt), isalt = 1, cs_db%num_salts)
            end if
        end if
      end if         !! end average annual - HYDOUT     
     
      return
      end subroutine header_cs