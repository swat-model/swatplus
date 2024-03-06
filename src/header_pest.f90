     subroutine header_pest
    
     use basin_module
     use reservoir_module
     use hydrograph_module, only : sp_ob
     use output_ls_pesticide_module
     use constituent_mass_module
     use ch_pesticide_module
     use res_pesticide_module
     use aqu_pesticide_module
     
     implicit none 

    !! HRU_PESTICIDE - daily
     if (sp_ob%hru > 0) then
      if (pco%pest%d == "y" .and. cs_db%num_tot > 0) then
        open (2800,file="hru_pest_day.txt",recl=800)
        write (2800,*) bsn%name, prog
        write (9000,*) "HRU_PEST                  hru_pest_day.txt"
        write (2800,*) pestb_hdr

          if (pco%csvout == "y") then
            open (2804,file="hru_pest_day.csv",recl=800)
            write (2804,*) bsn%name, prog
            write (2804,'(*(G0.3,:","))') pestb_hdr
            write (9000,*) "HRU_PEST                  hru_pest_day.csv"
          end if
      end if
      
!! HRU_PESTICIDE - monthly
      if (pco%pest%m == "y" .and. cs_db%num_tot > 0 ) then
        open (2801,file="hru_pest_mon.txt",recl=800)
        write (2801,*) bsn%name, prog
        write (9000,*) "HRU_PEST                  hru_pest_mon.txt"
        write (2801,*) pestb_hdr

          if (pco%csvout == "y") then
            open (2805,file="hru_pest_mon.csv",recl=800)
            write (2805,*) bsn%name, prog
            write (2805,'(*(G0.3,:","))') pestb_hdr
            write (9000,*) "HRU_PEST                  hru_pest_mon.csv"
          end if
      end if
      
!! HRU_PESTICIDE - yearly
      if (pco%pest%y == "y" .and. cs_db%num_tot > 0) then
        open (2802,file="hru_pest_yr.txt",recl=800)
        write (2802,*) bsn%name, prog
        write (9000,*) "HRU_PEST                  hru_pest_yr.txt"
        write (2802,*) pestb_hdr

          if (pco%csvout == "y") then
            open (2806,file="hru_pest_yr.csv",recl=800)
            write (2806,*) bsn%name, prog
            write (2806,'(*(G0.3,:","))') pestb_hdr
            write (9000,*) "HRU_PEST                  hru_pest_yr.csv"
          end if
      end if
      
!! HRU_PESTICIDE - ave annual
      if (pco%pest%a == "y" .and. cs_db%num_tot > 0) then
        open (2803,file="hru_pest_aa.txt",recl=800)
        write (2803,*) bsn%name, prog
        write (9000,*) "HRU_PEST                  hru_pest_aa.txt"
        write (2803,*) pestb_hdr

          if (pco%csvout == "y") then
            open (2807,file="hru_pest_aa.csv",recl=800)
            write (2807,*) bsn%name, prog
            write (2807,'(*(G0.3,:","))') pestb_hdr
            write (9000,*) "HRU_PEST                  hru_pest_aa.csv"
          end if
      end if
     end if
      
 !-----------------------------------------------     
      
    !! CHANNEL_PESTICIDE - daily
     if (sp_ob%chandeg > 0) then
      if (pco%pest%d == "y" .and. cs_db%num_tot > 0) then
        open (2808,file="channel_pest_day.txt",recl=800)
        write (2808,*) bsn%name, prog
        write (9000,*) "CHANNEL_PEST              channel_pest_day.txt"
        write (2808,*) chpest_hdr

          if (pco%csvout == "y") then
            open (2812,file="channel_pest_day.csv",recl=800)
            write (2812,*) bsn%name, prog
            write (2812,'(*(G0.3,:","))') chpest_hdr
            write (9000,*) "CHANNEL_PEST              channel_pest_day.csv"
          end if
      end if
      
!! CHANNEL_PESTICIDE - monthly
      if (pco%pest%m == "y" .and. cs_db%num_tot > 0 ) then
        open (2809,file="channel_pest_mon.txt",recl=800)
        write (2809,*) bsn%name, prog
        write (9000,*) "CHANNEL_PEST              channel_pest_mon.txt"
        write (2809,*) chpest_hdr

          if (pco%csvout == "y") then
            open (2813,file="channel_pest_mon.csv",recl=800)
            write (2813,*) bsn%name, prog
            write (2813,'(*(G0.3,:","))') chpest_hdr
            write (9000,*) "CHANNEL_PEST              channel_pest_mon.csv"
          end if
      end if
      
!! CHANNEL_PESTICIDE - yearly
      if (pco%pest%y == "y" .and. cs_db%num_tot > 0) then
        open (2810,file="channel_pest_yr.txt",recl=800)
        write (2810,*) bsn%name, prog
        write (9000,*) "CHANNEL_PEST              channel_pest_yr.txt"
        write (2810,*) chpest_hdr

          if (pco%csvout == "y") then
            open (2814,file="channel_pest_yr.csv",recl=800)
            write (2814,*) bsn%name, prog
            write (2814,'(*(G0.3,:","))') chpest_hdr
            write (9000,*) "CHANNEL_PEST              channel_pest_yr.csv"
          end if
      end if
      
!! CHANNEL_PESTICIDE - ave annual
      if (pco%pest%a == "y" .and. cs_db%num_tot > 0) then
        open (2811,file="channel_pest_aa.txt",recl=800)
        write (2811,*) bsn%name, prog
        write (9000,*) "CHANNEL_PEST              channel_pest_aa.txt"
        write (2811,*) chpest_hdr

          if (pco%csvout == "y") then
            open (2815,file="channel_pest_aa.csv",recl=800)
            write (2815,*) bsn%name, prog
            write (2815,'(*(G0.3,:","))') chpest_hdr
            write (9000,*) "CHANNEL_PEST              channel_pest_aa.csv"
          end if
      end if
     end if
      
!----------------------------------------
            
    !! RESERVOIR_PESTICIDE - daily
     if (sp_ob%res > 0) then
      if (pco%pest%d == "y" .and. cs_db%num_tot > 0) then
        open (2816,file="reservoir_pest_day.txt",recl=800)
        write (2816,*) bsn%name, prog
        write (9000,*) "RESERVOIR_PEST            reservoir_pest_day.txt"
        write (2816,*) respest_hdr

          if (pco%csvout == "y") then
            open (2820,file="reservoir_pest_day.csv",recl=800)
            write (2820,*) bsn%name, prog
            write (2820,'(*(G0.3,:","))') respest_hdr
            write (9000,*) "RESERVOIR_PEST            reservoir_pest_day.csv"           
          end if
      end if
      
!! RESERVOIR_PESTICIDE - monthly
      if (pco%pest%m == "y" .and. cs_db%num_tot > 0 ) then
        open (2817,file="reservoir_pest_mon.txt",recl=800)
        write (2817,*) bsn%name, prog
        write (9000,*) "RESERVOIR_PEST            reservoir_pest_mon.txt"
        write (2817,*) respest_hdr

          if (pco%csvout == "y") then
            open (2821,file="reservoir_pest_mon.csv",recl=800)
            write (2821,*) bsn%name, prog
            write (2821,'(*(G0.3,:","))') respest_hdr
            write (9000,*) "RESERVOIR_PEST            reservoir_pest_mon.csv"
          end if
      end if
      
!! RESERVOIR_PESTICIDE - yearly
      if (pco%pest%y == "y" .and. cs_db%num_tot > 0) then
        open (2818,file="reservoir_pest_yr.txt",recl=800)
        write (2818,*) bsn%name, prog
        write (9000,*) "RESERVOIR_PEST            reservoir_pest_yr.txt"
        write (2818,*) respest_hdr

          if (pco%csvout == "y") then
            open (2822,file="reservoir_pest_yr.csv",recl=800)
            write (2822,*) bsn%name, prog
            write (2822,'(*(G0.3,:","))') respest_hdr
            write (9000,*) "RESERVOIR_PEST            reservoir_pest_yr.csv"
          end if
      end if
      
!! RESERVOIR_PESTICIDE - ave annual
      if (pco%pest%a == "y" .and. cs_db%num_tot > 0) then
        open (2819,file="reservoir_pest_aa.txt",recl=800)
        write (2819,*) bsn%name, prog
        write (9000,*) "RESERVOIR_PEST            reservoir_pest_aa.txt"
        write (2819,*) respest_hdr

          if (pco%csvout == "y") then
            open (2823,file="reservoir_pest_aa.csv",recl=800)
            write (2823,*) bsn%name, prog
            write (2823,'(*(G0.3,:","))') respest_hdr
            write (9000,*) "RESERVOIR_PEST            reservoir_pest_aa.csv"
          end if
      end if
     end if
         
!----------------------------------------
                     
    !! BASIN AQUIFER_PESTICIDE - daily
     if (sp_ob%aqu > 0) then
      if (pco%pest%d == "y" .and. cs_db%num_tot > 0) then
        open (3000,file="basin_aqu_pest_day.txt",recl=800)
        write (3000,*) bsn%name, prog
        write (9000,*) "BASIN_AQUIFER_PEST        basin_aqu_pest_day.txt"
        write (3000,*) aqupest_hdr

          if (pco%csvout == "y") then
            open (3004,file="basin_aqu_pest_day.csv",recl=800)
            write (3004,*) bsn%name, prog
            write (3004,'(*(G0.3,:","))') aqupest_hdr
            write (9000,*) "BASIN_AQUIFER_PEST        basin_aqu_pest_day.csv"
          end if
      end if
      
!! BASIN AQUIFER_PESTICIDE - monthly
      if (pco%pest%m == "y" .and. cs_db%num_tot > 0 ) then
        open (3001,file="basin_aqu_pest_mon.txt",recl=800)
        write (3001,*) bsn%name, prog
        write (9000,*) "BASIN_AQUIFER_PEST        basin_aqu_pest_mon.txt"
        write (3001,*) aqupest_hdr

          if (pco%csvout == "y") then
            open (3005,file="basin_aqu_pest_mon.csv",recl=800)
            write (3005,*) bsn%name, prog
            write (3005,'(*(G0.3,:","))') aqupest_hdr
            write (9000,*) "BASIN_AQUIFER_PEST        basin_aqu_pest_mon.csv"
          end if
      end if
      
!! BASIN AQUIFER_PESTICIDE - yearly
      if (pco%pest%y == "y" .and. cs_db%num_tot > 0) then
        open (3002,file="basin_aqu_pest_yr.txt",recl=800)
        write (3002,*) bsn%name, prog
        write (9000,*) "BASIN_AQUIFER_PEST        basin_aqu_pest_yr.txt"
        write (3002,*) aqupest_hdr

          if (pco%csvout == "y") then
            open (3006,file="basin_aqu_pest_yr.csv",recl=800)
            write (3006,*) bsn%name, prog
            write (3006,'(*(G0.3,:","))') aqupest_hdr
            write (9000,*) "BASIN_AQUIFER_PEST        basin_aqu_pest_yr.csv" 
          end if
      end if
      
!! BASIN AQUIFER_PESTICIDE - ave annual
      if (pco%pest%a == "y" .and. cs_db%num_tot > 0) then
        open (3003,file="basin_aqu_pest_aa.txt",recl=800)
        write (3003,*) bsn%name, prog
        write (9000,*) "BASIN_AQUIFER_PEST        basin_aqu_pest_aa.txt"
        write (3003,*) aqupest_hdr

          if (pco%csvout == "y") then
            open (3007,file="basin_aqu_pest_aa.csv",recl=800)
            write (3007,*) bsn%name, prog
            write (3007,'(*(G0.3,:","))') aqupest_hdr
            write (9000,*) "BASIN_AQUIFER_PEST        basin_aqu_pest_aa.csv"
          end if
      end if
     end if
         
!----------------------------------------
                     
    !! AQUIFER_PESTICIDE - daily
     if (sp_ob%aqu > 0) then
      if (pco%pest%d == "y" .and. cs_db%num_tot > 0) then
        open (3008,file="aquifer_pest_day.txt",recl=800)
        write (3008,*) bsn%name, prog
        write (9000,*) "AQUIFER_PEST              aquifer_pest_day.txt"
        write (3008,*) aqupest_hdr

          if (pco%csvout == "y") then
            open (3012,file="aquifer_pest_day.csv",recl=800)
            write (3012,*) bsn%name, prog
            write (3012,'(*(G0.3,:","))') aqupest_hdr
            write (9000,*) "AQUIFER_PEST              aquifer_pest_day.csv"
          end if
      end if
      
!! AQUIFER_PESTICIDE - monthly
      if (pco%pest%m == "y" .and. cs_db%num_tot > 0 ) then
        open (3009,file="aquifer_pest_mon.txt",recl=800)
        write (3009,*) bsn%name, prog
        write (9000,*) "AQUIFER_PEST              aquifer_pest_mon.txt"
        write (3009,*) aqupest_hdr

          if (pco%csvout == "y") then
            open (3013,file="aquifer_pest_mon.csv",recl=800)
            write (3013,*) bsn%name, prog
            write (3013,'(*(G0.3,:","))') aqupest_hdr
            write (9000,*) "AQUIFER_PEST              aquifer_pest_mon.csv"
          end if
      end if
      
!! AQUIFER_PESTICIDE - yearly
      if (pco%pest%y == "y" .and. cs_db%num_tot > 0) then
        open (3010,file="aquifer_pest_yr.txt",recl=800)
        write (3010,*) bsn%name, prog
        write (9000,*) "AQUIFER_PEST              aquifer_pest_yr.txt"
        write (3010,*) aqupest_hdr

          if (pco%csvout == "y") then
            open (3014,file="aquifer_pest_yr.csv",recl=800)
            write (3014,*) bsn%name, prog
            write (3014,'(*(G0.3,:","))') aqupest_hdr
            write (9000,*) "AQUIFER_PEST              aquifer_pest_yr.csv"
          end if
      end if
      
!! AQUIFER_PESTICIDE - ave annual
      if (pco%pest%a == "y" .and. cs_db%num_tot > 0) then
        open (3011,file="aquifer_pest_aa.txt",recl=800)
        write (3011,*) bsn%name, prog
        write (9000,*) "AQUIFER_PEST              aquifer_pest_aa.txt"
        write (3011,*) aqupest_hdr

          if (pco%csvout == "y") then
            open (3015,file="aquifer_pest_aa.csv",recl=800)
            write (3015,*) bsn%name, prog
            write (3015,'(*(G0.3,:","))') aqupest_hdr
            write (9000,*) "AQUIFER_PEST              aquifer_pest_aa.csv"
          end if
      end if
     end if
         
!----------------------------------------
          
    !! BASIN_CH_PESTICIDE - daily
      if (sp_ob%chandeg > 0) then
       if (pco%pest%d == "y" .and. cs_db%num_tot > 0) then
        open (2832,file="basin_ch_pest_day.txt",recl=800)
        write (2832,*) bsn%name, prog
        write (9000,*) "BASIN_CH_PEST             basin_ch_pest_day.txt"
        write (2832,*) chpest_hdr

          if (pco%csvout == "y") then
            open (2836,file="basin_ch_pest_day.csv",recl=800)
            write (2836,*) bsn%name, prog
            write (2836,'(*(G0.3,:","))') chpest_hdr
            write (9000,*) "BASIN_CH_PEST             reservoir_pest_day.csv"
          end if
       end if
      
!! BASIN_CH_PESTICIDE - monthly
      if (pco%pest%m == "y" .and. cs_db%num_tot > 0 ) then
        open (2833,file="basin_ch_pest_mon.txt",recl=800)
        write (2833,*) bsn%name, prog
        write (9000,*) "BASIN_CH_PEST             basin_ch_pest_mon.txt"
        write (2833,*) chpest_hdr

          if (pco%csvout == "y") then
            open (2837,file="basin_ch_pest_mon.csv",recl=800)
            write (2837,*) bsn%name, prog
            write (2837,'(*(G0.3,:","))') chpest_hdr
            write (9000,*) "BASIN_CH_PEST             basin_ch_pest_mon.csv"
          end if
      end if
      
!! BASIN_CH_PESTICIDE - yearly
      if (pco%pest%y == "y" .and. cs_db%num_tot > 0) then
        open (2834,file="basin_ch_pest_yr.txt",recl=800)
        write (2834,*) bsn%name, prog
        write (9000,*) "BASIN_CH_PEST             basin_ch_pest_yr.txt"
        write (2834,*) chpest_hdr

          if (pco%csvout == "y") then
            open (2838,file="basin_ch_pest_yr.csv",recl=800)
            write (2838,*) bsn%name, prog
            write (2838,'(*(G0.3,:","))') chpest_hdr
            write (9000,*) "BASIN_CH_PEST             basin_ch_pest_yr.csv"
          end if
      end if
      
!! BASIN_CH_PESTICIDE - ave annual
      if (pco%pest%a == "y" .and. cs_db%num_tot > 0) then
        open (2835,file="basin_ch_pest_aa.txt",recl=800)
        write (2835,*) bsn%name, prog
        write (9000,*) "BASIN_CH_PEST             basin_ch_pest_aa.txt"
        write (2835,*) chpest_hdr

          if (pco%csvout == "y") then
            open (2839,file="basin_ch_pest_aa.csv",recl=800)
            write (2839,*) bsn%name, prog
            write (2839,'(*(G0.3,:","))') chpest_hdr
            write (9000,*) "BASIN_CH_PEST             basin_ch_pest_aa.csv"
          end if
      end if
     end if
 
!----------------------------------------
             
    !! BASIN_RES_PESTICIDE - daily
      if (sp_ob%res > 0) then
       if (pco%pest%d == "y" .and. cs_db%num_tot > 0) then
        open (2848,file="basin_res_pest_day.txt",recl=800)
        write (2848,*) bsn%name, prog
        write (9000,*) "BASIN_RES_PEST            basin_res_pest_day.txt"
        write (2848,*) respest_hdr

          if (pco%csvout == "y") then
            open (2852,file="basin_res_pest_day.csv",recl=800)
            write (2852,*) bsn%name, prog
            write (2852,'(*(G0.3,:","))') respest_hdr
            write (9000,*) "BASIN_RES_PEST          reservoir_pest_day.csv"
          end if
       end if
      
!! BASIN_RES_PESTICIDE - monthly
      if (pco%pest%m == "y" .and. cs_db%num_tot > 0 ) then
        open (2849,file="basin_res_pest_mon.txt",recl=800)
        write (2849,*) bsn%name, prog
        write (9000,*) "BASIN_RES_PEST            basin_res_pest_mon.txt"
        write (2849,*) respest_hdr

          if (pco%csvout == "y") then
            open (2853,file="basin_res_pest_mon.csv",recl=800)
            write (2853,*) bsn%name, prog
            write (2853,'(*(G0.3,:","))') respest_hdr
            write (9000,*) "BASIN_RES_PEST            basin_res_pest_mon.csv" 
          end if
      end if
      
!! BASIN_RES_PESTICIDE - yearly
      if (pco%pest%y == "y" .and. cs_db%num_tot > 0) then
        open (2850,file="basin_res_pest_yr.txt",recl=800)
        write (2850,*) bsn%name, prog
        write (9000,*) "BASIN_RES_PEST            basin_res_pest_yr.txt"
        write (2850,*) respest_hdr

          if (pco%csvout == "y") then
            open (2854,file="basin_res_pest_yr.csv",recl=800)
            write (2854,*) bsn%name, prog
            write (2854,'(*(G0.3,:","))') respest_hdr
            write (9000,*) "BASIN_RES_PEST            basin_res_pest_yr.csv"
          end if
      end if
      
!! BASIN_RES_PESTICIDE - ave annual
      if (pco%pest%a == "y" .and. cs_db%num_tot > 0) then
        open (2851,file="basin_res_pest_aa.txt",recl=800)
        write (2851,*) bsn%name, prog
        write (9000,*) "BASIN_RES_PEST            basin_res_pest_aa.txt"
        write (2851,*) respest_hdr

          if (pco%csvout == "y") then
            open (2855,file="basin_res_pest_aa.csv",recl=800)
            write (2855,*) bsn%name, prog
            write (2855,'(*(G0.3,:","))') respest_hdr
            write (9000,*) "BASIN_RES_PEST            basin_res_pest_aa.csv"
          end if
      end if
      end if
 
!----------------------------------------
             
    !! BASIN_LS_PESTICIDE - daily
      if (sp_ob%hru > 0) then
       if (pco%pest%d == "y" .and. cs_db%num_tot > 0) then
        open (2864,file="basin_ls_pest_day.txt",recl=800)
        write (2864,*) bsn%name, prog
        write (9000,*) "BASIN_LS_PEST             basin_ls_pest_day.txt"
        write (2864,*) pestb_hdr

          if (pco%csvout == "y") then
            open (2868,file="basin_ls_pest_day.csv",recl=800)
            write (2868,*) bsn%name, prog
            write (2868,'(*(G0.3,:","))') pestb_hdr
            write (9000,*) "BASIN_LS_PEST             basin_ls_pest_day.csv"
          end if
       end if
      
!! BASIN_LS_PESTICIDE - monthly
      if (pco%pest%m == "y" .and. cs_db%num_tot > 0 ) then
        open (2865,file="basin_ls_pest_mon.txt",recl=800)
        write (2865,*) bsn%name, prog
        write (9000,*) "BASIN_LS_PEST             basin_ls_pest_mon.txt"
        write (2865,*) pestb_hdr

          if (pco%csvout == "y") then
            open (2869,file="basin_ls_pest_mon.csv",recl=800)
            write (2869,*) bsn%name, prog
            write (2869,'(*(G0.3,:","))') pestb_hdr
            write (9000,*) "BASIN_LS_PEST             basin_ls_pest_mon.csv"
          end if
      end if
      
!! BASIN_LS_PESTICIDE - yearly
      if (pco%pest%y == "y" .and. cs_db%num_tot > 0) then
        open (2866,file="basin_ls_pest_yr.txt",recl=800)
        write (2866,*) bsn%name, prog
        write (9000,*) "BASIN_LS_PEST             basin_ls_pest_yr.txt"
        write (2866,*) pestb_hdr

          if (pco%csvout == "y") then
            open (2870,file="basin_ls_pest_yr.csv",recl=800)
            write (2870,*) bsn%name, prog
            write (2870,'(*(G0.3,:","))') pestb_hdr
            write (9000,*) "BASIN_LS_PEST             basin_ls_pest_yr.csv"
          end if
      end if
      
!! BASIN_LS_PESTICIDE - ave annual
      if (pco%pest%a == "y" .and. cs_db%num_tot > 0) then
        open (2867,file="basin_ls_pest_aa.txt",recl=800)
        write (2867,*) bsn%name, prog
        write (9000,*) "BASIN_LS_PEST             basin_ls_pest_aa.txt"
        write (2867,*) pestb_hdr

          if (pco%csvout == "y") then
            open (2871,file="basin_ls_pest_aa.csv",recl=800)
            write (2871,*) bsn%name, prog
            write (2871,'(*(G0.3,:","))') pestb_hdr
            write (9000,*) "BASIN_LS_PEST             basin_ls_pest_aa.csv"
          end if
      end if
    end if
      
      return
      end subroutine header_pest  