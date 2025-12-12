     subroutine header_pest
    
     use basin_module
     use reservoir_module
     use hydrograph_module, only : sp_ob
     use output_ls_pesticide_module
     use constituent_mass_module
     use ch_pesticide_module
     use res_pesticide_module
     use aqu_pesticide_module
     use output_path_module
     
     implicit none 

    !! HRU_PESTICIDE - daily
     if (sp_ob%hru > 0) then
      if (pco%pest%d == "y" .and. cs_db%num_tot > 0) then
        call open_output_file(2800, "hru_pest_day.txt", 800)
        write (2800,*) bsn%name, prog
        write (9000,*) "HRU_PEST                  hru_pest_day.txt"
        write (2800,*) pestb_hdr

          if (pco%csvout == "y") then
            call open_output_file(2804, "hru_pest_day.csv", 800)
            write (2804,*) bsn%name, prog
            write (2804,'(*(G0.3,:","))') pestb_hdr
            write (9000,*) "HRU_PEST                  hru_pest_day.csv"
          end if
      end if
      
!! HRU_PESTICIDE - monthly
      if (pco%pest%m == "y" .and. cs_db%num_tot > 0 ) then
        call open_output_file(2801, "hru_pest_mon.txt", 800)
        write (2801,*) bsn%name, prog
        write (9000,*) "HRU_PEST                  hru_pest_mon.txt"
        write (2801,*) pestb_hdr

          if (pco%csvout == "y") then
            call open_output_file(2805, "hru_pest_mon.csv", 800)
            write (2805,*) bsn%name, prog
            write (2805,'(*(G0.3,:","))') pestb_hdr
            write (9000,*) "HRU_PEST                  hru_pest_mon.csv"
          end if
      end if
      
!! HRU_PESTICIDE - yearly
      if (pco%pest%y == "y" .and. cs_db%num_tot > 0) then
        call open_output_file(2802, "hru_pest_yr.txt", 800)
        write (2802,*) bsn%name, prog
        write (9000,*) "HRU_PEST                  hru_pest_yr.txt"
        write (2802,*) pestb_hdr

          if (pco%csvout == "y") then
            call open_output_file(2806, "hru_pest_yr.csv", 800)
            write (2806,*) bsn%name, prog
            write (2806,'(*(G0.3,:","))') pestb_hdr
            write (9000,*) "HRU_PEST                  hru_pest_yr.csv"
          end if
      end if
      
!! HRU_PESTICIDE - ave annual
      if (pco%pest%a == "y" .and. cs_db%num_tot > 0) then
        call open_output_file(2803, "hru_pest_aa.txt", 800)
        write (2803,*) bsn%name, prog
        write (9000,*) "HRU_PEST                  hru_pest_aa.txt"
        write (2803,*) pestb_hdr

          if (pco%csvout == "y") then
            call open_output_file(2807, "hru_pest_aa.csv", 800)
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
        call open_output_file(2808, "channel_pest_day.txt", 800)
        write (2808,*) bsn%name, prog
        write (9000,*) "CHANNEL_PEST              channel_pest_day.txt"
        write (2808,*) chpest_hdr

          if (pco%csvout == "y") then
            call open_output_file(2812, "channel_pest_day.csv", 800)
            write (2812,*) bsn%name, prog
            write (2812,'(*(G0.3,:","))') chpest_hdr
            write (9000,*) "CHANNEL_PEST              channel_pest_day.csv"
          end if
      end if
      
!! CHANNEL_PESTICIDE - monthly
      if (pco%pest%m == "y" .and. cs_db%num_tot > 0 ) then
        call open_output_file(2809, "channel_pest_mon.txt", 800)
        write (2809,*) bsn%name, prog
        write (9000,*) "CHANNEL_PEST              channel_pest_mon.txt"
        write (2809,*) chpest_hdr

          if (pco%csvout == "y") then
            call open_output_file(2813, "channel_pest_mon.csv", 800)
            write (2813,*) bsn%name, prog
            write (2813,'(*(G0.3,:","))') chpest_hdr
            write (9000,*) "CHANNEL_PEST              channel_pest_mon.csv"
          end if
      end if
      
!! CHANNEL_PESTICIDE - yearly
      if (pco%pest%y == "y" .and. cs_db%num_tot > 0) then
        call open_output_file(2810, "channel_pest_yr.txt", 800)
        write (2810,*) bsn%name, prog
        write (9000,*) "CHANNEL_PEST              channel_pest_yr.txt"
        write (2810,*) chpest_hdr

          if (pco%csvout == "y") then
            call open_output_file(2814, "channel_pest_yr.csv", 800)
            write (2814,*) bsn%name, prog
            write (2814,'(*(G0.3,:","))') chpest_hdr
            write (9000,*) "CHANNEL_PEST              channel_pest_yr.csv"
          end if
      end if
      
!! CHANNEL_PESTICIDE - ave annual
      if (pco%pest%a == "y" .and. cs_db%num_tot > 0) then
        call open_output_file(2811, "channel_pest_aa.txt", 800)
        write (2811,*) bsn%name, prog
        write (9000,*) "CHANNEL_PEST              channel_pest_aa.txt"
        write (2811,*) chpest_hdr

          if (pco%csvout == "y") then
            call open_output_file(2815, "channel_pest_aa.csv", 800)
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
        call open_output_file(2816, "reservoir_pest_day.txt", 800)
        write (2816,*) bsn%name, prog
        write (9000,*) "RESERVOIR_PEST            reservoir_pest_day.txt"
        write (2816,*) respest_hdr

          if (pco%csvout == "y") then
            call open_output_file(2820, "reservoir_pest_day.csv", 800)
            write (2820,*) bsn%name, prog
            write (2820,'(*(G0.3,:","))') respest_hdr
            write (9000,*) "RESERVOIR_PEST            reservoir_pest_day.csv"           
          end if
      end if
      
!! RESERVOIR_PESTICIDE - monthly
      if (pco%pest%m == "y" .and. cs_db%num_tot > 0 ) then
        call open_output_file(2817, "reservoir_pest_mon.txt", 800)
        write (2817,*) bsn%name, prog
        write (9000,*) "RESERVOIR_PEST            reservoir_pest_mon.txt"
        write (2817,*) respest_hdr

          if (pco%csvout == "y") then
            call open_output_file(2821, "reservoir_pest_mon.csv", 800)
            write (2821,*) bsn%name, prog
            write (2821,'(*(G0.3,:","))') respest_hdr
            write (9000,*) "RESERVOIR_PEST            reservoir_pest_mon.csv"
          end if
      end if
      
!! RESERVOIR_PESTICIDE - yearly
      if (pco%pest%y == "y" .and. cs_db%num_tot > 0) then
        call open_output_file(2818, "reservoir_pest_yr.txt", 800)
        write (2818,*) bsn%name, prog
        write (9000,*) "RESERVOIR_PEST            reservoir_pest_yr.txt"
        write (2818,*) respest_hdr

          if (pco%csvout == "y") then
            call open_output_file(2822, "reservoir_pest_yr.csv", 800)
            write (2822,*) bsn%name, prog
            write (2822,'(*(G0.3,:","))') respest_hdr
            write (9000,*) "RESERVOIR_PEST            reservoir_pest_yr.csv"
          end if
      end if
      
!! RESERVOIR_PESTICIDE - ave annual
      if (pco%pest%a == "y" .and. cs_db%num_tot > 0) then
        call open_output_file(2819, "reservoir_pest_aa.txt", 800)
        write (2819,*) bsn%name, prog
        write (9000,*) "RESERVOIR_PEST            reservoir_pest_aa.txt"
        write (2819,*) respest_hdr

          if (pco%csvout == "y") then
            call open_output_file(2823, "reservoir_pest_aa.csv", 800)
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
        call open_output_file(3000, "basin_aqu_pest_day.txt", 800)
        write (3000,*) bsn%name, prog
        write (9000,*) "BASIN_AQUIFER_PEST        basin_aqu_pest_day.txt"
        write (3000,*) aqupest_hdr

          if (pco%csvout == "y") then
            call open_output_file(3004, "basin_aqu_pest_day.csv", 800)
            write (3004,*) bsn%name, prog
            write (3004,'(*(G0.3,:","))') aqupest_hdr
            write (9000,*) "BASIN_AQUIFER_PEST        basin_aqu_pest_day.csv"
          end if
      end if
      
!! BASIN AQUIFER_PESTICIDE - monthly
      if (pco%pest%m == "y" .and. cs_db%num_tot > 0 ) then
        call open_output_file(3001, "basin_aqu_pest_mon.txt", 800)
        write (3001,*) bsn%name, prog
        write (9000,*) "BASIN_AQUIFER_PEST        basin_aqu_pest_mon.txt"
        write (3001,*) aqupest_hdr

          if (pco%csvout == "y") then
            call open_output_file(3005, "basin_aqu_pest_mon.csv", 800)
            write (3005,*) bsn%name, prog
            write (3005,'(*(G0.3,:","))') aqupest_hdr
            write (9000,*) "BASIN_AQUIFER_PEST        basin_aqu_pest_mon.csv"
          end if
      end if
      
!! BASIN AQUIFER_PESTICIDE - yearly
      if (pco%pest%y == "y" .and. cs_db%num_tot > 0) then
        call open_output_file(3002, "basin_aqu_pest_yr.txt", 800)
        write (3002,*) bsn%name, prog
        write (9000,*) "BASIN_AQUIFER_PEST        basin_aqu_pest_yr.txt"
        write (3002,*) aqupest_hdr

          if (pco%csvout == "y") then
            call open_output_file(3006, "basin_aqu_pest_yr.csv", 800)
            write (3006,*) bsn%name, prog
            write (3006,'(*(G0.3,:","))') aqupest_hdr
            write (9000,*) "BASIN_AQUIFER_PEST        basin_aqu_pest_yr.csv" 
          end if
      end if
      
!! BASIN AQUIFER_PESTICIDE - ave annual
      if (pco%pest%a == "y" .and. cs_db%num_tot > 0) then
        call open_output_file(3003, "basin_aqu_pest_aa.txt", 800)
        write (3003,*) bsn%name, prog
        write (9000,*) "BASIN_AQUIFER_PEST        basin_aqu_pest_aa.txt"
        write (3003,*) aqupest_hdr

          if (pco%csvout == "y") then
            call open_output_file(3007, "basin_aqu_pest_aa.csv", 800)
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
        call open_output_file(3008, "aquifer_pest_day.txt", 800)
        write (3008,*) bsn%name, prog
        write (9000,*) "AQUIFER_PEST              aquifer_pest_day.txt"
        write (3008,*) aqupest_hdr

          if (pco%csvout == "y") then
            call open_output_file(3012, "aquifer_pest_day.csv", 800)
            write (3012,*) bsn%name, prog
            write (3012,'(*(G0.3,:","))') aqupest_hdr
            write (9000,*) "AQUIFER_PEST              aquifer_pest_day.csv"
          end if
      end if
      
!! AQUIFER_PESTICIDE - monthly
      if (pco%pest%m == "y" .and. cs_db%num_tot > 0 ) then
        call open_output_file(3009, "aquifer_pest_mon.txt", 800)
        write (3009,*) bsn%name, prog
        write (9000,*) "AQUIFER_PEST              aquifer_pest_mon.txt"
        write (3009,*) aqupest_hdr

          if (pco%csvout == "y") then
            call open_output_file(3013, "aquifer_pest_mon.csv", 800)
            write (3013,*) bsn%name, prog
            write (3013,'(*(G0.3,:","))') aqupest_hdr
            write (9000,*) "AQUIFER_PEST              aquifer_pest_mon.csv"
          end if
      end if
      
!! AQUIFER_PESTICIDE - yearly
      if (pco%pest%y == "y" .and. cs_db%num_tot > 0) then
        call open_output_file(3010, "aquifer_pest_yr.txt", 800)
        write (3010,*) bsn%name, prog
        write (9000,*) "AQUIFER_PEST              aquifer_pest_yr.txt"
        write (3010,*) aqupest_hdr

          if (pco%csvout == "y") then
            call open_output_file(3014, "aquifer_pest_yr.csv", 800)
            write (3014,*) bsn%name, prog
            write (3014,'(*(G0.3,:","))') aqupest_hdr
            write (9000,*) "AQUIFER_PEST              aquifer_pest_yr.csv"
          end if
      end if
      
!! AQUIFER_PESTICIDE - ave annual
      if (pco%pest%a == "y" .and. cs_db%num_tot > 0) then
        call open_output_file(3011, "aquifer_pest_aa.txt", 800)
        write (3011,*) bsn%name, prog
        write (9000,*) "AQUIFER_PEST              aquifer_pest_aa.txt"
        write (3011,*) aqupest_hdr

          if (pco%csvout == "y") then
            call open_output_file(3015, "aquifer_pest_aa.csv", 800)
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
        call open_output_file(2832, "basin_ch_pest_day.txt", 800)
        write (2832,*) bsn%name, prog
        write (9000,*) "BASIN_CH_PEST             basin_ch_pest_day.txt"
        write (2832,*) chpest_hdr

          if (pco%csvout == "y") then
            call open_output_file(2836, "basin_ch_pest_day.csv", 800)
            write (2836,*) bsn%name, prog
            write (2836,'(*(G0.3,:","))') chpest_hdr
            write (9000,*) "BASIN_CH_PEST             reservoir_pest_day.csv"
          end if
       end if
      
!! BASIN_CH_PESTICIDE - monthly
      if (pco%pest%m == "y" .and. cs_db%num_tot > 0 ) then
        call open_output_file(2833, "basin_ch_pest_mon.txt", 800)
        write (2833,*) bsn%name, prog
        write (9000,*) "BASIN_CH_PEST             basin_ch_pest_mon.txt"
        write (2833,*) chpest_hdr

          if (pco%csvout == "y") then
            call open_output_file(2837, "basin_ch_pest_mon.csv", 800)
            write (2837,*) bsn%name, prog
            write (2837,'(*(G0.3,:","))') chpest_hdr
            write (9000,*) "BASIN_CH_PEST             basin_ch_pest_mon.csv"
          end if
      end if
      
!! BASIN_CH_PESTICIDE - yearly
      if (pco%pest%y == "y" .and. cs_db%num_tot > 0) then
        call open_output_file(2834, "basin_ch_pest_yr.txt", 800)
        write (2834,*) bsn%name, prog
        write (9000,*) "BASIN_CH_PEST             basin_ch_pest_yr.txt"
        write (2834,*) chpest_hdr

          if (pco%csvout == "y") then
            call open_output_file(2838, "basin_ch_pest_yr.csv", 800)
            write (2838,*) bsn%name, prog
            write (2838,'(*(G0.3,:","))') chpest_hdr
            write (9000,*) "BASIN_CH_PEST             basin_ch_pest_yr.csv"
          end if
      end if
      
!! BASIN_CH_PESTICIDE - ave annual
      if (pco%pest%a == "y" .and. cs_db%num_tot > 0) then
        call open_output_file(2835, "basin_ch_pest_aa.txt", 800)
        write (2835,*) bsn%name, prog
        write (9000,*) "BASIN_CH_PEST             basin_ch_pest_aa.txt"
        write (2835,*) chpest_hdr

          if (pco%csvout == "y") then
            call open_output_file(2839, "basin_ch_pest_aa.csv", 800)
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
        call open_output_file(2848, "basin_res_pest_day.txt", 800)
        write (2848,*) bsn%name, prog
        write (9000,*) "BASIN_RES_PEST            basin_res_pest_day.txt"
        write (2848,*) respest_hdr

          if (pco%csvout == "y") then
            call open_output_file(2852, "basin_res_pest_day.csv", 800)
            write (2852,*) bsn%name, prog
            write (2852,'(*(G0.3,:","))') respest_hdr
            write (9000,*) "BASIN_RES_PEST          reservoir_pest_day.csv"
          end if
       end if
      
!! BASIN_RES_PESTICIDE - monthly
      if (pco%pest%m == "y" .and. cs_db%num_tot > 0 ) then
        call open_output_file(2849, "basin_res_pest_mon.txt", 800)
        write (2849,*) bsn%name, prog
        write (9000,*) "BASIN_RES_PEST            basin_res_pest_mon.txt"
        write (2849,*) respest_hdr

          if (pco%csvout == "y") then
            call open_output_file(2853, "basin_res_pest_mon.csv", 800)
            write (2853,*) bsn%name, prog
            write (2853,'(*(G0.3,:","))') respest_hdr
            write (9000,*) "BASIN_RES_PEST            basin_res_pest_mon.csv" 
          end if
      end if
      
!! BASIN_RES_PESTICIDE - yearly
      if (pco%pest%y == "y" .and. cs_db%num_tot > 0) then
        call open_output_file(2850, "basin_res_pest_yr.txt", 800)
        write (2850,*) bsn%name, prog
        write (9000,*) "BASIN_RES_PEST            basin_res_pest_yr.txt"
        write (2850,*) respest_hdr

          if (pco%csvout == "y") then
            call open_output_file(2854, "basin_res_pest_yr.csv", 800)
            write (2854,*) bsn%name, prog
            write (2854,'(*(G0.3,:","))') respest_hdr
            write (9000,*) "BASIN_RES_PEST            basin_res_pest_yr.csv"
          end if
      end if
      
!! BASIN_RES_PESTICIDE - ave annual
      if (pco%pest%a == "y" .and. cs_db%num_tot > 0) then
        call open_output_file(2851, "basin_res_pest_aa.txt", 800)
        write (2851,*) bsn%name, prog
        write (9000,*) "BASIN_RES_PEST            basin_res_pest_aa.txt"
        write (2851,*) respest_hdr

          if (pco%csvout == "y") then
            call open_output_file(2855, "basin_res_pest_aa.csv", 800)
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
        call open_output_file(2864, "basin_ls_pest_day.txt", 800)
        write (2864,*) bsn%name, prog
        write (9000,*) "BASIN_LS_PEST             basin_ls_pest_day.txt"
        write (2864,*) pestb_hdr

          if (pco%csvout == "y") then
            call open_output_file(2868, "basin_ls_pest_day.csv", 800)
            write (2868,*) bsn%name, prog
            write (2868,'(*(G0.3,:","))') pestb_hdr
            write (9000,*) "BASIN_LS_PEST             basin_ls_pest_day.csv"
          end if
       end if
      
!! BASIN_LS_PESTICIDE - monthly
      if (pco%pest%m == "y" .and. cs_db%num_tot > 0 ) then
        call open_output_file(2865, "basin_ls_pest_mon.txt", 800)
        write (2865,*) bsn%name, prog
        write (9000,*) "BASIN_LS_PEST             basin_ls_pest_mon.txt"
        write (2865,*) pestb_hdr

          if (pco%csvout == "y") then
            call open_output_file(2869, "basin_ls_pest_mon.csv", 800)
            write (2869,*) bsn%name, prog
            write (2869,'(*(G0.3,:","))') pestb_hdr
            write (9000,*) "BASIN_LS_PEST             basin_ls_pest_mon.csv"
          end if
      end if
      
!! BASIN_LS_PESTICIDE - yearly
      if (pco%pest%y == "y" .and. cs_db%num_tot > 0) then
        call open_output_file(2866, "basin_ls_pest_yr.txt", 800)
        write (2866,*) bsn%name, prog
        write (9000,*) "BASIN_LS_PEST             basin_ls_pest_yr.txt"
        write (2866,*) pestb_hdr

          if (pco%csvout == "y") then
            call open_output_file(2870, "basin_ls_pest_yr.csv", 800)
            write (2870,*) bsn%name, prog
            write (2870,'(*(G0.3,:","))') pestb_hdr
            write (9000,*) "BASIN_LS_PEST             basin_ls_pest_yr.csv"
          end if
      end if
      
!! BASIN_LS_PESTICIDE - ave annual
      if (pco%pest%a == "y" .and. cs_db%num_tot > 0) then
        call open_output_file(2867, "basin_ls_pest_aa.txt", 800)
        write (2867,*) bsn%name, prog
        write (9000,*) "BASIN_LS_PEST             basin_ls_pest_aa.txt"
        write (2867,*) pestb_hdr

          if (pco%csvout == "y") then
            call open_output_file(2871, "basin_ls_pest_aa.csv", 800)
            write (2871,*) bsn%name, prog
            write (2871,'(*(G0.3,:","))') pestb_hdr
            write (9000,*) "BASIN_LS_PEST             basin_ls_pest_aa.csv"
          end if
      end if
    end if
      
      return
      end subroutine header_pest  