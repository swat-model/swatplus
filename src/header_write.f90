      subroutine header_write
    
      use basin_module
      use aquifer_module
      use channel_module
      use reservoir_module
      use hydrograph_module
      use sd_channel_module
      use maximum_data_module
      use calibration_data_module
      use output_path_module
      
      implicit none
     
      if (pco%fdcout == "y") then
        call open_output_file(6000, "flow_duration_curve.out", 800)
        write (6000,*) bsn%name, prog
        write (6000,*) fdc_hdr
        write (9000,*) "FDC                       flow_duration_curve.out"
      end if 

      !! hru-out.cal - hru soft calibration output including soft and predicted budgets and 
      !! calibration parameter adjustments
      if (cal_soft == "y") then
        open (4999,file="hru-out.cal", recl = 800)
        write (4999,*) bsn%name, prog
        write (4999,*) calb_hdr
        write (9000,*) "HRU_SOFT_CALIB_OUT        hru-out.cal"
      end if
      
!!!!!! hru-new.cal - hru soft calibration output file.  The same format as calibration.upd and
!!!!!! can be used as input (calibration.upd) in subsequent simulations
      if (cal_codes%hyd_hru /= "n") then
        open (5000,file="hru-new.cal", recl = 800)
      !  write (5000,*) " calibration.upd_developed_from_soft_data_calibration"
      !  write (9000,*) "HRU SOFT OUT CALIB  hru-new.cal"
      !  write (5000,*) calb3_hdr
      end if
      if (cal_codes%hyd_hru /= "n" .or. cal_codes%plt == "y") then
        open (5001,file="hydrology-cal.hyd", recl = 800)
      !  write (5000,*) " calibration.upd_developed_from_soft_data_calibration"
      !  write (9000,*) "HRU SOFT OUT CALIB  hru-new.cal"
      !  write (5000,*) calb3_hdr
      end if
      
!!!!!! hru-lte-out.cal - hru lte soft calibration output including soft and predicted budgets and 
!!!!!! calibration parameter adjustments
      !open (5003,file="hru-lte-out.cal", recl = 800)
      !write (9000,*) "LTE SOFT OUT CALIB  hru-lte-out.cal"
      !write (5003,*) calb_hdr
      
!!!!!! hru-lte-new.cal - hru lte soft calibration output file.  The same format as hru-lte.hru and
!!!!!! can be used as input (hru-lte.hru) in subsequent simulations 
      !open (5002,file="hru-lte-new.cal", recl = 800)
      !write (9000,*) "LTE SOFT CAL INPUT  hru-lte-new.cal"
      !write (5002,*) calb2_hdr
      
!! BASIN AQUIFER OUTPUT
        if (pco%aqu_bsn%d == "y") then
          call open_output_file(2090, "basin_aqu_day.txt", 1500)
          write (2090,*) bsn%name, prog
          write (2090,*) aqu_hdr
          write (2090,*) aqu_hdr_units
          write (9000,*) "BASIN_AQUIFER             basin_aqu_day.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2094, "basin_aqu_day.csv", 1500)
            write (2094,*) bsn%name, prog
            write (2094,'(*(G0.3,:","))') aqu_hdr
            write (2094,'(*(G0.3,:","))') aqu_hdr_units
            write (9000,*) "BASIN_AQUIFER             basin_aqu_day.csv"
          end if
        endif
        
      if (pco%aqu_bsn%m == "y") then
        call open_output_file(2091, "basin_aqu_mon.txt", 1500)
        write (2091,*) bsn%name, prog
        write (2091,*) aqu_hdr 
        write (2091,*) aqu_hdr_units
        write (9000,*) "BASIN_AQUIFER             basin_aqu_mon.txt"
         if (pco%csvout == "y") then 
           call open_output_file(2095, "basin_aqu_mon.csv", 1500)
           write (2095,*) bsn%name, prog
           write (2095,'(*(G0.3,:","))') aqu_hdr
           write (2095,'(*(G0.3,:","))') aqu_hdr_units
           write (9000,*) "BASIN_AQUIFER             basin_aqu_mon.csv"
         end if
      end if 
      
      if (pco%aqu_bsn%y == "y") then
        call open_output_file(2092, "basin_aqu_yr.txt", 1500)
        write (2092,*) bsn%name, prog
        write (2092,*) aqu_hdr
        write (2092,*) aqu_hdr_units
        write (9000,*) "BASIN_AQUIFER             basin_aqu_yr.txt"
         if (pco%csvout == "y") then 
           call open_output_file(2096, "basin_aqu_yr.csv", 1500)
           write (2096,*) bsn%name, prog
           write (2096,'(*(G0.3,:","))') aqu_hdr 
           write (2096,'(*(G0.3,:","))') aqu_hdr_units
           write (9000,*) "BASIN_AQUIFER             basin_aqu_yr.csv"
         end if
      end if 
      
     if (pco%aqu_bsn%a == "y") then
        call open_output_file(2093, "basin_aqu_aa.txt", 1500)
        write (2093,*) bsn%name, prog
        write (2093,*) aqu_hdr 
        write (2093,*) aqu_hdr_units
        write (9000,*) "BASIN_AQUIFER             basin_aqu_aa.txt"
         if (pco%csvout == "y") then 
           call open_output_file(2097, "basin_aqu_aa.csv", 1500)
           write (2097,*) bsn%name, prog
           write (2097,'(*(G0.3,:","))') aqu_hdr 
           write (2097,'(*(G0.3,:","))') aqu_hdr_units
           write (9000,*) "BASIN_AQUIFER             basin_aqu_aa.csv"
         end if
      end if 
!! BASIN AQUIFER OUTPUT

!! BASIN RESERVOIR OUTPUT
        if (pco%res_bsn%d == "y") then
          call open_output_file(2100, "basin_res_day.txt", 1500)
          write (2100,*) bsn%name, prog
          write (2100,*) ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
          write (2100,*) ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3          
          write (9000,*) "BASIN_RESERVOIR           basin_res_day.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2104, "basin_res_day.csv", 1500)
            write (2104,*) bsn%name, prog
            write (2104,'(*(G0.3,:","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
            write (2104,'(*(G0.3,:","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
            write (9000,*) "BASIN_RESERVOIR           basin_res_day.csv"
          end if
        endif
        
      if (pco%res_bsn%m == "y") then
        call open_output_file(2101, "basin_res_mon.txt", 1500)
        write (2101,*) bsn%name, prog
        write (2101,*) ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
        write (2101,*) ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
        write (9000,*) "BASIN_RESERVOIR           basin_res_mon.txt"
       if (pco%csvout == "y") then 
          call open_output_file(2105, "basin_res_mon.csv", 1500)
          write (2105,*) bsn%name, prog
          write (2105,'(*(G0.3,:","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
          write (2105,'(*(G0.3,:","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
          write (9000,*) "BASIN_RESERVOIR           basin_res_mon.csv"
       end if
      end if
       
       if (pco%res_bsn%y == "y") then
          call open_output_file(2102, "basin_res_yr.txt", 1500)
          write (2102,*) bsn%name, prog
          write (2102,*) ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
          write (2102,*) ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
          write (9000,*) "BASIN_RESERVOIR           basin_res_yr.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2106, "basin_res_yr.csv", 1500)
            write (2106,*) bsn%name, prog
            write (2106,'(*(G0.3,:","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
            write (2106,'(*(G0.3,:","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
            write (9000,*) "BASIN_RESERVOIR           basin_res_yr.csv"
          end if
       endif
        
      if (pco%res_bsn%a == "y") then
       call open_output_file(2103, "basin_res_aa.txt", 1500)
        write (2103,*) bsn%name, prog
        write (2103,*) ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
        write (2103,*) ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
        write (9000,*) "BASIN_RESERVOIR           basin_res_aa.txt"
       if (pco%csvout == "y") then 
          call open_output_file(2107, "basin_res_aa.csv", 1500)
          write (2107,*) bsn%name, prog
          write (2107,'(*(G0.3,:","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
          write (2107,'(*(G0.3,:","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
          write (9000,*) "BASIN_RESERVOIR           basin_res_aa.csv"
       end if
      end if
!! BASIN RESERVOIR OUTPUT
      
!! RECALL OUTPUT
        if (pco%recall%d == "y") then
          call open_output_file(4600, "recall_day.txt", 1500)
          write (4600,*) bsn%name, prog
          write (4600,*) hyd_hdr_time, hyd_hdr
          write (4600,*) hyd_hdr_units
          write (9000,*) "RECALL                    recall_day.txt"
          if (pco%csvout == "y") then 
            call open_output_file(4604, "recall_day.csv", 1500)
            write (4604,*) bsn%name, prog
            write (4604,'(*(G0.3,:","))') hyd_hdr_time, hyd_hdr 
            write (4604,'(*(G0.3,:","))') hyd_hdr_units3
            write (9000,*) "RECALL                    recall_day.csv"
          end if
        endif
        
        if (pco%recall%m == "y") then
        call open_output_file(4601, "recall_mon.txt", 1500)
        write (4601,*) bsn%name, prog
        write (4601,*) hyd_hdr_time, hyd_hdr
        write (4601,*) hyd_hdr_units
        write (9000,*) "RECALL                    recall_mon.txt"
         if (pco%csvout == "y") then 
            call open_output_file(4605, "recall_mon.csv", 1500)
            write (4605,*) bsn%name, prog
            write (4605,'(*(G0.3,:","))') hyd_hdr_time, hyd_hdr 
            write (4605,'(*(G0.3,:","))') hyd_hdr_units3
            write (9000,*) "RECALL                    recall_mon.csv"
         end if
       end if
       
        if (pco%recall%y == "y") then
          call open_output_file(4602, "recall_yr.txt", 1500)
          write (4602,*) bsn%name, prog
          write (4602,*) hyd_hdr_time, hyd_hdr            
          write (4602,*) hyd_hdr_units
          write (9000,*) "RECALL                    recall_yr.txt"
          if (pco%csvout == "y") then 
            call open_output_file(4606, "recall_yr.csv", 1500)
            write (4606,*) bsn%name, prog
            write (4606,'(*(G0.3,:","))') hyd_hdr_time, hyd_hdr 
            write (4606,'(*(G0.3,:","))') hyd_hdr_units3
            write (9000,*) "RECALL                    recall_yr.csv"
          end if
        endif
        
        if (pco%recall%a == "y") then 
        call open_output_file(4603, "recall_aa.txt", 1500) 
        write (4603,*) bsn%name, prog
        write (4603,*) hyd_hdr_time, hyd_hdr              
        write (4603,*) hyd_hdr_units
        write (9000,*) "RECALL_AA                 recall_aa.txt"
         if (pco%csvout == "y") then 
            call open_output_file(4607, "recall_aa.csv", 1500)
            write (4607,*) bsn%name, prog
            write (4607,'(*(G0.3,:","))') hyd_hdr_time, hyd_hdr 
            write (4607,'(*(G0.3,:","))') hyd_hdr_units3
            write (9000,*) "RECALL                    recall_aa.csv"
         end if
        end if
        
!! RECALL OUTPUT

!! BASIN CHANNEL OUTPUT
        if (pco%chan_bsn%d == "y") then
          call open_output_file(2110, "basin_cha_day.txt", 1500)
          write (2110,*) bsn%name, prog
          write (2110,*) ch_hdr
          write (2110,*) ch_hdr_units
          write (9000,*) "BASIN_CHANNEL             basin_cha_day.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2114, "basin_cha_day.csv", 1500)
            write (2114,*) bsn%name, prog
            write (2114,'(*(G0.3,:","))') ch_hdr
            write (2114,'(*(G0.3,:","))') ch_hdr_units
            write (9000,*) "BASIN_CHANNEL             basin_cha_day.txt"
          end if
        endif
        
       if (pco%chan_bsn%m == "y") then
        call open_output_file(2111, "basin_cha_mon.txt", 1500)
        write (2111,*) bsn%name, prog
        write (2111,*) ch_hdr
        write (2111,*) ch_hdr_units
        write (9000,*) "BASIN_CHANNEL             basin_cha_mon.txt"
         if (pco%csvout == "y") then 
           call open_output_file(2115, "basin_cha_mon.csv", 1500)
           write (2115,*) bsn%name, prog
           write (2115,'(*(G0.3,:","))') ch_hdr 
           write (2115,'(*(G0.3,:","))') ch_hdr_units
           write (9000,*) "BASIN_CHANNEL             basin_cha_mon.txt"
         end if
        end if
       
        if (pco%chan_bsn%y == "y") then
          call open_output_file(2112, "basin_cha_yr.txt", 1500)
          write (2112,*) bsn%name, prog
          write (2112,*) ch_hdr
          write (2112,*) ch_hdr_units
          write (9000,*) "BASIN_CHANNEL             basin_cha_yr.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2116, "basin_cha_yr.csv", 1500)
            write (2116,*) bsn%name, prog
            write (2116,'(*(G0.3,:","))') ch_hdr
            write (2116,'(*(G0.3,:","))') ch_hdr_units
            write (9000,*) "BASIN_CHANNEL             basin_cha_yr.csv"
          end if
        endif
        
        if (pco%chan_bsn%a == "y") then
          call open_output_file(2113, "basin_cha_aa.txt", 1500)
          write (2113,*) bsn%name, prog
          write (2113,*) ch_hdr
          write (2113,*) ch_hdr_units
          write (9000,*) "BASIN_CHANNEL             basin_cha_aa.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2117, "basin_cha_aa.csv", 1500)
            write (2117,*) bsn%name, prog
            write (2117,'(*(G0.3,:","))') ch_hdr
            write (2117,'(*(G0.3,:","))') ch_hdr_units
            write (9000,*) "BASIN_CHANNEL             basin_cha_aa.csv"
          end if
        end if
!! BASIN CHANNEL OUTPUT
        
!! BASIN SWAT DEG CHANNEL OUTPUT
        if (pco%sd_chan_bsn%d == "y") then
          call open_output_file(4900, "basin_sd_cha_day.txt", 1500)
          write (4900,*) bsn%name, prog
          write (4900,*) ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
          write (4900,*) ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units1, hyd_hdr_units1
          write (9000,*) "BASIN_SWAT_DEG_CHANNEL    basin_sd_cha_day.txt"
          if (pco%csvout == "y") then 
            call open_output_file(4904, "basin_sd_cha_day.csv", 1500)
            write (4904,*) bsn%name, prog
            write (4904,'(*(G0.3,:","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
            write (4904,'(*(G0.3,:","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units1, hyd_hdr_units1 
            write (9000,*) "BASIN_SWAT_DEG_CHANNEL    basin_sd_cha_day.csv"
          end if
        endif
        
       if (pco%sd_chan_bsn%m == "y") then
        call open_output_file(4901, "basin_sd_cha_mon.txt", 1500)
        write (4901,*) bsn%name, prog
        write (4901,*) ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
        write (4901,*) ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units1, hyd_hdr_units1 
        write (9000,*) "BASIN_SWAT_DEG_CHANNEL    basin_sd_cha_mon.txt"
         if (pco%csvout == "y") then 
           call open_output_file(4905, "basin_sd_cha_mon.csv", 1500)
           write (4905,*) bsn%name, prog
           write (4905,'(*(G0.3,:","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
           write (4905,'(*(G0.3,:","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units1, hyd_hdr_units1
           write (9000,*) "BASIN_SWAT_DEG_CHANNEL    basin_sd_cha_mon.csv"
         end if
        end if
       
        if (pco%sd_chan_bsn%y == "y") then
          call open_output_file(4902, "basin_sd_cha_yr.txt", 1500)
          write (4902,*) bsn%name, prog
          write (4902,*) ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
          write (4902,*) ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units1, hyd_hdr_units1
          write (9000,*) "BASIN_SWAT_DEG_CHANNEL    basin_sd_cha_yr.txt"
          if (pco%csvout == "y") then 
            call open_output_file(4906, "basin_sd_cha_yr.csv", 1500)
            write (4906,*) bsn%name, prog
            write (4906,'(*(G0.3,:","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
            write (4906,'(*(G0.3,:","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units1, hyd_hdr_units1 
            write (9000,*) "BASIN_SWAT_DEG_CHANNEL    basin_sd_cha_yr.csv"
          end if
        endif
        
        if (pco%sd_chan_bsn%a == "y") then
          call open_output_file(4903, "basin_sd_cha_aa.txt", 1500)
          write (4903,*) bsn%name, prog
          write (4903,*) ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
          write (4903,*) ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units1, hyd_hdr_units1 
          write (9000,*) "BASIN_SWAT_DEG_CHANNEL    basin_sd_cha_aa.txt"
          if (pco%csvout == "y") then 
            call open_output_file(4907, "basin_sd_cha_aa.csv", 1500)
            write (4907,*) bsn%name, prog
            write (4907,'(*(G0.3,:","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
            write (4907,'(*(G0.3,:","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units1, hyd_hdr_units1
            write (9000,*) "BASIN_SWAT_DEG_CHANNEL    basin_sd_cha_aa.csv"
          end if
        end if
!! BASIN SWAT DEG CHANNEL OUTPUT


!! BASIN SWAT DEG CHANNEL MORPH OUTPUT
        if (pco%sd_chan_bsn%d == "y") then
          call open_output_file(2120, "basin_sd_chamorph_day.txt", 1500)
          write (2120,*) bsn%name, prog
          write (2120,*) sdch_hdr
          write (2120,*) sdch_hdr_units
          write (9000,*) "BASIN_SWAT_DEG_CHAN_MORPH basin_sd_chamorph_day.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2124, "basin_sd_chamorph_day.csv", 1500)
            write (2124,*) bsn%name, prog
            write (2124,'(*(G0.3,:","))') sdch_hdr
            write (2124,'(*(G0.3,:","))') sdch_hdr_units        
            write (9000,*) "BASIN_SWAT_DEG_CHAN_MORPH basin_sd_chamorph_day.csv"
          end if
        endif
        
       if (pco%sd_chan_bsn%m == "y") then
        call open_output_file(2121, "basin_sd_chamorph_mon.txt", 1500)
        write (2121,*) bsn%name, prog
        write (2121,*) sdch_hdr
        write (2121,*) sdch_hdr_units
        write (9000,*) "BASIN_SWAT_DEG_CHAN_MORPH basin_sd_chamorph_mon.txt"
         if (pco%csvout == "y") then 
           call open_output_file(2125, "basin_sd_chamorph_mon.csv", 1500)
           write (2125,*) bsn%name, prog
           write (2125,'(*(G0.3,:","))') sdch_hdr 
           write (2125,'(*(G0.3,:","))') sdch_hdr_units        
           write (9000,*) "BASIN_SWAT_DEG_CHAN_MORPH basin_sd_chamorph_mon.csv"
         end if
        end if
       
        if (pco%sd_chan_bsn%y == "y") then
          call open_output_file(2122, "basin_sd_chamorph_yr.txt", 1500)
          write (2122,*) bsn%name, prog
          write (2122,*) sdch_hdr 
          write (2122,*) sdch_hdr_units
          write (9000,*) "BASIN_SWAT_DEG_CHAN_MORPH basin_sd_chamorph_yr.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2126, "basin_sd_chamorph_yr.csv", 1500)
            write (2126,*) bsn%name, prog
            write (2126,'(*(G0.3,:","))') sdch_hdr
            write (2126,'(*(G0.3,:","))') sdch_hdr_units        
            write (9000,*) "BASIN_SWAT_DEG_CHAN_MORPH basin_sd_chamorph_yr.csv"
          end if
        endif
        
        if (pco%sd_chan_bsn%a == "y") then
          call open_output_file(2123, "basin_sd_chamorph_aa.txt", 1500)
          write (2123,*) bsn%name, prog
          write (2123,*) sdch_hdr 
          write (2123,*) sdch_hdr_units
          write (9000,*) "BASIN_SWAT_DEG_CHAN_MORPH basin_sd_chamorph_aa.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2127, "basin_sd_chamorph_aa.csv", 1500)
            write (2127,*) bsn%name, prog
            write (2127,'(*(G0.3,:","))') sdch_hdr 
            write (2127,'(*(G0.3,:","))') sdch_hdr_units 
            write (9000,*) "BASIN_SWAT_DEG_CHAN_MORPH basin_sd_chamorph_aa.csv"
          end if
        end if
!! BASIN SWAT DEG CHANNEL MORPH OUTPUT
        
!! BASIN SWAT DEG CHANBUD OUTPUT
        if (pco%sd_chan_bsn%d == "y") then
          call open_output_file(2128, "basin_sd_chanbud_day.txt", 1500)
          write (2128,*) bsn%name, prog
          write (2128,*) sdch_bud_hdr
          write (2128,*) sdch_bud_hdr_units
          write (9000,*) "BASIN_SWAT_DEG_CHAN_BUD   basin_sd_chanbud_day.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2132, "basin_sd_chanbud_day.csv", 1500)
            write (2132,*) bsn%name, prog
            write (2132,'(*(G0.3,:","))') sdch_bud_hdr
            write (2132,'(*(G0.3,:","))') sdch_bud_hdr_units        
            write (9000,*) "BASIN_SWAT_DEG_CHAN_BUD   basin_sd_chanbud_day.csv"
          end if
        endif
        
       if (pco%sd_chan_bsn%m == "y") then
        call open_output_file(2129, "basin_sd_chanbud_mon.txt", 1500)
        write (2129,*) bsn%name, prog
        write (2129,*) sdch_bud_hdr
        write (2129,*) sdch_bud_hdr_units
        write (9000,*) "BASIN_SWAT_DEG_CHAN_BUD   basin_sd_chanbud_mon.txt"
         if (pco%csvout == "y") then 
           call open_output_file(2133, "basin_sd_chanbud_mon.csv", 1500)
           write (2133,*) bsn%name, prog
           write (2133,'(*(G0.3,:","))') sdch_bud_hdr 
           write (2133,'(*(G0.3,:","))') sdch_bud_hdr_units        
           write (9000,*) "BASIN_SWAT_DEG_CHAN_BUD   basin_sd_chanbud_mon.csv"
         end if
        end if
       
        if (pco%sd_chan_bsn%y == "y") then
          call open_output_file(2130, "basin_sd_chanbud_yr.txt", 1500)
          write (2130,*) bsn%name, prog
          write (2130,*) sdch_bud_hdr 
          write (2130,*) sdch_bud_hdr_units
          write (9000,*) "BASIN_SWAT_DEG_CHAN_BUD   basin_sd_chanbud_yr.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2134, "basin_sd_chanbud_yr.csv", 1500)
            write (2134,*) bsn%name, prog
            write (2134,'(*(G0.3,:","))') sdch_bud_hdr
            write (2134,'(*(G0.3,:","))') sdch_bud_hdr_units        
            write (9000,*) "BASIN_SWAT_DEG_CHAN_BUD   basin_sd_chanbud_yr.csv"
          end if
        endif
        
        if (pco%sd_chan_bsn%a == "y") then
          call open_output_file(2131, "basin_sd_chanbud_aa.txt", 1500)
          write (2131,*) bsn%name, prog
          write (2131,*) sdch_bud_hdr 
          write (2131,*) sdch_bud_hdr_units
          write (9000,*) "BASIN_SWAT_DEG_CHAN_BUD   basin_sd_chanbud_aa.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2135, "basin_sd_chanbud_aa.csv", 1500)
            write (2135,*) bsn%name, prog
            write (2135,'(*(G0.3,:","))') sdch_bud_hdr 
            write (2135,'(*(G0.3,:","))') sdch_bud_hdr_units 
            write (9000,*) "BASIN_SWAT_DEG_CHAN_BUD   basin_sd_chanbud_aa.csv"
          end if
        end if
!! BASIN SWAT DEG CHANBUD OUTPUT


!! BASIN RECALL OUTPUT (PTS - Point Source)
        if (pco%recall_bsn%d == "y") then
          call open_output_file(4500, "basin_psc_day.txt", 1500)
          write (4500,*) bsn%name, prog
          write (4500,*) rec_hdr_time, hyd_hdr             
          write (4500,*) hyd_hdr_units
          write (9000,*) "BASIN_RECALL              basin_psc_day.txt"
          if (pco%csvout == "y") then 
            call open_output_file(4504, "basin_psc_day.csv", 1500)
            write (4504,*) bsn%name, prog
            write (4504,'(*(G0.3,:","))') rec_hdr_time, hyd_hdr 
            write (4504,'(*(G0.3,:","))') hyd_hdr_units
            write (9000,*) "BASIN_RECALL              basin_psc_day.csv"
          end if
        endif
        
        if (pco%recall_bsn%m == "y") then
        call open_output_file(4501, "basin_psc_mon.txt", 1500)
        write (4501,*) bsn%name, prog
        write (4501,*) rec_hdr_time, hyd_hdr
        write (4501,*) hyd_hdr_units
        write (9000,*) "BASIN_RECALL              basin_psc_mon.txt"
         if (pco%csvout == "y") then 
            call open_output_file(4505, "basin_psc_mon.csv", 1500)
            write (4505,*) bsn%name, prog
            write (4505,'(*(G0.3,:","))') rec_hdr_time, hyd_hdr 
            write (4505,'(*(G0.3,:","))') hyd_hdr_units
            write (9000,*) "BASIN_RECALL              basin_psc_mon.csv"
         end if
       end if
       
        if (pco%recall_bsn%y == "y") then
          call open_output_file(4502, "basin_psc_yr.txt", 1500)
          write (4502,*) bsn%name, prog
          write (4502,*) rec_hdr_time, hyd_hdr
          write (4502,*) hyd_hdr_units
          write (9000,*) "BASIN_RECALL              basin_psc_yr.txt"
          if (pco%csvout == "y") then 
            call open_output_file(4506, "basin_psc_yr.csv", 1500)
            write (4506,*) bsn%name, prog
            write (4506,'(*(G0.3,:","))') rec_hdr_time, hyd_hdr 
            write (4506,'(*(G0.3,:","))') hyd_hdr_units
            write (9000,*) "BASIN_RECALL              basin_psc_yr.csv"
          end if
        endif
        
        if (pco%recall_bsn%a == "y") then 
        call open_output_file(4503, "basin_psc_aa.txt", 1500) 
        write (4503,*) bsn%name, prog
        write (4503,*) rec_hdr_time, hyd_hdr
        write (4503,*) hyd_hdr_units
        write (9000,*) "BASIN_RECALL_AA           basin_psc_aa.txt"
         if (pco%csvout == "y") then 
            call open_output_file(4507, "basin_psc_aa.csv", 1500)
            write (4507,*) bsn%name, prog
            write (4507,'(*(G0.3,:","))') rec_hdr_time, hyd_hdr 
            write (4507,'(*(G0.3,:","))') hyd_hdr_units
            write (9000,*) "BASIN_RECALL_AA           basin_psc_aa.csv"
         end if
        end if
        
!! BASIN RECALL OUTPUT

!! BASIN ROUTING UNIT OUTPUT
        if (pco%ru%d == "y") then
          call open_output_file(2600, "ru_day.txt", 1500)
          write (2600,*) bsn%name, prog
          write (2600,*) hyd_hdr_time, hyd_hdr  
          write (2600,*) hyd_hdr_units
          write (9000,*) "ROUTING_UNITS             ru_day.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2604, "ru_day.csv", 1500)
            write (2604,*) bsn%name, prog
            write (2604,'(*(G0.3,:","))') hyd_hdr_time, hyd_hdr
            write (2604,'(*(G0.3,:","))') hyd_hdr_units
            write (9000,*) "ROUTING_UNITS             ru_day.csv"
          end if
        endif
        
        if (pco%ru%m == "y") then
        call open_output_file(2601, "ru_mon.txt", 1500)
        write (2601,*) bsn%name, prog
        write (2601,*) hyd_hdr_time, hyd_hdr 
        write (2601,*) hyd_hdr_units
        write (9000,*) "ROUTING_UNITS             ru_mon.txt"
        if (pco%csvout == "y") then 
            call open_output_file(2605, "ru_mon.csv", 1500)
            write (2605,*) bsn%name, prog
            write (2605,'(*(G0.3,:","))') hyd_hdr_time, hyd_hdr
            write (2605,'(*(G0.3,:","))') hyd_hdr_units
            write (9000,*) "ROUTING_UNITS             ru_mon.csv"
         end if
       end if
       
        if (pco%ru%y == "y") then
          call open_output_file(2602, "ru_yr.txt", 1500)
          write (2602,*) bsn%name, prog
          write (2602,*) hyd_hdr_time, hyd_hdr 
          write (2602,*) hyd_hdr_units
          write (9000,*) "ROUTING_UNITS             ru_yr.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2606, "ru_yr.csv", 1500)
            write (2606,*) bsn%name, prog
            write (2606,'(*(G0.3,:","))') hyd_hdr_time, hyd_hdr
            write (2606,'(*(G0.3,:","))') hyd_hdr_units
            write (9000,*) "ROUTING_UNITS             ru_yr.csv"
          end if
        endif
        
        if (pco%ru%a == "y") then 
        call open_output_file(2603, "ru_aa.txt", 1500) 
        write (2603,*) bsn%name, prog
        write (2603,*) hyd_hdr_time, hyd_hdr 
        write (2603,*) hyd_hdr_units
        write (9000,*) "ROUTING_UNITS             ru_aa.txt"
         if (pco%csvout == "y") then 
            call open_output_file(2607, "ru_aa.csv", 1500)
            write (2607,*) bsn%name, prog
            write (2607,'(*(G0.3,:","))') hyd_hdr_time, hyd_hdr
            write (2607,'(*(G0.3,:","))') hyd_hdr_units
            write (9000,*) "ROUTING_UNITS             ru_aa.csv"
         end if
        end if
        
!! BASIN ROUTING UNIT OUTPUT

      return
      end subroutine header_write  