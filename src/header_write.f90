      subroutine header_write
    
      use basin_module
      use aquifer_module
      use channel_module
      use reservoir_module
      use hydrograph_module
      use sd_channel_module
      use maximum_data_module
      use calibration_data_module
      
      implicit none
     
      if (pco%fdcout == "y") then
        open (6000,file="flow_duration_curve.out", recl=800)
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
          open (2090,file="basin_aqu_day.txt", recl = 1500)
          write (2090,*) bsn%name, prog
          write (2090,*) aqu_hdr
          write (2090,*) aqu_hdr_units
          write (9000,*) "BASIN_AQUIFER             basin_aqu_day.txt"
          if (pco%csvout == "y") then 
            open (2094,file="basin_aqu_day.csv", recl = 1500)
            write (2094,*) bsn%name, prog
            write (2094,'(*(G0.3,:","))') aqu_hdr
            write (2094,'(*(G0.3,:","))') aqu_hdr_units
            write (9000,*) "BASIN_AQUIFER             basin_aqu_day.csv"
          end if
        endif
        
      if (pco%aqu_bsn%m == "y") then
        open (2091,file="basin_aqu_mon.txt",recl = 1500)
        write (2091,*) bsn%name, prog
        write (2091,*) aqu_hdr 
        write (2091,*) aqu_hdr_units
        write (9000,*) "BASIN_AQUIFER             basin_aqu_mon.txt"
         if (pco%csvout == "y") then 
           open (2095,file="basin_aqu_mon.csv",recl = 1500)
           write (2095,*) bsn%name, prog
           write (2095,'(*(G0.3,:","))') aqu_hdr
           write (2095,'(*(G0.3,:","))') aqu_hdr_units
           write (9000,*) "BASIN_AQUIFER             basin_aqu_mon.csv"
         end if
      end if 
      
      if (pco%aqu_bsn%y == "y") then
        open (2092,file="basin_aqu_yr.txt",recl = 1500)
        write (2092,*) bsn%name, prog
        write (2092,*) aqu_hdr
        write (2092,*) aqu_hdr_units
        write (9000,*) "BASIN_AQUIFER             basin_aqu_yr.txt"
         if (pco%csvout == "y") then 
           open (2096,file="basin_aqu_yr.csv",recl = 1500)
           write (2096,*) bsn%name, prog
           write (2096,'(*(G0.3,:","))') aqu_hdr 
           write (2096,'(*(G0.3,:","))') aqu_hdr_units
           write (9000,*) "BASIN_AQUIFER             basin_aqu_yr.csv"
         end if
      end if 
      
     if (pco%aqu_bsn%a == "y") then
        open (2093,file="basin_aqu_aa.txt",recl = 1500)
        write (2093,*) bsn%name, prog
        write (2093,*) aqu_hdr 
        write (2093,*) aqu_hdr_units
        write (9000,*) "BASIN_AQUIFER             basin_aqu_aa.txt"
         if (pco%csvout == "y") then 
           open (2097,file="basin_aqu_aa.csv",recl = 1500)
           write (2097,*) bsn%name, prog
           write (2097,'(*(G0.3,:","))') aqu_hdr 
           write (2097,'(*(G0.3,:","))') aqu_hdr_units
           write (9000,*) "BASIN_AQUIFER             basin_aqu_aa.csv"
         end if
      end if 
!! BASIN AQUIFER OUTPUT

!! BASIN RESERVOIR OUTPUT
        if (pco%res_bsn%d == "y") then
          open (2100,file="basin_res_day.txt", recl = 1500)
          write (2100,*) bsn%name, prog
          write (2100,*) ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
          write (2100,*) ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3          
          write (9000,*) "BASIN_RESERVOIR           basin_res_day.txt"
          if (pco%csvout == "y") then 
            open (2104,file="basin_res_day.csv", recl = 1500)
            write (2104,*) bsn%name, prog
            write (2104,'(*(G0.3,:","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
            write (2104,'(*(G0.3,:","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
            write (9000,*) "BASIN_RESERVOIR           basin_res_day.csv"
          end if
        endif
        
      if (pco%res_bsn%m == "y") then
        open (2101,file="basin_res_mon.txt",recl = 1500)
        write (2101,*) bsn%name, prog
        write (2101,*) ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
        write (2101,*) ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
        write (9000,*) "BASIN_RESERVOIR           basin_res_mon.txt"
       if (pco%csvout == "y") then 
          open (2105,file="basin_res_mon.csv",recl = 1500)
          write (2105,*) bsn%name, prog
          write (2105,'(*(G0.3,:","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
          write (2105,'(*(G0.3,:","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
          write (9000,*) "BASIN_RESERVOIR           basin_res_mon.csv"
       end if
      end if
       
       if (pco%res_bsn%y == "y") then
          open (2102,file="basin_res_yr.txt", recl = 1500)
          write (2102,*) bsn%name, prog
          write (2102,*) ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
          write (2102,*) ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
          write (9000,*) "BASIN_RESERVOIR           basin_res_yr.txt"
          if (pco%csvout == "y") then 
            open (2106,file="basin_res_yr.csv", recl = 1500)
            write (2106,*) bsn%name, prog
            write (2106,'(*(G0.3,:","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
            write (2106,'(*(G0.3,:","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
            write (9000,*) "BASIN_RESERVOIR           basin_res_yr.csv"
          end if
       endif
        
      if (pco%res_bsn%a == "y") then
       open (2103,file="basin_res_aa.txt",recl = 1500)
        write (2103,*) bsn%name, prog
        write (2103,*) ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
        write (2103,*) ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
        write (9000,*) "BASIN_RESERVOIR           basin_res_aa.txt"
       if (pco%csvout == "y") then 
          open (2107,file="basin_res_aa.csv",recl = 1500)
          write (2107,*) bsn%name, prog
          write (2107,'(*(G0.3,:","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
          write (2107,'(*(G0.3,:","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units3, hyd_hdr_units3
          write (9000,*) "BASIN_RESERVOIR           basin_res_aa.csv"
       end if
      end if
!! BASIN RESERVOIR OUTPUT
      
!! RECALL OUTPUT
        if (pco%recall%d == "y") then
          open (4600,file="recall_day.txt", recl = 1500)
          write (4600,*) bsn%name, prog
          write (4600,*) hyd_hdr_time, hyd_hdr
          write (4600,*) hyd_hdr_units
          write (9000,*) "RECALL                    recall_day.txt"
          if (pco%csvout == "y") then 
            open (4604,file="recall_day.csv", recl = 1500)
            write (4604,*) bsn%name, prog
            write (4604,'(*(G0.3,:","))') hyd_hdr_time, hyd_hdr 
            write (4604,'(*(G0.3,:","))') hyd_hdr_units3
            write (9000,*) "RECALL                    recall_day.csv"
          end if
        endif
        
        if (pco%recall%m == "y") then
        open (4601,file="recall_mon.txt",recl = 1500)
        write (4601,*) bsn%name, prog
        write (4601,*) hyd_hdr_time, hyd_hdr
        write (4601,*) hyd_hdr_units
        write (9000,*) "RECALL                    recall_mon.txt"
         if (pco%csvout == "y") then 
            open (4605,file="recall_mon.csv",recl = 1500)
            write (4605,*) bsn%name, prog
            write (4605,'(*(G0.3,:","))') hyd_hdr_time, hyd_hdr 
            write (4605,'(*(G0.3,:","))') hyd_hdr_units3
            write (9000,*) "RECALL                    recall_mon.csv"
         end if
       end if
       
        if (pco%recall%y == "y") then
          open (4602,file="recall_yr.txt", recl = 1500)
          write (4602,*) bsn%name, prog
          write (4602,*) hyd_hdr_time, hyd_hdr            
          write (4602,*) hyd_hdr_units
          write (9000,*) "RECALL                    recall_yr.txt"
          if (pco%csvout == "y") then 
            open (4606,file="recall_yr.csv", recl = 1500)
            write (4606,*) bsn%name, prog
            write (4606,'(*(G0.3,:","))') hyd_hdr_time, hyd_hdr 
            write (4606,'(*(G0.3,:","))') hyd_hdr_units3
            write (9000,*) "RECALL                    recall_yr.csv"
          end if
        endif
        
        if (pco%recall%a == "y") then 
        open (4603,file="recall_aa.txt",recl = 1500) 
        write (4603,*) bsn%name, prog
        write (4603,*) hyd_hdr_time, hyd_hdr              
        write (4603,*) hyd_hdr_units
        write (9000,*) "RECALL_AA                 recall_aa.txt"
         if (pco%csvout == "y") then 
            open (4607,file="recall_aa.csv",recl = 1500)
            write (4607,*) bsn%name, prog
            write (4607,'(*(G0.3,:","))') hyd_hdr_time, hyd_hdr 
            write (4607,'(*(G0.3,:","))') hyd_hdr_units3
            write (9000,*) "RECALL                    recall_aa.csv"
         end if
        end if
        
!! RECALL OUTPUT

!! BASIN CHANNEL OUTPUT
        if (pco%chan_bsn%d == "y") then
          open (2110,file="basin_cha_day.txt", recl = 1500)
          write (2110,*) bsn%name, prog
          write (2110,*) ch_hdr
          write (2110,*) ch_hdr_units
          write (9000,*) "BASIN_CHANNEL             basin_cha_day.txt"
          if (pco%csvout == "y") then 
            open (2114,file="basin_cha_day.csv", recl = 1500)
            write (2114,*) bsn%name, prog
            write (2114,'(*(G0.3,:","))') ch_hdr
            write (2114,'(*(G0.3,:","))') ch_hdr_units
            write (9000,*) "BASIN_CHANNEL             basin_cha_day.txt"
          end if
        endif
        
       if (pco%chan_bsn%m == "y") then
        open (2111,file="basin_cha_mon.txt",recl = 1500)
        write (2111,*) bsn%name, prog
        write (2111,*) ch_hdr
        write (2111,*) ch_hdr_units
        write (9000,*) "BASIN_CHANNEL             basin_cha_mon.txt"
         if (pco%csvout == "y") then 
           open (2115,file="basin_cha_mon.csv",recl = 1500)
           write (2115,*) bsn%name, prog
           write (2115,'(*(G0.3,:","))') ch_hdr 
           write (2115,'(*(G0.3,:","))') ch_hdr_units
           write (9000,*) "BASIN_CHANNEL             basin_cha_mon.txt"
         end if
        end if
       
        if (pco%chan_bsn%y == "y") then
          open (2112,file="basin_cha_yr.txt", recl = 1500)
          write (2112,*) bsn%name, prog
          write (2112,*) ch_hdr
          write (2112,*) ch_hdr_units
          write (9000,*) "BASIN_CHANNEL             basin_cha_yr.txt"
          if (pco%csvout == "y") then 
            open (2116,file="basin_cha_yr.csv", recl = 1500)
            write (2116,*) bsn%name, prog
            write (2116,'(*(G0.3,:","))') ch_hdr
            write (2116,'(*(G0.3,:","))') ch_hdr_units
            write (9000,*) "BASIN_CHANNEL             basin_cha_yr.csv"
          end if
        endif
        
        if (pco%chan_bsn%a == "y") then
          open (2113,file="basin_cha_aa.txt",recl = 1500)
          write (2113,*) bsn%name, prog
          write (2113,*) ch_hdr
          write (2113,*) ch_hdr_units
          write (9000,*) "BASIN_CHANNEL             basin_cha_aa.txt"
          if (pco%csvout == "y") then 
            open (2117,file="basin_cha_aa.csv",recl = 1500)
            write (2117,*) bsn%name, prog
            write (2117,'(*(G0.3,:","))') ch_hdr
            write (2117,'(*(G0.3,:","))') ch_hdr_units
            write (9000,*) "BASIN_CHANNEL             basin_cha_aa.csv"
          end if
        end if
!! BASIN CHANNEL OUTPUT
        
!! BASIN SWAT DEG CHANNEL OUTPUT
        if (pco%sd_chan_bsn%d == "y") then
          open (4900,file="basin_sd_cha_day.txt", recl = 1500)
          write (4900,*) bsn%name, prog
          write (4900,*) ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
          write (4900,*) ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units1, hyd_hdr_units1
          write (9000,*) "BASIN_SWAT_DEG_CHANNEL    basin_sd_cha_day.txt"
          if (pco%csvout == "y") then 
            open (4904,file="basin_sd_cha_day.csv", recl = 1500)
            write (4904,*) bsn%name, prog
            write (4904,'(*(G0.3,:","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
            write (4904,'(*(G0.3,:","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units1, hyd_hdr_units1 
            write (9000,*) "BASIN_SWAT_DEG_CHANNEL    basin_sd_cha_day.csv"
          end if
        endif
        
       if (pco%sd_chan_bsn%m == "y") then
        open (4901,file="basin_sd_cha_mon.txt",recl = 1500)
        write (4901,*) bsn%name, prog
        write (4901,*) ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
        write (4901,*) ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units1, hyd_hdr_units1 
        write (9000,*) "BASIN_SWAT_DEG_CHANNEL    basin_sd_cha_mon.txt"
         if (pco%csvout == "y") then 
           open (4905,file="basin_sd_cha_mon.csv",recl = 1500)
           write (4905,*) bsn%name, prog
           write (4905,'(*(G0.3,:","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
           write (4905,'(*(G0.3,:","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units1, hyd_hdr_units1
           write (9000,*) "BASIN_SWAT_DEG_CHANNEL    basin_sd_cha_mon.csv"
         end if
        end if
       
        if (pco%sd_chan_bsn%y == "y") then
          open (4902,file="basin_sd_cha_yr.txt", recl = 1500)
          write (4902,*) bsn%name, prog
          write (4902,*) ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
          write (4902,*) ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units1, hyd_hdr_units1
          write (9000,*) "BASIN_SWAT_DEG_CHANNEL    basin_sd_cha_yr.txt"
          if (pco%csvout == "y") then 
            open (4906,file="basin_sd_cha_yr.csv", recl = 1500)
            write (4906,*) bsn%name, prog
            write (4906,'(*(G0.3,:","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
            write (4906,'(*(G0.3,:","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units1, hyd_hdr_units1 
            write (9000,*) "BASIN_SWAT_DEG_CHANNEL    basin_sd_cha_yr.csv"
          end if
        endif
        
        if (pco%sd_chan_bsn%a == "y") then
          open (4903,file="basin_sd_cha_aa.txt",recl = 1500)
          write (4903,*) bsn%name, prog
          write (4903,*) ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
          write (4903,*) ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units1, hyd_hdr_units1 
          write (9000,*) "BASIN_SWAT_DEG_CHANNEL    basin_sd_cha_aa.txt"
          if (pco%csvout == "y") then 
            open (4907,file="basin_sd_cha_aa.csv",recl = 1500)
            write (4907,*) bsn%name, prog
            write (4907,'(*(G0.3,:","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr
            write (4907,'(*(G0.3,:","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units1, hyd_hdr_units1
            write (9000,*) "BASIN_SWAT_DEG_CHANNEL    basin_sd_cha_aa.csv"
          end if
        end if
!! BASIN SWAT DEG CHANNEL OUTPUT


!! BASIN SWAT DEG CHANNEL MORPH OUTPUT
        if (pco%sd_chan_bsn%d == "y") then
          open (2120,file="basin_sd_chamorph_day.txt", recl = 1500)
          write (2120,*) bsn%name, prog
          write (2120,*) sdch_hdr
          write (2120,*) sdch_hdr_units
          write (9000,*) "BASIN_SWAT_DEG_CHAN_MORPH basin_sd_chamorph_day.txt"
          if (pco%csvout == "y") then 
            open (2124,file="basin_sd_chamorph_day.csv", recl = 1500)
            write (2124,*) bsn%name, prog
            write (2124,'(*(G0.3,:","))') sdch_hdr
            write (2124,'(*(G0.3,:","))') sdch_hdr_units        
            write (9000,*) "BASIN_SWAT_DEG_CHAN_MORPH basin_sd_chamorph_day.csv"
          end if
        endif
        
       if (pco%sd_chan_bsn%m == "y") then
        open (2121,file="basin_sd_chamorph_mon.txt",recl = 1500)
        write (2121,*) bsn%name, prog
        write (2121,*) sdch_hdr
        write (2121,*) sdch_hdr_units
        write (9000,*) "BASIN_SWAT_DEG_CHAN_MORPH basin_sd_chamorph_mon.txt"
         if (pco%csvout == "y") then 
           open (2125,file="basin_sd_chamorph_mon.csv",recl = 1500)
           write (2125,*) bsn%name, prog
           write (2125,'(*(G0.3,:","))') sdch_hdr 
           write (2125,'(*(G0.3,:","))') sdch_hdr_units        
           write (9000,*) "BASIN_SWAT_DEG_CHAN_MORPH basin_sd_chamorph_mon.csv"
         end if
        end if
       
        if (pco%sd_chan_bsn%y == "y") then
          open (2122,file="basin_sd_chamorph_yr.txt", recl = 1500)
          write (2122,*) bsn%name, prog
          write (2122,*) sdch_hdr 
          write (2122,*) sdch_hdr_units
          write (9000,*) "BASIN_SWAT_DEG_CHAN_MORPH basin_sd_chamorph_yr.txt"
          if (pco%csvout == "y") then 
            open (2126,file="basin_sd_chamorph_yr.csv", recl = 1500)
            write (2126,*) bsn%name, prog
            write (2126,'(*(G0.3,:","))') sdch_hdr
            write (2126,'(*(G0.3,:","))') sdch_hdr_units        
            write (9000,*) "BASIN_SWAT_DEG_CHAN_MORPH basin_sd_chamorph_yr.csv"
          end if
        endif
        
        if (pco%sd_chan_bsn%a == "y") then
          open (2123,file="basin_sd_chamorph_aa.txt",recl = 1500)
          write (2123,*) bsn%name, prog
          write (2123,*) sdch_hdr 
          write (2123,*) sdch_hdr_units
          write (9000,*) "BASIN_SWAT_DEG_CHAN_MORPH basin_sd_chamorph_aa.txt"
          if (pco%csvout == "y") then 
            open (2127,file="basin_sd_chamorph_aa.csv",recl = 1500)
            write (2127,*) bsn%name, prog
            write (2127,'(*(G0.3,:","))') sdch_hdr 
            write (2127,'(*(G0.3,:","))') sdch_hdr_units 
            write (9000,*) "BASIN_SWAT_DEG_CHAN_MORPH basin_sd_chamorph_aa.csv"
          end if
        end if
!! BASIN SWAT DEG CHANNEL MORPH OUTPUT
        
!! BASIN SWAT DEG CHANBUD OUTPUT
        if (pco%sd_chan_bsn%d == "y") then
          open (2128,file="basin_sd_chanbud_day.txt", recl = 1500)
          write (2128,*) bsn%name, prog
          write (2128,*) sdch_bud_hdr
          write (2128,*) sdch_bud_hdr_units
          write (9000,*) "BASIN_SWAT_DEG_CHAN_BUD   basin_sd_chanbud_day.txt"
          if (pco%csvout == "y") then 
            open (2132,file="basin_sd_chanbud_day.csv", recl = 1500)
            write (2132,*) bsn%name, prog
            write (2132,'(*(G0.3,:","))') sdch_bud_hdr
            write (2132,'(*(G0.3,:","))') sdch_bud_hdr_units        
            write (9000,*) "BASIN_SWAT_DEG_CHAN_BUD   basin_sd_chanbud_day.csv"
          end if
        endif
        
       if (pco%sd_chan_bsn%m == "y") then
        open (2129,file="basin_sd_chanbud_mon.txt",recl = 1500)
        write (2129,*) bsn%name, prog
        write (2129,*) sdch_bud_hdr
        write (2129,*) sdch_bud_hdr_units
        write (9000,*) "BASIN_SWAT_DEG_CHAN_BUD   basin_sd_chanbud_mon.txt"
         if (pco%csvout == "y") then 
           open (2133,file="basin_sd_chanbud_mon.csv",recl = 1500)
           write (2133,*) bsn%name, prog
           write (2133,'(*(G0.3,:","))') sdch_bud_hdr 
           write (2133,'(*(G0.3,:","))') sdch_bud_hdr_units        
           write (9000,*) "BASIN_SWAT_DEG_CHAN_BUD   basin_sd_chanbud_mon.csv"
         end if
        end if
       
        if (pco%sd_chan_bsn%y == "y") then
          open (2130,file="basin_sd_chanbud_yr.txt", recl = 1500)
          write (2130,*) bsn%name, prog
          write (2130,*) sdch_bud_hdr 
          write (2130,*) sdch_bud_hdr_units
          write (9000,*) "BASIN_SWAT_DEG_CHAN_BUD   basin_sd_chanbud_yr.txt"
          if (pco%csvout == "y") then 
            open (2134,file="basin_sd_chanbud_yr.csv", recl = 1500)
            write (2134,*) bsn%name, prog
            write (2134,'(*(G0.3,:","))') sdch_bud_hdr
            write (2134,'(*(G0.3,:","))') sdch_bud_hdr_units        
            write (9000,*) "BASIN_SWAT_DEG_CHAN_BUD   basin_sd_chanbud_yr.csv"
          end if
        endif
        
        if (pco%sd_chan_bsn%a == "y") then
          open (2131,file="basin_sd_chanbud_aa.txt",recl = 1500)
          write (2131,*) bsn%name, prog
          write (2131,*) sdch_bud_hdr 
          write (2131,*) sdch_bud_hdr_units
          write (9000,*) "BASIN_SWAT_DEG_CHAN_BUD   basin_sd_chanbud_aa.txt"
          if (pco%csvout == "y") then 
            open (2135,file="basin_sd_chanbud_aa.csv",recl = 1500)
            write (2135,*) bsn%name, prog
            write (2135,'(*(G0.3,:","))') sdch_bud_hdr 
            write (2135,'(*(G0.3,:","))') sdch_bud_hdr_units 
            write (9000,*) "BASIN_SWAT_DEG_CHAN_BUD   basin_sd_chanbud_aa.csv"
          end if
        end if
!! BASIN SWAT DEG CHANBUD OUTPUT


!! BASIN RECALL OUTPUT (PTS - Point Source)
        if (pco%recall_bsn%d == "y") then
          open (4500,file="basin_psc_day.txt", recl = 1500)
          write (4500,*) bsn%name, prog
          write (4500,*) rec_hdr_time, hyd_hdr             
          write (4500,*) hyd_hdr_units
          write (9000,*) "BASIN_RECALL              basin_psc_day.txt"
          if (pco%csvout == "y") then 
            open (4504,file="basin_psc_day.csv", recl = 1500)
            write (4504,*) bsn%name, prog
            write (4504,'(*(G0.3,:","))') rec_hdr_time, hyd_hdr 
            write (4504,'(*(G0.3,:","))') hyd_hdr_units
            write (9000,*) "BASIN_RECALL              basin_psc_day.csv"
          end if
        endif
        
        if (pco%recall_bsn%m == "y") then
        open (4501,file="basin_psc_mon.txt",recl = 1500)
        write (4501,*) bsn%name, prog
        write (4501,*) rec_hdr_time, hyd_hdr
        write (4501,*) hyd_hdr_units
        write (9000,*) "BASIN_RECALL              basin_psc_mon.txt"
         if (pco%csvout == "y") then 
            open (4505,file="basin_psc_mon.csv",recl = 1500)
            write (4505,*) bsn%name, prog
            write (4505,'(*(G0.3,:","))') rec_hdr_time, hyd_hdr 
            write (4505,'(*(G0.3,:","))') hyd_hdr_units
            write (9000,*) "BASIN_RECALL              basin_psc_mon.csv"
         end if
       end if
       
        if (pco%recall_bsn%y == "y") then
          open (4502,file="basin_psc_yr.txt", recl = 1500)
          write (4502,*) bsn%name, prog
          write (4502,*) rec_hdr_time, hyd_hdr
          write (4502,*) hyd_hdr_units
          write (9000,*) "BASIN_RECALL              basin_psc_yr.txt"
          if (pco%csvout == "y") then 
            open (4506,file="basin_psc_yr.csv", recl = 1500)
            write (4506,*) bsn%name, prog
            write (4506,'(*(G0.3,:","))') rec_hdr_time, hyd_hdr 
            write (4506,'(*(G0.3,:","))') hyd_hdr_units
            write (9000,*) "BASIN_RECALL              basin_psc_yr.csv"
          end if
        endif
        
        if (pco%recall_bsn%a == "y") then 
        open (4503,file="basin_psc_aa.txt",recl = 1500) 
        write (4503,*) bsn%name, prog
        write (4503,*) rec_hdr_time, hyd_hdr
        write (4503,*) hyd_hdr_units
        write (9000,*) "BASIN_RECALL_AA           basin_psc_aa.txt"
         if (pco%csvout == "y") then 
            open (4507,file="basin_psc_aa.csv",recl = 1500)
            write (4507,*) bsn%name, prog
            write (4507,'(*(G0.3,:","))') rec_hdr_time, hyd_hdr 
            write (4507,'(*(G0.3,:","))') hyd_hdr_units
            write (9000,*) "BASIN_RECALL_AA           basin_psc_aa.csv"
         end if
        end if
        
!! BASIN RECALL OUTPUT

!! BASIN ROUTING UNIT OUTPUT
        if (pco%ru%d == "y") then
          open (2600,file="ru_day.txt", recl = 1500)
          write (2600,*) bsn%name, prog
          write (2600,*) hyd_hdr_time, hyd_hdr  
          write (2600,*) hyd_hdr_units
          write (9000,*) "ROUTING_UNITS             ru_day.txt"
          if (pco%csvout == "y") then 
            open (2604,file="ru_day.csv", recl = 1500)
            write (2604,*) bsn%name, prog
            write (2604,'(*(G0.3,:","))') hyd_hdr_time, hyd_hdr
            write (2604,'(*(G0.3,:","))') hyd_hdr_units
            write (9000,*) "ROUTING_UNITS             ru_day.csv"
          end if
        endif
        
        if (pco%ru%m == "y") then
        open (2601,file="ru_mon.txt",recl = 1500)
        write (2601,*) bsn%name, prog
        write (2601,*) hyd_hdr_time, hyd_hdr 
        write (2601,*) hyd_hdr_units
        write (9000,*) "ROUTING_UNITS             ru_mon.txt"
        if (pco%csvout == "y") then 
            open (2605,file="ru_mon.csv",recl = 1500)
            write (2605,*) bsn%name, prog
            write (2605,'(*(G0.3,:","))') hyd_hdr_time, hyd_hdr
            write (2605,'(*(G0.3,:","))') hyd_hdr_units
            write (9000,*) "ROUTING_UNITS             ru_mon.csv"
         end if
       end if
       
        if (pco%ru%y == "y") then
          open (2602,file="ru_yr.txt", recl = 1500)
          write (2602,*) bsn%name, prog
          write (2602,*) hyd_hdr_time, hyd_hdr 
          write (2602,*) hyd_hdr_units
          write (9000,*) "ROUTING_UNITS             ru_yr.txt"
          if (pco%csvout == "y") then 
            open (2606,file="ru_yr.csv", recl = 1500)
            write (2606,*) bsn%name, prog
            write (2606,'(*(G0.3,:","))') hyd_hdr_time, hyd_hdr
            write (2606,'(*(G0.3,:","))') hyd_hdr_units
            write (9000,*) "ROUTING_UNITS             ru_yr.csv"
          end if
        endif
        
        if (pco%ru%a == "y") then 
        open (2603,file="ru_aa.txt",recl = 1500) 
        write (2603,*) bsn%name, prog
        write (2603,*) hyd_hdr_time, hyd_hdr 
        write (2603,*) hyd_hdr_units
        write (9000,*) "ROUTING_UNITS             ru_aa.txt"
         if (pco%csvout == "y") then 
            open (2607,file="ru_aa.csv",recl = 1500)
            write (2607,*) bsn%name, prog
            write (2607,'(*(G0.3,:","))') hyd_hdr_time, hyd_hdr
            write (2607,'(*(G0.3,:","))') hyd_hdr_units
            write (9000,*) "ROUTING_UNITS             ru_aa.csv"
         end if
        end if
        
!! BASIN ROUTING UNIT OUTPUT

      return
      end subroutine header_write  