      subroutine header_sd_channel

      use sd_channel_module
      use basin_module
      use hydrograph_module
      use output_path_module
      
      implicit none 

!!!  SWAT-DEG CHANNEL - SUBDAILY OUTPUT
      if (sp_ob%chandeg > 0) then
        if (pco%sd_chan%d == "y") then
          if (time%step > 1) then
!!!!!!!! SD_CHANNEL
            call open_output_file(2508, "channel_sd_subday.txt", 1500)
            write (2508,*) bsn%name, prog
            write (2508,*) sdch_hdr_subday !! swat deg channel 
            write (2508,*) sdch_hdr_units_sub
          write (9000,*) "SWAT-DEG_CHANNEL         channel_sd_subday.txt"
          if (pco%csvout == "y") then
            call open_output_file(4814, "channel_sd_subday.csv", 1500)
            write (4814,*) bsn%name, prog
            write (4814,'(*(G0.3,:,","))') sdch_hdr 
            write (4814,'(*(G0.3,:,","))') sdch_hdr_units_sub
            write (9000,*) "SWAT-DEG_CHANNEL          channel_sd_subday.csv"
          end if
           end if         

          call open_output_file(2500, "channel_sd_day.txt", 1500)
          write (2500,*) bsn%name, prog
          write (2500,*) ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr, wtmp_hdr
          write (2500,*) ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units1, hyd_hdr_units1, wtmp_units 
          write (9000,*) "SWAT-DEG_CHANNEL          channel_sd_day.txt"
          
          !call open_output_file(2509, "channel_sd_day_new.txt", 1500)
          !write (2509,*) bsn%name, prog
          !write (2509,*) ch_wbod_inouthdr,  hyd_inout_hdr
          !write (2509,*) ch_wbod_inouthdr_units,  hydinout_hdr_units1
          !write (9000,*) "SWAT-DEG_CHANNEL          channel_sd_day_new.txt"
          if (pco%csvout == "y") then
            call open_output_file(2504, "channel_sd_day.csv", 1500)
            write (2504,*) bsn%name, prog
            write (2504,'(*(G0.3,:,","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr, wtmp_hdr
            write (2504,'(*(G0.3,:,","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units1, hyd_hdr_units1, wtmp_units 
            write (9000,*) "SWAT-DEG_CHANNEL          channel_sd_day.csv"
            
            !call open_output_file(2510, "channel_sd_day_new.csv", 1500)
            !write (2510,*) bsn%name, prog
            !write (2510,'(*(G0.3,:,","))') ch_wbod_inouthdr, hyd_inout_hdr
            !write (2510,'(*(G0.3,:,","))') ch_wbod_inouthdr_units, hydinout_hdr_units1
            !write (9000,*) "SWAT-DEG_CHANNEL          channel_sd_day_new.csv"                  
          end if
        endif
      endif
      
        if (sp_ob%chandeg > 0) then
          if (pco%sd_chan%m == "y") then  
          call open_output_file(2501, "channel_sd_mon.txt", 1500)
          write (2501,*) bsn%name, prog
          write (2501,*) ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr, wtmp_hdr
          write (2501,*) ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units1, hyd_hdr_units1, wtmp_units 
          write (9000,*) "SWAT-DEG_CHANNEL          channel_sd_mon.txt"
          
          !call open_output_file(3511, "channel_sd_mon_new.txt", 1500)
          !write (3511,*) bsn%name, prog
          !write (3511,*) ch_wbod_inouthdr,  hyd_inout_hdr
          !write (3511,*) ch_wbod_inouthdr_units,  hydinout_hdr_units1
          !write (9000,*) "SWAT-DEG_CHANNEL          channel_sd_mon_new.txt"
          
          if (pco%csvout == "y") then
            call open_output_file(2505, "channel_sd_mon.csv", 1500)
            write (2505,*) bsn%name, prog
            write (2505,'(*(G0.3,:,","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr, wtmp_hdr
            write (2505,'(*(G0.3,:,","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units1, hyd_hdr_units1, wtmp_units 
            write (9000,*) "SWAT-DEG_CHANNEL          channel_sd_mon.csv"
            
           !call open_output_file(3512, "channel_sd_mon_new.csv", 1500)
            !write (3512,*) bsn%name, prog
            !write (3512,'(*(G0.3,:,","))') ch_wbod_inouthdr, hyd_inout_hdr
            !write (3512,'(*(G0.3,:,","))') ch_wbod_inouthdr_units, hydinout_hdr_units1
            !write (9000,*) "SWAT-DEG_CHANNEL          channel_sd_mon_new.csv"    
          end if
          end if
         end if 
        
      if (sp_ob%chandeg > 0) then
        if (pco%sd_chan%y == "y") then
          call open_output_file(2502, "channel_sd_yr.txt", 1500)
          write (2502,*) bsn%name, prog
          write (2502,*) ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr, wtmp_hdr
          write (2502,*) ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units1, hyd_hdr_units1, wtmp_units 
          write (9000,*) "SWAT-DEG_CHANNEL          channel_sd_yr.txt"
          
          !call open_output_file(3513, "channel_sd_yr_new.txt", 1500)
          !write (3513,*) bsn%name, prog
          !write (3513,*) ch_wbod_inouthdr,  hyd_inout_hdr
          !write (3513,*) ch_wbod_inouthdr_units,  hydinout_hdr_units1
          !write (9000,*) "SWAT-DEG_CHANNEL          channel_sd_yr_new.txt"
          
          if (pco%csvout == "y") then
            call open_output_file(2506, "channel_sd_yr.csv", 1500)
            write (2506,*) bsn%name, prog
            write (2506,'(*(G0.3,:,","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr, wtmp_hdr
            write (2506,'(*(G0.3,:,","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units1, hyd_hdr_units1, wtmp_units 
            write (9000,*) "SWAT-DEG_CHANNEL          channel_sd_yr.csv"
            
          !call open_output_file(3514, "channel_sd_yr_new.csv", 1500)
          !  write (3514,*) bsn%name, prog
          !  write (3514,'(*(G0.3,:,","))') ch_wbod_inouthdr, hyd_inout_hdr
          !  write (3514,'(*(G0.3,:,","))') ch_wbod_inouthdr_units, hydinout_hdr_units1
          !  write (9000,*) "SWAT-DEG_CHANNEL          channel_sd_yr_new.csv"    
          end if
        endif
      endif
      
        if (sp_ob%chandeg > 0) then
          if (pco%sd_chan%a == "y") then
          call open_output_file(2503, "channel_sd_aa.txt", 1500)
          write (2503,*) bsn%name, prog
          write (2503,*) ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr, wtmp_hdr
          write (2503,*) ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units1, hyd_hdr_units1, wtmp_units
          write (9000,*) "SWAT-DEG_CHANNEL          channel_sd_aa.txt"
          
         !call open_output_file(3515, "channel_sd_aa_new.txt", 1500)
         ! write (3515,*) bsn%name, prog
         ! write (3515,*) ch_wbod_inouthdr,  hyd_inout_hdr
         ! write (3515,*) ch_wbod_inouthdr_units,  hydinout_hdr_units1
         ! write (9000,*) "SWAT-DEG_CHANNEL          channel_sd_aa_new.txt"
    
          if (pco%csvout == "y") then
            call open_output_file(2507, "channel_sd_aa.csv", 1500)
            write (2507,*) bsn%name, prog
            write (2507,'(*(G0.3,:,","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr, wtmp_hdr
            write (2507,'(*(G0.3,:,","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units1, hyd_hdr_units1, wtmp_units
            write (9000,*) "SWAT-DEG_CHANNEL          channel_sd_aa.csv"
            
          !call open_output_file(3516, "channel_sd_aa_new.csv", 1500)
          !  write (3516,*) bsn%name, prog
          !  write (3516,'(*(G0.3,:,","))') ch_wbod_inouthdr, hyd_inout_hdr
          !  write (3516,'(*(G0.3,:,","))') ch_wbod_inouthdr_units, hydinout_hdr_units1
          !  write (9000,*) "SWAT-DEG_CHANNEL          channel_sd_aa_new.csv"
            
          end if
          end if
         end if 
         
!!!!!!!! SD_CHANMORPH
      if (sp_ob%chandeg > 0) then
        if (pco%sd_chan%d == "y") then
          call open_output_file(4800, "channel_sdmorph_day.txt", 1500)
          write (4800,*) bsn%name, prog
          write (4800,*) sdch_hdr !! swat deg channel morph
          write (4800,*) sdch_hdr_units
          write (9000,*) "SWAT-DEG_CHANNEL_MORPH    channel_sdmorph_day.txt"
          if (pco%csvout == "y") then
            call open_output_file(4804, "channel_sdmorph_day.csv", 1500)
            write (4804,*) bsn%name, prog
            write (4804,'(*(G0.3,:,","))') sdch_hdr 
            write (4804,'(*(G0.3,:,","))') sdch_hdr_units
            write (9000,*) "SWAT-DEG_CHANNEL_MORPH    channel_sdmorph_day.csv"
          end if
        endif
      endif
      
        if (sp_ob%chandeg > 0) then
          if (pco%sd_chan%m == "y") then  
          call open_output_file(4801, "channel_sdmorph_mon.txt", 1500)
          write (4801,*) bsn%name, prog
          write (4801,*) sdch_hdr   !! swat deg channel morph
          write (4801,*) sdch_hdr_units
          write (9000,*) "SWAT-DEG_CHANNEL_MORPH    channel_sdmorph_mon.txt"
          if (pco%csvout == "y") then
            call open_output_file(4805, "channel_mon_sdmorph.csv", 1500)
            write (4805,*) bsn%name, prog
            write (4805,'(*(G0.3,:,","))') sdch_hdr   
            write (4805,'(*(G0.3,:,","))') sdch_hdr_units
            write (9000,*) "SWAT-DEG_CHANNEL_MORPH    channel_sdmorph_mon.csv"
          end if
          end if
         end if 
        
      if (sp_ob%chandeg > 0) then
        if (pco%sd_chan%y == "y") then
          call open_output_file(4802, "channel_sdmorph_yr.txt", 1500)
          write (4802,*) bsn%name, prog
          write (4802,*) sdch_hdr !! swat deg channel morph
          write (4802,*) sdch_hdr_units
          write (9000,*) "SWAT-DEG_CHANNEL_MORPH    channel_sdmorph_yr.txt"
          if (pco%csvout == "y") then
            call open_output_file(4806, "channel_sdmorph_yr.csv", 1500)
            write (4806,*) bsn%name, prog
            write (4806,'(*(G0.3,:,","))') sdch_hdr !! swat deg channel morph csv
            write (4806,'(*(G0.3,:,","))') sdch_hdr_units
            write (9000,*) "SWAT-DEG_CHANNEL_MORPH    channel_sdmorph_yr.csv"
          end if
        endif
      endif
      
        if (sp_ob%chandeg > 0) then
          if (pco%sd_chan%a == "y") then
          call open_output_file(4803, "channel_sdmorph_aa.txt", 1500)
          write (4803,*) bsn%name, prog
          write (4803,*) sdch_hdr   !! swat deg channel morph
          write (4803,*) sdch_hdr_units
          write (9000,*) "SWAT-DEG_CHANNEL_MORPH    channel_sdmorph_aa.txt"
          if (pco%csvout == "y") then
            call open_output_file(4807, "channel_sdmorph_aa.csv", 1500)
            write (4807,*) bsn%name, prog
            write (4807,'(*(G0.3,:,","))') sdch_hdr   
            write (4807,'(*(G0.3,:,","))') sdch_hdr_units
            write (9000,*) "SWAT-DEG_CHANNEL_MORPH    channel_sdmorph_aa.csv"
          end if
          end if
         end if 
!!!!!!!! SD_CHANMORPH
         
!! SWAT DEG CHANBUD OUTPUT
        if (pco%sd_chan%d == "y") then
          call open_output_file(4808, "sd_chanbud_day.txt", 1500)
          write (4808,*) bsn%name, prog
          write (4808,*) sdch_bud_hdr
          write (4808,*) sdch_bud_hdr_units
          write (9000,*) "SWAT_DEG_CHAN_BUD         sd_chanbud_day.txt"
          if (pco%csvout == "y") then 
            call open_output_file(4812, "sd_chanbud_day.csv", 1500)
            write (4812,*) bsn%name, prog
            write (4812,'(*(G0.3,:","))') sdch_bud_hdr
            write (4812,'(*(G0.3,:","))') sdch_bud_hdr_units        
            write (9000,*) "SWAT_DEG_CHAN_BUD         sd_chanbud_day.csv"
          end if
        endif
        
       if (pco%sd_chan%m == "y") then
        call open_output_file(4809, "sd_chanbud_mon.txt", 1500)
        write (4809,*) bsn%name, prog
        write (4809,*) sdch_bud_hdr
        write (4809,*) sdch_bud_hdr_units
        write (9000,*) "SWAT_DEG_CHAN_BUD         sd_chanbud_mon.txt"
         if (pco%csvout == "y") then 
           call open_output_file(4813, "sd_chanbud_mon.csv", 1500)
           write (4813,*) bsn%name, prog
           write (4813,'(*(G0.3,:","))') sdch_bud_hdr 
           write (4813,'(*(G0.3,:","))') sdch_bud_hdr_units        
           write (9000,*) "SWAT_DEG_CHAN_BUD         sd_chanbud_mon.csv"
         end if
        end if
       
        if (pco%sd_chan%y == "y") then
          call open_output_file(4810, "sd_chanbud_yr.txt", 1500)
          write (4810,*) bsn%name, prog
          write (4810,*) sdch_bud_hdr 
          write (4810,*) sdch_bud_hdr_units
          write (9000,*) "SWAT_DEG_CHAN_BUD         sd_chanbud_yr.txt"
          if (pco%csvout == "y") then 
            call open_output_file(4814, "sd_chanbud_yr.csv", 1500)
            write (4814,*) bsn%name, prog
            write (4814,'(*(G0.3,:","))') sdch_bud_hdr
            write (4814,'(*(G0.3,:","))') sdch_bud_hdr_units        
            write (9000,*) "SWAT_DEG_CHAN_BUD         sd_chanbud_yr.csv"
          end if
        endif
        
        if (pco%sd_chan%a == "y") then
          call open_output_file(4811, "sd_chanbud_aa.txt", 1500)
          write (4811,*) bsn%name, prog
          write (4811,*) sdch_bud_hdr 
          write (4811,*) sdch_bud_hdr_units
          write (9000,*) "SWAT_DEG_CHAN_BUD         sd_chanbud_aa.txt"
          if (pco%csvout == "y") then 
            call open_output_file(4815, "sd_chanbud_aa.csv", 1500)
            write (4815,*) bsn%name, prog
            write (4815,'(*(G0.3,:","))') sdch_bud_hdr 
            write (4815,'(*(G0.3,:","))') sdch_bud_hdr_units 
            write (9000,*) "SWAT_DEG_CHAN_BUD         sd_chanbud_aa.csv"
          end if
        end if
!! SWAT DEG CHANBUD OUTPUT
       
      return
      end subroutine header_sd_channel