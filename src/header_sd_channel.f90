      subroutine header_sd_channel

      use sd_channel_module
      use basin_module
      use hydrograph_module
      
      implicit none 

!!!  SWAT-DEG CHANNEL - SUBDAILY OUTPUT
      if (sp_ob%chandeg > 0) then
        if (pco%sd_chan%d == "y") then
          if (time%step > 1) then
!!!!!!!! SD_CHANNEL
            open (2508,file="channel_sd_subday.txt",recl = 1500)
            write (2508,*) bsn%name, prog
            write (2508,*) sdch_hdr_subday !! swat deg channel 
            write (2508,*) sdch_hdr_units_sub
          write (9000,*) "SWAT-DEG_CHANNEL         channel_sd_subday.txt"
          if (pco%csvout == "y") then
            open (4814,file="channel_sd_subday.csv",recl = 1500)
            write (4814,*) bsn%name, prog
            write (4814,'(*(G0.3,:,","))') sdch_hdr 
            write (4814,'(*(G0.3,:,","))') sdch_hdr_units_sub
            write (9000,*) "SWAT-DEG_CHANNEL          channel_sd_subday.csv"
          end if
           end if		  
         end if
          
          open (2500,file="channel_sd_day.txt",recl = 1500)
          write (2500,*) bsn%name, prog
          write (2500,*) ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr, wtmp_hdr
          write (2500,*) ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units1, hyd_hdr_units1, wtmp_units 
          write (9000,*) "SWAT-DEG_CHANNEL          channel_sd_day.txt"
          
          !open (2509,file="channel_sd_day_new.txt",recl = 1500)
          !write (2509,*) bsn%name, prog
          !write (2509,*) ch_wbod_inouthdr,  hyd_inout_hdr
          !write (2509,*) ch_wbod_inouthdr_units,  hydinout_hdr_units1
          !write (9000,*) "SWAT-DEG_CHANNEL          channel_sd_day_new.txt"
          if (pco%csvout == "y") then
            open (2504,file="channel_sd_day.csv",recl = 1500)
            write (2504,*) bsn%name, prog
            write (2504,'(*(G0.3,:,","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr, wtmp_hdr
            write (2504,'(*(G0.3,:,","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units1, hyd_hdr_units1, wtmp_units 
            write (9000,*) "SWAT-DEG_CHANNEL          channel_sd_day.csv"
            
            !open (2510,file="channel_sd_day_new.csv",recl = 1500)
            !write (2510,*) bsn%name, prog
            !write (2510,'(*(G0.3,:,","))') ch_wbod_inouthdr, hyd_inout_hdr
            !write (2510,'(*(G0.3,:,","))') ch_wbod_inouthdr_units, hydinout_hdr_units1
            !write (9000,*) "SWAT-DEG_CHANNEL          channel_sd_day_new.csv"                  
          end if
        endif
      !endif
      
        if (sp_ob%chandeg > 0) then
          if (pco%sd_chan%m == "y") then  
          open (2501,file="channel_sd_mon.txt",recl = 1500)
          write (2501,*) bsn%name, prog
          write (2501,*) ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr, wtmp_hdr
          write (2501,*) ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units1, hyd_hdr_units1, wtmp_units 
          write (9000,*) "SWAT-DEG_CHANNEL          channel_sd_mon.txt"
          
          !open (3511,file="channel_sd_mon_new.txt",recl = 1500)
          !write (3511,*) bsn%name, prog
          !write (3511,*) ch_wbod_inouthdr,  hyd_inout_hdr
          !write (3511,*) ch_wbod_inouthdr_units,  hydinout_hdr_units1
          !write (9000,*) "SWAT-DEG_CHANNEL          channel_sd_mon_new.txt"
          
          if (pco%csvout == "y") then
            open (2505,file="channel_sd_mon.csv",recl = 1500)
            write (2505,*) bsn%name, prog
            write (2505,'(*(G0.3,:,","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr, wtmp_hdr
            write (2505,'(*(G0.3,:,","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units1, hyd_hdr_units1, wtmp_units 
            write (9000,*) "SWAT-DEG_CHANNEL          channel_sd_mon.csv"
            
           !open (3512,file="channel_sd_mon_new.csv",recl = 1500)
            !write (3512,*) bsn%name, prog
            !write (3512,'(*(G0.3,:,","))') ch_wbod_inouthdr, hyd_inout_hdr
            !write (3512,'(*(G0.3,:,","))') ch_wbod_inouthdr_units, hydinout_hdr_units1
            !write (9000,*) "SWAT-DEG_CHANNEL          channel_sd_mon_new.csv"    
          end if
          end if
         end if 
        
      if (sp_ob%chandeg > 0) then
        if (pco%sd_chan%y == "y") then
          open (2502,file="channel_sd_yr.txt",recl = 1500)
          write (2502,*) bsn%name, prog
          write (2502,*) ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr, wtmp_hdr
          write (2502,*) ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units1, hyd_hdr_units1, wtmp_units 
          write (9000,*) "SWAT-DEG_CHANNEL          channel_sd_yr.txt"
          
          !open (3513,file="channel_sd_yr_new.txt",recl = 1500)
          !write (3513,*) bsn%name, prog
          !write (3513,*) ch_wbod_inouthdr,  hyd_inout_hdr
          !write (3513,*) ch_wbod_inouthdr_units,  hydinout_hdr_units1
          !write (9000,*) "SWAT-DEG_CHANNEL          channel_sd_yr_new.txt"
          
          if (pco%csvout == "y") then
            open (2506,file="channel_sd_yr.csv",recl = 1500)
            write (2506,*) bsn%name, prog
            write (2506,'(*(G0.3,:,","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr, wtmp_hdr
            write (2506,'(*(G0.3,:,","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units1, hyd_hdr_units1, wtmp_units 
            write (9000,*) "SWAT-DEG_CHANNEL          channel_sd_yr.csv"
            
          !open (3514,file="channel_sd_yr_new.csv",recl = 1500)
          !  write (3514,*) bsn%name, prog
          !  write (3514,'(*(G0.3,:,","))') ch_wbod_inouthdr, hyd_inout_hdr
          !  write (3514,'(*(G0.3,:,","))') ch_wbod_inouthdr_units, hydinout_hdr_units1
          !  write (9000,*) "SWAT-DEG_CHANNEL          channel_sd_yr_new.csv"    
          end if
        endif
      endif
      
        if (sp_ob%chandeg > 0) then
          if (pco%sd_chan%a == "y") then
          open (2503,file="channel_sd_aa.txt",recl = 1500)
          write (2503,*) bsn%name, prog
          write (2503,*) ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr, wtmp_hdr
          write (2503,*) ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units1, hyd_hdr_units1, wtmp_units
          write (9000,*) "SWAT-DEG_CHANNEL          channel_sd_aa.txt"
          
         !open (3515,file="channel_sd_aa_new.txt",recl = 1500)
         ! write (3515,*) bsn%name, prog
         ! write (3515,*) ch_wbod_inouthdr,  hyd_inout_hdr
         ! write (3515,*) ch_wbod_inouthdr_units,  hydinout_hdr_units1
         ! write (9000,*) "SWAT-DEG_CHANNEL          channel_sd_aa_new.txt"
    
          if (pco%csvout == "y") then
            open (2507,file="channel_sd_aa.csv",recl = 1500)
            write (2507,*) bsn%name, prog
            write (2507,'(*(G0.3,:,","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr, wtmp_hdr
            write (2507,'(*(G0.3,:,","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units1, hyd_hdr_units1, wtmp_units
            write (9000,*) "SWAT-DEG_CHANNEL          channel_sd_aa.csv"
            
          !open (3516,file="channel_sd_aa_new.csv",recl = 1500)
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
          open (4800,file="channel_sdmorph_day.txt",recl = 1500)
          write (4800,*) bsn%name, prog
          write (4800,*) sdch_hdr !! swat deg channel morph
          write (4800,*) sdch_hdr_units
          write (9000,*) "SWAT-DEG_CHANNEL_MORPH    channel_sdmorph_day.txt"
          if (pco%csvout == "y") then
            open (4804,file="channel_sdmorph_day.csv",recl = 1500)
            write (4804,*) bsn%name, prog
            write (4804,'(*(G0.3,:,","))') sdch_hdr 
            write (4804,'(*(G0.3,:,","))') sdch_hdr_units
            write (9000,*) "SWAT-DEG_CHANNEL_MORPH    channel_sdmorph_day.csv"
          end if
        endif
      endif
      
        if (sp_ob%chandeg > 0) then
          if (pco%sd_chan%m == "y") then  
          open (4801,file="channel_sdmorph_mon.txt",recl = 1500)
          write (4801,*) bsn%name, prog
          write (4801,*) sdch_hdr   !! swat deg channel morph
          write (4801,*) sdch_hdr_units
          write (9000,*) "SWAT-DEG_CHANNEL_MORPH    channel_sdmorph_mon.txt"
          if (pco%csvout == "y") then
            open (4805,file="channel_mon_sdmorph.csv",recl = 1500)
            write (4805,*) bsn%name, prog
            write (4805,'(*(G0.3,:,","))') sdch_hdr   
            write (4805,'(*(G0.3,:,","))') sdch_hdr_units
            write (9000,*) "SWAT-DEG_CHANNEL_MORPH    channel_sdmorph_mon.csv"
          end if
          end if
         end if 
        
      if (sp_ob%chandeg > 0) then
        if (pco%sd_chan%y == "y") then
          open (4802,file="channel_sdmorph_yr.txt",recl = 1500)
          write (4802,*) bsn%name, prog
          write (4802,*) sdch_hdr !! swat deg channel morph
          write (4802,*) sdch_hdr_units
          write (9000,*) "SWAT-DEG_CHANNEL_MORPH    channel_sdmorph_yr.txt"
          if (pco%csvout == "y") then
            open (4806,file="channel_sdmorph_yr.csv",recl = 1500)
            write (4806,*) bsn%name, prog
            write (4806,'(*(G0.3,:,","))') sdch_hdr !! swat deg channel morph csv
            write (4806,'(*(G0.3,:,","))') sdch_hdr_units
            write (9000,*) "SWAT-DEG_CHANNEL_MORPH    channel_sdmorph_yr.csv"
          end if
        endif
      endif
      
        if (sp_ob%chandeg > 0) then
          if (pco%sd_chan%a == "y") then
          open (4803,file="channel_sdmorph_aa.txt",recl = 1500)
          write (4803,*) bsn%name, prog
          write (4803,*) sdch_hdr   !! swat deg channel morph
          write (4803,*) sdch_hdr_units
          write (9000,*) "SWAT-DEG_CHANNEL_MORPH    channel_sdmorph_aa.txt"
          if (pco%csvout == "y") then
            open (4807,file="channel_sdmorph_aa.csv",recl = 1500)
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
          open (4808,file="sd_chanbud_day.txt", recl = 1500)
          write (4808,*) bsn%name, prog
          write (4808,*) sdch_bud_hdr
          write (4808,*) sdch_bud_hdr_units
          write (9000,*) "SWAT_DEG_CHAN_BUD         sd_chanbud_day.txt"
          if (pco%csvout == "y") then 
            open (4812,file="sd_chanbud_day.csv", recl = 1500)
            write (4812,*) bsn%name, prog
            write (4812,'(*(G0.3,:","))') sdch_bud_hdr
            write (4812,'(*(G0.3,:","))') sdch_bud_hdr_units        
            write (9000,*) "SWAT_DEG_CHAN_BUD         sd_chanbud_day.csv"
          end if
        endif
        
       if (pco%sd_chan%m == "y") then
        open (4809,file="sd_chanbud_mon.txt",recl = 1500)
        write (4809,*) bsn%name, prog
        write (4809,*) sdch_bud_hdr
        write (4809,*) sdch_bud_hdr_units
        write (9000,*) "SWAT_DEG_CHAN_BUD         sd_chanbud_mon.txt"
         if (pco%csvout == "y") then 
           open (4813,file="sd_chanbud_mon.csv",recl = 1500)
           write (4813,*) bsn%name, prog
           write (4813,'(*(G0.3,:","))') sdch_bud_hdr 
           write (4813,'(*(G0.3,:","))') sdch_bud_hdr_units        
           write (9000,*) "SWAT_DEG_CHAN_BUD         sd_chanbud_mon.csv"
         end if
        end if
       
        if (pco%sd_chan%y == "y") then
          open (4810,file="sd_chanbud_yr.txt", recl = 1500)
          write (4810,*) bsn%name, prog
          write (4810,*) sdch_bud_hdr 
          write (4810,*) sdch_bud_hdr_units
          write (9000,*) "SWAT_DEG_CHAN_BUD         sd_chanbud_yr.txt"
          if (pco%csvout == "y") then 
            open (4814,file="sd_chanbud_yr.csv", recl = 1500)
            write (4814,*) bsn%name, prog
            write (4814,'(*(G0.3,:","))') sdch_bud_hdr
            write (4814,'(*(G0.3,:","))') sdch_bud_hdr_units        
            write (9000,*) "SWAT_DEG_CHAN_BUD         sd_chanbud_yr.csv"
          end if
        endif
        
        if (pco%sd_chan%a == "y") then
          open (4811,file="sd_chanbud_aa.txt",recl = 1500)
          write (4811,*) bsn%name, prog
          write (4811,*) sdch_bud_hdr 
          write (4811,*) sdch_bud_hdr_units
          write (9000,*) "SWAT_DEG_CHAN_BUD         sd_chanbud_aa.txt"
          if (pco%csvout == "y") then 
            open (4815,file="sd_chanbud_aa.csv",recl = 1500)
            write (4815,*) bsn%name, prog
            write (4815,'(*(G0.3,:","))') sdch_bud_hdr 
            write (4815,'(*(G0.3,:","))') sdch_bud_hdr_units 
            write (9000,*) "SWAT_DEG_CHAN_BUD         sd_chanbud_aa.csv"
          end if
        end if
!! SWAT DEG CHANBUD OUTPUT
       
      return
      end subroutine header_sd_channel