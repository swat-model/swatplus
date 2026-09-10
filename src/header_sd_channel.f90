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
            write (4814,'(*(G0.6,:,","))') sdch_hdr 
            write (4814,'(*(G0.6,:,","))') sdch_hdr_units_sub
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
            write (2504,'(*(G0.6,:,","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr, wtmp_hdr
            write (2504,'(*(G0.6,:,","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units1, hyd_hdr_units1, wtmp_units 
            write (9000,*) "SWAT-DEG_CHANNEL          channel_sd_day.csv"
            
            !call open_output_file(2510, "channel_sd_day_new.csv", 1500)
            !write (2510,*) bsn%name, prog
            !write (2510,'(*(G0.6,:,","))') ch_wbod_inouthdr, hyd_inout_hdr
            !write (2510,'(*(G0.6,:,","))') ch_wbod_inouthdr_units, hydinout_hdr_units1
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
            write (2505,'(*(G0.6,:,","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr, wtmp_hdr
            write (2505,'(*(G0.6,:,","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units1, hyd_hdr_units1, wtmp_units 
            write (9000,*) "SWAT-DEG_CHANNEL          channel_sd_mon.csv"
            
           !call open_output_file(3512, "channel_sd_mon_new.csv", 1500)
            !write (3512,*) bsn%name, prog
            !write (3512,'(*(G0.6,:,","))') ch_wbod_inouthdr, hyd_inout_hdr
            !write (3512,'(*(G0.6,:,","))') ch_wbod_inouthdr_units, hydinout_hdr_units1
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
            write (2506,'(*(G0.6,:,","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr, wtmp_hdr
            write (2506,'(*(G0.6,:,","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units1, hyd_hdr_units1, wtmp_units 
            write (9000,*) "SWAT-DEG_CHANNEL          channel_sd_yr.csv"
            
          !call open_output_file(3514, "channel_sd_yr_new.csv", 1500)
          !  write (3514,*) bsn%name, prog
          !  write (3514,'(*(G0.6,:,","))') ch_wbod_inouthdr, hyd_inout_hdr
          !  write (3514,'(*(G0.6,:,","))') ch_wbod_inouthdr_units, hydinout_hdr_units1
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
            write (2507,'(*(G0.6,:,","))') ch_wbod_hdr, hyd_stor_hdr, hyd_in_hdr, hyd_out_hdr, wtmp_hdr
            write (2507,'(*(G0.6,:,","))') ch_wbod_hdr_units, hyd_hdr_units3, hyd_hdr_units1, hyd_hdr_units1, wtmp_units
            write (9000,*) "SWAT-DEG_CHANNEL          channel_sd_aa.csv"
            
          !call open_output_file(3516, "channel_sd_aa_new.csv", 1500)
          !  write (3516,*) bsn%name, prog
          !  write (3516,'(*(G0.6,:,","))') ch_wbod_inouthdr, hyd_inout_hdr
          !  write (3516,'(*(G0.6,:,","))') ch_wbod_inouthdr_units, hydinout_hdr_units1
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
            write (4804,'(*(G0.6,:,","))') sdch_hdr 
            write (4804,'(*(G0.6,:,","))') sdch_hdr_units
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
            call open_output_file(4805, "channel_sdmorph_mon.csv", 1500)
            write (4805,*) bsn%name, prog
            write (4805,'(*(G0.6,:,","))') sdch_hdr   
            write (4805,'(*(G0.6,:,","))') sdch_hdr_units
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
            write (4806,'(*(G0.6,:,","))') sdch_hdr !! swat deg channel morph csv
            write (4806,'(*(G0.6,:,","))') sdch_hdr_units
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
            write (4807,'(*(G0.6,:,","))') sdch_hdr   
            write (4807,'(*(G0.6,:,","))') sdch_hdr_units
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
            write (4812,'(*(G0.6,:","))') sdch_bud_hdr
            write (4812,'(*(G0.6,:","))') sdch_bud_hdr_units        
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
           write (4813,'(*(G0.6,:","))') sdch_bud_hdr 
           write (4813,'(*(G0.6,:","))') sdch_bud_hdr_units        
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
            write (4814,'(*(G0.6,:","))') sdch_bud_hdr
            write (4814,'(*(G0.6,:","))') sdch_bud_hdr_units        
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
            write (4815,'(*(G0.6,:","))') sdch_bud_hdr 
            write (4815,'(*(G0.6,:","))') sdch_bud_hdr_units 
            write (9000,*) "SWAT_DEG_CHAN_BUD         sd_chanbud_aa.csv"
          end if
        end if
!! SWAT DEG CHANBUD OUTPUT
        
!! SWAT DEG CHANNEL BUDGET ORDER
!! all four tiers share ch_bud_hdr/ch_bud_hdr_units, which now carry the same
!! day/mon/day_mo/yr date prefix used on every other output in this codebase
   if (sp_ob%chandeg > 0) then
     if (pco%sed_bud%d == "y") then
       call open_output_file(3153, "chanbud_day.txt", 1500)
       write (3153,*) bsn%name, prog
       write (3153,*) ch_bud_hdr
       write (3153,*) ch_bud_hdr_units
       write (9000,*) "SWAT_DEG_CHANBUD        chanbud_day.txt"
       if (pco%csvout == "y") then
         call open_output_file(3157, "chanbud_day.csv", 1500)
         write (3157,*) bsn%name, prog
         write (3157,'(*(G0.6,:","))') ch_bud_hdr
         write (3157,'(*(G0.6,:","))') ch_bud_hdr_units
         write (9000,*) "SWAT_DEG_CHANBUD        chanbud_day.csv"
       end if
     end if
     if (pco%sed_bud%m == "y") then
       call open_output_file(3154, "chanbud_mon.txt", 1500)
       write (3154,*) bsn%name, prog
       write (3154,*) ch_bud_hdr
       write (3154,*) ch_bud_hdr_units
       write (9000,*) "SWAT_DEG_CHANBUD        chanbud_mon.txt"
       if (pco%csvout == "y") then
         call open_output_file(3158, "chanbud_mon.csv", 1500)
         write (3158,*) bsn%name, prog
         write (3158,'(*(G0.6,:","))') ch_bud_hdr
         write (3158,'(*(G0.6,:","))') ch_bud_hdr_units
         write (9000,*) "SWAT_DEG_CHANBUD        chanbud_mon.csv"
       end if
     end if
     if (pco%sed_bud%y == "y") then
       call open_output_file(3155, "chanbud_yr.txt", 1500)
       write (3155,*) bsn%name, prog
       write (3155,*) ch_bud_hdr
       write (3155,*) ch_bud_hdr_units
       write (9000,*) "SWAT_DEG_CHANBUD        chanbud_yr.txt"
       if (pco%csvout == "y") then
         call open_output_file(3159, "chanbud_yr.csv", 1500)
         write (3159,*) bsn%name, prog
         write (3159,'(*(G0.6,:","))') ch_bud_hdr
         write (3159,'(*(G0.6,:","))') ch_bud_hdr_units
         write (9000,*) "SWAT_DEG_CHANBUD        chanbud_yr.csv"
       end if
     end if
     if (pco%sed_bud%a == "y") then
       call open_output_file(3150, "chanbud.txt", 1500)
       write (3150,*) bsn%name, prog
       write (3150,*) ch_bud_hdr
       write (3150,*) ch_bud_hdr_units
       write (9000,*) "SWAT_DEG_CHANBUD        chanbud.txt"
       if (pco%csvout == "y") then
         call open_output_file(3160, "chanbud.csv", 1500)
         write (3160,*) bsn%name, prog
         write (3160,'(*(G0.6,:","))') ch_bud_hdr
         write (3160,'(*(G0.6,:","))') ch_bud_hdr_units
         write (9000,*) "SWAT_DEG_CHANBUD        chanbud.csv"
       end if
     end if
   end if
!! SWAT DEG CHANNEL BUDGET ORDER

!! SWAT DEG CHANNEL BUDGET MORPH
!! all four tiers share ch_bud_order_hdr/ch_bud_order_hdr_units, which now carry
!! the same day/mon/day_mo/yr date prefix used on every other output
   if (sp_ob%chandeg > 0) then
     if (pco%sed_bud%d == "y") then
       call open_output_file(3161, "chanbud_order_day.txt", 1500)
       write (3161,*) bsn%name, prog
       write (3161,*) ch_bud_order_hdr
       write (3161,*) ch_bud_order_hdr_units
       write (9000,*) "CHANBUD_ORDER         chanbud_order_day.txt"
       if (pco%csvout == "y") then
         call open_output_file(3165, "chanbud_order_day.csv", 1500)
         write (3165,*) bsn%name, prog
         write (3165,'(*(G0.6,:","))') ch_bud_order_hdr
         write (3165,'(*(G0.6,:","))') ch_bud_order_hdr_units
         write (9000,*) "CHANBUD_ORDER         chanbud_order_day.csv"
       end if
     end if
     if (pco%sed_bud%m == "y") then
       call open_output_file(3162, "chanbud_order_mon.txt", 1500)
       write (3162,*) bsn%name, prog
       write (3162,*) ch_bud_order_hdr
       write (3162,*) ch_bud_order_hdr_units
       write (9000,*) "CHANBUD_ORDER         chanbud_order_mon.txt"
       if (pco%csvout == "y") then
         call open_output_file(3166, "chanbud_order_mon.csv", 1500)
         write (3166,*) bsn%name, prog
         write (3166,'(*(G0.6,:","))') ch_bud_order_hdr
         write (3166,'(*(G0.6,:","))') ch_bud_order_hdr_units
         write (9000,*) "CHANBUD_ORDER         chanbud_order_mon.csv"
       end if
     end if
     if (pco%sed_bud%y == "y") then
       call open_output_file(3163, "chanbud_order_yr.txt", 1500)
       write (3163,*) bsn%name, prog
       write (3163,*) ch_bud_order_hdr
       write (3163,*) ch_bud_order_hdr_units
       write (9000,*) "CHANBUD_ORDER         chanbud_order_yr.txt"
       if (pco%csvout == "y") then
         call open_output_file(3167, "chanbud_order_yr.csv", 1500)
         write (3167,*) bsn%name, prog
         write (3167,'(*(G0.6,:","))') ch_bud_order_hdr
         write (3167,'(*(G0.6,:","))') ch_bud_order_hdr_units
         write (9000,*) "CHANBUD_ORDER         chanbud_order_yr.csv"
       end if
     end if
     if (pco%sed_bud%a == "y") then
       call open_output_file(3151, "chanbud_order.txt", 1500)
       write (3151,*) bsn%name, prog
       write (3151,*) ch_bud_order_hdr
       write (3151,*) ch_bud_order_hdr_units
       write (9000,*) "CHANBUD_ORDER         chanbud_order.txt"
       if (pco%csvout == "y") then
         call open_output_file(3168, "chanbud_order.csv", 1500)
         write (3168,*) bsn%name, prog
         write (3168,'(*(G0.6,:","))') ch_bud_order_hdr
         write (3168,'(*(G0.6,:","))') ch_bud_order_hdr_units
         write (9000,*) "CHANBUD_ORDER         chanbud_order.csv"
       end if
     end if
   end if
!! SWAT DEG CHANNEL BUDGET MORPH

!! SWAT DEG CHANNEL SEDIMENT BUDGET
!! all four tiers share ch_sed_bud_hdr/ch_sed_bud_hdr_units, which now carry the
!! same day/mon/day_mo/yr date prefix used on every other output. no known
!! limitation on any tier here - basin_sediment_budget has no geometry fields
   if (sp_ob%chandeg > 0) then
     if (pco%sed_bud%d == "y") then
       call open_output_file(3169, "bsn_sedbud_day.txt", 1500)
       write (3169,*) bsn%name, prog
       write (3169,*) ch_sed_bud_hdr
       write (3169,*) ch_sed_bud_hdr_units
       write (9000,*) "BASIN SEDBUD          bsn_sedbud_day.txt"
       if (pco%csvout == "y") then
         call open_output_file(3173, "bsn_sedbud_day.csv", 1500)
         write (3173,*) bsn%name, prog
         write (3173,'(*(G0.6,:","))') ch_sed_bud_hdr
         write (3173,'(*(G0.6,:","))') ch_sed_bud_hdr_units
         write (9000,*) "BASIN SEDBUD          bsn_sedbud_day.csv"
       end if
     end if
     if (pco%sed_bud%m == "y") then
       call open_output_file(3170, "bsn_sedbud_mon.txt", 1500)
       write (3170,*) bsn%name, prog
       write (3170,*) ch_sed_bud_hdr
       write (3170,*) ch_sed_bud_hdr_units
       write (9000,*) "BASIN SEDBUD          bsn_sedbud_mon.txt"
       if (pco%csvout == "y") then
         call open_output_file(3174, "bsn_sedbud_mon.csv", 1500)
         write (3174,*) bsn%name, prog
         write (3174,'(*(G0.6,:","))') ch_sed_bud_hdr
         write (3174,'(*(G0.6,:","))') ch_sed_bud_hdr_units
         write (9000,*) "BASIN SEDBUD          bsn_sedbud_mon.csv"
       end if
     end if
     if (pco%sed_bud%y == "y") then
       call open_output_file(3171, "bsn_sedbud_yr.txt", 1500)
       write (3171,*) bsn%name, prog
       write (3171,*) ch_sed_bud_hdr
       write (3171,*) ch_sed_bud_hdr_units
       write (9000,*) "BASIN SEDBUD          bsn_sedbud_yr.txt"
       if (pco%csvout == "y") then
         call open_output_file(3175, "bsn_sedbud_yr.csv", 1500)
         write (3175,*) bsn%name, prog
         write (3175,'(*(G0.6,:","))') ch_sed_bud_hdr
         write (3175,'(*(G0.6,:","))') ch_sed_bud_hdr_units
         write (9000,*) "BASIN SEDBUD          bsn_sedbud_yr.csv"
       end if
     end if
     if (pco%sed_bud%a == "y") then
       call open_output_file(3152, "bsn_sedbud.txt", 1500)
       write (3152,*) bsn%name, prog
       write (3152,*) ch_sed_bud_hdr
       write (3152,*) ch_sed_bud_hdr_units
       write (9000,*) "BASIN SEDBUD          bsn_sedbud.txt"
       if (pco%csvout == "y") then
         call open_output_file(3176, "bsn_sedbud.csv", 1500)
         write (3176,*) bsn%name, prog
         write (3176,'(*(G0.6,:","))') ch_sed_bud_hdr
         write (3176,'(*(G0.6,:","))') ch_sed_bud_hdr_units
         write (9000,*) "BASIN SEDBUD          bsn_sedbud.csv"
       end if
     end if
   end if
!! SWAT DEG CHANNEL SEDIMENT BUDGET
  
      return
      end subroutine header_sd_channel