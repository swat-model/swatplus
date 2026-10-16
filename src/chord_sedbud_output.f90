      subroutine chord_sedbud_output

      use time_module
      use hydrograph_module
      use sd_channel_module
      use basin_module
      
      implicit none
      
      integer :: iord
      integer :: nch                  !none          |number of channels in the order

      !! loop through and print each use object
      do iord = 1, 12
        !! channels in the order on the print day - if a decision table changes a channel's order during a period,
        !! ebank_m, ebtm_m and fp_mm for that period are divided by the new count
        nch = count(sd_ch(1:sp_ob%chandeg)%order == iord)
        
        !! sum monthly variables
        ch_morph_ordm(iord) = ch_morph_ordm(iord) + ch_morph_ord(iord)
        
        !! daily print
        if (pco%sd_chan%d == "y") then
            write (3161,*) time%day, time%mo, time%day_mo, time%yrc, iord, chsedbud_ave(ch_morph_ord(iord), nch)

          if (pco%csvout == "y") then
          write (3165,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, iord, chsedbud_ave(ch_morph_ord(iord), nch)
          end if
        end if
       
        !! zero daily
        ch_morph_ord(iord) = ch_morphz

        !! monthly print
        if (time%end_mo == 1) then
          !! add into the yearly total
          ch_morph_ordy(iord) = ch_morph_ordy(iord) + ch_morph_ordm(iord)   

          if (pco%sd_chan%m == "y") then
          write (3162,*) time%day, time%mo, time%day_mo, time%yrc, iord, chsedbud_ave(ch_morph_ordm(iord), nch)
 
          if (pco%csvout == "y") then
          write (3166,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, iord, chsedbud_ave(ch_morph_ordm(iord), nch)
          end if
          end if

          !! zero monthly
          ch_morph_ordm(iord) = ch_morphz

        end if

      !! yearly print
      if (time%end_yr == 1) then
        !! add into the yearly total
        ch_morph_ordy(iord) =  ch_morph_ordy(iord) + ch_morph_ordm(iord)
          
        if (pco%sd_chan%y == "y") then
          write (3163,*) time%day, time%mo, time%day_mo, time%yrc, iord, chsedbud_ave(ch_morph_ordy(iord), nch)
  
              if (pco%csvout == "y") then
          write (3167,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, iord, chsedbud_ave(ch_morph_ordy(iord), nch)
          end if
        end if

        !! accumulate the year's total into the average-annual accumulator
        !! (fix: ch_morph_orda was divided by yrs_prt at end of sim but never summed -> AA printed 0)
        ch_morph_orda(iord) = ch_morph_orda(iord) + ch_morph_ordy(iord)

        !! zero yearly
        ch_morph_ordy(iord) = ch_morphz

      end if

      !! average annual print
      if (time%end_sim == 1) then
        !! convert the accumulated total to an average annual value
        !! / divides the summed amounts only - wid, dep and fp_km2 are averaged by chsedbud_ave
        ch_morph_orda(iord) = ch_morph_orda(iord) / time%yrs_prt

        if (pco%sd_chan%a == "y") then
        write (3164,*) time%day, time%mo, time%day_mo, time%yrc, iord, chsedbud_ave(ch_morph_orda(iord), nch)

        if (pco%csvout == "y") then
        write (3168,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, iord, chsedbud_ave(ch_morph_orda(iord), nch)
        end if
       end if
      end if

      end do    ! do iord = 1, 12
      
      return
      
      end subroutine chord_sedbud_output

