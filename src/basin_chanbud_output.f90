      subroutine basin_chanbud_output
      
      use time_module
      use basin_module
      use sd_channel_module
      use hydrograph_module
      
      implicit none

      real :: const
                  
      bch_sed_bud_d = ch_sed_budz

      !! sum all channel output
      do ich = 1, sp_ob%chandeg
        bch_sed_bud_d = bch_sed_bud_d + ch_sed_bud(ich)
        ch_sed_bud(ich) = ch_sed_budz
      end do
  
      bch_sed_bud_m = bch_sed_bud_m + bch_sed_bud_d
      
!!!!! daily print
       if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
        if (pco%sd_chan_bsn%d == "y") then
          write (2128,100) time%day, time%mo, time%day_mo, time%yrc, "       1", "     1", bsn%name, bch_sed_bud_d
          if (pco%csvout == "y") then
            write (2132,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "     1", bsn%name, bch_sed_bud_d
          end if 
        end if 
      end if

!!!!! monthly print
      if (time%end_mo == 1) then
        bch_sed_bud_y = bch_sed_bud_y + bch_sed_bud_m
        !const = float (ndays(time%mo + 1) - ndays(time%mo))
        !bch_sed_bud_m = bch_sed_bud_m // const
          
        if (pco%sd_chan_bsn%m == "y") then
          write (2129,100) time%day, time%mo, time%day_mo, time%yrc, "       1", "     1", bsn%name, bch_sed_bud_m
          if (pco%csvout == "y") then
            write (2133,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "     1", bsn%name, bch_sed_bud_m
          end if
        end if
        bch_sed_bud_m = ch_sed_budz
      end if

!!!!! yearly print
      if (time%end_yr == 1) then
        bch_sed_bud_a = bch_sed_bud_a + bch_sed_bud_y
        !const = time%day_end_yr
        !bch_sed_bud_a = bch_sed_bud_a // const
        
        if (pco%sd_chan_bsn%y == "y") then 
          write (2130,100) time%day, time%mo, time%day_mo, time%yrc, "       1", "     1", bsn%name, bch_sed_bud_y
          if (pco%csvout == "y") then
            write (2134,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "     1", bsn%name, bch_sed_bud_y
          end if
        end if
        
        bch_sed_bud_y = ch_sed_budz
      end if

!!!!! average annual print
      if (time%end_sim == 1 .and. pco%sd_chan_bsn%a == "y") then
        bch_sed_bud_a = bch_sed_bud_a / time%yrs_prt
        !bch_sed_bud_a = bch_sed_bud_a // time%days_prt
        
        write (2131,100) time%day, time%mo, time%day_mo, time%yrc, "       1", "     1", bsn%name, bch_sed_bud_a
        if (pco%csvout == "y") then
          write (2135,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "     1", bsn%name, bch_sed_bud_a
        end if
      end if

100   format (4i6,2x,2a,2x,a17,60(1x,e14.4))

      return
      
      end subroutine basin_chanbud_output