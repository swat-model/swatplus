      subroutine basin_sdchannel_output
      
      use time_module
      use basin_module
      use channel_module
      use hydrograph_module
      use water_body_module
      
      implicit none
             
      integer :: ichan      !none       |counter
      real :: const         !           |

      bch_stor_d = chaz
      bch_in_d = chaz
      bch_out_d = chaz
      bch_wat_d = wbodz

      !! sum all channel output
      do ichan = 1, sp_ob%chandeg
        bch_stor_d = bch_stor_d + ch_stor(ichan)
        bch_in_d = bch_in_d + ch_in_d(ichan)
        bch_out_d = bch_out_d + ch_out_d(ichan)
        bch_wat_d = bch_wat_d + ch_wat_d(ichan)
      end do

      bch_in_m = bch_in_m + bch_in_d
      bch_out_m = bch_out_m + bch_out_d
      bch_wat_m = bch_wat_m + bch_wat_d

       !! daily print
       if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
        if (pco%sd_chan_bsn%d == "y") then
          write (4900,100) time%day, time%mo, time%day_mo, time%yrc, "       1", "     1", bsn%name, bch_wat_d, &
          bch_stor_d, bch_in_d, bch_out_d
          if (pco%csvout == "y") then
            write (4904,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "     1", bsn%name, &
            bch_wat_d, bch_stor_d, bch_in_d, bch_out_d
          end if 
        end if 
      end if

      !! monthly print
      if (time%end_mo == 1) then
        bch_in_y = bch_in_y + bch_in_m
        bch_out_y = bch_out_y + bch_out_m
        bch_wat_y = bch_wat_y + bch_wat_m     
        const = float (ndays(time%mo + 1) - ndays(time%mo))
        bch_wat_m = bch_wat_m // const            !! // only divides area (daily average values)

        if (pco%sd_chan_bsn%m == "y") then
          write (4901,100) time%day, time%mo, time%day_mo, time%yrc, "       1", "     1", bsn%name, bch_wat_m, &
            bch_stor_d, bch_in_m, bch_out_m
          if (pco%csvout == "y") then
            write (4905,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "     1", bsn%name, &
              bch_wat_m, bch_stor_d, bch_in_m, bch_out_m
          end if
        end if
        bch_in_m = chaz
        bch_out_m = chaz
        bch_wat_m = wbodz
      end if

      !! yearly print
      if (time%end_yr == 1) then
        bch_in_a = bch_in_a + bch_in_y
        bch_out_a = bch_out_a + bch_out_y
        bch_wat_a = bch_wat_a + bch_wat_y       
        const = time%day_end_yr
        bch_wat_y = bch_wat_y // const      !! // only divides area (daily average values)

        if (pco%sd_chan_bsn%y == "y") then 
          write (4902,100) time%day, time%mo, time%day_mo, time%yrc, "       1", "     1", bsn%name, bch_wat_y, &
            bch_stor_d, bch_in_y, bch_out_y
          if (pco%csvout == "y") then
            write (4906,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "     1", bsn%name,&
              bch_wat_y, bch_stor_d, bch_in_y, bch_out_y
          end if
        end if
        bch_in_y = chaz
        bch_out_y = chaz
        bch_wat_y = wbodz
      end if

      !! average annual print
      if (time%end_sim == 1 .and. pco%sd_chan_bsn%a == "y") then
        bch_in_a = bch_in_a / time%yrs_prt          !! all inflow and outflow variables (summed) are divided by years
        bch_out_a = bch_out_a / time%yrs_prt
        bch_wat_a = bch_wat_a / time%yrs_prt        !! all summed variables divided by years

        write (4903,100) time%day, time%mo, time%day_mo, time%yrc, "       1", "     1", bsn%name, bch_wat_a, &
          bch_stor_d, bch_in_a, bch_out_a
        if (pco%csvout == "y") then
          write (4907,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "     1", bsn%name, &
            bch_wat_a, bch_stor_d, bch_in_a, bch_out_a
        end if
      end if

 !100   format (4i6,2x,2a,2x,a17,f14.4,59(1x,e14.4))
100   format (4i6,2x,2a,2x,a17,f16.4,59(1x,e14.4))
      return
      
      end subroutine basin_sdchannel_output