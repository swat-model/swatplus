      subroutine basin_sedbud_output

      use time_module
      use basin_module
      
      implicit none

      

      !! daily print - today's value, before it is folded into the monthly total
      if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
        if (pco%sed_bud%d == "y") then
          write (3169,*) time%day, time%mo, time%day_mo, time%yrc, bsn_sedbud
          if (pco%csvout == "y") then
            write (3173,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, bsn_sedbud
          end if
        end if
      end if

      !! fold today into the monthly total. bsn_sedbudm/y/a are three separate
      !! variables on purpose - the original bug reused one variable for both the
      !! yearly total and the AA average, which zeroed the AA total right before
      !! it was divided, so the AA report always printed zero
      bsn_sedbudm = bsn_sedbudm + bsn_sedbud
      bsn_sedbud = bsn_sedbudz

      if (time%end_mo == 1) then
        !! monthly print
        if (pco%sed_bud%m == "y") then
          write (3170,*) time%day, time%mo, time%day_mo, time%yrc, bsn_sedbudm
          if (pco%csvout == "y") then
            write (3174,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, bsn_sedbudm
          end if
        end if
        bsn_sedbudy = bsn_sedbudy + bsn_sedbudm
        bsn_sedbudm = bsn_sedbudz
      end if

      if (time%end_yr == 1) then
        if (time%end_mo == 0) then
          bsn_sedbudy = bsn_sedbudy + bsn_sedbudm
          bsn_sedbudm = bsn_sedbudz
        end if
        !! yearly print
        if (pco%sed_bud%y == "y") then
          write (3171,*) time%day, time%mo, time%day_mo, time%yrc, bsn_sedbudy
          if (pco%csvout == "y") then
            write (3175,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, bsn_sedbudy
          end if
        end if
        bsn_sedbuda = bsn_sedbuda + bsn_sedbudy
        bsn_sedbudy = bsn_sedbudz
      end if

      !! on the last simulation day, convert the simulation total to an annual average
      if (time%end_sim == 1 .and. time%yrs_prt > 0.) then
        bsn_sedbuda = bsn_sedbuda / time%yrs_prt
        if (pco%sed_bud%a == "y") then
          write (3152,*) time%day, time%mo, time%day_mo, time%yrc, bsn_sedbuda
          if (pco%csvout == "y") then
            write (3176,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, bsn_sedbuda
          end if
        end if
      end if

      return
      end subroutine basin_sedbud_output
