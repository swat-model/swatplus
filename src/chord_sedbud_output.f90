      subroutine chord_sedbud_output

      use time_module
      use sd_channel_module
      use basin_module
      use hydrograph_module, only : sp_ob

      implicit none

      integer :: iord
      integer :: ichan
      real :: rnum

      !! day/mon/yr use the low-level per-order accumulator that sd_channel_sediment3.f90
      !! feeds directly every day. known limitation: wid/dep/fp_km2 are summed across
      !! every channel in the order AND every day in the period, not averaged - only
      !! %a (below) is reliable, since it aggregates each channel's already-finalized
      !! AA value instead of maintaining its own running total
      do iord = 1, 12

        !! daily print
        if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
          if (pco%sed_bud%d == "y") then
            write (3161,*) time%day, time%mo, time%day_mo, time%yrc, iord, ch_morph_ord(iord)
            if (pco%csvout == "y") then
              write (3165,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, iord, ch_morph_ord(iord)
            end if
          end if
        end if

        !! fold today into the monthly total, then clear the daily structure
        ch_morph_ordm(iord) = ch_morph_ordm(iord) + ch_morph_ord(iord)
        ch_morph_ord(iord) = ch_morphz

        if (time%end_mo == 1) then
          !! monthly print
          if (pco%sed_bud%m == "y") then
            write (3162,*) time%day, time%mo, time%day_mo, time%yrc, iord, ch_morph_ordm(iord)
            if (pco%csvout == "y") then
              write (3166,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, iord, ch_morph_ordm(iord)
            end if
          end if
          ch_morph_ordy(iord) = ch_morph_ordy(iord) + ch_morph_ordm(iord)
          ch_morph_ordm(iord) = ch_morphz
        end if

        if (time%end_yr == 1) then
          if (time%end_mo == 0) then
            ch_morph_ordy(iord) = ch_morph_ordy(iord) + ch_morph_ordm(iord)
            ch_morph_ordm(iord) = ch_morphz
          end if
          !! yearly print - the yearly total has no further destination since the AA
          !! report below is built from finalized channel values, not from this total
          if (pco%sed_bud%y == "y") then
            write (3163,*) time%day, time%mo, time%day_mo, time%yrc, iord, ch_morph_ordy(iord)
            if (pco%csvout == "y") then
              write (3167,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, iord, ch_morph_ordy(iord)
            end if
          end if
          ch_morph_ordy(iord) = ch_morphz
        end if

      end do

      !! ch_sedbud_output has already finalized each channel's average annual values
      !! rebuild the stream-order report from those channel records at the end of simulation
      if (time%end_sim == 1 .and. time%yrs_prt > 0.) then
        ch_morph_orda = ch_morphz

        !! group finalized channel AA values by order
        do ichan = 1, sp_ob%chandeg
          iord = sd_ch(ichan)%order
          if (iord >= 1 .and. iord <= 12) then
            ch_morph_orda(iord) = ch_morph_orda(iord) + ch_morpha(ichan)
          end if
        end do

        !! report the mean channel morphology in each stream order
        !! max(...,1) avoids division by zero and leaves unused orders as zero records
        do iord = 1, 12
          rnum = real(max(ch_morph_orda(iord)%num, 1))
          ch_morph_orda(iord) = ch_morph_orda(iord) / rnum

          !! write stream order followed by the new channel morphology output type
          if (pco%sed_bud%a == "y") then
            write (3151,*) time%day, time%mo, time%day_mo, time%yrc, iord, ch_morph_orda(iord)
            if (pco%csvout == "y") then
              write (3168,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, iord, ch_morph_orda(iord)
            end if
          end if
        end do
      end if

      return
      end subroutine chord_sedbud_output
