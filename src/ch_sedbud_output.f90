      subroutine ch_sedbud_output

      use time_module
      use hydrograph_module
      use sd_channel_module
      
      implicit none
      
      integer :: ichan
      integer :: iob

      !! command calls this routine after the daily channel morphology calculations
      !! loop through each channel and retain its daily values for the AA report
      do ichan = 1, sp_ob%chandeg
        iob = sp_ob1%chandeg + ichan - 1

        !! daily print - raw values, before folding into the monthly total.
        !! known limitation: wid/dep/fp_km2 are summed by the (+) operator, not
        !! averaged, so at this tier they are the channel's own current geometry
        !! (nothing has been added to it yet today) and are still correct; once
        !! folded into a month or year total below, they are not - only %a
        !! (further down) recomputes them fresh from sd_ch and is reliable
        if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
          if (pco%sed_bud%d == "y") then
            write (3153,*) time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%name, ob(iob)%area_ha, sd_ch(ichan)%chl, ch_morph(ichan)
            if (pco%csvout == "y") then
              write (3157,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%name, ob(iob)%area_ha, sd_ch(ichan)%chl, ch_morph(ichan)
            end if
          end if
        end if

        !! fold today into the monthly total. ch_morphm/y/a are three separate
        !! arrays on purpose - reusing one for both the yearly total and the AA
        !! average is the original bug that made the AA report always print zero
        ch_morphm(ichan) = ch_morphm(ichan) + ch_morph(ichan)

        if (time%end_mo == 1) then
          !! monthly print - same known limitation as daily, now worse: wid/dep/
          !! fp_km2 have been summed across a month's worth of days
          if (pco%sed_bud%m == "y") then
            write (3154,*) time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%name, ob(iob)%area_ha, sd_ch(ichan)%chl, ch_morphm(ichan)
            if (pco%csvout == "y") then
              write (3158,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%name, ob(iob)%area_ha, sd_ch(ichan)%chl, ch_morphm(ichan)
            end if
          end if
          ch_morphy(ichan) = ch_morphy(ichan) + ch_morphm(ichan)
          ch_morphm(ichan) = ch_morphz
        end if

        if (time%end_yr == 1) then
          if (time%end_mo == 0) then
            ch_morphy(ichan) = ch_morphy(ichan) + ch_morphm(ichan)
            ch_morphm(ichan) = ch_morphz
          end if
          !! yearly print - same known limitation, summed across a year
          if (pco%sed_bud%y == "y") then
            write (3155,*) time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%name, ob(iob)%area_ha, sd_ch(ichan)%chl, ch_morphy(ichan)
            if (pco%csvout == "y") then
              write (3159,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%name, ob(iob)%area_ha, sd_ch(ichan)%chl, ch_morphy(ichan)
            end if
          end if
          ch_morpha(ichan) = ch_morpha(ichan) + ch_morphy(ichan)
          ch_morphy(ichan) = ch_morphz
        end if

        !! clear the daily structure before the next simulation day
        ch_morph(ichan) = ch_morphz

        !! only the last daily call converts the accumulated total and writes output
        if (time%end_sim == 1 .and. time%yrs_prt > 0.) then
          ch_morpha(ichan) = ch_morpha(ichan) / time%yrs_prt

          !! set channel geometry after averaging because these fields are not daily loads
          !! num is one because this record represents one channel, not a stream-order group
          ch_morpha(ichan)%num = 1
          ch_morpha(ichan)%wid = sd_ch(ichan)%chw
          ch_morpha(ichan)%dep = sd_ch(ichan)%chd
          ch_morpha(ichan)%fp_km2 = 5. * sd_ch(ichan)%chw * sd_ch(ichan)%chl / 1000.

          !! calculate AA flood plain deposition depth using final channel geometry
          ch_morpha(ichan)%fp_mm = 0.
          if (sd_ch(ichan)%chw > 1.e-6 .and. sd_ch(ichan)%chl > 1.e-6) then
            ch_morpha(ichan)%fp_mm = ch_morpha(ichan)%fp_t /                 &
              (5. * sd_ch(ichan)%chw * sd_ch(ichan)%chl)
          end if

          !! write metadata followed by the new channel morphology output type
          if (pco%sed_bud%a == "y") then
            write (3150,*) time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%name, ob(iob)%area_ha, sd_ch(ichan)%chl, &
              ch_morpha(ichan)
            if (pco%csvout == "y") then
              write (3160,'(*(G0.6,:","))') time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%name, ob(iob)%area_ha, &
                sd_ch(ichan)%chl, ch_morpha(ichan)
            end if
          end if
        end if

      end do    ! do ichan = 1, sp_ob%chandeg
      
      return
      end subroutine ch_sedbud_output
