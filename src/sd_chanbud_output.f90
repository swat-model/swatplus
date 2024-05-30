      subroutine sd_chanbud_output (ichan)
    
      use sd_channel_module
      use basin_module
      use time_module
      use hydrograph_module
      
      implicit none
      integer, intent (in) :: ichan         !             |
      integer :: iob                        !             |
      real :: const                         !             |
       
      iob = sp_ob1%chandeg + ichan - 1

      ch_sed_bud_m(ichan) = ch_sed_bud_m(ichan) + ch_sed_bud(ichan)
      
!!!!! daily print
       if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
        if (pco%sd_chan%d == "y") then
          write (4808,100) time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name, ch_sed_bud(ichan)
           if (pco%csvout == "y") then
             write (4812,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name, &
             ch_sed_bud(ichan)
           end if
        end if
      end if

!!!!! monthly print
        if (time%end_mo == 1) then
          ch_sed_bud_y(ichan) = ch_sed_bud_y(ichan) + ch_sed_bud_m(ichan)
          !const = float (ndays(time%mo + 1) - ndays(time%mo))
          !ch_sed_bud_m(ichan) = ch_sed_bud_m(ichan) / const
          
          if (pco%sd_chan%m == "y") then
          write (4809,100) time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name, ch_sed_bud_m(ichan)
          if (pco%csvout == "y") then
            write (4813,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name, &
              ch_sed_bud_m(ichan)
          end if
        end if
        ch_sed_bud_m(ichan) = ch_sed_budz
        end if

!!!!! yearly print
      if (time%end_yr == 1) then
        ch_sed_bud_a(ichan) = ch_sed_bud_a(ichan) + ch_sed_bud_y(ichan)
        !const = time%day_end_yr
        !ch_sed_bud_y(ichan) = ch_sed_bud_y(ichan) / const
          
        if (pco%sd_chan%y == "y") then 
          write (4810,100) time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name, ch_sed_bud_y(ichan)
          if (pco%csvout == "y") then
           write (4814,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name, &
             ch_sed_bud_y(ichan)
          end if
        end if
      end if

!!!!! average annual print
      if (time%end_sim == 1) then
        !ch_sed_bud_a(ichan) = ch_sed_bud_a(ichan) / time%days_prt
        ch_sed_bud_a(ichan) = ch_sed_bud_a(ichan) / time%yrs_prt
        
        if (pco%sd_chan%a == "y") then
        write (4811,100) time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name, ch_sed_bud_a(ichan)
        if (pco%csvout == "y") then
          write (4815,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name, &
            ch_sed_bud_a(ichan)
        end if
       end if
     end if 
      
      return

100   format (4i6,2i8,2x,a,61e15.4)      
       
      end subroutine sd_chanbud_output