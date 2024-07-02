      subroutine channel_om_output !(jrch)
      
      use time_module
      use basin_module
      use hydrograph_module
      use channel_module
      use climate_module
      
      implicit none
      
      !integer, intent (in) :: jrch    !units         |description 
      integer :: iob                   !              |
      
      iob = sp_ob1%chandeg + jrch - 1
             
      ch_in_m(jrch) = ch_in_m(jrch) + ch_in_d(jrch)
      ch_out_m(jrch) = ch_out_m(jrch) + ch_out_d(jrch)
      
!!!!! daily print   (channel_om_day.txt/csv no opening for files)
       if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
        if (pco%chan%d == "y") then
          write (2900,100) time%day, time%mo, time%day_mo, time%yrc, jrch, ob(iob)%gis_id, ob(iob)%name, ch_in_d(jrch), &
          ch_out_d(jrch)
          if (pco%csvout == "y") then
            write (2904,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, jrch, ob(iob)%gis_id, ob(iob)%name, &
            ch_in_d(jrch), ch_out_d(jrch)
          end if 
        end if 
      end if

!!!!! monthly print   (channel_om_mon.txt/csv no opening for file)
      if (time%end_mo == 1) then
        ch_in_y(jrch) = ch_in_y(jrch) + ch_in_m(jrch)
        ch_out_y(jrch) = ch_out_y(jrch) + ch_out_m(jrch)        
        if (pco%chan%m == "y") then
          write (2901,100) time%day, time%mo, time%day_mo, time%yrc, jrch, ob(iob)%gis_id, ob(iob)%name, ch_in_m(jrch), &
          ch_out_m(jrch)
          if (pco%csvout == "y") then
            write (2905,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, jrch, ob(iob)%gis_id, ob(iob)%name, &
            ch_in_m(jrch), ch_out_m(jrch)
          end if
        end if
        ch_in_m(jrch) = chomz
        ch_out_m(jrch) = chomz
      end if

!!!!! yearly print   (channel_om_yr.txt/csv no opening for file)
      if (time%end_yr == 1) then
        ch_in_a(jrch) = ch_in_a(jrch) + ch_in_y(jrch)
        ch_out_a(jrch) = ch_out_a(jrch) + ch_out_y(jrch)
        if (pco%chan%y == "y") then 
          write (2902,100) time%day, time%mo, time%day_mo, time%yrc, jrch, ob(iob)%gis_id, ob(iob)%name, ch_in_y(jrch), &
          ch_out_y(jrch)
          if (pco%csvout == "y") then
            write (2906,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, jrch, ob(iob)%gis_id, ob(iob)%name, &
            ch_in_y(jrch), ch_out_y(jrch)
          end if
        end if
        
        ch_in_y(jrch) = chomz
        ch_out_y(jrch) = chomz
      end if

!!!!! average annual print (channel_om_aa.txt/csv no opening for file)
      if (time%end_sim == 1 .and. pco%chan%a == "y") then
        ch_in_a(jrch) = ch_in_a(jrch) / time%yrs_prt
        ch_out_a(jrch) = ch_out_a(jrch) / time%yrs_prt
        write (2903,100) time%day, time%mo, time%day_mo, time%yrc, jrch, ob(iob)%gis_id, ob(iob)%name, ch_in_a(jrch), &
        ch_out_a(jrch)
        if (pco%csvout == "y") then
          write (2907,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, jrch, ob(iob)%gis_id, ob(iob)%name, &
          ch_in_a(jrch), ch_out_a(jrch)
        end if
      end if

100   format (4i6,2i8,2x,a,60e15.4)
      
      return
      end subroutine channel_om_output