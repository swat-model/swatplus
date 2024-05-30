      subroutine sd_chanmorph_output (ichan)
    
      use sd_channel_module
      use basin_module
      use time_module
      use hydrograph_module
      
      implicit none
      integer, intent (in) :: ichan         !             |
      integer :: iob                        !             |
      real :: const                         !             |
       
      iob = sp_ob1%chandeg + ichan - 1

      chsd_m(ichan) = chsd_m(ichan) + chsd_d(ichan)
      
!!!!! daily print
       if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
        if (pco%sd_chan%d == "y") then
          write (4800,100) time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name, chsd_d(ichan)
           if (pco%csvout == "y") then
             write (4804,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name, &
             chsd_d(ichan)
           end if
        end if
      end if

!!!!! monthly print
        if (time%end_mo == 1) then
          chsd_y(ichan) = chsd_y(ichan) + chsd_m(ichan)
          const = float (ndays(time%mo + 1) - ndays(time%mo))
          chsd_m(ichan) = chsd_m(ichan) / const
          
          if (pco%sd_chan%m == "y") then
          write (4801,100) time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name, chsd_m(ichan)
          if (pco%csvout == "y") then
            write (4805,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name, &
              chsd_m(ichan)
          end if
        end if
        chsd_m(ichan) = chsdz
        end if

!!!!! yearly print
      if (time%end_yr == 1) then
        chsd_a(ichan) = chsd_a(ichan) + chsd_y(ichan)
        const = time%day_end_yr
        chsd_y(ichan) = chsd_y(ichan) / const
          
        if (pco%sd_chan%y == "y") then 
          write (4802,100) time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name, chsd_y(ichan)
          if (pco%csvout == "y") then
           write (4806,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name, &
             chsd_y(ichan)
          end if
        end if
      end if

!!!!! average annual print
      if (time%end_sim == 1) then
        chsd_a(ichan) = chsd_a(ichan) / time%days_prt
        chsd_a(ichan) = chsd_a(ichan) // time%yrs_prt
        
        if (pco%sd_chan%a == "y") then
        write (4803,100) time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name, chsd_a(ichan)
        if (pco%csvout == "y") then
          write (4807,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, ichan, ob(iob)%gis_id, ob(iob)%name, &
            chsd_a(ichan)
        end if
       end if
     end if 
      
      return

100   format (4i6,2i8,2x,a,61e15.4)      
       
      end subroutine sd_chanmorph_output