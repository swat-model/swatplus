      subroutine channel_output (jrch)
      
      use time_module
      use basin_module
      use hydrograph_module, only : ob, sp_ob1
      use channel_module
      use climate_module
      
      implicit none
      
      integer, intent (in) :: jrch    !units         |description 
      integer :: iob                  !              |
      
      iob = sp_ob1%chan + jrch - 1
             
      ch_m(jrch) = ch_m(jrch) + ch_d(jrch)
      
!!!! subdaily print      
      !if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
      ! if (pco%chan%t == "y".and.time%step > 0) then
      !     do ii = 1, time%step 
      !        write (4400,"(4i6,10(1x,e15.4))") jrch, time%yrc,time%day, ii,wst(iwst)%weat%ts(ii),ob(icmd)%ts(1,ii)%flo/Real(time%dtm)/60., ob(icmd)%ts(1,ii)%sed
	  !     end do
      !  end if 
      !end if
!!!!! daily print
       if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
        if (pco%chan%d == "y") then
          write (2480,100) time%day, time%mo, time%day_mo, time%yrc, jrch, ob(iob)%gis_id, ob(iob)%name, ch_d(jrch)
          if (pco%csvout == "y") then
            write (2484,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, jrch, ob(iob)%gis_id, ob(iob)%name, ch_d(jrch)
          end if 
        end if 
      end if

!!!!! monthly print
      if (time%end_mo == 1) then
        ch_y(jrch) = ch_y(jrch) + ch_m(jrch)
        if (pco%chan%m == "y") then
          write (2481,100) time%day, time%mo, time%day_mo, time%yrc, jrch, ob(iob)%gis_id, ob(iob)%name, ch_m(jrch)
          if (pco%csvout == "y") then
            write (2485,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, jrch, ob(iob)%gis_id, ob(iob)%name, ch_m(jrch)
          end if
        end if
        ch_m(jrch) = chz
      end if

!!!!! yearly print
      if (time%end_yr == 1) then
        ch_a(jrch) = ch_a(jrch) + ch_y(jrch)
        if (pco%chan%y == "y") then 
          write (2482,100) time%day, time%mo, time%day_mo, time%yrc, jrch, ob(iob)%gis_id, ob(iob)%name, ch_y(jrch)
          if (pco%csvout == "y") then
            write (2486,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, jrch, ob(iob)%gis_id, ob(iob)%name, ch_y(jrch)
          end if
        end if
        
        ch_y(jrch) = chz
      end if

!!!!! average annual print
      if (time%end_sim == 1 .and. pco%chan%a == "y") then
        ch_a(jrch) = ch_a(jrch) / time%yrs_prt
        write (2483,100) time%day, time%mo, time%day_mo, time%yrc, jrch, ob(iob)%gis_id, ob(iob)%name, ch_a(jrch)
        if (pco%csvout == "y") then
          write (2487,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, jrch, ob(iob)%gis_id, ob(iob)%name, ch_a(jrch)
        end if
      end if

100   format (4i6,2i8,2x,a,60e15.4)
      return
      end subroutine channel_output