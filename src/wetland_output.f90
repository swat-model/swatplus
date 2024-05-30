      subroutine wetland_output(j)
      
      use time_module
      use basin_module
      use reservoir_module
      use hydrograph_module
      use water_body_module
      
      implicit none
      
      integer :: j             !none          |hru number
      real :: const            !              |constant used for rate, days, etc
      integer :: iob              !                |
      
      iob = sp_ob1%hru + j - 1

!!!!! daily print
         if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
          if (pco%res%d == "y") then
            write (2548,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, wet_wat_d(j), wet(j), &
            wet_in_d(j), wet_out_d(j)
             if (pco%csvout == "y") then
               write (2552,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
               wet_wat_d(j), wet(j), wet_in_d(j), wet_out_d(j)
             end if
          end if 
        end if 
                                                    
        wet_in_m(j) = wet_in_m(j) + wet_in_d(j)
        wet_out_m(j) = wet_out_m(j) + wet_out_d(j)
        wet_wat_m(j) = wet_wat_m(j) + wet_wat_d(j)
        wet_in_d(j) = resmz
        wet_out_d(j) = resmz
        !wet_wat_d(j) = wbodz

!!!!! monthly print
        if (time%end_mo == 1) then
          const = float (ndays(time%mo + 1) - ndays(time%mo))
          wet_in_y(j) = wet_in_y(j) + wet_in_m(j)
          wet_out_y(j) = wet_out_y(j) + wet_out_m(j)
          wet_wat_m(j) = wet_wat_m(j) // const
          wet_wat_y(j) = wet_wat_y(j) + wet_wat_m(j)
          wet_in_m(j)%flo = wet_in_m(j)%flo / const
          wet_out_m(j)%flo = wet_out_m(j)%flo / const
          if (pco%res%m == "y") then
            write (2549,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, wet_wat_m(j), wet(j), &
            wet_in_m(j), wet_out_m(j)
              if (pco%csvout == "y") then
                write (2553,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                wet_wat_m(j), wet(j), wet_in_m(j), wet_out_m(j)
              end if 
          end if
          wet_in_m(j) = resmz
          wet_out_m(j) = resmz
          wet_wat_m(j) = wbodz
        end if

!!!!! yearly print
       if (time%end_yr == 1) then
          wet_in_a(j) = wet_in_a(j) + wet_in_y(j)
          wet_out_a(j) = wet_out_a(j) + wet_out_y(j)
          wet_wat_a(j) = wet_wat_a(j) + wet_wat_y(j)
          wet_in_y(j)%flo = wet_in_y(j)%flo / 12.
          wet_out_y(j)%flo = wet_out_y(j)%flo / 12.
          if (pco%res%y == "y") then
            write (2550,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, wet_wat_y(j), wet(j), &
            wet_in_y(j), wet_out_y(j)
              if (pco%csvout == "y") then
                write (2554,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                wet_wat_y(j), wet(j), wet_in_y(j), wet_out_y(j)
              end if
          end if
          wet_in_y(j) = resmz
          wet_out_y(j) = resmz
          wet_wat_y(j) = wbodz
       end if

!!!!! average annual print
        if (time%end_sim == 1 .and. pco%res%a == "y") then
          wet_in_a(j) = wet_in_y(j) / time%yrs_prt
          wet_out_a(j) = wet_out_y(j) / time%yrs_prt
          wet_wat_a(j) = wet_wat_a(j) / time%yrs_prt
          write (2551,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, wet_wat_a(j), wet(j), &
          wet_in_a(j), wet_out_a(j)
          if (pco%csvout == "y") then
            write (2555,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, wet_wat_a(j), &
            wet(j), wet_in_a(j), wet_out_a(j)
          end if 
        end if
        
      return
        
100   format (4i6,2i10,2x,a,63e15.4) 
       
      end subroutine wetland_output