      subroutine reservoir_output(j)
      
      use time_module
      use basin_module
      use reservoir_module
      use hydrograph_module
      use water_body_module
      
      implicit none
      
      integer, intent (in) :: j   !                |
      integer :: iob              !                |
      real :: const               !                |
      
      iob = sp_ob1%res + j - 1

!!!!! daily print
         if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
          if (pco%res%d == "y") then
            write (2540,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, res_wat_d(j), res(j), &
                res_in_d(j), res_out_d(j)
               if (pco%csvout == "y") then
                 write (2544,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                    res_wat_d(j), res(j), res_in_d(j), res_out_d(j)
             end if
          end if 
         end if 
  
        res_in_m(j) = res_in_m(j) + res_in_d(j)
        res_out_m(j) = res_out_m(j) + res_out_d(j)
        res_wat_m(j) = res_wat_m(j) + res_wat_d(j)

!!!!! monthly print
        if (time%end_mo == 1) then
          const = float (ndays(time%mo + 1) - ndays(time%mo))
          !res_in_m(j)%flo = res_in_m(j)%flo / const
          res_in_y(j) = res_in_y(j) + res_in_m(j)
          !res_out_m(j)%flo = res_out_m(j)%flo / const
          res_out_y(j) = res_out_y(j) + res_out_m(j)
          res_wat_m(j) = res_wat_m(j) // const
          res_wat_y(j) = res_wat_y(j) + res_wat_m(j)
          if (pco%res%m == "y") then
            write (2541,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, res_wat_m(j), res(j), &
                res_in_m(j), res_out_m(j)
              if (pco%csvout == "y") then
                write (2545,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                    res_wat_m(j), res(j), res_in_m(j), res_out_m(j)
              end if 
          end if
          res_in_m(j) = resmz
          res_out_m(j) = resmz
          res_wat_m(j) = wbodz
        end if

!!!!! yearly print
       if (time%end_yr == 1) then
          res_in_a(j) = res_in_a(j) + res_in_y(j)
          res_out_a(j) = res_out_a(j) + res_out_y(j)
          res_wat_y(j) = res_wat_y(j) // 12.
          res_wat_a(j) = res_wat_a(j) + res_wat_y(j)
          !write (7780,*) res_in_y(j)
          if (pco%res%y == "y") then
            write (2542,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, res_wat_y(j), res(j), &
                res_in_y(j), res_out_y(j)
              if (pco%csvout == "y") then
                write (2546,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                    res_wat_y(j), res(j), res_in_y(j), res_out_y(j)
              end if
          end if
          res_in_y(j) = resmz
          res_out_y(j) = resmz
          res_wat_y(j) = wbodz
       end if

!!!!! average annual print
        if (time%end_sim == 1 .and. pco%res%a == "y") then
          res_in_a(j) = res_in_a(j) / time%yrs_prt
          res_out_a(j) = res_out_a(j) / time%yrs_prt
          res_wat_a(j) = res_wat_a(j) / time%yrs_prt
          write (2543,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, res_wat_a(j), res(j), &
            res_in_a(j), res_out_a(j)
          if (pco%csvout == "y") then
            write (2547,'(*(G0.3,:","))')time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                res_wat_a(j), res(j), res_in_a(j), res_out_a(j)
          end if 
          res_in_a(j) = resmz
          res_out_a(j) = resmz
          res_wat_a(j) = wbodz
        end if
        
      return

100   format (4i6,2i10,2x,a,63e15.4)       
     
      end subroutine reservoir_output