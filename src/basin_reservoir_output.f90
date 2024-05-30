      subroutine basin_reservoir_output
      
      use time_module
      use basin_module
      use reservoir_module
      use hydrograph_module
      use water_body_module
      
      implicit none
      
      integer :: ires        !none      |counter
      real :: const
     
        !! zero daily variables
        bres = resmz
        bres_in_d = resmz
        bres_out_d = resmz
        bres_wat_d = wbodz
        
        !! sum all reservoirs to get basin totals
        do ires = 1, sp_ob%res
          bres = bres + res(ires)
          bres_in_d = bres_in_d + res_in_d(ires)
          res_in_d(ires) = resmz
          bres_out_d = bres_out_d + res_out_d(ires)
          res_out_d(ires) = resmz
          bres_wat_d = bres_wat_d + res_wat_d(ires)
          !res_wat_d(ires) = wbodz
        end do

        !! sum monthly variables
        bres_in_m = bres_in_m + bres_in_d
        bres_out_m = bres_out_m + bres_out_d
        bres_wat_m = bres_wat_m + bres_wat_d
        
        !! daily print - RESERVOIR
        if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
          if (pco%res_bsn%d == "y") then
            write (2100,100) time%day, time%mo, time%day_mo, time%yrc, ires, "     1", bsn%name, bres_wat_d, &
              bres, bres_in_d, bres_out_d
            if (pco%csvout == "y") then
              write (2104,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, ires, "     1", bsn%name, &
                bres_wat_d, bres, bres_in_d, bres_out_d
            end if
          end if
        end if

        !! monthly print - RESERVOIR
        if (time%end_mo == 1) then
          const = float (ndays(time%mo + 1) - ndays(time%mo))
          bres_in_y = bres_in_y + bres_in_m
          bres_out_y = bres_out_y + bres_out_m
          bres_wat_m = bres_wat_m // const
          bres_wat_y = bres_wat_y + bres_wat_m
          if (pco%res_bsn%m == "y") then
           write (2101,100) time%day, time%mo, time%day_mo, time%yrc, ires, "     1", bsn%name,  bres_wat_m, bres, &
             bres_in_m, bres_out_m 
            if (pco%csvout == "y") then
              write (2105,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, ires, "     1", bsn%name,  &
                bres_wat_m, bres, bres_in_m, bres_out_m
            endif
          end if
          bres_in_m = resmz
          bres_out_m = resmz
          bres_wat_m = wbodz
        end if

        !! yearly print - RESERVOIR
        if (time%end_yr == 1) then
          bres_in_a = bres_in_a + bres_in_y
          bres_out_a = bres_out_a + bres_out_y
          bres_wat_y = bres_wat_y // 12.
          bres_wat_a = bres_wat_a + bres_wat_y
          if (pco%res_bsn%y == "y") then
            write (2102,100) time%day, time%mo, time%day_mo, time%yrc, ires, "     1", bsn%name,  bres_wat_y, &
              bres, bres_in_y, bres_out_y
            if (pco%csvout == "y") then
              write (2106,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, ires, "     1", bsn%name, &
                bres_wat_y, bres, bres_in_y, bres_out_y 
            end if
          end if
          !! zero yearly variables        
          bres_in_y = resmz
          bres_out_y = resmz
          bres_wat_y = wbodz
        end if
        
      !! average annual print - RESERVOIR
      if (time%end_sim == 1 .and. pco%res_bsn%a == "y") then
        bres_in_a = bres_in_a / time%yrs_prt
        bres_out_a = bres_out_a / time%yrs_prt
        bres_wat_a = bres_wat_a / time%yrs_prt
        write (2103,100) time%day, time%mo, time%day_mo, time%yrc, ires, "     1", bsn%name, bres_wat_a, bres, &
          bres_in_a, bres_out_a
        if (pco%csvout == "y") then 
          write (2107,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, ires, "     1", bsn%name, &
            bres_wat_a, bres, bres_in_a, bres_out_a
        end if 
      end if
      
      return
100   format (4i6,i8,2x,a,2x,a17,f14.4,59(1x,e14.4))  
      
      end subroutine basin_reservoir_output