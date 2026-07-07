      subroutine hru_pesticide_output(ihru)
    
      use output_ls_pesticide_module
      use plant_module
      use plant_data_module
      use time_module
      use basin_module
      use output_landscape_module
      use constituent_mass_module
      use hydrograph_module, only : sp_ob1, ob
      
      implicit none
      
      integer, intent (in) :: ihru             !            |
      integer :: ipest                         !            |
      integer :: j
      integer :: iob
      real :: const
                         
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs HRU variables on daily, monthly and annual time steps

      j = ihru
      
      iob = sp_ob1%hru + j - 1
          
      !! print balance for each pesticide
      do ipest = 1, cs_db%num_pests
          
      hpestb_m(j)%pest(ipest) = hpestb_m(j)%pest(ipest) + hpestb_d(j)%pest(ipest)

      !! daily print
        if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
          if (pco%pest%d == "y") then
             write (2800,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, cs_db%pests(ipest), &
               hpestb_d(j)%pest(ipest)   !! pesticide balance
             if (pco%csvout == "y") then
               write (2804,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                 cs_db%pests(ipest), hpestb_d(j)%pest(ipest)
             end if
          end if
        end if
        !! zero daily output
        
        
        !! check end of month
        if (time%end_mo == 1) then
          hpestb_y(j)%pest(ipest) = hpestb_y(j)%pest(ipest) + hpestb_m(j)%pest(ipest)
          const = float (ndays(time%mo + 1) - ndays(time%mo))
          hpestb_m(j)%pest(ipest) = hpestb_m(j)%pest(ipest) // const

          !! monthly print
           if (pco%pest%m == "y") then
             write (2801,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, cs_db%pests(ipest), &
               hpestb_m(j)%pest(ipest)
               if (pco%csvout == "y") then
                 write (2805,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                   cs_db%pests(ipest), hpestb_m(j)%pest(ipest)
               end if
           end if
          
          hpestb_m(j)%pest(ipest) = pestbz
        end if
        
        !! check end of year
        if (time%end_yr == 1) then
          hpestb_a(j)%pest(ipest) = hpestb_a(j)%pest(ipest) + hpestb_y(j)%pest(ipest)
          const = time%day_end_yr
          hpestb_y(j)%pest(ipest) = hpestb_y(j)%pest(ipest) // const

          !! yearly print
           if (time%end_yr == 1 .and. pco%pest%y == "y") then
             write (2802,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, cs_db%pests(ipest), &
               hpestb_y(j)%pest(ipest)
               if (pco%csvout == "y") then
                 write (2806,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                   cs_db%pests(ipest), hpestb_y(j)%pest(ipest)
               end if
           end if
           
        end if
        
!!!!! average annual print
         if (time%end_sim == 1 .and. pco%pest%a == "y") then
           hpestb_a(j)%pest(ipest) = hpestb_a(j)%pest(ipest) / time%yrs_prt
           hpestb_a(j)%pest(ipest) = hpestb_a(j)%pest(ipest) // time%days_prt
           write (2803,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, cs_db%pests(ipest), &
             hpestb_a(j)%pest(ipest)
           if (pco%csvout == "y") then
             write (2807,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
               cs_db%pests(ipest), hpestb_a(j)%pest(ipest)
           end if
           hpestb_a(j)%pest(ipest) = pestbz
         end if

      end do    !pesticide loop
      return
      
100   format (4i6,2i8,2x,2a,14e12.4)      

      end subroutine hru_pesticide_output