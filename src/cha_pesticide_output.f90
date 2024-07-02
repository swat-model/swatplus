      subroutine cha_pesticide_output(jrch)
    
      use output_ls_pesticide_module
      use ch_pesticide_module
      use plant_module
      use plant_data_module
      use time_module
      use basin_module
      use output_landscape_module
      use constituent_mass_module
      use hydrograph_module, only : sp_ob1, ob
      
      implicit none
      
      integer, intent (in) :: jrch             !            |
      integer :: ipest                         !            |
      integer :: j
      integer :: iob
      real :: const
                         
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs HRU variables on daily, monthly and annual time steps

      j = jrch  !!! nbs
      
      iob = sp_ob1%chandeg + j - 1
          
      !! print balance for each pesticide
      do ipest = 1, cs_db%num_pests
          
       chpst_m(j)%pest(ipest) = chpst_m(j)%pest(ipest) + chpst_d(j)%pest(ipest)

      !! daily print
        if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
          if (pco%pest%d == "y") then
             write (2808,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, cs_db%pests(ipest), &
               chpst_d(j)%pest(ipest)   !! pesticide balance
             if (pco%csvout == "y") then
               write (2812,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                 cs_db%pests(ipest), chpst_d(j)%pest(ipest)
             end if
          end if
        end if
        !! zero daily output
        
        
        !! check end of month
        if (time%end_mo == 1) then
          chpst_y(j)%pest(ipest) = chpst_y(j)%pest(ipest) + chpst_m(j)%pest(ipest)
          const = float (ndays(time%mo + 1) - ndays(time%mo))
          chpst_m(j)%pest(ipest) = chpst_m(j)%pest(ipest) // const

          !! monthly print
           if (pco%pest%m == "y") then
             write (2809,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, cs_db%pests(ipest), &
               chpst_m(j)%pest(ipest)
               if (pco%csvout == "y") then
                 write (2813,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                   cs_db%pests(ipest), chpst_m(j)%pest(ipest)
               end if
           end if
          
          chpst_m(j)%pest(ipest) = ch_pestbz
        end if
        
        !! check end of year
        if (time%end_yr == 1) then
          chpst_a(j)%pest(ipest) = chpst_a(j)%pest(ipest) + chpst_y(j)%pest(ipest)
          const = time%day_end_yr
          chpst_y(j)%pest(ipest) = chpst_y(j)%pest(ipest) // const

          !! yearly print
           if (time%end_yr == 1 .and. pco%pest%y == "y") then
             write (2810,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, cs_db%pests(ipest), &
               chpst_y(j)%pest(ipest)
               if (pco%csvout == "y") then
                 write (2814,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                   cs_db%pests(ipest), chpst_y(j)%pest(ipest)
               end if
           end if
           
        end if
        
!!!!! average annual print
         if (time%end_sim == 1 .and. pco%pest%a == "y") then
           chpst_a(j)%pest(ipest) = chpst_a(j)%pest(ipest) / time%yrs_prt
           chpst_a(j)%pest(ipest) = chpst_a(j)%pest(ipest) // time%yrs_prt
           write (2811,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, cs_db%pests(ipest), &
             chpst_a(j)%pest(ipest)
           if (pco%csvout == "y") then
             write (2815,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
               cs_db%pests(ipest), chpst_a(j)%pest(ipest)
           end if
           chpst_a(j)%pest(ipest) = ch_pestbz
         end if

      end do    !pesticide loop
      return
      
100   format (4i6,2i8,2x,2a,14e12.4)      

      end subroutine cha_pesticide_output