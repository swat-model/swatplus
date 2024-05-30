      subroutine res_pesticide_output(j)
    
      use output_ls_pesticide_module
      use res_pesticide_module
      use plant_module
      use plant_data_module
      use time_module
      use basin_module
      use output_landscape_module
      use constituent_mass_module
      use hydrograph_module, only : sp_ob1, ob
      
      implicit none
      
      integer :: ipest                         !            |
      integer :: j
      integer :: iob
      real :: const
                         
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs HRU variables on daily, monthly and annual time steps
     
      iob = sp_ob1%res + j - 1
          
      !! print balance for each pesticide
      do ipest = 1, cs_db%num_pests
          
       respst_m(j)%pest(ipest) = respst_m(j)%pest(ipest) + respst_d(j)%pest(ipest)

      !! daily print
        if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
          if (pco%pest%d == "y") then
             write (2816,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, cs_db%pests(ipest), &
                respst_d(j)%pest(ipest)   !! pesticide balance
             if (pco%csvout == "y") then
               write (2820,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                    cs_db%pests(ipest), respst_d(j)%pest(ipest)
             end if
          end if
        end if
        !! zero daily output
        
        
        !! check end of month
        if (time%end_mo == 1) then
          respst_y(j)%pest(ipest) = respst_y(j)%pest(ipest) + respst_m(j)%pest(ipest)
          const = float (ndays(time%mo + 1) - ndays(time%mo))
          respst_m(j)%pest(ipest) = respst_m(j)%pest(ipest) // const

          !! monthly print
           if (pco%pest%m == "y") then
             write (2817,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, cs_db%pests(ipest), &
                respst_m(j)%pest(ipest)
               if (pco%csvout == "y") then
                 write (2821,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                    cs_db%pests(ipest), respst_m(j)%pest(ipest)
               end if
           end if
          
          respst_m(j)%pest(ipest) = res_pestbz
        end if
        
        !! check end of year
        if (time%end_yr == 1) then
          respst_a(j)%pest(ipest) = respst_a(j)%pest(ipest) + respst_y(j)%pest(ipest)
          const = time%day_end_yr
          respst_y(j)%pest(ipest) = respst_y(j)%pest(ipest) // const

          !! yearly print
           if (time%end_yr == 1 .and. pco%pest%y == "y") then
             write (2818,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, cs_db%pests(ipest), &
                respst_y(j)%pest(ipest)
               if (pco%csvout == "y") then
                 write (2822,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                    cs_db%pests(ipest), respst_y(j)%pest(ipest)
               end if
           end if
           
        end if
        
!!!!! average annual print
         if (time%end_sim == 1 .and. pco%pest%a == "y") then
           respst_a(j)%pest(ipest) = respst_a(j)%pest(ipest) / time%yrs_prt
           respst_a(j)%pest(ipest) = respst_a(j)%pest(ipest) // time%days_prt
           write (2819,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, cs_db%pests(ipest), &
            respst_a(j)%pest(ipest)
           if (pco%csvout == "y") then
             write (2823,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                cs_db%pests(ipest), respst_a(j)%pest(ipest)
           end if
           respst_a(j)%pest(ipest) = res_pestbz
         end if

      end do    !pesticide loop
      return
      
100   format (4i6,2i8,2x,2a,14e12.4)      

      end subroutine res_pesticide_output