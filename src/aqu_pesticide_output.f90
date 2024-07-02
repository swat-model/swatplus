      subroutine aqu_pesticide_output(j)
    
      use output_ls_pesticide_module
      use aqu_pesticide_module
      use plant_module
      use plant_data_module
      use time_module
      use basin_module
      use output_landscape_module
      use constituent_mass_module
      use hydrograph_module, only : sp_ob1, ob
      
      implicit none
      
      integer :: ipest
      integer :: j
      integer :: iob
      real :: const
      real :: stor_init      !kg         |store initial pesticide when entire object is zero'd
                         
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs HRU variables on daily, monthly and annual time steps
     
      iob = sp_ob1%aqu + j - 1
          
      !! print balance for each pesticide
      do ipest = 1, cs_db%num_pests
          
       aqupst_m(j)%pest(ipest) = aqupst_m(j)%pest(ipest) + aqupst_d(j)%pest(ipest)

      !! daily print
        if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
          if (pco%pest%d == "y") then
             write (3008,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, cs_db%pests(ipest),&
               aqupst_d(j)%pest(ipest)   !! pesticide balance
             if (pco%csvout == "y") then
               write (3012,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                    cs_db%pests(ipest), aqupst_d(j)%pest(ipest)
             end if
          end if
        end if
        
        !! check end of month
        if (time%end_mo == 1) then
          aqupst_y(j)%pest(ipest) = aqupst_y(j)%pest(ipest) + aqupst_m(j)%pest(ipest)
          const = float (ndays(time%mo + 1) - ndays(time%mo))
          aqupst_m(j)%pest(ipest) = aqupst_m(j)%pest(ipest) // const
          aqupst_m(j)%pest(ipest)%stor_final = aqupst_d(j)%pest(ipest)%stor_final

          !! monthly print
           if (pco%pest%m == "y") then
             write (3009,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, cs_db%pests(ipest), &
               aqupst_m(j)%pest(ipest)
               if (pco%csvout == "y") then
                 write (3013,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                   cs_db%pests(ipest), aqupst_m(j)%pest(ipest)
               end if
           end if
          !! reset pesticide at start of next time step
          stor_init = aqupst_d(j)%pest(ipest)%stor_final
          aqupst_m(j)%pest(ipest) = aqu_pestbz
          aqupst_m(j)%pest(ipest)%stor_init = stor_init
        end if
        
        !! check end of year
        if (time%end_yr == 1) then
          aqupst_a(j)%pest(ipest) = aqupst_a(j)%pest(ipest) + aqupst_y(j)%pest(ipest)
          const = time%day_end_yr
          aqupst_y(j)%pest(ipest) = aqupst_y(j)%pest(ipest) // const
          aqupst_y(j)%pest(ipest)%stor_final = aqupst_d(j)%pest(ipest)%stor_final

          !! yearly print
           if (time%end_yr == 1 .and. pco%pest%y == "y") then
             write (3010,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, cs_db%pests(ipest), &
               aqupst_y(j)%pest(ipest)
               if (pco%csvout == "y") then
                 write (3014,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                   cs_db%pests(ipest), aqupst_y(j)%pest(ipest)
               end if
           end if
          !! reset pesticide at start of next time step
          stor_init = aqupst_d(j)%pest(ipest)%stor_final
          aqupst_y(j)%pest(ipest) = aqu_pestbz
          aqupst_y(j)%pest(ipest)%stor_init = stor_init
        end if
        
         !! average annual print
         if (time%end_sim == 1 .and. pco%pest%a == "y") then
           aqupst_a(j)%pest(ipest) = aqupst_a(j)%pest(ipest) / time%yrs_prt
           aqupst_a(j)%pest(ipest) = aqupst_a(j)%pest(ipest) // time%days_prt
           aqupst_a(j)%pest(ipest)%stor_final = aqupst_d(j)%pest(ipest)%stor_final
           write (3011,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, cs_db%pests(ipest), &
             aqupst_a(j)%pest(ipest)
           if (pco%csvout == "y") then
             write (3015,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
               cs_db%pests(ipest), aqupst_a(j)%pest(ipest)
           end if
           
         end if

      end do    !pesticide loop
      return
      
100   format (4i6,2i8,2x,2a,13e12.4)      

      end subroutine aqu_pesticide_output