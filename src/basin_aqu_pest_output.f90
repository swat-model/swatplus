      subroutine basin_aqu_pest_output
    
      use output_ls_pesticide_module
      use aqu_pesticide_module
      use plant_module
      use plant_data_module
      use time_module
      use basin_module
      use output_landscape_module
      use constituent_mass_module
      use hydrograph_module, only : sp_ob, sp_ob1, ob
      
      implicit none
      
      integer :: ipest 
      integer :: iaq
      integer :: iob
      real :: const
      real :: stor_init      !kg         |store initial pesticide when entire object is zero'd
                         
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs HRU variables on daily, monthly and annual time steps
     
      iob = sp_ob1%aqu
          
      !! print balance for each pesticide
      do ipest = 1, cs_db%num_pests
        baqupst_d%pest(ipest) = aqu_pestbz
        
          do iaq = 1, sp_ob%aqu
            baqupst_d%pest(ipest) = baqupst_d%pest(ipest) .sum. aqupst_d(iaq)%pest(ipest)
            !! reset pesticide at start of next time step
            aqupst_d(iaq)%pest(ipest)%stor_init = aqupst_d(iaq)%pest(ipest)%stor_final
          end do
          
       baqupst_m%pest(ipest) = baqupst_m%pest(ipest) + baqupst_d%pest(ipest)

      !! daily print
        if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
          if (pco%pest%d == "y") then
             write (3000,100) time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", ob(iob)%name, cs_db%pests(ipest), &
               baqupst_d%pest(ipest)   !! pesticide balance
             if (pco%csvout == "y") then
               write (3004,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", ob(iob)%name, &
                 cs_db%pests(ipest), baqupst_d%pest(ipest)
             end if
          end if
        end if
        !! zero daily output
        
        
        !! check end of month
        if (time%end_mo == 1) then
          baqupst_y%pest(ipest) = baqupst_y%pest(ipest) + baqupst_m%pest(ipest)
          const = float (ndays(time%mo + 1) - ndays(time%mo))
          baqupst_m%pest(ipest) = baqupst_m%pest(ipest) // const
          baqupst_m%pest(ipest)%stor_final = baqupst_d%pest(ipest)%stor_final

          !! monthly print
           if (pco%pest%m == "y") then
             write (3001,100) time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", ob(iob)%name, &
               cs_db%pests(ipest), baqupst_m%pest(ipest)
               if (pco%csvout == "y") then
                 write (3005,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", &
                   ob(iob)%name, cs_db%pests(ipest), baqupst_m%pest(ipest)
               end if
           end if
          !! reset pesticide at start of next time step
          stor_init = baqupst_d%pest(ipest)%stor_final
          baqupst_m%pest(ipest) = aqu_pestbz
          baqupst_m%pest(ipest)%stor_init = stor_init
        end if
        
        !! check end of year
        if (time%end_yr == 1) then
          baqupst_a%pest(ipest) = baqupst_a%pest(ipest) + baqupst_y%pest(ipest)
          const = time%day_end_yr
          baqupst_y%pest(ipest) = baqupst_y%pest(ipest) // const
          baqupst_y%pest(ipest)%stor_final = baqupst_d%pest(ipest)%stor_final

          !! yearly print
           if (time%end_yr == 1 .and. pco%pest%y == "y") then
             write (3002,100) time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", ob(iob)%name, &
               cs_db%pests(ipest), baqupst_y%pest(ipest)
               if (pco%csvout == "y") then
                 write (3006,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", &
                   ob(iob)%name, cs_db%pests(ipest), baqupst_y%pest(ipest)
               end if
           end if
          !! reset pesticide at start of next time step
          stor_init = baqupst_d%pest(ipest)%stor_final
          baqupst_y%pest(ipest) = aqu_pestbz
          baqupst_y%pest(ipest)%stor_init = stor_init
        end if
        
!!!!! average annual print
         if (time%end_sim == 1 .and. pco%pest%a == "y") then
           baqupst_a%pest(ipest) = baqupst_a%pest(ipest) / time%yrs_prt
           baqupst_a%pest(ipest) = baqupst_a%pest(ipest) // time%days_prt
           baqupst_a%pest(ipest)%stor_final = baqupst_d%pest(ipest)%stor_final
           write (3003,100) time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", ob(iob)%name, &
             cs_db%pests(ipest), baqupst_a%pest(ipest)
           if (pco%csvout == "y") then
             write (3007,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", &
               ob(iob)%name, cs_db%pests(ipest), baqupst_a%pest(ipest)
           end if
           baqupst_a%pest(ipest) = aqu_pestbz
         end if

      end do    !pesticide loop
      
      return
      
100   format (4i6,2a,2x,2a,14e12.4)      

      end subroutine basin_aqu_pest_output