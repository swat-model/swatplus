      subroutine basin_res_pest_output
    
      use output_ls_pesticide_module
      use res_pesticide_module
      use plant_module
      use plant_data_module
      use time_module
      use basin_module
      use output_landscape_module
      use constituent_mass_module
      use hydrograph_module, only : sp_ob, sp_ob1, ob
      
      implicit none
      
      integer :: ipest 
      integer :: ires
      integer :: iob
      real :: const
                         
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs HRU variables on daily, monthly and annual time steps
     
      iob = sp_ob1%res
          
      !! print balance for each pesticide
      do ipest = 1, cs_db%num_pests
        brespst_d%pest(ipest) = res_pestbz
        
          do ires = 1, sp_ob%res
            brespst_d%pest(ipest) = brespst_d%pest(ipest) + respst_d(ires)%pest(ipest)
          end do
          
       brespst_m%pest(ipest) = brespst_m%pest(ipest) + brespst_d%pest(ipest)

      !! daily print
        if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
          if (pco%pest%d == "y") then
             write (2848,100) time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", &
               ob(iob)%name, cs_db%pests(ipest), brespst_d%pest(ipest)   !! pesticide balance
             if (pco%csvout == "y") then
               write (2852,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", &
                 ob(iob)%name, cs_db%pests(ipest), brespst_d%pest(ipest)
             end if
          end if
        end if
        !! zero daily output
        
        
        !! check end of month
        if (time%end_mo == 1) then
          brespst_y%pest(ipest) = brespst_y%pest(ipest) + brespst_m%pest(ipest)
          const = float (ndays(time%mo + 1) - ndays(time%mo))
          brespst_m%pest(ipest) = brespst_m%pest(ipest) // const

          !! monthly print
           if (pco%pest%m == "y") then
             write (2849,100) time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", ob(iob)%name, &
               cs_db%pests(ipest), brespst_m%pest(ipest)
               if (pco%csvout == "y") then
                 write (2853,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", &
                   ob(iob)%name, cs_db%pests(ipest), brespst_m%pest(ipest)
               end if
           end if
          
          brespst_m%pest(ipest) = res_pestbz
        end if
        
        !! check end of year
        if (time%end_yr == 1) then
          brespst_a%pest(ipest) = brespst_a%pest(ipest) + brespst_y%pest(ipest)
          const = time%day_end_yr
          brespst_y%pest(ipest) = brespst_y%pest(ipest) // const

          !! yearly print
           if (time%end_yr == 1 .and. pco%pest%y == "y") then
             write (2850,100) time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", ob(iob)%name, &
               cs_db%pests(ipest), brespst_y%pest(ipest)
               if (pco%csvout == "y") then
                 write (2854,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", &
                   ob(iob)%name, cs_db%pests(ipest), brespst_y%pest(ipest)
               end if
           end if
           
        end if
        
!!!!! average annual print
         if (time%end_sim == 1 .and. pco%pest%a == "y") then
           brespst_a%pest(ipest) = brespst_a%pest(ipest) / time%yrs_prt
           brespst_a%pest(ipest) = brespst_a%pest(ipest) // time%days_prt
           write (2851,100) time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", ob(iob)%name, &
             cs_db%pests(ipest), brespst_a%pest(ipest)
           if (pco%csvout == "y") then
             write (2855,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", &
               ob(iob)%name, cs_db%pests(ipest), brespst_a%pest(ipest)
           end if
           brespst_a%pest(ipest) = res_pestbz
         end if

      end do    !pesticide loop
      return
      
100   format (4i6,2a,2x,2a,14e12.4)

      end subroutine basin_res_pest_output