      subroutine basin_ch_pest_output
    
      use output_ls_pesticide_module
      use ch_pesticide_module
      use plant_module
      use plant_data_module
      use time_module
      use basin_module
      use output_landscape_module
      use constituent_mass_module
      use hydrograph_module, only : sp_ob, sp_ob1, ob
      
      implicit none
      
      integer :: ipest                         !            |
      integer :: iob
      integer :: jrch
      real :: const
                         
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs HRU variables on daily, monthly and annual time steps
      
      iob = sp_ob1%chandeg
               
!! print balance for each pesticide          
      do ipest = 1, cs_db%num_pests
        bchpst_d%pest(ipest) = ch_pestbz
        
          do jrch = 1, sp_ob%chandeg
            bchpst_d%pest(ipest) = bchpst_d%pest(ipest) + chpst_d(jrch)%pest(ipest)
          end do
          
       bchpst_m%pest(ipest) = bchpst_m%pest(ipest) + bchpst_d%pest(ipest)

      !! daily print
        if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
          if (pco%pest%d == "y") then
             write (2832,100) time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", ob(iob)%name, &
               cs_db%pests(ipest), bchpst_d%pest(ipest)   !! pesticide balance
             if (pco%csvout == "y") then
               write (2836,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", &
                 ob(iob)%name, cs_db%pests(ipest), bchpst_d%pest(ipest)
             end if
          end if
        end if
        !! zero daily output
        
        
        !! check end of month
        if (time%end_mo == 1) then
          bchpst_y%pest(ipest) = bchpst_y%pest(ipest) + bchpst_m%pest(ipest)
          const = float (ndays(time%mo + 1) - ndays(time%mo))
          bchpst_m%pest(ipest) = bchpst_m%pest(ipest) // const

          !! monthly print
           if (pco%pest%m == "y") then
             write (2833,100) time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", ob(iob)%name, &
               cs_db%pests(ipest), bchpst_m%pest(ipest)
               if (pco%csvout == "y") then
                 write (2837,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", &
                   ob(iob)%name, cs_db%pests(ipest), bchpst_m%pest(ipest)
               end if
           end if
          
          bchpst_m%pest(ipest) = ch_pestbz
        end if
        
        !! check end of year
        if (time%end_yr == 1) then
          bchpst_a%pest(ipest) = bchpst_a%pest(ipest) + bchpst_y%pest(ipest)
          const = time%day_end_yr
          bchpst_y%pest(ipest) = bchpst_y%pest(ipest) // const

          !! yearly print
           if (time%end_yr == 1 .and. pco%pest%y == "y") then
             write (2834,100) time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", ob(iob)%name, &
               cs_db%pests(ipest), bchpst_y%pest(ipest)
               if (pco%csvout == "y") then
                 write (2838,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", &
                   ob(iob)%name, cs_db%pests(ipest), bchpst_y%pest(ipest)
               end if
           end if
           
        end if
        
!!!!! average annual print
         if (time%end_sim == 1 .and. pco%pest%a == "y") then
           bchpst_a%pest(ipest) = bchpst_a%pest(ipest) / time%yrs_prt
           bchpst_a%pest(ipest) = bchpst_a%pest(ipest) // time%days_prt
           write (2835,100) time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", ob(iob)%name, &
             cs_db%pests(ipest), bchpst_a%pest(ipest)
           if (pco%csvout == "y") then
             write (2839,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", &
               ob(iob)%name, cs_db%pests(ipest), bchpst_a%pest(ipest)
           end if
           bchpst_a%pest(ipest) = ch_pestbz
         end if

      end do    ! pesticide loop

      return
      
100   format (4i6,2a,2x,2a,14e12.4)  

      end subroutine basin_ch_pest_output