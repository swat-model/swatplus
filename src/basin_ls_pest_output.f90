      subroutine basin_ls_pest_output
    
      use output_ls_pesticide_module
      use plant_module
      use plant_data_module
      use time_module
      use basin_module
      use calibration_data_module
      use output_landscape_module
      use constituent_mass_module
      use hydrograph_module, only : sp_ob, sp_ob1, ob
      
      implicit none
      
      integer :: ipest 
      integer :: ls 
      integer :: iihru
      integer :: iob
      real :: const
                               
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs HRU variables on daily, monthly and annual time steps
     
      iob = sp_ob1%hru
          
      !! print balance for each pesticide
      do ipest = 1, cs_db%num_pests
        bpestb_d%pest(ipest) = pestbz
        
        do ls = 1, sp_ob%hru
          iihru = lsu_elem(ls)%obtypno
          const = lsu_elem(iihru)%bsn_frac
          bpestb_d%pest(ipest) = bpestb_d%pest(ipest) + hpestb_d(iihru)%pest(ipest)
        end do
          
        bpestb_m%pest(ipest) = bpestb_m%pest(ipest) + bpestb_d%pest(ipest)

        !! daily print
        if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
          if (pco%pest%d == "y") then
            write (2864,100) time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", ob(iob)%name, &
               cs_db%pests(ipest), bpestb_d%pest(ipest)   !! pesticide balance
            if (pco%csvout == "y") then
              write (2868,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", &
                 ob(iob)%name, cs_db%pests(ipest), bpestb_d%pest(ipest)
            end if
          end if
        end if
        !! zero daily output
        
        
        !! check end of month
        if (time%end_mo == 1) then
          bpestb_y%pest(ipest) = bpestb_y%pest(ipest) + bpestb_m%pest(ipest)
          const = float (ndays(time%mo + 1) - ndays(time%mo))
          bpestb_m%pest(ipest) = bpestb_m%pest(ipest) // const

          !! monthly print
           if (pco%pest%m == "y") then
             write (2865,100) time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", ob(iob)%name, &
               cs_db%pests(ipest), bpestb_m%pest(ipest)
               if (pco%csvout == "y") then
                 write (2869,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", &
                   ob(iob)%name, cs_db%pests(ipest), bpestb_m%pest(ipest)
               end if
           end if
          
          bpestb_m%pest(ipest) = pestbz
        end if
        
        !! check end of year
        if (time%end_yr == 1) then
          bpestb_a%pest(ipest) = bpestb_a%pest(ipest) + bpestb_y%pest(ipest)
          const = time%day_end_yr
          bpestb_y%pest(ipest) = bpestb_y%pest(ipest) // const

          !! yearly print
           if (time%end_yr == 1 .and. pco%pest%y == "y") then
             write (2866,100) time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", ob(iob)%name, &
               cs_db%pests(ipest), bpestb_y%pest(ipest)
               if (pco%csvout == "y") then
                 write (2870,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", &
                   ob(iob)%name, cs_db%pests(ipest), bpestb_y%pest(ipest)
               end if
           end if
           
        end if
        
!!!!! average annual print
         if (time%end_sim == 1 .and. pco%pest%a == "y") then
           bpestb_a%pest(ipest) = bpestb_a%pest(ipest) / time%yrs_prt
           bpestb_a%pest(ipest) = bpestb_a%pest(ipest) // time%days_prt
           write (2867,100) time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", ob(iob)%name, &
             cs_db%pests(ipest), bpestb_a%pest(ipest)
           if (pco%csvout == "y") then
             write (2871,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "       1", &
               ob(iob)%name, cs_db%pests(ipest), bpestb_a%pest(ipest)
           end if
           bpestb_a%pest(ipest) = pestbz
         end if

      end do    !pesticide loop
      return
      
100   format (4i6,2a,2x,2a,16e12.4)      

      end subroutine basin_ls_pest_output