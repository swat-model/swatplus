      subroutine basin_recall_output
      
      use time_module
      use basin_module
      use hydrograph_module
      
      implicit none
             
      integer ::  irec      !none      |counter

           
        brec_d = hz
        !! sum monthly variables
        do irec = 1,sp_ob%recall
          brec_d = brec_d + rec_d(irec)
        end do
        
        brec_d = brec_d / bsn%area_tot_ha
        brec_m = brec_m + brec_d
        
        !! daily print - BASIN RECALL
        if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
          if (pco%recall_bsn%d == "y") then
            write (4500,*) time%day, time%mo, time%day_mo, time%yrc, bsn%name, brec_d
            if (pco%csvout == "y") then
              write (4504,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, bsn%name, brec_d
            end if
          end if
        end if

        !! monthly print - BASIN RECALL
        if (time%end_mo == 1) then
          brec_y = brec_y + brec_m
          if (pco%recall_bsn%m == "y") then
            write (4501,*) time%day, time%mo, time%day_mo, time%yrc, bsn%name, brec_m
            if (pco%csvout == "y") then
              write (4505,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, bsn%name, brec_m
            endif
          end if
          brec_m = hz
        end if

        !! yearly print - BASIN RECALL
        if (time%end_yr == 1) then
          brec_a = brec_a + brec_y
          if (pco%recall_bsn%y == "y") then
            write (4502,*) time%day, time%mo, time%day_mo, time%yrc, bsn%name, brec_y
            if (pco%csvout == "y") then
              write (4506,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, bsn%name, brec_y 
            end if
          end if
          !! zero yearly variables        
          brec_y = hz
        end if
        
      !! average annual print - BASIN RECALL

      if (time%end_sim == 1 .and. pco%recall_bsn%a == "y") then
        brec_a = brec_a / time%yrs_prt
        write (4503,*) time%day, time%mo, time%day_mo, time%yrc, bsn%name, brec_a
        if (pco%csvout == "y") then 
          write (4507,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, bsn%name, brec_a 
        end if 
      end if
      
      return
       
      end subroutine basin_recall_output