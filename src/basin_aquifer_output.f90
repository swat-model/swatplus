      subroutine basin_aquifer_output
      
      use time_module
      use basin_module
      use aquifer_module
      use calibration_data_module
      use hydrograph_module, only : sp_ob
      
      implicit none
      
      integer :: iaq   !none      |counter
      real :: const    !          |     

     
        !! sum monthly variables

        baqu_d = aquz
        
        do iaq = 1, sp_ob%aqu
          const = aqu_prm(iaq)%area_ha / bsn%area_tot_ha
          baqu_d = baqu_d + aqu_d(iaq) * const
        end do
        
        baqu_m = baqu_m + baqu_d
        
        !! daily print - AQUIFER
         if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
          if (pco%aqu_bsn%d == "y") then
            write (2090,100) time%day, time%mo, time%day_mo, time%yrc, "       1", "     1", bsn%name, baqu_d
            if (pco%csvout == "y") then
              write (2094,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "     1", bsn%name, baqu_d
            end if
          end if
        end if

        !! monthly print - AQUIFER
        if (time%end_mo == 1) then
          const = float (ndays(time%mo + 1) - ndays(time%mo))
          baqu_m%stor = baqu_m%stor / const 
          baqu_m%dep_wt = baqu_m%dep_wt / const
          baqu_m%no3_st = baqu_m%no3_st / const
          baqu_y = baqu_y + baqu_m
          if (pco%aqu_bsn%m == "y") then
            write (2091,100) time%day, time%mo, time%day_mo, time%yrc, "       1", "     1", bsn%name, baqu_m
            if (pco%csvout == "y") then
              write (2095,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "     1", bsn%name, baqu_m
            endif
          end if
          baqu_m = aquz
        end if

        !! yearly print - AQUIFER
        if (time%end_yr == 1) then
          baqu_y%stor = baqu_y%stor / 12.
          baqu_y%dep_wt = baqu_y%dep_wt / 12.
          baqu_y%no3_st = baqu_y%no3_st / 12.
          baqu_a = baqu_a + baqu_y
          if (pco%aqu_bsn%y == "y") then
            write (2092,102) time%day, time%mo, time%day_mo, time%yrc, "       1", "     1", bsn%name, baqu_y
            if (pco%csvout == "y") then
              write (2096,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "     1", bsn%name, baqu_y 
            end if
          end if
          !! zero yearly variables        
          baqu_y = aquz
        end if
        
      !! average annual print - AQUIFER
      if (time%end_sim == 1 .and. pco%aqu_bsn%a == "y") then
        baqu_a = baqu_a / time%yrs_prt
        write (2093,102) time%day, time%mo, time%day_mo, time%yrc, "       1", "     1", bsn%name, baqu_a
        if (pco%csvout == "y") then 
          write (2097,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, "       1", "     1", bsn%name, baqu_a 
        end if 
      end if
      
      return
      
!100   format (4i6,i8,2x,a,2x,a16,17f15.3)
100   format (4i6,a,2x,a,2x,a16,17f15.3)
!102   format (4i6,2x,2a,2x,a16,17f15.3)
102   format (4i6,2x,2a,2x,a16,17f15.3)
       
      end subroutine basin_aquifer_output