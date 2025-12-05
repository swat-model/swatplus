      subroutine wallo_treat_output (iwallo)

      use time_module
      use hydrograph_module
      use water_allocation_module
      
      implicit none
      
      integer, intent (in) :: iwallo        !             |
      integer :: itrt

      !! loop through and print each water treatment object
      do itrt = 1, wallo(iwallo)%wtp
      
!!!!! daily print
        if (pco%water_allo%d == "y") then
          write (3110,*) time%day, time%mo, time%day_mo, time%yrc, itrt, om_treat_name(itrt), wal_tr_omd(itrt)

          if (pco%csvout == "y") then
          write (3114,'(*(G0.3,:","))') time%mo, time%day_mo, time%yrc, itrt, om_treat_name(itrt), wal_tr_omd(itrt)
          end if
        end if
       
        !! sum amount of monthly treated water
        wal_tr_omd(itrt) = hz

!!!!! monthly print
        if (time%end_mo == 1) then
          !! sum amount of yearly treated water
          wal_tr_omy(itrt) = wal_tr_omy(itrt) + wal_tr_omm(itrt)

          if (pco%water_allo%m == "y") then
          write (3111,*) time%mo, time%day_mo, time%yrc, itrt, om_treat_name(itrt), wal_tr_omm(itrt)
 
          if (pco%csvout == "y") then
          write (3115,'(*(G0.3,:","))') time%mo, time%day_mo, time%yrc, itrt, om_treat_name(itrt), wal_tr_omm(itrt)
          end if
        end if

        wal_tr_omm(itrt) = hz

      end if

!!!!! yearly print
      if (time%end_yr == 1) then
        !! sum amount of yearly treated water
        wal_tr_oma(itrt) =  wal_tr_oma(itrt) + wal_tr_omy(itrt)
          
        if (pco%water_allo%y == "y") then
          write (3112,*) time%mo, time%day_mo, time%yrc, itrt, om_treat_name(itrt), wal_tr_omy(itrt)
  
              if (pco%csvout == "y") then
          write (3116,'(*(G0.3,:","))') time%mo, time%day_mo, time%yrc, itrt, om_treat_name(itrt), wal_tr_omy(itrt)
          end if
        end if

        wal_tr_omy(itrt) = hz

      end if

!!!!! average annual print
      if (time%end_sim == 1) then
        !! sum amount of average annual treated water
        wal_tr_oma(itrt) = wal_tr_oma(itrt) / time%yrs_prt

        if (pco%water_allo%a == "y") then
        write (3113,*) time%mo, time%day_mo, time%yrc, itrt, om_treat_name(itrt), wal_tr_oma(itrt)

        if (pco%csvout == "y") then
        write (3117,'(*(G0.3,:","))') time%mo, time%day_mo, time%yrc, itrt, om_treat_name(itrt), wal_tr_oma(itrt)
        end if
       end if
      end if

      end do    ! do itrt = 1, wallo(iwallo)%wtp
      
      return
      
100   format (4i6,i8,5x,a,5x,i8,5x,i8,5x,a,5x,i8,20(7x,a,5x,i8,3f15.1))
    end subroutine wallo_treat_output
