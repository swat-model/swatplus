      subroutine wallo_use_output (iwallo)

      use time_module
      use hydrograph_module
      use water_allocation_module
      
      implicit none
      
      integer, intent (in) :: iwallo        !             |
      integer :: iuse

      !! loop through and print each use object
      do iuse = 1, wallo(iwallo)%uses
      
!!!!! daily print
        if (pco%water_allo%d == "y") then
          write (3110,*) time%day, time%mo, time%day_mo, time%yrc, iuse, om_use_name(iuse), wal_use_omd(iuse)

          if (pco%csvout == "y") then
          write (3114,'(*(G0.3,:","))') time%mo, time%day_mo, time%yrc, iuse, om_use_name(iuse), wal_use_omd(iuse)
          end if
        end if
       
        !! sum amount of monthly used water
        wal_use_omd(iuse) = hz

!!!!! monthly print
        if (time%end_mo == 1) then
          !! sum amount of yearly used water
          wal_use_omy(iuse) = wal_use_omy(iuse) + wal_use_omm(iuse)

          if (pco%water_allo%m == "y") then
          write (3111,*) time%mo, time%day_mo, time%yrc, iuse, om_use_name(iuse), wal_use_omm(iuse)
 
          if (pco%csvout == "y") then
          write (3115,'(*(G0.3,:","))') time%mo, time%day_mo, time%yrc, iuse, om_use_name(iuse), wal_use_omm(iuse)
          end if
        end if

        wal_use_omm(iuse) = hz

      end if

!!!!! yearly print
      if (time%end_yr == 1) then
        !! sum amount of yearly used water
        wal_use_oma(iuse) =  wal_use_oma(iuse) + wal_use_omy(iuse)
          
        if (pco%water_allo%y == "y") then
          write (3112,*) time%mo, time%day_mo, time%yrc, iuse, om_use_name(iuse), wal_use_omy(iuse)
  
              if (pco%csvout == "y") then
          write (3116,'(*(G0.3,:","))') time%mo, time%day_mo, time%yrc, iuse, om_use_name(iuse), wal_use_omy(iuse)
          end if
        end if

        wal_use_omy(iuse) = hz

      end if

!!!!! average annual print
      if (time%end_sim == 1) then
        !! sum amount of average annual used water
        wal_use_oma(iuse) = wal_use_oma(iuse) / time%yrs_prt

        if (pco%water_allo%a == "y") then
        write (3113,*) time%mo, time%day_mo, time%yrc, iuse, om_use_name(iuse), wal_use_oma(iuse)

        if (pco%csvout == "y") then
        write (3117,'(*(G0.3,:","))') time%mo, time%day_mo, time%yrc, iuse, om_use_name(iuse), wal_use_oma(iuse)
        end if
       end if
      end if

      end do    ! do iuse = 1, wallo(iwallo)%uses
      
      return
      
100   format (4i6,i8,5x,a,5x,i8,5x,i8,5x,a,5x,i8,20(7x,a,5x,i8,3f15.1))
      end subroutine wallo_use_output