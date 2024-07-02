      subroutine ch_pathogen_output(ihru)
    
      use output_ls_pathogen_module
      use plant_module
      use plant_data_module
      use time_module
      use basin_module
      use output_landscape_module
      use constituent_mass_module
      use hydrograph_module, only : sp_ob1, ob
      
      implicit none
      
      integer, intent (in) :: ihru              !            |
      integer :: ipaths                         !            |
      integer :: j
      integer :: iob
      real :: const
                         
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs HRU variables on daily, monthly and annual time steps

      j = ihru
      
      iob = sp_ob1%hru + j - 1
          
      !! print balance for each pathicide
      do ipaths = 1, cs_db%num_paths
          
      hpathb_m(j)%path(ipaths) = hpathb_m(j)%path(ipaths) + hpath_bal(j)%path(ipaths)

      !! daily print  (channel_path_day.txt/csv no opening for file)
        if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
          if (pco%wb_hru%d == "y") then
             write (2780,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpath_bal(j)%path(ipaths)   !! pathicide balance
             if (pco%csvout == "y") then
                  write (2784,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                    hpath_bal(j)%path(ipaths)
             end if
          end if
        end if
        !! check end of month
        if (time%end_mo == 1) then
          const = float (ndays(time%mo + 1) - ndays(time%mo))
          hpathb_m(j)%path(ipaths) = hpathb_m(j)%path(ipaths) // const
          !hwb_m(j) = hwb_m(j) // const
          !hwb_m(j)%cn = hwb_m(j)%cn / const
          !hwb_m(j)%snopack = hwb_m(j)%snopack / const
          !hwb_m(j)%sw = hwb_m(j)%sw / const
          !hwb_m(j)%sw_300 = hwb_m(j)%sw_300 / const
          
          hpathb_y(j)%path(ipaths) = hpathb_y(j)%path(ipaths) + hpathb_m(j)%path(ipaths)

          !! monthly print  (channel_path_mon.txt/csv no opening for file)
           if (pco%wb_hru%m == "y") then
             write (2781,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpathb_m(j)%path(ipaths)
               if (pco%csvout == "y") then
                 write (2785,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                   hpathb_m(j)%path(ipaths)
               end if
           end if
          
          hpathb_m(j)%path(ipaths) = pathbz
        end if
        
        !! check end of year
        if (time%end_yr == 1) then
          hpathb_y(j)%path(ipaths) = hpathb_y(j)%path(ipaths) // 12.
          !hwb_y(j) = hwb_y(j) // 12.
          !hwb_y(j)%cn = hwb_y(j)%cn / 12.
          !hwb_y(j)%snopack = hwb_y(j)%snopack / 12.
          !hwb_y(j)%sw = hwb_y(j)%sw / 12.
          !hwb_y(j)%sw_300 = hwb_y(j)%sw_300 / 12.
          hpathb_a(j)%path(ipaths) = hpathb_a(j)%path(ipaths) + hpathb_y(j)%path(ipaths)

          !! yearly print   (channel_path_yr.txt/csv no opening for file)
           if (time%end_yr == 1 .and. pco%wb_hru%y == "y") then
             write (2782,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpathb_y(j)%path(ipaths)
               if (pco%csvout == "y") then
                 write (2786,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                    hpathb_y(j)%path(ipaths)
               end if
           end if
          
        end if
        
!!!!! average annual print  (channel_path_aa.txt/csv no opening for file)
         if (time%end_sim == 1 .and. pco%wb_hru%a == "y") then
           hpathb_a(j)%path(ipaths) = hpathb_a(j)%path(ipaths) / time%yrs_prt
           write (2783,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpathb_a(j)%path(ipaths)
           if (pco%csvout == "y") then
             write (2787,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpathb_a(j)%path(ipaths)
           end if
           hpathb_a(j)%path(ipaths) = pathbz
         end if

      end do    !pathicide loop
      return
      
100   format (4i6,2i8,2x,a,28f12.3)
101   format (4i6,2i8,2x,a,20f12.3)
102   format (4i6,2i8,2x,a,20f12.3)
103   format (2i6,i8,4x,a,5x,f12.3)
104   format (4i6,2i8,2x,a,27f18.3)
       
      end subroutine ch_pathogen_output