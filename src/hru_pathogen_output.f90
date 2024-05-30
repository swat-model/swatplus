      subroutine hru_pathogen_output(ihru)
    
      use output_ls_pathogen_module
      use plant_module
      use plant_data_module
      use time_module
      use basin_module
      use output_landscape_module
      use constituent_mass_module
      use hydrograph_module, only : sp_ob1, ob
      
      implicit none
      
      integer, intent (in) :: ihru             !            |
      integer :: ipath                         !            |
      integer :: j
      integer :: iob
      real :: const
                         
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs HRU variables on daily, monthly and annual time steps

      j = ihru
      
      iob = sp_ob1%hru + j - 1
          
      !! print balance for each pathogen
      do ipath = 1, cs_db%num_paths
          
      hpathb_m(j)%path(ipath) = hpathb_m(j)%path(ipath) + hpath_bal(j)%path(ipath)

      !! daily print
        if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
          if (pco%wb_hru%d == "y") then
             write (2790,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpath_bal(j)%path(ipath)   !! pathogen balance
             if (pco%csvout == "y") then
               write (2794,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                 hpath_bal(j)%path(ipath)
             end if
          end if
        end if
        !! check end of month
        if (time%end_mo == 1) then
          const = float (ndays(time%mo + 1) - ndays(time%mo))
          hpathb_m(j)%path(ipath) = hpathb_m(j)%path(ipath) // const
          !hwb_m(j) = hwb_m(j) // const
          !hwb_m(j)%cn = hwb_m(j)%cn / const
          !hwb_m(j)%snopack = hwb_m(j)%snopack / const
          !hwb_m(j)%sw = hwb_m(j)%sw / const
          !hwb_m(j)%sw_300 = hwb_m(j)%sw_300 / const
          
          hpathb_y(j)%path(ipath) = hpathb_y(j)%path(ipath) + hpathb_m(j)%path(ipath)

          !! monthly print
           if (pco%wb_hru%m == "y") then
             write (2791,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpathb_m(j)%path(ipath)
               if (pco%csvout == "y") then
                 write (2795,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                   hpathb_m(j)%path(ipath)
               end if
           end if
          
          hpathb_m(j)%path(ipath) = pathbz
        end if
        
        !! check end of year
        if (time%end_yr == 1) then
          hpathb_y(j)%path(ipath) = hpathb_y(j)%path(ipath) // 12.
          !hwb_y(j) = hwb_y(j) // 12.
          !hwb_y(j)%cn = hwb_y(j)%cn / 12.
          !hwb_y(j)%snopack = hwb_y(j)%snopack / 12.
          !hwb_y(j)%sw = hwb_y(j)%sw / 12.
          !hwb_y(j)%sw_300 = hwb_y(j)%sw_300 / 12.
          hpathb_a(j)%path(ipath) = hpathb_a(j)%path(ipath) + hpathb_y(j)%path(ipath)

          !! yearly print
           if (time%end_yr == 1 .and. pco%wb_hru%y == "y") then
             write (2792,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpathb_y(j)%path(ipath)
               if (pco%csvout == "y") then
                 write (2796,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                   hpathb_y(j)%path(ipath)
               end if
           end if
          
        end if
        
!!!!! average annual print
         if (time%end_sim == 1 .and. pco%wb_hru%a == "y") then
           hpathb_a(j)%path(ipath) = hpathb_a(j)%path(ipath) / time%yrs_prt
           write (2793,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpathb_a(j)%path(ipath)
           if (pco%csvout == "y") then
             write (2797,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpathb_a(j)%path(ipath)
           end if
           hpathb_a(j)%path(ipath) = pathbz
         end if

      end do    !pathogen loop
      return
      
100   format (4i6,2i8,2x,a,11e12.3)
101   format (4i6,2i8,2x,a,11e12.3)
102   format (4i6,2i8,2x,a,11e12.3)
103   format (2i6,i8,4x,a,5x,11e12.3)
       
      end subroutine hru_pathogen_output