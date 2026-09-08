      subroutine basin_sedbud_output

      use time_module
      use basin_module
      
      implicit none
      
      integer :: iuse

        !! sum monthly variables
        bsn_sedbudm = bsn_sedbudm + bsn_sedbud
        
        !! daily print
        if (pco%water_allo%d == "y") then
          write (3118,*) time%day, time%mo, time%day_mo, time%yrc, bsn_sedbud

          if (pco%csvout == "y") then
          write (3122,'(*(G0.6,:","))') time%mo, time%day_mo, time%yrc, bsn_sedbud
          end if
        end if
       
        !! zero daily
        bsn_sedbud = bsn_sedbudz

        !! monthly print
        if (time%end_mo == 1) then
          !! sum amount of yearly used water
          bsn_sedbudy = bsn_sedbudy + bsn_sedbudm

          if (pco%water_allo%m == "y") then
          write (3119,*) time%mo, time%day_mo, time%yrc, bsn_sedbudm
 
          if (pco%csvout == "y") then
          write (3123,'(*(G0.6,:","))') time%mo, time%day_mo, time%yrc, bsn_sedbudm
          end if
          end if

          !! zero monthly
          bsn_sedbudm = bsn_sedbudz

        end if

      !! yearly print
      if (time%end_yr == 1) then
        !! sum amount of yearly used water
        bsn_sedbudy =  bsn_sedbudy + bsn_sedbudm
          
        if (pco%water_allo%y == "y") then
          write (3120,*) time%mo, time%day_mo, time%yrc, bsn_sedbudy
  
              if (pco%csvout == "y") then
          write (3124,'(*(G0.6,:","))') time%mo, time%day_mo, time%yrc, bsn_sedbudy
          end if
        end if

        !! zero yearly
        bsn_sedbudy = bsn_sedbudz

      end if

      !! average annual print
      if (time%end_sim == 1) then
        !! sum amount of average annual used water
        bsn_sedbuda = bsn_sedbuda / time%yrs_prt

        if (pco%water_allo%a == "y") then
        write (3121,*) time%mo, time%day_mo, time%yrc, bsn_sedbuda

        if (pco%csvout == "y") then
        write (3125,'(*(G0.6,:","))') time%mo, time%day_mo, time%yrc, bsn_sedbuda
        end if
       end if
      end if

      return
      
100   format (4i6,i8,5x,a,5x,i8,5x,i8,5x,a,5x,i8,20(7x,a,5x,i8,3f15.1))
      end subroutine basin_sedbud_output