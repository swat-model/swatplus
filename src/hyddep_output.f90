      subroutine hyddep_output
    
      use hydrograph_module
      use time_module
      use basin_module
      
      implicit none

             
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs hyd variables on daily, monthly and annual time steps
      
      !!  0 = average annual (always print)
      !!  1 = yearly
      !!  2 = monthly
      !!  3 = daily  

!!!!! daily print
       if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
        if (pco%hyd%d == "y") then
            write (2700,*) time%day, time%mo, time%day_mo, time%yrc, ob(icmd)%name, ob(icmd)%typ, ht1
          if (pco%csvout == "y") then
            write (2704,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, ob(icmd)%name, ob(icmd)%typ, ht1
          end if 
        endif
      end if
                                                    
      ob(icmd)%hdep_m = ob(icmd)%hdep_m + ht1

!!!!! monthly print
      if (time%end_mo == 1) then
        if (pco%hyd%m == "y") then
            write (2701,*) time%day, time%mo, time%day_mo, time%yrc, ob(icmd)%name, ob(icmd)%typ,     &
              ob(icmd)%hdep_m
          if (pco%csvout == "y") then
            write (2705,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, ob(icmd)%name, ob(icmd)%typ,     &
              ob(icmd)%hdep_m
          end if
        end if
          ob(icmd)%hdep_y = ob(icmd)%hdep_y + ob(icmd)%hdep_m
          ob(icmd)%hdep_m = hz
      endif
        
!!!!! yearly print
      if (time%end_yr == 1) then
        if (pco%hyd%y == "y") then
            write (2702,*) time%day, time%mo, time%day_mo, time%yrc, ob(icmd)%name, ob(icmd)%typ,     &
              ob(icmd)%hdep_y
 !                         ob(icmd)%hin_y
          if (pco%csvout == "y") then
            write (2706,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, ob(icmd)%name, ob(icmd)%typ,     &
             ob(icmd)%hdep_y
!                          ob(icmd)%hin_y
          end if 
        end if
          ob(icmd)%hdep_a = ob(icmd)%hdep_a + ob(icmd)%hdep_y
          ob(icmd)%hdep_y = hz
      endif
        
!!!!! average annual print
        if (time%end_sim == 1 .and. pco%hyd%a == "y") then
          ob(icmd)%hdep_a = ob(icmd)%hdep_a / time%yrs_prt
          write (2703,*) time%day, time%mo, time%day_mo, time%yrc,   ob(icmd)%name,      &
             ob(icmd)%typ, ob(icmd)%hdep_a
           if (pco%csvout == "y") then
             write (2707,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, ob(icmd)%name,      &
              ob(icmd)%typ, ob(icmd)%hdep_a
           end if 
        end if
        
      return
!100   format (4i12,a8,i8,a13,30(1x,e11.4))
!101   format (4i12,a8,i8,a13,30(1x,e11.4))
       
      end subroutine hyddep_output