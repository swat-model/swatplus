      subroutine recall_output (irec)
      
      use time_module
      use basin_module
      use hydrograph_module
      
      implicit none
      
      integer, intent (in) :: irec            !          |     
      integer :: iob                          !          |
   
      iob = sp_ob1%recall + irec - 1   !!!!!! added for new output write
             
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs SUBBASIN variables on daily, monthly and annual time steps
     
        !! sum monthly variables
        rec_m(irec) = rec_m(irec) + rec_d(irec)
        
        !! daily print - RECALL
         if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
          if (pco%recall%d == "y") then
            write (4600,*) time%day, time%mo, time%day_mo, time%yrc, ob(iob)%name, ob(iob)%typ, rec_d(irec)
            if (pco%csvout == "y") then
              write (4604,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, ob(iob)%name, ob(iob)%typ, rec_d(irec)
            end if
          end if
        end if

        !! monthly print - RECALL
        if (time%end_mo == 1) then
          rec_y(irec) = rec_y(irec) + rec_m(irec)
          if (pco%recall%m == "y") then
            write (4601,*) time%day, time%mo, time%day_mo, time%yrc, ob(iob)%name, ob(iob)%typ, rec_m(irec)
            if (pco%csvout == "y") then
              write (4605,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, ob(iob)%name, ob(iob)%typ, rec_m(irec)
            endif
          end if
          rec_m(irec) = hz
        end if

        !! yearly print - RECALL
        if (time%end_yr == 1) then
          rec_a(irec) = rec_a(irec) + rec_y(irec)
          if (pco%recall%y == "y") then
            write (4602,*) time%day, time%mo, time%day_mo, time%yrc, ob(iob)%name, ob(iob)%typ, rec_y(irec)
            if (pco%csvout == "y") then
              write (4606,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, ob(iob)%name, ob(iob)%typ, rec_y(irec) 
            end if
          end if
          !! zero yearly variables        
          rec_y(irec) = hz
        end if
        
      !! average annual print - RECALL
          if (time%end_sim == 1 .and. pco%recall%a == "y") then
          rec_a(irec) = rec_a(irec) / time%yrs_prt
            write (4603,*) time%day, time%mo, time%day_mo, time%yrc, ob(iob)%name, ob(iob)%typ, rec_a(irec)
            if (pco%csvout == "y") then 
              write (4607,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, ob(iob)%name, ob(iob)%typ, rec_a(irec)  
            end if 
          end if

      return
       
      end subroutine recall_output