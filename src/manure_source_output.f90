      subroutine manure_source_output (imallo)

      use time_module
      use hydrograph_module
      use manure_allocation_module
      
      implicit none
      
      integer, intent (in) :: imallo        !             |
      integer :: idmd
      integer :: isrc

      !! loop through and print each demand object
      do isrc = 1, mallo(imallo)%src_obs
        !! sum output (stored, produced, and withdrawals for each source
        mallo(imallo)%src(isrc)%bal_m = mallo(imallo)%src(isrc)%bal_m + mallo(imallo)%src(isrc)%bal_d
      
!!!!! daily print
        if (pco%water_allo%d == "y") then   !!using region water balance print codes for now
          write (3200,100) time%day, time%mo, time%day_mo, time%yrc, idmd, mallo(imallo)%src(isrc)%num,                &
              mallo(imallo)%src(isrc)%mois_typ, mallo(imallo)%src(isrc)%manure_typ, mallo(imallo)%src(isrc)%bal_d  

           if (pco%csvout == "y") then
          write (3201,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, idmd, mallo(imallo)%src(isrc)%num,   &
              mallo(imallo)%src(isrc)%mois_typ, mallo(imallo)%src(isrc)%manure_typ, mallo(imallo)%src(isrc)%bal_d  

           end if
        end if

        mallo(imallo)%src(isrc)%bal_d = malloz

!!!!! monthly print
        if (time%end_mo == 1) then
          !! sum output (stored, produced, and withdrawals for each source
          mallo(imallo)%src(isrc)%bal_y = mallo(imallo)%src(isrc)%bal_y + mallo(imallo)%src(isrc)%bal_m
          
          if (pco%water_allo%m == "y") then
          write (3202,100) time%day, time%mo, time%day_mo, time%yrc, idmd, mallo(imallo)%src(isrc)%num,                &
              mallo(imallo)%src(isrc)%mois_typ, mallo(imallo)%src(isrc)%manure_typ, mallo(imallo)%src(isrc)%bal_m
 
              if (pco%csvout == "y") then
          write (3203,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, idmd, mallo(imallo)%src(isrc)%num,   &
              mallo(imallo)%src(isrc)%mois_typ, mallo(imallo)%src(isrc)%manure_typ, mallo(imallo)%src(isrc)%bal_m  

          end if
        end if
        
        mallo(imallo)%src(isrc)%bal_m = malloz

        end if

!!!!! yearly print
      if (time%end_yr == 1) then
          !! sum output (stored, produced, and withdrawals for each source
          mallo(imallo)%src(isrc)%bal_a = mallo(imallo)%src(isrc)%bal_a + mallo(imallo)%src(isrc)%bal_y
          
        if (pco%water_allo%y == "y") then
          write (3204,100) time%day, time%mo, time%day_mo, time%yrc, idmd, mallo(imallo)%src(isrc)%num,                &
              mallo(imallo)%src(isrc)%mois_typ, mallo(imallo)%src(isrc)%manure_typ, mallo(imallo)%src(isrc)%bal_y
  
              if (pco%csvout == "y") then
          write (3205,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, idmd, mallo(imallo)%src(isrc)%num,   &
              mallo(imallo)%src(isrc)%mois_typ, mallo(imallo)%src(isrc)%manure_typ, mallo(imallo)%src(isrc)%bal_y
          end if
        end if
        
        mallo(imallo)%src(isrc)%bal_y = malloz

      end if

!!!!! average annual print
      if (time%end_sim == 1) then
          !! sum output (stored, produced, and withdrawals for each source
          mallo(imallo)%src(isrc)%bal_a = mallo(imallo)%src(isrc)%bal_a / time%yrs_prt
          
        if (pco%water_allo%a == "y") then
        write (3206,100) time%day, time%mo, time%day_mo, time%yrc, idmd, mallo(imallo)%src(isrc)%num,                &
              mallo(imallo)%src(isrc)%mois_typ, mallo(imallo)%src(isrc)%manure_typ, mallo(imallo)%src(isrc)%bal_a

        if (pco%csvout == "y") then
        write (3207,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, idmd, mallo(imallo)%src(isrc)%num,   &
              mallo(imallo)%src(isrc)%mois_typ, mallo(imallo)%src(isrc)%manure_typ, mallo(imallo)%src(isrc)%bal_a
        end if
       end if
      end if 
      
      end do    ! do idmd = 1, mallo(imallo)%src_obs
      
      return
      
100   format (4i6,i8,5x,a,5x,i8,5x,i8,3(7x,a,5x,i8,3f15.1))   
      end subroutine manure_source_output