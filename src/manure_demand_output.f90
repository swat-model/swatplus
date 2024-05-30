      subroutine manure_demand_output (imallo)

      use time_module
      use hydrograph_module
      use manure_allocation_module
      
      implicit none
      
      integer, intent (in) :: imallo        !             |
      integer :: idmd
      integer :: isrc

      !! loop through and print each demand object
      do idmd = 1, mallo(imallo)%dmd_obs
        !! sum output (demand, withdrawals, and unmet) for each source
        do isrc = 1, mallo(imallo)%src_obs
          mallo(imallo)%dmd(idmd)%withdr_m(isrc) = mallo(imallo)%dmd(idmd)%withdr_m(isrc) +     &
                                                mallo(imallo)%dmd(idmd)%withdr(isrc)
        end do
      
!!!!! daily print
        if (pco%water_allo%d == "y") then
          write (3210,100) time%day, time%mo, time%day_mo, time%yrc, idmd, mallo(imallo)%dmd(idmd)%ob_typ,      &
              mallo(imallo)%dmd(idmd)%ob_num, (mallo(imallo)%src(isrc)%num, mallo(imallo)%src(isrc)%mois_typ,   &
              mallo(imallo)%src(isrc)%manure_typ, mallo(imallo)%dmd(idmd)%withdr(isrc),                         &
              isrc = 1, mallo(imallo)%src_obs)  

           if (pco%csvout == "y") then
          write (3211,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, idmd, mallo(imallo)%dmd(idmd)%ob_typ, &
              mallo(imallo)%dmd(idmd)%ob_num, (mallo(imallo)%src(isrc)%num, mallo(imallo)%src(isrc)%mois_typ,   &
              mallo(imallo)%src(isrc)%manure_typ, mallo(imallo)%dmd(idmd)%withdr(isrc),                         &
              isrc = 1, mallo(imallo)%src_obs)  
           end if
        end if

       !! zero daily values
       do isrc = 1, mallo(imallo)%src_obs
          mallo(imallo)%dmd(idmd)%withdr(isrc) = 0.
       end do

!!!!! monthly print
        if (time%end_mo == 1) then
          !! sum output (demand, withdrawals, and unmet) for each source
          do isrc = 1, mallo(imallo)%src_obs
            mallo(imallo)%dmd(idmd)%withdr_y(isrc) = mallo(imallo)%dmd(idmd)%withdr_y(isrc) +     &
                                                mallo(imallo)%dmd(idmd)%withdr_m(isrc)
          end do
          
          if (pco%water_allo%m == "y") then
          write (3212,100) time%day, time%mo, time%day_mo, time%yrc, idmd, mallo(imallo)%dmd(idmd)%ob_typ,      &
              mallo(imallo)%dmd(idmd)%ob_num, (mallo(imallo)%src(isrc)%num, mallo(imallo)%src(isrc)%mois_typ,   &
              mallo(imallo)%src(isrc)%manure_typ, mallo(imallo)%dmd(idmd)%withdr_m(isrc),                       &
              isrc = 1, mallo(imallo)%src_obs) 
 
              if (pco%csvout == "y") then
          write (3213,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, idmd, mallo(imallo)%dmd(idmd)%ob_typ, &
              mallo(imallo)%dmd(idmd)%ob_num, (mallo(imallo)%src(isrc)%num, mallo(imallo)%src(isrc)%mois_typ,   &
              mallo(imallo)%src(isrc)%manure_typ, mallo(imallo)%dmd(idmd)%withdr_m(isrc),                       &
              isrc = 1, mallo(imallo)%src_obs)  
          end if
        end if
        
       do isrc = 1,  mallo(imallo)%src_obs
            mallo(imallo)%dmd(idmd)%withdr_m(isrc) = 0.
       end do

        end if

!!!!! yearly print
      if (time%end_yr == 1) then
        !! sum output (demand, withdrawals, and unmet) for each source
        do isrc = 1, mallo(imallo)%src_obs
            mallo(imallo)%dmd(idmd)%withdr_a(isrc) = mallo(imallo)%dmd(idmd)%withdr_a(isrc) +     &
                                                mallo(imallo)%dmd(idmd)%withdr_y(isrc)
        end do
          
        if (pco%water_allo%y == "y") then
          write (3214,100) time%day, time%mo, time%day_mo, time%yrc, idmd, mallo(imallo)%dmd(idmd)%ob_typ,      &
              mallo(imallo)%dmd(idmd)%ob_num, (mallo(imallo)%src(isrc)%num, mallo(imallo)%src(isrc)%mois_typ,   &
              mallo(imallo)%src(isrc)%manure_typ, mallo(imallo)%dmd(idmd)%withdr_y(isrc),                       &
              isrc = 1, mallo(imallo)%src_obs) 
  
              if (pco%csvout == "y") then
          write (3215,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, idmd, mallo(imallo)%dmd(idmd)%ob_typ, &
              mallo(imallo)%dmd(idmd)%ob_num, (mallo(imallo)%src(isrc)%num, mallo(imallo)%src(isrc)%mois_typ,   &
              mallo(imallo)%src(isrc)%manure_typ, mallo(imallo)%dmd(idmd)%withdr_y(isrc),                       &
              isrc = 1, mallo(imallo)%src_obs)  
          end if
        end if
        
       do isrc = 1, mallo(imallo)%src_obs
            mallo(imallo)%dmd(idmd)%withdr_y(isrc) = 0.
       end do

      end if

!!!!! average annual print
      if (time%end_sim == 1) then
        !! sum output (demand, withdrawals, and unmet) for each source
        do isrc = 1, mallo(imallo)%src_obs
          mallo(imallo)%dmd(idmd)%withdr_a(isrc) = mallo(imallo)%dmd(idmd)%withdr_a(isrc) / time%yrs_prt
        end do
        
        if (pco%water_allo%a == "y") then
        write (3216,100) time%day, time%mo, time%day_mo, time%yrc, idmd, mallo(imallo)%dmd(idmd)%ob_typ,      &
              mallo(imallo)%dmd(idmd)%ob_num, (mallo(imallo)%src(isrc)%num, mallo(imallo)%src(isrc)%mois_typ,   &
              mallo(imallo)%src(isrc)%manure_typ, mallo(imallo)%dmd(idmd)%withdr_a(isrc),                       &
              isrc = 1, mallo(imallo)%src_obs) 

        if (pco%csvout == "y") then
        write (3217,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, idmd, mallo(imallo)%dmd(idmd)%ob_typ, &
              mallo(imallo)%dmd(idmd)%ob_num, (mallo(imallo)%src(isrc)%num, mallo(imallo)%src(isrc)%mois_typ,   &
              mallo(imallo)%src(isrc)%manure_typ, mallo(imallo)%dmd(idmd)%withdr_a(isrc),                       &
              isrc = 1, mallo(imallo)%src_obs)  
        end if
       end if
      end if 
      
      end do    ! do idmd = 1, mallo(imallo)%dmd_obs
      
      return
      
100   format (4i6,i8,5x,a,5x,i8,5x,i8,3(7x,a,5x,i8,3f15.1))   
      end subroutine manure_demand_output