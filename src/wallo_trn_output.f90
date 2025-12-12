      subroutine wallo_trn_output (iwallo)

      use time_module
      use hydrograph_module
      use water_allocation_module
      
      implicit none
      
      integer, intent (in) :: iwallo        !             |
      integer :: itrn = 0
      integer :: isrc = 0

      !! loop through and print each demand object
      do itrn = 1, wallo(iwallo)%trn_obs
      
        !! sum output (demand, withdrawals, and unmet) for each source
        do isrc = 1, wallo(iwallo)%trn(itrn)%src_num
          wal_omm(iwallo)%trn(itrn)%src(isrc)%hd = wal_omd(iwallo)%trn(itrn)%src(isrc)%hd +         &
                                                          wal_omm(iwallo)%trn(itrn)%src(isrc)%hd
        end do
      
!!!!! daily print
        if (pco%water_allo%d == "y") then
          write (3110,*) time%day, time%mo, time%day_mo, time%yrc, itrn, wallo(iwallo)%trn(itrn)%trn_typ,          &
              wallo(iwallo)%trn(itrn)%num, wallo(iwallo)%trn(itrn)%num, wallo(iwallo)%trn(itrn)%num,     &
              (wallo(iwallo)%trn(itrn)%src(isrc)%typ, wallo(iwallo)%trn(itrn)%src(isrc)%num,                &
              wal_omd(iwallo)%trn(itrn)%src(isrc)%hd, isrc = 1, wallo(iwallo)%trn(itrn)%src_num)

           if (pco%csvout == "y") then
          write (3114,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, itrn, wallo(iwallo)%trn(itrn)%trn_typ,   &
              wallo(iwallo)%trn(itrn)%num, wallo(iwallo)%trn(itrn)%num, wallo(iwallo)%trn(itrn)%num,           &
              (wallo(iwallo)%trn(itrn)%src(isrc)%typ, wallo(iwallo)%trn(itrn)%src(isrc)%num,                      &
              wal_omd(iwallo)%trn(itrn)%src(isrc)%hd, isrc = 1, wallo(iwallo)%trn(itrn)%src_num)
           end if
        end if
       
       do isrc = 1, wallo(iwallo)%trn(itrn)%src_num
          wal_omd(iwallo)%trn(itrn)%src(isrc)%hd = hz
       end do

!!!!! monthly print
        if (time%end_mo == 1) then
          !! sum output (demand, withdrawals, and unmet) for each source
          do isrc = 1, wallo(iwallo)%trn(itrn)%src_num
            wal_omy(iwallo)%trn(itrn)%src(isrc)%hd = wal_omy(iwallo)%trn(itrn)%src(isrc)%hd +         &
                                                          wal_omm(iwallo)%trn(itrn)%src(isrc)%hd
          end do

          if (pco%water_allo%m == "y") then
          write (3111,*) time%day, time%mo, time%day_mo, time%yrc, itrn, wallo(iwallo)%trn(itrn)%trn_typ,          &
              wallo(iwallo)%trn(itrn)%num, wallo(iwallo)%trn(itrn)%num, wallo(iwallo)%trn(itrn)%num,     &
              (wallo(iwallo)%trn(itrn)%src(isrc)%typ, wallo(iwallo)%trn(itrn)%src(isrc)%num,                &
              wal_omm(iwallo)%trn(itrn)%src(isrc)%hd, isrc = 1, wallo(iwallo)%trn(itrn)%src_num)
 
              if (pco%csvout == "y") then
          write (3115,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, itrn, wallo(iwallo)%trn(itrn)%trn_typ, &
              wallo(iwallo)%trn(itrn)%num, wallo(iwallo)%trn(itrn)%num, wallo(iwallo)%trn(itrn)%num,         &
              (wallo(iwallo)%trn(itrn)%src(isrc)%typ, wallo(iwallo)%trn(itrn)%src(isrc)%num,                    &
              wal_omm(iwallo)%trn(itrn)%src(isrc)%hd, isrc = 1, wallo(iwallo)%trn(itrn)%src_num)
          end if
        end if

        do isrc = 1, wallo(iwallo)%trn(itrn)%src_num
          wal_omm(iwallo)%trn(itrn)%src(isrc)%hd = hz
        end do

      end if

!!!!! yearly print
      if (time%end_yr == 1) then
        !! sum output (demand, withdrawals, and unmet) for each source
        do isrc = 1, wallo(iwallo)%trn(itrn)%src_num
          wal_oma(iwallo)%trn(itrn)%src(isrc)%hd = wal_oma(iwallo)%trn(itrn)%src(isrc)%hd +         &
                                                          wal_omy(iwallo)%trn(itrn)%src(isrc)%hd
        end do
          
        if (pco%water_allo%y == "y") then
          write (3112,*) time%day, time%mo, time%day_mo, time%yrc, itrn, wallo(iwallo)%trn(itrn)%trn_typ,          &
              wallo(iwallo)%trn(itrn)%num, wallo(iwallo)%trn(itrn)%num, wallo(iwallo)%trn(itrn)%num,     &
              (wallo(iwallo)%trn(itrn)%src(isrc)%typ, wallo(iwallo)%trn(itrn)%src(isrc)%num,                &
              wal_omy(iwallo)%trn(itrn)%src(isrc)%hd, isrc = 1, wallo(iwallo)%trn(itrn)%src_num)
  
              if (pco%csvout == "y") then
          write (3116,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, itrn, wallo(iwallo)%trn(itrn)%trn_typ, &
              wallo(iwallo)%trn(itrn)%num, wallo(iwallo)%trn(itrn)%num, wallo(iwallo)%trn(itrn)%num,         &
              (wallo(iwallo)%trn(itrn)%src(isrc)%typ, wallo(iwallo)%trn(itrn)%src(isrc)%num,                    &
              wal_omy(iwallo)%trn(itrn)%src(isrc)%hd, isrc = 1, wallo(iwallo)%trn(itrn)%src_num)
          end if
        end if

        do isrc = 1, wallo(iwallo)%trn(itrn)%src_num
          wal_omy(iwallo)%trn(itrn)%src(isrc)%hd = hz
        end do

      end if

!!!!! average annual print
      if (time%end_sim == 1) then
        !! sum output (demand, withdrawals, and unmet) for each source
        do isrc = 1, wallo(iwallo)%trn(itrn)%src_num
          wal_oma(iwallo)%trn(itrn)%src(isrc)%hd = wal_oma(iwallo)%trn(itrn)%src(isrc)%hd / time%yrs_prt
        end do

        if (pco%water_allo%a == "y") then
        write (3113,*) time%day, time%mo, time%day_mo, time%yrc, itrn, wallo(iwallo)%trn(itrn)%trn_typ,            &
              wallo(iwallo)%trn(itrn)%num, wallo(iwallo)%trn(itrn)%num, wallo(iwallo)%trn(itrn)%num,     &
              (wallo(iwallo)%trn(itrn)%src(isrc)%typ, wallo(iwallo)%trn(itrn)%src(isrc)%num,                &
              wal_oma(iwallo)%trn(itrn)%src(isrc)%hd, isrc = 1, wallo(iwallo)%trn(itrn)%src_num)

        if (pco%csvout == "y") then
        write (3117,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, itrn, wallo(iwallo)%trn(itrn)%trn_typ,   &
              wallo(iwallo)%trn(itrn)%num, wallo(iwallo)%trn(itrn)%num, wallo(iwallo)%trn(itrn)%num,         &
              (wallo(iwallo)%trn(itrn)%src(isrc)%typ, wallo(iwallo)%trn(itrn)%src(isrc)%num,                    &
              wal_oma(iwallo)%trn(itrn)%src(isrc)%hd, isrc = 1, wallo(iwallo)%trn(itrn)%src_num)
        end if
       end if
      end if

      end do    ! do itrn = 1, wallo(iwallo)%trn_obs
      
      return
      
100   format (4i6,i8,5x,a,5x,i8,5x,i8,5x,a,5x,i8,20(7x,a,5x,i8,3f15.1))
    end subroutine wallo_trn_output