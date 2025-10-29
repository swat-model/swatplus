      subroutine wallo_allo_output (iwallo)

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
          wallom_out(iwallo)%trn(itrn)%src(isrc) = wallom_out(iwallo)%trn(itrn)%src(isrc) +         &
                                                          wallod_out(iwallo)%trn(itrn)%src(isrc)
        end do
      
!!!!! daily print
        if (pco%water_allo%d == "y") then
          write (3110,*) time%day, time%mo, time%day_mo, time%yrc, itrn, wallo(iwallo)%trn(itrn)%trn_typ,          &
              wallo(iwallo)%trn(itrn)%num, wallo(iwallo)%trn(itrn)%num, wallo(iwallo)%trn(itrn)%num,     &
              (wallo(iwallo)%trn(itrn)%src(isrc)%typ, wallo(iwallo)%trn(itrn)%src(isrc)%num,                &
              wallod_out(iwallo)%trn(itrn)%src(isrc), isrc = 1, wallo(iwallo)%trn(itrn)%src_num)

           if (pco%csvout == "y") then
          write (3114,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, itrn, wallo(iwallo)%trn(itrn)%trn_typ,   &
              wallo(iwallo)%trn(itrn)%num, wallo(iwallo)%trn(itrn)%num, wallo(iwallo)%trn(itrn)%num,           &
              (wallo(iwallo)%trn(itrn)%src(isrc)%typ, wallo(iwallo)%trn(itrn)%src(isrc)%num,                      &
              wallod_out(iwallo)%trn(itrn)%src(isrc), isrc = 1, wallo(iwallo)%trn(itrn)%src_num)
           end if
        end if
       
       do isrc = 1, wallo(iwallo)%trn(itrn)%src_num
          wallod_out(iwallo)%trn(itrn)%src(isrc) = walloz
       end do

!!!!! monthly print
        if (time%end_mo == 1) then
          !! sum output (demand, withdrawals, and unmet) for each source
          do isrc = 1, wallo(iwallo)%trn(itrn)%src_num
            walloy_out(iwallo)%trn(itrn)%src(isrc) = walloy_out(iwallo)%trn(itrn)%src(isrc) +         &
                                                          wallom_out(iwallo)%trn(itrn)%src(isrc)
          end do

          if (pco%water_allo%m == "y") then
          write (3111,*) time%day, time%mo, time%day_mo, time%yrc, itrn, wallo(iwallo)%trn(itrn)%trn_typ,          &
              wallo(iwallo)%trn(itrn)%num, wallo(iwallo)%trn(itrn)%num, wallo(iwallo)%trn(itrn)%num,     &
              (wallo(iwallo)%trn(itrn)%src(isrc)%typ, wallo(iwallo)%trn(itrn)%src(isrc)%num,                &
              wallom_out(iwallo)%trn(itrn)%src(isrc), isrc = 1, wallo(iwallo)%trn(itrn)%src_num)
 
              if (pco%csvout == "y") then
          write (3115,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, itrn, wallo(iwallo)%trn(itrn)%trn_typ, &
              wallo(iwallo)%trn(itrn)%num, wallo(iwallo)%trn(itrn)%num, wallo(iwallo)%trn(itrn)%num,         &
              (wallo(iwallo)%trn(itrn)%src(isrc)%typ, wallo(iwallo)%trn(itrn)%src(isrc)%num,                    &
              wallom_out(iwallo)%trn(itrn)%src(isrc), isrc = 1, wallo(iwallo)%trn(itrn)%src_num)
          end if
        end if

        do isrc = 1, wallo(iwallo)%trn(itrn)%src_num
          wallom_out(iwallo)%trn(itrn)%src(isrc) = walloz
        end do

      end if

!!!!! yearly print
      if (time%end_yr == 1) then
        !! sum output (demand, withdrawals, and unmet) for each source
        do isrc = 1, wallo(iwallo)%trn(itrn)%src_num
          walloa_out(iwallo)%trn(itrn)%src(isrc) = walloa_out(iwallo)%trn(itrn)%src(isrc) +         &
                                                          walloy_out(iwallo)%trn(itrn)%src(isrc)
        end do
          
        if (pco%water_allo%y == "y") then
          write (3112,*) time%day, time%mo, time%day_mo, time%yrc, itrn, wallo(iwallo)%trn(itrn)%trn_typ,          &
              wallo(iwallo)%trn(itrn)%num, wallo(iwallo)%trn(itrn)%num, wallo(iwallo)%trn(itrn)%num,     &
              (wallo(iwallo)%trn(itrn)%src(isrc)%typ, wallo(iwallo)%trn(itrn)%src(isrc)%num,                &
              walloy_out(iwallo)%trn(itrn)%src(isrc), isrc = 1, wallo(iwallo)%trn(itrn)%src_num)
  
              if (pco%csvout == "y") then
          write (3116,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, itrn, wallo(iwallo)%trn(itrn)%trn_typ, &
              wallo(iwallo)%trn(itrn)%num, wallo(iwallo)%trn(itrn)%num, wallo(iwallo)%trn(itrn)%num,         &
              (wallo(iwallo)%trn(itrn)%src(isrc)%typ, wallo(iwallo)%trn(itrn)%src(isrc)%num,                    &
              walloy_out(iwallo)%trn(itrn)%src(isrc), isrc = 1, wallo(iwallo)%trn(itrn)%src_num)
          end if
        end if

        do isrc = 1, wallo(iwallo)%trn(itrn)%src_num
          walloy_out(iwallo)%trn(itrn)%src(isrc) = walloz
        end do

      end if

!!!!! average annual print
      if (time%end_sim == 1) then
        !! sum output (demand, withdrawals, and unmet) for each source
        do isrc = 1, wallo(iwallo)%trn(itrn)%src_num
          walloa_out(iwallo)%trn(itrn)%src(isrc) = walloa_out(iwallo)%trn(itrn)%src(isrc) / time%yrs_prt
        end do

        if (pco%water_allo%a == "y") then
        write (3113,*) time%day, time%mo, time%day_mo, time%yrc, itrn, wallo(iwallo)%trn(itrn)%trn_typ,            &
              wallo(iwallo)%trn(itrn)%num, wallo(iwallo)%trn(itrn)%num, wallo(iwallo)%trn(itrn)%num,     &
              (wallo(iwallo)%trn(itrn)%src(isrc)%typ, wallo(iwallo)%trn(itrn)%src(isrc)%num,                &
              walloa_out(iwallo)%trn(itrn)%src(isrc), isrc = 1, wallo(iwallo)%trn(itrn)%src_num)

        if (pco%csvout == "y") then
        write (3117,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, itrn, wallo(iwallo)%trn(itrn)%trn_typ,   &
              wallo(iwallo)%trn(itrn)%num, wallo(iwallo)%trn(itrn)%num, wallo(iwallo)%trn(itrn)%num,         &
              (wallo(iwallo)%trn(itrn)%src(isrc)%typ, wallo(iwallo)%trn(itrn)%src(isrc)%num,                    &
              walloa_out(iwallo)%trn(itrn)%src(isrc), isrc = 1, wallo(iwallo)%trn(itrn)%src_num)
        end if
       end if
      end if

      end do    ! do itrn = 1, wallo(iwallo)%trn_obs
      
      return
      
100   format (4i6,i8,5x,a,5x,i8,5x,i8,5x,a,5x,i8,20(7x,a,5x,i8,3f15.1))
      end subroutine wallo_allo_output
    
    
    
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