    subroutine cli_staread 

    use input_file_module
    use maximum_data_module 
    use climate_module
    use time_module
    use hydrograph_module

    implicit none
          
    character (len=80) :: titldum   !           |title of file
    character (len=80) :: header    !           |header of file
    integer :: eof                  !           |end of file
    integer :: imax                 !none       |determine max number for array (imax) and total number in file
    integer :: iwgn                 !           |
    logical :: i_exist              !none       |check to determine if file exists
    !integer :: iwst                 !none       |counter
    integer :: i                    !none       |counter
    
    eof = 0
    imax = 0

    inquire (file=in_cli%weat_sta, exist=i_exist)
    if (.not. i_exist .or. in_cli%weat_sta == "null") then
        allocate (wst(0:1))
        allocate (wst_n(0:0))
    else
        do
            !! read weather stations data from weather.wst - gages and meas/gen
            open (107,file=in_cli%weat_sta)
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            read (107,*,iostat=eof) header
            if (eof < 0) exit
            !! determine max number for array (imax) and total number in file
            do while (eof == 0)
                read (107,*,iostat=eof) titldum
                if (eof < 0) exit
                imax = imax + 1
            end do

            db_mx%wst = imax
            
            allocate (wst(imax))
            allocate (wst_n(imax))
            do iwst = 1, db_mx%wst
              allocate (wst(iwst)%weat%ts(time%step))
              allocate (wst(iwst)%weat%ts_next(time%step))
              wst(iwst)%weat%precip_prior_day = "dry"
              allocate (wst(iwst)%tlag(w_temp%airlag_d))
              iwgn = wst(iwst)%wco%wgn
              wst(iwst)%tlag = (wgn(iwgn)%tmpmn(1) + wgn(iwgn)%tmpstdmx(1)) / 2.
            end do

            rewind (107)
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            read (107,*,iostat=eof) header
            if (eof < 0) exit
            do i = 1, db_mx%wst
                read (107,*,iostat=eof) titldum
                if (eof < 0) exit
                backspace (107)
                read (107,*,iostat=eof) wst(i)%name, wst(i)%wco_c
                if (eof < 0) exit
               wst_n(i) = wst(i)%name
               if (db_mx%wgnsta > 0) call search (wgn_n, db_mx%wgnsta, wst(i)%wco_c%wgn, wst(i)%wco%wgn)
               if (wst(i)%wco%wgn == 0 .and. wst(i)%wco_c%wgn /= "sim") write (9001,*) wst(i)%wco_c%wgn, "file not found (wgn)"
               if (db_mx%pcpfiles > 0) call search (pcp_n, db_mx%pcpfiles, wst(i)%wco_c%pgage, wst(i)%wco%pgage)
                 wst(i)%pcp_ts = pcp(wst(i)%wco%pgage)%tstep
               if (wst(i)%wco%pgage == 0 .and. wst(i)%wco_c%pgage /= "sim") write (9001,*) &
                    wst(i)%wco_c%pgage,"file not found (pgage)"
               if (db_mx%tmpfiles > 0) call search (tmp_n, db_mx%tmpfiles, wst(i)%wco_c%tgage, wst(i)%wco%tgage)
               if (wst(i)%wco%tgage == 0 .and. wst(i)%wco_c%tgage /= "sim") write (9001,*) &
                    wst(i)%wco_c%tgage, "file not found (tgage)"
               if (db_mx%slrfiles > 0) call search (slr_n, db_mx%slrfiles, wst(i)%wco_c%sgage, wst(i)%wco%sgage)
               if (wst(i)%wco%sgage == 0 .and. wst(i)%wco_c%sgage /= "sim") write (9001,*) &
                    wst(i)%wco_c%sgage, "file not found (sgage)"
               if (db_mx%rhfiles > 0) call search (hmd_n, db_mx%rhfiles, wst(i)%wco_c%hgage, wst(i)%wco%hgage) 
               if (wst(i)%wco%hgage == 0 .and. wst(i)%wco_c%hgage /= "sim") write (9001,*) &
                    wst(i)%wco_c%hgage, "file not found (hgage)"
               if (db_mx%wndfiles > 0) call search (wnd_n, db_mx%wndfiles, wst(i)%wco_c%wgage, wst(i)%wco%wgage)  
               if (wst(i)%wco%wgage == 0 .and. wst(i)%wco_c%wgage /= "sim" ) write (9001,*) &
                    wst(i)%wco_c%wgage, "file not found (wgage)"
               if (db_mx% petfiles > 0) call search (petm_n, db_mx%petfiles, wst(i)%wco_c%petgage, wst(i)%wco%petgage)  
               !if (wst(i)%wco%petgage == 0 .and. wst(i)%wco_c%petgage /= "sim" ) write (9001,*) &
               if (wst(i)%wco%petgage == 0 .and. wst(i)%wco_c%petgage /= "null" ) write (9001,*) &
                   wst(i)%wco_c%petgage, "file not found (petgage)"
               if (db_mx%atmodep > 0) call search (atmo_n, db_mx%atmodep, wst(i)%wco_c%atmodep, wst(i)%wco%atmodep)  
               if (wst(i)%wco%atmodep == 0 .and. wst(i)%wco_c%atmodep /= "null" ) write (9001,*) &
                    wst(i)%wco_c%atmodep, "file not found (atmodep)"
               
                if (eof < 0) exit  
            end do
            exit
        enddo
    endif
                      
    close (107) 

    return
    end subroutine cli_staread         