subroutine cli_staread

   use input_file_module
   use maximum_data_module
   use climate_module
   use time_module
   use hydrograph_module

   implicit none

   character (len=80) :: titldum = ""!           |title of file
   character (len=80) :: header = "" !           |header of file
   integer :: eof = 0              !           |end of file
   integer :: imax = 0             !none       |determine max number for array (imax) and total number in file
   integer :: iwgn = 0             !           |
   logical :: i_exist              !none       |check to determine if file exists
   !integer :: iwst                 !none       |counter
   integer :: i = 0                !none       |counter

   eof = 0
   imax = 0

   ! celray james edit
   ! if weat_sta == "netcdf.ncw" then print "netcdf.ncw is not supported, please use weather.wst" else use weather.wst
   if (in_cli%weat_sta == "netcdf.ncw") then
      print *, "reading netcdf.ncw"

      inquire (file=in_cli%weat_sta, exist=i_exist)
      if (.not. i_exist) then
         allocate (wst(0:1))
         allocate (wst_n(0:0))
      else
         do
            !! read weather stations data from netcdf.ncw file
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
               allocate (wst(iwst)%weat%ts(time%step), source = 0.)
               allocate (wst(iwst)%weat%ts_next(time%step), source = 0.)
               wst(iwst)%weat%precip_prior_day = "dry"
               allocate (wst(iwst)%tlag(w_temp%airlag_d), source = 0.)
               ! Initialize other character fields to empty
               wst(iwst)%wco_c%pgage = ""
               wst(iwst)%wco_c%tgage = ""
               wst(iwst)%wco_c%sgage = ""
               wst(iwst)%wco_c%hgage = ""
               wst(iwst)%wco_c%wgage = ""
               wst(iwst)%wco_c%petgage = ""
               wst(iwst)%wco_c%atmodep = ""
            end do

            rewind (107)
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            read (107,*,iostat=eof) header
            if (eof < 0) exit

            ! set files same as db_mx%wst if using NetCDF
            db_mx%pcpfiles = db_mx%wst
            db_mx%tmpfiles = db_mx%wst
            db_mx%slrfiles = db_mx%wst
            db_mx%rhfiles = db_mx%wst
            db_mx%wndfiles = db_mx%wst


            do i = 1, db_mx%wst
               read (107,*,iostat=eof) wst(i)%name, wst(i)%wco_c%wgn, wst(i)%lat, wst(i)%lon, wst(i)%elev, &
                  wst(i)%pcp_factor, wst(i)%tmin_factor, wst(i)%tmax_factor, &
                  wst(i)%slr_factor, wst(i)%hmd_factor, wst(i)%wnd_factor, &
                  wst(i)%wco_c%petgage, wst(i)%wco_c%atmodep, wst(i)%wco_c%atmodep
               if (eof < 0) exit
               wst_n(i) = wst(i)%name
               if (db_mx%wgnsta > 0) call search (wgn_n, db_mx%wgnsta, wst(i)%wco_c%wgn, wst(i)%wco%wgn)
               if (wst(i)%wco%wgn == 0 .and. wst(i)%wco_c%wgn /= "sim") write (9001,*) wst(i)%wco_c%wgn, "file not found (wgn)"
               
               ! For NetCDF, set climate file character names based on scale factors
               ! If scale factor is not -99, use station name, else use "sim"
               if (wst(i)%pcp_factor /= -99.0) then
                  wst(i)%wco_c%pgage = wst(i)%name
               else
                  wst(i)%wco_c%pgage = "sim"
               end if
               
               if (wst(i)%tmax_factor /= -99.0 .and. wst(i)%tmin_factor /= -99.0) then
                  wst(i)%wco_c%tgage = wst(i)%name
               else
                  wst(i)%wco_c%tgage = "sim"
               end if
               
               if (wst(i)%slr_factor /= -99.0) then
                  wst(i)%wco_c%sgage = wst(i)%name
               else
                  wst(i)%wco_c%sgage = "sim"
               end if
               
               if (wst(i)%hmd_factor /= -99.0) then
                  wst(i)%wco_c%hgage = wst(i)%name
               else
                  wst(i)%wco_c%hgage = "sim"
               end if
               
               if (wst(i)%wnd_factor /= -99.0) then
                  wst(i)%wco_c%wgage = wst(i)%name
               else
                  wst(i)%wco_c%wgage = "sim"
               end if
               
               ! note: For NetCDF, the numeric indices (wco%*gage) will be set by cli_ncdf_meas
               ! based on the station index. Traditional system use search() to map file names to indices
               
               ! Set temperature lag array
               if (wst(i)%wco%wgn > 0) then
                  iwgn = wst(i)%wco%wgn
                  wst(i)%tlag = (wgn(iwgn)%tmpmn(1) + wgn(iwgn)%tmpstdmx(1)) / 2.
               end if
               if (eof < 0) exit
            end do
            exit
         enddo
      endif

   else

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
               allocate (wst(iwst)%weat%ts(time%step), source = 0.)
               allocate (wst(iwst)%weat%ts_next(time%step), source = 0.)
               wst(iwst)%weat%precip_prior_day = "dry"
               allocate (wst(iwst)%tlag(w_temp%airlag_d), source = 0.)
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
   endif

   close (107)

   return
end subroutine cli_staread
