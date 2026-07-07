      subroutine cli_wgnread
      
      use input_file_module
      use time_module
      use maximum_data_module
      use climate_module
      
      implicit none
            
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      integer :: iwgn                 !           | 
      integer :: mwnd_dir             !           | 
      integer :: iwndir               !none       |counter
      integer :: imo                  !none       |counter 
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists  
      integer :: mo                   !none       !counter
      integer :: idir                 !none       !counter

      
      eof = 0
      imax = 0

      !! read weather generator data from weather_generator.dat - wgn parameters
      inquire (file=in_cli%weat_wgn, exist=i_exist)
      if (.not. i_exist .or. in_cli%weat_wgn == "null") then              
        allocate (wgn(0:1))
        allocate (wgn_n(1))
        allocate (wgn_orig(0:1))
        allocate (wgncur(3,0:1))
        allocate (wgnold(3,0:1))
        wgncur = 0.
        wgnold = 0.
        allocate (wgn_pms(0:1))
        allocate (frad(0:1,1))
        allocate (rnd2(0:1))
         allocate (rnd3(0:1))
         allocate (rnd8(0:1))
         allocate (rnd9(0:1))
         allocate (rndseed(10,0:1))
         allocate (idg(9))
        call gcycl
      else 
      do
        open (114,file=in_cli%weat_wgn)
        read (114,*,iostat=eof) titldum
        if (eof < 0) exit
        !! determine max number for array (imax) and total number in file
        do while (eof == 0)
          read (114,*,iostat=eof) titldum
          if (eof < 0) exit
          read (114,*,iostat=eof) header
          if (eof < 0) exit
          do mo = 1, 12
            read (114,*,iostat=eof) titldum
            if (eof < 0) exit
          end do
          imax = imax + 1
        end do

        db_mx%wgnsta = imax
        
        !! allocate weather variables
        allocate (wgn(imax))
        allocate (wgn_n(imax))
        allocate (wgn_pms(imax))
        allocate (wgn_orig(imax))
        allocate (wgncur(3,imax))
        allocate (wgnold(3,imax))
        wgncur = 0.
        wgnold = 0.
        allocate (rnd2(imax))
        allocate (frad(imax,time%step))
        allocate (rnd3(imax))
        allocate (rnd8(imax))
        allocate (rnd9(imax))
        allocate (rndseed(10,imax))
        allocate (idg(9))
        rnd2 = 0.
        rnd3 = 0.
        rnd8 = 0.
        rnd9 = 0.
        rndseed = 0

        rewind (114)
        read (114,*,iostat=eof) titldum
        if (eof < 0) exit
        
        call gcycl
        
      do iwgn = 1, db_mx%wgnsta
        read (114,*,iostat=eof) wgn_n(iwgn), wgn(iwgn)%lat, wgn(iwgn)%long, wgn(iwgn)%elev, wgn(iwgn)%rain_yrs
        if (eof < 0) exit
        read (114,*,iostat=eof) header
        if (eof < 0) exit
        do mo = 1, 12
          read (114,*,iostat=eof) wgn(iwgn)%tmpmx(mo), wgn(iwgn)%tmpmn(mo), wgn(iwgn)%tmpstdmx(mo),             &
              wgn(iwgn)%tmpstdmn(mo), wgn(iwgn)%pcpmm(mo), wgn(iwgn)%pcpstd(mo), wgn(iwgn)%pcpskw(mo),          &
              wgn(iwgn)%pr_wd(mo), wgn(iwgn)%pr_ww(mo), wgn(iwgn)%pcpd(mo), wgn(iwgn)%rainhmx(mo),              &
              wgn(iwgn)%solarav(mo), wgn(iwgn)%dewpt(mo), wgn(iwgn)%windav(mo)
        end do
        
        !! initialize weather generator parameters
        call cli_initwgn(iwgn)
        if (eof < 0) exit
      end do    
      exit
      enddo
      endif
      close (114) 

      !! read wind direction generator data from wind_direction.dat
      inquire (file=in_cli%wind_dir, exist=i_exist)
      if (.not. i_exist .or. in_cli%wind_dir == "null") then
        allocate (wnd_dir(0:0))
      else
      do 
        open (114,file=in_cli%wind_dir)
        read (114,*,iostat=eof) titldum
        if (eof < 0) exit
        read (114,*,iostat=eof) mwnd_dir
        if (eof < 0) exit
        if (mwnd_dir > 0) then
        !! arrays containing wind direction
          allocate (wnd_dir(mwnd_dir))
        do iwndir = 1, mwnd_dir
          read (114,*,iostat=eof) wnd_dir(iwndir)%name
          if (eof < 0) exit
          read (114,*,iostat=eof) titldum
          if (eof < 0) exit
          read (114,*,iostat=eof) titldum
          if (eof < 0) exit
          do idir = 1, 16
            read (114,*,iostat=eof) (wnd_dir(iwndir)%dir(imo,idir),     &    
                                                           imo = 1, 12)
            if (eof < 0) exit
  !          exit
          end do
  !      enddo 
  !      endif
                  
          do imo = 1, 12
            wnd_dir(iwndir)%dir(imo,1) =                                   &
                                    wnd_dir(iwndir)%dir(imo,1) / 100.
            do idir = 2, 16
              wnd_dir(iwndir)%dir(imo,idir) =                              &
                                 wnd_dir(iwndir)%dir(imo,idir) / 100.      &
                                 + wnd_dir(iwndir)%dir(imo,idir-1)
            end do
            do idir = 1, 16
              wnd_dir(iwndir)%dir(imo,idir) =                              &
                                        wnd_dir(iwndir)%dir(imo,idir)      &
                                        / wnd_dir(iwndir)%dir(imo,16)
            end do
          end do
        end do

      end if
      exit
      enddo
      endif
      
      close (114) 
           
      return
      end subroutine cli_wgnread           