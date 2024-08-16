      subroutine cli_wgnread
      
      use input_file_module
      use time_module
      use maximum_data_module
      use climate_module
      
      implicit none
            
      character (len=80) :: titldum = ""!           |title of file
      character (len=80) :: header = "" !           |header of file
      integer :: iwgn = 0             !           | 
      integer :: mwnd_dir = 0         !           | 
      integer :: iwndir = 0           !none       |counter
      integer :: imo = 0              !none       |counter 
      integer :: eof = 0              !           |end of file
      integer :: imax = 0             !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists  
      integer :: mo = 0               !none       !counter
      integer :: idir = 0             !none       !counter

      
      eof = 0
      imax = 0

      !! read weather generator data from weather_generator.dat - wgn parameters
      inquire (file=in_cli%weat_wgn, exist=i_exist)
      if (.not. i_exist .or. in_cli%weat_wgn == "null") then              
        allocate (wgn(0:1))
        allocate (wgn_n(1))
        allocate (wgn_orig(0:1))
        allocate (wgncur(3,0:1), source = 0.)
        allocate (wgnold(3,0:1), source = 0.)
        wgncur = 0.
        wgnold = 0.
        allocate (wgn_pms(0:1))
        allocate (frad(0:1,1), source = 0.)
        allocate (rnd2(0:1), source = 0.)
        allocate (rnd3(0:1), source = 0.)
        allocate (rnd8(0:1), source = 0.)
        allocate (rnd9(0:1), source = 0.)
        allocate (rndseed(10,0:1), source = 0)
        allocate (idg(9), source = 0)
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
        allocate (wgncur(3,imax), source = 0.)
        allocate (wgnold(3,imax), source = 0.)
        wgncur = 0.
        wgnold = 0.
        allocate (rnd2(imax), source = 0.)
        allocate (frad(imax,time%step), source = 0.)
        allocate (rnd3(imax), source = 0.)
        allocate (rnd8(imax), source = 0.)
        allocate (rnd9(imax), source = 0.)
        allocate (rndseed(10,imax), source = 0)
        allocate (idg(9), source = 0)
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
      !!!removed 1_22_2024
           
      return
      end subroutine cli_wgnread           