      subroutine overbank_read
    
      use hydrograph_module
      use input_file_module
      use maximum_data_module
      use sd_channel_module
      
      implicit none 

      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      character (len=16) :: namedum   !           |
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      integer :: nspu                 !           |
      logical :: i_exist              !none       |check to determine if file exists
      integer :: max                  !           |
      integer :: mcha_sp              !           |
      integer :: i                    !none       |counter
      integer :: isp                  !none       |counter
      integer :: numb                 !           |
      integer :: ise                  !none       |counter  
          
      eof = 0
      imax = 0
      
    !!read data for surface elements in the floodplain-for overbank flooding
      inquire (file=in_link%chan_surf, exist=i_exist)
      if (i_exist .or. in_link%chan_surf /= "null") then
      do
        open (107,file=in_link%chan_surf)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) mcha_sp
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        imax = 0
        do while (eof == 0)
          read (107,*,iostat=eof) i
          if (eof < 0) exit
          imax = imax + 1
        end do
        imax = max (imax, mcha_sp)
          
        allocate (ch_sur(imax))
        rewind (107)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) mcha_sp
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit

        db_mx%ch_surf = imax
        
        do ise = 1, imax
          read (107,*,iostat=eof) i, namedum, nspu
          if (eof < 0) exit
          
          if (nspu > 0) then
            allocate (sd_ch(i)%fp%obtyp(nspu))
            allocate (sd_ch(i)%fp%obtypno(nspu))
            backspace (107)
            read (107,*,iostat=eof) numb, sd_ch(i)%fp%name, sd_ch(i)%fp%obj_tot, &
               (sd_ch(i)%fp%obtyp(isp), sd_ch(i)%fp%obtypno(isp), isp = 1, nspu)
            if (eof < 0) exit
          end if

        end do
        exit
      end do
      close (107)
      end if
       return
      end subroutine overbank_read