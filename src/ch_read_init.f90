      subroutine ch_read_init
      
      use basin_module
      use input_file_module
      use maximum_data_module
      use channel_data_module
      use sd_channel_module

      implicit none
      
      character (len=80) :: titldum    !          |title of file
      character (len=80) :: header     !          |header of file
      integer :: eof                   !          |end of file
      integer :: imax                  !units     |description
      logical :: i_exist               !          |check to determine if file exists
      integer :: ich                   !none      |counter
      
      eof = 0
      imax = 0
      
      inquire (file=in_cha%init, exist=i_exist)
      if (.not. i_exist .or. in_cha%init == "null") then
        allocate (ch_init(0:0))
        allocate (sd_init(0:0))
      else   
      do
       open (105,file=in_cha%init)
       read (105,*,iostat=eof) titldum
       if (eof < 0) exit
       read (105,*,iostat=eof) header
       if (eof < 0) exit
        do while (eof == 0)
          read (105,*,iostat=eof) titldum
          if (eof < 0) exit
          imax = imax + 1
        end do
        
      db_mx%ch_init = imax
      
      allocate (ch_init(0:imax))
      allocate (sd_init(0:imax))
      rewind (105)
      read (105,*,iostat=eof) titldum
      if (eof < 0) exit
      read (105,*,iostat=eof) header
      if (eof < 0) exit
      
       do ich = 1, db_mx%ch_init
         read (105,*,iostat=eof) ch_init(ich)
         if (eof < 0) exit
       end do
       close (105)
      exit
      enddo
      endif

      return    
      end subroutine ch_read_init