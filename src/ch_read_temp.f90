      subroutine ch_read_temp

      use basin_module
      use time_module
      use input_file_module
      use maximum_data_module
      use channel_data_module
      use hydrograph_module

      implicit none

      character (len=80) :: titldum = "" !          |title of file
      character (len=80) :: header = ""  !          |header of file
      integer :: eof = 0               !          |end of file
      integer :: imax = 0              !units     |description
      integer :: ich_temp = 0           !none      |counter (renamed from ich to avoid module conflict)
      logical :: i_exist               !          |check to determine if file exists

      eof = 0
      imax = 0

      inquire (file=in_cha%temp, exist=i_exist)
      if (.not. i_exist .or. in_cha%temp == "null") then
        allocate (w_temp(0:0))
      else
      do
        open (105,file=in_cha%temp)
        read (105,*,iostat=eof) titldum
        if (eof < 0) exit
        read (105,*,iostat=eof) header

        if (eof < 0) exit
          do while (eof == 0)
            read (105,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do

        db_mx%w_temp = imax

        if (allocated(w_temp)) deallocate(w_temp)

        allocate (w_temp(0:imax))
        rewind (105)
        read (105,*,iostat=eof) titldum
        if (eof < 0) exit
        read (105,*,iostat=eof) header
        if (eof < 0) exit

        do ich_temp = 1, db_mx%w_temp
            read (105,*,iostat=eof) titldum
            if (eof < 0) exit
            backspace (105)
            read (105,*,iostat=eof) w_temp(ich_temp)
            if (eof < 0) exit
        end do

        exit
      enddo
      endif
      close (105)
      return
      end subroutine ch_read_temp
