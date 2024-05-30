      subroutine dr_read

      use hydrograph_module
      use input_file_module
      use organic_mineral_mass_module
      
      implicit none
      
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      integer :: eof                  !           |end of file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: imax                 !           |
      integer :: mdr_sp               !           |ending of loop 
      integer :: ii                   !none       |counter
      integer :: i                    !none       |counter

      eof = 0
      imax = 0
      
      !read all delivery ratio data here - don't need a module
      inquire (file=in_delr%del_ratio, exist=i_exist)
      if (i_exist .or. in_delr%del_ratio /= 'null') then
      do
        open (107,file=in_delr%del_ratio)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) mdr_sp
        allocate (dr(mdr_sp))
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        do ii = 1, mdr_sp
          read (107,*,iostat=eof) dr_om(i,ii)   ! read dr for every pesticide community
          if (eof < 0) exit
        end do
        exit
      end do
      end if
      return
      end subroutine dr_read