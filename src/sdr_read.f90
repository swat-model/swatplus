      subroutine sdr_read
      
      use input_file_module
      use maximum_data_module
      use hru_module, only : sdr
      
      implicit none 
      
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: isdr                 !none       |counter
      
      eof = 0
      imax = 0
      
      !! read all subsurface drainage data from sdr.dat
      inquire (file=in_str%tiledrain_str, exist=i_exist)
      if (.not. i_exist .or. in_str%tiledrain_str == "null") then
        allocate (sdr(0:0))
      else
        do
          open (107,file=in_str%tiledrain_str)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          do while (eof == 0)
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do
          
          allocate (sdr(0:imax))
          
          rewind (107)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit

          do isdr = 1, imax 
            read (107,*,iostat=eof) sdr(isdr)          
            if (eof < 0) exit
          end do

          exit
        enddo
      endif
      
      db_mx%sdr = imax
      
      close(107)
      
      return
      end subroutine sdr_read