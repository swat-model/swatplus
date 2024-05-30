      subroutine basin_read_prm
      
      use input_file_module
      use basin_module
      
      implicit none
      
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header
      integer :: eof                  !           |end of file
      logical :: i_exist              !           |check to determine if file exists
      
      eof = 0

      inquire (file=in_basin%parms_bas, exist=i_exist)
      if (i_exist .or. in_basin%parms_bas /= "null") then
        !! read basin parameters
      do
        open (107,file=in_basin%parms_bas)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        read (107,*,iostat=eof) bsn_prm
        if (eof < 0) exit
        exit
      enddo
      end if
        close(107)
      
      return       
      end subroutine basin_read_prm