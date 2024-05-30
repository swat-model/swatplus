      subroutine cons_prac_read
      
      use input_file_module
      use maximum_data_module
      use landuse_data_module
      
      implicit none
      
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: eof                  !           |end of file
      integer :: imax                 !           |
      integer :: icp                  !none       |counter
      
      eof = 0
      imax = 0
      
    !! read all curve number data from cn.tbl
      inquire (file=in_lum%cons_prac_lum, exist=i_exist)
      if (.not. i_exist .or. in_lum%cons_prac_lum == "null") then
        allocate (cons_prac(0:0))
      else
      do
        open (107,file=in_lum%cons_prac_lum)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
         do while (eof == 0)
           read (107,*,iostat=eof) titldum
           if (eof < 0) exit
           imax = imax + 1
         end do
         
        allocate (cons_prac(0:imax))
        
        rewind (107)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        
        do icp = 1, imax
          read (107,*,iostat=eof) cons_prac(icp)
          if (eof < 0) exit
        end do
        exit
      enddo
      endif
      
      db_mx%cons_prac = imax
      
      close(107)
      
      return 
      end subroutine cons_prac_read