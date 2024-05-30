      subroutine hydrol_read
      
      use input_file_module
      use maximum_data_module
      use hydrology_data_module
      
      implicit none
     
      integer :: mhydrol              !           | 
      integer :: ithyd                !none       |counter
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      
      mhydrol = 0
      eof = 0
      imax = 0
      
      !! read all data from hydrol.dat
      inquire (file=in_hyd%hydrol_hyd, exist=i_exist)
      if (.not. i_exist .or. in_hyd%hydrol_hyd == "null") then
        allocate (hyd_db(0:0))
      else
        do
          open (107,file=in_hyd%hydrol_hyd)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          do while (eof == 0)
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
             imax = imax + 1
          end do
          
          allocate (hyd_db(0:imax))
        
          rewind (107)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
                 
          do ithyd = 1, imax
             read (107,*,iostat=eof) hyd_db(ithyd)            
             if (eof < 0) exit
          end do
          exit
        enddo
      endif
      close (107)
 
      db_mx%hyd = imax
      
      return
      end subroutine hydrol_read