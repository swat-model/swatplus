      subroutine till_parm_read
      
      use input_file_module
      use maximum_data_module
      use tillage_data_module
      
      implicit none      

      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: itl                  !none       |counter
      integer :: mtl                  !           |
      
      eof = 0
      imax = 0
      mtl = 0
      
      inquire (file=in_parmdb%till_til, exist=i_exist)
      if (.not. i_exist .or. in_parmdb%till_til == "null") then
          allocate (tilldb(0:0))
      else
      do
        open (105,file=in_parmdb%till_til)
        read (105,*,iostat=eof) titldum
        if (eof < 0) exit
        read (105,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof == 0)
            read (105,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do
          
        allocate (tilldb(0:imax)) 
        
        rewind (105)
        read (105,*,iostat=eof) titldum
        if (eof < 0) exit
        read (105,*,iostat=eof) header  
        if (eof < 0) exit
        
          do itl = 1, imax
            read (105,*,iostat=eof) tilldb(itl)
            if (eof < 0) exit
          end do    
        exit
      enddo
      endif
      
      db_mx%tillparm = imax

      close (105)
      return
      end subroutine till_parm_read