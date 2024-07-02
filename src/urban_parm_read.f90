      subroutine urban_parm_read
      
      use input_file_module
      use maximum_data_module
      use urban_data_module
      
      implicit none
      
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      integer :: eof                  !           |end of file
      integer :: imax = 0.            !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: iu                   !none       |counter 
           
      inquire (file=in_parmdb%urban_urb, exist=i_exist)
      if (.not. i_exist .or. in_parmdb%urban_urb == "null") then
          allocate (urbdb(0:0))
      else
      do
        open (108,file=in_parmdb%urban_urb)
        read (108,*,iostat=eof) titldum
        if (eof < 0) exit
        read (108,*,iostat=eof) header
        if (eof < 0) exit
          imax = 0
          do while (eof == 0)
            read (108,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do
          
        allocate (urbdb(0:imax)) 
        
        rewind (108)
        read (108,*,iostat=eof) titldum
        if (eof < 0) exit
        read (108,*,iostat=eof) header
        if (eof < 0) exit
            
        do iu = 1, imax
           read (108,*,iostat=eof) urbdb(iu)
           if (eof < 0) exit
         end do
       exit
      enddo
      endif

      db_mx%urban = imax
      
      close (108)
      return
      end subroutine urban_parm_read