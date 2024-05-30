      subroutine septic_parm_read
      
      use input_file_module
      use maximum_data_module
      use septic_data_module
      
      implicit none
         
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: is                   !none       |counter
      
      eof = 0
      imax = 0

      inquire (file=in_parmdb%septic_sep, exist=i_exist)
      if (.not. i_exist .or. in_parmdb%septic_sep == "null") then
          allocate (sepdb(0:0))
      else
      do
        open (171,file=in_parmdb%septic_sep)
        read (171,*,iostat=eof) titldum
        if (eof < 0) exit
        read (171,*,iostat=eof) header
        if (eof < 0) exit
           do while (eof == 0) 
             read (171,*,iostat=eof) titldum
             if (eof < 0) exit
             imax = imax + 1
           end do
           
        db_mx%sep = imax
           
        allocate (sepdb(0:imax))        
        rewind (171)
        read (171,*,iostat=eof) titldum
        if (eof < 0) exit
        read (171,*,iostat=eof) header
        if (eof < 0) exit
    
        do is = 1, db_mx%sep
          read (171,*,iostat=eof) titldum
          if (eof < 0) exit
          backspace (171)
          read (171,*,iostat=eof) sepdb(is)
          if (eof < 0) exit
        end do

        close (171)
        exit
      enddo
      endif
      
      return
      end subroutine septic_parm_read