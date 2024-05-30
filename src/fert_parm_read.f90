      subroutine fert_parm_read
      
      use input_file_module
      use maximum_data_module
      use fertilizer_data_module
      
      implicit none
   
      integer :: it                   !none       |counter
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      integer :: mfrt                 !           |
      logical :: i_exist              !none       |check to determine if file exists
      
      
      eof = 0
      imax = 0
      mfrt = 0
      
      inquire (file=in_parmdb%fert_frt, exist=i_exist)
      if (.not. i_exist .or. in_parmdb%fert_frt == "null") then
         allocate (fertdb(0:0))
      else
      do  
        open (107,file=in_parmdb%fert_frt)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
           do while (eof == 0) 
             read (107,*,iostat=eof) titldum
             if (eof < 0) exit
             imax = imax + 1
           end do
           
        allocate (fertdb(0:imax))
        
        rewind (107)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        
        do it = 1, imax
          read (107,*,iostat=eof) fertdb(it)
          if (eof < 0) exit
        end do
       exit
      enddo
      endif
      
      db_mx%fertparm  = imax 
      
      close (107)
      return
      end subroutine fert_parm_read