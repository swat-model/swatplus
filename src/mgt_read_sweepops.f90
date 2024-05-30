      subroutine mgt_read_sweepops
      
      use input_file_module
      use maximum_data_module
      use mgt_operations_module
      
      implicit none 
      
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: msweepops            !           |!
      integer :: isweepop             !none       |counter
      
      msweepops = 0
      eof = 0
      imax = 0
      
      !! read street sweeping operations 
      inquire (file=in_ops%sweep_ops, exist=i_exist)
      if (.not. i_exist .or. in_ops%sweep_ops == "null") then 
        allocate (sweepop_db(0:0))
      else
      do
        open (107,file=in_ops%sweep_ops)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        do while (eof == 0)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          imax = imax + 1
        end do
        
         allocate (sweepop_db(0:imax))
         
         rewind (107)
         read (107,*,iostat=eof) titldum
         if (eof < 0) exit
         read (107,*,iostat=eof) header
         if (eof < 0) exit
              
        do isweepop = 1, imax
          read (107,*,iostat=eof) sweepop_db(isweepop)
          if (eof < 0) exit
        end do

        exit
      enddo
      endif
      close(107)
      
      db_mx%sweepop_db = imax
      
      return
      end subroutine mgt_read_sweepops