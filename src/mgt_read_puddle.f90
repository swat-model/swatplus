      subroutine  mgt_read_puddle
      
      use maximum_data_module
      use mgt_operations_module
      
      implicit none 

      integer :: ic                   !none       |counter
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      
      eof = 0
      imax = 0

      inquire (file="puddle.ops", exist=i_exist)
      if (.not. i_exist .or. "puddle.ops" == " null") then
        allocate (pudl_db(0:0))
      else
      do
        open (104,file="puddle.ops")
        read (104,*,iostat=eof) titldum
        if (eof < 0) exit
        read (104,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof == 0)
            read (104,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do
        allocate (pudl_db(0:imax))
        
        rewind (104)
        read (104,*,iostat=eof) titldum
        if (eof < 0) exit
        read (104,*,iostat=eof) header
        if (eof < 0) exit
        
        do ic = 1, imax
          read (104,*,iostat=eof) pudl_db(ic)
          if (eof < 0) exit
        end do
        
        exit
      enddo
      endif

      db_mx%pudl_db = imax
      
      close (104)
      return
      end subroutine mgt_read_puddle