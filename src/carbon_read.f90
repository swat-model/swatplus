      subroutine carbon_read
    
      use carbon_module
      
      implicit none
      
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file 
      integer :: icarb                !           |
      logical :: i_exist              !none       |check to determine if file exists
      
      eof = 0
      
      inquire (file='basins_carbon.tes', exist=i_exist)
      if (.not. i_exist) then
        write (9001,*) "file not found (basins_carbon.tes)"
       else
      do
        open (104,file='basins_carbon.tes')
        read (104,*,iostat=eof) titldum
        if (eof < 0) exit
        read (104,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof == 0)
            read (104,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do
          
        rewind (104)
        read (104,*,iostat=eof) titldum
        if (eof < 0) exit
        read (104,*,iostat=eof) header
        if (eof < 0) exit
        
        do icarb = 1, imax
          read (104,*,iostat=eof) cbn_tes
          if (eof < 0) exit
        end do
        
        exit
      enddo
      endif
      
      close (104)
      return
      end subroutine carbon_read