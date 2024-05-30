      subroutine mgt_read_chemapp
      
      use input_file_module
      use maximum_data_module
      use mgt_operations_module
      
      implicit none      

      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: ichemapp             !none       |counter

      eof = 0
      imax = 0
                                      
      !! read grazing operations
      inquire (file=in_ops%chem_ops, exist=i_exist)
      if (.not. i_exist .or. in_ops%chem_ops == "null") then
         allocate (chemapp_db(0:0))
      else
      do
        open (107,file=in_ops%chem_ops)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        do while (eof == 0)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          imax = imax + 1
        end do
        
        allocate (chemapp_db(0:imax)) 
        
        rewind (107)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
              
        do ichemapp = 1, imax 
          read (107,*,iostat=eof) chemapp_db(ichemapp)
          if (eof < 0) exit
        end do
 
        exit
      enddo
      endif
      close(107)
 
      db_mx%chemapp_db = imax
      
      return  
      end subroutine mgt_read_chemapp