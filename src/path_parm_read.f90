      subroutine path_parm_read
      
      use input_file_module
      use pathogen_data_module, only : path_db
      use maximum_data_module
      
      implicit none

      character (len=80) :: titldum      !              |title of file
      character (len=80) :: header       !              |header of file
      integer :: ibac                    !none          |counter  
      integer :: eof                     !              |end of file
      integer :: imax                    !none          |counter 
      logical :: i_exist                 !              |check to determine if file exists
      
      eof = 0
      imax = 0    
      
      !! read pathogen properties
      inquire (file=in_parmdb%pathcom_db,exist=i_exist)
      if (.not. i_exist .or. in_parmdb%pathcom_db == "null") then
         allocate (path_db(0:0))
      else
      do
        open (107,file=in_parmdb%pathcom_db)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof == 0)
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do

        db_mx%path = imax
          
        allocate (path_db(0:imax))
        rewind (107)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit

        do ibac = 1, db_mx%path
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          backspace (107)
          read (107,*,iostat=eof) path_db(ibac)
          if (eof < 0) exit
        end do
        exit
      enddo
      endif
      
      close (107)
      
      return
      end subroutine path_parm_read