      subroutine res_read_init
      
      use basin_module
      use input_file_module
      use maximum_data_module
      use reservoir_data_module
      
      implicit none      
      
      character (len=80) :: titldum     !             |title of file
      character (len=80) :: header      !             |header of file
      integer :: eof                    !             |end of file
      integer :: imax                   !             |determine max number for array (imax) and total number in file
      logical :: i_exist                !none         |check to determine if file exists
      integer :: ires                   !none         |counter
      
      eof = 0
      imax = 0
      
      !read init
      inquire (file=in_res%init_res, exist=i_exist)
      if (.not. i_exist .or. in_res%init_res == "null") then
        allocate (res_init(0:0))
        allocate (wet_init(0:0))
      else   
      do
       open (105,file=in_res%init_res)
       read (105,*,iostat=eof) titldum
       if (eof < 0) exit
       read (105,*,iostat=eof) header
       if (eof < 0) exit
        do while (eof == 0)
          read (105,*,iostat=eof) titldum
          if (eof < 0) exit
          imax = imax + 1
        end do
        
      db_mx%res_init = imax
      
      allocate (res_init(0:imax))
      allocate (wet_init(0:imax))
      allocate (res_init_dat_c(0:imax))
      
      rewind (105)
      read (105,*,iostat=eof) titldum
      if (eof < 0) exit
      read (105,*,iostat=eof) header
      if (eof < 0) exit
           
       do ires = 1, db_mx%res_init
         read (105,*,iostat=eof) res_init_dat_c(ires)
         if (eof < 0) exit
       end do
       close (105)
      exit
      enddo
      endif
      
      return
      end subroutine res_read_init