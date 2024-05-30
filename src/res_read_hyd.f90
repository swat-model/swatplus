      subroutine res_read_hyd
      
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

      inquire (file=in_res%hyd_res, exist=i_exist)
      if (.not. i_exist .or. in_res%hyd_res == "null") then
        allocate (res_hyddb(0:0))
      else   
      do
       open (105,file=in_res%hyd_res)
       read (105,*,iostat=eof) titldum
       if (eof < 0) exit
       read (105,*,iostat=eof) header
       if (eof < 0) exit
        do while (eof == 0)
          read (105,*,iostat=eof) titldum
          if (eof < 0) exit
          imax = imax + 1
        end do
        
      db_mx%res_hyd = imax
      
      allocate (res_hyddb(0:imax))
      rewind (105)
      read (105,*,iostat=eof) titldum
      if (eof < 0) exit
      read (105,*,iostat=eof) header
      if (eof < 0) exit
      
       do ires = 1, imax
         
         !read (105,*,iostat=eof) titldum
         !backspace (105)
         read (105,*,iostat=eof) res_hyddb(ires)
         if (eof < 0) exit

        if (res_hyddb(ires)%pvol + res_hyddb(ires)%evol > 0.) then
          if(res_hyddb(ires)%pvol <= 0) res_hyddb(ires)%pvol = 0.9 * res_hyddb(ires)%evol
        else
          if (res_hyddb(ires)%pvol <= 0) res_hyddb(ires)%pvol = 60000.0
        end if
        if (res_hyddb(ires)%evol <= 0.0) res_hyddb(ires)%evol = 1.11 * res_hyddb(ires)%pvol
        if (res_hyddb(ires)%psa <= 0.0) res_hyddb(ires)%psa = 0.08 * res_hyddb(ires)%pvol
        if (res_hyddb(ires)%esa <= 0.0) res_hyddb(ires)%esa = 1.5 * res_hyddb(ires)%psa
        if (res_hyddb(ires)%evrsv <= 0.) res_hyddb(ires)%evrsv = 0.6

       end do
       close (105)
      exit
      enddo
      endif
  
      return
      end subroutine res_read_hyd