      subroutine cli_hmeas

      use input_file_module
      use climate_module
      use maximum_data_module
      use time_module
      
      implicit none
      
      character (len=80) :: titldum     !title of file
      character (len=80) :: header      !header of file
      integer :: eof                    !end of file
      integer ::i                       !none      |counter
      integer :: imax                   !none      |determine max number for array (imax) and total number in file
      integer :: iyr                    !none      |number of years
      logical :: i_exist                !none      |check to determine if file exists
      integer :: istep                  !units     |description
      integer :: iyr_prev               !none      |previous year
      integer :: iyrs                   !units     |description
       
       eof = 0
       imax = 0

      !! read all measured daily relative humidity data
      inquire (file=in_cli%hmd_cli, exist=i_exist)
      if (.not. i_exist .or. in_cli%hmd_cli == "null") then
         allocate (hmd(0:0))
         allocate (hmd_n(0))
      else
      do 
        open (107,file=in_cli%hmd_cli)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof == 0)
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do
          
      allocate (hmd(0:imax))
      allocate (hmd_n(imax))
      
      rewind (107)
      read (107,*,iostat=eof) titldum
      if (eof < 0) exit
      read (107,*,iostat=eof) header
      if (eof < 0) exit
      do i = 1, imax
        read (107,*,iostat = eof) hmd_n(i)
        if (eof < 0) exit
      end do
      
      rewind (107)
      read (107,*,iostat=eof) titldum
      if (eof < 0) exit
      read (107,*,iostat=eof) header
      if (eof < 0) exit
      
      do i = 1, imax
        read (107,*,iostat = eof) hmd(i)%filename
        if (eof < 0) exit
        
!!!!!weather path code
       if (in_path_hmd%hmd == "null") then
         open (108,file = hmd(i)%filename)
       else
        open (108,file = TRIM(ADJUSTL(in_path_hmd%hmd))//hmd(i)%filename)
       endif
!!!!!weather path code

        read (108,*,iostat=eof) titldum
        if (eof < 0) exit
        read (108,*,iostat=eof) header
        if (eof < 0) exit
        read (108,*,iostat=eof) hmd(i)%nbyr, hmd(i)%tstep, hmd(i)%lat,            &
                hmd(i)%long, hmd(i)%elev
        if (eof < 0) exit
        
       ! the precip time step has to be the same as time%step
       allocate (hmd(i)%ts(366,hmd(i)%nbyr))
       
       ! read and save start jd and yr
       read (108,*,iostat=eof) iyr, istep
       if (eof < 0) exit
       
       hmd(i)%start_day = istep
       hmd(i)%start_yr = iyr
       
       backspace (108)
         
      if (iyr > time%yrc) then
        hmd(i)%yrs_start = iyr - time%yrc
      else
        ! read and store entire year
        hmd(i)%yrs_start = 0
      end if
      
        ! read and store entire year
       do 
         read (108,*,iostat=eof) iyr, istep
         if (eof < 0) exit
         if (iyr >= time%yrc .and. istep >= time%day_start) exit
       end do

       backspace (108)
       iyr_prev = iyr
       iyrs = 1
       
       do
         read (108,*,iostat=eof) iyr, istep, hmd(i)%ts(istep,iyrs)
         if (eof < 0) exit
         if (istep == 365 .or. istep == 366) then
           read (108,*,iostat=eof) iyr, istep
           if (eof < 0) exit
           backspace (108)
           if (iyr /= iyr_prev) then
             iyr_prev = iyr
             iyrs = iyrs + 1
           end if
         end if
       end do
       close (108)
              
       ! save end jd and year
       hmd(i)%end_day = istep
       hmd(i)%end_yr = iyr
       
      end do
      close (107)
      exit
      end do
      endif
      
      db_mx%rhfiles = imax
      
      return      
      end subroutine cli_hmeas