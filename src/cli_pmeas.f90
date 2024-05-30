      subroutine cli_pmeas
      
      use climate_module
      use maximum_data_module
      use basin_module
      use input_file_module
      use time_module

      implicit none
            
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      integer :: iyr                  !none       |number of years 
      logical :: i_exist              !none       |check to determine if file exists 
      integer :: mpcp                 !           |
      integer :: i                    !none       |counter
      integer :: istep                !           |
      integer :: iyr_prev             !none       |previous year
      integer :: iyrs                 !           |
      integer :: iss                  !none       |counter
      integer :: mo
      integer :: day_mo
      integer :: ihr
            
       mpcp = 0
       eof = 0
       imax = 0

      !! read all measured daily precipitation data
      inquire (file=in_cli%pcp_cli, exist=i_exist)
      if (.not. i_exist .or. in_cli%pcp_cli == "null") then
        allocate (pcp(0:0))
        allocate (pcp_n(0))
      else
      do 
        open (107,file=in_cli%pcp_cli)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof == 0)
            read (107,*,iostat = eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do
          
      allocate (pcp(0:imax))
      allocate (pcp_n(imax))
      
      rewind (107)
      read (107,*,iostat=eof) titldum
      if (eof < 0) exit
      read (107,*,iostat=eof) header
      if (eof < 0) exit
      do i = 1, imax
        read (107,*,iostat = eof) pcp_n(i)
        if (eof < 0) exit
      end do
      
      rewind (107)
      read (107,*,iostat=eof) titldum
      if (eof < 0) exit
      read (107,*,iostat=eof) header
      if (eof < 0) exit
      
      do i = 1, imax
        read (107,*,iostat = eof) pcp(i)%filename
        if (eof < 0) exit
        
      ! weather path code
      if (in_path_pcp%pcp == "null") then 
        open (108,file = pcp(i)%filename)
      else
        open (108,file = TRIM(ADJUSTL(in_path_pcp%pcp))//pcp(i)%filename)
      endif
        ! weather path code       
        read (108,*,iostat=eof) titldum
        if (eof < 0) exit
        read (108,*,iostat=eof) header
        if (eof < 0) exit
        read (108,*,iostat=eof) pcp(i)%nbyr, pcp(i)%tstep, pcp(i)%lat,    & 
                pcp(i)%long, pcp(i)%elev
        if (eof < 0) exit
        
        if (pcp(i)%tstep > 0) then
          ! the precip time step has to be the same as time%step
          allocate (pcp(i)%tss(time%step,366,pcp(i)%nbyr))
        else
         allocate (pcp(i)%ts(366,pcp(i)%nbyr))
        end if

        ! read and save start jd and yr
         read (108,*,iostat=eof) iyr, istep
         if (eof < 0) exit        
         pcp(i)%start_day = istep
         pcp(i)%start_yr = iyr
         
         backspace (108)

      if (iyr > time%yrc) then
        pcp(i)%yrs_start = iyr - time%yrc
      else
        ! read and store entire year
        pcp(i)%yrs_start = 0
      end if
      
        do
          read (108,*,iostat=eof) iyr, istep
          if (eof < 0) exit
          if (iyr >= time%yrc .and. istep >= time%day_start) exit
        end do

       backspace (108)
       iyr_prev = iyr
       iyrs = 1
       iss = 1

       do
         if (pcp(i)%tstep > 0) then
           read (108,*,iostat=eof)iyr, istep, mo, day_mo, ihr, pcp(i)%tss(iss,istep,iyrs)
           iss = iss + 1
           if (iss > time%step) iss = 1
           if (eof < 0) exit
         else    
           read (108,*,iostat=eof)iyr, istep, pcp(i)%ts(istep,iyrs)
           if (eof < 0) exit
         endif
         !! check to see when next year
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
       pcp(i)%end_day = istep
       pcp(i)%end_yr = iyr
       
      end do

      close (107)
      exit
      end do
      endif
      
      db_mx%pcpfiles = imax

      return
      end subroutine cli_pmeas