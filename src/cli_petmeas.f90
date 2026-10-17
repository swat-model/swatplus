      subroutine cli_petmeas
      
      use climate_module
      use input_file_module
      use time_module
      use maximum_data_module
      
      implicit none
   
      integer :: i = 0
      character (len=80) :: titldum = ""!           |title of file
      character (len=80) :: header = "" !           |header of file
      character (len=250) :: line_buffer!           |temporary variable
      character (len=80) :: pet_read    !           |temporary variable
      integer :: eof = 0                !           |end of file
      integer :: imax = 0               !none       |determine max number for array (imax) and total number in file
      integer :: iyr = 0                !none       |number of years
      logical :: i_exist                !none       |check to determine if file exists
      integer :: istep = 0              !           |
      integer :: iyr_prev = 0           !none       |previous year
      integer :: iyrs = 0               !           |
      integer :: num_cols = 0           !none       |number of data columns
      integer :: pos = 0                !none       |string position
       
      eof = 0
      imax = 0

      !! read all measured daily pet data
      inquire (file=in_cli%pet_cli, exist=i_exist)
      if (.not. i_exist .or. in_cli%pet_cli == "null") then
        allocate (petm(0:0))
        allocate (petm_n(0))
      else
      do 
        open (107,file=in_cli%pet_cli)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof == 0)
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1 
          end do
          
      allocate (petm(0:imax))
      allocate (petm_n(imax))
      
      rewind (107)
      read (107,*,iostat=eof) titldum
      if (eof < 0) exit
      read (107,*,iostat=eof) header
      if (eof < 0) exit
      do i = 1, imax
        read (107,*, iostat=eof) petm_n(i)
        if (eof < 0) exit
      end do
      
      rewind (107)
      read (107,*,iostat=eof) titldum
      if (eof < 0) exit
      read (107,*,iostat=eof) header  
      if (eof < 0) exit
      
      do i = 1, imax
        read (107,*,iostat = eof) petm(i)%filename
        if (eof < 0) exit
        
!!!!!weather path code
       if (in_path_pet%peti == "null") then
         open (108,file = petm(i)%filename)
       else
        open (108,file = TRIM(ADJUSTL(in_path_pet%peti))//petm(i)%filename)
       endif
!!!!!weather path code
        
       read (108,*,iostat=eof) titldum
       if (eof < 0) exit
       read (108,*,iostat=eof) header
       if (eof < 0) exit
       read (108,*,iostat=eof) petm(i)%nbyr, petm(i)%tstep, petm(i)%lat, petm(i)%long,     &
                               petm(i)%elev
       if (eof < 0) exit
       
       ! the pet time step has to be the same as time%step
       allocate (petm(i)%ts(366,petm(i)%nbyr),  source = -99.)
       allocate (petm(i)%ts2(366,petm(i)%nbyr), source = -99.)
       allocate (petm(i)%ts3(366,petm(i)%nbyr), source = -99.)
        
       ! read and save start jd and yr
       read (108,*,iostat=eof) iyr, istep
       if (eof < 0) exit
        
       petm(i)%start_day = istep
       petm(i)%start_yr = iyr
        
       backspace (108)

       if (iyr > time%yrc) then
         petm(i)%yrs_start = iyr - time%yrc
       else
         ! read and store entire year
         petm(i)%yrs_start = 0
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

       ! determine number of ET variables in file
       read(108,'(A)',iostat=eof) line_buffer
       if (eof < 0) exit
       num_cols = 0
       pos = 1
       do 
        read(line_buffer(pos:), *, iostat=eof) pet_read
        if (eof /= 0) exit
        num_cols = num_cols + 1
        pos = pos + verify(line_buffer(pos:), ' ') - 1
        pos = pos + len(trim(adjustl(pet_read)))
       end do
       backspace (108)
       
       do
        if (num_cols .eq. 3) then
         read (108,*,iostat=eof) iyr, istep, petm(i)%ts(istep,iyrs) !pet
        elseif (num_cols .eq. 5) then
         read (108,*,iostat=eof) iyr, istep, petm(i)%ts(istep,iyrs), &
           petm(i)%ts2(istep,iyrs), petm(i)%ts3(istep,iyrs)
           !Potential evapotranspiration
           !Short crop (12-cm grass) reference ET
           !Tall crop (50-cm alfalfa) reference ET
        endif  
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
       petm(i)%end_day = istep
       petm(i)%end_yr = iyr
       
      end do
      close (107)
      exit
      end do
      endif
      
      db_mx%petfiles = imax
      
      return
      end subroutine cli_petmeas