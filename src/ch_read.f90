      subroutine ch_read
      
      use basin_module
      use input_file_module
      use channel_data_module
      use maximum_data_module
      use hydrograph_module
      use pesticide_data_module

      implicit none
      
      character (len=80) :: titldum     !              |title of file
      character (len=80) :: header      !              |header of file
      integer :: eof                    !              |end of file
      integer :: i                      !units         |description
      integer :: imax                   !units         |description
      logical :: i_exist                !              |check to determine if file exists
      integer :: ichi                   !none          |counter
      integer :: k                      !units         |description
      integer :: iinit                  !none          |counter
      integer :: ihyd                   !none          |counter
      integer :: ised                   !none          |counter
      integer :: inut                   !none          |counter
      
      eof = 0
      imax = 0

      inquire (file=in_cha%dat, exist=i_exist)
      if (.not. i_exist .or. in_cha%dat == "null") then
        allocate (ch_dat(0:0))
        allocate (ch_dat_c(0:0))
      else   
      do
       open (105,file=in_cha%dat)
       read (105,*,iostat=eof) titldum
       if (eof < 0) exit
       read (105,*,iostat=eof) header
       if (eof < 0) exit
        do while (eof == 0)
          read (105,*,iostat=eof) i
          if (eof < 0) exit
          imax = Max(imax,i)
        end do
    
      db_mx%ch_dat = imax
      
      allocate (ch_dat(0:imax))
      allocate (ch_dat_c(0:imax))
      
      rewind (105)
      read (105,*,iostat=eof) titldum
      if (eof < 0) exit
      read (105,*,iostat=eof) header
      if (eof < 0) exit
     
       do ichi = 1, db_mx%ch_dat
         read (105,*,iostat=eof) i
         if (eof < 0) exit
         backspace (105)
         read (105,*,iostat=eof) k, ch_dat_c(ichi)
         if (eof < 0) exit

         
         do iinit = 1, db_mx%ch_init
           if (ch_init(iinit)%name == ch_dat_c(ichi)%init) then
             ch_dat(ichi)%init = iinit
             exit
           end if
         end do
         
       
         do ihyd = 1, db_mx%ch_hyd
           if (ch_hyd(ihyd)%name == ch_dat_c(ichi)%hyd) then
             ch_dat(ichi)%hyd = ihyd
             exit
           end if
         end do
       
         do ised = 1, db_mx%ch_sed
           if (ch_sed(ised)%name == ch_dat_c(ichi)%sed) then
             ch_dat(ichi)%sed = ised
             exit
           end if
         end do      

         do inut = 1, db_mx%ch_nut
           if (ch_nut(inut)%name == ch_dat_c(ichi)%nut) then
             ch_dat(ichi)%nut = inut
             exit
           end if
         end do   

       if (ch_dat(ichi)%init == 0) write (9001,*) ch_dat_c(ichi)%init, " not found (initial.cha)"
       if (ch_dat(ichi)%hyd == 0) write (9001,*) ch_dat_c(ichi)%hyd, " not found (hydrology.cha)"
       if (ch_dat(ichi)%sed == 0) write (9001,*) ch_dat_c(ichi)%sed, " not found (sediment.cha)"
       if (ch_dat(ichi)%nut == 0) write (9001,*) ch_dat_c(ichi)%nut, " not found (nutrients.cha)"      
       
       end do
              
       close (105)
      exit
      enddo
      endif
      
      return
      
    end subroutine ch_read