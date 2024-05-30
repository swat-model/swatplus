      subroutine plant_parm_read
      
      use input_file_module
      use maximum_data_module
      use plant_data_module
      
      implicit none 

      integer :: ic                   !none       |counter
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      integer :: mpl                  !           | 
      logical :: i_exist              !none       |check to determine if file exists
      
      
      eof = 0
      imax = 0
      mpl = 0

      inquire (file=in_parmdb%plants_plt, exist=i_exist)
      if (.not. i_exist .or. in_parmdb%plants_plt == " null") then
        allocate (pldb(0:0))
        allocate (plcp(0:0))
      else
      do
        open (104,file=in_parmdb%plants_plt)
        read (104,*,iostat=eof) titldum
        if (eof < 0) exit
        read (104,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof == 0)
            read (104,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do
        allocate (pldb(0:imax))
        allocate (plcp(0:imax))
        
        rewind (104)
        read (104,*,iostat=eof) titldum
        if (eof < 0) exit
        read (104,*,iostat=eof) header
        if (eof < 0) exit
        
        do ic = 1, imax
          read (104,*,iostat=eof) pldb(ic)
          if (eof < 0) exit
          pldb(ic)%mat_yrs = Max (1, pldb(ic)%mat_yrs)
        end do
        
        exit
      enddo
      endif

      db_mx%plantparm = imax
      
      close (104)
      return
      end subroutine plant_parm_read