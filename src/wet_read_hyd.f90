      subroutine wet_read_hyd
      
      use basin_module
      use input_file_module
      use maximum_data_module
      use reservoir_data_module
      use output_landscape_module
      
      implicit none
      
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: i                    !none       |counter
      integer :: ires                 !none       |counter 

      eof = 0
      imax = 0

      inquire (file=in_res%hyd_wet, exist=i_exist)
      if (.not. i_exist .or. in_res%hyd_wet == "null") then
        allocate (wet_hyd(0:0))
      else   
      do
       open (105,file=in_res%hyd_wet)
       read (105,*,iostat=eof) titldum
       if (eof < 0) exit
       read (105,*,iostat=eof) header
       if (eof < 0) exit
        do while (eof == 0)
          read (105,*,iostat=eof) titldum
          if (eof < 0) exit
          imax = imax + 1
        end do
        
      db_mx%wet_hyd = imax
      
      allocate (wet_hyd(0:imax))
      rewind (105)
      read (105,*,iostat=eof) titldum
      if (eof < 0) exit
      read (105,*,iostat=eof) header
      if (eof < 0) exit
      
       do ires = 1, imax
         read (105,*,iostat=eof) titldum
         if (eof < 0) exit
         backspace (105)
         read (105,*,iostat=eof) wet_hyd(ires)
         if (eof < 0) exit

        if (wet_hyd(ires)%psa <= 0.0) wet_hyd(ires)%psa = 0.08 * wet_hyd(ires)%pdep
        if (wet_hyd(ires)%esa <= 0.0) wet_hyd(ires)%esa = 1.5 * wet_hyd(ires)%psa
        if (wet_hyd(ires)%evrsv <= 0.) wet_hyd(ires)%evrsv = 0.6

       end do
       close (105)
      exit
      enddo
      endif
  
      return
      end subroutine wet_read_hyd