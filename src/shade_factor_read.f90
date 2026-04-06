      subroutine shade_factor_read
    
    !! subroutine to read the shade factor input - todo: needs to be called in main (not sure if correct)
      
      use input_file_module
      use maximum_data_module
      use sd_channel_module
      use hydrograph_module
      
      implicit none
     
      character (len=13) :: file      !           |
      integer :: i                    !           | 
      integer :: idlsu                !none       |counter
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      
      eof = 0
      imax = 0
      
      !! read all data from shade factor file
      inquire (file=in_shf%ssff_shf, exist=i_exist)
      if (.not. i_exist .or. in_shf%ssff_shf == "null") then
        allocate (shf_db(0:0))
      else
        do
          open (107,file=in_shf%ssff_shf)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          do while (eof == 0)
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
             imax = imax + 1
          end do
          
          allocate (shf_db(0:imax))
        
          rewind (107)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
                 
          do idlsu = 1, imax
             read (107,*,iostat=eof) shf_db(idlsu)            
             if (eof < 0) exit
          end do
          exit
        enddo
      endif
      close (107)
 
      db_mx%shf = imax
      
      return
	end subroutine shade_factor_read
	