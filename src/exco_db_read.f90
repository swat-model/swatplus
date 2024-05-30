      subroutine exco_db_read
    
      use exco_module
      use constituent_mass_module
      use input_file_module
      use maximum_data_module

      implicit none
 
      character (len=80) :: titldum, header
      integer :: eof, imax
      logical :: i_exist              !none       |check to determine if file exists
      integer :: ii

      eof = 0
      
      !read all export coefficient data
      inquire (file=in_exco%exco, exist=i_exist)
      if (i_exist .or. in_exco%exco /= "null") then
        do
          open (107,file=in_exco%exco)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          imax = 0
          do while (eof == 0)
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do
          
          db_mx%exco = imax
          
          allocate (exco_db(0:imax))
          rewind (107)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          
        do ii = 1, imax
          read (107,*,iostat=eof) exco_db(ii)
          if (eof < 0) exit
        end do
          close (107)
          exit
        end do
      end if

      ! read export coefficient data for all constituent types
      call exco_read_om
      if (cs_db%num_pests > 0) call exco_read_pest
      if (cs_db%num_paths > 0) call exco_read_path
      if (cs_db%num_metals > 0) call exco_read_hmet
      if (cs_db%num_salts > 0) call exco_read_salt
      
      return
      end subroutine exco_db_read