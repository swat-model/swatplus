      subroutine dr_db_read
    
      use dr_module
      use input_file_module
      use constituent_mass_module
      !use organic_mineral_mass_module
      use maximum_data_module

      implicit none
 
      character (len=80) :: titldum, header
      integer :: eof, imax
      logical :: i_exist              !none       |check to determine if file exists
      integer :: ii

      eof = 0
      
      !read all delivery ratio data
      inquire (file=in_delr%del_ratio, exist=i_exist)
      if (i_exist .or. in_delr%del_ratio /= "null") then
        do
          open (107,file=in_delr%del_ratio)
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
          
          db_mx%dr = imax
        
          allocate (dr_db(imax))
          rewind (107)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          
        do ii = 1, imax
          read (107,*,iostat=eof) dr_db(ii)
          if (eof < 0) exit
        end do
          close (107)
          exit
        end do
      end if

      ! read delivery ratio data for all constituent types
      call dr_read_om
      if (cs_db%num_pests > 0) call dr_read_pest
      if (cs_db%num_paths > 0) call dr_path_read
      if (cs_db%num_metals > 0) call dr_read_hmet
      if (cs_db%num_salts > 0) call dr_read_salt
      
      return
      end subroutine dr_db_read