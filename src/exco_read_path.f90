      subroutine exco_read_path
    
      use hydrograph_module
      use input_file_module
      use organic_mineral_mass_module
      use constituent_mass_module
      use exco_module
      use maximum_data_module

      implicit none
 
      character (len=80) :: titldum, header
      integer :: eof, imax, ob1, ob2
      logical :: i_exist              !none       |check to determine if file exists
      integer :: iexco_path,ii, ipath, iexco, iob

      eof = 0
      imax = 0
      
      !read all export coefficient data
      inquire (file=in_exco%path, exist=i_exist)
      if (i_exist .or. in_exco%path /= "null") then
        do
          open (107,file=in_exco%path)
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
          
          db_mx%exco_path = imax
          
          allocate (exco_path(imax))
          do iexco_path = 1, imax
            allocate (exco_path(iexco_path)%path(cs_db%num_paths))
          end do
          allocate (exco_path_num(imax))
          allocate (exco_path_name(imax))
          rewind (107)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
      
          !read all export coefficient data
          do ii = 1, db_mx%exco_path
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            backspace (107)
            read (107,*,iostat=eof) exco_path_name(ii), (exco_path(ii)%path(ipath), ipath = 1, cs_db%num_paths)   
            if (eof < 0) exit
          end do
          close (107)
          exit
        end do
      end if
                  
      ! xwalk with exco file to get sequential number
      do iexco = 1, db_mx%exco
        do iexco_path = 1, db_mx%exco_path
          if (exco_db(iexco)%path_file == exco_path_name(iexco_path)) then
            exco_path_num(iexco) = iexco_path
            exit
          end if
        end do
      end do
      
      !set exco object hydrograph
      ob1 = sp_ob1%exco
      ob2 = sp_ob1%exco + sp_ob%exco - 1
      do iob = ob1, ob2
        iexco = ob(iob)%props
		if (exco_db(iexco)%path_file == "null") then
		  obcs(iob)%hd(1)%path = 0.
		else
          iexco_path = exco_path_num(iexco)
          obcs(iob)%hd(1)%path = exco_path(iexco_path)%path
		end if 
      end do
      
      return
      end subroutine exco_read_path