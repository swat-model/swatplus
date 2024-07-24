      subroutine dr_path_read
    
      use hydrograph_module
      use dr_module
      use input_file_module
      use organic_mineral_mass_module
      use constituent_mass_module
      use maximum_data_module

      implicit none
 
      character (len=80) :: titldum, header
      integer :: eof, imax, ob1, ob2
      logical :: i_exist              !none       |check to determine if file exists
      integer :: idr_path, ii, ipath, idr, iob

      eof = 0
      imax = 0
      
      !read all delivery ratio data
      inquire (file=in_delr%path, exist=i_exist)
      if (i_exist .or. in_delr%path /= "null") then
        do
          open (107,file=in_delr%path)
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
          
          db_mx%dr_path = imax
          
          allocate (dr_path(imax))
          do idr_path = 1, imax
            allocate (dr_path(idr_path)%path(cs_db%num_paths))
          end do
          allocate (dr_path_num(imax))
          allocate (dr_path_name(imax))
          rewind (107)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
      
          !read all delivery ratio data
          do ii = 1, db_mx%dr_path
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            backspace (107)
            read (107,*,iostat=eof) dr_path_name(ii), (dr_path(ii)%path(ipath), ipath = 1, cs_db%num_paths)   
            if (eof < 0) exit
          end do
          close (107)
          exit
        end do
      end if
                  
      ! xwalk with dr file to get sequential number
      do idr = 1, db_mx%dr
        do idr_path = 1, db_mx%dr_path
          if (dr_db(idr)%path_file == dr_path_name(idr_path)) then
            dr_path_num(idr) = idr_path
            exit
          end if
        end do
      end do
      
      !set dr object hydrograph
      ob1 = sp_ob1%dr
      ob2 = sp_ob1%dr + sp_ob%dr - 1
      do iob = ob1, ob2
        idr = ob(iob)%props
        idr_path = dr_path_num(idr)
        obcs(iob)%hd(1)%path = dr_path(idr_path)%path
      end do
      
      return
      end subroutine dr_path_read