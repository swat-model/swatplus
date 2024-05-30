      subroutine dr_read_salt
    
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
      integer :: idr_salt, ii, isalt, idr, iob

      eof = 0
      imax = 0
      
      !read all export coefficient data
      inquire (file=in_delr%salt, exist=i_exist)
      if (i_exist .or. in_delr%salt /= "null") then
        do
          open (107,file=in_delr%salt)
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
          
          db_mx%dr_salt = imax
          
          allocate (dr_salt(imax))
          do idr_salt = 1, imax
            allocate (dr_salt(idr_salt)%salt(cs_db%num_salts))
          end do
          allocate (dr_salt_num(imax))
          allocate (dr_salt_name(imax))
          rewind (107)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
      
          !read all export coefficient data
          do ii = 1, db_mx%dr_salt
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            backspace (107)
            read (107,*,iostat=eof) dr_salt_name(ii), (dr_salt(ii)%salt(isalt), isalt = 1, cs_db%num_salts)   
            if (eof < 0) exit
          end do
          close (107)
          exit
        end do
      end if
            
      ! xwalk with dr file to get sequential number
      do idr = 1, db_mx%dr
        do idr_salt = 1, db_mx%dr_salt
          if (dr_db(idr)%salts_file == dr_salt_name(idr_salt)) then
            dr_salt_num(idr) = idr_salt
            exit
          end if
        end do
      end do
            !set exco object hydrograph
      ob1 = sp_ob1%dr
      ob2 = sp_ob1%dr + sp_ob%dr - 1
      do iob = ob1, ob2
        idr = ob(iob)%props
        idr_salt = dr_salt_num(idr)
        obcs(iob)%hd(1)%salt = dr_salt(idr_salt)%salt
      end do
      
      return
      end subroutine dr_read_salt