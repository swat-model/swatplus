      subroutine dr_read_hmet
    
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
      integer :: idr_hmet, ii, ihmet, idr, iob


      eof = 0
      imax = 0
      
      !read all delivery ratio data
      inquire (file=in_delr%hmet, exist=i_exist)
      if (i_exist .or. in_delr%hmet /= "null") then
        do
          open (107,file=in_delr%hmet)
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
          
          db_mx%dr_hmet = imax
          
          allocate (dr_hmet(imax))
          do idr_hmet = 1, imax
            allocate (dr_hmet(idr_hmet)%hmet(cs_db%num_metals))
          end do
          allocate (dr_hmet_num(imax))
          allocate (dr_hmet_name(imax))
          rewind (107)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
      
          !read all export coefficient data
          do ii = 1, db_mx%dr_hmet
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            backspace (107)
            read (107,*,iostat=eof) dr_hmet_name(ii), (dr_hmet(ii)%hmet(ihmet), ihmet = 1, cs_db%num_metals)   
            if (eof < 0) exit
          end do
          close (107)
          exit
        end do
      end if
                  
      ! xwalk with dr file to get sequential number
      do idr = 1, db_mx%dr
        do idr_hmet = 1, db_mx%dr_hmet
          if (dr_db(idr)%hmet_file == dr_hmet_name(idr_hmet)) then
            dr_hmet_num(idr) = idr_hmet
            exit
          end if
        end do
      end do
      
      !set dr object hydrograph
      ob1 = sp_ob1%dr
      ob2 = sp_ob1%dr + sp_ob%dr - 1
      do iob = ob1, ob2
        idr = ob(iob)%props
        idr_hmet = dr_hmet_num(idr)
        obcs(iob)%hd(1)%hmet = dr_hmet(idr_hmet)%hmet
      end do
      
      return
      end subroutine dr_read_hmet