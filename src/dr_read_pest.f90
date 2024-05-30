      subroutine dr_read_pest
    
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
      integer :: idr_pest, ii, ipest, idr, iob

      eof = 0
      imax = 0
      
      !read all delivery ratio data
      inquire (file=in_delr%pest, exist=i_exist)
      if (i_exist .or. in_delr%pest /= "null") then
        do
          open (107,file=in_delr%pest)
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
          
          db_mx%dr_pest = imax
          
          allocate (dr_pest(imax))
          do idr_pest = 1, imax
            allocate (dr_pest(idr_pest)%pest(cs_db%num_pests))
          end do
          allocate (dr_pest_num(imax))
          allocate (dr_pest_name(imax))
          rewind (107)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
      
          !read all delivery ratio data
          do ii = 1, db_mx%dr_pest
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            backspace (107)
            read (107,*,iostat=eof) dr_pest_name(ii), (dr_pest(ii)%pest(ipest), ipest = 1, cs_db%num_pests)   
            if (eof < 0) exit
          end do
          close (107)
          exit
        end do
      end if
            
      ! xwalk with dr file to get sequential number
      do idr = 1, db_mx%dr
        do idr_pest = 1, db_mx%dr_pest
          if (dr_db(idr)%pest_file == dr_pest_name(idr_pest)) then
            dr_pest_num(idr) = idr_pest
            exit
          end if
        end do
      end do
      
      !set dr_pest object hydrograph
      ob1 = sp_ob1%dr
      ob2 = sp_ob1%dr + sp_ob%dr - 1
      do iob = ob1, ob2
        idr = ob(iob)%props
        idr_pest = dr_pest_num(idr)
        obcs(iob)%hd(1)%pest = dr_pest(idr_pest)%pest
      end do
      
      return
      end subroutine dr_read_pest