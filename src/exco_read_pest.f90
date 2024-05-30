      subroutine exco_read_pest
    
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
      integer :: iexco_pest, ii, ipest, iexco, iob
      eof = 0
      imax = 0
      
      !read all export coefficient data
      inquire (file=in_exco%pest, exist=i_exist)
      if (i_exist .or. in_exco%pest /= "null") then
        do
          open (107,file=in_exco%pest)
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
          
          db_mx%exco_pest = imax
          
          allocate (exco_pest(imax))
          do iexco_pest = 1, imax
            allocate (exco_pest(iexco_pest)%pest(cs_db%num_pests))
          end do
          allocate (exco_pest_num(imax))
          allocate (exco_pest_name(imax))
          rewind (107)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
      
          !read all pesticide export coefficient data
          do ii = 1, db_mx%exco_pest
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            backspace (107)
            read (107,*,iostat=eof) exco_pest_name(ii), (exco_pest(ii)%pest(ipest), ipest = 1, cs_db%num_pests)   
            if (eof < 0) exit
          end do
          close (107)
          exit
        end do
      end if
            
      ! xwalk with exco file to get sequential number
      do iexco = 1, db_mx%exco
        do iexco_pest = 1, db_mx%exco_pest
          if (exco_db(iexco)%pest_file == exco_pest_name(iexco_pest)) then
            exco_pest_num(iexco) = iexco_pest
            exit
          end if
        end do
      end do
      
      ! set exco_pest object hydrograph
      ob1 = sp_ob1%exco
      ob2 = sp_ob1%exco + sp_ob%exco - 1
      do iob = ob1, ob2
        iexco = ob(iob)%props
        if (exco_db(iexco)%pest_file == "null") then
          obcs(iob)%hd(1)%pest = 0.
        else
          iexco_pest = exco_pest_num(iexco)
          obcs(iob)%hd(1)%pest = exco_pest(iexco_pest)%pest
        end if
      end do
      
      return
      end subroutine exco_read_pest