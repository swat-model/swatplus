      subroutine exco_read_om
    
      use hydrograph_module
      use input_file_module
      use organic_mineral_mass_module
      use constituent_mass_module
      use maximum_data_module
      use exco_module

      implicit none
 
      character (len=80) :: titldum, header
      integer :: eof, imax, ob1, ob2
      logical :: i_exist              !none       |check to determine if file exists
      integer :: ii, iexco, iexco_om, iob

      eof = 0
      imax = 0
      
      !read all export coefficient data
      inquire (file=in_exco%om, exist=i_exist)
      if (i_exist .or. in_exco%om /= "null") then
        do
          open (107,file=in_exco%om)
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
          
          db_mx%exco_om = imax
          
          allocate (exco(0:imax))       !! change to exco_om         
          allocate (exco_om_num(0:imax))
          allocate (exco_om_name(0:imax))
          rewind (107)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
      
          !read all export coefficient data
          do ii = 1, db_mx%exco_om
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            backspace (107)
            read (107,*,iostat=eof) exco_om_name(ii), exco(ii)   
            if (eof < 0) exit
          end do
          close (107)
          exit
        end do
      end if
      
      ! xwalk with exco file to get sequential number
      do iexco = 1, db_mx%exco_om
        do iexco_om = 1, db_mx%exco_om
          if (exco_db(iexco)%om_file == exco_om_name(iexco_om)) then
            exco_om_num(iexco) = iexco_om
            exit
          end if
        end do
      end do
      
      !set exco_om object hydrograph
      ob1 = sp_ob1%exco
      ob2 = sp_ob1%exco + sp_ob%exco - 1
      do iob = ob1, ob2
        iexco = ob(iob)%props
        iexco_om = exco_om_num(iexco)
        ob(iob)%hd(1) = exco(iexco_om)
      end do
      
      return
      end subroutine exco_read_om