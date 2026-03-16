      subroutine exco_read_om
    
      use hydrograph_module
      use input_file_module
      use organic_mineral_mass_module
      use constituent_mass_module
      use maximum_data_module
      use exco_module

      implicit none
 
      character (len=80) :: titldum = ""
      character (len=80) :: header = ""
      integer :: eof = 0
      integer :: imax = 0
      integer :: ob1 = 0
      integer :: ob2 = 0
      logical :: i_exist              !none       |check to determine if file exists
      integer :: ii = 0
      integer :: iexco = 0
      integer :: iexco_om = 0
      integer :: iob = 0

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
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          imax = 0
          do while (eof == 0)
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do
          
          db_mx%exco_om = imax
          
          allocate (exco(imax))       !! change to exco_om         
          allocate (exco_om_num(imax), source = 0)
          allocate (exco_om_name(imax))
          rewind (107)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
      
          !read all export coefficient data
          do ii = 1, db_mx%exco_om
            read (107,*,iostat=eof) exco_om_name(ii), exco(ii)   
            if (eof < 0) exit
          end do
          close (107)
          exit
        end do
      end if
      
      !set exco_om object hydrograph
      !ob1 = sp_ob1%exco
      !ob2 = sp_ob1%exco + sp_ob%exco - 1
      !do iob = ob1, ob2
      !  iexco = ob(iob)%props
      !  iexco_om = exco_om_num(iexco)
      !  ob(iob)%hd(1) = exco(iexco_om)
      !end do
      
      return
      end subroutine exco_read_om