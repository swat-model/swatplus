      subroutine exco_read
    
      use hydrograph_module
      use input_file_module
      use organic_mineral_mass_module
      use maximum_data_module
      
      implicit none
 
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      character (len=16) :: namedum   !           |
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      integer :: ob1                  !none       |beginning of loop
      integer :: ob2                  !none       |ending of loop
      logical :: i_exist              !none       |check to determine if file exists
      integer :: ii                   !none       |counter
      integer :: iob                  !none       |counter
      integer :: iexco                !           |
      

      eof = 0
      imax = 0
      
      !read all export coefficient data
      inquire (file=in_exco%exco, exist=i_exist)
      if (i_exist .or. in_exco%exco /= 'null') then
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
          
          allocate (exco(0:imax))
          rewind (107)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
      
          !read all export coefficient data
          do ii = 1, db_mx%exco
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            backspace (107)
            read (107,*,iostat=eof) namedum, exco(ii)   
            if (eof < 0) exit
          end do
          close (107)
          exit
        end do
      end if
      
      !set exco object hydrograph
      ob1 = sp_ob1%exco
      ob2 = sp_ob1%exco + sp_ob%exco - 1
      do iob = ob1, ob2
        iexco = ob(iob)%props
        ob(iob)%hd(1) = exco(iexco)
      end do
      
      return
      end subroutine exco_read