      !read in constituent concentrations for irrigation water originating outside the watershed (rtb cs)
      subroutine cs_irr_read 
    
      use constituent_mass_module
      use input_file_module
      use maximum_data_module
      
      implicit none
 
      character (len=80) :: titldum
      character (len=80) :: header
      integer :: icsi,ics
      integer :: eof, imax
      logical :: i_exist              !none       |check to determine if file exists

      eof = 0
      
      !read cs data for irrigation water originating from outside the watershed
      inquire (file="cs_irrigation", exist=i_exist)
      if (i_exist) then
        do
          open (107,file="cs_irrigation")
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          
          !count the number of names
          imax = 0
          do while (eof == 0)
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do
          db_mx%cs_ini = imax
          
          !allocate cs irrigation array
          allocate(cs_water_irr(imax))
          do ics=1,imax
            allocate(cs_water_irr(ics)%water(cs_db%num_cs))
          end do
           
          !read in values
          rewind (107)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          do icsi=1,imax
            read (107,*,iostat=eof) cs_water_irr(icsi)%name,cs_water_irr(icsi)%water
            if (eof < 0) exit
          end do
          close (107)
          exit
        end do
      end if
      
      return
      end subroutine cs_irr_read
      