      !the purpose of this subroutine is to read constituent concentration data for soils and plants in HRUs
      subroutine cs_hru_read !rtb cs
    
      use constituent_mass_module
      use input_file_module
      use maximum_data_module
      
      implicit none
 
      character (len=80) :: titldum
      character (len=80) :: header
      integer :: ics
      integer :: eof, imax
      logical :: i_exist              !none       |check to determine if file exists

      eof = 0
      
      !read cs data for soils and plants
      inquire (file="cs_hru.ini", exist=i_exist)
      if (i_exist .or. "cs_hru.ini" /= "null") then
        do
          open (107,file="cs_hru.ini")
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          imax = 0
          do while (eof == 0)
            if (eof < 0) exit
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do
          
          db_mx%cs_ini = imax
          
          allocate (cs_soil_ini(imax))
          
          do ics = 1, imax
            allocate (cs_soil_ini(ics)%soil(cs_db%num_cs + cs_db%num_cs))
            allocate (cs_soil_ini(ics)%plt(cs_db%num_cs + cs_db%num_cs))
          end do
           
          rewind (107)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit

          do ics = 1, imax
            read (107,*,iostat=eof) cs_soil_ini(ics)%name
            if (eof < 0) exit
            read (107,*,iostat=eof) cs_soil_ini(ics)%soil
            if (eof < 0) exit
            read (107,*,iostat=eof) cs_soil_ini(ics)%plt
            if (eof < 0) exit
          end do
          close (107)
          exit
        end do
      end if
      
      return
      end subroutine cs_hru_read