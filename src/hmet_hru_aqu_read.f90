      subroutine hmet_hru_aqu_read
    
      use constituent_mass_module
      use input_file_module
      use maximum_data_module

      implicit none
 
      character (len=80) :: titldum
      character (len=80) :: header
      integer :: ihmet
      integer :: ihmeti
      integer :: eof, imax
      logical :: i_exist              !none       |check to determine if file exists
      integer :: ipest

      eof = 0
      
      !read all export coefficient data
      inquire (file=in_init%hmet_soil, exist=i_exist)
      if (i_exist .or. in_init%hmet_soil /= "null") then
        do
          open (107,file=in_init%hmet_soil)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          imax = 0
          do while (eof == 0)
            read (107,*,iostat=eof) header
            if (eof < 0) exit
            read (107,*,iostat=eof) titldum     !name
            if (eof < 0) exit
            do ihmet = 1, cs_db%num_metals
              read (107,*,iostat=eof) titldum
              if (eof < 0) exit
            end do
            imax = imax + 1
          end do
          
          db_mx%hmet_ini = imax
          
          allocate (hmet_soil_ini(imax))
          allocate (cs_hmet_solsor(cs_db%num_metals))
          
          do ihmet = 1, imax
            allocate (hmet_soil_ini(ihmet)%soil(cs_db%num_metals))
            allocate (hmet_soil_ini(ihmet)%plt(cs_db%num_metals))
          end do
           
          rewind (107)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit

          do ihmeti = 1, imax
            read (107,*,iostat=eof) header
            if (eof < 0) exit
            read (107,*,iostat=eof) hmet_soil_ini(ihmeti)%name
            if (eof < 0) exit
            do ipest = 1, cs_db%num_metals
              read (107,*,iostat=eof) titldum, hmet_soil_ini(ihmeti)%soil(ihmet)
              if (eof < 0) exit
              read (107,*,iostat=eof) titldum, hmet_soil_ini(ihmeti)%plt(ihmet)
              if (eof < 0) exit
            end do
          end do
          close (107)
          exit
        end do
      end if
      
      return
      end subroutine hmet_hru_aqu_read