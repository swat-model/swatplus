      subroutine pest_hru_aqu_read
    
      use constituent_mass_module
      use input_file_module
      use maximum_data_module
 
      implicit none 
        
      character (len=80) :: titldum
      character (len=80) :: header
      integer :: eof, imax
      integer :: ipest
      integer :: ipesti
      logical :: i_exist              !none       |check to determine if file exists

      eof = 0
      
      !read all export coefficient data
      inquire (file=in_init%pest_soil, exist=i_exist)
      if (i_exist .or. in_init%pest_soil /= "null") then
        do
          open (107,file=in_init%pest_soil)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          imax = 0
          do while (eof == 0)
            read (107,*,iostat=eof) header
            if (eof < 0) exit
            read (107,*,iostat=eof) titldum     !name
            if (eof < 0) exit
            do ipest = 1, cs_db%num_pests
              read (107,*,iostat=eof) titldum
              if (eof < 0) exit
            end do
            imax = imax + 1
          end do
          
          db_mx%pest_ini = imax
          
          allocate (pest_soil_ini(imax))
          allocate (cs_pest_solsor(cs_db%num_pests))
          
          do ipest = 1, imax
            allocate (pest_soil_ini(ipest)%soil(cs_db%num_pests))
            allocate (pest_soil_ini(ipest)%plt(cs_db%num_pests))
          end do
          
          rewind (107)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          
          do ipesti = 1, imax
            read (107,*,iostat=eof) pest_soil_ini(ipesti)%name
            do ipest = 1, cs_db%num_pests
              read (107,*,iostat=eof) titldum, pest_soil_ini(ipesti)%soil(ipest), &
                                            pest_soil_ini(ipesti)%plt(ipest)
              if (eof < 0) exit
            end do
          end do
          close (107)
          exit
        end do
      end if
      
      return
      end subroutine pest_hru_aqu_read