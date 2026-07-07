      subroutine salt_cha_res_read
    
      use constituent_mass_module
      use input_file_module
      use maximum_data_module
      use channel_data_module
      use hydrograph_module
      use sd_channel_module
      use organic_mineral_mass_module
 
      implicit none
      
      character (len=80) :: titldum
      character (len=80) :: header
      integer :: eof, imax
      logical :: i_exist
      integer :: isalt
      integer :: isalti

      eof = 0
      
      !read all export coefficient data
      inquire (file=in_init%salt_water, exist=i_exist)
      if (i_exist .or. in_init%salt_water /= "null") then
        do
          open (107,file=in_init%salt_water)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          imax = 0
          do while (eof == 0)
            read (107,*,iostat=eof) titldum     !name
            if (eof < 0) exit
            do isalt = 1, cs_db%num_salts
              read (107,*,iostat=eof) titldum   !water
              if (eof < 0) exit
              read (107,*,iostat=eof) titldum   !benthic
              if (eof < 0) exit
            end do
            imax = imax + 1
          end do
          
          db_mx%saltw_ini = imax
          
          allocate (salt_water_ini(imax))
          allocate (salt_init_name(imax))

          do isalt = 1, imax
            allocate (salt_water_ini(isalt)%water(cs_db%num_salts+5))
            allocate (salt_water_ini(isalt)%benthic(cs_db%num_salts+5))
          end do
          
          rewind (107)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          
          do isalti = 1, imax
            read (107,*,iostat=eof) header
            if (eof < 0) exit
            read (107,*,iostat=eof) salt_init_name(isalti)
            if (eof < 0) exit
            read (107,*,iostat=eof) titldum, salt_water_ini(isalti)%water
            if (eof < 0) exit
            read (107,*,iostat=eof) titldum, salt_water_ini(isalti)%benthic
            if (eof < 0) exit
          end do
          close (107)
          exit
        end do
      end if

      return
      end subroutine salt_cha_res_read