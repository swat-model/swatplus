      subroutine pest_cha_res_read
    
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
      logical :: i_exist              !none       |check to determine if file exists
      integer :: ipest, ipesti

      eof = 0
      
      !read all export coefficient data
      inquire (file=in_init%pest_water, exist=i_exist)
      if (i_exist .or. in_init%pest_water /= "null") then
        do
          open (107,file=in_init%pest_water)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          imax = 0
          do while (eof == 0)
            read (107,*,iostat=eof) titldum     !name
            if (eof < 0) exit
            do ipest = 1, cs_db%num_pests
              read (107,*,iostat=eof) titldum   !water
              if (eof < 0) exit
              read (107,*,iostat=eof) titldum   !benthic
              if (eof < 0) exit
            end do
            imax = imax + 1
          end do
          
          db_mx%pestw_ini = imax
          
          allocate (pest_water_ini(imax))
          allocate (pest_init_name(imax))

          do ipesti = 1, imax
            allocate (pest_water_ini(ipesti)%water(cs_db%num_pests))
            allocate (pest_water_ini(ipesti)%benthic(cs_db%num_pests))
          end do
          
          rewind (107)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          
          do ipesti = 1, imax
            read (107,*,iostat=eof) pest_init_name(ipesti)
            if (eof < 0) exit
              read (107,*,iostat=eof) titldum, pest_water_ini(ipesti)%water
              if (eof < 0) exit
              read (107,*,iostat=eof) titldum, pest_water_ini(ipesti)%benthic
              if (eof < 0) exit
          end do
          close (107)
          exit
        end do
      end if

      return
      end subroutine pest_cha_res_read