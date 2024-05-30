      subroutine path_cha_res_read
    
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
      integer :: ipathi
      integer :: eof, imax
      logical :: i_exist              !none       |check to determine if file exists
      integer :: ipath

      eof = 0
      
      !read all export coefficient data
      inquire (file=in_init%path_water, exist=i_exist)
      if (i_exist .or. in_init%path_water /= "null") then
        do
          open (107,file=in_init%path_water)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
            if (eof < 0) exit
          imax = 0
          do while (eof == 0)
            read (107,*,iostat=eof) titldum     !name
            if (eof < 0) exit
            do ipath = 1, cs_db%num_paths
              read (107,*,iostat=eof) titldum   !water
              if (eof < 0) exit
              read (107,*,iostat=eof) titldum   !benthic
              if (eof < 0) exit
            end do
            imax = imax + 1
          end do
          
          db_mx%pathw_ini = imax

          allocate (path_water_ini(imax))
          allocate (path_init_name(imax))

          do ipathi = 1, imax
            allocate (path_water_ini(ipathi)%water(cs_db%num_paths))
            allocate (path_water_ini(ipathi)%benthic(cs_db%num_paths))
          end do
          rewind (107)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          
          do ipathi = 1, imax
            read (107,*,iostat=eof) header
            if (eof < 0) exit
            read (107,*,iostat=eof) path_init_name(ipathi)
            if (eof < 0) exit
            read (107,*,iostat=eof) titldum, path_water_ini(ipathi)%water
            if (eof < 0) exit
            read (107,*,iostat=eof) titldum, path_water_ini(ipathi)%benthic
            if (eof < 0) exit
          end do
          close (107)
          exit
        end do
      end if

      return
      end subroutine path_cha_res_read