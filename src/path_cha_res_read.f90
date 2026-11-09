      subroutine path_cha_res_read
    
      use constituent_mass_module
      use input_file_module
      use maximum_data_module
      use channel_data_module
      use hydrograph_module
      use sd_channel_module
      use organic_mineral_mass_module

      implicit none
 
      character (len=80) :: titldum = ""
      character (len=80) :: header = ""
      character (len=16) :: pname = ""
      integer :: ipathi = 0
      integer :: eof = 0
      integer :: imax = 0
      logical :: i_exist              !none       |check to determine if file exists
      integer :: ipath = 0
      integer :: irow = 0
      real :: wat = 0.
      real :: ben = 0.

      eof = 0
      
      !read all export coefficient data
      !! the layout the SWAT+ Editor writes: a header, then each set as its name and one
      !! 'pathogen water benthic' row per simulated pathogen
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
              read (107,*,iostat=eof) titldum   !pathogen water benthic
              if (eof < 0) exit
            end do
            imax = imax + 1
          end do
          
          db_mx%pathw_ini = imax

          allocate (path_water_ini(imax))
          allocate (path_init_name(imax))

          do ipathi = 1, imax
            allocate (path_water_ini(ipathi)%water(cs_db%num_paths), source = 0.)
            allocate (path_water_ini(ipathi)%benthic(cs_db%num_paths), source = 0.)
          end do
          rewind (107)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          
          do ipathi = 1, imax
            read (107,*,iostat=eof) path_init_name(ipathi)
            if (eof < 0) exit
            !! each row is stored by its pathogen's name; a row for a pathogen that is not simulated is skipped
            do irow = 1, cs_db%num_paths
              read (107,*,iostat=eof) pname, wat, ben
              if (eof < 0) exit
              if (eof > 0) cycle
              do ipath = 1, cs_db%num_paths
                if (cs_db%paths(ipath) == pname) then
                  path_water_ini(ipathi)%water(ipath) = wat
                  path_water_ini(ipathi)%benthic(ipath) = ben
                end if
              end do
            end do
          end do
          close (107)
          exit
        end do
      end if

      return
      end subroutine path_cha_res_read