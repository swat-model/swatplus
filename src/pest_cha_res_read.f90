      subroutine pest_cha_res_read
    
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
      integer :: eof = 0
      integer :: imax = 0
      logical :: i_exist              !none       |check to determine if file exists
      integer :: ipest = 0
      integer :: ipesti = 0
      character (len=16) :: pname = ""
      integer :: irow = 0
      real :: wat = 0.
      real :: ben = 0.

      eof = 0
      
      !read all export coefficient data
      !! the layout the SWAT+ Editor writes: a header, then each set as its name and one
      !! 'pesticide water benthic' row per simulated pesticide
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
              read (107,*,iostat=eof) titldum   !pesticide water benthic
              if (eof < 0) exit
            end do
            imax = imax + 1
          end do
          
          db_mx%pestw_ini = imax
          
          allocate (pest_water_ini(imax))
          allocate (pest_init_name(imax))

          do ipesti = 1, imax
            allocate (pest_water_ini(ipesti)%water(cs_db%num_pests), source = 0.)
            allocate (pest_water_ini(ipesti)%benthic(cs_db%num_pests), source = 0.)
          end do
          
          rewind (107)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          
          do ipesti = 1, imax
            read (107,*,iostat=eof) pest_init_name(ipesti)
            if (eof < 0) exit
            !! cs_db%num_pests rows per set, stored by pesticide name; a row for a pesticide not simulated stores nothing
            do irow = 1, cs_db%num_pests
              read (107,*,iostat=eof) pname, wat, ben
              if (eof < 0) exit
              if (eof > 0) cycle
              do ipest = 1, cs_db%num_pests
                if (cs_db%pests(ipest) == pname) then
                  pest_water_ini(ipesti)%water(ipest) = wat
                  pest_water_ini(ipesti)%benthic(ipest) = ben
                end if
              end do
            end do
          end do
          close (107)
          exit
        end do
      end if

      return
      end subroutine pest_cha_res_read