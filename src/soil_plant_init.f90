      subroutine soil_plant_init
    
      use hru_module
      use input_file_module
      use maximum_data_module
      use constituent_mass_module

      implicit none
 
      character (len=80) :: titldum
      character (len=80) :: header
      integer :: eof, imax
      logical :: i_exist              !none       |check to determine if file exists
      integer :: ii

      eof = 0
      
      !read all export coefficient data
      inquire (file=in_init%soil_plant_ini, exist=i_exist)
      if (i_exist .or. in_init%soil_plant_ini /= "null") then
        do
          open (107,file=in_init%soil_plant_ini)
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
          
          db_mx%sol_plt_ini = imax
          
          allocate (sol_plt_ini(imax))
          rewind (107)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          
          do ii = 1, imax
            read (107,*,iostat=eof) sol_plt_ini(ii)%name, sol_plt_ini(ii)%sw_frac, sol_plt_ini(ii)%nutc,  &
                sol_plt_ini(ii)%pestc, sol_plt_ini(ii)%pathc, sol_plt_ini(ii)%saltc, sol_plt_ini(ii)%hmetc
            if (eof < 0) exit
          end do
          close (107)
          exit
        end do
      end if
      
      return
      end subroutine soil_plant_init