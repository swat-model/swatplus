      subroutine soil_plant_init_cs !rtb salt/cs
    
      use hru_module
      use input_file_module
      use maximum_data_module
      use constituent_mass_module

      implicit none
 
      character (len=80) :: titldum
      character (len=80) :: header
      integer :: eof
      logical :: i_exist              !none       |check to determine if file exists
      integer :: ii

      eof = 0
      
      !read soil_plant.ini_cs
      inquire (file="soil_plant.ini_cs", exist=i_exist)
      if(i_exist) then
        open (107,file="soil_plant.ini_cs")
        read (107,*,iostat=eof) titldum
        read (107,*,iostat=eof) header
        allocate (sol_plt_ini_cs(db_mx%sol_plt_ini))
        do ii = 1, db_mx%sol_plt_ini
          read (107,*,iostat=eof) sol_plt_ini_cs(ii)%name,sol_plt_ini_cs(ii)%pestc,sol_plt_ini_cs(ii)%pathc, &
                                  sol_plt_ini_cs(ii)%saltc,sol_plt_ini_cs(ii)%hmetc,sol_plt_ini_cs(ii)%csc
        end do
        close(107)
      end if
      
      return
      end subroutine soil_plant_init_cs