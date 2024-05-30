      subroutine cli_read_atmodep_salt
      !read in wet and dry deposition values for salt ions
      !(stations are the same as in atmo.cli input file)
  
      use basin_module
      use input_file_module
      use climate_module
      use time_module
      use maximum_data_module
      use constituent_mass_module
      
      implicit none
      
      character (len=80) :: file      !           |filename
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      character (len=4) :: salt_ion   !           |
      character (len=15) :: station_name	!       |
      integer :: eof                  !           |end of file
      integer :: iadep                !           |counter
      integer :: imo                  !           |counter
      integer :: iyr                  !           |counter
      integer :: imo_atmo             !           |
      logical :: i_exist              !none       |check to determine if file exists
      integer :: iyrc_atmo            !           |
      integer :: isalt                !           |salt ion counter
      
      eof = 0

      !only proceed if there are salt ions in the simulation
      if(cs_db%num_salts > 0) then
      
      inquire (file='salt_atmo.cli',exist=i_exist)
      if(i_exist) then
        salt_atmo = "y"
        
        !open the file; skip first two lines (commentary)
        open(5050,file='salt_atmo.cli')
        read(5050,*)
        read(5050,*)
        read(5050,*)
        read(5050,*)
        read(5050,*)
        read(5050,*)
      
        !allocate arrays
        allocate(atmodep_salt(0:atmodep_cont%num_sta))
        
        !loop through the stations (num_sta is set in cli_read_atmodep subroutine)
        do iadep = 1, atmodep_cont%num_sta
          
          !allocate arrays
          allocate(atmodep_salt(iadep)%salt(cs_db%num_salts))
          
          !average annual values
          if (atmodep_cont%timestep == "aa") then
            read(5050,*) station_name !station name --> already read in cli_read_atmodep
            !wet (rainfall) concentrations (mg/L)
            do isalt=1,cs_db%num_salts
              read(5050,*) salt_ion,atmodep_salt(iadep)%salt(isalt)%rf
            enddo 
            !dry deposition (kg/ha)
            do isalt=1,cs_db%num_salts
              read(5050,*) salt_ion,atmodep_salt(iadep)%salt(isalt)%dry
            enddo 
          endif
          
          !monthly values
          if (atmodep_cont%timestep == "mo") then
            read(5050,*) station_name !station name
            do isalt=1,cs_db%num_salts
              allocate(atmodep_salt(iadep)%salt(isalt)%rfmo(atmodep_cont%num))
              read(5050,*) salt_ion,(atmodep_salt(iadep)%salt(isalt)%rfmo(imo),imo=1,atmodep_cont%num)
            enddo 
            do isalt=1,cs_db%num_salts
              allocate(atmodep_salt(iadep)%salt(isalt)%drymo(atmodep_cont%num))
              read(5050,*) salt_ion,(atmodep_salt(iadep)%salt(isalt)%drymo(imo),imo=1,atmodep_cont%num)
            enddo 
          end if
					
          !yearly values
          if (atmodep_cont%timestep == "yr") then
            read(5050,*) station_name !station name
            do isalt=1,cs_db%num_salts
              allocate(atmodep_salt(iadep)%salt(isalt)%rfyr(atmodep_cont%num))
              read(5050,*) salt_ion,(atmodep_salt(iadep)%salt(isalt)%rfyr(iyr),iyr=1,atmodep_cont%num)
            enddo 
            do isalt=1,cs_db%num_salts
              allocate(atmodep_salt(iadep)%salt(isalt)%dryyr(atmodep_cont%num))
              read(5050,*) salt_ion,(atmodep_salt(iadep)%salt(isalt)%dryyr(iyr),iyr=1,atmodep_cont%num)
            enddo 
          end if
          
        end do !go to next station
      
      endif
      endif
      
      close(5050)
      
      return
      end subroutine cli_read_atmodep_salt