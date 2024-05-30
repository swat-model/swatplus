      subroutine cli_read_atmodep_cs
      !read in wet and dry deposition values for constituents
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
      integer :: eof                  !           |end of file
      integer :: iadep                !           |counter
      integer :: imo                  !           |counter
      integer :: iyr                  !           |counter
      integer :: imo_atmo             !           |
      logical :: i_exist              !none       |check to determine if file exists
      integer :: iyrc_atmo            !           |
      integer :: ics                  !           |constituent counter
      real    :: station_name
      
      eof = 0

      !only proceed if there are constituents in the simulation
      if(cs_db%num_cs > 0) then
      
      inquire (file='cs_atmo.cli',exist=i_exist)
      if(i_exist) then
        cs_atmo = "y"
        
        !open the file; skip first two lines (commentary)
        open(5050,file='cs_atmo.cli')
        read(5050,*)
        read(5050,*)
        read(5050,*)
      
        !allocate arrays
        allocate(atmodep_cs(0:atmodep_cont%num_sta))
        
        !loop through the stations (num_sta is set in cli_read_atmodep subroutine)
        do iadep = 1, atmodep_cont%num_sta
          
          !allocate arrays
          allocate(atmodep_cs(iadep)%cs(cs_db%num_cs))
          
          !average annual values
          if (atmodep_cont%timestep == "aa") then
            read(5050,*) station_name !station name --> already read in cli_read_atmodep
            !wet (rainfall) concentrations (mg/L)
            do ics=1,cs_db%num_cs
              read(5050,*) atmodep_cs(iadep)%cs(ics)%rf
            enddo
            !dry deposition mass (kg/ha)
            do ics=1,cs_db%num_cs
              read(5050,*) atmodep_cs(iadep)%cs(ics)%dry
            enddo
          endif
          
          !monthly values
          if (atmodep_cont%timestep == "mo") then
            read(5050,*) station_name !station name
            !wet deposition
            do ics=1,cs_db%num_cs
              allocate(atmodep_cs(iadep)%cs(ics)%rfmo(atmodep_cont%num))
            enddo
            do ics=1,cs_db%num_cs
              read(5050,*) (atmodep_cs(iadep)%cs(ics)%rfmo(imo),imo=1,atmodep_cont%num)
            enddo
            !dry deposition
            do ics=1,cs_db%num_cs
              allocate(atmodep_cs(iadep)%cs(ics)%drymo(atmodep_cont%num))
            enddo
            do ics=1,cs_db%num_cs
              read(5050,*) (atmodep_cs(iadep)%cs(ics)%drymo(imo),imo=1,atmodep_cont%num)
            enddo
          end if
					
          !yearly values
          if (atmodep_cont%timestep == "yr") then
            read(5050,*) station_name !station name
            !wet deposition
            do ics=1,cs_db%num_cs
              allocate(atmodep_cs(iadep)%cs(ics)%rfyr(atmodep_cont%num)) 
            enddo
            do ics=1,cs_db%num_cs
              read(5050,*) (atmodep_cs(iadep)%cs(ics)%rfyr(iyr),iyr=1,atmodep_cont%num)
            enddo
            !dry deposition
            do ics=1,cs_db%num_cs
              allocate(atmodep_cs(iadep)%cs(ics)%dryyr(atmodep_cont%num))
            enddo
            do ics=1,cs_db%num_cs
              read(5050,*) (atmodep_cs(iadep)%cs(ics)%dryyr(iyr),iyr=1,atmodep_cont%num)
            enddo
          end if
          
        end do !go to next station
      
      endif
      endif
      
      close(5050)   
      
      return
      end subroutine cli_read_atmodep_cs