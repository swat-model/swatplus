      subroutine salt_roadsalt_read
      !read in road salt loadings (kg/ha)
  
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
      integer :: imonth               !           |month counter
      integer :: year_index,yr_weat,year_days,iday,day_flag(366),dum
      real    :: day_precip,day_temp,year_precip,day_fraction
      
      
      eof = 0

      !only proceed if there are salt ions in the simulation
      if(cs_db%num_salts > 0) then
      
      inquire (file='salt_road',exist=i_exist)
      if(i_exist) then
        
        !open the file; skip first two lines (commentary)
        open(5051,file='salt_road')
        read(5051,*)
        read(5051,*)
        read(5051,*)
        read(5051,*)
      
        !allocate arrays
        allocate(rdapp_salt(0:atmodep_cont%num_sta))

        !loop through the stations (num_sta is set in cli_read_atmodep subroutine)
        do iadep = 1, atmodep_cont%num_sta
          
          !allocate arrays
          allocate(rdapp_salt(iadep)%salt(cs_db%num_salts))
          
          !average annual values
          if (atmodep_cont%timestep == "aa") then
            read(5051,*) station_name !station name --> already read in cli_read_atmodep
            do isalt=1,cs_db%num_salts
              read(5051,*) salt_ion,rdapp_salt(iadep)%salt(isalt)%road
            enddo  
          endif
          
          !monthly values
          if (atmodep_cont%timestep == "mo") then
            read(5051,*) station_name !station name
            do isalt=1,cs_db%num_salts
              allocate(rdapp_salt(iadep)%salt(isalt)%roadmo(atmodep_cont%num))
              read(5051,*) salt_ion,(rdapp_salt(iadep)%salt(isalt)%roadmo(imo),imo=1,atmodep_cont%num)
            enddo 
          end if
					
          !yearly values
          if (atmodep_cont%timestep == "yr") then
            read(5051,*) station_name !station name
            do isalt=1,cs_db%num_salts
              allocate(rdapp_salt(iadep)%salt(isalt)%roadyr(atmodep_cont%num))
              read(5051,*) salt_ion,(rdapp_salt(iadep)%salt(isalt)%roadyr(iyr),iyr=1,atmodep_cont%num)
            enddo 
            !loop through the years, partitioning annual salt load to daily salt loadings
            !(based on precipitation and temperature)
            do isalt=1,cs_db%num_salts
              allocate(rdapp_salt(iadep)%salt(isalt)%roadday(366,atmodep_cont%num))
              rdapp_salt(iadep)%salt(isalt)%roadday = 0.
						enddo
            year_index = atmodep_cont%yr_init
            yr_weat = 1 !weather year index
            do iyr=1,atmodep_cont%num
              !number of days in the year
              if(mod(year_index,4) == 0) then
                year_days = 366
              else
                year_days = 365
              endif
              if(year_index .ge. time%yrc_start) then
                year_precip = 0.
                day_flag = 0
                !first, determine days of snow, and total precipitation on days of snow
                do iday=1,year_days
                  !determine if there is snow (likely) 
                  day_precip = pcp(iadep)%ts(iday,yr_weat) !mm
                  day_temp = (tmp(iadep)%ts(iday,yr_weat) + tmp(iadep)%ts2(iday,yr_weat)) / 2 !average daily temp (oC)
                  if(day_temp.ne.-99) then
                    if(day_precip > 0 .and. day_temp < 0) then
                      day_flag(iday) = 1
                      year_precip = year_precip + day_precip !mm
                    endif
                  endif
                enddo
                !second, calculate daily salt loading (using  fractions (fraction of yearly snow that falls on each day)
                do iday=1,year_days
                  if(day_flag(iday) == 1) then
                    day_fraction = pcp(iadep)%ts(iday,yr_weat) / year_precip
                    do isalt=1,cs_db%num_salts
                      rdapp_salt(iadep)%salt(isalt)%roadday(iday,iyr) = rdapp_salt(iadep)%salt(isalt)%roadyr(iyr) * day_fraction !kg/ha
                    enddo
                  endif
                enddo
                yr_weat = yr_weat + 1
              else !no precipitation data --> set daily values to 0
                do iday=1,year_days
                  do isalt=1,cs_db%num_salts
                    rdapp_salt(iadep)%salt(isalt)%roadday(iday,iyr) = 0.
                  enddo
                enddo
              endif
              year_index = year_index + 1
            enddo 
          end if
          
        end do !go to next station
      
      endif
      endif
           
      return
      end subroutine salt_roadsalt_read