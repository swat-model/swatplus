      subroutine salt_roadsalt_read
      !read in road salt loadings (kg/ha)
  
      use basin_module
      use input_file_module
      use climate_module
      use time_module
      use maximum_data_module
      use constituent_mass_module
      
      implicit none
      
      character (len=80) :: file = "" !           |filename
      character (len=80) :: titldum = ""!           |title of file
      character (len=80) :: header = "" !           |header of file
      character (len=12) :: first_word = ""
      character (len=4) :: salt_ion = ""!           |
      character (len=15) :: station_name = "" !       |
      integer :: eof = 0              !           |end of file
      integer :: iwst = 0             !           |weather station counter
      integer :: num_wth = 0          !           |number of weather stations
      integer :: iyr = 0              !           |counter       !           |
      logical :: i_exist              !none       |check to determine if file exists
      integer :: isalt = 0            !           |salt ion counter
      integer :: year_index = 0
      integer :: yr_weat = 0
      integer :: year_days = 0
      integer :: iday = 0
      integer :: day_flag(366) = 0
      integer :: dum = 0
      integer :: start_year = 0
      integer :: end_year = 0
      integer :: num_yr = 0
      real    :: day_precip = 0.
      real    :: day_temp = 0.
      real    :: year_precip = 0.
      real    :: day_fraction = 0.
      
      
      eof = 0

      !only proceed if there are salt ions in the simulation
      if(cs_db%num_salts > 0) then
      
      inquire (file='salt_road',exist=i_exist)
      if(i_exist) then
        
		!set flag
		salt_road_flag = 1
			
        !open the file; skip first two lines (commentary)
        open(5051,file='salt_road')
        read(5051,*)
        read(5051,*)
        read(5051,*) first_word,start_year
				read(5051,*) first_word,end_year
				num_yr = end_year - start_year
      
				!number of weather stations
				num_wth = db_mx%wst
				
        !allocate arrays
        allocate(rdapp_salt(0:num_wth))

        !loop through the weather stations (listed in weather-sta.cli)
        do iwst=1,num_wth
          
          !allocate arrays
          allocate(rdapp_salt(iwst)%salt(cs_db%num_salts))
          
          !yearly values
          read(5051,*) station_name !station name
          do isalt=1,cs_db%num_salts
            allocate(rdapp_salt(iwst)%salt(isalt)%roadyr(num_yr))
            read(5051,*) salt_ion,(rdapp_salt(iwst)%salt(isalt)%roadyr(iyr),iyr=1,num_yr)
          enddo 
          !loop through the years, partitioning annual salt load to daily salt loadings
          !(based on precipitation and temperature)
          do isalt=1,cs_db%num_salts
            allocate(rdapp_salt(iwst)%salt(isalt)%roadday(366,num_yr))
            rdapp_salt(iwst)%salt(isalt)%roadday = 0.
					enddo
          yr_weat = 1
          year_index = start_year
          do iyr=1,num_yr
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
                !determine if there is snow (likely, based on precipitation and temperature) 
                day_precip = pcp(iwst)%ts(iday,yr_weat) !mm
                day_temp = (tmp(iwst)%ts(iday,yr_weat) + tmp(iwst)%ts2(iday,yr_weat)) / 2 !average daily temp (oC)
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
                  day_fraction = pcp(iwst)%ts(iday,yr_weat) / year_precip
                  do isalt=1,cs_db%num_salts
                    rdapp_salt(iwst)%salt(isalt)%roadday(iday,yr_weat) = rdapp_salt(iwst)%salt(isalt)%roadyr(yr_weat) * day_fraction !kg/ha
                  enddo
                endif
              enddo
			        yr_weat = yr_weat + 1
            else !no precipitation data --> set daily values to 0
              do iday=1,year_days
                do isalt=1,cs_db%num_salts
                  rdapp_salt(iwst)%salt(isalt)%roadday(iday,iyr) = 0.
                enddo
              enddo
            endif
            year_index = year_index + 1
          enddo 
          
        enddo !go to next weather station
      
      endif
      endif
           
      return
    end subroutine salt_roadsalt_read
    