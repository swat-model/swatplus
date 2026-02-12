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
      
      character (len=80) :: file = "" !           |filename
      character (len=80) :: titldum = ""!           |title of file
      character (len=80) :: header = "" !           |header of file
      character (len=12) :: first_word = ""
      character (len=4) :: salt_ion = ""!           |
      character (len=15) :: station_name = ""!       |
      integer :: eof = 0              !           |end of file
      integer :: iwst = 0             !           |weather station counter
      integer :: num_wth = 0          !           |number of weather stations
      integer :: iyr = 0              !           |counter
      logical :: i_exist              !none       |check to determine if file exists
      integer :: isalt = 0            !           |salt ion counter
      integer :: start_year = 0
      integer :: end_year = 0
      integer :: num_yr = 0
      
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
				read(5050,*) first_word,start_year
				read(5050,*) first_word,end_year
				num_yr = end_year - start_year + 1
        
				!number of weather stations
				num_wth = db_mx%wst
				
        !allocate arrays
        allocate(atmodep_salt(0:num_wth))
        
        !loop through the stations (num_sta is set in cli_read_atmodep subroutine)
        do iwst=1,num_wth
          
          !allocate arrays
          allocate(atmodep_salt(iwst)%salt(cs_db%num_salts))
          
          !yearly values
          read(5050,*) station_name !station name
					!wet (rain) deposition
          do isalt=1,cs_db%num_salts
            allocate(atmodep_salt(iwst)%salt(isalt)%rfyr(num_yr))
            read(5050,*) salt_ion,(atmodep_salt(iwst)%salt(isalt)%rfyr(iyr),iyr=1,num_yr)
          enddo 
					!dry deposition
          do isalt=1,cs_db%num_salts
            allocate(atmodep_salt(iwst)%salt(isalt)%dryyr(num_yr))
            read(5050,*) salt_ion,(atmodep_salt(iwst)%salt(isalt)%dryyr(iyr),iyr=1,num_yr)
          enddo 
          
        enddo !go to next station
      
      endif
      endif
      
      close(5050)
      
      return
    end subroutine cli_read_atmodep_salt
    