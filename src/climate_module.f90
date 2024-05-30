      module climate_module
    
      implicit none
      
      integer :: ppet_ndays = 30                                !none          |number of days for precip/pet sum
      integer :: ppet_mce = 0                                   !none          |current element in precip/pet linked list
      real, dimension (:,:), allocatable :: frad                !none          |fraction of solar radiation occuring 
                                                                !              |during hour in day in HRU
      real, dimension (:,:), allocatable :: wgncur              !none          |parameter to predict the impact of precip on
                                                                !              |other weather attributes
      real, dimension (:,:), allocatable :: wgnold              !none          |previous value of wgncur(:,:)
      integer, dimension (:), allocatable :: elevp              !m             |elevation of precipitation gage station
      integer, dimension (:), allocatable :: elevt              !m             |elevation of temperature gage station
      integer, dimension (:), allocatable :: idg                !none          |array location of random number seed
                                                                !              |used for a given process
      integer, dimension (:,:), allocatable :: rndseed          !none          |random number generator seeds
      real, dimension (:), allocatable :: rnd2                  !none          |random number between 0.0 and 1.0
      real, dimension (:), allocatable :: rnd3                  !none          |random number between 0.0 and 1.0
      real, dimension (:), allocatable :: rnd8                  !none          |random number between 0.0 and 1.0
      real, dimension (:), allocatable :: rnd9                  !none          |random number between 0.0 and 1.0
      integer :: rndseed_cond = 748932582   ! random number seed for dtbl conditional
      real, dimension(:), allocatable :: co2y

      type weather_generator_db      
        real :: lat =  0.0                          !! degrees      |latitude of weather station used to compile data
        real :: long = 0.0                          !! degrees      |longitude of weather station 
        real :: elev = 0.0                          !!              |elevation of weather station used to compile weather generator data
        real :: rain_yrs = 10.0                     !! none         |number of years of recorded maximum 0.5h rainfall used to calculate values for rainhhmx(:)
        real, dimension (12) :: tmpmx               !! deg C        |avg monthly maximum air temperature
        real, dimension (12) :: tmpmn               !! deg C        |avg monthly minimum air temperature
        real, dimension (12) :: tmpstdmx            !! deg C        |standard deviation for avg monthly maximum air temperature 
        real, dimension (12) :: tmpstdmn            !! deg C        |standard deviation for avg monthly minimum air temperature
        real, dimension (12) :: pcpmm               !! mm           |amount of precipitation in month
        real, dimension (12) :: pcpstd              !! mm/day       |standard deviation for the average daily
        real, dimension (12) :: pcpskw              !! none         |skew coefficient for the average daily precipitation
        real, dimension (12) :: pr_wd               !! none         |probability of wet day after dry day in month 
        real, dimension (12) :: pr_ww               !! none         |probability of wet day after wet day in month
        real, dimension (12) :: pcpd                !! days         |average number of days of precipitation in the month
        real, dimension (12) :: rainhmx             !! mm           |maximum 0.5 hour rainfall in month
        real, dimension (12) :: solarav             !! MJ/m^2/day   |average daily solar radiation for the month
        real, dimension (12) :: dewpt               !! deg C        |average dew point temperature for the month
        real, dimension (12) :: windav              !! m/s          |average wind speed for the month
      end type weather_generator_db
      type (weather_generator_db), dimension(:),allocatable :: wgn
      type (weather_generator_db), dimension(:),allocatable :: wgn_orig

      type wgn_parms
        real, dimension (12) :: pr_wdays = 0.   !! none          |proportion of wet days in a month
        real, dimension (12) :: pcpmean = 0.    !! mm/day        |average amount of precipitation falling in one day for the month
        real :: daylmn = 0.                     !!               |minimum day length
        real :: daylth = 0.                     !!               |day length threshhold to trigger dormancy
        real :: latsin = 0.                     !!               |sine of latitude
        real :: latcos = 0.                     !!               |cosine of latitude
        real :: phutot = 0.                     !!               |total base zero heat units for year
        real :: pcpdays = 0.                    !!               |days of precip in year
        real :: tmp_an = 0.                     !!               |average annual air temperature
        real :: pcp_an = 0.                     !!               |average annual precipitation
        real :: ppet_an = 0.                    !!               |average annual precip/pet
        real :: precip_sum = 0.                 !!               |30 day sum of PET (mm)
        real :: pet_sum = 0.                    !!               |30 day sum of PRECIP (mm)
        real :: p_pet_rto = 0.                  !!               |30 day sum of PRECIP/PET ratio
        real, dimension (12) :: pcf = 0.        !!               |normalization factor for precipitation
        real, dimension (12) :: amp_r = 0.      !!               |alpha factor for rain(mo max 0.5h rain)
        real, dimension (12) :: pet             !!               |average monthly PET (mm)
        integer, dimension (:), allocatable :: mne_ppet          !!none          |next element in precip-pet linked list
        real, dimension (:), allocatable :: precip_mce           !!mm            |precip on current day of 30 day list 
        real, dimension (:), allocatable :: pet_mce              !!mm            |pet on current day of 30 day list 
        integer :: ireg = 1                     !!               |annual precip category-1 <= 508 mm; 2 > 508 and <= 1016 mm; 3 > 1016 mm/yr
        integer :: idewpt = 0                   !!               |0=dewpoint; 1=rel humididty input
      end type wgn_parms
      type (wgn_parms), dimension(:),allocatable :: wgn_pms
          
      type wind_direction_db
        character(len=16) :: name = "default-uniform"
        real, dimension (12,16) :: dir = 1.     !! 1-16         |avg monthly wind direstion
      end type wind_direction_db
      type (wind_direction_db), dimension(:),allocatable :: wnd_dir
      
      type weather_daily
        real :: precip
        real :: precip_next = 0.                            !! mm           |precip generated for next day
        real :: tmax
        real :: tmin
        real :: tave
        real :: solrad
        real :: solradmx
        real :: rhum
        real :: dewpt
        real :: windsp
        real :: pet
        real :: wndir
        real :: phubase0                                    !! deg C        |cumulative base 0 heat units
        real :: ppet                                        !! mm/mm        |climatic moisture index - cumulative p/pet
        real :: daylength                                   !! hr           |day length
        real :: precip_half_hr                              !! frac         |fraction of total rainfall on day that occurs
                                                            !!              |during 0.5h highest intensity rainfall
        character(len=3) :: precip_prior_day = "dry"        !!              |"dry" or "wet"
        real, dimension(:), allocatable :: ts               !! mm           |subdaily precip - current day
        real, dimension(:), allocatable :: ts_next          !! mm           |subdaily precip - next day
      end type weather_daily
      type (weather_daily) :: w
            
      type weather_codes_station
        integer :: wgn = 1        !!  weather generator station number
        integer :: pgage = 0      !!  gage number for rainfall (sim if generating)
        integer :: tgage = 0      !!  gage number fo temperature (sim if generating)
        integer :: sgage = 0      !!  gage number for solar radiation (sim if generating) 
        integer :: hgage = 0      !!  gage number for relative humidity (sim if generating)
        integer :: wgage = 0      !!  gage number for windspeed (sim if generating)
        integer :: petgage = 0      !!  number of pet gage files used in sim
        integer :: atmodep = 0    !!  atmospheric depostion data file locator
      end type weather_codes_station
      
      type weather_codes_station_char
        !character (len=50) ::  wst = ""      !!  weather station name
        character (len=50) ::  wgn = ""       !!  weather generator name
        character (len=50) :: pgage = ""      !!  gage name for rainfall 
        character (len=50) :: tgage = ""      !!  gage name for temperature
        character (len=50) :: sgage = ""      !!  gage name for solar radiation
        character (len=50) :: hgage = ""      !!  gage name for relative humidity
        character (len=50) :: wgage = ""      !!  gage name for windspeed
        character (len=50) :: petgage = ""    !!  name of pet gage ?
        character (len=50) :: atmodep = ""    !!  atmospheric depostion data file locator
      end type weather_codes_station_char

      type weather_station
        character(len=50) :: name = "Farmer Branch IL"
        real :: lat                         ! degrees    |latitude
        type (weather_codes_station_char) :: wco_c
        type (weather_codes_station) :: wco 
        type (weather_daily) :: weat
        real :: precip_aa = 0.              ! mm         |average annual precipitation
        real :: pet_aa = 0.                 ! mm         |average annual potential ET
        integer :: pcp_ts = 0               ! 1/day      |precipitation time steps per day (0 or 1 = daily)
        real, dimension(12) :: rfinc = 0    ! deg C      |monthly precipitation adjustment
        real, dimension(12) :: tmpinc = 0   ! deg C      |monthly temperature adjustment
        real, dimension(12) :: radinc = 0   ! MJ/m^2     |monthly solar radiation adjustment
        real, dimension(12) :: huminc = 0   ! none       |monthly humidity adjustment
        real, dimension(:), allocatable :: tlag     ! deg C      |daily average temperature for channel temp lag
        real :: airlag_temp                 ! deg C      |average temperature w_temp%airlag_d days ago
        integer :: tlag_mne = 1             !            |next element (day) for the air temp linked list
      end type weather_station
      type (weather_station), dimension(:),allocatable :: wst
         
      type climate_change_variables
        character(len=50) :: name = "Increment or Scenario"
        integer :: ref_yr                   ! none       |reference year to begin incremental adjustments
        real :: co2inc = 0                  ! ppm        |annual CO2 increment
        real, dimension(12) :: rfinc = 0    ! deg C      |monthly precipitation annual increment
        real, dimension(12) :: tmpinc = 0   ! deg C      |monthly temperature annual increment
        real, dimension(12) :: radinc = 0   ! MJ/m^2     |monthly solar radiation annual increment
        real, dimension(12) :: huminc = 0   ! none       |monthly humidity annual increment
        real :: co2scen = 0                 ! ppm        |annual CO2 scenario adjustment
        real, dimension(12) :: rfscen = 0   ! deg C      |monthly precipitation scenario adjustment
        real, dimension(12) :: tmpscen = 0  ! deg C      |monthly temperature scenario adjustment
        real, dimension(12) :: radscen = 0  ! MJ/m^2     |monthly solar radiation scenario adjustment
        real, dimension(12) :: humscen = 0  ! none       |monthly humidity scenario adjustment
      end type climate_change_variables
         
      type climate_measured_data
        character (len=50) :: filename
        real :: lat                         !! latitude of raingage         
        real :: long                        !! longitude of raingage
        real :: elev                        !! elevation of raingage
        integer :: nbyr                     !! number of years of daily rainfall
        integer :: tstep                    !! timestep of precipitation  
        
        integer :: days_gen = 0             !! number of missing days - generated 
        integer :: yrs_start = 1            !! number of years of simulation before record starts
        
        integer :: start_day                !! daily precip start julian day
        integer :: start_yr                 !! daily precip start year
        integer :: end_day                  !! daily precip end julian day
        integer :: end_yr                   !! daily precip end year
        real, dimension (12) :: mean_mon    !! same as variable unit        |mean monthly measured value
        real, dimension (12) :: max_mon     !! same as variable unit        |maximum monthly measured value
        real, dimension (12) :: min_mon     !! same as variable unit        |minimum monthly measured value
        
        real, dimension (:,:), allocatable :: ts
        real, dimension (:,:), allocatable :: ts2
        real, dimension (:,:,:), allocatable :: tss
      end type climate_measured_data
      type (climate_measured_data), dimension(:), allocatable :: pcp
      type (climate_measured_data), dimension(:), allocatable :: tmp    
      type (climate_measured_data), dimension(:), allocatable :: slr
      type (climate_measured_data), dimension(:), allocatable :: hmd
      type (climate_measured_data), dimension(:), allocatable :: wnd
      type (climate_measured_data), dimension(:), allocatable :: petm
      
      type atmospheric_deposition
        real :: nh4_rf = 1.         !! ave annual ammonia in rainfall - mg/l
        real :: no3_rf = .2         !! ave annual nitrate in rainfall - mg/l
        real :: nh4_dry = 0.        !! ave annual ammonia dry deposition - kg/ha/yr
        real :: no3_dry = 0.        !! ave annual nitrate dry deposition - kg/ha/yr
        character(len=50) :: name
        real, dimension(:), allocatable :: nh4_rfmo
        real, dimension(:), allocatable :: no3_rfmo
        real, dimension(:), allocatable :: nh4_drymo
        real, dimension(:), allocatable :: no3_drymo
        real, dimension(:), allocatable :: nh4_rfyr
        real, dimension(:), allocatable :: no3_rfyr
        real, dimension(:), allocatable :: nh4_dryyr 
        real, dimension(:), allocatable :: no3_dryyr     
      end type atmospheric_deposition
      type (atmospheric_deposition),dimension(:), allocatable :: atmodep
 
      type atmospheric_deposition_control
        integer :: num_sta = 0
        character(len=2) :: timestep
        integer :: ts = 0
        integer :: mo_init = 0
        integer :: yr_init = 0
        integer :: num = 0
        integer :: first = 1
      end type atmospheric_deposition_control
      type (atmospheric_deposition_control), save :: atmodep_cont
      
      !rtb salt / rtb cs
      character(len=1) :: salt_atmo = "n"
      character(len=1) :: cs_atmo = "n"
      
      type atmospheric_deposition_cs
        real :: rf                                  !! concentration in rainfall - mg/l
        real :: dry                                 !! dry deposition - kg/ha/yr
        real, dimension(:), allocatable :: rfmo
        real, dimension(:), allocatable :: drymo
        real, dimension(:), allocatable :: rfyr
        real, dimension(:), allocatable :: dryyr 
      end type atmospheric_deposition_cs
      type object_deposition_cs
        type (atmospheric_deposition_cs), dimension (:), allocatable :: salt
        type (atmospheric_deposition_cs), dimension (:), allocatable :: cs
      end type object_deposition_cs
      type (object_deposition_cs), dimension (:), allocatable :: atmodep_salt
      type (object_deposition_cs), dimension (:), allocatable :: atmodep_cs
      
      !rtb salt
      type road_salt
        real :: road                                 !! ave annual salt ion loading via road salt application (kg/ha)
        real, dimension(:,:), allocatable :: roadday !! daily salt ion loading via road salt application (kg/ha)
        real, dimension(:), allocatable :: roadmo    !! monthly salt ion loading via road salt application (kg/ha)
        real, dimension(:), allocatable :: roadyr    !! yearly salt ion loading via road salt application (kg/ha)
      end type road_salt
      type object_road_salt
        type (road_salt), dimension (:), allocatable :: salt
      end type object_road_salt
      type (object_road_salt), dimension (:), allocatable :: rdapp_salt !applied road salt
      
      character(len=50), dimension(:), allocatable :: wst_n
      character(len=50), dimension(:), allocatable :: wgn_n
      character(len=50), dimension(:), allocatable :: pcp_n
      character(len=50), dimension(:), allocatable :: tmp_n 
      character(len=50), dimension(:), allocatable :: slr_n
      character(len=50), dimension(:), allocatable :: hmd_n
      character(len=50), dimension(:), allocatable :: wnd_n   
      character(len=50), dimension(:), allocatable :: atmo_n
      character(len=50), dimension(:), allocatable :: petm_n
          
      end module climate_module