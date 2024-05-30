      module time_module
    
      implicit none

      !integer :: int_print = 1       !! current interval between daily prints
      character (len=25) :: cal_sim = " Original Simulation"
      real :: cal_adj = 0.0
      real :: yrs_print
      integer, dimension (13) :: ndays = (/0,31,60,91,121,152,182,213,244,274,305,335,366/)
      integer, dimension (13) :: ndays_leap = (/0,31,60,91,121,152,182,213,244,274,305,335,366/)
      integer, dimension (13) :: ndays_noleap = (/0,31,59,90,120,151,181,212,243,273,304,334,365/)
      integer, dimension (12) :: ndmo = (/0,0,0,0,0,0,0,0,0,0,0,0/)     !! cumulative number of days accrued in the
      !! month since the simulation began - the array location number is the number of the month

      
      type time_current
        character (len=1)  :: day_print = "n"
        integer :: day = 0            !! current day of simulation
        integer :: mo = 0             !! current month of simulation
        integer :: mo_start = 0       !! starting month
        integer :: yrc = 0            !! current calendar year
        integer :: yrc_start = 0      !! starting calendar year
        integer :: yrc_end = 0        !! ending calendar year
        integer :: yrs = 0            !! current sequential year
        integer :: day_mo = 0         !! day of month (1-31)  
        integer :: end_mo = 0         !! set to 1 if end of month
        integer :: end_yr = 0         !! set to 1 if end of year
        integer :: end_sim = 0        !! set to 1 if end of simulation
        integer :: end_aa_prt = 0     !! set to 1 if end of simulation
        integer :: day_start = 0      !! beginning julian day of simulation
        integer :: day_end_yr = 0     !! ending julian day of each year
        integer :: day_end = 0        !! input ending julian day of simulation
        integer :: nbyr = 3           !! number of years of simulation run
        integer :: step = 0           !! number of time steps in a day for rainfall, runoff and routing
                                      !! 0 = daily; 1=increment(12 hrs); 24=hourly; 96=15 mins; 1440=minute;
        real :: dtm = 0.              !! time step in minutes for rainfall, runoff and routing
        real :: days_prt = 0.         !! number of days for average annual printing for entire time period
        real :: yrs_prt = 0.          !! number of years for average annual printing for entire time period
        real :: yrs_prt_int = 0.      !! number of years for average annual printing for printing interval- pco%aa_yrs()
        integer :: num_leap = 0       !! number of leap years in sumulation for average annual printing
        integer :: prt_int_cur = 1    !! current average annual print interval
        integer :: yrc_tot = 0
      end type time_current
      type (time_current) :: time
      type (time_current) :: time_init

      !elapsed simulation time
      !real :: sim_start,sim_finish
      
      end module time_module