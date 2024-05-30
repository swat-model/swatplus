      subroutine ch_rtpath
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine routes bacteria through the stream network

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name             |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hrchwtr(:)       |m^3 H2O     |water stored in reach at beginning of hour
!!    rch_bactlp(:)    |# cfu/100ml |less persistent bacteria stored in reach
!!    rch_bactp(:)     |# cfu/100ml |persistent bacteria stored in reach
!!    rchwtr           |m^3 H2O     |water stored in reach at beginning of day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hbactlp(:)   |# cfu/100mL  |less persistent bacteria in reach/outflow
!!                               |during hour
!!    hbactp(:)    |# cfu/100mL  |persistent bacteria in reach/outflow during
!!                               |hour
!!    rch_bactlp(:)|# cfu/100ml  |less persistent bacteria in reach/outflow
!!                               |at end of day
!!    rch_bactp(:) |# cfu/100ml  |persistent bacteria in reach/outflow at end
!!                               |of day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |counter
!!    initlp      |# cfu/100mL   |bacteria concentration in reach at beginning
!!                               |of hour (less persistent)
!!    initp       |# cfu/100mL   |bacteria concentration in reach at beginning
!!                               |of hour (persistent)
!!    jrch        |none          |reach number
!!    netwtr      |m^3 H2O       |net amount of water in reach during time step
!!    tday        |day           |routing time for the reach
!!    totbactlp   |10^4 cfu      |mass less persistent bacteria
!!    totbactp    |10^4 cfu      |mass persistent bacteria
!!    wtmp        |deg C         |temperature of water in reach
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Max
!!    SWAT: Theta

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module
      use time_module
      use pathogen_data_module
      use channel_module
      use hydrograph_module, only : ob
      use climate_module
      use constituent_mass_module
      
      implicit none

      real, external :: Theta  !units         |description        
      real :: path_tot         !10^4 cfu      |mass persistent bacteria
      real :: netwtr           !m^3 H2O       |net amount of water in reach during time step
      real :: tday             !day           |routing time for the reach
      real :: wtmp             !deg C         |temperature of water in reach
      real :: rchwtr           !m^3 H2O       |water stored in reach at beginning of day
      integer :: iwst          !units         |description
      integer :: ipath         !none          |pathogen counter
      integer :: jrch          !none          |reach number
      integer :: iob           !none          |
      integer :: icmd          !none          |

      if (rtwtr > 0. .and. rchdep > 0.) then

        wtmp = 5.0 + 0.75 * wst(iwst)%weat%tave
        if (wtmp <= 0.) wtmp = 0.1

        do ipath = 1, cs_db%num_paths
          !isp_ini = hru(ihru)%dbs%soil_plant_init
          !ipath_db = sol_plt_ini(isp_ini)%path
      
          !! total pathogen mass in reach
          path_tot = obcs(iob)%hd(1)%path(ipath) * ob(icmd)%hin%flo + ch(jrch)%bactp * rchwtr

          !! compute pathogen die-off
          tday = rttime / 24.0            !! calculate flow duration
          if (tday > 1.0) tday = 1.0
          path_tot = path_tot * Exp(-Theta(path_db(ipath)%do_stream, path_db(ipath)%t_adj,wtmp) * tday)
          path_tot = Max(0., path_tot)

          !! new concentration
          netwtr = ob(icmd)%hin%flo  + rchwtr
	
  	      !! change made by CS while running region 4; date 2 jan 2006	
	      if (path_tot < 1.e-6) path_tot = 0.0 
          if (netwtr >= 1.) then
            ch_water(jrch)%path(ipath) = path_tot / netwtr
          else
            ch_water(jrch)%path(ipath) = 0.
          end if

        end do
      end if

      return
      end subroutine ch_rtpath