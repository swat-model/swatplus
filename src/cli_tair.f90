      function cli_tair(hr,jj)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this function approximates hourly air temperature from daily max and
!!    min temperatures as documented by Campbell (1985)

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    tair        |deg C         |air temperature for hour in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Real, Cos

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

!!    subroutine developed by A. Van Griensven
!!    Hydrology-Vrije Universiteit Brussel, Belgium
!!    subroutine modified by SLN

      use climate_module
      
      implicit none
       
      integer, intent (in) ::  jj     !none          |HRU number
      real, intent(in) :: hr          !none          |hour if the day
      real :: cli_tair                !              |
      real :: tmp_lo                  !deg C         |last minimum temperature in HRU
      real :: tmp_hi                  !deg C         |last maximum temperature in HRU
      integer :: iwst                 !none          |counter

!! update hi or lo temperature depending on hour of day
      if (hr == 3) tmp_lo = wst(iwst)%weat%tmax
      if (hr == 15) tmp_hi = wst(iwst)%weat%tmin

!! SWAT manual equation 2.3.1
      cli_tair = 0.
      cli_tair = 0.5 * (tmp_hi + tmp_lo + (tmp_hi - tmp_lo * Cos(0.2618 * Real(hr - 15))))

      return
      end function