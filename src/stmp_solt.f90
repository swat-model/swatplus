      subroutine stmp_solt
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine estimates daily average temperature at the bottom
!!    of each soil layer     

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    albday      |none          |albedo of ground for day
!!    tmp_an(:)   |deg C         |average annual air temperature
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Log, Max, Min

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use climate_module
      use septic_data_module
      use hru_module, only : hru, iseptic, ihru, i_sep, iwgen, albday, isep 
      use soil_module
      use time_module
      use organic_mineral_mass_module
      
      implicit none

      integer :: j               !none          |HRU number
      integer :: k               !none          |counter
      real :: f                  !none          |variable to hold intermediate calculation result
      real :: dp                 !mm            |maximum damping depth
      real :: ww                 !none          |variable to hold intermediate calculation
      real :: b                  !none          |variable to hold intermediate calculation
      real :: wc                 !none          |scaling factor for soil water impact on daily damping depth
      real :: dd                 !mm            |damping depth for day
      real :: xx                 !none          |variable to hold intermediate calculation
      real :: st0                !MJ/m^2        |radiation hitting soil surface on day
      real :: tlag               !none          |lag coefficient for soil temperature
      real :: df                 !none          |depth factor
      real :: zd                 !none          |ratio of depth at center of layer to damping depth 
      real :: bcv                !none          |lagging factor for cover
      real :: tbare              !deg C         |temperature of bare soil surface
      real :: tcov               !deg C         |temperature of soil surface corrected for cover
      real :: tmp_srf            !deg C         |temperature of soil surface
      real :: cover              !kg/ha         |soil cover

      j = ihru

      tlag = 0.8

!! calculate damping depth

      !! calculate maximum damping depth
      !! SWAT manual equation 2.3.6
      f = 0.
      dp = 0.
      f = soil(j)%avbd / (soil(j)%avbd + 686. * Exp(-5.63 *       &       
              soil(j)%avbd))
      dp = 1000. + 2500. * f

      !! calculate scaling factor for soil water
      !! SWAT manual equation 2.3.7
      ww = 0.
      wc = 0.
      ww = .356 - .144 * soil(j)%avbd
      wc = soil(j)%sw / (ww * soil(j)%phys(soil(j)%nly)%d)

      !! calculate daily value for damping depth
      !! SWAT manual equation 2.3.8
      b = 0.
      f = 0.
      dd = 0.
      b = Log(500. / dp)
      f = Exp(b * ((1. - wc) / (1. + wc))**2)
      dd = f * dp

!! calculate lagging factor for soil cover impact on soil surface temp
!! SWAT manual equation 2.3.11
      cover = pl_mass(j)%ab_gr_com%m + rsd1(j)%tot_com%m
      bcv = cover / (cover + Exp(7.563 - 1.297e-4 * cover))
      if (hru(j)%sno_mm /= 0.) then
        if (hru(j)%sno_mm <= 120.) then
          xx = 0.
          xx = hru(j)%sno_mm / (hru(j)%sno_mm + Exp(6.055 - .3002 * hru(j)%sno_mm))
        else
          xx = 1.
        end if
        bcv = Max(xx,bcv)
      end if

!! calculate temperature at soil surface
      st0 = 0.
      tbare = 0.
      tcov = 0.
      tmp_srf = 0.
      !! SWAT manual equation 2.3.10
      st0 = (w%solrad * (1. - albday) - 14.) / 20.
      !! SWAT manual equation 2.3.9
      tbare = w%tave + 0.5 * (w%tmax - w%tmin) * st0
      !! SWAT manual equation 2.3.12
      tcov = bcv * soil(j)%phys(2)%tmp + (1. - bcv) * tbare

!!    taking average of bare soil and covered soil as in APEX
!!    previously using minumum causing soil temp to decrease
!!    in summer due to high biomass

      tmp_srf = 0.5 * (tbare + tcov)  ! following Jimmy"s code

!! calculate temperature for each layer on current day
      xx = 0.
      do k = 1, soil(j)%nly
        zd = 0.
        df = 0.
        zd = (xx + soil(j)%phys(k)%d) / 2.  ! calculate depth at center of layer
        zd = zd / dd                 ! SWAT manual equation 2.3.5
        !! SWAT manual equation 2.3.4
        df = zd / (zd + Exp(-.8669 - 2.0775 * zd))
        !! SWAT manual equation 2.3.3
        soil(j)%phys(k)%tmp = tlag * soil(j)%phys(k)%tmp + (1. - tlag) *       &
                      (df * (wgn_pms(iwgen)%tmp_an - tmp_srf) + tmp_srf)
        xx = soil(j)%phys(k)%d

        ! Temperature correction for Onsite Septic systems
        isep = iseptic(j)
        if (sep(isep)%opt /= 0 .and. time%yrc >= sep(isep)%yr .and. k >=       &
                                                          i_sep(j)) then
	   if (soil(j)%phys(k)%tmp < 10.) then
	       soil(j)%phys(k)%tmp = 10. - (10. - soil(j)%phys(k)%tmp) * 0.1
	   end if     
	  endif

      end do

      return
      end subroutine stmp_solt