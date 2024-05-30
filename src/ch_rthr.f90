      subroutine ch_rthr
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine routes flow at any required time step through the reach 
!!    using a constant storage coefficient  
!!	Routing method: Variable Storage routing   

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name            |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |counter (hour)
!!    inhyd       |none          |inflow hydrograph storage location number
!!    jrch        |none          |reach number
!!    scoef       |none          |storage coefficient
!!    topw        |m             |width of channel at water level
!!    vol         |m^3 H2O       |volume of water in reach
!!    wtrin       |m^3 H2O       |water entering reach during hour
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Sum, Min, Sqrt

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

!!    subroutine developed by A. Van Griensven,
!!    Hydrology-Vrije Universiteit Brussel, Belgium
!!	  Modified by Jeahak Jeong, Blackland Research, Temple, USA

      use basin_module
      use climate_module
      use channel_data_module
      use time_module
      use channel_module
      use hydrograph_module, only : ob, isdch, icmd
      use sd_channel_module
      
      implicit none

      integer :: ii        !none          |counter (hour)
      integer :: jrch      !none          |reach number
      real :: scoef        !none          |storage coefficient
      real :: vol          !m^3 H2O       |volume of water in reach
      real :: topw         !m             |width of channel at water level
      real :: inflo_rate   !m^3/s         |inflow rate
      real :: ttime        !hr            |travel time through the reach
      real :: t_inc        !hr            |time in routing step - 1/time%step
      real :: outflo       !m^3           |outflow water volume
      real :: tl           !m^3           |transmission losses during time step
      real :: trans_loss   !m^3           |transmission losses during day
      real :: ev           !m^3           |evaporation during time step
      real :: evap         !m^3           |evaporation losses during day
      real :: rto          !              |ratio for interpolating rating curve
      real :: outflo_sum   !m^3           |total outflow for the day
      integer :: iwst
      integer :: ielev

      jrch = isdch
      jhyd = sd_dat(jrch)%hyd
      
      trans_loss = 0.
      evap = 0.
      tl = 0.
      ev = 0.
      outflo = 0.
      outflo_sum = 0.
      hyd_rad = 0.
      
      !! volume at start of day
      rcurv = ch_rcurv(jrch)%out2

      !! subdaily time step
      do ii = 1, time%step

        !! flow rate during time step m3/s = m3 / (24 / (1/day) * 3600) 
        inflo_rate = ob(icmd)%tsin(ii) / (86400. / time%step)
        
        !! update volume of water in reach - m3
        vol = vol + ob(icmd)%tsin(ii) 
        vol = Max(vol, 1.e-14)

        !! find where flow fits in rating curve (0.1,1.0,1.5 * bankfull flow rate)
        do ielev = 1, 4
          if (inflo_rate < ch_rcurv(jrch)%elev(ielev)%flo_rate) then
            if (ielev == 1) then
              rto = inflo_rate / ch_rcurv(jrch)%elev(ielev)%flo_rate
              rcurv = ch_rcurv(jrch)%elev(ielev) * rto
              rcurv%ttime = ch_rcurv(jrch)%elev(ielev)%ttime
              exit
            end if
            if (ielev > 1) then
              rto = (inflo_rate - ch_rcurv(jrch)%elev(ielev-1)%flo_rate) /     &
                (ch_rcurv(jrch)%elev(ielev)%flo_rate - ch_rcurv(jrch)%elev(ielev-1)%flo_rate)
              call chrc_interp (ch_rcurv(jrch)%elev(ielev-1), ch_rcurv(jrch)%elev(ielev), rto, rcurv)
              exit
            end if
          end if
          if (ielev == 4) then
            rto = 1. + (inflo_rate - ch_rcurv(jrch)%elev(ielev)%flo_rate) / ch_rcurv(jrch)%elev(ielev)%flo_rate
            rcurv = ch_rcurv(jrch)%elev(ielev) * rto
            !! keep max travel time at 1.5 bankfull
            rcurv%ttime = rcurv%ttime / rto
            exit
          end if
          hyd_rad(ii) = rcurv%xsec_area / rcurv%wet_perim
          trav_time(ii) = rcurv%ttime
          flo_dep(ii) = rcurv%dep
          
        end do

        !! flood routing using variable storage coeffiecient
        if (rcurv%flo_rate > 0.) then
          !! interpolated travel time
          ttime = rcurv%ttime
          
          !! add incoming flow to channel and flood plain storage
          !! based on ratio from rating curve - floodplain/channel volumes
          ch_rcurv(jrch)%in2%vol = ch_rcurv(jrch)%out1%vol + ob(icmd)%tsin(ii)
          rto = rcurv%vol_fp / rcurv%vol_ch
          
          !! Variable Storage Coefficient - calculate volume of water leaving reach during timestep
          t_inc = 24. / time%step
          scoef = t_inc / (ttime + t_inc)
          if (scoef > 1.) scoef = 1.
          outflo = scoef * vol
          if (outflo < 1.e-12) outflo = 0.

          !! subtract routed outflow from from channel storage
          
          !! calculate flow from flood plain wetland storage
          !! below principal spillway is wetland storage - above is flood plain storage
          
          !! readjust channel and flood plain storage?
          
          
          !! calculate transmission losses
          tl = sd_ch(jrch)%chk * sd_ch(jrch)%chl * rcurv%wet_perim * 24. / real(time%step)   !mm/hr * km * mm * hr = m3       
          tl = Min(tl, outflo)
          outflo = outflo - tl
          trans_loss = trans_loss + tl

          !! calculate evaporation
          if (outflo > 0.) then
            !! calculate width of channel at water level
            if (rcurv%dep <= sd_ch(jrch)%chd) then
              topw = ch_rcurv(jrch)%wid_btm + 2. * rcurv%dep * sd_ch(jrch)%chss
            else
              topw = 5. * sd_ch(jrch)%chw + 8. * (rcurv%dep - sd_ch(jrch)%chd)
            end if
            
            iwst = 1
            ev = bsn_prm%evrch * wst(iwst)%weat%pet * sd_ch(jrch)%chl * topw / time%step
            if (ev < 0.) ev = 0.
            ev = Min(ev, outflo)
            outflo = outflo - ev
            evap = evap + ev
          end if

          !! set volume of water in channel at end of hour
          !write (2612,*) ii, ttime, scoef, vol, ob(icmd)%tsin(ii), outflo
          vol = vol - outflo !- tl - ev
          ob(icmd)%hyd_flo(1,ii) = outflo
          outflo_sum = outflo_sum + outflo
          
        end if          !! rcurv%flo_rate > 0. 

      end do            !! end of sub-daily loop

      !! save storage volume for next day and set outflow for day
      !ch_stor(jrch)%flo = vol
      !ht2%flo = outflo_sum

      return
      end subroutine ch_rthr