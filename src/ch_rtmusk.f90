      subroutine ch_rtmusk
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine routes a daily flow through a reach using the
!!    Muskingum method

!!    code provided by Dr. Valentina Krysanova, Pottsdam Institute for
!!    Climate Impact Research, Germany
!!    Modified by Balaji Narasimhan
!!    Spatial Sciences Laboratory, Texas A&M University

      use basin_module
      use channel_data_module
      use channel_module
      use hydrograph_module !, only : ob, icmd, jrch, isdch, fp_stor, ch_stor, wet
      use time_module
      use channel_velocity_module
      use sd_channel_module
      use climate_module
      use reservoir_module
      use reservoir_data_module
      use water_body_module
      use hru_module, only : hru
      use conditional_module
      
      implicit none
      
      integer :: ii         !none              |current day of simulation
      integer :: ihru
      integer :: iihru
      integer :: icha
      integer :: irtstep
      integer :: isubstep
      integer :: ires
      integer :: ihyd
      integer :: irel
      
      real :: ch_stor_init      !m3             |storage in channel at beginning of day
      real :: fp_stor_init      !m3             |storage in flood plain above wetlands emergency spillway at beginning of day
      real :: wet_stor_init     !m3             |storage in flood plain wetlands at beginning of day
      real :: tot_stor_init
      real :: inout             !m3             |inflow - outflow for day
      real :: del_stor          !m3             |change in storage of channel + flood plain + wetlands
      real :: topw              !m                 |top width of main channel
      real :: qinday            !units             |description 
      real :: qoutday           !units             |description   
      real :: inflo             !m^3           |inflow water volume
      real :: inflo_rate        !m^3/s         |inflow rate
      real :: dep_flo = 0.      !m             |depth of flow
      real :: ttime             !hr            |travel time through the reach
      real :: outflo            !m^3           |outflow water volume
      real :: tl                !m^3           |transmission losses during time step
      real :: trans_loss = 0.   !m^3           |transmission losses during day
      real :: ev                !m^3           |evaporation during time step
      real :: evap = 0.         !m^3           |evaporation losses during day
      real :: precip = 0.       !m^3           |precip during routing time step
      real :: rto
      real :: rto1
      real :: rto_w
      real :: rto_emer
      real :: outflo_rate
      real :: dts               !seconds    |time step interval for substep
      real :: dthr
      real :: scoef
      real :: vol_ch
      real :: sum_inflo, sum_outflo
      real :: dep
      real :: evol_m3, pvol_m3
      real :: wet_evol 
      real :: bf_flow               !m3/s           |bankfull flow rate * adjustment factor
      real :: pk_rto                !ratio          |peak to mean flow rate ratio

      jrch = isdch
      jhyd = sd_dat(jrch)%hyd
      
      qinday = 0
      qoutday = 0
      ht2 = hz
      ob(icmd)%hyd_flo = 0.
      hyd_rad = 0.
      trav_time = 0.
      flo_dep = 0.
      trans_loss = 0.
      evap = 0.
      ch_wat_d(jrch)%evap = 0.
      ch_wat_d(jrch)%seep = 0.
      
      !***jga
      !ob(icmd)%tsin = (/0., 800., 2000., 4200., 5200., 4400., 3200., 2500., 2000., 1500., 1000., 700., 400.,     &
      !                 0., 0., 0., 0., 0., 1000000., 1000000., 1000000., 1000000., 1000000., 1000000./)
      sum_inflo = sum (ob(icmd)%tsin)
        
      !! total wetland volume at start of day
      wet_stor(jrch) = hz
      wet_evol = 0.
      do ihru = 1, sd_ch(jrch)%fp%hru_tot
        iihru = sd_ch(jrch)%fp%hru(ihru)
        wet_stor(jrch) = wet_stor(jrch) + wet(iihru)
        wet_evol = wet_evol + wet_ob(iihru)%evol
      end do
      wet_stor_init = wet_stor(jrch)%flo
      ch_stor_init = ch_stor(jrch)%flo
      fp_stor_init = fp_stor(jrch)%flo
      tot_stor_init = ch_stor_init + fp_stor_init
      
      !! set for daily time step
      if (time%step == 1) then
        sd_ch(jrch)%msk%nsteps = 1
        sd_ch(jrch)%msk%substeps = 1
      end if
      irtstep = 1
      isubstep = 0
      dts = time%dtm / sd_ch(jrch)%msk%substeps * 60.
      dthr = dts / 3600.
      
      !! subdaily time step
      do ii = 1, sd_ch(jrch)%msk%nsteps
        !! water entering reach during time step - substeps for stability
        isubstep = isubstep + 1
        if (isubstep > sd_ch(jrch)%msk%substeps) then
          irtstep = irtstep + 1
          isubstep = 1
        end if
        
        !! add inflow to total storage
        if (ht1%flo > 1.e-6) then
          !! subdaily inflow
          inflo = ob(icmd)%tsin(irtstep) / sd_ch(jrch)%msk%substeps
          rto = inflo / ht1%flo
          rto = Max(0., rto)
          rto = Min(1., rto)
          tot_stor(jrch) = tot_stor(jrch) + rto * ht1
        end if    ! ht1%flo > 1.e-6
        
        !! interpolate rating curve using inflow rates
        icha = jrch
        inflo_rate = inflo / 86400.
        call rcurv_interp_flo (icha, inflo_rate)
        ch_rcurv(jrch)%in2 = rcurv
        
        !! if no water in channel - skip routing and set rating curves to zero
        if (tot_stor(jrch)%flo < 1.e-6) then
          ch_rcurv(jrch)%in1 = rcz
          ch_rcurv(jrch)%out1 = rcz
          sd_ch(jrch)%in1_vol = 0.
          sd_ch(jrch)%out1_vol = 0.
        else
          if (bsn_cc%rte == 1) then
          !! Muskingum flood routing method
            outflo = sd_ch(jrch)%msk%c1 * inflo + sd_ch(jrch)%msk%c2 * sd_ch(jrch)%in1_vol +     &
                                                sd_ch(jrch)%msk%c3 * sd_ch(jrch)%out1_vol
	        outflo = Min (outflo, tot_stor(jrch)%flo)
            outflo = Max (outflo, 0.)
               
            !! save inflow/outflow volumes for next time step (and day) for Muskingum
            sd_ch(jrch)%in1_vol = inflo
            sd_ch(jrch)%out1_vol = outflo
          else

            !! Variable Storage Coefficent method - sc=2*dt/(2*ttime+dt) - ttime=(in2+out1)/2
            scoef = dthr / (ch_rcurv(jrch)%in2%ttime + ch_rcurv(jrch)%out1%ttime + dthr)
            scoef = Min (scoef, 1.)
            outflo = scoef * tot_stor(jrch)%flo
          end if
          
          !! compute outflow rating curve for next time step
          outflo_rate = outflo / dts      !convert to cms
          call rcurv_interp_flo (jrch, outflo_rate)
          ch_rcurv(jrch)%out2 = rcurv
 
          !! add outflow to daily hydrograph and subdaily flow
          rto = outflo / tot_stor(jrch)%flo
          rto = Min (1., rto)
          ht2 = ht2 + rto * tot_stor(jrch)
          ob(icmd)%hyd_flo(1,irtstep) = ob(icmd)%hyd_flo(1,irtstep) + outflo
          !! subtract outflow from total storage
          tot_stor(jrch) = (1. - rto) * tot_stor(jrch)
        
          !! set rating curve for next time step
          ch_rcurv(jrch)%in1 = ch_rcurv(jrch)%in2
          ch_rcurv(jrch)%out1 = ch_rcurv(jrch)%out2
          
          !! partition channel and flood plain based on bankfull volume
          if (tot_stor(jrch)%flo > ch_rcurv(jrch)%elev(2)%vol_ch) then
            !! fill channel to bank full if below
            rto = (tot_stor(jrch)%flo - ch_rcurv(jrch)%elev(2)%vol_ch) / tot_stor(jrch)%flo
            fp_stor(jrch) = rto * tot_stor(jrch)
            ch_stor(jrch) = (1. - rto) * tot_stor(jrch)
          else
            ch_stor(jrch) = tot_stor(jrch)
            fp_stor(jrch) = hz
          end if
        
          tot_stor(jrch) = ch_stor(jrch) + fp_stor(jrch)
          
        end if  ! tot_stor(jrch)%flo < 1.e-6

      end do    ! end of sub-daily loop
      
      !! compute water balance - evap and seep
      !! calculate transmission losses (seepage)
      if (ch_stor(jrch)%flo > 1.e-6) then
        !! mm/hr * km * m * 24. = m3
        trans_loss = sd_ch(jrch)%chk * sd_ch(jrch)%chl * rcurv%wet_perim * 24.
        trans_loss = sd_ch(jrch)%chk * sd_ch(jrch)%chl * sd_ch(jrch)%chw * 24.
        trans_loss = Min(trans_loss, ch_stor(jrch)%flo)
        !! subtract transmission loses from outflow
        rto = trans_loss / ch_stor(jrch)%flo
        ch_stor(jrch) = (1. - rto) * ch_stor(jrch)
      end if
      ch_wat_d(ich)%seep = trans_loss

      !! calculate evaporation losses
      if (ch_stor(jrch)%flo > 1.e-6) then
        !! calculate width of channel at water level - flood plain evap calculated in wetlands
        !if (dep_flo <= sd_ch(jrch)%chd) then
        !  topw = ch_rcurv(jrch)%out2%surf_area
        !else
          topw = 1000. * sd_ch(jrch)%chl * sd_ch(jrch)%chw
        !end if
        iwst = ob(icmd)%wst
        !! mm/day * m2 / 1000.
        evap = bsn_prm%evrch * wst(iwst)%weat%pet * topw / 1000.
        evap = Min(evap, ch_stor(jrch)%flo)
        rto = evap / ch_stor(jrch)%flo
        ch_stor(jrch)%flo = (1. - rto) * ch_stor(jrch)%flo
      end if
      ch_wat_d(ich)%evap = evap
      
      tot_stor(jrch) = ch_stor(jrch) + fp_stor(jrch)

      !! check water balance at end of day
      sum_outflo = ht2%flo
      inout = sum_inflo - sum_outflo - trans_loss - evap
      !! total wetland volume at end of day
      wet_stor(jrch) = hz
      do ihru = 1, sd_ch(jrch)%fp%hru_tot
        iihru = sd_ch(jrch)%fp%hru(ihru)
        wet_stor(jrch) = wet_stor(jrch) + wet(iihru)
      end do
      del_stor = (ch_stor(jrch)%flo - ch_stor_init) + (fp_stor(jrch)%flo - fp_stor_init) +          &
                                                    (wet_stor(jrch)%flo - wet_stor_init)
      ch_fp_wb(jrch)%inflo = sum_inflo
      ch_fp_wb(jrch)%outflo = sum_outflo
      ch_fp_wb(jrch)%tl = trans_loss
      ch_fp_wb(jrch)%ev = evap
      ch_fp_wb(jrch)%ch_stor_init = ch_stor_init
      ch_fp_wb(jrch)%ch_stor = ch_stor(jrch)%flo
      ch_fp_wb(jrch)%fp_stor_init = fp_stor_init
      ch_fp_wb(jrch)%fp_stor = fp_stor(jrch)%flo
      ch_fp_wb(jrch)%tot_stor_init = tot_stor_init
      ch_fp_wb(jrch)%tot_stor = tot_stor(jrch)%flo
      ch_fp_wb(jrch)%wet_stor_init = wet_stor_init
      ch_fp_wb(jrch)%wet_stor = wet_stor(jrch)%flo
      
      return
      end subroutine ch_rtmusk