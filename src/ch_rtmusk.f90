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
      real :: inout             !m3             |inflow - outflow for day
      real :: del_stor          !m3             |change in storage of channel + flood plain + wetlands
      
      real :: c1                !units             |description 
      real :: c2                !units             |description
      real :: c3                !units             |description
      real :: c4                !m^3 H2O           |
      real :: p                 !m                 |wetted perimeter
      real :: c                 !none              |inverse of channel side slope
      real :: rh                !m                 |hydraulic radius
      real :: topw              !m                 |top width of main channel
      real :: qinday            !units             |description 
      real :: qoutday           !units             |description  
	  real :: volrt             !units             |description 
      real :: maxrt             !units             |description 
      real :: adddep            !units             |description 
      real :: addp              !units             |description 
      real :: addarea           !units             |description 
	  real :: rttlc1            !units             |description 
      real :: rttlc2            !units             |description 
      real :: rtevp1            !units             |description 
      real :: rtevp2            !units             |description 
      real :: qman              !m^3/s or m/s      |flow rate or flow velocity
      real :: vc                !m/s               |flow velocity in reach
      real :: aaa               !units             |description 
      real :: inflo             !m^3           |inflow water volume
      real :: inflo_rate        !m^3/s         |inflow rate
      real :: xs_area           !m^2           |cross section area of channel
      real :: dep_flo = 0.      !m             |depth of flow
      real :: wet_perim         !m             |wetted perimeter
      real :: ttime             !hr            |travel time through the reach
      real :: t_inc             !hr            |time in routing step - 1/time%step
      real :: outflo            !m^3           |outflow water volume
      real :: tl                !m^3           |transmission losses during time step
      real :: trans_loss = 0.   !m^3           |transmission losses during day
      real :: rate_flo          !m^3/s         |flow rate
      real :: ev                !m^3           |evaporation during time step
      real :: evap = 0.         !m^3           |evaporation losses during day
      real :: above_prin_fr
      real :: ch_out_fr
      real :: fp_out_fr
      real :: ch_fp_fr
      real :: fp_ch_fr
      real :: rto
      real :: rto1
      real :: rto_w
      real :: rto_emer
      real :: outflo_rate
      real :: ch_st             !m^3        |water storage in and above channel
      real :: fp_st             !m^3        |water storage in flood plain
      real :: wet_st            !m^3        |wetland storage above principal
      real :: dts               !seconds    |time step interval for substep
      real :: dthr
      real :: scoef
      real :: vol_ch
      real :: vol_fp_av
      real :: vol_tot_av
      real :: sum_inflo, sum_outflo

      jrch = isdch
      jhyd = sd_dat(jrch)%hyd
      
      qinday = 0
      qoutday = 0
      ob(icmd)%hd(1) = hz
      ob(icmd)%hyd_flo = 0.
      hyd_rad = 0.
      trav_time = 0.
      flo_dep = 0.
      trans_loss = 0.
      evap = 0.
      
      sum_inflo = sum (ob(icmd)%tsin)
        
      !! total wetland volume at start of day
      wet_stor(jrch) = hz
      do ihru = 1, sd_ch(jrch)%fp%hru_tot
        wet_stor(jrch) = wet_stor(jrch) + wet(ihru)
      end do
      wet_stor_init = wet_stor(jrch)%flo
      ch_stor_init = ch_stor(jrch)%flo
      fp_stor_init = fp_stor(jrch)%flo
      
      irtstep = 1
      isubstep = 0
      dts = time%dtm / sd_ch(jrch)%msk%substeps * 60.
      dthr = dts / 3600.
      
      if (jrch == 3) ht1%flo = 480000.

      !! subdaily time step
      do ii = 1, sd_ch(jrch)%msk%nsteps

        !! water entering reach during time step - substeps for stability
        isubstep = isubstep + 1
        if (isubstep > sd_ch(jrch)%msk%substeps) then
          irtstep = irtstep + 1
          isubstep = 1
        end if
        inflo = ob(icmd)%tsin(irtstep) / sd_ch(jrch)%msk%substeps
        if (jrch == 3) inflo = 10000.
        inflo_rate = inflo / dts 

        !! interpolate rating curve using inflow rates
        icha = jrch
        call rcurv_interp_flo (icha, inflo_rate)
        ch_rcurv(jrch)%in2 = rcurv
        
        !! save variables at each routing time step for sediment routing
        if (isubstep == 1 .and. rcurv%wet_perim > 1.e-6) then
          hyd_rad(irtstep) = rcurv%xsec_area / rcurv%wet_perim
          trav_time(irtstep) = rcurv%ttime
          flo_dep(irtstep) = rcurv%dep
        end if
        
        !! add inflow to total storage
        if (ht1%flo > 1.e-6) then
          rto = inflo / ht1%flo
          tot_stor(jrch) = tot_stor(jrch) + rto * ht1
        end if
        
        !! partition channel and flood plain based on bankfull volume
        if (tot_stor(jrch)%flo > ch_rcurv(jrch)%elev(2)%vol_ch) then
          !! fill channel to bank full if below
          rto = (ch_rcurv(jrch)%elev(2)%vol_ch - ch_stor(jrch)%flo) / tot_stor(jrch)%flo
          vol_ch = ch_stor(jrch)%flo
          if (rto > 0.) then
            ch_stor(jrch) = ch_stor(jrch) + rto * tot_stor(jrch)
          end if
          
          !! if flood plain link - adjust flood plain and wetland storages for inflow
          if (bsn_cc%i_fpwet == 2) then
            !! add flood plain inflow to wetland
            do ihru = 1, sd_ch(jrch)%fp%hru_tot
              iihru = sd_ch(jrch)%fp%hru(ihru)
              !! distribute water by hru fraction of the flood plain
              rto1 = (inflo - (ch_rcurv(jrch)%elev(2)%vol_ch - vol_ch)) / tot_stor(jrch)%flo
              rto_w = rto1 * sd_ch(jrch)%fp%hru_fr(ihru)
              wet(iihru) = wet(iihru) + rto_w * tot_stor(jrch)
              !! if above emergency - move back to flood plain storage
              rto_emer = (wet(iihru)%flo - wet_ob(iihru)%evol) / wet(iihru)%flo
              if (rto_emer > 0.) then
                fp_stor(jrch) = fp_stor(jrch) + rto_emer * wet(iihru)
                wet(iihru) = wet(iihru) -  rto_emer * wet(iihru)
              end if
            end do
          else  
            !! if no flood plain link - add rest to flood plain 
            fp_stor(jrch) = fp_stor(jrch) + (1. - rto) * tot_stor(jrch)
          end if
          
        else
          !! total volume below bankfull
          ch_stor(jrch) = tot_stor(jrch)
          fp_stor(jrch) = hz
        end if      ! flow rate above bank full

        tot_stor(jrch) = ch_stor(jrch) + fp_stor(jrch)
        
        !! if no water in channel - skip routing and set rating curves to zero
        if (tot_stor(jrch)%flo < 1.e-6) then
          ch_rcurv(jrch)%in1 = rcz
          ch_rcurv(jrch)%out1 = rcz
          sd_ch(jrch)%in1_vol = 0.
          sd_ch(jrch)%out1_vol = 0.
        else
       
        !! Muskingum flood routing method
        outflo = sd_ch(jrch)%msk%c1 * inflo + sd_ch(jrch)%msk%c2 * sd_ch(jrch)%in1_vol +     &
                                                sd_ch(jrch)%msk%c3 * sd_ch(jrch)%out1_vol
               
        !! save inflow/outflow volumes for next time step (and day) for Muskingum
        sd_ch(jrch)%in1_vol = inflo
        sd_ch(jrch)%out1_vol = outflo

        !! Variable Storage Coefficent method - sc=2*dt/(2*ttime+dt) - ttime=(in2+out1)/2
        scoef = 2. * dthr / (ch_rcurv(jrch)%in2%ttime + ch_rcurv(jrch)%out1%ttime + dthr)
        scoef = Min (scoef, 1.)
        !outflo = scoef * tot_stor(jrch)%flo
        
	    outflo = Min (outflo, tot_stor(jrch)%flo)
        outflo = Max (outflo, 0.)
        
        !! compute outflow rating curve for next time step
        outflo_rate = outflo / dts      !convert to cms
        call rcurv_interp_flo (jrch, outflo_rate)
        ch_rcurv(jrch)%out2 = rcurv
 
        !! add outflow to daily hydrograph and subdaily flow
        rto = outflo / tot_stor(jrch)%flo
        rto = amin1 (1., rto)
        ob(icmd)%hd(1) = ob(icmd)%hd(1) + rto * tot_stor(jrch)
        ob(icmd)%hyd_flo(1,irtstep) = ob(icmd)%hyd_flo(1,irtstep) + outflo
               
        !! subtract outflow from total storage
        rto = outflo / tot_stor(jrch)%flo
        tot_stor(jrch) = (1. - rto) * tot_stor(jrch)
        
        !! readjust channel and flood plain volumes after outflow
        if (tot_stor(jrch)%flo <= ch_rcurv(jrch)%elev(2)%vol_ch) then
          ch_stor(jrch) = tot_stor(jrch)
          fp_stor(jrch) = hz
        else
          rto = ch_rcurv(jrch)%elev(2)%vol_ch / tot_stor(jrch)%flo
          ch_stor(jrch) = rto * tot_stor(jrch)
          rto = 1. - rto
          fp_stor(jrch) = rto * tot_stor(jrch)
        end if
        
        !! if flood plain link - adjust wetland storages for outflow
        if (bsn_cc%i_fpwet == 2) then
          !! compute release from flood plain wetlands
          do ihru = 1, sd_ch(jrch)%fp%hru_tot
            iihru = sd_ch(jrch)%fp%hru(ihru)
            ires= hru(iihru)%dbs%surf_stor
            ihyd = wet_dat(ires)%hyd
            irel = wet_dat(ires)%release
            !! calc release from decision table
            d_tbl => dtbl_res(irel)
            wbody => wet(iihru)
            wbody_wb => wet_wat_d(iihru)
            call conditions (iihru, irel)
            call res_hydro (iihru, irel, ihyd, wet_ob(ihru)%pvol, wet_ob(ihru)%evol)
      
            !! subtract outflow from wetland and add to flood plain storage
            wet(iihru) =  wet(iihru) - ht2
            fp_stor(jrch) = fp_stor(jrch) + ht2
            tot_stor(jrch) = tot_stor(jrch) + ht2
          end do
        end if      ! bsn_cc%i_fpwet == 2
 
        !! set rating curve for next time step
        ch_rcurv(jrch)%in1 = ch_rcurv(jrch)%in2
        ch_rcurv(jrch)%out1 = ch_rcurv(jrch)%out2
    
          !! calculate transmission losses
          if (tot_stor(jrch)%flo > 1.e-6) then
            ttime = Min(24., rcurv%ttime)
            tl = sd_ch(jrch)%chk * sd_ch(jrch)%chl * rcurv%wet_perim * ttime   !mm/hr * km * mm * hr = m3       
            tl = Min(tl, tot_stor(jrch)%flo)
            trans_loss = trans_loss + tl
            !! subtract evap and transmission loses from channel and flood plain storage
            if (tot_stor(jrch)%flo > 1.e-6) then
              rto = tl / tot_stor(jrch)%flo
              rto = amin1 (1., rto)
              ch_stor(jrch) = (1. - rto) * ch_stor(jrch)
              fp_stor(jrch) = (1. - rto) * fp_stor(jrch)
              tot_stor(jrch) = (1. - rto) * tot_stor(jrch)
            else
              ch_stor(jrch) = hz
              fp_stor(jrch) = hz
              tot_stor(jrch) = hz
            end if
          end if

          !! calculate evaporation
          if (tot_stor(jrch)%flo > 1.e-6) then
            !! calculate width of channel at water level - flood plain evap calculated in wetlands
            if (dep_flo <= sd_ch(jrch)%chd) then
              topw = ch_rcurv(jrch)%out2%surf_area
            else
              topw = 1000. * sd_ch(jrch)%chl * sd_ch(jrch)%chw
            end if
            
            iwst = ob(icmd)%wst
            !! mm/day * m2 / (1000. * sd_ch(jrch)%msk%nsteps)
            ev = bsn_prm%evrch * wst(iwst)%weat%pet * topw / (1000. * sd_ch(jrch)%msk%nsteps)
            if (ev < 0.) ev = 0.
            ev = Min(ev, tot_stor(jrch)%flo)
            evap = evap + ev
            !! subtract evap and transmission loses from channel and flood plain storage
            if (tot_stor(jrch)%flo > 1.e-6) then
              rto = ev / tot_stor(jrch)%flo
              rto = amin1 (1., rto)
              ch_stor(jrch) = (1. - rto) * ch_stor(jrch)
              fp_stor(jrch) = (1. - rto) * fp_stor(jrch)
              tot_stor(jrch) = (1. - rto) * tot_stor(jrch)
            else
              ch_stor(jrch) = hz
              fp_stor(jrch) = hz
              tot_stor(jrch) = hz
            end if
          end if
   
          tot_stor(jrch) = ch_stor(jrch) + fp_stor(jrch)
          
        end if          !! tot_stor(jrch)%flo < 1.e-6 loop

      end do            !! end of sub-daily loop

      !! check water balance at end of day
      sum_outflo = sum (ob(icmd)%hyd_flo(1,:))
      inout = sum_inflo - sum_outflo - trans_loss - evap
      del_stor = (ch_stor(jrch)%flo - ch_stor_init) + (fp_stor(jrch)%flo - fp_stor_init) +  &
                                                    (wet_stor(jrch)%flo - wet_stor_init)
        
      return
      end subroutine ch_rtmusk