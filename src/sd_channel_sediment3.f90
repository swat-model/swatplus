      subroutine sd_channel_sediment3
        
      use climate_module
      use sd_channel_module
      use channel_module
      use hydrograph_module
      use time_module
      use hru_module, only : hru
      use water_body_module
      use reservoir_module
      use utils
      use basin_module, only : bsn_cc
      use gwflow_module, only : flood_freq
      use channel_velocity_module

      implicit none

      external :: gwflow_floodplain     
      
      external :: rcurv_interp_flo

      integer :: iob = 0            !               |object number
      integer :: ihru = 0
      integer :: iihru = 0
      integer :: ires = 0
      real :: rto = 0.
      real :: rto1 = 0.
      real :: trap_eff = 0.         !frac           |trap efficiency in the flood plain
      real :: ebtm_m = 0.           !m              |erosion of bottom of channel
      real :: ebank_m = 0.          !m              |meander cut on one side
      real :: ebtm_t = 0.           !tons           |bottom erosion
      real :: ebank_t = 0.          !tons           |bank erosion
      real :: shear_btm_cr = 0.     !               |
      real :: shear_btm = 0.        !               |  
      real :: bf_flow = 0.          !m3/s           |bankfull flow rate * adjustment factor
      real :: pk_rto = 0.           !ratio          |peak to mean flow rate ratio
      real :: cohes_fac = 0.        !               |cohesion factor for critical velocity calculation
      real :: vel = 0.
      real :: veg_fac = 0.
      real :: vel_bf = 0.
      real :: vel_cr = 0.
      real :: vel_bend = 0.
      real :: vel_rch = 0.
      real :: v_vc = 0.
      real :: q_q1 = 0.
      real :: e_max = 0.            !m              |maximum bank erosion rate for each event
      real :: fp_km2 = 0.
      real :: exp_co = 0.
      real :: precip = 0.
      real :: flovol_ob = 0.
      real :: wet_fill = 0.
      real :: ave_rate   
      real :: vc_coef
      real :: q_coef
      real :: fr_ob
      real :: trap_adj
      integer :: iord = 0

      ich = isdch
      iob = sp_ob1%chandeg + jrch - 1
      iord = sd_ch(ich)%order

      ebtm_m = 0.
      ebank_m = 0.
      ebtm_t = 0.
      ebank_t = 0.
      flovol_ob = 0.
      fp_dep = hz 
      ch_dep = hz 
      bank_ero = hz 
      bed_ero = hz 
      ch_trans = hz
      ch_wat_d(ich)%precip = 0.

      !***jga
      !sd_ch(ich)%bankfull_flo = 2.0
      !sd_ch(ich)%fp_inun_days = 1.0
      !sd_ch(ich)%d50 = 1.0
      !sd_ch(ich)%bed_exp = 1.0
      
      !! calculate channel sed and nutrient processes if inflow > 0
      if (ht1%flo > 1.e-6) then

      !! peak daily flow rate - m3/s using the Fuller and Peck (1974) equation for peak to mean flow ratio
      pk_rto = sd_ch(ich)%pk_rto * (1. + 2.66 * (ob(icmd)%area_ha / 100.) ** (-.3))
      peakrate = pk_rto * ht1%flo / 86400.     !m3/s

      !! interpolate rating curve using peak rate
      call rcurv_interp_flo (ich, peakrate)
      
      !! channel velocity from the interpolated rating curve
      !! (rating-curve vel avoids the unrealistically high velocities at low flow from continuity Q/A)
      vel = rcurv%vel

      rttime = sd_ch(ich)%chl / (3.6 * vel)
      sd_ch_vel(ich)%vel = vel       !store for ch_temp
      sd_ch_vel(ich)%rttime = rttime !store for ch_temp

      !! add precip to inflow - km * m * 1000 m/km * ha/10000 m2 = ha
      ch_wat_d(ich)%area_ha = sd_ch(ich)%chl * sd_ch(ich)%chw / 10.
      !! m3 = 10. * mm * ha
      precip = 10. * wst(iwst)%weat%precip * ch_wat_d(ich)%area_ha
      ch_wat_d(ich)%precip = precip
      rto = precip / ht1%flo
      ob(icmd)%tsin(:) = (1. + rto) * ob(icmd)%tsin(:)
      ht1%flo = ht1%flo + precip

      !! compute overbank flow and flood plain deposition
      bf_flow = sd_ch(ich)%bankfull_flo * ch_rcurv(ich)%elev(2)%flo_rate
      
      q_q1 = peakrate / bf_flow
      !! fraction of flow that goes overbank
      q_coef = 0.5 / (1. + exp(- 20. * (q_q1 - 1.)))
      flovol_ob = q_coef * ht1%flo
      
      fr_ob = 0.
      ave_rate = ht1%flo / 86400.
      if (ave_rate >= bf_flow) then
        fr_ob = 1. - bf_flow / ave_rate
      else if (peakrate > bf_flow) then
        fr_ob = 1. / pk_rto
      end if
          
      !if (flovol_ob < 0.05 * ht1%flo) flovol_ob = 0.
      if (fr_ob > 0.01) then
        flovol_ob = fr_ob * ht1%flo
        !! add to overbank days
        ch_morph(ich)%ob_days = ch_morph(ich)%ob_days + 1
        trap_eff = 0.05 * log(sd_ch(ich)%fp_inun_days) + 0.1
        trap_eff = Min (1., trap_eff)
        trap_eff = Max (0., trap_eff)
        !! trap efficiency modified from Dynamic SedNet Component Model Reference Guide: Update 2017
        fp_km2 = 5. * sd_ch(ich)%chw * sd_ch(ich)%chl / 10.
        !trap_adj = 0.1 * fp_km2 ** 0.5
        trap_adj = 0.1 * fp_km2 ** 0.5
        trap_adj = (fp_km2 ** 2.12) / (fp_km2 ** 2.12 + 132.18)
        fp_dep%sed = trap_eff * trap_adj * fr_ob * ht1%sed
        !! deposit Particulate P and N in the floodplain
        fp_dep%orgn = trap_eff * fr_ob * sd_ch(ich)%n_dep_enr * ht1%orgn
        fp_dep%sedp = trap_eff * fr_ob * sd_ch(ich)%p_dep_enr * ht1%sedp
        !! trap nitrate and sol P in flood plain - when not simulating flood plain interactions?
        fp_dep%no3 = 0.         !trap_eff * ht1%no3
        fp_dep%solp = 0.        !trap_eff * ht1%solp
      
        !! floodplain exchange with aquifer (gwflow)
        if(bsn_cc%gwflow.eq.1) then
          flood_freq(ich) = 1
          call gwflow_floodplain(ich)
        endif
          
        !! overbank volume is assumed = flood plain volume at peak rate (from the rating curve)
        !flovol_ob = rcurv%vol_fp
        !trap_eff = 0.05 * log(sd_ch(ich)%fp_inun_days) + 0.1
        !! trap efficiency from Dynamic SedNet Component Model Reference Guide: Update 2017
        !fp_m2 = 5. * sd_ch(ich)%chw * sd_ch(ich)%chl * 1000.
        !exp_co = 0.0001 * fp_m2 / rcurv%flo_rate
        !ave_rate = ht1%flo / 86400.
        !trap_eff = sd_ch(ich)%fp_inun_days * (rcurv%flo_rate / ave_rate) * (1. - exp_w(-exp_co))
        !trap_eff = Min (1., trap_eff)
        !fp_dep%sed = trap_eff * ht1%sed
        
        !! if flood plain link - fill wetlands to emergency
        do ihru = 1, sd_ch(ich)%fp%hru_tot
          iihru = sd_ch(ich)%fp%hru(ihru)
          ires= hru(iihru)%dbs%surf_stor
          !! fill wetland storage to emergency volume
          wet_fill = wet_ob(iihru)%evol - wet(iihru)%flo
          !! calculate overbank volume left to fill wetland
          flovol_ob = flovol_ob - wet_fill
          if (flovol_ob < 0.) then
            wet_fill = wet_fill + flovol_ob
            flovol_ob = 0.
          end if
          if (ires > 0 .and. wet_fill > 0.) then
            rto = wet_fill / ht1%flo
            rto = Min (1., rto)
            if (rto > 1.e-6) then
              wet(iihru) = wet(iihru) + rto * ht1
              wet_in_d(iihru) = wet_in_d(iihru) + rto * ht1
              hru(iihru)%wet_obank_in = (rto * ht1%flo) / (10. * hru(iihru)%area_ha)
              rto1 = 1. - rto
              ob(icmd)%tsin(:) = rto1 * ob(icmd)%tsin(:)
              ht1 = rto1 * ht1
            end if
          end if
        end do

      end if     ! florate_ob > 0.

      !! calc bank erosion
      !sd_ch(ich)%vcr_coef = 1.
      
      !! new critical velocity function from Peter 10-26-2026
      sd_ch(ich)%cov = 1.0
      veg_fac = sd_ch(ich)%cov
      cohes_fac = 0.87 * exp (0.0202 * sd_ch(ich)%ch_clay)
      vel_cr = 0.8 * veg_fac * cohes_fac  !!jga - 0.5 could be the vcr factor not being used
      if (sd_ch(ich)%sinu <= 2.0) then
        vel_bend = sd_ch(ich)%sinu ** 0.38
      else
        vel_bend = 1.3
      end if
      vel_rch = 0.33 * vel_bend + 0.66 * vel
      
      !! bank erosion m/yr
      vel_bf = ch_rcurv(ich)%elev(2)%vel
      sd_ch(ich)%w_yr = 0.02
      e_max = (sd_ch(ich)%w_yr * sd_ch(ich)%chw) / 3.65  !(3.65 * (1. - (vel_cr / (0.8 * vel_bf))))
      e_max = amax1 (0., e_max)
      !e_max = 0.03
      v_vc = vel_rch / vel_cr
      q_q1 = peakrate / (0.6 * ch_rcurv(ich)%elev(2)%flo_rate)
      vc_coef = 1. / (1. + exp(- 2. * (v_vc - 1.)))
      q_coef = 1. / (1. + exp(- 20. * (q_q1 - 1.)))
      ebank_m = e_max * vc_coef * q_coef

      !! mass of sediment eroded -> t = bankcut (m) * depth (m) * bd (t/m3) * lengthcut (km) * 1000. (m/km)
      ebank_t = ebank_m * sd_ch(ich)%chd * sd_ch(ich)%ch_bd * sd_ch(ich)%chl * 1000.
      bank_ero%sed = ebank_t
      
      !! calculate associated nutrients
      bank_ero%orgn = bank_ero%sed * sd_ch(ich)%n_conc / 1000.
      bank_ero%sedp = (1. - sd_ch(ich)%p_bio) * bank_ero%sed * sd_ch(ich)%p_conc / 1000.
      bank_ero%no3 = 0.
      bank_ero%solp = sd_ch(ich)%p_bio * bank_ero%sed * sd_ch(ich)%p_conc / 1000.
      bank_ero%no2 = 0.
      rto = bank_ero%flo / ht1%flo
      ob(icmd)%tsin(:) = (1. - rto) * ob(icmd)%tsin(:)
      ht1 = ht1 + bank_ero

      !! calculate channel deposition based on fall velocity - SWRRB book
      !! assume particle size = 0.03 mm -- median silt size
      !vel_fall = 411. * sd_ch(ich)%part_size ** 2     ! m/h
      !dep_fall = vel_fall * rcurv%ttime
      !! assume bankfull flow depth
      !if (dep_fall < sd_ch(ich)%chd) then
      !  del_rto = 1. - .5 * dep_fall / sd_ch(ich)%chd
      !else
      !  del_rto = .5 * sd_ch(ich)%chd / dep_fall
      !end if
      !ch_dep%sed = (1. - del_rto) * ht1%sed
      !ch_dep%orgn = sd_ch(ich)%n_dep_enr * (1. - del_rto) * ht1%orgn
      !ch_dep%sedp = sd_ch(ich)%p_dep_enr * (1. - del_rto) * ht1%sedp
      !rto = ch_dep%flo / ht1%flo
      !ob(icmd)%tsin(:) = (1. - rto) * ob(icmd)%tsin(:)
      !ht1 = ht1 - ch_dep
      
      !! calculate channel deposition as the bedload fraction of bank erosion 
      ch_dep%sed = sd_ch(ich)%wash_bed_fr * bank_ero%sed
      ch_dep%orgn = sd_ch(ich)%n_dep_enr * sd_ch(ich)%wash_bed_fr * bank_ero%orgn
      ch_dep%sedp = sd_ch(ich)%p_dep_enr * sd_ch(ich)%wash_bed_fr * bank_ero%sedp
      ht1 = ht1 - ch_dep
      
      !! calculate bed erosion
      !! no downcutting below equilibrium slope
      if (sd_ch(ich)%chs > 0.000001) then
        !! calc critical shear and shear on bottom of channel
        shear_btm_cr = sd_ch(ich)%d50
        shear_btm = 9800. * rcurv%dep * sd_ch(ich)%chs   !! Pa = N/m^2 * m * m/m
        !! critical shear function of d50
        !sd_ch(ich)%d50 = 5.   !!jga
        vel_cr = 0.293 * (sd_ch(ich)%d50) ** 0.5
        if (vel > vel_cr) then
          !! bed erosion m/yr
          ebtm_m = 0.0001 * (vel / vel_cr) ** sd_ch(ich)%bed_exp
        end if
        !! calc mass of sediment eroded -> t = m * width (m) * length (km) * 1000 m/km * bd (t/m3)
        ebtm_t = 1000. * ebtm_m * sd_ch(ich)%chw * sd_ch(ich)%chl * sd_ch(ich)%ch_bd
      end if
      bed_ero%sed = sd_ch(ich)%wash_bed_fr * ebtm_t
      !! calculate associated nutrients
      bed_ero%orgn = bed_ero%sed * sd_ch(ich)%n_conc
      bed_ero%sedp = (1. - sd_ch(ich)%p_bio) * bed_ero%sed * sd_ch(ich)%p_conc
      bed_ero%no3 = 0.
      bed_ero%solp = sd_ch(ich)%p_bio * bed_ero%sed * sd_ch(ich)%p_conc
      bed_ero%no2 = 0.
      rto = bed_ero%flo / ht1%flo
      !ob(icmd)%tsin(:) = (1. - rto) * ob(icmd)%tsin(:)
      !ht1 = ht1 + bed_ero
      
      !! sediment flood plain deposition - tons and mm
      !! mm = t / (5.*bd*w*l) -> assume fp width = 5*chw; len(m)=1000.*km; bd=1.0 t/m3; mm=1000.*m
      sd_ch(ich)%fp_km2 = 5. * sd_ch(ich)%chw * sd_ch(ich)%chl / 1000.
      ch_morph(ich)%fp_t = fp_dep%sed
      ch_morph(ich)%fp_mm = ch_morph(ich)%fp_t / (5. * sd_ch(ich)%chw * sd_ch(ich)%chl)

      !! daily bank erosion in t and m - calculate w/yr later
      ch_morph(ich)%ebank_m = ebank_m
      ch_morph(ich)%ebank_t = ebank_t
      
      !! daily bed erosion in t and m - calculate w/yr later
      ch_morph(ich)%ebtm_m = ebtm_m
      ch_morph(ich)%ebtm_t = ebtm_t
      
      !! add to ordered sediment budget
      ch_morph_ord(iord) = ch_morph_ord(iord) + ch_morph(ich)

      !! basin sediment budget outputs - don't sum during skip years
      if (time%yrs > pco%nyskip) then
        bsn_sedbud%ebank_t = bsn_sedbud%ebank_t + ch_morph(ich)%ebank_t
        bsn_sedbud%ebtm_t = bsn_sedbud%ebtm_t + ch_morph(ich)%ebtm_t
        bsn_sedbud%fp_t = bsn_sedbud%fp_t + ch_morph(ich)%fp_t
      end if
        
      end if        ! inflow>0

      return
      end subroutine sd_channel_sediment3

