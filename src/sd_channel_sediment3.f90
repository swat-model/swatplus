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

      implicit none     

      integer :: iob = 0            !               |object number
      integer :: ihru = 0
      integer :: iihru = 0
      integer :: ires = 0
      real :: rto = 0.
      real :: rto1 = 0.
      real :: trap_eff = 0.         !frac           |trap efficiency in the flood plain
      real :: cohesion = 0.         !               |soil bank cohesion 
      real :: b_exp = 0.            !               |exponent for bank erosion equation
      real :: vel_fall = 0.         !m/s            |fall velocity of sediment particles in channel
      real :: dep_fall = 0.         !m              |fall depth of sediment particles in channel
      real :: del_rto = 0.          !frac           |fraction of sediment deposited in channel
      real :: ebtm_m = 0.           !m              |erosion of bottom of channel
      real :: ebank_m = 0.          !m              |meander cut on one side
      real :: ebtm_t = 0.           !tons           |bottom erosion
      real :: ebank_t = 0.          !tons           |bank erosion
      real :: shear_btm_cr = 0.     !               |
      real :: shear_btm = 0.        !               |  
      real :: inflo = 0.                 !m^3            |inflow water volume
      real :: inflo_rate = 0.            !m^3/s          |inflow rate
      real :: flo_time = 0.              !s              |estimate of total flow time through the channel
      real :: bf_flow = 0.          !m3/s           |bankfull flow rate * adjustment factor
      real :: pk_rto = 0.           !ratio          |peak to mean flow rate ratio
      real :: bd_fac = 0.           !               |bulk density factor for critical velocity calculation
      real :: cohes_fac = 0.        !               |cohesion factor for critical velocity calculation
      real :: florate               !m^3/s          |flow rate below the triangle for flow lasting more than a day
      real :: vel = 0.
      real :: veg = 0.
      real :: vel_cr = 0.
      real :: rad_curv = 0.
      real :: vel_bend = 0.
      real :: vel_rch = 0.
      real :: arc_len = 0.
      real :: prot_len = 0.
      real :: h_rad = 0.
      real :: fp_m2 = 0.
      real :: exp_co = 0.
      real :: florate_ob = 0.
      real :: precip = 0.
      real :: flovol_ob = 0.
      real :: wet_fill = 0.
      real :: ave_rate
      real :: v_vc = 0.
      real :: m_exhaust = 0.
      real :: dur_scale = 0.
      !!

      ich = isdch
      iob = sp_ob1%chandeg + jrch - 1

      ebtm_m = 0.
      ebank_m = 0.
      ebtm_t = 0.
      ebank_t = 0.
      fp_dep = hz 
      ch_dep = hz 
      bank_ero = hz 
      bed_ero = hz 
      ch_trans = hz
      ch_wat_d(ich)%precip = 0.

      !! calculate channel sed and nutrient processes if inflow > 0
      if (ht1%flo > 1.e-6) then

      !! calculate peak daily flow
      pk_rto = sd_ch(ich)%pk_rto * (1. + 2.66 * (ob(icmd)%area_ha / 100.) ** (-.3))
      peakrate = pk_rto * ht1%flo / 86400.     !m3/s

      !! interpolate rating curve using peak rate
      call rcurv_interp_flo (ich, peakrate)
      !! use peakrate as flow rate
      h_rad = rcurv%xsec_area / rcurv%wet_perim
      vel = h_rad ** .6666 * Sqrt(sd_ch(ich)%chs) / (sd_ch(ich)%chn + .001)
      vel = peakrate / rcurv%xsec_area
      rttime = sd_ch(ich)%chl / (3.6 * vel)

      !! add precip to inflow - km * m * 1000 m/km * ha/10000 m2 = ha
      ch_wat_d(ich)%area_ha = sd_ch(ich)%chl * sd_ch(ich)%chw / 10.
      !! m3 = 10. * mm * ha
      precip = 10. * wst(iwst)%weat%precip * ch_wat_d(ich)%area_ha
      ch_wat_d(ich)%precip = precip
      rto = precip / ht1%flo
      ob(icmd)%tsin(:) = (1. + rto) * ob(icmd)%tsin(:)
      ht1%flo = ht1%flo + precip

      !! compute flood plain deposition
      ave_rate = ht1%flo / 86400.     !m3/s

      bf_flow = sd_ch(ich)%bankfull_flo * ch_rcurv(ich)%elev(2)%flo_rate
      florate_ob = ave_rate - bf_flow
      if (florate_ob > 0.) then
        flo_time = 2. * ht1%flo / ave_rate
        !! assume a triangular distribution
        if (flo_time < 86400.) then
          !! flow is over within the day
          flovol_ob = ht1%flo * (((ave_rate - bf_flow) / ave_rate) ** 2)
        else
          !! flow continues over the day - florate is the rate under the triangle
          florate = 2. * ht1%flo - ave_rate
          flovol_ob = ht1%flo * (((ave_rate - bf_flow) / (ave_rate - florate)) ** 2)
        end if
        flovol_ob = Min (0.8 * ht1%flo, flovol_ob)
        !trap_eff = 0.05 * log(sd_ch(ich)%fp_inun_days) + 0.1
        !! trap efficiency from Dynamic SedNet Component Model Reference Guide: Update 2017
        fp_m2 = 3. * sd_ch(ich)%chw * sd_ch(ich)%chl * 1000.
        exp_co = 0.0001 * fp_m2 / florate_ob
        trap_eff = sd_ch(ich)%fp_inun_days * (florate_ob / ave_rate) * (1. - exp(-exp_co))
        trap_eff = Min (1., trap_eff)
        fp_dep%sed = trap_eff * ht1%sed

        !! deposit Particulate P and N in the floodplain
        fp_dep%orgn = trap_eff * sd_ch(ich)%n_dep_enr * ht1%orgn
        fp_dep%sedp = trap_eff * sd_ch(ich)%p_dep_enr * ht1%sedp
        !! trap nitrate and sol P in flood plain - when not simulating flood plain interactions?
        fp_dep%no3 = 0.         !trap_eff * ht1%no3
        fp_dep%solp = 0.        !trap_eff * ht1%solp

        ht1 = ht1 - fp_dep

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

      !! add sediment deposition to calculate mm of deposition over the flood plain later
      ch_morph(ich)%fp_mm = ch_morph(ich)%fp_mm + fp_dep%sed

      !! calc bank erosion
      cohesion = (-87.1 + (42.82 * sd_ch(ich)%ch_clay) - (0.261 * sd_ch(ich)%ch_clay ** 2.) &
                                     + (0.029 * sd_ch(ich)%ch_clay ** 3.))
      cohesion = amax1 (1000., cohesion)    !min 1000 for sandy soils
      veg = exp (-5. * sd_ch(ich)%chd) * 4500. * sd_ch(ich)%cov
      bd_fac = Max (0.001, 0.03924 * sd_ch(ich)%ch_bd * 1000. - 1000.)
      cohes_fac = 0.021 * cohesion + veg
      vel_cr = log10 (2200. * sd_ch(ich)%chd) * (0.0004 * (bd_fac + cohes_fac)) ** 0.5
      !sd_ch(ich)%vcr_coef = 1.
      vel_cr = sd_ch(ich)%vcr_coef * vel_cr

      !! calculate radius of curvature
      rad_curv = ((12. * sd_ch(ich)%chw) * sd_ch(ich)%sinu ** 1.5) /               &
                                            (13. * (sd_ch(ich)%sinu -1.) ** 0.5)
      vel_bend = vel * (1. / rad_curv + 1.)
      vel_rch = 0.33 * vel_bend + 0.66 * vel
      b_exp = 12.3 / sqrt (sd_ch(ich)%ch_clay + 1.)
      b_exp = min (3.5, b_exp)
      if (vel_rch > vel_cr) then
        !! bank erosion m/yr

        dur_scale = 0.0001 * (ob(icmd)%area_ha / 100.) ** (-0.0858)
        v_vc = dur_scale * sd_ch(ich)%chw * (1. / (1. + exp_w(-4. * (vel_rch / vel_cr - 1.))))
        m_exhaust = 0.0002 * sd_ch(ich)%chw
        ebank_m = 1. / (1. / v_vc + 1. / m_exhaust)
        !ebank_m = 0.0001 * sd_ch(ich)%chw * (1. / (1. + exp(-4. * (vel_rch / vel_cr - 1.))) - 0.5)
        !ebank_m = 0.001 / (1. + exp(-4. * (vel_rch / vel_cr) / sd_ch(ich)%chw))
        !ebank_m = 0.00024 * (vel_rch / vel_cr) ** sd_ch(ich)%bank_exp
      else
        ebank_m = 0.
      end if

      !! write for Peter
      !if (ich == 2133) then
      !write () time%day, time%yrc, ich, sd_ch(ich)%chw, sd_ch(ich)%chw, sd_ch(ich)%chl,   &
      !    sd_ch(ich)%chn, sd_ch(ich)%sinu, sd_ch(ich)%pk_rto, pk_rto, peakrate, vel,      &
      !    sd_ch(ich)%ch_clay, cohesion, sd_ch(ich)%cov, veg, cohes_fac, sd_ch(ich)%ch_bd, &
      !    bd_fac, sd_ch(ich)%vcr_coef, vel_cr, sd_ch(ich)%bank_exp, ebank_m, "  0.24 ")
      !end if

      ch_morph(ich)%w_yr = ch_morph(ich)%w_yr + ebank_m

      !! mass of sediment eroded -> t = 1000 * bankcut (mm) * depth (m) * lengthcut (m) * bd (t/m3)
      !! arc length = 0.33 * meander wavelength * sinuosity 
      !arc_len = 0.66 *  (12. * sd_ch(ich)%chw) * sd_ch(ich)%sinu
      arc_len = 0.25 * sd_ch(ich)%chl
      prot_len = arc_len * sd_ch(ich)%arc_len_fr
      !ebank_t = ebank_m * sd_ch(ich)%chd * sd_ch(ich)%arc_len_fr * prot_len * sd_ch(ich)%ch_bd
      ebank_t = 1000. * ebank_m * sd_ch(ich)%chd * arc_len * sd_ch(ich)%ch_bd
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
        vel_cr = 0.293 * (sd_ch(ich)%d50) ** 0.5
        if (vel > vel_cr) then
          !! bed erosion m/yr
          ebtm_m = 0.0001 * (vel_rch / vel_cr) ** sd_ch(ich)%bed_exp
        end if
        !! calc mass of sediment eroded -> t = m * width (m) * length (km) * 1000 m/km * bd (t/m3)
        ebtm_t = 1000. * ebtm_m * sd_ch(ich)%chw * sd_ch(ich)%chl * sd_ch(ich)%ch_bd
      end if
      ch_morph(ich)%d_yr = ch_morph(ich)%d_yr + ebtm_m

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

      end if        ! inflow>0

      return
      end subroutine sd_channel_sediment3
