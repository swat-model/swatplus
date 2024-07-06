      subroutine sd_channel_sediment3

      use sd_channel_module
      use hydrograph_module
      use time_module
    
      implicit none     
    
      integer :: iob                !               |object number
      real :: trap_eff              !frac           |trap efficiency in the flood plain
      real :: cohesion              !               |soil bank cohesion 
      real :: b_exp                 !               |exponent for bank erosion equation
      real :: vel_fall              !m/s            |fall velocity of sediment particles in channel
      real :: dep_fall              !m              |fall depth of sediment particles in channel
      real :: del_rto               !frac           |fraction of sediment deposited in channel
      real :: conc_chng             !               |change in concentration (and mass) in channel sol and org N and P
      real :: ebtm_m                !m              |erosion of bottom of channel
      real :: ebank_m               !m              |meander cut on one side
      real :: ebtm_t                !tons           |bottom erosion
      real :: ebank_t               !tons           |bank erosion
      real :: shear_btm_cr          !               |
      real :: shear_btm             !               |  
      real :: bf_flow               !m3/s           |bankfull flow rate * adjustment factor
      real :: pk_rto                !ratio          |peak to mean flow rate ratio
      real :: bd_fac                !               |bulk density factor for critical velocity calculation
      real :: cohes_fac             !               |cohesion factor for critical velocity calculation
      !real :: qman                  !m^3/s or m/s   |flow rate or flow velocity
      real :: vel, veg, vel_cr, rad_curv, vel_bend, vel_rch, arc_len, prot_len, h_rad
      real :: fp_m2, exp_co, florate_ob
      
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
      
      !! calculate channel sed and nutrient processes if inflow > 0
      if (ht1%flo > 1.e-6) then
      
      !! Another eq from Peter - Qmax=Qmean*(1+2.66*Drainage Area^-.3)
      pk_rto = 0.2 + 0.5 / 250. * ob(icmd)%area_ha
      pk_rto = Max (1., pk_rto)
      pk_rto = 1. + 2.66 * (ob(icmd)%area_ha / 100.) ** (-.3)
      !pk_rto = 1. + 1.33 * (ob(icmd)%area_ha / 100.) ** (-.3)
      !pk_rto = 1. + 2. * (ob(icmd)%area_ha / 100.) ** (-.3)
      !pk_rto = Min (2., pk_rto)
      !pk_rto = 1.5
      peakrate = pk_rto * ht1%flo / 86400.     !m3/s
        
      !! interpolate rating curve using peak rate
      call rcurv_interp_flo (ich, peakrate)
      !! use peakrate as flow rate
      h_rad = rcurv%xsec_area / rcurv%wet_perim
      sd_ch(ich)%chn = 0.39 * sd_ch(ich)%chs ** 0.38 * h_rad ** (-0.16)
      sd_ch(ich)%chn = 0.5 + 0.2 * (ob(icmd)%area_ha / 100.) ** (-.3)
      if (ob(icmd)%area_ha <= 5.) then
        sd_ch(ich)%chn = 0.13
      end if
      if (ob(icmd)%area_ha >= 2500.) then
        sd_ch(ich)%chn = 0.03
      end if
      if (ob(icmd)%area_ha > 5. .and. ob(icmd)%area_ha >= 2500.) then
        sd_ch(ich)%chn = 0.2 / log(ob(icmd)%area_ha / 100.)
      end if
      sd_ch(ich)%chn = Min (0.15, sd_ch(ich)%chn)
      sd_ch(ich)%chn = Max (0.02, sd_ch(ich)%chn)
      vel = h_rad ** .6666 * Sqrt(sd_ch(ich)%chs) / (sd_ch(ich)%chn + .001)
      !vel = peakrate / rcurv%xsec_area
      
      !! compute flood plain deposition
      !sd_ch(ich)%bankfull_flo = 3.0
      bf_flow = sd_ch(ich)%bankfull_flo * ch_rcurv(ich)%elev(2)%flo_rate
      florate_ob = (ht1%flo / 86400.) - bf_flow
      if (florate_ob > 0.) then
        trap_eff = 0.05 * log(sd_ch(ich)%fp_inun_days) + 0.1
        fp_m2 = 3. * sd_ch(ich)%chw * sd_ch(ich)%chl * 1000.
        exp_co = 0.00001 * fp_m2 / florate_ob
        trap_eff = sd_ch(ich)%fp_inun_days * (florate_ob / peakrate) * (1. - exp(-exp_co))
        fp_dep%sed = trap_eff * ht1%sed
        
        !! deposit Particulate P and N in the floodplain
        fp_dep%orgn = trap_eff * sd_ch(ich)%n_dep_enr * ht1%orgn
        fp_dep%sedp = trap_eff * sd_ch(ich)%p_dep_enr * ht1%sedp
        !! trap nitrate and sol P in flood plain - when not simulating flood plain interactions?
        fp_dep%no3 = trap_eff * ht1%no3
        fp_dep%solp = trap_eff * ht1%solp
      end if
      !fp_dep = hz !***jga
      ch_morph(ich)%fp_mm = ch_morph(ich)%fp_mm + fp_dep%sed
      
      ht2 = ht1 - fp_dep
      
      !! calculate channel deposition based on fall velocity - SWRRB book
      !! assume particle size = 0.03 mm -- median silt size
      vel_fall = 411. * sd_ch(ich)%part_size ** 2     ! m/h
      dep_fall = vel_fall * rcurv%ttime
      !! assume bankfull flow depth
      if (dep_fall < sd_ch(ich)%chd) then
        del_rto = 1. - .5 * dep_fall / sd_ch(ich)%chd
      else
        del_rto = .5 * sd_ch(ich)%chd / dep_fall
      end if
      
      ch_dep%sed = (1. - del_rto) * ht2%sed
      ch_dep%orgn = sd_ch(ich)%n_dep_enr * (1. - del_rto) * ht2%orgn
      ch_dep%sedp = sd_ch(ich)%p_dep_enr * (1. - del_rto) * ht2%sedp
      ht2 = ht2 - ch_dep

      !! calculate in channel nutrient losses -  input delivery ratio
      !! calculate in channel nutrient transformations
      conc_chng = 1. - exp(-sd_ch(ich)%n_sol_part * rcurv%ttime)
      ch_trans%no3 = conc_chng * ht2%no3
      ch_trans%orgn = -ch_trans%no3
      conc_chng = 1. - exp(-sd_ch(ich)%p_sol_part * rcurv%ttime)
      ch_trans%solp = conc_chng * ht2%solp
      ch_trans%sedp = -ch_trans%solp
      
      !! calc bank erosion
      cohesion = (-87.1 + (42.82 * sd_ch(ich)%ch_clay) - (0.261 * sd_ch(ich)%ch_clay ** 2.) &
                                     + (0.029 * sd_ch(ich)%ch_clay ** 3.))
      cohesion = amax1 (1000., cohesion)    !min 1000 for sandy soils
      veg = exp (-5. * sd_ch(ich)%chd) * 4500. * sd_ch(ich)%cov
      bd_fac = Max (0.001, 0.03924 * sd_ch(ich)%ch_bd * 1000. - 1000.)
      cohes_fac = 0.021 * cohesion + veg
      vel_cr = log10 (2200. * sd_ch(ich)%chd) * (0.0004 * (bd_fac + cohes_fac)) ** 0.5
        
      !cohesion = (15. / (15.3 - 0.438 * sd_ch(ich)%ch_clay + 0.0044 * sd_ch(ich)%ch_clay ** 2.)) * 1000.
      !veg = exp (-5. * sd_ch(ich)%chd) * cha1(icha)%dat%cov                              !function of cover factor
      !vel_cr = log10 (8.8 * sd_ch(ich)%chd / 0.004) * (0.0004 * ((sd_ch(ich)%ch_bd *     &
      !                  1000. - 1000.) * 9.81 * 0.004 + 0.021 * cohesion + veg)) ** 0.5
      
      !! calculate radius of curvature
      rad_curv = ((12. * sd_ch(ich)%chw) * sd_ch(ich)%sinu ** 1.5) / (13. * (sd_ch(ich)%sinu -1.) ** 0.5)
      vel_bend = vel * (1. / rad_curv + 1.)
      vel_rch = 0.33 * vel_bend + 0.66 * vel
      b_exp = 12.3 / sqrt (sd_ch(ich)%ch_clay + 1.)
      b_exp = min (3.5, b_exp)
      if (vel_rch > vel_cr) then   ! .and. rcurv%dep / sd_ch(ich)%chd > 0.1) then
        ebank_m = 0.0024 * (vel_rch / vel_cr) ** b_exp    !bank erosion m/yr
      else
        ebank_m = 0.
      end if
      ch_morph(ich)%w_yr = ch_morph(ich)%w_yr + ebank_m

      !! calc mass of sediment eroded -> t = bankcut (m) * depth (m) * lengthcut (m) * bd (t/m3)
      !! arc length = 0.33 * meander wavelength * sinuosity  -> protected length 
      arc_len = 0.33 *  (12. * sd_ch(ich)%chw) * sd_ch(ich)%sinu
      prot_len = arc_len * sd_ch(ich)%arc_len_fr
      prot_len = 0.2 * sd_ch(ich)%chl * 1000.
      !rad_curv = (12. * sd_ch(ich)%chw * sd_ch(ich)%sinu ** 1.5) /        &
      !                                 (13. * (sd_ch(ich)%sinu - 0.999) ** 0.5)
      !cutbank_adj = 2.57 - 0.36 * log(rad_curv / sd_ch(ich)%chw)
      !ebank_t = ebank_m * sd_ch(ich)%chd * sd_ch(ich)%arc_len_fr * prot_len * sd_ch(ich)%ch_bd
      !ebank_t = 0.8 * ebank_t     !assume 80% wash load and 20% bed deposition
      !ebank_t = max (0., ebank_t)
      !ebank_t = 1000. * (vel_rch * sd_ch(ich)%chs) ** 2. * (1. - sd_ch(ich)%cov) * (sd_ch(ich)%ch_clay + 1.)
      !sd_ch(ich)%pk_rto = 500.
      ebank_t = (1000. * sd_ch(ich)%pk_rto * vel_rch * sd_ch(ich)%chs) ** 2. *           &
                                                (1. - sd_ch(ich)%ch_clay / 100.)
      prot_len = 0.2 * sd_ch(ich)%chl
      ebank_t = ebank_t * prot_len
      bank_ero%sed = ebank_t
      !! calculate associated nutrients
      bank_ero%orgn = bank_ero%sed * sd_ch(ich)%n_conc
      bank_ero%sedp = (1. - sd_ch(ich)%p_bio) * bank_ero%sed * sd_ch(ich)%p_conc
      bank_ero%no3 = 0.
      bank_ero%solp = sd_ch(ich)%p_bio * bank_ero%sed * sd_ch(ich)%p_conc
      bank_ero%no2 = 0.
      ht2 = ht2 + bank_ero
      
      !! calculate bed erosion
      !! no downcutting below equilibrium slope
      if (sd_ch(ich)%chs > 0.000001) then       !sd_ch(ich)%chseq) then
        !! calc critical shear and shear on bottom of channel
        shear_btm_cr = sd_ch(ich)%d50
        shear_btm = 9800. * rcurv%dep * sd_ch(ich)%chs   !! Pa = N/m^2 * m * m/m
        !! critical shear function of d50
        vel_cr = 0.293 * (sd_ch(ich)%d50) ** 0.5
        vel_cr = 1.
        if (vel > vel_cr) then
          ebtm_m = 0.0001 * (vel_rch / vel_cr) ** 1.5    !bed erosion m/yr
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
      ht2 = ht2 + bed_ero
      
      end if        ! inflow>0

      !! channel sediment budget for output
      ch_sed_bud(ich)%in_sed = ht1%sed
      ch_sed_bud(ich)%out_sed = ht2%sed
      ch_sed_bud(ich)%fp_dep = fp_dep%sed
      ch_sed_bud(ich)%ch_dep = ch_dep%sed
      ch_sed_bud(ich)%bank_ero = bank_ero%sed
      ch_sed_bud(ich)%bed_ero = bed_ero%sed

      !! channel nutrient budget for output
      ch_sed_bud(ich)%in_no3 = ht1%no3
      ch_sed_bud(ich)%in_orgn = ht1%orgn
      ch_sed_bud(ich)%out_no3 = ht2%no3
      ch_sed_bud(ich)%out_orgn = ht2%orgn
      ch_sed_bud(ich)%fp_no3 = fp_dep%no3
      ch_sed_bud(ich)%bank_no3 = bank_ero%no3
      ch_sed_bud(ich)%bed_no3 = bed_ero%no3
      ch_sed_bud(ich)%fp_orgn = fp_dep%orgn
      ch_sed_bud(ich)%ch_orgn = ch_dep%orgn
      ch_sed_bud(ich)%bank_orgn = bank_ero%orgn
      ch_sed_bud(ich)%bed_orgn = bed_ero%orgn
      ch_sed_bud(ich)%in_solp = ht1%solp
      ch_sed_bud(ich)%in_orgp = ht1%sedp
      ch_sed_bud(ich)%out_solp = ht2%solp
      ch_sed_bud(ich)%out_orgp = ht2%sedp
      ch_sed_bud(ich)%fp_solp = fp_dep%solp
      ch_sed_bud(ich)%bank_solp = bank_ero%solp
      ch_sed_bud(ich)%bed_solp = bed_ero%solp
      ch_sed_bud(ich)%fp_orgp = fp_dep%sedp
      ch_sed_bud(ich)%ch_orgp = ch_dep%sedp
      ch_sed_bud(ich)%bank_orgp = bank_ero%sedp
      ch_sed_bud(ich)%bed_orgp = bed_ero%sedp
      ch_sed_bud(ich)%no3_orgn = ch_trans%no3
      ch_sed_bud(ich)%solp_orgp = ch_trans%solp


      return
      end subroutine sd_channel_sediment3