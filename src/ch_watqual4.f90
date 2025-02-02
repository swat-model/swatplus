      subroutine ch_watqual4

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs in-stream nutrient transformations and water
!!    quality calculations

      use channel_module
      use hydrograph_module      
      use climate_module
      use channel_data_module
      use sd_channel_module
      use water_body_module
      
      implicit none

      real :: tday = 0.
      real :: wtmp = 0.
      real :: fll = 0.
      real :: gra = 0.
      real :: lambda = 0.
      real :: fnn = 0.
      real :: fpp = 0.
      real :: algi = 0.
      real :: fl_1 = 0.
      real :: xx = 0.
      real :: yy = 0.
      real :: zz = 0.
      real :: ww = 0.
      real :: cordo = 0.
      real :: f1 = 0.
      real :: algcon = 0.
      real :: thgra = 1.047
      real :: thrho = 1.047
      real :: thrs1 = 1.024
      real :: thrs2 = 1.074
      real :: thrs3 = 1.074
      real :: thrs4 = 1.024
      real :: thrs5 = 1.024
      real :: thbc1 = 1.083
      real :: thbc2 = 1.047
      real :: thbc3 = 1.047
      real :: thbc4 = 1.047
      real :: thrk1 = 1.047
      real :: thrk2 = 1.024
      real :: thrk3 = 1.024
      real :: thrk4 = 1.060
      real :: soxy = 0.        !mg O2/L       |saturation concetration of dissolved oxygen
      real :: theta
      real :: rs2_s = 0.
      real :: rs3_s = 0.
      real :: rk4_s = 0.
      real :: disoxin = 0.
      real :: dispin = 0.
      real :: ammoin = 0.
      real :: cinn = 0.
      real :: algin = 0.
      real :: factk = 0.
      real :: wq_semianalyt
      real :: alg_m1 = 0.
      real :: alg_m = 0.
      real :: alg_m2 = 0.
      real :: alg_no3_m = 0.
      real :: alg_nh4_m = 0.
      real :: alg_p_m = 0.
      real :: alg_set = 0.
      real :: algcon_out = 0.
      real :: cbodo = 0.
      real :: cbodoin = 0.
      real :: rk1_k = 0.
      real :: wq_k2m 
      real :: rk1_m = 0.
      real :: rk3_k = 0.
      real :: factm = 0.
      real :: bc1_k = 0.
      real :: bc3_k = 0.
      real :: rs4_k = 0.
      real :: bc3_m = 0.
      real :: rk2_m = 0.
      real :: rk2_k = 0.
      real :: alg_m_o2 = 0.
      real :: bc2_k = 0.
      real :: bc1_m = 0.
      real :: bc2_m = 0.
      real :: bc4_k = 0.
      real :: bc4_m = 0.
      real :: rs5_k = 0.
      real :: flo_rate = 0.
      integer :: iwgn = 0

      jrch = isdch
      icmd = sp_ob1%chandeg + jrch - 1
      !! calculate flow duration
      tday = rcurv%ttime / 24.0
      tday = amin1 (1., tday)
      rt_delt = 1.
      
      !! ht3 is incoming concentration
      ht3%orgn = 1000. * ht1%orgn / ht1%flo
      ht3%sedp = 1000. * ht1%sedp / ht1%flo
      ht3%no3 = 1000. * ht1%no3 / ht1%flo
      ht3%solp = 1000. * ht1%solp / ht1%flo
      ht3%chla = 1000. * ht1%chla / ht1%flo
      ht3%nh3 = 1000. * ht1%nh3 / ht1%flo
      ht3%no2 = 1000. * ht1%no2 / ht1%flo
      ht3%cbod = 1000. * ht1%cbod / ht1%flo
      ht3%dox = 1000. * ht1%dox / ht1%flo


      !! calculate temperature in stream Stefan and Preudhomme. 1993.  Stream temperature estimation 
      !! from air temperature.  Water Res. Bull. p. 27-45 SWAT manual equation 2.3.13
      wtmp = 5.0 + 0.75 * wst(iwst)%weat%tave
      if (wtmp <= 0.) wtmp = 0.1
      ht2%temp = wtmp

      !! benthic sources/losses in mg   
      !ch_nut(jnut)%rs2 = 5.   !!***jga
      rs2_s =  Theta(ch_nut(jnut)%rs2,thrs2,wtmp) * ben_area    !ch_hyd(jhyd)%l *ch_hyd(jhyd)%w * rt_delt
      rs3_s =  Theta(ch_nut(jnut)%rs3,thrs3,wtmp) * ben_area    !ch_hyd(jhyd)%l *ch_hyd(jhyd)%w * rt_delt
      rk4_s =  Theta(ch_nut(jnut)%rk4,thrk4,wtmp) * ben_area    !ch_hyd(jhyd)%l *ch_hyd(jhyd)%w * rt_delt

      !! ht3 = concentration of incoming nutrients
      flo_rate = ht1%flo / 86400.     !m3/s
      !! interpolate rating curve using peak rate
      call rcurv_interp_flo (ich, flo_rate)
      rchdep = rcurv%dep
      if (ht1%flo > 0. .and. rchdep > 0.) then
        disoxin = ht3%dox - rk4_s / (ht1%flo * 1000.)   !m3*1000 l/m3 = liters
        disoxin = max (0., disoxin)
        dispin = ht3%solp + rs2_s / (ht1%flo * 1000.)
        ammoin = ht3%nh3 + rs3_s / (ht1%flo * 1000.)

        !! calculate effective concentration of available nitrogen QUAL2E equation III-15
        cinn = ch_stor(jrch)%nh3 + ch_stor(jrch)%no3

        !! calculate saturation concentration for dissolved oxygen
        !! QUAL2E section 3.6.1 equation III-29
        ww = -139.34410 + (1.575701e05 / (wtmp + 273.15))
        xx = 6.642308e07 / ((wtmp + 273.15)**2)
        yy = 1.243800e10 / ((wtmp + 273.15)**3)
        zz = 8.621949e11 / ((wtmp + 273.15)**4)
        soxy = Exp(ww - xx + yy - zz)
        if (soxy < 1.e-6) soxy = 0. 
        !! end initialize concentrations

        !! O2 impact calculations
        !! calculate nitrification rate correction factor for low oxygen QUAL2E equation III-21
        if (ht3%dox < 0.001) ht3%dox = 0.001
        if (ht3%dox > 30.) ht3%dox = 30.
        cordo = 1.0 - Exp(-0.6 * ht3%dox)
        !! end O2 impact calculations
       
        !! algal growth
        !! calculate chlorophyll-a concentration at end of day QUAL2E equation III-1
        algcon = 1000. * ht3%chla / ch_nut(jnut)%ai0
        algin = 1000. * ht3%chla / ch_nut(jnut)%ai0
         
        !! calculate light extinction coefficient (algal self shading) QUAL2E equation III-12
        if (ch_nut(jnut)%ai0 * algcon > 1.e-6) then
          lambda = ch_nut(jnut)%lambda0 + (ch_nut(jnut)%lambda1 * ch_nut(jnut)%ai0 * algcon)   &
              + ch_nut(jnut)%lambda2 * (ch_nut(jnut)%ai0 * algcon) ** (.66667)
        else
          lambda = ch_nut(jnut)%lambda0
        endif

        if (lambda > ch_nut(jnut)%lambda0) lambda = ch_nut(jnut)%lambda0
        !! calculate algal growth limitation factors for nitrogen
        !! and phosphorus QUAL2E equations III-13 & III-14
        fnn = cinn / (cinn + ch_nut(jnut)%k_n)
        fpp = ht3%solp / (ht3%solp + ch_nut(jnut)%k_p)

        !! calculate daylight average, photosynthetically active, light intensity QUAL2E equation III-8
        !! Light Averaging Option # 2
        iwgn = wst(iwst)%wco%wgn
        if (wgn_pms(iwgn)%daylth > 0.) then
          algi = wst(iwst)%weat%solrad * ch_nut(jnut)%tfact /  wgn_pms(iwgn)%daylth
        else
          algi = 0.00001
        end if

        !! calculate growth attenuation factor for light, based on
        !! daylight average light intensity QUAL2E equation III-7b
        fl_1 = (1. / (lambda * rchdep)) * Log((ch_nut(jnut)%k_l + algi) /     &
                (ch_nut(jnut)%k_l + algi *  (Exp(-lambda * rchdep))))
        fll = 0.92 * (wgn_pms(iwgn)%daylth / 24.) * fl_1

        !ch_nut(jnut)%mumax = 3.   !***jga
        !! calculate local algal growth rate
        if (algcon < 5000.) then
          select case (ch_nut(jnut)%igropt)
          case (1)
            !! multiplicative QUAL2E equation III-3a
            gra = ch_nut(jnut)%mumax * fll * fnn * fpp
          case (2)
            !! limiting nutrient QUAL2E equation III-3b
            gra = ch_nut(jnut)%mumax * fll * Min(fnn, fpp)
          case (3)
            !! harmonic mean QUAL2E equation III-3c
            if (fnn > 1.e-6 .and. fpp > 1.e-6) then
              gra = ch_nut(jnut)%mumax * fll * 2. / ((1. / fnn) + (1. / fpp))
            else
              gra = 0.
            endif
          end select
        end if

        !! calculate algal biomass concentration at end of day (phytoplanktonic algae) QUAL2E equation III-2
        factk = Theta(gra,thgra,wtmp) - Theta(ch_nut(jnut)%rhoq, thrho, wtmp)
        algcon = 1000. * ht3%chla / ch_nut(jnut)%ai0
        alg_m1 = wq_semianalyt (tday, rt_delt, 0., factk, algcon, algin)
         
        alg_m = wq_semianalyt (tday, rt_delt, 0., factk, algcon, algin)
        alg_m2 = alg_m - alg_m1
        !! calculate fraction of algal nitrogen uptake from ammonia pool QUAL2E equation III-18
        f1 = ch_nut(jnut)%p_n * ht3%nh3 / (ch_nut(jnut)%p_n * ht3%nh3 +       &
                          (1. - ch_nut(jnut)%p_n) * ht3%no3 + 1.e-6)
        alg_no3_m = -alg_m * (1. - f1) * ch_nut(jnut)%ai1
        alg_nh4_m = -alg_m * f1 * ch_nut(jnut)%ai1
        alg_P_m = -alg_m * ch_nut(jnut)%ai2
        alg_set = 0.
        if (rchdep > 0.001) alg_set = Theta (ch_nut(jnut)%rs1, thrs1, wtmp) / rchdep 
         
        algcon_out = wq_semianalyt (tday, rt_delt, alg_m, -alg_set, algcon, algin)           
 
        if (algcon_out < 1.e-6) algcon_out = 0.
        if (algcon_out > 5000.) algcon_out = 5000.

        !! calculate chlorophyll-a concentration at end of day QUAL2E equation III-1
        ht3%chla = algcon_out * ch_nut(jnut)%ai0 / 1000.
         
        !! end algal growth 

        !! oxygen calculations
        !! calculate carbonaceous biological oxygen demand at end of day QUAL2E section 3.5 equation III-26
        !! adjust rk1 to m-term and BOD & O2 mass availability
         
        cbodo = min (ht3%cbod, ht3%dox)
        cbodoin = min (ht3%cbod, ht3%dox)
        rk1_k = -Theta (ch_nut(jnut)%rk1, thrk1,wtmp)
        rk1_m = wq_k2m (tday, rt_delt, rk1_k, ht3%cbod, ht3%cbod)
        !! calculate corresponding m-term
        rk3_k=0.
        if (rchdep > 0.001)  rk3_k = -Theta (ch_nut(jnut)%rk3, thrk3, wtmp) / rchdep
        factm = rk1_m
        factk = rk3_k
        ht3%cbod = wq_semianalyt (tday, rt_delt, factm, factk, ht3%cbod, ht3%cbod)

        !! nitrogen calculations
        !! calculate organic N concentration at end of day
        !! QUAL2E section 3.3.1 equation III-16
        bc1_k = Theta(ch_nut(jnut)%bc1,thbc1,wtmp)
        bc3_k = Theta(ch_nut(jnut)%bc3,thbc3,wtmp) 
        bc1_k = bc1_k * 2.
        bc3_k = bc3_k * 2.
        rs4_k = 0.
        if (rchdep > 0.001)  rs4_k = Theta (ch_nut(jnut)%rs4, thrs4, wtmp) / rchdep   

        bc3_m = wq_k2m (tday, rt_delt, -bc3_k, ht3%orgn, ht3%orgn)
        factk = -rs4_k
        factm = bc3_m
        ht3%orgn = wq_semianalyt (tday, rt_delt, factm, factk, ht3%orgn, ht3%orgn)
        if (ht3%orgn <0.) ht3%orgn = 0.

        !! calculate dissolved oxygen concentration if reach at end of day QUAL2E section 3.6 equation III-28

        rk2_m = Theta (ch_nut(jnut)%rk2, thrk2, wtmp) * soxy
        rk2_k = Theta (ch_nut(jnut)%rk2, thrk2, wtmp) 

        alg_m_o2 = ch_nut(jnut)%ai4 * alg_m2 + ch_nut(jnut)%ai3 * alg_m1
     
        factk = - rk2_k
        bc2_k = -Theta (ch_nut(jnut)%bc2, thbc2, wtmp)
        bc1_m = wq_k2m (tday, rt_delt, factk, ht3%nh3, ammoin)
        bc2_m = wq_k2m (tday, rt_delt, bc2_k, ht3%no2, ht3%no2)
        factm = rk1_m + rk2_m - rs4_k + bc1_m * ch_nut(jnut)%ai5 + bc2_m * ch_nut(jnut)%ai6
        ht3%dox = wq_semianalyt (tday, rt_delt, factm, factk, ht3%dox, ht3%dox)
        if (ht3%dox <0.) ht3%dox = 0.
          
        !! end oxygen calculations        

        !! calculate ammonia nitrogen concentration at end of day QUAL2E section 3.3.2 equation III-17
        factk = -bc1_k
        factm = bc1_m - bc3_m 
        ht3%nh3 = wq_semianalyt (tday, rt_delt, factm, 0., ht3%nh3, ammoin)
        if (ht3%nh3 < 1.e-6) ht3%nh3 = 0.
  
        !! calculate concentration of nitrite at end of day QUAL2E section 3.3.3 equation III-19
        factm = -bc1_m + bc2_m
        ht3%no2 = wq_semianalyt (tday, rt_delt, factm, 0., ht3%no2, ht3%no2)
        if (ht3%no2 < 1.e-6) ht3%no2 = 0.

        !! calculate nitrate concentration at end of day QUAL2E section 3.3.4 equation III-20
        factk = 0.
        factm = -bc2_m
        
        ht3%no3 = wq_semianalyt (tday, rt_delt, factm, 0., ht3%no3, ht3%no3)
        if (ht3%no3 < 1.e-6) ht3%no3 = 0.
        !! end nitrogen calculations

        !! phosphorus calculations
        !! calculate organic phosphorus concentration at end of day QUAL2E section 3.3.6 equation III-24
        bc4_k = Theta (ch_nut(jnut)%bc4, thbc4,wtmp)
        bc4_m = wq_k2m (tday, rt_delt, -bc4_k, ht3%sedp, ht3%sedp) 
        rs5_k = 0.
        !ch_nut(jnut)%rs5 = 0.   !  ***jga
        if (rchdep > 0.001) rs5_k = Theta (ch_nut(jnut)%rs5, thrs5, wtmp) / rchdep 

        factk = -rs5_k
        factm = bc4_m 

        ht3%sedp = wq_semianalyt (tday, rt_delt, factm, factk, ht3%sedp, ht3%sedp)
        if (ht3%sedp < 1.e-6) ht3%sedp = 0.
    
        !! calculate dissolved phosphorus concentration at end of day QUAL2E section 3.4.2 equation III-25
        !factk = 0.
        !factm = -bc4_m + ch_nut(jnut)%ai2 * alg_m
        !ht3%solp = wq_semianalyt (tday, rt_delt, factm, 0., ht3%solp, dispin)
        !if (ht3%solp < 1.e-6) ht3%solp = 0.
        !! calculate dissolved phosphorus concentration at end of day QUAL2E section 3.4.2 equation III-25
        factk = 0.
        factm = -bc4_m + ch_nut(jnut)%ai2 * alg_m
        !ht3%solp = wq_semianalyt (tday, rt_delt, factm, 0., ht5%solp, dispin)
        xx = Theta (ch_nut(jnut)%bc4, thbc4,wtmp) * ht3%sedp
        yy = Theta(ch_nut(jnut)%rs2, thrs2, wtmp) / (sd_chd(jrch)%chd)
        zz = ch_nut(jnut)%ai2 * Theta(gra,thgra,wtmp) * algin
        ht3%solp = ht3%solp + (xx + yy - zz) * tday
        if (ht3%solp < 1.e-6) ht3%solp = 0.
        !! end phosphorus calculations

        !! convert back from concentration to mass for routing
        ht2%orgn = ht3%orgn * ht1%flo / 1000.
        ht2%sedp = ht3%sedp * ht1%flo / 1000.
        ht2%no3 = ht3%no3 * ht1%flo / 1000.
        ht2%solp = ht3%solp * ht1%flo / 1000.
        ht2%chla = ht3%chla * ht1%flo / 1000.
        ht2%nh3 = ht3%nh3 * ht1%flo / 1000.
        ht2%no2 = ht3%no2 * ht1%flo / 1000.
        ht2%cbod = ht3%cbod * ht1%flo / 1000.
        ht2%dox = ht3%dox * ht1%flo / 1000.
      else
        !! all water quality variables set to zero when no flow
        ht2 = hz
        ch_stor(jrch) = hz
      endif
      
      return
      end subroutine ch_watqual4
      
