      subroutine ch_temp
      
      use basin_module
      use input_file_module
      use maximum_data_module
      use channel_data_module
      use sd_channel_module
      use hydrograph_module
      use climate_module
      use output_landscape_module
      use aquifer_module
      use calibration_data_module
      use time_module
      use channel_velocity_module

      implicit none
      
      integer :: iob               
      integer :: ig
      integer :: yrs_to_start

     !parameters for temperature model
      real :: tdx                   
      real :: t_md
      real :: ke_beta
      real :: f_wind
      real :: k_e
      real :: ssff
      real :: h_sr
      real :: e_s
      real :: e_a
      real :: cloud
      real :: e_atm
      real :: h_atm
      real :: numerator
      real :: t_equil
      real :: k_factor
      real :: t_heat_exch
      real :: dep_chan
      real :: wid_chan
      real :: t_sno
      real :: t_gw
      real :: t_surf
      real :: t_lat
      real :: t_air
      real :: t_air_min_av
      real :: t_air_max_av
      real :: surf_lag
      real :: lat_lag
      real :: sno_lag
      real :: gw_lag
      real :: surf_contr
      real :: lat_contr
      real :: gw_contr
      real :: sno_contr
      real :: airlag_d
      real :: surf_lag_coef
      real :: lat_lag_coef
      real :: gw_lag_coef
      real :: sno_coef
      real :: gw_coef
      real :: sur_lat_coef
      real :: wid_flow
      real :: dep_flow
      
      real :: q_lsu_sno
      real :: q_gw
      real :: q_lsu_surf
      real :: q_lsu_lat
      real :: q_lsu_wyld
      
      real :: tw_final
      real :: tw_local
      real :: tw_init
      real :: tw_up 
      integer :: ilsu           !none       |counter
      real :: sw_init           
      real :: sno_init          
      integer :: ielem          
      integer :: ihru 
      integer, dimension(:), allocatable :: ruid_array
      integer :: ru_index
      integer :: ru_count
      real :: const  
      real :: jday
      integer :: i
      real :: rttime
      real :: vc
      real :: tw_local_prev
      real :: trib1_temp
      real :: trib2_temp
      real :: trib1_flo
      real :: trib2_flo
      real :: trib_flo
      real :: tw_def
      real :: tw_mix
      real :: tw_eq
      real :: bulk_co
      real :: eps 
      real :: tdx_cal
      integer :: in

      
      ! Kristin Peters (last update 03.11.2025):
      
      ! =========================================================
      !  Step 1. Initialization and setup
      ! =========================================================
      
      ! get jday and year
      jday = time%day 
      
      ! set weather station
      iob = sp_ob1%chandeg + ich - 1
      iwst = ob(iob)%wst
      ig = wst(iwst)%wco%tgage
      w = wst(iwst)%weat  
      
      ! define default water temperature by old equation
      tw_def = 5.0 + 0.75 * w%tave 
      
      ! initialize ruid_array to store the number of incoming objects
      ru_count = 0
      ru_index = 1

      do in = 1, size(ob(iob)%obtyp_in)
          if (allocated(ob(iob)%obtyp_in) .and. ob(iob)%obtyp_in(in) == "ru") then
              ru_count = count(ob(iob)%obtyp_in == "ru")
          end if
      end do
      
      if (allocated(ruid_array)) deallocate(ruid_array)
      allocate(ruid_array(ru_count))
   
      ! =========================================================
      !  Step 2. Tributary mixing
      ! =========================================================
      
      ! summing the temperature of the incoming tributaries (maximum 2)    
      
      ht1 = ob(iob)%hd(1)
      do in = 1, ob(iob)%rcv_tot
          if (ob(iob)%obtyp_in(in) == "chandeg") then
              trib1_temp = ob(iob)%hin_d(in-1)%temp
              trib2_temp = ob(iob)%hin_d(in)%temp
              trib1_flo = ob(iob)%hin_d(in-1)%flo/86400
              trib2_flo = ob(iob)%hin_d(in)%flo/86400
              trib_flo = trib1_flo + trib2_flo
              if (trib_flo < 1e-6) then             ! if it has tributaries but the flows are 0, estimate from old equation (to avoid division by 0)
                  ht1%temp = 5.0 + 0.75 * w%tave
              else
                  ht1%temp = (trib1_temp * trib1_flo + trib2_temp * trib2_flo) / trib_flo
              end if
          else                  ! if it does not have any tributaries, also use old equation to avoid 0
              ht1%temp = 5.0 + 0.75 * w%tave
          end if
          if (ob(iob)%obtyp_in(in) == "ru") then            ! get lsu ID 
              ruid_array(ru_index) = ob(iob)%obtypno_in(in)
              ru_index = ru_index + 1
          end if
      end do  
      
      ! =========================================================
      !  Step 3. mixing of components
      ! =========================================================

      ! initialize the mixing parameters

      sno_lag = w_temp(0)%sno_lag            !snow lag time (1-3)
      gw_lag = w_temp(0)%gw_lag              !gw lag time (200-365)
      t_air = w%tave                         !air temperature
      lat_lag = w_temp(0)%lat_lag            !lat lag time (5-10)
      surf_lag = w_temp(0)%surf_lag          !surf_lag time (2-5)
      
      surf_lag_coef = w_temp(0)%surf_lag_coef    !surface air lag calibration coefficient (used also for snow)
      gw_lag_coef = w_temp(0)%gw_lag_coef        !gw air lag calibration coefficient
      lat_lag_coef = w_temp(0)%lat_lag_coef      !lateral air lag calibration coefficient
      sno_coef = w_temp(0)%sno_mlt               !contribution of snowmelt for contribution scenarios (currently not used)
      gw_coef = w_temp(0)%gw                     !contribution of gw flow for contribution scenarios (currently not used)
      sur_lat_coef = w_temp(0)%sur_lat           !contribution of surface and lateral flow for contribution scenarios (currently not used)

      
      ! -------------------get lsu wb outputs: snomlt, gwflo, surq, latq and wyld----------------
      ! seems not possible to get them directly from lsu_output, all 0
      ! so here calculate again, in a newly created array lsu_wb_d, which is the same as ruwb_d, but does not overwrite it
      ! maybe there is a way to get the variables in a better way

      !initialize 0 outputs
      do ilsu = 1, db_mx%lsu_out
          lsu_wb_d(ilsu) = hwbz
          q_lsu_sno = 0
          q_lsu_surf = 0
          q_lsu_lat = 0
          q_lsu_wyld = 0
      end do

      ! summing HRU output for the landscape unit
      do ru_index = 1, ru_count
          ilsu = ruid_array(ru_index) 
          do ielem = 1, lsu_out(ilsu)%num_tot
            ihru = lsu_out(ilsu)%num(ielem)
            if (lsu_elem(ihru)%ru_frac > 1.e-9) then
              const = lsu_elem(ihru)%ru_frac
              if (lsu_elem(ihru)%obtyp == "hru") then
                lsu_wb_d(ilsu) = ruwb_d(ilsu) + hwb_d(ihru) * const
              end if
              ! summing HRU_LTE output
              if (lsu_elem(ihru)%obtyp == "hlt") then
                lsu_wb_d(ilsu) = ruwb_d(ilsu) + hltwb_d(ihru) * const
              end if
            end if
          end do 
          ! define components for basic mixing
          q_lsu_sno = q_lsu_sno + lsu_wb_d(ilsu)%snomlt
          q_lsu_surf = q_lsu_surf + lsu_wb_d(ilsu)%surq_gen
          q_lsu_lat = q_lsu_lat + lsu_wb_d(ilsu)%latq
          q_lsu_wyld = q_lsu_wyld + lsu_wb_d(ilsu)%wateryld
      end do    
              
      if (q_lsu_wyld == 0) then     ! to avoid crash if division by 0 in tw_local
          q_lsu_wyld = 1e-6
      end if
        
      ! add gw flow 
      q_gw = hdsep1%flo_gwsw / 86400   
      if (q_gw < 10) then       ! model runs into error if the number is too high, set threshold 10 m3/s
          q_gw = hdsep1%flo_gwsw / 86400
      else
          q_gw = 10
      end if     

      ! previous local water temperature
      tw_local_prev = ch_out_d(ich)%temp
      
      ! ---------------------calculate average temperature of the previous x days ------------------------------------
     ! surface lag
      if (time%yrs == 1) then !only if simulation year = 1
          surf_lag = min (jday, surf_lag)
      else 
          surf_lag = surf_lag
      end if 
      
      yrs_to_start = time%yrs - tmp(ig)%yrs_start   !model year - (tmp start year is model year)
      
      if (jday <= surf_lag) then
          t_air_max_av = (sum(tmp(ig)%ts(1:jday,yrs_to_start))+sum(tmp(ig)%ts(jday+365-surf_lag+1:365,yrs_to_start-1)))/surf_lag
          t_air_min_av = (sum(tmp(ig)%ts2(1:jday,yrs_to_start))+sum(tmp(ig)%ts2(jday+365-surf_lag+1:365,yrs_to_start-1)))/surf_lag
      else
          t_air_max_av = sum(tmp(ig)%ts(jday-surf_lag+1:jday,yrs_to_start))/surf_lag
          t_air_min_av = sum(tmp(ig)%ts2(jday-surf_lag+1:jday,yrs_to_start))/surf_lag
      end if
      t_surf = max(0.01,(t_air_max_av + t_air_min_av) / 2)
      
    ! lateral lag
      if (time%yrs == 1) then !only if simulation year = 1
          lat_lag = min (jday, lat_lag)
      else 
          lat_lag = lat_lag
      end if 
      
      yrs_to_start = time%yrs - tmp(ig)%yrs_start   !model year - tmp start year is model year
      
      if (jday <= lat_lag) then
          t_air_max_av = (sum(tmp(ig)%ts(1:jday,yrs_to_start))+sum(tmp(ig)%ts(jday+365-lat_lag+1:365,yrs_to_start-1)))/lat_lag
          t_air_min_av = (sum(tmp(ig)%ts2(1:jday,yrs_to_start))+sum(tmp(ig)%ts2(jday+365-lat_lag+1:365,yrs_to_start-1)))/lat_lag
      else
          t_air_max_av = sum(tmp(ig)%ts(jday-lat_lag+1:jday,yrs_to_start))/lat_lag
          t_air_min_av = sum(tmp(ig)%ts2(jday-lat_lag+1:jday,yrs_to_start))/lat_lag
      end if
      t_lat = max(0.01,(t_air_max_av + t_air_min_av) / 2)
      
    ! gw lag
      if (time%yrs == 1) then !only if simulation year = 1
          gw_lag = min (jday, gw_lag)
      else 
          gw_lag = gw_lag
      end if 
      
      yrs_to_start = time%yrs - tmp(ig)%yrs_start   !model year -tmp start year is model year
      
      if (jday <= gw_lag) then
          t_air_max_av = (sum(tmp(ig)%ts(1:jday,yrs_to_start))+sum(tmp(ig)%ts(jday+365-gw_lag+1:365,yrs_to_start-1)))/gw_lag
          t_air_min_av = (sum(tmp(ig)%ts2(1:jday,yrs_to_start))+sum(tmp(ig)%ts2(jday+365-gw_lag+1:365,yrs_to_start-1)))/gw_lag
      else
          t_air_max_av = sum(tmp(ig)%ts(jday-gw_lag+1:jday,yrs_to_start))/gw_lag
          t_air_min_av = sum(tmp(ig)%ts2(jday-gw_lag+1:jday,yrs_to_start))/gw_lag
      end if
      t_gw = max(0.01,(t_air_max_av + t_air_min_av) / 2)
      
    ! sno lag
      if (time%yrs == 1) then !only if simulation year = 1
          sno_lag = min (jday, sno_lag)
      else 
          sno_lag = sno_lag
      end if 
      
      yrs_to_start = time%yrs - tmp(ig)%yrs_start   !model year - tmp start year is model year
      
      if (jday <= sno_lag) then
          t_air_max_av = (sum(tmp(ig)%ts(1:jday,yrs_to_start))+sum(tmp(ig)%ts(jday+365-sno_lag+1:365,yrs_to_start-1)))/sno_lag
          t_air_min_av = (sum(tmp(ig)%ts2(1:jday,yrs_to_start))+sum(tmp(ig)%ts2(jday+365-sno_lag+1:365,yrs_to_start-1)))/sno_lag
      else
          t_air_max_av = sum(tmp(ig)%ts(jday-sno_lag+1:jday,yrs_to_start))/sno_lag
          t_air_min_av = sum(tmp(ig)%ts2(jday-sno_lag+1:jday,yrs_to_start))/sno_lag
      end if
      t_sno = min(2.5,max(0.01,(t_air_max_av + t_air_min_av) / 2))
      
      !-------------------------end calculate lagged temperatures--------------------------------------

    ! calculate the components contributions
      sno_contr = sno_coef * (surf_lag_coef * t_sno) * q_lsu_sno
      gw_contr = gw_coef * (gw_lag_coef * t_gw) * q_gw
      lat_contr = sur_lat_coef  * (lat_lag_coef * t_lat) * q_lsu_lat
      surf_contr = sur_lat_coef * (surf_lag_coef * t_surf) * q_lsu_surf 

    !  mixing of components
      if(bsn_cc%gwflow == 1) then !groundwater contribution handled in gwflow subroutines
			  tw_local = (sno_contr + lat_contr + surf_contr) / q_lsu_wyld	 
			else
			  tw_local = (sno_contr + gw_contr + lat_contr + surf_contr) / q_lsu_wyld
			endif
             
      if (abs(tw_local - tw_local_prev) > 5) then           ! difference of tmp between two days cannot be larger than 5 degrees - plausibility check for high peaks presumably resulting from routing errors
          tw_local = 5.0 + 0.75 * w%tave
      end if
      
      ! ----------------- initial water temperature of upstream channel------------------
        
      tw_up = ht1%temp
      if (ht1%flo/86400 > 1e-6) then            !error if channel flow is 0
          ! initial stream temperature if there is flow
          tw_init = (tw_up * (ht1%flo/86400) + (tw_local * q_lsu_wyld)) / ((ht1%flo/86400)+q_lsu_wyld)
      else
          ! if no flow from upstream, tw_init comes only from mixing in step 1
          tw_init = tw_local
      end if    
           
      ! =========================================================
      !  Step 4. Heat exchange and equilibrium temperature (by Efrain Noa-Yarasca)
      ! =========================================================
      
      tdx_cal = w%dewpt * w_temp(0)%hex_coef1 !calibrated dew point temperature
      tdx = amin1(tdx_cal, t_air)   !get tdx out of SWAT                
      t_md = (tw_init + tdx) / 2.
      ke_beta = 0.35 + 0.015 * t_md + 0.0012 * (t_md**2)
      f_wind = 9.2 + 0.46 * (w%windsp**2)              ! These coefficients might vary. See Figure 2.4.1 in Edinger et al. 1974
      k_e = 4.48 + 0.05 * tw_init + (ke_beta + 0.47) * f_wind

      ! Solar radiation (Short wave radiation). The shade-factor is included in the Solar radiation calculation
      ! read shadefactor input file (has to be computed manually with python), if switched on (1) in temperature.cha, else the default is 0.5

      if (w_temp(0)%sf_on == 1) then
          do i = 1, size(shf_db)
              if (jday == 366) then !for leap year set SF to 0.5
                  ssff = 0.5
              else
                  if (shf_db(i)%jday == jday .and. shf_db(i)%lsu == ilsu) then
                      ssff = shf_db(i)%value
                      exit   
                  end if
              end if
          end do
      else
          ssff= w_temp(0)%ssff
      end if
                 
                  
      h_sr = 0.97 * (w%solrad * 11.57) * (1. - ssff)                        ! SR same hardcoded - Get the Solar radiation (Short wave radiation in W/m2)
    
      ! Atmospheric radiation (Long wave radiation)
      e_s = 6.1275 * Exp(17.62 * w%tave / (237.3 + w%tave))                 ! Get the Saturated vapor pressure 
      e_a = e_s * w%rhum                                                    ! Get the vapor pressure
              
      ! cloud cover factor equation 2.2.19 SWAT manual
      if (w%solradmx < 1.e-4) then
          cloud = 0.
      else
          cloud = 0.9 * (w%solrad / w%solradmx) + 0.1
      end if  

      e_atm = 0.74 + 0.0065 * e_a * (1. + 0.17 * cloud**2)                  ! Get the emissivity of the atmosphere 
      h_atm = 0.96 * e_atm * 5.67e-8 * (w%tave + 273.15)**4                 ! Atmospheric radiation (Long wave radiation)

      ! Equilibrium temperature 
      numerator = h_atm - 305.5 - 4.48 * tdx
      t_equil = tdx + h_sr / k_e + numerator / k_e                          !E: Reduced T-equilibrium approach

      ! The temperature gain/loss to/from the stream due to heat transfer
      
      vc = max(0.01,sd_ch_vel(ich)%vel)     ! channel velocity from sd_channel_sediment3
      
      ! recalculate triangular channel geometry for calibration
      wid_chan = sd_chd(ich)%chw
      
      dep_chan = sd_chd(ich)%chd
      
      ! Calculate flow_wid with calibration factor
      wid_flow = sqrt((( ( ht2%flo/86400) / vc) * 2 ) / ((dep_chan/w_temp(0)%hex_coef2)/wid_chan)) 

      !prevent 0 division error
      if(wid_chan < 0.1) then
          wid_chan = sd_chd(ich)%chw * 0.25
      end if
      
      ! Calculate flow_dep with calibration factor
      dep_flow = max(0.1,(((dep_chan/w_temp(0)%hex_coef2)/wid_chan)) * wid_flow )
      
      if (dep_flow > sd_chd(ich)%chd*2) then
          dep_flow = sd_chd(ich)%chd*2
      end if
      
      rttime = sd_ch_vel(ich)%rttime / 24   !routing time in days
              
      k_factor = 1000. * 4186. * dep_flow / 86400       ! E: Density*Spec_heat_water*water_depth / time_convers 
      t_heat_exch = k_e * (t_equil - tw_init) / k_factor * rttime
        
      ! =========================================================
      !  Step 6. Final output
      ! =========================================================
      
      ! final new stream temperature      
      tw_final = tw_init + t_heat_exch
      
      !prevent negative temperatures
      if (tw_final < 0.001) then
          tw_final  = 0.001
      end if

      ! Final stream temperature saving to variables
      ht2%temp = tw_final                          ! set stream temperature of flo out
      ch_stor(ich)%temp = tw_final
      ch_out_d(ich)%temp = tw_final                ! for writing the output in channel_sd_day
      wtemp = 5.0 + 0.75 * wst(iwst)%weat%tave     ! this writes the last column in channel_sd_day
      
      !output for variable analysis      
      hyd_sep_array(ich,1) = q_lsu_surf
      hyd_sep_array(ich,2) = q_lsu_lat
      hyd_sep_array(ich,3) = q_gw
      hyd_sep_array(ich,4) = q_lsu_wyld
      hyd_sep_array(ich,5) = q_lsu_sno
      hyd_sep_array(ich,6) = tw_final 
      hyd_sep_array(ich,7) = tw_init
    
      return    
	end subroutine ch_temp
	