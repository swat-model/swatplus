      subroutine sd_channel_control3

      use sd_channel_module
      use channel_velocity_module
      use basin_module
      use hydrograph_module
      use constituent_mass_module
      use conditional_module
      use channel_data_module
      use channel_module
      use ch_pesticide_module
      use climate_module
      use water_body_module
      use time_module
      use water_allocation_module, only: wallo
      use ch_salt_module !rtb salt
      use ch_cs_module !rtb cs
      use gwflow_module, only: flood_freq !rtb gwflow
      use ch_pesticide_module               !!!  nbs added 7-20-23
      use channel_velocity_module
      
      implicit none     
    
      !real :: rcharea                !m^2           |cross-sectional area of flow
      real :: flo_rt                  !m^3/s         |flow rate in reach for day
      integer :: isd_db               !              |
      integer :: iob                  !              |
      integer :: idb                  !none          |channel data pointer
      integer :: ihyd                 !              |
      integer :: ipest                !              |
      integer :: isalt                !              |salt ion counter (rtb salt)
      integer :: ihru                 !              |
      integer :: iru                  !              |
      integer :: ise                  !              |
      integer :: ielem                !              |
      integer :: id
      integer :: iter
      real :: ebtm_m                  !m             |erosion of bottom of channel
      real :: ebank_m                 !m             |meander cut on one side
      real :: erode_bank_cut          !cm            |widening caused by downcutting (both sides)
      real :: ebtm_t                  !tons          |bottom erosion
      real :: ebank_t                 !tons          |bank erosion
      real :: sedout                  !mg		     |sediment out of waterway channel
      real :: washld                  !tons          |wash load  
      real :: bedld                   !tons          |bed load
      real :: dep                     !tons          |deposition
      real :: hc_sed                  !tons          |headcut erosion
      real :: chside                  !none          |change in horizontal distance per unit
                                      !              |change in vertical distance on channel side
                                      !              |slopes; always set to 2 (slope=1/2)
      real :: a                       !m^2           |cross-sectional area of channel
      real :: b                       !m             |bottom width of channel
      real :: c                       !none          |inverse of channel side slope
      real :: p                       !m             |wetting perimeter

      real :: rh                      !m             |hydraulic radius
      real :: qman                    !m^3/s or m/s  |flow rate or flow velocity
      real :: frac                    !0-1           |fraction of hydrograph 
      real :: valint                  !              | 
      integer :: ivalint              !              |
      real :: tbase                   !none          |flow duration (fraction of 24 hr)
      real :: tb_pr                   !              |
      real :: tb                      !              |
      real :: vol_ovb                 !              |
      real :: const                   !              |
      integer :: ics                  !none          |counter
      real :: ob_const                !              |
      integer :: ii                   !none          |counter
      real :: sum_vol                 !              |
      real :: xx                      !              | 
      integer :: ic                   !              |
      real :: vol_overmx              !              |
      real :: flood_dep               !              | 
      real :: dep_e                   !              |
      real :: rto                     !none          |cloud cover factor 
      real :: sumtime                 !              |
      real :: vc                      !m/s           |flow velocity in reach
      real :: pr_ratio                !              |
      real :: shear_btm_cr            !              |
      real :: shear_btm               !              |  
      real :: hc                      !m/yr          |head cut advance
      integer :: max                  !              |  
      integer :: iaq
      integer :: iaq_ch
      real :: det                     !hr            |time step
      real :: scoef                   !none          |Storage coefficient
      real :: flo_ls
      real :: vel, cohes, vel_cr, b_coef, qcms, veg
      real :: rad_curv, cla, pk_rto, vel_bend, vel_rch
      real :: arc_len, hyd_radius, prot_len
      real :: gw_salt_in              !kg            |salt loading to channel from aquifer
      real :: gw_cs_in                !kg            |constituent loading to channel from aquifer
      real :: seep_mass               !kg            |salt mass in seepage water
      real :: salt_conc(8)            !kg            |salt concentration in channel water
      real :: cs_conc(8)              !kg            |constituent concentration in channel water
      real :: bf_flow                 !m3/s          |bankfull flow rate * adjustment factor
      
      ich = isdch
      isd_db = sd_dat(ich)%hyd
      iwst = ob(icmd)%wst
      
      !rtb floodplain
      if(bsn_cc%gwflow.eq.1) flood_freq(ich) = 0
      
      !! set ht1 to incoming hydrograph
      ht1 = ob(icmd)%hin
      
      !! set outgoing flow and sediment - ht2
      ht2 = hz
      
      !! add water transfer
      if (ob(icmd)%trans%flo > 1.e-6) then
        ht1 = ht1 + ob(icmd)%trans
        ob(icmd)%trans = hz
      end if
      
      !set constituents to incoming loads (rtb salt; rtb cs)
      if (cs_db%num_tot > 0) then
        hcs1 = obcs(icmd)%hin(1)
      endif
      
      chsd_d(ich)%flo_in = ht1%flo / 86400.     !flow for morphology output
      ch_in_d(ich) = ht1                        !set inflow om hydrograph
      ch_in_d(ich)%flo = ht1%flo / 86400.       !flow for om output
      
      !rtb hydrograph separation
      hdsep1%flo_surq = ob(icmd)%hdsep_in%flo_surq
      hdsep1%flo_latq = ob(icmd)%hdsep_in%flo_latq
      hdsep1%flo_gwsw = ob(icmd)%hdsep_in%flo_gwsw
      hdsep1%flo_swgw = ob(icmd)%hdsep_in%flo_swgw
      hdsep1%flo_satex = ob(icmd)%hdsep_in%flo_satex
      hdsep1%flo_satexsw = ob(icmd)%hdsep_in%flo_satexsw
      hdsep1%flo_tile = ob(icmd)%hdsep_in%flo_tile
      !rtb hydrograph separation
      
      !! adjust precip and temperature for elevation using lapse rates
      w = wst(iwst)%weat
      if (bsn_cc%lapse == 1) call cli_lapse
      wst(iwst)%weat = w
      ht1%temp = 5.0 + 0.75 * wst(iwst)%weat%tave
      wtemp = 5.0 + 0.75 * wst(iwst)%weat%tave

      !! if connected to aquifer - add flow
      if (sd_ch(ich)%aqu_link > 0) then
        iaq = sd_ch(ich)%aqu_link
        iaq_ch = sd_ch(ich)%aqu_link_ch
        if (aq_ch(iaq)%ch(iaq_ch)%flo_fr > 0.) then
          chsd_d(ich)%aqu_in = (aq_ch(iaq)%ch(iaq_ch)%flo_fr * aq_ch(iaq)%hd%flo) / 86400.
          chsd_d(ich)%aqu_in_mm = (aq_ch(iaq)%ch(iaq_ch)%flo_fr * aq_ch(iaq)%hd%flo) / (10. * ob(icmd)%area_ha)
          ht1 = ht1 + aq_ch(iaq)%ch(iaq_ch)%flo_fr * aq_ch(iaq)%hd
          !rtb salt
          do isalt=1,cs_db%num_salts
            gw_salt_in = aq_ch(iaq)%ch(iaq_ch)%flo_fr * aq_chcs(iaq)%hd(1)%salt(isalt) !kg
            chsalt_d(ich)%salt(isalt)%gw_in = gw_salt_in !kg
            hcs1%salt(isalt) = hcs1%salt(isalt) + gw_salt_in !kg
          enddo
          !rtb cs
          do ics=1,cs_db%num_cs
            gw_cs_in = aq_ch(iaq)%ch(iaq_ch)%flo_fr * aq_chcs(iaq)%hd(1)%cs(ics) !kg
            chcs_d(ich)%cs(ics)%gw_in = gw_cs_in !kg
            hcs1%cs(ics) = hcs1%cs(ics) + gw_cs_in !kg
          enddo
          aq_ch(iaq)%ch(iaq_ch)%flo_fr = 0.
        end if
      end if
      
      !if gwflow is active, calulate aquifer interactions (ht1 is updated)
      if(bsn_cc%gwflow.eq.1) then
        call gwflow_gwsw(ich) !channel <--> groundwater
        call gwflow_canl(ich) !channel --> canal seepage
        call gwflow_tile(ich) !groundwater --> channel
        call gwflow_satx(ich) !groundwater --> channel
      endif
      
      !! set inflow hyds for printing
      chsd_d(ich)%flo_in = ht1%flo / 86400.     !flow for morphology output - m3/s
      chsd_d(ich)%flo_in_mm = ht1%flo / (10. * ob(icmd)%area_ha)   !flow in mm
      ch_in_d(ich) = ht1                        !set inflow om hydrograph
      ch_in_d(ich)%flo = ht1%flo / 86400.       !flow for om output - m3/s
      
      !set constituents (rtb salt) to incoming loads
      if (cs_db%num_tot > 0) then
        hcs1 = obcs(icmd)%hin(1)
      end if
      !! zero outgoing flow and sediment - ht2
      ht2 = hz

      !call Muskingum and variable storage coefficient flood routing method
      call ch_rtmusk
              
      call sd_channel_sediment3
        
        !! route constituents
        call ch_rtpest
        !! call mike winchell's new routine for pesticide routing
        ! call ch_rtpest2
        call ch_rtpath
      
      !salt and constituent concentrations (g/m3) for inflow water
      if(cs_db%num_salts > 0 .or. cs_db%num_cs > 0) then
        hcs2 = hcs1 !set outflow to inflow
        do isalt=1,cs_db%num_salts
          if(ht2%flo > 0) then
            salt_conc(isalt) = (hcs2%salt(isalt) * 1000.) / ht2%flo !g/m3 = mg/L 
          else
            salt_conc(isalt) = 0.
          endif
        enddo
        do ics=1,cs_db%num_cs
          if(ht2%flo > 0) then
            cs_conc(ics) = (hcs2%cs(ics) * 1000.) / ht2%flo !g/m3 = mg/L 
          else
            cs_conc(ics) = 0.
          endif
        enddo
      endif
      
      !salt mass in seepage
      do isalt=1,cs_db%num_salts
        seep_mass = salt_conc(isalt) * ch_wat_d(ich)%seep !g/m3 * m3 = g
        seep_mass = seep_mass / 1000. !kg
        if(seep_mass > hcs2%salt(isalt)) then
          seep_mass = hcs2%salt(isalt)
        endif
        hcs2%salt(isalt) = hcs2%salt(isalt) - seep_mass !kg
        chsalt_d(ich)%salt(isalt)%seep = seep_mass !kg (channel salt output)
      enddo
      
      !constituent mass in seepage
      do ics=1,cs_db%num_cs
        seep_mass = cs_conc(ics) * ch_wat_d(ich)%seep !g/m3 * m3 = g
        seep_mass = seep_mass / 1000. !kg
        if(seep_mass > hcs2%cs(ics)) then
          seep_mass = hcs2%cs(ics)
        endif
        hcs2%cs(ics) = hcs2%cs(ics) - seep_mass !kg
        chcs_d(ich)%cs(ics)%seep = seep_mass !kg (channel constituent output)
      enddo
      
      !! subtract evaporation
      if (ht2%flo < ch_wat_d(ich)%evap) then
        ch_wat_d(ich)%evap = ht2%flo
        ht2%flo = 0.
      else
        ht2%flo = ht2%flo - ch_wat_d(ich)%evap
      end if

      !! total outgoing to output to SWIFT
      ob(icmd)%hout_tot = ob(icmd)%hout_tot + ht2
        
      !compute stream temperature
      ! Call Subroutune for Ficklin Model, Linear Equation Model, Energy Balance Model
      !ht2%temp = "output from subroutine"
      
      !salt, constituent mass
      if(cs_db%num_salts > 0 .or. cs_db%num_cs > 0) then
        hcs3 = hcs2 + ch_water(ich) !incoming + storage
      endif
      
      !rtb hydrograph separation
      if (rttime > det) then      ! ht1 = incoming + storage
        !! travel time > timestep -- then all incoming is stored and frac of stored is routed
        hdsep2%flo_surq = scoef * ch_stor_hdsep(ich)%flo_surq
        hdsep2%flo_latq = scoef * ch_stor_hdsep(ich)%flo_latq
        hdsep2%flo_gwsw = scoef * ch_stor_hdsep(ich)%flo_gwsw
        hdsep2%flo_swgw = scoef * ch_stor_hdsep(ich)%flo_swgw
        hdsep2%flo_satex = scoef * ch_stor_hdsep(ich)%flo_satex
        hdsep2%flo_satexsw = scoef * ch_stor_hdsep(ich)%flo_satexsw
        hdsep2%flo_tile = scoef * ch_stor_hdsep(ich)%flo_tile
        ch_stor_hdsep(ich)%flo_surq = (frac*ch_stor_hdsep(ich)%flo_surq) + hdsep1%flo_surq
        ch_stor_hdsep(ich)%flo_latq = (frac*ch_stor_hdsep(ich)%flo_latq) + hdsep1%flo_latq
        ch_stor_hdsep(ich)%flo_gwsw = (frac*ch_stor_hdsep(ich)%flo_gwsw) + hdsep1%flo_gwsw
        ch_stor_hdsep(ich)%flo_swgw = (frac*ch_stor_hdsep(ich)%flo_swgw) + hdsep1%flo_swgw
        ch_stor_hdsep(ich)%flo_satex = (frac*ch_stor_hdsep(ich)%flo_satex) + hdsep1%flo_satex
        ch_stor_hdsep(ich)%flo_satexsw = (frac*ch_stor_hdsep(ich)%flo_satexsw) + hdsep1%flo_satexsw
        ch_stor_hdsep(ich)%flo_tile = (frac*ch_stor_hdsep(ich)%flo_tile) + hdsep1%flo_tile
      else
        !! travel time < timestep -- route all stored and frac of incoming
        hdsep2%flo_surq = scoef * hdsep1%flo_surq
        hdsep2%flo_latq = scoef * hdsep1%flo_latq
        hdsep2%flo_gwsw = scoef * hdsep1%flo_gwsw
        hdsep2%flo_swgw = scoef * hdsep1%flo_swgw
        hdsep2%flo_satex = scoef * hdsep1%flo_satex
        hdsep2%flo_satexsw = scoef * hdsep1%flo_satexsw
        hdsep2%flo_tile = scoef * hdsep1%flo_tile
        hdsep2%flo_surq = hdsep2%flo_surq + ch_stor_hdsep(ich)%flo_surq
        hdsep2%flo_latq = hdsep2%flo_latq + ch_stor_hdsep(ich)%flo_latq
        hdsep2%flo_gwsw = hdsep2%flo_gwsw + ch_stor_hdsep(ich)%flo_gwsw
        hdsep2%flo_swgw = hdsep2%flo_swgw + ch_stor_hdsep(ich)%flo_swgw
        hdsep2%flo_satex = hdsep2%flo_satex + ch_stor_hdsep(ich)%flo_satex
        hdsep2%flo_satexsw = hdsep2%flo_satexsw + ch_stor_hdsep(ich)%flo_satexsw
        hdsep2%flo_tile = hdsep2%flo_tile + ch_stor_hdsep(ich)%flo_tile
        ch_stor_hdsep(ich)%flo_surq = frac * hdsep1%flo_surq
        ch_stor_hdsep(ich)%flo_latq = frac * hdsep1%flo_latq
        ch_stor_hdsep(ich)%flo_gwsw = frac * hdsep1%flo_gwsw
        ch_stor_hdsep(ich)%flo_swgw = frac * hdsep1%flo_swgw
        ch_stor_hdsep(ich)%flo_satex = frac * hdsep1%flo_satex
        ch_stor_hdsep(ich)%flo_satexsw = frac * hdsep1%flo_satexsw
        ch_stor_hdsep(ich)%flo_tile = frac * hdsep1%flo_tile
      end if
      ob(icmd)%hdsep%flo_surq = hdsep2%flo_surq
      ob(icmd)%hdsep%flo_latq = hdsep2%flo_latq
      ob(icmd)%hdsep%flo_gwsw = hdsep2%flo_gwsw
      ob(icmd)%hdsep%flo_swgw = hdsep2%flo_swgw
      ob(icmd)%hdsep%flo_satex = hdsep2%flo_satex
      ob(icmd)%hdsep%flo_satexsw = hdsep2%flo_satexsw
      ob(icmd)%hdsep%flo_tile = hdsep2%flo_tile
      !store outflow components for writing (and convert from m3 --> m3/sec)
      hyd_sep_array(ich,1) = hdsep2%flo_surq / 86400.
      hyd_sep_array(ich,2) = hdsep2%flo_latq / 86400.
      hyd_sep_array(ich,3) = hdsep2%flo_gwsw / 86400.
      hyd_sep_array(ich,4) = hdsep2%flo_swgw / 86400.
      hyd_sep_array(ich,5) = hdsep2%flo_satex / 86400.
      hyd_sep_array(ich,6) = hdsep2%flo_satexsw / 86400.
      hyd_sep_array(ich,7) = 0. !hdsep2%flo_tile / 86400.
      !rtb hydrograph separation
      !end if

      ich = isdch
            
      !! check decision table for flow control - water diversion
      if (ob(icmd)%ruleset /= "null" .and. ob(icmd)%ruleset /= "0") then
        id = ob(icmd)%flo_dtbl
        d_tbl => dtbl_flo(id)
        call conditions (ich, id)
        call actions (ich, icmd, id)
      end if
 
      !! check decision table for water allocation
      if (sd_ch(isdch)%wallo > 0) then
        call wallo_control (sd_ch(isdch)%wallo)
      end if
      
      !! set outflow hyd to ht2 after diverting water
      ob(icmd)%hd(1) = ht2

      !channel salt updates
      if(cs_db%num_salts > 0) then
        do isalt=1,cs_db%num_salts
          hcs2%salt(isalt) = scoef * hcs3%salt(isalt)
          ch_water(ich)%salt(isalt) = hcs3%salt(isalt) - hcs2%salt(isalt)
        enddo
      endif
      
      !channel constituent updates
      if(cs_db%num_cs > 0) then
        do ics=1,cs_db%num_cs
          hcs2%cs(ics) = scoef * hcs3%cs(ics)
          ch_water(ich)%cs(ics) = hcs3%cs(ics) - hcs2%cs(ics)
        enddo
      endif
      
      !! calculate stream temperature
      ob(icmd)%hd(1)%temp = 5. + .75 * wst(iwst)%weat%tave
      ht2%temp = 5. + .75 * wst(iwst)%weat%tave
      ch_stor(isdch)%temp = 5. + .75 * wst(iwst)%weat%tave
      
      !! set constituents for routing
      if (cs_db%num_pests > 0) then
        obcs(icmd)%hd(1)%pest = hcs2%pest
      end if
      if (cs_db%num_salts > 0) then !rtb salt
        obcs(icmd)%hd(1)%salt = hcs2%salt
      end if
      if (cs_db%num_cs > 0) then !rtb cs
        obcs(icmd)%hd(1)%cs = hcs2%cs
      end if
      
      !! output channel organic-mineral
      ch_out_d(isdch) = ob(icmd)%hd(1)                       !set outflow om hydrograph
      ch_out_d(isdch)%flo = ob(icmd)%hd(1)%flo / 86400.      !m3 -> m3/s
      
      !! output channel morphology
      chsd_d(isdch)%flo = ob(icmd)%hd(1)%flo / 86400.        !adjust if overbank flooding is moved to landscape
      chsd_d(isdch)%flo_mm = ob(icmd)%hd(1)%flo / (10. * ob(icmd)%area_ha)   !flow out in mm
      chsd_d(isdch)%peakr = peakrate 
      chsd_d(isdch)%sed_in = ob(icmd)%hin%sed
      chsd_d(isdch)%sed_out = ob(icmd)%hd(1)%sed
      chsd_d(isdch)%sed_stor = ch_stor(isdch)%sed
      ch_sed_bud(isdch)%fp_dep = fp_dep%sed
      ch_sed_bud(isdch)%ch_dep = ch_dep%sed
      ch_sed_bud(isdch)%bank_ero = bank_ero%sed
      ch_sed_bud(isdch)%bed_ero = bed_ero%sed
      chsd_d(isdch)%washld = ob(icmd)%hd(1)%sed
      chsd_d(isdch)%bedld = ch_dep%sed
      chsd_d(isdch)%dep = fp_dep%sed
      chsd_d(isdch)%deg_btm = bed_ero%sed
      chsd_d(isdch)%deg_bank = bank_ero%sed
      chsd_d(isdch)%hc_sed = hc_sed
      chsd_d(isdch)%width = sd_ch(isdch)%chw
      chsd_d(isdch)%depth = sd_ch(isdch)%chd
      chsd_d(isdch)%slope = sd_ch(isdch)%chs
      chsd_d(isdch)%deg_btm_m = ebtm_m
      chsd_d(isdch)%deg_bank_m = ebank_m
      chsd_d(isdch)%n_tot = ob(icmd)%hd(1)%orgn + ob(icmd)%hd(1)%no3 + ob(icmd)%hd(1)%nh3 + ob(icmd)%hd(1)%no2
      chsd_d(isdch)%p_tot = ob(icmd)%hd(1)%sedp + ob(icmd)%hd(1)%solp
      chsd_d(ich)%dep_bf = sd_ch_vel(ich)%dep_bf
      chsd_d(ich)%velav_bf = sd_ch_vel(ich)%vel_bf
      
      !! set pesticide output variables
      do ipest = 1, cs_db%num_pests
        chpst_d(isdch)%pest(ipest)%tot_in = obcs(icmd)%hin(1)%pest(ipest)
        chpst_d(isdch)%pest(ipest)%sol_out = frsol * obcs(icmd)%hd(1)%pest(ipest)
        chpst_d(isdch)%pest(ipest)%sor_out = frsrb * obcs(icmd)%hd(1)%pest(ipest)
        chpst_d(isdch)%pest(ipest)%react = chpst%pest(ipest)%react
        chpst_d(isdch)%pest(ipest)%volat = chpst%pest(ipest)%volat
        chpst_d(isdch)%pest(ipest)%settle = chpst%pest(ipest)%settle
        chpst_d(isdch)%pest(ipest)%resus = chpst%pest(ipest)%resus
        chpst_d(isdch)%pest(ipest)%difus = chpst%pest(ipest)%difus
        chpst_d(isdch)%pest(ipest)%react_bot = chpst%pest(ipest)%react_bot
        chpst_d(isdch)%pest(ipest)%bury = chpst%pest(ipest)%bury 
        chpst_d(isdch)%pest(ipest)%water = ch_water(ich)%pest(ipest)
        chpst_d(isdch)%pest(ipest)%benthic = ch_benthic(ich)%pest(ipest)
      end do
      
      !rtb salt - set salt output variables
      do isalt = 1, cs_db%num_salts
        chsalt_d(ich)%salt(isalt)%tot_in = hcs1%salt(isalt) + chsalt_d(ich)%salt(isalt)%gw_in !mass entering the channel (upstream + groundwater)
        chsalt_d(ich)%salt(isalt)%tot_out = hcs2%salt(isalt) !mass leaving the channel (for current day)
        chsalt_d(ich)%salt(isalt)%water = ch_water(ich)%salt(isalt) !mass stored in channel (for current day)
        !concentration of channel water (= concentration of outflow water)
        if(ht2%flo > 0) then 
          chsalt_d(ich)%salt(isalt)%conc = (hcs2%salt(isalt) * 1000.) / ht2%flo !g/m3 = mg/L 
        else
          chsalt_d(ich)%salt(isalt)%conc = 0.
        endif
      enddo
      
      !rtb cs - set constituent output variables
      do ics = 1, cs_db%num_cs
        chcs_d(ich)%cs(ics)%tot_in = hcs1%cs(ics) + chcs_d(ich)%cs(ics)%gw_in !mass entering the channel (upstream + groundwater)
        chcs_d(ich)%cs(ics)%tot_out = hcs2%cs(ics) !kg mass leaving the channel (for current day)
        chcs_d(ich)%cs(ics)%water = ch_water(ich)%cs(ics) !kg mass stored in channel (for current day)
        if(ht2%flo > 0) then !concentration of outflow water
          chcs_d(ich)%cs(ics)%conc = (hcs2%cs(ics) * 1000.) / ht2%flo !g/m3 = mg/L 
        else
          chcs_d(ich)%cs(ics)%conc = 0.
        endif
      enddo
      
        
      !! set values for recharge hydrograph - should be trans losses
      !ob(icmd)%hd(2)%flo = perc  

      return
      
      end subroutine sd_channel_control3