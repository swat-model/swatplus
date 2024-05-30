      subroutine sd_channel_control2

      use sd_channel_module
      use hydrograph_module
      use constituent_mass_module
      use conditional_module
      use channel_data_module
      use channel_module
      use ch_pesticide_module
      use climate_module
    
      implicit none     
    
      integer :: isd_db               !              |
      integer :: ipest                !              |
      integer :: id
      integer :: iaq
      integer :: iaq_ch
      integer :: ii
      
      ich = isdch
      isd_db = sd_dat(ich)%hyd
      iwst = ob(icmd)%wst
      
      !! set ht1 to incoming daily hydrograph
      ht1 = ob(icmd)%hin
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
          aq_ch(iaq)%ch(iaq_ch)%flo_fr = 0.
        end if
      end if
      
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

      !call variable storage coefficient flood routing method
      call ch_rtmusk
              
      call sd_channel_sediment (time%step)
        
      !! use modified qual-2e routines
      ht3 = ht1
      
      !! don't route constituents if flow is zero
      if (ht1%flo > 1.e-6) then
          
      !! convert mass to concentration
      call hyd_convert_mass_to_conc (ht3)
      jnut = sd_dat(ich)%nut
      ben_area = sd_ch(ich)%chw * sd_ch(ich)%chl
      
      !! compute max flow depth and corresponding travel time during day
      rchdep = 0.
      rttime = 0.
      do ii = 1, time%step
        if (flo_dep(ii) > rchdep) then
          rchdep = flo_dep(ii)
          rttime = trav_time(ii)
        end if
      end do
          
      call ch_watqual4
      !! convert concentration to mass
      call hyd_convert_conc_to_mass (ht2)
      
      !! route constituents
      call ch_rtpest
      !! call mike winchell's new routine for pesticide routing
      !call ch_rtpest2
      if (cs_db%num_pests > 0) then
        obcs(icmd)%hd(1)%pest = hcs2%pest
      end if
      
      !! route pathogens
      call ch_rtpath
        
      end if    ! ht1%flo > 0
      
      !! check decision table for flow control - water allocation
      if (ob(icmd)%ruleset /= "null" .and. ob(icmd)%ruleset /= "0") then
        id = ob(icmd)%flo_dtbl
        d_tbl => dtbl_flo(id)
        call conditions (ich, id)
        call actions (ich, icmd, id)
      end if
 
      !! output channel organic-mineral
      ch_out_d(ich) = ob(icmd)%hd(1)                       !set outflow om hydrograph
      ch_out_d(ich)%flo = ob(icmd)%hd(1)%flo / 86400.      !m3 -> m3/s
   
      !! output flow to channel morphology
      chsd_d(ich)%flo = ob(icmd)%hd(1)%flo / 86400.        !adjust if overbank flooding is moved to landscape
      chsd_d(ich)%flo_mm = ob(icmd)%hd(1)%flo / (10. * ob(icmd)%area_ha)   !flow out in mm
      chsd_d(ich)%peakr = peakrate
      
      !! set pesticide output variables
      do ipest = 1, cs_db%num_pests
        chpst_d(ich)%pest(ipest)%tot_in = obcs(icmd)%hin(1)%pest(ipest)
        chpst_d(ich)%pest(ipest)%sol_out = frsol * obcs(icmd)%hd(1)%pest(ipest)
        chpst_d(ich)%pest(ipest)%sor_out = frsrb * obcs(icmd)%hd(1)%pest(ipest)
        chpst_d(ich)%pest(ipest)%react = chpst%pest(ipest)%react
        chpst_d(ich)%pest(ipest)%volat = chpst%pest(ipest)%volat
        chpst_d(ich)%pest(ipest)%settle = chpst%pest(ipest)%settle
        chpst_d(ich)%pest(ipest)%resus = chpst%pest(ipest)%resus
        chpst_d(ich)%pest(ipest)%difus = chpst%pest(ipest)%difus
        chpst_d(ich)%pest(ipest)%react_bot = chpst%pest(ipest)%react_bot
        chpst_d(ich)%pest(ipest)%bury = chpst%pest(ipest)%bury 
        chpst_d(ich)%pest(ipest)%water = ch_water(ich)%pest(ipest)
        chpst_d(ich)%pest(ipest)%benthic = ch_benthic(ich)%pest(ipest)
      end do

      return
      
      end subroutine sd_channel_control2