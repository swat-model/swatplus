      subroutine channel_control
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine simulates channel routing     

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition  
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bankst(:)   |m^3 H2O       |bank storage
!!    inum1       |none          |reach number
!!    inum2       |none          |inflow hydrograph storage location number
!!    pet_ch      |mm H2O        |potential evapotranspiration on day
!!    rchdep      |m             |depth of flow on day
!!    rttlc       |m^3 H2O       |transmission losses from reach on day
!!    rtwtr       |m^3 H2O       |water leaving reach on day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    revapday    |m^3 H2O       |amount of water moving from bank storage
!!                               |into the soil profile or being taken
!!                               |up by plant roots in the bank storage zone
!!    rtwtr       |m^3 H2O       |water leaving reach on day
!!    qdbank      |m^3 H2O       |streamflow contribution from bank storage
!!    sedrch      |metric tons   |sediment transported out of reach on day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |counter
!!    jrch        |none          |reach number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min
!!    SWAT: rchinit, rtover, rtday, rtmusk, rthourly, rtsed, rthsed, watqual
!!    SWAT: noqual, hhwatqual, hhnoqual, rtpest, rthpest
!!    SWAT: rchuse, reachout

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use climate_module
      use hydrograph_module
      use basin_module
      use channel_data_module
      use time_module
      use channel_module
      use constituent_mass_module
      
      implicit none

      integer :: ii       !units         |description
      real :: qdbank      !m^3 H2O       |streamflow contribution from bank storage
      real :: revapday    !m^3 H2O       |amount of water moving from bank storage
                          !              |into the soil profile or being taken
                          !              |up by plant roots in the bank storage zone
      real :: chlin            !mg chl-a/L    |chlorophyll-a concentration in inflow
      real :: algin            !mg alg/L      |algal biomass concentration in inflow
      real :: orgnin           !mg N/L        |organic N concentration in inflow
      real :: ammoin           !mg N/L        |ammonium N concentration in inflow
      real :: nitratin         !mg N/L        |nitrate concentration in inflow
      real :: nitritin         !mg N/L        |nitrite concentration in inflow
      real :: orgpin           !mg P/L        |organic P concentration in inflow 
      real :: dispin           !mg P/L        |soluble P concentration in inflow
      real :: cbodin           !mg/L          |carbonaceous biological oxygen demand
      real :: disoxin          !mg O2/L       |dissolved oxygen concentration in inflow
      real :: cinn             !mg N/L        |effective available nitrogen concentration
      real :: rttlc_d
      real :: rtevp_d
                         
      ch_d(jrch)%flo_out = 0.
      ch_d(jrch)%evap = 0.
      jhyd = ch_dat(jrch)%hyd
      jsed = ch_dat(jrch)%sed
      jnut = ch_dat(jrch)%nut

      iwst = ob(icmd)%wst
      pet_ch = wst(iwst)%weat%pet
      qdbank = 0.
      revapday = 0.
      
      !! initialize variables for route command loop
      call ch_rchinit

      !! zero flow out variables
      ob(icmd)%hd(1) = hz
      if (time%step > 0) then
        do ii = 1, time%step
          ob(icmd)%ts(1,ii) = hz
        end do
      end if  

      ch(jrch)%vel_chan = 0.
      ch(jrch)%dep_chan = 0.
	  sedrch = 0.
	  rch_san = 0.
	  rch_sil = 0.
	  rch_cla = 0.
	  rch_sag = 0.
	  rch_lag = 0.
	  rch_gra = 0.
      wtrin = 0.
      chlin = 0.
         algin = 0.
         orgnin = 0.
         ammoin = 0.
         nitritin = 0.
         nitratin = 0.
         orgpin = 0.
         dispin = 0.
         cbodin = 0.
         disoxin = 0.
         cinn = 0.
!! route water through reach
        rtwtr_d=0.
        rttlc_d=0.
        rtevp_d =0.

!! route water through reach
      if (time%step == 0) then
          rt_delt = 1.
        wtrin = ob(icmd)%hin%flo
!! initialize inflow concentrations
        
        if (bsn_cc%rte == 0) call ch_rtday
        if (bsn_cc%rte == 1) call ch_rtmusk

        ben_area = ch_hyd(jhyd)%l *ch_hyd(jhyd)%w 
        
!        if (cs_db%num_pests > 0) then

!          call ch_rtpest 
!        end if
        
        rtwtr_d = rtwtr
        rttlc_d = rttlc
        rtevp_d = rtevp
      else
        rt_delt = 1. / time%step  
        wtrin = ob(icmd)%hin%flo * rt_delt
        rtwtr = 0.
        do ii = 1, time%step
        ! wtrin = ob(icmd)%ts(1,ii)%flo  
        call ch_rtday
!        call ch_rthpest
               rtwtr_d=rtwtr_d+rtwtr
               rttlc_d=rttlc_d+ rttlc
               rtevp_d =rtevp_d+ rtevp

!       hsedyld(ii) = ob(icmd)%ts(1,ii)%sed 
!       sedrch = sedrch + hsedyld(ii)
        rch_san = 0.
!       rch_sil = rch_sil + hsedyld(ii)  !!All are assumed to be silt type particles
        rch_cla = 0.
        rch_sag = 0.
        rch_lag = 0.
        rch_gra = 0.
        
        end do    
      endif

!! average daily water depth for sandi doty 09/26/07
      ch(jrch)%dep_chan = rchdep


!! add transmission losses to bank storage/deep aquifer in subbasin
      if (rttlc > 0.) then
        ch(jrch)%bankst = ch(jrch)%bankst + rttlc
      end if
 
!! compute revap from bank storage
      revapday = 0.6 * pet_ch *ch_hyd(jhyd)%l *ch_hyd(jhyd)%w
      revapday = Min(revapday, ch(jrch)%bankst)
      ch(jrch)%bankst = ch(jrch)%bankst - revapday

!! compute contribution of water in bank storage to streamflow
      qdbank = ch(jrch)%bankst * (1. - ch_hyd(jhyd)%alpha_bnk)
      ch(jrch)%bankst = ch(jrch)%bankst - qdbank
      rtwtr_d = rtwtr_d + qdbank
      if (time%step > 0) then
        do ii = 1, time%step
          hrtwtr(ii) = hrtwtr(ii) + qdbank / real(time%step)
        end do
      end if

!!    Channel Deposition (Only new deposits during the current time step)
      if (ch(jrch)%depch >= ch(jrch)%depprch) then
	    ch_d(jrch)%ch_dep = ch(jrch)%depch - ch(jrch)%depprch
	  else
	    ch_d(jrch)%ch_dep = 0.
	  end if
!!    Floodplain Deposition (Only new deposits during the current time step)
      if (ch(jrch)%depfp >= ch(jrch)%depprfp) then
	    ch_d(jrch)%fp_dep = ch(jrch)%depfp - ch(jrch)%depprfp
	  else
	    ch_d(jrch)%fp_dep = 0.
	  end if
!!    Total suspended sediments (only silt and clay)
      if (ch_sed(jsed)%eqn == 0) then
        ch_d(jrch)%tot_ssed = sedrch
      else
        ch_d(jrch)%tot_ssed = rch_sil + rch_cla
      endif
      
      return

      end subroutine channel_control