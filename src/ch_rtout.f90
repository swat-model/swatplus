       subroutine ch_rtout
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine summarizes data for reaches

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units      |definition  
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ammonian(:)   |mg N/L     |ammonia concentration in reach
!!    bury          |mg pst     |loss of pesticide from active sediment layer
!!                              |by burial
!!    chlora(:)     |mg chl-a/L |chlorophyll-a concentration in reach
!!    difus         |mg pst     |diffusion of pesticide from sediment to reach
!!    disolvp(:)    |mg P/L     |dissolved phosphorus concentration in reach
!!    hbactlp(:)    |# cfu/100mL|less persistent bacteria in reach/outflow
!!                              |during hour
!!    hbactp(:)     |# cfu/100mL|persistent bacteria in reach/outflow during
!!                              |hour
!!    hbod(:)       |mg O2/L    |carbonaceous biochemical oxygen demand in
!!                              |reach at end of hour
!!    hchla(:)      |mg chl-a/L |chlorophyll-a concentration in reach at end of
!!                              |hour
!!    hdisox(:)     |mg O2/L    |dissolved oxygen concentration in reach at
!!                              |end of hour
!!    hnh4(:)       |mg N/L     |ammonia concentration in reach at end of hour
!!    hno2(:)       |mg N/L     |nitrite concentration in reach at end of hour
!!    hno3(:)       |mg N/L     |nitrate concentration in reach at end of hour
!!    horgn(:)      |mg N/L     |organic nitrogen concentration in reach at
!!                              |end of hour
!!    horgp(:)      |mg P/L     |organic phosphorus concentration in reach at
!!                              |end of hour
!!    hsedyld(:)    |metric tons|sediment transported out of reach during hour
!!    hsolp(:)      |mg P/L     |dissolved phosphorus concentration in reach at
!!                              |end of hour
!!    hsolpst(:)    |mg pst/m^3 |soluble pesticide concentration in outflow
!!                              |on day
!!    hsorpst(:)    |mg pst/m^3 |sorbed pesticide concentration in outflow
!!                              |on day
!!    hrtwtr(:)     |m^3 H2O    |water leaving reach during hour
!!    ihout         |none       |outflow hydrograph location
!!    nitraten(:)   |mg N/L     |nitrate concentration in reach
!!    nitriten(:)   |mg N/L     |nitrite concentration in reach
!!    organicn(:)   |mg N/L     |organic nitrogen concentration in reach
!!    organicp(:)   |mg P/L     |organic phosphorus concentration in reach
!!    rch_bactlp(:) |# cfu/100ml|less persistent bacteria in reach/outflow
!!                              |at end of day
!!    rch_bactp(:)  |# cfu/100ml|persistent bacteria in reach/outflow at end
!!                              |of day
!!    rch_cbod(:)   |mg O2/L    |carbonaceous biochemical oxygen demand in
!!                              |reach
!!    rch_dox(:)    |mg O2/L    |dissolved oxygen concentration in reach
!!    reactb        |mg pst     |amount of pesticide in sediment that is lost
!!                              |through reactions
!!    reactw        |mg pst     |amount of pesticide in reach that is lost
!!                              |through reactions
!!    resuspst      |mg pst     |amount of pesticide moving from sediment to
!!                              |reach due to resuspension
!!    rtevp         |m^3 H2O    |evaporation from reach on day
!!    rttlc         |m^3 H2O    |transmission losses from reach on day
!!    rtwtr         |m^3 H2O    |water leaving reach on day
!!    sedrch        |metric tons|sediment transported out of channel
!!                              |during time step
!!    setlpst       |mg pst     |amount of pesticide moving from water to
!!                              |sediment due to settling
!!    volatpst      |mg pst     |amount of pesticide in reach lost by
!!                              |volatilization
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name             |units      |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bedvol      |m^3           |volume of river bed sediment
!!    ii          |none          |counter
!!    jrch        |none          |reach number
!!    sedcon      |mg/L          |sediment concentration in outflow
!!    sedpest     |mg pst        |pesticide in river bed sediment
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module
      use channel_data_module
      use time_module
      use channel_module
      use hydrograph_module, only : ob, iwst, icmd, jrch
      use climate_module
      
      implicit none
           
      real :: sedcon       !mg/L          |sediment concentration in outflow
      real :: bedvol       !m^3           |volume of river bed sediment
      real :: sedpest      !mg pst        |pesticide in river bed sediment
      real :: wtmp         !deg C         |temperature of water in reach 
      integer :: ii        !none          |counter

      wtmp = 5.0 + 0.75 * wst(iwst)%weat%tave
!! set values for routing variables
      ob(icmd)%hd(1)%temp = wtmp
      ob(icmd)%hd(1)%flo = rtwtr_d
      ob(icmd)%hd(1)%sed = sedrch
      
!!    sediment routing
      ob(icmd)%hd(1)%san = rch_san
      ob(icmd)%hd(1)%sil = rch_sil
      ob(icmd)%hd(1)%cla = rch_cla
      ob(icmd)%hd(1)%sag = rch_sag
      ob(icmd)%hd(1)%lag = rch_lag
      ob(icmd)%hd(1)%grv = rch_gra
      if (time%step == 0) then
       ob(icmd)%hd(1)%orgn = ch(jrch)%organicn * rtwtr / 1000. + ch(jrch)%orgn
       ob(icmd)%hd(1)%sedp = ch(jrch)%organicp * rtwtr / 1000. + ch(jrch)%orgp
       ob(icmd)%hd(1)%no3 = ch(jrch)%nitraten * rtwtr / 1000.
       ob(icmd)%hd(1)%solp = ch(jrch)%disolvp * rtwtr / 1000.
       ob(icmd)%hd(1)%chla = ch(jrch)%chlora * rtwtr / 1000.
       ob(icmd)%hd(1)%nh3 = ch(jrch)%ammonian * rtwtr / 1000.
       ob(icmd)%hd(1)%no2 = ch(jrch)%nitriten * rtwtr / 1000.
       ob(icmd)%hd(1)%cbod = ch(jrch)%rch_cbod *  rtwtr/ 1000.
       ob(icmd)%hd(1)%dox = ch(jrch)%rch_dox *  rtwtr/ 1000.
      else
       do ii = 1, time%step 
          ob(icmd)%ts(1,ii)%flo = hrtwtr(ii)   ! urban modeling by J.Jeong
          ob(icmd)%ts(1,ii)%sed = hsedyld(ii)  ! urban modeling by J.Jeong

	! From this point, check each variables if it is simulated at subdaily interval before using the output - Jaehak 9/11/09
          ob(icmd)%ts(1,ii)%temp = 0.
          ob(icmd)%ts(1,ii)%orgn = horgn(ii) * hrtwtr(ii) / 1000.
          ob(icmd)%ts(1,ii)%sedp = horgp(ii) *  hrtwtr(ii) / 1000.
          ob(icmd)%ts(1,ii)%no3 = hno3(ii) * hrtwtr(ii) / 1000.
          ob(icmd)%ts(1,ii)%solp = hsolp(ii) * hrtwtr(ii) / 1000.
          ob(icmd)%ts(1,ii)%chla = hchla(ii) * hrtwtr(ii) / 1000.
          ob(icmd)%ts(1,ii)%nh3 = hnh4(ii) * hrtwtr(ii) / 1000.
          ob(icmd)%ts(1,ii)%no2 = hno2(ii) * hrtwtr(ii) / 1000.
          ob(icmd)%ts(1,ii)%cbod = hbod(ii) *  hrtwtr(ii)/ 1000.
          ob(icmd)%ts(1,ii)%dox = hdisox(ii) *  hrtwtr(ii)/ 1000.

          ob(icmd)%hd(1)%orgn = ob(icmd)%hd(1)%orgn +                   &                 
                                             ob(icmd)%ts(1,ii)%orgn
          ob(icmd)%hd(1)%sedp = ob(icmd)%hd(1)%sedp +                   &
                                             ob(icmd)%ts(1,ii)%sedp
          ob(icmd)%hd(1)%no3 = ob(icmd)%hd(1)%no3 +                     &
                                             ob(icmd)%ts(1,ii)%no3
          ob(icmd)%hd(1)%solp = ob(icmd)%hd(1)%solp +                   &
                                             ob(icmd)%ts(1,ii)%solp
          ob(icmd)%hd(1)%chla = ob(icmd)%hd(1)%chla +                   &
                                             ob(icmd)%ts(1,ii)%chla
          ob(icmd)%hd(1)%nh3 = ob(icmd)%hd(1)%nh3 +                     &                   
                                             ob(icmd)%ts(1,ii)%nh3
          ob(icmd)%hd(1)%no2 = ob(icmd)%hd(1)%no2 +                     &
                                             ob(icmd)%ts(1,ii)%no2
          ob(icmd)%hd(1)%cbod = ob(icmd)%hd(1)%cbod +                   &
                                             ob(icmd)%ts(1,ii)%cbod
          ob(icmd)%hd(1)%dox = ob(icmd)%hd(1)%dox +                     &
                                             ob(icmd)%ts(1,ii)%dox
        end do
      end if


!! set subdaily reach output    - by jaehak jeong for urban project, subdaily output in output.rch file
!	if (time%step > 0) then
!	  do ii=1,time%step
!! determine sediment concentration in outflow
!          sedcon = 0.
!          if (hrtwtr(ii) > 0.01) then
!            sedcon = hsedyld(ii) / hrtwtr(ii) * 1.e6
!          else
!            sedcon = 0.
!          end if
!          rchhr(1,jrch,ii) = ob(icmd)%ts(1,ii)%flo !!flow in (m^3/s)
!     &      / (time%dtm * 60.)		       
!          rchhr(2,jrch,ii) = hrtwtr(ii) / (time%dtm * 60.)            !!flow out (m^3/s)
!          rchhr(3,jrch,ii) = hrtevp(ii) / (time%dtm * 60.)            !!evap (m^3/s)
!          rchhr(4,jrch,ii) = hrttlc(ii) / (time%dtm * 60.)            !!tloss (m^3/s)
!          rchhr(5,jrch,ii) =ob(icmd)%ts(1,ii)%sed    !!sed in (tons)
!          rchhr(6,jrch,ii) = hsedyld(ii)                         !!sed out (tons)
!          rchhr(7,jrch,ii) = sedcon						       !!sed conc (mg/L)
!	  end do
!	endif

!! determine sediment concentration in outflow
      sedcon = 0.
      if (rtwtr > 0.01) then
        sedcon = sedrch / rtwtr * 1.e6
      else
        sedcon = 0.
      end if
      if (sedcon > 200000.) sedcon = 200000.

      return
      end subroutine ch_rtout