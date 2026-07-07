      subroutine ch_hhwatqual

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs in-stream nutrient transformations and water
!!    quality calculations for hourly timestep

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name             |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ai0              |ug chla/mg alg|ratio of chlorophyll-a to algal biomass
!!    ai1              |mg N/mg alg   |fraction of algal biomass that is N
!!    ai2              |mg P/mg alg   |fraction of algal biomass that is P
!!    ai3              |mg O2/mg alg  |the rate of oxygen production per unit of
!!                                    |algal photosynthesis
!!    ai4              |mg O2/mg alg  |the rate of oxygen uptake per unit of
!!                                    |algae respiration
!!    ai5              |mg O2/mg N    |the rate of oxygen uptake per unit of NH3
!!                                    |nitrogen oxidation
!!    ai6              |mg O2/mg N    |the rate of oxygen uptake per unit of NO2
!!                                    |nitrogen oxidation
!!    algae(:)         |mg alg/L      |algal biomass concentration in reach
!!    ammonian(:)      |mg N/L        |ammonia concentration in reach
!!    bc1(:)           |1/hr          |rate constant for biological oxidation of
!!                                    |NH3 to NO2 in reach at 20 deg C
!!    bc2(:)           |1/hr          |rate constant for biological oxidation of
!!                                    |NO2 to NO3 in reach at 20 deg C
!!    bc3(:)           |1/hr          |rate constant for hydrolysis of organic N
!!                                    |to ammonia in reach at 20 deg C
!!    bc4(:)           |1/hr          |rate constant for the decay of organic P
!!                                    |to dissolved P in reach at 20 deg C
!!    chlora(:)        |mg chl-a/L    |chlorophyll-a concentration in reach
!!    disolvp(:)       |mg P/L        |dissolved P concentration in reach
!!    frad(:,:)        |none          |fraction of solar radiation occuring 
!!                                    |during hour in day in HRU
!!    hdepth(:)        |m             |depth of flow on day
!!    hhtime(:)        |hr            |flow travel time for hour
!!    hrchwtr(ii)      |m^3 H2O       |water stored in reach at beginning of day
!!    hrtwtr(:)        |m^3 H2O       |flow out of reach
!!    inum2            |none          |inflow hydrograph storage location number
!!    k_l              |MJ/(m2*hr)    |half saturation coefficient for light
!!    k_n              |mg N/L        |michaelis-menton half-saturation constant
!!                                    |for nitrogen
!!    k_p              |mg P/L        |michaelis-menton half saturation constant
!!                                    |for phosphorus
!!    lambda0          |1/m           |non-algal portion of the light extinction
!!                                    |coefficient
!!    lambda1          |1/(m*ug chla/L)|linear algal self-shading coefficient
!!    lambda2          |(1/m)(ug chla/L)**(-2/3)
!!                                    |nonlinear algal self-shading coefficient
!!    mumax            |1/hr          |maximum specific algal growth rate at 
!!                                    |20 deg C
!!    nitraten(:)      |mg N/L        |nitrate concentration in reach
!!    nitriten(:)      |mg N/L        |nitrite concentration in reach
!!    organicn(:)      |mg N/L        |organic nitrogen concentration in reach
!!    organicp(:)      |mg P/L        |organic phosphorus concentration in reach
!!    rch_cbod(:)      |mg O2/L       |carbonaceous biochemical oxygen demand in
!!                                    |reach 
!!    rch_dox(:)       |mg O2/L       |dissolved oxygen concentration in reach
!!    rhoq             |1/hr          |algal respiration rate at 20 deg C
!!    rttime           |hr            |reach travel time
!!    tfact            |none          |fraction of solar radiation that is
!!                                    |photosynthetically active
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    algae(:)    |mg alg/L      |algal biomass concentration in reach
!!    ammonian(:) |mg N/L        |ammonia concentration in reach
!!    chlora(:)   |mg chl-a/L    |chlorophyll-a concentration in reach
!!    disolvp(:)  |mg P/L        |dissolved phosphorus concentration in reach
!!    nitraten(:) |mg N/L        |nitrate concentration in reach
!!    nitriten(:) |mg N/L        |nitrite concentration in reach
!!    organicn(:) |mg N/L        |organic nitrogen concentration in reach
!!    organicp(:) |mg P/L        |organic phosphorus concentration in reach
!!    rch_cbod(:) |mg O2/L       |carbonaceous biochemical oxygen demand in
!!                               |reach
!!    rch_dox(:)  |mg O2/L       |dissolved oxygen concentration in reach
!!    soxy        |mg O2/L       |saturation concetration of dissolved oxygen
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    algcon      |mg alg/L      |initial algal biomass concentration in reach
!!    algi        |MJ/(m2*hr)    |photosynthetically active light intensity
!!                               |for hour
!!    algin       |mg alg/L      |algal biomass concentration in inflow
!!    ammoin      |mg N/L        |ammonium N concentration in inflow
!!    bc1mod      |1/day         |rate constant for biological oxidation of NH3
!!                               |to NO2 modified to reflect impact of low 
!!                               |oxygen concentration
!!    bc2mod      |1/day         |rate constant for biological oxidation of NO2
!!                               |to NO3 modified to reflect impact of low
!!                               |oxygen concentration
!!    cbodcon     |mg/L          |initial carbonaceous biological oxygen demand
!!                               |concentration in reach
!!    cbodin      |mg/L          |carbonaceous biological oxygen demand 
!!                               |concentration in inflow
!!    chlin       |mg chl-a/L    |chlorophyll-a concentration in inflow
!!    cinn        |mg N/L        |effective available nitrogen concentration
!!    cordo       |none          |nitrification rate correction factor
!!    disoxin     |mg O2/L       |dissolved oxygen concentration in inflow
!!    dispin      |mg P/L        |soluble P concentration in inflow
!!    f1          |none          |fraction of algal nitrogen uptake from
!!                               |ammonia pool
!!    fll         |none          |growth attenuation factor for light
!!    fnn         |none          |algal growth limitation factor for nitrogen
!!    fpp         |none          |algal growth limitation factor for phosphorus
!!    gra         |1/hr          |local algal growth rate at 20 deg C
!!    jrch        |none          |reach number
!!    lambda      |1/m           |light extinction coefficient
!!    nh3con      |mg N/L        |initial ammonia concentration in reach
!!    nitratin    |mg N/L        |nitrate concentration in inflow
!!    nitritin    |mg N/L        |nitrite concentration in inflow
!!    no2con      |mg N/L        |initial nitrite concentration in reach
!!    no3con      |mg N/L        |initial nitrate concentration in reach
!!    o2con       |mg O2/L       |initial dissolved oxygen concentration in 
!!                               |reach
!!    orgncon     |mg N/L        |initial organic N concentration in reach
!!    orgnin      |mg N/L        |organic N concentration in inflow
!!    orgpcon     |mg P/L        |initial organic P concentration in reach
!!    orgpin      |mg P/L        |organic P concentration in inflow
!!    solpcon     |mg P/L        |initial soluble P concentration in reach
!!    thbc1       |none          |temperature adjustment factor for local
!!                               |biological oxidation of NH3 to NO2
!!    thbc2       |none          |temperature adjustment factor for local
!!                               |biological oxidation of NO2 to NO3
!!    thbc3       |none          |temperature adjustment factor for local
!!                               |hydrolysis of organic N to ammonia N
!!    thbc4       |none          |temperature adjustment factor for local
!!                               |decay of organic P to dissolved P
!!    thgra       |none          |temperature adjustment factor for local algal
!!                               |growth rate
!!    thour       |none          |flow duration (fraction of hr)
!!    thrho       |none          |temperature adjustment factor for local algal
!!                               |respiration rate
!!    thrk1       |none          |temperature adjustment factor for local CBOD
!!                               |deoxygenation
!!    thrk2       |none          |temperature adjustment factor for local oxygen
!!                               |reaeration rate
!!    thrk3       |none          |temperature adjustment factor for loss of
!!                               |CBOD due to settling
!!    thrk4       |none          |temperature adjustment factor for local
!!                               |sediment oxygen demand
!!    thrs1       |none          |temperature adjustment factor for local algal
!!                               |settling rate
!!    thrs2       |none          |temperature adjustment factor for local
!!                               |benthos source rate for dissolved phosphorus
!!    thrs3       |none          |temperature adjustment factor for local
!!                               |benthos source rate for ammonia nitrogen
!!    thrs4       |none          |temperature adjustment factor for local
!!                               |organic N settling rate
!!    thrs5       |none          |temperature adjustment factor for local
!!                               |organic P settling rate
!!    wtmp        |deg C         |temperature of water in reach
!!    wtrin       |m^3 H2O       |water flowing into reach on day
!!    uu          |varies        |variable to hold intermediate calculation
!!                               |result
!!    vv          |varies        |variable to hold intermediate calculation
!!                               |result
!!    wtrtot      |m^3 H2O       |inflow + storage water
!!    ww          |varies        |variable to hold intermediate calculation
!!                               |result
!!    xx          |varies        |variable to hold intermediate calculation
!!                               |result
!!    yy          |varies        |variable to hold intermediate calculation
!!                               |result
!!    zz          |varies        |variable to hold intermediate calculation
!!                               |result
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Log, Exp, Min
!!    SWAT: Theta

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ 
      
      use channel_data_module
      use time_module
      use channel_module
      use hydrograph_module, only : ob
      use climate_module
      
      implicit none

      integer :: ii             !none          |counter
      integer :: icmd           !units         |description 
      integer :: jrch           !units         |description 
      integer :: iwst           !none          |counter
      integer :: iwgn           !units         |description
      real :: orgnpin           !units         |description
      real :: theta             !units         |description
      real :: soxy              !mg O2/L       |saturation concetration of dissolved oxygen
      real :: chlin             !mg chl-a/L    |chlorophyll-a concentration in inflow
      real :: algin             !mg alg/L      |algal biomass concentration in inflow
      real :: orgnin            !mg N/L        |organic N concentration in inflow
      real :: ammoin            !mg N/L        |ammonium N concentration in inflow
      real :: nitratin          !mg N/L        |nitrate concentration in inflow
      real :: nitritin          !mg N/L        |nitrite concentration in inflow
      real :: orgpin            !mg P/L        |organic P concentration in inflow
      real :: dispin            !mg P/L        |soluble P concentration in inflow
      real :: cbodin            !mg/L          |carbonaceous biological oxygen demand 
      real :: disoxin           !mg O2/L       |dissolved oxygen concentration in inflow
      real :: thour             !none          |flow duration (fraction of hr)
      real :: wtmp              !deg C         |temperature of water in reach
      real :: fll               !none          |growth attenuation factor for light
      real :: gra               !1/hr          |local algal growth rate at 20 deg C
      real :: lambda            !1/m           |light extinction coefficient
      real :: fnn               !none          |algal growth limitation factor for nitrogen
      real :: fpp               !none          |algal growth limitation factor for phosphorus
      real :: algi              !MJ/(m2*hr)    |photosynthetically active light intensity
      real :: xx                !varies        |variable to hold intermediate calculation result
      real :: yy                !varies        |variable to hold intermediate calculation result
      real :: zz                !varies        |variable to hold intermediate calculation result
      real :: ww                !varies        |variable to hold intermediate calculation result
      real :: cinn              !mg N/L        |effective available nitrogen concentration
      real :: uu                !varies        |variable to hold intermediate calculation result
      real :: vv                !varies        |variable to hold intermediate calculation result
      real :: cordo             !none          |nitrification rate correction factor
      real :: f1                !none          |fraction of algal nitrogen uptake from
                                !              |ammonia pool 
      real :: algcon            !mg alg/L      |initial algal biomass concentration in reach
      real :: orgncon           !mg N/L        |initial organic N concentration in reach
      real :: nh3con            !mg N/L        |initial ammonia concentration in reach
      real :: no2con            !mg N/L        |initial nitrite concentration in reach
      real :: no3con            !mg N/L        |initial nitrate concentration in reach
      real :: orgpcon           !mg P/L        |initial organic P concentration in reach
      real :: solpcon           !mg P/L        |initial soluble P concentration in reach
      real :: cbodcon           !mg/L          |initial carbonaceous biological oxygen demand
                                !              |concentration in reach
      real :: o2con             !mg O2/L       |initial dissolved oxygen concentration in 
                                !              |reach
      real :: wtrtot            !m^3 H2O       |inflow + storage water
      real :: bc1mod            !1/day         |rate constant for biological oxidation of NH3
                                !              |to NO2 modified to reflect impact of low 
                                !              |oxygen concentration
      real :: bc2mod            !1/day         |rate constant for biological oxidation of NO2
                                !              |to NO3 modified to reflect impact of low
                                !              |oxygen concentration
      real :: thgra = 1.047     !none          |temperature adjustment factor for local algal growth rate
      real :: thrho = 1.047     !none          |temperature adjustment factor for local algal
                                !              |respiration rate 
      real :: thrs1 = 1.024     !none          |temperature adjustment factor for local algal
                                !              |settling rate
      real :: thrs2 = 1.074     !none          |temperature adjustment factor for local
                                !              |benthos source rate for dissolved phosphorus
      real :: thrs3 = 1.074     !none          |temperature adjustment factor for local
                                !              |benthos source rate for ammonia nitrogen
      real :: thrs4 = 1.024     !none          |temperature adjustment factor for local
                                !              |organic N settling rate
      real :: thrs5 = 1.024     !none          |temperature adjustment factor for local
                                !              |organic P settling rate
      real :: thbc1 = 1.083     !none          |temperature adjustment factor for local
                                !              |biological oxidation of NH3 to NO2
      real :: thbc2 = 1.047     !none          |temperature adjustment factor for local
                                !              |biological oxidation of NO2 to NO3
      real :: thbc3 = 1.047     !none          |temperature adjustment factor for local
                                !              |hydrolysis of organic N to ammonia N
      real :: thbc4 = 1.047     !none          |temperature adjustment factor for local
                                !              |decay of organic P to dissolved P
      real :: thrk1 = 1.047     !none          |temperature adjustment factor for local CBOD
                                !              |deoxygenation
      real :: thrk2 = 1.024     !none          |temperature adjustment factor for local oxygen
                                !              |reaeration rate
      real :: thrk3 = 1.024     !none          |temperature adjustment factor for loss of
                                !              |CBOD due to settling
      real :: thrk4 = 1.060     !none          |temperature adjustment factor for local
                                !              |sediment oxygen demand

!! hourly loop
      do ii = 1, time%step
       !! initialize water flowing into reach
       wtrin = 0.
       wtrin = ob(icmd)%ts(1,ii)%flo

       if (hrtwtr(ii) / (time%dtm * 60.) > 0.01) then
!! concentrations
         !! initialize inflow concentrations
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
         if (wtrin > 0.001) then
         chlin = 1000. * ob(icmd)%ts(1,ii)%chla / wtrin
         algin = 1000. * chlin / ch_nut(jnut)%ai0        !! QUAL2E equation III-1
         orgnin = 1000. * ob(icmd)%ts(1,ii)%orgn / wtrin
         ammoin = 1000. * ob(icmd)%ts(1,ii)%nh3 / wtrin
         nitritin = 1000. * ob(icmd)%ts(1,ii)%no2 / wtrin
         nitratin = 1000. * ob(icmd)%ts(1,ii)%no3 / wtrin
         orgpin = 1000. * ob(icmd)%ts(1,ii)%sedp / wtrin
         dispin = 1000. * ob(icmd)%ts(1,ii)%solp / wtrin
         cbodin = 1000. * ob(icmd)%ts(1,ii)%cbod / wtrin
         disoxin= 1000. * ob(icmd)%ts(1,ii)%dox / wtrin
         end if

         if (chlin < 1.e-6) chlin = 0.0
         if (algin < 1.e-6) algin = 0.0
         if (orgnin < 1.e-6) orgnin = 0.0
         if (ammoin < 1.e-6) ammoin = 0.0
         if (nitritin < 1.e-6) nitritin = 0.0
         if (nitratin < 1.e-6) nitratin = 0.0
         if (orgnpin < 1.e-6) orgnpin = 0.0
         if (dispin < 1.e-6) dispin = 0.0
         if (cbodin < 1.e-6) cbodin = 0.0
         if (disoxin < 1.e-6) disoxin = 0.0

         !! initialize concentration of nutrient in reach
         wtrtot = 0.
         algcon = 0.
         orgncon = 0.
         nh3con = 0.
         no2con = 0.
         no3con = 0.
         orgpcon = 0.
         solpcon = 0.
         cbodcon = 0.
         o2con = 0.
         wtrtot = wtrin + hrchwtr(ii)
         if (ii == 1) then
         algcon = (algin * wtrin + ch(jrch)%algae * hrchwtr(ii)) / wtrtot
         orgncon = (orgnin * wtrin + ch(jrch)%organicn * hrchwtr(ii)) / wtrtot
         nh3con =(ammoin * wtrin + ch(jrch)%ammonian * hrchwtr(ii)) / wtrtot
         no2con = (nitritin * wtrin + ch(jrch)%nitriten * hrchwtr(ii)) / wtrtot
         no3con = (nitratin * wtrin + ch(jrch)%nitraten * hrchwtr(ii)) / wtrtot
         orgpcon = (orgpin * wtrin + ch(jrch)%organicp * hrchwtr(ii)) / wtrtot
         solpcon = (dispin * wtrin + ch(jrch)%disolvp * hrchwtr(ii))  / wtrtot
         cbodcon = (cbodin * wtrin +  ch(jrch)%rch_cbod * hrchwtr(ii)) / wtrtot
         o2con = (disoxin * wtrin + ch(jrch)%rch_dox * hrchwtr(ii)) / wtrtot
         else
         algcon = (algin * wtrin + halgae(ii-1) * hrchwtr(ii)) / wtrtot
         orgncon = (orgnin * wtrin + horgn(ii-1) * hrchwtr(ii)) / wtrtot
         nh3con = (ammoin * wtrin + hnh4(ii-1) * hrchwtr(ii)) / wtrtot
         no2con = (nitritin * wtrin + hno2(ii-1) * hrchwtr(ii)) / wtrtot
         no3con = (nitratin * wtrin + hno3(ii-1) * hrchwtr(ii)) / wtrtot
         orgpcon = (orgpin * wtrin + horgp(ii-1) * hrchwtr(ii)) / wtrtot
         solpcon = (dispin * wtrin + hsolp(ii-1) * hrchwtr(ii)) / wtrtot
         cbodcon = (cbodin * wtrin + hbod(ii-1) * hrchwtr(ii)) / wtrtot
         o2con = (disoxin * wtrin + hdisox(ii-1) * hrchwtr(ii)) / wtrtot
         end if

         if (algcon < 1.e-6) algcon = 0.0
         if (orgncon < 1.e-6) orgncon = 0.0
         if (nh3con < 1.e-6) nh3con = 0.0
         if (no2con < 1.e-6) no2con = 0.0
         if (no3con < 1.e-6) no3con = 0.0
         if (orgpcon < 1.e-6) orgpcon = 0.0
         if (solpcon < 1.e-6) solpcon = 0.0
         if (cbodcon < 1.e-6) cbodcon = 0.0
         if (o2con < 1.e-6) o2con = 0.0
         !! calculate temperature in stream
         !! Stefan and Preudhomme. 1993.  Stream temperature estimation 
         !! from air temperature.  Water Res. Bull. p. 27-45
         !! SWAT manual equation 2.3.13
         wtmp = 0.
         wtmp = 5.0 + 0.75 * wst(iwst)%weat%tave
         if (wtmp <= 0.) wtmp = 0.1

         !! calculate effective concentration of available nitrogen
         !! QUAL2E equation III-15
         cinn = nh3con + no3con

         !! calculate saturation concentration for dissolved oxygen
         !! QUAL2E section 3.6.1 equation III-29
         ww = 0.
         xx = 0.
         yy = 0.
         zz = 0.
         ww = -139.34410 + (1.575701e05 / (wtmp + 273.15))
         xx = 6.642308e07 / ((wtmp + 273.15)**2)
         yy = 1.243800e10 / ((wtmp + 273.15)**3)
         zz = 8.621949e11 / ((wtmp + 273.15)**4)
         soxy = Exp(ww - xx + yy - zz)
         if (soxy < 0.) soxy = 0.
!! end initialize concentrations

!! O2 impact calculations
        !! calculate nitrification rate correction factor for low
        !! oxygen QUAL2E equation III-21
        cordo = 0.
        cordo = 1.0 - Exp(-0.6 * o2con)
        !! modify ammonia and nitrite oxidation rates to account for
        !! low oxygen
        bc1mod = 0.
        bc2mod = 0.
        bc1mod = ch_nut(jnut)%bc1 * cordo
        bc2mod = ch_nut(jnut)%bc2  * cordo
!! end O2 impact calculations

         !! calculate flow duration
         thour = 0.
         thour = hhtime(ii)
         if (thour > 1.0) thour = 1.0
         thour = 1.0

!! algal growth
         !! calculate light extinction coefficient 
         !! (algal self shading) QUAL2E equation III-12
         if (ch_nut(jnut)%ai0 * algcon > 1.e-6) then
           lambda = ch_nut(jnut)%lambda0 + (ch_nut(jnut)%lambda1 *      &
               ch_nut(jnut)%ai0 * algcon)                               &
               + ch_nut(jnut)%lambda2 * (ch_nut(jnut)%ai0 *             &
               algcon) ** (.66667)
         else
           lambda = ch_nut(jnut)%lambda0
         endif

         !! calculate algal growth limitation factors for nitrogen
         !! and phosphorus QUAL2E equations III-13 & III-14
         fnn = 0.
         fpp = 0.
         fnn = cinn / (cinn + ch_nut(jnut)%k_n)
         fpp = solpcon / (solpcon + ch_nut(jnut)%k_p)

         !! calculate hourly, photosynthetically active,
         !! light intensity QUAL2E equation III-9c
         !! Light Averaging Option # 3
         algi = 0.
         algi = frad(iwgn,ii) * wst(iwst)%weat%solrad *                  &
                  ch_nut(jnut)%tfact 

         !! calculate growth attenuation factor for light, based on
         !! hourly light intensity QUAL2E equation III-6a
         fll = 0.
         fll = (1. / (lambda * hdepth(ii))) *                           &                          
         Log((ch_nut(jnut)%k_l + algi) / (ch_nut(jnut)%k_l + algi *     &
            (Exp(-lambda * hdepth(ii)))))

         !! calculcate local algal growth rate
         gra = 0.
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
               gra = ch_nut(jnut)%mumax * fll * 2. /              &
                  ((1. / fnn) + (1. / fpp))
             else
               gra = 0.
             endif
         end select

         !! calculate algal biomass concentration at end of day
         !! (phytoplanktonic algae)
         !! QUAL2E equation III-2
         halgae(ii) = 0.
         halgae(ii) = algcon + (Theta(gra,thgra,wtmp) * algcon -        &        
         Theta(ch_nut(jnut)%rhoq,thrho,wtmp) * algcon -                 &
         Theta(ch_nut(jnut)%rs1,                                        &
         thrs1,wtmp)  / hdepth(ii) * algcon) * thour                    
         if (halgae(ii) < 0.) halgae(ii) = 0.

         !! calculate chlorophyll-a concentration at end of day
         !! QUAL2E equation III-1
         hchla(ii) = 0.
         hchla(ii) = halgae(ii) * ch_nut(jnut)%ai0 / 1000.
!! end algal growth 

!! oxygen calculations
         !! calculate carbonaceous biological oxygen demand at end
         !! of day QUAL2E section 3.5 equation III-26
         yy = 0.
         zz = 0.
         yy = Theta(ch_nut(jnut)%rk1,thrk1,wtmp) * cbodcon
         zz = Theta(ch_nut(jnut)%rk3,thrk3,wtmp) * cbodcon
         hbod(ii) = 0.
         hbod(ii) = cbodcon - (yy + zz) * thour
         if (hbod(ii) < 0.) hbod(ii) = 0.

         !! calculate dissolved oxygen concentration if reach at 
         !! end of day QUAL2E section 3.6 equation III-28
         uu = 0.
         vv = 0.
         ww = 0.
         xx = 0.
         yy = 0.
         zz = 0.
         uu = Theta(ch_nut(jnut)%rk1,thrk2,wtmp) * (soxy - o2con)
         vv = (ch_nut(jnut)%ai3 * Theta(gra,thgra,wtmp) -                &
            ch_nut(jnut)%ai4 * Theta(ch_nut(jnut)%rhoq,thrho,wtmp)) *    &
            algcon
         ww = Theta(ch_nut(jnut)%rk1,thrk1,wtmp) * cbodcon
         xx = Theta(ch_nut(jnut)%rk4,thrk4,wtmp) / (hdepth(ii) * 1000.)
         yy = ch_nut(jnut)%ai5 * Theta(bc1mod,thbc1,wtmp) * nh3con
         zz = ch_nut(jnut)%ai6 * Theta(bc2mod,thbc2,wtmp) * no2con
         hdisox(ii) = 0.
         hdisox(ii) = o2con + (uu + vv - ww - xx - yy - zz) * thour
         if (hdisox(ii) < 0.) hdisox(ii) = 0.
!! end oxygen calculations

!! nitrogen calculations
         !! calculate organic N concentration at end of day
         !! QUAL2E section 3.3.1 equation III-16
         xx = 0.
         yy = 0.
         zz = 0.
         xx = ch_nut(jnut)%ai1 * Theta(ch_nut(jnut)%rhoq,thrho,wtmp) *    &
             algcon
         yy = Theta(ch_nut(jnut)%bc2,thbc3,wtmp) * orgncon
         zz = Theta(ch_nut(jnut)%rs4,thrs4,wtmp) * orgncon
         horgn(ii) = 0.
         horgn(ii) = orgncon + (xx - yy - zz) * thour
         if (horgn(ii) < 0.) horgn(ii) = 0.

        !! calculate fraction of algal nitrogen uptake from ammonia
        !! pool QUAL2E equation III-18
        f1 = 0.
        f1 = ch_nut(jnut)%p_n * nh3con / (ch_nut(jnut)%p_n * nh3con +    &
           (1. - ch_nut(jnut)%p_n) * no3con + 1.e-6)

        !! calculate ammonia nitrogen concentration at end of day
        !! QUAL2E section 3.3.2 equation III-17
        ww = 0.
        xx = 0.
        yy = 0.
        zz = 0.
        ww = Theta(ch_nut(jnut)%bc2,thbc3,wtmp) * orgncon
        xx = Theta(bc1mod,thbc1,wtmp) * nh3con
        yy = Theta(ch_nut(jnut)%rs3,thrs3,wtmp) / (hdepth(ii) * 1000.)
        zz = f1 * ch_nut(jnut)%ai1 * algcon * Theta(gra,thgra,wtmp)
        hnh4(ii) = 0.
        hnh4(ii) = nh3con + (ww - xx + yy - zz) * thour
        if (hnh4(ii) < 0.) hnh4(ii) = 0.

        !! calculate concentration of nitrite at end of day
        !! QUAL2E section 3.3.3 equation III-19
        yy = 0.
        zz = 0.
        yy = Theta(bc1mod,thbc1,wtmp) * nh3con
        zz = Theta(bc2mod,thbc2,wtmp) * no2con
        hno2(ii) = 0.
        hno2(ii) = no2con + (yy - zz) * thour
        if (hno2(ii) < 0.) hno2(ii) = 0.

        !! calculate nitrate concentration at end of day
        !! QUAL2E section 3.3.4 equation III-20
        yy = 0.
        zz = 0.
        yy = Theta(bc2mod,thbc2,wtmp) * no2con
        zz = (1. - f1) * ch_nut(jnut)%ai1 * algcon *                     &
                           Theta(gra,thgra,wtmp)
        hno3(ii) = 0.
        hno3(ii) = no3con + (yy - zz) * thour
        if (hno3(ii) < 0.) hno3(ii) = 0.
!! end nitrogen calculations

!! phosphorus calculations
        !! calculate organic phosphorus concentration at end of
        !! day QUAL2E section 3.3.6 equation III-24
        xx = 0.
        yy = 0.
        zz = 0.
        xx = ch_nut(jnut)%ai2 * Theta(ch_nut(jnut)%rhoq,thrho,wtmp) *    &
               algcon
        yy = Theta(ch_nut(jnut)%bc4,thbc4,wtmp) * orgpcon
        zz = Theta(ch_nut(jnut)%rs5,thrs5,wtmp) * orgpcon
        horgp(ii) = 0.
        horgp(ii) = orgpcon + (xx - yy - zz) * thour
        if (horgp(ii) < 0.) horgp(ii) = 0.

        !! calculate dissolved phosphorus concentration at end
        !! of day QUAL2E section 3.4.2 equation III-25
        xx = 0.
        yy = 0.
        zz = 0.
        xx = Theta(ch_nut(jnut)%bc4,thbc4,wtmp) * orgpcon
        yy = Theta(ch_nut(jnut)%rs2,thrs2,wtmp) / (hdepth(ii) * 1000.)
        zz = ch_nut(jnut)%ai2 * Theta(gra,thgra,wtmp) * algcon
        hsolp(ii) = 0.
        hsolp(ii) = solpcon + (xx + yy - zz) * thour
        if (hsolp(ii) < 0.) hsolp(ii) = 0.
!! end phosphorus calculations

      else
        !! all water quality variables set to zero when no flow
        algin = 0.0
        chlin = 0.0
        orgnin = 0.0
        ammoin = 0.0
        nitritin = 0.0
        nitratin = 0.0
        orgpin = 0.0
        dispin = 0.0
        cbodin = 0.0
        disoxin = 0.0
        halgae(ii) = 0.0
        hchla(ii) = 0.0
        horgn(ii) = 0.0
        hnh4(ii) = 0.0
        hno2(ii) = 0.0
        hno3(ii) = 0.0
        horgp(ii) = 0.0
        hsolp(ii) = 0.0
        hbod(ii) = 0.0
        hdisox(ii) = 0.0
        soxy = 0.0
      endif
        if (halgae(ii) < 1.e-6) halgae(ii) = 0.0
        if (hchla(ii) < 1.e-6) hchla(ii) = 0.0
        if (horgn(ii) < 1.e-6) horgn(ii) = 0.0
        if (hnh4(ii) < 1.e-6) hnh4(ii) = 0.0
        if (hno2(ii) < 1.e-6) hno2(ii) = 0.0
        if (hno3(ii) < 1.e-6) hno3(ii) = 0.0
        if (horgp(ii) < 1.e-6) horgp(ii) = 0.0
        if (hsolp(ii) < 1.e-6) hsolp(ii) = 0.0
        if (hbod(ii) < 1.e-6) hbod(ii) = 0.0
        if (hdisox(ii) < 1.e-6) hdisox(ii) = 0.0
        if (soxy < 1.e-6) soxy = 0.0

      end do
!! end hourly loop

!! set end of day concentrations
      ch(jrch)%algae = halgae(time%step)
      ch(jrch)%chlora = hchla(time%step)
      ch(jrch)%organicn = horgn(time%step)
      ch(jrch)%ammonian = hnh4(time%step)
      ch(jrch)%nitriten = hno2(time%step)
      ch(jrch)%nitraten = hno3(time%step)
      ch(jrch)%organicp = horgp(time%step)
      ch(jrch)%disolvp = hsolp(time%step)
      ch(jrch)%rch_cbod = hbod(time%step)
      ch(jrch)%rch_dox = hdisox(time%step)

      if (ch(jrch)%algae < 1.e-6) ch(jrch)%algae = 0.0
      if (ch(jrch)%chlora < 1.e-6) ch(jrch)%chlora = 0.0
      if (ch(jrch)%organicn < 1.e-6) ch(jrch)%organicn = 0.0
      if (ch(jrch)%ammonian < 1.e-6) ch(jrch)%ammonian = 0.0
      if (ch(jrch)%nitriten < 1.e-6) ch(jrch)%nitriten = 0.0
      if (ch(jrch)%organicp < 1.e-6) ch(jrch)%organicp = 0.0
      if (ch(jrch)%disolvp < 1.e-6) ch(jrch)%disolvp = 0.0
      if (ch(jrch)%rch_cbod < 1.e-6) ch(jrch)%rch_cbod = 0.0
      if (ch(jrch)%rch_dox < 1.e-6) ch(jrch)%rch_dox = 0.0

      return
      end subroutine ch_hhwatqual