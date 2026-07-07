      subroutine ch_watqual

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs in-stream nutrient transformations and water
!!    quality calculations

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ai0          |ug chla/mg alg|ratio of chlorophyll-a to algal biomass
!!    ai1          |mg N/mg alg   |fraction of algal biomass that is nitrogen
!!    ai2          |mg P/mg alg   |fraction of algal biomass that is phosphorus
!!    ai3          |mg O2/mg alg  |the rate of oxygen production per unit of
!!                                |algal photosynthesis
!!    ai4          |mg O2/mg alg  |the rate of oxygen uptake per unit of algae
!!                                |respiration
!!    ai5          |mg O2/mg N    |the rate of oxygen uptake per unit of NH3
!!                                |nitrogen oxidation
!!    ai6          |mg O2/mg N    |the rate of oxygen uptake per unit of NO2
!!                                |nitrogen oxidation
!!    algae(:)     |mg alg/L      |algal biomass concentration in reach
!!    ammonian(:)  |mg N/L        |ammonia concentration in reach
!!    bc1(:)       |1/day         |rate constant for biological oxidation of NH3
!!                                |to NO2 in reach at 20 deg C
!!    bc2(:)       |1/day         |rate constant for biological oxidation of NO2
!!                                |to NO3 in reach at 20 deg C
!!    bc3(:)       |1/day         |rate constant for hydrolysis of organic N to
!!                                |ammonia in reach at 20 deg C
!!    bc4(:)       |1/day         |rate constant for the decay of organic P to
!!                                |dissolved P in reach at 20 deg C
!!    chlora(:)    |mg chl-a/L    |chlorophyll-a concentration in reach
!!    disolvp(:)   |mg P/L        |dissolved phosphorus concentration in reach
!!    k_l          |MJ/(m2*hr)    |half saturation coefficient for light
!!    k_n          |mg N/L        |michaelis-menton half-saturation constant
!!                                |for nitrogen
!!    k_p          |mg P/L        |michaelis-menton half saturation constant
!!                                |for phosphorus
!!    lambda0      |1/m           |non-algal portion of the light extinction
!!                                |coefficient
!!    lambda1      |1/(m*ug chla/L)|linear algal self-shading coefficient
!!    lambda2      |(1/m)(ug chla/L)**(-2/3)
!!                                |nonlinear algal self-shading coefficient
!!    mumax        |1/day         |maximum specific algal growth rate at 20 deg 
!!                                |C
!!    nitraten(:)  |mg N/L        |nitrate concentration in reach
!!    nitriten(:)  |mg N/L        |nitrite concentration in reach
!!    organicn(:)  |mg N/L        |organic nitrogen concentration in reach
!!    organicp(:)  |mg P/L        |organic phosphorus concentration in reach
!!    rch_cbod(:)  |mg O2/L       |carbonaceous biochemical oxygen demand in
!!                                |reach 
!!    rch_dox(:)   |mg O2/L       |dissolved oxygen concentration in reach
!!    rchdep       |m             |depth of flow on day
!!    rchwtr       |m^3 H2O       |water stored in reach at beginning of day
!!    rhoq         |1/day         |algal respiration rate at 20 deg C
!!    rttime       |hr            |reach travel time
!!    tfact        |none          |fraction of solar radiation computed in the
!!                                |temperature heat balance that is
!!                                |photosynthetically active
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
!!    algi        |MJ/(m2*hr)    |daylight average, photosynthetically active,
!!                               |light intensity
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
!!    fl_1        |none          |growth attenuation factor for light, based on
!!                               |daylight-average light intensity
!!    fll         |none          |growth attenuation factor for light averaged
!!                               |over the diurnal cycle
!!    fnn         |none          |algal growth limitation factor for nitrogen
!!    fpp         |none          |algal growth limitation factor for phosphorus
!!    gra         |1/day         |local algal growth rate at 20 deg C
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
!!    tday        |none          |flow duration (fraction of 24 hr)
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

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      
      use channel_data_module
      use channel_module
      use hydrograph_module, only : ob
      use climate_module

      implicit none

      integer :: icmd        !units         |description
      integer :: jrch        !none          |reach number
      integer :: iwst        !none          |counter
      integer :: iwgn        !units         |description
      real :: soxy           !mg O2/L       |saturation concetration of dissolved oxygen
      real :: chlin          !mg chl-a/L    |chlorophyll-a concentration in inflow
      real :: algin          !mg alg/L      |algal biomass concentration in inflow
      real :: orgnin         !mg N/L        |organic N concentration in inflow
      real :: ammoin         !mg N/L        |ammonium N concentration in inflow 
      real :: nitratin       !mg N/L        |nitrate concentration in inflow
      real :: nitritin       !mg N/L        |nitrite concentration in inflow
      real :: orgpin         !mg P/L        |organic P concentration in inflow
      real :: dispin         !mg P/L        |soluble P concentration in inflow
      real :: cbodin         !mg/L          |carbonaceous biological oxygen demand 
                             !              |concentration in inflow
      real :: disoxin        !mg O2/L       |dissolved oxygen concentration in inflow
      real :: tday           !none          |flow duration (fraction of 24 hr)
      real :: wtmp           !deg C         |temperature of water in reach
      real :: fll            !none          |growth attenuation factor for light averaged
                             !              |over the diurnal cycle
      real :: gra            !1/day         |local algal growth rate at 20 deg C
      real :: lambda         !1/m           |light extinction coefficient
      real :: fnn            !none          |algal growth limitation factor for nitrogen
      real :: fpp            !none          |algal growth limitation factor for phosphorus 
      real :: algi           !MJ/(m2*hr)    |daylight average, photosynthetically active,
                             !              |light intensity
      real :: fl_1           !none          |growth attenuation factor for light, based on
                             !              |daylight-average light intensity
      real :: xx             !varies        |variable to hold intermediate calculation
                             !              |result
      real :: yy             !varies        |variable to hold intermediate calculation
                             !              |result
      real :: zz             !varies        |variable to hold intermediate calculation
                             !              |result
      real :: ww             !varies        |variable to hold intermediate calculation
                             !              |result
      real :: cinn           !mg N/L        |effective available nitrogen concentration
      real :: uu             !varies        |variable to hold intermediate calculation
                             !              |result
      real :: vv             !varies        |variable to hold intermediate calculation
                             !              |result
      real :: cordo          !none          |nitrification rate correction factor 
      real :: f1             !none          |fraction of algal nitrogen uptake from
                             !              |ammonia pool
      real :: algcon         !mg alg/L      |initial algal biomass concentration in reach
      real :: orgncon        !mg N/L        |initial organic N concentration in reach
      real :: nh3con         !mg N/L        |initial ammonia concentration in reach
      real :: no2con         !mg N/L        |initial nitrite concentration in reach
      real :: no3con         !mg N/L        |initial nitrate concentration in reach
      real :: orgpcon        !mg P/L        |initial organic P concentration in reach
      real :: solpcon        !mg P/L        |initial soluble P concentration in reach 
      real :: cbodcon        !mg/L          |initial carbonaceous biological oxygen demand
                             !              |concentration in reach
      real :: o2con          !mg O2/L       |initial dissolved oxygen concentration in 
                             !              |reach
      real :: wtrtot         !m^3 H2O       |inflow + storage water
      real :: bc1mod         !1/day         |rate constant for biological oxidation of NH3
                             !              |to NO2 modified to reflect impact of low 
                             !              |oxygen concentration
      real :: bc2mod         !1/day         |rate constant for biological oxidation of NO2
                             !              |to NO3 modified to reflect impact of low
                             !              |oxygen concentration
      real :: thgra = 1.047  !none          |temperature adjustment factor for local algal
                             !              |growth rate
      real :: thrho = 1.047  !none          |temperature adjustment factor for local algal
                             !              |respiration rate
      real :: thrs1 = 1.024  !none          |temperature adjustment factor for local algal
                             !              |settling rate
      real :: thrs2 = 1.074  !none          |temperature adjustment factor for local
                             !              |benthos source rate for dissolved phosphorus
      real :: thrs3 = 1.074  !none          |temperature adjustment factor for local
                             !              |benthos source rate for ammonia nitrogen
      real :: thrs4 = 1.024  !none          |temperature adjustment factor for local
                             !              |organic N settling rate
      real :: thrs5 = 1.024  !none          |temperature adjustment factor for local
                             !              |organic P settling rate
      real :: thbc1 = 1.083  !none          |temperature adjustment factor for local
                             !              |biological oxidation of NH3 to NO2
      real :: thbc2 = 1.047  !none          |temperature adjustment factor for local
                             !              |biological oxidation of NO2 to NO3
      real :: thbc3 = 1.047  !none          |temperature adjustment factor for local
                             !              |hydrolysis of organic N to ammonia N
      real :: thbc4 = 1.047  !none          |temperature adjustment factor for local
                             !              |decay of organic P to dissolved P      
      real :: thrk1 = 1.047  !none          |temperature adjustment factor for local CBOD
                             !              |deoxygenation
      real :: thrk2 = 1.024  !none          |temperature adjustment factor for local oxygen
                             !              |reaeration rate
      real :: thrk3 = 1.024  !none          |temperature adjustment factor for loss of
                             !              |CBOD due to settling
      real :: thrk4 = 1.060  !none          |temperature adjustment factor for local
                             !              |sediment oxygen demand      
      real :: dcoef          !units         |description
      real :: rchwtr         !m^3 H2O       |water stored in reach at beginning of day    
      real :: theta          !units         |description
      real :: coef           !units         |description
      real :: cbodrch        !units         |description
      real :: doxrch         !units         |description
      

      dcoef= 3.

       !! initialize water flowing into reach
       wtrin = 0.
       wtrin = ob(icmd)%hin%flo 

       if (wtrin > 1.e-4) then
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
         chlin = 1000. * ob(icmd)%hin%chla  / wtrin
         algin = 1000. * chlin / ch_nut(jnut)%ai0        !! QUAL2E equation III-1
         orgnin = 1000. * ob(icmd)%hin%orgn  / wtrin
         ammoin = 1000. * ob(icmd)%hin%nh3  / wtrin
         nitritin = 1000. * ob(icmd)%hin%no2  / wtrin
         nitratin = 1000. * ob(icmd)%hin%no3  / wtrin
         orgpin = 1000. * ob(icmd)%hin%sedp  / wtrin
         dispin = 1000. * ob(icmd)%hin%solp  / wtrin
         cbodin = 1000. * ob(icmd)%hin%cbod  / wtrin
         disoxin = 1000. * ob(icmd)%hin%dox  / wtrin
         end if

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
         ch(jrch)%rch_cbod = Max(1.e-6, ch(jrch)%rch_cbod)
         wtrtot = wtrin + rchwtr
         algcon =(algin * wtrin + ch(jrch)%algae * rchwtr) / wtrtot
         orgncon =(orgnin * wtrin + ch(jrch)%organicn * rchwtr) / wtrtot
         nh3con = (ammoin * wtrin + ch(jrch)%ammonian * rchwtr) / wtrtot
         no2con = (nitritin * wtrin + ch(jrch)%nitriten * rchwtr) / wtrtot
         no3con = (nitratin * wtrin + ch(jrch)%nitraten * rchwtr) / wtrtot
         orgpcon =(orgpin * wtrin + ch(jrch)%organicp * rchwtr) / wtrtot
         solpcon = (dispin * wtrin + ch(jrch)%disolvp * rchwtr) / wtrtot
         cbodcon= (cbodin * wtrin + ch(jrch)%rch_cbod * rchwtr) / wtrtot
         o2con = (disoxin * wtrin + ch(jrch)%rch_dox * rchwtr) / wtrtot

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
         if (soxy < 1.e-6) soxy = 0. 
!! end initialize concentrations

!! O2 impact calculations
        !! calculate nitrification rate correction factor for low
        !! oxygen QUAL2E equation III-21
        cordo = 0.
	if (o2con.le.0.001) o2con=0.001
	if (o2con.gt.30.) o2con=30.
        cordo = 1.0 - Exp(-0.6 * o2con)
        !! modify ammonia and nitrite oxidation rates to account for
        !! low oxygen
        bc1mod = 0.
        bc2mod = 0.
        bc1mod = ch_nut(jnut)%bc1 * cordo
        bc2mod = ch_nut(jnut)%bc2 * cordo
!! end O2 impact calculations

         !! calculate flow duration
         tday = 0.
         tday = rttime / 24.0
         if (tday > 1.0) tday = 1.0
    !!     tday = 1.0

!! algal growth
         !! calculate light extinction coefficient 
         !! (algal self shading) QUAL2E equation III-12
         if (ch_nut(jnut)%ai0 * algcon > 1.e-6) then
           lambda = ch_nut(jnut)%lambda0 + (ch_nut(jnut)%lambda1 *      &
              ch_nut(jnut)%ai0 * algcon)                                &
              + ch_nut(jnut)%lambda2 * (ch_nut(jnut)%ai0 *              & 
              algcon) ** (.66667)
         else
           lambda = ch_nut(jnut)%lambda0
         endif

	   If (lambda > ch_nut(jnut)%lambda0) lambda = ch_nut(jnut)%lambda0

         !! calculate algal growth limitation factors for nitrogen
         !! and phosphorus QUAL2E equations III-13 & III-14
         fnn = 0.
         fpp = 0.
         fnn = cinn / (cinn + ch_nut(jnut)%k_n)
         fpp = solpcon / (solpcon + ch_nut(jnut)%k_p)

         !! calculate daylight average, photosynthetically active,
         !! light intensity QUAL2E equation III-8
         !! Light Averaging Option # 2
         iwgn = wst(iwst)%wco%wgn
         if (wgn_pms(iwgn)%daylth > 0.) then
           algi = wst(iwst)%weat%solrad * ch_nut(jnut)%tfact /           &
                 wgn_pms(iwgn)%daylth
         else
           algi = 0.00001
         end if

         !! calculate growth attenuation factor for light, based on
         !! daylight average light intensity QUAL2E equation III-7b
         fl_1 = 0.
         fll = 0.
         fl_1 = (1. / (lambda * rchdep)) *                               &                             
             Log((ch_nut(jnut)%k_l + algi) / (ch_nut(jnut)%k_l + algi *  &
             (Exp(-lambda * rchdep))))
         fll = 0.92 * (wgn_pms(iwgn)%daylth / 24.) * fl_1

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
               gra = ch_nut(jnut)%mumax * fll * 2. /                     &
                   ((1. / fnn) + (1. / fpp))
             else
               gra = 0.
             endif
         end select

         !! calculate algal biomass concentration at end of day
         !! (phytoplanktonic algae)
         !! QUAL2E equation III-2
         ch(jrch)%algae = 0.
         ch(jrch)%algae = algcon + (Theta(gra,thgra,wtmp) * algcon      &     
                         -  Theta(ch_nut(jnut)%rhoq,thrho,wtmp) *       &         
                         algcon - Theta(ch_nut(jnut)%rs1,thrs1,wtmp)    &
                               / rchdep * algcon) * tday
         if (ch(jrch)%algae < 1.e-6) ch(jrch)%algae = 0.
	!! JGA added to set algae limit *****
	   if (ch(jrch)%algae > 5000.) ch(jrch)%algae = 5000.
         if (ch(jrch)%algae > dcoef * algcon) ch(jrch)%algae =          &         
                                                       dcoef * algcon

         !! calculate chlorophyll-a concentration at end of day
         !! QUAL2E equation III-1
         ch(jrch)%chlora = 0.
         ch(jrch)%chlora = ch(jrch)%algae * ch_nut(jnut)%ai0 / 1000.
         !! end algal growth 

!! oxygen calculations
         !! calculate carbonaceous biological oxygen demand at end
         !! of day QUAL2E section 3.5 equation III-26
         yy = 0.
         zz = 0.
         yy = Theta(ch_nut(jnut)%rk1,thrk1,wtmp) * cbodcon
         zz = Theta(ch_nut(jnut)%rk3,thrk3,wtmp) * cbodcon
         ch(jrch)%rch_cbod = 0.
         ch(jrch)%rch_cbod = cbodcon - (yy + zz) * tday
         
         !!deoxygenation rate
         coef = exp(-Theta(ch_nut(jnut)%rk1,thrk1,wtmp) * tday)
         cbodrch = coef * cbodcon
         !!cbod rate loss due to settling
         coef = exp(-Theta(ch_nut(jnut)%rk3,thrk3,wtmp) * tday)
         cbodrch = coef * cbodrch
         
         ch(jrch)%rch_cbod = cbodrch
         if (ch(jrch)%rch_cbod < 1.e-6) ch(jrch)%rch_cbod = 0.
	   if (ch(jrch)%rch_cbod > dcoef * cbodcon) ch(jrch)%rch_cbod = dcoef * cbodcon

         !! calculate dissolved oxygen concentration if reach at 
         !! end of day QUAL2E section 3.6 equation III-28
         uu = 0.
         vv = 0.
         ww = 0.
         xx = 0.
         yy = 0.
         zz = 0.
         ch_nut(jnut)%rhoq = 1.0
         ch_nut(jnut)%rk2 = 1.0
         uu = Theta(ch_nut(jnut)%rk2,thrk2,wtmp) * (soxy - o2con)
         vv = (ch_nut(jnut)%ai3 * Theta(gra,thgra,wtmp) -                &
               ch_nut(jnut)%ai4 *                                        &
               Theta(ch_nut(jnut)%rhoq,thrho,wtmp)) * algcon
         ww = Theta(ch_nut(jnut)%rk1,thrk1,wtmp) * cbodcon
         xx = Theta(ch_nut(jnut)%rk4,thrk4,wtmp) / (rchdep * 1000.)
         yy = ch_nut(jnut)%ai5 * Theta(bc1mod,thbc1,wtmp) * nh3con
         zz = ch_nut(jnut)%ai6 * Theta(bc2mod,thbc2,wtmp) * no2con
         ch(jrch)%rch_dox = 0.
         ch(jrch)%rch_dox = o2con + (uu + vv - ww - xx - yy - zz) * tday
         
         !algea O2 production minus respiration
         if (vv > 0.) then
           doxrch = soxy
         else
           coef = exp(-0.03 * vv)
           doxrch = coef * soxy
         end if
         
         !cbod deoxygenation
         coef = exp(-0.1 * ww)
         doxrch = coef * doxrch
         
         !benthic sediment oxidation
         coef = 1. - (Theta(ch_nut(jnut)%rk4,thrk4,wtmp) / 100.)
         doxrch = coef * doxrch
         
         !ammonia oxydation
         coef = exp(-0.05 * yy)
         doxrch = coef * doxrch
         
         !nitrite oxydation
         coef = exp(-0.05 * zz)
         doxrch = coef * doxrch
         
         !reaeration
         uu = Theta(ch_nut(jnut)%rk2,thrk2,wtmp) / 100. *                &
                (soxy - doxrch)
         ch(jrch)%rch_dox = doxrch + uu
         
         if (ch(jrch)%rch_dox < 1.e-6) ch(jrch)%rch_dox = 0.
         if (ch(jrch)%rch_dox > soxy) ch(jrch)%rch_dox = soxy
         if (ch(jrch)%rch_dox > dcoef * o2con) ch(jrch)%rch_dox=         &        
                                                       dcoef * o2con
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
!        red_fac = orgncon / 4.
!        if (red_fac > 0.75) red_fac = 0.75
!        zz = zz + red_fac
         ch(jrch)%organicn = 0.
         ch(jrch)%organicn = orgncon + (xx - yy - zz) * tday
         if (ch(jrch)%organicn < 1.e-6) ch(jrch)%organicn = 0.
   	   if(ch(jrch)%organicn > dcoef * orgncon) ch(jrch)%organicn        &       
                                                      = dcoef * orgncon

        !! calculate fraction of algal nitrogen uptake from ammonia
        !! pool QUAL2E equation III-18
        f1 = 0.
        f1 = ch_nut(jnut)%p_n * nh3con / (ch_nut(jnut)%p_n * nh3con +     &
            (1. - ch_nut(jnut)%p_n) * no3con + 1.e-6)

        !! calculate ammonia nitrogen concentration at end of day
        !! QUAL2E section 3.3.2 equation III-17
        ww = 0.
        xx = 0.
        yy = 0.
        zz = 0.
        ww = Theta(ch_nut(jnut)%bc2,thbc3,wtmp) * orgncon
        xx = Theta(bc1mod,thbc1,wtmp) * nh3con
        yy = Theta(ch_nut(jnut)%rs3,thrs3,wtmp) / (rchdep * 1000.)
        zz = f1 * ch_nut(jnut)%ai1 * algcon * Theta(gra,thgra,wtmp)
        ch(jrch)%ammonian = 0.
        ch(jrch)%ammonian = nh3con + (ww - xx + yy - zz) * tday
        if (ch(jrch)%ammonian < 1.e-6) ch(jrch)%ammonian = 0.
        if (ch(jrch)%ammonian > dcoef * nh3con .and. nh3con > 0.)        & 
             ch(jrch)%ammonian = dcoef * nh3con  

        !! calculate concentration of nitrite at end of day
        !! QUAL2E section 3.3.3 equation III-19
        yy = 0.
        zz = 0.
        yy = Theta(bc1mod,thbc1,wtmp) * nh3con
        zz = Theta(bc2mod,thbc2,wtmp) * no2con
        ch(jrch)%nitriten = 0.
        ch(jrch)%nitriten = no2con + (yy - zz) * tday
        if (ch(jrch)%nitriten < 1.e-6) ch(jrch)%nitriten = 0.
	  if (ch(jrch)%nitriten > dcoef * no2con .and. no2con > 0.)             &
                  ch(jrch)%nitriten = dcoef * no2con

        !! calculate nitrate concentration at end of day
        !! QUAL2E section 3.3.4 equation III-20
        yy = 0.
        zz = 0.
        yy = Theta(bc2mod,thbc2,wtmp) * no2con
        zz = (1. - f1) * ch_nut(jnut)%ai1 * algcon *                     &
                         Theta(gra,thgra,wtmp)
        ch(jrch)%nitraten = 0.
        ch(jrch)%nitraten = no3con + (yy - zz) * tday
        if (ch(jrch)%nitraten > dcoef * no3con) ch(jrch)%nitraten =      &     
                            dcoef * no3con
	
        if (ch(jrch)%nitraten < 1.e-6) ch(jrch)%nitraten = 0.
!! end nitrogen calculations

!! phosphorus calculations
        !! calculate organic phosphorus concentration at end of
        !! day QUAL2E section 3.3.6 equation III-24
        xx = 0.
        yy = 0.
        zz = 0.
        xx = ch_nut(jnut)%ai2 * Theta(ch_nut(jnut)%rhoq,thrho,wtmp) *     &
           algcon
        yy = Theta(ch_nut(jnut)%bc4,thbc4,wtmp) * orgpcon
        zz = Theta(ch_nut(jnut)%rs5,thrs5,wtmp) * orgpcon
        ch(jrch)%organicp = 0.
        ch(jrch)%organicp = orgpcon + (xx - yy - zz) * tday
        if (ch(jrch)%organicp < 1.e-6) ch(jrch)%organicp = 0.
        if (ch(jrch)%organicp > dcoef * orgpcon) ch(jrch)%organicp =     &   
                                                       dcoef * orgpcon

        !! calculate dissolved phosphorus concentration at end
        !! of day QUAL2E section 3.4.2 equation III-25
        xx = 0.
        yy = 0.
        zz = 0.
        xx = Theta(ch_nut(jnut)%bc4,thbc4,wtmp) * orgpcon
        yy = Theta(ch_nut(jnut)%rs2,thrs2,wtmp) / (rchdep * 1000.)
        zz = ch_nut(jnut)%ai2 * Theta(gra,thgra,wtmp) * algcon
        ch(jrch)%disolvp = 0.
        ch(jrch)%disolvp = solpcon + (xx + yy - zz) * tday
        if (ch(jrch)%disolvp < 1.e-6) ch(jrch)%disolvp = 0.
	  if (ch(jrch)%disolvp > dcoef * solpcon) ch(jrch)%disolvp = dcoef *    &
                 solpcon   
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
        ch(jrch)%algae = 0.0
        ch(jrch)%chlora = 0.0
        ch(jrch)%organicn = 0.0
        ch(jrch)%ammonian = 0.0
        ch(jrch)%nitriten = 0.0
        ch(jrch)%nitraten = 0.0
        ch(jrch)%organicp = 0.0
        ch(jrch)%disolvp = 0.0
        ch(jrch)%rch_cbod = 0.0
        ch(jrch)%rch_dox = 0.0
        soxy = 0.0
      endif

      return
      end subroutine ch_watqual