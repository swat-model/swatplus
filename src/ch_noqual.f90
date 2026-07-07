      subroutine ch_noqual
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs in-stream nutrient calculations. No transformations
!!    are calculated. New concentrations of the nutrients are calculated based
!!    on the loading to the reach from upstream.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ai0          |ug chla/mg alg|ratio of chlorophyll-a to algal biomass
!!    algae(:)     |mg alg/L      |algal biomass concentration in reach
!!    ammonian(:)  |mg N/L        |ammonia concentration in reach
!!    disolvp(:)   |mg P/L        |dissolved phosphorus concentration in reach
!!    nitraten(:)  |mg N/L        |nitrate concentration in reach
!!    nitriten(:)  |mg N/L        |nitrite concentration in reach
!!    organicn(:)  |mg N/L        |organic nitrogen concentration in reach
!!    organicp(:)  |mg P/L        |organic phosphorus concentration in reach
!!    rch_cbod(:)  |mg O2/L       |carbonaceous biochemical oxygen demand in
!!                                |reach 
!!    rch_dox(:)   |mg O2/L       |dissolved oxygen concentration in reach
!!    rchwtr       |m^3 H2O       |water stored in reach at beginning of day
!!    rtwtr        |m^3 H2O       |flow out of reach
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
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    algcon      |mg alg/L      |initial algal biomass concentration in reach
!!    algin       |mg alg/L      |algal biomass concentration in inflow
!!    ammoin      |mg N/L        |ammonium N concentration in inflow
!!    cbodcon     |mg/L          |initial carbonaceous biological oxygen demand
!!                               |concentration in reach
!!    cbodin      |mg/L          |carbonaceous biological oxygen demand 
!!                               |concentration in inflow
!!    chlin       |mg chl-a/L    |chlorophyll-a concentration in inflow
!!    disoxin     |mg O2/L       |dissolved oxygen concentration in inflow
!!    dispin      |mg P/L        |soluble P concentration in inflow
!!    jrch        |none          |reach number
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
!!    wtrin       |m^3 H2O       |water flowing into reach on day
!!    wtrtot      |m^3 H2O       |inflow + storage water
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      
      use channel_data_module
      use channel_module
      use hydrograph_module, only : ob, icmd, jrch
      
      implicit none

      real :: chlin       !mg chl-a/L    |chlorophyll-a concentration in inflow
      real :: algin       !mg alg/L      |algal biomass concentration in inflow
      real :: orgnin      !mg N/L        |organic N concentration in inflow
      real :: ammoin      !mg N/L        |ammonium N concentration in inflow
      real :: nitratin    !mg N/L        |nitrate concentration in inflow
      real :: nitritin    !mg N/L        |nitrite concentration in inflow
      real :: orgpin      !mg P/L        |organic P concentration in inflow
      real :: dispin      !mg P/L        |soluble P concentration in inflow
      real :: cbodin      !mg/L          |carbonaceous biological oxygen demand 
                          !              |concentration in inflow
      real :: disoxin     !mg O2/L       |dissolved oxygen concentration in inflow
      real :: algcon      !mg alg/L      |initial algal biomass concentration in reach
      real :: orgncon     !mg N/L        |initial organic N concentration in reach
      real :: nh3con      !mg N/L        |initial ammonia concentration in reach
      real :: no2con      !mg N/L        |initial nitrite concentration in reach
      real :: no3con      !mg N/L        |initial nitrate concentration in reach
      real :: orgpcon     !mg P/L        |initial organic P concentration in reach
      real :: solpcon     !mg P/L        |initial soluble P concentration in reach
      real :: cbodcon     !mg/L          |initial carbonaceous biological oxygen demand
                          !              |concentration in reach
      real :: o2con       !mg O2/L       |initial dissolved oxygen concentration in 
                          !              |reach
      real :: wtrtot      !m^3 H2O       |inflow + storage water
      real :: cinn        !mg N/L        |effective available nitrogen concentration
      real :: rchwtr      !m^3 H2O       |water stored in reach at beginning of day

       !! initialize water flowing into reach
       wtrin = 0.
       wtrin = ob(icmd)%hd(1)%flo

       if (rtwtr / 86400. > 0.01 .and. wtrin > 0.01) then
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
         if (ob(icmd)%hd(1)%chla < 1.e-6) ob(icmd)%hd(1)%cbod = 0.0
         chlin = 1000. * ob(icmd)%hd(1)%chla  / wtrin
         algin = 1000. * chlin / ch_nut(jnut)%ai0        !! QUAL2E equation III-1
         orgnin = 1000. * ob(icmd)%hd(1)%orgn   / wtrin
         ammoin = 1000. * ob(icmd)%hd(1)%nh3  / wtrin
         nitritin = 1000. * ob(icmd)%hd(1)%no2  / wtrin
         nitratin = 1000. * ob(icmd)%hd(1)%no3  / wtrin
         orgpin = 1000. * ob(icmd)%hd(1)%sedp  / wtrin
         dispin = 1000. * ob(icmd)%hd(1)%solp  / wtrin
         if (ob(icmd)%hd(1)%cbod < 1.e-6) ob(icmd)%hd(1)%cbod = 0.0
         cbodin = 1000. * ob(icmd)%hd(1)%cbod  / wtrin
         if (ob(icmd)%hd(1)%dox < 1.e-6) ob(icmd)%hd(1)%dox = 0.0
         disoxin= 1000. * ob(icmd)%hd(1)%dox  / wtrin

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
         if (ch(jrch)%algae < 1.e-6) ch(jrch)%algae = 0.0
         if (ch(jrch)%organicn < 1.e-6) ch(jrch)%organicn = 0.0
         if (ch(jrch)%ammonian < 1.e-6) ch(jrch)%ammonian = 0.0
         if (ch(jrch)%nitriten < 1.e-6) ch(jrch)%nitriten = 0.0
         if (ch(jrch)%nitraten < 1.e-6) ch(jrch)%nitraten = 0.0
         if (ch(jrch)%organicp < 1.e-6) ch(jrch)%organicp = 0.0
         if (ch(jrch)%disolvp < 1.e-6) ch(jrch)%disolvp = 0.0
         if (ch(jrch)%rch_cbod < 1.e-6) ch(jrch)%rch_cbod = 0.0
         if (ch(jrch)%rch_dox < 1.e-6) ch(jrch)%rch_dox = 0.0
         wtrtot = wtrin + rchwtr
         algcon =(algin * wtrin + ch(jrch)%algae * rchwtr) / wtrtot
         orgncon = (orgnin * wtrin + ch(jrch)%organicn * rchwtr) /wtrtot
         nh3con = (ammoin * wtrin + ch(jrch)%ammonian * rchwtr) / wtrtot
         no2con = (nitritin * wtrin + ch(jrch)%nitriten  * rchwtr) / wtrtot
         no3con = (nitratin * wtrin + ch(jrch)%nitraten * rchwtr) / wtrtot
         orgpcon =(orgpin * wtrin + ch(jrch)%organicp * rchwtr) / wtrtot
         solpcon = (dispin * wtrin + ch(jrch)%disolvp * rchwtr) / wtrtot
         cbodcon= (cbodin * wtrin + ch(jrch)%rch_cbod * rchwtr) / wtrtot
         o2con = (disoxin * wtrin + ch(jrch)%rch_dox * rchwtr) / wtrtot

         !! calculate algal biomass concentration at end of day
         ch(jrch)%algae = 0.
         ch(jrch)%algae = algcon
         if (ch(jrch)%algae < 1.e-6) ch(jrch)%algae = 0.

         !! calculate chlorophyll-a concentration at end of day
         ch(jrch)%chlora = 0.
         ch(jrch)%chlora = ch(jrch)%algae * ch_nut(jnut)%ai0 / 1000.

!! oxygen calculations
         !! calculate carbonaceous biological oxygen demand at end
         !! of day 
         ch(jrch)%rch_cbod = 0.
         ch(jrch)%rch_cbod = cbodcon
         if (ch(jrch)%rch_cbod < 1.e-6) ch(jrch)%rch_cbod = 0.

         !! calculate dissolved oxygen concentration if reach at 
         !! end of day
         ch(jrch)%rch_dox = 0.
         ch(jrch)%rch_dox = o2con
         if (ch(jrch)%rch_dox < 1.e-6) ch(jrch)%rch_dox = 0.
!! end oxygen calculations

!! nitrogen calculations
        !! calculate organic N concentration at end of day
        ch(jrch)%organicn = 0.
        ch(jrch)%organicn = orgncon
        if (ch(jrch)%organicn < 1.e-6) ch(jrch)%organicn = 0.

        !! calculate ammonia nitrogen concentration at end of day
        ch(jrch)%ammonian = 0.
        ch(jrch)%ammonian = nh3con
        if (ch(jrch)%ammonian < 1.e-6) ch(jrch)%ammonian = 0.

        !! calculate concentration of nitrite at end of day
        ch(jrch)%nitriten = 0.
        ch(jrch)%nitriten = no2con
        if (ch(jrch)%nitriten < 1.e-6) ch(jrch)%nitriten = 0.

        !! calculate nitrate concentration at end of day
        ch(jrch)%nitraten = 0.
        ch(jrch)%nitraten = no3con
        if (ch(jrch)%nitraten < 1.e-6) ch(jrch)%nitraten = 0.
!! end nitrogen calculations

!! phosphorus calculations
        !! calculate organic phosphorus concentration at end of
        !! day
        ch(jrch)%organicp = 0.
        ch(jrch)%organicp = orgpcon
        if (ch(jrch)%organicp < 1.e-6) ch(jrch)%organicp = 0.

        !! calculate dissolved phosphorus concentration at end
        !! of day (mineral P)
        ch(jrch)%disolvp = 0.
        ch(jrch)%disolvp = solpcon
        if (ch(jrch)%disolvp < 1.e-6) ch(jrch)%disolvp = 0.
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
      endif

      return
      end subroutine ch_noqual