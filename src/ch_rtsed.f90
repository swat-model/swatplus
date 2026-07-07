      subroutine ch_rtsed
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine routes sediment from subbasin to basin outlets
!!    deposition is based on fall velocity and degradation on stream

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!                               |1 no vegetative cover on channel
!!    ch_d(:)     |m             |average depth of main channel
!!    ch_li(:)    |km            |initial length of main channel
!!    ch_n(2,:)   |none          |Manning"s "n" value for the main channel
!!    ch_s(2,:)   |m/m           |average slope of main channel
!!    ch_si(:)    |m/m           |initial slope of main channel
!!    ch_wdr(:)   |m/m           |channel width to depth ratio
!!    rchdep      |m             |depth of flow on day
!!    sdti        |m^3/s         |average flow on day in reach
!!    sedst(:)    |metric tons   |amount of sediment stored in reach
!!                               |reentrained in channel sediment routing
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ch_d(:)     |m             |average depth of main channel
!!    ch_s(2,:)   |m/m           |average slope of main channel
!!    peakr       |m^3/s         |peak runoff rate in channel
!!    sedst(:)    |metric tons   |amount of sediment stored in reach
!!    sedrch      |metric tons   |sediment transported out of channel
!!                               |during time step
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dat2        |m             |change in channel depth during time step
!!    deg         |metric tons   |sediment reentrained in water by channel
!!                               |degradation
!!    dep         |metric tons   |sediment deposited on river bottom
!!    depdeg      |m             |depth of degradation/deposition from original
!!    depnet      |metric tons   |
!!    dot         |
!!    jrch        |none          |reach number
!!    qdin        |m^3 H2O       |water in reach during time step
!!    vc          |m/s           |flow velocity in reach
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max
!!    SWAT: ttcoef

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module
      use channel_data_module
      use channel_module
      use hydrograph_module, only : ob, jrch, icmd
      use time_module
             
      implicit none

      real :: qdin             !m^3 H2O       |water in reach during time step
      real :: sedin            !units         |description
      real :: vc               !m/s           |flow velocity in reach
      real :: cyin             !units         |description
      real :: cych             !units         |description
      real :: depnet           !metric tons   |
      real :: deg1             !units         |description
      real :: deg2             !units         |description
      real :: dep              !metric tons   |sediment deposited on river bottom
      real :: depdeg           !m             |depth of degradation/deposition from original
      real :: dot              !mm            |actual depth from impermeable layer to water level
                               !              |above drain during subsurface irrigation
      real :: outfract         !units         |description
      real :: deg              !metric tons   |sediment reentrained in water by channel
                               !              |degradation
      real :: sedinorg         !units         |description
      real :: tbase            !none          |flow duration (fraction of 1 hr)
      real :: dat2             !m             |change in channel depth during time step

      sedin = 0.0

      if (rtwtr > 0. .and. rchdep > 0.) then

!! initialize water in reach during time step
      qdin = 0.
      qdin = rtwtr + ch(jrch)%rchstor

!! do not perform sediment routing if no water in reach
      if (qdin > 0.01) then

!! initialize sediment in reach during time step
      sedin = 0.
      sedin = ob(icmd)%hin%sed  + ch(jrch)%sedst
      sedinorg = sedin
!! initialize reach peak runoff rate
      peakr = sdti

!! calculate flow velocity
      vc = 0.
      if (rchdep < .010) then
        vc = 0.01
      else
        vc = peakr / (rcharea + .1)  !dont merge
      end if
      if (vc > 5.) vc = 5.

      tbase = 0.
      tbase = ch_hyd(jhyd)%l * 1000. / (3600. * 24. * vc + .1)  !dont merge

      if (tbase > 1.) tbase = 1.


!! JIMMY"S NEW IMPROVED METHOD for sediment transport
      cyin = 0.
      cych = 0.
      depnet = 0.
	  deg = 0.
      deg1 = 0.
	  deg2 = 0.
      dep = 0.
      cyin = sedin / qdin
      cych = bsn_prm%spcon * vc ** bsn_prm%spexp
      depnet = qdin * (cych - cyin)
	if(abs(depnet) < 1.e-6) depnet = 0.
      if (vc < 0.) depnet = 0.

!!  tbase is multiplied so that erosion is proportional to the traveltime, 
!!  which is directly related to the length of the channel
!!  Otherwise for the same discharge rate and sediment deficit
!!  the model will erode more sediment per unit length of channel 
!!  from a small channel than a larger channel. Modification made by Balaji Narasimhan

      if (depnet > 1.e-6) then
        deg = depnet * tbase
	  !! First the deposited material will be degraded before channel bed
	  if (deg >= ch(jrch)%depch) then
	    deg1 = ch(jrch)%depch
        deg2 = (deg - deg1) * ch_sed(jsed)%erod(time%mo) * ch_sed(jsed)%cov2
	  else
	    deg1 = deg
	    deg2 = 0.
	  endif
        dep = 0.
      else
        dep = -depnet * tbase
        deg = 0.
	  deg1 = 0.
	  deg2 = 0.
      endif

	ch(jrch)%depch = ch(jrch)%depch + dep - deg1
      if (ch(jrch)%depch < 1.e-6) ch(jrch)%depch = 0.

	sedin = sedin + deg1 + deg2 - dep
	if (sedin < 1.e-6) sedin = 0.

	outfract = rtwtr / qdin
	if (outfract > 1.) outfract = 1.

      sedrch = sedin * outfract
      if (sedrch < 1.e-6) sedrch = 0.

      ch(jrch)%sedst = sedin - sedrch
      if (ch(jrch)%sedst < 1.e-6) ch(jrch)%sedst = 0.

!!    Mass balance tests
!!	ambalsed = sedinorg + deg1 + deg2 - dep - sedrch - sedst(jrch)
!!	if (ambalsed .gt. 1e-3) write (*,*) time%day, jrch, ambalsed

!!  In this default sediment routing sediment is not tracked by particle size
      rch_san = 0.
      rch_sil = sedrch  !! As particles are not tracked by size, the sediments 
      rch_cla = 0.      !! in reach is assumed to be silt for mass conservation
      rch_sag = 0.
      rch_lag = 0.
	rch_gra = 0.

!!    Organic nitrogen and Organic Phosphorus contribution from channel erosion
!!    ch_orgn(jrch) = deg2 * ch_nut(jnut)%onco * 1000.
!!    ch_orgp(jrch) = deg2 * ch_nut(jnut)%opco * 1000.

      ch(jrch)%orgn = deg2 * ch_nut(jnut)%onco / 1000.
      ch(jrch)%orgp = deg2 * ch_nut(jnut)%opco / 1000.

!! compute changes in channel dimensions
      if (bsn_cc%deg == 1) then
        depdeg = 0.
        depdeg = ch_hyd(jhyd)%d - ch(jrch)%di
        if (depdeg < ch(jrch)%si * ch(jrch)%li * 1000.) then
          if (qdin > 1400000.) then
            dot = 0.
            dot = 358.6 * rchdep * ch_hyd(jhyd)%s * ch_sed(jsed)%cov1
            dat2 = 1.
            dat2 =  dat2 * dot
            ch_hyd(jhyd)%d = ch_hyd(jhyd)%d + dat2
            ch_hyd(jhyd)%w = ch_hyd(jhyd)%wdr * ch_hyd(jhyd)%d
            ch_hyd(jhyd)%s = ch_hyd(jhyd)%s - dat2 / (ch_hyd(jhyd)%l  *     &
                                                           1000.)
            ch_hyd(jhyd)%s = Max(.0001, ch_hyd(jhyd)%s)
            call ch_ttcoef(jrch)
          endif
        endif
      endif

	else
	  sedrch = 0.
	  rch_san = 0.
	  rch_sil = 0.
	  rch_cla = 0.
	  rch_sag = 0.
	  rch_lag = 0.
	  rch_gra = 0.
        ch(jrch)%sedst = sedin

	endif !! end of qdin > 0.01 loop

      endif  !! end of rtwtr and rchdep > 0 loop

      return
      end subroutine ch_rtsed