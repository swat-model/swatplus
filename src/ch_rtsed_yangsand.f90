      subroutine ch_rtsed_yangsand
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine routes sediment from subbasin to basin outlets
!!    deposition is based on fall velocity and degradation on stream

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
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
!!	Modification to the original SWAT sediment routine
!!	By Balaji Narasimhan and Peter Allen
!!    Yang"s Sand and gravel equation combined with Einsteins deposition equation
!!    Plus particle size tracking.

      use basin_module
      use channel_data_module
      use channel_module
      use channel_velocity_module
      use hydrograph_module, only : ob
      
      implicit none

      integer :: ch_d50type          !units         |description
      integer :: jrch                !none          |reach number
      integer :: icmd                !units         |description
      real :: qdin                   !m^3 H2O       |water in reach during time step
      real :: sedin                  !units         |description
      real :: vc                     !m/s           |flow velocity in reach         
      real :: cyin                   !units         |description
      real :: cych                   !units         |description
      real :: depnet                 !metric tons   |
      real :: deg                    !metric tons   |sediment reentrained in water by channel
                                     !              |degradation 
      real :: dep                    !metric tons   |sediment deposited on river bottom
      real :: tbase                  !units         |description
      real :: depdeg                 !m             |depth of degradation/deposition from original
      real :: dot                    !mm            |actual depth from impermeable layer to water level
                                     !              |above drain during subsurface irrigation
      real :: vs                     !units         |description
      real :: x                      !units         |description
      real :: SC                     !units         |description
      real :: Tcbnk                  !units         |description
      real :: Tcbed                  !units         |description
      real :: Tbank                  !units         |description
      real :: Tbed                   !units         |description
      real :: asinea                 !units         |description
      real :: Tou                    !units         |description
      real :: sanin                  !units         |description
      real :: silin                  !units         |description
      real :: clain                  !units         |description
      real :: sagin                  !units         |description
      real :: lagin                  !units         |description
      real :: grain                  !units         |description
      real :: outfract               !units         |description
      real :: depsan                 !units         |description
      real :: depsil                 !units         |description
      real :: depcla                 !units         |description
      real :: depsag                 !units         |description
      real :: deplag                 !units         |description 
      real :: depgra                 !units         |description
      real :: degsan                 !units         |description
      real :: degsil                 !units         |description
      real :: degcla                 !units         |description
      real :: deggra                 !units         |description
      real :: bnksan                 !units         |description
      real :: bnksil                 !units         |description
      real :: bnkcla                 !units         |description
      real :: bnkgra                 !units         |description
      real :: pdep                   !units         |description
      real :: pdepbed                !units         |description
      real :: bedsize                !units         |description
      real :: USpower                !units         |description
      real :: adddep                 !units         |description
      real :: fpratio                !units         |description
      real :: watdep                 !units         |description
      real :: bnkrt                  !units         |description
      real :: bedrt                  !units         |description
      real :: effbnkbed              !units         |description
      real :: sedinorg               !units         |description
      real :: deg1                   !units         |description
      real :: deg1san                !units         |description
      real :: deg1sil                !units         |description
      real :: deg1cla                !units         |description
      real :: deg1sag                !units         |description
      real :: deg1lag                !units         |description
      real :: deg1gra                !units         |description
      real :: degremain              !units         |description
      real :: c                      !units         |description 
      real :: pbed                   !units         |description
      real :: pbank                  !units         |description
      real :: rh                     !m             |hydraulic radius
      real :: topw                   !m             |top width of main channel
      real :: sfbank                 !units         |description
      real :: vsh                    !units         |description
      real :: w50                    !units         |description
      real :: var1                   !units         |description
      real  :: var2                  !units         |description
      real  :: var3                  !units         |description
      real  :: var4                  !units         |description
      real  :: var5                  !units         |description 
      real  :: var6                  !units         |description 
      real  :: var56                 !units         |description 
      real :: cychppm                !units         |description
      real :: cychw                  !units         |description
      real :: cychv                  !units         |description
      real :: vgra                   !units         |description
      real :: vsan                   !units         |description
      real :: vsil                   !units         |description
      real :: vcla                   !units         |description
      real :: vsag                   !units         |description
      real :: vlag                   !units         |description
      real :: dat2                   !m             |change in channel depth during time step 
      real :: alog10cychppm          !units         |description
      
      
      if (rtwtr > 0. .and. rchdep > 0.) then

!! initialize water in reach during time step
      qdin = 0.
      qdin = rtwtr + ch(jrch)%rchstor

!! initialize sediment in reach during time step
      sedin = 0.
	sanin = 0.
	silin = 0.
	clain = 0.
	sagin = 0.
	lagin = 0.
      sedin = ob(icmd)%hin%sed  + ch(jrch)%sedst
      sanin = ob(icmd)%hin%sed  + ch(jrch)%sanst
      silin = ob(icmd)%hin%sil  + ch(jrch)%silst
      clain = ob(icmd)%hin%cla  + ch(jrch)%clast
      sagin = ob(icmd)%hin%sag  + ch(jrch)%sagst
      lagin = ob(icmd)%hin%lag  + ch(jrch)%lagst
	grain = ob(icmd)%hin%grv  + ch(jrch)%grast
      sedinorg = sedin

!! do not perform sediment routing if no water in reach
      if (qdin > 0.01) then

!! initialize reach peak runoff rate
      peakr = 1. * sdti

!! calculate peak flow velocity
      vc = 0.
      if (rcharea < .010) then
        vc = 0.01
      else
        vc = peakr / rcharea
      end if
      
	if (vc > 5.) vc = 5.

      tbase = 0.
      tbase = ch_hyd(jhyd)%l * 1000. / (3600. * 24. * vc)
      if (tbase > 1.) tbase = 1.

!! JIMMY"S NEW IMPROVED METHOD for sediment transport
      cyin = 0.
      cych = 0.
      depnet = 0.
      deg = 0.

	deg1 = 0.
	deg1san = 0.
	deg1sil = 0.
	deg1cla = 0.
	deg1sag = 0.
	deg1lag = 0.
	deg1gra = 0.

	degrte = 0.
	degremain = 0.
	deggra = 0.
      degsan = 0.
      degsil = 0.
      degcla = 0.
      bnksan = 0.
      bnksil = 0.
      bnkcla = 0.
	bnkgra = 0.
	bnkrte = 0.
      dep = 0.
      depsan = 0.
      depsil = 0.
      depcla = 0.
      depsag = 0.
      deplag = 0.
	depgra = 0.
	watdep = 0.
	bnkrt = 0.
	bedrt = 0.
	effbnkbed = 0.

      c = ch_hyd(jhyd)%side
	  pbed = ch_vel(jrch)%wid_btm
      pbank = 2. * rchdep * Sqrt(1. + c * c)
      rh = rcharea / (pbed + pbank)

      topw = 0.
      if (rchdep <= ch_hyd(jhyd)%d) then
        topw = ch_vel(jrch)%wid_btm + 2. * rchdep * c
	  fpratio = 0.
	  watdep = rchdep
      else
        topw = 5 * ch_hyd(jhyd)%w + 2. * (rchdep - ch_hyd(jhyd)%d) * 4.
	  adddep = rchdep - ch_hyd(jhyd)%d
	  !! Area Ratio of water in flood plain to total cross sectional area
        fpratio = (rcharea - ch_vel(jrch)%area - ch_hyd(jhyd)%w           &     
                                                    *adddep)/rcharea
	  fpratio = max(0.,fpratio)
	  watdep = ch_hyd(jhyd)%d
      end if

!!	Applied Bank Shear Stress
!!    Equations from Eaton and Millar (2004)
	SFbank = 10**(-1.4026 * log10((pbed/pbank) + 1.5) + 2.247)

	Tou = 9800. * rchdep * ch_hyd(jhyd)%s

	asinea = 1. / sqrt((1.**2) + (c**2))

      Tbank = Tou * (SFbank/100.) * (topw + pbed) * asinea/ (4.*rchdep)

      Tbed  = Tou * (1. - (SFbank/100.)) * (topw/(2.*pbed) + 0.5)

!!    Potential Bank Erosion rate in metric tons per day
!!    Assumed on an average Only one bank eroding due to meandering of channel
      bnkrte = ch_sed(jsed)%bnk_kd * (Tbank - ch_sed(jsed)%tc_bnk)*1e-06
	if (bnkrte < 0.) bnkrte = 0.
      bnkrte = bnkrte * ch_hyd(jhyd)%l * 1000.* (watdep *               &              
                  Sqrt(1. + c * c)) * ch_sed(jsed)%bnk_bd * 86400.

!!    Potential Bed degradation rate in metric tons per day
      degrte = ch_sed(jsed)%bed_kd * (Tbed - ch_sed(jsed)%tc_bed)*1e-06
      if (degrte < 0.) degrte = 0.
      degrte = degrte * ch_hyd(jhyd)%l * 1000.* ch_vel(jrch)%wid_btm    &   
                                      * ch_sed(jsed)%bed_bd * 86400.

!!    Relative potential for bank/bed erosion
      if (bnkrte + degrte > 1.e-6) then
	  bnkrt = bnkrte / (bnkrte + degrte)
      else
	  bnkrt = 1.0
      end if
	bnkrt = Min(1.0, bnkrt)
!!    Relative potential for bed erosion
      bedrt = 1. - bnkrt

!!    Incoming sediment concentration
      cyin = sedin/qdin

!!    Shear velocity
	vsh = sqrt(9.81 * rh * ch_hyd(jhyd)%s)

	var1 = vsh * ch_sed(jsed)%bed_d50 * 1e-006 / 1.16e-6

	if (var1 < 70) then
	  if (var1 < 1.2) var1 = 1.2
	  var2 = 2.5/(log10(var1) - 0.06)
	end if

	if (var1 >= 70) then
	  var2 = 2.05
	end if

!!    Settling velocity for ch_d50(jrch) particle size
	w50 = 411.0 * ((ch_sed(jsed)%bed_d50/1000.)**2.) / (3600.)

	var3 = w50 * ch_sed(jsed)%bed_d50 * 1e-006 / 1.16e-6

	var4 = vsh / w50

	var5 = vc * ch_hyd(jhyd)%s / w50

	var6 = var2 * ch_hyd(jhyd)%s

	var56 = var5 - var6

!!    This needs to be checked for accuracy
	if (var56 <= 0.) var56 = 1e-06

      bedsize = ch_sed(jsed)%bed_d50/1000.
      if (bedsize <= 2.) then  
      !!  Yangs sand equation for particles less than 2 mm (2000 mircometer)
        alog10cychppm = 5.435 - 0.286*log10(var3) - 0.457*log10(var4)      &   
                   +(1.799 - 0.409*log10(var3) - 0.314*log10(var4))       &    
     &              *log10(var56)
	end if

	if (bedsize > 2.) then
      !!  Yangs gravel equation for particles between 2mm and 10mm
        alog10cychppm = 6.681 - 0.633*log10(var3) - 4.816*log10(var4)    &  
                   +(2.784 - 0.305*log10(var3) - 0.282*log10(var4))      &
                    *log10(var56)
	end if

!!    Concentration in ppm by weight
      cychppm = 10**alog10cychppm

!!    Concentration by weight
	cychw = cychppm / 1e+06

!!    Concentration by volume
	cychv = (cychw / (cychw + (1. - cychw)*2.65))

!!    concentration in metric Tons / m^3
	cych =  cychv * 2.65

!!    Potential sediment Transport capacity
      depnet = qdin * (cych - cyin)

      if (depnet .LE. 1.e-6) then
        depnet = 0.
	  bnkrte = 0.
	  degrte = 0.
	else
	  !! First the deposited material will be degraded before channel bed or bank erosion
	  if (depnet >= ch(jrch)%depch) then
	    !! Effective erosion
          effbnkbed = depnet - ch(jrch)%depch
          !! Effective bank erosion
          if (effbnkbed*bnkrt <= bnkrte) bnkrte = effbnkbed*bnkrt
          bnksan = bnkrte * ch(jrch)%bnk_san
          bnksil = bnkrte * ch(jrch)%bnk_sil
          bnkcla = bnkrte * ch(jrch)%bnk_cla
	    bnkgra = bnkrte * ch(jrch)%bnk_gra

          !! Effective bed erosion
          if (effbnkbed*bedrt <= degrte) degrte = effbnkbed*bedrt
          degsan = degrte * ch(jrch)%bed_san
          degsil = degrte * ch(jrch)%bed_sil
          degcla = degrte * ch(jrch)%bed_cla
	    deggra = degrte * ch(jrch)%bed_gra

          deg1 = ch(jrch)%depch
          deg1san = ch(jrch)%depsanch
          deg1sil = ch(jrch)%depsilch
          deg1cla = ch(jrch)%depclach
	    deg1sag = ch(jrch)%depsagch
	    deg1lag = ch(jrch)%deplagch
	    deg1gra = ch(jrch)%depgrach

	    ch(jrch)%depch = 0.
	    ch(jrch)%depsanch = 0.
	    ch(jrch)%depsilch = 0.
	    ch(jrch)%depclach = 0.
	    ch(jrch)%depsagch = 0.
	    ch(jrch)%deplagch = 0.
	    ch(jrch)%depgrach = 0.

	  else

	    bnkrte = 0.
	    degrte = 0.
          degsan = 0.
          degsil = 0.
          degcla = 0.
	    deggra = 0.
          bnksan = 0.
          bnksil = 0.
          bnkcla = 0.
	    bnkgra = 0.

	    ch(jrch)%depch = ch(jrch)%depch - depnet
          deg1 = depnet

  	    if (ch(jrch)%depclach >= depnet) then
	      ch(jrch)%depclach = ch(jrch)%depclach - depnet
	      deg1cla = depnet
	      degremain = 0.
	    else
	      degremain = depnet - ch(jrch)%depclach
	      deg1cla = ch(jrch)%depclach
	      ch(jrch)%depclach = 0.
	      if (ch(jrch)%depsilch >= degremain) then
	        ch(jrch)%depsilch = ch(jrch)%depsilch - degremain
	        deg1sil = degremain
	        degremain = 0.
	      else
	        degremain = degremain - ch(jrch)%depsilch
	        deg1sil = ch(jrch)%depsilch
	        ch(jrch)%depsilch = 0.
	        if (ch(jrch)%depsagch >= degremain) then
	          ch(jrch)%depsagch = ch(jrch)%depsagch - degremain
	          deg1sag = degremain
	          degremain = 0.
	        else
	          degremain = degremain - ch(jrch)%depsagch
	          deg1sag = ch(jrch)%depsagch
	          ch(jrch)%depsagch = 0.
	          if (ch(jrch)%depsanch >= degremain) then
	            ch(jrch)%depsanch = ch(jrch)%depsanch - degremain
	            deg1san = degremain
	            degremain = 0.
	          else
	            degremain = degremain - ch(jrch)%depsanch
	            deg1san = ch(jrch)%depsanch
	            ch(jrch)%depsanch = 0.
	            if (ch(jrch)%deplagch >= degremain) then
	              ch(jrch)%deplagch = ch(jrch)%deplagch - degremain
	              deg1lag = degremain
	              degremain = 0.
	            else
	              degremain = degremain - ch(jrch)%deplagch
	              deg1lag = ch(jrch)%deplagch
	              ch(jrch)%deplagch = 0.
	              if (ch(jrch)%depgrach >= degremain) then
	                ch(jrch)%depgrach = ch(jrch)%depgrach - degremain
	                deg1gra = degremain
					degremain = 0.
	              else
	                degremain = degremain - ch(jrch)%depgrach
	                deg1gra = ch(jrch)%depgrach
	                ch(jrch)%depgrach = 0.
	              endif
	            endif
	          endif
	        endif
 	      endif
	    endif

	  endif

      end if

      if (ch(jrch)%depch < 1.e-6) then
	  ch(jrch)%depch = 0.
        ch(jrch)%depsanch = 0.
        ch(jrch)%depsilch = 0.
        ch(jrch)%depclach = 0.
        ch(jrch)%depsagch = 0.
        ch(jrch)%deplagch = 0.
        ch(jrch)%depgrach = 0.
	end if

!!	Fall velocity Based on equation 1.36 from SWRRB manual
        vgra = 411.0 * ((2.00)**2.) / (3600.)
	  vsan = 411.0 * ((0.20)**2.) / (3600.)
	  vsil = 411.0 * ((0.01)**2.) / (3600.)
	  vcla = 411.0 * ((0.002)**2.) / (3600.)
	  vsag = 411.0 * ((0.03)**2.) / (3600.)
	  vlag = 411.0 * ((0.50)**2.) / (3600.)

!!	Deposition calculated based on Einstein Equation
        x = 0.

!!	Gravel deposition
	  x = 1.055 * 1000. * ch_hyd(jhyd)%l * vgra / (vc * rchdep)
        if (x > 20.) x = 20.
	  pdep = min((1. - exp(-x)), 1.)
        depgra = grain * pdep

!!	sand deposition
	  x = 1.055 * 1000. * ch_hyd(jhyd)%l * vsan / (vc * rchdep)
        if (x > 20.) x = 20.
	  pdep = min((1. - exp(-x)), 1.)
        depsan = sanin * pdep

!!	Silt deposition
	  x = 1.055 * 1000. * ch_hyd(jhyd)%l * vsil / (vc * rchdep)
        if (x > 20.) x = 20.
	  pdep = min((1. - exp(-x)), 1.)

	  depsil = silin * pdep

!!	Clay deposition
	  x = 1.055 * 1000. * ch_hyd(jhyd)%l * vcla / (vc * rchdep)
        if (x > 20.) x = 20.
	  pdep = min((1. - exp(-x)), 1.)

	  depcla = clain * pdep

!!	Small aggregates deposition
	  x = 1.055 * 1000. * ch_hyd(jhyd)%l * vsag / (vc * rchdep)
        if (x > 20.) x = 20.
	  pdep = min((1. - exp(-x)), 1.)

	  depsag = sagin * pdep

!!	Large aggregates deposition
	  x = 1.055 * 1000. * ch_hyd(jhyd)%l * vlag / (vc * rchdep)
        if (x > 20.) x = 20.
	  pdep = min((1. - exp(-x)), 1.)

	  deplag = lagin * pdep

	  dep = depsan + depsil + depcla + depsag + deplag + depgra

!!    Particles deposited on Floodplain (only silt and clay type particles)
        ch(jrch)%depfp    = ch(jrch)%depfp + (depsil + depcla) * fpratio
	  ch(jrch)%depsilfp = ch(jrch)%depsilfp + depsil * fpratio
	  ch(jrch)%depclafp = ch(jrch)%depclafp + depcla * fpratio

!!    Remaining is deposited in the channel
        ch(jrch)%depch =ch(jrch)%depch + dep - (depsil + depcla)*fpratio
        ch(jrch)%depsilch = ch(jrch)%depsilch + depsil * (1. - fpratio)
        ch(jrch)%depclach = ch(jrch)%depclach + depcla * (1. - fpratio)
        ch(jrch)%depsanch = ch(jrch)%depsanch + depsan
        ch(jrch)%depsagch = ch(jrch)%depsagch + depsag
        ch(jrch)%deplagch = ch(jrch)%deplagch + deplag
        ch(jrch)%depgrach = ch(jrch)%depgrach + depgra

      sedin  = sedin + degrte + bnkrte + deg1    - dep
	grain  = grain + deggra + bnkgra + deg1gra - depgra
	sanin  = sanin + degsan + bnksan + deg1san - depsan
	silin  = silin + degsil + bnksil + deg1sil - depsil
	clain  = clain + degcla + bnkcla + deg1cla - depcla
	sagin  = sagin + deg1sag - depsag
	lagin  = lagin + deg1lag - deplag

      if (sedin  < 1.e-6) then
	  sedin = 0.
	  sanin = 0.
        silin = 0.
        clain = 0.
        sagin = 0.
        lagin = 0.
        grain = 0.
	end if

	outfract = rtwtr / qdin
	if (outfract > 1.) outfract = 1.

      sedrch =  sedin * outfract
      rch_san = sanin * outfract
      rch_sil = silin * outfract
      rch_cla = clain * outfract
      rch_sag = sagin * outfract
      rch_lag = lagin * outfract
      rch_gra = grain * outfract

      if (sedrch  < 1.e-6) then
	  sedrch = 0.
	  rch_san = 0.
        rch_sil = 0.
        rch_cla = 0.
        rch_sag = 0.
        rch_lag = 0.
        rch_gra = 0.
	end if

      ch(jrch)%sedst = sedin - sedrch
      ch(jrch)%sanst = sanin - rch_san
      ch(jrch)%silst = silin - rch_sil
      ch(jrch)%clast = clain - rch_cla
      ch(jrch)%sagst = sagin - rch_sag
      ch(jrch)%lagst = lagin - rch_lag
      ch(jrch)%grast = grain - rch_gra

      if (ch(jrch)%sedst < 1.e-6) then
	  ch(jrch)%sedst = 0.
        ch(jrch)%sanst = 0.
        ch(jrch)%silst = 0.
        ch(jrch)%clast = 0.
        ch(jrch)%sagst = 0.
        ch(jrch)%lagst = 0.
        ch(jrch)%grast = 0.
	end if

!!    Mass balance tests
!!      ambalsed = sedinorg + degrte + bnkrte + deg1 - dep - sedrch       
!!     &            - sedst(jrch))
!!      ambalsed = depch(jrch) - depsanch(jrch)-depsilch(jrch)            
!!     &-ch(jrch)%depclach-depsagch(jrch)-deplagch(jrch)-depgrach(jrch)
!!      ambalsed = sedst(jrch) - sanst(jrch)-silst(jrch)-clast(jrch)      
!!     &-sagst(jrch)-lagst(jrch)-grast(jrch)
!!      ambalsed = (sedin-sanin-silin-clain-sagin-lagin-grain)/sedin
!!      ambalsed = sedrch-rch_san-rch_sil-rch_cla-rch_sag-rch_lag-rch_gra
!!      if (abs(ambalsed) .gt. 1e-3) write (*,*) time%day,jrch,ambalsed,sedrch

!!    Deposition during the previous time step
      ch(jrch)%depprch = ch(jrch)%depch  !! Channel
	ch(jrch)%depprfp = ch(jrch)%depfp  !! Flood plain

!!    Organic nitrogen and Organic Phosphorus contribution from channel erosion
!!    Only bank erosion is assumed to contribute to channel erosion
   !!     ch_orgn(jrch) = bnkrte * ch_nut(jnut)%onco * 1000.
   !!     ch_orgp(jrch) = bnkrte * ch_nut(jnut)%opco * 1000.
        ch(jrch)%orgn = bnkrte * ch_nut(jnut)%onco / 1000.
        ch(jrch)%orgp = bnkrte * ch_nut(jnut)%opco / 1000.

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
            ch_hyd(jhyd)%s = ch_hyd(jhyd)%s - dat2 / (ch_hyd(jhyd)%l *     &
                                                     1000.)
            ch_hyd(jhyd)%s = Max(.0001, ch_hyd(jhyd)%s)
            call ch_ttcoef(jrch)
          endif
        endif
      endif

	else

        ch(jrch)%sedst = sedin
        ch(jrch)%sanst = sanin
        ch(jrch)%silst = silin
        ch(jrch)%clast = clain
        ch(jrch)%sagst = sagin
        ch(jrch)%lagst = lagin
        ch(jrch)%grast = grain

	end if !! end of qdin > 0.01 loop

      end if  !! end of rtwtr and rchdep > 0 loop

      return
      end subroutine ch_rtsed_yangsand