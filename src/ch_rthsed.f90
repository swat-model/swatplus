      subroutine ch_rthsed
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine routes sediment from subbasin to basin outlets
!!    on a sub-daily timestep

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
!!    hdepth(:)   |m             |depth of flow on day
!!    hhstor(:)   |m^3 H2O       |water stored in reach at end of time step
!!    hrtwtr(:)   |m^3 H2O       |water leaving reach during time step
!!    hsdti(:)    |m^3/s         |average flow rate during time step
!!    rhy(:)      |m H2O         |main channel hydraulic radius
!!    sedst(:)    |metric tons   |amount of sediment stored in reach
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ch_d(:)     |m             |average depth of main channel
!!    ch_s(2,:)   |m/m           |average slope of main channel
!!    peakr       |m^3/s         |peak runoff rate in channel
!!    sedst(:)    |metric tons   |amount of sediment stored in reach
!!    sedrch      |metric tons   |sediment transported out of channel on day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dat2        |m             |change in channel depth during time step
!!    deg         |metric tons   |sediment reentrained in water by channel degradation
!!    dep         |metric tons   |sediment deposited on river bottom
!!    depdeg      |m             |depth of degradation/deposition from original
!!    depnet      |metric tons   |
!!    dot         |
!!    ii          |none          |counter
!!    jrch        |none          |reach number
!!    qin         |m^3 H2O       |water in reach during time step
!!    vc          |m/s           |flow velocity in reach
!!    sedcon      |mg/L          |sediment concentration
!!    shrstrss    |none          |critical shear stress for bed erosion
!!    Reynolds_g  |none          |grain Reynolds number 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max
!!    SWAT: ttcoef

!!    code modified by J.Jeong and N.Kannan for urban sub-hourly sediment modeling
!!    and by Balagi for bank erosion.
!!    Brownlie (1981) bed load model and Yang (1973, 1984) model added. 

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

    use basin_module
    use channel_data_module
    use time_module
    use channel_module
    use hydrograph_module, only : ob
    use climate_module
      
    implicit none

	integer :: ii                         !none          |counter
    integer :: icmd                       !units         |description
    integer :: jrch                       !none          |reach number
    integer :: iwst                       !none          |counter
	real :: qin                           !m^3 H2O       |water in reach during time step
    real :: qdin                          !m^3 H2O       |water in reach during time step
    real :: sedin                         !units         |description
    real :: vc                            !m/s           |flow velocity in reach
    real :: cyin                          !units         |description
    real :: cych                          !units         |description
    real :: depnet                        !metric tons   |
    real :: deg                           !metric tons   |sediment reentrained in water by channel degradation 
    real :: dep                           !metric tons   |sediment deposited on river bottom
	real :: depdeg                        !m             |depth of degradation/deposition from original
    real :: dot                           !mm            |actual depth from impermeable layer to water level
                                          !              |above drain during subsurface irrigation
    real :: ycoeff                        !units         |description
    real :: Reynolds_g                    !none          |grain Reynolds number 
    real :: visco_h2o                     !units         |description
    real :: tmpw                          !units         |description
	real :: channel_d50                   !units         |description
    real :: particle_specific_gravity     !units         |description
    real :: Fr_g                          !units         |description                     
    real :: Fr_gc                         !units         |description
	real :: log10sedcon                   !units         |description
    real :: sedcon                        !g/m^3         |sediment concentration
    real :: deg24                         !units         |description
    real :: dep24                         !units         |description
	real :: vfall                         !units         |description
    real :: coefa                         !units         |description
    real :: coefb                         !units         |description
    real :: coefc                         !units         |description
    real :: coefd                         !units         |description
    real :: coefe                         !units         |description
    real :: thbase                        !units         |description
    real :: shear_stress                  !units         |description
    real :: vshear                        !units         |description
    real :: deg1                          !units         |description 
    real :: deg2                          !units         |description
    real :: d_fract                       !units         |description
    real :: dat2                          !m             |change in channel depth during time step

	deg24=0.; dep24=0
	channel_d50 = bsn_prm%ch_d50 / 1000. !! unit change mm->m
	particle_specific_gravity = 2.65
	sedin = 0.

	do ii = 1, time%step

	if (hrtwtr(ii)>0. .and. hdepth(ii)>0.) then

	 !! initialize water in reach during time step
	 qin = 0.
       sedin = 0.
	 qin = hrtwtr(ii) + hhstor(ii)

	 !! do not perform sediment routing if no water in reach
	 if (qin > 0.01) then

	   !! initialize sediment in reach during time step
	   if (ii == 1) then
           sedin = ob(icmd)%ts(1,ii)%sed  + ch(jrch)%sedst
	   else
		 sedin = ob(icmd)%ts(1,ii)%sed  + hsedst(ii-1)
	   end if

       if (sedin < 1.e-6) sedin = 0.
       !! initialize reach peak runoff rate
       peakr = hsdti(ii)

       !! calculate flow velocity
       vc = 0.
       if (hharea(ii) < .010) then
         vc = 0.01
       else
         vc = peakr / hharea(ii)
       end if
      if (vc > 5.) vc = 5.

	   thbase = 0.
	   thbase = ch_hyd(jhyd)%l * 1000. / (3600. * 24. * vc)
	   if (thbase > 1.) thbase = 1.
	   
      !! JIMMY"S NEW IMPROVED METHOD for sediment transport

       cyin = 0.
       cych = 0.
       depnet = 0.
       deg = 0.
       dep = 0.
       if (sedin < 1e-6) sedin = 0.
	   cyin = sedin / qin !tons/m3

	   !!water temperature (Celsius)
	   tmpw = 5.0 + 0.75 * wst(iwst)%weat%tave

	   !! water viscosity (m2/s) using 3rd order polynomial interpolation
	   visco_h2o = -3.e-6 * tmpw ** 3 + 0.0006 * tmpw ** 2 -               &
               	   0.0469 * tmpw + 1.7517		
	   visco_h2o = visco_h2o * 1.e-6
   
	   !! Use either Brownlie or Yang Model for bead load calculation
	   select case (bsn_cc%sed_ch)
	     case (0)
		   !! Bagnold"s (1977) stream power
           cych = bsn_prm%spcon * vc ** bsn_prm%spexp
		 case (1)
		   !!Brownlie Model 
		   !! grain Reynolds number
		   Reynolds_g = sqrt(9.81 * channel_d50 ** 3) / visco_h2o
 
		   !!critical shear stress for grain Froude number
		   ycoeff = (sqrt(particle_specific_gravity - 1.) *                     &
      	              Reynolds_g) ** (-0.6)
		   shear_stress = 0.22 * ycoeff + 0.06 * 10 ** (-7.7 * ycoeff)

		   !! critical grain Froude number
		   fr_gc = 4.596 * shear_stress ** 0.5293 * ch_hyd(jhyd)%s **           &
                                (-0.1405)  * 1.57 ** (-0.1606)

		   !! grain Froude number
		   fr_g = vc / sqrt((particle_specific_gravity - 1.) *                  &
      		   9.81 * (bsn_prm%ch_d50 / 1000.))

		   !! sediment concentration at the channel outlet [ppm, or g/m3]
		   if(fr_g>fr_gc) then                                              
		     sedcon = 7115 * 1.268 * (fr_g - fr_gc) ** 1.978 *                &               
       	                       ch_hyd(jhyd)%s ** 0.6601 * (rhy(ii) /          &
                                 channel_d50) ** (-0.3301)
		   else
		     sedcon = 0.
		   endif
		   cych = sedcon * 1.e-6 !tons/m3		 
		
		 case (2)
		   !!Yang Model
		   !! particle fall velocity
		   vfall = 9.81 * channel_d50 ** 2 * (particle_specific_gravity - 1.)    &
         		    / (18.* visco_h2o)
		   
		   !! shear velocity
		   vshear = sqrt(9.81 * rhy(ii) * ch_hyd(jhyd)%s)

		   coefa = vfall * channel_d50 / visco_h2o
		   coefe = vshear * channel_d50 / visco_h2o
		 
		   if(coefe<70) then
		     if (coefe<1.2) coefe = 1.2
			 coefb = 2.5 / (log10(coefe) - 0.06) + 0.66
		   elseif(coefe>=70) then
		     coefb = 2.05
		   else
		     write(*,*) "Error in implementing Yang erosion model"
!!		     stop
		   endif

		   coefc = vshear / vfall
		   coefd = vc * ch_hyd(jhyd)%s / vfall - coefb * ch_hyd(jhyd)%s
 		   if(coefd<=0) coefd = 1.e-6
		   
		   if(bsn_prm%ch_d50 <= 2.0) then ! in millimeter 
		     !! use sand equation (1973)
		     log10sedcon = 5.435 - 0.286 * log10(coefa) - 0.457 *            &
                log10(coefc) +(1.799 - 0.409 *log10(coefa) - 0.314 *           &
                log10(coefc)) * log10(coefd)

		   elseif (bsn_prm%ch_d50 > 2.0) then 
		     !! use gravel equation (1984)
		     log10sedcon = 6.681 - 0.633 * log10(coefa) - 4.816 *            &
                 log10(coefc) +(2.784 - 0.305 *log10(coefa) - 0.282 *          & 
                 log10(coefc)) * log10(coefd)
		   endif
		   sedcon = 10 ** log10sedcon !ppm
   		   cych = sedcon * 1.e-6 !tons/m3		 
	   end select

	   depnet = qin * (cych - cyin)
	   if(abs(depnet) < 1.e-6) depnet = 0.

!!  tbase is multiplied so that erosion is proportional to the traveltime, 
!!  which is directly related to the length of the channel
!!  Otherwise for the same discharge rate and sediment deficit
!!  the model will erode more sediment per unit length of channel 
!!  from a small channel than a larger channel. Modification made by Balaji Narasimhan
           if (depnet > 1.e-6) then
             deg = depnet * thbase
!! First the deposited material will be degraded before channel bed
	       if (deg >= ch(jrch)%depch) then
	         deg1 = ch(jrch)%depch
               deg2 = (deg - deg1) * ch_sed(jsed)%cov1*ch_sed(jsed)%cov2
	         ch(jrch)%depch = 0.
	       else
	         deg1 = deg
	         deg2 = 0.
	         ch(jrch)%depch = ch(jrch)%depch- deg1
	       endif
             dep = 0.
           else
             dep = -depnet * thbase
             deg = 0.
	         deg1 = 0.
	         deg2 = 0.
           endif
  	     hsedyld(ii) = sedin + deg1 + deg2 - dep
           if (hsedyld(ii) < 1.e-12) hsedyld(ii) = 0.



	     d_fract = hrtwtr(ii) / qin
	     if (d_fract > 1.) d_fract = 1.

           hsedyld(ii) = hsedyld(ii) * d_fract

!!  In this default sediment routing sediment is not tracked by particle size
           rch_san = 0.
           rch_sil = rch_sil + hsedyld(ii) !All are assumed to silt part
           rch_cla = 0.
           rch_sag = 0.
           rch_lag = 0.
	     rch_gra = 0.
     
           hsedst(ii) = sedin + deg1 + deg2 - dep - hsedyld(ii) 
           if (hsedst(ii) < 1.e-12) hsedst(ii) = 0.
	     ch(jrch)%depch = ch(jrch)%depch + dep
           ch(jrch)%sedst = hsedst(ii)

	     deg24 = deg24 + deg2
	     dep24 = dep24 + dep
	   else
	     hsedst(ii) = sedin
           ch(jrch)%sedst = hsedst(ii)
         end if
        end if
      end do

!!    Organic nitrogen and Organic Phosphorus contribution from channel erosion
      ch(jrch)%orgn = deg24 * ch_nut(jnut)%onco * 1000.
      ch(jrch)%orgp = deg24 * ch_nut(jnut)%opco * 1000.



      qdin = 0.
      qdin = rtwtr + ch(jrch)%rchstor


      if ((rtwtr == 0. .and. rchdep == 0.) .or. qdin <= 0.01) then
	  sedrch = 0.
	  rch_san = 0.
	  rch_sil = 0.
	  rch_cla = 0.
	  rch_sag = 0.
	  rch_lag = 0.
	  rch_gra = 0.
	end if


      !! compute changes in channel dimensions
      if ((rtwtr > 0. .and. rchdep > 0.) .or. qdin > 0.01) then
        if (bsn_cc%deg == 1) then
          qdin = 0.

          qdin = rtwtr + ch(jrch)%rchstor
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
            ch_hyd(jhyd)%s = ch_hyd(jhyd)%s - dat2 / (ch_hyd(jhyd)%l         &
                                                              * 1000.)
            ch_hyd(jhyd)%s = Max(.0001, ch_hyd(jhyd)%s)
            call ch_ttcoef(jrch)
          endif
        endif
        endif
	endif

      return
      end subroutine ch_rthsed