      subroutine res_nutrient (iob)

      use reservoir_data_module
      use time_module
      use reservoir_module
      use hydrograph_module, only : resz, ob, ht2, wbody
      use climate_module
      
      implicit none      
      
      real, external :: theta
      
      integer, intent (in) :: iob
      real :: nitrok = 0.        !              |
      real :: phosk = 0.         !              |
      real :: nitrosolk = 0.     !              |
      real :: phossolk = 0.      !              |
      real :: tpco = 0.          !              |
      real :: chlaco = 0.        !              |
      integer :: iwst = 0        !none          |weather station number
      real :: nsetlr = 0.        !              |
      real :: psetlr = 0.        !              |
      real :: nsolr = 0.         !              |
      real :: psolr = 0.         !              |
      real :: conc_n = 0.        !              |
      real :: conc_p = 0.        !              |
      real :: conc_soln = 0.     !              |
      real :: conc_solp = 0.     !              |
      real :: flo_tot = 0.       !              |wbody%flo + ht2%flo, guarded against ~0
      real :: rto = 0.           !              |ht2%flo / flo_tot, outflow fraction


      !! if reservoir volume less than 1 m^3, set all nutrient levels to
      !! zero and perform no nutrient calculations
      if (wbody%flo < 1.e-6) then
        wbody = resz
        return
      end if

      !! snap negligible nutrient masses to zero before any arithmetic below.
      !! the settling block multiplies each of these pools by a factor < 1 every day
      !! and NOTHING in the hru -> reservoir path replenishes no2 (and nh3/no2 only
      !! rarely), so they decay geometrically with no floor. Left alone they inevitably
      !! reach the denormal range (< 1.18e-38 for real4), and the next multiply or the
      !! conc_* divides below then trip -ffpe-trap=underflow, which this project builds
      !! with in EVERY configuration (see CMakeLists fdialect). Observed: a reservoir
      !! holding 75599 m^3, no3 = 3052 kg and orgn = 1347 kg died on no2 = 1.208e-38.
      !! 1.e-30 kg is physically indistinguishable from zero and matches the threshold
      !! already used in the outflow block further down.
      if (wbody%orgn < 1.e-30) wbody%orgn = 0.
      if (wbody%sedp < 1.e-30) wbody%sedp = 0.
      if (wbody%solp < 1.e-30) wbody%solp = 0.
      if (wbody%no3  < 1.e-30) wbody%no3  = 0.
      if (wbody%nh3  < 1.e-30) wbody%nh3  = 0.
      if (wbody%no2  < 1.e-30) wbody%no2  = 0.
      if (wbody%chla < 1.e-30) wbody%chla = 0.

      !! if reservoir volume greater than 1 m^3, perform nutrient calculations
      if (time%mo >= wbody_prm%nut%ires1 .and. time%mo <= wbody_prm%nut%ires2) then
        nsetlr = wbody_prm%nut%nsetlr1
        psetlr = wbody_prm%nut%psetlr1
      else
        nsetlr = wbody_prm%nut%nsetlr2
        psetlr = wbody_prm%nut%psetlr2
      endif
      nsolr = wbody_prm%nut%nsolr
      psolr = wbody_prm%nut%psolr

      !! n and p concentrations kg/m3 * kg/1000 t * 1000000 ppp = 1000
      conc_n = 1000. * wbody%orgn / wbody%flo
      conc_p = 1000. * wbody%sedp / wbody%flo
      conc_soln = 1000. * (wbody%no3 + wbody%nh3 + wbody%no2) / wbody%flo
      conc_solp = 1000. * wbody%solp / wbody%flo
      
      !! new inputs thetn, thetap, conc_pmin, conc_nmin
      !! Ikenberry wetland eqs modified - not function of area - fraction of difference in concentrations
      iwst = ob(iob)%wst
      nitrok = (conc_n - wbody_prm%nut%conc_nmin) * Theta(nsetlr, wbody_prm%nut%theta_n, wst(iwst)%weat%tave)
      nitrok = amin1 (nitrok, 1.)
      nitrok = max (nitrok, 0.)
      phosk = (conc_p - wbody_prm%nut%conc_pmin) * Theta(psetlr, wbody_prm%nut%theta_p, wst(iwst)%weat%tave)
      phosk = amin1 (phosk, 1.)
      phosk = max (phosk, 0.)
      nitrosolk = (conc_soln - wbody_prm%nut%conc_nmin) * Theta(nsolr, wbody_prm%nut%theta_n, wst(iwst)%weat%tave)
      nitrosolk = amin1 (nitrosolk, 1.)
      nitrosolk = max (nitrosolk, 0.)
      phossolk = (conc_solp - wbody_prm%nut%conc_pmin) * Theta(psolr, wbody_prm%nut%theta_p, wst(iwst)%weat%tave)
      phossolk = amin1 (phossolk, 1.)
      phossolk = max (phossolk, 0.)

      !! remove nutrients from reservoir by settling - exclude soluble nutrients
      !! other part of equation 29.1.3 in SWAT manual
      wbody%solp = wbody%solp * (1. - phossolk * wbody_prm%solp_stl_fr)
      wbody%sedp = wbody%sedp * (1. - phosk)
      wbody%orgn = wbody%orgn * (1. - nitrok)
      wbody%no3 = wbody%no3 * (1. - nitrosolk * wbody_prm%soln_stl_fr)
      wbody%nh3 = wbody%nh3 * (1. - nitrosolk * wbody_prm%soln_stl_fr)
      wbody%no2 = wbody%no2 * (1. - nitrosolk * wbody_prm%soln_stl_fr)

      !! calculate chlorophyll-a and water clarity
      chlaco = 0.
      wbody%chla = 0.
      tpco = 1.e+6 * (wbody%solp + wbody%sedp) / (wbody%flo + ht2%flo)
      if (tpco > 1.e-4) then
        !! equation 29.1.6 in SWAT manual
        !chlaco = wbody_prm%nut%chlar * 0.551 * (tpco**0.76)
        wbody%chla = (wbody%flo + ht2%flo) * 1.e-6
      endif
      
      !! check nutrient masses greater than zero
      wbody%no3 = max (wbody%no3, 0.0)
      wbody%orgn = max (wbody%orgn, 0.0)
      wbody%sedp = max (wbody%sedp, 0.0)
      wbody%solp = max (wbody%solp, 0.0)
      wbody%chla = max (wbody%chla, 0.0)
      wbody%nh3 = max (wbody%nh3, 0.0)
      wbody%no2 = max (wbody%no2, 0.0)

      !! calculate amount of nutrients leaving reservoir. Compute the outflow FRACTION
      !! once (bounded ~[0,1]) rather than multiplying wbody%X by raw ht2%flo then dividing:
      !! guarding each operand at 1.e-30 isn't enough when BOTH clear it but their product
      !! still underflows (e.g. 1e-29*1e-29). Gate ht2%flo at the 1.e-6 "meaningfully
      !! nonzero flow" threshold so rto can't land in the denormal range.
      flo_tot = wbody%flo + ht2%flo
      if (flo_tot > 1.e-6 .and. abs(ht2%flo) >= 1.e-6) then
        rto = ht2%flo / flo_tot
        ht2%no3 = 0.;  if (abs(wbody%no3)  >= 1.e-30 .and. abs(rto) >= 1.e-30) ht2%no3  = wbody%no3  * rto
        ht2%orgn = 0.; if (abs(wbody%orgn) >= 1.e-30 .and. abs(rto) >= 1.e-30) ht2%orgn = wbody%orgn * rto
        ht2%sedp = 0.; if (abs(wbody%sedp) >= 1.e-30 .and. abs(rto) >= 1.e-30) ht2%sedp = wbody%sedp * rto
        ht2%solp = 0.; if (abs(wbody%solp) >= 1.e-30 .and. abs(rto) >= 1.e-30) ht2%solp = wbody%solp * rto
        ht2%chla = 0.; if (abs(wbody%chla) >= 1.e-30 .and. abs(rto) >= 1.e-30) ht2%chla = wbody%chla * rto
        ht2%nh3 = 0.;  if (abs(wbody%nh3)  >= 1.e-30 .and. abs(rto) >= 1.e-30) ht2%nh3  = wbody%nh3  * rto
        ht2%no2 = 0.;  if (abs(wbody%no2)  >= 1.e-30 .and. abs(rto) >= 1.e-30) ht2%no2  = wbody%no2  * rto
      else
        ht2%no3 = 0.; ht2%orgn = 0.; ht2%sedp = 0.; ht2%solp = 0.
        ht2%chla = 0.; ht2%nh3 = 0.; ht2%no2 = 0.
      end if
      
      !! remove nutrients leaving reservoir
      wbody%no3 = max(0.,wbody%no3 - ht2%no3) !No less than zero, Jaehak 2024
      wbody%orgn = max(0.,wbody%orgn - ht2%orgn)
      wbody%sedp = max(0.,wbody%sedp - ht2%sedp)
      wbody%solp = max(0.,wbody%solp - ht2%solp)
      wbody%chla = max(0.,wbody%chla - ht2%chla)
      wbody%nh3 = max(0.,wbody%nh3 - ht2%nh3)
      wbody%no2 = max(0.,wbody%no2 - ht2%no2)

      return
      end subroutine res_nutrient