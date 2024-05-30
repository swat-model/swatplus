      subroutine res_nutrient (iob)

      use reservoir_data_module
      use time_module
      use reservoir_module
      use hydrograph_module, only : resz, ob, ht2, wbody
      use climate_module
      
      implicit none      
      
      integer, intent (in) :: iob
      real :: nitrok             !              |
      real :: phosk              !              |
      real :: tpco               !              |
      real :: chlaco             !              |
      integer :: iwst            !none          |weather station number
      real :: nsetlr             !              |
      real :: psetlr             !              |
      real :: conc_n             !              |
      real :: conc_p             !              |
      real :: theta              !              |
      

      !! if reservoir volume less than 1 m^3, set all nutrient levels to
      !! zero and perform no nutrient calculations
      if (wbody%flo < 1.e-6) then
        wbody = resz
        return
      end if

      !! if reservoir volume greater than 1 m^3, perform nutrient calculations
      if (time%mo >= wbody_prm%nut%ires1 .and. time%mo <= wbody_prm%nut%ires2) then
        nsetlr = wbody_prm%nut%nsetlr1
        psetlr = wbody_prm%nut%psetlr1
      else
        nsetlr = wbody_prm%nut%nsetlr2
        psetlr = wbody_prm%nut%psetlr2
      endif

      !! n and p concentrations kg/m3 * kg/1000 t * 1000000 ppp = 1000
      conc_n = 1000. * (wbody%orgn + wbody%no3 + wbody%nh3 + wbody%no2) / wbody%flo
      conc_p = 1000. * (wbody%sedp + wbody%solp) / wbody%flo
      
      !! new inputs thetn, thetap, conc_pmin, conc_nmin
      !! Ikenberry wetland eqs modified - not function of area - fraction of difference in concentrations
      iwst = ob(iob)%wst
      nitrok = (conc_n - wbody_prm%nut%conc_nmin) * Theta(nsetlr, wbody_prm%nut%theta_n, wst(iwst)%weat%tave)
      nitrok = amin1 (nitrok, 1.)
      nitrok = max (nitrok, 0.)
      phosk = (conc_p - wbody_prm%nut%conc_pmin) * Theta(psetlr, wbody_prm%nut%theta_p, wst(iwst)%weat%tave)
      phosk = amin1 (phosk, 1.)
      phosk = max (phosk, 0.)

      !! remove nutrients from reservoir by settling
      !! other part of equation 29.1.3 in SWAT manual
      wbody%solp = wbody%solp * (1. - phosk)
      wbody%sedp = wbody%sedp * (1. - phosk)
      wbody%orgn = wbody%orgn * (1. - nitrok)
      wbody%no3 = wbody%no3 * (1. - nitrok)
      wbody%nh3 = wbody%nh3 * (1. - nitrok)
      wbody%no2 = wbody%no2 * (1. - nitrok)

      !! calculate chlorophyll-a and water clarity
      chlaco = 0.
      wbody%chla = 0.
      tpco = 1.e+6 * (wbody%solp + wbody%sedp) / (wbody%flo + ht2%flo)
      if (tpco > 1.e-4) then
        !! equation 29.1.6 in SWAT manual
        chlaco = wbody_prm%nut%chlar * 0.551 * (tpco**0.76)
        wbody%chla = chlaco * (wbody%flo + ht2%flo) * 1.e-6
      endif
      
      !! check nutrient masses greater than zero
      wbody%no3 = max (wbody%no3, 0.0)
      wbody%orgn = max (wbody%orgn, 0.0)
      wbody%sedp = max (wbody%sedp, 0.0)
      wbody%solp = max (wbody%solp, 0.0)
      wbody%chla = max (wbody%chla, 0.0)
      wbody%nh3 = max (wbody%nh3, 0.0)
      wbody%no2 = max (wbody%no2, 0.0)

      !! calculate amount of nutrients leaving reservoir
      ht2%no3 = wbody%no3 * ht2%flo / (wbody%flo + ht2%flo)
      ht2%orgn = wbody%orgn * ht2%flo / (wbody%flo + ht2%flo)
      ht2%sedp = wbody%sedp * ht2%flo / (wbody%flo + ht2%flo)
      ht2%solp = wbody%solp * ht2%flo / (wbody%flo + ht2%flo)
      ht2%chla = wbody%chla * ht2%flo / (wbody%flo + ht2%flo)
      ht2%nh3 = wbody%nh3 * ht2%flo / (wbody%flo + ht2%flo)
      ht2%no2 = wbody%no2 * ht2%flo / (wbody%flo + ht2%flo)
      
      !! remove nutrients leaving reservoir
      wbody%no3 = wbody%no3 - ht2%no3
      wbody%orgn = wbody%orgn - ht2%orgn
      wbody%sedp = wbody%sedp - ht2%sedp
      wbody%solp = wbody%solp - ht2%solp
      wbody%chla = wbody%chla - ht2%chla
      wbody%nh3 = wbody%nh3 - ht2%nh3
      wbody%no2 = wbody%no2 - ht2%no2

      return
      end subroutine res_nutrient