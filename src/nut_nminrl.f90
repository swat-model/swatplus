      subroutine nut_nminrl

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine estimates daily nitrogen and phosphorus
!!    mineralization and immobilization considering fresh organic
!!    material (plant residue) and active and stable humus material

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru          |none          |HRU number
!!    rsdco_pl(:)   |none          |plant residue decomposition coefficient. The
!!                                 |fraction of residue which will decompose in
!!                                 |a day assuming optimal moisture,
!!                                 |temperature, C:N ratio, and C:P ratio
!!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max, Exp, Sqrt, Min, Abs

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use septic_data_module
      use plant_data_module
      use basin_module
      use organic_mineral_mass_module
      use hru_module, only : rsdco_plcom, i_sep, ihru, ipl, isep 
      use soil_module
      use plant_module
      use output_landscape_module, only : hnb_d
      
      implicit none 

      integer :: j          !none          |HRU number
      integer :: k          !none          |counter (soil layer)
      integer :: kk         !none          |soil layer used to compute soil water and
                            !              |soil temperature factors
      integer :: idp
      real :: rmn1          !kg N/ha       |amount of nitrogen moving from fresh organic
                            !              |to nitrate(80%) and active organic(20%)
                            !              |pools in layer
      real :: rmp           !              |to labile(80%) and organic(20%) pools in layer
      real :: xx            !varies        |variable to hold intermediate calculation result
      real :: csf           !none          |combined temperature/soil water factor
      real :: rwn           !kg N/ha       |amount of nitrogen moving from active organic
                            !              |to stable organic pool in layer
      real :: hmn           !kg N/ha       |amount of nitrogen moving from active organic
                            !              |nitrogen pool to nitrate pool in layer
      real :: hmp           !kg P/ha       |amount of phosphorus moving from the organic
                            !              |pool to the labile pool in layer
      real :: cnr           !              |carbon nitrogen ratio
      real :: cnrf          !              |carbon nitrogen ratio factor 
      real :: cpr           !              |carbon phosphorus ratio
      real :: cprf          !              |carbon phosphorus ratio factor
      real :: ca            !              |
      real :: decr          !              |
      real :: rdc           !              |
      real :: wdn           !kg N/ha       |amount of nitrogen lost from nitrate pool in
                            !              |layer due to denitrification
      real :: cdg           !none          |soil temperature factor
      real :: sut           !none          |soil water factor
      real :: nactfr        !none          |nitrogen active pool fraction. The fraction
                            !              |of organic nitrogen in the active pool. 

      j = ihru
      nactfr = .02
      !zero transformations for summing layers
      hnb_d(j)%rsd_nitorg_n = 0.
      hnb_d(j)%rsd_laborg_p = 0.
      hnb_d(j)%act_nit_n = 0.
      hnb_d(j)%org_lab_p = 0.
      hnb_d(j)%act_sta_n = 0.
      hnb_d(j)%denit = 0.

      !! mineralization can occur only if temp above 0 deg
      if (soil(j)%phys(1)%tmp > 0.) then
      !! compute residue decomp and mineralization of fresh organic n and p of flat residue
        do ipl = 1, pcom(j)%npl        !! we need to decompose each plant
          rmn1 = 0.
          rmp = 0.
          if (rsd1(j)%tot(ipl)%n > 1.e-4) then
            cnr = rsd1(j)%tot(ipl)%c / rsd1(j)%tot(ipl)%n
            if (cnr > 500.) cnr = 500.
            cnrf = Exp(-.693 * (cnr - 25.) / 25.)
          else
            cnrf = 1.
          end if
            
          if (rsd1(j)%tot(ipl)%p > 1.e-4) then
            cpr = rsd1(j)%tot(ipl)%c / rsd1(j)%tot(ipl)%p
            if (cpr > 5000.) cpr = 5000.
            cprf = Exp(-.693 * (cpr - 200.) / 200.)
          else
            cprf = 1.
          end if
        
          !! compute soil water factor
	      if (soil(j)%phys(1)%st < 0.) soil(j)%phys(1)%st = .0000001
          sut = .1 + .9 * Sqrt(soil(j)%phys(1)%st / soil(j)%phys(1)%fc)
          sut = Max(.05, sut)

          !!compute soil temperature factor
          xx = soil(j)%phys(1)%tmp
          cdg = .9 * xx / (xx + Exp(9.93 - .312 * xx)) + .1
          cdg = Max(.1, cdg)

          !! compute combined factor
          xx = cdg * sut
          if (xx < 0.) xx = 0.
          if (xx > 1.e6) xx = 1.e6
          csf = Sqrt(xx)
          ca = Min(cnrf, cprf, 1.)
          !! compute residue decomp and mineralization for each plant
          if (pcom(j)%npl > 0) then
            idp = pcom(j)%plcur(ipl)%idplt
            decr = pldb(idp)%rsdco_pl * ca * csf
          else
            decr = 0.05 * ca * csf
          end if
          decr = Max(bsn_prm%decr_min, decr)
          decr = Min(decr, 1.)
          
          !! mineralization of mass and carbon
          rsd1(j)%tot(ipl)%m = Max(1.e-6, rsd1(j)%tot(ipl)%m)
          rdc = decr * rsd1(j)%tot(ipl)%m
          rsd1(j)%tot(ipl)%m = rsd1(j)%tot(ipl)%m - rdc
          if (rsd1(j)%tot(ipl)%m < 0.) rsd1(j)%tot(ipl)%m = 0.
          rsd1(j)%tot(ipl)%c = (1. - decr) * rsd1(j)%tot(ipl)%c
          if (rsd1(j)%tot(ipl)%c < 0.) rsd1(j)%tot(ipl)%c = 0.
          soil1(j)%hact(1)%c = soil1(j)%hact(1)%c + decr * rsd1(j)%tot(ipl)%c
          
          !! mineralization of residue n and p
          rmn1 = decr * rsd1(j)%tot(ipl)%n 
          rsd1(j)%tot(ipl)%n = Max(1.e-6, rsd1(j)%tot(ipl)%n)
          rsd1(j)%tot(ipl)%n = rsd1(j)%tot(ipl)%n - rmn1
          soil1(j)%mn(1)%no3 = soil1(j)%mn(1)%no3 + .8 * rmn1
          soil1(j)%hact(1)%n = soil1(j)%hact(1)%n + .2 * rmn1
          
          rsd1(j)%tot(ipl)%p = Max(1.e-6, rsd1(j)%tot(ipl)%p)
          rmp = decr * rsd1(j)%tot(ipl)%p
          rsd1(j)%tot(ipl)%p = rsd1(j)%tot(ipl)%p - rmp
          soil1(j)%mp(1)%lab = soil1(j)%mp(1)%lab + .8 * rmp
          soil1(j)%hact(1)%p = soil1(j)%hact(1)%p + .2 * rmp
          
          hnb_d(j)%rsd_nitorg_n = hnb_d(j)%rsd_nitorg_n + .8 * rmn1
          hnb_d(j)%rsd_laborg_p = hnb_d(j)%rsd_laborg_p + .8 * rmp
          
        end do      ! ipl = 1, pcom(j)%npl
      end if
          
      !! compute humus mineralization of organic soil pools 
      do k = 1, soil(j)%nly

        if (k == 1) then
          kk = 2
        else
          kk = k
        end if

        !! mineralization can occur only if temp above 0 deg
        if (soil(j)%phys(kk)%tmp > 0.) then
          !! compute soil water factor
          sut = 0.
	      !! change for domain error 1/29/09 gsm check with Jeff !!!
	      if (soil(j)%phys(kk)%st < 0.) soil(j)%phys(kk)%st = .0000001
          sut = .1 + .9 * Sqrt(soil(j)%phys(kk)%st / soil(j)%phys(kk)%fc)
          sut = Max(.05, sut)

          !!compute soil temperature factor
          xx = soil(j)%phys(kk)%tmp
          cdg = .9 * xx / (xx + Exp(9.93 - .312 * xx)) + .1
          cdg = Max(.1, cdg)

          !! compute combined factor
          xx = cdg * sut
          if (xx < 0.) xx = 0.
          if (xx > 1.e6) xx = 1.e6
          csf = Sqrt(xx)

          !! compute flow from active to stable pools- maintain fraction of active (nactfr)
          rwn = .1e-4 * ((soil1(j)%hact(k)%n * (1. / nactfr - 1.) - soil1(j)%hsta(k)%n))
          if (rwn > 0.) then
            rwn = Min(rwn, soil1(j)%hact(k)%n)
          else
            rwn = -(Min(Abs(rwn), soil1(j)%hsta(k)%n))
          endif
          soil1(j)%hsta(k)%n = Max(1.e-6, soil1(j)%hsta(k)%n + rwn)
          soil1(j)%hact(k)%n = Max(1.e-6, soil1(j)%hact(k)%n - rwn)
          hnb_d(j)%act_sta_n = hnb_d(j)%act_sta_n + rwn

          !! compute humus mineralization on active organic n
          hmn = bsn_prm%cmn * csf * soil1(j)%hact(k)%n
          hmn = Min(hmn, soil1(j)%hact(k)%n)
          !! compute humus mineralization on active organic p
          xx = soil1(j)%hsta(k)%n + soil1(j)%hact(k)%n
          if (xx > 1.e-6) then
            hmp = 1.4 * hmn * soil1(j)%hsta(k)%p / xx
          else
            hmp = 0.
          end if
          hmp = Min(hmp, soil1(j)%hsta(k)%p)
          !! move mineralized nutrients between pools
          soil1(j)%hact(k)%n = Max(1.e-6, soil1(j)%hact(k)%n - hmn)
          soil1(j)%mn(k)%no3 = soil1(j)%mn(k)%no3 + hmn
          soil1(j)%hsta(k)%p = soil1(j)%hsta(k)%p - hmp
          soil1(j)%mp(k)%lab = soil1(j)%mp(k)%lab + hmp
          
          hnb_d(j)%act_nit_n = hnb_d(j)%act_nit_n + hmn
          hnb_d(j)%org_lab_p = hnb_d(j)%org_lab_p + hmp

          !! compute residue decomp and mineralization of 
          !! fresh organic n and p (upper two layers only)
          rmn1 = 0.
          rmp = 0.
          if (soil1(j)%rsd(k)%n > 1.e-4) then
            cnr = soil1(j)%rsd(k)%c / soil1(j)%rsd(k)%n
            if (cnr > 500.) cnr = 500.
            cnrf = Exp(-.693 * (cnr - 25.) / 25.)
          else
            cnrf = 1.
          end if
            
            if (soil1(j)%rsd(k)%p > 1.e-4) then
              cpr = soil1(j)%rsd(k)%c / soil1(j)%rsd(k)%p
              if (cpr > 5000.) cpr = 5000.
              cprf = Exp(-.693 * (cpr - 200.) / 200.)
            else
              cprf = 1.
            end if

            ca = Min(cnrf, cprf, 1.)
            
            !! compute root and incorporated residue decomposition
            !! all plant residue in soil is mixed - don't track individual plant residue in soil
              
            if (pcom(j)%npl > 0) then
              decr = rsdco_plcom(j) / pcom(j)%npl * ca * csf
            else
              decr = 0.05
            end if
            decr = Max(bsn_prm%decr_min, decr)
            decr = Min(decr, 1.)
            rmn1 = decr * (soil1(j)%str(k)%n + soil1(j)%lig(k)%n + soil1(j)%meta(k)%n)
            rmp = decr * (soil1(j)%str(k)%p + soil1(j)%lig(k)%p + soil1(j)%meta(k)%p)

            soil1(j)%str(k)%n = soil1(j)%str(k)%n * (1. - decr)
            soil1(j)%lig(k)%n = soil1(j)%lig(k)%n * (1. - decr)
            soil1(j)%meta(k)%n = soil1(j)%meta(k)%n * (1. - decr)
            soil1(j)%str(k)%p = soil1(j)%str(k)%p * (1. - decr)
            soil1(j)%lig(k)%p = soil1(j)%lig(k)%p * (1. - decr)
            soil1(j)%meta(k)%p = soil1(j)%meta(k)%p * (1. - decr)

         !   soil1(j)%mn(k)%no3 = soil1(j)%mn(k)%no3 + .8 * rmn1
         !   soil1(j)%hact(k)%n = soil1(j)%hact(k)%n + .2 * rmn1
         !   soil1(j)%mp(k)%lab = soil1(j)%mp(k)%lab + .8 * rmp
         !   soil1(j)%hsta(k)%p = soil1(j)%hsta(k)%p + .2 * rmp
            
         !   hnb_d(j)%rsd_nitorg_n = hnb_d(j)%rsd_nitorg_n + rmn1
         !   hnb_d(j)%rsd_laborg_p = hnb_d(j)%rsd_laborg_p + rmp

          !!  compute denitrification
          wdn = 0.   
	      if (i_sep(j) /= k .or. sep(isep)%opt  /= 1) then
	        if (sut >= bsn_prm%sdnco) then
	          wdn = soil1(j)%mn(k)%no3 * (1.-Exp(-bsn_prm%cdn * cdg * soil1(j)%cbn(k) / 100.))
	        else
	          wdn = 0.
	        endif
	        soil1(j)%mn(k)%no3 = max(0.0001,soil1(j)%mn(k)%no3 - wdn)
          end if
          hnb_d(j)%denit = hnb_d(j)%denit + wdn

          !call nut_denit(k,j,cdg,wdn,0.05)

        end if
      end do        ! k = 1, soil(j)%nly

      return
      end subroutine nut_nminrl