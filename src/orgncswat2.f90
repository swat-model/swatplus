      subroutine orgncswat2

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the amount of organic nitrogen removed in
!!    surface runoff - when using CSWAT==2 it 


!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    enratio       |none         |enrichment ratio calculated for day in HRU
!!    erorgn(:)     |none         |organic N enrichment ratio, if left blank
!!                                |the model will calculate for every event
!!    ihru          |none         |HRU number
!!    sedc_d(:)     |kg C/ha      |amount of C lost with sediment
!!
!!
!!     
!!                                |pools 

      use hru_module, only : enratio, hru, ihru, sedorgn, sedyld, surfq
      use soil_module
      use organic_mineral_mass_module
      use carbon_module
      
      implicit none 

      integer :: j                  !none          |HRU number
      real :: xx                    !kg N/ha       |amount of organic N in first soil layer
      real :: wt1                   !none          |conversion factor (mg/kg => kg/ha)
      real :: er                    !none          |enrichment ratio
      real :: conc                  !              |concentration of organic N in soil
      real :: xx1                   !              |
      real :: sol_thick             !              |
      real :: y1                    !              |
      real :: sol_mass              !              |  
      real :: QBC                   !              |c loss with runoff or lateral flow
      real :: VBC                   !              |c los with vertical flow
      real :: YBC                   !              |BMC LOSS WITH SEDIMENT
      real :: YOC                   !              |Organic C loss with sediment
      real :: YW                    !              |YW = WIND EROSION (T/HA)
      real :: TOT                   !              |Total organic carbon in layer 1
      real :: YEW                   !frac          |fraction of soil erosion of total soil mass
      real :: X1                    !              |
      real :: PRMT_21               !              |KOC FOR CARBON LOSS IN WATER AND SEDIMENT(500._1500.) KD = KOC * C
      real :: PRMT_44               !              |RATIO OF SOLUBLE C CONCENTRATION IN RUNOFF TO PERCOLATE(0.1_1.)
      real :: DK                    !              |
      real :: V                     !              |
      real :: X3                    !              | 
      real :: CO                    !              | 
      real :: CS                    !              | 
      real :: perc_clyr             !              | 
      real :: latc_clyr             !              | 
      integer :: k                  !none          |counter 
      
      j = ihru
      
      latc_clyr = 0.
      perc_clyr = 0.
      
      xx = 0.
	  wt1 = 0.  !! conversion factor
      er = 0.	!! enrichment ratio
        !! HRU calculations
        xx = rsd1(j)%tot_str%n + rsd1(j)%tot_meta%n + soil1(j)%hsta(1)%n + soil1(j)%hact(1)%n
        !wt = sol_bd(1,j) * sol_z(1,j) * 10. (tons/ha)
        !wt1 = wt/1000
        wt1 = soil(j)%phys(1)%bd * soil(j)%phys(1)%d / 100.

        if (hru(j)%hyd%erorgn > .001) then
          er = hru(j)%hyd%erorgn
        else
          er = enratio
        end if

      conc = xx * er / wt1

        !! HRU calculations
        sedorgn(j) = .001 * conc * sedyld(j) / hru(j)%area_ha

	!! update soil nitrogen pools only for HRU calculations
      if (xx > 1.e-6) then
        xx1 = (1. - sedorgn(j) / xx)
        
        !!add by zhang to update soil nitrogen pools
        
		rsd1(j)%str%n = rsd1(j)%str%n * xx1
		rsd1(j)%meta%n = rsd1(j)%meta%n * xx1
		soil1(j)%hsta(1)%n = soil1(j)%hsta(1)%n * xx1
		soil1(j)%hact(1)%n = soil1(j)%hact(1)%n * xx1
		!sol_BMN(1,j) = sol_BMN(1,j) * xx1
      end if
      
      !return
      
      !Calculate runoff and leached C&N from micro-biomass
      latc_clyr = 0.
      sol_mass = 0.  
      !kg/ha
      sol_mass = (soil(j)%phys(1)%d / 1000.) * 10000. * soil(j)%phys(1)%bd * 1000. * (1- soil(j)%phys(1)%rock / 100.)
      
      
      QBC=0.    !c loss with runoff or lateral flow
      VBC=0.    !c los with vertical flow
      YBC=0.    !BMC LOSS WITH SEDIMENT
      YOC=0.    !Organic C loss with sediment
      YW=0.     !YW = WIND EROSION (T/HA)
      TOT = soil1(j)%hsta(1)%c + soil1(j)%hact(1)%c + rsd1(j)%tot_meta%c + rsd1(j)%tot_str%c !Total organic carbon in layer 1
      !YEW = MIN(er*(sedyld(j)/hru(j)%area_ha+YW/hru(j)%area_ha)/(sol_mass/1000.),.9)
      ! Not sure whether should consider enrichment ratio or not!
      YEW = MIN((sedyld(j)/hru(j)%area_ha+YW/hru(j)%area_ha)/(sol_mass/1000.),.9) !fraction of soil erosion of total soil mass
      X1=1.-YEW
	  !YEW=MIN(ER*(YSD(NDRV)+YW)/WT(LD1),.9)
	  !ER enrichment ratio
	  !YSD water erosion
	  !YW wind erosion
      YOC=YEW*TOT
      soil1(j)%hact(1)%c = soil1(j)%hact(1)%c * X1
      soil1(j)%hsta(1)%c = soil1(j)%hsta(1)%c * X1
      rsd1(j)%tot_str%m = rsd1(j)%tot_str%m * X1
      rsd1(j)%tot_meta%m = rsd1(j)%tot_meta%m * X1
      rsd1(j)%tot_lignin%m = rsd1(j)%tot_lignin%m * X1
      rsd1(j)%tot_str%c = rsd1(j)%tot_str%c * X1
      rsd1(j)%tot_meta%c = rsd1(j)%tot_meta%c * X1
      rsd1(j)%tot_lignin%c = rsd1(j)%tot_lignin%c * X1
      !rsd1(j)%tot_lignin%c = rsd1(j)%tot_str%c - rsd1(j)%tot_lignin%c
      
      IF(soil1(j)%microb(1)%c > .01) THEN
          PRMT_21 = 0.  !KOC FOR CARBON LOSS IN WATER AND SEDIMENT(500._1500.) KD = KOC * C
          PRMT_21 = 1000.
          soil1(j)%water(1)%c = rsd1(j)%tot_str%c + rsd1(j)%tot_meta%c + soil1(j)%hsta(1)%c + soil1(j)%hact(1)%c + &
                soil1(j)%microb(1)%c 
          DK = .0001 * PRMT_21 * soil1(j)%water(1)%c
          !X1=PO(LD1)-S15(LD1)
          X1 = soil(j)%phys(1)%por*soil(j)%phys(1)%d-soil(j)%phys(1)%wpmm !mm
          IF (X1 <= 0.) THEN
            X1 = 0.01
          END IF
          XX=X1+DK
          !V=QD+Y4
          V = surfq(j) + soil(j)%ly(k)%prk + soil(j)%ly(1)%flat
	      !QD surface runoff
          X3=0.
          IF(V>1.E-10)THEN
              X3 = soil1(j)%microb(1)%c * (1.-EXP(-V/XX)) !loss of biomass C
              PRMT_44 = 0. !RATIO OF SOLUBLE C CONCENTRATION IN RUNOFF TO PERCOLATE(0.1_1.)
              PRMT_44 = .5
              CO=X3/(soil(j)%ly(k)%prk + PRMT_44*(surfq(j)+soil(j)%ly(1)%flat)) !CS is the horizontal concentration
              CS=PRMT_44*CO                                     !CO is the vertical concentration
              VBC=CO*(soil(j)%ly(k)%prk) 
              soil1(j)%microb(1)%c = soil1(j)%microb(1)%c - X3
              QBC=CS*(surfq(j)+soil(j)%ly(1)%flat)
        !     COMPUTE WBMC LOSS WITH SEDIMENT
              IF(YEW>0.)THEN
                  CS = DK * soil1(j)%microb(1)%c / XX
                  YBC=YEW*CS
              END IF
          END IF
      END IF

      soil1(j)%microb(1)%c = soil1(j)%microb(1)%c - YBC 
      hsc_d(j)%surq_c = QBC * (surfq(j) / (surfq(j) + soil(j)%ly(1)%flat + 1.e-6))
       
      soil(j)%ly(1)%latc = QBC*(soil(j)%ly(1)%flat/(surfq(j)+soil(j)%ly(1)%flat+1.e-6))
      soil(j)%ly(1)%percc = VBC 
      hsc_d(j)%sed_c = YOC + YBC
      
      DO k = 2, soil(j)%nly
          if (soil(j)%ly(k)%prk > 0 .and. k == soil(j)%nly) then
          end if
          sol_thick = 0.
          sol_thick = soil(j)%phys(k)%d-soil(j)%phys(k-1)%d
          soil1(j)%water(1)%c = soil1(j)%str(k)%c + soil1(j)%meta(k)%c + soil1(j)%hsta(k)%c + soil1(j)%hact(k)%c 
          Y1 = soil1(j)%microb(k)%c + VBC
          VBC=0.
          IF(Y1>=.01)THEN
              V=soil(j)%ly(k)%prk + soil(j)%ly(k)%flat
              IF(V>0.)VBC=Y1*(1.-EXP(-V/(soil(j)%phys(k)%por*sol_thick-soil(j)%phys(k)%wpmm+.0001*PRMT_21*soil1(j)%water(1)%c)))              
          END IF
          soil(j)%ly(k)%latc = VBC*(soil(j)%ly(k)%flat/(soil(j)%ly(k)%prk + soil(j)%ly(k)%flat+1.e-6))
          soil(j)%ly(k)%percc = VBC-soil(j)%ly(k)%latc
          soil1(j)%microb(k)%c = Y1 - VBC

        !! calculate nitrate in percolate and lateral flow
        if (k == soil(j)%nly) then
          hsc_d(j)%perc_c = soil(j)%ly(k)%percc
        end if
        latc_clyr = latc_clyr + soil(j)%ly(k)%latc
      END DO
     
      hsc_d(j)%latq_c = latc_clyr

      return


      end subroutine orgncswat2
