      subroutine nut_orgnc2

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
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use hru_module, only : enratio, hru, ihru, sedorgn, sedyld, surfq
      use soil_module
      use organic_mineral_mass_module
      use carbon_module
      use plant_module
      
      implicit none

      integer :: j = 0       !none          |HRU number
      real :: xx = 0.        !kg N/ha       |amount of organic N in first soil layer
      real :: wt1 = 0.       !none          |conversion factor (mg/kg => kg/ha)
      real :: er = 0.        !none          |enrichment ratio
      real :: conc = 0.      !              |concentration of organic N in soil
      real :: sol_mass = 0.  !              |  
      real :: QBC = 0.              !              |c loss with runoff or lateral flow
      real :: VBC = 0.              !              |c los with vertical flow
      real :: YBC = 0.              !              |BMC LOSS WITH SEDIMENT
      real :: YOC = 0.              !              |Organic C loss with sediment
      real :: YW = 0.               !              |YW = WIND EROSION (T/HA)
      real :: TOT = 0.              !              |Total organic carbon in layer 1
      real :: YEW = 0.              !frac          |fraction of soil erosion of total soil mass
      real :: X1 = 0.               !              |
      real :: PRMT_21 = 0.          !              |KOC FOR CARBON LOSS IN WATER AND SEDIMENT(500._1500.) KD = KOC * C
      real :: PRMT_44 = 0.          !              |RATIO OF SOLUBLE C CONCENTRATION IN RUNOFF TO PERCOLATE(0.1_1.)
      real :: DK = 0.               !              |
      real :: V = 0.                !              |
      real :: X3 = 0.               !              | 
      real :: CO = 0.               !              | 
      real :: CS = 0.               !              | 
      real :: perc_clyr = 0.        !              | 
      real :: latc_clyr = 0.        !              | 
      integer :: k = 0              !none          |counter 
      real :: xx1 = 0.              !              |
      real :: sol_thick = 0.        !              |
      real :: y1 = 0.               !              |
      real :: c_ly1 = 0.
      real :: ipl                   !              |counter for plant in the community
      
      j = ihru
      
      latc_clyr = 0.        
      perc_clyr = 0.
      wt1 = 0.  !! conversion factor
      er = 0.   !! enrichment ratio
      
      !! total carbon in surface residue and soil humus
      c_ly1 = soil1(j)%hp(1)%n + soil1(j)%hs(1)%n + soil1(j)%meta(1)%n + soil1(j)%str(1)%n
      !! wt = sol_bd(1,j) * sol_z(1,j) * 10. (tons/ha) -> wt1 = wt/1000
      wt1 = soil(j)%phys(1)%bd * soil(j)%phys(1)%d / 100.

      if (hru(j)%hyd%erorgn > .001) then
        er = hru(j)%hyd%erorgn
      else
        er = enratio
      end if

      conc = c_ly1 * er / wt1

      !! organic n leaving hru
      sedorgn(j) = .001 * conc * sedyld(j) / hru(j)%area_ha

      !! update soil carbon organic nitrogen pools
      if (c_ly1 > 1.e-6) then
        xx1 = (1. - sedorgn(j) / c_ly1)
        soil1(j)%tot(1)%n = soil1(j)%tot(1)%n * xx1
        soil1(j)%hs(1)%n = soil1(j)%hs(1)%n * xx1
        soil1(j)%hp(1)%n = soil1(j)%hp(1)%n * xx1
        soil1(j)%rsd(1)%n = soil1(j)%rsd(1)%n * xx1
        soil1(j)%meta(1)%n = soil1(j)%meta(1)%n * xx1
        soil1(j)%str(1)%n = soil1(j)%str(1)%n * xx1
        soil1(j)%lig(1)%n = soil1(j)%lig(1)%n * xx1
      end if
      
      !! Calculate runoff and leached C&N from microbial biomass
      latc_clyr = 0.
      sol_mass = (soil(j)%phys(1)%d / 1000.) * 10000. * soil(j)%phys(1)%bd * 1000. * (1- soil(j)%phys(1)%rock / 100.)
      
      QBC=0.    !c loss with runoff or lateral flow
      VBC=0.    !c los with vertical flow
      YBC=0.    !BMC LOSS WITH SEDIMENT
      YOC=0.    !Organic C loss with sediment
      YW=0.     !YW = WIND EROSION (T/HA)
      TOT = soil1(j)%hp(1)%c + soil1(j)%hs(1)%c + soil1(j)%meta(1)%c + soil1(j)%str(1)%c !Total organic carbon in layer 1
      !YEW = MIN(er*(sedyld(j)/hru(j)%area_ha+YW/hru(j)%area_ha)/(sol_mass/1000.),.9)
      ! Not sure whether should consider enrichment ratio or not!
      YEW = MIN((sedyld(j)/hru(j)%area_ha+YW/hru(j)%area_ha)/(sol_mass/1000.),.9) !fraction of soil erosion of total soil mass
      X1=1.- YEW
      !YEW=MIN(ER*(YSD(NDRV)+YW)/WT(LD1),.9)
      !ER enrichment ratio
      !YSD water erosion
      !YW wind erosion
      YOC=YEW*TOT
      soil1(j)%tot(1)%c = soil1(j)%tot(1)%c * X1
      soil1(j)%hs(1)%c = soil1(j)%hs(1)%c * X1
      soil1(j)%hp(1)%c = soil1(j)%hp(1)%c * X1
      soil1(j)%rsd(1)%c = soil1(j)%rsd(1)%c * X1
      soil1(j)%str(1)%c = soil1(j)%str(1)%c * X1
      soil1(j)%meta(1)%c = soil1(j)%meta(1)%c * X1
      soil1(j)%lig(1)%c = soil1(j)%lig(1)%c * X1
          
        
      if (soil1(j)%microb(1)%c > .01) then
          PRMT_21 = cb_wtr_coef%prmt_21 !KOC FOR CARBON LOSS IN WATER AND SEDIMENT(500._1500.) KD = KOC * C
          soil1(j)%tot(1)%c = soil1(j)%str(1)%c + soil1(j)%meta(1)%c + soil1(j)%hp(1)%c + soil1(j)%hs(1)%c + soil1(j)%microb(1)%c 
          DK = .0001 * PRMT_21 * soil1(j)%tot(1)%c
          !X1=PO(LD1)-S15(LD1)
          X1 = soil(j)%phys(1)%por*soil(j)%phys(1)%d-soil(j)%phys(1)%wpmm !mm
          IF (X1 <= 0.) THEN
            X1 = 0.01
          END IF
          XX=X1+DK
          V = surfq(j) + soil(j)%ly(1)%prk + soil(j)%ly(1)%flat
          !QD surface runoff
          X3=0.
          IF(V>1.E-10)THEN
              X3 = soil1(j)%microb(1)%c * (1.-EXP(-V/XX)) !loss of biomass C
              PRMT_44 = cb_wtr_coef%prmt_44  !RATIO OF SOLUBLE C CONCENTRATION IN RUNOFF TO PERCOLATE(0.1_1.)
              CO = X3/(soil(j)%ly(1)%prk + PRMT_44 * (surfq(j) + soil(j)%ly(1)%flat)) !CS is the horizontal concentration
              CS = PRMT_44*CO                                     !CO is the vertical concentration
              VBC = CO*(soil(j)%ly(1)%prk) 
              soil1(j)%microb(1)%c = soil1(j)%microb(1)%c - X3
              QBC = CS*(surfq(j)+soil(j)%ly(1)%flat)
        !     COMPUTE WBMC LOSS WITH SEDIMENT
              IF(YEW>0.)THEN
                  CS = DK * soil1(j)%microb(1)%c / XX
                  YBC = YEW * CS
              END IF
          END IF
      END IF

      soil1(j)%microb(1)%c = soil1(j)%microb(1)%c - YBC 
      soil1(j)%tot(1)%c = soil1(j)%str(1)%c + soil1(j)%meta(1)%c + soil1(j)%hp(1)%c + soil1(j)%hs(1)%c + soil1(j)%microb(1)%c 
      hsc_d(j)%surq_c = QBC * (surfq(j) / (surfq(j) + soil(j)%ly(1)%flat + 1.e-6))
       
      soil(j)%ly(1)%latc = QBC*(soil(j)%ly(1)%flat/(surfq(j)+soil(j)%ly(1)%flat+1.e-6))
      soil(j)%ly(1)%percc = VBC 
      hsc_d(j)%sed_c = YOC + YBC
      
      do k = 2, soil(j)%nly
          ! if (soil(j)%ly(k)%prk > 0 .and. k == soil(j)%nly) then   ! commented out by FG because it doesn't do anything.
          ! end if
          sol_thick = 0.
          sol_thick = soil(j)%phys(k)%d-soil(j)%phys(k-1)%d
          ! soil1(j)%tot(1)%c = soil1(j)%hp(k)%c + soil1(j)%hs(k)%c 
          ! soil1(j)%tot(k)%c = soil1(j)%hp(k)%c + soil1(j)%hs(k)%c  ! commented out by FG because is not needed here.
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

          soil1(j)%tot(k)%c = soil1(j)%str(k)%c + soil1(j)%meta(k)%c + soil1(j)%hp(k)%c + soil1(j)%hs(k)%c + soil1(j)%microb(k)%c 
          soil1(j)%seq(k)%c = soil1(j)%hp(k)%c + soil1(j)%hs(k)%c + soil1(j)%microb(k)%c 

      end do
     
      hsc_d(j)%latq_c = latc_clyr

      return
      end subroutine nut_orgnc2