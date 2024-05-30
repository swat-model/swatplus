      subroutine pl_leaf_drop (resnew, resnew_n)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine checks the dormant status of the different plant types

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    daylmn(:)      |hours         |shortest daylength occurring during the
!!                                  |year
!!    ihru           |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use climate_module
      use basin_module
      use hydrograph_module
      use plant_data_module
      use organic_mineral_mass_module
      use hru_module, only : hru,  ipl, ihru
      use soil_module
      use plant_module
      use carbon_module
      use time_module
      
      implicit none

      real, intent (in) :: resnew       !              |
      real, intent (in) :: resnew_n     !              |
      integer :: j                      !none          |HRU number
      integer :: idp                    !              |
      integer :: orgc_f                 !fraction      |fraction of organic carbon in fertilizer
      integer :: iob                    !              |
      integer :: iwgn                   !              |
      real :: xx                        !varies        |variable to hold calculation results 
      real :: rln                       !              |  
      real :: rlr                       !fraction      |fraction of lignin in the added residue
      real :: BLG1                      !              |LIGNIN FRACTION IN PLANT AT .5 MATURITY
      real :: BLG2                      !              |LIGNIN FRACTION IN PLANT AT MATURITY 
      real :: BLG3                      !              |             
      real :: CLG                       !              | 
      real :: sf                        !fraction      |fraction of mineral n sorbed to litter: 0.05 for surface litter, 0.1 for belowground litter 
      real :: sol_min_n                 !              |
      real :: resnew_ne                 !              |
      real :: LMF                       !frac          |fraction of the litter that is metabolic
      real :: LSF                       !frac          |fraction of the litter that is structural
      real :: LMNF                      !kg kg-1       |fraction of metabolic litter that is N
      real :: LSLF                      !kg kg-1       |fraction of structural litter that is lignin 
      real :: LSNF                      !kg kg-1       |fraction of structural litter that is N	
      
      orgc_f = 0.
      BLG1 = 0.
      BLG2 = 0.
      BLG3 = 0.
      CLG = 0.
      sf = 0.
      sol_min_n = 0.
      resnew_ne = 0.
      LMF = 0.
      LSF = 0.
      LSLF = 0.
      LSNF = 0.
      LMNF = 0.

      !!by zhang
      !!====================

      j = ihru
      idp = pcom(j)%plcur(ipl)%idplt
      iob = hru(j)%obj_no
      iwst = ob(iob)%wst
      iwgn = wst(iwst)%wco%wgn

      if (bsn_cc%cswat == 2) then
        hrc_d(j)%plant_c = hrc_d(j)%plant_c + pl_mass(j)%ab_gr(ipl)%c
        hpc_d(j)%drop_c = hpc_d(j)%drop_c + pl_mass(j)%ab_gr(ipl)%c
      end if

        if (bsn_cc%cswat == 2) then
          BLG1 = 0.01/0.10
          BLG2 = 0.99
          BLG3 = 0.10
          XX = log(0.5/BLG1-0.5)
          BLG2 = (XX -log(1./BLG2-1.))/(1.-0.5)
          BLG1 = XX + 0.5*BLG2
          CLG=BLG3*pcom(j)%plcur(ipl)%phuacc/ (pcom(j)%plcur(ipl)%phuacc +    &
                EXP(BLG1-BLG2*pcom(j)%plcur(ipl)%phuacc))

          sf = 0.05	
          sol_min_n = (soil1(j)%mn(1)%no3 + soil1(j)%mn(1)%nh4)  	    
          resnew_ne = resnew_n + sf * sol_min_n

          RLN = (resnew * CLG/(resnew_n+1.E-5))
          RLR = MIN(.8, resnew * CLG/1000/(resnew/1000+1.E-5))
        	    
          LMF = 0.85 - 0.018 * RLN
          if (LMF <0.01) then
            LMF = 0.01
          else
            if (LMF >0.7) then
              LMF = 0.7
            end if
          end if      	  

          LSF =  1 - LMF  
        	  
          rsd1(j)%meta%m = rsd1(j)%meta%m + LMF * resnew
          rsd1(j)%str%m = rsd1(j)%str%m + LSF * resnew

          LSLF = CLG          
	          
          rsd1(j)%tot_str%c = rsd1(j)%tot_str%c + 0.42*LSF * resnew  
	          
          rsd1(j)%tot_lignin%c = rsd1(j)%tot_lignin%c + RLR * 0.42 * LSF * resnew
          rsd1(j)%tot_lignin%c = rsd1(j)%tot_str%c - rsd1(j)%tot_lignin%c

          if (resnew_ne >= (0.42 * LSF * resnew /150)) then
            rsd1(j)%tot_str%n = rsd1(j)%tot_str%n + 0.42*LSF*resnew / 150
            rsd1(j)%tot_meta%n = rsd1(j)%tot_meta%n + resnew_ne -         &
                              (0.42 * LSF * resnew / 150) + 1.E-25
          else
            rsd1(j)%tot_str%n = rsd1(j)%tot_str%n + resnew_ne
            rsd1(j)%tot_meta%n = rsd1(j)%tot_meta%n + 1.E-25
          end if	

          rsd1(j)%tot_meta%c = rsd1(j)%tot_meta%c + 0.42 * LMF * resnew

          !update no3 and nh3 in soil
          soil1(j)%mn(1)%no3 = soil1(j)%mn(1)%no3 * (1-sf)
          soil1(j)%mn(1)%nh4 = soil1(j)%mn(1)%nh4 * (1-sf)
        end if

        rsd1(j)%tot(ipl)%m = rsd1(j)%tot(ipl)%m + resnew
        rsd1(j)%tot(ipl)%m = Max(rsd1(j)%tot(ipl)%m, 0.)
        rsd1(j)%tot(ipl)%n = resnew * pcom(j)%plm(ipl)%n_fr + rsd1(j)%tot(ipl)%n
        rsd1(j)%tot(ipl)%p = resnew * pcom(j)%plm(ipl)%p_fr + rsd1(j)%tot(ipl)%p
        
        pl_mass(j)%tot(ipl)%m = pl_mass(j)%tot(ipl)%m - resnew
        pl_mass(j)%tot(ipl)%n = pl_mass(j)%tot(ipl)%n - resnew * pcom(j)%plm(ipl)%n_fr
        pl_mass(j)%tot(ipl)%p = pl_mass(j)%tot(ipl)%p - resnew * pcom(j)%plm(ipl)%p_fr
        
        !pcom(j)%plstr(ipl)%strsw = 1.
        !pcom(j)%plg(ipl)%lai = pldb(idp)%alai_min
        !pcom(j)%plcur(ipl)%phuacc = 0.
        !pcom(j)%plg(ipl)%laimxfr = 0.

      return
      end subroutine pl_leaf_drop