        subroutine cbn_zhang2
    
        use hru_module, only : ihru, tillage_days, tillage_depth, tillage_factor, tillage_switch
        use soil_module
        use basin_module
        use organic_mineral_mass_module
        use carbon_module
        use output_landscape_module
        use time_module
        
        implicit none
        
        !!transput variables;
        !!  std(:)          : standing dead (kg ha-1)                                               (not used)
        !!  stdl(:)         : mass of lignin in standing dead (kg ha-1)                             (not used)
        !!  stdn(:)         : mass of n in standing dead (dead plants + sorbed from soil; kg ha-1)  (not used)
        !!  stdnel(:)       : standing dead n after enrichment with sorbed n in a soil layer (kg ha-1)

        !!==============================================
        !! local variables
       !rnmn
       !abco2   : allocation from biomass to co2; 0.6 (surface litter), 0.85–0.68*(claf + silf) (all other layers) (parton et al., 1993, 1994)
       !abl     : carbon allocation from biomass to leaching; abl = (1-exp(-f/(0.01* sw+ 0.1*(kdbm)*db)) (williams, 1995)
       !abp     : allocation from biomass to passive humus; 0 (surface litter), 0.003 + 0.032*claf (all other layers) (parton et al., 1993, 1994)
       !almco2  : allocation from metabolic litter to co2; 0.6 (surface litter), 0.55 (all other layers) (parton et al., 1993, 1994)
       !alslco2 : allocation from lignin of structural litter to co2; 0.3 (parton et al., 1993, 1994)
       !alslnco2: allocation from non-lignin of structural litter to co2; 0.6 (surface litter), 0.55 (all other layers) (parton et al., 1993, 1994)
       !apco2   : allocation from passive humus to co2; 0.55 (parton et al., 1993, 1994)
       !asco2   : allocation from slow humus to co2; 0.55 (parton et al., 1993, 1994)
       !asp     : allocation from slow humus to passive; 0 (surface litter), 0.003-0.009*claf (all other layers) (parton et al., 1993, 1994)
       !bmc     : mass of c in soil microbial biomass and associated products (kg ha-1)
       !bmctp   : potential transformation of c in microbial biomass (kg ha-1 day-1)
       !bmn     : mass of n in soil microbial biomass and associated products (kg ha-1)
       !bmntp   : potential transformation of n in microbial biomass (kg ha-1 day-1)
       !bmr     : rate of transformation of microbial biomass and associated products under optimal
       !            conditions (surface = 0.0164 day-1; all other layers = 0.02 day-1) (parton et al., 1993, 1994)       
       !cf      : carbon fraction of organic materials 0.42; from data of pinck et al., 1950)
       !cdg     : soil temperature control on biological processes
       !cnr     : c/n ratio of standing dead
       !cpn1    : potential n deficit resulting from the transformation of structural litter; calc as (pn1+pn2)-lsntp if lsntp < (pn1+pn2), otherwise = 0 (kg n ha-1 day-1)
       !cpn2    : potential n deficit resulting from the transformation of metabolic litter; calc as pn3-lmntp if lmntp < p3, otherwise = 0 (kg n ha-1 day-1)
       !cpn3    : potential n deficit resulting from the transformation of microbial biomass; calc as (pn5+pn6)-bmntp if bmntp < (pn5+pn6), otherwise = 0 (kg n ha-1 day-1)
       !cpn4    : potential n deficit resulting from the transformation of slow humus; calc as (pn7+pn8)-hsntp if hsntp < (pn7+pn8), otherwise = 0 (kg n ha-1 day-1)
       !cpn5    : potential n deficit resulting from the transformation of passive humus; calc as pn9-hpntp if hpntp < pn9, otherwise = 0 (kg n ha-1 day-1)
       !cs      : combined factor controlling biological processes [cs = sqrt(cdg×sut)* 0.8*ox*x1), cs < 10; cs = 10, cs>=10 (williams, 1995)]
       !dbp     : soil bulk density of plow layer (mg m-3) (not used)
       !hsctp   : potential transformation of c in slow humus (kg ha-1 day-1)
       !hsntp   : potential transformation of n in slow humus (kg ha-1 day-1)
       !hpctp   : potential transformation of c in passive humus (kg ha-1 day-1)
       !hpntp   : potential transformation of n in passive humus (kg ha-1 day-1)
       !hpr     : rate of transformation of passive humus under optimal conditions (subsurface
                !layers = 0.000012 day-1) (parton et al.,1993, 1994)
       !hsr     : rate of transformation of slow humus under optimal conditions (all layers
                != 0.0005 day-1) (parton et al., 1993, 1994; vitousek et al., 1993)
       !koc     : liquid–solid partition coefficient for microbial biomass (10^3 m^3 mg-1)     
       !lmf     : fraction of the litter that is metabolic    
       !lmnf    : fraction of metabolic litter that is n (kg kg-1)  
       !lmr     : rate of transformation of metabolic litter under optimal conditions (surface =
                !0.0405 day-1; all other layers = 0.0507 day-1) (parton et al., 1994)
       !lmctp   : potential transformation of c in metabolic litter (kg ha-1 day-1)
       !lmntp   : potential transformation of n in metabolic litter (kg ha-1 day-1)
       !lsctp   : potential transformation of c in structural litter (kg ha-1 day-1)
       !lsf     : fraction of the litter that is structural
       !lslf    : fraction of structural litter that is lignin (kg kg-1)
       !lsnf    : fraction of structural litter that is n (kg kg-1)
       !lslctp  : potential transformation of c in lignin of structural litter (kg ha-1 day-1)
       !lslnctp : potential transformation of c in nonlignin structural litter (kg ha-1 day-1)  
       !lsntp   : potential transformation of n in structural litter (kg ha-1 day-1)
       !lsr     : rate of potential transformation of structural litter under optimal conditions
                !(surface = 0.0107 day-1; all other layers= 0.0132 day-1) (parton et al., 1994)
       !ncbm    : n/c ratio of biomass
       !nchp    : n/c ratio passive humus
       !nchs    : n/c ratio of the slow humus
       !ox      : oxygen control on biological processes with soil depth
       !pn1     : potential n demand resulting from the transformation from structural litter to microbial biomass (kg n ha-1 day-1)
       !pn2     : decomposition rate of humus p; hmp_rate = 1.4* (hsnta + hpnta)/(sol_hsn(k,j) + sol_hpn(k,j) + 1.e-6)
       !pn3     : potential n demand resulting from the transformation from metabolic litter to microbial biomass (kg n ha-1 day-1)
       !pn4     : biomass to leaching (calculated in ncsed_leach) (kg n ha-1 day-1)
       !pn5     : potential n demand resulting from the transformation from microbial biomass to passive (kg n ha-1 day-1)
       !pn6     : potential n demand resulting from the transformation from microbial biomass to slow (kg n ha-1 day-1)
       !pn7     : potential n demand resulting from the transformation from slow humus to microbial biomass (kg n ha-1 day-1)
       !pn8     : potential n demand resulting from the transformation from slow humus to passive (kg n ha-1 day-1)
       !pn9     : potential n demand resulting from the transformation from passive to microbial biomass (kg n ha-1 day-1)
       !sf      : fraction of mineral n sorbed to litter: 0.05 for surface litter, 0.1 for belowground litter 
       !sum1    : potential n supply resulting from the transformation of structural litter; calc as lsntp-(pn1+pn2) if lsntp > (pn1+pn2), otherwise = 0 (kg n ha-1 day-1)
       !sum2    : potential n supply resulting from the transformation of structural litter; calc as lmntp-pn3 if lmntp > p3, otherwise = 0 (kg n ha-1 day-1)
       !sum3    : potential n supply resulting from the transformation of metabolic litter; calc as bmntp-(pn5+pn6) if bmntp > (pn5+pn6), otherwise = 0 (kg n ha-1 day-1)
       !sum4    : potential n supply resulting from the transformation of slow humus; calc as hsntp-(pn7+pn8) if hsntp > (pn7+pn8), otherwise = 0 (kg n ha-1 day-1)
       !sum5    : potential n supply resulting from the transformation of passive humus; calc as hpntp-pn9 if hpntp > pn9, otherwise = 0 (kg n ha-1 day-1)
       !sut     : soil water control on biological processes 
       !x1      : tillage control on residue decomposition (not used)
       !xbmt    : control on transformation of microbial biomass by soil texture and structure.
                !its values: surface litter layer = 1; all other layers = 1-0.75*(silf + claf) (parton et al., 1993, 1994)
       !xlslf   : control on potential transformation of structural litter by lignin fraction
                !of structural litter [xlslf = exp(-3* lslf) (parton et al., 1993, 1994)]
       !prmt_51 !coef adjusts microbial activity function in top soil layer (0.1_1.)
       
       integer :: j              !                     |number of hru
       integer :: k              !none                 |counter
       integer :: kk             !                     |
       integer :: lmnta          !                     |      
       integer :: min_n_ppm      !                     |
       integer :: lslncat        !                     |
       integer :: min_n          !                     |
       real :: sol_mass          !                     |
       real :: sol_min_n         !                     |
       real :: fc                !mm H2O               |amount of water available to plants in soil layer at field capacity (fc - wp),Index:(layer,HRU)
       real :: wc                !none                 |scaling factor for soil water impact on daily
       real :: sat               !                     |
       real :: void              !                     |
       real :: cdg               !                     |soil temperature control on biological processes
       real :: x3                !none                 |amount of c transformed from passive, slow, metabolic, and non-lignin structural pools to microbial pool
       real :: lscta             !                     |
       real :: lslcta            !                     |
       real :: lslncta           !                     |
       real :: lsnta             !                     |
       real :: lmcta             !                     |
       real :: nf                !                     |
       real :: a1                !                     |
       real :: asx               !                     |
       real :: apx               !                     |
       real :: a1co2             !                     |
       real :: df1               !                     |
       real :: df2               !                     |
       real :: snmn              !
       real :: df3               !                     |
       real :: df4               !                     |
       real :: df5               !                     |
       real :: df6               !                     |
       real :: add               !                     |
       real :: adf1              !                     |
       real :: adf2              !                     |
       real :: adf3              !                     |
       real :: adf4              !                     |
       real :: adf5              !                     |
       real :: tot               !                     |
       real :: pn1               !                     |
       real :: pn2               !                     |
       real :: pn3               !                     |
       real :: pn4               !                     |
       real :: pn5               !                     |
       real :: pn6               !                     |
       real :: pn7               !                     |
       real :: pn8               !                     |
       real :: pn9               !                     |
       real :: cpn1              !                     |
       real :: cpn2              !                     |
       real :: cpn3              !                     |
       real :: cpn4              !                     |
       real :: cpn5              !                     |
       real :: wmin              !                     |
       real :: dmdn              !                     |
       real :: wdn               !kg N/ha              |amount of nitrogen lost from nitrate pool in
       real :: deltawn           !                     |
       real :: deltabmc          !                     |
       real :: snta              !                     |
       real :: till_eff          !                     |
       real :: rlr               !                     |
       real :: xbm               !                     |
       real :: bmcta             !                     |
       real :: bmnta             !                     |
       real :: hscta             !                     |
       real :: hsnta             !                     |
       real :: hpcta             !                     |
       real :: hpnta             !                     |
       real :: fcgd              !                     |
       real :: rsdn_pct          !                     |
       real :: sum               !                     |
       real :: sum1              !                     |potential n supply resulting from the transformation of structural litter; calc as lsntp-(pn1+pn2) if lsntp > (pn1+pn2), otherwise = 0 (kg n ha-1 day-1)
       real :: sum2              !                     |
       real :: sum3              !                     |
       real :: sum4              !                     |
       real :: sum5              !                     |
       real :: reduc             !none                 |fraction of water uptake by plants achieved
       real :: rnmn              !                     |
       real :: hmp_rate          !                     |
       real :: hmp               !kg P/ha              |amount of phosphorus moving from the organic
       real :: decr              !                     |
       real :: rmp               !kg P/ha              |amount of phosphorus moving from fresh organic
       real :: rto               !none                 |cloud cover factor
       real :: rspc              !                     |
       real :: xx                !varies    |variable to hold calculation results
       
       !! initialize local variables
       deltawn = 0.
       deltabmc = 0.   
       wdn = 0.   
       org_con%x1 = 0.
       x3 = 0.
       xx = 0.
       fc = 0.
       wc = 0.
       sat = 0.
       void = 0.
       org_con%sut = 0.
       org_con%cdg = 0.
       org_con%ox = 0.
       org_con%cs = 0.
       org_frac%lmf = 0.
       org_frac%lsf = 0.
       org_frac%lslf = 0.
       org_con%xlslf = 0.
       carbdb%str_rate = 0.
       org_tran%lsctp = 0.
       lscta = 0.
       lslcta = 0.
       lslncta = 0.
       snta = 0.
       lmcta = 0.
       lmnta = 0.
       bmcta = 0.
       bmnta = 0.
       hscta = 0.
       hsnta = 0. 
       hpcta = 0.
       hpnta = 0.
       org_tran%lslctp = 0.
       org_tran%lslnctp = 0.
       org_tran%lsntp= 0.
       carbdb%meta_rate= 0.
       org_tran%lmctp = 0.
       org_tran%lmntp = 0.
       carbdb%microb_rate = 0.
       org_con%xbmt = 0.
       org_tran%bmctp = 0.
       carbdb%hs_rate = 0.
       org_tran%hsctp = 0.
       org_tran%hsntp = 0.
       carbdb%hp_rate = 0.
       org_tran%hpctp = 0.
       org_tran%hpntp = 0.
       org_ratio%nchp = 0.
       nf = 0.
       org_ratio%ncbm = 0.
       org_ratio%nchs = 0.
       org_allo%alslco2 = 0.
       org_allo%alslnco2 = 0.
       org_allo%almco2 = 0.
       org_allo%abco2 = 0.
       a1co2 = 0.
       org_allo%apco2= 0.
       org_allo%asco2 = 0.
       org_allo%abp = 0.
       org_allo%asp = 0.
       a1 = 0.
       asx = 0.
       apx = 0.
       df1 = 0.
       df2 = 0.
       snmn = 0.
       df3 = 0.
       df4 = 0.
       df5 = 0.
       df6 = 0.
       add = 0.
       adf1 = 0.
       adf2 = 0.
       adf3 = 0.
       adf4 = 0.
       adf5 = 0.
       pn1 = 0.
       pn2 = 0.
       pn3 = 0.
       pn4 = 0.
       pn5 = 0.
       pn6 = 0.
       pn7 = 0.
       pn8 = 0.
       pn9 = 0.
       tot = 0.
       sum = 0.
       cpn1 = 0.
       cpn2 = 0.
       cpn3 = 0.
       cpn4 = 0.
       cpn5 = 0.
       wmin = 0.
       dmdn = 0.

       j = ihru
        
      !calculate tillage factor using dssat
      if (tillage_switch(j) .eq. 1 .and. tillage_days(j) .le. 30) then
         tillage_factor(j) = 1.6
      else
         tillage_factor(j) = 1.0
      end if	

      !!calculate c/n dynamics for each soil layer
      !!===========================================
      do k = 1, soil(j)%nly
        if (k == 1) then
          !10 cm / 1000 = 0.01m; 1 ha = 10000 m2; ton/m3; * 1000 --> final unit is kg/ha; rock fraction is considered
          sol_mass = (10) / 1000.* 10000. * soil(j)%phys(k)%bd * 1000. * (1. - soil(j)%phys(k)%rock / 100.)            
        else
          sol_mass = (soil(j)%phys(k)%d - soil(j)%phys(k-1)%d) / 1000. * 10000. * soil(j)%phys(k)%bd * 1000. * &
                    (1- soil(j)%phys(k)%rock / 100.)
        end if        
         
        ! if k = 1, then using temperature, soil moisture in layer 2 to calculate decomposition factor
        if (k == 1) then
          kk = 2
        else
          kk = k
        end if
          
        !! mineralization can occur only if temp above 0 deg
        !check sol_st soil water content in each soil ayer mm h2o
        if (soil(j)%phys(k)%tmp > 0. .and. soil(j)%phys(k)%st > 0.) then
          !!compute soil water factor - sut
          fc = soil(j)%phys(k)%fc + soil(j)%phys(k)%wpmm        ! units mm
          wc = soil(j)%phys(k)%st + soil(j)%phys(k)%wpmm        ! units mm
          sat = soil(j)%phys(k)%ul + soil(j)%phys(k)%wpmm       ! units mm
          void = soil(j)%phys(k)%por * (1. - wc / sat)          ! fraction

          if (wc - soil(j)%phys(k)%wpmm < 0.) then
            org_con%sut = .1 * (soil(j)%phys(kk)%st /soil(j)%phys(k)%wpmm) ** 2
          else
            org_con%sut = .1 + .9 * sqrt(soil(j)%phys(k)%st / soil(j)%phys(k)%fc)
          end if             
          org_con%sut = min(1., org_con%sut)
          org_con%sut = max(.05, org_con%sut)
 
          !compute tillage factor (till_eff) from armen
          till_eff = 1.0
          
          !calculate tillage factor using dssat
          if (tillage_switch(j) .eq. 1 .and. tillage_days(j) .le. 30) then
            if (k == 1) then
              till_eff = 1.6
            else
              if (soil(j)%phys(k)%d .le. tillage_depth(j)) then
                till_eff = 1.6
              else if (soil(j)%phys(k-1)%d .lt. tillage_depth(j)) then
                till_eff = 1.0 + 0.6 * (tillage_depth(j) - soil(j)%phys(k-1)%d) / (soil(j)%phys(k)%d - soil(j)%phys(k-1)%d)
              end if		         
            end if
          else
            till_eff = 1.0
          end if	

          !!compute soil temperature factor - when sol_tep is larger than 35, cdg is negative?
          org_con%cdg = soil(j)%phys(k)%tmp / (soil(j)%phys(k)%tmp + exp(5.058459 - 0.2503591 * soil(j)%phys(k)%tmp))
          org_con%cdg = fcgd(soil(j)%phys(k)%tmp)

          !!compute oxygen (ox)
          org_con%ox = 1. - 0.8 * ((soil(j)%phys(kk)%d + soil(j)%phys(kk-1)%d) / 2) / (((soil(j)%phys(kk)%d + &
             soil(j)%phys(kk-1)%d) / 2) + exp(18.40961 - 0.023683632 * ((soil(j)%phys(kk)%d + soil(j)%phys(kk-1)%d) / 2))) 
          
          !! compute combined factor
          org_con%cs = min(10., sqrt(org_con%cdg * org_con%sut) * 0.9* org_con%ox * till_eff) 
          
          !! call denitrification (to use void and cdg factor)
          !wdn = 0.
          !org_con%cdg = fcgd(soil(j)%phys(k)%tmp)
          !if (org_con%cdg > 0. .and. void <= 0.1) then
          !  call nut_denit(k, j, org_con%cdg, wdn, void)
          !end if
          
          if (org_con%sut >= bsn_prm%sdnco) then
            wdn = soil1(j)%mn(k)%no3 * (1.-Exp(-bsn_prm%cdn * org_con%cdg * soil1(j)%cbn(k) / 100.))
          else
            wdn = 0.
          endif
          soil1(j)%mn(k)%no3 = max(0.0001,soil1(j)%mn(k)%no3 - wdn)
          hnb_d(j)%denit = hnb_d(j)%denit + wdn
          sol_min_n = soil1(j)%mn(k)%no3 + soil1(j)%mn(k)%nh4
              
          !lignin content in structural litter (fraction)          
          rlr = min(0.8, soil1(j)%lig(k)%m / (soil1(j)%str(k)%m + 1.e-5))  

          !carbdb%hs_rate=prmt(47) !century slow humus transformation rate d^-1(0.00041_0.00068) original value = 0.000548,
          carbdb%hs_rate = 5.4799998e-04
          
          !carbdb%hp_rate=prmt(48) !century passive humus transformation rate d^-1(0.0000082_0.000015) original value = 0.000012 
          carbdb%hp_rate = 1.2000000e-05

          ! set nitrogen carbon ratios for upper layer
          if (k == 1) then
            org_con%cs = org_con%cs * carbdb%microb_top_rate
            org_allo%abco2 = .55
            a1co2 = .55
            carbdb%microb_rate = .0164
            carbdb%meta_rate = .0405
            carbdb%str_rate = .0107
            org_ratio%nchp = .1
            xbm = 1.
            ! compute n/c ratios - relative nitrogen content in residue
            rsdn_pct = 0.1 * (rsd1(j)%tot_str%n + rsd1(j)%tot_meta%n) / (rsd1(j)%tot_com%c / 1000. + 1.e-5)
            if (rsdn_pct > 2.) then
              org_ratio%ncbm = .1
              org_ratio%nchs = org_ratio%ncbm / (5. * org_ratio%ncbm + 1.)
              org_allo%abp = .003 + .00032 * soil(j)%phys(k)%clay
            end if
            if (rsdn_pct > .01 .and. rsdn_pct <= 2.) then
              org_ratio%ncbm = 1. / (20.05 - 5.0251 * rsdn_pct)
              org_ratio%nchs = org_ratio%ncbm / (5. * org_ratio%ncbm + 1.)
              org_allo%abp = .003 + .00032 * soil(j)%phys(k)%clay
            else
              org_ratio%ncbm = .05
              org_ratio%nchs = org_ratio%ncbm / (5. * org_ratio%ncbm + 1.)
              org_allo%abp = .003 + .00032 * soil(j)%phys(k)%clay
            end if    
          else
            ! set nitrogen carbon ratios for lower layers
            org_allo%abco2 = 0.17 + 0.0068 * soil(j)%phys(k)%sand
            a1co2 = .55
            carbdb%microb_rate = .02
            carbdb%meta_rate = .0507
            carbdb%str_rate = .0132
            xbm = .25 + .0075 * soil(j)%phys(k)%sand
             min_n_ppm = 1000. * sol_min_n / (sol_mass / 1000)
            if (min_n_ppm > 7.15) then
              org_ratio%ncbm = .33
              org_ratio%nchs = .083
              org_ratio%nchp = .143       
            else
              org_ratio%ncbm = 1. / (15. - 1.678 * min_n_ppm)
              org_ratio%nchs = 1. / (20. - 1.119 * min_n_ppm)
              org_ratio%nchp = 1. / (10. - .42 * min_n_ppm)
            end if
            org_allo%abp = .003 + .00032 * soil(j)%phys(k)%clay
          end if          

          !coef in century eq allocating slow to passive humus(0.001_0.05) original value = 0.003,
              carbdb%hs_hp = 5.0000001e-02
              org_allo%asp = max(.001, carbdb%hs_hp - .00009 * soil(j)%phys(k)%clay)
              
        !     potential transformations structural litter
              org_con%x1 = carbdb%str_rate * org_con%cs * exp(-3. * rlr)
              org_tran%lsctp = org_con%x1 * soil1(j)%str(k)%c
              org_tran%lslctp = org_tran%lsctp * rlr
              org_tran%lslnctp = org_tran%lsctp * (1.-rlr)
              org_tran%lsntp=org_con%x1 * soil1(j)%str(k)%n
              
        !     potential transformations metabolic litter
              org_con%x1 = carbdb%meta_rate * org_con%cs
              org_tran%lmctp = soil1(j)%meta(k)%c * org_con%x1
              org_tran%lmntp = soil1(j)%meta(k)%n * org_con%x1
              
        !     potential transformations microbial biomass
              org_con%x1 = carbdb%microb_rate * org_con%cs * xbm
              org_tran%bmctp = soil1(j)%microb(k)%c * org_con%x1
              org_tran%bmntp = soil1(j)%microb(k)%n * org_con%x1
              
        !     potential transformations slow humus
              org_con%x1 = carbdb%hs_rate * org_con%cs
              org_tran%hsctp = soil1(j)%hs(k)%c * org_con%x1
              org_tran%hsntp = soil1(j)%hs(k)%n * org_con%x1
              
        !     potential transformations passive humus
              org_con%x1 = org_con%cs * carbdb%hp_rate
              org_tran%hpctp = soil1(j)%hp(k)%c * org_con%x1
              org_tran%hpntp = soil1(j)%hp(k)%n * org_con%x1
              
        !     estimate n demand
              a1 = 1.- a1co2
              asx = 1. - org_allo%asco2 - org_allo%asp
              apx=1. - org_allo%apco2            
              pn1 = org_tran%lslnctp*a1 * org_ratio%ncbm                  !structural litter to biomass
              pn2 = .7 * org_tran%lslctp * org_ratio%nchs                 !structural litter to slow
              pn3 = org_tran%lmctp * a1 * org_ratio%ncbm                  !metabolic litter to biomass
              !pn4 = org_tran%bmctp * org_allo%abl * org_ratio%ncbm       !biomass to leaching (calculated in ncsed_leach)
              pn5 = org_tran%bmctp * org_allo%abp * org_ratio%nchp        !biomass to passive
              pn6 = org_tran%bmctp * (1.-org_allo%abp - org_allo%abco2) * org_ratio%nchs     !biomass to slow
              pn7 = org_tran%hsctp * asx * org_ratio%ncbm                 !slow to biomass
              pn8 = org_tran%hsctp * org_allo%asp * org_ratio%nchp        !slow to passive
              pn9 = org_tran%hpctp * apx * org_ratio%ncbm                   !passive to biomass

        !     compare supply and demand for n
              sum = 0.
              sum1 = 0.
              sum2 = 0.
              sum3 = 0.
              sum4 = 0.
              sum5 = 0.
              cpn1 = 0.
              cpn2 = 0.
              cpn3 = 0.
              cpn4 = 0.
              cpn5 = 0.
              org_con%x1 = pn1 + pn2    !rename org_con%x1
              if (org_tran%lsntp < org_con%x1) then
                  cpn1 = org_con%x1 - org_tran%lsntp
              else
                  sum1 = sum1 + org_tran%lsntp - org_con%x1
              end if
              if (org_tran%lmntp < pn3) then
                  cpn2 = pn3 - org_tran%lmntp
              else
                  sum2 = sum2 + org_tran%lmntp - pn3
              end if
              org_con%x1 = pn5 + pn6
              if (org_tran%bmntp < org_con%x1) then
                  cpn3 = org_con%x1 - org_tran%bmntp
              else
                  sum3 = sum3 + org_tran%bmntp - org_con%x1
              end if      
              org_con%x1 = pn7 + pn8
              if (org_tran%hsntp < org_con%x1) then
                  cpn4 = org_con%x1 - org_tran%hsntp
              else
                  sum4 = sum4 + org_tran%hsntp - org_con%x1
              end if
              if (org_tran%hpntp < pn9) then
                  cpn5 = pn9 - org_tran%hpntp
              else
                  sum5 = sum5 + org_tran%hpntp - pn9
              end if
        !     wnh3(isl)=wnh3(isl)+sum
        
              !total available n
              sum = sum1 + sum2 + sum3 + sum4 + sum5
              wmin = max(1.e-5,soil1(j)%mn(k)%no3 + soil1(j)%mn(k)%nh4 + sum)
              
              !total demand for potential tranformaiton of som
              dmdn = cpn1 +cpn2 + cpn3 + cpn4 + cpn5
              
              reduc = 1.
        !     reduce demand if supply limits
        
              if (wmin < dmdn) then
                  reduc = wmin / dmdn
              end if
              
        !     actual transformations
              if (cpn1 > 0.) then
                  lscta = org_tran%lsctp * reduc
                  lsnta = org_tran%lsntp * reduc
                  lslcta = org_tran%lslctp * reduc
                  lslncta = org_tran%lslnctp * reduc
              else
                  lscta = org_tran%lsctp
                  lsnta = org_tran%lsntp
                  lslcta = org_tran%lslctp
                  lslncat = org_tran%lslnctp
              end if
              if (cpn2>0.) then
                  lmcta = org_tran%lmctp * reduc
                  lmnta = org_tran%lmntp * reduc
              else
                  lmcta = org_tran%lmctp
                  lmnta = org_tran%lmntp
              end if
              if (cpn3 > 0.) then
                  bmcta = org_tran%bmctp * reduc
                  bmnta = org_tran%bmntp * reduc
              else
                  bmcta = org_tran%bmctp
                  bmnta = org_tran%bmntp
              end if
              if (cpn4>0.) then
                  hscta = org_tran%hsctp * reduc
                  hsnta = org_tran%hsntp * reduc
              else
                  hscta = org_tran%hsctp
                  hsnta = org_tran%hsntp
              end if
              if (cpn5 > 0.) then
                  hpcta = org_tran%hpctp * reduc
                  hpnta = org_tran%hpntp * reduc
              else
                  hpcta = org_tran%hpctp
                  hpnta = org_tran%hpntp
              end if        
              
              !recalculate demand
              !revised from epic code by zhang
                  pn1 = lslncta * a1 * org_ratio%ncbm                                !structural litter to biomass
                  pn2 = .7 * lslcta * org_ratio%nchs                                 !structural litter to slow
                  pn3 = lmcta * a1 * org_ratio%ncbm                                  !metabolic litter to biomass
                  !pn4=org_tran%bmctp*org_allo%abl*org_ratio%ncbm                    !biomass to leaching (calculated to ncsed_leach)
                  pn5 = bmcta * org_allo%abp * org_ratio%nchp                        !biomass to passive
                  pn6 = bmcta * (1.-org_allo%abp - org_allo%abco2) * org_ratio%nchs  !biomass to slow
                  pn7 = hscta * asx * org_ratio%ncbm                                 !slow to biomass
                  pn8 = hscta * org_allo%asp * org_ratio%nchp                        !slow to passive
                  pn9 = hpcta * apx * org_ratio%ncbm                                 !passive to biomass
                  
            !     compare supply and demand for n
                  sum = 0.
                  sum1 = 0.
                  sum2 = 0.
                  sum3 = 0.
                  sum4 = 0.
                  sum5 = 0.
                  cpn1 = 0.
                  cpn2 = 0.
                  cpn3 = 0.
                  cpn4 = 0.
                  cpn5 = 0.
                  org_con%x1 = pn1 + pn2
                  if (lsnta < org_con%x1) then
                      cpn1 = org_con%x1 - lsnta
                  else
                      sum1 = sum1 + lsnta - org_con%x1
                  end if
                  if (lmnta < pn3) then
                      cpn2 = pn3 - lmnta
                  else
                      sum2 = sum2 + lmnta - pn3
                  end if
                  org_con%x1 = pn5 + pn6
                  if (bmnta < org_con%x1) then
                      cpn3 = org_con%x1 - bmnta
                  else
                      sum3 = sum3 + bmnta - org_con%x1
                  end if      
                  org_con%x1 = pn7 + pn8
                  if (hsnta < org_con%x1) then
                      cpn4 = org_con%x1 - hsnta
                  else
                      sum4 = sum4 + hsnta - org_con%x1
                  end if
                  if (hpnta < pn9) then
                      cpn5 = pn9 - hpnta
                  else
                      sum5 = sum5 + hpnta - pn9
                  end if
                  
                  !total available n
                  sum = sum1 + sum2 + sum3 + sum4 + sum5
                  wmin = max(1.e-5, soil1(j)%mn(k)%no3 + soil1(j)%mn(k)%nh4 + sum)
                  
                  !total demand for potential tranformaiton of som
                  dmdn = cpn1 + cpn2 + cpn3 + cpn4 + cpn5              

              !supply - demand
              rnmn = sum - dmdn
              
        !     update
              if (rnmn > 0.) then
                  soil1(j)%mn(k)%nh4 = soil1(j)%mn(k)%nh4 + rnmn     
	          min_n = soil1(j)%mn(k)%no3 + rnmn
	            if (min_n < 0.) then
	              rnmn = -soil1(j)%mn(k)%no3
	              soil1(j)%mn(k)%no3 = 1.e-10
	            else
	              soil1(j)%mn(k)%no3 = min_n
                end if   
              end if
              
	          ! calculate p flows
              ! compute humus mineralization on active organic p
              hmp_rate = 1.4 * (hsnta + hpnta) / (soil1(j)%hs(k)%n + soil1(j)%hp(k)%n + 1.e-6)
              
              !hmp_rate = 1.4* (hsnta )/(soil1(j)%hs(k)%n + soil1(j)%hp(k)%n + 1.e-6)
              hmp = hmp_rate * soil1(j)%hp(k)%p
              hmp = min(hmp, soil1(j)%hp(k)%p)
              soil1(j)%hp(k)%p = soil1(j)%hp(k)%p - hmp
              soil1(j)%mp(k)%lab = soil1(j)%mp(k)%lab + hmp	          
	
	          !! compute residue decomp and mineralization of 
              !! fresh organic n and p (upper two layers only)  
                decr = (lscta + lmcta) / (soil1(j)%str(k)%c + soil1(j)%meta(k)%c + 1.e-6)
                decr = min(1., decr)
                rmp = decr * soil1(j)%tot(k)%p

                soil1(j)%tot(k)%p = soil1(j)%tot(k)%p - rmp
                soil1(j)%mp(k)%lab = soil1(j)%mp(k)%lab + .8 * rmp
                soil1(j)%hp(k)%p = soil1(j)%hp(k)%p + .2 * rmp	          

              !!!=================================
              !!determine the final rate of the decomposition of each carbon pool and 
              !!allocation of c and nutrients to different som pools, as well as co2 emissions from different pools
	          lscta = min(soil1(j)%str(k)%c, lscta)              
              lslcta = min(soil1(j)%lig(k)%c, lslcta)
              
              org_flux%co2fstr = .3 * lslcta
              org_flux%co2fstr = a1co2 * lslncta
              
              org_flux%cfstrs1 = a1 * lslncta
              org_flux%cfstrs2 = .7 * lslcta
              
              lmcta = min(soil1(j)%meta(k)%c, lmcta)
              org_flux%co2fmet = a1co2 * lmcta
              
              
              org_flux%cfmets1 = a1 * lmcta
              
              org_flux%co2fs1 = org_allo%abco2 * bmcta
              org_flux%co2fs2 = org_allo%asco2 * hscta
              org_flux%co2fs3 = org_allo%apco2 * hpcta
              
              !!!=================================
              !!transformation processes from passive (s3), slow (s2), metabolic (met), and non-lignin structural (str) pools to microbial pool
              
                      !!s3 (passive humus) to s1 (microbial)
                      org_flux%cfs3s1 = apx * hpcta              
                      call nut_np_flow (&
                            soil1(j)%hp(k)%c, soil1(j)%hp(k)%n,          & !input
                            1/org_ratio%ncbm, org_flux%cfs3s1,           & !input
                            org_flux%co2fs3,                             & !input
                            org_flux%efs3s1, org_flux%imms3s1,           & !output
                            org_flux%mnrs3s1)                            !output 
                                    
                      !!s2 (slow humus) to s1 (microbial)
                      org_flux%cfs2s1 = asx * hscta   
                      call nut_np_flow (&
                            soil1(j)%hs(k)%c, soil1(j)%hs(k)%n,          & !input
                            1/org_ratio%ncbm, org_flux%cfs2s1,           & !input
                            org_flux%co2fs2,                             & !input
                            org_flux%efs2s1, org_flux%imms2s1,           & !output
                            org_flux%mnrs2s1)                            !output              
                      
                      !!metabolic litter to s1 (microbial)
                      org_flux%cfmets1 = a1 * lmcta              
                      call nut_np_flow (&
                             soil1(j)%meta(k)%c, soil1(j)%meta(k)%n,     &  !input
                             1/org_ratio%ncbm, org_flux%cfmets1,         &  !input
                             org_flux%co2fmet,                           &  !input
                             org_flux%efmets1, org_flux%immmets1,        &  !output
                             org_flux%mnrmets1)                          !output             
                         
                      !!structural to s1   
                      org_flux%cfstrs1 = a1 * lslncta        
                      call nut_np_flow (                                     &
                             soil1(j)%str(k)%c, soil1(j)%str(k)%n,       &  !input
                             1/org_ratio%ncbm, org_flux%cfstrs1,         &  !input
                             org_flux%co2fstr,                           &  !input
                             org_flux%efstrs1, org_flux%immstrs1,        &  !output
                             org_flux%mnrstrs1)                          !output              
              
              !!!=================================
              !!transformation processes from lignin structural (str) and metabolic (met) and  pools to s2 (slow humus)

                      !!str (structrual litter) to s2 (slow humus)
                      org_flux%cfstrs2 = .7 * lslcta              
                      call nut_np_flow (&
                             soil1(j)%str(k)%c, soil1(j)%str(k)%n,       & !input
                             1/org_ratio%nchs, org_flux%cfstrs2,         & !input
                             org_flux%co2fstr,                           & !input
                             org_flux%efstrs2, org_flux%immstrs2,        & !output
                             org_flux%mnrstrs2)                          !output              
                      
                      !!s1 (microbial biomass)to s2 (slow humus)
                      org_flux%cfs1s2 = bmcta * (1. - org_allo%abp - org_allo%abco2)                
                      call nut_np_flow (&
                            soil1(j)%microb(k)%c, soil1(j)%microb(k)%n,     & !input
                            1/org_ratio%nchs, org_flux%cfs1s2,              & !input
                            org_flux%co2fs1,                                & !input
                            org_flux%efs1s2, org_flux%imms1s2,              & !output
                            org_flux%mnrs1s2)                               !output  


              !!!=================================
              !!transformation processes from lignin structural (str) and metabolic (met) and  pools to s2 (slow humus)
                          
                      !!s1 (microbial biomass) to s3 (passive humus)
                      org_flux%cfs1s3 = bmcta * org_allo%abp              

                      call nut_np_flow (&
                            soil1(j)%microb(k)%c, soil1(j)%microb(k)%n,      & !input
                            1/org_ratio%nchs, org_flux%cfs1s3,               & !input
                            -99.0,                                           & !input  
                            org_flux%efs1s3, org_flux%imms1s3,               & !output
                            org_flux%mnrs1s3)                                !output  
                            
                            !-99 is used to here to avoid repeated calculation of 
                            !n supply during co2 emission during the decomposition of microbial biomass
                            ! as this process has been accounted for duing s1 to s2 transormaiton.
                      
                      !!s2 to s3 (passive humus)
                      org_flux%cfs2s3 = hscta * org_allo%asp   
                      call nut_np_flow (&
                            soil1(j)%hs(k)%c, soil1(j)%hs(k)%n,           & !input
                            1/org_ratio%nchp, org_flux%cfs2s3,            & !input
                            -99.0,                                        & !input  
                            org_flux%efs2s3, org_flux%imms2s3,            & !output
                            org_flux%mnrs2s3)                             !output
                            !-99 is used to here to avoid repeated calculation of 
                            !n supply during co2 emission during the decomposition of microbial biomass
                            ! as this process has been accounted for duing s1 to s2 transormaiton.              
              
              !!!=================================
              !!epic procedures (not used): calculating n supply - n demand 
                  !!df1 is the supply of n during structural litter decomposition (lsnta) - demand of n to meet the transformaitons of other pools
                  !! c pools into structural litter (0 as no other pools transformed into structural litter)  
	              df1 = lsnta 
                 
                  !!df2 is the supply of n during metabolic litter decomposition (lsnta) - demand of n to meet the transformaitons of other pools
                  !! c pools into metabolic litter (0 as no other pools transformed into structural litter)  
                  df2 = lmnta

                  !!!=================================              
                  x3 = apx * hpcta + asx * hscta + a1 * (lmcta + lslncta)  
                  !!x3 = amount of c transformed from passive, slow, metabolic, and non-lignin structural pools to microbial pool              
                  df3 = bmnta - org_ratio%ncbm * x3              
                  !!df3 is the supply of n during structural litter decomposition (lsnta) - demand of n to meet the transformaitons of passive, slow, metabolic, and non-lignin structural 
                  !! c pools into microbiomass pool    
                  soil1(j)%microb(k)%c = soil1(j)%microb(k)%c - bmcta + x3

                  !!!================================= 
                  org_con%x1 = .7 * lslcta + bmcta * (1. - org_allo%abp - org_allo%abco2)
                  !!x1 = amount of c transformed from  lignin structural and metabolic pools into slow humus             
                  df4 = hsnta - org_ratio%nchs * org_con%x1               
                  !!df4 is the supply of n during slow humus decomposition (hsnta) - demand of n to meet the transformaitons of lignin structural and metabolic pools 
                  !! c pools into slow humus      
                  soil1(j)%hs(k)%c = soil1(j)%hs(k)%c - hscta + org_con%x1

                  !!!=================================
                  org_con%x1 = hscta * org_allo%asp + bmcta * org_allo%abp
                  !!x1 = amount of c transformed from  s1 (microbial biomass) into s3 (passive humus)
                  df5 = hpnta - org_ratio%nchp * org_con%x1
                  !!df5 is the supply of n during passive humus decomposition (hpnta) - demand of n to meet the transformaitons of microbial biomass 
                  !! c pools into passive humus              
                  soil1(j)%hp(k)%c = soil1(j)%hp(k)%c - hpcta + org_con%x1
                                                               
                  !!!=================================              
                  df6 = sol_min_n - soil1(j)%mn(k)%no3
                  !!df6 supply of mineral n - available mineral n = n demanded from mineral pool
                  
                  !!!=================================
                  add = df1 + df2 + df3 + df4 + df5 + df6
                  adf1 = abs(df1)
                  adf2 = abs(df2)
                  adf3 = abs(df3)
                  adf4 = abs(df4)
                  adf5 = abs(df5)
                  tot = adf1 + adf2 + adf3 + adf4 + adf5
                  xx = add / (tot + 1.e-10)
                  !soil1(j)%str(k)%n=max(.001,soil1(j)%str(k)%n-df1+xx*adf1)
                  !soil1(j)%meta(k)%n=max(.001,soil1(j)%meta(k)%n-df2+xx*adf2)
                  !soil1(j)%microb(k)%n=soil1(j)%microb(k)%n-df3+xx*adf3
                  !soil1(j)%hs(k)%n=soil1(j)%hs(k)%n-df4+xx*adf4
                  !soil1(j)%hp(k)%n = soil1(j)%hp(k)%n - df5 + xx * adf5
              
              !!update c and n of different som pools
              !!=========================================
              soil1(j)%str(k)%c = max(1.e-10, soil1(j)%str(k)%c - lscta)
              soil1(j)%lig(k)%c = max(1.e-10, soil1(j)%lig(k)%c - lslcta)
              soil1(j)%lig(k)%n = max(1.e-10, soil1(j)%lig(k)%n - lslncta)
                            
              soil1(j)%lig(k)%m = max(1.e-10, soil1(j)%lig(k)%m - lslcta / .42)
              soil1(j)%str(k)%m = max(1.e-10, soil1(j)%str(k)%m - lscta / .42)
              
              if (soil1(j)%meta(k)%m > 0.) then
                rto = max(0.42, soil1(j)%meta(k)%c / soil1(j)%meta(k)%m)
                soil1(j)%meta(k)%m = soil1(j)%meta(k)%m - lmcta / rto
                soil1(j)%meta(k)%c = soil1(j)%meta(k)%c - lmcta
              end if
              !soil1(j)%meta(k)%c=max(1.e-10,soil1(j)%meta(k)%c-lmcta)
              !soil1(j)%meta(k)%m=max(1.e-10,soil1(j)%meta(k)%m-lmcta/.42)              
              
              soil1(j)%meta(k)%n = max(.001, soil1(j)%meta(k)%n - org_flux%efmets1 & !subtract n flow from met (metabolic litter) to s1 (microbial biomass)
                            - org_flux%mnrmets1)                    !subtract n immobilization during transformaiton from met (metabolic litter) to s1 (microbial biomass)

              soil1(j)%str(k)%n = max(.001, soil1(j)%str(k)%n - org_flux%efstrs1 & !subtract n flow from str (structural litter) to s1 (microbial biomass)
                            - org_flux%efstrs2                     &!subtract n flow from str (structural litter) to s2 (slow humus)
                            - org_flux%mnrstrs1                    &!subtract mineralization during tansformation from str (structural litter) to s1 (microbial biomass)
                            - org_flux%mnrstrs2)                    !subtract mineralization during tansformation from str (structural litter) to s2 (slow humus)

              soil1(j)%microb(k)%n = soil1(j)%microb(k)%n + org_flux%efmets1         & !add n flow from met (metabolic litter) to s1 (microbial biomass)
                    + org_flux%efstrs1                            & !add n flow from str (structural litter) to s1 (microbial biomass)
                    - org_flux%efs1s2                             & !subtract n flow from s1 (microbial biomass) to s2 (slow humus)
                    - org_flux%efs1s3                             & !subtract n flow from s1 (microbial biomass) to s3 (passive humus)
                    + org_flux%efs2s1                             & !add n flow from s2 (slow humus) to  s1 (microbial biomass)
                    + org_flux%efs3s1                             & !add n flow from s3 (passive humus) to s1 (microbial biomass)                  
                    - org_flux%mnrs1s2                            & !subtract mineralization during tansformation from s1 (microbial biomass) to s2 (slow humus)
                    - org_flux%mnrs1s3                            & !subtract mineralization during tansformation from s1 (microbial biomass) to s3 (passive humus)                   
                    + org_flux%immmets1                           & !add immobilization during transformaiton from met (metabolic litter) to s1 (microbial biomass)
                    + org_flux%immstrs1                           & !add immobilization during transformaiton from str (structural litter) to s1 (microbial biomass)
                    + org_flux%imms2s1                            & !add immobilization during transformaiton from s2 (slow humus) to  s1 (microbial biomass)
                    + org_flux%imms3s1                              !add immobilization during transformaiton from s3 (passive humus) to s1 (microbial biomass)
                    
              soil1(j)%hs(k)%n = soil1(j)%hs(k)%n + org_flux%efstrs2 +       & !add n flow from str (structural litter) to s2 (slow humus)
                    org_flux%efs1s2                               & !add n flow from s1 (microbial biomass) to s2 (slow humus)
                    - org_flux%efs2s1                             & !subtract n flow from s2 (slow humus) to  s1 (microbial biomass)
                    - org_flux%efs2s3                             & ! subtract n flow from s2 (slow humus) to  s3 (passive humus)                  
                    - org_flux%mnrs2s1                            & !subtract mineralization during tansformation from s2 (slow humus) to  s1 (microbial biomass)
                    - org_flux%mnrs2s3                            & !subtract mineralization during tansformation from s2 (slow humus) to  s3 (passive humus)                   
                    + org_flux%immstrs2                           & !add immobilization during tansformation from str (structural litter) to s2 (slow humus)
                    + org_flux%imms1s2                              !add immobilization during tansformation from s1 (microbial biomass) to s2 (slow humus)
              
              soil1(j)%hp(k)%n = soil1(j)%hp(k)%n + org_flux%efs1s3 +        & !add n flow from s1 (microbial biomass) to s3 (passive humus)
                    org_flux%efs2s3                               & !add n flow from s2 (slow humus) to s3 (passive humus)                   
                    - org_flux%efs3s1                             & !subtract n flow from s3 (passive humus) to s1 (microbial biomass)
                    - org_flux%mnrs3s1                            & !subtract mineralization.
                    + org_flux%imms1s3                            & !add immobilization during tansformation from s1 (microbial biomass) to s3 (passive humus)
                    + org_flux%imms2s3                              !add immobilization during tansformation from s2 (slow humus) to s3 (passive humus)
              
              !!update soil respiration
              !!===============================
              !!soil rspc for layer k
              rspc = .3 * lslcta + a1co2 * (lslncta + lmcta) + org_allo%abco2 * bmcta + org_allo%asco2 * hscta + &
                org_allo%apco2 * hpcta
              !!rspc_da is accounting variable summarizing co2 emissions from all soil layers
              hsc_d(j)%rsp_c = hsc_d(j)%rsp_c +  rspc 
              
              !!update other vairables used in swat
              !!==================================
              !soil1(j)%tot(k)%m = soil1(j)%str(k)%m + soil1(j)%meta(k)%m
              !soil1(j)%tot(k)%c = 100. * (soil1(j)%hs(k)%c + soil1(j)%hp(k)%c + soil1(j)%microb(k)%c) / sol_mass 
              soil1(j)%tot(k)%c = soil1(j)%hs(k)%c + soil1(j)%hp(k)%c + soil1(j)%microb(k)%c +      &
                                       soil1(j)%meta(k)%c + soil1(j)%str(k)%c + soil1(j)%lig(k)%c
        end if  !soil temp and soil water > 0.

      end do      !soil layer loop

    return
    end subroutine cbn_zhang2