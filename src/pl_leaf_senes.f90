      subroutine pl_leaf_senes
      
      use plant_data_module
      use basin_module
      use hru_module, only : hru, ihru, ipl
      use plant_module
      use plant_data_module
      use carbon_module
      use organic_mineral_mass_module
      use climate_module
      use hydrograph_module
      
      implicit none 
      
      integer :: j = 0          !none               |HRU number
      integer :: idp = 0        !                   |
      integer :: iob = 0        !                   |
      integer :: iwgn = 0       !                   |
      real :: rto = 0.          !none               |ratio of current years of growth:years to maturity of perennial
      real :: ppet = 0.         !mm/mm              |running average of precip over pet
      real :: leaf_tov_mon = 0. !months             |leaf turnover rate months
      real :: coef = 0.         !                   |coefficient for ppet - leaf turnover equation
      real :: exp_co = 0.       !                   |exponent for ppet - leaf turnover equation
      real :: lai_init = 0.     !                   |lai before senescence
      real :: lai_drop = 0.     !                   |lai decline due to senescence
      
      j = ihru
      idp = pcom(j)%plcur(ipl)%idplt
      
      leaf_drop%m = 0.
      
      !! lai decline for annuals - if dlai < phuacc < 1
      if (pldb(idp)%typ == "warm_annual" .or. pldb(idp)%typ == "cold_annual" .or.  &
             pldb(idp)%typ == "warm_annual_tuber" .or. pldb(idp)%typ == "cold_annual_tuber") then
        if (pcom(j)%plcur(ipl)%phuacc > pldb(idp)%dlai .and. pcom(j)%plcur(ipl)%phuacc < 1.) then
          lai_init = pcom(j)%plg(ipl)%lai
          rto = (1. - pcom(j)%plcur(ipl)%phuacc) / (1. - pcom(j)%plg(ipl)%dphu)
          pcom(j)%plg(ipl)%lai = pcom(j)%plg(ipl)%olai * rto ** pldb(idp)%dlai_rate
          
          !! compute leaf biomass drop
          !if (lai_init > 1.e-6) then
          !  lai_drop = (lai_init - pcom(j)%plg(ipl)%lai) / lai_init
          !  lai_drop = max (0., lai_drop)
          !  lai_drop = amin1 (1., lai_drop)
          !  leaf_drop%m = lai_drop * pl_mass(j)%leaf(ipl)%m
          !  leaf_drop%n = leaf_drop%m * pcom(j)%plm(ipl)%n_fr
          !  leaf_drop%n = max (0., leaf_drop%n)
          !  leaf_drop%p = leaf_drop%m * pcom(j)%plm(ipl)%p_fr
          !  leaf_drop%p = max (0., leaf_drop%p)
          !end if
        end if
      end if
      
      !! lai decline for temperature based perennials
      if (pldb(idp)%typ == "perennial" .and. pldb(idp)%trig == "temp_gro") then
        if (pcom(j)%plcur(ipl)%phuacc > pldb(idp)%dlai .and. pcom(j)%plg(ipl)%d_senes < 15.) then
          iob = hru(j)%obj_no
          iwst = ob(iob)%wst
          lai_init = pcom(j)%plg(ipl)%lai
          !! use 15 day senescence period
          pcom(j)%plg(ipl)%d_senes = pcom(j)%plg(ipl)%d_senes + 1.
          rto = 1. - (pcom(j)%plg(ipl)%d_senes / 15.)     !! assume 15 day senescence and leaf drop
          pcom(j)%plg(ipl)%lai = (pcom(j)%plg(ipl)%olai - pldb(idp)%alai_min) * rto + pldb(idp)%alai_min
          pcom(j)%plg(ipl)%lai = max (pcom(j)%plg(ipl)%lai, pldb(idp)%alai_min)
          !! logistic decline rate - Strauch and Volk
          !pcom(j)%plg(ipl)%lai = (pcom(j)%plg(ipl)%olai - pldb(idp)%alai_min) /   &
          !      (1. + Exp((rto - .5) * (-12))) + pldb(idp)%alai_min
          !if (j==1866) then
          !write (2222, *) time%day, time%mo, time%yrc, j, pcom(j)%plg(ipl)%lai, pl_mass(j)%leaf(ipl)%m
          !end if
          !! compute leaf biomass drop
          if (lai_init > 0.05) then
            lai_drop = (lai_init - pcom(j)%plg(ipl)%lai) / lai_init
          else
            lai_drop = 0.
          end if
          lai_drop = max (0., lai_drop)
          lai_drop = amin1 (1., lai_drop)
          !! forest -- total tree n_conc = 1.75%; leaf  = 2.25%, falling leaf = 50%*2.25% = 1.12% --> 1.12/1.75 = 0.68
          leaf_drop%m = pcom(j)%plcur(ipl)%leaf_tov * pl_mass(j)%leaf(ipl)%m
          leaf_drop%n = 0.68 * leaf_drop%m * pcom(j)%plm(ipl)%n_fr
          leaf_drop%n = max (0., leaf_drop%n)
          leaf_drop%p = 0.68 * leaf_drop%m * pcom(j)%plm(ipl)%p_fr
          leaf_drop%p = max (0., leaf_drop%p)
        end if
      end if
      
      !! lai decline for moisture based perennials - use f(P/PET) to estimate drought stress
      if (pldb(idp)%typ == "perennial" .and. pldb(idp)%trig == "moisture_gro") then
        iob = hru(j)%obj_no
        iwst = ob(iob)%wst
        iwgn = wst(iwst)%wco%wgn
        !! linear lai decline based on soil moisture (max loss at p/pet=0.1, min loss at p/pet=0.5)
        ppet = wgn_pms(iwgn)%precip_sum / wgn_pms(iwgn)%pet_sum
        if (ppet < 0.5) then
          coef = 1. ! / .36
          exp_co = -10. * ppet + 6.
          leaf_tov_mon = coef * exp (-exp_co) * (pldb(idp)%leaf_tov_min - pldb(idp)%leaf_tov_max) + pldb(idp)%leaf_tov_max
          !leaf_tov_mon = pldb(idp)%leaf_tov_min - (0.5 - ppet) * (pldb(idp)%leaf_tov_min - pldb(idp)%leaf_tov_max) / 0.4
        else
          leaf_tov_mon = pldb(idp)%leaf_tov_min
        end if
        leaf_tov_mon = amin1 (leaf_tov_mon, pldb(idp)%leaf_tov_min)
        leaf_tov_mon = max (leaf_tov_mon, pldb(idp)%leaf_tov_max)
        !! daily turnover - from monthly turnover rate
        pcom(j)%plcur(ipl)%leaf_tov = (1. / (30. * leaf_tov_mon))
        
        !! assume an lai-biomass relationship - linear with slope = 0.0002 LAI/leaf biomass(kg/ha) ***should be plant parm in plants.plt
        pcom(j)%plg(ipl)%lai = pcom(j)%plg(ipl)%lai - pcom(j)%plcur(ipl)%leaf_tov
        pcom(j)%plg(ipl)%lai = max (pcom(j)%plg(ipl)%lai, pldb(idp)%alai_min)
        
        !! compute leaf biomass drop
        !! forest -- total tree n_conc = 1.75%; leaf  = 2.25%, falling leaf = 50%*2.25% = 1.12% --> 1.12/1.75 = 0.68
        leaf_drop%m = pcom(j)%plcur(ipl)%leaf_tov * pl_mass(j)%leaf(ipl)%m
        leaf_drop%n = 0.68 * leaf_drop%m * pcom(j)%plm(ipl)%n_fr
        leaf_drop%n = max (0., leaf_drop%n)
        leaf_drop%p = 0.68 * leaf_drop%m * pcom(j)%plm(ipl)%p_fr
        leaf_drop%p = max (0., leaf_drop%p)
        
      end if
          
      !! update plant and residue masses
      if (leaf_drop%m > 0.) then
        !! update individual masses
        pl_mass(j)%leaf(ipl) = pl_mass(j)%leaf(ipl) - leaf_drop
        pl_mass(j)%tot(ipl) = pl_mass(j)%tot(ipl) - leaf_drop
        pl_mass(j)%ab_gr(ipl) = pl_mass(j)%ab_gr(ipl) - leaf_drop
        
        soil1(j)%rsd(1) = soil1(j)%rsd(1) + leaf_drop
        if (bsn_cc%cswat == 2) then
          soil1(j)%meta(1) = soil1(j)%meta(1) + 0.85 * leaf_drop
          soil1(j)%str(1) = soil1(j)%str(1) + 0.15 * leaf_drop
          soil1(j)%lig(1) = soil1(j)%lig(1) + 0.12 * leaf_drop
        end if
        
        !! update total community masses
        pl_mass(j)%tot_com = pl_mass(j)%tot_com - leaf_drop
        pl_mass(j)%ab_gr_com = pl_mass(j)%ab_gr_com - leaf_drop
        pl_mass(j)%leaf_com = pl_mass(j)%leaf_com - leaf_drop
        
        hrc_d(j)%plant_surf_c = hrc_d(j)%plant_surf_c + leaf_drop%c
        hpc_d(j)%drop_c = hpc_d(j)%drop_c + leaf_drop%c
      end if
      
      return
      end subroutine pl_leaf_senes