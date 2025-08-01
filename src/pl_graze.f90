      subroutine pl_graze      
    
      use mgt_operations_module
      use fertilizer_data_module
      use basin_module
      use organic_mineral_mass_module
      use hru_module, only : ihru, grazn, grazp 
      use soil_module
      use plant_module
      use carbon_module
      
      implicit none 

      integer :: j = 0     !none        |HRU number
      integer :: l = 0     !none        |number of soil layer that manure applied
      integer :: it = 0    !none        |manure/fertilizer id from fertilizer.frt
      real :: xx = 0.      !none        |variable to hold intermediate calculation result
      real :: dmi = 0.     !kg/ha       |biomass in HRU prior to grazing and trampling
      real :: zz = 0.      !none        |variable to hold intermediate calculation
                           !            |result
      real :: yz = 0.
      real :: yy = 0.      !none        |variable to hold intermediate calculation
                           !            |result
      real :: xz = 0.      !            |the amount of organic carbon allocated to structural litter C pool  
      real :: x8 = 0.      !            |organic carbon applied (kg C/ha)   
      real :: x10 = 0.     !frac        |the fraction of carbon in fertilizer that is allocated to metabolic litter C pool
      real :: x1 = 0.      !            |fertlizer applied to layer (kg/ha)
      real :: rln = 0.     !            | 
      real :: orgc_f = 0.  !frac        |fraction of organic carbon in fertilizer
      integer :: ipl = 0   !none        |counter
      real :: manure_kg = 0.
      real :: eat_plant = 0.  !frac       |fraction of above ground biomass of each plant eaten
      real :: eat_seed = 0. !frac       |fraction of seed of each plant eaten
      real :: eat_leaf = 0. !frac       |fraction of leaf of each plant eaten
      real :: eat_stem = 0. !frac       |fraction of stem of each plant eaten
      real :: tramp_plant = 0.!frac       |fraction of above ground biomass of each plant trampled
      real :: tramp_seed = 0. !frac       |fraction of seed of each plant trampled
      real :: tramp_leaf = 0. !frac       |fraction of leaf of each plant trampled
      real :: tramp_stem = 0. !frac       |fraction of stem of each plant trampled

      j = ihru

      !! graze only if adequate biomass in HRU
      if (pl_mass(j)%ab_gr_com%m < graze%biomin) return
            
      do ipl = 1, pcom(j)%npl
        !! set initial biomass before eating and trampling
        dmi = pl_mass(j)%ab_gr(ipl)%m
        if (dmi < 1.e-6) exit
        
        !! remove biomass eaten - assume evenly divided by biomass of plant
        !! later we can add preferences - by animal type or simply by n and p content
        eat_plant =  graze%eat / pl_mass(j)%ab_gr_com%m
        eat_plant = amin1 (eat_plant, 1.)
        
        !! remove biomass and organics from plant pools
        !! update remaining plant organic pools
        pl_mass(j)%seed(ipl) = pl_mass(j)%seed(ipl) - eat_plant * pl_mass(j)%seed(ipl)
        pl_mass(j)%leaf(ipl) = pl_mass(j)%leaf(ipl) - eat_plant * pl_mass(j)%leaf(ipl)
        pl_mass(j)%stem(ipl) = pl_mass(j)%stem(ipl) - eat_plant * pl_mass(j)%stem(ipl)
        pl_mass(j)%tot(ipl) = pl_mass(j)%tot(ipl) - eat_plant * pl_mass(j)%ab_gr(ipl)
        pl_mass(j)%ab_gr(ipl) = pl_mass(j)%ab_gr(ipl) - eat_plant * pl_mass(j)%ab_gr(ipl)

        !! remove biomass trampled - assume evenly divided by biomass of plant
        tramp_plant = graze%tramp / pl_mass(j)%ab_gr_com%m
        tramp_plant = 0. !***jga amin1 (tramp_plant, 1.)
        
        !! remove biomass and organics from plant pools
        !! update remaining plant organic pools
        pl_mass(j)%seed(ipl) = pl_mass(j)%seed(ipl) - tramp_plant * pl_mass(j)%seed(ipl)
        pl_mass(j)%leaf(ipl) = pl_mass(j)%leaf(ipl) - tramp_plant * pl_mass(j)%leaf(ipl)
        pl_mass(j)%stem(ipl) = pl_mass(j)%stem(ipl) - tramp_plant * pl_mass(j)%stem(ipl)
        pl_mass(j)%tot(ipl) = pl_mass(j)%tot(ipl) - tramp_plant * pl_mass(j)%ab_gr(ipl)
        pl_mass(j)%ab_gr(ipl) = pl_mass(j)%ab_gr(ipl) - tramp_plant * pl_mass(j)%ab_gr(ipl)

        !! reset leaf area index and fraction of growing season
        if (dmi > 1.) then
          !! assume lai doesn't start decreasing until 2,500 kg/ha at 1.0 lai per 1000 kg/ha
          if (dmi < 2500.) then
            pcom(j)%plg(ipl)%lai = pcom(j)%plg(ipl)%lai - graze%eat / 1000.
            pcom(j)%plcur(ipl)%phuacc = pcom(j)%plcur(ipl)%phuacc * (1. - graze%eat / 1000.)
          end if
        else
          pcom(j)%plg(ipl)%lai = 0.05
          pcom(j)%plcur(ipl)%phuacc = 0.
        endif

      end do    !! plant loop
        
        !! apply manure
        it = graze%manure_id
        manure_kg = graze%manure
        if (manure_kg > 0.) then 
          l = 1
          if (bsn_cc%cswat == 0) then
            soil1(j)%mn(l)%no3 = soil1(j)%mn(l)%no3 + manure_kg * (1. - fertdb(it)%fnh3n) * fertdb(it)%fminn
            soil1(j)%tot(l)%n = soil1(j)%tot(l)%n + manure_kg * fertdb(it)%forgn 
            soil1(j)%mn(l)%nh4 = soil1(j)%mn(l)%nh4 + manure_kg * fertdb(it)%fnh3n * fertdb(it)%fminn
            soil1(j)%mp(l)%lab = soil1(j)%mp(l)%lab + manure_kg * fertdb(it)%fminp
            soil1(j)%tot(l)%p = soil1(j)%tot(l)%p + manure_kg * fertdb(it)%forgp
            grazn = manure_kg * (fertdb(it)%forgn + fertdb(it)%fminn)
            grazp = manure_kg * (fertdb(it)%forgp + fertdb(it)%fminp)
          end if
          if (bsn_cc%cswat == 1) then
          soil1(j)%mn(l)%no3 = soil1(j)%mn(l)%no3 + manure_kg *       &
                  (1. - fertdb(it)%fnh3n) * fertdb(it)%fminn
          soil1(j)%man(l)%n = soil1(j)%man(l)%n + manure_kg *         &
                       fertdb(it)%forgn
          soil1(j)%mn(l)%nh4 = soil1(j)%mn(l)%nh4 + manure_kg *       &
                       fertdb(it)%fnh3n * fertdb(it)%fminn
          soil1(j)%mp(l)%lab = soil1(j)%mp(l)%lab + manure_kg *       &
                       fertdb(it)%fminp
          soil1(j)%man(l)%p = soil1(j)%man(l)%p + manure_kg *         &
                       fertdb(it)%forgp         
          soil1(j)%man(l)%c = soil1(j)%man(l)%c + manure_kg *         &
                       fertdb(it)%forgn * 10.
          end if
          
          !!By Zhang for C/N cycling
          !!===============================  
          if (bsn_cc%cswat == 2) then
          soil1(j)%mn(l)%no3 = soil1(j)%mn(l)%no3 + manure_kg *        &   
                       (1. - fertdb(it)%fnh3n) * fertdb(it)%fminn
          !sol_fon(l,j) = sol_fon(l,j) + manure_kg(j) * forgn(it)
          orgc_f = 0.35  
          X1 = manure_kg
          X8 = X1 * orgc_f          
          RLN = .175 *(orgc_f)/(fertdb(it)%fminp + fertdb(it)%forgn + 1.e-5)
          X10 = .85-.018*RLN
          if (X10<0.01) then
            X10 = 0.01
          else
            if (X10 > .7) then
                X10 = .7
            end if
          end if
          XX = X8 * X10
          soil1(j)%meta(l)%c = soil1(j)%meta(l)%c + XX
          YY = manure_kg * X10
          soil1(j)%meta(l)%m = soil1(j)%meta(l)%m + YY
          ZZ = manure_kg * fertdb(it)%forgn * X10
          soil1(j)%meta(l)%n = soil1(j)%meta(l)%n + ZZ
          soil1(j)%str(l)%n = soil1(j)%str(l)%n + manure_kg * fertdb(it)%forgn - ZZ
          XZ = manure_kg * orgc_f-XX
          soil1(j)%str(l)%c = soil1(j)%str(l)%c + XZ
          soil1(j)%lig(l)%c = soil1(j)%lig(l)%c + XZ * .175
          soil1(j)%lig(l)%n = soil1(j)%lig(l)%n + XZ * (1.-.175) 
          YZ = manure_kg - YY
          soil1(j)%str(l)%m = soil1(j)%str(l)%m + YZ
          soil1(j)%lig(l)%m = soil1(j)%lig(l)%m + YZ *.175
          soil1(j)%tot(l)%n = soil1(j)%meta(l)%n + soil1(j)%str(l)%n
          soil1(j)%mn(l)%nh4 = soil1(j)%mn(l)%nh4 + manure_kg *       &   
                       fertdb(it)%fnh3n * fertdb(it)%fminn
          soil1(j)%mp(l)%lab = soil1(j)%mp(l)%lab + manure_kg *       & 
                       fertdb(it)%fminp
          soil1(j)%tot(l)%p = soil1(j)%tot(l)%p + manure_kg *       &   
                       fertdb(it)%forgp  
          end if

        end if

      return
      end subroutine pl_graze