      subroutine pl_partition(j, init)
      
      use plant_data_module
      use basin_module
      use hru_module, only : ipl
      use plant_module
      use carbon_module
      use organic_mineral_mass_module
      
      implicit none 
      
      integer, intent (in) :: j     !none               |HRU number
      integer, intent (in) :: init  !none               |init=1 to initialize and transplant; init=0 during simulation
      integer :: idp = 0            !                   |
      real :: root_frac = 0.        !none               |root mass fraction
      real :: ab_gr_frac = 0.       !none               |above ground mass fraction
      real :: leaf_mass_frac = 0.   !none               |leaf mass fraction of above ground biomass
      real :: stem_mass_frac = 0.   !none               |stem mass fraction of above ground biomass
      real :: seed_mass_frac = 0.   !none               |stem mass fraction of above ground biomass
      real :: n_left = 0.           !none               |n left after seed is removed
      real :: n_frac = 0.           !none               |n fraction in remainder of plant
      real :: p_left = 0.           !none               |p left after seed is removed
      real :: p_frac = 0.           !none               |p fraction in remainder of plant
      real :: mass_left = 0.        !none               |mass left after plant component is removed
      real :: mass_act = 0.         !none               |actual mass in each plant component 
      real :: mass_opt = 0.         !none               |optimal mass in each plant component 
      real :: mass_add = 0.         !none               |added mass in each plant component
      real :: leaf_frac_veg = 0.    !none               |fraction veg mass (stem+leaf) that is leaf
      real :: leaf_mass_frac_veg = 0. !none               |fraction veg mass (stem+leaf) that is leaf
           
      idp = pcom(j)%plcur(ipl)%idplt
      
      !! update plant mass for daily biomass/c increase
      pl_mass(j)%tot(ipl)%m = pl_mass(j)%tot(ipl)%m + pl_mass_up%m
      pl_mass(j)%tot(ipl)%c = pl_mass(j)%tot(ipl)%c + pl_mass_up%c
      
      !! partition leaf and stem (stalk) and seed (grain) mass
      if (pldb(idp)%typ == "perennial") then
        leaf_frac_veg = 0.02    !forest
      else
        leaf_frac_veg = 0.30    !should be plant parm
      end if
      leaf_mass_frac_veg = leaf_frac_veg * pcom(j)%plg(ipl)%lai / pcom(j)%plcur(ipl)%lai_pot
      
      !! partition root and above ground biomass for tuber crops
      if (pldb(idp)%typ == "warm_annual_tuber" .or. pldb(idp)%typ == "cold_annual_tuber") then
        root_frac = pcom(j)%plg(ipl)%root_frac
        !! for tubers, the tuber (or seed) is not part of ab_gr --> tot = root + ab_gr + seed
        !! 1. = root + ab_gr + hi * ab_gr --> solve for ab_gr
        ab_gr_frac = (1. - root_frac) / (1. + pcom(j)%plg(ipl)%hi_adj)
        seed_mass_frac = 1. - root_frac - ab_gr_frac
        leaf_mass_frac = leaf_mass_frac_veg * ab_gr_frac
        stem_mass_frac = (1. - leaf_mass_frac_veg) * ab_gr_frac
      else
      !! partition root and above ground biomass for all other annuals (above ground grain)
        root_frac = pcom(j)%plg(ipl)%root_frac
        ab_gr_frac = 1. - root_frac
        seed_mass_frac = pcom(j)%plg(ipl)%hi_adj
        leaf_mass_frac = leaf_mass_frac_veg * (1. - seed_mass_frac)
        stem_mass_frac = 1. - (leaf_mass_frac_veg + seed_mass_frac)
      end if
      
      !! check if initializing
      if (init == 0) then
        !! first maintain root fraction - root mass/total mass
        mass_left = pl_mass_up%m
        mass_act = pl_mass(j)%root(ipl)%m
        mass_opt = root_frac * pl_mass(j)%tot(ipl)%m
        if (mass_act < mass_opt) then
          mass_add = mass_opt - mass_act
          mass_add = Min (mass_add, mass_left)
          pl_mass(j)%root(ipl)%m = pl_mass(j)%root(ipl)%m + mass_add
          mass_left = mass_left - mass_add
        end if
        !! next maintain harvest index on yield (seed/fruit) component
        mass_act = pl_mass(j)%seed(ipl)%m
        mass_opt = seed_mass_frac * pl_mass(j)%tot(ipl)%m
        if (mass_act < mass_opt) then
          mass_add = mass_opt - mass_act
          mass_add = Min (mass_add, mass_left)
          pl_mass(j)%seed(ipl)%m = pl_mass(j)%seed(ipl)%m + mass_add
          mass_left = mass_left - mass_add
        end if
        !! next maintain leaf component
        mass_act = pl_mass(j)%leaf(ipl)%m
        mass_opt = leaf_mass_frac * pl_mass(j)%tot(ipl)%m
        if (mass_act < mass_opt) then
          mass_add = mass_opt - mass_act
          mass_add = Min (mass_add, mass_left)
          pl_mass(j)%leaf(ipl)%m = pl_mass(j)%leaf(ipl)%m + mass_add
          mass_left = mass_left - mass_add
        end if
        !! remainder goes to stem
        pl_mass(j)%stem(ipl)%m = pl_mass(j)%stem(ipl)%m + mass_left
        pl_mass(j)%ab_gr(ipl)%m = pl_mass(j)%stem(ipl)%m + pl_mass(j)%leaf(ipl)%m + pl_mass(j)%seed(ipl)%m
      else
        !! initialize at initial fractions
        pl_mass(j)%ab_gr(ipl)%m = ab_gr_frac * pl_mass(j)%tot(ipl)%m
        pl_mass(j)%root(ipl)%m = root_frac * pl_mass(j)%tot(ipl)%m
        pl_mass(j)%leaf(ipl)%m = leaf_mass_frac * pl_mass(j)%ab_gr(ipl)%m
        pl_mass(j)%seed(ipl)%m = seed_mass_frac * pl_mass(j)%ab_gr(ipl)%m
        pl_mass(j)%stem(ipl)%m = stem_mass_frac * pl_mass(j)%ab_gr(ipl)%m
      end if
          
      !! partition carbon with constant fractions
      pl_mass(j)%leaf(ipl)%c = c_frac%leaf * pl_mass(j)%leaf(ipl)%m
      pl_mass(j)%stem(ipl)%c = c_frac%stem * pl_mass(j)%stem(ipl)%m
      pl_mass(j)%seed(ipl)%c = c_frac%seed * pl_mass(j)%seed(ipl)%m
      pl_mass(j)%root(ipl)%c = c_frac%root * pl_mass(j)%root(ipl)%m
      pl_mass(j)%ab_gr(ipl)%c = pl_mass(j)%leaf(ipl)%c + pl_mass(j)%stem(ipl)%c + pl_mass(j)%seed(ipl)%c
      pl_mass(j)%tot(ipl)%c = pl_mass(j)%ab_gr(ipl)%c + pl_mass(j)%root(ipl)%c
          
      !! partition n and p
      if (pldb(idp)%typ == "perennial") then
        !! partition leaves and seed (stem is woody biomass)
        mass_left = pl_mass(j)%leaf(ipl)%m + pl_mass(j)%stem(ipl)%m + pl_mass(j)%root(ipl)%m
        if (mass_left > 1.e-9) then
          pl_mass(j)%seed(ipl)%n = pldb(idp)%cnyld * pl_mass(j)%seed(ipl)%m
          n_left = pl_mass(j)%tot(ipl)%n - pl_mass(j)%seed(ipl)%n
          !! if n is neg after seed is removed - assume 0 n in seed - plant database cnyld and fr_n_mat are off
          if (n_left < 0.) then
            pl_mass(j)%seed(ipl)%n = 0.
            n_left = pl_mass(j)%seed(ipl)%n +  n_left
          end if
          !! partition n_left between remaining masses - assume equal concentrations
          pl_mass(j)%leaf(ipl)%n = n_left * pl_mass(j)%leaf(ipl)%m / mass_left
          pl_mass(j)%stem(ipl)%n = n_left * pl_mass(j)%stem(ipl)%m / mass_left
          pl_mass(j)%root(ipl)%n = n_left * pl_mass(j)%root(ipl)%m / mass_left
          pl_mass(j)%ab_gr(ipl)%n = pl_mass(j)%seed(ipl)%n + pl_mass(j)%leaf(ipl)%n + pl_mass(j)%stem(ipl)%n
        
          pl_mass(j)%seed(ipl)%p = pldb(idp)%cpyld * pl_mass(j)%seed(ipl)%m
          p_left = pl_mass(j)%tot(ipl)%p - pl_mass(j)%seed(ipl)%p
          !! if n is neg after seed is removed - assume 0 n in seed - plant database cnyld and fr_n_mat are off
          if (p_left < 0.) then
            pl_mass(j)%seed(ipl)%p = 0.
            p_left = pl_mass(j)%seed(ipl)%p +  p_left
          end if
          !! partition p_left between remaining masses - assume equal concentrations
          pl_mass(j)%leaf(ipl)%p = p_left * pl_mass(j)%leaf(ipl)%m / mass_left
          pl_mass(j)%stem(ipl)%p = p_left * pl_mass(j)%stem(ipl)%m / mass_left
          pl_mass(j)%root(ipl)%p = p_left * pl_mass(j)%root(ipl)%m / mass_left
          pl_mass(j)%ab_gr(ipl)%p = pl_mass(j)%seed(ipl)%p + pl_mass(j)%leaf(ipl)%p + pl_mass(j)%stem(ipl)%p
        end if
      else
        !! annual or grass (stem is stalk) - partition seed (grain)
        pl_mass(j)%seed(ipl)%n = pldb(idp)%cnyld * pl_mass(j)%seed(ipl)%m
        !! assume same concentration in rest of plant
        n_left = pl_mass(j)%tot(ipl)%n - pl_mass(j)%seed(ipl)%n
        if (n_left < 0.) then
          pl_mass(j)%seed(ipl)%n = 0.9 * pl_mass(j)%tot(ipl)%n
          n_left = 0.1 * pl_mass(j)%tot(ipl)%n
        end if
        if (pl_mass(j)%tot(ipl)%m - pl_mass(j)%seed(ipl)%m > 1.e-6) then
          n_frac = n_left / (pl_mass(j)%tot(ipl)%m - pl_mass(j)%seed(ipl)%m)
        else
          n_frac = 0.
        end if
        pl_mass(j)%leaf(ipl)%n = n_frac * pl_mass(j)%leaf(ipl)%m
        pl_mass(j)%stem(ipl)%n = n_frac * pl_mass(j)%stem(ipl)%m
        pl_mass(j)%root(ipl)%n = n_frac * pl_mass(j)%root(ipl)%m
        pl_mass(j)%ab_gr(ipl)%n = pl_mass(j)%seed(ipl)%n + pl_mass(j)%leaf(ipl)%n + pl_mass(j)%stem(ipl)%n

        pl_mass(j)%seed(ipl)%p = pldb(idp)%cpyld * pl_mass(j)%seed(ipl)%m
        !! assume same concentration in rest of plant
        p_left = pl_mass(j)%tot(ipl)%p - pl_mass(j)%seed(ipl)%p
        if (p_left < 0.) then
          pl_mass(j)%seed(ipl)%p = 0.9 * pl_mass(j)%tot(ipl)%p
          p_left = 0.1 * pl_mass(j)%tot(ipl)%p
        end if
        if (pl_mass(j)%tot(ipl)%m - pl_mass(j)%seed(ipl)%m > 1.e-6) then
          p_frac = p_left / (pl_mass(j)%tot(ipl)%m - pl_mass(j)%seed(ipl)%m)
        else
          p_frac = 0.
        end if
        pl_mass(j)%leaf(ipl)%p = p_frac * pl_mass(j)%leaf(ipl)%m
        pl_mass(j)%stem(ipl)%p = p_frac * pl_mass(j)%stem(ipl)%m
        pl_mass(j)%root(ipl)%p = p_frac * pl_mass(j)%root(ipl)%m
        pl_mass(j)%ab_gr(ipl)%p = pl_mass(j)%seed(ipl)%p + pl_mass(j)%leaf(ipl)%p + pl_mass(j)%stem(ipl)%p
      end if
           
      return
      end subroutine pl_partition