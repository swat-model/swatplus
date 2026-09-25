      module pesticide_data_module
    
      implicit none    
          
      type pesticide_db
        character(len=16) :: name = ""!!                      |pesticide name
        real :: koc = 0.            !! (mL/g)               |soil adsorption coeff normalized for soil org carbon content
        real :: washoff = 0.        !! none                 |frac of pesticide on foliage which is washed off by rainfall event 
        real :: foliar_hlife = 0.   !! days                 |half-life of pest on foliage
        real :: soil_hlife = 0.     !! days                 |half-life of pest in soil
        real :: solub = 0.          !! mg/L (ppm)           |solubility of chemical in water
        real :: aq_hlife = 0.       !! days                 |aquatic half-life
        real :: aq_volat = 0.       !! m/day                |aquatic volatilization coeff
        real :: mol_wt = 0.         !! g/mol                |molecular weight - to calulate mixing velocity
        real :: aq_resus = 0.       !! m/day                |aquatic resuspension velocity for pesticide sorbed to sediment
        real :: aq_settle = 0.      !! m/day                |aquatic settling velocity for pesticide sorbed to sediment
        real :: ben_act_dep = 0.    !! m                    |depth of active benthic layer
        real :: ben_bury = 0.       !! m/day                |burial velocity in benthic sediment
        real :: ben_hlife = 0.      !! days                 |half-life of pest in benthic sediment
        real :: pl_uptake = 0.      !! none                 |fraction taken up by plant 
        real :: harvest_sink = 1.   !! 0/1                  |1=plant is 100% sink (lost on uptake); 0=accumulates in pl_in, returned to soil on harvest/kill
        real :: resistance_n = 1.   !! 0-1                  |resistance blending: 0=actual st conc; 1=field-capacity conc; st_eff = st + n*max(0,fc-st)
        character(len=32) :: descrip = ""                   !pesticide description
      end type pesticide_db
      type (pesticide_db), dimension(:), allocatable, save :: pestdb
      
      type daughter_decay_fractions
        character(len=16) :: name = ""!! daughter pesticide name
        integer :: num = 0          !! sequential pesticide number in simulation
        real :: foliar_fr = 0.      !! 0-1                  |fraction of parent foilar degrading to daughter
        real :: soil_fr = 0.        !! 0-1                  |fraction of parent soil degrading to daughter
        real :: aq_fr = 0.          !! 0-1                  |fraction of parent aquatic degrading to daughter
        real :: ben_fr = 0.         !! 0-1                  |fraction of parent benthic degrading to daughter
      end type daughter_decay_fractions
      
      type pesticide_cp         !! calculated parameters from input parms
        integer :: num_metab = 0          !! number of metabolites
        type (daughter_decay_fractions), dimension(:), allocatable :: daughter
        real :: decay_f = 0.        !! none                 |exp of the rate const for degradation of the pest on foliage
        real :: decay_s = 0.        !! none                 |exp of the rate const for degradation of the pest in soil
        real :: decay_a = 0.        !! none                 |exp of the rate const for degradation of the pest in aquatic
        real :: decay_b = 0.        !! none                 |exp of the rate const for degradation of the pest in benthic layer
      end type pesticide_cp
      type (pesticide_cp), dimension(:), allocatable, save:: pestcp

      !! --- scenario flags for plant-uptake behaviour (hardcoded) ---
      !! pest_uptake_lamshoeft : .true.  = exact Lamshoeft PUF log-ratio formulation
      !!                         .false. = original linear TSCF * C * V (default)
      !! pest_uptake_skip_ly1  : .true.  = skip layer 1 in root uptake (dominated by evaporation)
      !!                         .false. = include all layers (default)
      !! -- per-pesticide parameters (read from pesticide.pes, stored in pesticide_db) --
      !! harvest_sink (pesticide_db field): 1=plant is 100% sink (uptake permanently lost);
      !!                                    0=accumulates in pl_in, returned to soil on harvest/kill
      !! resistance_n (pesticide_db field): 0=actual st conc; 1=field-capacity conc;
      !!                                    st_eff = st + n * max(0, fc - st)
      logical, parameter :: pest_uptake_lamshoeft = .false.
      logical, parameter :: pest_uptake_skip_ly1  = .false.

      end module pesticide_data_module