      module pathogen_data_module
    
      implicit none
    
      type pathogen_db
        character(len=16) :: pathnm         
        real :: do_soln = 0.       !! 1/day         |Die-off factor for pers bac in soil solution
        real :: gr_soln = 0.       !! 1/day         |Growth factor for pers bac in soil solution
        real :: do_sorb = 0.       !! 1/day         |Die-off factor for pers bac adsorbed to soil part
        real :: gr_sorb = 0.       !! 1/day         |Growth factor for pers bac adsorbed to soil part
        real :: kd = 0.            !! none          |Pathogen part coeff bet sol and sorbed phase in surf runoff
        real :: t_adj = 0.         !! none          |temp adj factor for bac die-off/growth
        real :: washoff = 0.       !! none          |frac of pers bac on foliage washed off by a rainfall event
        real :: do_plnt = 0.       !! 1/day         |Die-off factor for pers bac on foliage
        real :: gr_plnt = 0.       !! 1/day         |Growth factor for persistent pathogen on foliage
        real :: fr_manure = 0.     !! none          |frac of manure containing active colony forming units (cfu)
        real :: perco = 0.         !! none          |Pathogen perc coeff ratio of solution bacteria in surf layer
        real :: det_thrshd         !! # cfu/m^2     |Threshold detection level for less pers bac when pathogen levels
                                                    !drop to this amt the model considers bac in the soil to be 
                                                    !insignificant and sets the levels to zero
        real :: do_stream = 0.     !! 1/day         |Die-off factor for persistent pathogen in streams
        real :: gr_stream = 0.     !! 1/day         |growth factor for persistent pathogen in streams
        real :: do_res = 0.        !! 1/day         |Die-off factor for less persistent pathogen in reservoirs
        real :: gr_res = 0.        !! 1/day         |growth factor for less persistent pathogen in reservoirs
        real :: conc_min           !!               |minimum pathogen concentration
      end type pathogen_db
      type (pathogen_db), dimension(:), allocatable  :: path_db
      
      contains

      end module pathogen_data_module 