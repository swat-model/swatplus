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
      real :: tramp_plant = 0.!frac       |fraction of above ground biomass of each plant trampled

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
        
        !! apply manure deposited by the grazing animals to the soil SURFACE only
        !! (surf_frac = 1.0). Use the manure constituent database (manure_om) via
        !! pl_manure -- the same routine and inputs the "manu" operation uses --
        !! instead of the fertilizer database. graze%manure_id is the manure.frt
        !! index (crosswalked in mgt_read_grazeops.f90).
        it = graze%manure_id
        manure_kg = graze%manure
        if (manure_kg > 0. .and. it > 0) then
          call pl_manure (it, manure_kg, 1.0)
          !! grazing manure N and P for the grazn/grazp landscape output
          grazn = manure_kg * (manure_om(it)%forgn + manure_om(it)%fminn)
          grazp = manure_kg * (manure_om(it)%forgp + manure_om(it)%fminp)
        end if

      return
      end subroutine pl_graze