      subroutine pl_manure (ifrt, frt_kg, fertop)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine applies N and P specified by date and
!!    amount in the management file (.mgt)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: Erfc

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use mgt_operations_module
      use fertilizer_data_module
      use basin_module
      use soil_module
      use organic_mineral_mass_module
      use hru_module, only : ihru, fertn, fertp, fertnh3, fertno3, fertorgn, fertorgp, fertp,  &
        fertsolp
      
      implicit none 
      
      real:: rtof                         !none          |weighting factor used to partition the 
                                          !              |organic N & P concentration of septic effluent
                                          !              |between the fresh organic and the stable 
                                          !              |organic pools
      integer :: j = 0                    !none          |counter
      integer :: l = 0                    !none          |counter 
      integer, intent (in) :: ifrt        !              |fertilizer type from fert data base
      integer, intent (in) :: fertop      !              | 
      real, intent (in) :: frt_kg         !kg/ha         |amount of fertilizer applied
      real :: fr_ly = 0.                  !              |fraction of fertilizer applied to layer
      
      !!added by zhang
      !!======================
      real :: org_c = 0.                       !organic carbon applied (kg C/ha)
      real :: meta_fr = 0.  !fraction of carbon in fertilizer that is allocated to metabolic litter C pool
      real :: meta_c = 0.                  !organic carbon allocated to metabolic litter C pool
      real :: meta_m = 0.        !fertilizer (including C and N) allocated into metabolic litter SOM pool
      real :: meta_n = 0.       !organic N allocated to metabolic litter N pool
      real :: str_c = 0.        !organic carbon allocated to structural litter C pool
      real :: str_m = 0.         !fertilizer (including C and N) allocated into structure litter SOM pool
      real :: c_n_fac = 0.        !function of C:N ratio in fertilizer
      real :: liq_manure_kg = 0.   !kilograms of the liquid portion of the manure 
      real :: liq_manure_mm = 0.   !mm/ha of of the liquid portion of the manure
      real :: frac_solids          !fraction of solids in manure which by definition is solids/(solids + liquids)
      real :: fr_mass              !fraction of dry weight applied.
      
      j = ihru
      
      rtof = man_coef%rtof
          
      !! add water to layer - mm = kg/ha * 1.0m3/t * ha/10,000m2 * 1,000mm/m
      !! or 
      !! 1 kg of water = 1liter * 1000cm^3/liter * 1000mm^3/cm^3 = 1,000,000 mm^3
      !! 1 hectare = 10,000m^2 x 1,000,000mm^2/m2 = 10,000,000,000 mm2
      !! 1 kg of water/ha = 1,000,000 mm^2 / 10,000,000,000 mm^2 = .0001 mm/ha
      !!
      !! frac_solids = solids/(solids + liquid)
      !! therefore: solids + liquid = solids/frac_solids
      !! therefore: liquid = solids/frac_solids - solids
      !! and
      !! frac_solids = 1 - frac_liquid 
      !! therefore: liquid = solids/(1 - frac_liquid) - solids
       
      frac_solids = (1. - manure_om(ifrt)%frac_water) 
      liq_manure_kg = frt_kg/(frac_solids) - frt_kg
      liq_manure_mm = liq_manure_kg * .0001 !this results in mm/ha units

      do l = 1, 2
        fr_ly = 0.
        if (l == 1) then
          fr_ly = chemapp_db(fertop)%surf_frac
        else
          fr_ly = 1. - chemapp_db(fertop)%surf_frac                     
        endif
        
        fr_mass = fr_ly * frt_kg ! mass of applied manure in kg/ha to the soil layer

        ! soil(j)%phys(l)%st = soil(j)%phys(l)%st + fr_ly * frt_kg / 10.
        soil(j)%phys(l)%st = soil(j)%phys(l)%st + fr_ly * liq_manure_mm
        
        soil1(j)%mn(l)%no3 = soil1(j)%mn(l)%no3 + fr_mass *          &
            (1. - manure_om(ifrt)%fnh3n) * manure_om(ifrt)%fminn

        if (bsn_cc%cswat == 0) then
          soil1(j)%tot(l)%n = soil1(j)%tot(l)%n + rtof * fr_mass *   &
                        manure_om(ifrt)%forgn
          soil1(j)%hact(l)%n = soil1(j)%hact(l)%n + (1. - rtof) * fr_mass * &
                        manure_om(ifrt)%forgn
          soil1(j)%tot(l)%p = soil1(j)%tot(l)%p + rtof * fr_mass *   &
                        manure_om(ifrt)%forgp
          soil1(j)%hsta(l)%p = soil1(j)%hsta(l)%p + (1. - rtof) * fr_mass *  &
                        manure_om(ifrt)%forgp
        end if
        
        if (bsn_cc%cswat == 1) then
          ! soil1(j)%man(l)%c = soil1(j)%man(l)%c + fr_ly * frt_kg *          &
          !       manure_om(ifrt)%forgn * 10.
          soil1(j)%man(l)%c = soil1(j)%man(l)%c + fr_mass * manure_om(ifrt)%fcbn

          soil1(j)%man(l)%n = soil1(j)%man(l)%n + fr_mass * manure_om(ifrt)%forgn
          soil1(j)%man(l)%p = soil1(j)%man(l)%p + fr_mass * manure_om(ifrt)%forgp
        end if

        !!By Zhang for C/N cycling 
        !!===========================
        if (bsn_cc%cswat == 1) then
          soil1(j)%tot(l)%p = soil1(j)%tot(l)%p + rtof * fr_mass * manure_om(ifrt)%forgp
          soil1(j)%hs(l)%p = soil1(j)%hs(l)%p + (1. - rtof) * fr_mass * manure_om(ifrt)%forgp
          
          !!allocate organic fertilizer to Slow N pool;
          soil1(j)%hs(l)%n = soil1(j)%hs(l)%n + (1. - rtof) * fr_mass * manure_om(ifrt)%forgn
        
          org_c = fr_mass * manure_om(ifrt)%fcbn 
                    
          ! c_n_fac = .175 * orgc_fr / (manure_om(ifrt)%fminn + manure_om(ifrt)%forgn + 1.e-5)
          c_n_fac = .175 * org_c / (fr_mass * (manure_om(ifrt)%fminn + manure_om(ifrt)%forgn)  + 1.e-5)
          
          meta_fr = .85 - .018 * c_n_fac
          if (meta_fr < 0.01) then
            meta_fr = 0.01
          else
            if (meta_fr > .7) then
              meta_fr = .7
            end if
          end if
          
          meta_c = org_c * meta_fr
          soil1(j)%meta(l)%c = soil1(j)%meta(l)%c + meta_c
          
          ! meta_m = org_c * meta_fr
          meta_m = meta_c / 0.58
          soil1(j)%meta(l)%m = soil1(j)%meta(l)%m + meta_m
          
          meta_n = org_c * rtof * manure_om(ifrt)%forgn * meta_fr
          
          soil1(j)%meta(l)%n = soil1(j)%meta(l)%n + meta_n
          
          !! remaining organic N is llocated to structural litter N pool
          soil1(j)%str(l)%n = soil1(j)%str(l)%n + fr_mass * manure_om(ifrt)%forgn - meta_n
            
          str_c = org_c - meta_c
          soil1(j)%str(l)%c = soil1(j)%str(l)%c + str_c
          
          !assuming lignin C fraction of organic carbon to be 0.175; updating lignin amount in structural litter pool
          soil1(j)%lig(l)%c = soil1(j)%lig(l)%c + str_c * .175
          !non-lignin part of the structural litter C is also updated;
          soil1(j)%lig(l)%n = soil1(j)%lig(l)%n + str_c * (1.-.175) 
          
          
          ! str_m = fr_ly - meta_m  ! I think this is wrong.  
          str_m = str_c / .58
          soil1(j)%str(l)%m = soil1(j)%str(l)%m + str_m
          !assuming lignin fraction of the organic fertilizer allocated into structure litter SOM pool to be 0.175;
          !update lignin weight in structural litter.
          soil1(j)%lig(l)%m = soil1(j)%lig(l)%m + str_m * .175
          !soil1(j)%rsd(l)%n = soil1(j)%meta(l)%n + soil1(j)%str(l)%n
              
        end if
        !!By Zhang for C/N cycling 
        !!=========================== 

        soil1(j)%mn(l)%nh4 = soil1(j)%mn(l)%nh4 + fr_mass *          &
            manure_om(ifrt)%fnh3n * manure_om(ifrt)%fminn

        soil1(j)%mp(l)%lab = soil1(j)%mp(l)%lab + fr_mass *          & 
            manure_om(ifrt)%fminp

      end do 

!! summary calculations
      fertno3 = frt_kg * manure_om(ifrt)%fminn * (1. - manure_om(ifrt)%fnh3n)
      fertnh3 = frt_kg * (manure_om(ifrt)%fminn * manure_om(ifrt)%fnh3n)
      fertorgn = frt_kg * manure_om(ifrt)%forgn
      fertsolp = frt_kg * manure_om(ifrt)%fminp
      fertorgp = frt_kg * manure_om(ifrt)%forgp  
      fertn = fertn + frt_kg * (manure_om(ifrt)%fminn + manure_om(ifrt)%forgn)
      fertp = fertp + frt_kg * (manure_om(ifrt)%fminp + manure_om(ifrt)%forgp)
      
      return
      end subroutine pl_manure