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
        fertsolp, hru
      
      implicit none 
      
      real, parameter :: rtof = 0.5         !none          |weighting factor used to partition the 
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
      real :: frt_ly = 0.                   !kg/ha        |fertlizer applied to layer (kg/ha)
      real :: org_c = 0.                       !organic carbon applied (kg C/ha)
      real :: meta_fr = 0.  !fraction of carbon in fertilizer that is allocated to metabolic litter C pool
      real :: meta_c = 0.                  !organic carbon allocated to metabolic litter C pool
      real :: meta_m = 0.        !fertilizer (including C and N) allocated into metabolic litter SOM pool
      real :: meta_n = 0.       !organic N allocated to metabolic litter N pool
      real :: str_c = 0.        !organic carbon allocated to structural litter C pool
      real :: str_m = 0.         !fertilizer (including C and N) allocated into structure litter SOM pool
      real :: c_n_fac = 0.        !function of C:N ratio in fertilizer
      real :: orgc_fr = 0.                   !fraction of organic carbon in fertilizer - 0.42
      
      j = ihru
      
      orgc_fr = 0.42
          
      do l = 1, 2
        fr_ly = 0.
        if (l == 1) then
          fr_ly = chemapp_db(fertop)%surf_frac
        else
          fr_ly = 1. - chemapp_db(fertop)%surf_frac                     
        endif

        !! add water to layer - mm = kg/ha * 1.0m3/t * ha/10,000m2 * 1,000mm/m
        soil(j)%phys(l)%st = soil(j)%phys(l)%st + fr_ly * frt_kg / 10.
        
        soil1(j)%mn(l)%no3 = soil1(j)%mn(l)%no3 + fr_ly * frt_kg *          &
            (1. - manure_om(ifrt)%fnh3n) * manure_om(ifrt)%fminn

        if (bsn_cc%cswat == 0) then
        soil1(j)%tot(l)%n = soil1(j)%tot(l)%n + rtof * fr_ly * frt_kg *   &
                       manure_om(ifrt)%forgn
        soil1(j)%hact(l)%n = soil1(j)%hact(l)%n + (1. - rtof) * fr_ly * &
            frt_kg * manure_om(ifrt)%forgn
        soil1(j)%tot(l)%p = soil1(j)%tot(l)%p + rtof * fr_ly * frt_kg *   &
                       manure_om(ifrt)%forgp
        soil1(j)%hsta(l)%p = soil1(j)%hsta(l)%p + (1. - rtof) * fr_ly * frt_kg *  &
                       manure_om(ifrt)%forgp
        end if
        
      if (bsn_cc%cswat == 1) then
      soil1(j)%man(l)%c = soil1(j)%man(l)%c + fr_ly * frt_kg *            &
            manure_om(ifrt)%forgn * 10.
      soil1(j)%man(l)%n = soil1(j)%man(l)%n + fr_ly * frt_kg *            &
            manure_om(ifrt)%forgn
      soil1(j)%man(l)%p = soil1(j)%man(l)%p + fr_ly * frt_kg *            &
            manure_om(ifrt)%forgp
      end if

        !!By Zhang for C/N cycling 
        !!===========================
      if (bsn_cc%cswat == 2 .or. bsn_cc%cswat == 3) then
        soil1(j)%tot(l)%p = soil1(j)%tot(l)%p + rtof * fr_ly *           &
            frt_kg * manure_om(ifrt)%forgp
        soil1(j)%hs(l)%p = soil1(j)%hs(l)%p + (1. - rtof) * fr_ly *      &
            frt_kg * manure_om(ifrt)%forgp
        
        !!allocate organic fertilizer to Slow N pool;
          soil1(j)%hs(l)%n = soil1(j)%hs(l)%n + (1. - rtof) * fr_ly *    &
                        frt_kg * manure_om(ifrt)%forgn
        
          frt_ly = fr_ly * frt_kg 
          
          org_c = frt_ly * orgc_fr
                    
          c_n_fac = .175 * orgc_fr / (manure_om(ifrt)%fminn + manure_om(ifrt)%forgn + 1.e-5)
          
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
          
          meta_m = frt_ly * meta_fr
          soil1(j)%meta(l)%m = soil1(j)%meta(l)%m + meta_m
          
          
          meta_n = frt_ly *rtof * manure_om(ifrt)%forgn * meta_fr
          
          
          soil1(j)%meta(l)%n = soil1(j)%meta(l)%n + meta_n
           
          !! remaining organic N is llocated to structural litter N pool
          soil1(j)%str(l)%n = soil1(j)%str(l)%n + frt_ly * manure_om(ifrt)%forgn - meta_n
             
          str_c = frt_ly * orgc_fr - meta_c
          soil1(j)%str(l)%c = soil1(j)%str(l)%c + str_c
          
          !assuming lignin C fraction of organic carbon to be 0.175; updating lignin amount in structural litter pool
          soil1(j)%lig(l)%c = soil1(j)%lig(l)%c + str_c * .175
          !non-lignin part of the structural litter C is also updated;
          soil1(j)%lig(l)%n = soil1(j)%lig(l)%n + str_c * (1.-.175) 
          
          
          str_m = frt_ly - meta_m
          soil1(j)%str(l)%m = soil1(j)%str(l)%m + str_m
          !assuming lignin fraction of the organic fertilizer allocated into structure litter SOM pool to be 0.175;
          !update lignin weight in structural litter.
          soil1(j)%lig(l)%m = soil1(j)%lig(l)%m + str_m * .175
          !soil1(j)%rsd(l)%n = soil1(j)%meta(l)%n + soil1(j)%str(l)%n
          
      end if
        !!By Zhang for C/N cycling 
        !!=========================== 

        soil1(j)%mn(l)%nh4 = soil1(j)%mn(l)%nh4 + fr_ly * frt_kg *          &
            manure_om(ifrt)%fnh3n * manure_om(ifrt)%fminn

        soil1(j)%mp(l)%lab = soil1(j)%mp(l)%lab + fr_ly * frt_kg *          & 
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