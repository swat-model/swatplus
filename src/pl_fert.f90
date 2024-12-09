      subroutine pl_fert (ifrt, frt_kg, fertop)
      
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
      use organic_mineral_mass_module
      use hru_module, only : ihru, fertn, fertp, fertnh3, fertno3, fertorgn, fertorgp, fertp,  &
        fertsolp  
      
      implicit none 
      
      real, parameter :: rtof=0.5         !none          |weighting factor used to partition the 
                                          !              |organic N & P concentration of septic effluent
                                          !              |between the fresh organic and the stable 
                                          !              |organic pools
      integer :: j = 0                    !none          |counter
      integer :: l = 0                    !none          |counter 
      integer, intent (in) :: ifrt        !              |fertilizer type from fert data base
      integer, intent (in) :: fertop      !              | 
      real, intent (in) :: frt_kg         !kg/ha         |amount of fertilizer applied
      real :: xx = 0.                     !              |
      
      j = ihru

      do l = 1, 2
        xx = 0.
        if (l == 1) then
          xx = chemapp_db(fertop)%surf_frac
        else
          xx = 1. - chemapp_db(fertop)%surf_frac                     
        endif

        !! add mineral n and p for all methods
        soil1(j)%mn(l)%no3 = soil1(j)%mn(l)%no3 + xx * frt_kg *          &
                       (1. - fertdb(ifrt)%fnh3n) * fertdb(ifrt)%fminn
        soil1(j)%mn(l)%nh4 = soil1(j)%mn(l)%nh4 + xx * frt_kg *          &
                       fertdb(ifrt)%fnh3n * fertdb(ifrt)%fminn
        soil1(j)%mp(l)%lab = soil1(j)%mp(l)%lab + xx * frt_kg *          & 
                       fertdb(ifrt)%fminp

        !! add total organic n and p for all methods
        soil1(j)%tot(l)%n = soil1(j)%tot(l)%n + rtof * xx * frt_kg *   &
                       fertdb(ifrt)%forgn
        soil1(j)%tot(l)%p = soil1(j)%tot(l)%p + rtof * xx * frt_kg *   &
                       fertdb(ifrt)%forgp

        !! for stable carbon - add n and p to active humus pool
        if (bsn_cc%cswat == 0) then
          soil1(j)%hact(l)%n = soil1(j)%hact(l)%n + (1. - rtof) * xx * &
                       frt_kg * fertdb(ifrt)%forgn
          soil1(j)%hact(l)%p = soil1(j)%hsta(l)%p + (1. - rtof)*xx*frt_kg *  &
                       fertdb(ifrt)%forgp
        end if
        
        !! for C-FARM add to manure pool - assume C:N ratio = 10
        if (bsn_cc%cswat == 1) then
          soil1(j)%man(l)%c = soil1(j)%man(l)%c + xx * frt_kg *            &
              fertdb(ifrt)%forgn * 10.
          soil1(j)%man(l)%n = soil1(j)%man(l)%n + xx * frt_kg *            &
              fertdb(ifrt)%forgn
          soil1(j)%man(l)%p = soil1(j)%man(l)%p + xx * frt_kg *            &
              fertdb(ifrt)%forgp
        end if

        !! for SWAT-C (Century) add n and p to slow humus pool - assume C:N ratio = 10
        if (bsn_cc%cswat == 2) then
          soil1(j)%hs(l)%c = soil1(j)%hs(l)%c + xx * frt_kg *            &
              fertdb(ifrt)%forgn * 10.
          soil1(j)%hs(l)%n = soil1(j)%hs(l)%n + xx * frt_kg *            &
              fertdb(ifrt)%forgn
          soil1(j)%hp(l)%p = soil1(j)%hs(l)%p + xx * frt_kg *            &
              fertdb(ifrt)%forgp
        end if

      end do 

      !! summary calculations
      fertno3 = frt_kg * fertdb(ifrt)%fminn * (1. - fertdb(ifrt)%fnh3n)
      fertnh3 = frt_kg * (fertdb(ifrt)%fminn * fertdb(ifrt)%fnh3n)
      fertorgn = frt_kg * fertdb(ifrt)%forgn
      fertsolp = frt_kg * fertdb(ifrt)%fminp
      fertorgp = frt_kg * fertdb(ifrt)%forgp  
      fertn = fertn + frt_kg * (fertdb(ifrt)%fminn + fertdb(ifrt)%forgn)
      fertp = fertp + frt_kg * (fertdb(ifrt)%fminp + fertdb(ifrt)%forgp)
      
      return
      end subroutine pl_fert