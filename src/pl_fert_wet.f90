      subroutine pl_fert_wet (ifrt, frt_kg)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine applies N and P specified by date and
!!    amount in the management file (.mgt) for rice fields during ponding periods
!!    Jaehak 2022
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
      use hydrograph_module

      
      implicit none 
      
      real, parameter :: rtof=0.5         !none          |weighting factor used to partition the 
                                          !              |organic N & P concentration of septic effluent
                                          !              |between the fresh organic and the stable 
                                          !              |organic pools
      integer :: j = 0                    !none          |counter
      integer, intent (in) :: ifrt        !              |fertilizer type from fert data base
      real, intent (in) :: frt_kg         !kg/ha         |amount of fertilizer applied
      

      !!added by zhang
      !!======================
      real :: X1 = 0.
      real :: X8 = 0.
      real :: X10 = 0.
      real :: XXX = 0.
      real :: YY = 0.
      real :: ZZ = 0.
      real :: XZ = 0.
      real :: YZ = 0.
      real :: RLN = 0.
      real :: orgc_f = 0.
      
      j = ihru
      
      X1 = 0.
      X8 = 0.
      X10 = 0.
      XXX = 0.
      YY = 0.
      ZZ = 0.
      XZ = 0.
      YZ = 0.
      RLN = 0.
      orgc_f = 0.
      !!added by zhang
      !!======================  

      if (bsn_cc%cswat == 1 ) then
        wet(j)%no3 = wet(j)%no3 + frt_kg * (1. - fertdb(ifrt)%fnh3n) * fertdb(ifrt)%fminn
        wet(j)%nh3 = wet(j)%nh3 + frt_kg * fertdb(ifrt)%fnh3n * fertdb(ifrt)%fminn
        wet(j)%solp = wet(j)%solp + frt_kg * fertdb(ifrt)%fminp
        wet(j)%orgn = wet(j)%orgn + frt_kg * fertdb(ifrt)%forgn
        wet(j)%sedp = wet(j)%sedp + frt_kg * fertdb(ifrt)%forgp
      end if

!! summary calculations
      fertno3 = frt_kg * fertdb(ifrt)%fminn * (1. - fertdb(ifrt)%fnh3n)
      fertnh3 = frt_kg * (fertdb(ifrt)%fminn * fertdb(ifrt)%fnh3n)
      fertorgn = frt_kg * fertdb(ifrt)%forgn
      fertsolp = frt_kg * fertdb(ifrt)%fminp
      fertorgp = frt_kg * fertdb(ifrt)%forgp  
      fertn = fertn + frt_kg * (fertdb(ifrt)%fminn + fertdb(ifrt)%forgn)
      fertp = fertp + frt_kg * (fertdb(ifrt)%fminp + fertdb(ifrt)%forgp)
      return
      end subroutine pl_fert_wet