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
      integer :: j                        !none          |counter
      integer :: l                        !none          |counter 
      integer, intent (in) :: ifrt        !              |fertilizer type from fert data base
      integer, intent (in) :: fertop      !              | 
      real, intent (in) :: frt_kg         !kg/ha         |amount of fertilizer applied
      real :: xx                          !              |
      
      !!added by zhang
      !!======================
      real :: X1, X8, X10, XXX, YY, ZZ, XZ, YZ, RLN, orgc_f
      
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

      !j = jj

      do l = 1, 2
        xx = 0.
        if (l == 1) then
          xx = chemapp_db(fertop)%surf_frac
        else
          xx = 1. - chemapp_db(fertop)%surf_frac                     
        endif

        soil1(j)%mn(l)%no3 = soil1(j)%mn(l)%no3 + xx * frt_kg *          &
            (1. - fertdb(ifrt)%fnh3n) * fertdb(ifrt)%fminn

        if (bsn_cc%cswat == 0) then
        soil1(j)%tot(l)%n = soil1(j)%tot(l)%n + rtof * xx * frt_kg *   &
                       fertdb(ifrt)%forgn
        soil1(j)%hact(l)%n = soil1(j)%hact(l)%n + (1. - rtof) * xx * &
            frt_kg * fertdb(ifrt)%forgn
        soil1(j)%tot(l)%p = soil1(j)%tot(l)%p + rtof * xx * frt_kg *   &
                       fertdb(ifrt)%forgp
        soil1(j)%hsta(l)%p = soil1(j)%hsta(l)%p + (1. - rtof)*xx*frt_kg *  &
                       fertdb(ifrt)%forgp
        end if
	  if (bsn_cc%cswat == 1) then
	  soil1(j)%man(l)%c = soil1(j)%man(l)%c + xx * frt_kg *            &
      		fertdb(ifrt)%forgn * 10.
	  soil1(j)%man(l)%n = soil1(j)%man(l)%n + xx * frt_kg *            &
      		fertdb(ifrt)%forgn
	  soil1(j)%man(l)%p = soil1(j)%man(l)%p + xx * frt_kg *            &
      		fertdb(ifrt)%forgp
	  end if

        !!By Zhang for C/N cycling 
        !!===========================
	  if (bsn_cc%cswat == 2) then
        soil1(j)%tot(l)%p = soil1(j)%tot(l)%p + rtof * xx *           &
            frt_kg * fertdb(ifrt)%forgp
        soil1(j)%hsta(l)%p = soil1(j)%hsta(l)%p + (1. - rtof) * xx *  &
            frt_kg * fertdb(ifrt)%forgp
        
        !!Allocate organic fertilizer to Slow (SWAT_active) N pool;
          soil1(j)%hact(l)%n = soil1(j)%hact(l)%n + (1. - rtof) * xx *  &
                        frt_kg * fertdb(ifrt)%forgn
        
          !orgc_f is the fraction of organic carbon in fertilizer
          !for most fertilziers this value is set to 0.
          orgc_f = 0.0
          !X1 is fertlizer applied to layer (kg/ha)
          !xx is fraction of fertilizer applied to layer
          X1 = xx * frt_kg 
          !X8: organic carbon applied (kg C/ha)
          X8 = X1 * orgc_f
          !RLN is calculated as a function of C:N ration in fertilizer          
          RLN = .175 *(orgc_f)/(fertdb(ifrt)%fminn + fertdb(ifrt)%forgn  & 
                                                               + 1.e-5)
          
          !X10 is the fraction of carbon in fertilizer that is allocated to metabolic litter C pool
          X10 = .85-.018*RLN
          if (X10<0.01) then
            X10 = 0.01
          else
            if (X10 > .7) then
                X10 = .7
            end if
          end if
          
          !XXX is the amount of organic carbon allocated to metabolic litter C pool
          XXX = X8 * X10
          soil1(j)%meta(l)%c = soil1(j)%meta(l)%c + XXX
          !YY is the amount of fertilizer (including C and N) allocated into metabolic litter SOM pool
          YY = X1 * X10
          soil1(j)%meta(l)%m = soil1(j)%meta(l)%m + YY
          
          !ZZ is amount of organic N allocated to metabolic litter N pool
          ZZ = X1 *rtof * fertdb(ifrt)%forgn * X10
          
          
          soil1(j)%meta(l)%n = soil1(j)%meta(l)%n + ZZ
           
          !!remaining organic N is llocated to structural litter N pool
          soil1(j)%str(l)%n = soil1(j)%str(l)%n + X1 * fertdb(ifrt)%forgn - ZZ
          !XZ is the amount of organic carbon allocated to structural litter C pool   
          XZ = X1 *orgc_f-XXX
          soil1(j)%str(l)%c = soil1(j)%str(l)%c + XZ
          
          !assuming lignin C fraction of organic carbon to be 0.175; updating lignin amount in strucutral litter pool
          soil1(j)%lig(l)%c = soil1(j)%lig(l)%c + XZ * .175
          !non-lignin part of the structural litter C is also updated;
          soil1(j)%lig(l)%n = soil1(j)%lig(l)%n + XZ * (1.-.175) 
          
          !YZ is the amount of fertilizer (including C and N) allocated into strucutre litter SOM pool
          YZ = X1 - YY
          soil1(j)%str(l)%m = soil1(j)%str(l)%m + YZ
          !assuming lignin fraction of the organic fertilizer allocated into structure litter SOM pool to be 0.175;
          !update lignin weight in structural litter.
          soil1(j)%lig(l)%m = soil1(j)%lig(l)%m + YZ*.175
          soil1(j)%tot(l)%n = soil1(j)%meta(l)%n + soil1(j)%str(l)%n
          
          !end if
      
	  end if
        !!By Zhang for C/N cycling 
        !!=========================== 

        soil1(j)%mn(l)%nh4 = soil1(j)%mn(l)%nh4 + xx * frt_kg *          &
            fertdb(ifrt)%fnh3n * fertdb(ifrt)%fminn

        soil1(j)%mp(l)%lab = soil1(j)%mp(l)%lab + xx * frt_kg *          & 
            fertdb(ifrt)%fminp

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