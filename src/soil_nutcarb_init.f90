     subroutine soil_nutcarb_init (isol)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine initializes soil chemical properties

      use hru_module, only : hru, ihru, sol_plt_ini
      use soil_module
      use soil_data_module
      use basin_module
      use organic_mineral_mass_module

      implicit none 
      
      integer, intent (in)  :: isol     !none       |soil (hru) number
      integer :: nly = 0
      integer :: ly = 0
      integer :: isolt = 0              !counter    |soil plant initialization file pointer
      integer :: isol_pl = 0            !counter    |soil nutrient initialization pointer (nutrients.sol)
      real :: wt1 = 0.                  !kg/ha      |weight of the soil layer
      real :: dep_frac = 0.             !0-1        |fraction of surface concentration at depth
      real :: frac_hum_active = 0.      !0-1        |fraction of humus in active pool - old SWAT
      real :: frac_hum_microb = 0.      !0-1        |fraction of humus in microbial pool - CENTURY
      real :: frac_hum_slow = 0.        !0-1        |fraction of humus in slow pool - CENTURY
      real :: frac_hum_passive = 0.     !0-1        |fraction of humus in passive pool - CENTURY
      real :: actp = 0.
      real :: solp = 0.
      real :: ssp = 0.
      real :: psp = 0.                  !              | 

      nly = soil(ihru)%nly

      !! set soil nutrient initialization from nutrients.sol
      isol_pl = hru(ihru)%dbs%soil_plant_init
      isolt = sol_plt_ini(isol_pl)%nut          ! isolt = 0 = default in type
      
      !! set soil carbon
      soil1(ihru)%cbn(1) = max(0.001, soildb(isol)%ly(1)%cbn)    !! assume 0.001% carbon if zero
      !! calculate percent carbon for lower layers using exponential decrease
      !do ly = 2, nly
        !dep_frac = Exp(-solt_db(isolt)%exp_co * soil(ihru)%phys(ly)%d)
        !soil1(ihru)%cbn(ly) = soil1(ihru)%cbn(1) * dep_frac
      !end do
      !! use carbon content in the soils database
      do ly = 2, nly
        if (ly - 1 <= soildb(isol)%s%nly) then
          soil1(ihru)%cbn(ly) = soildb(isol)%ly(ly-1)%cbn
        else
          soil1(ihru)%cbn(ly) = soildb(isol)%ly(soildb(isol)%s%nly)%cbn
        end if
      end do

      !! calculate initial nutrient contents of layers, profile and
      !! average in soil for the entire watershed

      do ly = 1, nly
        soil(ihru)%phys(ly)%conv_wt = soil(ihru)%phys(ly)%bd * soil(ihru)%phys(ly)%thick / 100.    ! mg/kg => kg/ha
        wt1 = soil(ihru)%phys(ly)%conv_wt
        
        !! set initial mineral pools - no3
        dep_frac = Exp(-solt_db(isolt)%exp_co * soil(ihru)%phys(ly)%d)
        if (solt_db(isolt)%nitrate > 1.e-9) then
          soil1(ihru)%mn(ly)%no3 = solt_db(isolt)%nitrate * dep_frac
        else
          soil1(ihru)%mn(ly)%no3 = 7. * dep_frac
        end if
        soil1(ihru)%mn(ly)%no3 =  soil1(ihru)%mn(ly)%no3 * wt1      !! mg/kg => kg/ha

        !set initial labile P pool
        if (solt_db(isolt)%lab_p > 1.e-9) then
          soil1(ihru)%mp(ly)%lab = solt_db(isolt)%lab_p * dep_frac
        else
          !! assume initial concentration of 5 mg/kg
          soil1(ihru)%mp(ly)%lab = 5. * dep_frac
        end if
        soil1(ihru)%mp(ly)%lab = soil1(ihru)%mp(ly)%lab * wt1   !! mg/kg => kg/ha

        !! set active mineral P pool based on dynamic PSP MJW
        if (bsn_cc%sol_P_model == 1) then 
          !! Allow Dynamic PSP Ratio
          !! convert to concentration
          solp = soil1(ihru)%mp(ly)%lab / wt1
          !! PSP = -0.045*log (% clay) + 0.001*(Solution P, mg kg-1) - 0.035*(% Organic C) + 0.43
          if (soil(ihru)%phys(ly)%clay > 0.) then
            psp = -0.045 * log(soil(ihru)%phys(ly)%clay) + (0.001 * solp) 
            psp = psp - (0.035 * soil1(ihru)%cbn(ly)) + 0.43 
          endif         
          !! Limit PSP range
          if (psp < .10) then
            psp = 0.10
          else if (psp > 0.7) then
            psp = 0.7
          end if
        else
          psp = hru(ihru)%nut%psp
        end if
        soil1(ihru)%mp(ly)%act = soil1(ihru)%mp(ly)%lab * (1. - psp) / psp

        !! Set Stable pool based on dynamic coefficient
        if (bsn_cc%sol_P_model == 1) then  !! From White et al 2009 
            !! convert to concentration for ssp calculation
            actp = soil1(ihru)%mp(ly)%act / wt1
            solp = soil1(ihru)%mp(ly)%lab / wt1
            !! estimate Total Mineral P in this soil based on data from sharpley 2004
            ssp = 25.044 * (actp + solp)** (-0.3833)
            !!limit SSP Range
            if (ssp > 7.) ssp = 7.
            if (ssp < 1.) ssp = 1.            
            soil1(ihru)%mp(ly)%sta = ssp * (soil1(ihru)%mp(ly)%act + soil1(ihru)%mp(ly)%lab)
         else
          !! the original code
          soil1(ihru)%mp(ly)%sta = 4. * soil1(ihru)%mp(ly)%act
       end if
      end do

      !! set initial organic pools - originally by Zhang
      do ly = 1, nly

        !initialize total soil organic pool - no litter
        !kg/ha = mm * t/m3 * m/1,000 mm * 1,000 kg/t * 10,000 m2/ha
        soil1(ihru)%tot(ly)%m = 10000. * soil(ihru)%phys(ly)%thick * soil(ihru)%phys(ly)%bd
        soil1(ihru)%tot(ly)%c = soil1(ihru)%tot(ly)%m * soil1(ihru)%cbn(ly) / 100.
        soil1(ihru)%tot(ly)%n = soil1(ihru)%tot(ly)%c / 10.     !assume 10:1 C:N ratio
        soil1(ihru)%tot(ly)%p = soil1(ihru)%tot(ly)%c / 100.    !assume 100:1 C:P ratio
           
        if (bsn_cc%cswat == 0) then
          !set active humus fraction for original SWAT model
          if (solt_db(isolt)%fr_hum_act < 1.e-9) solt_db(isolt)%fr_hum_act = .02
          frac_hum_active = solt_db(isolt)%fr_hum_act
        
          !initialize oringinal SWAT active and stable organic pools (from EPIC)
          !initialize active humus pool
          soil1(ihru)%hact(ly)%m = frac_hum_active * soil1(ihru)%tot(ly)%m
          soil1(ihru)%hact(ly)%c = frac_hum_active * soil1(ihru)%tot(ly)%c
          soil1(ihru)%hact(ly)%n = soil1(ihru)%hact(ly)%c / solt_db(isolt)%hum_c_n
          soil1(ihru)%hact(ly)%p = soil1(ihru)%hact(ly)%c / solt_db(isolt)%hum_c_p
            
          !initialize stable humus pool
          soil1(ihru)%hsta(ly)%m = (1. - frac_hum_active) * soil1(ihru)%tot(ly)%m
          soil1(ihru)%hsta(ly)%c = (1. - frac_hum_active) * soil1(ihru)%tot(ly)%c
          soil1(ihru)%hsta(ly)%n = soil1(ihru)%hsta(ly)%c / solt_db(isolt)%hum_c_n
          soil1(ihru)%hsta(ly)%p = soil1(ihru)%hsta(ly)%c / solt_db(isolt)%hum_c_p
        end if
        
        if (bsn_cc%cswat == 2) then
          !initialize CENTURY organic pools
          !set soil humus fractions for CENTURY from DSSAT
          frac_hum_microb = 0.02
          frac_hum_slow = 0.54
          frac_hum_passive = 0.44
 
          !initialize passive humus pool
          soil1(ihru)%hp(ly)%m = frac_hum_passive * soil1(ihru)%tot(ly)%m
          soil1(ihru)%hp(ly)%c = frac_hum_passive * soil1(ihru)%tot(ly)%c
          soil1(ihru)%hp(ly)%n = soil1(ihru)%hp(ly)%c / 10.                       !assume 10:1 C:N ratio
          soil1(ihru)%hp(ly)%p = soil1(ihru)%hp(ly)%c / 80.                       !assume 80:1 C:P ratio
            
          !initialize slow humus pool
          soil1(ihru)%hs(ly)%m = frac_hum_slow * soil1(ihru)%tot(ly)%m
          soil1(ihru)%hs(ly)%c = frac_hum_slow * soil1(ihru)%tot(ly)%c
          soil1(ihru)%hs(ly)%n = soil1(ihru)%hs(ly)%c / 10.                       !assume 10:1 C:N ratio
          soil1(ihru)%hs(ly)%p = soil1(ihru)%hs(ly)%c / 80.                       !assume 80:1 C:P ratio
            
          !initialize microbial pool
          soil1(ihru)%microb(ly)%m = frac_hum_microb * soil1(ihru)%tot(ly)%m
          soil1(ihru)%microb(ly)%c = frac_hum_microb * soil1(ihru)%tot(ly)%c
          soil1(ihru)%microb(ly)%n = soil1(ihru)%microb(ly)%c / 8.                !assume 8:1 C:N ratio
          soil1(ihru)%microb(ly)%p = soil1(ihru)%microb(ly)%c / 80.               !assume 80:1 C:P ratio

          soil1(ihru)%tot(ly) = soil1(ihru)%hs(ly) + soil1(ihru)%hp(ly) + soil1(ihru)%microb(ly)
          if (ly == 1) then
            soil1(ihru)%seq(ly)%c = 0.0
          else
            soil1(ihru)%seq(ly) = soil1(ihru)%hs(ly) + soil1(ihru)%hp(ly) + soil1(ihru)%microb(ly)
          endif

        end if
      end do   !! ens soil layer loop 

      return
      end subroutine soil_nutcarb_init