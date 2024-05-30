      subroutine mgt_newtillmix_wet (jj, idtill)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine mixes residue and nutrients in soil layers and ponding water during tillage  

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    npmx          |none          |number of different pesticides used in the simulation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min, Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use tillage_data_module
      use basin_module
      use organic_mineral_mass_module
      use hru_module, only: hru
      use soil_module
      use constituent_mass_module
      use plant_module
      use reservoir_module
      use hydrograph_module
      
      implicit none

      integer, intent (in) :: jj       !none           |HRU number
      integer, intent (in) :: idtill   !none           |tillage type
      integer :: l                     !none           |counter
      integer :: k                     !none           |counter
      integer :: npmx                  !               |
      !CB 12/2/09 nl and a are not used.
      real :: emix                     !none           |mixing efficiency
      real :: dtil                     !mm             |depth of mixing
      real :: frac_mixed               !               |
      real :: frac_non_mixed           !               |
      !!by zhang
      !!=============   
      real :: smix(22+cs_db%num_pests+12)         !varies         |amount of substance in soil profile
                                       !               |that is being redistributed between mixed layers
      !CB 12/2/09 thtill is not used. mjw rev 490
      !!changed the dimension from 22 + npmx to 22 + npmx + 12
      !!by zhang
      !!=============
      real :: sol_mass(soil(jj)%nly)    !              |mass of the soil layer
      real :: sol_msm(soil(jj)%nly)     !              |sol_mass mixed
      real :: sol_msn(soil(jj)%nly)     !              |sol_mass not mixed 
      real :: frac_dep(soil(jj)%nly)    !              |fraction of soil layer in tillage depth
      real :: frac_dep1(soil(jj)%nly)    !              |fraction of soil layer in tillage depth plus ponding water
      real :: tdep

      npmx = cs_db%num_pests

      frac_mixed = 0.
      frac_non_mixed = 0.
      emix = 0.
      dtil = 0.

      !! tillage operation
      emix = tilldb(idtill)%effmix
      dtil = tilldb(idtill)%deptil

      smix = 0.
      sol_mass = 0.
      sol_msm = 0.
      sol_msn = 0.
      tdep = wet_ob(jj)%depth * 1000. + dtil !ponding depth + tillage depth

      ! mix water and plough depth soils
      smix(1) = smix(1) + wet(jj)%no3 / hru(jj)%area_ha
      smix(3) = smix(3) + wet(jj)%nh3 / hru(jj)%area_ha
      smix(4) = smix(4) + wet(jj)%solp / hru(jj)%area_ha
      smix(8) = smix(8) + wet(jj)%orgn / hru(jj)%area_ha
      smix(9) = smix(9) + wet(jj)%sedp / hru(jj)%area_ha

      do l = 1, soil(jj)%nly
        sol_mass(l) = (soil(jj)%phys(l)%thick / 1000.) * 10000. *                    &
            soil(jj)%phys(1)%bd * 1000. * (1.- soil(jj)%phys(l)%rock/ 100.)
      end do

      smix = 0.

      if (dtil > 0.) then
        ! added by Armen 09/10/2010 next line only
        if (dtil < 10.0) dtil = 11.0
        do l = 1, soil(jj)%nly

          if (soil(jj)%phys(l)%d <= dtil) then
            !! msm = mass of soil mixed for the layer
            !! msn = mass of soil not mixed for the layer		
            sol_msm(l) = emix * sol_mass(l)	
            sol_msn(l) = sol_mass(l) - sol_msm(l)
            frac_dep(l) = soil(jj)%phys(l)%thick / dtil
            frac_dep1(l) = soil(jj)%phys(l)%thick / tdep
          else if (soil(jj)%phys(l)%d > dtil .and. soil(jj)%phys(l-1)%d < dtil) then 
            sol_msm(l) = emix * sol_mass(l) * (dtil - soil(jj)%phys(l-1)%d) / soil(jj)%phys(l)%thick
            sol_msn(l) =  sol_mass(l) - sol_msm(l)
            frac_dep(l) = (dtil - soil(jj)%phys(l-1)%d) / dtil
            frac_dep1(l) = (tdep - soil(jj)%phys(l-1)%d) / tdep
          else
            sol_msm(l) = 0.
            sol_msn(l) = sol_mass(l)
            frac_dep(l) = 0.
            frac_dep1(l) = 0.
          end if

          !! calculate the mass each mixed element
          frac_mixed = sol_msm(l) / sol_mass(l)
          smix(1) = smix(1) + soil1(jj)%mn(l)%no3 * frac_mixed
          smix(2) = smix(2) + soil1(jj)%hsta(l)%n * frac_mixed
          smix(3) = smix(3) + soil1(jj)%mn(l)%nh4 * frac_mixed
          smix(4) = smix(4) + soil1(jj)%mp(l)%lab * frac_mixed
          smix(5) = smix(5) + soil1(jj)%hsta(l)%p * frac_mixed
          smix(6) = smix(6) + soil1(jj)%hact(l)%n * frac_mixed
          smix(7) = smix(7) + soil1(jj)%mp(l)%act * frac_mixed
          smix(8) = smix(8) + soil1(jj)%tot(l)%n * frac_mixed
          smix(9) = smix(9) + soil1(jj)%tot(l)%p * frac_mixed
          smix(10) = smix(10) + soil1(jj)%mp(l)%sta * frac_mixed
          smix(11) = smix(11) + soil1(jj)%tot(l)%m * frac_mixed
          smix(12) = smix(12) + soil1(jj)%man(l)%c * frac_mixed
          smix(13) = smix(13) + soil1(jj)%man(l)%n * frac_mixed
          smix(14) = smix(14) + soil1(jj)%man(l)%p * frac_mixed
          smix(15) = smix(15) + soil1(jj)%tot(l)%c * frac_mixed
          smix(16) = smix(16) + soil1(jj)%tot(l)%n * frac_mixed
          !! sand, silt and clay are % so take weighted ave by depth
          smix(17) = smix(17) + soil(jj)%phys(l)%clay * frac_dep(l)
          smix(18) = smix(18) + soil(jj)%phys(l)%silt * frac_dep(l)
          smix(19) = smix(19) + soil(jj)%phys(l)%sand * frac_dep(l)

            !!by zhang
            !!============== 
          if (bsn_cc%cswat == 2) then         
	          smix(20+npmx+1) = smix(20+npmx+1) + soil1(jj)%str(l)%c * frac_mixed
	          smix(20+npmx+2) = smix(20+npmx+2) + soil1(jj)%lig(l)%c * frac_mixed
	          smix(20+npmx+3) = smix(20+npmx+3) + soil1(jj)%lig(l)%n* frac_mixed
	          smix(20+npmx+4) = smix(20+npmx+4) + soil1(jj)%meta(l)%c * frac_mixed
	          smix(20+npmx+5) = smix(20+npmx+5) + soil1(jj)%meta(l)%m * frac_mixed
	          smix(20+npmx+6) = smix(20+npmx+6) + soil1(jj)%lig(l)%m * frac_mixed
	          smix(20+npmx+7) = smix(20+npmx+7) + soil1(jj)%str(l)%m * frac_mixed  
	        
	          smix(20+npmx+8) = smix(20+npmx+8) + soil1(jj)%str(l)%n * frac_mixed
	          smix(20+npmx+9) = smix(20+npmx+9) + soil1(jj)%meta(l)%n * frac_mixed
	          smix(20+npmx+10) = smix(20+npmx+10) +soil1(jj)%microb(l)%n* frac_mixed
	          smix(20+npmx+11) = smix(20+npmx+11) + soil1(jj)%hact(l)%n * frac_mixed
	          smix(20+npmx+12) = smix(20+npmx+12) + soil1(jj)%hsta(l)%n * frac_mixed  
	        end if
        end do
     
        ! sand, silt and clay are % so divide by tillage depth
        smix(17) = smix(17) / dtil
        smix(18) = smix(18) / dtil
        smix(19) = smix(19) / dtil
        
        ! update nutreint amount in the ponding water
        wet(jj)%no3  = smix(1) * wet_ob(jj)%depth * 1000. / tdep * hru(jj)%area_ha !kg
        wet(jj)%nh3  = smix(3) * wet_ob(jj)%depth * 1000. / tdep * hru(jj)%area_ha
        wet(jj)%solp = smix(7) * wet_ob(jj)%depth * 1000. / tdep * hru(jj)%area_ha
        wet(jj)%orgn = smix(8) * wet_ob(jj)%depth * 1000. / tdep * hru(jj)%area_ha
        wet(jj)%sedp = smix(9) * wet_ob(jj)%depth * 1000. / tdep * hru(jj)%area_ha


        do l = 1, soil(jj)%nly
			
          ! reconstitute each soil layer 
          frac_non_mixed = sol_msn(l) / sol_mass(l)
            
          soil1(jj)%mn(l)%no3 = soil1(jj)%mn(l)%no3 * frac_non_mixed + smix(1) * frac_dep1(l) ! mixed in water and soils
          soil1(jj)%hsta(l)%n = soil1(jj)%hsta(l)%n * frac_non_mixed + smix(2) * frac_dep(l)
          soil1(jj)%mn(l)%nh4 = soil1(jj)%mn(l)%nh4 * frac_non_mixed + smix(3) * frac_dep1(l) ! mixed in water and soils
          soil1(jj)%mp(l)%lab = soil1(jj)%mp(l)%lab * frac_non_mixed + smix(4) * frac_dep(l)
          soil1(jj)%hsta(l)%p = soil1(jj)%hsta(l)%p * frac_non_mixed + smix(5) * frac_dep(l)
          soil1(jj)%hact(l)%n = soil1(jj)%hact(l)%n * frac_non_mixed + smix(6) * frac_dep(l)
          soil1(jj)%mp(l)%act = soil1(jj)%mp(l)%act * frac_non_mixed + smix(7) * frac_dep1(l) ! mixed in water and soils
          soil1(jj)%tot(l)%n = soil1(jj)%tot(l)%n * frac_non_mixed + smix(8) * frac_dep1(l)   ! mixed in water and soils
          soil1(jj)%tot(l)%p = soil1(jj)%tot(l)%p * frac_non_mixed + smix(9) * frac_dep1(l)   ! mixed in water and soils
          soil1(jj)%mp(l)%sta = soil1(jj)%mp(l)%sta * frac_non_mixed + smix(10) * frac_dep(l)
          soil1(jj)%tot(l)%m = soil1(jj)%tot(l)%m * frac_non_mixed + smix(11) * frac_dep(l)
          soil1(jj)%man(l)%c = soil1(jj)%man(l)%c * frac_non_mixed + smix(12) * frac_dep(l)
          soil1(jj)%man(l)%n = soil1(jj)%man(l)%n * frac_non_mixed + smix(13) * frac_dep(l)
          soil1(jj)%man(l)%p = soil1(jj)%man(l)%p * frac_non_mixed + smix(14) * frac_dep(l)
          soil1(jj)%tot(l)%c = soil1(jj)%tot(l)%c * frac_non_mixed + smix(15) * frac_dep(l)
            
          soil(jj)%phys(l)%clay = soil(jj)%phys(l)%clay * frac_non_mixed + smix(17) * frac_mixed
          soil(jj)%phys(l)%silt = soil(jj)%phys(l)%silt * frac_non_mixed + smix(18) * frac_mixed
          soil(jj)%phys(l)%sand = soil(jj)%phys(l)%sand * frac_non_mixed + smix(19) * frac_mixed

          do k = 1, npmx
            cs_soil(jj)%ly(l)%pest(k) = cs_soil(jj)%ly(l)%pest(k) * frac_non_mixed + smix(20+k) * frac_dep(l)
          end do

          if (bsn_cc%cswat == 2) then
          soil1(jj)%str(l)%c = soil1(jj)%str(l)%c * frac_non_mixed + smix(20+npmx+1) * frac_dep(l)
          soil1(jj)%lig(l)%c = soil1(jj)%lig(l)%c * frac_non_mixed + smix(20+npmx+2) * frac_dep(l)
          soil1(jj)%lig(l)%n = soil1(jj)%lig(l)%n * frac_non_mixed + smix(20+npmx+3) * frac_dep(l)
          soil1(jj)%meta(l)%c = soil1(jj)%meta(l)%c * frac_non_mixed + smix(20+npmx+4) * frac_dep(l)
          soil1(jj)%meta(l)%m = soil1(jj)%meta(l)%m * frac_non_mixed + smix(20+npmx+5) * frac_dep(l)
          soil1(jj)%lig(l)%m = soil1(jj)%lig(l)%m * frac_non_mixed + smix(20+npmx+6) * frac_dep(l)
          soil1(jj)%str(l)%m = soil1(jj)%str(l)%m * frac_non_mixed + smix(20+npmx+7)* frac_dep(l)
          soil1(jj)%str(l)%n = soil1(jj)%str(l)%n * frac_non_mixed + smix(20+npmx+8) * frac_dep(l)
          soil1(jj)%meta(l)%n = soil1(jj)%meta(l)%n * frac_non_mixed + smix(20+npmx+9) * frac_dep(l)
          soil1(jj)%microb(l)%n = soil1(jj)%microb(l)%n * frac_non_mixed + smix(20 + npmx + 10) * frac_dep(l)
          soil1(jj)%hact(l)%n = soil1(jj)%hact(l)%n * frac_non_mixed + smix(20 + npmx + 11) * frac_dep(l)
          soil1(jj)%hsta(l)%n = soil1(jj)%hsta(l)%n * frac_non_mixed + smix(20 + npmx+12) * frac_dep(l)
          end if
	       end do
	
        !if (bsn_cc%cswat == 1) then
        !    call mgt_tillfactor(jj,bmix,emix,dtil)
        !end if
      end if

      return
      end subroutine mgt_newtillmix_wet