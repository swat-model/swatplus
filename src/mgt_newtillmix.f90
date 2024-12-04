      subroutine mgt_newtillmix (jj, bmix, idtill)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine mixes residue and nutrients during tillage and 
!!    biological mixing
!!    New version developed by Armen R. Kemanian in collaboration with Stefan Julich and Cole Rossi
!!    Mixing was extended to all layers
!!    A subroutine to simulate stimulation of organic matter decomposition was added
!!    March 2009: testing has been minimal and further adjustments are expected
!!    use with caution and report anomalous results to akemanian@brc.tamus.edu and jeff.arnold@usda.edu

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
      use hru_module, only: tillage_days, tillage_depth, tillage_switch
      use soil_module
      use constituent_mass_module
      use plant_module
      
      implicit none

      integer, intent (in) :: jj       !none           |HRU number
      integer, intent (in) :: idtill   !none           |tillage type
      real, intent (in) :: bmix        !               | 
      integer :: l = 0                 !none           |counter
      integer :: k = 0                 !none           |counter
      integer :: kk = 0                !               |
      integer :: npmx = 0              !               |
      integer :: ipl = 0
      real :: emix = 0.                !none           |mixing efficiency
      real :: dtil = 0.                !mm             |depth of mixing
      real :: frac_mixed = 0.          !               |
      real :: frac_non_mixed = 0.      !               |
      real :: sol_mass(soil(jj)%nly)    !              |mass of the soil layer
      real :: sol_msm(soil(jj)%nly)     !              |sol_mass mixed
      real :: sol_msn(soil(jj)%nly)     !              |sol_mass not mixed 
      real :: frac_dep(soil(jj)%nly)    !              |fraction of soil layer in tillage depth
      real :: frac1 = 0.
      real :: frac2 = 0.
      real :: mix_clay
      real :: mix_silt
      real :: mix_sand

      npmx = cs_db%num_pests

      mix_mn = mnz
      mix_mp = mpz
      mix_org%tot = orgz
      mix_org%rsd = orgz
      mix_org%hact = orgz
      mix_org%hsta = orgz
      mix_org%hs = orgz
      mix_org%hp = orgz
      mix_org%microb = orgz
      mix_org%str = orgz
      mix_org%lig = orgz
      mix_org%meta = orgz
      mix_org%man = orgz
      mix_org%water = orgz
    
      frac_mixed = 0.
      frac_non_mixed = 0.
      emix = 0.
      dtil = 0.
      if (bmix > 1.e-6) then
        !! biological mixing
        emix = bmix !bmix MJW (rev 412)
        kk = soil(jj)%nly
        dtil = Min(soil(jj)%phys(kk)%d, 50.) ! it was 300.  MJW (rev 412)
      else 
        !! tillage operation
        emix = tilldb(idtill)%effmix
        dtil = tilldb(idtill)%deptil
      end if

      !!by zhang DSSAT tillage
      !!=======================
      if (bsn_cc%cswat == 2) then
          tillage_days(jj) = 0
          tillage_depth(jj) = dtil
          tillage_switch(jj) = 1
      end if
      !!by zhang DSSAT tillage
      !!=======================

      sol_mass = 0.
      sol_msm = 0.
      sol_msn = 0.

      !! incorporate pathogens - no mixing - lost from transport
      if (dtil > 10.) then     
        !! incorporate pathogens
      end if

      do l = 1, soil(jj)%nly
        sol_mass(l) = (soil(jj)%phys(l)%thick / 1000.) * 10000. *                    &
            soil(jj)%phys(1)%bd * 1000. * (1.- soil(jj)%phys(l)%rock/ 100.)
      end do

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
          else if (soil(jj)%phys(l)%d > dtil .and. soil(jj)%phys(l-1)%d < dtil) then 
            sol_msm(l) = emix * sol_mass(l) * (dtil - soil(jj)%phys(l-1)%d) / soil(jj)%phys(l)%thick
            sol_msn(l) =  sol_mass(l) - sol_msm(l)
            frac_dep(l) = (dtil - soil(jj)%phys(l-1)%d) / dtil
          else
            sol_msm(l) = 0.
            sol_msn(l) = sol_mass(l)
            frac_dep(l) = 0.
          end if

          !! calculate the mass each mixed element
          frac_mixed = sol_msm(l) / sol_mass(l)
          
          mix_mn = mix_mn + frac_mixed * soil1(jj)%mn(l)
          mix_mp = mix_mp + frac_mixed * soil1(jj)%mp(l)
          mix_org%tot = mix_org%tot + frac_mixed * soil1(jj)%tot(l)
          mix_org%rsd = mix_org%rsd + frac_mixed * soil1(jj)%rsd(l)
          mix_org%hact = mix_org%hact + frac_mixed * soil1(jj)%hact(l)
          mix_org%hsta = mix_org%hsta + frac_mixed * soil1(jj)%hsta(l)
          mix_org%hs = mix_org%hs + frac_mixed * soil1(jj)%hs(l)
          mix_org%hp = mix_org%hp + frac_mixed * soil1(jj)%hp(l)
          mix_org%microb = mix_org%microb + frac_mixed * soil1(jj)%microb(l)
          mix_org%str = mix_org%str + frac_mixed * soil1(jj)%str(l)
          mix_org%lig = mix_org%lig + frac_mixed * soil1(jj)%lig(l)
          mix_org%meta = mix_org%meta + frac_mixed * soil1(jj)%meta(l)
          mix_org%man = mix_org%man + frac_mixed * soil1(jj)%man(l)
          mix_org%water = mix_org%water + frac_mixed * soil1(jj)%water(l)
        end do
     
          !! sand, silt and clay are % so divide by tillage depth
          mix_clay = mix_clay / dtil
          mix_silt = mix_silt / dtil
          mix_sand = mix_sand / dtil

          do l = 1, soil(jj)%nly
            
            ! reconstitute each soil layer 
            frac_non_mixed = sol_msn(l) / sol_mass(l)
            
            soil1(jj)%mn(l) = frac_non_mixed * soil1(jj)%mn(l) + frac_dep(l) * mix_mn
            soil1(jj)%mp(l) = frac_non_mixed * soil1(jj)%mp(l) + frac_dep(l) * mix_mp
            soil1(jj)%tot(l) = frac_non_mixed * soil1(jj)%tot(l) + frac_dep(l) * mix_org%tot
            soil1(jj)%rsd(l) = frac_non_mixed * soil1(jj)%rsd(l) + frac_dep(l) * mix_org%rsd
            soil1(jj)%hact(l) = frac_non_mixed * soil1(jj)%hact(l) + frac_dep(l) * mix_org%hact
            soil1(jj)%hsta(l) = frac_non_mixed * soil1(jj)%hsta(l) + frac_dep(l) * mix_org%hsta
            soil1(jj)%hs(l) = frac_non_mixed * soil1(jj)%hs(l) + frac_dep(l) * mix_org%hs
            soil1(jj)%hp(l) = frac_non_mixed * soil1(jj)%hp(l) + frac_dep(l) * mix_org%hp
            soil1(jj)%microb(l) = frac_non_mixed * soil1(jj)%microb(l) + frac_dep(l) * mix_org%microb
            soil1(jj)%str(l) = frac_non_mixed * soil1(jj)%str(l) + frac_dep(l) * mix_org%str
            soil1(jj)%lig(l) = frac_non_mixed * soil1(jj)%lig(l) + frac_dep(l) * mix_org%lig
            soil1(jj)%meta(l) = frac_non_mixed * soil1(jj)%meta(l) + frac_dep(l) * mix_org%meta
            soil1(jj)%man(l) = frac_non_mixed * soil1(jj)%man(l) + frac_dep(l) * mix_org%man
            soil1(jj)%water(l) = frac_non_mixed * soil1(jj)%water(l) + frac_dep(l) * mix_org%water
            
            soil(jj)%phys(l)%clay = frac_non_mixed * soil(jj)%phys(l)%clay + frac_mixed * mix_clay
            soil(jj)%phys(l)%silt = frac_non_mixed * soil(jj)%phys(l)%silt + frac_mixed * mix_silt
            soil(jj)%phys(l)%sand = frac_non_mixed * soil(jj)%phys(l)%sand + frac_mixed * mix_sand

            !do k = 1, npmx
            !  cs_soil(jj)%ly(l)%pest(k) = cs_soil(jj)%ly(l)%pest(k) * frac_non_mixed + smix(20+k) * frac_dep(l)
            !end do

      end do
    
        if (bsn_cc%cswat == 1) then
            call mgt_tillfactor(jj,bmix,emix,dtil)
        end if
      end if

      return
      end subroutine mgt_newtillmix