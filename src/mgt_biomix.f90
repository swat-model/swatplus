      subroutine mgt_biomix (jj, bmix)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine mixes residue and nutrients from biological mixing
!!    New version developed by Armen R. Kemanian in collaboration with Stefan Julich and Cole Rossi
!!    A subroutine to simulate stimulation of organic matter decomposition was added
!!    March 2009: testing has been minimal and further adjustments are expected
!!    use with caution and report anomalous results to akemanian@brc.tamus.edu and jeff.arnold@usda.edu
!!    Mixing was was changed by fgeter n March 2026 to only mix soil layers from the surface down to 
!!    the top of a frozen soil or to the biomixing depth whichever is less.
!!    The amount of surf residue that can be mixed as defined by bio mixing efficieny
!!    is reduced by temperture and the depth from the surface to the top of a
!!    of a frozon soil layer.   

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
      use soil_module
      use constituent_mass_module
      use plant_module
      use plant_data_module
      use tillage_data_module
      
      implicit none
      
      external :: mgt_tillfactor,  fcgd

      integer, intent (in) :: jj       !none           |HRU number
      real, intent (in) :: bmix        !               | 
      real :: fcgd              !                     |
      integer :: l = 0                 !none           |counter
      integer :: kk = 0                !               |
      integer :: npmx = 0              !               |
      integer :: ipl = 0               !               |
      integer :: lyr_exit = 0
      real :: avg_emix = 0.             !none           |weighted averagte mixing efficiency
      real :: emix = 0.                !none           |mixing efficiency
      real :: emix_sum = 0.            !none           |sum used in computing weighted average mixing efficiency
      real :: dtil = 0.                !mm             |depth of mixing
      real :: frac_mixed = 0.          !               |
      real :: frac_non_mixed = 0.      !               |
      real, dimension(:), allocatable :: sol_mass    !              |mass of the soil layer
      real, dimension(:), allocatable :: sol_msm     !              |sol_mass mixed
      real, dimension(:), allocatable :: sol_msn     !              |sol_mass not mixed 
      real, dimension(:), allocatable :: frac_dep    !              |fraction of soil layer in tillage depth
      real :: mix_clay
      real :: mix_silt
      real :: mix_sand
      real :: mix_sw
      real :: mix_rock
      real :: mix_bd
      real :: mix_rsd
      logical :: bio_mix_event

      npmx = cs_db%num_pests
      bio_mix_event = .true.
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
      mix_clay = 0.
      mix_silt = 0.
      mix_sand = 0.
      mix_rock = 0.
      mix_sw = 0.
      mix_bd = 0.
      mix_rsd =0.
    
      frac_mixed = 0.
      frac_non_mixed = 0.
      emix = 0.
      dtil = 0.
      mix_clay = 0.
      mix_silt = 0.
      mix_sand = 0.
      lyr_exit = 0

      allocate (sol_mass(soil(jj)%nly), source = 0.)    
      allocate (sol_msm(soil(jj)%nly), source = 0.)    
      allocate (sol_msn(soil(jj)%nly), source = 0.)    
      allocate (frac_dep(soil(jj)%nly),source = 0.)    

      ! intialize soil emix and tillagef to 0.
      do l = 1, soil(jj)%nly
        soil1(jj)%emix(l) = 0.
        soil(jj)%ly(l)%tillagef = 0.
      enddo

      if (bmix > 1.e-6) then
        emix = bmix 
        kk = soil(jj)%nly
        dtil = Min(soil(jj)%phys(kk)%d, bmix_depth) ! bmix_depth as read from tillage.till

        !! incorporate pathogens - no mixing - lost from transport
        if (dtil > 10.) then     
          !! incorporate pathogens
        end if

        if (dtil > 1.e-6) then

          ! added by Armen 09/10/2010 next line only
          if (dtil < 10.0) dtil = 11.0

          ! Calculated a temperature weighted average of emix 
          emix_sum = 0.
          do l = 1, soil(jj)%nly
            if (l == 3) soil(jj)%phys(l)%tmp = -1.
            if (soil(jj)%phys(l)%tmp > 1.e-6) then
              if (soil(jj)%phys(l)%d <= dtil) then
                emix = bmix
                emix = emix * fcgd(soil(jj)%phys(l)%tmp) 
                emix_sum = emix_sum + emix * soil(jj)%phys(l)%thick
              elseif (soil(jj)%phys(l)%d > dtil .and. soil(jj)%phys(l-1)%d < dtil) then 
                emix = bmix
                emix = emix * fcgd(soil(jj)%phys(l)%tmp) 
                emix_sum = emix_sum + emix * (dtil - soil(jj)%phys(l-1)%d)
              else
                lyr_exit = l
                exit
              endif
            else
              if (l > 1) then
                dtil = soil(jj)%phys(l-1)%d
              else
                dtil = 0.
              endif
               lyr_exit = l
              exit
            endif
          enddo
          
          if (dtil > 1.e-6) then
            ! Calculate the weighted average emix. This will apply to both how
            ! much surface residue is mixed into the soil and to how much
            ! mixing occurs within the soil.
            avg_emix = emix_sum/dtil

            ! for each soil layer determine the soil mass.
            do l = 1, soil(jj)%nly
              if (l == lyr_exit) exit
              sol_mass(l) = (soil(jj)%phys(l)%thick / 1000.) * 10000. *                    &
                  soil(jj)%phys(1)%bd * 1000. * (1.- soil(jj)%phys(l)%rock/ 100.)
            end do

          
            ! Determin how much is mixed and not mixed in each soil layer.
            !! msm = mass of soil mixed for the layer
            !! msn = mass of soil not mixed for the layer       
            do l = 1, soil(jj)%nly
              if (l == lyr_exit) exit
              if (soil(jj)%phys(l)%d <= dtil) then
                sol_msm(l) = avg_emix * sol_mass(l) 
                sol_msn(l) = sol_mass(l) - sol_msm(l)
                frac_dep(l) = soil(jj)%phys(l)%thick / dtil
              else if (soil(jj)%phys(l)%d > dtil .and. soil(jj)%phys(l-1)%d < dtil) then 
                sol_msm(l) = avg_emix * sol_mass(l) * (dtil - soil(jj)%phys(l-1)%d) / soil(jj)%phys(l)%thick
                sol_msn(l) =  sol_mass(l) - sol_msm(l)
                frac_dep(l) = (dtil - soil(jj)%phys(l-1)%d) / dtil
              else
                sol_msm(l) = 0.
                sol_msn(l) = sol_mass(l)
                frac_dep(l) = 0.
                exit
              end if

              !! calculate the mass each mixed element
              frac_mixed = sol_msm(l) / sol_mass(l)
              mix_sw = mix_sw + frac_mixed * soil(jj)%phys(l)%st
              mix_bd = mix_bd + frac_mixed * soil(jj)%phys(l)%bd
              mix_sand = mix_sand + frac_mixed * soil(jj)%phys(l)%sand * sol_mass(l) / 100.
              mix_silt = mix_silt + frac_mixed * soil(jj)%phys(l)%silt * sol_mass(l) / 100.
              mix_clay = mix_clay + frac_mixed * soil(jj)%phys(l)%clay * sol_mass(l) / 100.
              
              mix_mn = mix_mn + frac_mixed * soil1(jj)%mn(l)
              mix_mp = mix_mp + frac_mixed * soil1(jj)%mp(l)
              mix_org%tot = mix_org%tot + frac_mixed * soil1(jj)%tot(l)
              
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

              do ipl = 1, pcom(jj)%npl
                ! sum up the amount mixed rsd in the soil from each plant in plant comunity
                mix_org%rsd(ipl)= mix_org%rsd(ipl) + frac_dep(l) * avg_emix * soil1(jj)%pl(ipl)%rsd(l) 
                ! now add the amount of surface residue that is mixed into each layer.
                mix_org%surf_rsd = frac_dep(l) * avg_emix * pl_mass(jj)%rsd(ipl)
                mix_org%rsd(ipl)= mix_org%rsd(ipl) + mix_org%surf_rsd
                ! subtract the amount of surface residue added to the soil from the surface residue.
                pl_mass(jj)%rsd(ipl) = pl_mass(jj)%rsd(ipl) - mix_org%surf_rsd
                pl_mass(jj)%rsd_tot = pl_mass(jj)%rsd_tot - mix_org%surf_rsd
              enddo
            end do

            do l = 1, soil(jj)%nly
              if (l == lyr_exit) exit
              ! reconstitute each soil layer 
              frac_non_mixed = sol_msn(l) / sol_mass(l)
              frac_mixed = 1. - frac_non_mixed
              
              soil1(jj)%mn(l) = frac_non_mixed * soil1(jj)%mn(l) + frac_dep(l) * mix_mn
              soil1(jj)%mp(l) = frac_non_mixed * soil1(jj)%mp(l) + frac_dep(l) * mix_mp
              soil1(jj)%tot(l) = frac_non_mixed * soil1(jj)%tot(l) + frac_dep(l) * mix_org%tot

              !! reconstitute each soil plant residue component separately
              do ipl = 1, pcom(jj)%npl
                soil1(jj)%pl(ipl)%rsd(l) = frac_non_mixed * soil1(jj)%pl(ipl)%rsd(l) +        &
                                                            frac_dep(l) * mix_org%rsd(ipl)
              enddo

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
              
              soil(jj)%phys(l)%clay = 100. / sol_mass(l) * (frac_non_mixed * soil(jj)%phys(l)%clay * sol_mass(l) / 100. &
                                                                                              + frac_dep(l) * mix_clay)
              soil(jj)%phys(l)%silt = 100. / sol_mass(l) * (frac_non_mixed * soil(jj)%phys(l)%silt * sol_mass(l) / 100. &
                                                                                              + frac_dep(l) * mix_silt)
              soil(jj)%phys(l)%sand = 100. / sol_mass(l) * (frac_non_mixed * soil(jj)%phys(l)%sand * sol_mass(l) / 100. &
                                                                                              + frac_dep(l) * mix_sand)
              soil(jj)%phys(l)%st = frac_non_mixed * soil(jj)%phys(l)%st + frac_dep(l) * mix_sw
              !soil(jj)%phys(l)%bd = frac_non_mixed * soil(jj)%phys(l)%bd + frac_dep(l) * mix_bd

              !do k = 1, npmx
              !  cs_soil(jj)%ly(l)%pest(k) = cs_soil(jj)%ly(l)%pest(k) * frac_non_mixed + smix(20+k) * frac_dep(l)
              !end do
            end do
            call mgt_tillfactor(jj,bio_mix_event,avg_emix,dtil)
          endif
        endif
      endif
      deallocate (sol_mass)    
      deallocate (sol_msm)    
      deallocate (sol_msn)    
      deallocate (frac_dep)    
      return
      end subroutine mgt_biomix
