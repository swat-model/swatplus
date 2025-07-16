      subroutine pl_mortality
      
      use plant_data_module
      use basin_module
      use hru_module, only : ihru, ipl
      use plant_module
      use carbon_module
      use organic_mineral_mass_module
      use soil_module
      
      implicit none 
      
      integer :: j = 0          !none               |HRU number
      integer :: idp = 0        !                   |
      integer :: ly = 0         !                   |soil layer number
      real :: bm_dieoff = 0.
      real :: rto = 0.
      real :: rto1 = 0.
               
      j = ihru
      idp = pcom(j)%plcur(ipl)%idplt

      !keep biomass below maximum - excess to residue (need to include c, n and p adjustments)
      bm_dieoff = (1. + pldb(idp)%bm_dieoff) * (pl_mass(j)%ab_gr(ipl)%m - (pldb(idp)%bmx_peren * 1000.))  !t/ha -> kg/ha
      if (bm_dieoff > 1.e-6 .and. pl_mass(j)%ab_gr(ipl)%m > bm_dieoff) then

        !! partition all plant components by above ground ratio

        rto = bm_dieoff / pl_mass(j)%ab_gr(ipl)%m  ! Fraction of dead biomass

        rto = amin1 (1., rto)

        !! add dead material to residue
        rto1 = 1. - rto  ! Fraction remaining of living biomass
        rto1 = max (0., rto1)
        
        !! add above ground biomass that has died off to surface residue pools
        soil1(j)%rsd(1) = soil1(j)%rsd(1) + rto * pl_mass(j)%ab_gr(ipl)
        
        !! add dead roots to soil residue pools
        ! if (bsn_cc%cswat == 2) then
          do ly = 1, soil(j)%nly
            soil1(j)%rsd(ly) = soil1(j)%rsd(ly) + rto * soil(j)%ly(ly)%rtfr * pl_mass(j)%root(ipl)
          end do
        ! end if

        ! Reduce remaining living biomass  
        pl_mass(j)%tot(ipl) = rto1 * pl_mass(j)%tot(ipl)
        pl_mass(j)%ab_gr(ipl) = rto1 * pl_mass(j)%ab_gr(ipl)
        pl_mass(j)%leaf(ipl) = rto1 * pl_mass(j)%leaf(ipl)
        pl_mass(j)%stem(ipl) = rto1 * pl_mass(j)%stem(ipl)
        pl_mass(j)%seed(ipl) = rto1 * pl_mass(j)%seed(ipl)
        pl_mass(j)%root(ipl) = rto1 * pl_mass(j)%root(ipl)
        
      end if

      return
      end subroutine pl_mortality
