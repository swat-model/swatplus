      subroutine ero_cfactor
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine predicts daily soil loss caused by water erosion
!!    using the modified universal soil loss equation

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cvm(:)      |none          |natural log of USLE_C (the minimum value
!!                               |of the USLE C factor for the land cover)
!!    hru_km(:)   |km**2         |area of HRU in square kilometers
!!    surfq(:)    |mm H2O        |surface runoff for the day in HRU
!!    usle_ei     |100(ft-tn in)/(acre-hr)|USLE rainfall erosion index
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cklsp(:)    |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module
      use hru_module, only : usle_cfac, cvm_com, ihru 
      use plant_module
      use plant_data_module
      use organic_mineral_mass_module
      use time_module
      use erosion_module
      
      implicit none

      integer :: j = 0      !none          |HRU number
      integer :: ipl = 0    !none          |sequential plant number
      integer :: idp = 0    !none          |plant number in data file - pldb
      real :: c = 0.        !              |
      real :: ab_gr_t = 0.  !tons          |above ground biomass of each plant
      real :: rsd_pctcov = 0. !              |percent of cover by residue
      real :: rsd_covfact = 0.!              |residue cover factor
      real :: can_covfact = 0.!              |canopy cover factor
      real :: can_frcov = 0.  !              |fraction of canopy cover
      real :: rsd_sumfac = 0. !              |sum of residue cover factor by plant
      real :: grnd_sumfac = 0.!              |ground cover factor for each plant
      real :: grnd_covfact = 0.!              |sum of plant ground cover factor by plant
      real :: bio_covfac = 0.
      real :: cover = 0.    !kg/ha         |soil cover - sum of residue and biomass
      j = ihru

      bsn_cc%cfac = 1   !***jga
      !! HRU sediment calculations
      if (bsn_cc%cfac == 0) then
        !! old method using minimum c factor (average of each plant in community)
        cover = pl_mass(j)%ab_gr_com%m + soil1(j)%rsd(1)%m
        if (pcom(j)%npl > 0) then
          c = Exp((-.2231 - cvm_com(j)) * Exp(-.00115 * cover) + cvm_com(j))
        else
          if (cover > 1.e-4) then
            c = Exp(-.2231 * Exp(-.00115 * cover))               
          else
            c = .8
          end if
        end if
      else
        !! new method using residue and biomass cover
        grnd_sumfac = 0.
        rsd_sumfac = pldb(idp)%rsd_pctcov * (soil1(j)%rsd(1)%m +1.) / 1000.
        do ipl = 1, pcom(j)%npl
          idp = pcom(j)%plcur(ipl)%idplt
          if (pl_mass(j)%ab_gr(ipl)%m > 1.e-6) then
            ab_gr_t = pl_mass(j)%ab_gr(ipl)%m / 1000.
            grnd_sumfac = grnd_sumfac + 100. * pldb(idp)%usle_c / ab_gr_t
          end if
        end do
        if (grnd_sumfac < 1.e-6) then
          grnd_sumfac = 10.
        end if
        
        rsd_pctcov = 100. * (1. - Exp(-rsd_sumfac))
        rsd_pctcov = amin1 (100., rsd_pctcov)
        rsd_pctcov = max (0., rsd_pctcov)
        rsd_covfact = Exp (-pcom(j)%rsd_covfac * rsd_pctcov)
        
        can_frcov = amin1 (1., pcom(j)%lai_sum)
        can_frcov = amin1 (1., pcom(j)%lai_sum / 3.)
        can_covfact = 1. - can_frcov * Exp(-.328 * pcom(j)%cht_mx)
        can_covfact = amin1 (1., can_covfact)
        can_covfact = max (0., can_covfact)
        
        grnd_sumfac = Min (10., grnd_sumfac)
        grnd_covfact = (1. - Exp(-grnd_sumfac))
        grnd_covfact = amin1 (1., grnd_covfact)
        grnd_covfact = max (0., grnd_covfact)
        
        !! ***jga
        !grnd_covfact = 1.34 + 0.225 * log(pldb(idp)%usle_c)
        !grnd_covfact = amin1 (1., grnd_covfact)
        !grnd_covfact = max (0., grnd_covfact)
        c = Max(1.e-10, rsd_covfact * can_covfact * grnd_covfact)
        
        !! newer method using residue and biomass cover
        rsd_sumfac = (soil1(j)%rsd(1)%m +1.) / 1000.
        grnd_sumfac = 0.
        can_covfact = 10000.
        do ipl = 1, pcom(j)%npl
          idp = pcom(j)%plcur(ipl)%idplt
          ab_gr_t = pl_mass(j)%ab_gr(ipl)%m / 1000.
          grnd_sumfac = grnd_sumfac + ab_gr_t
          !! grnd_covfact = grnd_covfact + pldb(idp)%usle_c * ab_gr_t / (ab_gr_t + exp(1.175 - 1.748 * ab_gr_t))
          can_covfact = amin1 (can_covfact, pcom(j)%plg(ipl)%cht)
        end do
        !grnd_covfact = grnd_sumfac / (grnd_sumfac + exp(1.175 - 1.748 * grnd_sumfac))
        rsd_covfact = exp(-bsn_prm%rsd_covco * rsd_sumfac)
        
        can_frcov = amin1 (1., pcom(j)%lai_sum / 3.)
        can_covfact = 1. - can_frcov * Exp(-.328 * pcom(j)%cht_mx)
        
        grnd_covfact = exp(-pldb(idp)%usle_c * grnd_sumfac)
        !! bio_covfac = 1. - grnd_covfact * exp(-0.1 * can_covfact)
        c = Max(1.e-10, rsd_covfact * grnd_covfact)  ! * can_covfact)
        
        !! erosion output variables
        ero_output(j)%ero_d%c = c
        ero_output(j)%ero_d%rsd_m = soil1(j)%rsd(1)%m
        ero_output(j)%ero_d%rsd_pctcov = rsd_pctcov
        ero_output(j)%ero_d%rsd_cfac = rsd_covfact
        ero_output(j)%ero_d%can_lai3 = can_frcov
        ero_output(j)%ero_d%canhgt = pcom(j)%cht_mx
        ero_output(j)%ero_d%can_cfac = can_covfact
        
      end if

      
      usle_cfac(ihru) = c
      
      return
      end subroutine ero_cfactor