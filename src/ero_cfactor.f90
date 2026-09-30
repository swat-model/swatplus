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
      use hru_module, only : usle_cfac, ihru 
      use plant_module
      use plant_data_module
      use organic_mineral_mass_module
      use time_module
      use erosion_module
      use utils
      
      implicit none

      integer :: j = 0              !none          |HRU number
      integer :: ipl = 0            !none          |sequential plant number
      integer :: idp = 0            !none          |plant number in data file - pldb
      real :: c = 0.                !              |usle c factor
      real :: ab_gr_t = 0.          !tons          |total above ground biomass of plant community
      real :: rsd_covfact = 0.      !              |combined exponent for residue cover factor
      real :: rsd_sumfac = 0.       !tons          |sum of residue cover factor by plant
      real :: grcov_frac = 0.       !frac          |fraction of ground cover factor for all plants
      real :: bio_covfact = 0.      !              |combined exponent for growing biomass factor
      
      j = ihru
      
      c = 0.
      rsd_sumfac = 0.
      ab_gr_t = 0.
      
      !! new method using residue and biomass cover - from APEX
      do ipl = 1, pcom(j)%npl
        idp = pcom(j)%plcur(ipl)%idplt
        rsd_sumfac = rsd_sumfac + pldb(idp)%ero_rsdfac * (pl_mass(j)%rsd(ipl)%m + 1.) / 1000.
        ab_gr_t = ab_gr_t + 0.2 * pldb(idp)%ero_biofac * pl_mass(j)%ab_gr(ipl)%m / 1000.
      end do
      
      rsd_covfact = exp_w(-rsd_sumfac)
      rsd_covfact = Max(1.e-8, rsd_covfact)
      rsd_covfact = Min(1., rsd_covfact)
        
      bio_covfact = exp_w(-ab_gr_t)
      bio_covfact = Max(1.e-8, bio_covfact)
      bio_covfact = Min(1., bio_covfact)
        
      c = Max(1.e-10, rsd_covfact * bio_covfact)
        
      !! erosion output variables
      ero_output(j)%ero_d%c = c
      ero_output(j)%ero_d%rsd_m = pl_mass(j)%rsd_tot%m
      ero_output(j)%ero_d%grcov_frac = grcov_frac
      ero_output(j)%ero_d%rsd_covfact = rsd_covfact
      ero_output(j)%ero_d%bio_covfact = bio_covfact
      
      usle_cfac(ihru) = c
      
      return
      end subroutine ero_cfactor