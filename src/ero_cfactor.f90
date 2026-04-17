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
      use utils
      
      implicit none

      integer :: j = 0              !none          |HRU number
      integer :: ipl = 0            !none          |sequential plant number
      integer :: idp = 0            !none          |plant number in data file - pldb
      real :: c = 0.                !              |usle c factor
      real :: ab_gr_t = 0.          !tons          |total above ground biomass of plant community
      real :: rsd_covfact = 0.      !              |residue cover factor
      real :: rsd_sumfac = 0.       !tons          |sum of residue cover factor by plant
      real :: grcov_frac = 0.       !frac          |fraction of ground cover factor for all plants
      real :: bio_covfact = 0.      !              |growing biomass factor
      real :: cover = 0.            !kg/ha         |soil cover - sum of residue and biomass
      
      j = ihru

      bsn_cc%cfac = 1
      !! HRU sediment calculations
      if (bsn_cc%cfac == 0) then
        !! old method using minimum c factor (average of each plant in community)
        cover = pl_mass(j)%ab_gr_com%m + pl_mass(j)%rsd_tot%m
        if (pcom(j)%npl > 0) then
          c = exp_w((-.2231 - cvm_com(j)) * Exp(-.00115 * cover) + cvm_com(j))
        else
          if (cover > 1.e-4) then
            c = exp_w(-.2231 * exp_w(-.00115 * cover))               
          else
            c = .8
          end if
        end if
      else
        
        !! new method using residue and biomass cover - from APEX
        rsd_sumfac = (pl_mass(j)%rsd_tot%m + 1.) / 1000.
        rsd_covfact = exp_w(-bsn_prm%rsd_covco * rsd_sumfac)
        
        ab_gr_t = pl_mass(j)%ab_gr_com%m / 1000.
        grcov_frac = ab_gr_t / (ab_gr_t + exp_w(1.175 - 1.748 * ab_gr_t))
        bio_covfact = 1. - grcov_frac * exp_w(-.328 * pcom(j)%cht_mx)
        bio_covfact = Max(1.e-10, bio_covfact)
        bio_covfact = Min(1., bio_covfact)
        
        c = Max(1.e-10, rsd_covfact * bio_covfact)
        
        !! erosion output variables
        ero_output(j)%ero_d%c = c
        ero_output(j)%ero_d%rsd_m = pl_mass(j)%rsd_tot%m
        ero_output(j)%ero_d%grcov_frac = grcov_frac
        ero_output(j)%ero_d%rsd_covfact = rsd_covfact
        ero_output(j)%ero_d%bio_covfact = bio_covfact
        
      end if
      
      usle_cfac(ihru) = c
      
      return
      end subroutine ero_cfactor