      subroutine nut_orgnc2

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the amount of organic nitrogen removed in
!!    surface runoff - when using CSWAT==2 it 


!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    enratio       |none         |enrichment ratio calculated for day in HRU
!!    erorgn(:)     |none         |organic N enrichment ratio, if left blank
!!                                |the model will calculate for every event
!!    ihru          |none         |HRU number
!!    sedc_d(:)     |kg C/ha      |amount of C lost with sediment
!!
!!
!!     
!!                                |pools 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use hru_module, only : enratio, hru, ihru, sedorgn, sedyld, surfq, ipl
      use soil_module
      use organic_mineral_mass_module
      use carbon_module
      use plant_module
      use plant_data_module
      
      implicit none

      integer :: j = 0       !none          |HRU number
      integer :: ly = 0       !none          |soil layer number 
      real :: flo_loss_co = 0.        !kg N/ha       |amount of organic N in first soil layer
      real :: wt1 = 0.       !none          |conversion factor (mg/kg => kg/ha)
      real :: er = 0.        !none          |enrichment ratio
      real :: conc = 0.      !              |concentration of organic N in soil
      real :: sol_mass = 0.  !              |  
      real :: c_surlat = 0.         !              |c loss with runoff or lateral flow
      real :: c_vert = 0.           !              |c loss with vertical flow
      real :: c_horiz = 0.           !              |c loss with vertical flow
      real :: c_microb = 0.         !              |BMC LOSS WITH SEDIMENT
      real :: c_sed = 0.            !              |Organic C loss with sediment
      real :: ero_fr = 0.           !frac          |fraction of soil erosion of total soil mass
      real :: koc = 0.          !              |KOC FOR CARBON LOSS IN WATER AND SEDIMENT(500._1500.) KD = KOC * C
      real :: c_microb_fac = 0.               !              |
      real :: flo_tot = 0.          !mm            |total flow from the soil layer
      real :: c_microb_loss = 0.               !              | 
      real :: horiz_conc = 0.               !              | 
      real :: vert_conc = 0.               !              | 
      real :: perc_clyr = 0.        !              | 
      real :: latc_clyr = 0.        !              | 
      real :: n_left_rto = 0.              !              |
      real :: c_microb_perc = 0.               !              |
      real :: c_microb_sed = 0.               !              |
      real :: c_ly1 = 0.
      
      j = ihru
      
      latc_clyr = 0.        
      perc_clyr = 0.
      wt1 = 0.
      er = 0.
      
      !! total carbon in surface residue and soil humus
      c_ly1 = soil1(j)%hp(1)%n + soil1(j)%hs(1)%n + pl_mass(j)%rsd_tot%n
      !! wt = sol_bd(1,j) * sol_z(1,j) * 10. (tons/ha) -> wt1 = wt/1000
      wt1 = soil(j)%phys(1)%bd * soil(j)%phys(1)%d / 100.

      if (hru(j)%hyd%erorgn > .001) then
        er = hru(j)%hyd%erorgn
      else
        er = enratio
      end if

      !! organic n leaving hru
      conc = c_ly1 * er / wt1
      sedorgn(j) = .001 * conc * sedyld(j) / hru(j)%area_ha

      !! update soil carbon organic nitrogen pools
      if (c_ly1 > 1.e-6) then
        n_left_rto = (1. - sedorgn(j) / c_ly1)
        soil1(j)%tot(1)%n = soil1(j)%tot(1)%n * n_left_rto
        soil1(j)%hs(1)%n = soil1(j)%hs(1)%n * n_left_rto
        soil1(j)%hp(1)%n = soil1(j)%hp(1)%n * n_left_rto
        do ipl = 1, pcom(j)%npl
          pl_mass(j)%rsd(1)%n =   pl_mass(j)%rsd(1)%n * n_left_rto
        end do
        soil1(j)%meta(1)%n = soil1(j)%meta(1)%n * n_left_rto
        soil1(j)%str(1)%n = soil1(j)%str(1)%n * n_left_rto
        soil1(j)%lig(1)%n = soil1(j)%lig(1)%n * n_left_rto
      end if
      
      !! Calculate runoff and leached C&N from microbial biomass
      latc_clyr = 0.
      sol_mass = (soil(j)%phys(1)%d / 1000.) * 10000. * soil(j)%phys(1)%bd * 1000. * (1- soil(j)%phys(1)%rock / 100.)
      
      c_surlat = 0.
      c_vert = 0.
      c_microb = 0.
      c_sed = 0.
      soil1(j)%tot(1)%c = soil1(j)%hp(1)%c + soil1(j)%hs(1)%c + soil1(j)%meta(1)%c + soil1(j)%str(1)%c !Total organic carbon in layer 1
      ero_fr = MIN((sedyld(j)/hru(j)%area_ha) / (sol_mass / 1000.),.9) !fraction of soil erosion of total soil mass
      c_sed = ero_fr * soil1(j)%tot(1)%c
      soil1(j)%tot(1)%c = soil1(j)%tot(1)%c * (1.- ero_fr)
      soil1(j)%hs(1)%c = soil1(j)%hs(1)%c * (1.- ero_fr)
      soil1(j)%hp(1)%c = soil1(j)%hp(1)%c * (1.- ero_fr)
      do ipl = 1, pcom(j)%npl
        pl_mass(j)%rsd(ipl)%c = pl_mass(j)%rsd(ipl)%c * (1.- ero_fr)
      end do
          
        
      if (soil1(j)%microb(1)%c > .01) then
        koc = cb_wtr_coef%prmt_21 !KOC FOR CARBON LOSS IN WATER AND SEDIMENT(500._1500.) KD = KOC * C
        soil1(j)%tot(1)%c = soil1(j)%str(1)%c + soil1(j)%meta(1)%c + soil1(j)%hp(1)%c + soil1(j)%hs(1)%c + soil1(j)%microb(1)%c 
        c_microb_fac = .0001 * koc * soil1(j)%tot(1)%c
        flo_loss_co = soil(j)%phys(1)%ul + c_microb_fac
        flo_tot = surfq(j) + soil(j)%ly(1)%prk + soil(j)%ly(1)%flat
        c_microb_loss = 0.
        if (flo_tot > 1.E-10) then
          c_microb_loss = soil1(j)%microb(1)%c * (1. - EXP(-flo_tot / flo_loss_co)) !loss of biomass C
          !! cb_wtr_coef%prmt_44 is the ratio of soluble C conc - runoff/perc (0.1-1.)
          vert_conc = c_microb_loss / (soil(j)%ly(1)%prk + cb_wtr_coef%prmt_44 * (surfq(j) + soil(j)%ly(1)%flat))
          horiz_conc = cb_wtr_coef%prmt_44 * vert_conc
          c_horiz = vert_conc * soil(j)%ly(1)%prk 
          soil1(j)%microb(1)%c = soil1(j)%microb(1)%c - c_microb_loss
          c_surlat = vert_conc * (surfq(j) + soil(j)%ly(1)%flat)
          !! microbial carbon loss with sediment
          if (ero_fr > 0.) then
            c_horiz = c_microb_fac * soil1(j)%microb(1)%c / flo_loss_co
            c_microb_sed = ero_fr * c_horiz
          end if
        end if
      end if

      soil1(j)%microb(1)%c = soil1(j)%microb(1)%c - c_microb_sed 
      soil1(j)%tot(1)%c = soil1(j)%str(1)%c + soil1(j)%meta(1)%c + soil1(j)%hp(1)%c + soil1(j)%hs(1)%c + soil1(j)%microb(1)%c 
      hsc_d(j)%surq_c = c_surlat * (surfq(j) / (surfq(j) + soil(j)%ly(1)%flat + 1.e-6))
       
      soil(j)%ly(1)%latc = c_surlat * (soil(j)%ly(1)%flat / (surfq(j) + soil(j)%ly(1)%flat + 1.e-6))
      soil(j)%ly(1)%percc = c_vert 
      hsc_d(j)%sed_c = c_sed + c_microb_sed
      
      do ly = 2, soil(j)%nly
        c_microb_perc = soil1(j)%microb(ly)%c + c_vert
        c_vert = 0.
        if (c_microb_perc >=.01) then
          flo_tot = soil(j)%ly(ly)%prk + soil(j)%ly(ly)%flat
          if (flo_tot > 0.) then
            c_vert = c_microb_perc * (1. - EXP(-flo_tot / (soil(j)%phys(ly)%por * soil(j)%phys(ly)%thick  &
                                                           - soil(j)%phys(ly)%wpmm + .0001 * koc * soil1(j)%water(1)%c)))
          end if
        end if
        soil(j)%ly(ly)%latc = c_vert * (soil(j)%ly(ly)%flat/(soil(j)%ly(ly)%prk + soil(j)%ly(ly)%flat+1.e-6))
        soil(j)%ly(ly)%percc = c_vert - soil(j)%ly(ly)%latc
        soil1(j)%microb(ly)%c = c_microb_perc - c_vert

        !! calculate carbon in percolate and lateral flow
        if (ly == soil(j)%nly) then
          hsc_d(j)%perc_c = soil(j)%ly(ly)%percc
        end if
        latc_clyr = latc_clyr + soil(j)%ly(ly)%latc

        soil1(j)%tot(ly)%c = soil1(j)%str(ly)%c + soil1(j)%meta(ly)%c + soil1(j)%hp(ly)%c + soil1(j)%hs(ly)%c + soil1(j)%microb(ly)%c 
        soil1(j)%seq(ly)%c = soil1(j)%hp(ly)%c + soil1(j)%hs(ly)%c + soil1(j)%microb(ly)%c
      end do
     
      hsc_d(j)%latq_c = latc_clyr

      return
      end subroutine nut_orgnc2