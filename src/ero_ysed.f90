      subroutine ero_ysed
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine predicts daily soil loss caused by water erosion
!!    using the modified universal soil loss equation

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cvm(:)      |none          |natural log of USLE_C (the minimum value
!!                               |of the USLE C factor for the land cover)
!!    hru_km(:)   |km**2         |area of HRU in square kilometers
!!    qp_cms      |m^3/s         |peak runoff rate
!!    surfq(:)    |mm H2O        |surface runoff for the day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cklsp(:)    |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bio_frcov   |              |fraction of cover by biomass - adjusted for
!!                                  canopy height
!!    grcov_fr    |              |fraction of cover by biomass as function of lai
!!    rsd_frcov   |              |fraction of cover by residue
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use hru_module, only : hru, usle_cfac, cklsp, surfq, sedyld, sanyld, silyld, clayld, lagyld, sagyld,  &
         ihru, qp_cms, usle_ei
      use soil_module
      use erosion_module
      use climate_module
      
      implicit none

      integer :: j           !none                   |HRU number
      real :: usle           !metric tons/ha         | daily soil loss predicted with USLE equation
      real :: rock           !percent                |rock fragments

      j = ihru
      
      !! initialize variables
      cklsp(j) = usle_cfac(j) * hru(j)%lumv%usle_mult
      rock = Exp(-.053 * soil(j)%phys(1)%rock)

      !! compute sediment yield with musle - t
      sedyld(j) = (10. * surfq(j) * qp_cms * hru(j)%area_ha) ** .56 * cklsp(j)
      !qp_cms = qp_cms * 3.6 / hru(j)%km !cms--> mm/h
      !! this is the form of MUSLE in APEX documentation - same results as swat equation above ! t/ha
      !sedyld(j) = 1.586 * rock * (surfq(j) * qp_cms) ** .56 * (hru(j)%area_ha) ** 0.12 * &
      !           usle_cfac(j) * soil(j)%ly(1)%usle_k * hru(j)%lumv%usle_p * hru(j)%lumv%usle_ls
      !sedyld(j) = sedyld(j) * hru(j)%area_ha

      if (sedyld(j) < 0.) sedyld(j) = 0.

      !!adjust sediment yield for protection of snow cover
      if (hru(j)%sno_mm > 0.) then
        if (sedyld(j) < 1.e-6) sedyld(j) = 0.0
      else if (hru(j)%sno_mm > 100.) then
        sedyld(j) = 0.
      else
        sedyld(j) = sedyld(j) / Exp(hru(j)%sno_mm * 3. / 25.4)
      end if

	  !! Particle size distribution of sediment yield
	  !sanyld(j) = sedyld(j) * soil(j)%det_san    !! Sand yield
	  !silyld(j) = sedyld(j) * soil(j)%det_sil    !! Silt yield
	  !clayld(j) = sedyld(j) * soil(j)%det_cla    !! Clay yield
	  !sagyld(j) = sedyld(j) * soil(j)%det_sag    !! Small Aggregate yield
	  !lagyld(j) = sedyld(j) * soil(j)%det_lag    !! Large Aggregate yield

      !! compute erosion with usle (written to output for comparison)
      usle = 1.292 * usle_ei * cklsp(j) / 11.8

      !! erosion output variables
      ero_output(j)%ero_d%sedyld = sedyld(j)
      ero_output(j)%ero_d%precip = w%precip
      ero_output(j)%ero_d%surfq = surfq(j)
      ero_output(j)%ero_d%peak = qp_cms
      !! sum daily erosion output
      ero_output(j)%n_events = ero_output(j)%n_events
      ero_output(j)%ero_ave = ero_output(j)%ero_ave + ero_output(j)%ero_d
        
      return
      end subroutine ero_ysed