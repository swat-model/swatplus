      subroutine plantparm_init
    
      use basin_module
      use maximum_data_module
      use plant_data_module
      
      implicit none 

      integer :: ic           !none        |counter
      real :: c1              !            |
      real :: b1              !            | 
      real :: b2              !            |
      real :: b3              !            |
      
      do ic = 1, db_mx%plantparm
        if (pldb(ic)%bm_dieoff <= 1.e-6) pldb(ic)%bm_dieoff = 1.00

        !! set default values
        if (pldb(ic)%ext_coef < 1.e-6) pldb(ic)%ext_coef = 0.65
        if (pldb(ic)%rsdco_pl < 1.e-6) pldb(ic)%rsdco_pl = bsn_prm%rsdco
        if (pldb(ic)%usle_c <= 0.0) pldb(ic)%usle_c = 0.001
        if (pldb(ic)%usle_c >= 1.0) pldb(ic)%usle_c = 1.0
        if (pldb(ic)%blai <= 0.0) pldb(ic)%blai = 0.0
        if (pldb(ic)%blai >= 10.0) pldb(ic)%blai = 10.0
	    if (pldb(ic)%rsr1 <= 0.0) pldb(ic)%rsr1 = 0.4
	    if (pldb(ic)%rsr2 <= 0.0) pldb(ic)%rsr2 = 0.2
        if (pldb(ic)%aeration <= 0.0) pldb(ic)%aeration = 0.2
	    if (pldb(ic)%rsd_pctcov <= 0.0) pldb(ic)%rsd_pctcov = 0.4
	    if (pldb(ic)%rsd_covfac <= 0.0) pldb(ic)%rsd_covfac = 0.04
        
        !! check if tuber, root to total biomass ratio = 0.7
        if (pldb(ic)%typ == "warm_annual_tuber" .or. pldb(ic)%typ == "cold_annual_tuber") then
           pldb(ic)%rsr2 = 0.7
        end if
        !! set seed harvest index for trees and grasses to 0.02
        if (pldb(ic)%hvsti > 0.7) pldb(ic)%hvsti = 0.02

        if (pldb(ic)%bio_e > 0. .and. pldb(ic)%plantnm /= "WATR") then

!!        determine shape parameters for the plant population-lai equation
          if (pldb(ic)%pop1 + pldb(ic)%pop2 > 1.e-6) then
            pldb(ic)%pop1 = pldb(ic)%pop1 / 1001.
            pldb(ic)%pop2 = pldb(ic)%pop2 / 1001.
            call ascrv(pldb(ic)%frlai1,pldb(ic)%frlai2,                     &               
              pldb(ic)%pop1,pldb(ic)%pop2,plcp(ic)%popsc1,plcp(ic)%popsc2)
          end if
!!        determine shape parameters for the leaf area development equation
          call ascrv(pldb(ic)%laimx1,pldb(ic)%laimx2,pldb(ic)%frgrw1,       & 
              pldb(ic)%frgrw2,plcp(ic)%leaf1,plcp(ic)%leaf2)
          
!!        The other point used to determine shape parameters for radiation
!!        use efficiency is the ambient CO2 level (330 ul/l) and the
!!        biomass-energy ratio (bio_e) given for the crop/land cover.
          c1 = 330.                        !! ambient CO2
          if (pldb(ic)%co2hi == 330.) pldb(ic)%co2hi = 660.
          b1 = pldb(ic)%bio_e * .01        !! "ambient" bio-e ratio/100
          b2 = pldb(ic)%bioehi * .01                !! "elevated" bio-e ratio/100

!!        determine shape parameters for the radiation use efficiency equation
          call ascrv(b1, b2, c1, pldb(ic)%co2hi, plcp(ic)%ruc1, plcp(ic)%ruc2)

          plcp(ic)%cvm = Log(pldb(ic)%usle_c)

!!        nitrogen uptake parameters
!!        fix bad input for pltnfr(3,ic)
          if (pldb(ic)%pltnfr1 - pldb(ic)%pltnfr2 < .0001)                  &             
                         pldb(ic)%pltnfr2 = pldb(ic)%pltnfr1 - .0001
          if (pldb(ic)%pltnfr2 - pldb(ic)%pltnfr3 < .0001)                  &             
                         pldb(ic)%pltnfr3 = .75 * pldb(ic)%pltnfr3
          b1 = pldb(ic)%pltnfr1 - pldb(ic)%pltnfr3   !!normalize N fractions
          b2 = 1. - (pldb(ic)%pltnfr2 - pldb(ic)%pltnfr3) / b1
          b3 = 1. - .00001 / b1
!!        determine shape parameters for plant nitrogen uptake equation
          call ascrv(b2, b3, 0.5, 1.0, plcp(ic)%nup1, plcp(ic)%nup2)

!!        phosphorus uptake parameters
!!        fix bad input for pltpfr3
          if (pldb(ic)%pltpfr1 - pldb(ic)%pltpfr2 < .0001)                  &              
                         pldb(ic)%pltpfr2 = pldb(ic)%pltpfr1 - .0001
          if (pldb(ic)%pltpfr2 - pldb(ic)%pltpfr3 < .0001)                  &           
                         pldb(ic)%pltpfr3 = .75 * pldb(ic)%pltpfr3
          b1 = pldb(ic)%pltpfr1 - pldb(ic)%pltpfr3     !!normalize P fractions
          b2 = 1. - (pldb(ic)%pltpfr2 - pldb(ic)%pltpfr3) / b1
          b3 = 1. - .00001 / b1
!!        determine shape parameters for plant phosphorus uptake equation
          call ascrv(b2, b3, .5, 1., plcp(ic)%pup1, plcp(ic)%pup2)

!!        calculate slope in stomatal conductance equation
          plcp(ic)%vpd2 = (1. - pldb(ic)%gmaxfr) / (pldb(ic)%vpdfr - 1.)
 
        end if

      end do

      return
      end subroutine plantparm_init