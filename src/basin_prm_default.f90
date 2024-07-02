       subroutine basin_prm_default
    
       use basin_module
       use hru_module, only :  uptake
       
       implicit none
      
         if (bsn_prm%evlai < 1.e-6) bsn_prm%evlai = 3.0           !! leaf area index at which no evap occurs
         if (bsn_prm%ffcb < 1.e-6) bsn_prm%ffcb = 0.              !! intial soil water cont expressed as a fraction of fc 
         if (bsn_prm%surlag < 1.e-6) bsn_prm%surlag = 4.0         !! surface runoff lag time (days)
         if (bsn_prm%adj_pkr < 1.e-6) bsn_prm%adj_pkr = 1.0       !! peak rate adjustment factor in the subbasin
         if (bsn_prm%prf < 1.e-6) bsn_prm%prf = 1.0               !! peak rate adjustment factor for sediment routing in the channel
         if (bsn_prm%cmn < 1.e-6) bsn_prm%cmn = 0.0003            !! rate factor for mineralization on active org N
         if (bsn_prm%n_updis < 1.e-6) bsn_prm%n_updis = 20.0      !! nitrogen uptake dist parm
         if (bsn_prm%p_updis < 1.e-6) bsn_prm%p_updis = 20.0      !! phosphorus uptake dist parm
         if (bsn_prm%nperco < 1.e-6) bsn_prm%nperco = 0.1         !! nitrate perc coeff (0-1)
                                                                  !!   0 = conc of nitrate in surface runoff is zero                                                                  !!   1 = perc has same conc of nitrate as surf runoff
         if (bsn_prm%pperco < 1.e-6) bsn_prm%pperco = 10.0        !! phos perc coeff (0-1)
                                                                  !!  0 = conc of sol P in surf runoff is zero                                                                  !!  1 = percolate has some conc of sol P as surf runoff      
         if (bsn_prm%phoskd < 1.e-6) bsn_prm%phoskd = 175.0       !! phos soil partitioning coef
         if (bsn_prm%psp < 1.e-6) bsn_prm%psp = 0.40              !! phos availability index
         if (bsn_prm%rsdco < 1.e-6) bsn_prm%rsdco = 0.05          !! residue decomposition coeff
         if (bsn_prm%percop < 1.e-6) bsn_prm%percop = 0.5         !! pestcide perc coeff (0-1)
         if (bsn_prm%msk_co1 < 1.e-6) bsn_prm%msk_co1 = 0.75      !! calibration coeff to control impact of the storage
                                                                  !!  time constant for the reach at bankfull depth
         if (bsn_prm%msk_co2 < 1.e-6) bsn_prm%msk_co2 = 0.25      !! 
         if (bsn_prm%msk_x < 1.e-6) bsn_prm%msk_x = 0.20          !! weighting factor control relative importance of inflow rate 
                                                                  !!  and outflow rate in determining storage on reach
         if (bsn_prm%nperco_lchtile < 1.e-6) bsn_prm%nperco_lchtile = 0.50        !! n concentration coeff for tile flow and 
                                                                  !! leach from bottom layer
         !if (bsn_prm%evrch < 1.e-6) bsn_prm%evrch = 0.60          !! reach evaporation adjustment factor
         if (bsn_prm%cdn < 1.e-6) bsn_prm%cdn = 1.40              !! denitrification expoential rate coefficient        
         if (bsn_prm%sdnco < 1.e-6) bsn_prm%sdnco = 1.30          !! denitrification threshold frac of field cap
         if (bsn_prm%bact_swf < 1.e-6) bsn_prm%bact_swf = 0.15    !! frac of manure containing active colony forming units
         if (bsn_prm%tb_adj < 1.e-6) bsn_prm%tb_adj = 0.          !! adjustment factor for subdaily unit hydrograph basetime
         if (bsn_prm%cn_froz < 1.e-6) bsn_prm%cn_froz = 0.000862  !! 
         !if (bsn_prm%dorm_hr < 1.e-6) bsn_prm%dorm_hr = -1.        !! time threshold used to define dormant (hrs)
         if (bsn_prm%nfixmx < 1.e-6) bsn_prm%nfixmx = 20.0        !! max daily n-fixation (kg/ha)
         if (bsn_prm%decr_min < 1.e-6) bsn_prm%decr_min = 0.01    !! 
         if (bsn_prm%rsd_covco < 1.e-6) bsn_prm%rsd_covco = 0.30  !! residue cover factor for computing frac of cover         
         if (bsn_prm%urb_init_abst < 1.e-6) bsn_prm%urb_init_abst = 0.  !! PET adjustment (%) for Penman-Montieth and Preiestly-Taylor methods
         if (bsn_prm%petco_pmpt < 0.5 .and. bsn_prm%petco_pmpt > 0.) bsn_prm%petco_pmpt = 0.0   !! reservoir sediment settling coeff
         bsn_prm%petco_pmpt = (100. + bsn_prm%petco_pmpt) / 100.    !! convert to fraction
         if (bsn_prm%uhalpha < 1.e-6) bsn_prm%uhalpha = 1.0       !! alpha coeff for est unit hydrograph using gamma func
         if (bsn_prm%eros_spl < 1.e-6) bsn_prm%eros_spl = 0.      !! coeff of splash erosion varing 0.9-3.1 
         if (bsn_prm%rill_mult < 1.e-6) bsn_prm%rill_mult = 0.    !! 
         if (bsn_prm%eros_expo < 1.e-6) bsn_prm%eros_expo = 0.    !!
         if (bsn_prm%c_factor < 1.e-6) bsn_prm%c_factor = 0.      !!
         if (bsn_prm%ch_d50 < 1.e-6) bsn_prm%ch_d50 = 0.          !! median particle diameter of main channel (mm)
         if (bsn_prm%co2 < 100.) bsn_prm%co2 = 400.               !! assume co2 concentration = 400 ppm

         !! set additional parameters
         uptake%water_dis = 10.0       !! the uptake distribution for water is hardwired - users are not allowed to modify
         uptake%water_norm = 1. - exp(-uptake%water_dis)
         uptake%n_norm = 1. - exp(-bsn_prm%n_updis)
         uptake%p_norm = 1. - exp(-bsn_prm%p_updis)
         
         !! set maximum days to store hru, ru and channel output
         bsn_prm%day_lag_mx = 2
         
         return
          
      end subroutine basin_prm_default