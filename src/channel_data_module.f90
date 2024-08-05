      module channel_data_module
    
      implicit none
   
       type routing_nut_data         ! used for 2-stage ditch in chandeg and overland flow
        character(len=16) :: name = "Drainage_Ditch"
        real :: len_inc = 250       ! m               |segment length for reduction
        real :: no3_slp = 0.86      ! (mgN/m2/h)/ppm  |slope of denitrification (y-axis) and inflow no3 (x-axis)
        real :: no3_int = 0.17      ! mgN/m2/h        |intercept of denitrification rate equation
        real :: no3_slp_ob = 0.48   ! (mgN/m2/h)/ppm  |slope of denitrification (y-axis) and inflow no3 (x-axis)
        real :: no3_int_ob = 1.30   ! mgN/m2/h        |intercept of denitrification rate equation
        real :: no3_slp_ub = 1.50   ! (mgN/m2/h)/ppm  |slope of denitrification (y-axis) and inflow no3 (x-axis)
        real :: no3_int_ub = 0.03   ! mgN/m2/h        |intercept of denitrification rate equation
        real :: turb_slp = -.0002   ! (del ppm/ppm)   |slope of turbidity reduction (y) and inflow turbidity (x)
        real :: turb_int = 0.175    ! ppm             |intecept of turbidity reduction equation
        real :: tss_slp = 0.457     ! (del ppm/ppm)   |slope of total suspended solids (y) and inflow turbidity (x)
        real :: tss_int = 0.534     ! ppm             |intecept of tss reduction equation
        real :: tp_slp = 0.375      ! (del ppm/ppm)   |slope of total P reduction (y) and turbidity reduction (x)
        real :: tp_int = 1.312      ! ppm             |intecept of total P reduction equation
        real :: srp_slp = 0.646     ! (del ppm/ppm)   |slope of soluble reactive P reduction (y) and total P reduction (x)
        real :: srp_int = 0.207     ! ppm             |intecept of soluble reactive P reduction equation
        real :: turb_tss_slp = .35  ! ppm             |slope of turbidity and total suspended solids (0.2-0.4)
        real :: no3_min_conc = .05  ! ppm             |minimum no3 concentration
        real :: tp_min_conc = .06   ! ppm             |minimum tp concentration
        real :: tss_min_conc = 5    ! ppm             |minimum tss concentration
        real :: srp_min_conc = .015 ! ppm             |minimum srp concentration
      end type routing_nut_data
      type (routing_nut_data), dimension(:), allocatable :: rte_nut

      type channel_data_char_input
        character(len=16) :: name = "default"
        character(len=16) :: init                       !points to initial_cha
        character(len=16) :: hyd                        !points to hydrology.res for hydrology inputs
        character(len=16) :: sed                        !sediment inputs-points to sediment.res
        character(len=16) :: nut                        !nutrient inputs-points to nutrient.res
      end type channel_data_char_input
      type (channel_data_char_input), dimension(:), allocatable :: ch_dat_c

      type channel_init_datafiles
        character(len=16) :: name = "default"
        character(len=16) :: org_min = ""             !points to initial organic-mineral input file
        character(len=16) :: pest = ""                !points to initial pesticide input file
        character(len=16) :: path = ""                !points to initial pathogen input file
        character(len=16) :: hmet = ""                !points to initial heavy metals input file
        character(len=16) :: salt = ""                !points to initial salt input file
      end type channel_init_datafiles
      type (channel_init_datafiles), dimension(:), allocatable :: ch_init

      !rtb salt/cs
      type channel_init_datafiles_cs
        character(len=16) :: name = "default"
        character(len=16) :: pest = ""                !points to initial pesticide input file
        character(len=16) :: path = ""                !points to initial pathogen input file
        character(len=16) :: hmet = ""                !points to initial heavy metals input file
        character(len=16) :: salt = ""                !points to initial salt input file
        character(len=16) :: cs = ""                  !points to initial constituent input file
      end type channel_init_datafiles_cs
      type (channel_init_datafiles_cs), dimension(:), allocatable :: ch_init_cs
      
      type channel_data
        character(len=16) :: name = "default"
        integer :: init = 0                   !initial data-points to initial.res
        integer :: hyd = 0                    !points to hydrology.res for hydrology inputs
        integer :: sed = 0                    !sediment inputs-points to sediment.res
        integer :: nut = 0                    !nutrient inputs-points to nutrient.res
      end type channel_data
      type (channel_data), dimension(:), allocatable :: ch_dat
            
      type channel_hyd_data
        !variables are conditional on res_dat()%hyd = 0 for reservoirs and 1 for hru impounding
        !surface areas are ha for 0 and frac of hru for 1; volumes are ha-m for 0 and mm for 1
        !br1 and br2 are used for 0 and acoef for 0 -- for surface area - volume relationship
        character(len=16) :: name = "default"
        real :: w = 2.           ! m             |average width of main channel
        real :: d = .5           ! m             |average depth of main channel
        real :: s = .01          ! m/m           |average slope of main channel
        real :: l = 0.1          ! km            |main channel length in subbasin
        real :: n = .05          ! none          |Manning"s "n" value for the main channel
        real :: k = 0.01         ! mm/hr         |effective hydraulic conductivity of main channel alluvium
        real :: wdr = 6.         ! m/m           |channel width to depth ratio
        real :: alpha_bnk = 0.03 ! days          |alpha factor for bank storage recession curve
        real :: side = 0.        !               |change in horizontal distance per unit
      end type channel_hyd_data
      type (channel_hyd_data), dimension(:), allocatable :: ch_hyd
      
      type channel_sed_data
        character(len=16) :: name
        integer :: eqn  = 0      !               |sediment routine methods: 
                                   !                   0 = original SWAT method
                                   !                   1 = Bagnold"seqn        
                                   !                   2 = Kodatie
                                   !                   3 = Molinas WU
                                   !                   4 = Yang
        real :: cov1 = 0.1       ! none          |channel erodibility factor (0.0-1.0)
        real :: cov2 = 0.1       ! none          |channel cover factor (0.0-1.0)
        real :: bnk_bd  = 0.     ! (g/cc)        |bulk density of channel bank sediment (1.1-1.9)
        real :: bed_bd  = 0.     ! (g/cc)        |bulk density of channel bed sediment (1.1-1.9)
        real :: bnk_kd  = 0.     !               |erodibility of channel bank sediment by jet test
        real :: bed_kd  = 0.     !               |erodibility of channel bed sediment by jet test
        real :: bnk_d50  = 0.    !               |D50(median) particle size diameter of channel 
        real :: bed_d50  = 0.    !               |D50(median) particle size diameter of channel
        real :: tc_bnk  = 0.     ! N/m2          |critical shear stress of channel bank
        real :: tc_bed  = 0.     ! N/m2          |critical shear stress of channel bed 
        real, dimension(12) :: erod  = 0.  !     |value of 0.0 indicates a non-erosive channel while a value
                                                     !of 1.0 indicates no resistance to erosion
      end type channel_sed_data
      type (channel_sed_data), dimension(:), allocatable :: ch_sed
            
      type channel_nut_data
        character(len=16) :: name
        real :: onco = 0.        ! ppm           |channel organic n concentration
        real :: opco = 0.        ! ppm           |channel organic p concentration
        real :: rs1 = 1.          ! m/day or m/hr   |local algal settling rate in reach at 20 deg C
        real :: rs2 = .05         ! (mg disP-P)/    |benthos source rate for dissolved phos ((m**2)*day)|in reach at 20 deg C
        !                                              or (mg disP-P)/((m**2)*hr)|
        real :: rs3 = .5          ! (mg NH4-N)/     |benthos source rate for ammonia nit in ((m**2)*day)|reach at 20 deg C
        !                                              or (mg NH4-N)/((m**2)*hr)|
        real :: rs4 = .05         ! 1/day or 1/hr   |rate coeff for organic nitrogen settling in reach at 20 deg C
        real :: rs5 = .05         ! 1/day or 1/hr   |org phos settling rate in reach at 20 deg C
        real :: rs6 = 2.5         ! 1/day           |rate coeff for settling of arbitrary non-conservative constituent in reach
        real :: rs7 = 2.5         ! (mg ANC)/       |benthal source rate for arbitrary ((m**2)*day)|non-conservative constituent in reach
        real :: rk1 = 1.71        ! 1/day or 1/hr   |CBOD deoxygenation rate coeff in reach at 20 deg C
        real :: rk2 = 1.          ! 1/day or 1/hr   |reaeration rate in accordance with Fickian diffusion in reach at 20 deg C
        real :: rk3 = 2.          ! 1/day or 1/hr   |rate of loss of CBOD due to settling in reach at 20 deg C
        real :: rk4 = 0.          ! mg O2/          |sed oxygen demand rate in reach ((m**2)*day)|at 20 deg C or mg O2/((m**2)*hr)
        real :: rk5 = 1.71        ! 1/day           |coliform die-off rate in reach
        real :: rk6 = 1.71        ! 1/day           |decay rate for arbitrary non-conservative constituent in reach
        real :: bc1 = .55         ! 1/hr            |rate constant for biological oxidation of NH3 to NO2 in reach at 20 deg C
        real :: bc2 = 1.1         ! 1/hr            |rate constant for biological oxidation of NO2 to NO3 in reach at 20 deg C
        real :: bc3 = .21         ! 1/hr            |rate constant for hydrolysis of organic N to ammonia in reach at 20 deg C
        real :: bc4 = .35         ! 1/hr            |rate constant for the decay of organic P to dissolved P in reach at 20 deg C
        real :: lao  = 2          ! NA              |Qual2E light averaging option. Qual2E defines four light averaging options. The only option
                                                    !currently available in SWAT is #2.
        integer :: igropt = 2     ! none            |Qual2E option for calculating the local specific growth rate of algae
                                                    ! 1: multiplicative: u = mumax * fll * fnn * fpp
                                                    ! 2: limiting nutrient: u = mumax * fll * Min(fnn, fpp)
                                                    ! 3: harmonic mean: u = mumax * fll * 2. / ((1/fnn)+(1/fpp))
        real :: ai0 = 50.         ! ug chla/mg alg  |ratio of chlorophyll-a to algal biomass
        real :: ai1 = 0.08        ! mg N/mg alg     |fraction of algal biomass that is nitrogen
        real :: ai2 = 0.015       ! mg P/mg alg     |fraction of algal biomass that is phosphorus
        real :: ai3 = 1.60        ! mg O2/mg alg    |the rate of oxygen production per unit of algal photosynthesis
        real :: ai4 = 2.0         ! mg O2/mg alg    |the rate of oxygen uptake per unit of algae respiration
        real :: ai5 = 3.5         ! mg O2/mg N      |the rate of oxygen uptake per unit of NH3 nitrogen oxidation
        real :: ai6 = 1.07        ! mg O2/mg N      |the rate of oxygen uptake per unit of NO2 nitrogen oxidation
        real :: mumax = 2.0       ! 1/hr            |maximum specific algal growth rate at 20 deg C
        real :: rhoq = 2.5        ! 1/day or 1/hr   |algal respiration rate
        real :: tfact = 0.3       ! none            |fraction of solar radiation computed in the temperature heat balance that is 
                                                    ! photosynthetically active
        real :: k_l = 0.75        ! MJ/(m2*hr)      |half-saturation coefficient for light
        real :: k_n = 0.02        ! mg N/L          |michaelis-menton half-saturation constant for nitrogen
        real :: k_p = 0.025       ! mg P/L          |michaelis-menton half saturation constant for phosphorus
        real :: lambda0 = 1.0     ! 1/m             |non-algal portion of the light extinction coefficient
        real :: lambda1 = 0.03    ! 1/(m*ug chla/L) |linear algal self-shading coefficient
        real :: lambda2 = 0.054   ! (1/m)(ug chla/L)**(-2/3) |nonlinear algal self-shading coefficient
        real :: p_n = 0.5         ! none            |algal preference factor for ammonia
      end type channel_nut_data
      type (channel_nut_data), dimension(:), allocatable :: ch_nut

      type channel_temperature_data
        character(len=16) :: name
        real :: sno_mlt = 1.        ! none          |coefficient influencing snowmelt temperature contributions
        real :: gw = .97            ! none          |coefficient influencing groundwater temperature contributions
        real :: sur_lat = 1.        ! none          |coefficient influencing suface and lateral flow temperature contributions
        real :: bulk_co = .0025     ! 1/hour        |bulk coefficient of heat transfer
        real :: air_lag = 6.        ! days          |average air temperature lag
      end type channel_temperature_data
      type (channel_temperature_data), dimension(:), allocatable :: ch_temp
       
      end module channel_data_module 