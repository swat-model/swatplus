      module carbon_module 
      
      implicit none

      !! per-family output gating for the standard carbon files lives in print.prt (hru_cb_* rows).
      !! cbn_diagnostics only drives the legacy CSU output path (hru_cb / hru_cb_vars rows). it is
      !! set in carbon_legacy_open from the cb_hru flag at startup, so no input file is needed for
      !! it. the .false. here is only the pre-run default; it is assigned its real value before any
      !! writer runs, so it does not disable legacy diagnostics. will be removed in revision 63.
      logical :: cbn_diagnostics = .false.   !! turns on the legacy plc/cflux/cpool and soil-prop files

      !! basin-wide residue decomposition tunables (read from carbon.bsn)
      real :: n_act_frac = 0.02    !! frac    |fraction of organic N in the active humus pool (used in nut_nminrl active to stable flow)
      real :: cnr_cap    = 500.    !! none    |upper cap on residue C:N ratio before computing decomp factor
      real :: cnr_ref    = 25.     !! none    |reference C:N ratio where decomp factor equals 1
      real :: cpr_cap    = 5000.   !! none    |upper cap on residue C:P ratio before computing decomp factor
      real :: cpr_ref    = 200.    !! none    |reference C:P ratio where decomp factor equals 1

      !! number of soil layers to include in per-layer carbon outputs.
      !! if carbon_layers.prt is supplied it sets this explicitly; otherwise it
      !! defaults to the largest soil layer count across all HRUs (set in
      !! output_landscape_init). the 7 here is only a fallback.
      integer :: cb_n_layers = 7
      logical :: cb_n_layers_explicit = .false.   !! .true. when carbon_layers.prt set the count
      real, parameter :: cb_lyr_missing = -99.0   !! sentinel written when a soil has fewer layers than cb_n_layers

      !! variable-name lists for the wide-per-layer carbon output files.
      !! the helper cb_write_wide_header appends _lyr1..._lyrN to each entry.
      character(len=16), parameter :: cpool_vars(10) = [character(len=16) :: &
        "residue_c", "structural_c", "metabolic_c", "hs_c", "hp_c", &
        "microbial_c", "lignin_c", "nonlignin_c", "root_mass", "soil_water"]

      character(len=16), parameter :: n_p_pool_vars(18) = [character(len=16) :: &
        "tot_pool_n", "residue_n", "structural_n", "metabolic_n", "hs_n", "hp_n", &
        "microbial_n", "lignin_n", "nonlignin_n", &
        "tot_pool_p", "residue_p", "structural_p", "metabolic_p", "hs_p", "hp_p", &
        "microbial_p", "lignin_p", "nonlignin_p"]

      character(len=16), parameter :: cflux_vars(37) = [character(len=16) :: &
        "cfmets1", "cfstrs1", "cfstrs2", "efmets1", "efstrs1", "efstrs2", &
        "immmets1", "immstrs1", "immstrs2", "mnrmets1", "mnrstrs1", "mnrstrs2", &
        "co2fmet", "co2fstr", &
        "cfs1s2", "cfs1s3", "cfs2s1", "cfs2s3", "cfs3s1", &
        "efs1s2", "efs1s3", "efs2s1", "efs2s3", "efs3s1", &
        "imms1s2", "imms1s3", "imms2s1", "imms2s3", "imms3s1", &
        "mnrs1s2", "mnrs1s3", "mnrs2s1", "mnrs2s3", "mnrs3s1", &
        "co2fs1", "co2fs2", "co2fs3"]

      character(len=16), parameter :: carb_drv_vars(14) = [character(len=16) :: &
        "sut", "tillagef", "cons_bmix", "tillagef_biomix", "tillagef_tillmix", &
        "till_eff", "cdg", "ox", "cs", "no3", "nh4", "co2_resp", "soil_temp", "emix"]

      character(len=16), parameter :: carb_dyn_vars(21) = [character(len=16) :: &
        "asp", "abp", "abco2", "a1co2", "asco2", "apco2", &
        "ncbm", "nchp", "nchs", &
        "bmctp", "bmntp", "hsctp", "hsntp", "hpctp", "hpntp", &
        "lmctp", "lmntp", "lsctp", "lslctp", "lslnctp", "lsntp"]

      character(len=16), parameter :: soil_snap_vars(13) = [character(len=16) :: &
        "bd", "awc", "soil_k", "tot_c", "clay", "silt", "sand", "rock", &
        "alb", "usle_k", "ec", "caco3", "ph"]
      
      type carbon_inputs
          real :: hp_rate = 0.          !               |rate of transformation of passive humus under optimal conditions
          real :: hs_rate = 0.          !               |rate of transformation of slow humus under optimal conditions
          real :: microb_rate = 0.      !               |rate of transformation of microbial biomass and associated products under optimal conditions
          real :: meta_rate = 0.        !               |rate of transformation of metabolic litter under optimal conditions
          real :: str_rate = 0.         !               |rate of potential transformation of structural litter under optimal conditions
          real :: microb_top_rate = 0.  !               |coef adjusts mocribial activity function in top soil layer
          real :: hs_hp = 0.            !               |coef in century eq allocating slow to passive humus
          real :: microb_koc = 0.       !10^3 m^3 Mg-1  |liquid-solid partition coefficient for microbial biomass
          real :: min_n_frac = 0.       !               |fraction of mineral n sorbed to litter
          real :: c_org_frac = 0.       !               |carbon fraction of organic materials      
      end type carbon_inputs
      type (carbon_inputs), dimension(2) :: carbdb
      type (carbon_inputs) :: carbz

      type manure_coef
          real :: rtof = 0.5            !none          |weighting factor used to partition the 
                                        !              |organic N & P concentration of septic effluent
                                        !              |between the fresh organic and the stable organic pools
          real :: man_to_c = 0.42       !              |conversion of manure solids to carbon
      end type manure_coef
      type (manure_coef) :: man_coef
      
      type organic_allocations
          ! real :: abl = 0.        !               |Fraction of microbial biomass loss due to leaching
          real :: abp = 0.        !               |Fraction of decomposed microbial biomass allocated to passive humus
          real :: asp = 0.        !               |Fraction of decomposed slow humus allocated to passive
          ! real :: almco2 = 0.     !               |Fraction of decomposed metabolic litter allocated to CO2
          ! real :: alslco2 = 0.    !               |Fraction of decomposed lignin of structural litter allocated to CO2
          ! real :: alslnco2 = 0.   !               |Fraction of decomposed lignin of structural litter allocated to CO2
          real :: a1co2 =  0.     !               |Fraction of decomposed metabolic and passive pools to CO2
          real :: asco2 = 0.      !               |Fraction of decomposed slow humus allocated to CO2
          real :: apco2 = 0.      !               |Fraction of decomposed  passive humus allocated to CO2
          real :: abco2 = 0.      !               |Fraction of decomposed microbial biomass allocated to CO2
      end type organic_allocations
      type (organic_allocations), dimension(2) :: org_allo 
      type (organic_allocations) :: org_alloz
        
      type organic_controls
          real :: sut = 0.           !                 |soil water control on biological processes
          real :: cdg = 0.           !                 |soil temperature control on biological processes
          real :: cs = 0.            !                 |combined factor controlling biological processes
          real :: ox = 0.            !                 |oxygen control on biological processes 
          real :: till_eff           !                 |tillage effect
          real :: x1 = 0.            !                 |tillage control on residue decomposition
          real :: no3 = 0.           !                 |no3 as adjusted in cbn_zhang2
          real :: nh4 = 0.           !                 |nh4 as adjusted in cbn_zhang2
          real :: resp               !                 |co2 respiration
          ! real :: xbmt = 0.          !               |control on transformation of microbial biomass by soil texture and structure
          ! real :: xlslf = 0.         !               |control on potential transformation of structural litter by lignin fraction
          ! The following three parameters resolve the shape of the temperature effect equation:  
          real :: tn = -5.           ! celsius         |minimum temperature bound
          real :: top = 30.          ! celsius         |peak (optimum) temperature 
          real :: tx = 50.           ! celsius         |maximum temperature bound
          integer :: tmpf = 2        !                 |temperature factor approach used in cbn_zhang2 
          integer :: watf = 1        !                 |water factor approach used in cbn_zhang2 
      end type organic_controls
      type (organic_controls) :: org_con
        
      type organic_fractions
          real :: lmf = 0.      !frac               |fraction of the litter that is metabolic
          real :: lmnf = 0.     !kg kg-1            |fraction of metabolic litter that is N
          real :: lsf = 0.      !frac               |fraction of the litter that is structural
          real :: lslf = 0.     !kg kg-1            |fraction of structural litter that is lignin 
          real :: lsnf = 0.     !kg kg-1            |fraction of structural litter that is N      
          real :: frac_seq = .95             !      |fraction of total carbon the is sequestered carbon when initializing sequestered pools
          real :: frac_not_seq = .05         !      |fraction of total carbon the is NOT sequestered carbon when initializing non-sequestered pools
          real :: frac_hum_microb = 0.02     !      !fraction of carbon that is microbrial pool when initializing microbrial pools
          real :: frac_hum_slow = 0.54       !      !fraction of carbon that is humas slow pool  when initializing humus slow pools
          real :: frac_hum_passive = 0.44    !      |fraction of carbon that is humas passive pool when initializing humas passive pools
          logical :: mathers_method = .false. !     !logical indicating whether to use the mathers_method to initialize humus slow pools
      end type organic_fractions
      type (organic_fractions) :: org_frac                    
      
      type organic_ratio
          ! real :: cnr = 0.         !                  |c/n ratio of standing dead
          real :: ncbm = 0.        !                  |n/c ratio of biomass           
          real :: nchp = 0.        !                  |n/c ratio of passive humus
          real :: nchs = 0.        !                  |n/c ration of slow humus
      end type organic_ratio
      type (organic_ratio) :: org_ratio                   
      type (organic_ratio) :: org_ratio_zero                   
      
      type carbon_water_coef
          real :: prmt_21 = 1000.   !   |KOC FOR CARBON LOSS IN WATER AND SEDIMENT(500._1500.) KD = KOC * C          
          real :: prmt_44 = 0.5     !   |RATIO OF SOLUBLE C CONCENTRATION IN RUNOFF TO PERCOLATE(0.1_1.)
      end type carbon_water_coef
      type (carbon_water_coef) :: cb_wtr_coef                  

      type organic_transformations
          real :: bmctp = 0.       !kg ha-1 day-1        |potential transformation of C in microbial biomass
          real :: bmntp = 0.       !kg ha-1 day-1        |potential transformation of N in microbial biomass
          real :: hsctp = 0.       !kg ha-1 day-1        |potential transformation of C in slow humus
          real :: hsntp = 0.       !kg ha-1 day-1        |potential transformation of N in slow humus
          real :: hpctp = 0.       !kg ha-1 day-1        |potential transformation of C in passive humus 
          real :: hpntp = 0.       !kg ha-1 day-1        |potential transformation of N in passive humus
          real :: lmctp = 0.       !kg ha-1 day-1        |potential transformation of C in metabolic litter
          real :: lmntp = 0.       !kg ha-1 day-1        |potential transformation of N in metabolic litter 
          real :: lsctp = 0.       !kg ha-1 day-1        |potential transformation of C in structural litter
          real :: lslctp = 0.      !kg ha-1 day-1        |potential transformation of C in lignin of structural litter
          real :: lslnctp = 0.     !kg ha-1 day-1        |potential transformation of C in nonlignin structural litter
          real :: lsntp = 0.       !kg ha-1 day-1        |potential transformation of N in structural litter              
      end type organic_transformations
      type (organic_transformations) :: org_tran
      type (organic_transformations) :: org_tran_zero
      
      type organic_flux
          real :: cfmets1 = 0.           !(kg C ha-1 day-1) |C transformed from Metabolic Litter to S1 (Microbial Biomass) 
          real :: cfstrs1 = 0.           !(kg C ha-1 day-1) |C transformed from Structural Litter to S1 (Microbial Biomass)  
          real :: cfstrs2 = 0.           !(kg C ha-1 day-1) |C transformed from Structural Litter to S2 (Slow Humus) 
          real :: efmets1 = 0.           !(kg N ha-1 day-1) |N transformed from Metabolic Litter to S1 (Microbial Biomass) 
          real :: efstrs1 = 0.           !(kg N ha-1 day-1) |N transformed from Structural Litter to S1 (Microbial Biomass) 
          real :: efstrs2 = 0.           !(kg N ha-1 day-1) |N transformed from Structural Litter to S2 (Slow Humus)  
          real :: immmets1 = 0.          !(kg N ha-1 day-1) |N immibolization resulting from transforming Metabolic Litter to S1 (Microbial Biomass)   
          real :: immstrs1 = 0.          !(kg N ha-1 day-1) |N immibolization resulting from transforming Structural Litter to S1 (Microbial Biomass) 
          real :: immstrs2 = 0.          !(kg N ha-1 day-1) |N immibolization resulting from transforming Structural Litter to S2 (Slow Humus)  
          real :: mnrmets1 = 0.          !(kg N ha-1 day-1) |N mineralization resulting from transforming Metabolic Litter to S1 (Microbial Biomass)   
          real :: mnrstrs1 = 0.          !(kg N ha-1 day-1) |N mineralization resulting from transforming Structural Litter to S1 (Microbial Biomass) 
          real :: mnrstrs2 = 0.          !(kg N ha-1 day-1) |N mineralization resulting from transforming Structural Litter to S2 (Slow Humus)  
          real :: co2fmet = 0.           !(kg C ha-1 day-1) |CO2 production resulting from metabolic litter transformaitons               
          real :: co2fstr = 0.           !(kg C ha-1 day-1) |CO2 production resulting from lignin structural litter transformaitons  
          real :: cfs1s2 = 0.            !(kg C ha-1 day-1) |C transformed from S1 (Microbial Biomass) to S2 (Slow Humus)    
          real :: cfs1s3 = 0.            !(kg C ha-1 day-1) |C transformed from S1 (Microbial Biomass) to S3 (Passive Humus)  
          real :: cfs2s1 = 0.            !(kg C ha-1 day-1) |C transformed from S2 (Slow Humus) to S1 (Microbial Biomass)  
          real :: cfs2s3 = 0.            !(kg C ha-1 day-1) |C transformed from S2 (Slow Humus) to S3 (Passive Humus)  
          real :: cfs3s1 = 0.            !(kg C ha-1 day-1) |C transformed from  S3 (Passive Humus) to S1 (Microbial Biomass) 
          real :: efs1s2 = 0.            !(kg N ha-1 day-1) |N transformed from from S1 (Microbial Biomass) to S2 (Slow Humus)  
          real :: efs1s3 = 0.            !(kg N ha-1 day-1) |N transformed from from S1 (Microbial Biomass) to S3 (Passive Humus) 
          real :: efs2s1 = 0.            !(kg N ha-1 day-1) |N transformed from from S2 (Slow Humus) to S1 (Microbial Biomass) 
          real :: efs2s3 = 0.            !(kg N ha-1 day-1) |N transformed from from S2 (Slow Humus) to S3 (Passive Humus) 
          real :: efs3s1 = 0.            !(kg N ha-1 day-1) |N transformed from from  S3 (Passive Humus) to S1 (Microbial Biomass) 
          real :: imms1s2 = 0.           !(kg N ha-1 day-1) |N immibolization resulting from transforming S1 (Microbial Biomass) to S2 (Slow Humus)   
          real :: imms1s3 = 0.           !(kg N ha-1 day-1) |N immibolization resulting from transforming S1 (Microbial Biomass) to S3 (Passive Humus)  
          real :: imms2s1 = 0.           !(kg N ha-1 day-1) |N immibolization resulting from transforming S2 (Slow Humus) to S1 (Microbial Biomass) 
          real :: imms2s3 = 0.           !(kg N ha-1 day-1) |N immibolization resulting from transforming S2 (Slow Humus) to S3 (Passive Humus)  
          real :: imms3s1 = 0.           !(kg N ha-1 day-1) |N immibolization resulting from transforming  S3 (Passive Humus) to S1 (Microbial Biomass)  
          real :: mnrs1s2 = 0.           !(kg N ha-1 day-1) |N mineralization resulting from transforming S1 (Microbial Biomass) to S2 (Slow Humus)  
          real :: mnrs1s3 = 0.           !(kg N ha-1 day-1) |N mineralization resulting from transforming S1 (Microbial Biomass) to S3 (Passive Humus) 
          real :: mnrs2s1 = 0.           !(kg N ha-1 day-1) |N mineralization resulting from transforming S2 (Slow Humus) to S1 (Microbial Biomass)  
          real :: mnrs2s3 = 0.           !(kg N ha-1 day-1) |N mineralization resulting from transforming S2 (Slow Humus) to S3 (Passive Humus)  
          real :: mnrs3s1 = 0.           !(kg N ha-1 day-1) |N mineralization resulting from transforming  S3 (Passive Humus) to S1 (Microbial Biomass)  
          real :: co2fs1 = 0.            !(kg C ha-1 day-1) |CO2 production resulting from S1 (Microbial Biomass) transformations  
          real :: co2fs2 = 0.            !(kg C ha-1 day-1) |CO2 production resulting from S2 (Slow Humus)  transformations  
          real :: co2fs3 = 0.            !(kg C ha-1 day-1) |CO2 production resulting from S3 (Passive Humus) transformations  
      end type organic_flux
      type (organic_flux) :: org_flux
      type (organic_flux) :: org_flux_zero
      
      type carbon_soil_transformations
          real :: meta_micr = 0.        !(kg C ha-1 day-1) |C transformed from Metabolic Litter to S1 (Microbial Biomass) 
          real :: str_micr = 0.         !(kg C ha-1 day-1) |C transformed from Structural Litter to S1 (Microbial Biomass)  
          real :: str_hs = 0.           !(kg C ha-1 day-1) |C transformed from Structural Litter to S2 (Slow Humus)
          real :: co2_meta = 0.         !(kg C ha-1 day-1) |CO2 production resulting from metabolic litter transformations               
          real :: co2_str = 0.          !(kg C ha-1 day-1) |CO2 production resulting from lignin structural litter transformations  
          real :: micr_hs = 0.          !(kg C ha-1 day-1) |C transformed from S1 (Microbial Biomass) to S2 (Slow Humus)    
          real :: micr_hp = 0.          !(kg C ha-1 day-1) |C transformed from S1 (Microbial Biomass) to S3 (Passive Humus)  
          real :: hs_micr = 0.          !(kg C ha-1 day-1) |C transformed from S2 (Slow Humus) to S1 (Microbial Biomass)  
          real :: hs_hp = 0.            !(kg C ha-1 day-1) |C transformed from S2 (Slow Humus) to S3 (Passive Humus)  
          real :: hp_micr = 0.          !(kg C ha-1 day-1) |C transformed from  S3 (Passive Humus) to S1 (Microbial Biomass) 
          real :: co2_micr = 0.         !(kg C ha-1 day-1) |CO2 production resulting from S1 (Microbial Biomass) transformations  
          real :: co2_hs = 0.           !(kg C ha-1 day-1) |CO2 production resulting from S2 (Slow Humus)  transformations  
          real :: co2_hp = 0.           !(kg C ha-1 day-1) |CO2 production resulting from S3 (Passive Humus) transformations  
      end type carbon_soil_transformations
      type (carbon_soil_transformations) :: hscfz
      
      !! hru soil carbon transformations
      type (carbon_soil_transformations), dimension (:), allocatable :: hscf_d
      type (carbon_soil_transformations), dimension (:), allocatable :: hscf_m
      type (carbon_soil_transformations), dimension (:), allocatable :: hscf_y
      type (carbon_soil_transformations), dimension (:), allocatable :: hscf_a
      !! lsu soil carbon transformations
      type (carbon_soil_transformations), dimension (:), allocatable :: lscf_d
      type (carbon_soil_transformations), dimension (:), allocatable :: lscf_m
      type (carbon_soil_transformations), dimension (:), allocatable :: lscf_y
      type (carbon_soil_transformations), dimension (:), allocatable :: lscf_a
      type (carbon_soil_transformations), dimension (:), allocatable :: lcsf_a
      !! basin soil carbon transformations
      type (carbon_soil_transformations) :: bscf_d
      type (carbon_soil_transformations) :: bscf_m
      type (carbon_soil_transformations) :: bscf_y
      type (carbon_soil_transformations) :: bscf_a
      
      type carbon_soil_gain_losses
        real :: sed_c = 0.              !kg C/ha            |C transported with sediment yield
        real :: surq_c = 0.             !kg C/ha            |total dissolved C transported with surface runoff
        real :: latq_c = 0.             !kg C/ha            |dissolved organic C transported with lateral flow (all layers)
        real :: perc_c = 0.             !kg C/ha            |total dissolved C transported with percolate
        real :: rsd_decay_c = 0.        !kg C/ha            |carbon added to soil from residue decay
        real :: man_app_c = 0.          !kg C/ha            |amount of carbon applied to soil from manure
        real :: man_graz_c = 0.         !kg C/ha            |amount of carbon manure from grazing animals
        real :: rsp_c = 0.              !kg C/ha            |CO2 production from soil respiration summarized for the profile
        real :: emit_c = 0.             !kg C/ha            |CO2 production from burning soil carbon
      end type carbon_soil_gain_losses
      type (carbon_soil_gain_losses) :: hscz 
      
      !! hru soil carbon gains and losses
      type (carbon_soil_gain_losses), dimension (:), allocatable :: hsc_d
      type (carbon_soil_gain_losses), dimension (:), allocatable :: hsc_m
      type (carbon_soil_gain_losses), dimension (:), allocatable :: hsc_y
      type (carbon_soil_gain_losses), dimension (:), allocatable :: hsc_a
      !! lsu soil carbon gains and losses
      type (carbon_soil_gain_losses), dimension (:), allocatable :: lsc_d
      type (carbon_soil_gain_losses), dimension (:), allocatable :: lsc_m
      type (carbon_soil_gain_losses), dimension (:), allocatable :: lsc_y
      type (carbon_soil_gain_losses), dimension (:), allocatable :: lsc_a
      !! basin soil carbon gains and losses
      type (carbon_soil_gain_losses) :: bsc_d
      type (carbon_soil_gain_losses) :: bsc_m
      type (carbon_soil_gain_losses) :: bsc_y
      type (carbon_soil_gain_losses) :: bsc_a
      
      type carbon_residue_gain_losses
        real :: plant_surf_c = 0.       !kg C/ha            |carbon added to surface residue from leaf drop and kill
        real :: plant_root_c = 0.       !kg C/ha            |carbon added to soil residue from root kill
        real :: rsd_surfdecay_c = 0.    !kg C/ha            |carbon lost to soil from surface residue decay
        real :: rsd_rootdecay_c = 0.    !kg C/ha            |carbon lost to soil from soil/root and incorporated residue decay
        real :: harv_stov_c = 0.        !kg C/ha            |carbon removed during surface residue harvest  
        real :: emit_c = 0.             !kg C/ha            |CO2 production from burning surface residue carbon
      end type carbon_residue_gain_losses
      type (carbon_residue_gain_losses) :: hrcz
      
      !! hru residue carbon gains and losses
      type (carbon_residue_gain_losses), dimension (:), allocatable :: hrc_d
      type (carbon_residue_gain_losses), dimension (:), allocatable :: hrc_m
      type (carbon_residue_gain_losses), dimension (:), allocatable :: hrc_y
      type (carbon_residue_gain_losses), dimension (:), allocatable :: hrc_a
      !! lsu residue carbon gains and losses
      type (carbon_residue_gain_losses), dimension (:), allocatable :: lrc_d
      type (carbon_residue_gain_losses), dimension (:), allocatable :: lrc_m
      type (carbon_residue_gain_losses), dimension (:), allocatable :: lrc_y
      type (carbon_residue_gain_losses), dimension (:), allocatable :: lrc_a
      !! basin residue carbon gains and losses
      type (carbon_residue_gain_losses) :: brc_d
      type (carbon_residue_gain_losses) :: brc_m
      type (carbon_residue_gain_losses) :: brc_y
      type (carbon_residue_gain_losses) :: brc_a
      
      type carbon_plant_gain_losses
        real :: npp_c = 0.              !kg C/ha            |plant carbon growth from photosynthesis
        real :: harv_abgr_c = 0.        !kg C/ha            |carbon removed during grain/biomass harvest
        real :: harv_root_c = 0.        !kg C/ha            |carbon removed during tuber (root) harvest
        real :: drop_c = 0.             !kg C/ha            |carbon added to residue from leaf drop and kill
        real :: grazeat_c = 0.          !kg C/ha            |amount of carbon ate by animals in grazing
        real :: emit_c = 0.             !kg C/ha            |CO2 production from burning residue carbon
      end type carbon_plant_gain_losses
      type (carbon_plant_gain_losses) :: hpcz
      
      !! hru plant carbon gains and losses
      type (carbon_plant_gain_losses), dimension (:), allocatable :: hpc_d
      type (carbon_plant_gain_losses), dimension (:), allocatable :: hpc_m
      type (carbon_plant_gain_losses), dimension (:), allocatable :: hpc_y
      type (carbon_plant_gain_losses), dimension (:), allocatable :: hpc_a
      !! lsu plant carbon gains and losses
      type (carbon_plant_gain_losses), dimension (:), allocatable :: lpc_d
      type (carbon_plant_gain_losses), dimension (:), allocatable :: lpc_m
      type (carbon_plant_gain_losses), dimension (:), allocatable :: lpc_y
      type (carbon_plant_gain_losses), dimension (:), allocatable :: lpc_a
      !! basin plant carbon gains and losses
      type (carbon_plant_gain_losses) :: bpc_d
      type (carbon_plant_gain_losses) :: bpc_m
      type (carbon_plant_gain_losses) :: bpc_y
      type (carbon_plant_gain_losses) :: bpc_a

      interface operator (+)
        module procedure carbon_soil_flux__add
      end interface
      
      interface operator (*)
        module procedure carbon_soil_flux_mult
      end interface
        
      interface operator (/)
        module procedure carbon_soil_flux_div
      end interface
        
      interface operator (+)
        module procedure carbon_soil_gl__add
      end interface
      
      interface operator (*)
        module procedure carbon_soil_gl_mult
      end interface
        
      interface operator (/)
        module procedure carbon_soil_gl_div
      end interface
        
      interface operator (+)
        module procedure carbon_residue_gl__add
      end interface
      
      interface operator (*)
        module procedure carbon_residue_gl_mult
      end interface
        
      interface operator (/)
        module procedure carbon_residue_gl_div
      end interface
        
      interface operator (+)
        module procedure carbon_plant_gl__add
      end interface
      
      interface operator (*)
        module procedure carbon_plant_gl_mult
      end interface 
        
      interface operator (/)
        module procedure carbon_plant_gl_div
      end interface 
        
      contains

      function carbon_soil_flux__add (hru1, hru2) result (hru3)
        type (carbon_soil_transformations), intent (in) :: hru1
        type (carbon_soil_transformations), intent (in) :: hru2
        type (carbon_soil_transformations) :: hru3
        hru3%meta_micr = hru1%meta_micr + hru2%meta_micr
        hru3%str_micr = hru1%str_micr + hru2%str_micr
        hru3%str_hs = hru1%str_hs + hru2%str_hs
        hru3%co2_meta = hru1%co2_meta + hru2%co2_meta
        hru3%co2_str = hru1%co2_str + hru2%co2_str
        hru3%micr_hs = hru1%micr_hs + hru2%micr_hs
        hru3%micr_hp = hru1%micr_hp + hru2%micr_hp
        hru3%hs_micr = hru1%hs_micr + hru2%hs_micr
        hru3%hs_hp = hru1%hs_hp + hru2%hs_hp
        hru3%hp_micr = hru1%hp_micr + hru2%hp_micr
        hru3%co2_micr = hru1%co2_micr + hru2%co2_micr
        hru3%co2_hs = hru1%co2_hs + hru2%co2_hs
        hru3%co2_hp = hru1%co2_hp + hru2%co2_hp
       end function carbon_soil_flux__add
       
      function carbon_soil_flux_mult (hru1,const) result (hru2)
        type (carbon_soil_transformations), intent (in) :: hru1
        real, intent (in) :: const
        type (carbon_soil_transformations) :: hru2
        hru2%meta_micr = hru1%meta_micr * const
        hru2%str_micr = hru1%str_micr * const
        hru2%str_hs = hru1%str_hs * const
        hru2%co2_meta = hru1%co2_meta * const
        hru2%co2_str = hru1%co2_str * const
        hru2%micr_hs = hru1%micr_hs * const
        hru2%micr_hp = hru1%micr_hp * const
        hru2%hs_micr = hru1%hs_micr * const
        hru2%hs_hp = hru1%hs_hp * const
        hru2%hp_micr = hru1%hp_micr * const
        hru2%co2_micr = hru1%co2_micr * const
        hru2%co2_hs = hru1%co2_hs * const
        hru2%co2_hp = hru1%co2_hp * const
      end function carbon_soil_flux_mult
      
      function carbon_soil_flux_div (hru1,const) result (hru2)
        type (carbon_soil_transformations), intent (in) :: hru1
        real, intent (in) :: const
        type (carbon_soil_transformations) :: hru2
        hru2%meta_micr = hru1%meta_micr / const
        hru2%str_micr = hru1%str_micr / const
        hru2%str_hs = hru1%str_hs / const
        hru2%co2_meta = hru1%co2_meta / const
        hru2%co2_str = hru1%co2_str / const
        hru2%micr_hs = hru1%micr_hs / const
        hru2%micr_hp = hru1%micr_hp / const
        hru2%hs_micr = hru1%hs_micr / const
        hru2%hs_hp = hru1%hs_hp / const
        hru2%hp_micr = hru1%hp_micr / const
        hru2%co2_micr = hru1%co2_micr / const
        hru2%co2_hs = hru1%co2_hs / const
        hru2%co2_hp = hru1%co2_hp / const
      end function carbon_soil_flux_div
      
      function carbon_soil_gl__add (hru1, hru2) result (hru3)
        type (carbon_soil_gain_losses), intent (in) :: hru1
        type (carbon_soil_gain_losses), intent (in) :: hru2
        type (carbon_soil_gain_losses) :: hru3
        hru3%sed_c = hru1%sed_c + hru2%sed_c
        hru3%surq_c = hru1%surq_c + hru2%surq_c
        hru3%latq_c = hru1%latq_c + hru2%latq_c
        hru3%perc_c = hru1%perc_c + hru2%perc_c
        hru3%rsd_decay_c = hru1%rsd_decay_c + hru2%rsd_decay_c
        hru3%man_app_c = hru1%man_app_c + hru2%man_app_c
        hru3%man_graz_c = hru1%man_graz_c + hru2%man_graz_c
        hru3%rsp_c = hru1%rsp_c + hru2%rsp_c
        hru3%emit_c = hru1%emit_c + hru2%emit_c
       end function carbon_soil_gl__add
       
      function carbon_soil_gl_mult (hru1,const) result (hru2)
        type (carbon_soil_gain_losses), intent (in) :: hru1
        real, intent (in) :: const
        type (carbon_soil_gain_losses) :: hru2
        hru2%sed_c = hru1%sed_c * const
        hru2%surq_c = hru1%surq_c * const
        hru2%latq_c = hru1%latq_c * const
        hru2%perc_c = hru1%perc_c * const
        hru2%rsd_decay_c = hru1%rsd_decay_c * const
        hru2%man_app_c = hru1%man_app_c * const
        hru2%man_graz_c = hru1%man_graz_c * const
        hru2%rsp_c = hru1%rsp_c * const
        hru2%emit_c = hru1%emit_c * const
      end function carbon_soil_gl_mult
      
      function carbon_soil_gl_div (hru1,const) result (hru2)
        type (carbon_soil_gain_losses), intent (in) :: hru1
        real, intent (in) :: const
        type (carbon_soil_gain_losses) :: hru2
        hru2%sed_c = hru1%sed_c / const
        hru2%surq_c = hru1%surq_c / const
        hru2%latq_c = hru1%latq_c / const
        hru2%perc_c = hru1%perc_c / const
        hru2%rsd_decay_c = hru1%rsd_decay_c / const
        hru2%man_app_c = hru1%man_app_c / const
        hru2%man_graz_c = hru1%man_graz_c / const
        hru2%rsp_c = hru1%rsp_c / const
        hru2%emit_c = hru1%emit_c / const
      end function carbon_soil_gl_div
      
      function carbon_residue_gl__add (hru1, hru2) result (hru3)
        type (carbon_residue_gain_losses), intent (in) :: hru1
        type (carbon_residue_gain_losses), intent (in) :: hru2
        type (carbon_residue_gain_losses) :: hru3
        hru3%plant_surf_c = hru1%plant_surf_c + hru2%plant_surf_c
        hru3%plant_root_c = hru1%plant_root_c + hru2%plant_root_c
        hru3%rsd_surfdecay_c = hru1%rsd_surfdecay_c + hru2%rsd_surfdecay_c
        hru3%rsd_rootdecay_c = hru1%rsd_rootdecay_c + hru2%rsd_rootdecay_c
        hru3%harv_stov_c = hru1%harv_stov_c + hru2%harv_stov_c
        hru3%emit_c = hru1%emit_c + hru2%emit_c
       end function carbon_residue_gl__add
       
      function carbon_residue_gl_mult (hru1,const) result (hru2)
        type (carbon_residue_gain_losses), intent (in) :: hru1
        real, intent (in) :: const
        type (carbon_residue_gain_losses) :: hru2
        hru2%plant_surf_c = hru1%plant_surf_c * const
        hru2%plant_root_c = hru1%plant_root_c * const
        hru2%rsd_surfdecay_c = hru1%rsd_surfdecay_c * const
        hru2%rsd_rootdecay_c = hru1%rsd_rootdecay_c * const
        hru2%harv_stov_c = hru1%harv_stov_c * const
        hru2%emit_c = hru1%emit_c * const
      end function carbon_residue_gl_mult
      
      function carbon_residue_gl_div (hru1,const) result (hru2)
        real, intent (in) :: const
        type (carbon_residue_gain_losses), intent (in) :: hru1
        type (carbon_residue_gain_losses) :: hru2
        hru2%plant_surf_c = hru1%plant_surf_c / const
        hru2%plant_surf_c = hru1%plant_surf_c / const
        hru2%rsd_surfdecay_c = hru1%rsd_surfdecay_c / const
        hru2%rsd_rootdecay_c = hru1%rsd_rootdecay_c / const
        hru2%harv_stov_c = hru1%harv_stov_c / const
        hru2%emit_c = hru1%emit_c * const
      end function carbon_residue_gl_div
      
      function carbon_plant_gl__add (hru1, hru2) result (hru3)
        type (carbon_plant_gain_losses), intent (in) :: hru1
        type (carbon_plant_gain_losses), intent (in) :: hru2
        type (carbon_plant_gain_losses) :: hru3
        hru3%npp_c = hru1%npp_c + hru2%npp_c
        hru3%harv_abgr_c = hru1%harv_abgr_c + hru2%harv_abgr_c
        hru3%harv_root_c = hru1%harv_root_c + hru2%harv_root_c
        hru3%drop_c = hru1%drop_c + hru2%drop_c
        hru3%grazeat_c = hru1%grazeat_c + hru2%grazeat_c
        hru3%emit_c = hru1%emit_c + hru2%emit_c
       end function carbon_plant_gl__add
       
      function carbon_plant_gl_mult (hru1,const) result (hru2)
        type (carbon_plant_gain_losses), intent (in) :: hru1
        real, intent (in) :: const
        type (carbon_plant_gain_losses) :: hru2
        hru2%npp_c = hru1%npp_c * const
        hru2%harv_abgr_c = hru1%harv_abgr_c * const
        hru2%harv_root_c = hru1%harv_root_c * const
        hru2%drop_c = hru1%drop_c * const
        hru2%grazeat_c = hru1%grazeat_c * const
        hru2%emit_c = hru1%emit_c * const
      end function carbon_plant_gl_mult
      
      function carbon_plant_gl_div (hru1,const) result (hru2)
        type (carbon_plant_gain_losses), intent (in) :: hru1
        real, intent (in) :: const
        type (carbon_plant_gain_losses) :: hru2
        hru2%npp_c = hru1%npp_c / const
        hru2%harv_abgr_c = hru1%harv_abgr_c / const
        hru2%harv_root_c = hru1%harv_root_c / const
        hru2%drop_c = hru1%drop_c / const
        hru2%grazeat_c = hru1%grazeat_c / const
        hru2%emit_c = hru1%emit_c / const
      end function carbon_plant_gl_div

      !! helpers for wide-per-layer carbon outputs.

      subroutine cb_write_flat_header(unit_no, var_names, is_csv)
        !! emits a header line for non-layered carbon files (no soil layers).
        !! id columns + each var_name once (no _lyrK suffix).
        integer, intent(in) :: unit_no
        character(len=*), intent(in) :: var_names(:)
        logical, intent(in) :: is_csv

        character(len=32) :: tag
        integer :: i

        if (is_csv) then
          write (unit_no, '(a)', advance='no') "jday,mon,day,yr,unit,gis_id,name"
          do i = 1, size(var_names)
            write (unit_no, '(a,a)', advance='no') ",", trim(var_names(i))
          end do
          write (unit_no, '(a)') ""
        else
          write (unit_no, '(a)', advance='no') "        jday         mon         day          yr        unit                gis_id    name        "
          do i = 1, size(var_names)
            tag = var_names(i)
            write (unit_no, '(1x,a22)', advance='no') tag
          end do
          write (unit_no, '(a)') ""
        end if
      end subroutine cb_write_flat_header

      subroutine cb_write_wide_header(unit_no, var_names, is_csv)
        !! emits the header line for any wide-per-layer carbon file.
        !! id columns + depth_lyr1..depth_lyrN + for each var_name: <var>_lyr1..<var>_lyrN
        !! caller already wrote the banner row; this writes the column-label row.
        integer, intent(in) :: unit_no
        character(len=*), intent(in) :: var_names(:)
        logical, intent(in) :: is_csv

        character(len=32) :: tag
        integer :: i, k

        if (is_csv) then
          write (unit_no, '(a)', advance='no') "jday,mon,day,yr,unit,gis_id,name"
          do k = 1, cb_n_layers
            write (tag, '(a,i0)') ",depth_lyr", k
            write (unit_no, '(a)', advance='no') trim(tag)
          end do
          do i = 1, size(var_names)
            do k = 1, cb_n_layers
              write (tag, '(a,a,i0)') ",", trim(var_names(i))//"_lyr", k
              write (unit_no, '(a)', advance='no') trim(tag)
            end do
          end do
          write (unit_no, '(a)') ""
        else
          write (unit_no, '(a)', advance='no') "        jday         mon         day          yr        unit                gis_id    name        "
          do k = 1, cb_n_layers
            write (tag, '(a,i0)') "depth_lyr", k
            write (unit_no, '(1x,a22)', advance='no') tag
          end do
          do i = 1, size(var_names)
            do k = 1, cb_n_layers
              write (tag, '(a,a,i0)') trim(var_names(i))//"_lyr", "", k
              write (unit_no, '(1x,a22)', advance='no') tag
            end do
          end do
          write (unit_no, '(a)') ""
        end if
      end subroutine cb_write_wide_header

      subroutine cb_write_cbn_lyr_header(unit_no, is_csv)
        !! header for the hru_cbn_lyr files. Unlike the generic wide header, this
        !! one interleaves the 300 mm scalar sums (tot_300_sum, seq_300_sum)
        !! between the per-layer blocks, so it is built explicitly here. Column
        !! widths match the data row (1x,g22.7 per value), so labels line up.
        integer, intent(in) :: unit_no
        logical, intent(in) :: is_csv

        character(len=32) :: tag
        integer :: k

        if (is_csv) then
          write (unit_no, '(a)', advance='no') "jday,mon,day,yr,unit,gis_id,name"
          do k = 1, cb_n_layers
            write (tag, '(a,i0)') ",depth_lyr", k
            write (unit_no, '(a)', advance='no') trim(tag)
          end do
          write (unit_no, '(a)', advance='no') ",tot_300_sum"
          do k = 1, cb_n_layers
            write (tag, '(a,i0)') ",tot_lyr", k
            write (unit_no, '(a)', advance='no') trim(tag)
          end do
          write (unit_no, '(a)', advance='no') ",seq_300_sum"
          do k = 1, cb_n_layers
            write (tag, '(a,i0)') ",seq_lyr", k
            write (unit_no, '(a)', advance='no') trim(tag)
          end do
          write (unit_no, '(a)') ""
        else
          write (unit_no, '(a)', advance='no') "        jday         mon         day          yr        unit                gis_id    name        "
          do k = 1, cb_n_layers
            write (tag, '(a,i0)') "depth_lyr", k
            write (unit_no, '(1x,a22)', advance='no') tag
          end do
          write (unit_no, '(1x,a22)', advance='no') "tot_300_sum"
          do k = 1, cb_n_layers
            write (tag, '(a,i0)') "tot_lyr", k
            write (unit_no, '(1x,a22)', advance='no') tag
          end do
          write (unit_no, '(1x,a22)', advance='no') "seq_300_sum"
          do k = 1, cb_n_layers
            write (tag, '(a,i0)') "seq_lyr", k
            write (unit_no, '(1x,a22)', advance='no') tag
          end do
          write (unit_no, '(a)') ""
        end if
      end subroutine cb_write_cbn_lyr_header

      subroutine cb_write_depth_row(unit_no, depths, n_use, is_csv, advance_str)
        !! emits the depth columns for a data row, padded with cb_lyr_missing past n_use.
        !! caller has already emitted the id columns and must continue with var columns after.
        integer, intent(in) :: unit_no
        real, intent(in) :: depths(:)            !! depth at each soil layer (size >= n_use)
        integer, intent(in) :: n_use             !! actual number of soil layers in this HRU
        logical, intent(in) :: is_csv
        character(len=*), intent(in) :: advance_str  !! "no" to keep building the row, "yes" to terminate

        integer :: k
        real :: v

        do k = 1, cb_n_layers
          if (k <= n_use) then
            v = depths(k)
          else
            v = cb_lyr_missing
          end if
          if (is_csv) then
            write (unit_no, '(a,g0.7)', advance='no') ",", v
          else
            write (unit_no, '(1x,g22.7)', advance='no') v
          end if
        end do
        if (advance_str == "yes") write (unit_no, '(a)') ""
      end subroutine cb_write_depth_row

      subroutine cb_write_var_block(unit_no, vals, n_use, is_csv, advance_str)
        !! emits one variable's per-layer values, padded with cb_lyr_missing past n_use.
        integer, intent(in) :: unit_no
        real, intent(in) :: vals(:)
        integer, intent(in) :: n_use
        logical, intent(in) :: is_csv
        character(len=*), intent(in) :: advance_str

        integer :: k
        real :: v

        do k = 1, cb_n_layers
          if (k <= n_use) then
            v = vals(k)
          else
            v = cb_lyr_missing
          end if
          if (is_csv) then
            write (unit_no, '(a,g0.7)', advance='no') ",", v
          else
            write (unit_no, '(1x,g22.7)', advance='no') v
          end if
        end do
        if (advance_str == "yes") write (unit_no, '(a)') ""
      end subroutine cb_write_var_block

     end module carbon_module