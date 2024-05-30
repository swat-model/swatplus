      module carbon_module 
	  
      implicit none
      
      type carbon_terrestrial_inputs
        real :: er_POC_para = 1.5       !           |POC enrichment ratio ! 0-10 ! 0.0-5.0  MOST SENSITIVE  
        real :: CFB_para = 0.42         !           |Carbon fraction of residue (0.42; from data of Pinck et al., 1950) 
        real :: Sf_para_sur	= 0.05      !           |Fraction of mineral N sorbed to litter: 0.05 for surface litter, 0.1 for below ground litter
        real :: Sf_para_sub	= 0.10      !           |Fraction of mineral N sorbed to litter: 0.05 for surface litter, 0.1 for belowg round litter
        !Dissovled carbon
        real :: ABL_para = 0.0          !           |Calculated - Carbon allocation from Microbial Biomass to Leaching			
        real :: peroc_DIC_para = 0.95   !0-1        |DIC percolation coefficient  
        real :: peroc_DOC_para	= 0.70  !0-1        |DOC percolation coefficient
        real :: part_DOC_para = 4000.   !           |organic carbon partition coefficient 1000 to 1200 ! 500-2000 !replacing KOC=Liquid-solid partition coefficient for Microbial Biomass (10^3 m3 Mg-1)
        real :: hlife_doc_para = 50.	!days       |DOC half life in groundwater, calculating DOC decay in groundwater ! 0-100
        !Allocation of CO2 and Carbon transformation
	    real :: ABCO2_para_sur = 0.6    !           |Allocation from Microbial Biomass C pool to CO2; 0.6 (surface Litter), 0.85 - 0.68 × (CLAY+SILT) (all other layers) (Parton et al., 1993, 1994)
        real :: ABCO2_para_sub = 0.	!           |Calculated -Allocation from Microbial Biomass C pool to CO2; 0.6 (surface Litter), 0.85 - 0.68 × (CLAY+SILT) (all other layers) (Parton et al., 1993, 1994)
	    real :: ABP_para_sur = 0.0      !           |Allocation from Biomass to passive Humus; 0 (surface Litter), 0.003 + 0.032 x SOL_CLAY (all other layers) (Parton et al., 1993, 1994)
        real :: ABP_para_sub = 0.0      !           |Calculated - Allocation from Biomass to passive Humus; 0 (surface Litter), 0.003 + 0.032 x SOL_CLAY (all other layers) (Parton et al., 1993, 1994)
        real :: ALMCO2_para_sur	= 0.6	!           |Allocation from metabolic Litter to CO2; 0.6 (surface Litter), 0.55 (all other layers) (Parton et al., 1993, 1994)
        real :: ALMCO2_para_sub	= 0.55	!           |Allocation from metabolic Litter to CO2; 0.6 (surface Litter), 0.55 (all other layers) (Parton et al., 1993, 1994)
        real :: ALSLNCO2_para_sur = 0.6	!           |Allocation from non-lignin of structural Litter to CO2; 0.6 (surface Litter), 0.55 (all other layers) (Parton et al., 1993, 1994)
        real :: ALSLNCO2_para_sub =0.55 !	        |Allocation from non-lignin of structural Litter to CO2; 0.6 (surface Litter), 0.55 (all other layers) (Parton et al., 1993, 1994)
        real :: ASP_para_sur = 0.0	    !           |Allocation from slow Humus to passive; 0 (surface Litter), 0.003 + 0.00009 × CLAF (all other layers) (Parton et al., 1993, 1994)
        real :: ASP_para_sub = 0.0	    !           |Calculated - Allocation from slow Humus to passive; 0 (surface Litter), 0.003 + 0.00009 × CLAF (all other layers) (Parton et al., 1993, 1994)
        real :: ALSLCO2_para = 0.3	    !           |Allocation from lignin of structural Litter to CO2; 0.3 (Parton et al., 1993, 1994)
        real :: APCO2_para = 0.55	    !           |Allocation from passive Humus to CO2; 0.55 (Parton et al., 1993, 1994)
        real :: ASCO2_para = 0.55	    !           |Allocation from slow Humus to CO2; 0.55 (Parton et al., 1993, 1994)
        !decomposition rates
        real :: PRMT_51_para = 1.0	    !           |COEF ADJUSTS MICROBIAL ACTIVITY FUNCTION IN TOP SOIL LAYER (0.1_1.),
        real :: PRMT_45_para = 0.003	!           |COEF IN CENTURY EQ ALLOCATING SLOW TO PASSIVE HUMUS(0.001_0.05) ORIGINAL VALUE = 0.003, ASP=MAX(.001,PRMT_45-.00009*sol_clay(k,j)), ASP=MAX(.001,PRMT_45+.009*sol_clay(k,j)/100)
	    real :: BMR_para_sur = 0.0164	!           |Rate of transformation of microbial Biomass and associated products under optimal conditions (surface = 0.0164 d-1; all other layers = 0.02 d-1) (Parton et al., 1993, 1994)
	    real :: BMR_para_sub = 0.02	    !           |Rate of transformation of microbial Biomass and associated products under optimal conditions (surface = 0.0164 d-1; all other layers = 0.02 d-1) (Parton et al., 1993, 1994)
        real :: HPR_para = 0.000012	    !           |Rate of transformation of passive Humus under optimal conditions (subsurface layers = 0.000012 d-1) (Parton et al., 1993, 1994)
        real :: HSR_para = 0.000548	    !           |Rate of transformation of slow Humus under optimal conditions (all layers = 0.0005 d-1) (Parton et al., 1993, 1994; Vitousek et al., 1993)
        real :: LMR_para_sur = 0.0405	!           |Rate of transformation of metabolic Litter under optimal conditions (surface = 0.0405 d-1; all other layers = 0.0507 d-1) (Parton et al., 1994)
        real :: LMR_para_sub = 0.0507	!           |Rate of transformation of metabolic Litter under optimal conditions (surface = 0.0405 d-1; all other layers = 0.0507 d-1) (Parton et al., 1994)
        real :: LSR_para_sur = 0.0107	!           |Rate of potential transformation of structural Litter under optimal conditions (surface = 0.0107 d-1; all other layers = 0.0132 d-1) (Parton et al., 1994)
        real :: LSR_para_sub = 0.0132	!           |Rate of potential transformation of structural Litter under optimal conditions (surface = 0.0107 d-1; all other layers = 0.0132 d-1) (Parton et al., 1994)
        !Soil texture controls of microbial activity
        real :: XBM_para_sur = 1.0	    !           |Control on transformation of microbial Biomass by soil texture and structure. Its values: surface Litter layer = 1; all other layers = 1 – 0.75 × (SILT + CLAY) (Parton et al., 1993, 1994)
        real :: XBM_para_sub = 0.0	    !           |Calculated - Control on transformation of microbial Biomass by soil texture and structure. Its values: surface Litter layer = 1; all other layers = 1 – 0.75 × (SILT + CLAY) (Parton et al., 1993, 1994)
        real :: XLSLF_para = 0.0	    !           |Calculated - Control on potential transformation of structural Litter by lignin fraction of structural Litter [XLSLF = exp(-3 × LSLF) (Parton et al., 1993, 1994)]
        !Oxygen factor control parameters
        real :: OX_aa_para = 10.0	    !           !Coefficient in calculating oxygen factor 
        real :: OX_bb_para = 0.035	    !           |Coefficient in calculating oxygen factor 
      end type carbon_terrestrial_inputs
      type (carbon_terrestrial_inputs) :: cbn_tes

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
      type (carbon_inputs) :: carbdb 
      type (carbon_inputs) :: carbz  
	  
      type organic_allocations
          real :: abco2           !               |Fraction of decomposed microbial biomass allocated to CO2
		  real :: abl             !               |Fraction of microbial biomass loss due to leaching
		  real :: abp             !               |Fraction of decomposed microbial biomass allocated to passive humus
		  real :: almco2          !               |Fraction of decomposed metabolic litter allocated to CO2
		  real :: alslco2         !               |Fraction of decomposed lignin of structural litter allocated to CO2
		  real :: alslnco2        !               |Fraction of decomposed lignin of structural litter allocated to CO2
		  real :: apco2           !               |Fraction of decomposed  passive humus allocated to CO2
		  real :: asco2           !               |Fraction of decomposed slow humus allocated to CO2
		  real :: asp             !               |Fraction of decomposed slow humus allocated to passive
      end type organic_allocations
      type (organic_allocations) :: org_allo 
      type (organic_allocations) :: org_alloz
	    
      type organic_controls
	      real :: cdg                !                 |soil temperature control on biological processes
		  real :: cs                 !                 |combined factor controlling biological processes
		  real :: ox                 !                 |oxygen control on biological processes 
		  real :: sut                !                 |soil water control on biological processes
	      real :: x1                 !                 |tillage control on residue decomposition
          real :: xbmt               !                 |control on transformation of microbial biomass by soil texture and structure
          real :: xlslf              !                 |control on potential transformation of structural litter by lignin fraction
      end type organic_controls
      type (organic_controls) :: org_con                     
	    
      type organic_fractions
          real :: lmf           !frac               |fraction of the litter that is metabolic
          real :: lmnf          !kg kg-1            |fraction of metabolic litter that is N
          real :: lsf           !frac               |fraction of the litter that is structural
          real :: lslf          !kg kg-1            |fraction of structural litter that is lignin 
          real :: lsnf          !kg kg-1            |fraction of structural litter that is N		  
      end type organic_fractions
      type (organic_fractions) :: org_frac                    
	    
      type organic_ratio
          real :: cnr              !                  |c/n ratio of standing dead
          real :: ncbm             !                  |n/c ratio of biomass           
          real :: nchp             !                  |n/c ratio of passive humus
		  real :: nchs             !                  |n/c ration of slow humus
      end type organic_ratio
      type (organic_ratio) :: org_ratio                   
	  
      type organic_transformations
          real :: bmctp            !kg ha-1 day-1        |potential transformation of C in microbial biomass
		  real :: bmntp            !kg ha-1 day-1        |potential transformation of N in microbial biomass
		  real :: hsctp            !kg ha-1 day-1        |potential transformation of C in slow humus
		  real :: hsntp            !kg ha-1 day-1        |potential transformation of N in slow humus
          real :: hpctp            !kg ha-1 day-1        |potential transformation of C in passive humus 
          real :: hpntp            !kg ha-1 day-1        |potential transformation of N in passive humus
          real :: lmctp            !kg ha-1 day-1        |potential transformation of C in metabolic litter
          real :: lmntp            !kg ha-1 day-1        |potential transformation of N in metabolic litter	
          real :: lsctp            !kg ha-1 day-1        |potential transformation of C in structural litter
          real :: lslctp           !kg ha-1 day-1        |potential transformation of C in lignin of structural litter
          real :: lslnctp          !kg ha-1 day-1        |potential transformation of C in nonlignin structural litter
          real :: lsntp            !kg ha-1 day-1        |potential transformation of N in structural litter			  
      end type organic_transformations
      type (organic_transformations) :: org_tran
      
      type organic_flux
          real :: cfmets1                !(kg C ha-1 day-1) |C transfromed from Metabolic Litter to S1 (Microbial Biomass) 
          real :: cfstrs1                !(kg C ha-1 day-1) |C transfromed from Structural Litter to S1 (Microbial Biomass)  
          real :: cfstrs2                !(kg C ha-1 day-1) |C transfromed from Structural Litter to S2 (Slow Humus) 
          real :: efmets1                !(kg N ha-1 day-1) |N transformed from Metabolic Litter to S1 (Microbial Biomass) 
          real :: efstrs1                !(kg N ha-1 day-1) |N transformed from Structural Litter to S1 (Microbial Biomass) 
          real :: efstrs2                !(kg N ha-1 day-1) |N transformed from Structural Litter to S2 (Slow Humus)  
          real :: immmets1               !(kg N ha-1 day-1) |N immibolization resulting from transforming Metabolic Litter to S1 (Microbial Biomass)   
          real :: immstrs1               !(kg N ha-1 day-1) |N immibolization resulting from transforming Structural Litter to S1 (Microbial Biomass) 
          real :: immstrs2               !(kg N ha-1 day-1) |N immibolization resulting from transforming Structural Litter to S2 (Slow Humus)  
          real :: mnrmets1               !(kg N ha-1 day-1) |N mineralization resulting from transforming Metabolic Litter to S1 (Microbial Biomass)   
          real :: mnrstrs1               !(kg N ha-1 day-1) |N mineralization resulting from transforming Structural Litter to S1 (Microbial Biomass) 
          real :: mnrstrs2               !(kg N ha-1 day-1) |N mineralization resulting from transforming Structural Litter to S2 (Slow Humus)  
          real :: co2fmet                !(kg C ha-1 day-1) |CO2 production resulting from metabolic litter transformaitons               
          real :: co2fstr                !(kg C ha-1 day-1) |CO2 production resulting from lignin structural litter transformaitons  
          real :: cfs1s2                 !(kg C ha-1 day-1) |C transformed from S1 (Microbial Biomass) to S2 (Slow Humus)    
          real :: cfs1s3                 !(kg C ha-1 day-1) |C transformed from S1 (Microbial Biomass) to S3 (Passive Humus)  
          real :: cfs2s1                 !(kg C ha-1 day-1) |C transformed from S2 (Slow Humus) to S1 (Microbial Biomass)  
          real :: cfs2s3                 !(kg C ha-1 day-1) |C transformed from S2 (Slow Humus) to S3 (Passive Humus)  
          real :: cfs3s1                 !(kg C ha-1 day-1) |C transformed from  S3 (Passive Humus) to S1 (Microbial Biomass) 
          real :: efs1s2                 !(kg N ha-1 day-1) |N transformed from from S1 (Microbial Biomass) to S2 (Slow Humus)  
          real :: efs1s3                 !(kg N ha-1 day-1) |N transformed from from S1 (Microbial Biomass) to S3 (Passive Humus) 
          real :: efs2s1                 !(kg N ha-1 day-1) |N transformed from from S2 (Slow Humus) to S1 (Microbial Biomass) 
          real :: efs2s3                 !(kg N ha-1 day-1) |N transformed from from S2 (Slow Humus) to S3 (Passive Humus) 
          real :: efs3s1                 !(kg N ha-1 day-1) |N transfromed from from  S3 (Passive Humus) to S1 (Microbial Biomass) 
          real :: imms1s2                !(kg N ha-1 day-1) |N immibolization resulting from transforming S1 (Microbial Biomass) to S2 (Slow Humus)   
          real :: imms1s3                !(kg N ha-1 day-1) |N immibolization resulting from transforming S1 (Microbial Biomass) to S3 (Passive Humus)  
          real :: imms2s1                !(kg N ha-1 day-1) |N immibolization resulting from transforming S2 (Slow Humus) to S1 (Microbial Biomass) 
          real :: imms2s3                !(kg N ha-1 day-1) |N immibolization resulting from transforming S2 (Slow Humus) to S3 (Passive Humus)  
          real :: imms3s1                !(kg N ha-1 day-1) |N immibolization resulting from transforming  S3 (Passive Humus) to S1 (Microbial Biomass)  
          real :: mnrs1s2                !(kg N ha-1 day-1) |N mineralization resulting from transforming S1 (Microbial Biomass) to S2 (Slow Humus)  
          real :: mnrs1s3                !(kg N ha-1 day-1) |N mineralization resulting from transforming S1 (Microbial Biomass) to S3 (Passive Humus) 
          real :: mnrs2s1                !(kg N ha-1 day-1) |N mineralization resulting from transforming S2 (Slow Humus) to S1 (Microbial Biomass)  
          real :: mnrs2s3                !(kg N ha-1 day-1) |N mineralization resulting from transforming S2 (Slow Humus) to S3 (Passive Humus)  
          real :: mnrs3s1                !(kg N ha-1 day-1) |N mineralization resulting from transforming  S3 (Passive Humus) to S1 (Microbial Biomass)  
          real :: co2fs1                 !(kg C ha-1 day-1) |CO2 production resulting from S1 (Microbial Biomass) transformations  
          real :: co2fs2                 !(kg C ha-1 day-1) |CO2 production resulting from S2 (Slow Humus)  transformations  
          real :: co2fs3                 !(kg C ha-1 day-1) |CO2 production resulting from S3 (Passive Humus) transformations  
      end type organic_flux
      type (organic_flux) :: org_flux
      
      type carbon_soil_transformations
          real :: meta_micr             !(kg C ha-1 day-1) |C transformed from Metabolic Litter to S1 (Microbial Biomass) 
          real :: str_micr              !(kg C ha-1 day-1) |C transformed from Structural Litter to S1 (Microbial Biomass)  
          real :: str_hs                !(kg C ha-1 day-1) |C transformed from Structural Litter to S2 (Slow Humus)
          real :: co2_meta              !(kg C ha-1 day-1) |CO2 production resulting from metabolic litter transformations               
          real :: co2_str               !(kg C ha-1 day-1) |CO2 production resulting from lignin structural litter transformations  
          real :: micr_hs               !(kg C ha-1 day-1) |C transformed from S1 (Microbial Biomass) to S2 (Slow Humus)    
          real :: micr_hp               !(kg C ha-1 day-1) |C transformed from S1 (Microbial Biomass) to S3 (Passive Humus)  
          real :: hs_micr               !(kg C ha-1 day-1) |C transformed from S2 (Slow Humus) to S1 (Microbial Biomass)  
          real :: hs_hp                 !(kg C ha-1 day-1) |C transformed from S2 (Slow Humus) to S3 (Passive Humus)  
          real :: hp_micr               !(kg C ha-1 day-1) |C transformed from  S3 (Passive Humus) to S1 (Microbial Biomass) 
          real :: co2_micr              !(kg C ha-1 day-1) |CO2 production resulting from S1 (Microbial Biomass) transformations  
          real :: co2_hs                !(kg C ha-1 day-1) |CO2 production resulting from S2 (Slow Humus)  transformations  
          real :: co2_hp                !(kg C ha-1 day-1) |CO2 production resulting from S3 (Passive Humus) transformations  
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
      type (carbon_soil_transformations), dimension (:), allocatable :: lcsf_a
      !! basin soil carbon transformations
      type (carbon_soil_transformations) :: bscf_d
      type (carbon_soil_transformations) :: bscf_m
      type (carbon_soil_transformations) :: bscf_y
      type (carbon_soil_transformations) :: bscf_a
      
      type carbon_soil_gain_losses
        real :: sed_c = 0.              !kg C/ha            |C transported with sediment yield
        real :: surq_c = 0.             !kg C/ha            |total dissolved C transported with surface runoff
        real :: surq_doc = 0.           !kg C/ha            |dissolved organic C transported with surface runoff
        real :: surq_dic = 0.           !kg C/ha            |dissolved inorganic C transported with surface runoff
        real :: latq_c = 0.             !kg C/ha            |dissolved organic C transported with lateral flow (all layers)
        real :: latq_doc= 0.            !kg C/ha            |total dissolved C transported with lateral flow (all layers)
        real :: latq_dic = 0.           !kg C/ha            |dissolved inorganic C transported with lateral flow (all layers)
        real :: perc_c = 0.             !kg C/ha            |total dissolved C transported with percolate
        real :: perc_doc = 0.           !kg C/ha            |dissolved organic C transported with percolate
        real :: perc_dic = 0.           !kg C/ha            |dissolved inorganic C transported with percolate
        real :: res_decay_c = 0.        !kg C/ha            |carbon added to soil from residue decay
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
      type (carbon_soil_gain_losses), dimension (:), allocatable :: lcs_a
      !! basin soil carbon gains and losses
      type (carbon_soil_gain_losses) :: bsc_d
      type (carbon_soil_gain_losses) :: bsc_m
      type (carbon_soil_gain_losses) :: bsc_y
      type (carbon_soil_gain_losses) :: bsc_a
      
      type carbon_residue_gain_losses
        real :: plant_c = 0.            !kg C/ha            |carbon added to residue from leaf drop and kill 
        real :: res_decay_c = 0.        !kg C/ha            |carbon lost to soil from residue decay
        real :: harv_stov_c = 0.        !kg C/ha            |carbon removed during residue harvest  
        real :: emit_c = 0.             !kg C/ha            |CO2 production from burning residue carbon
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
      type (carbon_residue_gain_losses), dimension (:), allocatable :: lrs_a
      !! basin residue carbon gains and losses
      type (carbon_residue_gain_losses) :: brc_d
      type (carbon_residue_gain_losses) :: brc_m
      type (carbon_residue_gain_losses) :: brc_y
      type (carbon_residue_gain_losses) :: brc_a
      
      type carbon_plant_gain_losses
        real :: npp_c = 0.              !kg C/ha            |plant carbon growth from photosynthesis
        real :: harv_c = 0.             !kg C/ha            |carbon removed during grain/biomass harvest
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
      type (carbon_plant_gain_losses), dimension (:), allocatable :: lps_a
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
        hru3%surq_doc = hru1%surq_doc + hru2%surq_doc
        hru3%surq_dic = hru1%surq_dic + hru2%surq_dic
        hru3%latq_c = hru1%latq_c + hru2%latq_c
        hru3%latq_doc = hru1%latq_doc + hru2%latq_doc
        hru3%latq_dic = hru1%latq_dic + hru2%latq_dic
        hru3%perc_c = hru1%perc_c + hru2%perc_c
        hru3%perc_doc = hru1%perc_doc + hru2%perc_doc
        hru3%perc_dic = hru1%perc_dic + hru2%perc_dic
        hru3%res_decay_c = hru1%res_decay_c + hru2%res_decay_c
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
        hru2%surq_doc = hru1%surq_doc * const
        hru2%surq_dic = hru1%surq_dic * const
        hru2%latq_c = hru1%latq_c * const
        hru2%latq_doc = hru1%latq_doc * const
        hru2%latq_dic = hru1%latq_dic * const
        hru2%perc_c = hru1%perc_c * const
        hru2%perc_doc = hru1%perc_doc * const
        hru2%perc_dic = hru1%perc_dic * const
        hru2%res_decay_c = hru1%res_decay_c * const
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
        hru2%surq_doc = hru1%surq_doc / const
        hru2%surq_dic = hru1%surq_dic / const
        hru2%latq_c = hru1%latq_c / const
        hru2%latq_doc = hru1%latq_doc / const
        hru2%latq_dic = hru1%latq_dic / const
        hru2%perc_c = hru1%perc_c / const
        hru2%perc_doc = hru1%perc_doc / const
        hru2%perc_dic = hru1%perc_dic / const
        hru2%res_decay_c = hru1%res_decay_c / const
        hru2%man_app_c = hru1%man_app_c / const
        hru2%man_graz_c = hru1%man_graz_c / const
        hru2%rsp_c = hru1%rsp_c / const
        hru2%emit_c = hru1%emit_c / const
      end function carbon_soil_gl_div
      
      function carbon_residue_gl__add (hru1, hru2) result (hru3)
        type (carbon_residue_gain_losses), intent (in) :: hru1
        type (carbon_residue_gain_losses), intent (in) :: hru2
        type (carbon_residue_gain_losses) :: hru3
        hru3%plant_c = hru1%plant_c + hru2%plant_c
        hru3%res_decay_c = hru1%res_decay_c + hru2%res_decay_c
        hru3%harv_stov_c = hru1%harv_stov_c + hru2%harv_stov_c
        hru3%emit_c = hru1%emit_c + hru2%emit_c
       end function carbon_residue_gl__add
       
      function carbon_residue_gl_mult (hru1,const) result (hru2)
        type (carbon_residue_gain_losses), intent (in) :: hru1
        real, intent (in) :: const
        type (carbon_residue_gain_losses) :: hru2
        hru2%plant_c = hru1%plant_c * const
        hru2%res_decay_c = hru1%res_decay_c * const
        hru2%harv_stov_c = hru1%harv_stov_c * const
        hru2%emit_c = hru1%emit_c * const
      end function carbon_residue_gl_mult
      
      function carbon_residue_gl_div (hru1,const) result (hru2)
        real, intent (in) :: const
        type (carbon_residue_gain_losses), intent (in) :: hru1
        type (carbon_residue_gain_losses) :: hru2
        hru2%plant_c = hru1%plant_c / const
        hru2%res_decay_c = hru1%res_decay_c / const
        hru2%harv_stov_c = hru1%harv_stov_c / const
        hru2%emit_c = hru1%emit_c * const
      end function carbon_residue_gl_div
      
      function carbon_plant_gl__add (hru1, hru2) result (hru3)
        type (carbon_plant_gain_losses), intent (in) :: hru1
        type (carbon_plant_gain_losses), intent (in) :: hru2
        type (carbon_plant_gain_losses) :: hru3
        hru3%npp_c = hru1%npp_c + hru2%npp_c
        hru3%harv_c = hru1%harv_c + hru2%harv_c
        hru3%drop_c = hru1%drop_c + hru2%drop_c
        hru3%grazeat_c = hru1%grazeat_c + hru2%grazeat_c
        hru3%emit_c = hru1%emit_c + hru2%emit_c
       end function carbon_plant_gl__add
       
      function carbon_plant_gl_mult (hru1,const) result (hru2)
        type (carbon_plant_gain_losses), intent (in) :: hru1
        real, intent (in) :: const
        type (carbon_plant_gain_losses) :: hru2
        hru2%npp_c = hru1%npp_c * const
        hru2%harv_c = hru1%harv_c * const
        hru2%drop_c = hru1%drop_c * const
        hru2%grazeat_c = hru1%grazeat_c * const
        hru2%emit_c = hru1%emit_c * const
      end function carbon_plant_gl_mult
      
      function carbon_plant_gl_div (hru1,const) result (hru2)
        type (carbon_plant_gain_losses), intent (in) :: hru1
        real, intent (in) :: const
        type (carbon_plant_gain_losses) :: hru2
        hru2%npp_c = hru1%npp_c / const
        hru2%harv_c = hru1%harv_c / const
        hru2%drop_c = hru1%drop_c / const
        hru2%grazeat_c = hru1%grazeat_c / const
        hru2%emit_c = hru1%emit_c / const
      end function carbon_plant_gl_div
      
     end module carbon_module   