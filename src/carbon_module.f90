      module carbon_module 
	  
      implicit none
      
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
      
      type carbon_losses
         real :: sedc_d               !kg C/ha                |amount of C lost with sediment
         real :: surfqc_d
         real :: latc_d
       	 real :: percc_d
         real :: foc_d
         real :: nppc_d
         real :: rsdc_d
         real :: grainc_d
         real :: stoverc_d 
         real :: rspc_d              !(kg C ha-1 day-1)       |CO2 production from soil respiration summarized for the profile  
         real :: emitc_d 	        
      end type carbon_losses
      type (carbon_losses), dimension(:), allocatable :: cbn_loss
      type (carbon_losses) :: cbn_lossz
      
     end module carbon_module   