      module calibration_data_module
    
      implicit none
      
       type calibration_parameters
        character(len=25) :: name = "default"   !         |cn2, esco, awc, etc.
        character(len=25) :: ob_typ = "plt"     !         |object type the parameter is associated with (hru, chan, res, basin, etc)
        real :: absmin = 0.                     !         |minimum range for variable
        real :: absmax = 1.e6                   !         |maximum change for variable
        character(len=25) :: units = "null"     !         |units used for each parameter
      end type calibration_parameters
      type (calibration_parameters), dimension (:), allocatable :: cal_parms    !dimensioned to db_mx%cal_parms_tot
      
      type calibration_conditions
        character(len=25) :: var
        character(len=25) :: alt
        real :: targ
        character(len=25) :: targc
      end type calibration_conditions   

      type update_parameters
        character(len=25) :: name       !! cn2, terrace, land use, mgt, etc.
        integer :: num_db = 0           !! crosswalk number of parameter, structure or land use to get database array number
        character(len=16) :: chg_typ    !! type of change (absval,abschg,pctchg)
        real :: val                     !! value of change
        integer :: conds                !! number of conditions
        integer :: lyr1                 !! first layer in range for soil variables (0 assumes all layers are modified)
        integer :: lyr2                 !! last layer in range for soil variables (0 assumes through last layer)
        integer :: year1                !! first year for precip and temp
        integer :: year2                !! last year for precip and temp
        integer :: day1                 !! first day in range for precip and temp
        integer :: day2                 !! last day in range for precip and temp
        integer :: num_tot = 0          !! total number of integers read in
        integer :: num_elem = 0         !! total number of elements modified (ie - 1 -5 18; num_tot=3 and num_elem=6)
        integer, dimension(:), allocatable :: num
        integer :: num_cond
        type (calibration_conditions), dimension(:), allocatable :: cond
      end type update_parameters

      type (update_parameters), dimension (:), allocatable :: cal_upd   !dimensioned to db_mx%cal_parms
      type (update_parameters) :: chg

      type update_conditional
        integer :: max_hits = 0         !! maximum number of times the table will be executed
        integer :: num_hits = 0         !! current number of times the table will be executed
        character(len=25) :: typ        !! type of table- "lu_change" checks all hru; "hru_fr_change" sets all hru fractions
        character(len=25) :: dtbl       !! points to ruleset in conditional.ctl for scheduling the update
        integer :: cond_num             !! integer pointer to d_table in conditional.ctl
      end type update_conditional
      type (update_conditional), dimension (:), allocatable :: upd_cond
      
      type soft_calibration_codes
        character (len=1) :: hyd_hru = "n"      !! if a, calibrate all hydrologic balance processes for hru by land use in each region
                                                !! if b, calibrate baseflow and total runoff for hru by land use in each region
                                                !! if y, defaults to b for existing NAM simulations
        character (len=1) :: hyd_hrul = "n"     !! if y, calibrate hydrologic balance for hru_lte by land use in each region
        character (len=1) :: plt = "n"          !! if y, calibrate plant growth by land use (by plant) in each region
        character (len=1) :: sed = "n"          !! if y, calibrate sediment yield by land use in each region  
        character (len=1) :: nut = "n"          !! if y, calibrate nutrient balance by land use in each region
        character (len=1) :: chsed = "n"        !! if y, calibrate channel widening and bank accretion by stream order
        character (len=1) :: chnut = "n"        !! if y, calibrate channel nutrient balance by stream order
        character (len=1) :: res = "n"          !! if y, calibrate reservoir budgets by reservoir
      end type soft_calibration_codes
      type (soft_calibration_codes) :: cal_codes
      character (len=1) :: cal_soft = "n"       !! if y, calibrate at least one balance
      character (len=1) :: cal_hard = "n"       !! if y, perform hard calibration
      
      type soft_calib_parms
        character(len=16) :: name       !! cn2, terrace, land use, mgt, etc.
        integer :: num_db = 0           !! crosswalk number of parameter, structure or land use to get database array number
        character(len=16) :: chg_typ    !! type of change (absval,abschg,pctchg)
        real :: neg                     !! negative limit of change
        real :: pos                     !! positive limit of change
        real :: lo                      !! lower limit of parameter
        real :: up                      !! upper limit of parameter
      end type soft_calib_parms
      type (soft_calib_parms), dimension(:), allocatable :: ls_prms
      type (soft_calib_parms), dimension(:), allocatable :: ch_prms

      type soft_calib_ls_adjust
        real :: cn = 0.         !+/- or 0/1       |cn2 adjustment or at limit
        real :: esco = 0.       !+/- or 0/1       |esco adjustment or at limit
        real :: lat_len = 0.    !+/- or 0/1       |lateral flow soil length adjustment or at limit
        real :: petco = 0.      !+/- or 0/1       |k (lowest layer) adjustment or at limit
        real :: slope = 0.      !+/- or 0/1       |slope adjustment or at limit        
        real :: tconc = 0.      !+/- or 0/1       |time of concentration adjustment or at limit
        real :: etco = 0.       !+/- or 0/1       |etco adjustment or at limit
        real :: perco = 0.      !+/- or 0/1       |percolation coefficient adjustment or at limit
        real :: revapc = 0.     !+/- or 0/1       |slope adjustment or at limit
        real :: cn3_swf = 0.    !+/- or 0/1       |cn3_swf adjustment or at limit
      end type soft_calib_ls_adjust

      type soft_calib_ls_processes
        !database of soft ave annual landscape calibration values
        character(len=16) :: name = "default"
        ! srr + lfr + pcr + etr + tfr = 1
        real :: srr = 0.    !- or m3        |surface runoff ratio - surface runoff/precip
        real :: lfr = 0.    !- or m3        |lateral flow ratio - soil lat flow/precip 
        real :: pcr = 0.    !- or m3        |percolation ratio - perc/precip
        real :: etr = 0.    !- or m3        |et ratio - et/precip
        real :: tfr = 0.    !- or m3        |tile flow ratio - tile flow/total runoff 
        real :: pet = 0.    !- or m3        |ave annual potential et
        real :: sed = 0.    !t/ha or t      |sediment yield
        !real :: orgn = 0.  !kg/ha or kg    |organic n yield
        real :: wyr = 0.    !- or m3        |water yield ratio - total water yield/precip
        real :: bfr = 0.    !- or m3        |base flow ratio - base flow/precip - lat+prec+tile
        real :: solp = 0.   !kg/ha or kg    |soluble p yield
      end type soft_calib_ls_processes
      type (soft_calib_ls_processes) :: lscal_z  !to zero values

      type ls_calib_regions
        character(len=16) :: name = "default"
        integer :: lum_no                                       !xwalk lum()%name with lscal()%lum()%name
        real :: ha                                              !ha of each land use
        integer :: nbyr = 0                                     !number of years the land use occurred 
        type (soft_calib_ls_processes) :: meas                  !input soft calibration parms of each land use - ratio,t/ha,kg/ha
        real :: precip = 0.                                     !model precip for each land use to determine ratios
        real :: precip_aa = 0.                                  !model ave annual precip for each land use to determine ratios
        real :: precip_aa_sav = 0.                              !model ave annual precip for each land use to determine ratios for final output
        real :: pet = 0.                                        !model precip for each land use to determine ratios
        real :: pet_aa = 0.                                     !model ave annual precip for each land use to determine ratios
        real :: petco = 0.                                      !potential et coefficient - linear adjustment, no iterating
        type (soft_calib_ls_processes) :: sim                   !simulated sum of soft calibration parms of each land use - m3,t,kg
        type (soft_calib_ls_processes) :: aa                    !average annual soft calibration parms of each land use - mm,t/ha,kg/ha
        type (soft_calib_ls_processes) :: prev                  !simulated sum of soft calibration parms of previous run - m3,t,kg
        type (soft_calib_ls_adjust) :: prm                      !parameter adjustments used in landscape calibration
        type (soft_calib_ls_adjust) :: prm_prev                 !parameter adjustments used in landscape calibration
        type (soft_calib_ls_adjust) :: prm_lim                  !code if parameters are at limits
        type (soft_calib_ls_adjust) :: pcur                     !current parameter
        type (soft_calib_ls_adjust) :: phi                      !high parameter
        type (soft_calib_ls_adjust) :: plo                      !low parameter
        type (soft_calib_ls_processes) :: scur                  !simulated sum of soft calibration parms of each land use - m3,t,kg
        type (soft_calib_ls_processes) :: shi                   !average annual soft calibration parms of each land use - mm,t/ha,kg/ha
        type (soft_calib_ls_processes) :: slo                   !simulated sum of soft calibration parms of previous run - m3,t,kg

      end type ls_calib_regions
            
      type soft_data_calib_landscape
        character(len=16) :: name = "default"                               !name of region - (number of regions = db_mx%lsu_reg)
        integer :: lum_num                                                  !number of land uses in each region
        integer :: num_tot                                                  !number of hru"s in each region
        integer, dimension(:), allocatable :: num                           !hru"s that are included in the region
        integer :: num_reg                                                  !number of regions the soft data applies to
        character(len=16), dimension(:), allocatable :: reg                 !name of regions the soft data applies to
        integer, dimension(:), allocatable :: ireg                          !name of regions the soft data applies to
        type (ls_calib_regions), dimension(:), allocatable :: lum           !dimension for land uses within a region
      end type soft_data_calib_landscape
      type (soft_data_calib_landscape), dimension(:), allocatable :: lscal  !dimension by region for hru"s
      type (soft_data_calib_landscape), dimension(:), allocatable :: lscalt !dimension by region for hru_lte"s
            
      type pl_parms_cal
        character(len=16) :: var = "default"
        character(len=16) :: name = "default"
        real :: init_val                            !! xwalk lum()%name with lscal()%lum()%name
        character(len=16) :: chg_typ                !! type of change (absval,abschg,pctchg)
        real :: neg                     !! negative limit per interation
        real :: pos                     !! positive limit per iteration
        real :: lo                      !! ultimate lower limit of parameter
        real :: up                      !! ultimate upper limit of parameter
      end type pl_parms_cal

      type pl_parm_region
        character(len=16) :: name = "default"                           !name of region - (number of regions = db_mx%lsu_reg)
        integer :: lum_num                                              !number of land uses in each region
        integer :: parms = 2                                            !number of plant parameters used in calibration
        integer :: num_tot                                              !number of hru"s in each region
        integer, dimension(:), allocatable :: num                       !hru"s that are included in the region
        type (pl_parms_cal), dimension(:), allocatable :: prm           !dimension for land uses within a region
      end type pl_parm_region
      type (pl_parm_region), dimension(:), allocatable :: pl_prms       !dimension by region for hru"s
 
      type cataloging_units
        character(len=16) :: name = "basin"                     !name of region - (number of regions = db_mx%lsu_reg)
        real :: area_ha                                         !area of landscape cataloging unit -hectares
        integer :: num_tot                                      !number of hru"s in each region
        integer, dimension(:), allocatable :: num               !hru"s that are included in the region
        integer :: nlum                                         !number of land use and mgt in the region
        character(len=16), dimension(:), allocatable :: lumc    !land use groups
        integer, dimension(:), allocatable :: lum_num           !db number of land use in the region - dimensioned by lum in the region
        integer, dimension(:), allocatable :: lum_num_tot       !db number of land use in the region each year- dimensioned by lum in database
        real, dimension(:), allocatable :: lum_ha               !area (ha) of land use in the region - dimensioned by lum in the region
        real, dimension(:), allocatable :: lum_ha_tot           !sum of area (ha) of land use in the region each year- dimensioned by lum in database
        real, dimension(:), allocatable :: hru_ha               !area (ha) of hrus in the region 
      end type cataloging_units
      type (cataloging_units), dimension(:), allocatable :: region     !dimension by region for hru"s
      type (cataloging_units), dimension(:), allocatable :: ccu_cal    !channel cataoging unit region
      type (cataloging_units), dimension(:), allocatable :: acu_cal    !aquifer cataoging unit region
      type (cataloging_units), dimension(:), allocatable :: rcu_cal    !reservoir cataoging unit region
      type (cataloging_units), dimension(:), allocatable :: pcu_cal    !point source cataoging unit region
          
      type landscape_units
        character(len=16) :: name = "basin"                     !name of region - (number of regions = db_mx%lsu_out)
        real :: area_ha                                         !area of landscape cataloging unit -hectares
        integer :: num_tot                                      !number of hru"s in each region
        integer, dimension(:), allocatable :: num               !hru"s that are included in the region
      end type landscape_units
      type (landscape_units), dimension(:), allocatable :: lsu_out     !dimension by region for hrus
      type (landscape_units), dimension(:), allocatable :: lsu_reg     !dimension by region for hrus
      type (landscape_units), dimension(:), allocatable :: acu_out     !dimension by region for aquifers
      type (landscape_units), dimension(:), allocatable :: acu_reg     !dimension by region for aquifers
      type (landscape_units), dimension(:), allocatable :: ccu_out     !dimension by region for channels
      type (landscape_units), dimension(:), allocatable :: ccu_reg     !dimension by region for channels
      type (landscape_units), dimension(:), allocatable :: rcu_out     !dimension by region for reservoirs
      type (landscape_units), dimension(:), allocatable :: rcu_reg     !dimension by region for reservoirs
      type (landscape_units), dimension(:), allocatable :: pcu_out     !dimension by region for point sources
      type (landscape_units), dimension(:), allocatable :: pcu_reg     !dimension by region for point sources
      
      type landscape_region_elements
        character(len=16) :: name
        real :: ha                      !area of region element -hectares
        integer :: obj = 1              !object number
        character (len=3) :: obtyp      !object type- hru, hru_lte, lsu, etc
        integer :: obtypno = 0          !2-number of hru_lte"s or 1st hru_lte command
      end type landscape_region_elements
      type (landscape_region_elements), dimension(:), allocatable :: reg_elem       !landscape region elements
      
      type landscape_elements
        character(len=16) :: name
        integer :: obj = 1              !object number
        character (len=3) :: obtyp      !object type- 1=hru, 2=hru_lte, 11=export coef, etc
        integer :: obtypno = 0          !2-number of hru_lte"s or 1st hru_lte command
        real :: bsn_frac = 0            !fraction of element in basin (expansion factor)
        real :: ru_frac = 0             !fraction of element in ru (expansion factor)
        real :: reg_frac = 0            !fraction of element in calibration region (expansion factor)
      end type landscape_elements
      type (landscape_elements), dimension(:), allocatable :: lsu_elem       !landscape cataoging unit
      type (landscape_elements), dimension(:), allocatable :: ccu_elem       !channel cataoging unit
      type (landscape_elements), dimension(:), allocatable :: acu_elem       !aquifer cataoging unit
      type (landscape_elements), dimension(:), allocatable :: rcu_elem       !reservoir cataoging unit
      type (landscape_elements), dimension(:), allocatable :: pcu_elem       !point source cataoging unit

      type soft_calib_pl_adjust
        real :: epco = 0.               !+/- or 0/1     ||plant water uptake compensation factor (0-1)
        real :: pest_stress = -1.e-9    !+/- or 0/1     |pest stress - insect/disease
        real :: harv_idx = 0.           !+/- or 0/1     |harvest index
        real :: lai_pot = 0.            !+/- or 0/1     |potential leaf area index
      end type soft_calib_pl_adjust
      
      type soft_calib_pl_processes
        !database of soft ave annual landscape calibration values
        character(len=16) :: name = "default"
        real :: yield = 0.      !t/ha or t      |crop yield
        real :: npp = 0.        !t/ha or t      |net primary productivity (biomass) dry weight
        real :: lai_mx = 0.     !               |maximum leaf area index
        real :: wstress = 0.    !               |sum of water (drought) stress
        real :: astress = 0.    !               |sum of water (aeration) stress
        real :: tstress = 0.    !               |sum of temperature stress
      end type soft_calib_pl_processes
      type (soft_calib_pl_processes) :: plcal_z  !to zero values

      type pl_calib_regions
        character(len=16) :: name = "default"
        integer :: plant_no                                     !xwalk lum()%name with lscal()%lum()%name
        real :: ha                                              !ha of each land use
        integer :: nbyr = 0                                     !number of years the land use occurred 
        type (soft_calib_pl_processes) :: meas                  !input soft calibration parms of each land use - ratio,t/ha,kg/ha
        real :: precip = 0.                                     !model precip for each land use to determine ratios
        real :: precip_aa = 0.                                  !model ave annual precip for each land use to determine ratios
        real :: precip_aa_sav = 0.                              !model ave annual precip for each land use to determine ratios for final output
        type (soft_calib_pl_processes) :: sim                   !simulated sum of soft calibration parms of each land use - m3,t,kg
        type (soft_calib_pl_processes) :: aa                    !average annual soft calibration parms of each land use - mm,t/ha,kg/ha
        type (soft_calib_pl_processes) :: prev                  !simulated sum of soft calibration parms of previous run - m3,t,kg
        type (soft_calib_pl_adjust) :: prm                      !parameter adjustments used in landscape calibration
        type (soft_calib_pl_adjust) :: prm_prev                 !parameter adjustments used in landscape calibration
        type (soft_calib_pl_adjust) :: prm_lim                  !code if parameters are at limits
        type (soft_calib_pl_adjust) :: prm_uplim                !parameter adjustments used in landscape calibration
        type (soft_calib_pl_adjust) :: prm_lowlim               !code if parameters are at limits
      end type pl_calib_regions
      
      type soft_data_calib_plant
        character(len=16) :: name = "default"   !name of region - (number of regions = db_mx%lsu_reg)
        integer :: lum_num                                                  !number of land uses in each region
        integer :: num_tot                                                  !number of hru"s in each region
        integer, dimension(:), allocatable :: num                           !hru"s that are included in the region
        type (pl_calib_regions), dimension(:), allocatable :: lum           !dimension for land uses within a region
      end type soft_data_calib_plant
      type (soft_data_calib_plant), dimension(:), allocatable :: plcal      !dimension by region for plants

      type soft_calib_chan_adjust
        real :: cov = 0.            !+/- or 0/1     |cover adjustment or at limit
        real :: erod = 0.           !+/- or 0/1     |channel erodibility adjustment or at limit
        real :: shear_bnk = 0.      !+/- or 0/1     |bank shear coefficient adjustment or at limit
        real :: hc_erod = 0.        !+/- or 0/1     |head cut erodibility adjustment or at limit
      end type soft_calib_chan_adjust
      
      type soft_calib_chan_processes
        !database of soft ave annual landscape calibration values
        character(len=16) :: name
        real :: chw = 0.    !mm/yr          |channel widening 
        real :: chd = 0.    !mm/yr          |channel downcutting or accretion
        real :: hc = 0.     !m/yr           |head cut advance
        real :: fpd = 0.    !mm/yr          |flood plain accretion 
      end type soft_calib_chan_processes
      type (soft_calib_chan_processes) :: chcal_z  !to zero values

      type chan_calib_regions
        character(len=16) :: name = "default"
        real :: length                                          !ha of each land use
        integer :: nbyr = 0                                     !number of years the land use occurred 
        type (soft_calib_chan_processes) :: meas                !input soft calibration parms of each land use - ratio,t/ha,kg/ha
        type (soft_calib_chan_processes) :: sim                 !simulated sum of soft calibration parms of each land use - m3,t,kg
        type (soft_calib_chan_processes) :: aa                  !average annual soft calibration parms of each land use - mm,t/ha,kg/ha
        type (soft_calib_chan_processes) :: prev                !simulated sum of soft calibration parms of previous run - m3,t,kg
        type (soft_calib_chan_adjust) :: prm                    !parameter adjustments used in landscape calibration
        type (soft_calib_chan_adjust) :: prm_prev               !parameter adjustments used in landscape calibration
        type (soft_calib_chan_adjust) :: prm_lim                !code if parameters are at limits
      end type chan_calib_regions
      
      type soft_data_calib_channel
        character(len=16) :: name = "default"   !name of region - (number of regions = db_mx%lsu_reg)
        integer :: ord_num                                                  !number of stream orders in each region
        integer :: num_tot                                                  !number of channels in each region
        integer, dimension(:), allocatable :: num                           !channels that are included in the region
        type (chan_calib_regions), dimension(:), allocatable :: ord         !dimension for stream order within a region
      end type soft_data_calib_channel
      type (soft_data_calib_channel), dimension(:), allocatable :: chcal  !dimension by region
                                 
      end module calibration_data_module 