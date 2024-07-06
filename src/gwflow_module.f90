      module gwflow_module
     
      implicit none

      !general variables ----------------------------------------------------------------------------------------------
      integer :: ncell          !               !number of gwflow cells
      integer :: num_active     !               !number of active cells
      real    :: gw_time_step   !days           !flow solution time step
      integer :: gwflag_day     !               !flag for writing daily mass balance file
      integer :: gwflag_yr      !               !flag for writing yearly mass balance file
      integer :: gwflag_aa      !               !flag for writing average annual mass balance file
      integer :: bc_type        !               !boundary conditions (1=constant head; 2=no-flow)
      integer :: conn_type      !               !recharge/ET connections (1=HRU; 2=LSU)
      integer :: out_cols       !               !number of columns used in writing output variables
      integer :: gw_daycount    !               !simulation day counter (for pumping time series)
      real*8  :: gwflow_area    !m2             !area of the watershed occupied by active gwflow cells
      
      !grid type ------------------------------------------------------------------------------------------------------
      character*15 :: grid_type                               !"structured" or "unstructured" (usg)
      integer :: grid_nrow                                    !number of rows in structured grid
      integer :: grid_ncol                                    !number of columns in structured grid
      integer, dimension (:,:), allocatable :: cell_id_usg    !usg cell number, for cell in structured grid (array)
      integer, dimension (:), allocatable :: cell_id_list     !usg cell number, for cell in structured grid (list)
      integer, dimension (:,:), allocatable :: grid_status    !cell status for structured grid
      integer, dimension (:,:), allocatable :: grid_int       !generic array for reading in values for structured grid
      real, dimension (:,:), allocatable :: grid_val          !generic array for reading in values for structured grid
      
      !groundwater state variables for each cell ----------------------------------------------------------------------
      type groundwater_state
        real :: elev = 0.           !m            |ground surface elevation
        real :: thck = 0.           !m            |aquifer thickness
        real :: botm = 0.           !m            |bottom (bedrock) elevation
        real :: xcrd = 0.           !m            |x coordinate of cell centroid
        real :: ycrd = 0.           !m            |y coordinate of cell centroid
        real :: area = 0.           !m2           |surface area
        real :: init = 0.           !m            |initial groundwater head (beginning of simulation)
        real :: head = 0.           !m            |current simulated groundwater head
        real :: hydc = 0.           !m/day        |aquifer hydraulic conductivity
        real :: spyd = 0.           !m3/m3        |aquifer specific yield
        real :: exdp = 0.           !m            |groundwater ET extinction depth
        integer :: stat = 0         !             |status (0=inactive; 1=active; 2=boundary)
        integer :: ncon = 0         !             |number of connected cells
        integer :: tile = 0         !             |tile drainage flag (0=no tile; 1=tile is present)
        real :: hnew = 0.           !m            |new groundwater head (at end of day)
        real :: hold = 0.           !m            |old groundwater head (at beginning of day)
        real :: stor = 0.           !m3           |currently available groundwater storage
        real :: vbef = 0.           !m3           |groundwater volume at beginning of day
        real :: vaft = 0.           !m3           |groundwater volume at end of day
        real :: hdmo = 0.           !m            |monthly average groundwater head
        real :: hdyr = 0.           !m            |annual average groundwater head
      end type groundwater_state
      type (groundwater_state), dimension (:), allocatable :: gw_state
      
      
      !variables for HRU (and LSU) linkage to grid cells --------------------------------------------------------------
      !variables for linking HRUs to grid cells
      integer :: hru_cells_link                                              !        |
      integer, dimension (:), allocatable :: hru_num_cells                   !        |
      integer, dimension (:), allocatable :: cell_num_hrus                   !        |
      integer, dimension (:,:), allocatable :: hru_cells                     !        |
      integer, dimension (:,:), allocatable :: cell_hrus                     !        |
      real, dimension (:,:), allocatable :: hru_cells_fract                  !        |
      real, dimension (:,:), allocatable :: cells_fract                      !        |
      real, dimension (:,:), allocatable :: cell_hrus_fract                  !        |
      !variables for linking LSUs (landscape units) to grid cells
      integer :: lsu_cells_link                                              !        |
      integer :: in_lsu_cell                                                 !        |
      integer, dimension (:), allocatable :: lsu_num_cells                   !        |
      integer, dimension (:,:), allocatable :: lsu_cells                     !        |
      real, dimension (:,:), allocatable :: lsu_cells_fract                  !        |
      integer, dimension (:), allocatable :: lsus_connected                  !        |
      
      
      !variables for groundwater sources and sinks --------------------------------------------------------------------
      
      !daily flow rates
      type groundwater_ss
        real :: rech = 0.           !m3            |volume of recharge water added to cell
        real :: gwet = 0.           !m3            |volume of ET water removed from cell
        real :: gwsw = 0.           !m3            |volume of groundwater discharging to channels
        real :: swgw = 0.           !m3            |volume of channel water seeping to groundwater via channel bed
        real :: satx = 0.           !m3            |volume of groundwater discharging to channels via saturation excess flow
        real :: soil = 0.           !m3            |volume of groundwater added to the soil profile
        real :: latl = 0.           !m3            |volume of groundwater flowing between adjacent cells
        real :: bndr = 0.           !m3            |volume of groundwater exchanged across watershed boundary
        real :: ppag = 0.           !m3            |volume of groundwater removed via agricultural pumping (irrigation)
        real :: ppdf = 0.           !m3            |volume of groundwater pumping not met by groundwater storage (deficit)
        real :: ppex = 0.           !m3            |volume of groundwater removed via external pumping (water lost from system)
        real :: tile = 0.           !m3            |volume of groundwater removed via tile drainage outflow
        real :: resv = 0.           !m3            |volume of groundwater exchanged with reservoirs
        real :: wetl = 0.           !m3            |volume of groundwater exchanged with wetlands
        real :: fpln = 0.           !m3            |volume of groundwater exchanged with floodplains
        real :: canl = 0.           !m3            |volume of groundwater exchanged with canals
        real :: totl = 0.           !m3            |sum of groundwater inputs and outputs
      end type groundwater_ss
      type (groundwater_ss), dimension (:), allocatable :: gw_ss
      
      !sums for annual flow rates
      type groundwater_ss_sum
        real :: rech = 0.           !m3            |volume of recharge water added to cell
        real :: gwet = 0.           !m3            |volume of ET water removed from cell
        real :: gwsw = 0.           !m3            |volume of groundwater discharging to channels
        real :: swgw = 0.           !m3            |volume of channel water seeping to groundwater via channel bed
        real :: satx = 0.           !m3            |volume of groundwater discharging to channels via saturation excess flow
        real :: soil = 0.           !m3            |volume of groundwater added to the soil profile
        real :: latl = 0.           !m3            |volume of groundwater flowing between adjacent cells
        real :: bndr = 0.           !m3            |volume of groundwater exchanged across watershed boundary
        real :: ppag = 0.           !m3            |volume of groundwater removed via agricultural pumping (irrigation)
        real :: ppdf = 0.           !m3            |volume of groundwater pumping not met by groundwater storage (deficit)
        real :: ppex = 0.           !m3            |volume of groundwater removed via external pumping (water lost from system)
        real :: tile = 0.           !m3            |volume of groundwater removed via tile drainage outflow
        real :: resv = 0.           !m3            |volume of groundwater exchanged with reservoirs
        real :: wetl = 0.           !m3            |volume of groundwater exchanged with wetlands
        real :: fpln = 0.           !m3            |volume of groundwater exchanged with floodplains
        real :: canl = 0.           !m3            |volume of groundwater exchanged with canals
      end type groundwater_ss_sum
      type (groundwater_ss), dimension (:), allocatable :: gw_ss_sum
      
      !grid totals for the year
      type ss_grid
        real :: chng = 0.           !m3            |grid annual total change in groundwater storage        
        real :: rech = 0.           !m3            |grid annual total recharge
        real :: gwet = 0.           !m3            |grid annual total groundwater ET 
        real :: gwsw = 0.           !m3            |grid annual total groundwater discharing to channels
        real :: swgw = 0.           !m3            |grid annual total channel water seeping to groundwater via channel bed
        real :: satx = 0.           !m3            |grid annual total groundwater discharging to channels via saturation excess flow
        real :: soil = 0.           !m3            |grid annual total groundwater added to the soil profile
        real :: latl = 0.           !m3            |grid annual total groundwater flowing between adjacent cells
        real :: bndr = 0.           !m3            |grid annual total groundwater exchanged across watershed boundary
        real :: ppag = 0.           !m3            |grid annual total groundwater removed via agricultural pumping (irrigation)
        real :: ppdf = 0.           !m3            |grid annual total groundwater pumping not met by groundwater storage (deficit)
        real :: ppex = 0.           !m3            |grid annual total groundwater removed via external pumping (water lost from system)
        real :: tile = 0.           !m3            |grid annual total groundwater removed via tile drainage outflow
        real :: resv = 0.           !m3            |grid annual total groundwater exchanged with reservoirs
        real :: wetl = 0.           !m3            |grid annual total groundwater exchanged with wetlands
        real :: fpln = 0.           !m3            |grid annual total groundwater exchanged with floodplains
        real :: canl = 0.           !m3            |grid annual total groundwater exchanged with canals
      end type ss_grid
      type (ss_grid) :: ss_grid_yr 
      type (ss_grid) :: ss_grid_tt
      
      !rech: variables for groundwater recharge ---------------------------------------------------
      integer, dimension (:), allocatable :: gw_bound_near  !           |nearest active cell to each boundary cell
      real, dimension (:), allocatable :: gw_bound_dist     !m          |distance of nearest active cell to each boundary cell
      real, dimension (:), allocatable :: gwflow_perc       !     |      
      real, dimension (:), allocatable :: gw_delay          !           |
      real, dimension (:), allocatable :: gw_rech           !           |
      real, dimension (:), allocatable :: delay             !           |
			
      !gwet: variables for groundwater evapotranspiration -----------------------------------------
      integer :: gw_et_flag                                 !           |
      real, dimension (:), allocatable :: etremain          !           |
      
      !gwsw: variables for groundwater-channel exchange -------------------------------------------
      integer :: num_chancells                              !           |
      integer, dimension (:), allocatable :: gw_chan_id     !           |
      integer, dimension (:), allocatable :: gw_chan_cell   !           |
      integer, dimension (:), allocatable :: gw_chan_chan   !           |
      integer, dimension (:), allocatable :: gw_chan_zone   !           |
      integer, dimension (:), allocatable :: gw_chan_ncell  !           |number of cells connected to each channel
      real, dimension (:), allocatable :: gw_chan_len       !           |
      real, dimension (:), allocatable :: gw_chan_elev      !           |
      real, dimension (:), allocatable :: gw_chan_K         !           |
      real, dimension (:), allocatable :: gw_chan_thick     !           |
      real :: gw_bed_change                                 !           |
      !channel-cell connection
      type cell_channel_info
        integer :: ncon                                     !           |number of cells connected to the channel
        integer, allocatable :: cells(:)                    !           |cells connected to the channel
        real, allocatable :: leng(:)                        !m          |length of channel in the cell
        real, allocatable :: elev(:)                        !m          |elevation of channel bed in the cell
        real, allocatable :: hydc(:)                        !m          |hydraulic conductivity of channel bed in the cell
        real, allocatable :: thck(:)                        !m          |thickness of channel bed in the cell
      endtype cell_channel_info
      type (cell_channel_info), dimension(:), allocatable :: gw_chan_info
      
      !satx: variables for saturated excess flow --------------------------------------------------
      integer :: gw_satx_flag                               !           |
      integer :: satx_count                                 !           |for each day: number of cells that are saturated
      type satx_channel_info
        integer :: ncon                                       !           |number of cells connected to the channel
        integer, allocatable :: cells (:)                     !           |cells connected to the channel
      endtype satx_channel_info
      type (satx_channel_info), dimension(:), allocatable :: gw_satx_info
      
      !soil: variables for gw-->soil exchange -----------------------------------------------------
      integer :: gw_soil_flag                               !           |
      real, dimension (:,:,:), allocatable :: hru_soil      !           |
      
      !latl: variables for groundwater lateral flow -----------------------------------------------
      type cell_connections
        integer, allocatable :: cell_id(:)                  !           |cells connected to the cell
        real, allocatable :: latl(:)                        !m3         |groundwater flow to/from connected cell
        real, allocatable :: sat(:)                         !m          |saturated thickness of connected cell
      endtype cell_connections
      type (cell_connections), dimension(:), allocatable :: cell_con
      
      !ppag: variables for irrigation pumping -----------------------------------------------------
      real, dimension (:), allocatable :: hru_pump          !           |
      real, dimension (:), allocatable :: hru_pump_mo       !           |
      real, dimension (:), allocatable :: hru_pump_yr       !           |
      real, dimension (:,:), allocatable :: hru_pump_mo_all !           |
      real, dimension (:,:), allocatable :: hru_pump_yr_all !           |
      !logical :: hru_pump_flag !           |
      integer :: hru_pump_flag
      integer :: in_hru_pump_obs                            !           |
      integer :: num_hru_pump_obs                           !           |
      integer, dimension (:), allocatable :: hru_pump_ids   !           |
      real, dimension (:), allocatable :: hru_pump_obs      !           |
      
      !ppex: variables for specified groundwater pumping ------------------------------------------
      integer :: gw_pumpex_flag                                    !           |
      integer :: gw_npumpex                                        !           |
      integer, dimension (:), allocatable :: gw_pumpex_cell        !           |
      integer, dimension (:), allocatable :: gw_pumpex_nperiods    !           |
      integer, dimension (:,:,:), allocatable :: gw_pumpex_dates   !           |
      real, dimension (:,:), allocatable :: gw_pumpex_rates        !           |
      
      !tile: variables for tile drainage outflow --------------------------------------------------
      integer :: gw_tile_flag                                      !           |
      integer :: gw_tile_group_flag                                !           |
      integer :: gw_tile_num_group                                 !           |
      integer :: num_tile_cells(50)                                !           |
      real    :: gw_tile_depth                                     !           |
      real    :: gw_tile_drain_area                                !           |
      real    :: gw_tile_K                                         !           |
      integer, dimension (:,:), allocatable :: gw_cell_tile        !           |
      integer, dimension (:), allocatable :: gw_tilecell_chancell  !           |
      integer, dimension (:,:), allocatable :: gw_tile_groups      !           |
      !channel-tile connection
      type tile_channel_info
        integer :: ncon                                            !     |number of cells connected to the channel
        integer, allocatable :: cells(:)                           !     |cells connected to the channel
      endtype tile_channel_info
      type (tile_channel_info), dimension(:), allocatable :: gw_tile_info
      
      !resv: variables for groundwater-reservoir exchange -----------------------------------------
      integer :: gw_res_flag                !     |
      real    :: res_thick,res_K            !     |
      integer :: num_res_cells              !     |
      !cell-reservoir connection
      type cell_reservoir_info
        integer :: ncon                     !     |number of cells connected to the channel
        integer, allocatable :: cells(:)    !     |cells connected to the channel
        real, allocatable :: elev(:)        !m    |elevation of channel bed in the cell
        real, allocatable :: hydc(:)        !m    |hydraulic conductivity of channel bed in the cell
        real, allocatable :: thck(:)        !m    |thickness of channel bed in the cell
      endtype cell_reservoir_info
      type (cell_reservoir_info), dimension(:), allocatable :: gw_resv_info
      
      !wetl: variables for groundwater-wetland exchange -------------------------------------------
      integer :: gw_wet_flag                             !     |
      real, dimension (:), allocatable :: wet_thick      !     |
      
      !fpln: variables for groundwater-floodplain exchange ----------------------------------------
      integer :: gw_fp_flag                                                 !     |
      integer :: in_fp_cell,gw_fp_ncells                                    !     |
      integer, dimension (:), allocatable :: gw_fp_cellid,gw_fp_chanid      !     |
      real, dimension (:), allocatable :: gw_fp_K,gw_fp_area                !     |
      integer, dimension (:), allocatable :: flood_freq                     !     |
      !channel-cell connection
      type cell_floodplain_info
        integer :: ncon                                  !     |number of cells connected to the channel
        integer, allocatable :: cells(:)                 !     |cells connected to the channel
        real, allocatable :: hydc(:)                     !m    |hydraulic conductivity of floodplain bottom in the cell
        real, allocatable :: area(:)                     !m    |floodplain area in connection with cell
        integer, allocatable :: mtch(:)                  !     |matching channel cell
      endtype cell_floodplain_info
      type (cell_floodplain_info), dimension(:), allocatable :: gw_fpln_info
      
      !canl: variables for groundwater-canal exchange ---------------------------------------------
      integer :: gw_canal_flag                           !     |
      integer :: gw_ncanal,gw_canal_ncells               !     |
      integer :: num_canalK_zones                        !     |
      real, dimension (:), allocatable :: canalK_zones   !     |
      !canal-channel connection
      type canal_chan_info
        integer :: ncanal                                !     |number of canals connected to the channel
        integer, allocatable :: canals(:)                !     |canals connected to the channel
        real, allocatable :: wdth(:)                     !m    |canal width
        real, allocatable :: dpth(:)                     !m    |canal depth
        real, allocatable :: thck(:)                     !m    |canal thickness
        integer, allocatable :: dayb(:)                  !     |beginning day of active canal
        integer, allocatable :: daye(:)                  !     |ending day of active canal
      endtype canal_chan_info
      type (canal_chan_info), dimension(:), allocatable :: gw_chan_canl_info
      !canal-cell connection
      type cell_canal_info
        integer :: ncon                                  !     |number of cells connected to the canal
        integer, allocatable :: cells(:)                 !     |cells connected to the canal
        real, allocatable :: leng(:)                     !m    |length of canal in the cell
        real, allocatable :: elev(:)                     !m    |stage of canal in the cell
        real, allocatable :: hydc(:)                     !m    |hydraulic conductivity of canal bed in the cell
      endtype cell_canal_info
      type (cell_canal_info), dimension(:), allocatable :: gw_canl_info 
      !canal-cell connection for canals that receive water outside of the model domain
      type cell_canal_out_info
        integer :: cell_id
        real :: wdth
        real :: dpth
        real :: thck
        real :: leng
        real :: elev
        real :: hydc
        integer :: dayb
        integer :: daye
      end type cell_canal_out_info
      type (cell_canal_out_info), dimension (:), allocatable :: gw_canl_out_info
      integer :: gw_canal_ncells_out                     !     |number of cells connected to canals that receive outside water
      real, allocatable :: canal_out_info(:,:)           !     |characteristics for canals that receive outside water
      real, allocatable :: canal_out_conc(:)             !     |solute concentration in canals that receive outside water
      
      
      !general: nearest channel for each grid cell
      real, allocatable :: cell_channel(:)               !     |nearest channel for each grid cell 
      

      !variables for writing head and concentration values ------------------------------------------------------------
      integer :: gw_num_output                                  !     |
      integer :: gw_output_index                                !     |
      integer, dimension (:), allocatable :: gw_output_yr       !     |
      integer, dimension (:), allocatable :: gw_output_day      !     |
      
      
      !variables for observation wells --------------------------------------------------------------------------------
      integer :: gw_num_obs_wells                               !     |
      integer, dimension (:), allocatable :: gw_obs_cells       !     |
      real, dimension (:), allocatable :: gw_obs_head           !     |
      integer :: gw_cell_obs_ss                                 !     |
      real, dimension (:), allocatable :: gw_cell_obs_ss_vals   !     |
      !logical :: usgs_obs!     |
      integer :: usgs_obs
      real(8), dimension (:), allocatable :: usgs_id            !     |
      real, dimension (:,:), allocatable :: usgs_head_vals      !     |
      real, dimension (:,:), allocatable :: gw_obs_head_annual  !     |
      real, dimension (:,:), allocatable :: sim_head_vals       !     |
      real, dimension (:,:), allocatable :: gw_obs_sat_annual   !     |
      real, dimension (:,:), allocatable :: sim_sat_vals        !     |
      
      
      !variables for streamflow testing and output --------------------------------------------------------------------
      !logical stream_obs!     |
      integer :: stream_obs
      integer :: gw_num_obs_chan                                !     |
      integer :: num_months                                     !     |
      integer :: sim_month                                      !     |
      integer, dimension (:), allocatable :: obs_channels       !     |
      real, dimension (:,:), allocatable :: stream_nse          !     |
      real, dimension (:,:), allocatable :: stream_nse1         !     |
      real, dimension (:,:), allocatable :: stream_nnse         !     |
      real, dimension (:,:), allocatable :: stream_kg           !     |
      real, dimension (:,:), allocatable :: stream_pbias        !     |
      real, dimension (:,:), allocatable :: obs_flow_vals       !     |
      real, dimension (:,:), allocatable :: sim_flow_vals       !     |
      integer gw_flow_cal_yrs                                   !     |
      integer :: gw_flow_cal = 0                                !     |
      

      !variables for hydrograph separation ----------------------------------------------------------------------------
      real, dimension (:,:), allocatable :: chan_hyd_sep
      integer, dimension (:), allocatable :: hydsep_flag
      
      
      !variables for groundwater solute transport ---------------------------------------------------------------------
      
      !general solute variables
      integer :: gw_solute_flag                               !    |main flag
      integer :: gw_nsolute                                   !    |number of solutes
      integer :: num_ts_transport                             !    |number of transport time steps per day
      real ::    gw_long_disp                                 !m   |aquifer longitudinal dispersivity
      integer :: gwsol_salt                                   !    |flag for simulating salt ion groundwater transport (so4,ca,mg,na,k,cl,co3,hco3)
      integer :: gwsol_cons																		!    |flag for simulating constituent groundwater transport (seo4,seo3,boron)
      integer :: gwsol_minl                                   !    |flag for simulating salt mineral precipitation-dissolution
      integer :: gw_nminl                                     !    |number of salt minerals (set to 5)
       
      !main attributes of solutes
      character (len=16) :: gwsol_nm(100)
      real :: gwsol_rctn(100)
      real :: gwsol_sorb(100)
      
      !solute cell state variables
      type solute_state
        real :: mass = 0.           !g            |solute mass in groundwater
        real :: init = 0.           !g/m3         |solute concentration in groundwater at beginning of simulation
        real :: conc = 0.           !g/m3         |solute concentration in groundwater      
        real :: cnew = 0.           !g/m3         |new concentrations at end of time step
        real :: mbef = 0.           !g            |solute mass at beginning of time step
        real :: maft = 0.           !g            |solute mass at end of time step
        real :: cnmo = 0.           !g/m3         |monthly average concentration
        real :: cnyr = 0.           !g/m3         |annual average concentration
      end type solute_state
      type object_solute_state
        type (solute_state), dimension (:), allocatable :: solute
      end type object_solute_state
      type (object_solute_state), dimension (:), allocatable :: gwsol_state

      !salt mineral cell state variables
      real :: mass_min(100)                              !g       |solute mass added/removed from cell via precipitation-dissolution
      type minl_state
        real, dimension (:), allocatable :: fract        !        |fraction of cell that is the salt mineral
      end type minl_state
      type (minl_state), dimension (:), allocatable :: gwsol_minl_state
      
      !solute cell chemical reaction variables
      integer, dimension (:), allocatable :: cell_int
      real :: mass_rct(100)         !g            |solute mass added/removed from cell via chemical reaction
      type solute_chem
        real :: ino3 = 0.           !             |selenium reduction inhibition factor
        real :: oxyg = 0.           !g/m3         |oxygen concentration in groundwater
        real :: kd_seo4 = 0.        !             |seo4 sorption partitioning coefficient
        real :: kd_seo3 = 0.        !             |seo3 sorption partitioning coefficient
        real :: kd_boron = 0.       !             |boron sorption partitioning coefficient
        real :: kseo4 = 0.          !1/day        |seo4 microbial reduction rate
        real :: kseo3 = 0.          !1/day        |seo3 microbial reduction rate
        integer :: nshale = 0                               !        |number of shale formations
        integer, dimension (:), allocatable :: shale        !        |presence of shale in cell
        real, dimension (:), allocatable :: shale_sseratio  !        |sulfur:se ratio in shale
        real, dimension (:), allocatable :: shale_o2a       !1/day   |o2 oxidation rate in presence of shale
        real, dimension (:), allocatable :: shale_no3a      !1/day   |no3 oxidation rate in presence of shale
      end type solute_chem
      type (solute_chem), dimension (:), allocatable :: gwsol_chem
      
      !solute cell mass sources and sinks (inputs and outputs)
      type solute_ss
        real :: rech = 0.           !g            |solute mass entering cell via recharge water
        real :: gwsw = 0.           !g            |solute mass leaving cell via groundwater discharging to channels
        real :: swgw = 0.           !g            |solute mass entering cell via channel water seeping to groundwater
        real :: soil = 0.           !g            |solute mass leaving cell via gw-->soil transfer
        real :: satx = 0.           !g            |solute mass leaving cell via saturation excess flow
        real :: ppag = 0.           !g            |solute mass leaving cell via pumping (for agriculture)
        real :: ppex = 0.           !g            |solute mass leaving cell via pumping (external demand)
        real :: tile = 0.           !g            |solute mass leaving cell via tile drainage outflow
        real :: resv = 0.           !g            |solute mass exchanged with reservoir
        real :: wetl = 0.           !g            |solute mass exchanged with wetland
        real :: fpln = 0.           !g            |solute mass exchanged with channel in floodplain
        real :: canl = 0.           !g            |solute mass exchanged with irrigation canal
        real :: advn = 0.           !g            |solute mass advected to/from cell
        real :: disp = 0.           !g            |solute mass dispersed to/from cell
        real :: rcti = 0.           !g            |solute mass of chemical reaction (input)
        real :: rcto = 0.           !g            |solute mass of chemical reaction (output)
        real :: minl = 0.           !g            |solute mass added (dissolution) or removed (precipitation) via salt mineral interactions
        real :: sorb = 0.           !g            |solute mass of sorption
        real :: totl = 0.           !g            |sum of mass inputs and outputs
      end type solute_ss
      type object_solute_ss
        type (solute_ss), dimension (:), allocatable :: solute
      end type object_solute_ss
      type (object_solute_ss), dimension (:), allocatable :: gwsol_ss
      
      !summed values for solutes
      type solute_ss_sum
        real :: rech = 0.           !g            |solute mass entering cell via recharge water
        real :: gwsw = 0.           !g            |solute mass leaving cell via groundwater discharging to channels
        real :: swgw = 0.           !g            |solute mass entering cell via channel water seeping to groundwater
        real :: soil = 0.           !g            |solute mass leaving cell via gw-->soil transfer
        real :: satx = 0.           !g            |solute mass leaving cell via saturation excess flow
        real :: ppag = 0.           !g            |solute mass leaving cell via pumping (for agriculture)
        real :: ppex = 0.           !g            |solute mass leaving cell via pumping (external demand)
        real :: tile = 0.           !g            |solute mass leaving cell via tile drainage outflow
        real :: resv = 0.           !g            |solute mass exchanged with reservoir
        real :: wetl = 0.           !g            |solute mass exchanged with wetland
        real :: fpln = 0.           !g            |solute mass exchanged with channel in floodplain
        real :: canl = 0.           !g            |solute mass exchanged with irrigation canal
        real :: advn = 0.           !g            |solute mass advected to/from cell
        real :: disp = 0.           !g            |solute mass dispersed to/from cell
        real :: rcti = 0.           !g            |solute mass produced by chemical reaction
        real :: rcto = 0.           !g            |solute mass consumed by chemical reaction
        real :: minl = 0.           !g            |solute mass produced by salt mineral dissolution
        real :: sorb = 0.           !g            |solute mass of sorption
      end type solute_ss_sum
      type object_solute_ss_sum
        type (solute_ss_sum), dimension (:), allocatable :: solute
      end type object_solute_ss_sum
      type (object_solute_ss_sum), dimension (:), allocatable :: gwsol_ss_sum
      
      !percolation and recharge arrays
      real, dimension (:,:), allocatable :: gwflow_percsol         !kg/ha    |solute mass leaving the soil profile
      real, dimension (:,:), allocatable :: gw_rechsol             !kg/ha    |solute mass in daily recharge (reaching water table)
      
      !grid mass for year and total (kg)
      real, dimension (:), allocatable :: sol_grid_chng_yr,sol_grid_rech_yr,sol_grid_gwsw_yr,sol_grid_swgw_yr, &
                                          sol_grid_satx_yr,sol_grid_advn_yr,sol_grid_disp_yr, &
                                          sol_grid_rcti_yr,sol_grid_rcto_yr,sol_grid_minl_yr, &
                                          sol_grid_sorb_yr,sol_grid_ppag_yr,sol_grid_ppex_yr,sol_grid_tile_yr, &
                                          sol_grid_soil_yr,sol_grid_resv_yr,sol_grid_wetl_yr,sol_grid_canl_yr, &
                                          sol_grid_fpln_yr
      real, dimension (:), allocatable :: sol_grid_chng_tt,sol_grid_rech_tt,sol_grid_gwsw_tt,sol_grid_swgw_tt, &
                                          sol_grid_satx_tt,sol_grid_advn_tt,sol_grid_disp_tt, &
                                          sol_grid_rcti_tt,sol_grid_rcto_tt,sol_grid_minl_tt, &
                                          sol_grid_sorb_tt,sol_grid_ppag_tt,sol_grid_ppex_tt,sol_grid_tile_tt, &
                                          sol_grid_soil_tt,sol_grid_resv_tt,sol_grid_wetl_tt,sol_grid_canl_tt, &
                                          sol_grid_fpln_tt
      
			!solute concentrations at observation cells
      real, dimension (:,:), allocatable :: gw_obs_solute          !         |                                 
                                          
      
      !variables specific to national model (NAM) ---------------------------------------------------------------------
      !logical  nat_model!         |
      integer :: nat_model
      integer, dimension (:), allocatable :: huc12_nhru            !         |
      integer, dimension (:), allocatable :: huc12_ncell           !         |
      integer, dimension (:), allocatable :: hrus_connected        !         |
      integer, dimension (:,:), allocatable :: huc12_hrus          !         |
      integer, dimension (:), allocatable :: cell_received         !         |
      integer, dimension (:,:), allocatable :: huc12_cells         !         |
      integer, dimension (:), allocatable :: cell_included_huc12   !         |
      real(8), dimension (:), allocatable :: huc12                 !         |
      real, dimension (:,:), allocatable :: gw_huc12_wb            !         |
      real, dimension (:,:), allocatable :: gw_huc12_wb_mo         !         |
      
      
      !reading and writing --------------------------------------------------------------------------------------------
      integer :: out_gw = 1228
      integer :: in_wet_cell = 1239
      integer :: out_gwobs = 1240
      integer :: out_gwconnect = 1241
      integer :: out_gwheads = 1242
      integer :: out_gwbal = 1243
      integer :: out_gwsw_chan = 1245
      integer :: out_gw_chan = 1246
      integer :: out_gw_rech = 1247
      integer :: out_gw_et = 1248
      integer :: out_gw_grid = 1249
      integer :: out_gw_satex = 1251
      integer :: out_gwsw = 1252
      integer :: out_lateral = 1253
      integer :: out_gw_etact = 1254
      integer :: out_gw_tile = 1255
      integer :: out_gwbal_yr = 1256
      integer :: out_gwbal_aa = 1257
      integer :: out_hyd_sep = 1258
      integer :: out_tile_cells = 1259
      integer :: out_gwconc = 1260
      integer :: out_gwtile_hru = 1267
      integer :: out_gwobs_ss = 1268
      integer :: out_gw_soil = 1270
      integer :: out_gw_res = 1271
      integer :: out_gw_wet = 1310
      integer :: out_gw_pumpag = 1272 
      integer :: out_gw_pumpex = 1273
      integer :: out_gwobs_usgs = 1274
      integer :: out_strobs = 1275
      integer :: out_huc12wb = 1276
      integer :: out_huc12wb_mo = 1312
      integer :: out_gw_pumpdef = 1277
      integer :: out_gw_canal = 1278
      integer :: out_gw_fp = 1283
      integer :: out_gw_chem = 1284
      integer :: out_hru_pump_mo = 1285
      integer :: out_hru_pump_yr = 1286
      integer :: out_hru_pump_obs = 1287
      integer :: out_head_mo = 1288
      integer :: out_head_yr = 1289
      integer :: out_conc_mo = 1290
      integer :: out_conc_yr = 1291
      !solute fluxes
      integer :: out_sol_rech = 1292
      integer :: out_sol_gwsw = 1293
      integer :: out_sol_soil = 1294
      integer :: out_sol_satx = 1295
      integer :: out_sol_ppag = 1296
      integer :: out_sol_ppex = 1297
      integer :: out_sol_tile = 1298
      integer :: out_sol_resv = 1299
      integer :: out_sol_fpln = 1300
      integer :: out_sol_canl = 1301
      integer :: out_sol_wetl = 1302
      integer :: out_sol_rcti = 1303
      integer :: out_sol_rcto = 1304
      integer :: out_sol_minl = 1305
      integer :: out_sol_sorb = 1306
      !solute mass balance (daily, yearly, average annual)
      integer :: out_solbal_dy = 7100
      integer :: out_solbal_yr = 7200
      integer :: out_solbal_aa = 7300
      !solute observation cell concentrations
      integer :: out_gwobs_sol = 1305
        
      end module gwflow_module     