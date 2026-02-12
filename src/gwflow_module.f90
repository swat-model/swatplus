      module gwflow_module
     
      implicit none

      !general variables ----------------------------------------------------------------------------------------------
      integer :: ncell = 0          !               !number of gwflow cells
      integer :: num_active = 0     !               !number of active cells
      real    :: gw_time_step = 0.  !days           !flow solution time step
      integer :: gwflag_day = 0     !               !flag for writing daily mass balance file
	  integer :: gwflag_mon = 0     !								!flag for writing monthly mass balance file
      integer :: gwflag_yr = 0      !               !flag for writing yearly mass balance file
      integer :: gwflag_aa = 0      !               !flag for writing average annual mass balance file
      integer :: conn_type = 0      !               !recharge/ET connections (1=HRU; 2=LSU)
      integer :: out_cols = 0       !               !number of columns used in writing output variables
      integer :: gw_daycount = 0    !               !simulation day counter (for pumping time series)
      real*8  :: gwflow_area = 0.   !m2							!area of the watershed occupied by active gwflow cells
	  real, dimension (:), allocatable :: bc_type !generic array for reading in values for structured grid
      
      !grid type ------------------------------------------------------------------------------------------------------
      character*15 :: grid_type                               !"structured" or "unstructured" (usg)
      integer :: grid_nrow = 0                                !number of rows in structured grid
      integer :: grid_ncol = 0                                !number of columns in structured grid
      integer, dimension (:,:), allocatable :: cell_id_usg    !usg cell number, for cell in structured grid (array)
      integer, dimension (:), allocatable :: cell_id_list     !usg cell number, for cell in structured grid (list)
      integer, dimension (:,:), allocatable :: grid_status    !cell status for structured grid
      integer, dimension (:,:), allocatable :: grid_int       !generic array for reading in values for structured grid
      real, dimension (:,:), allocatable :: grid_val          !generic array for reading in values for structured grid
      
      !groundwater state variables for each cell ----------------------------------------------------------------------
      type groundwater_state
        real :: elev = 0.			      !m            |ground surface elevation
        real :: thck = 0.           !m            |aquifer thickness
        real :: botm = 0.			      !m            |bottom (bedrock) elevation
        real :: xcrd = 0.           !m            |x coordinate of cell centroid
        real :: ycrd = 0.           !m            |y coordinate of cell centroid
        real :: area = 0.           !m2           |surface area
        real :: init = 0.			      !m            |initial groundwater head (beginning of simulation)
        real :: head = 0.			      !m            |current simulated groundwater head
        real :: hydc = 0.			      !m/day        |aquifer hydraulic conductivity
        real :: spyd = 0.			      !m3/m3        |aquifer specific yield
        real :: exdp = 0.           !m            |groundwater ET extinction depth
        integer :: stat = 0 			  !             |status (0=inactive; 1=active; 2=boundary)
				integer :: zone = 0         !             |aquifer zone
        integer :: ncon = 0         !             |number of connected cells
        integer :: tile = 0         !             |tile drainage flag (0=no tile; 1=tile is present)
        real :: hnew = 0.			      !m            |new groundwater head (at end of day)
        real :: hold = 0.			      !m            |old groundwater head (at beginning of day)
        real :: stor = 0.			      !m3           |currently available groundwater storage
        real :: vbef = 0.			      !m3           |groundwater volume at beginning of day
        real :: vaft = 0.			      !m3           |groundwater volume at end of day
        real :: hdmo = 0.			      !m            |monthly average groundwater head
        real :: hdyr = 0.			      !m            |annual average groundwater head
				real :: delx = 0.           !m            |change in groundwater position (x direction) for current time step
				real :: dely = 0.           !m            |change in groundwater position (y direction) for current time step
      end type groundwater_state
      type (groundwater_state), dimension (:), allocatable :: gw_state
      
      !groundwater transit time variables for each cell ---------------------------------------------------------------
			integer :: gw_ttime = 0
			integer :: gw_transit_num = 0
			integer, dimension (:), allocatable :: gw_transit_cells
			integer, dimension (:), allocatable :: gw_cell_chan_flag
			real, dimension (:), allocatable :: gw_cell_chan_time
			real, dimension (:), allocatable :: gw_cell_tile_time
      type groundwater_transit
        real*8 :: x = 0.  	        !m            |x coordinate
        real*8 :: y = 0.            !m            |y coordinate
				integer :: cell             !             |current cell where groundwater is located
        real :: t = 0.   			      !d            |cumulative groundwater travel time from recharge area
				real :: t_chan = 0.         !d            |time for groundwater to reach a channel
				real :: t_tile = 0.         !d            |time for groundwater to reach a tile drain
				real :: t_well = 0.         !d            |time for groundwater to reach pumping well
      end type groundwater_transit
      type (groundwater_transit), dimension (:), allocatable :: gw_transit
			
			!groundwater-channel cell groups --------------------------------------------------------------------------------
			integer :: gw_gwsw_group_flag = 0
			integer :: gw_gwsw_ngroup = 0
			integer :: gw_gwsw_max = 0
			integer, dimension (:), allocatable :: gw_gwsw_ncell
			integer, dimension (:,:), allocatable :: gw_gwsw_group
			
			!channel observation cells --------------------------------------------------------------------------------------
			integer :: gw_chan_obs_flag = 0
			integer :: gw_chan_nobs = 0
			integer, dimension (:), allocatable :: gw_chan_obs_cell
			
      !variables for HRU (and LSU) linkage to grid cells --------------------------------------------------------------
      !variables for linking HRUs to grid cells
      integer :: hru_cells_link = 0					                             !        |
      integer, dimension (:), allocatable :: hru_num_cells					 !        |
      integer, dimension (:), allocatable :: cell_num_hrus					 !        |
      integer, dimension (:,:), allocatable :: hru_cells					   !        |
      integer, dimension (:,:), allocatable :: cell_hrus					   !        |
      real, dimension (:,:), allocatable :: hru_cells_fract					 !        |
      real, dimension (:,:), allocatable :: cells_fract					     !        |
      real, dimension (:,:), allocatable :: cell_hrus_fract					 !        |
      !variables for linking LSUs (landscape units) to grid cells
      integer :: lsu_cells_link = 0					                             !        |
      integer :: in_lsu_cell = 0					                               !        |
      integer, dimension (:), allocatable :: lsu_num_cells					 !        |
      integer, dimension (:,:), allocatable :: lsu_cells					   !        |
      real, dimension (:,:), allocatable :: lsu_cells_fract					 !        |
      integer, dimension (:), allocatable :: lsus_connected					 !        |
      
      
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
				real :: pond = 0.           !m3						 |volume of recharge pond seepage added to cell
				real :: phyt = 0.           !m3            |volume of transpiration from phreatophytes
        real :: totl = 0.           !m3            |sum of groundwater inputs and outputs
      end type groundwater_ss
      type (groundwater_ss), dimension (:), allocatable :: gw_ss
      
      !sums for groundwater flow rates
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
				real :: pond = 0.           !m3            |volume of recharge pond seepage added to cell
				real :: phyt = 0.						!m3            |volume of transpiration from phreatophytes
      end type groundwater_ss_sum
      type (groundwater_ss), dimension (:), allocatable :: gw_ss_sum
      type (groundwater_ss), dimension (:), allocatable :: gw_ss_sum_mo
      
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
				real :: pond = 0.           !m3            |grid annual total recharge pond seepage
				real :: phyt = 0.           !m3            |grid annual total phreatophyte transpiration
      end type ss_grid
			type (ss_grid) :: ss_grid_mo
      type (ss_grid) :: ss_grid_yr 
      type (ss_grid) :: ss_grid_tt
      
      !rech: variables for groundwater recharge ---------------------------------------------------
      integer, dimension (:), allocatable :: gw_bound_near  !           |nearest active cell to each boundary cell
			real, dimension (:), allocatable :: gw_bound_dist     !m          |distance of nearest active cell to each boundary cell
			real, dimension (:), allocatable :: gwflow_perc       !           |
      real, dimension (:), allocatable :: gw_delay          !           |
      real, dimension (:), allocatable :: gw_rech           !           |
      real, dimension (:), allocatable :: delay             !           |
			
      !gwet: variables for groundwater evapotranspiration -----------------------------------------
      integer :: gw_et_flag = 0                                 !           |
      real, dimension (:), allocatable :: etremain          !           |
      
      !gwsw: variables for groundwater-channel exchange -------------------------------------------
      integer :: num_chancells = 0                              !           |
      integer, dimension (:), allocatable :: gw_chan_id     !           |
      integer, dimension (:), allocatable :: gw_chan_cell   !           |
      integer, dimension (:), allocatable :: gw_chan_chan   !           |
      integer, dimension (:), allocatable :: gw_chan_zone   !           |bed K zone
			integer, dimension (:), allocatable :: gw_chan_dpzn   !           |depth zone
      integer, dimension (:), allocatable :: gw_chan_ncell  !           |number of cells connected to each channel
      real, dimension (:), allocatable :: gw_chan_len       !           |
      real, dimension (:), allocatable :: gw_chan_elev      !           |
      real, dimension (:), allocatable :: gw_chan_K         !           |
      real, dimension (:), allocatable :: gw_chan_thick     !           |
      real :: gw_bed_change = 0.                                 !           |
			integer :: gw_chan_dep_flag = 0
			integer :: gw_chan_ndpzn = 0                              !						|number of channel depth zones
			real, dimension (:), allocatable :: gw_chan_dep       !m          |specified daily channel depths
      !channel-cell connection
      type cell_channel_info
        integer :: ncon = 0                                     !           |number of cells connected to the channel
        integer, allocatable :: cells(:)                    !           |cells connected to the channel
        real, allocatable :: leng(:)                        !m          |length of channel in the cell
        real, allocatable :: elev(:)                        !m          |elevation of channel bed in the cell
        real, allocatable :: hydc(:)                        !m          |hydraulic conductivity of channel bed in the cell
        real, allocatable :: thck(:)                        !m          |thickness of channel bed in the cell
				integer, allocatable :: dpzn(:)                      !           |channel depth zone (optional) 
      endtype cell_channel_info
      type (cell_channel_info), dimension(:), allocatable :: gw_chan_info
      
      !satx: variables for saturated excess flow --------------------------------------------------
      integer :: gw_satx_flag = 0                               !           |
      integer :: satx_count = 0                                 !           |for each day: number of cells that are saturated
      type satx_channel_info
        integer :: ncon = 0                                     !           |number of cells connected to the channel
        integer, allocatable :: cells (:)                   !           |cells connected to the channel
      endtype satx_channel_info
      type (satx_channel_info), dimension(:), allocatable :: gw_satx_info
      
      !soil: variables for gw-->soil exchange -----------------------------------------------------
      integer :: gw_soil_flag = 0                               !           |
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
      logical :: hru_pump_flag                              !           |
      integer :: in_hru_pump_obs = 0                            !           |
      integer :: num_hru_pump_obs = 0                           !           |
      integer, dimension (:), allocatable :: hru_pump_ids   !           |
      real, dimension (:), allocatable :: hru_pump_obs      !           |
      
      !ppex: variables for specified groundwater pumping ------------------------------------------
      integer :: gw_pumpex_flag = 0                                    !           |
      integer :: gw_npumpex = 0                                        !           |
      integer, dimension (:), allocatable :: gw_pumpex_cell        !           |
      integer, dimension (:), allocatable :: gw_pumpex_nperiods    !           |
      integer, dimension (:,:,:), allocatable :: gw_pumpex_dates   !           |
      real, dimension (:,:), allocatable :: gw_pumpex_rates        !           |
      
      !tile: variables for tile drainage outflow --------------------------------------------------
      integer :: gw_tile_flag = 0                                      !           |
      integer :: gw_tile_group_flag = 0                                !           |
      integer :: gw_tile_num_group = 0                                 !           |
      integer :: num_tile_cells(50) = 0                                !           |
			real, dimension (:), allocatable :: gw_tile_depth            !           |
			real, dimension (:), allocatable :: gw_tile_drain_area       !           |
			real, dimension (:), allocatable :: gw_tile_K                                         !           |
      integer, dimension (:,:), allocatable :: gw_cell_tile        !           |
      integer, dimension (:), allocatable :: gw_tilecell_chancell  !           |
      integer, dimension (:,:), allocatable :: gw_tile_groups      !           |
      !channel-tile connection
      type tile_channel_info
        integer :: ncon = 0                                            !     |number of cells connected to the channel
        integer, allocatable :: cells(:)                           !     |cells connected to the channel
      endtype tile_channel_info
      type (tile_channel_info), dimension(:), allocatable :: gw_tile_info
      
      !resv: variables for groundwater-reservoir exchange -----------------------------------------
      integer :: gw_res_flag = 0                !     |
      real    :: res_thick,res_K = 0.            !     |
      integer :: num_res_cells = 0              !     |
      !cell-reservoir connection
      type cell_reservoir_info
        integer :: ncon = 0                     !     |number of cells connected to the channel
        integer, allocatable :: cells(:)    !     |cells connected to the channel
        real, allocatable :: elev(:)        !m    |elevation of channel bed in the cell
        real, allocatable :: hydc(:)        !m    |hydraulic conductivity of channel bed in the cell
        real, allocatable :: thck(:)        !m    |thickness of channel bed in the cell
      endtype cell_reservoir_info
      type (cell_reservoir_info), dimension(:), allocatable :: gw_resv_info
      
      !wetl: variables for groundwater-wetland exchange -------------------------------------------
      integer :: gw_wet_flag = 0                             !     |
      real, dimension (:), allocatable :: wet_thick      !     |
      
      !fpln: variables for groundwater-floodplain exchange ----------------------------------------
      integer :: gw_fp_flag = 0                                                 !     |
      integer :: in_fp_cell = 0
      integer :: gw_fp_ncells = 0                                    !     |
      integer, dimension (:), allocatable :: gw_fp_cellid,gw_fp_chanid      !     |
      real, dimension (:), allocatable :: gw_fp_K,gw_fp_area                !     |
      integer, dimension (:), allocatable :: flood_freq                     !     |
      !channel-cell connection
      type cell_floodplain_info
        integer :: ncon = 0                                  !     |number of cells connected to the channel
        integer, allocatable :: cells(:)                 !     |cells connected to the channel
        real, allocatable :: hydc(:)                     !m    |hydraulic conductivity of floodplain bottom in the cell
        real, allocatable :: area(:)                     !m    |floodplain area in connection with cell
        integer, allocatable :: mtch(:)                  !     |matching channel cell
      endtype cell_floodplain_info
      type (cell_floodplain_info), dimension(:), allocatable :: gw_fpln_info
      
      !canl: variables for groundwater-canal exchange ---------------------------------------------
      integer :: gw_canal_flag = 0                           !     |
      integer :: gw_ncanal = 0
			integer :: gw_canal_ncells = 0                         !     |
      !canal-channel connection
      type canal_chan_info
        integer :: ncanal = 0                                !     |number of canals connected to the channel
        integer, allocatable :: canals(:)                !     |canals connected to the channel
        real, allocatable :: wdth(:)                     !m    |canal width
        real, allocatable :: dpth(:)                     !m    |canal depth
        real, allocatable :: thck(:)                     !m    |canal thickness
				real, allocatable :: hydc(:)                     !m/d	 |hydraulic conductivity of canal bed sediments
        integer, allocatable :: dayb(:)                  !     |beginning day of active canal
        integer, allocatable :: daye(:)                  !     |ending day of active canal
      endtype canal_chan_info
      type (canal_chan_info), dimension(:), allocatable :: gw_chan_canl_info
      !canal-cell connection
      type cell_canal_info
        integer :: ncon = 0                                  !     |number of cells connected to the canal
        integer, allocatable :: cells(:)                 !     |cells connected to the canal
        real, allocatable :: leng(:)                     !m    |length of canal in the cell
        real, allocatable :: elev(:)                     !m    |stage of canal in the cell
        real, allocatable :: hydc(:)                     !m    |hydraulic conductivity of canal bed in the cell
      endtype cell_canal_info
      type (cell_canal_info), dimension(:), allocatable :: gw_canl_info 
      !canal-cell connection for canals that receive water outside of the model domain
      type cell_canal_out_info
        integer :: cell_id = 0
        real :: wdth = 0.
        real :: dpth = 0.
        real :: thck = 0.
        real :: leng = 0.
        real :: elev = 0.
        real :: hydc = 0.
        integer :: dayb = 0
        integer :: daye = 0
      end type cell_canal_out_info
      type (cell_canal_out_info), dimension (:), allocatable :: gw_canl_out_info
      integer :: gw_canal_ncells_out										 !     |number of cells connected to canals that receive outside water
      real, allocatable :: canal_out_info(:,:)           !     |characteristics for canals that receive outside water
      real, allocatable :: canal_out_conc(:)             !     |solute concentration in canals that receive outside water
      !canal-cell connection for canals that receive water from a point source diversion
      type cell_canal_div_info
        integer :: cell_id = 0
				integer :: canal_id = 0
        real :: leng = 0.
        real :: elev = 0.
      end type cell_canal_div_info
      type (cell_canal_div_info), dimension (:), allocatable :: gw_canl_div_cell
      integer :: gw_canal_ncells_div										 !     |number of cells connected to canals that receive from diversions
			!canal characteristics
			type canal_info
			  integer :: canal_id = 0
				integer :: divr = 0
				real :: width = 0.
				real :: depth = 0.
				real :: thick = 0.
				real :: bed_K = 0.
				real :: frc_ret = 0. 																 !     |fraction of diverted volume that should not be used
				real :: div = 0.                                      !m3   |volume of water diverted from channel source
				real :: stor = 0.                                     !m3	 |current volume of canal water
				real :: div_ret = 0.                                  !m3   |volume of diversion water not used (return)
				real :: out_seep = 0.                                 !m3   |volume of canal water seeped to aquifer
				real :: out_pond = 0.                                 !m3   |volume of canal water routed to recharge pond
				real :: out_irrg = 0.                                 !m3   |volume of canal water applied to fields as irrigation
				integer :: nhru = 0                                  !     |number of HRUs that receive irrigation water from diversion
				integer, allocatable :: hrus(:)                  !     |HRUs that receive irrigation water from diversion
				real, allocatable :: hru_ro(:)                   !     |runoff fraction for each HRU
			end type canal_info
			type (canal_info), dimension (:), allocatable :: gw_canl_div_info
			
			!pond: variables for recharge pond seepage --------------------------------------------------
			integer :: gw_pond_flag = 0                            !     |flag = 0 (off) or 1 (on)
			integer :: gw_npond = 0                                !     |number of recharge ponds in the model domain
			!pond features
			type cell_pond_info
			  integer :: id = 0.                               !     |recharge pond id
				integer :: chan = 0                              !     |channel which provides water to the recharge pond
				integer :: canal = 0                             !     |canal which provides water to the recharge pond
				integer :: unl = 0                               !     |flag for outside source (1 = outside source)
				integer :: ncell = 0                             !     |number of cells connected to the recharge pond
				integer :: wsta = 0                              !     |weather station id
				real :: area = 0.                                !m2   |recharge pond surface area
				real :: bed_k = 0.															 !m/d  |hydraulic conductivity of the pond bed sediments
				real :: evap_co = 0.6                            !     |pond evaporation coefficient
				real :: stor = 0.                                !m3   |current daily volume of the recharge pond
				real :: seep = 0.                                !m3   |current daily seepage from the pond to the aquifer
				real :: div  = 0.                                !m3   |current daily specified diversion volume
				real :: div_uns = 0.                             !m3   |unsatisfied diversion volume
				real :: evap = 0.                                !m3   |current daily volume of evaporation from the recharge pond
				integer :: dy_start                              !     |year when recharge pond begins operation
				integer, allocatable :: cells(:)                 !     |cells connected to the recharge pond
        real, allocatable :: conn_area(:)                !m2   |connection area between recharge pond and cell	
				real, allocatable :: sol_mass(:)                 !kg   |solute mass in the pond water
				real, allocatable :: sol_conc(:)                 !g/m3 |solute concentration in the pond water
				real, allocatable :: unl_conc(:)                 !g/m3 |solute concentrations for an outside water source
			end type cell_pond_info
			type (cell_pond_info), dimension (:), allocatable :: gw_pond_info

			!phyt: variables for phyreatophyte transpiration --------------------------------------------
			integer :: gw_phyt_flag                            !     |flag = 0 (off) or 1 (on)
			integer :: gw_phyt_ncells                          !     |number of cells with phreatophytes
			integer :: gw_phyt_npts
			!phreatophyte features
			integer, allocatable :: gw_phyt_ids(:)             !     |ids of cells with phreatophytes
			real, allocatable :: gw_phyt_area(:)               !m2   |area of each cell that contains phreatophytes
			real, allocatable :: gw_phyt_dep(:)                !m    |depth below ground surface; used to define ET-rate relationship
			real, allocatable :: gw_phyt_rate(:)               !m/day|rate of transpiration at corresponding depth
			
			!tvh: variables for time-varying boundary conditions ----------------------------------------
			integer :: gw_tvh_flag = 0                             !     |flag = 0 (off) or 1 (on)
			integer :: gw_ntvh = 0                                 !     |number of time-varying boundary cells
			integer, allocatable :: gw_tvh_ids(:)              !     |boundary cell IDs
			real, allocatable :: gw_tvh_vals(:,:)              !     |boundary cell head values for each year
			
			
      !general: nearest channel for each grid cell
      real, allocatable :: cell_channel(:)               !     |nearest channel for each grid cell 
      
      !variables for writing out groundwater balance for selected groups of cells
      integer :: gw_group_flag = 0                       !     |flag to make active
      integer :: gw_wb_grp_num = 0                           !     |number of water balance groups
      integer, allocatable :: gw_wb_grp_ncell(:)         !     |number of cells in each group
      integer, allocatable :: gw_wb_grp_cells(:,:)       !     |cell IDs in each water balance group

      !variables for writing head and concentration values ------------------------------------------------------------
      integer :: gw_num_output = 0                                  !     |
      integer :: gw_output_index = 0                                !     |
      integer, dimension (:), allocatable :: gw_output_yr       !     |
      integer, dimension (:), allocatable :: gw_output_day      !     |
      
      
      !variables for observation wells --------------------------------------------------------------------------------
      integer :: gw_num_obs_wells = 0                               !     |
      integer, dimension (:), allocatable :: gw_obs_cells       !     |
      real, dimension (:), allocatable :: gw_obs_head           !     |
      integer :: gw_cell_obs_ss = 0                                 !     |
      real, dimension (:), allocatable :: gw_cell_obs_ss_vals   !     |
      logical :: usgs_obs                                       !     |
      real(8), dimension (:), allocatable :: usgs_id            !     |
      real, dimension (:,:), allocatable :: usgs_head_vals      !     |
      real, dimension (:,:), allocatable :: gw_obs_head_annual  !     |
      real, dimension (:,:), allocatable :: sim_head_vals       !     |
      real, dimension (:,:), allocatable :: gw_obs_sat_annual   !     |
      real, dimension (:,:), allocatable :: sim_sat_vals        !     |
      
      
      !variables for streamflow testing and output --------------------------------------------------------------------
      logical stream_obs                                        !     |
      integer :: gw_num_obs_chan = 0                            !     |
      integer :: num_months = 0                                 !     |
      integer :: sim_month = 0                                  !     |
      integer, dimension (:), allocatable :: obs_channels       !     |
      real, dimension (:,:), allocatable :: stream_nse          !     |
      real, dimension (:,:), allocatable :: stream_nse1         !     |
      real, dimension (:,:), allocatable :: stream_nnse         !     |
      real, dimension (:,:), allocatable :: stream_kg           !     |
      real, dimension (:,:), allocatable :: stream_pbias        !     |
      real, dimension (:,:), allocatable :: obs_flow_vals       !     |
      real, dimension (:,:), allocatable :: sim_flow_vals       !     |
      integer :: gw_flow_cal_yrs = 0                            !     |
      integer :: gw_flow_cal = 0                                !     |
      

      !variables for hydrograph separation ----------------------------------------------------------------------------
      real, dimension (:,:), allocatable :: chan_hyd_sep
      integer, dimension (:), allocatable :: hydsep_flag
      
      
      !variables for groundwater head transport -----------------------------------------------------------------------
      integer :: gw_heat_flag = 0         !             |flag (0 or 1) 1 = heat transport is simulated
      real :: gw_rho = 1000.              !kg/m3        |density of groundwater
      real :: gw_cp = 4182                !J/(kg C)     |specific heat of groundwater
      real, dimension (:), allocatable :: gw_rechheat   !J: heat in daily recharge (reaching water table)
      real, dimension (:), allocatable :: gw_obs_temp   !deg C: temperature in observation cells 
			real, dimension (:), allocatable :: heat_cell     !J: heat storage for current day (before heat change loop)
      type groundwater_heat_state
        real :: stor = 0.           !Joule        |current heat stored in groundwater
				real :: thmc = 0.           !J/(d m K)    !thermal conductivity
        real :: temp = 0.			      !C            |current groundwater temperature
        real :: tnew = 0.			      !C            |new groundwater temperature (at end of day)
        real :: told = 0.			      !C            |old groundwater temperature (at beginning of day)
        real :: hbef = 0.			      !Joule        |groundwater heat at beginning of day
        real :: haft = 0.			      !Joule        |groundwater heat at end of day
        real :: tpmo = 0.			      !C            |monthly average groundwater temperature
        real :: tpyr = 0.			      !C            |annual average groundwater temperature
      end type groundwater_heat_state
      type (groundwater_heat_state), dimension (:), allocatable :: gwheat_state
      
      !daily heat fluxes
      type groundwater_ss_heat
        real :: rech = 0.           !Joule         |heat of recharge water added to cell
        real :: gwet = 0.           !Joule         |heat of ET water removed from cell
        real :: gwsw = 0.           !Joule         |heat of groundwater discharging to channels
        real :: swgw = 0.           !Joule         |heat of channel water seeping to groundwater via channel bed
        real :: satx = 0.           !Joule         |heat of groundwater discharging to channels via saturation excess flow
        real :: soil = 0.           !Joule         |heat of groundwater added to the soil profile
        real :: latl = 0.           !Joule         |heat of groundwater flowing between adjacent cells
        real :: disp = 0.           !Joule         |heat of groundwater in dispersion
        real :: bndr = 0.           !Joule         |heat of groundwater exchanged across watershed boundary
        real :: ppag = 0.           !Joule         |heat of groundwater removed via agricultural pumping (irrigation)
        real :: ppex = 0.           !Joule         |heat of groundwater removed via external pumping (water lost from system)
        real :: tile = 0.           !Joule         |heat of groundwater removed via tile drainage outflow
        real :: resv = 0.           !Joule         |heat of groundwater exchanged with reservoirs
        real :: wetl = 0.           !Joule         |heat of groundwater exchanged with wetlands
        real :: fpln = 0.           !Joule         |heat of groundwater exchanged with floodplains
        real :: canl = 0.           !Joule         |heat of groundwater exchanged with canals
		real :: pond = 0.           !Joule				 |heat of groundwater in recharge pond seepage
        real :: totl = 0.           !Joule         |sum of groundwater heat inputs and outputs
      end type groundwater_ss_heat
      type (groundwater_ss_heat), dimension (:), allocatable :: gw_heat_ss
      
      !sums for groundwater heat fluxes
      type groundwater_ss_heat_sum
        real :: rech = 0.           !Joule         |heat of recharge water added to cell
        real :: gwet = 0.           !Joule         |heat of ET water removed from cell
        real :: gwsw = 0.           !Joule         |heat of groundwater discharging to channels
        real :: swgw = 0.           !Joule         |heat of channel water seeping to groundwater via channel bed
        real :: satx = 0.           !Joule         |heat of groundwater discharging to channels via saturation excess flow
        real :: soil = 0.           !Joule         |heat of groundwater added to the soil profile
        real :: latl = 0.           !Joule         |heat of groundwater flowing between adjacent cells
        real :: disp = 0.           !Joule         |heat of groundwater in dispersion
        real :: bndr = 0.           !Joule         |heat of groundwater exchanged across watershed boundary
        real :: ppag = 0.           !Joule         |heat of groundwater removed via agricultural pumping (irrigation)
        real :: ppex = 0.           !Joule         |heat of groundwater removed via external pumping (water lost from system)
        real :: tile = 0.           !Joule         |heat of groundwater removed via tile drainage outflow
        real :: resv = 0.           !Joule         |heat of groundwater exchanged with reservoirs
        real :: wetl = 0.           !Joule         |heat of groundwater exchanged with wetlands
        real :: fpln = 0.           !Joule         |heat of groundwater exchanged with floodplains
        real :: canl = 0.           !Joule         |heat of groundwater exchanged with canals
		real :: pond = 0.           !Joule         |heat of recharge pond seepage water
      end type groundwater_ss_heat_sum
      type (groundwater_ss_heat_sum), dimension (:), allocatable :: gw_heat_ss_sum
      
      !grid heat flux totals (million joules) for the year
      type ss_heat_grid
        real :: chng = 0.           !MJ            |grid annual total change in groundwater heat storage        
        real :: rech = 0.           !MJ            |grid annual total recharge heat 
        real :: gwet = 0.           !MJ            |grid annual total groundwater ET heat  
        real :: gwsw = 0.           !MJ            |grid annual total groundwater heat discharing to channels
        real :: swgw = 0.           !MJ            |grid annual total channel water heat seeping to groundwater via channel bed
        real :: satx = 0.           !MJ            |grid annual total groundwater heat discharging to channels via saturation excess flow
        real :: soil = 0.           !MJ            |grid annual total groundwater heat added to the soil profile
        real :: latl = 0.           !MJ            |grid annual total groundwater heat transported between adjacent cells
        real :: disp = 0.           !MJ            |grid annual total groundwater heat transported via dispersion
        real :: bndr = 0.           !MJ            |grid annual total groundwater heat exchanged across watershed boundary
        real :: ppag = 0.           !MJ            |grid annual total groundwater heat removed via agricultural pumping (irrigation)
        real :: ppex = 0.           !MJ            |grid annual total groundwater heat removed via external pumping (water lost from system)
        real :: tile = 0.           !MJ            |grid annual total groundwater heat removed via tile drainage outflow
        real :: resv = 0.           !MJ            |grid annual total groundwater heat exchanged with reservoirs
        real :: wetl = 0.           !MJ            |grid annual total groundwater heat exchanged with wetlands
        real :: fpln = 0.           !MJ            |grid annual total groundwater heat exchanged with floodplains
        real :: canl = 0.           !MJ            |grid annual total groundwater heat exchanged with canals
		real :: pond = 0.           !MJ            |grid annual total heat in recharge pond seepage water
      end type ss_heat_grid
      type (ss_heat_grid) :: ss_heat_grid_yr 
      type (ss_heat_grid) :: ss_heat_grid_tt
      
      
      !variables for groundwater solute transport ---------------------------------------------------------------------
      
      !general solute variables
      integer :: gw_solute_flag = 0                               !    |main flag
      integer :: gw_nsolute = 0                                   !    |number of solutes
      integer :: num_ts_transport = 0                             !    |number of transport time steps per day
      real ::    gw_long_disp = 0.                                 !m   |aquifer longitudinal dispersivity
      integer :: gwsol_salt = 0                                   !    |flag for simulating salt ion groundwater transport (so4,ca,mg,na,k,cl,co3,hco3)
      integer :: gwsol_cons = 0																		!    |flag for simulating constituent groundwater transport (seo4,seo3,boron)
      integer :: gwsol_minl = 0                                   !    |flag for simulating salt mineral precipitation-dissolution
      integer :: gw_nminl = 0                                     !    |number of salt minerals (set to 5)
       
      !main attributes of solutes
      character (len=16) :: gwsol_nm(100)
      real :: gwsol_rctn(100)
      real :: gwsol_sorb(100)
      
      !solute cell state variables
      type solute_state
        real :: mass = 0.			      !g            |solute mass in groundwater
        real :: init = 0.           !g/m3         |solute concentration in groundwater at beginning of simulation
        real :: conc = 0.           !g/m3         |solute concentration in groundwater      
        real :: cnew = 0.           !g/m3				  |new concentrations at end of time step
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
				integer :: bed_flag = 0     !             |flag (0,1) for presence of shale in bedrock
				real :: bed_sse = 0.				!             |sulfure:se ratio in bedrock shale
				real :: bed_o2a = 0.        !1/day        |o2 oxidation rate in presence of bedrock shale
				real :: bed_no3a = 0.       !1/day        |no3 oxidation rate in presence of bedrock shale
				integer :: ripar            !             |flag: 1=cell in riparian area; 0=cell not in riparian area
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
		real :: pond = 0.           !g            |solute mass in recharge pond seepage water
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
		real :: pond = 0.           !g            |solute mass in recharge pond seepag water
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
			type (object_solute_ss_sum), dimension (:), allocatable :: gwsol_ss_sum_mo
      
      !percolation and recharge arrays
      real, dimension (:,:), allocatable :: gwflow_percsol         !kg/ha    |solute mass leaving the soil profile
      real, dimension (:,:), allocatable :: gw_rechsol             !kg/ha    |solute mass in daily recharge (reaching water table)
      
      !grid mass for month, year, and total (kg)
	  real, dimension (:), allocatable :: sol_grid_chng_mo,sol_grid_rech_mo,sol_grid_gwsw_mo,sol_grid_swgw_mo, &
                                          sol_grid_satx_mo,sol_grid_advn_mo,sol_grid_disp_mo, &
                                          sol_grid_rcti_mo,sol_grid_rcto_mo,sol_grid_minl_mo, &
                                          sol_grid_sorb_mo,sol_grid_ppag_mo,sol_grid_ppex_mo,sol_grid_tile_mo, &
                                          sol_grid_soil_mo,sol_grid_resv_mo,sol_grid_wetl_mo,sol_grid_canl_mo, &
                                          sol_grid_fpln_mo,sol_grid_pond_mo
      real, dimension (:), allocatable :: sol_grid_chng_yr,sol_grid_rech_yr,sol_grid_gwsw_yr,sol_grid_swgw_yr, &
                                          sol_grid_satx_yr,sol_grid_advn_yr,sol_grid_disp_yr, &
                                          sol_grid_rcti_yr,sol_grid_rcto_yr,sol_grid_minl_yr, &
                                          sol_grid_sorb_yr,sol_grid_ppag_yr,sol_grid_ppex_yr,sol_grid_tile_yr, &
                                          sol_grid_soil_yr,sol_grid_resv_yr,sol_grid_wetl_yr,sol_grid_canl_yr, &
                                          sol_grid_fpln_yr,sol_grid_pond_yr
      real, dimension (:), allocatable :: sol_grid_chng_tt,sol_grid_rech_tt,sol_grid_gwsw_tt,sol_grid_swgw_tt, &
                                          sol_grid_satx_tt,sol_grid_advn_tt,sol_grid_disp_tt, &
                                          sol_grid_rcti_tt,sol_grid_rcto_tt,sol_grid_minl_tt, &
                                          sol_grid_sorb_tt,sol_grid_ppag_tt,sol_grid_ppex_tt,sol_grid_tile_tt, &
                                          sol_grid_soil_tt,sol_grid_resv_tt,sol_grid_wetl_tt,sol_grid_canl_tt, &
                                          sol_grid_fpln_tt,sol_grid_pond_tt
      
			!solute concentrations at observation cells
      real, dimension (:,:), allocatable :: gw_obs_solute          !         |                                 
                                          
      
      !variables specific to national model (NAM) ---------------------------------------------------------------------
      logical  nat_model                                           !         |
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
	  integer :: in_ponds = 1219
      integer :: out_gwobs = 1240
      integer :: out_gwconnect = 1241
      integer :: out_gwheads = 1242
      integer :: out_gwbal = 1243
      integer :: out_gwsw_chan = 1245
      integer :: out_gw_chan = 1246
      integer :: out_gw_grid = 1249
      integer :: out_gwsw = 1252
      integer :: out_lateral = 1253
      integer :: out_gw_etact = 1254
      integer :: out_gwbal_mon = 1255
      integer :: out_gwbal_yr = 1256
      integer :: out_gwbal_aa = 1257
      integer :: out_hyd_sep = 1258
      integer :: out_tile_cells = 1259
      integer :: out_gwconc = 1260
      integer :: out_gwtile_hru = 1267
      integer :: out_gwobs_ss = 1268
      integer :: out_gwobs_usgs = 1274
      integer :: out_strobs = 1275
      integer :: out_huc12wb = 1276
      integer :: out_huc12wb_mo = 1309
      integer :: out_gw_pumpdef = 1277
      integer :: out_hru_pump_mo = 1285
      integer :: out_hru_pump_yr = 1286
      integer :: out_hru_pump_obs = 1287
      integer :: out_head_mo = 1288
      integer :: out_head_yr = 1289
      integer :: out_conc_mo = 1290
      integer :: out_conc_yr = 1291
      integer :: out_gwbal_grp = 1601
      !solute fluxes (yearly)
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
      integer :: out_sol_pond = 1307
      !solute fluxes (monthly)
      integer :: out_sol_rech_mo = 1430
      integer :: out_sol_gwsw_mo = 1431
      integer :: out_sol_soil_mo = 1432
      integer :: out_sol_satx_mo = 1433
      integer :: out_sol_ppag_mo = 1434
      integer :: out_sol_ppex_mo = 1435
      integer :: out_sol_tile_mo = 1436
      integer :: out_sol_resv_mo = 1437
      integer :: out_sol_fpln_mo = 1438
      integer :: out_sol_canl_mo = 1439
      integer :: out_sol_wetl_mo = 1440
      integer :: out_sol_rcti_mo = 1441
      integer :: out_sol_rcto_mo = 1442
      integer :: out_sol_minl_mo = 1443
      integer :: out_sol_sorb_mo = 1444
      integer :: out_sol_pond_mo = 1445
      !solute mass balance (daily, yearly, average annual)
      integer :: out_solbal_dy = 7100
      integer :: out_solbal_mo = 7200
      integer :: out_solbal_yr = 7300
      integer :: out_solbal_aa = 7400
      !solute observation cell concentrations
      integer :: out_gwobs_sol = 1308
      !annual groundwater fluxes
      integer :: out_gw_rech = 1310
      integer :: out_gw_gwet = 1311
      integer :: out_gw_gwsw = 1312
      integer :: out_gw_soil = 1313
      integer :: out_gw_satx = 1314
      integer :: out_gw_ppag = 1315
      integer :: out_gw_ppex = 1316
      integer :: out_gw_tile = 1317
      integer :: out_gw_resv = 1318
      integer :: out_gw_wetl = 1319
      integer :: out_gw_fpln = 1320
      integer :: out_gw_canl = 1321
      integer :: out_gw_pond = 1322
      integer :: out_pond_bal = 1323
      integer :: out_pond_sol = 1324
      integer :: out_gw_phyt = 1325
      integer :: out_pond_mass = 1326
      integer :: out_pond_conc = 1327
      integer :: out_canal_bal = 1328
      integer :: out_canal_sol = 1329
      !monthly groundwater fluxes
      integer :: out_gw_rech_mo = 1330
      integer :: out_gw_gwet_mo = 1331
      integer :: out_gw_gwsw_mo = 1332
      integer :: out_gw_soil_mo = 1333
      integer :: out_gw_satx_mo = 1334
      integer :: out_gw_ppag_mo = 1335
      integer :: out_gw_ppex_mo = 1336
      integer :: out_gw_tile_mo = 1337
      integer :: out_gw_resv_mo = 1338
      integer :: out_gw_wetl_mo = 1339
      integer :: out_gw_fpln_mo = 1340
      integer :: out_gw_canl_mo = 1341
      integer :: out_gw_pond_mo = 1342
      integer :: out_gw_phyt_mo = 1343
      !groundwater heat fluxes - balance files
      integer :: out_heatbal_dy = 1360
      integer :: out_heatbal_yr = 1361
      integer :: out_heatbal_aa = 1362
      !groundwater temperature output files
      integer :: out_gwtemps = 1370
      integer :: out_temp_mo = 1371
      integer :: out_temp_yr = 1372
      integer :: out_gwobs_temp = 1373
      !annual groundwater heat fluxes
      integer :: out_heat_rech = 1380
      integer :: out_heat_gwet = 1381
      integer :: out_heat_gwsw = 1382
      integer :: out_heat_satx = 1383
      integer :: out_heat_soil = 1384
      integer :: out_heat_tile = 1385
      integer :: out_heat_ppag = 1386
      integer :: out_heat_ppex = 1387
      integer :: out_heat_resv = 1388
      integer :: out_heat_wetl = 1389
      integer :: out_heat_fpln = 1390
      integer :: out_heat_canl = 1391
      integer :: out_heat_pond = 1392
      !groundwater transit time
      integer :: out_gw_transit = 1410
      integer :: out_gw_transit_chan = 1411
      integer :: out_gw_transit_tile = 1412
      !gwsw cell groups
      integer :: out_gwsw_groups = 1413
      !channel obs cells
      integer :: out_gwsw_chanobs_flow = 1414
      integer :: out_gwsw_chanobs_no3 = 1415
			
      end module gwflow_module
      