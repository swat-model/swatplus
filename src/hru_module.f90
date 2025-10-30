      module hru_module
    
      implicit none
      
      integer :: isep = 0            !          |
      integer :: ilu = 0             !          | 
      integer :: ulu = 0             !          |
      integer :: iwgen = 0           !          |
      character (len=1) :: timest = "" !          |
     
      type uptake_parameters
       real :: water_dis = 10.        !               |the uptake distribution for water is hardwired
       real :: water_norm = 0.        !none           |water uptake normalization parameter 
       real :: n_norm = 0.            !none           |nitrogen uptake normalization parameter 
       real :: p_norm = 0.            !none           |phosphorus uptake normalization parameter
      end type uptake_parameters
      type (uptake_parameters)  :: uptake

      type irrigation_sources
        integer :: flag = 0   !0= don't irrigate, 1=irrigate
        integer, dimension(:), allocatable :: chan
        integer, dimension(:), allocatable :: res
        integer, dimension(:), allocatable :: pond
        integer, dimension(:), allocatable :: shal
        integer, dimension(:), allocatable :: deep
      end type irrigation_sources
      
      type topography
           character(len=40) :: name = ""
           real :: elev = 0.         !!               |m             |elevation of HRU
           real :: slope = 0.        !! hru_slp(:)    |m/m           |average slope steepness in HRU
           real :: slope_len = 0.    !! slsubbsn(:)   |m             |average slope length for erosion
           real :: dr_den = 0.       !!               |km/km2        |drainage density
           real :: lat_len = 0.      !! slsoil(:)     |m             |slope length for lateral subsurface flow
           real :: dis_stream = 0.   !! dis_stream(:) | m            |average distance to stream
           real :: dep_co = 1.       !!               |              |deposition coefficient
           integer :: field_db = 0   !!               |              |pointer to field.fld
           integer :: channel_db=0   !!               |              |pointer to channel.dat
      end type topography
      
      type field
           character(len=40) :: name = "default"
           real :: length = 500. !!               |m             |field length for wind erosion
           real :: wid = 100.    !!               |m             |field width for wind erosion
           real :: ang = 30.     !!               |deg           |field angle for wind erosion
      end type field
      
      type hydrology
           character(len=40) :: name = ""
           real :: lat_ttime = 0.   !! lat_ttime(:)  |days          |days of lateral soil flow across the hillslope
           real :: lat_sed = 0.     !! lat_sed(:)    |g/L           |sediment concentration in lateral flow
           real :: canmx = 0.       !! canmx(:)      |mm H2O        |maximum canopy storage
           real :: esco = 0.        !! esco(:)       |none          |soil evaporation compensation factor
           real :: epco = 0.        !! epco(:)       |none          |plant water uptake compensation factor (0-1)
           real :: erorgn = 0.      !! erorgn(:)     |none          |organic N enrichment ratio, if left blank
                                    !!                              |the model will calculate for every event
           real :: erorgp = 0.      !! erorgp(:)     |none          |organic P enrichment ratio, if left blank
                                    !!                              |the model will calculate for every event
           real :: cn3_swf = 0.     !!               |none          |curve number adjustment factor - sw at cn3
           real :: biomix = 0.      !! biomix(:)     |none          |biological mixing efficiency.
                                    !!                              |Mixing of soil due to activity of earthworms
                                    !!                              |and other soil biota. Mixing is performed at
                                    !!                              |the end of every calendar year.
           real :: perco = 0.       !!               |0-1           |percolation coefficient - linear adjustment to daily perc
           real :: lat_orgn = 0.
           real :: lat_orgp = 0.
           real :: pet_co  = 1.0
           real :: latq_co = 0.3    !!               |              |lateral soil flow coefficient - linear adjustment to daily lat flow
           real :: perco_lim = 1.   !!               |              |percolation coefficient-limits perc from bottom layer
      end type hydrology
      
      type snow_parameters
         character (len=40) :: name = ""
         real :: falltmp = 0.     !deg C         |snowfall temp
         real :: melttmp = 0.5    !deg C         |snow melt base temp 
         real :: meltmx = 4.5     !mm/deg C/day  |Max melt rate for snow during year (June 21)
         real :: meltmn = 0.5     !mm/deg C/day  |Min melt rate for snow during year (Dec 21)
         real :: timp = 0.8       !none          |snow pack temp lag factor (0-1)
         real :: covmx = 25.0     !mm H20        |snow water content at full ground cover
         real :: cov50 = 0.5      !none          |frac of covmx at 50% snow cover
         real :: init_mm = 0.     !mm H20        |initial snow water content at start of simulation
      end type snow_parameters
      type (snow_parameters), dimension (:), allocatable :: snodb
      
      type subsurface_drainage_parameters
        character(len=40) :: name = "null"
        real :: depth = 0.    !! |mm            |depth of drain tube from the soil surface
        real :: time = 0.     !! |hrs           |time to drain soil to field capacity
        real :: lag = 0.      !! |hours         |drain tile lag time
        real :: radius = 0.   !! |mm            |effective radius of drains
        real :: dist = 0.     !! |mm            |distance between two drain tubes or tiles
        real :: drain_co = 0. !! |mm/day        |drainage coefficient
        real :: pumpcap = 0.  !! |mm/hr         |pump capacity 
        real :: latksat = 0.  !! !na            |multiplication factor to determine lat sat hyd conductivity for profile
      end type subsurface_drainage_parameters
      type (subsurface_drainage_parameters), dimension (:), allocatable :: sdr
      
      type saturated_buffer_parameters
        character(len=40) :: name = "null"
        integer :: hru_src = 0                   !!     |source of tile inflow
        real :: frac_src = 0.                    !!     |fration of source hru contributing to tile flow
        character(len=40) :: flocon_dtbl = " "   !!     |decision table to control flow into buffer hru
        integer :: hru_rcv = 0                   !!     |receiving (buffer) hru
        integer :: lyr = 0                       !!     |soil layer for incoming tile flow (0 = surface)
      end type saturated_buffer_parameters
      type (saturated_buffer_parameters), dimension (:), allocatable :: satbuff_db
      
      type saturated_buffer
        type (saturated_buffer_parameters) :: sb_db
        integer :: dtbl = 0
        real :: inflo = 0.
        real :: no3 = 0.
      end type saturated_buffer
              
      type landuse
          character(len=40) :: name = ""
          integer :: cn_lu = 0
          integer :: cons_prac = 0
          real :: usle_p = 0.           !! none     | USLE equation support practice (P) factor daily
          character (len=40) :: urb_ro = ""!! none     | urban runoff model
                                        !!          | "usgs_reg", simulate using USGS regression eqs
                                        !!          | "buildup_washoff", simulate using build up/wash off alg 
          integer :: urb_lu = 0         !! none     | urban land type identification number
          real :: ovn = 0.05            !! none     | Manning's "n" value for overland flow
      end type landuse
      type (landuse), dimension (:), allocatable :: luse
      
      type soil_plant_initialize
        character(len=40) :: name = ""
        real :: sw_frac = 0.
        character(len=40) :: nutc = ""
        character(len=40) :: pestc = ""
        character(len=40) :: pathc = ""
        character(len=40) :: saltc = ""
        character(len=40) :: hmetc = ""
        character(len=40) :: csc = "" !rtb cs
        integer :: nut = 0
        integer :: pest = 1
        integer :: path = 1
        integer :: salt = 1
        integer :: hmet = 1
        integer :: cs = 1 !rtb cs
      end type soil_plant_initialize
      type (soil_plant_initialize), dimension (:), allocatable :: sol_plt_ini
        
      type hru_databases
        character(len=40) :: name = ""
        integer :: topo = 1
        integer :: hyd = 1
        integer :: soil = 1
        integer :: land_use_mgt = 1
        integer :: soil_plant_init = 1
        integer :: surf_stor = 0
        integer :: snow = 1
        integer :: field = 0
      end type hru_databases
      
      type hru_databases_char
        character(len=40) :: name = ""
        character(len=40) :: topo = ""
        character(len=40) :: hyd = ""
        character(len=40) :: soil = ""
        character(len=40) :: land_use_mgt = ""
        character(len=40) :: soil_plant_init = ""
        character(len=40) :: surf_stor = ""
        character(len=40) :: snow = ""
        character(len=40) :: field = ""
      end type hru_databases_char

      type hydrologic_response_unit_db
        character(len=40) :: name = "default"
        type (hru_databases) :: dbs
        type (hru_databases_char) :: dbsc
      end type hydrologic_response_unit_db
      type (hydrologic_response_unit_db), dimension(:),allocatable :: hru_db
      
      type land_use_mgt_variables
        real :: usle_p = 0.                 !! |none          |USLE equation comservation practice (P) factor
        real :: usle_ls = 0.                !! |none          |USLE equation length slope (LS) factor
        real :: usle_mult = 0.              !! |none          |product of USLE K,P,LS,exp(rock)
        real :: sdr_dep = 0.                !! |
        integer :: ldrain= 0.               !! |none          |soil layer where drainage tile is located
        real :: tile_ttime = 0.             !! |none          |Exponential of the tile flow travel time
        integer :: vfsi = 0                 !! |none          |on/off flag for vegetative filter strip
        real :: vfsratio = 0.               !! |none          |contouring USLE P factor
        real :: vfscon = 0.                 !! |none          |fraction of the total runoff from the entire field
        real :: vfsch = 0.                  !! |none          |fraction of flow entering the most concentrated 10% of the VFS.
                                            !!                     which is fully channelized
        integer :: ngrwat = 0
        integer :: grwat_i = 0              !! |none          |On/off Flag for waterway simulation
        real :: grwat_n = 0.                !! |none          |Mannings's n for grassed waterway
        real :: grwat_spcon = 0.            !! |none          |sediment transport coefficant defined by user
        real :: grwat_d = 0.                !! |m             |depth of Grassed waterway
        real :: grwat_w = 0.                !! |none          |Width of grass waterway
        real :: grwat_l = 0.                !! |km            |length of Grass Waterway
        real :: grwat_s = 0.                !! |m/m           |slope of grass waterway
        integer :: bmp_flag = 0             !! |none          |On/off Flag for user defeined bmp efficiency
        real :: bmp_sed = 0.                !! |%             | Sediment removal by BMP 
        real :: bmp_pp = 0.                 !! |%             | Particulate P removal by BMP
        real :: bmp_sp = 0.                 !! |%             | Soluble P removal by BMP
        real :: bmp_pn = 0.                 !! |%             | Particulate N removal by BMP 
        real :: bmp_sn = 0.                 !! |%             | Soluble N removal by BMP  
        real :: bmp_bac = 0.                !! |%             | Bacteria removal by BMP
      end type land_use_mgt_variables
     
      type nutrient_parameters
          real :: phoskd = 0.               !! |kg/m3         |
          real :: pperco = 0.               !! |kg/m3         |
          real :: psp = 0.                  !! |kg/m3         |
          real :: nperco = 0.               !! |kg/m3         |
          real :: cmn = 0.                  !! |kg/m3         |
          real :: nperco_lchtile = 0.       !! |kg/m3         |
      end type nutrient_parameters
      type hydrologic_response_unit
        character(len=40) :: name = ""
        integer :: obj_no = 0
        real :: area_ha = 0.
        real :: km = 0.
        integer :: surf_stor = 0                !points to res() for surface storage
        type (hru_databases) :: dbs             !database pointers
        type (hru_databases_char) :: dbsc       !database pointers
        integer :: land_use_mgt = 0
        character(len=40) :: land_use_mgt_c = ""
        integer :: lum_group = 0
        character(len=40) :: lum_group_c = ""   !land use group for soft cal and output
        character(len=40) :: cal_group = ""
        integer :: plant_cov = 0
        integer :: mgt_ops = 0
        integer :: tiledrain = 0
        integer :: septic = 0
        integer :: fstrip = 0
        integer :: grassww = 0
        integer :: bmpuser = 0
        integer :: crop_reg = 0
        integer :: paddy_irr = 0  !Jaehak 2022

        !! other data
        type (topography) :: topo
        type (field) :: field
        type (hydrology) :: hyd
        type (hydrology) :: hydcal
        type (landuse) :: luse
        type (land_use_mgt_variables) :: lumv
        type (subsurface_drainage_parameters) :: sdr
        type (snow_parameters) :: sno
        type (nutrient_parameters) :: nut
        type (saturated_buffer) :: sb
        real :: snocov1 = 0.
        real :: snocov2 = 0.
        integer :: cur_op = 1
        integer :: irr = 0                      !none       |set to 1 if irrigated during simulation - for wb soft cal
        integer :: irr_trn_dtbl = 0
        integer :: man_trn_dtbl = 0
        integer :: irr_trn_iauto = 0
        integer :: man_trn_iauto = 0
        integer :: wet_db = 0                   !none       |pointer to wetland data - saved so turn on/off
        real :: wet_hc = 0.                     !mm/h       |hydraulic conductivity of upper layer - wetlands
        real :: sno_mm = 0.                     !mm H2O     |amount of water in snow on current day
        real :: water_seep = 0.
        real :: water_evap = 0.
        real :: wet_obank_in = 0.               !mm         |inflow from overbank into wetlands
        real :: precip_aa = 0.
        real :: irr_yr = 0.                     !mm         |irrigation for year - used as dtbl condition jga6-25
        character(len=1) :: wet_fp = "n"
        character(len=40) :: irr_src = "unlim"   !           |irrigation source, Jaehak 2022
        real :: strsa = 0.
        real :: irr_hmax = 0                    !mm H2O     |target ponding depth during paddy irrigation Jaehak 2022
        real :: irr_hmin = 0                    !mm H2O     |threshold ponding depth to trigger paddy irrigation
        real :: irr_isc = 0                     !mm H2O     |ID of the source cha/res/aqu for paddy irrigation
        real, dimension(5) :: flow = 0          !mm H2O     |average annual flow (1=wyld,2=perc,3=surface,4=lateral,5=tile)
      end type hydrologic_response_unit
      type (hydrologic_response_unit), dimension(:), allocatable, target :: hru
      type (hydrologic_response_unit), dimension(:), allocatable, target :: hru_init

      
      real :: precip_eff = 0.   !! mm   |daily effective precip for runoff calculations = precipday + ls_overq + snomlt - canstor
                                !!      |precip_eff = precipday + ls_overq - snofall + snomlt - canstor
      real :: qday = 0.         !! mm   |surface runoff that reaches main channel during day in HRU
      real :: satexq_chan = 0.  !! mm   |saturation excess runoff that reaches main channel during day in HRU


!!    new/modified arrays for plant competition
      integer :: ipl = 0
      integer :: isol = 0

      real :: strsa_av = 0.
      real :: strsn_av = 0.
      real :: strsp_av = 0.
      real :: strstmp_av = 0.
      real :: rto_no3 = 0.
      real :: rto_solp = 0.
      real :: uno3d_tot = 0.
      real :: uapd_tot = 0.
      real :: sum_no3 = 0.
      real :: sum_solp = 0.
      real, dimension (:), allocatable :: epmax
      real, dimension (:), allocatable :: cvm_com
      real, dimension (:), allocatable :: rsdco_plcom
      real, dimension (:), allocatable :: translt
      real, dimension (:), allocatable :: uno3d
      real, dimension (:), allocatable :: uapd
      real, dimension (:), allocatable :: par
      real, dimension (:), allocatable :: htfac
      real, dimension (:), allocatable :: un2
      real, dimension (:), allocatable :: up2
      integer, dimension (:), allocatable :: iseptic
     
!! septic variables for output.std
      real :: qp_cms = 0.
      real :: sw_excess = 0.
      real :: albday = 0.
      real :: wt_shall = 0.
      real :: sq_rto = 0.
      real :: snomlt = 0.
      real :: snofall = 0.
      real :: fixn = 0.
      real :: qtile = 0.
      real :: latlyr = 0.            !!mm            |lateral flow in soil layer for the day
      real :: inflpcp = 0.           !!mm            |amount of precipitation that infiltrates
      real :: fertn = 0.
      real :: sepday = 0.
      real :: bioday = 0.
      real :: sepcrk = 0.
      real :: sepcrktot = 0.
      real :: fertno3 = 0.
      real :: fertnh3 = 0.
      real :: fertorgn = 0.
      real :: fertsolp = 0.
      real :: fertorgp = 0.
      real :: fertp = 0.
      real :: grazn = 0.
      real :: grazp = 0.
      real :: sdti = 0.
      real :: voltot = 0.            !!mm            |total volume of cracks expressed as depth per area unit
      real :: volcrmin = 0.          !!mm            |minimum crack volume allowed in any soil layer
      real :: canev = 0.
      real :: usle = 0.
      real :: rcn = 0.
      real :: enratio = 0.
      real :: vpd = 0.
      real :: pet_day = 0.
      real :: ep_day = 0.
      real :: snoev = 0.
      real :: es_day = 0.
      real :: ls_overq = 0.
      real :: latqrunon = 0.
      real :: tilerunon = 0.
      real :: ep_max = 0.
      real :: bsprev = 0.
      real :: usle_ei = 0.
      real :: snocov1 = 0.
      real :: snocov2 = 0.
      real :: lyrtile = 0.

      real :: etday = 0.
      integer :: mo = 0
      integer :: ihru = 0         !!none          |HRU number
      integer :: nd_30 = 0
      integer :: mpst = 0
      integer :: mlyr = 0
! date
      character(len=8) :: date = ""

!! septic change added iseptic 1/28/09 gsm
      integer :: isep_ly = 0
      real, dimension (:), allocatable :: qstemm
!! septic changes added 1/28/09 gsm
      real, dimension (:), allocatable :: bio_bod
      real, dimension (:), allocatable :: biom
      real, dimension (:), allocatable :: rbiom
      real, dimension (:), allocatable :: fcoli
      real, dimension (:), allocatable :: bz_perc
      real, dimension (:), allocatable :: plqm
!! Septic system by Jaehak Jeong
      integer, dimension (:), allocatable :: i_sep
      integer, dimension (:), allocatable :: sep_tsincefail
      
      real, dimension (:), allocatable :: sol_sumno3
      real, dimension (:), allocatable :: sol_sumsolp

!     Sediment parameters added by Balaji for the new routines

      real, dimension (:), allocatable :: sanyld
      real, dimension (:), allocatable :: silyld
      real, dimension (:), allocatable :: clayld
      real, dimension (:), allocatable :: sagyld
      real, dimension (:), allocatable :: lagyld
      real, dimension (:), allocatable :: grayld
      integer, dimension (:), allocatable :: itb
      
!!!!!! drains
      real, dimension (:), allocatable :: wnan

      real, dimension (:), allocatable :: phusw
      integer, dimension (:), allocatable :: yr_skip
      integer, dimension (:), allocatable :: isweep
      real :: sweepeff = 0.

      real, dimension (:), allocatable :: ranrns_hru
      integer, dimension (:), allocatable :: itill

      real, dimension (:), allocatable :: tc_gwat
      real, dimension (:), allocatable :: wfsh
      real, dimension (:), allocatable :: sed_con
      real, dimension (:), allocatable :: orgn_con
      real, dimension (:), allocatable :: orgp_con
      real, dimension (:), allocatable :: soln_con
      real, dimension (:), allocatable :: solp_con
      real, dimension (:), allocatable :: filterw
      real, dimension (:), allocatable :: cn2
      real, dimension (:), allocatable :: smx
      real, dimension (:), allocatable :: cnday
      real, dimension (:), allocatable :: tconc
      real, dimension (:), allocatable :: usle_cfac
      real, dimension (:), allocatable :: usle_eifac
      real, dimension (:), allocatable :: t_ov
      real, dimension (:), allocatable :: canstor
      real, dimension (:), allocatable :: ovrlnd

!    Drainmod tile equations  08/2006 
      real, dimension (:), allocatable :: cumei
      real, dimension (:), allocatable :: cumeira
      real, dimension (:), allocatable :: cumrt
      real, dimension (:), allocatable :: cumrai
      real, dimension (:), allocatable :: sstmaxd
      real, dimension (:), allocatable :: stmaxd
!    Drainmod tile equations  08/2006
      real, dimension (:), allocatable :: surqsolp
      real, dimension (:), allocatable :: cklsp
      real, dimension (:), allocatable :: pplnt
      real, dimension (:), allocatable :: brt

      real, dimension (:), allocatable :: twash
      real, dimension (:), allocatable :: doxq
      real, dimension (:), allocatable :: percn
      real, dimension (:), allocatable :: cbodu
      real, dimension (:), allocatable :: chl_a
      real, dimension (:), allocatable :: qdr
      real, dimension (:), allocatable :: latno3
      real, dimension (:), allocatable :: latq
      real, dimension (:), allocatable :: nplnt
      real, dimension (:), allocatable :: tileno3
      real, dimension (:), allocatable :: sedminpa
      real, dimension (:), allocatable :: sedminps
      real, dimension (:), allocatable :: sedorgn
      real, dimension (:), allocatable :: sedorgp
      real, dimension (:), allocatable :: sedyld
      real, dimension (:), allocatable :: sepbtm
      real, dimension (:), allocatable :: surfq
      real, dimension (:), allocatable :: surqno3
      real, dimension (:,:), allocatable :: surqsalt
      real, dimension (:,:), allocatable :: latqsalt
      real, dimension (:,:), allocatable :: tilesalt
      real, dimension (:,:), allocatable :: percsalt
      real, dimension (:,:), allocatable :: gwupsalt
      real, dimension (:,:), allocatable :: urbqsalt
      real, dimension (:,:), allocatable :: irswsalt
      real, dimension (:,:), allocatable :: irgwsalt
      real, dimension (:,:), allocatable :: wetqsalt
      real, dimension (:,:), allocatable :: wtspsalt
      real, dimension (:,:), allocatable :: surqcs                                                                       !rtb cs
      real, dimension (:,:), allocatable :: latqcs                                                                       !rtb cs
      real, dimension (:,:), allocatable :: tilecs                                                                       !rtb cs
      real, dimension (:,:), allocatable :: perccs                                                                       !rtb cs
      real, dimension (:,:), allocatable :: gwupcs                                                                       !rtb cs
      real, dimension (:,:), allocatable :: urbqcs                                                                       !rtb cs
      real, dimension (:,:), allocatable :: sedmcs                                                                       !rtb cs
      real, dimension (:,:), allocatable :: irswcs                                                                       !rtb cs
      real, dimension (:,:), allocatable :: irgwcs                                                                       !rtb cs
      real, dimension (:,:), allocatable :: wetqcs                                                                       !rtb cs
      real, dimension (:,:), allocatable :: wtspcs                                                                       !rtb cs
      real, dimension (:), allocatable :: phubase
      real, dimension (:), allocatable :: dormhr
      real, dimension (:,:), allocatable :: wrt
      real, dimension (:,:), allocatable :: bss
      real, dimension (:,:), allocatable :: surf_bs
      integer, dimension (:), allocatable :: swtrg
      real, dimension (:), allocatable :: rateinf_prev
      real, dimension (:), allocatable :: urb_abstinit
      !! burn
      integer, dimension (:), allocatable :: grz_days
      integer, dimension (:), allocatable :: igrz
      integer, dimension (:), allocatable :: ndeat

      real, dimension (:), allocatable :: gwsoilq        !rtb gwflow
      real, dimension (:), allocatable :: satexq         !rtb gwflow
      real, dimension (:,:), allocatable :: bss_ex !rtb gwflow
      real, dimension (:), allocatable :: gwsoiln                !rtb gwflow
      real, dimension (:), allocatable :: gwsoilp                !rtb gwflow
      real, dimension (:), allocatable :: satexn                 !rtb gwflow
      real, dimension (:), allocatable :: irrn      !rtb irrig (irrigation nutrient mass)
      real, dimension (:), allocatable :: irrp      !rtb irrig (irrigation nutrient mass)

!!     gsm added for sdr (drainage) 7/24/08
      integer, dimension (:,:), allocatable :: mgt_ops

      real, dimension (:,:), allocatable :: hhqday
! additional reach variables , added by Ann van Griensven
! Modifications to Pesticide and Water routing routines by Balaji Narasimhan
!Additional buffer and filter strip variables Mike White

    real, dimension (:), allocatable :: ubnrunoff
    real, dimension (:), allocatable :: ubntss
    real, dimension (:,:), allocatable :: ovrlnd_dt
    real, dimension (:,:), allocatable :: hhsurfq
    real, dimension (:,:,:), allocatable :: hhsurf_bs

!! subdaily erosion modeling by Jaehak Jeong
    real, dimension(:,:), allocatable:: hhsedy
    real, dimension(:), allocatable:: init_abstrc

      integer, dimension(:), allocatable :: tillage_switch
      real, dimension(:), allocatable :: tillage_depth
      integer, dimension(:), allocatable :: tillage_days
      real, dimension(:), allocatable :: tillage_factor

      end module hru_module