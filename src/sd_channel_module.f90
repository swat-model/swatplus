      module sd_channel_module
    
      implicit none

      integer :: maxint                           !number of intervals in hydrograph for degredation
      real :: wtemp                             !stream water temperature C
      real :: peakrate, sed_reduc_t, no3_reduc_kg, tp_reduc_kg, tp_reduc, srp_reduc_kg
      real, dimension(:), allocatable :: hyd_rad    !m^2        |hydraulic radius for each hydrograph time step
      real, dimension(:), allocatable :: trav_time  !days       |time spent in each hydrograph time step
      real, dimension(:), allocatable :: flo_dep    !m^2        |hydraulic radius for each hydrograph time step
      real, dimension(:), allocatable :: timeint    !days       |time spent in each hydrograph time step
      
      type swatdeg_hydsed_data
        character(len=25) :: name
        character(len=16) :: order
        real :: chw = 0.            !m          |channel width
        real :: chd = 0.            !m          |channel depth
        real :: chs = 0.            !m/m        |channel slope
        real :: chl = 0.            !km         |channel length
        real :: chn = 0.            !           |channel Manning's n
        real :: chk = 0.            !mm/h       |channel bottom conductivity
        real :: cherod = 0.         !           |channel erodibility
        real :: cov = 0.            !0-1        |channel cover factor
        real :: sinu                !none       |sinuousity - ratio of channel length and straight line length
        real :: chseq = 0.          !m/m        |equilibrium channel slope
        real :: d50 = 0.            !mm         |channel median sediment size
        real :: ch_clay = 0.        !%          |clay percent of bank and bed
        real :: carbon = 0.         !%          |carbon percent of bank and bed
        real :: ch_bd = 0.          !t/m3       |dry bulk density
        real :: chss = 0.           !           |channel side slope
        real :: bankfull_flo = 0.   !           |bank full flow rate
        real :: fps = 0.000001      !m/m        |flood plain slope
        real :: fpn = 0.1           !           |flood plain Manning's n
        real :: n_conc = 0.         !mg/kg      |nitrogen concentration in channel bank
        real :: p_conc = 0.         !mg/kg      |phosphorus concentration in channel bank
        real :: p_bio = 0.          !frac       |fraction of p in bank that is bioavailable
      end type swatdeg_hydsed_data
      type (swatdeg_hydsed_data), dimension (:), allocatable :: sd_chd
      
      type swatdeg_sednut_data
        character(len=25) :: name
        character(len=16) :: order
        real :: pk_rto = 1.         !ratio      |ratio of peak to mean daily flow in channel
        real :: fp_inun_days = 5.   !days       |number of days fllod plain is inundated after flood
        real :: n_setl = 0.5        !ratio      |ratio of amount of N settling and sediment settling
        real :: p_setl = 0.5        !ratio      |ratio of amount of P settling and sediment settling
        real :: n_sol_part = 0.01   !           |instream nitrogen soluble to particulate transformation coefficient
        real :: p_sol_part = 0.01   !           |instream phosphorus soluble to particulate transformation coefficient
        real :: n_dep_enr = 0.5     !           |enrichment of N in remaining water - deposition = 1/enrichment ratio
        real :: p_dep_enr = 0.5     !           |enrichment of P in remaining water - deposition = 1/enrichment ratio
        real :: arc_len_fr = 1.2    !frac       |fraction of arc length where bank erosion occurs
        real :: part_size = 0.002   !mm         |particle size of channel washload
        real :: wash_bed_fr = 0.1   !frac       |fraction of bank erosion that is washload
      end type swatdeg_sednut_data
      type (swatdeg_sednut_data), dimension (:), allocatable :: sd_chd1
      
      !! channel sediment budget for output
      type channel_sediment_budget_output
        real :: in_sed = 0.         !t          |incoming sediment to channel
        real :: out_sed = 0.        !t          |outgoing sediment from channel
        real :: fp_dep = 0.         !t          |flood plain deposition
        real :: ch_dep = 0.         !t          |channel deposition
        real :: bank_ero = 0.       !t          |channel bank erosion
        real :: bed_ero = 0.        !t          |channel bed erosion
        real :: in_no3 = 0.         !t          |incoming no3 to channel
        real :: in_orgn = 0.        !t          |incoming organic n to channel
        real :: out_no3 =  0.       !t          |outgoing no3 from channel
        real :: out_orgn =  0.      !t          |outgoing organic n from channel
        real :: fp_no3 = 0.         !t          |flood plain no3 lost
        real :: bank_no3 = 0.       !t          |bank no3 gain
        real :: bed_no3 = 0.        !t          |bed no3 gain
        real :: fp_orgn = 0.        !t          |flood plain organic n deposited
        real :: ch_orgn = 0.        !t          |channel organic n deposited
        real :: bank_orgn = 0.      !t          |bank organic n gain from erosion
        real :: bed_orgn = 0.       !t          |bed organic n gain from erosion
        real :: in_solp = 0.        !t          |incoming soluble p to channel
        real :: in_orgp = 0.        !t          |incoming organic p to channel
        real :: out_solp =  0.      !t          |outgoing soluble p from channel
        real :: out_orgp =  0.      !t          |outgoing organic p from channel
        real :: fp_solp = 0.        !t          |flood plain soluble p lost
        real :: bank_solp = 0.      !t          |bank no3 gain
        real :: bed_solp = 0.       !t          |bed no3 gain
        real :: fp_orgp =  0.       !t          |flood plain organic p deposited
        real :: ch_orgp = 0.        !t          |channel organic p deposited
        real :: bank_orgp = 0.      !t          |bank organic p gain from erosion
        real :: bed_orgp = 0.       !t          |bed organic n gain from erosion
        real :: no3_orgn = 0.       !t          |in channel transformation from no3 to organic n
        real :: solp_orgp = 0.      !t          |in channel transformation from no3 to organic n
      end type channel_sediment_budget_output
      type (channel_sediment_budget_output), dimension (:), allocatable :: ch_sed_bud
      type (channel_sediment_budget_output), dimension (:), allocatable :: ch_sed_bud_m, ch_sed_bud_y, ch_sed_bud_a
      type (channel_sediment_budget_output) :: ch_sed_budz, bch_sed_bud_d, bch_sed_bud_m, bch_sed_bud_y, bch_sed_bud_a

      type channel_morphology_output
        real :: w_yr = 0.           !ratio      |bank cutting - widths per year 
        real :: d_yr = 0.           !ratio      |bed down cutting - depths per year
        real :: fp_mm = 0.          !mm/yr      |flood plain deposition - uniform across the flood plain
      end type channel_morphology_output
      type (channel_morphology_output), dimension (:), allocatable :: ch_morph
      
      type gully_data
        character(len=16) :: name
        real :: hc_kh           !           |headcut erodibility
        real :: hc_hgt          !m          |headcut height
        real :: hc_ini          !km         |initial channel length for gullies
      end type gully_data
      type (gully_data), dimension (:), allocatable :: gully

      type swatdeg_init_datafiles
        integer :: init = 1                 !initial data-points to initial.cha
        integer :: org_min = 1              !points to initial organic-mineral input file
        integer :: pest = 1                 !points to initial pesticide input file
        integer :: path = 1                 !points to initial pathogen input file
        integer :: hmet = 1                 !points to initial heavy metals input file
        integer :: salt = 1                 !points to initial salt input file (salt_channel.ini) (rtb salt)
        integer :: cs = 1                   !points to initial constituent input file (cs_channel.ini) (rtb cs)
      end type swatdeg_init_datafiles
      type (swatdeg_init_datafiles), dimension(:), allocatable :: sd_init
            
      type swatdeg_datafiles
        character(len=16) :: name = ""
        character(len=16) :: initc = ""
        character(len=16) :: hydc = ""
        character(len=16) :: sedc = ""
        character(len=16) :: nutc = ""
        integer :: init = 1
        integer :: hyd = 1
        integer :: sed = 1
        integer :: nut = 1
        integer :: sednut = 1
      end type swatdeg_datafiles
      type (swatdeg_datafiles), dimension(:),allocatable :: sd_dat
      
      type floodplain_parameters
        character(len=25) :: name = "floodplain"    !           |name of flood plain
        integer :: obj_tot = 0                      !           |number of objects (hru and/or ru) in the flood plain
        integer :: hru_tot = 0                      !           |number of hru in the flood plain
        real :: ha = 0                              !ha         |sum of area of all hru in flood plain
        character (len=3), dimension(:), allocatable :: obtyp   !object type- 1=hru, 2=hru_lte, 11=export coef, etc
        integer, dimension(:), allocatable :: obtypno           !2-number of hru_lte"s or 1st hru_lte command
        integer, dimension (:), allocatable :: hru  !           |flood plain hru number
        real, dimension (:), allocatable :: hru_fr  !           |hru area fraction of the flood plain
      end type floodplain_parameters
        
      type muskingum_parameters
        integer :: nsteps = 1       !none       |number of daily time steps required for stability
        integer :: substeps = 1     !none       |number of time substeps required for stability
        real :: c1
        real :: c2
        real :: c3
      end type muskingum_parameters

      type swatdeg_channel_dynamic
        character(len=25) :: name = "default"
        integer :: props
        integer :: obj_no
        integer :: wallo                    !water allocation object number
        integer :: aqu_link = 0             !aquifer the channel is linked to
        integer :: aqu_link_ch = 0          !sequential channel number in the aquifer
        character(len=25) :: region
        character(len=25) :: order
        real :: chw = 3.        !m          |channel width
        real :: chd = .5        !m          |channel depth
        real :: chs = .01       !m/m        |channel slope
        real :: chl = .1        !km         |channel length
        real :: chn             !           |channel Manning's n
        real :: chk             !mm/h       |channel bottom conductivity
        real :: cov             !0-1        |channel cover factor
        real :: sinu            !none       |sinuousity - ratio of channel length and straight line length
        real :: chseq           !m/m        |equilibrium channel slope
        real :: d50
        real :: ch_clay
        real :: carbon
        real :: ch_bd
        real :: chss
        real :: bankfull_flo
        real :: fps
        real :: fpn
        real :: n_conc = 0.         !mg/kg      |nitrogen concentration in channel bank
        real :: p_conc = 0.         !mg/kg      |phosphorus concentration in channel bank
        real :: p_bio = 0.          !frac       |fraction of p in bank that is bioavailable
        real :: pk_rto = 1.         !ratio      |ratio of peak to mean daily flow in channel
        real :: fp_inun_days = 5.   !days       |number of days fllod plain is inundated after flood
        real :: n_setl = 0.5        !ratio      |ratio of amount of N settling and sediment settling
        real :: p_setl = 0.5        !ratio      |ratio of amount of P settling and sediment settling
        real :: n_sol_part = 0.01   !frac       |instream nitrogen soluble to particulate transformation coefficient
        real :: p_sol_part = 0.01   !frac       |instream phosphorus soluble to particulate transformation coefficient
        real :: n_dep_enr = 0.5     !           |enrichment of N in remaining water - deposition = 1/enrichment ratio
        real :: p_dep_enr = 0.5     !           |enrichment of P in remaining water - deposition = 1/enrichment ratio
        real :: arc_len_fr = 1.2    !frac       |fraction of arc length where bank erosion occurs
        real :: part_size = 0.002   !mm         |particle size of channel washload
        real :: wash_bed_fr = 0.2   !frac       |fraction of bank erosion that is washload
        real :: hc_kh = 0.
        real :: hc_hgt = 0.     !m          |headcut height
        real :: hc_ini = 0.
        real :: cherod = 0.     !           |channel erodibility
        real :: shear_bnk = 0.  !0-1        |bank shear coefficient - fraction of bottom shear
        real :: hc_erod = 0.    !           |headcut erodibility
        real :: hc_co = 0.      !m/m        |proportionality coefficient for head cut
        real :: hc_len = 0.     !m          |length of head cut
        real :: in1_vol = 0.    !m3         |inflow during previous time step for Muskingum
        real :: out1_vol = 0.   !m3         |outflow during previous time step for Muskingum
        real :: stor_dis_01bf = 0.      !hr         |storage time constant at 0.1*bankfull
        real :: stor_dis_bf = 0.        !hr         |storage time constant at bankfull
        type (muskingum_parameters) :: msk
        type (floodplain_parameters) :: fp
        real, dimension (:), allocatable :: kd      !           |aquatic mixing velocity (diffusion/dispersion)-using mol_wt
        real, dimension (:), allocatable :: aq_mix  ! m/day     |aquatic mixing velocity (diffusion/dispersion)-using mol_wt
        character (len=2) :: overbank               !           |"ib"=in bank; "ob"=overbank flood
      end type swatdeg_channel_dynamic
      type (swatdeg_channel_dynamic), dimension (:), allocatable :: sd_ch
      type (swatdeg_channel_dynamic), dimension (:), allocatable :: sdch_init  
            
      type channel_rating_curve_parameters
        real :: flo_rate = 0.           !m^3/s      |flow rate
        real :: xsec_area = 0.          !m^2        |cross sectional area of flow
        real :: surf_area = 0.          !m^2        |total surface area
        real :: dep = 0.                !m          |depth of water
        real :: top_wid = 0.            !m          |depth of water
        real :: vol = 0.                !m^3        |total volume of water in reach and flood plain
        real :: vol_fp = 0.             !m^3        |volume of water in flood plain
        real :: vol_ch = 0.             !m^3        |volume of water in and above channel
        real :: wet_perim = 0.          !m          |wetted perimeter
        real :: ttime = 0.              !hr         |travel time
      end type channel_rating_curve_parameters
      type (channel_rating_curve_parameters) :: rcurv   !rating curve at each time step
      type (channel_rating_curve_parameters) :: rcz     !zero rating curve
      
      type channel_rating_curve
        integer :: npts = 4         !none       |number of points on the rating curve
        real :: wid_btm = 0.        !m          |bottom width of main channel
        !! elev - 1=.1 bf dep; 2=bf dep; 3=1.2*bf dep; 4=2*bf dep
        type (channel_rating_curve_parameters) :: in1                   !rating curve - inflow previous time step
        type (channel_rating_curve_parameters) :: in2                   !rating curve - inflow current time step
        type (channel_rating_curve_parameters) :: out1                  !rating curve - outflow previous time step
        type (channel_rating_curve_parameters) :: out2                  !rating curve - outflow current time step
        type (channel_rating_curve_parameters), dimension(4) :: elev    !rating curve at each depth
      end type channel_rating_curve
      type (channel_rating_curve), dimension(:), allocatable :: ch_rcurv
           
      type sd_ch_output
        real :: flo_in = 0.             !(m^3/s)       |average daily inflow rate during time step
        real :: aqu_in = 0.             !(m^3/s)       |geomorphic aquifer flow into channel/aquifer inflow using geomorphic baseflow method
        real :: flo = 0.                !(m^3/s)       |average daily outflow rate during timestep
        real :: peakr = 0.              !(m^3/s)       |average peak runoff rate during timestep
        real :: sed_in = 0.             !(tons)        |sediment in
        real :: sed_out = 0.            !(tons)        |sediment out
        real :: washld = 0.             !(tons)        |wash load (suspended) out
        real :: bedld = 0.              !(tons)        |bed load out
        real :: dep = 0.                !(tons)        |deposition in channel and flood plain
        real :: deg_btm = 0.            !(tons)        |erosion of channel bottom 
        real :: deg_bank = 0.           !(tons)        |erosion of channel bank
        real :: hc_sed = 0.             !(tons)        |erosion from gully head cut
        real :: width = 0.              !m             |channel bank full top width at end of time step
        real :: depth = 0.              !m             |channel bank full depth at end of time step
        real :: slope = 0.              !m/m           |channel slope
        real :: deg_btm_m = 0.          !(m)           !downcutting of channel bottom
        real :: deg_bank_m = 0.         !(m)           |widening of channel banks
        real :: hc_m = 0.               !(m)           |headcut retreat
        real :: flo_in_mm = 0.          !(mm)          |inflow rate total sum for each time step
        real :: aqu_in_mm = 0.          !(mm)          |aquifer inflow rate total sum for each time step
        real :: flo_mm = 0.             !(mm)          |outflow rate total sum for each time step
        real :: sed_stor = 0.           !(tons)        |sed storage at end of timestep 
        real :: n_tot = 0.              !(kg N)        |total nitrogen leaving the reach 
        real :: p_tot = 0.              !(kg N)        |total phosphorus leaving the reach
        real :: dep_bf = 0.             !m             |depth of water when reach is at bankfull depth
        real :: velav_bf = 0.           !m/s           |average velocity when reach is at bankfull depth
      end type sd_ch_output
      
      type (sd_ch_output), dimension(:), allocatable, save :: chsd_d
      type (sd_ch_output), dimension(:), allocatable, save :: chsd_m
      type (sd_ch_output), dimension(:), allocatable, save :: chsd_y
      type (sd_ch_output), dimension(:), allocatable, save :: chsd_a
      type (sd_ch_output), dimension(:), allocatable, save :: schsd_d
      type (sd_ch_output), dimension(:), allocatable, save :: schsd_m
      type (sd_ch_output), dimension(:), allocatable, save :: schsd_y
      type (sd_ch_output), dimension(:), allocatable, save :: schsd_a
      type (sd_ch_output) :: bchsd_d
      type (sd_ch_output) :: bchsd_m
      type (sd_ch_output) :: bchsd_y
      type (sd_ch_output) :: bchsd_a
      type (sd_ch_output) :: chsdz
            
      type sdch_header
          character (len=6) :: day        =  "  jday"
          character (len=6) :: mo         =  "   mon"
          character (len=6) :: day_mo     =  "   day"
          character (len=6) :: yrc        =  "    yr"
          character (len=8) :: isd        =  "   unit "
          character (len=8) :: id         =  " gis_id "           
          character (len=16) :: name      =  " name          "         
          character(len=16) :: flo_in     =  "         flo_in"        ! (m^3/s)
          character(len=16) :: aqu_in     =  "         geo_bf"        ! (m^3/s)
          character(len=16) :: flo        =  "        flo_out"        ! (m^3/s)
          character(len=15) :: peakr      =  "          peakr"        ! (m^3/s)
          character(len=15) :: sed_in     =  "         sed_in"        ! (tons)
          character(len=15) :: sed_out    =  "        sed_out"        ! (tons)
          character(len=15) :: washld     =  "         washld"        ! (tons)
          character(len=15) :: bedld      =  "          bedld"        ! (tons)
          character(len=15) :: dep        =  "            dep"        ! (tons)
          character(len=15) :: deg_btm    =  "        deg_btm"        ! (tons)
          character(len=15) :: deg_bank   =  "       deg_bank"        ! (tons)
          character(len=15) :: hc_sed     =  "         hc_sed"        ! (tons)
          character(len=15) :: width      =  "          width"        ! (m)
          character(len=15) :: depth      =  "          depth"        ! (m)
          character(len=15) :: slope      =  "          slope"        ! (m/m)
          character(len=15) :: deg_btm_m  =  "        deg_btm"        ! (m)
          character(len=15) :: deg_bank_m =  "       deg_bank"        ! (m)
          character(len=15) :: hc_len     =  "         hc_len"        ! (m)
          character(len=16) :: flo_in_mm  =  "      flo_in_mm"        ! (mm)
          character(len=16) :: aqu_in_mm  =  "      aqu_in_mm"        ! (mm)
          character(len=16) :: flo_mm     =  "     flo_out_mm"        ! (mm)
          character(len=16) :: sed_stor   =  "       sed_stor"        ! (tons)
          character(len=16) :: n_tot      =  "     n_tot     "        ! (kg_N)
          character(len=16) :: p_tot      =  "    p_tot      "        ! (kg_N)
          character(len=16) :: dep_bf     =  "  dep_bf       "        ! (m/s)
          character(len=16) :: velav_bf   =  "velav_bf       "        ! (m/s)
      end type sdch_header
      type (sdch_header) :: sdch_hdr
      
      type sdch_header_units
          character (len=6) :: day        =  "      "
          character (len=6) :: mo         =  "      "
          character (len=6) :: day_mo     =  "      "
          character (len=6) :: yrc        =  "      "
          character (len=8) :: isd        =  "        "
          character (len=8) :: id         =  "        "           
          character (len=16) :: name      =  "              "        
          character(len=16) :: flo_in     =  "          m^3/s"       ! (m^3/s)
          character(len=16) :: aqu_in     =  "          m^3/s"       ! (m^3/s)      
          character(len=16) :: flo        =  "          m^3/s"       ! (m^3/s) 
          character(len=15) :: peakr      =  "          m^3/s"        ! (m^3/s)
          character(len=15) :: sed_in     =  "           tons"        ! (tons)
          character(len=15) :: sed_out    =  "           tons"        ! (tons)
          character(len=15) :: washld     =  "           tons"        ! (tons)
          character(len=15) :: bedld      =  "           tons"        ! (tons)
          character(len=15) :: dep        =  "           tons"        ! (tons)
          character(len=15) :: deg_btm    =  "           tons"        ! (tons)
          character(len=15) :: deg_bank   =  "           tons"        ! (tons)
          character(len=15) :: hc_sed     =  "           tons"        ! (tons)
          character(len=15) :: width      =  "              m"        ! (m)
          character(len=15) :: depth      =  "              m"        ! (m)
          character(len=15) :: slope      =  "            m/m"        ! (m/m)
          character(len=15) :: deg_btm_m  =  "              m"        ! (m)
          character(len=15) :: deg_bank_m =  "              m"        ! (m)
          character(len=15) :: hc_len     =  "              m"        ! (m)
          character(len=16) :: flo_in_mm  =  "             mm"        ! (mm)
          character(len=16) :: aqu_in_mm  =  "             mm"        ! (mm)      
          character(len=16) :: flo_mm     =  "             mm"        ! (mm) 
          character(len=16) :: sed_stor   =  "           tons"        ! (tons)
          character(len=16) :: n_tot      =  "      kg_N     "        ! (kg_N)
          character(len=16) :: p_tot      =  "     kg_P      "        ! (kg_P)
          character(len=16) :: dep_bf     =  "     m/s       "        ! (m/s)
          character(len=16) :: velav_bf   =  "     m/s       "        ! (m/s)
      end type sdch_header_units
      type (sdch_header_units) :: sdch_hdr_units  
      
      
!!    SD_CHAN_BUD_HEADERS      
      type sdch_bud
          character (len=6) :: day        =  "  jday"
          character (len=6) :: mo         =  "   mon"
          character (len=6) :: day_mo     =  "   day"
          character (len=6) :: yrc        =  "    yr"
          character (len=8) :: isd        =  "   unit "
          character (len=8) :: id         =  " gis_id "           
          character (len=16) :: name      =  " name          "          
          character(len=16) :: in_sed     =  "         in_sed"        ! (tons)
          character(len=16) :: out_sed    =  "        out_sed"        ! (tons)
          character(len=16) :: fp_dep     =  "       fp_dep  "        ! (tons)
          character(len=16) :: ch_dep     =  "      ch_dep   "        ! (tons)
          character(len=16) :: bank_ero   =  "    bank_ero   "        ! (tons)
          character(len=16) :: bed_ero    =  "    bed_ero    "        ! (tons)
          character(len=16) :: in_no3     =  "    in_no3     "        ! (tons)
          character(len=16) :: in_orgn    =  "  in_orgn      "        ! (tons)
          character(len=15) :: out_no3    =  " out_no3      "         ! (tons)
          character(len=16) :: out_orgn   =  "out_orgn       "        ! (tons)
          character(len=15) :: fp_no3     =  " fp_no3       "        ! (tons)
          character(len=15) :: bank_no3   =  "bank_no3      "        ! (tons)
          character(len=15) :: bed_no3    =  "bed_no3       "        ! (tons)
          character(len=15) :: fp_orgn    =  "fp_orgn       "        ! (tons)
          character(len=15) :: ch_orgn    =  "ch_orgn       "        ! (tons)
          character(len=15) :: bank_orgn  =  "bank_orgn     "        ! (tons)
          character(len=15) :: bed_orgn   =  "bed_orgn      "        ! (tons)
          character(len=15) :: in_solp    =  "in_solp       "        ! (tons)
          character(len=15) :: in_orgp    =  "in_orgp       "        ! (tons)
          character(len=15) :: out_solp   =  "out_solp      "        ! (tons)
          character(len=15) :: out_orgp   =  "out_orgp      "        ! (tons)
          character(len=15) :: fp_solp    =  "fp_solp       "        ! (tons)
          character(len=15) :: bank_solp  =  "bank_solp     "        ! (tons)
          character(len=15) :: bed_solp   =  "bed_solp      "        ! (tons)
          character(len=15) :: fp_orgp    =  "fp_orgp       "        ! (tons)
          character(len=15) :: ch_orgp    =  "ch_orgp       "        ! (tons)
          character(len=15) :: bank_orgp  =  "bank_orgp     "        ! (tons)
          character(len=15) :: bed_orgp   =  "bed_orgp      "        ! (tons)
          character(len=15) :: no3_orgn   =  "no3_orgn      "        ! (tons)
          character(len=15) :: solp_orgp  =  "solp_orgp     "        ! (tons)
      end type sdch_bud
      type (sdch_bud) :: sdch_bud_hdr
      
      type sdch_bud_units
          character (len=6) :: day        =  "      "
          character (len=6) :: mo         =  "      "
          character (len=6) :: day_mo     =  "      "
          character (len=6) :: yrc        =  "      "
          character (len=8) :: isd        =  "        "
          character (len=8) :: id         =  "        "           
          character (len=16) :: name      =  "              "        
          character(len=16) :: in_sed     =  "           tons"       ! (tons)
          character(len=16) :: out_sed    =  "           tons"       ! (tons)      
          character(len=16) :: fp_dep     =  "         tons  "       ! (tons) 
          character(len=16) :: ch_dep     =  "        tons   "       ! (tons)
          character(len=16) :: bank_ero   =  "        tons   "       ! (tons)
          character(len=16) :: bed_ero    =  "       tons    "       ! (tons)
          character(len=16) :: in_no3     =  "      tons     "       ! (tons)
          character(len=16) :: in_orgn    =  "     tons      "       ! (tons)
          character(len=15) :: out_no3    =  "    tons       "       ! (tons)
          character(len=16) :: out_orgn   =  "   tons        "       ! (tons)
          character(len=15) :: fp_no3     =  "  tons        "       ! (tons)
          character(len=15) :: bank_no3   =  "  tons        "       ! (tons)
          character(len=15) :: bed_no3    =  "  tons        "       ! (tons)
          character(len=16) :: fp_orgn    =  "  tons        "       ! (tons)
          character(len=15) :: ch_orgn    =  "  tons        "       ! (tons)
          character(len=15) :: bank_orgn  =  "  tons        "       ! (tons)
          character(len=15) :: bed_orgn   =  "  tons        "       ! (tons)
          character(len=15) :: in_solp    =  "  tons        "       ! (tons)
          character(len=15) :: in_orgp    =  "  tons        "       ! (tons)
          character(len=15) :: out_solp   =  "  tons        "       ! (tons)      
          character(len=15) :: out_orgp   =  "  tons        "       ! (tons) 
          character(len=15) :: fp_solp    =  "  tons        "       ! (tons)
          character(len=15) :: bank_solp  =  "  tons        "       ! (tons)
          character(len=15) :: bed_solp   =  "  tons        "       ! (tons)
          character(len=15) :: fp_orgp    =  "  tons        "       ! (tons)
          character(len=15) :: ch_orgp    =  "  tons        "       ! (tons)
          character(len=15) :: bank_orgp  =  "  tons        "       ! (tons)
          character(len=15) :: bed_orgp   =  "  tons        "       ! (tons)
          character(len=15) :: no3_orgp   =  "  tons        "       ! (tons)
          character(len=15) :: solp_orgp  =  "  tons        "       ! (tons)
      end type sdch_bud_units
      type (sdch_bud_units) :: sdch_bud_hdr_units   
!!    SD_CHAN_BUD_HEADERS      
      
      type sdch_header_sub
          character (len=6) :: day        =  "  jday"
          character (len=6) :: mo         =  "   mon"
          character (len=6) :: day_mo     =  "   day"
          character (len=6) :: yrc        =  "    yr"
          character (len=8) :: isd        =  "   unit "
          character (len=8) :: id         =  " gis_id " 
          character (len=8) :: ii         =  "  tstep "
          character (len=16) :: name      =  " name          "        
          character(len=16) :: hyd_flo    =  "        flo_out"        ! (m^3/s)
      end type sdch_header_sub
      type (sdch_header_sub) :: sdch_hdr_subday
  
      type sdch_header_units_sub
          character (len=6) :: day        =  "      "
          character (len=6) :: mo         =  "      "
          character (len=6) :: day_mo     =  "      "
          character (len=6) :: yrc        =  "      "
          character (len=8) :: isd        =  "        "
          character (len=8) :: id         =  "        " 
          character (len=8) :: ii         =  "        "
          character (len=16) :: name      =  "              "         
          character (len=16) :: hyd_flo   =  "        m^3/s   "        ! (m^3/s)
      end type sdch_header_units_sub
      type (sdch_header_units) :: sdch_hdr_units_sub
     
      interface operator (+)
        module procedure chsd_add
      end interface
      
      interface operator (/)
        module procedure chsd_div
      end interface
              
      interface operator (//)
        module procedure chsd_ave
      end interface
        
      interface operator (*)
        module procedure chsd_mult
      end interface 
                           
      interface operator (*)
        module procedure chrc_mult
      end interface 
             
      interface operator (+)
        module procedure chsednut_add
      end interface
      
      interface operator (/)
        module procedure chsednut_div
      end interface
              
      contains
!! routines for swatdeg_hru module

      function chsednut_add(cho1,cho2) result (cho3)
      type (channel_sediment_budget_output),  intent (in) :: cho1
      type (channel_sediment_budget_output),  intent (in) :: cho2
      type (channel_sediment_budget_output) :: cho3
        cho3%in_sed = cho1%in_sed + cho2%in_sed
        cho3%out_sed = cho1%out_sed + cho2%out_sed
        cho3%fp_dep = cho1%fp_dep + cho2%fp_dep
        cho3%ch_dep = cho1%ch_dep + cho2%ch_dep
        cho3%bank_ero = cho1%bank_ero + cho2%bank_ero
        cho3%bed_ero = cho1%bed_ero + cho2%bed_ero
        cho3%in_no3 = cho1%in_no3 + cho2%in_no3
        cho3%in_orgn = cho1%in_orgn + cho2%in_orgn
        cho3%out_no3 =  cho1%out_no3 + cho2%out_no3
        cho3%out_orgn =  cho1%out_orgn + cho2%out_orgn
        cho3%fp_no3 = cho1%fp_no3 + cho2%fp_no3
        cho3%bank_no3 = cho1%bank_no3 + cho2%bank_no3
        cho3%bed_no3 = cho1%bed_no3 + cho2%bed_no3
        cho3%fp_orgn = cho1%fp_orgn + cho2%fp_orgn
        cho3%ch_orgn = cho1%ch_orgn + cho2%ch_orgn
        cho3%bank_orgn = cho1%bank_orgn + cho2%bank_orgn
        cho3%bed_orgn = cho1%bed_orgn + cho2%bed_orgn
        cho3%in_solp = cho1%in_solp + cho2%in_solp
        cho3%in_orgp = cho1%in_orgp + cho2%in_orgp
        cho3%out_solp = cho1%out_solp + cho2%out_solp
        cho3%out_orgp = cho1%out_orgp + cho2%out_orgp
        cho3%fp_solp = cho1%fp_solp + cho2%fp_solp
        cho3%bank_solp = cho1%bank_solp + cho2%bank_solp
        cho3%bed_solp = cho1%bed_solp + cho2%bed_solp
        cho3%fp_orgp =  cho1%fp_orgp + cho2%fp_orgp
        cho3%ch_orgp = cho1%ch_orgp + cho2%ch_orgp
        cho3%bank_orgp = cho1%bank_orgp + cho2%bank_orgp
        cho3%bed_orgp = cho1%bed_orgp + cho2%bed_orgp
        cho3%no3_orgn = cho1%no3_orgn + cho2%no3_orgn
        cho3%solp_orgp = cho1%solp_orgp + cho2%solp_orgp
      end function
       
      function chsednut_div (cho1,const) result (cho2)
        type (channel_sediment_budget_output), intent (in) :: cho1
        real, intent (in) :: const
        type (channel_sediment_budget_output) :: cho2
        cho2%in_sed = cho1%in_sed / const
        cho2%out_sed = cho1%out_sed / const
        cho2%fp_dep = cho1%fp_dep / const
        cho2%ch_dep = cho1%ch_dep / const
        cho2%bank_ero = cho1%bank_ero / const
        cho2%bed_ero = cho1%bed_ero / const
        cho2%in_no3 = cho1%in_no3 / const
        cho2%in_orgn = cho1%in_orgn / const
        cho2%out_no3 =  cho1%out_no3 / const
        cho2%out_orgn =  cho1%out_orgn / const
        cho2%fp_no3 = cho1%fp_no3 / const
        cho2%bank_no3 = cho1%bank_no3 / const
        cho2%bed_no3 = cho1%bed_no3 / const
        cho2%fp_orgn = cho1%fp_orgn / const
        cho2%ch_orgn = cho1%ch_orgn / const
        cho2%bank_orgn = cho1%bank_orgn / const
        cho2%bed_orgn = cho1%bed_orgn / const
        cho2%in_solp = cho1%in_solp / const
        cho2%in_orgp = cho1%in_orgp / const
        cho2%out_solp = cho1%out_solp / const
        cho2%out_orgp = cho1%out_orgp / const
        cho2%fp_solp = cho1%fp_solp / const
        cho2%bank_solp = cho1%bank_solp / const
        cho2%bed_solp = cho1%bed_solp / const
        cho2%fp_orgp =  cho1%fp_orgp / const
        cho2%ch_orgp = cho1%ch_orgp / const
        cho2%bank_orgp = cho1%bank_orgp / const
        cho2%bed_orgp = cho1%bed_orgp / const
        cho2%no3_orgn = cho1%no3_orgn / const
        cho2%solp_orgp = cho1%solp_orgp / const
      end function chsednut_div
      
      function chsd_add(cho1,cho2) result (cho3)
      type (sd_ch_output),  intent (in) :: cho1
      type (sd_ch_output),  intent (in) :: cho2
      type (sd_ch_output) :: cho3
       cho3%flo_in = cho1%flo_in + cho2%flo_in
       cho3%aqu_in = cho1%aqu_in + cho2%aqu_in
       cho3%flo = cho1%flo + cho2%flo
       cho3%peakr = cho1%peakr + cho2%peakr
       cho3%sed_in = cho1%sed_in + cho2%sed_in
       cho3%sed_out = cho1%sed_out + cho2%sed_out
       cho3%washld = cho1%washld + cho2%washld
       cho3%bedld = cho1%bedld + cho2%bedld
       cho3%dep = cho1%dep + cho2%dep
       cho3%deg_btm = cho1%deg_btm + cho2%deg_btm
       cho3%deg_bank = cho1%deg_bank + cho2%deg_bank
       cho3%hc_sed = cho1%hc_sed + cho2%hc_sed
       cho3%width = cho2%width
       cho3%depth = cho2%depth
       cho3%slope = cho2%slope
       cho3%deg_btm_m = cho1%deg_btm_m + cho2%deg_btm_m
       cho3%deg_bank_m = cho1%deg_bank_m + cho2%deg_bank_m
       cho3%hc_m = cho1%hc_m + cho2%hc_m
       cho3%flo_in_mm = cho1%flo_in_mm + cho2%flo_in_mm
       cho3%aqu_in_mm = cho1%aqu_in_mm + cho2%aqu_in_mm
       cho3%flo_mm = cho1%flo_mm + cho2%flo_mm
       cho3%sed_stor = cho1%sed_stor + cho2%sed_stor
       cho3%n_tot = cho1%n_tot + cho2%n_tot
       cho3%p_tot = cho1%p_tot + cho2%p_tot
       cho3%dep_bf = cho1%dep_bf + cho2%dep_bf
       cho3%velav_bf = cho1%velav_bf + cho2%velav_bf
      end function chsd_add
       
      function chsd_div (ch1,const) result (ch2)
        type (sd_ch_output), intent (in) :: ch1
        real, intent (in) :: const
        type (sd_ch_output) :: ch2
        ch2%flo_in = ch1%flo_in / const
        ch2%aqu_in = ch1%aqu_in / const
        ch2%flo = ch1%flo / const
        ch2%peakr = ch1%peakr / const
        ch2%sed_in = ch1%sed_in
        ch2%sed_out = ch1%sed_out
        ch2%washld = ch1%washld
        ch2%bedld = ch1%bedld
        ch2%dep = ch1%dep
        ch2%deg_btm = ch1%deg_btm
        ch2%deg_bank = ch1%deg_bank
        ch2%hc_sed = ch1%hc_sed
        ch2%width = ch1%width
        ch2%depth = ch1%depth
        ch2%slope = ch1%slope
        ch2%deg_btm_m = ch1%deg_btm_m
        ch2%deg_bank_m = ch1%deg_bank_m
        ch2%hc_m = ch1%hc_m
        ch2%flo_in_mm = ch1%flo_in_mm
        ch2%aqu_in_mm = ch1%aqu_in_mm
        ch2%flo_mm = ch1%flo_mm
        ch2%sed_stor = ch1%sed_stor / const
        ch2%p_tot = ch1%p_tot / const
        ch2%n_tot = ch1%n_tot / const
        ch2%dep_bf = ch1%dep_bf / const
        ch2%velav_bf = ch1%velav_bf / const
      end function chsd_div
      
      function chsd_ave (ch1,const) result (ch2)
        type (sd_ch_output), intent (in) :: ch1
        real, intent (in) :: const
        type (sd_ch_output) :: ch2
        ch2%flo_in = ch1%flo_in
        ch2%aqu_in = ch1%aqu_in
        ch2%flo = ch1%flo
        ch2%peakr = ch1%peakr
        ch2%sed_in = ch1%sed_in / const
        ch2%sed_out = ch1%sed_out / const
        ch2%washld = ch1%washld / const
        ch2%bedld = ch1%bedld / const
        ch2%dep = ch1%dep / const
        ch2%deg_btm = ch1%deg_btm / const
        ch2%deg_bank = ch1%deg_bank / const
        ch2%hc_sed = ch1%hc_sed / const
        ch2%width = ch1%width
        ch2%depth = ch1%depth
        ch2%slope = ch1%slope
        ch2%deg_btm_m = ch1%deg_btm_m / const
        ch2%deg_bank_m = ch1%deg_bank_m / const
        ch2%hc_m = ch1%hc_m / const
        ch2%flo_in_mm = ch1%flo_in_mm / const
        ch2%aqu_in_mm = ch1%aqu_in_mm / const
        ch2%flo_mm = ch1%flo_mm / const
        ch2%sed_stor = ch1%sed_stor
        ch2%p_tot = ch1%p_tot
        ch2%n_tot = ch1%n_tot
        ch2%dep_bf = ch1%dep_bf
        ch2%velav_bf = ch1%velav_bf
      end function chsd_ave
           
      function chsd_mult (const, chn1) result (chn2)
        type (sd_ch_output), intent (in) :: chn1
        real, intent (in) :: const
        type (sd_ch_output) :: chn2
        chn2%flo_in = const * chn1%flo_in
        chn2%aqu_in = const * chn1%aqu_in
        chn2%flo = const * chn1%flo
        chn2%peakr = const * chn1%peakr
        chn2%sed_in = const * chn1%sed_in
        chn2%sed_out = const * chn1%sed_out
        chn2%washld = const * chn1%washld
        chn2%bedld = const * chn1%bedld
        chn2%dep = const * chn1%dep
        chn2%deg_btm = const * chn1%deg_btm
        chn2%deg_bank = const * chn1%deg_bank
        chn2%hc_sed = const * chn1%hc_sed 
        chn2%width = const * chn1%width
        chn2%depth = const * chn1%depth
        chn2%slope = const * chn1%slope
        chn2%deg_btm_m = const * chn1%deg_btm_m
        chn2%deg_bank_m = const * chn1%deg_bank_m
        chn2%hc_m = const * chn1%hc_m
        chn2%flo_in_mm = const * chn1%flo_in_mm
        chn2%aqu_in_mm = const * chn1%aqu_in_mm
        chn2%flo_mm = const * chn1%flo_mm
        chn2%sed_stor = const * chn1%sed_stor
        chn2%p_tot = const * chn1%p_tot
        chn2%n_tot = const * chn1%n_tot
        chn2%dep_bf = const * chn1%dep_bf
        chn2%velav_bf = const * chn1%velav_bf
      end function chsd_mult
                  
      !! this function multiplies the rating curve by a ratio
      !! used when interpolating flow rates in flood routing
      function chrc_mult (rc1, const) result (rc2)
        type (channel_rating_curve_parameters), intent (in) :: rc1
        real, intent (in) :: const
        type (channel_rating_curve_parameters) :: rc2
        rc2%xsec_area = rc1%xsec_area * const
        rc2%surf_area = rc1%surf_area * const
        rc2%flo_rate = rc1%flo_rate * const
        rc2%dep = rc1%dep * const 
        rc2%top_wid = rc1%top_wid * const
        rc2%vol = rc1%vol * const
        rc2%vol_ch = rc1%vol_ch * const
        rc2%vol_fp = rc1%vol_fp * const
        rc2%wet_perim = rc1%wet_perim * const
        rc2%ttime = rc1%ttime * const
      end function chrc_mult
      
      subroutine chrc_interp (rc1, rc2, const, rci)
        type (channel_rating_curve_parameters), intent (in) :: rc1
        type (channel_rating_curve_parameters), intent (in) :: rc2
        type (channel_rating_curve_parameters), intent (out) :: rci
        real, intent (in) :: const
        rci%xsec_area = rc1%xsec_area + const * (rc2%xsec_area - rc1%xsec_area)
        rci%surf_area = rc1%surf_area + const * (rc2%surf_area - rc1%surf_area)
        rci%flo_rate = rc1%flo_rate + const * (rc2%flo_rate - rc1%flo_rate)
        rci%dep = rc1%dep + const * (rc2%dep - rc1%dep)
        rci%top_wid = rc1%top_wid + const * (rc2%top_wid - rc1%top_wid)
        rci%vol = rc1%vol + const * (rc2%vol - rc1%vol)
        rci%vol_ch = rc1%vol_ch + const * (rc2%vol_ch - rc1%vol_ch)
        rci%vol_fp = rc1%vol_fp + const * (rc2%vol_fp - rc1%vol_fp)
        rci%wet_perim = rc1%wet_perim + const * (rc2%wet_perim - rc1%wet_perim)
        rci%ttime = rc1%ttime + const * (rc2%ttime - rc1%ttime)
     end subroutine chrc_interp
    
      end module sd_channel_module