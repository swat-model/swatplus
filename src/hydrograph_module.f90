      module hydrograph_module
      
      use time_module
      use basin_module
      
      implicit none
      
      integer :: mhyd                                                    !none          |max number of hydrographs
      integer :: mcmd                                                    !              |
      integer :: inum2                                                   !none          |inflow hydrograph storage location number
      integer :: jrch                                                    !none          |reach number
      integer :: jrchq                                                   !              |
      integer :: mrte                                                    !              |
      integer :: ihout                                                   !none          |outflow hydrograph storage location number
      integer :: iwst                                                    !              | 
      integer :: isdch                                                   !              |
      integer :: icmd                                                    !              |
      integer :: ich                                                     !none          |object number 
      integer :: mobj_out                                                !none          |end of loop
      integer :: isd_chsur                                               !              | 
      integer, dimension (:), allocatable :: rcv_sum                     !              |
      integer, dimension (:), allocatable :: dfn_sum                     !              |
      integer, dimension (:), allocatable :: elem_cnt                    !              |
      integer, dimension (:), allocatable :: defunit_num                 !              |
      integer, dimension (:), allocatable :: ru_seq                      !              |
      real, dimension (:), allocatable :: hyd_km2                        !              |  
      integer, dimension (:), allocatable :: ob_order                    !              |
      real, dimension(:,:,:), allocatable:: rchhr                        !              |
      
      type hyd_output
        real :: flo = 0.               !! m^3           |volume of water
        real :: sed = 0.               !! metric tons   |sediment
        real :: orgn = 0.              !! kg N          |organic N
        real :: sedp = 0.              !! kg P          |organic P
        real :: no3 = 0.               !! kg N          |NO3-N
        real :: solp = 0.              !! kg P          |mineral (soluble P)
        real :: chla = 0.              !! kg            |chlorophyll-a
        real :: nh3 = 0.               !! kg N          |NH3
        real :: no2 = 0.               !! kg N          |NO2
        real :: cbod = 0.              !! kg            |carbonaceous biological oxygen demand
        real :: dox = 0.               !! kg            |dissolved oxygen
        real :: san = 0.               !! tons          |detached sand
        real :: sil = 0.               !! tons          |detached silt
        real :: cla = 0.               !! tons          |detached clay
        real :: sag = 0.               !! tons          |detached small ag
        real :: lag = 0.               !! tons          |detached large ag
        real :: grv = 0.               !! tons          |gravel
        real :: temp = 0.              !! deg c         |temperature
      end type hyd_output
      
      !rtb gwflow - hydrograph separation
      type hyd_sep
        real :: flo_surq = 0.          !! m3           |volume of water from surface runoff
        real :: flo_latq = 0.          !! m3           |volume of water from surface runoff
        real :: flo_gwsw = 0.          !! m3           |volume of water from groundwater discharge
        real :: flo_swgw = 0.          !! m3           |volume of water from stream seepage
        real :: flo_satex = 0.         !! m3           |volume of water from saturation excess (high water table; from gwflow module)
        real :: flo_satexsw = 0.       !! m3           |volume of water from saturation excess (saturated profile)
        real :: flo_tile = 0.          !! m3           |volume of water from tile flow
      end type
      
      type (hyd_output), dimension(:),allocatable :: hd
      type (hyd_output), dimension(:), allocatable :: rec_d
      type (hyd_output), dimension(:), allocatable :: rec_m
      type (hyd_output), dimension(:), allocatable :: rec_y
      type (hyd_output), dimension(:), allocatable :: rec_a
      type (hyd_output), dimension(:), allocatable :: srec_d
      type (hyd_output), dimension(:), allocatable :: srec_m
      type (hyd_output), dimension(:), allocatable :: srec_y
      type (hyd_output), dimension(:), allocatable :: srec_a
      type (hyd_output), dimension(:), allocatable :: ru_d
      type (hyd_output), dimension(:), allocatable :: ru_m
      type (hyd_output), dimension(:), allocatable :: ru_y
      type (hyd_output), dimension(:), allocatable :: ru_a
      type (hyd_output) :: brec_d, brec_m, brec_y, brec_a
      type (hyd_output) :: bru_d, bru_m, bru_y, bru_a
      type (hyd_output) :: binhyd_d
      type (hyd_output) :: hz
      type (hyd_output) :: dr1
      type (hyd_output), dimension(:),allocatable :: hcnst
      type (hyd_output), dimension(:),allocatable :: hhr
      type (hyd_output) :: ht1, ht2, ht3, ht4, ht5, delrto
      type (hyd_output) :: fp_dep, ch_dep, bank_ero, bed_ero, ch_trans
      
      !rtb hydrograph separation
      type (hyd_sep) :: hdsep1,hdsep2
      type (hyd_sep), dimension(:),allocatable :: ch_stor_hdsep
      real, dimension(:,:),allocatable :: hyd_sep_array
      
      character(len=16), dimension(:), allocatable :: om_init_name
      
      type (hyd_output), dimension(:),allocatable, target :: aqu
      type (hyd_output), dimension(:),allocatable, target :: res
      type (hyd_output), dimension(:),allocatable, target :: wet
      type (hyd_output), dimension(:),allocatable :: res_om_init
      type (hyd_output), dimension(:),allocatable :: wet_om_init
      type (hyd_output), dimension(:),allocatable :: wet_seep_day !Jaehak 2022 wetland seepage volume
      type (hyd_output) :: resz
      type (hyd_output), pointer :: wbody       !! used for reservoir and wetlands
      
      type (hyd_output), dimension(:),allocatable :: om_init_water
      type (hyd_output), dimension(:),allocatable :: ch_om_water_init
      type (hyd_output), dimension(:),allocatable :: fp_om_water_init
      type (hyd_output), dimension(:),allocatable :: ch_stor            !channel storage - max bankfull
      type (hyd_output), dimension(:),allocatable :: fp_stor            !flood plain storage above wetland emergency
      type (hyd_output), dimension(:),allocatable :: tot_stor           !total - channel + flood plain storage
      type (hyd_output), dimension(:),allocatable :: wet_stor           !wetland storage in flood plain
      type (hyd_output), dimension(:),allocatable :: ch_stor_m
      type (hyd_output), dimension(:),allocatable :: ch_stor_y
      type (hyd_output), dimension(:),allocatable :: ch_stor_a
      type (hyd_output) :: chaz
      
      type (hyd_output), dimension(:), allocatable, save :: res_in_d
      type (hyd_output), dimension(:), allocatable, save :: res_in_m
      type (hyd_output), dimension(:), allocatable, save :: res_in_y
      type (hyd_output), dimension(:), allocatable, save :: res_in_a
      type (hyd_output) :: bres_in_d
      type (hyd_output) :: bres_in_m
      type (hyd_output) :: bres_in_y
      type (hyd_output) :: bres_in_a
      type (hyd_output), dimension(:), allocatable, save :: res_out_d
      type (hyd_output), dimension(:), allocatable, save :: res_out_m
      type (hyd_output), dimension(:), allocatable, save :: res_out_y
      type (hyd_output), dimension(:), allocatable, save :: res_out_a
      type (hyd_output) :: bres
      type (hyd_output) :: bres_out_d
      type (hyd_output) :: bres_out_m
      type (hyd_output) :: bres_out_y
      type (hyd_output) :: bres_out_a
      type (hyd_output) :: resmz
            
      type (hyd_output), dimension(:), allocatable, save :: wet_in_d
      type (hyd_output), dimension(:), allocatable, save :: wet_in_m
      type (hyd_output), dimension(:), allocatable, save :: wet_in_y
      type (hyd_output), dimension(:), allocatable, save :: wet_in_a
      type (hyd_output) :: bwet_in_d
      type (hyd_output) :: bwet_in_m
      type (hyd_output) :: bwet_in_y
      type (hyd_output) :: bwet_in_a
      type (hyd_output), dimension(:), allocatable, save :: wet_out_d
      type (hyd_output), dimension(:), allocatable, save :: wet_out_m
      type (hyd_output), dimension(:), allocatable, save :: wet_out_y
      type (hyd_output), dimension(:), allocatable, save :: wet_out_a
      type (hyd_output) :: bwet_out_d
      type (hyd_output) :: bwet_out_m
      type (hyd_output) :: bwet_out_y
      type (hyd_output) :: bwet_out_a
      
      type (hyd_output), dimension(:), allocatable, save :: ch_in_d
      type (hyd_output), dimension(:), allocatable, save :: ch_in_m
      type (hyd_output), dimension(:), allocatable, save :: ch_in_y
      type (hyd_output), dimension(:), allocatable, save :: ch_in_a
      type (hyd_output) :: bch_stor_d
      type (hyd_output) :: bch_stor_m
      type (hyd_output) :: bch_stor_y
      type (hyd_output) :: bch_stor_a
      type (hyd_output) :: bch_in_d
      type (hyd_output) :: bch_in_m
      type (hyd_output) :: bch_in_y
      type (hyd_output) :: bch_in_a
      type (hyd_output), dimension(:), allocatable, save :: ch_out_d
      type (hyd_output), dimension(:), allocatable, save :: ch_out_m
      type (hyd_output), dimension(:), allocatable, save :: ch_out_y
      type (hyd_output), dimension(:), allocatable, save :: ch_out_a
      type (hyd_output) :: bch_out_d
      type (hyd_output) :: bch_out_m
      type (hyd_output) :: bch_out_y
      type (hyd_output) :: bch_out_a
      type (hyd_output) :: chomz

      type object_output
        character (len=3) :: name
        character (len=3) :: obtyp     !! object type: hru,hlt,hs,rxc,dr,out,sdc
        integer :: obtypno             !! object type number: 1=hru, 2=hru_lte, 3=channel
        character (len=6) :: hydtyp    !! hydrograph type: tot,rhg,sur,lat,til
        integer :: objno               !! object number
        integer :: hydno               !! code computes from hydtyp
        character (len=26) :: filename !! file with hydrograph output from the object
        integer :: unitno = 6100       !! filename unit number
      end type object_output
      type (object_output), dimension (:), allocatable :: ob_out
      
      type channel_floodplain_water_balance
        real :: inflo               !! m3       | inflow
        real :: outflo              !! m3       | outflow
        real :: tl                  !! m3       | transmission losses
        real :: ev                  !! m3       | evaporation
        real :: ch_stor_init        !! m3       | channel storage at start of time step
        real :: ch_stor             !! m3       | channel storage at end of time step
        real :: fp_stor_init        !! m3       | flood plain storage at start of time step (all flood plain storage above wetland emergency volume)
        real :: fp_stor             !! m3       | flood plain storage at end of time step
        real :: tot_stor_init       !! m3       | total channel + wetland storage at start of time step
        real :: tot_stor            !! m3       | total channel + wetland storage at end of time step
        real :: wet_stor_init       !! m3       | wetland flood plain storage at start of time step
        real :: wet_stor            !! m3       | wetland flood plain storage at end of time step
      end type channel_floodplain_water_balance
      type (channel_floodplain_water_balance), dimension (:), allocatable :: ch_fp_wb
      
      type timestep
        type (hyd_output), dimension(:),allocatable :: hh
      end type timestep
      type (timestep), dimension(:), allocatable, save :: ts

      type sorted_duration_curve
        !linked list to sort the flow duration curves
        real :: val = 0.
        integer :: next = 0
      end type sorted_duration_curve
      
      type duration_curve_points
        real :: min = 1.e10
        real :: max = 0.
        real :: mean = 0
        real, dimension (27) :: p        !probabilities for all points on the fdc
      end type duration_curve_points

      type water_temperature_data
        character(len=16) :: name
        real :: sno_mlt = 1.        ! none          |coefficient influencing snowmelt temperature contributions
        real :: gw = .97            ! none          |coefficient influencing groundwater temperature contributions
        real :: sur_lat = 1.        ! none          |coefficient influencing suface and lateral flow temperature contributions
        integer :: airlag_d = 6     ! days          |average air temperature lag
        real :: hex_coef1 = .67     ! 1/hour        |heat transfer coefficient 1
        real :: hex_coef2 = 1.16    ! 1/hour        |heat transfer coefficient 2
      end type water_temperature_data
      type (water_temperature_data) :: w_temp

      integer :: fdc_npts = 27
      real, dimension (27) :: fdc_p = (/.1,.5,1.,2.,3.,5.,10.,15.,20.,25.,30.,35.,40.,45.,50.,55.,60.,65.,70.,75.,80.,85.,90.,95.,&
                                        97.,98.,99./) !percent        |output percent on the fdc (input)
      integer, dimension (27) :: fdc_days = (/1,2,4,7,11,18,37,55,73,91,110,128,146,164,182,201,219,237,256,274,292,310,329,347,&
                                             354,358,361/)
      real, dimension (27) :: fdc_n             !               |flow on the fdc at given percents
      real, dimension (27) :: fdc_norm_mean     !               |normalized flow on the fdc at given percents
      
      type flow_duration_curve
        integer :: mfe = 1
        integer :: mle = 1
        type (duration_curve_points) :: p_md                                !median of all years
        type (duration_curve_points), dimension (:), allocatable :: yr      !flow on the fdc at given percents for each year
      end type flow_duration_curve
               
      type inflow_unit_hyds
        !need for incoming hru or ru that are a fraction of the hru or ru
        real, dimension (:,:), allocatable :: uh                  !unit hydrograph
        real, dimension (:,:), allocatable :: hyd_flo             !flow hydrograph
      end type inflow_unit_hyds
               
      type flashiness_index
        !flashiness index sum ((qi)-q(i-1)) / sum (qi)
        real :: sum_q_q1            !sum of difference in current day flow minus previous day flow
        real :: sum_q               !sum of daily flow over simulation period
        real :: q_prev = 0.         !previous day flow
        real :: index               !index
      end type flashiness_index
               
      type object_connectivity
        character(len=16) :: name = "default"
        character(len=8) :: typ = " "   !object type - ie hru, hru_lte, sub, chan, res, recall
        integer :: nhyds                !hru=5, chan=3 - see type hd_tot for each object
        real :: lat                     !latitude (degrees)
        real :: long                    !longitude (degrees)
        real :: elev = 100.             !elevation (m)
        real :: plaps                   !precipitation lapse applied to object precip
        real :: tlaps                   !temperature lapse applied to object precip
        real :: area_ha = 80.           !input drainag area - ha
        integer :: sp_ob_no = 1         !spatial object number - ie: hru number, channel number, etc
        real :: area_ha_calc = 80.      !calculated drainage area-ha. only for checking - doesn't work if routing across landscape
        integer :: props = 1            !properties number from data base (ie hru.dat, sub.dat) - change props to data
        character (len=50) ::  wst_c    !weather station name
        integer :: wst = 1              !weather station number
        integer :: constit              !constituent data pointer to pesticides, pathogens, metals, salts
        integer :: props2               !overbank connectivity pointer to landscape units - change props2 to overbank
        character(len=16) :: ruleset    !points to the name of the dtbl in flo_con.dtl for out flow control
        integer :: flo_dtbl             !dtbl pointer for flow fraction of hydrograph
        integer :: num = 1              !spatial object number- ie hru number corresponding to sequential command number
                                        !this is the first column in hru_dat (doesn"t have to be sequential)
        integer*8 :: gis_id             !gis number for database purposes
        integer :: fired = 0            !0=not fired; 1=fired off as a command
        integer :: cmd_next = 0         !next command (object) number
        integer :: cmd_prev = 0         !previous command (object) number
        integer :: cmd_order = 0        !1=headwater,2=2nd order,etc
        integer :: src_tot = 0          !total number of outgoing (source) objects
        integer :: rcv_tot = 0          !total number of incoming (receiving) hydrographs
        integer :: dfn_tot = 0          !total number of defining objects (ie hru"s within a subbasin)
        integer :: ru_tot               !number of routing units that contain this object
        integer,  dimension (:), allocatable :: ru                  !subbasin the element is in
        integer :: elem                 !subbasins element number for this object- used for routing over (can only have one)
        integer :: flood_ch_lnk = 0     !channel the landscape unit is linked to
        integer :: flood_ch_elem = 0    !landscape unit number - 1 is nearest to stream
        integer :: flood_frac = 0       !fraction of flood flow assigned to the object
        character (len=3), dimension (:), allocatable :: obtyp_out  !outflow object type (ie 1=hru, 2=sd_hru, 3=sub, 4=chan, etc)
        integer, dimension(:), allocatable :: obtypno_out           !outflow object type name
        integer, dimension(:), allocatable :: obj_out               !outflow object
        character (len=3), dimension (:), allocatable :: htyp_out   !outflow hyd type (ie 1=tot, 2= recharge, 3=surf, etc)
        integer, dimension (:), allocatable :: ihtyp_out            !outflow hyd type (ie 1=tot, 2= recharge, 3=surf, etc)
        real, dimension (:), allocatable :: frac_out                !fraction of hydrograph
        character(len=8), dimension(:), allocatable :: obtyp_in     !inflow object type (ie 1=hru, 2=sd_hru, 3=sub, 4=chan, etc)
        integer, dimension(:), allocatable :: obtypno_in            !inflow object type number
        integer, dimension(:), allocatable :: obj_in
        character (len=3), dimension(:), allocatable :: htyp_in     !inflow hyd type (ie 1=tot, 2= recharge, 3=surf, etc)
        integer, dimension(:), allocatable :: ihtyp_in
        real, dimension(:), allocatable :: frac_in
        integer, dimension(:), allocatable :: rcvob_inhyd           !inflow hydrograph number of recieving object - used for dtbl flow fractions
        type (flow_duration_curve) :: fdc                                   !use for daily flows and then use to get median of annual fdc"s
        type (sorted_duration_curve), dimension(:),allocatable :: fdc_ll    !linked list of daily flow for year - dimensioned to 366
        type (sorted_duration_curve), dimension(:),allocatable :: fdc_lla   !linked list of annual flow for simulation - dimensioned to nbyr
        type (flashiness_index) :: flash_idx                                !flashiness index object
        type (hyd_output) :: hin                                            !inflow hydrograph for surface runon - sum of all inflow hyds
        type (hyd_output) :: hin_sur                                        !inflow hydrograph for surface runoff - sum of all surface inflow hyds
        type (hyd_output) :: hin_lat                                        !inflow hydrograph for lateral soil flow - sum of all lateral inflow hyds
        type (hyd_output) :: hin_til                                        !inflow hydrograph for tile flow - sum of all tile inflow hyds
        type (hyd_output) :: hin_aqu                                        !inflow hydrograph for aquifer flow - sum of all aquifer inflow hyds
        type (hyd_output), dimension(:), allocatable :: hd                  !daily hydrograph (ie 1=tot, 2= recharge, 3=surf, etc)
        type (hyd_output), dimension(:), allocatable :: hd_aa               !ave annual hydrograph for hru for swift (ie 1=tot, 2= recharge, 3=surf, etc)
        type (hyd_output), dimension(:,:), allocatable :: ts                !subdaily hydrographs
        type (inflow_unit_hyds), dimension(:), allocatable :: hin_uh        !inflow unit hydrographs
        real, dimension(:,:), allocatable :: uh                             !subdaily surface runoff unit hydrograph
        real, dimension(:,:), allocatable :: hyd_flo                        !subdaily surface runoff hydrograph
        real, dimension(:),allocatable :: tsin                              !inflow subdaily flow hydrograph
        type (hyd_output) :: trans                                          !water transfer in water allocation
        type (hyd_output) :: hin_tot                                        !total inflow hydrograph to the object
        type (hyd_output) :: hout_tot                                       !total outflow hydrograph to the object
        real :: demand                                                      !water irrigation demand (ha-m)
        integer :: day_cur = 0                                              !current hydrograph day in ts
        integer :: day_max                                                  !maximum number of days to store the hydrograph
        real :: peakrate                                                    !peak flow rate during time step - m3/s
        
        type (hyd_output), dimension(:),allocatable :: hin_d
        type (hyd_output), dimension(:),allocatable :: hin_m
        type (hyd_output), dimension(:),allocatable :: hin_y
        type (hyd_output), dimension(:),allocatable :: hin_a
        type (hyd_output), dimension(:),allocatable :: hout_m
        type (hyd_output), dimension(:),allocatable :: hout_y
        type (hyd_output), dimension(:),allocatable :: hout_a
        type (hyd_output) :: hdep_m
        type (hyd_output) :: hdep_y
        type (hyd_output) :: hdep_a
        
        !rtb gwflow
        type (hyd_sep) :: hdsep,hdsep_in
        
        integer, dimension(:), allocatable :: obj_subs                      !subbasins object number that contain this object
      end type object_connectivity
      type (object_connectivity), dimension(:), allocatable, save :: ob
      
      !water rights elements (objects) within the water rights object
      type water_rights_elements
        integer :: num
        character (len=16) :: ob_typ            !object type - hru, channel, reservoir, etc
        integer :: ob_num                       !object number
        character (len=16) :: irr_typ           !character from irr.ops - was irr demand, minimum flow, flow fraction, etc
        integer :: irr_no                       !irrigation number from irr.ops - walk irr_typ
        real :: amount                          !0 for irr demand; ha-m for min_flo; frac for min_frac
        integer :: rights                       !0-100 scale
      end type water_rights_elements

      !water allocation
      type irrigation_water_transfer
        real :: demand = 0.                     !irrigation demand          |m3
        real :: applied = 0.                    !irrigation applied         |mm
        real :: runoff = 0.                     !irrigation surface runoff  |mm
        real :: eff = 1.                        !irrigation efficiency as a fraction of irrigation. Jaehak 2022
        real :: frac_surq                       !fraction of irrigation lost in runoff flow. Jaehak 2022
        !hyd_output units are in mm and mg/L
        type (hyd_output) :: water              !irrigation water
      end type irrigation_water_transfer
      type (irrigation_water_transfer),dimension(:),allocatable:: irrig         !dimension by hru
      
      !recall hydrograph inputs
      type recall_hydrograph_inputs
        character (len=25) :: name
        integer :: num = 0                    !number of elements
        integer :: typ = -1                   !recall type - 0=subdaily, 1=day, 2=mon, 3=year
        character(len=25) :: filename         !filename
        !hd and hyd_flo units are in cms and mg/L
        type (hyd_output), dimension (:,:), allocatable :: hd   !m3/s for flow  |input total hyd for daily, monthly, annual and exco
        real, dimension (:,:), allocatable :: hyd_flo           !m3/s           |input total flow hyd only for subdaily recall
        integer :: start_yr             !! start year of point source file
        integer :: end_yr               !! end year of point source file
      end type recall_hydrograph_inputs
      type (recall_hydrograph_inputs),dimension(:),allocatable:: recall

      type spatial_objects
        integer :: objs = 0      !number of objects or 1st object command
        integer :: hru = 0       !1-number of hru"s or 1st hru command
        integer :: hru_lte = 0   !2-number of hru_lte"s or 1st hru_lte command
        
        integer :: ru = 0        !3-number of ru"s or 1st ru command
        integer :: gwflow = 0    !4-number of gwflow"s or 1st gwflow command !rtb gwflow
        integer :: aqu = 0       !5-number of aquifer"s or 1st aquifer command
        integer :: chan = 0      !6-number of chan"s or 1st chan command
        integer :: res = 0       !7-number of res"s or 1st res command
        integer :: recall = 0    !8-number of recdays"s or 1st recday command
        integer :: exco = 0      !11-number of exco"s or 1st export coeff command
        integer :: dr = 0        !12-number of dr"s or 1st del ratio command
        integer :: canal = 0     !13-number of canal"s or 1st canal command
        integer :: pump = 0      !14-number of pump"s or 1st pump command
        integer :: outlet = 0    !15-number of outlet"s or 1st outlet command
        integer :: chandeg = 0   !16-number of swat-deg channel"s or 1st swat-deg channel command
        integer :: aqu2d = 0     !17-not currently used (number of 2D aquifer"s or 1st 2D aquifer command)
        integer :: herd = 0      !18-not currently used (number of herds)
        integer :: wro = 0       !19-not currently used (number of water rights)
      end type spatial_objects
      type (spatial_objects) :: sp_ob       !total number of the object
      type (spatial_objects) :: sp_ob1      !first sequential number of the object
            
      type object_total_hydrographs
        integer :: hru = 5          !1=total 2=recharge 3=surface 4=lateral 5= tile
        integer :: hru_lte = 5      !1=total 2=recharge 3=surface 4=lateral 5= tile
        integer :: ru = 5           !1=total 2=recharge 3=surface 4=lateral 5= tile
        integer :: gwflow = 1       !1=total
        integer :: aqu = 2          !1=return flow 2=deep perc
        integer :: chan = 3         !1=total 2=recharge 3=overbank
        integer :: res = 2          !1=total 2=recharge 
        integer :: recall = 1       !1=total
        integer :: exco = 2         !1=surface 2=groundwater
        integer :: dr = 2           !1=surface 2=groundwater
        integer :: pump = 1         !1=total
        integer :: outlet = 1       !1=total
        integer :: chandeg = 3      !1=total 2=recharge 3=overbank
        integer :: aqu2d = 2        !1=return flow 3=deep perc
        integer :: herd = 1
        integer :: wro = 1
      end type object_total_hydrographs
      type (object_total_hydrographs) :: hd_tot
      
      type routing_unit_data
        character(len=16) :: name
        integer :: num_tot
        integer, dimension (:), allocatable :: num             !points to subbasin element (sub_elem)
      end type routing_unit_data
      type (routing_unit_data),dimension(:), allocatable:: ru_def
      
      type routing_unit_elements
        character(len=16) :: name
        integer :: obj = 1              !object number
        character (len=3) :: obtyp      !object type- 1=hru, 2=hru_lte, 11=export coef, etc
        integer :: obtypno = 0          !2-number of hru_lte"s or 1st hru_lte command
        real :: frac = 0                !fraction of element in ru (expansion factor)
        character(len=16) :: dr_name    !name of dr in delratio.del
        type (hyd_output) :: dr         !calculated (or input in delratio.del) dr's for element
      end type routing_unit_elements
      type (routing_unit_elements), dimension(:), allocatable :: ru_elem
      
      integer,  dimension(:), allocatable :: ielem_ru   !sequential counter for ru the hru is in
            
      !channel-surface element linkage for overbank flooding
      type channel_surface_elements
        character(len=16) :: name
        integer :: num = 0                                      !number of elements
        integer :: chnum = 0                                    !channel number
        integer :: resnum = 0
        character (len=3), dimension(:), allocatable :: obtyp   !object type- 1=hru, 2=hru_lte, 11=export coef, etc
        integer, dimension(:), allocatable :: obtypno           !2-number of hru_lte"s or 1st hru_lte command
        
        real, dimension(:), allocatable :: wid                  !maxflood plain width for each element
        real, dimension(:), allocatable :: dep                  !max flood depth for each element
        real, dimension(:), allocatable :: flood_volmx          !max flood volume for each landscape unit
        type (hyd_output), dimension (:), allocatable :: hd     !flood water for each element
      end type channel_surface_elements
      type (channel_surface_elements),dimension(:),allocatable :: ch_sur
      
      !channel data for channel-aquifer linkage for geomorphic base flow model
      type geomorphic_baseflow_channel_data
        !linked list to sort the flow duration curves
        real :: area = 0.           !drainage area of the channel
        real :: len = 0.            !length of the channel
        real :: len_left = 0.       !fraction of chan length left when channel becomes non-contributing
        real :: flo_fr = 0.         !fraction of aquifer baseflow for each channel
      end type geomorphic_baseflow_channel_data
      type (geomorphic_baseflow_channel_data),dimension(:),allocatable :: aqu_cha       !unsorted
      
      !channel-aquifer linkage for geomorphic base flow model
      type channel_aquifer_elements
        character(len=16) :: name
        integer :: num_tot = 0                          !number of elements
        integer, dimension(:), allocatable :: num       !channel numbers
        real :: len_tot                                 !total length of channels in aquifer (km)
        type (hyd_output) :: hd                         !baseflow hydrograph for the aquifer
        type (geomorphic_baseflow_channel_data), dimension(:),allocatable :: ch
      end type channel_aquifer_elements
      type (channel_aquifer_elements),dimension(:),allocatable :: aq_ch         !sorted by drainage area (smallest first)
      
      !delivery ratio - all fractions 
      type (hyd_output), dimension(:), allocatable :: dr          !delivery ratio for objects- chan, res, lu

      !treatment - fraction of flow and ppm 
      type (hyd_output), dimension(:), allocatable :: trt         !wastewater treatment plants

      !export coefficient - m3, t, kg
      type (hyd_output), dimension(:), allocatable :: exco        !export coefficient

      type hyd_header                                       
        character (len=17) :: flo  =    "              flo"      !! ha-m         |volume of water
        character (len=15) :: sed  =    "            sed"        !! metric tons  |sediment
        character (len=15) :: orgn =    "           orgn"        !! kg N         |organic N
        character (len=15) :: sedp =    "           sedp"        !! kg P         |organic P
        character (len=15) :: no3  =    "            no3"        !! kg N         |NO3-N
        character (len=15) :: solp =    "           solp"        !! kg P         |mineral (soluble P)
        character (len=15) :: chla =    "           chla"        !! kg           |chlorophyll-a
        character (len=15) :: nh3  =    "            nh3"        !! kg N         |NH3
        character (len=15) :: no2  =    "            no2"        !! kg N         |NO2
        character (len=15) :: cbod =    "           cbod"        !! kg           |carbonaceous biological oxygen demand
        character (len=15) :: dox  =    "            dox"        !! kg           |dissolved oxygen
        character (len=15) :: san  =    "            san"        !! tons         |detached sand
        character (len=15) :: sil  =    "            sil"        !! tons         |detached silt
        character (len=15) :: cla  =    "            cla"        !! tons         |detached clay
        character (len=15) :: sag  =    "            sag"        !! tons         |detached small ag
        character (len=15) :: lag  =    "            lag"        !! tons         |detached large ag
        character (len=15) :: grv  =    "            grv"        !! tons         |gravel
        character (len=15) :: temp =    "           null"        !! deg c        |temperature
      end type hyd_header
      type (hyd_header) :: hyd_hdr
      
       type hyd_stor_header        
        character (len=17) :: flo_stor  =    "         flo_stor"      !! m^3/s        |water stored at the end of time period       
        character (len=15) :: sed_stor  =    "       sed_stor"        !! metric tons  |sediment stored at the end of time period
        character (len=15) :: orgn_stor =    "      orgn_stor"        !! kg N         |organic N stored at the end of time period
        character (len=15) :: sedp_stor =    "      sedp_stor"        !! kg P         |organic P stored at the end of time period
        character (len=15) :: no3_stor  =    "       no3_stor"        !! kg N         |NO3-N stored at the end of time period
        character (len=15) :: solp_stor =    "      solp_stor"        !! kg P         |mineral (soluble P) stored at end of time period
        character (len=15) :: chla_stor =    "      chla_stor"        !! kg           |chlorophyll-a stored at end of time period
        character (len=15) :: nh3_stor  =    "       nh3_stor"        !! kg N         |NH3-N (ammonium) stored at end of time period
        character (len=15) :: no2_stor  =    "       no2_stor"        !! kg N         |NO2-N (nitrite) stored at end of time period
        character (len=15) :: cbod_stor =    "      cbod_stor"        !! kg           |carbonaceous biological oxygen demand at end of time period
        character (len=15) :: dox_stor  =    "       dox_stor"        !! kg           |dissolved oxygen stored at end of time period
        character (len=15) :: san_stor  =    "       san_stor"        !! tons         |detached sand stored at end of time period
        character (len=15) :: sil_stor  =    "       sil_stor"        !! tons         |detached silt stored at end of time period
        character (len=15) :: cla_stor  =    "       cla_stor"        !! tons         |detached clay stored at end of time period
        character (len=15) :: sag_stor  =    "       sag_stor"        !! tons         |detached small ag stored at end of time period
        character (len=15) :: lag_stor  =    "       lag_stor"        !! tons         |detached large ag stored at end of time period
        character (len=15) :: grv_stor  =    "       grv_stor"        !! tons         |gravel stored at end of time period
        character (len=15) :: temp_stor =    "           null"        !! deg c        |water temperature
      end type hyd_stor_header
      type (hyd_stor_header) :: hyd_stor_hdr
      
      type hyd_in_header        
        character (len=15) :: flo_in  =    "         flo_in"        !! m^3/s        |water in          
        character (len=15) :: sed_in  =    "         sed_in"        !! metric tons  |sediment in
        character (len=15) :: orgn_in =    "        orgn_in"        !! kg N         |organic N in
        character (len=15) :: sedp_in =    "        sedp_in"        !! kg P         |organic P in
        character (len=15) :: no3_in  =    "         no3_in"        !! kg N         |NO3-N (nitrate) in
        character (len=15) :: solp_in =    "        solp_in"        !! kg P         |mineral (soluble P) in
        character (len=15) :: chla_in =    "        chla_in"        !! kg           |chlorophyll-a in
        character (len=15) :: nh3_in  =    "         nh3_in"        !! kg N         |NH3-N (ammonium) in
        character (len=15) :: no2_in  =    "         no2_in"        !! kg N         |NO2-N (nitrate) in
        character (len=15) :: cbod_in =    "        cbod_in"        !! kg           |carbonaceous biological oxygen demand in
        character (len=15) :: dox_in  =    "         dox_in"        !! kg           |dissolved oxygen in
        character (len=15) :: san_in  =    "         san_in"        !! tons         |detached sand in
        character (len=15) :: sil_in  =    "         sil_in"        !! tons         |detached silt in
        character (len=15) :: cla_in  =    "         cla_in"        !! tons         |detached clay in
        character (len=15) :: sag_in  =    "         sag_in"        !! tons         |detached small ag in
        character (len=15) :: lag_in  =    "         lag_in"        !! tons         |detached large ag in
        character (len=15) :: grv_in  =    "         grv_in"        !! tons         |gravel in
        character (len=15) :: temp_in =    "           null"        !! deg c        |temperature in
      end type hyd_in_header
      type (hyd_in_header) :: hyd_in_hdr
      
    type hyd_out_header        
        character (len=15) :: flo_out  =    "        flo_out"        !! m^3/s        |water out      
        character (len=15) :: sed_out  =    "        sed_out"        !! metric tons  |sediment out
        character (len=15) :: orgn_out =    "       orgn_out"        !! kg N         |organic N out
        character (len=15) :: sedp_out =    "       sedp_out"        !! kg P         |organic P out
        character (len=15) :: no3_out  =    "        no3_out"        !! kg N         |NO3-N out
        character (len=15) :: solp_out =    "       solp_out"        !! kg P         |mineral (soluble P) out
        character (len=15) :: chla_out =    "       chla_out"        !! kg           |chlorophyll-a out
        character (len=15) :: nh3_out  =    "        nh3_out"        !! kg N         |NH3-N (ammonium) out
        character (len=15) :: no2_out  =    "        no2_out"        !! kg N         |NO2-N (nitrite) out
        character (len=15) :: cbod_out =    "       cbod_out"        !! kg           |carbonaceous biological oxygen demand out
        character (len=15) :: dox_out  =    "        dox_out"        !! kg           |dissolved oxygen out
        character (len=15) :: san_out  =    "        san_out"        !! tons         |detached sand out
        character (len=15) :: sil_out  =    "        sil_out"        !! tons         |detached silt out
        character (len=15) :: cla_out  =    "        cla_out"        !! tons         |detached clay out
        character (len=15) :: sag_out  =    "        sag_out"        !! tons         |detached small ag out
        character (len=15) :: lag_out  =    "        lag_out"        !! tons         |detached large ag out
        character (len=15) :: grv_out  =    "        grv_out"        !! tons         |gravel out
        character (len=15) :: temp_out =    "           null"        !! deg c        |temperature out
      end type hyd_out_header
      type (hyd_out_header) :: hyd_out_hdr
      
      type hyd_inout_header        
        character (len=15) :: flo_in  =    "         flo_in"         !! m^3/s        |water in   
        character (len=15) :: flo_out  =    "        flo_out"        !! m^3/s        |water out    
        character (len=15) :: sed_in  =    "         sed_in"         !! metric tons  |sediment in
        character (len=15) :: sed_out  =    "        sed_out"        !! metric tons  |sediment out
        character (len=15) :: orgn_in =    "        orgn_in"         !! kg N         |organic N in
        character (len=15) :: orgn_out =    "       orgn_out"        !! kg N         |organic N out
        character (len=15) :: sedp_in =    "        sedp_in"         !! kg P         |organic P in
        character (len=15) :: sedp_out =    "       sedp_out"        !! kg P         |organic P out
        character (len=15) :: no3_in  =    "         no3_in"         !! kg N         |NO3-N (nitrate) in
        character (len=15) :: no3_out  =    "        no3_out"        !! kg N         |NO3-N out
        character (len=15) :: solp_in =    "        solp_in"         !! kg P         |mineral (soluble P) in
        character (len=15) :: solp_out =    "       solp_out"        !! kg P         |mineral (soluble P) out
        character (len=15) :: chla_in =    "        chla_in"         !! kg           |chlorophyll-a in
        character (len=15) :: chla_out =    "       chla_out"        !! kg           |chlorophyll-a out
        character (len=15) :: nh3_in  =    "         nh3_in"         !! kg N         |NH3-N (ammonium) in
        character (len=15) :: nh3_out  =    "        nh3_out"        !! kg N         |NH3-N (ammonium) out
        character (len=15) :: no2_in  =    "         no2_in"         !! kg N         |NO2-N (nitrate) in
        character (len=15) :: no2_out  =    "        no2_out"        !! kg N         |NO2-N (nitrite) out
        character (len=15) :: cbod_in =    "        cbod_in"         !! kg           |carbonaceous biological oxygen demand in
        character (len=15) :: cbod_out =    "       cbod_out"        !! kg           |carbonaceous biological oxygen demand out       
        character (len=15) :: dox_in  =    "         dox_in"         !! kg           |dissolved oxygen in
        character (len=15) :: dox_out  =    "        dox_out"        !! kg           |dissolved oxygen out       
      end type hyd_inout_header
      type (hyd_inout_header) :: hyd_inout_hdr
      
      type wtmp_out_header
        character (len=15) :: water_temp =    "     water_temp"        !! deg c        |temperature
        end type wtmp_out_header
      type (wtmp_out_header) :: wtmp_hdr
         
      type sed_hyd_header        
        character (len=15) :: flo_in  =    "         flo_in"        !! m^3/s        |volume of water 
        character (len=15) :: flo_out  =   "        flo_out"        !! m^3/s        |volume of water  
        character (len=15) :: sed_in  =    "         sed_in"        !! metric tons  |sediment
        character (len=15) :: sed_out  =   "        sed_out"        !! metric tons  |sediment      
        character (len=15) :: orgn_in =    "        orgn_in"        !! kg N         |organic N
        character (len=15) :: orgn_out =   "       orgn_out"        !! kg N         |organic N 
        character (len=15) :: sedp_in =    "        sedp_in"        !! kg P         |organic P
        character (len=15) :: sedp_out =   "       sedp_out"        !! kg P         |organic P
        character (len=15) :: no3_in  =    "         no3_in"        !! kg N         |NO3-N
        character (len=15) :: no3_out  =   "        no3_out"        !! kg N         |NO3-N
        character (len=15) :: solp_in =    "        solp_in"        !! kg P         |mineral (soluble P)
        character (len=15) :: solp_out =   "       solp_out"        !! kg P         |mineral (soluble P)       
        character (len=15) :: chla_in =    "        chla_in"        !! kg           |chlorophyll-a
        character (len=15) :: chla_out =   "       chla_out"        !! kg           |chlorophyll-a
        character (len=15) :: nh3_in  =    "         nh3_in"        !! kg N         |NH3
        character (len=15) :: nh3_out  =   "        nh3_out"        !! kg N         |NH3
        character (len=15) :: no2_in  =    "         no2_in"        !! kg N         |NO2
        character (len=15) :: no2_out  =   "        no2_out"        !! kg N         |NO2
        character (len=15) :: cbod_in =    "        cbod_in"        !! kg           |carbonaceous biological oxygen demand
        character (len=15) :: cbod_out =   "       cbod_out"        !! kg           |carbonaceous biological oxygen demand
        character (len=15) :: dox_in  =    "         dox_in"        !! kg           |dissolved oxygen
        character (len=15) :: dox_out  =   "        dox_out"        !! kg           |dissolved oxygen
        character (len=15) :: temp_in =    "        temp_in"        !! deg c        |temperature
        character (len=15) :: temp_out =   "       temp_out"        !! deg c        |temperature        
      end type sed_hyd_header
      type (sed_hyd_header) :: sd_hyd_hdr
     type sd_hyd_header_units
        character (len=15) :: flo_in    =  "          m^3/s"        !! avg daily m^3/s        |volume of water
        character (len=15) :: flo_out   =  "          m^3/s"        !! avg daily m^3/s        |volume of water
        character (len=15) :: sed_in    =  "           tons"        !! metric tons  |sediment
        character (len=15) :: sed_out   =  "           tons"        !! metric tons  |sediment
        character (len=15) :: orgn_in   =  "            kgN"        !! kg N         |organic N
        character (len=15) :: orgn_out  =  "            kgN"        !! kg N         |organic N
        character (len=15) :: sedp_in   =  "            kgP"        !! kg P         |organic P
        character (len=15) :: sedp_out  =  "            kgP"        !! kg P         |organic P
        character (len=15) :: no3_in    =  "            kgN"        !! kg N         |NO3-N
        character (len=15) :: no3_out   =  "            kgN"        !! kg N         |NO3-N
        character (len=15) :: solp_in   =  "            kgP"        !! kg P         |mineral (soluble P)
        character (len=15) :: solp_out  =  "            kgP"        !! kg P         |mineral (soluble P)
        character (len=15) :: chla_in   =  "             kg"        !! kg           |chlorophyll-a
        character (len=15) :: chla_out  =  "             kg"        !! kg           |chlorophyll-a
        character (len=15) :: nh3_in    =  "            kgN"        !! kg N         |NH3
        character (len=15) :: nh3_out   =  "            kgN"        !! kg N         |NH3
        character (len=15) :: no2_in    =  "            kgN"        !! kg N         |NO2
        character (len=15) :: no2_out   =  "            kgN"        !! kg N         |NO2
        character (len=15) :: cbod_in   =  "             kg"        !! kg           |carbonaceous biological oxygen demand
        character (len=15) :: cbod_out  =  "             kg"        !! kg           |carbonaceous biological oxygen demand
        character (len=15) :: dox_in    =  "             kg"        !! kg           |dissolved oxygen
        character (len=15) :: dox_out   =  "             kg"        !! kg           |dissolved oxygen
        character (len=15) :: temp_in   =  "           degc"        !! deg c        |temperature
        character (len=15) :: temp_out  =  "           degc"        !! deg c        |temperature
      end type sd_hyd_header_units
      type (sd_hyd_header_units) :: sd_hyd_hdr_units
      
      type sol_header        
        character (len=15) :: layer1 =  "        st_mm_1"        !!mm H2O       |plant name
        character (len=15) :: layer2 =  "        st_mm_2"        !!mm H2O       |amt of water stored in layer 2              
        character (len=15) :: layer3 =  "        st_mm_3"        !!mm H2O       |amt of water stored in layer 3
        character (len=15) :: layer4 =  "        st_mm_4"        !!mm H2O       |amt of water stored in layer 4      
        character (len=15) :: layer5 =  "        st_mm_5"        !!mm H2O       |amt of water stored in layer 5
        character (len=15) :: layer6 =  "        st_mm_6"        !!mm H2O       |amt of water stored in layer 6 
        character (len=15) :: layer7 =  "        st_mm_7"        !!mm H2O       |amt of water stored in layer 7
        character (len=15) :: layer8 =  "        st_mm_8"        !!mm H2O       |amt of water stored in layer 8
        character (len=15) :: layer9 =  "        st_mm_9"        !!mm H2O       |amt of water stored in layer 9
        character (len=15) :: layer10 = "       st_mm_10"        !!mm H2O       |amt of water stored in layer 10
      end type sol_header
      type (sol_header) :: sol_hdr
      
      type plant_header        
        character (len=15) :: name =  "     name        "       !!none         |plant name 
        character (len=15) :: growing =  "growing"              !!none         |plant growing             
        character (len=15) :: dormant =  "dormant"              !!none         |plant dormant
        character (len=15) :: lai =  "lai"                      !!none         |leaf area index 
        character (len=15) :: can_hgt =  "can_hgt"              !!m            |canopy height 
        character (len=15) :: root_dep =  "root_dep"            !!m            |root depth 
        character (len=15) :: phuacc =  "phuacc"                !!0-1          |accumulated heat units
        character (len=15) :: tot_m =  "tot_m"                  !!kg/ha        |total biomass 
        character (len=15) :: ab_gr_m =  "ab_gr_m"              !!kg/ha        |above ground biomass
        character (len=15) :: leaf_m =  "leaf_m"                !!kg/ha        |leaf biomass
        character (len=15) :: root_m =  "root_m"                !!kg/ha        |root biomass
        character (len=15) :: stem_m = "stem_m"                 !!kg/ha        |stem biomass
        character (len=15) :: seed_m = "seed_m"                 !!kg/ha        |seed biomass
      end type plant_header
      type (plant_header) :: plt_hdr
      
      type flood_plain_header        
        character (len=15) :: inflo =           "        inflo"     !!m3        | inflow 
        character (len=15) :: outflo =          "       outflo"     !!m3        | outflow             
        character (len=15) :: dormant =         "      dormant"     !!m3        | evaporation
        character (len=15) :: tl =              "           tl"     !!m3        | transmission losses
        character (len=15) :: ev =              "           ev"     !!m3        | evaporation 
        character (len=15) :: ch_stor_init =    " ch_stor_init"     !!m3        | channel storage at start of time step
        character (len=15) :: ch_stor =         "      ch_stor"     !!m3        | channel storage at end of time step
        character (len=15) :: fp_stor_init =    " fp_stor_init"     !!m3        | flood plain storage at start of time step (all flood plain storage above wetland emergency volume)
        character (len=15) :: fp_stor =         "      fp_stor"     !!m3        | flood plain storage at end of time step
        character (len=15) :: tot_stor_init =   "tot_stor_init"     !!m3        | total channel + wetland storage at start of time step
        character (len=15) :: tot_stor =        "     tot_stor"     !!m3        | total channel + wetland storage at end of time step
        character (len=15) :: wet_stor_init =   "wet_stor_init"     !!m3        | wetland flood plain storage at start of time step
        character (len=15) :: wet_stor =        "     wet_stor"     !!m3        | wetland flood plain storage at end of time step
      end type flood_plain_header
      type (flood_plain_header) :: fp_hdr
      
      type ch_watbod_header 
        character (len=6) :: day           = "  jday"       
        character (len=6) :: mo            = "   mon"
        character (len=6) :: day_mo        = "   day"
        character (len=6) :: yrc           = "    yr"       
        character (len=9) :: isd           = "     unit"        
        character (len=9) :: id            = "   gis_id"       
        character (len=15) :: name         = "   name        "   
        character (len=15) :: area_ha      = '          area'
        character (len=15) :: precip       = '        precip'
        character (len=15) :: evap         = '          evap'
        character (len=15) :: seep         = '          seep'
      end type ch_watbod_header
      type (ch_watbod_header) :: ch_wbod_hdr
      
      type ch_watbod_header_units
        character (len=6) :: day           = "      "
        character (len=6) :: mo            = "      "
        character (len=6) :: day_mo        = "      "
        character (len=6) :: yrc           = "      "       
        character (len=9) :: isd           = "         "                                            
        character (len=9) :: id            = "         "       
        character (len=15) :: name         = "              "    
        character (len=15) :: area_ha      = '            ha'
        character (len=15) :: precip       = '           m^3'
        character (len=15) :: evap         = '           m^3'
        character (len=15) :: seep         = '           m^3'
      end type ch_watbod_header_units
      type (ch_watbod_header_units) :: ch_wbod_hdr_units
      
      type ch_watbod_inoutheader 
        character (len=6) :: day           = "  jday"       
        character (len=6) :: mo            = "   mon"
        character (len=6) :: day_mo        = "   day"
        character (len=6) :: yrc           = "    yr"              
        character (len=9) :: id            = "  unit"
        character (len=9) :: gis_id        = " gis_id" 
        character (len=15) :: name         = "   name        "   
      end type ch_watbod_inoutheader
      type (ch_watbod_inoutheader) :: ch_wbod_inouthdr
      
      type ch_watbod_inoutheader_units
        character (len=6) :: day           = "      "
        character (len=6) :: mo            = "      "
        character (len=6) :: day_mo        = "      "
        character (len=6) :: yrc           = "      "                                                 
        character (len=9) :: id            = "         " 
        character (len=9) :: gis_id        = "         " 
        character (len=15) :: name         = "              "    
      end type ch_watbod_inoutheader_units
      type (ch_watbod_inoutheader_units) :: ch_wbod_inouthdr_units
        
      type hyd_header_units1
        character (len=15) :: flo    =  "          m^3/s"        !! m^3/s        |volume of water
        character (len=15) :: sed    =  "           tons"        !! metric tons  |sediment
        character (len=15) :: orgn   =  "            kgN"        !! kg N         |organic N
        character (len=15) :: sedp   =  "            kgP"        !! kg P         |organic P
        character (len=15) :: no3    =  "            kgN"        !! kg N         |NO3-N
        character (len=15) :: solp   =  "            kgP"        !! kg P         |mineral (soluble P)
        character (len=15) :: chla   =  "             kg"        !! kg           |chlorophyll-a
        character (len=15) :: nh3    =  "            kgN"        !! kg N         |NH3
        character (len=15) :: no2    =  "            kgN"        !! kg N         |NO2
        character (len=15) :: cbod   =  "             kg"        !! kg           |carbonaceous biological oxygen demand
        character (len=15) :: dox    =  "             kg"        !! kg           |dissolved oxygen
        character (len=15) :: san    =  "           tons"        !! tons         |detached sand
        character (len=15) :: sil    =  "           tons"        !! tons         |detached silt
        character (len=15) :: cla    =  "           tons"        !! tons         |detached clay
        character (len=15) :: sag    =  "           tons"        !! tons         |detached small ag
        character (len=15) :: lag    =  "           tons"        !! tons         |detached large ag
        character (len=15) :: grv    =  "           tons"        !! tons         |gravel
        character (len=15) :: temp   =  "               "        !! deg c        |temperature
      end type hyd_header_units1
      type (hyd_header_units1) :: hyd_hdr_units1 
         
      type hyd_header_units3
        character (len=15) :: flo    =  "            m^3"        !! m^3          |volume of water
        character (len=15) :: sed    =  "           tons"        !! metric tons  |sediment
        character (len=15) :: orgn   =  "            kgN"        !! kg N         |organic N
        character (len=15) :: sedp   =  "            kgP"        !! kg P         |organic P
        character (len=15) :: no3    =  "            kgN"        !! kg N         |NO3-N
        character (len=15) :: solp   =  "            kgP"        !! kg P         |mineral (soluble P)
        character (len=15) :: chla   =  "             kg"        !! kg           |chlorophyll-a
        character (len=15) :: nh3    =  "            kgN"        !! kg N         |NH3
        character (len=15) :: no2    =  "            kgN"        !! kg N         |NO2
        character (len=15) :: cbod   =  "             kg"        !! kg           |carbonaceous biological oxygen demand
        character (len=15) :: dox    =  "             kg"        !! kg           |dissolved oxygen
        character (len=15) :: san    =  "           tons"        !! tons         |detached sand
        character (len=15) :: sil    =  "           tons"        !! tons         |detached silt
        character (len=15) :: cla    =  "           tons"        !! tons         |detached clay
        character (len=15) :: sag    =  "           tons"        !! tons         |detached small ag
        character (len=15) :: lag    =  "           tons"        !! tons         |detached large ag
        character (len=15) :: grv    =  "           tons"        !! tons         |gravel
        character (len=15) :: temp   =  "               "        !! deg c        |temperature
      end type hyd_header_units3
      type (hyd_header_units3) :: hyd_hdr_units3 

      type hydinout_header_units1
        character (len=15) :: flo_in    =  " av daily m^3/s"        !! avg daily m^3/s        |volume of water
        character (len=15) :: flo_out   =  " av daily m^3/s"        !! avg daily m^3/s        |volume of water
        character (len=15) :: sed_in    =  "           tons"        !! metric tons  |sediment
        character (len=15) :: sed_out   =  "           tons"        !! metric tons  |sediment
        character (len=15) :: orgn_in   =  "            kgN"        !! kg N         |organic N
        character (len=15) :: orgn_out  =  "            kgN"        !! kg N         |organic N
        character (len=15) :: sedp_in   =  "            kgP"        !! kg P         |organic P
        character (len=15) :: sedp_ouy  =  "            kgP"        !! kg P         |organic P
        character (len=15) :: no3_in    =  "            kgN"        !! kg N         |NO3-N
        character (len=15) :: no3_out   =  "            kgN"        !! kg N         |NO3-N
        character (len=15) :: solp_in   =  "            kgP"        !! kg P         |mineral (soluble P)
        character (len=15) :: solp_out  =  "            kgP"        !! kg P         |mineral (soluble P)
        character (len=15) :: chla_in   =  "             kg"        !! kg           |chlorophyll-a
        character (len=15) :: chla_out  =  "             kg"        !! kg           |chlorophyll-a
        character (len=15) :: nh3_in    =  "            kgN"        !! kg N         |NH3
        character (len=15) :: nh3_out   =  "            kgN"        !! kg N         |NH3
        character (len=15) :: no2_in    =  "            kgN"        !! kg N         |NO2
        character (len=15) :: no2_out   =  "            kgN"        !! kg N         |NO2
        character (len=15) :: cbod_in   =  "             kg"        !! kg           |carbonaceous biological oxygen demand
        character (len=15) :: cbod_out  =  "             kg"        !! kg           |carbonaceous biological oxygen demand
        character (len=15) :: dox_in    =  "             kg"        !! kg           |dissolved oxygen
        character (len=15) :: dox_out   =  "             kg"        !! kg           |dissolved oxygen
      end type hydinout_header_units1
      type (hydinout_header_units1) :: hydinout_hdr_units1 
 
      type wtmp_header_units
        character (len=15) :: wtmp   =  "           degc"        !! deg c        |temperature
      end type wtmp_header_units
      type (wtmp_header_units) :: wtmp_units 
      
       type hyd_header_units  !pts (point source)/deposition/ru (routing_unit) files output uses this units header
        character (len=11) :: day    =  "           "
        character (len=12) :: mo     =  "            "
        character (len=12) :: day_mo =  "            "
        character (len=13) :: yrc    =  "            "
        character (len=12) :: name   =  "            "
        character (len=6) :: otype   =  "      "    
        character (len=17) :: flo    =  "            m^3/s"      !! m^3/s        |volume of water
        character (len=15) :: sed    =  "           tons"        !! metric tons  |sediment
        character (len=15) :: orgn   =  "            kgN"        !! kg N         |organic N
        character (len=15) :: sedp   =  "            kgP"        !! kg P         |organic P
        character (len=15) :: no3    =  "            kgN"        !! kg N         |NO3-N
        character (len=15) :: solp   =  "            kgP"        !! kg P         |mineral (soluble P)
        character (len=15) :: chla   =  "             kg"        !! kg           |chlorophyll-a
        character (len=15) :: nh3    =  "            kgN"        !! kg N         |NH3
        character (len=15) :: no2    =  "            kgN"        !! kg N         |NO2
        character (len=15) :: cbod   =  "             kg"        !! kg           |carbonaceous biological oxygen demand
        character (len=15) :: dox    =  "             kg"        !! kg           |dissolved oxygen
        character (len=15) :: san    =  "           tons"        !! tons         |detached sand
        character (len=15) :: sil    =  "           tons"        !! tons         |detached silt
        character (len=15) :: cla    =  "           tons"        !! tons         |detached clay
        character (len=15) :: sag    =  "           tons"        !! tons         |detached small ag
        character (len=15) :: lag    =  "           tons"        !! tons         |detached large ag
        character (len=15) :: grv    =  "           tons"        !! tons         |gravel
        character (len=15) :: temp   =  "           degc"        !! deg c        |temperature
      end type hyd_header_units
      type (hyd_header_units) :: hyd_hdr_units
      
      type hyd_header_units2  !hydin/hydout files uses this units header 
        character (len=11) :: day    =  "           "
        character (len=12) :: mo     =  "            "
        character (len=12) :: day_mo =  "            "
        character (len=13) :: yrc    =  "            "
        character (len=12) :: name   =  "            "
        character (len=6) :: otype   =  "      "    
        character (len=13) :: iotyp  =  "            "
        character (len=9) :: iotypno =  "         "
        character (len=8) :: hydio   =  "        "
        character (len=8) :: objno   =  "        "
        character (len=17) :: flo    =  "            m^3/s"      !! m^3/s          |volume of water
        character (len=15) :: sed    =  "           tons"        !! metric tons  |sediment
        character (len=15) :: orgn   =  "            kgN"        !! kg N         |organic N
        character (len=15) :: sedp   =  "            kgP"        !! kg P         |organic P
        character (len=15) :: no3    =  "            kgN"        !! kg N         |NO3-N
        character (len=15) :: solp   =  "            kgP"        !! kg P         |mineral (soluble P)
        character (len=15) :: chla   =  "             kg"        !! kg           |chlorophyll-a
        character (len=15) :: nh3    =  "            kgN"        !! kg N         |NH3
        character (len=15) :: no2    =  "            kgN"        !! kg N         |NO2
        character (len=15) :: cbod   =  "             kg"        !! kg           |carbonaceous biological oxygen demand
        character (len=15) :: dox    =  "             kg"        !! kg           |dissolved oxygen
        character (len=15) :: san    =  "           tons"        !! tons         |detached sand
        character (len=15) :: sil    =  "           tons"        !! tons         |detached silt
        character (len=15) :: cla    =  "           tons"        !! tons         |detached clay
        character (len=15) :: sag    =  "           tons"        !! tons         |detached small ag
        character (len=15) :: lag    =  "           tons"        !! tons         |detached large ag
        character (len=15) :: grv    =  "           tons"        !! tons         |gravel
        character (len=15) :: temp   =  "           degc"        !! deg c        |temperature
      end type hyd_header_units2
      type (hyd_header_units2) :: hyd_hdr_units2
            
                     
      type hyd_header_time                                       
        character (len=11) :: day    =  "       jday"
        character (len=12) :: mo     =  "         mon"
        character (len=12) :: day_mo =  "         day"
        character (len=13) :: yrc    =  "          yr"
        character (len=12) :: name   =  "  name      "
        character (len=6) :: otype   =  "  type"
      end type hyd_header_time
       type (hyd_header_time) :: hyd_hdr_time
    
      type rec_header_time                                       
        character (len=11) :: day    =  "       jday"
        character (len=12) :: mo     =  "         mon"
        character (len=12) :: day_mo =  "         day"
        character (len=13) :: yrc    =  "          yr"
        character (len=12) :: name   =  "name        "       
        character (len=6) ::  blank  =  "      "
      end type rec_header_time
       type (rec_header_time) :: rec_hdr_time
       
      type hyd_header_obj
        character (len=13) :: iotyp  =  "   objtyp   "
        character (len=9) :: iotypno =  "  typ_no "
        character (len=8) :: hydio   =  "hyd_typ "
        character (len=8) :: objno   =  "fraction"
      end type hyd_header_obj
      type (hyd_header_obj) :: hyd_hdr_obj
      
      type output_flow_duration_header
        character (len=11) :: obtyp =     "ob_typ     "
        character (len=12) :: props =    "    props   "
        character (len=12) :: area  =    "    area_ha "
        character (len=12) :: f_idx =    "  flash_idx "
        character (len=13) :: mean  =     "      mean "
        character (len=11) :: max   =     "       max "
        character (len=18) :: p01   =     "       p.1 "
        character (len=13) :: p05   =     "       p.5 "     
        character (len=19) :: p1    =     "        p1 "
        character (len=15) :: p2    =     "        p2 "
        character (len=15) :: p3    =     "        p3 "
        character (len=15) :: p5    =     "        p5 "
        character (len=15) :: p10   =     "       p10 "
        character (len=15) :: p15   =     "       p15 "
        character (len=15) :: p20   =     "       p20 "
        character (len=15) :: p25   =     "       p25 "
        character (len=15) :: p30   =     "       p30 "
        character (len=15) :: p35   =     "       p35 "
        character (len=15) :: p40   =     "       p40 "
        character (len=15) :: p45   =     "       p45 "
        character (len=15) :: p50   =     "       p50 "
        character (len=15) :: p55   =     "       p55 "
        character (len=15) :: p60   =     "       p60 "
        character (len=15) :: p65   =     "       p65 "
        character (len=15) :: p70   =     "       p70 "
        character (len=15) :: p75   =     "       p75 "
        character (len=15) :: p80   =     "       p80 "
        character (len=15) :: p85   =     "       p85 "
        character (len=15) :: p90   =     "       p90 "
        character (len=15) :: p95   =     "       p95 "
        character (len=15) :: p97   =     "       p97 "
        character (len=15) :: p98   =     "       p98 "
        character (len=15) :: p99   =     "       p99 "
        character (len=11) :: min   =     "       min "
      end type output_flow_duration_header    
      type (output_flow_duration_header) :: fdc_hdr
	  
      type calibration_header          
        character (len=16) :: name        =   "     name      "        
        character (len=12) :: ha          =   "     ha     "                                             
        character (len=12) :: nbyr        =   "   nbyr     "
        character (len=12) :: prec        =   "   precip   "
		character (len=16) :: meas        =   "     name      "
		character (len=12) :: srr         =   "    srr     "
		character (len=12) :: lfr         =   "    lfr     "
		character (len=12) :: pcr         =   "    pcr     "
		character (len=12) :: etr         =   "    etr     "
		character (len=12) :: tfr         =   "    tfr     "
		character (len=12) :: sed         =   "    sed     "
		character (len=12) :: orgn        =   "   orgn     "
		character (len=12) :: orgp        =   "   orgp     "
		character (len=12) :: no3         =   "    no3     "
		character (len=12) :: solp        =   "   solp     "
		character (len=16) :: aa          =   "   name     "
		character (len=12) :: srr_aa      =   "    srr     "
		character (len=12) :: lfr_aa      =   "    lfr     "
		character (len=12) :: pcr_aa      =   "    pcr     "
		character (len=12) :: etr_aa      =   "    etr     "
		character (len=12) :: tfr_aa      =   "    tfr     "
		character (len=12) :: sed_aa      =   "    sed     "
		character (len=12) :: orgn_aa     =   "   orgn     "
		character (len=12) :: orgp_aa     =   "   orgp     "
		character (len=12) :: no3_aa      =   "    no3     "
		character (len=12) :: solp_aa     =   "   solp     "
		character (len=12) :: cn_prm_aa   =   "     cn     "
		character (len=12) :: esco        =   "   esco     "
		character (len=12) :: lat_len     =   "lat_len     "
		character (len=12) :: petco       =   "  petco     "
		character (len=12) :: slope       =   "  slope     "
		character (len=12) :: tconc       =   "  tconc     "
		character (len=12) :: etco        =   "   etco     "
		character (len=12) :: perco       =   "  perco     "
		character (len=12) :: revapc      =   "  revapc    "
		character (len=12) :: cn3_swf     =   " cn3_swf    "	
      end type calibration_header    
      type (calibration_header) :: calb_hdr	 
	  
      type calibration2_header         
        character (len=16) :: name     =   "       name "
        character (len=12) :: dakm2    =   "     da_km2 "                                            
        character (len=12) :: cn2      =   "        cn2 "
        character (len=12) :: tc       =   "     tc_min "
        character (len=12) :: soildep  =   " soildep_mm "
        character (len=12) :: perco_co =   "   perco_co "
        character (len=12) :: slope    =   "  slope_m/m "
        character (len=12) :: slopelen =   "   slplen_m "
        character (len=12) :: etco     =   "       etco "
        character (len=12) :: sy       =   "      sy_mm "
        character (len=12) :: abf      =   "        abf "
        character (len=12) :: revapc   =   "     revapc "
        character (len=12) :: percc    =   "      percc "
        character (len=12) :: sw       =   "    sw_frac "
        character (len=12) :: gw       =   "      gw_mm "
        character (len=12) :: gwflow   =   "  gwflow_mm "        
        character (len=12) :: gwdeep   =   "  gwdeep_mm "
        character (len=12) :: snow     =   "    snow_mm "
        character (len=12) :: xlat     =   "       xlat "
        character (len=12) :: itext    =   "      itext "
        character (len=12) :: tropical =   "   tropical "
        character (len=12) :: igrow1   =   "  igrow1_jd "
        character (len=12) :: igrow2   =   "  igrow2_jd "
        character (len=12) :: plant    =   "      plant "
        character (len=12) :: ipet     =   "       ipet "
        character (len=12) :: irr      =   "        irr "
        character (len=12) :: irrsrc   =   "     irrsrc "
        character (len=12) :: tdrain   =   "  tdrain_hr "
        character (len=12) :: uslek    =   "      uslek "
        character (len=12) :: uslec    =   "      uslec "
        character (len=12) :: uslep    =   "      uslep "
        character (len=12) :: uslels   =   "     uslels "
      end type calibration2_header    
      type (calibration2_header) :: calb2_hdr	
      
      type calibration3_header         
        character (len=16) :: name     =   "       name "
        character (len=12) :: chgtyp   =   "    chg_typ "                                            
        character (len=12) :: val      =   "        val "
        character (len=12) :: conds    =   "      conds "
        character (len=12) :: lyr1     =   "       lyr1 "
        character (len=12) :: lyr2     =   "       lyr2 "
        character (len=12) :: year1    =   "      year1 "
        character (len=12) :: year2    =   "      year2 "
        character (len=12) :: day1     =   "       day1 "
        character (len=12) :: day2     =   "       day2 "
        character (len=12) :: objtot   =   "    obj_tot "
      end type calibration3_header    
      type (calibration3_header) :: calb3_hdr
      
      type output_checker_header         
        character (len=16) :: sname =    "                "
        character (len=16) :: hydgrp =   "                "        
        character (len=12) :: zmx    =   "       zmx  "
        character (len=12) :: usle_k  =  "     usle_k "
        character (len=12) :: sumfc   =  "      sumfc "
        character (len=12) :: sumul   =  "      sumul "
        character (len=12) :: usle_p  =  "     usle_p "     
        character (len=12) :: usle_ls =  "    usle_ls "
        character (len=12) :: esco    =  "       esco "
        character (len=12) :: epco    =  "       epco "
        character (len=12) :: cn3_swf =  "    cn3_swf "
        character (len=12) :: perco   =  "      perco "
        character (len=12) :: latq_co =  "    latq_co "
        character (len=12) :: tiledrain ="  tiledrain "
      end type output_checker_header    
      type (output_checker_header) :: chk_hdr
      
  type output_checker_unit         
        character (len=16) :: sname =    "sname           "
        character (len=16) :: hydgrp =   "hydgrp          "       
        character (len=12) :: zmx    =   "       (mm) "
        character (len=12) :: usle_k  =  "            "
        character (len=12) :: sumfc   =  "       (mm) "
        character (len=12) :: sumul   =  "       (mm) "
        character (len=12) :: usle_p  =  "            "     
        character (len=12) :: usle_ls =  "            "
        character (len=12) :: esco    =  "            "
        character (len=12) :: epco    =  "            "
        character (len=12) :: cn3_swf =  "            "
        character (len=12) :: perco   =  "            "
        character (len=12) :: latq_co =  "            "                                          
        character (len=16) :: tiledrain ="0=notile;1=tile;"
      end type output_checker_unit    
      type (output_checker_unit) :: chk_unit
      
      interface operator (+)
        module procedure hydout_add
      end interface
             
      interface operator (-)
        module procedure hydout_subtract
      end interface
            
      interface operator (**)
        module procedure hydout_mult
        end interface 
      
      interface operator (.add.)
        module procedure hydout_add_const
      end interface 

      interface operator (*)
        module procedure hydout_mult_const
      end interface 

      interface operator (/)
        module procedure hydout_div_const
      end interface   
             
      interface operator (//)
        module procedure hydout_div_conv
      end interface   
             
      contains

      !! function to convert mass to concentration
      subroutine hyd_convert_conc_to_mass (hyd1)
        type (hyd_output), intent (inout) :: hyd1
        ! m3/s to m3
        hyd1%flo = hyd1%flo ! * 86400.
        ! t = ppm * m3 / 1000000.
        hyd1%sed = hyd1%sed * hyd1%flo / 1000000.
        ! kg = ppm * m3 / 1000.
        hyd1%orgn = hyd1%orgn * hyd1%flo / 1000.
        hyd1%sedp = hyd1%sedp * hyd1%flo / 1000.
        hyd1%no3 = hyd1%no3 * hyd1%flo / 1000.
        hyd1%solp = hyd1%solp * hyd1%flo / 1000.
        hyd1%chla = hyd1%chla * hyd1%flo / 1000.
        hyd1%nh3 = hyd1%nh3 * hyd1%flo / 1000.
        hyd1%no2 = hyd1%no2 * hyd1%flo / 1000.
        hyd1%cbod = hyd1%cbod * hyd1%flo / 1000.
        hyd1%dox = hyd1%dox * hyd1%flo / 1000.
        hyd1%san = hyd1%san * hyd1%flo / 1000000.
        hyd1%sil = hyd1%sil * hyd1%flo / 1000000.
        hyd1%cla = hyd1%cla * hyd1%flo / 1000000.
        hyd1%sag = hyd1%sag * hyd1%flo / 1000000.
        hyd1%lag = hyd1%lag * hyd1%flo / 1000000.
        hyd1%grv = hyd1%grv * hyd1%flo / 1000000.
      end subroutine hyd_convert_conc_to_mass
      
      !! function to convert concentration to mass
      subroutine res_convert_mass (hyd1, pvol)
        real, intent (in) :: pvol
        type (hyd_output), intent (inout) :: hyd1
        
        hyd1%flo = hyd1%flo * pvol      ! input as frac of principal
        hyd1%sed = hyd1%sed * hyd1%flo / 1000000.   ! t = ppm (g/m3) * 1 t/m3 / 1000000. g/t
        hyd1%orgn = hyd1%orgn * hyd1%flo / 1000.    ! kg = ppm * m3 / 1000.
        hyd1%sedp = hyd1%sedp * hyd1%flo / 1000.
        hyd1%no3 = hyd1%no3 * hyd1%flo / 1000.
        hyd1%solp = hyd1%solp * hyd1%flo / 1000.
        hyd1%chla = hyd1%chla * hyd1%flo / 1000.
        hyd1%nh3 = hyd1%nh3 * hyd1%flo / 1000.
        hyd1%no2 = hyd1%no2 * hyd1%flo / 1000.
        hyd1%cbod = hyd1%cbod * hyd1%flo / 1000.
        hyd1%dox = hyd1%dox * hyd1%flo / 1000.
        hyd1%san = hyd1%san * hyd1%flo / 1000000.
        hyd1%sil = hyd1%sil * hyd1%flo / 1000000.
        hyd1%cla = hyd1%cla * hyd1%flo / 1000000.
        hyd1%sag = hyd1%sag * hyd1%flo / 1000000.
        hyd1%lag = hyd1%lag * hyd1%flo / 1000000.
        hyd1%grv = hyd1%grv * hyd1%flo / 1000000.
      end subroutine res_convert_mass
      
      !! function to convert mass to concentration
      subroutine hyd_convert_mass_to_conc (hyd1)
        type (hyd_output), intent (inout) :: hyd1
        hyd1%flo = hyd1%flo
        ! ppm = 1000000. * t / m3
        if (hyd1%flo > 0.01) then
          hyd1%sed = 1000000. * hyd1%sed / hyd1%flo
          ! ppm = 1000. * kg / m3
          hyd1%orgn = 1000. * hyd1%orgn / hyd1%flo
          hyd1%sedp = 1000. * hyd1%sedp / hyd1%flo
          hyd1%no3 = 1000. * hyd1%no3 / hyd1%flo
          hyd1%solp = 1000. * hyd1%solp / hyd1%flo
          hyd1%chla = 1000. * hyd1%chla / hyd1%flo
          hyd1%nh3 = 1000. * hyd1%nh3 / hyd1%flo
          hyd1%no2 = 1000. * hyd1%no2 / hyd1%flo
          hyd1%cbod = 1000. * hyd1%cbod / hyd1%flo
          hyd1%dox = 1000. * hyd1%dox / hyd1%flo
          hyd1%san = 0.  !1000000. * hyd1%san / hyd1%flo
          hyd1%sil = 0.  !1000000. * hyd1%sil / hyd1%flo
          hyd1%cla = 0.  !1000000. * hyd1%cla / hyd1%flo
          hyd1%sag = 0.  !1000000. * hyd1%sag / hyd1%flo
          hyd1%lag = 0.  !1000000. * hyd1%lag / hyd1%flo
          hyd1%grv = 0.  !1000000. * hyd1%grv / hyd1%flo
        else
          hyd1 = hz
        end if
      end subroutine hyd_convert_mass_to_conc
         
      !! routines for hydrograph module
      function hydout_add (hyd1, hyd2) result (hyd3)
        type (hyd_output), intent (in) :: hyd1
        type (hyd_output), intent (in) :: hyd2
        type (hyd_output) :: hyd3
        hyd3%flo = hyd1%flo + hyd2%flo
        hyd3%sed = hyd1%sed + hyd2%sed        
        hyd3%orgn = hyd1%orgn + hyd2%orgn        
        hyd3%sedp = hyd1%sedp + hyd2%sedp   
        hyd3%no3 = hyd1%no3 + hyd2%no3
        hyd3%solp = hyd1%solp + hyd2%solp
        hyd3%chla = hyd1%chla + hyd2%chla
        hyd3%nh3 = hyd1%nh3 + hyd2%nh3
        hyd3%no2 = hyd1%no2 + hyd2%no2
        hyd3%cbod = hyd1%cbod + hyd2%cbod
        hyd3%dox = hyd1%dox + hyd2%dox
        hyd3%san = hyd1%san + hyd2%san
        hyd3%sil = hyd1%sil + hyd2%sil
        hyd3%cla = hyd1%cla + hyd2%cla
        hyd3%sag = hyd1%sag + hyd2%sag
        hyd3%lag = hyd1%lag + hyd2%lag
        hyd3%grv = hyd1%grv + hyd2%grv
        if (hyd1%flo + hyd2%flo > 1.e-6) then
          hyd3%temp = (hyd1%flo * hyd2%temp + hyd2%flo + hyd2%temp) / (hyd1%flo + hyd2%flo)
        else
          hyd3%temp = 0.
        end if
      end function hydout_add
                     
      !! routines for hydrograph module
      function hydout_subtract (hyd1, hyd2) result (hyd3)
        type (hyd_output), intent (in) :: hyd1
        type (hyd_output), intent (in) :: hyd2
        type (hyd_output) :: hyd3
        hyd3%flo = hyd1%flo - hyd2%flo
        hyd3%sed = hyd1%sed - hyd2%sed        
        hyd3%orgn = hyd1%orgn - hyd2%orgn        
        hyd3%sedp = hyd1%sedp - hyd2%sedp   
        hyd3%no3 = hyd1%no3 - hyd2%no3
        hyd3%solp = hyd1%solp - hyd2%solp
        hyd3%chla = hyd1%chla - hyd2%chla
        hyd3%nh3 = hyd1%nh3 - hyd2%nh3
        hyd3%no2 = hyd1%no2 - hyd2%no2
        hyd3%cbod = hyd1%cbod - hyd2%cbod
        hyd3%dox = hyd1%dox - hyd2%dox
        hyd3%san = hyd1%san - hyd2%san
        hyd3%sil = hyd1%sil - hyd2%sil
        hyd3%cla = hyd1%cla - hyd2%cla
        hyd3%sag = hyd1%sag - hyd2%sag
        hyd3%lag = hyd1%lag - hyd2%lag
        hyd3%grv = hyd1%grv - hyd2%grv
        hyd3%temp = hyd1%temp
      end function hydout_subtract
            
      !! routines for hydrograph module
      function hydout_mult (hyd1, hyd2) result (hyd3)
        type (hyd_output), intent (in) :: hyd1
        type (hyd_output), intent (in) :: hyd2
        type (hyd_output) :: hyd3
        hyd3%flo = hyd1%flo * hyd2%flo
        hyd3%sed = hyd1%sed * hyd2%sed        
        hyd3%orgn = hyd1%orgn * hyd2%orgn        
        hyd3%sedp = hyd1%sedp * hyd2%sedp   
        hyd3%no3 = hyd1%no3 * hyd2%no3
        hyd3%solp = hyd1%solp * hyd2%solp
        hyd3%chla = hyd1%chla * hyd2%chla
        hyd3%nh3 = hyd1%nh3 * hyd2%nh3
        hyd3%no2 = hyd1%no2 * hyd2%no2
        hyd3%cbod = hyd1%cbod * hyd2%cbod
        hyd3%dox = hyd1%dox * hyd2%dox
        hyd3%san = hyd1%san * hyd2%san
        hyd3%sil = hyd1%sil * hyd2%sil
        hyd3%cla = hyd1%cla * hyd2%cla
        hyd3%sag = hyd1%sag * hyd2%sag
        hyd3%lag = hyd1%lag * hyd2%lag
        hyd3%grv = hyd1%grv * hyd2%grv
        hyd3%temp = hyd1%temp
      end function hydout_mult
            
      !! routines for hydrograph module
      function hydout_add_const (const, hyd1) result (hyd2)
        real, intent (in) :: const
        type (hyd_output), intent (in) :: hyd1
        type (hyd_output) :: hyd2
        hyd2%flo = const + hyd1%flo 
        hyd2%sed = const + hyd1%sed       
        hyd2%orgn = const + hyd1%orgn       
        hyd2%sedp = const + hyd1%sedp 
        hyd2%no3 = const + hyd1%no3
        hyd2%solp = const + hyd1%solp
        hyd2%chla = const + hyd1%chla
        hyd2%nh3 = const + hyd1%nh3
        hyd2%no2 = const + hyd1%no2
        hyd2%cbod = const + hyd1%cbod
        hyd2%dox = const + hyd1%dox
        hyd2%san = const + hyd1%san
        hyd2%sil = const + hyd1%sil
        hyd2%cla = const + hyd1%cla
        hyd2%sag = const + hyd1%sag
        hyd2%lag = const + hyd1%lag
        hyd2%grv = const + hyd1%grv
        hyd2%temp = hyd1%temp
      end function hydout_add_const
      
      function hydout_mult_const (const, hyd1) result (hyd2)
        type (hyd_output), intent (in) :: hyd1
        real, intent (in) :: const
        type (hyd_output) :: hyd2
        hyd2%temp = hyd1%temp
        hyd2%flo = const * hyd1%flo 
        hyd2%sed = const * hyd1%sed !/ 1000.
        hyd2%orgn = const * hyd1%orgn       
        hyd2%sedp = const * hyd1%sedp 
        hyd2%no3 = const * hyd1%no3
        hyd2%solp = const * hyd1%solp
        hyd2%chla = const * hyd1%chla
        hyd2%nh3 = const * hyd1%nh3
        hyd2%no2 = const * hyd1%no2
        hyd2%cbod = const * hyd1%cbod
        hyd2%dox = const * hyd1%dox
        hyd2%san = const * hyd1%san
        hyd2%sil = const * hyd1%sil
        hyd2%cla = const * hyd1%cla
        hyd2%sag = const * hyd1%sag
        hyd2%lag = const * hyd1%lag
        hyd2%grv = const * hyd1%grv
        hyd2%temp = hyd1%temp
      end function hydout_mult_const
      
      function hydout_div_const (hyd1,const) result (hyd2)
        type (hyd_output), intent (in) :: hyd1
        real, intent (in) :: const
        type (hyd_output) :: hyd2
        hyd2%flo = hyd1%flo / const
        hyd2%sed = hyd1%sed / const
        hyd2%orgn = hyd1%orgn / const
        hyd2%sedp = hyd1%sedp / const
        hyd2%no3 = hyd1%no3 / const
        hyd2%solp = hyd1%solp / const
        hyd2%chla = hyd1%chla / const
        hyd2%nh3 = hyd1%nh3 / const
        hyd2%no2 = hyd1%no2 / const
        hyd2%cbod = hyd1%cbod / const
        hyd2%dox = hyd1%dox / const
        hyd2%san = hyd1%san / const
        hyd2%sil = hyd1%sil / const
        hyd2%cla = hyd1%cla / const
        hyd2%sag = hyd1%sag / const
        hyd2%lag = hyd1%lag / const
        hyd2%grv = hyd1%grv / const
        hyd2%temp = hyd1%temp
      end function hydout_div_const
            
      !function to divide hyd by another hyd
      function hydout_div_conv (hyd1, hyd2) result (hyd3)
        type (hyd_output), intent (in) :: hyd1
        type (hyd_output), intent (in) :: hyd2
        type (hyd_output) :: hyd3
        if (hyd2%flo > 1.e-6) then
          hyd3%flo = hyd1%flo / hyd2%flo
        else
          hyd3%flo = 0.
        end if
        if (hyd2%sed > 1.e-6) then
          hyd3%sed = hyd1%sed / hyd2%sed
        else
          hyd3%sed = 0.
        end if
        if (hyd2%orgn > 1.e-6) then
          hyd3%orgn = hyd1%orgn / hyd2%orgn
        else
          hyd3%orgn = 0.
        end if
        if (hyd2%sedp > 1.e-6) then
          hyd3%sedp = hyd1%sedp / hyd2%sedp
        else
          hyd3%sedp = 0.
        end if 
        if (hyd2%no3 > 1.e-6) then
          hyd3%no3 = hyd1%no3 / hyd2%no3
        else
          hyd3%no3 = 0.
        end if
        if (hyd2%solp > 1.e-6) then
          hyd3%solp = hyd1%solp / hyd2%solp
        else
          hyd3%solp = 0.
        end if
        if (hyd2%chla > 1.e-6) then
          hyd3%chla = hyd1%chla / hyd2%chla
        else
          hyd3%chla = 0.
        end if
        if (hyd2%nh3 > 1.e-6) then
          hyd3%nh3 = hyd1%nh3 / hyd2%nh3
        else
          hyd3%nh3 = 0.
        end if
        if (hyd2%no2 > 1.e-6) then
          hyd3%no2 = hyd1%no2 / hyd2%no2
        else
          hyd3%no2 = 0.
        end if
        if (hyd2%cbod > 1.e-6) then
          hyd3%cbod = hyd1%cbod / hyd2%cbod
        else
          hyd3%cbod = 0.
        end if
        if (hyd2%dox > 1.e-6) then
          hyd3%dox = hyd1%dox / hyd2%dox
        else
          hyd3%dox = 0.
        end if
        if (hyd2%san > 1.e-6) then
          hyd3%san = hyd1%san / hyd2%san
        else
          hyd3%san = 0.
        end if
        if (hyd2%sil > 1.e-6) then
          hyd3%sil = hyd1%sil / hyd2%sil
        else
          hyd3%sil = 0.
        end if
        if (hyd2%cla > 1.e-6) then
          hyd3%cla = hyd1%cla / hyd2%cla
        else
          hyd3%cla = 0.
        end if
        if (hyd2%sag > 1.e-6) then
          hyd3%sag = hyd1%sag / hyd2%sag
        else
          hyd3%sag = 0.
        end if
        if (hyd2%lag > 1.e-6) then
          hyd3%lag = hyd1%lag / hyd2%lag
        else
          hyd3%lag = 0.
        end if
        if (hyd2%grv > 1.e-6) then
          hyd3%grv = hyd1%grv / hyd2%grv
        else
          hyd3%grv = 0.
        end if
        if (hyd1%flo + hyd2%flo > 1.e-6) then
          hyd3%temp = (hyd1%flo * hyd2%temp + hyd2%flo + hyd2%temp) / (hyd1%flo + hyd2%flo)
        else
          hyd3%temp = 0.
        end if
      end function hydout_div_conv
      
      !function to set dr to a constant
!      function dr_constant (dr1, const)
!        type (hyd_output) :: dr1
!        real, intent (in) :: const
!        dr1%flo = const
!        dr1%sed = const
!        dr1%orgn = const
!        dr1%sedp = const
!        dr1%no3 = const
!        dr1%solp = const
!        dr1%psol = const
!        dr1%psor = const
!        dr1%chla = const
!        dr1%nh3 = const
!        dr1%no2 = const
!        dr1%cbod = const
!        dr1%dox = const
!        dr1%san = const
!        dr1%sil = const
!        dr1%cla = const
!        dr1%sag = const
!        dr1%lag = const
!        dr1%grv = const
!      end function dr_constant
      
      end module hydrograph_module