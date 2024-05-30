      !module for constituent mass balance in soils (rtb cs)
      module cs_module
      
      implicit none

      type cs_balance
        !constituents = seo4,seo3,boron
        real :: soil = 0.            !! |kg/ha       |total mass in the soil profile
        real :: surq = 0.            !! |kg/ha       |mass lost in surface runoff in HRU
        real :: sedm = 0.            !! |kg/ha       |mass lost in sediment runoff in HRU
        real :: latq = 0.            !! |kg/ha       |mass in lateral flow in HRU
        real :: urbq = 0.            !! |kg/ha			 |mass in urban runoff
        real :: wetq = 0.            !! |kg/ha			 |mass in wetland outflow
        real :: tile = 0.            !! |kg/ha       |mass in tile flow in HRU
        real :: perc = 0.            !! |kg/ha       |mass leached past bottom of soil
        real :: gwup = 0.            !! |kg/ha       |mass from groundwater (to soil profile)
        real :: wtsp = 0.            !! |kg/ha			 |mass in wetland seepage (to soil profile)
        real :: irsw = 0.            !! |kg/ha       |mass applied on soil via surface water irrigation
        real :: irgw = 0.            !! |kg/ha       |mass applied on soil via groundwater irrigation
        real :: irwo = 0.            !! |kg/ha       |mass applied on soil via irrigation from without (wo) the watershed
        real :: rain = 0.            !! |kg/ha       |mass added to soil via rainfall
        real :: dryd = 0.            !! |kg/ha       |mass added to soil via dry atmospheric deposition  
        real :: fert = 0.            !! |kg/ha       |mass added to soil via fertilizer
        real :: uptk = 0.            !! |kg/ha       |mass taken up by crop roots
        real :: rctn = 0.            !! |kg/ha       |mass transferred by chemical reaction
        real :: sorb = 0.            !! |kg/ha       |mass transferred by sorption
        real :: conc = 0.						 !! |mg/L        |concentration in soil water (averaged over all soil layers)
        real :: srbd = 0.            !! |kg/ha       |mass sorbed to soil
        
        !boron terms to add...
        !uptake (already in list; do same as nitrate uptake)
        !act_sta
        !act_bor
        !rsd_act
        !rsd_bor
        !sedm (already in list)
        
      end type cs_balance
      
      type object_cs_balance
        type (cs_balance), dimension (:), allocatable :: cs
      end type object_cs_balance
      
      !soil system fluxes
      type (object_cs_balance), dimension (:), allocatable :: hcsb_d
      type (object_cs_balance), dimension (:), allocatable :: hcsb_m
      type (object_cs_balance), dimension (:), allocatable :: hcsb_y
      type (object_cs_balance), dimension (:), allocatable :: hcsb_a
      
      !routing unit fluxes (summation of all HRU inputs)
      type (object_cs_balance), dimension (:), allocatable :: ru_hru_csb_d
      type (object_cs_balance), dimension (:), allocatable :: ru_hru_csb_m
      type (object_cs_balance), dimension (:), allocatable :: ru_hru_csb_y
      type (object_cs_balance), dimension (:), allocatable :: ru_hru_csb_a
      
      !cs balance arrays
      real :: cs_basin_mo(87) = 0.
      real :: cs_basin_yr(87) = 0.
      real :: cs_basin_aa(87) = 0.
      
      !constituent fertilizer
      type fert_db_cs
        character(len=16) :: fertnm = " "
        real :: seo4 = 0.           !! kg seo4/ha      |fertilizer load of seo4 (kg/ha)
        real :: seo3 = 0.           !! kg seo3/ha      |fertilizer load of seo3 (kg/ha)
        real :: boron = 0.          !! kg boron/ha     |fertilizer load of boron (kg/ha)
      end type fert_db_cs
      type (fert_db_cs), dimension(:),allocatable, save :: fert_cs
      integer :: fert_cs_flag = 0
      
      !cs uptake
      real, dimension(:,:), allocatable :: cs_uptake_kg         !specified daily constituent mass taken up by crop roots (kg/ha)
      integer :: cs_uptake_on                                   !flag for simulating constituent uptake
      
      !urban constituent concentrations
      real, dimension(:,:), allocatable :: cs_urban_conc    !constituent conc in suspended solid load from imp areas (mg cs / kg sed)
      
      !header for daily (basin-wide) cs balance output
      type output_csbal_header
         character (len=8) :: yrc =            "      yr"
         character (len=8) :: mon =            "      mo"
         character (len=8) :: day =            "    jday"  
         !soil profile balance - seo4
         character(len=16) :: latseo4 =        "    latq_seo4" !1
         character(len=16) :: surseo4 =        "    surq_seo4" !2
         character(len=16) :: sedseo4 =        "    sedm_seo4" !3
         character(len=16) :: urbseo4 =        "    urbq_seo4" !4
         character(len=16) :: wetseo4 =        "    wetq_seo4" !5
         character(len=16) :: tileseo4 =       "    tile_seo4" !6
         character(len=16) :: percseo4 =       "    perc_seo4" !7
         character(len=16) :: gwupseo4 =       "    gwup_seo4" !8
         character(len=16) :: wtspseo4 =       "    wtsp_seo4" !9
         character(len=16) :: irswseo4 =       "    irsw_seo4" !10
         character(len=16) :: irgwseo4 =       "    irgw_seo4" !11
         character(len=16) :: irwoseo4 =       "    irwo_seo4" !12
         character(len=16) :: rainseo4 =       "    rain_seo4" !13
         character(len=16) :: drydseo4 =       "    dryd_seo4" !14
         character(len=16) :: fertseo4 =       "    fert_seo4" !15
         character(len=16) :: uptkseo4 =       "    uptk_seo4" !16
         character(len=16) :: rctnseo4 =       "  rct_sl_seo4" !17
         character(len=16) :: sorbseo4 =       "  srb_sl_seo4" !18
         character(len=16) :: ptsoseo4 =       "    ptso_seo4" !19
         character(len=16) :: poutseo4 =       "   ptout_seo4" !20
         character(len=16) :: sldsseo4 =       "  dis_sl_seo4" !21
         character(len=16) :: srbdseo4 =       "  sbd_sl_seo4" !22
         !aquifer balance - seo4
         character(len=16) :: gwseo4 =         "      gw_seo4" !23
         character(len=16) :: rchgseo4 =       "    rchg_seo4" !24
         character(len=16) :: seepseo4 =       "    seep_seo4" !25
         character(len=16) :: rctaseo4 =       "  rct_aq_seo4" !26
         character(len=16) :: srbaseo4 =       "  srb_aq_seo4" !27
         character(len=16) :: aqdsseo4 =       "  dis_aq_seo4" !28
         character(len=16) :: srdaseo4 =       "  sbd_aq_seo4" !29
         !soil profile balance - seo3
         character(len=16) :: latseo3 =        "    latq_seo3" !30
         character(len=16) :: surseo3 =        "    surq_seo3" !31
         character(len=16) :: sedseo3 =        "    sedm_seo3" !32
         character(len=16) :: urbseo3 =        "    urbq_seo3" !33
         character(len=16) :: wetseo3 =        "    wetq_seo3" !34
         character(len=16) :: tileseo3 =       "    tile_seo3" !35
         character(len=16) :: percseo3 =       "    perc_seo3" !36
         character(len=16) :: gwupseo3 =       "    gwup_seo3" !37
         character(len=16) :: wtspseo3 =       "    wtsp_seo3" !38
         character(len=16) :: irswseo3 =       "    irsw_seo3" !39
         character(len=16) :: irgwseo3 =       "    irgw_seo3" !40
         character(len=16) :: irwoseo3 =       "    irwo_seo3" !41
         character(len=16) :: rainseo3 =       "    rain_seo3" !42
         character(len=16) :: drydseo3 =       "    dryd_seo3" !43
         character(len=16) :: fertseo3 =       "    fert_seo3" !44
         character(len=16) :: uptkseo3 =       "    uptk_seo3" !45
         character(len=16) :: rctnseo3 =       "  rct_sl_seo3" !46
         character(len=16) :: sorbseo3 =       "  srb_sl_seo3" !47
         character(len=16) :: ptsoseo3 =       "    ptso_seo3" !48
         character(len=16) :: poutseo3 =       "   ptout_seo3" !49
         character(len=16) :: sldsseo3 =       "  dis_sl_seo3" !50
         character(len=16) :: srbdseo3 =       "  sbd_sl_seo3" !51
         !aquifer balance - seo3
         character(len=16) :: gwseo3 =         "      gw_seo3" !52
         character(len=16) :: rchgseo3 =       "    rchg_seo3" !53 
         character(len=16) :: seepseo3 =       "    seep_seo3" !54
         character(len=16) :: rctaseo3 =       "  rct_aq_seo3" !55
         character(len=16) :: srbaseo3 =       "  srb_aq_seo3" !56
         character(len=16) :: aqdsseo3 =       "  dis_aq_seo3" !57
         character(len=16) :: srdaseo3 =       "  sbd_aq_seo3" !58
         !soil profile balance - boron
         character(len=16) :: latborn =        "    latq_born" !59
         character(len=16) :: surborn =        "    surq_born" !60
         character(len=16) :: sedborn =        "    sedm_born" !61
         character(len=16) :: urbborn =        "    urbq_born" !62
         character(len=16) :: wetborn =        "    wetq_born" !63
         character(len=16) :: tileborn =       "    tile_born" !64
         character(len=16) :: percborn =       "    perc_born" !65
         character(len=16) :: gwupborn =       "    gwup_born" !66
         character(len=16) :: wtspborn =       "    wtsp_born" !67
         character(len=16) :: irswborn =       "    irsw_born" !68
         character(len=16) :: irgwborn =       "    irgw_born" !69
         character(len=16) :: irwoborn =       "    irwo_born" !70
         character(len=16) :: rainborn =       "    rain_born" !71
         character(len=16) :: drydborn =       "    dryd_born" !72
         character(len=16) :: fertborn =       "    fert_born" !73
         character(len=16) :: uptkborn =       "    uptk_born" !74
         character(len=16) :: rctnborn =       "  rct_sl_born" !75
         character(len=16) :: sorbborn =       "  srb_sl_born" !76
         character(len=16) :: ptsoborn =       "    ptso_born" !77
         character(len=16) :: poutborn =       "   ptout_born" !78
         character(len=16) :: sldsborn =       "  dis_sl_born" !79
         character(len=16) :: srbdborn =       "  sbd_sl_born" !80
         !aquifer balance - boron
         character(len=16) :: gwborn =         "      gw_born" !81
         character(len=16) :: rchgborn =       "    rchg_born" !82 
         character(len=16) :: seepborn =       "    seep_born" !83
         character(len=16) :: rctaborn =       "  rct_aq_born" !84
         character(len=16) :: srbaborn =       "  srb_aq_born" !85
         character(len=16) :: aqdsborn =       "  dis_aq_born" !86
         character(len=16) :: srdaborn =       "  sbd_aq_born" !87
      end type output_csbal_header      
      type (output_csbal_header) :: csb_hdr
      
      !header for daily, monthly, yearly, average annual HRU cs output
      type output_cs_hdr_hru
         character (len=6) :: day =        "  jday"
         character (len=6) :: mo =         "   mon"
         character (len=6) :: day_mo =     "   day"
         character (len=6) :: yrc =        "    yr"
         character (len=8) :: isd =        "   unit "
         character (len=12) :: id =        " gis_id "
         !total cs in soil profile (solution; sorbed)
         character(len=15) :: seo4sl =      "sl_seo4"
         character(len=15) :: seo3sl =      "sl_seo3"
         character(len=15) :: bornsl =      "sl_born"
         !surface runoff
         character(len=15) :: seo4sq =      "surq_seo4"
         character(len=15) :: seo3sq =      "surq_seo3"
         character(len=15) :: bornsq =      "surq_born"
         !sediment runoff
         character(len=15) :: seo4sd =      "sedm_seo4"
         character(len=15) :: seo3sd =      "sedm_seo3"
         character(len=15) :: bornsd =      "sedm_born"
         !lateral flow
         character(len=15) :: seo4lq =      "latq_seo4"
         character(len=15) :: seo3lq =      "latq_seo3"
         character(len=15) :: bornlq =      "latq_born"
         !urban sediment runoff
         character(len=15) :: seo4ub =      "urbq_seo4"
         character(len=15) :: seo3ub =      "urbq_seo3"
         character(len=15) :: bornub =      "urbq_born"
         !wetland outflow
         character(len=15) :: seo4wt =      "wetq_seo4"
         character(len=15) :: seo3wt =      "wetq_seo3"
         character(len=15) :: bornwt =      "wetq_born"
         !tile flow
         character(len=15) :: seo4tq =      "tile_seo4"
         character(len=15) :: seo3tq =      "tile_seo3"
         character(len=15) :: borntq =      "tile_born"
         !percolation
         character(len=15) :: seo4pc =      "perc_seo4"
         character(len=15) :: seo3pc =      "perc_seo3"
         character(len=15) :: bornpc =      "perc_born"
         !groundwater transfer
         character(len=15) :: seo4gt =      "gwup_seo4"
         character(len=15) :: seo3gt =      "gwup_seo3"
         character(len=15) :: borngt =      "gwup_born"
         !wetland seepage
         character(len=15) :: seo4ws =      "wtsp_seo4"
         character(len=15) :: seo3ws =      "wtsp_seo3"
         character(len=15) :: bornws =      "wtsp_born"
         !irrigation (surface water)
         character(len=15) :: seo4is =      "irsw_seo4"
         character(len=15) :: seo3is =      "irsw_seo3"
         character(len=15) :: bornis =      "irsw_born"
         !irrigation (groundwater)
         character(len=15) :: seo4ig =      "irgw_seo4"
         character(len=15) :: seo3ig =      "irgw_seo3"
         character(len=15) :: bornig =      "irgw_born"
         !irrigation (outside watershed)
         character(len=15) :: seo4io =      "irwo_seo4"
         character(len=15) :: seo3io =      "irwo_seo3"
         character(len=15) :: bornio =      "irwo_born"
         !rainfall (wet deposition)
         character(len=15) :: seo4rn =      "rain_seo4"
         character(len=15) :: seo3rn =      "rain_seo3"
         character(len=15) :: bornrn =      "rain_born"
         !dry deposition
         character(len=15) :: seo4dd =      "dryd_seo4"
         character(len=15) :: seo3dd =      "dryd_seo3"
         character(len=15) :: borndd =      "dryd_born"
         !fertilizer
         character(len=15) :: seo4fz =      "fert_seo4"
         character(len=15) :: seo3fz =      "fert_seo3"
         character(len=15) :: bornfz =      "fert_born"
         !cs uptake
         character(len=15) :: seo4up =      "uptk_seo4"
         character(len=15) :: seo3up =      "uptk_seo3"
         character(len=15) :: bornup =      "uptk_born"
         !cs chemial reactions
         character(len=15) :: seo4rc =      "rctn_seo4"
         character(len=15) :: seo3rc =      "rctn_seo3"
         character(len=15) :: bornrc =      "rctn_born"
         !cs sorption
         character(len=15) :: seo4sp =      "sorb_seo4"
         character(len=15) :: seo3sp =      "sorb_seo3"
         character(len=15) :: bornsp =      "sorb_born"
         !soil water concentration (averaged over layers)
         character(len=15) :: seo4c =       "conc_seo4"
         character(len=15) :: seo3c =       "conc_seo3"
         character(len=15) :: bornc =       "conc_born"
         !sorbed mass (total over layers)
         character(len=15) :: seo4srbd =    "seo4_srbd"
         character(len=15) :: seo3srbd =    "seo3_srbd"
         character(len=15) :: bornsrbd =    "born_srbd"
      end type output_cs_hdr_hru      
      type (output_cs_hdr_hru) :: cs_hdr_hru
      
      
      end module
      
