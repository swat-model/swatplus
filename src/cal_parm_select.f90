      subroutine cal_parm_select (ielem, ly, chg_parm, chg_typ, chg_val, absmin, absmax, num_db)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine finds the current parameter value based on 
!!    user defined change

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    val_cur     |variable      |current parameter value
!!                               |the standard temperature (20 degrees C)
!!    chg         |data type     |contains information on variable change
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      
      use basin_module
      use channel_data_module 
      use reservoir_data_module
      use hru_module, only : hru, isol, cn2, brt, tconc
      use soil_module
      use channel_module
      use conditional_module
      use sd_channel_module
      use reservoir_module
      use aquifer_module
      use hru_lte_module
      use organic_mineral_mass_module
      use hydrograph_module
      use pesticide_data_module
      use plant_module
      use plant_data_module
      use gwflow_module
      
      implicit none

      character(len=16), intent (in) :: chg_parm            !                |               
      character(len=16), intent (in) :: chg_typ             !variable        |type of change (absval, abschg, pctchg)
      real, intent (in) :: chg_val                          !                |      
      real, intent (in) :: absmin                           !                |minimum range for variable 
      real, intent (in) :: absmax                           !                |maximum change for variable
      integer, intent (in) :: ielem                         !                | 
      integer, intent (in) :: num_db                        !                | 
      integer, intent (in) :: ly                            !                |
      integer :: jj = 0                                     !                |soil layer counter
      integer :: ipl = 0                                    !                |soil layer counter
      integer :: ihru = 0                                   !                |hru counter
      integer :: icell                                      !                |gwflow cell counter (rtb)
      real :: exp                                           !                | 
      real :: c_val = 0.                                    !                | 
      real :: abmax = 0.                                    !                | 
      real :: chg_par                                       !variable        |new parameter value
      real :: perc_ln_func = 0.                             !none       |function to convert perco to perc_lim
      real :: rock = 0.                                     !                | 

      select case (chg_parm)

      case ("cn2")
        cn2(ielem) = chg_par (cn2(ielem), chg_typ, chg_val, absmin, absmax)
        call curno (cn2(ielem), ielem)

      !! HRU  
      case ("biomix") 
        hru(ielem)%hyd%biomix = chg_par (hru(ielem)%hyd%biomix,           &
                          chg_typ, chg_val, absmin, absmax)
        
      case ("cn3_swf")
        !! don't change for tile  *********************Mike
        if (hru(ielem)%tiledrain == 0) then
          hru(ielem)%hyd%cn3_swf = chg_par (hru(ielem)%hyd%cn3_swf,         &
                         chg_typ, chg_val, absmin, absmax)
          call curno (cn2(ielem), ielem)
        end if
        
      case ("usle_p")
        hru(ielem)%lumv%usle_p = chg_par (hru(ielem)%lumv%usle_p,         &
                          chg_typ, chg_val, absmin, absmax)
        
      case ("usle_c")
        pldb(ielem)%usle_c = chg_par (pldb(ielem)%usle_c,         &
                          chg_typ, chg_val, absmin, absmax)
        
      case ("ovn")
        hru(ielem)%luse%ovn = chg_par (hru(ielem)%luse%ovn,               &
                          chg_typ, chg_val, absmin, absmax)
        
      case ("elev")
        hru(ielem)%topo%elev = chg_par (hru(ielem)%topo%elev,             & 
                          chg_typ, chg_val, absmin, absmax)
        
      case ("slope")
        hru(ielem)%topo%slope = chg_par (hru(ielem)%topo%slope,           & 
                          chg_typ, chg_val, absmin, absmax)
        
      case ("slope_len")
        hru(ielem)%topo%slope_len = chg_par(hru(ielem)%topo%slope_len,    &
                          chg_typ, chg_val, absmin, absmax)
        
      case ("lat_ttime")
        hru(ielem)%hyd%lat_ttime = chg_par(hru(ielem)%hyd%lat_ttime,      &
                          chg_typ, chg_val, absmin, absmax)
        hru(ielem)%hyd%lat_ttime = 1. - Exp(-1. / hru(ielem)%hyd%lat_ttime)
            
      case ("lat_sed")
        hru(ielem)%hyd%lat_sed = chg_par (hru(ielem)%hyd%lat_sed,         & 
                          chg_typ, chg_val, absmin, absmax)
        
      case ("lat_len")
        hru(ielem)%topo%lat_len = chg_par (hru(ielem)%topo%lat_len,     &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("latq_co")
        hru(ielem)%hyd%latq_co = chg_par (hru(ielem)%hyd%latq_co,       &
                          chg_typ, chg_val, absmin, absmax)
        
      case ("canmx")
        hru(ielem)%hyd%canmx = chg_par (hru(ielem)%hyd%canmx,           & 
                         chg_typ, chg_val, absmin, absmax)
        
      case ("esco")
        hru(ielem)%hyd%esco = chg_par (hru(ielem)%hyd%esco,             & 
                         chg_typ, chg_val, absmin, absmax)
         
      case ("epco")
        hru(ielem)%hyd%epco = chg_par (hru(ielem)%hyd%epco,             & 
                         chg_typ, chg_val, absmin, absmax)             
        !! set epco parameter for each crop
        do ipl = 1, pcom(ielem)%npl
          pcom(ielem)%plcur(ipl)%epco = hru(ielem)%hyd%epco
        end do
        
      case ("erorgn")
        hru(ielem)%hyd%erorgn = chg_par (hru(ielem)%hyd%erorgn,         & 
                         chg_typ, chg_val, absmin, absmax)
        
      case ("erorgp")
        hru(ielem)%hyd%erorgp = chg_par (hru(ielem)%hyd%erorgp,         &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("dis_stream")
        hru(ielem)%topo%dis_stream = chg_par(hru(ielem)%topo%dis_stream,&
                         chg_typ, chg_val, absmin, absmax)
        
      case ("perco")
        !! don't change for tile  *********************Mike
        if (hru(ielem)%tiledrain == 0) then
        hru(ielem)%hyd%perco = chg_par (hru(ielem)%hyd%perco,           &
                         chg_typ, chg_val, absmin, absmax)
        if (hru(ielem)%hyd%perco > 1.e-9) then
          perc_ln_func = 1.0052 * log(-log(hru(ielem)%hyd%perco - 1.e-6)) + 5.6862
          hru(ielem)%hyd%perco_lim = exp(-perc_ln_func)
          hru(ielem)%hyd%perco_lim = amin1 (1., hru(ielem)%hyd%perco_lim)
        else
          hru(ielem)%hyd%perco_lim = 0.
        end if
        end if
                
      case ("petco")
        hru(ielem)%hyd%pet_co = chg_par (hru(ielem)%hyd%pet_co,     &
                         chg_typ, chg_val, absmin, absmax)

      case ("lat_orgn")
        hru(ielem)%hyd%lat_orgn = chg_par (hru(ielem)%hyd%lat_orgn,     &
                         chg_typ, chg_val, absmin, absmax)
      
      case ("lat_orgp")
        hru(ielem)%hyd%lat_orgp = chg_par (hru(ielem)%hyd%lat_orgp,     & 
                         chg_typ, chg_val, absmin, absmax)
        
      case ("field_len")
        hru(ielem)%field%length = chg_par(hru(ielem)%field%length,      &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("field_wid")
        hru(ielem)%field%wid = chg_par(hru(ielem)%field%wid,            &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("field_ang")
        hru(ielem)%field%ang = chg_par(hru(ielem)%field%ang,            &
                         chg_typ, chg_val, absmin, absmax)

       case ("snofall_tmp")
        hru(ielem)%sno%falltmp = chg_par(hru(ielem)%sno%falltmp,        &
                         chg_typ, chg_val, absmin, absmax)
               
      case ("snomelt_tmp")
        hru(ielem)%sno%melttmp = chg_par(hru(ielem)%sno%melttmp,        &
                         chg_typ, chg_val, absmin, absmax)
               
      case ("snomelt_max")
        hru(ielem)%sno%meltmx = chg_par(hru(ielem)%sno%meltmx,        &
                         chg_typ, chg_val, absmin, absmax)
               
      case ("snomelt_min")
        hru(ielem)%sno%meltmn = chg_par(hru(ielem)%sno%meltmn,        &
                         chg_typ, chg_val, absmin, absmax)
               
      case ("snomelt_lag")
        hru(ielem)%sno%timp = chg_par(hru(ielem)%sno%timp,            &
                         chg_typ, chg_val, absmin, absmax)
                     
      case ("tile_dep")
        hru(ielem)%lumv%sdr_dep = chg_par(hru(ielem)%lumv%sdr_dep,      &
                         chg_typ, chg_val, absmin, absmax)
        !! define soil layer the drainage tile is in
        if (hru(ielem)%lumv%sdr_dep > 0) then
          do jj = 1, soil(ielem)%nly
            if (hru(ielem)%lumv%sdr_dep < soil(ielem)%phys(jj)%d) hru(ielem)%lumv%ldrain = jj
            if (hru(ielem)%lumv%sdr_dep < soil(ielem)%phys(jj)%d) exit
          end do
        else
          hru(ielem)%lumv%ldrain = 0
        end if
               
      case ("tile_dtime")
        hru(ielem)%sdr%time = chg_par(hru(ielem)%sdr%time,             &
                         chg_typ, chg_val, absmin, absmax)
        !! setting tile lage time
        if (hru(ielem)%lumv%ldrain > 0 .and. hru(ielem)%sdr%lag > 0.01) then
          hru(ielem)%lumv%tile_ttime = 1. - Exp(-24. / hru(ielem)%sdr%lag)
        else
          hru(ielem)%lumv%tile_ttime = 0.
        end if
               
      case ("tile_lag")
        hru(ielem)%sdr%lag = chg_par(hru(ielem)%sdr%lag,               &
                         chg_typ, chg_val, absmin, absmax)

      case ("tile_rad")
        hru(ielem)%sdr%radius = chg_par(hru(ielem)%sdr%radius,         &
                         chg_typ, chg_val, absmin, absmax)
                      
      case ("tile_dist")
        hru(ielem)%sdr%dist = chg_par(hru(ielem)%sdr%dist,             &
                         chg_typ, chg_val, absmin, absmax)
                      
      case ("tile_drain")
        hru(ielem)%sdr%drain_co = chg_par(hru(ielem)%sdr%drain_co,     &
                         chg_typ, chg_val, absmin, absmax)
                      
      case ("tile_pump")
        hru(ielem)%sdr%pumpcap = chg_par(hru(ielem)%sdr%pumpcap,       &
                         chg_typ, chg_val, absmin, absmax)
                      
      case ("tile_latk")
        hru(ielem)%sdr%lag = chg_par(hru(ielem)%sdr%latksat,           &
                         chg_typ, chg_val, absmin, absmax)
                      
      !! SOL  
      case ("anion_excl")
        soil(ielem)%anion_excl = chg_par(soil(ielem)%anion_excl,         &
                         chg_typ, chg_val, absmin, absmax)
         
      case ("crk")
         soil(ielem)%crk = chg_par(soil(ielem)%crk,                      &
                         chg_typ, chg_val, absmin, absmax)
         
      case ("z")
          soil(ielem)%phys(ly)%d = chg_par(soil(ielem)%phys(ly)%d,     &
                         chg_typ, chg_val, absmin, absmax)
          call soil_awc_init (ielem)
          call curno (cn2(ielem), ielem)
         
      case ("bd")
          soil(ielem)%phys(ly)%bd = chg_par(soil(ielem)%phys(ly)%bd,    &
                         chg_typ, chg_val, absmin, absmax)
          call soil_awc_init (ielem)
          call curno (cn2(ielem), ielem)
         
      case ("awc")
          soil(ielem)%phys(ly)%awc = chg_par(soil(ielem)%phys(ly)%awc,  &
                         chg_typ, chg_val, absmin, absmax)
          call soil_awc_init (ielem)
          call curno (cn2(ielem), ielem)
        
      case ("k")
          soil(ielem)%phys(ly)%k = chg_par(soil(ielem)%phys(ly)%k,      &
                         chg_typ, chg_val, absmin, absmax)
          soil(ielem)%phys(ly)%hk = (soil(ielem)%phys(ly)%ul - soil(ielem)%phys(ly)%fc) / soil(ielem)%phys(ly)%k
          if (soil(ielem)%phys(ly)%hk < 1.) soil(ielem)%phys(ly)%hk = 1.
         
      case ("cbn")
          soil1(ielem)%tot(ly)%c = chg_par(soil1(ielem)%tot(ly)%c,    &
                         chg_typ, chg_val, absmin, absmax)
         
      case ("clay")
          soil(ielem)%phys(ly)%clay = chg_par(soil(ielem)%phys(ly)%clay, &
                         chg_typ, chg_val, absmin, absmax)
          call soil_awc_init (ielem)
          call soil_text_init (ielem)
          call curno (cn2(ielem), ielem)
         
      case ("silt")
          soil(ielem)%phys(ly)%silt = chg_par(soil(ielem)%phys(ly)%silt, &
                         chg_typ, chg_val, absmin, absmax)
          call soil_text_init (ielem)
         
      case ("sand")
          soil(ielem)%phys(ly)%sand = chg_par(soil(ielem)%phys(ly)%sand, &
                         chg_typ, chg_val, absmin, absmax)
          call soil_text_init (ielem)
         
      case ("rock")
          soil(ielem)%phys(ly)%rock = chg_par(soil(ielem)%phys(ly)%rock, &
                         chg_typ, chg_val, absmin, absmax)
          if (ly == 1) then
            rock = Exp(-.053 * soil(ielem)%phys(1)%rock)
            hru(ielem)%lumv%usle_mult = rock * soil(ielem)%ly(1)%usle_k *       &
                                 hru(ielem)%lumv%usle_p * hru(ielem)%lumv%usle_ls * 11.8
          end if

      case ("alb")
          soil(ielem)%ly(ly)%alb = chg_par(soil(ielem)%ly(ly)%alb,       &
                         chg_typ, chg_val, absmin, absmax)

      case ("usle_k")
          soil(ielem)%ly(ly)%usle_k = chg_par(soil(ielem)%ly(ly)%usle_k, &
                         chg_typ, chg_val, absmin, absmax)
          rock = Exp(-.053 * soil(ielem)%phys(1)%rock)
          hru(ielem)%lumv%usle_mult = rock * soil(ielem)%ly(1)%usle_k *       &
                                 hru(ielem)%lumv%usle_p * hru(ielem)%lumv%usle_ls * 11.8

      case ("ec")
          soil(ielem)%ly(ly)%ec = chg_par(soil(ielem)%ly(ly)%ec,         &
                         chg_typ, chg_val, absmin, absmax)
         
      case ("cal")
          soil(ielem)%ly(ly)%cal = chg_par(soil(ielem)%ly(ly)%cal,       &
                         chg_typ, chg_val, absmin, absmax)
      
      case ("ph")
           soil(ielem)%ly(ly)%ph = chg_par(soil(ielem)%ly(ly)%ph,        &
                         chg_typ, chg_val, absmin, absmax)
      case ("cmn_h")
        hru(ielem)%nut%cmn = chg_par(hru(ielem)%nut%cmn,           &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("nperco_h")
        hru(ielem)%nut%nperco = chg_par(hru(ielem)%nut%nperco,                  &
                         chg_typ, chg_val, absmin, absmax)
       
      case ("pperco_h")
        hru(ielem)%nut%pperco = chg_par(hru(ielem)%nut%pperco,                  &
                         chg_typ, chg_val, absmin, absmax)
      case ("phoskd_h")
        hru(ielem)%nut%phoskd = chg_par(hru(ielem)%nut%phoskd,                  &
                         chg_typ, chg_val, absmin, absmax)
      case ("psp_h")
        hru(ielem)%nut%psp = chg_par(hru(ielem)%nut%psp,                        &
                         chg_typ, chg_val, absmin, absmax)
      case ("nperco_lchtile_h")
        hru(ielem)%nut%nperco_lchtile = chg_par(hru(ielem)%nut%nperco_lchtile,  &
                         chg_typ, chg_val, absmin, absmax)                      
       !! BSN
      case ("plaps")
        bsn_prm%plaps = chg_par(bsn_prm%plaps,                         &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("tlaps")
        bsn_prm%tlaps = chg_par(bsn_prm%tlaps,                         &
                         chg_typ, chg_val, absmin, absmax)
                                            
      case ("surlag")
        bsn_prm%surlag = chg_par(bsn_prm%surlag,                         &
                         chg_typ, chg_val, absmin, absmax)
        do ihru = 1, sp_ob%hru
          brt(ihru) = 1. - Exp(-bsn_prm%surlag / tconc(ihru))
        end do
        
      case ("adj_pkr")
        bsn_prm%adj_pkr = chg_par(bsn_prm%adj_pkr,                      &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("prf")
        bsn_prm%prf = chg_par(bsn_prm%prf,                              &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("evrch")
        bsn_prm%evrch = chg_par(bsn_prm%evrch,                          &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("evlai")
        bsn_prm%evlai = chg_par(bsn_prm%evlai,                          &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("ffcb")
        bsn_prm%ffcb = chg_par(bsn_prm%ffcb,                            &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("cmn")
        bsn_prm%cmn = chg_par(bsn_prm%cmn,                              &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("nperco")
        bsn_prm%nperco = chg_par(bsn_prm%nperco,                        &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("pperco")
        bsn_prm%pperco = chg_par(bsn_prm%pperco,                        &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("phoskd")
        bsn_prm%phoskd = chg_par(bsn_prm%phoskd,                        &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("psp")
        bsn_prm%psp = chg_par(bsn_prm%psp,                              &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("rsdco")
        bsn_prm%rsdco = chg_par(bsn_prm%rsdco,                          &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("percop")
        bsn_prm%percop = chg_par(bsn_prm%percop,                        &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("msk_co1")
        bsn_prm%msk_co1= chg_par(bsn_prm%msk_co1,                       &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("msk_co2")
        bsn_prm%msk_co2 = chg_par(bsn_prm%msk_co2,                      &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("msk_x")
        bsn_prm%msk_x = chg_par(bsn_prm%msk_x,                          &
                         chg_typ, chg_val, absmin, absmax)
                    
      case ("nperco_lchtile")
        bsn_prm%msk_x = chg_par(bsn_prm%nperco_lchtile,                          &
                         chg_typ, chg_val, absmin, absmax)                         

      case ("cdn")
        bsn_prm%cdn = chg_par(bsn_prm%cdn,                              &
                         chg_typ, chg_val, absmin, absmax)
         
      case ("tb_adj")
        bsn_prm%tb_adj = chg_par(bsn_prm%tb_adj,                        &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("sdnco")
        bsn_prm%sdnco = chg_par(bsn_prm%sdnco,                          &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("n_updis")
        bsn_prm%n_updis = chg_par(bsn_prm%n_updis,                      &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("rsd_covco")
        bsn_prm%rsd_covco = chg_par(bsn_prm%rsd_covco,                      &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("dorm_hr")
        bsn_prm%dorm_hr = chg_par(bsn_prm%dorm_hr,                      &
                         chg_typ, chg_val, absmin, absmax)

!!     SWQ
      case ("mumax")
          ch_nut(ielem)%mumax = chg_par(ch_nut(ielem)%mumax,                &
                         chg_typ, chg_val, absmin, absmax)
      case ("rs1")
          ch_nut(ielem)%rs1 = chg_par(ch_nut(ielem)%rs1,                &
                         chg_typ, chg_val, absmin, absmax)
         
       case ("rs2")
          ch_nut(ielem)%rs2 = chg_par(ch_nut(ielem)%rs2,                &
                         chg_typ, chg_val, absmin, absmax)
        
       case ("rs3")
          ch_nut(ielem)%rs3 = chg_par(ch_nut(ielem)%rs3,                &
                         chg_typ, chg_val, absmin, absmax) 
  
       case ("rs4")
          ch_nut(ielem)%rs4 = chg_par(ch_nut(ielem)%rs4,                &
                         chg_typ, chg_val, absmin, absmax)
        
       case ("rs5")
          ch_nut(ielem)%rs5 = chg_par(ch_nut(ielem)%rs5,                &
                         chg_typ, chg_val, absmin, absmax)
        
       case ("rs6")
          ch_nut(ielem)%rs6 = chg_par(ch_nut(ielem)%rs6,                &
                         chg_typ, chg_val, absmin, absmax) 
        
       case ("rs7")
          ch_nut(ielem)%rs7 = chg_par(ch_nut(ielem)%rs7,                &
                         chg_typ, chg_val, absmin, absmax)
        
       case ("rk1")
          ch_nut(ielem)%rk1 = chg_par(ch_nut(ielem)%rk1,                &
                         chg_typ, chg_val, absmin, absmax)
        
       case ("rk2")
          ch_nut(ielem)%rk2 = chg_par(ch_nut(ielem)%rk2,                &
                         chg_typ, chg_val, absmin, absmax)
        
       case ("rk3")
          ch_nut(ielem)%rk3 = chg_par(ch_nut(ielem)%rk3,                &
                         chg_typ, chg_val, absmin, absmax) 
        
       case ("rk4")
          ch_nut(ielem)%rk4 = chg_par(ch_nut(ielem)%rk4,                &
                         chg_typ, chg_val, absmin, absmax) 
        
       case ("rk5")
          ch_nut(ielem)%rs2 = chg_par(ch_nut(ielem)%rs2,                &
                         chg_typ, chg_val, absmin, absmax) 
        
       case ("rk6")
          ch_nut(ielem)%rk6 = chg_par(ch_nut(ielem)%rk6,                &
                         chg_typ, chg_val, absmin, absmax)
        
       case ("bc1")
          ch_nut(ielem)%bc1 = chg_par(ch_nut(ielem)%bc1,                &
                         chg_typ, chg_val, absmin, absmax)
        
       case ("bc2")
          ch_nut(ielem)%bc2 = chg_par(ch_nut(ielem)%bc2,                &
                         chg_typ, chg_val, absmin, absmax)
        
       case ("bc3")
          ch_nut(ielem)%bc3 = chg_par(ch_nut(ielem)%bc3,                &
                         chg_typ, chg_val, absmin, absmax)
        
        case ("bc4")
          ch_nut(ielem)%bc4 = chg_par(ch_nut(ielem)%bc4,                &
                         chg_typ, chg_val, absmin, absmax)
          
!!     PST
        case ("pst_koc")
          pestdb(ielem)%koc = chg_par(pestdb(ielem)%koc,                &
                         chg_typ, chg_val, absmin, absmax) 
          
        case ("pst_washoff")
          pestdb(ielem)%washoff = chg_par(pestdb(ielem)%washoff,        &
                         chg_typ, chg_val, absmin, absmax) 
          
        case ("pst_foliar_hlife")
          pestdb(ielem)%foliar_hlife = chg_par(pestdb(ielem)%foliar_hlife, &
                         chg_typ, chg_val, absmin, absmax) 

        case ("pst_soil_hlife")
          pestdb(ielem)%soil_hlife = chg_par(pestdb(ielem)%soil_hlife,    &
                         chg_typ, chg_val, absmin, absmax) 
                    
        case ("pst_solub")
          pestdb(ielem)%solub = chg_par(pestdb(ielem)%solub,    &
                         chg_typ, chg_val, absmin, absmax) 
        
        case ("pst_aq_hlife")
          pestdb(ielem)%aq_hlife = chg_par(pestdb(ielem)%aq_hlife,        &
                         chg_typ, chg_val, absmin, absmax)
        
        case ("pst_aq_volat")
          pestdb(ielem)%aq_volat = chg_par(pestdb(ielem)%aq_volat,        &
                         chg_typ, chg_val, absmin, absmax) 
 
        case ("pst_aq_settle")
          pestdb(ielem)%aq_settle = chg_par(pestdb(ielem)%aq_settle,        &
                         chg_typ, chg_val, absmin, absmax) 
        
        case ("pst_aq_resus")
          pestdb(ielem)%aq_resus = chg_par(pestdb(ielem)%aq_resus,        &
                         chg_typ, chg_val, absmin, absmax)

        case ("pst_ben_hlife")
          pestdb(ielem)%ben_hlife = chg_par(pestdb(ielem)%ben_hlife,  &
                         chg_typ, chg_val, absmin, absmax) 
        
        case ("pst_ben_bury")
          pestdb(ielem)%ben_bury = chg_par(pestdb(ielem)%ben_bury,  &
                         chg_typ, chg_val, absmin, absmax)
        
        case ("pst_ben_act_dep")
          pestdb(ielem)%ben_act_dep = chg_par(pestdb(ielem)%ben_act_dep,  &
                         chg_typ, chg_val, absmin, absmax)
        
!!      channel hydrology and sediment parms
         case ("chw")
            sd_ch(ielem)%chw = chg_par(sd_ch(ielem)%chw,                  &
                         chg_typ, chg_val, absmin, absmax)
       
         case ("chd")
            sd_ch(ielem)%chd = chg_par(sd_ch(ielem)%chd,                  &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("chs")
            sd_ch(ielem)%chs = chg_par(sd_ch(ielem)%chs,                  &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("chl")
            sd_ch(ielem)%chl = chg_par(sd_ch(ielem)%chl,                  &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("chn")
            sd_ch(ielem)%chn = chg_par(sd_ch(ielem)%chn,                  &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("chk")
            sd_ch(ielem)%chk = chg_par(sd_ch(ielem)%chk,                  &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("bank_exp")
            sd_ch(ielem)%bank_exp = chg_par(sd_ch(ielem)%bank_exp,            &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("cov")
            sd_ch(ielem)%cov = chg_par(sd_ch(ielem)%cov,                  &
                        chg_typ, chg_val, absmin, absmax)
            
         case ("vcr_coef")
            sd_ch(ielem)%vcr_coef = chg_par(sd_ch(ielem)%vcr_coef,              &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("d50")
            sd_ch(ielem)%d50 = chg_par(sd_ch(ielem)%d50,  &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("ch_clay")
            sd_ch(ielem)%ch_clay = chg_par(sd_ch(ielem)%ch_clay,            &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("carbon")
            sd_ch(ielem)%carbon = chg_par(sd_ch(ielem)%carbon,            &
                         chg_typ, chg_val, absmin, absmax)     
 
         case ("ch_bd")
            sd_ch(ielem)%ch_bd = chg_par(sd_ch(ielem)%ch_bd, chg_typ, chg_val, absmin, absmax)
        
         case ("chss")
            sd_ch(ielem)%chss = chg_par(sd_ch(ielem)%chss, chg_typ, chg_val, absmin, absmax)
        
         case ("bankfull_flo")
            sd_ch(ielem)%bankfull_flo = chg_par(sd_ch(ielem)%bankfull_flo,      &
                         chg_typ, chg_val, absmin, absmax)
            
         case ("fps")
            sd_ch(ielem)%fps = chg_par(sd_ch(ielem)%fps, chg_typ, chg_val, absmin, absmax)
        
         case ("fpn")
            sd_ch(ielem)%fpn = chg_par(sd_ch(ielem)%fpn, chg_typ, chg_val, absmin, absmax)

         case ("hc_kh")
            sd_ch(ielem)%hc_kh = chg_par(sd_ch(ielem)%hc_kh,            &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("hc_hgt")
            sd_ch(ielem)%hc_hgt = chg_par(sd_ch(ielem)%hc_hgt,            &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("hc_ini")
            sd_ch(ielem)%hc_ini = chg_par(sd_ch(ielem)%hc_ini,            &
                         chg_typ, chg_val, absmin, absmax)
            
         case ("ch_n_conc")
             sd_ch(ielem)%n_conc = chg_par (sd_ch(ielem)%n_conc, chg_typ, chg_val, absmin, absmax)
          
        case ("ch_p_conc")
             sd_ch(ielem)%p_conc = chg_par (sd_ch(ielem)%p_conc, chg_typ, chg_val, absmin, absmax)
          
        case ("ch_p_bio")
             sd_ch(ielem)%p_bio = chg_par (sd_ch(ielem)%p_bio, chg_typ, chg_val, absmin, absmax)
             
         case ("pk_rto")
            sd_ch(ielem)%pk_rto = chg_par(sd_ch(ielem)%pk_rto,            &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("fp_inun_days")
            sd_ch(ielem)%fp_inun_days = chg_par(sd_ch(ielem)%fp_inun_days,            &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("n_setl")
            sd_ch(ielem)%n_setl = chg_par(sd_ch(ielem)%n_setl,            &
                         chg_typ, chg_val, absmin, absmax)
            
         case ("p_setl")
            sd_ch(ielem)%p_setl = chg_par(sd_ch(ielem)%p_setl,            &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("n_sol_part")
            sd_ch(ielem)%n_sol_part = chg_par(sd_ch(ielem)%n_sol_part,            &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("p_sol_part")
            sd_ch(ielem)%p_sol_part = chg_par(sd_ch(ielem)%p_sol_part,            &
                         chg_typ, chg_val, absmin, absmax)
            
         case ("n_dep_enr")
            sd_ch(ielem)%n_dep_enr = chg_par(sd_ch(ielem)%n_dep_enr,            &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("p_dep_enr")
            sd_ch(ielem)%p_dep_enr = chg_par(sd_ch(ielem)%p_dep_enr,            &
                         chg_typ, chg_val, absmin, absmax)
            
         case ("arc_len_fr")
            sd_ch(ielem)%arc_len_fr = chg_par(sd_ch(ielem)%arc_len_fr,            &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("bed_exp")
            sd_ch(ielem)%bed_exp = chg_par(sd_ch(ielem)%bed_exp,            &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("wash_bed_fr")
            sd_ch(ielem)%wash_bed_fr = chg_par(sd_ch(ielem)%wash_bed_fr,            &
                         chg_typ, chg_val, absmin, absmax)
            
      !!RES
         case ("esa")
           res_ob(ielem)%esa = chg_par(res_ob(ielem)%esa,             &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("evol")
           res_ob(ielem)%evol = chg_par(res_ob(ielem)%evol,           &
                         chg_typ, chg_val, absmin, absmax) 
        
         case ("psa")
           res_ob(ielem)%psa = chg_par(res_ob(ielem)%psa,             &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("pvol")
           res_ob(ielem)%pvol = chg_par(res_ob(ielem)%pvol,           &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("nsed")
           res_prm(ielem)%sed%nsed = chg_par(res_prm(ielem)%sed%nsed,           &
                         chg_typ, chg_val, absmin, absmax)
         case ("res_d50")
           res_prm(ielem)%sed%d50 = chg_par(res_prm(ielem)%sed%d50,           &
                         chg_typ, chg_val, absmin, absmax)
           !! d50 -micro meters
           res_prm(ielem)%sed_stlr_co = exp(-0.184 * res_prm(ielem)%sed%d50)
           
         case ("soln_stl_fr")
           res_prm(ielem)%soln_stl_fr = chg_par(res_prm(ielem)%soln_stl_fr,           &
                         chg_typ, chg_val, absmin, absmax)
         case ("solp_stl_fr")
           res_prm(ielem)%solp_stl_fr = chg_par(res_prm(ielem)%solp_stl_fr,           &
                         chg_typ, chg_val, absmin, absmax)
         case ("sed_stlr")
           res_prm(ielem)%sed%sed_stlr = chg_par(res_prm(ielem)%sed%sed_stlr,           &
                         chg_typ, chg_val, absmin, absmax)
           
         case ("velsetlr")
           res_prm(ielem)%sed%velsetlr = chg_par(res_prm(ielem)%sed%velsetlr,           &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("k_res")
           res_hyd(ielem)%k = chg_par(res_hyd(ielem)%k,                 &
                         chg_typ, chg_val, absmin, absmax)

         case ("evrsv")
            res_hyd(ielem)%evrsv = chg_par(res_hyd(ielem)%evrsv,        &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("vol")
            res(ielem)%flo = chg_par(res(ielem)%flo,                    &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("sed")
            res(ielem)%sed = chg_par(res(ielem)%sed,                    &
                         chg_typ, chg_val, absmin, absmax)

         case ("orgp")
            res(ielem)%sedp = chg_par(res(ielem)%sedp,                  &
                          chg_typ, chg_val, absmin, absmax)
       
         case ("orgn")
            res(ielem)%orgn = chg_par(res(ielem)%orgn,                  &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("solp")
            res(ielem)%solp = chg_par(res(ielem)%solp,                  &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("no3")
            res(ielem)%no3 = chg_par(res(ielem)%no3,                    &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("nh3")
            res(ielem)%nh3 = chg_par(res(ielem)%nh3,                    &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("no2")
            res(ielem)%no2 = chg_par(res(ielem)%no2,                    &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("psetlr1")
            res_prm(ielem)%nut%psetlr1 = chg_par(res_prm(ielem)%nut%psetlr1,    &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("psetlr2")
            res_prm(ielem)%nut%psetlr2 = chg_par(res_prm(ielem)%nut%psetlr2,    &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("nsetlr1")
            res_prm(ielem)%nut%nsetlr1 = chg_par(res_prm(ielem)%nut%nsetlr1,    &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("nsetlr2")
            res_prm(ielem)%nut%nsetlr2 = chg_par(res_prm(ielem)%nut%nsetlr2,    &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("nsolr")
            res_prm(ielem)%nut%nsolr = chg_par(res_prm(ielem)%nut%nsolr,        &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("psolr")
            res_prm(ielem)%nut%psolr = chg_par(res_prm(ielem)%nut%psolr,      &
                         chg_typ, chg_val, absmin, absmax)
            
      !! res decision tables
        case ("drawdown_days")
          if (ly <= dtbl_res(ielem)%acts)then
            dtbl_res(ielem)%act(ly)%const = chg_par(dtbl_res(ielem)%act(ly)%const,      &
                         chg_typ, chg_val, absmin, absmax)
          end if
        case ("withdraw_rate")
          if (ly <= dtbl_res(ielem)%acts)then
            dtbl_res(ielem)%act(ly)%const2 = chg_par(dtbl_res(ielem)%act(ly)%const2,    &
                         chg_typ, chg_val, absmin, absmax)
          end if
            
      !!AQU
         case ("flo_init_mm")
            aqu_dat(ielem)%flo = chg_par(aqu_dat(ielem)%flo,                &
                         chg_typ, chg_val, absmin, absmax)
         case ("dep_bot")
            aqu_dat(ielem)%dep_bot = chg_par(aqu_dat(ielem)%dep_bot,        &
                         chg_typ, chg_val, absmin, absmax)

         case ("dep_wt_init")
            aqu_dat(ielem)%dep_wt = chg_par(aqu_dat(ielem)%dep_wt,          &
                         chg_typ, chg_val, absmin, absmax)

         case ("no3_init")
            aqu_dat(ielem)%no3 = chg_par(aqu_dat(ielem)%no3,                &
                         chg_typ, chg_val, absmin, absmax)
            !! convert ppm -> kg    (m3=10*mm*ha)     kg=m3*ppm/1000
            aqu_d(ielem)%no3_st = (10. * aqu_d(ielem)%flo * aqu_prm(ielem)%area_ha) &
                                                       * aqu_dat(ielem)%no3 / 1000.
            
         case ("minp_init")
            aqu_dat(ielem)%minp = chg_par(aqu_dat(ielem)%minp,              &
                         chg_typ, chg_val, absmin, absmax)
               
         case ("cbn_init")
            aqu_dat(ielem)%cbn = chg_par(aqu_dat(ielem)%cbn,                &
                         chg_typ, chg_val, absmin, absmax)
                           
         case ("flo_dist")
            aqu_dat(ielem)%flo_dist = chg_par(aqu_dat(ielem)%flo_dist,      &
                         chg_typ, chg_val, absmin, absmax)
         
         case ("bf_max")
            aqu_dat(ielem)%bf_max = chg_par(aqu_dat(ielem)%bf_max,          &
                         chg_typ, chg_val, absmin, absmax)
   
         case ("alpha")
            aqu_dat(ielem)%alpha = chg_par(aqu_dat(ielem)%alpha,            &
                         chg_typ, chg_val, absmin, absmax)
            aqu_prm(ielem)%alpha_e = Exp(-aqu_dat(ielem)%alpha)

         case ("revap_co")
            aqu_dat(ielem)%revap_co = chg_par(aqu_dat(ielem)%revap_co,      &
                         chg_typ, chg_val, absmin, absmax)
                  
         case ("deep_seep")
            aqu_dat(ielem)%seep = chg_par(aqu_dat(ielem)%seep,              &
                         chg_typ, chg_val, absmin, absmax)
            
         case ("sp_yld")
            aqu_dat(ielem)%spyld = chg_par(aqu_dat(ielem)%spyld,            &
                         chg_typ, chg_val, absmin, absmax)
            aqu_d(ielem)%stor = 1000. * (aqu_dat(ielem)%dep_bot - aqu_d(ielem)%dep_wt) &
                                                                * aqu_dat(ielem)%spyld
                    
         case ("hlife_n")
            aqu_dat(ielem)%hlife_n = chg_par(aqu_dat(ielem)%hlife_n,        &
                         chg_typ, chg_val, absmin, absmax)
            aqu_prm(ielem)%nloss = Exp(-.693 / (aqu_dat(ielem)%hlife_n + .1))

         case ("flo_min")
            aqu_dat(ielem)%flo_min = chg_par(aqu_dat(ielem)%flo_min,        &
                         chg_typ, chg_val, absmin, absmax)
 
         case ("revap_min")
            aqu_dat(ielem)%revap_min = chg_par(aqu_dat(ielem)%revap_min,    &
                         chg_typ, chg_val, absmin, absmax)
               
      !!LTE
         case ("cn2_lte")
            hlt_db(ielem)%cn2 = chg_par (hlt_db(ielem)%cn2, chg_typ, chg_val, absmin, absmax)
            
         case ("awc_lte")
            c_val = chg_val * hlt(ielem)%soildep
            abmax = absmax * hlt(ielem)%soildep
            hlt(ielem)%awc = chg_par (hlt(ielem)%awc, chg_typ, c_val, absmin, abmax)
            
         case ("etco_lte")
            hlt_db(ielem)%etco = chg_par (hlt_db(ielem)%etco, chg_typ, chg_val, absmin, absmax)
                       
         case ("tc_lte")
            hlt_db(ielem)%tc = chg_par (hlt_db(ielem)%tc, chg_typ, chg_val, absmin, absmax)
            
         case ("soildep_lte")
            hlt_db(ielem)%soildep = chg_par (hlt_db(ielem)%soildep, chg_typ, chg_val, absmin, absmax)  
        
         case ("slope_lte")
            hlt_db(ielem)%slope = chg_par (hlt_db(ielem)%slope, chg_typ, chg_val, absmin, absmax)
            
         case ("slopelen_lte")
            hlt_db(ielem)%slopelen = chg_par (hlt_db(ielem)%slopelen, chg_typ, chg_val, absmin, absmax)
        
         case ("sy_lte")
            hlt_db(ielem)%sy = chg_par (hlt_db(ielem)%sy, chg_typ, chg_val, absmin, absmax)
            
         case ("abf_lte")
            hlt_db(ielem)%abf = chg_par (hlt_db(ielem)%abf, chg_typ, chg_val, absmin, absmax)
            
         case ("revapc_lte")
            hlt_db(ielem)%revapc = chg_par (hlt_db(ielem)%revapc, chg_typ, chg_val, absmin, absmax)
            
         case ("percc_lte")
            hlt_db(ielem)%percc = chg_par (hlt_db(ielem)%percc, chg_typ, chg_val, absmin, absmax)
            
         case ("sw_lte")
            hlt_db(ielem)%sw = chg_par (hlt_db(ielem)%sw, chg_typ, chg_val, absmin, absmax)
            
         case ("gw_lte")
            hlt_db(ielem)%gw = chg_par (hlt_db(ielem)%gw, chg_typ, chg_val, absmin, absmax)
            
         case ("gwflow_lte")
            hlt_db(ielem)%gwflow = chg_par (hlt_db(ielem)%gwflow, chg_typ, chg_val, absmin, absmax)
            
         case ("gwdeep_lte")
            hlt_db(ielem)%gwdeep = chg_par (hlt_db(ielem)%gwdeep, chg_typ, chg_val, absmin, absmax)
            
        case ("snow_lte")
            hlt_db(ielem)%snow = chg_par (hlt_db(ielem)%snow, chg_typ, chg_val, absmin, absmax)
            
        case ("uslek_lte")
            hlt_db(ielem)%uslek = chg_par (hlt_db(ielem)%uslek, chg_typ, chg_val, absmin, absmax)
            
        case ("uslec_lte")
            hlt_db(ielem)%uslec = chg_par (hlt_db(ielem)%uslec, chg_typ, chg_val, absmin, absmax)
            
        case ("uslep_lte")
            hlt_db(ielem)%uslep = chg_par (hlt_db(ielem)%uslep, chg_typ, chg_val, absmin, absmax)
            
        case ("uslels_lte")
            hlt_db(ielem)%uslels = chg_par (hlt_db(ielem)%uslels, chg_typ, chg_val, absmin, absmax)


        !!gwflow (rtb)
         case ("aquifer_K")
                    if(bsn_cc%gwflow.eq.1) then
                      gw_state(ielem)%hydc = chg_par(gw_state(ielem)%hydc, chg_typ, chg_val, absmin, absmax)        
                        endif
                        
                 case ("aquifer_Sy")
                    if(bsn_cc%gwflow.eq.1) then
                      gw_state(ielem)%spyd = chg_par(gw_state(ielem)%spyd, chg_typ, chg_val, absmin, absmax)    
                    endif
                            
                 case ("aquifer_delay")
                    if(bsn_cc%gwflow.eq.1) then
                      gw_delay(ielem) = chg_par(gw_delay(ielem), chg_typ, chg_val, absmin, absmax)
            endif
                            
                 case ("aquifer_exdp")
                    if(bsn_cc%gwflow.eq.1) then
                      gw_state(ielem)%exdp = chg_par(gw_state(ielem)%exdp, chg_typ, chg_val, absmin, absmax)        
                      endif 
                            
                 case ("stream_K")
                    if(bsn_cc%gwflow.eq.1) then
                      do icell=1,gw_chan_info(ielem)%ncon !loop through cells connected to channel
                            gw_chan_info(ielem)%hydc(icell) = chg_par(gw_chan_info(ielem)%hydc(icell), chg_typ, chg_val, absmin, absmax)
                          enddo
                    endif
                            
                 case ("stream_thk")
                    if(bsn_cc%gwflow.eq.1) then
                      do icell=1,gw_chan_info(ielem)%ncon !loop through cells connected to channel
                            gw_chan_info(ielem)%thck(icell) = chg_par(gw_chan_info(ielem)%thck(icell), chg_typ, chg_val, absmin, absmax)
                          enddo
                    endif
                            
                 case ("stream_bed")
                    if(bsn_cc%gwflow.eq.1) then
                      gw_bed_change = chg_par(gw_bed_change, chg_typ, chg_val, absmin, absmax)      
                    endif

        !! initial soil properties
        case ("lab_p")
          do jj = 1, soil(ielem)%nly
            soil1(ielem)%mp(ly)%lab = chg_par (soil1(ielem)%mp(ly)%lab, chg_typ, chg_val, absmin, absmax)
          end do
          
        case ("hum_c_n")
          do jj = 1, soil(ielem)%nly
            soil1(ielem)%hact(ly)%n = chg_par (soil1(ielem)%hact(ly)%n, chg_typ, chg_val, absmin, absmax)
          end do
          
        case ("hum_c_p")
          do jj = 1, soil(ielem)%nly
            soil1(ielem)%hact(ly)%p = chg_par (soil1(ielem)%hact(ly)%p, chg_typ, chg_val, absmin, absmax)
          end do
          
      end select

      return
      end subroutine cal_parm_select