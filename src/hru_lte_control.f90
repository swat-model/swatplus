      subroutine hru_lte_control (isd)
      
      use hru_lte_module
      use hydrograph_module
      use output_landscape_module
      use basin_module
      use climate_module
      use time_module
      use plant_data_module
      use conditional_module
      
      implicit none
      
      integer :: isd                    !             |
      !real :: timeint(1000)
      real :: a1 = .2                   !             |
      real :: a2 = .8                   !             | 
      integer :: ihlt_db                !             |
      integer :: iwgn                   !             |
      integer :: iplt                   !none         |counter
      real :: precip                    !mm           |precipitation
      real :: tmax                      !deg C        |maximum average monthly temperature
      real :: tmin                      !deg C        |minimum average monthly temperature
      real :: raobs                     !             | 
      real :: rmx                       !             | 
      real :: tave                      !             |  
      real :: yield                     !             | 
      real :: ws                        !             | 
      real :: strsair                   !             | 
      real :: snowfall                  !             |
      real :: snowmelt                  !             | 
      real :: runoff                    !             | 
      real :: xx                        !             | 
      real :: exp                       !             |  
      real :: r2                        !             | 
      real :: max                     !             | 
      real :: cn_sd                     !             | 
      real :: precipeff                 !             | 
      real :: xxi                       !             | 
      real :: xsd                       !             | 
      real :: ch                        !             | 
      real :: tan                       !             | 
      real :: hlat                      !             | 
      real :: acos                      !             | 
      real :: ramm                      !MJ/m2        |extraterrestrial radiation  
      real :: pet                       !             | 
      real :: tstress                   !             |sum of temperature stress
      real :: tk                        !deg C        |mean air temperature
      real :: alb                       !none         |albedo when soil is moist
      real :: d                         !cm           |displacement height for plant type
      real :: gma                       !kPa/deg C    |psychrometric constant
      real :: ho                        !none         |variable to hold intermediate calculation
                                        !             |result
      real :: aph                       !             | 
      real :: aet                       !mm            |sum of actual et during growing season (for hi water stress)
      real :: b1                        !             |
      real :: delg                      !             |
      real :: parad                     !             |
      real :: drymat                    !             |
      real :: satco                     !             |
      real :: pl_aerfac                 !             |
      integer :: iend                   !             |points to rule set in d_table
      integer :: istart                 !none         |points to rule set in d_table  
      real :: scparm                    !             |
      real :: air                       !             |
      real :: amin1                     !             |
      real :: tgx                       !             |
      real :: rto                       !none         |cloud cover factor
      real :: reg                       !             |
      real :: deltalai                  !             |
      real :: sw_excess                 !mm H2O       |amount of water in excess of field capacity
                                        !             |stored in soil layer on the current day
      real :: swf                       !cfu          |fraction of manure containing active colony forming units
      real :: flowlat                   !             |
      real :: f                         !             |
      real :: ff                        !             |
      real :: flow_tile                 !             |
      real :: perc                      !             |
      real :: revap                     !mm           |revap
      real :: percdeep                  !             |
      real :: chflow                    !             |
      real :: chflow_m3                 !m^3/s        |Runoff in CMS
      real :: runoff_m3                 !             |
      real :: bf_m3                     !             | 
      real :: peakr                     !m^3/s        |peak runoff rate in channel
      real :: peakrbf                   !             |
      real :: sedin                     !             | 
      real :: qssubconc                 !             |
      real :: qssub                     !             |
      real :: cnv                       !none         |conversion factor (mm => m^3)
        
      ihlt_db = ob(icmd)%props
      iwst = ob(icmd)%wst
      iwgn = wst(iwst)%wco%wgn
      iplt = hlt(isd)%iplant
      precip = wst(iwst)%weat%precip
      tmax = wst(iwst)%weat%tmax
      tmin = wst(iwst)%weat%tmin
      raobs = wst(iwst)%weat%solrad
      rmx = wst(iwst)%weat%solradmx
      
      tave  = (tmax + tmin) / 2. 
      yield = 0.
      ws = 0.
      strsair = 1.
      tstress = 0.
      snowfall = 0.
      snowmelt = 0.
      air = 0.
      
          !! compute curve number
          xx = hlt(isd)%wrt1 - hlt(isd)%wrt2 * hlt(isd)%sw
          if (xx < -20.) xx = -20.
          if (xx > 20.) xx = 20.
          if ((hlt(isd)%sw + Exp(xx)) > 0.001) then
            r2 = hlt(isd)%smx * (1. - hlt(isd)%sw / (hlt(isd)%sw + Exp(xx)))
          end if
          r2 = max(3.,r2)
          cn_sd = 25400. / (r2 + 254.)
          
          IF (tave .lt.0.) THEN 
            ! IF ave temp < 0  compute snowfall    
            snowfall = precip 
            hlt(isd)%snow = hlt(isd)%snow + precip 
            runoff = 0. 
          ELSE
            snowfall = 0.     
            ! IF ave temp > 0  compute runoff                             
            snowmelt = 4.57 * tave  
            IF (snowmelt > hlt(isd)%snow) THEN 
              snowmelt = hlt(isd)%snow 
              hlt(isd)%snow = 0.
            ELSE
              hlt(isd)%snow = hlt(isd)%snow - snowmelt
            END IF 
            
            precipeff = precip + snowmelt
            xx = precipeff - a1 * r2 
            IF (xx.gt.0.) THEN 
              runoff = xx ** 2 / (precipeff + a2 * r2) 
            ELSE 
              runoff = 0. 
            END IF 
            hlt(isd)%sw = hlt(isd)%sw + (precipeff - runoff) 
          END IF 
                                                                        
          xxi = 30. * time%mo - 15. 
          xsd = .4102 * SIN((xxi-80.25)/58.13) 
          ch = -hlt(isd)%yls * tan(xsd) / hlt(isd)%ylc 
          IF (ch.lt.1.) THEN 
            IF (ch.le.-1.) THEN 
              hlat = 3.1415 
            ELSE 
              hlat = acos(ch) 
            END IF 
          ELSE 
            hlat = 0. 
          END IF 
          
          IF (hlt_db(ihlt_db)%ipet .eq. "harg") THEN
            ! compute potential et with Hargreaves Method (harg)
            ramm = rmx / (2.5 - .0022 * tave)
            pet = .0032 * ramm * (tave +17.8) * (tmax - tmin) ** .6
          ELSE
            ! compute potential et with Preistley-Taylor Method (p_t)
            tk = tave  + 273.
            alb = .23
            d = EXP(21.255 - 5304. / tk) * 5304. / tk ** 2
            gma = d / (d +.68)
            ho = 23.9 * raobs * (1. - alb) / 58.3
            aph = 1.28
            pet = aph * ho * gma
          END IF
          pet = hlt(isd)%etco * pet
!
!         compute actual et
!
          xx = 1. - hlt(isd)%sw / hlt(isd)%awc
          IF (xx.lt.0.0001) xx = 0.0001 
          aet = pet * EXP(-xx)                  !at sw=0, exp(-xx)=.368
          aet = pet * hlt(isd)%sw / hlt(isd)%awc  !try linear- may need s-curve
           
!         begin growth for plants
          if (hlt(isd)%gro == "n") then
            ! istart points to rule set in d_table
            istart = hlt(isd)%start
            d_tbl => dtbl_lum(istart)
            call conditions (isd, istart)
            call actions (isd, icmd, istart)
          end if
          
!         end growth for plants
          if (hlt(isd)%gro == "y") then
            ! sum aet and pet for water stress on hi
            hlt(isd)%aet = hlt(isd)%aet + aet
            hlt(isd)%pet = hlt(isd)%pet + pet
            ! iend points to rule set in d_table
            iend = hlt(isd)%end
            d_tbl => dtbl_lum(iend)
            call conditions (isd, icmd)
            call actions (isd, icmd, iend)
          end if

         ! calc yield, print max lai, dm and yield
          if (pco%mgtout == "y") then
            write (4700,*) isd, time%day, time%yrc, pldb(iplt)%plantnm, hlt(isd)%alai, hlt(isd)%dm, yield
            if (pco%csvout == "y") then
              write (4701,*) isd, time%day, time%yrc, pldb(iplt)%plantnm, hlt(isd)%alai, hlt(isd)%dm, yield
            end if
          end if
                                                              
!                                                                       
!         compute plant growth - b1=et adjustment factor b1=1 during growing season b1=.6 IF no
!    
          b1 = hlt(isd)%etco - .4        !evap coef ranges from .4-.8 when etco ranges from .8-1.2
          IF (hlt(isd)%gro == "y") THEN
            b1 = hlt(isd)%etco
            delg = (tave - pldb(iplt)%t_base) / hlt(isd)%phu 
            IF (delg.lt.0.) THEN 
              delg = 0. 
            END IF 
            hlt(isd)%g = hlt(isd)%g + delg 
            parad = .5 * raobs * (1.-EXP(-.65 * (hlt(isd)%alai + .05))) 
            drymat = parad * pldb(iplt)%bio_e * hlt(isd)%stress
            ws = aet / pet
            
            !compute aeration stress
            if (hlt(isd)%sw .gt. hlt(isd)%awc) THEN 
              satco = (hlt(isd)%sw - hlt(isd)%awc) / (hlt(isd)%por - hlt(isd)%awc) 
              pl_aerfac = .85
              scparm = 100. * (satco - pl_aerfac) / (1.0001 - pl_aerfac)
              if (scparm > 0.) then
                strsair = 1. - (scparm / (scparm + Exp(2.9014 - .03867 * scparm)))
              else
                strsair = 1.
              end if
            end if
                                                                        
            !irrigate IF water stress is < 0.8                             
            IF (hlt_db(ihlt_db)%irr > "no_irr") THEN 
              IF (ws < 0.8) THEN 
                air = hlt(isd)%awc - hlt(isd)%sw 
                air = amin1 (76.2, air)     ! max application = 3 inches
                
                ! take from shallow aquifer
                IF (hlt_db(ihlt_db)%irrsrc == "shal_aqu") THEN 
                  hlt(isd)%gw = hlt(isd)%gw - air 
                  IF (hlt(isd)%gw < 0.) THEN 
                    air = air + hlt(isd)%gw 
                    hlt(isd)%gw = 0. 
                  END IF 
                END IF
                
                ! take from deep aquifer
                IF (hlt_db(ihlt_db)%irrsrc == "deep_aqu") THEN
                  hlt(isd)%gwdeep = hlt(isd)%gwdeep - air 
                  IF (hlt(isd)%gwdeep < 0.) THEN 
                    air = air + hlt(isd)%gwdeep 
                    hlt(isd)%gwdeep = 0. 
                  END IF 
                END IF
                hlt(isd)%sw = hlt(isd)%sw + air
              END IF 
            END IF                                  
                                                                  
            if (tave .gt.pldb(iplt)%t_base) THEN
              tgx = 2. * pldb(iplt)%t_opt - pldb(iplt)%t_base - tave
              rto = ((pldb(iplt)%t_opt - tave ) /(tgx + 1.e-6))**2
              IF (rto.le.200.) THEN 
                tstress = EXP(-0.1054*rto) 
              ELSE 
                tstress = 0. 
              END IF
            ELSE
              tstress = 0. 
            END IF 
               
                                                                                    
!                                                                       
!         compute boimass and leaf area                  
!    
            reg = amin1(ws,tstress,strsair) 
            hlt(isd)%dm = hlt(isd)%dm + reg * drymat 
            f = hlt(isd)%g / (hlt(isd)%g + EXP(plcp(iplt)%leaf1 - plcp(iplt)%leaf2 * hlt(isd)%g))
            ff = f - hlt(isd)%hufh
            hlt(isd)%hufh = f
            deltalai = ff * pldb(iplt)%blai * (1.0 - EXP(5.0 *(hlt(isd)%alai - pldb(iplt)%blai))) * sqrt(reg)
            hlt(isd)%alai = hlt(isd)%alai + deltalai
          END IF
                                                                  
!         adjust actual et for growing season
          aet = b1 * aet
          
          !compute lateral soil flow
          sw_excess = hlt(isd)%sw - hlt(isd)%awc
          if (sw_excess > 0.) then
            swf = (hlt(isd)%sw - hlt(isd)%awc) / (hlt(isd)%por - hlt(isd)%awc) 
            flowlat = .024 * swf * hlt(isd)%sc * hlt_db(ihlt_db)%slope / hlt_db(ihlt_db)%slopelen
            flowlat = amin1(hlt(isd)%sw, flowlat)
            hlt(isd)%sw = hlt(isd)%sw - flowlat
          else
            flowlat = 0.
          end if
        
          !compute tile flow
          sw_excess = hlt(isd)%sw - hlt(isd)%awc
          if (sw_excess > 0. .and. hlt(isd)%tdrain > 0.) then
            flow_tile = sw_excess * (1. - Exp(-24. / hlt(isd)%tdrain))
            flow_tile = amin1(flow_tile, 10.)     !assume a drainage coefficient of 12.5 mm
          else
            flow_tile = 0.
          end if
          flow_tile = amin1(hlt(isd)%sw, flow_tile)
          hlt(isd)%sw = hlt(isd)%sw - flow_tile
          
          !compute percolation from bottom of soil profile
          sw_excess = hlt(isd)%sw - hlt(isd)%awc * hlt(isd)%perco
          if (sw_excess > 0.) then
            perc = sw_excess * (1. - Exp(-24. / hlt(isd)%hk))
          else
            perc = 0.
          end if
          if (perc.lt.0.) perc = 0.
          perc = amin1(hlt(isd)%sw, perc)
          hlt(isd)%sw = hlt(isd)%sw - perc
          !limit perc with perco_co
          
          aet = amin1(hlt(isd)%sw, aet)
          hlt(isd)%sw = hlt(isd)%sw - aet 
                                                                        
          hlt(isd)%gw = hlt(isd)%gw + perc 
          revap = aet * hlt_db(ihlt_db)%revapc 
          percdeep = perc * hlt_db(ihlt_db)%percc
          hlt(isd)%gwflow = hlt(isd)%gwflow * hlt_db(ihlt_db)%abf + perc * (1. - hlt_db(ihlt_db)%abf)
          hlt(isd)%gwflow = amin1(hlt(isd)%gwflow, hlt(isd)%gw)
          hlt(isd)%gw = hlt(isd)%gw - hlt(isd)%gwflow
                                                                        
          revap = amin1(revap, hlt(isd)%gw)
          hlt(isd)%gw = hlt(isd)%gw - revap
                                                                        
          percdeep = amin1(percdeep, hlt(isd)%gw)
          hlt(isd)%gw = hlt(isd)%gw - percdeep
                                                                        
          hlt(isd)%gwdeep = hlt(isd)%gwdeep + percdeep
                                                                        
          chflow = runoff + flowlat + flow_tile + hlt(isd)%gwflow

!!        compute channel peak rate using SCS triangular unit hydrograph
          chflow_m3 = 1000. * chflow * ob(icmd)%area_ha
	      runoff_m3 = 1000. * runoff * ob(icmd)%area_ha
	      bf_m3 = 1000. * (flowlat + hlt(isd)%gwflow)*ob(icmd)%area_ha
          peakr = 2. * runoff_m3 / (1.5 * hlt_db(ihlt_db)%tc)
	      peakrbf = bf_m3 / 86400.
          peakr = (peakr + peakrbf)     !* prf     
          
!!        compute sediment yield with MUSLE
          sedin = (runoff * peakr * 1000. * ob(icmd)%area_ha) ** .56 * hlt(isd)%uslefac
          
	    !! add subsurf sediment - t=ppm*mm*km2/1000.
	    qssubconc = 500.
	    qssub = qssubconc * (flowlat + hlt(isd)%gwflow) * ob(icmd)%area_ha / 1000.
	    sedin = sedin + qssub

          cnv = ob(icmd)%area_ha * 1000.
          
         !   output_waterbal - SWAT-DEG0140
        hltwb_d(isd)%precip = precip             
        hltwb_d(isd)%snofall = snowfall     
        hltwb_d(isd)%snomlt = snowmelt           
        hltwb_d(isd)%surq_gen = runoff   
        hltwb_d(isd)%latq = flowlat + hlt(isd)%gwflow
        hltwb_d(isd)%wateryld = chflow
        hltwb_d(isd)%perc = perc                
        hltwb_d(isd)%et = aet                   
        hltwb_d(isd)%ecanopy = 0.                  
        hltwb_d(isd)%eplant = 0.                
        hltwb_d(isd)%esoil = 0.                
        hltwb_d(isd)%surq_cont = 0.
        hltwb_d(isd)%cn = cn_sd
        hltwb_d(isd)%sw = hlt(isd)%sw
        hltwb_d(isd)%sw_final = hlt(isd)%sw
        hltwb_d(isd)%sw_300 = hlt(isd)%sw / 300.
        hltwb_d(isd)%snopack = hlt(isd)%snow       
        hltwb_d(isd)%pet = pet             
        hltwb_d(isd)%qtile = flow_tile
        hltwb_d(isd)%irr = air
        
!    output_nutbal - no nutrients currently in SWAT-DEG

!    output_losses - SWAT-DEG
        hltls_d(isd)%sedyld = sedin / (100. * ob(icmd)%area_ha) !! sedyld(isd) / hru_ha(isd)
        hltls_d(isd)%sedorgn = 0.   !! sedorgn(isd)
        hltls_d(isd)%sedorgp = 0.   !! sedorgp(isd)
        hltls_d(isd)%surqno3 = 0.   !! surqno3(isd)
        hltls_d(isd)%latno3 = 0.    !! latno3(isd)
        hltls_d(isd)%surqsolp = 0.  !! surqsolp(isd)
        hltls_d(isd)%usle = 0.      !! usle
        hltls_d(isd)%sedminp = 0.    !! sedminpa(isd) + sedminps(isd)
        hltls_d(isd)%tileno3 = 0.   !! tileno3(isd)
        
!    output_plantweather - SWAT-DEG
        hltpw_d(isd)%lai =  hlt(isd)%alai     !! lai
        hltpw_d(isd)%bioms =  hlt(isd)%dm     !! total biomass
        hltpw_d(isd)%yield =  yield          !! crop yield
        hltpw_d(isd)%residue =  0.           !! residue
        hltpw_d(isd)%sol_tmp =  0.           !! soil(isd)%phys(2))%tmp
        hltpw_d(isd)%strsw = 1. - ws         !! (1.-strsw_av(isd))
        hltpw_d(isd)%strsa = 1. - strsair    !! (1.-strsw_av(isd))
        hltpw_d(isd)%strstmp = 1. - tstress  !! (1.-strstmp_av)
        hltpw_d(isd)%strsn = 0.              !! (1.-strsn_av)        
        hltpw_d(isd)%strsp = 0.              !! (1.-strsp_av)
        hltpw_d(isd)%strss = 0.              !! (1.-strss_av)
        hltpw_d(isd)%nplnt = 0.              !! nplnt(isd)
        hltpw_d(isd)%percn = 0.              !! percn(isd)
        hltpw_d(isd)%pplnt = 0.              !! pplnt(isd)
        hltpw_d(isd)%tmx = tmax              !! tmx(isd)
        hltpw_d(isd)%tmn = tmin              !! tmn(isd)
        hltpw_d(isd)%tmpav = tave            !! tmpav(isd)
        hltpw_d(isd)%solrad = raobs          !! hru_ra(isd)
        hltpw_d(isd)%wndspd = wst(iwst)%weat%windsp         !! windspeed(isd)
        hltpw_d(isd)%rhum = wst(iwst)%weat%rhum             !! relative humidity(isd)
        hltpw_d(isd)%phubase0 = wst(iwst)%weat%phubase0     !! base zero potential heat units

         !! set values for outflow hydrograph
         !! storage locations set to zero are not currently used
         ob(icmd)%peakrate = peakr
         ob(icmd)%hd(1)%temp = 5. + .75 * tave        !!wtmp
         ob(icmd)%hd(1)%flo = chflow * cnv            !!qdr m3/d
         ob(icmd)%hd(1)%sed = sedin                   !!sedyld
         ob(icmd)%hd(1)%orgn = 0.
         ob(icmd)%hd(1)%sedp = 0.
         ob(icmd)%hd(1)%no3 = 0.
         ob(icmd)%hd(1)%solp = 0.
         ob(icmd)%hd(1)%chla = 0.
         ob(icmd)%hd(1)%nh3 = 0.                         !! NH3
         ob(icmd)%hd(1)%no2 = 0.                         !! NO2
         ob(icmd)%hd(1)%cbod = 0.
         ob(icmd)%hd(1)%dox = 0.
         ob(icmd)%hd(1)%san = 0.                             !! det sand
         ob(icmd)%hd(1)%sil = 0.                             !! det silt
         ob(icmd)%hd(1)%cla = 0.                             !! det clay
         ob(icmd)%hd(1)%sag = 0.                             !! det sml ag
         ob(icmd)%hd(1)%lag = 0.                             !! det lrg ag
         
         !! set values for recharge hydrograph
         !ob(icmd)%hd(2)%flo = perc

       return
 
      end subroutine hru_lte_control