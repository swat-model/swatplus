      subroutine hru_control
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine controls the simulation of the land phase of the 
!!    hydrologic cycle

      use hru_module, only : hru, ihru, tillage_switch,                                           &
         tillage_days, ndeat, qdr, phubase, sedyld, surfq, grz_days,                              &
         yr_skip, latq, sepbtm, igrz, iseptic, i_sep, filterw, sed_con, soln_con, solp_con,       & 
         orgn_con, orgp_con, cnday, percn, tileno3, sedorgn, sedorgp, surqno3, latno3,            &
         surqsolp, sedminpa, sedminps, fertn, fertp, fixn, grazn, grazp, ipl, qp_cms, qtile,      &
         snofall, snomlt, usle, canev, ep_day, es_day, etday, inflpcp, isep, iwgen, ls_overq,     &
         nd_30, pet_day, precip_eff, qday, latqrunon, gwsoilq, satexq, surf_bs, bss, bss_ex, brt, &
         gwsoiln, gwsoilp, satexn, satexq_chan, surqsalt, latqsalt, tilesalt, percsalt, urbqsalt, & !rtb gwflow; rtb salt
         wetqsalt, wtspsalt,gwupsalt, usle_cfac,                                                  &
         surqcs, latqcs, tilecs, perccs, gwupcs, sedmcs, urbqcs, wetqcs, wtspcs                         !rtb cs
                                                                                                                                        !HAK 7/27/22
      use soil_module 
      use plant_module
      use basin_module
      use organic_mineral_mass_module
      use carbon_module
      use hydrograph_module
      use climate_module, only : wst, w, wgn_pms, salt_atmo, cs_atmo
      use septic_data_module
      use reservoir_data_module
      use plant_data_module
      use mgt_operations_module
      use reservoir_module
      use output_landscape_module
      use output_ls_pesticide_module
      use time_module
      use conditional_module
      use constituent_mass_module
      use water_body_module
      use salt_module !rtb salt
      use cs_module !rtb cs
      use gwflow_module !rtb gwflow
	    use tillage_data_module
      !use basin_module, only : bsn_cc
      
      implicit none

      integer :: j = 0              !none          |same as ihru (hru number)
      integer :: j1 = 0             !none          |counter (rtb)
      integer :: ulu = 0            !              | 
      integer :: iob = 0            !              |
      integer :: ith = 0            !              |
      integer :: iwgn = 0           !              |
      integer :: ires = 0           !none          |reservoir number
      integer :: isched = 0         !              |
      integer :: isalt = 0          !              |salt ion counter (rtb salt)
      integer :: ics = 0            !              |constituent counter (rtb cs)
      integer :: iauto = 0          !none          |counter
      integer :: id = 0             !              |
      integer :: jj = 0             !              |
      integer :: ly = 0             !none          |soil layer
      integer :: ipest = 0          !none          |sequential pesticide number
      real :: strsa_av = 0.         !              |
      integer :: icn = 0            !              |
      real :: xx = 0.               !              |
      integer :: iob_out = 0        !              |object type out 
      integer :: iout = 0           !none          |counter
      integer :: iac = 0
      integer :: npl_gro = 0        !              |number of plants currently growing
      real :: dep = 0.              !              |
      real :: strsw_av = 0.
      real :: strsn_av = 0.
      real :: strsp_av = 0.
      real :: strss_av = 0.         !none (rtb salt)
      real :: strstmp_av = 0.
      real :: wet_outflow = 0.      !mm             |outflow from wetland
      real  :: tile_fr_surf = 0.    !m3             |fraction of tile flow that is overland
      integer :: ifrt = 0
      integer :: idp = 0
      integer :: hru_rcv
      real :: rto
      real :: sw_volume_begin = 0.
      real :: soil_prof_labp = 0.
      real :: sum_conc = 0.              !rtb salt
      real :: sum_mass = 0.              !rtb salt
      real :: sum_sorb = 0.              !rtb salt
      real :: saltcon = 0.       !Jeong 2024
      real :: qsurf = 0.         !Jeong 2024
      real :: sedppm = 0.        !Jeong 2024
      
      j = ihru
      
      !rtb - calculate soil water at the beginning of the day
      sw_volume_begin = 0.
      do j1=1,soil(j)%nly
        sw_volume_begin = sw_volume_begin + soil(j)%phys(j1)%st
      enddo
     
      !h => hwb_d(j)
      !h = hwbz
      !if (pcom(j)%npl > 0) idp = pcom(ihru)%plcur(1)%idplt
      ulu = hru(j)%luse%urb_lu
      iob = hru(j)%obj_no
      iwst = ob(iob)%wst
      iwgen = wst(iwst)%wco%wgn
      ith = hru(j)%dbs%topo
      iwgn = wst(iwst)%wco%wgn
      ires =  hru(j)%dbs%surf_stor
      isched = hru(j)%mgt_ops
      
      w = wst(iwst)%weat
      !! adjust precip and temperature for elevation using lapse rates
      if (bsn_cc%lapse == 1) then
        w%precip = w%precip + ob(iob)%plaps
        w%precip = max (0., w%precip)
        w%tmax = w%tmax + ob(iob)%tlaps
        w%tmin = w%tmin + ob(iob)%tlaps
        w%tave = w%tave + ob(iob)%tlaps
      end if
      precip_eff = w%precip
      
      hsc_d(j) = hscz
      hrc_d(j) = hrcz
      hpc_d(j) = hpcz
      hscf_d(j) = hscfz
      hru(j)%water_seep = 0.
      irrig(j)%demand = 0.
      hnb_d(j)%nuptake = 0.
      hnb_d(j)%puptake = 0.
      hwb_d(j)%wet_out = 0.
      hnb_d(j)%denit = 0.

      if (bsn_cc%cswat == 2) then
        if (tillage_switch(ihru) .eq. 1) then
          if (tillage_days(ihru) .ge. 30) then
            tillage_switch(ihru) = 0
            tillage_days(ihru) = 0
          else
            tillage_days(ihru) = tillage_days(ihru) + 1
          end if                
          !tillage_depth(ihru) = dtil
          !tillage_switch(ihru) = .TRUE. 
        end if
      end if

      !! zero pesticide balance variables
      if (cs_db%num_pests > 0) then
        do ipest = 1, cs_db%num_pests
          !! zero all variables except pest in soil and in/on plant
          hpestb_d(j)%pest(ipest) = hpestb_d(j)%pest(ipest) * 0.
        end do
      end if
        
      call varinit
      nd_30 = nd_30 + 1
      if (nd_30 > 30) nd_30 = 1

        !!ht1== deposition: write to deposition.out
        !!ht2== outflow from inflow: added to hru generated flows
        ht1 = hz
        ht2 = hz

        !! check auto operations
        if (sched(isched)%num_autos > 0) then
          do iauto = 1, sched(isched)%num_autos
            id = sched(isched)%num_db(iauto)
            jj = j
            d_tbl => dtbl_lum(id)
            call conditions (jj, iauto)
            call actions (jj, iob, iauto)
            
            !! check day of future fertilizer application
            if (pcom(ihru)%fert_fut_num > 0) then
              do ifrt = 1, pcom(ihru)%fert_fut_num
                if (pcom(ihru)%fert_fut(ifrt)%day_fert == time%day) then
                  call pl_fert (pcom(ihru)%fert_fut(ifrt)%fertnum,                       &
                      pcom(ihru)%fert_fut(ifrt)%fert_kg, pcom(ihru)%fert_fut(ifrt)%appnum)
                  pcom(ihru)%fert_fut(ifrt)%day_fert = 0
                end if
              end do
            end if
            
            !! if end of year, reset the one time operation per year
            if (time%end_yr == 1) then
              do iac = 1, d_tbl%acts
                pcom(j)%dtbl(iauto)%num_actions(iac) = 1
              end do
            end if
          end do
          if (time%end_yr == 1) then
            pcom(j)%rot_yr = pcom(j)%rot_yr + 1
          end if
          do iauto = 1, sched(isched)%num_autos
            id = sched(isched)%num_db(iauto)
            do iac = 1, dtbl_lum(id)%acts
              if (pcom(j)%dtbl(iauto)%days_act(iac) > 0) then
                pcom(j)%dtbl(iauto)%days_act(iac) = pcom(j)%dtbl(iauto)%days_act(iac) + 1
              end if
            end do
          end do
        end if
        
        !! increment days since last plant and harvest
        pcom(j)%days_plant = pcom(j)%days_plant + 1
        pcom(j)%days_harv = pcom(j)%days_harv + 1
        pcom(j)%days_kill = pcom(j)%days_kill + 1
        pcom(j)%days_irr = pcom(j)%days_irr + 1
          
        !! update base zero total heat units
        if (w%tave > 0. .and. wgn_pms(iwgn)%phutot > 0.01) then
           phubase(j) = phubase(j) + w%tave / wgn_pms(iwgn)%phutot
        end if
   
        !! zero stresses
        do ipl = 1, pcom(j)%npl
          pcom(j)%plstr(ipl)%strsw = 1.
          pcom(j)%plstr(ipl)%strst = 1.
          pcom(j)%plstr(ipl)%strsn = 1.
          pcom(j)%plstr(ipl)%strsp = 1.
          pcom(j)%plstr(ipl)%strsa = 1.
          pcom(j)%plstr(ipl)%strss = 1.
        end do

        !! calculate albedo for day
        call albedo

        !rtb salt - calculate salt ion concentrations using salt equilibrium chemistry
        if (cs_db%num_salts > 0) then
          call salt_chem_hru
        endif
        
        !rtb cs - calculate change in constituent concentrations due to chemical reactions and sorption
        if (cs_db%num_cs > 0) then
          call cs_rctn_hru
          call cs_sorb_hru
        endif
        
        !! calculate soil temperature for soil layers
        call stmp_solt
        
        !!compute canopy interception
        call sq_canopyint

        !! compute snow melt
        call sq_snom
                  
        !!route overland flow across hru - add tile flow if not subirrigation or saturated buffer
        tile_fr_surf = 1.   !assume all tile goes overland until get saturated buffer dtbl
        if (ob(icmd)%hin_sur%flo > 1.e-6) then
          !!route incoming surface runoff
          if (ires > 0) then
            !! add surface runon to wetland
            ht1 = ob(icmd)%hin_sur + tile_fr_surf * ob(icmd)%hin_til
            wet(j) = wet(j) + ht1
          else
            !! route across hru - infiltrate and deposit sediment
            call rls_routesurf (icmd, tile_fr_surf)
          end if
        end if
        
        !!add lateral flow soil water
        if (ob(icmd)%hin_lat%flo > 0) then
          !!Route incoming lateral soil flow
          call rls_routesoil (icmd)
        end if
          
        !! add tile flow to tile (saturated buffer)
        if (hru(j)%sb%sb_db%hru_rcv == j) then
          call rls_routetile (icmd, tile_fr_surf)
        end if
        
        !!add aquifer flow to bottom soil layer and redistribute upwards
        if (ob(icmd)%hin_aqu%flo > 0) then
          !!Route incoming aquifer flow
          call rls_routeaqu (icmd)
        end if
          
        !! compute crack volume
        if (bsn_cc%crk == 1) call sq_crackvol
                  
        !! compute evapotranspiration
        call et_pot
        call et_act

        !! perform management operations
        if (yr_skip(j) == 0) call mgt_operatn
        
        !! compute surface runoff processes
        if (ires == 0) then
          call surface
        else
          !! if wetland - no runoff or sediment yield - all constituents
          !! transported in surface runoff and sediment will be zero
          surfq(j) = 0.
          irrig(j)%runoff = 0.
          sedyld(j) = 0.
        end if

        !! ht2%sed==sediment routed across hru from surface runon
        sedyld(j) = sedyld(j) + ht2%sed
        
        !! check wetland/paddy continuous irrigation Jaehak 2023
        !! manual irrigation on if ponding depth is lower than the threshold depth 
        if (hru(j)%paddy_irr > 0) then
          if (wet_ob(j)%depth < hru(j)%irr_hmin / 1000.) then 
            call wet_irrp
          endif
        endif
        
        !! wetland/paddy processes
        ht2 = hz
        wet_outflow = 0.
        if (ires > 0) then
          call wetland_control
        else
          ht2%flo = wet(j)%flo * hru(j)%area_ha * 10.
          wet(j)%flo = 0.
        end if
 
        !! compute effective rainfall (amount that percs into soil)
        if (ires > 0) then
          !! for wetland use seepage into soil from ponded water
          inflpcp = hru(j)%water_seep
        else
          !! no wetland (no ponded water)
          inflpcp = precip_eff - surfq(j)
        end if
        inflpcp = Max(0., inflpcp)
         
        !! add irrigation to subdaily effective precip
        if (time%step > 1) then
          w%ts(:) = w%ts(:) + irrig(j)%applied / time%step
        end if
        
        !! perform soil water routing
        call swr_percmain
        
        !rtb gwflow: calculate saturation excess reaching the main channel for the current day (qexcess)
        bss_ex(1,j) = bss_ex(1,j) + satexq(j)
        satexq_chan = bss_ex(1,j) * brt(j)
        bss_ex(1,j) = bss_ex(1,j) - satexq_chan

        !! compute peak rate similar to swat-deg using SCS triangular unit hydrograph
        !runoff_m3 = 10. * surfq(j) * hru(j)%area_ha
        !bf_m3 = 10. * latq(j) * hru(j)%area_ha
        !peakr = 2. * runoff_m3 / (1.5 * tconc(j) * 3600.)
        !peakrbf = bf_m3 / 86400.
        !peakr = (peakr + peakrbf)     !* prf     

        !! graze only if adequate biomass in HRU
        if (igrz(j) == 1) then
          ndeat(j) = ndeat(j) + 1
          !! if total above ground biomass is available - graze
          call pl_graze
          !! check to set if grazing period is over
          if (ndeat(j) == grz_days(j)) then
            igrz(j) = 0
            ndeat(j) = 0
          end if
        end if
       
        !! compute residue decomposition and nitrogen and phosphorus mineralization
        if (bsn_cc%cswat == 0) then
          call nut_nminrl
          call nut_nitvol
        end if

        !! compute residue decomposition and nitrogen and phosphorus mineralization
        if (bsn_cc%cswat == 2) then
          if (bmix_eff > 1.e-6) call mgt_newtillmix (ihru, bmix_eff, 0)
          call cbn_rsd_decomp      ! added by JC and FG, modified from nut_minrln.f90
          call cbn_zhang2
        end if

        call nut_nitvol

        if (bsn_cc%sol_P_model == 1) then  
          call nut_pminrl2
        else
          call nut_pminrl
        end if
        
        !! compute biozone processes in septic HRUs
        !! if 1) current is septic hru and 2) soil temperature is above zero
        isep = iseptic(j)
        if (sep(isep)%opt /= 0. .and. time%yrc >= sep(isep)%yr) then
          if (soil(j)%phys(i_sep(j))%tmp > 0.) call sep_biozone     
        endif

        !! compute plant community partitions
        call pl_community
        !if (j == 136) then
        !write (7778,*) time%day, j, pl_mass(j)%tot(1)%m, pl_mass(j)%ab_gr(1)%m, pl_mass(j)%stem(1)%m, &
        !    pl_mass(j)%leaf(1)%m, pl_mass(j)%root(1)%m, pl_mass(j)%seed(1)%m
        !end if
        !if (j == 173) then
        !  write (7778,*) time%day, j, sedyld(j)/hru(j)%area_ha, usle_cfac(j), surfq(j), qp_cms
        !end if

        !! check irrigation demand decision table for water allocation (after adding irrigation)
        if (hru(j)%irr_trn_dtbl > 0) then
          id = hru(j)%irr_trn_dtbl
          jj = j
          d_tbl => dtbl_lum(id)
          !! iauto points to pcom(j)%dtbl(iauto) for days between operation
          iauto = hru(j)%irr_trn_iauto
          call conditions (jj, iauto)
          call actions (jj, iob, iauto)
        end if

        soil_prof_labp = 0.
        do ly = 1, soil(j)%nly
          soil_prof_labp = soil_prof_labp + soil1(j)%mp(ly)%lab
        end do
        
        !! check monsoon season for tropical plants
        do ipl = 1, pcom(j)%npl
          if (pcom(j)%plcur(ipl)%mseas == "y") then
            idp = pcom(j)%plcur(ipl)%idplt
            if (pldb(idp)%trig == "moisture_gro") then
              if (wgn_pms(iwgn)%p_pet_rto > 0.5) then
                pcom(j)%plcur(ipl)%phuacc = 0.
                pcom(j)%plcur(ipl)%gro = "y" 
                pcom(j)%plcur(ipl)%idorm = "n"
                pcom(j)%plcur(ipl)%mseas = "n"
              end if
            end if
          end if
        end do
        
        !! compute plant biomass, leaf, root and seed growth
        call pl_grow

        !! reset harvested biomass and number of harvests for yearly yield output
        if (time%end_yr == 1) then
          do ipl = 1, pcom(j)%npl
            pl_mass(j)%yield_yr(ipl) = plt_mass_z
            pcom(j)%plcur(ipl)%harv_num_yr = 0
          end do
        end if
        
        !! compute total parms for all plants in the community
        strsw_av = 0.; strsa_av = 0.; strsn_av = 0.; strsp_av = 0.; strstmp_av = 0.
        strss_av = 0.
        npl_gro = 0
        do ipl = 1, pcom(j)%npl
          if (pcom(j)%plcur(ipl)%gro == 'y' .and. pcom(j)%plcur(ipl)%idorm == 'n'       &
                                            .and. pcom(j)%plcur(ipl)%phuacc <= 1.) then
            npl_gro = npl_gro + 1
            strsw_av = strsw_av + (1. - pcom(j)%plstr(ipl)%strsw)
            strsa_av = strsa_av + (1. - pcom(j)%plstr(ipl)%strsa)
            strsn_av = strsn_av + (1. - pcom(j)%plstr(ipl)%strsn)
            strsp_av = strsp_av + (1. - pcom(j)%plstr(ipl)%strsp)
            strss_av = strss_av + (1. - pcom(j)%plstr(ipl)%strss)
            strstmp_av = strstmp_av + (1. - pcom(j)%plstr(ipl)%strst)
          end if
        end do
        if (npl_gro > 0) then
          strsw_av = strsw_av / npl_gro
          strsa_av = strsa_av / npl_gro
          strsn_av = strsn_av / npl_gro
          strsp_av = strsp_av / npl_gro
          strstmp_av = strstmp_av / npl_gro
        end if

        !! compute aoil water content to 300 mm depth
        soil(j)%sw_300 = 0.
        do ly = 1, soil(j)%nly
          if (ly == 1) then
            dep = 0.
          else
            dep = soil(j)%phys(ly-1)%d
          end if
          if (soil(j)%phys(ly)%d >= 300.) then
            soil(j)%sw_300 = soil(j)%sw_300 + soil(j)%phys(ly)%st *         &
                                   (300. - dep) / soil(j)%phys(ly)%thick
            exit
          else
            soil(j)%sw_300 = soil(j)%sw_300 + soil(j)%phys(ly)%st
          end if
        end do

        ! compute total soil water for each layer in mm/mm of water content
        do ly = 1, soil(j)%nly
          soil(j)%phys(ly)%tot_sw = (soil(j)%phys(ly)%st / soil(j)%phys(ly)%thick) + soil(j)%phys(ly)%wp 
        end do
        
        !! compute actual ET for day in HRU
        etday = ep_day + es_day + canev
        es_day = es_day

        !rtb gwflow
        if(bsn_cc%gwflow.eq.1) then
          etremain(j) = pet_day - etday
        endif
 
        !! compute pesticide washoff   
        if (w%precip >= 2.54) call pest_washp
        
        !! compute pesticide uptake
        call pest_pl_up

        !! compute pesticide degradation
        call pest_decay

        !! compute pesticide movement in soil
        call pest_lch
      
        !! sum total pesticide in soil
        call pest_soil_tot
        
        if (surfq(j) > 0. .and. qp_cms > 1.e-6) then
          if (precip_eff > 0.) then
            call pest_enrsb
            if (sedyld(j) > 0.) call pest_pesty

            !! static carbon organic n in runoff
            if (bsn_cc%cswat == 0) then
              call nut_orgn
            end if
        
            !! C-Farm (Armen) c and organic n in runoff
            if (bsn_cc%cswat == 1) then
              call nut_orgnc
            end if
      
            !! SWAT-C Xuesong -- c and organic n in runoff
            if (bsn_cc%cswat == 2) then
              call nut_orgnc2
            end if
            call nut_psed
          end if
        end if

        !! add nitrate in rainfall to soil profile
        call nut_nrain

        !! compute nitrate movement leaching
        call nut_nlch
        if (ires > 0) then
          if (wet(j)%flo>0) then
            sedppm=wet(j)%sed/wet(j)%flo*1000000.
          else
            sedppm=0.
          end if
          if (wet_dat_c(ires)%hyd.eq.'paddy') then !.and.time%yrs > pco%nyskip) then
            if (wet_ob(j)%depth > 100.) then
           write(100100,'(4(I6,","),20(f20.1,","))') time%yrc,time%mo,time%day_mo,j,w%precip,irrig(j)%applied,hru(j)%water_seep,     &
            pet_day,etday,wet_ob(j)%weir_hgt*1000,wet_ob(j)%depth*1000.,ht2%flo/(hru(j)%area_ha*10.),soil(j)%sw,sedppm,ht2%sed*1000, &
            wet(j)%no3,ht2%no3,pcom(j)%lai_sum,saltcon 
            end if
          end if
        end if

        !! compute phosphorus movement
        call nut_solp

        !rtb salt
        if (cs_db%num_salts > 0) then
          if(salt_atmo == "y") then
            call salt_rain !add salt in atmospheric deposition to soil profile
          endif
          call salt_roadsalt !add salt in applied road salt to soil profile
          call salt_lch ! compute salt leaching
        endif
        
        !rtb cs
        if (cs_db%num_cs > 0) then
          if(cs_atmo == "y") then
            call cs_rain
          endif
          call cs_lch
        endif
        
        !! compute pathogen transport
        if (cs_db%num_paths > 0.) then
          call path_ls_swrouting
          call path_ls_runoff
          call path_ls_process
        end if

        !! compute loadings from urban areas
        if (hru(j)%luse%urb_lu > 0) then
          if (time%step == 1) then
            call hru_urban ! daily simulation
          else
            call hru_urbanhr ! subdaily simulation J.Jeong 4/20/2009
          endif
        endif     

        !! compute sediment loading in lateral flow and add to sedyld
        call swr_latsed

        !! lag nutrients and sediment in surface runoff
        call stor_surfstor

        !! lag subsurface flow and nitrate in subsurface flow
        call swr_substor

        !! compute reduction in pollutants due to edge-of-field filter strip
        if (hru(j)%lumv%vfsi > 0.)then
          call smp_filter
          if (filterw(j) > 0.) call smp_buffer
        end if

     !! compute reduction in pollutants due to in field grass waterway
         if (hru(j)%lumv%grwat_i == 1) then
          call smp_grass_wway
        end if

       !! compute reduction in pollutants due to in fixed BMP eff
       if (hru(j)%lumv%bmp_flag == 1) then
          call smp_bmpfixed
        end if

        !! ht2%flo is outflow from wetland or total saturation excess if no wetland
        if(ht2%flo > 0.) then
          wet_outflow = ht2%flo / hru(j)%area_ha / 10.   !! mm = m3/ha *ha/10000m2 *1000mm/m
          qday = qday + wet_outflow
          qdr(j) = qdr(j) + wet_outflow
          ht2%flo = 0.
        end if
        
        !! calculate amount of surface runoff during day (qday) and store the remainder
        call sq_surfst

        !! check if hru is a source for a saturated buffer
        if (hru(j)%sb%sb_db%hru_src == j) then
          !! use decision table
          id = hru(j)%sb%dtbl
          d_tbl => dtbl_flo(id)
          call conditions (j, id)
          call actions (j, iob, id)
          
          !! set amount of tile flow to send to buffer hru
          hru_rcv = hru(j)%sb%sb_db%hru_rcv
          !! convert to mm of the receiving hru and adjust for fraction of incoming hru
          rto =  hru(hru_rcv)%area_ha / hru(j)%area_ha
          hru(hru_rcv)%sb%inflo = rto * hru(j)%sb%sb_db%frac_src * hru(hru_rcv)%sb%inflo 
          qtile = (1. - hru(j)%sb%sb_db%frac_src) * hru(hru_rcv)%sb%inflo 
          hru(hru_rcv)%sb%no3 = hru(j)%sb%sb_db%frac_src * tileno3(j) 
          tileno3(j) = (1. - hru(j)%sb%sb_db%frac_src) * tileno3(j)
        end if
        !qday =  surfq(j)

        !! compute water yield for HRU
        qdr(j) = qday + latq(j) + qtile

        if (qdr(j) < 0.) qdr(j) = 0.

        !! compute chl-a, CBOD and dissolved oxygen loadings
        call swr_subwq

        xx = sed_con(j) + soln_con(j) + solp_con(j) + orgn_con(j) + orgp_con(j)
        if (xx > 1.e-6) then
          call hru_urb_bmp
        end if
      
      ! compute outflow objects (flow to channels, reservoirs, or landscape)
      ! if flow from hru is directly routed
      iob_out = iob
      ! if the hru is part of a ru and it is routed
      if (ob(iob)%ru_tot > 0) then
        iob_out = sp_ob1%ru + ob(iob)%ru(1) - 1
      end if
      qsurf=surfq(j)
      
      hwb_d(j)%surq_cha = 0.
      hwb_d(j)%latq_cha = 0.
      hwb_d(j)%surq_res = 0.
      hwb_d(j)%latq_res = 0.
      hwb_d(j)%surq_ls = 0.
      hwb_d(j)%latq_ls = 0.
      
      do iout = 1, ob(iob_out)%src_tot
        select case (ob(iob_out)%obtyp_out(iout))
        case ("cha")
          if (ob(iob_out)%htyp_out(iout) == "sur" .or. ob(iob_out)%htyp_out(iout) == "tot") then
            hwb_d(j)%surq_cha = hwb_d(j)%surq_cha + surfq(j) * ob(iob_out)%frac_out(iout)
          end if
          if (ob(iob_out)%htyp_out(iout) == "lat" .or. ob(iob_out)%htyp_out(iout) == "tot") then
            hwb_d(j)%latq_cha = hwb_d(j)%latq_cha + latq(j) * ob(iob_out)%frac_out(iout)
          end if
        case ("sdc")
          if (ob(iob_out)%htyp_out(iout) == "sur" .or. ob(iob_out)%htyp_out(iout) == "tot") then
            hwb_d(j)%surq_cha = hwb_d(j)%surq_cha + surfq(j) * ob(iob_out)%frac_out(iout)
          end if
          if (ob(iob_out)%htyp_out(iout) == "lat" .or. ob(iob_out)%htyp_out(iout) == "tot") then
            hwb_d(j)%latq_cha = hwb_d(j)%latq_cha + latq(j) * ob(iob_out)%frac_out(iout)
          end if
        case ("res")
          if (ob(iob_out)%htyp_out(iout) == "sur" .or. ob(iob_out)%htyp_out(iout) == "tot") then
            hwb_d(j)%surq_res = hwb_d(j)%surq_res + surfq(j) * ob(iob_out)%frac_out(iout)
          end if
          if (ob(iob_out)%htyp_out(iout) == "lat" .or. ob(iob_out)%htyp_out(iout) == "tot") then
            hwb_d(j)%latq_res = hwb_d(j)%latq_res + latq(j) * ob(iob_out)%frac_out(iout)
          end if
        case ("hru")
          if (ob(iob_out)%htyp_out(iout) == "sur" .or. ob(iob_out)%htyp_out(iout) == "tot") then
            hwb_d(j)%surq_ls = hwb_d(j)%surq_ls + surfq(j) * ob(iob_out)%frac_out(iout)
          end if
          if (ob(iob_out)%htyp_out(iout) == "lat" .or. ob(iob_out)%htyp_out(iout) == "tot") then
            hwb_d(j)%latq_ls = hwb_d(j)%latq_ls + latq(j) * ob(iob_out)%frac_out(iout)
          end if
        case ("ru")
          if (ob(iob_out)%htyp_out(iout) == "sur" .or. ob(iob_out)%htyp_out(iout) == "tot") then
            hwb_d(j)%surq_ls = hwb_d(j)%surq_ls + surfq(j) * ob(iob_out)%frac_out(iout)
          end if
          if (ob(iob_out)%htyp_out(iout) == "lat" .or. ob(iob_out)%htyp_out(iout) == "tot") then
            hwb_d(j)%latq_ls = hwb_d(j)%latq_ls + latq(j) * ob(iob_out)%frac_out(iout)
          end if
        case ("hlt")
          if (ob(iob_out)%htyp_out(iout) == "sur" .or. ob(iob_out)%htyp_out(iout) == "tot") then
            hwb_d(j)%surq_ls = hwb_d(j)%surq_ls + surfq(j) * ob(iob_out)%frac_out(iout)
          end if
          if (ob(iob_out)%htyp_out(iout) == "lat" .or. ob(iob_out)%htyp_out(iout) == "tot") then
            hwb_d(j)%latq_ls = hwb_d(j)%latq_ls + latq(j) * ob(iob_out)%frac_out(iout)
          end if
        end select
      end do

      ! output_waterbal
        hwb_d(j)%precip = w%precip
        hwb_d(j)%snofall = snofall
        hwb_d(j)%snomlt = snomlt
        hwb_d(j)%surq_gen = qday
        hwb_d(j)%latq = latq(j)
        hwb_d(j)%wateryld = qdr(j)
        hwb_d(j)%perc = sepbtm(j)
        if (sp_ob%gwflow > 0) then
          gwflow_perc(j) = sepbtm(j)
        end if
        !! add evap from impounded water (wetland) to et and esoil
        hwb_d(j)%et = etday + hru(j)%water_evap
        hwb_d(j)%ecanopy = canev
        hwb_d(j)%eplant = ep_day
        hwb_d(j)%esoil = es_day + hru(j)%water_evap 
        hwb_d(j)%wet_evap = hru(j)%water_evap 
        hwb_d(j)%wet_out = wet_outflow
        hwb_d(j)%wet_stor = wet(j)%flo / (10. * hru(j)%area_ha)
        hwb_d(j)%surq_cont = surfq(j)
        if (j==1 .and. surfq(j) > 1.) then
          icn = 0
        end if
        hwb_d(j)%cn = cnday(j)
        hwb_d(j)%sw = soil(j)%sw
        hwb_d(j)%sw_final = soil(j)%sw
        hwb_d(j)%sw_300 = soil(j)%sw_300
        hwb_d(j)%snopack = hru(j)%sno_mm
        hwb_d(j)%pet = pet_day
        hwb_d(j)%qtile = qtile
        hwb_d(j)%irr = irrig(j)%applied
        irrig(j)%applied = 0.
        irrig(j)%runoff = 0.
        hwb_d(j)%surq_runon = ls_overq
        hwb_d(j)%latq_runon = latqrunon 
        hwb_d(j)%overbank = hru(j)%wet_obank_in
        hru(j)%wet_obank_in = 0.

        hwb_d(j)%gwsoil = gwsoilq(j) !rtb gwflow - groundwater transferred to soil profile
        if (ires<1) hwb_d(j)%satex = satexq(j) !rtb gwflow - saturation excess generated from high water table
        hwb_d(j)%satex_chan = satexq_chan !rtb gwflow - saturation excess generated from high water table
        hwb_d(j)%delsw = soil(j)%sw - sw_volume_begin
        hwb_d(j)%lagsurf = surf_bs(1,j)
        hwb_d(j)%laglatq = bss(1,j)
        hwb_d(j)%lagsatex = bss_ex(1,j)
        satexq(j) = 0. !zero out for next day
        

      ! output_nutbal
        hnb_d(j)%grazn = grazn
        hnb_d(j)%grazp = grazp
        hnb_d(j)%fertn = fertn
        hnb_d(j)%fertp = fertp
        hnb_d(j)%fixn = fixn
        hnb_d(j)%gwsoiln = gwsoiln(j) !rtb gwflow
        hnb_d(j)%gwsoilp = gwsoilp(j) !rtb gwflow

      ! rtb salt - output salt balance
        do isalt=1,cs_db%num_salts
          hsaltb_d(j)%salt(isalt)%surq = surqsalt(j,isalt)
          hsaltb_d(j)%salt(isalt)%latq = latqsalt(j,isalt)
          hsaltb_d(j)%salt(isalt)%urbq = urbqsalt(j,isalt)
          hsaltb_d(j)%salt(isalt)%wetq = wetqsalt(j,isalt)
          hsaltb_d(j)%salt(isalt)%wtsp = wtspsalt(j,isalt)
          hsaltb_d(j)%salt(isalt)%tile = tilesalt(j,isalt)
          hsaltb_d(j)%salt(isalt)%perc = percsalt(j,isalt)
          hsaltb_d(j)%salt(isalt)%gwup = gwupsalt(j,isalt)
          sum_conc = 0.
          sum_mass = 0.
          do jj=1,soil(j)%nly !loop through soil layers
            sum_conc = sum_conc + cs_soil(j)%ly(jj)%saltc(isalt) !mg/L
            sum_mass = sum_mass + cs_soil(j)%ly(jj)%salt(isalt) !kg/ha
          enddo
          hsaltb_d(j)%salt(isalt)%conc = sum_conc / soil(j)%nly !mg/L (average)
          hsaltb_d(j)%salt(isalt)%soil = sum_mass !kg/ha (total salt in soil profile)
        enddo

      ! rtb cs - output constituent balance
        do ics=1,cs_db%num_cs
          sum_conc = 0.
          sum_mass = 0.
          sum_sorb = 0.
          do jj=1,soil(j)%nly !loop through soil layers
            sum_conc = sum_conc + cs_soil(j)%ly(jj)%csc(ics) !mg/L
            sum_mass = sum_mass + cs_soil(j)%ly(jj)%cs(ics) !kg/ha
            sum_sorb = sum_sorb + cs_soil(j)%ly(jj)%cs_sorb(ics) !kg/ha
          enddo
          hcsb_d(j)%cs(ics)%conc = sum_conc / soil(j)%nly !mg/L (average)
          hcsb_d(j)%cs(ics)%soil = sum_mass !kg/ha (total constituent mass in soil water)
          hcsb_d(j)%cs(ics)%srbd = sum_sorb !kg/ha (total constituent mass sorbed to soil)
          hcsb_d(j)%cs(ics)%surq = surqcs(j,ics)
          hcsb_d(j)%cs(ics)%sedm = sedmcs(j,ics)
          hcsb_d(j)%cs(ics)%latq = latqcs(j,ics)
          hcsb_d(j)%cs(ics)%urbq = urbqcs(j,ics)
          hcsb_d(j)%cs(ics)%wetq = wetqcs(j,ics)
          hcsb_d(j)%cs(ics)%wtsp = wtspcs(j,ics)
          hcsb_d(j)%cs(ics)%tile = tilecs(j,ics)
          hcsb_d(j)%cs(ics)%perc = perccs(j,ics)
          hcsb_d(j)%cs(ics)%gwup = gwupcs(j,ics)
          !hcsb_d(j)%cs(ics)%irsw (populated in cs_irrig)
          !hcsb_d(j)%cs(ics)%irgw (populated in cs_irrig)
          !hcsb_d(j)%cs(ics)%irwo (populated in cs_irrig)
          !hcsb_d(j)%cs(ics)%rain (populated in cs_rain)
          !hcsb_d(j)%cs(ics)%dryd (populated in cs_rain)
          !hcsb_d(j)%cs(ics)%fert (populated in cs_fert)
          !hcsb_d(j)%cs(ics)%uptk (populated in cs_uptake)
          !hcsb_d(j)%cs(ics)%rctn (populated in cs_rctn_hru)
          !hcsb_d(j)%cs(ics)%sorb (populated in cs_sorb_hru)
        enddo  
        
      ! output_plantweather
        hpw_d(j)%lai = pcom(j)%lai_sum
        hpw_d(j)%bioms = pl_mass(j)%tot_com%m
        if (pl_mass(j)%tot_com%m < 0.) then
          pl_mass(j)%tot_com%m = 0.
        end if
        hpw_d(j)%residue = soil1(j)%rsd(1)%m
        hpw_d(j)%yield = pl_yield%m
        pl_yield = plt_mass_z
        hpw_d(j)%sol_tmp =  soil(j)%phys(2)%tmp
        hpw_d(j)%strsw = strsw_av
        hpw_d(j)%strsa = strsa_av
        hpw_d(j)%strstmp = strstmp_av
        hpw_d(j)%strsn = strsn_av      
        hpw_d(j)%strsp = strsp_av
        hpw_d(j)%strss = strss_av
        hpw_d(j)%nplnt = pl_mass(j)%tot_com%n
        hpw_d(j)%percn = percn(j)
        !rtb gwflow: store nitrate leaching concentration for gwflow module
        if(bsn_cc%gwflow == 1  .and. gw_solute_flag == 1) then
          gwflow_percsol(j,1) = percn(j)
        endif
        hpw_d(j)%pplnt = pl_mass(j)%tot_com%p
        hpw_d(j)%tmx = w%tmax
        hpw_d(j)%tmn = w%tmin
        hpw_d(j)%tmpav = w%tave
        hpw_d(j)%solrad = w%solrad
        hpw_d(j)%wndspd = w%windsp
        hpw_d(j)%rhum = w%rhum
        hpw_d(j)%phubase0 = phubase(j)

      ! output_losses
        !! don't sum during skip years
        if (time%yrs > pco%nyskip) then
          bsn_sedbud%upland_t = bsn_sedbud%upland_t + sedyld(j)
        end if
        hls_d(j)%sedyld = sedyld(j) / hru(j)%area_ha
        hls_d(j)%sedorgn = sedorgn(j)
        hls_d(j)%sedorgp = sedorgp(j)
        hls_d(j)%surqno3 = surqno3(j)
        hls_d(j)%latno3 = latno3(j)
        hls_d(j)%surqsolp = surqsolp(j)
        hls_d(j)%usle = usle
        hls_d(j)%sedminp = sedminpa(j) + sedminps(j)
        hls_d(j)%tileno3 = tileno3(j)

      !! set hydrographs for direct routing or landscape unit
      call hru_hyds
      
!100   format (4i6,2i8,2x,a,40f12.3)
 
      return
      end subroutine hru_control