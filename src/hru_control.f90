      subroutine hru_control
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine controls the simulation of the land phase of the 
!!    hydrologic cycle

      use hru_module, only : hru, ihru, tillage_switch,     &
         tillage_days, ndeat, qdr, phubase, sedyld, surfq, grz_days,                                    &
         yr_skip, latq, tconc, smx, sepbtm, igrz, iseptic, i_sep, filterw, sed_con, soln_con, solp_con, & 
         orgn_con, orgp_con, cnday, nplnt, percn, tileno3, pplnt, sedorgn, sedorgp, surqno3, latno3,    &
         surqsolp, sedminpa, sedminps, fertn, fertp, fixn, grazn, grazp, ipl, qp_cms, qtile,            &
         snofall, snomlt, usle, canev, ep_day, es_day, etday, inflpcp, isep, iwgen, ls_overq,           &
         nd_30, pet_day, precip_eff, qday, latqrunon, gwtranq, satexq, surf_bs, bss, bss_ex, brt,       &
         gwtrann, gwtranp, satexn, satexq_chan !rtb gwflow
      use soil_module 
      use plant_module
      use basin_module
      use organic_mineral_mass_module
      use hydrograph_module
      use climate_module, only : wst, w, wgn_pms
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
      use reservoir_seepage_module, only : &
        apply_pending_reservoir_seepage_to_hru
      use gwflow_module !rtb gwflow
      
      implicit none

      integer :: j                  !none          |same as ihru (hru number)
      integer :: j1                 !none          |counter (rtb)
      integer :: sb                 !              |  
      integer :: idp                !              |
      real :: ulu                   !              | 
      integer :: iob                !              |
      integer :: ith                !              |
      integer :: iwgn               !              |
      integer :: ires               !none          |reservoir number
      integer :: isched             !              |
      integer :: idat               !              |
      integer :: ihyd               !              |
      integer :: ics                !              |
      integer :: iauto              !none          |counter
      integer :: id                 !              |
      integer :: jj                 !              |
      integer :: ly                 !none          |soil layer
      integer :: ipest              !none          |sequential pesticide number
      real :: strsa_av              !              |
      real :: runoff_m3             !              |
      real :: bf_m3                 !              |
      real :: peakrbf               !              |
      integer :: icn                !              |
      real :: xx                    !              |
      integer :: iob_out            !              |object type out 
      integer :: iout               !none          |counter
      integer :: iac
      integer :: npl_gro            !           |number of plants currently growing
      real :: over_flow             !              |
      real :: dep                   !              |
      real :: strsw_av
      real :: strsn_av
      real :: strsp_av
      real :: strstmp_av
      real :: wet_outflow           !mm             |outflow from wetland
      integer dum
      real :: sw_volume_begin
      real :: soil_prof_labp
      
      j = ihru

      !! Apply reservoir seepage accepted on the previous day before
      !! calculating the current HRU water balance.
      call apply_pending_reservoir_seepage_to_hru(j)
      
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
        w%precip = amax1 (0., w%precip)
        w%tmax = w%tmax + ob(iob)%tlaps
        w%tmin = w%tmin + ob(iob)%tlaps
        w%tave = w%tave + ob(iob)%tlaps
      end if
      precip_eff = w%precip
      
      hru(ihru)%water_seep = 0.
      irrig(j)%demand = 0.
      hnb_d(j)%nuptake = 0.
      hnb_d(j)%puptake = 0.

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
          hpestb_d(j)%pest(ipest) = pestbz
        end do
      end if
        
      call varinit
      nd_30 = nd_30 + 1
      if (nd_30 > 30) nd_30 = 1

        !!ht1== deposition: write to deposition.out
        !!ht2== outflow from inflow: added to hru generated flows
        ht1 = hz
        ht2 = hz

        !! check irrigation demand decision table for water allocation
        if (hru(ihru)%irr_dmd_dtbl > 0) then
          id = hru(ihru)%irr_dmd_dtbl
          jj = j
          d_tbl => dtbl_lum(id)
          call conditions (jj, iauto)
          call actions (jj, iob, iauto)
        end if
        
        !! check auto operations
        if (sched(isched)%num_autos > 0) then
          do iauto = 1, sched(isched)%num_autos
            id = sched(isched)%num_db(iauto)
            jj = j
            d_tbl => dtbl_lum(id)
            call conditions (jj, iauto)
            call actions (jj, iob, iauto)
            
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
          !! increment days since last plant and harvest
          pcom(j)%days_plant = pcom(j)%days_plant + 1
          pcom(j)%days_harv = pcom(j)%days_harv + 1
          do iauto = 1, sched(isched)%num_autos
            id = sched(isched)%num_db(iauto)
            do iac = 1, dtbl_lum(id)%acts
              if (pcom(j)%dtbl(iauto)%days_act(iac) > 0) then
                pcom(j)%dtbl(iauto)%days_act(iac) = pcom(j)%dtbl(iauto)%days_act(iac) + 1
              end if
            end do
          end do
        end if
        
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
        end do

        !! calculate albedo for day
        call albedo

        !! calculate soil temperature for soil layers
        call stmp_solt
        
        !!compute canopy interception
        call sq_canopyint

        !! compute snow melt
        call sq_snom
                  
        !!route overland flow across hru
        if (ob(icmd)%hin_sur%flo > 1.e-6) then
          !!route incoming surface runoff
          if (ires > 0) then
            !! add surface runon to wetland
            wet(j) = wet(j) + ob(icmd)%hin_sur
          else
            !! route across hru - infiltrate and deposit sediment
            call rls_routesurf (icmd)
          end if
        end if
        
        !!add lateral flow soil water
        if (ob(icmd)%hin_lat%flo > 0) then
          !!Route incoming lateral soil flow
          call rls_routesoil (icmd)
        end if
          
        !!add tile flow to tile (subirrigation and saturated buffer)
        if (ob(icmd)%hin_til%flo > 1.e-6) then
          call rls_routetile (icmd)
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

        !! compute surface runoff processes
        if (ires == 0) then
          call surface
        else
          !! if wetland - no runoff or sediment yield - all constituents
          !! transported in surface runoff and sediment will be zero
          surfq(j) = 0.
          sedyld(j) = 0.
        end if

        !! ht2%sed==sediment routed across hru from surface runon
        sedyld(j) = sedyld(j) + ht2%sed
      
        !! wetland processes
        if (ires > 0) then
          call wetland_control
        else
          ht2%flo = wet(j)%flo * hru(j)%area_ha * 10.
          wet(j)%flo = 0.
        end if
 
        !! compute effective rainfall (amount that percs into soil)
        if (ires > 0) then
          !! for wetland use seepage into soil from ponded water
          inflpcp = hru(j)%water_seep /(10.* hru(j)%area_ha)
        else
          !! no wetland (no ponded water)
          inflpcp = precip_eff - surfq(j)
        end if
        inflpcp = Max(0., inflpcp)
         
        !! perform management operations
        if (yr_skip(j) == 0) call mgt_operatn   
        
        !! add irrigation to subdaily effective precip
        if (time%step > 0) then
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
       
        !! compute nitrogen and phosphorus mineralization
        if (bsn_cc%cswat == 0) then
          call nut_nminrl
        end if

	    if (bsn_cc%cswat == 2) then
	      call cbn_zhang2
	    end if

        call nut_nitvol  
        call nut_pminrl
        
        !! compute biozone processes in septic HRUs
        !! if 1) current is septic hru and 2) soil temperature is above zero
        isep = iseptic(j)
	    if (sep(isep)%opt /= 0. .and. time%yrc >= sep(isep)%yr) then
	      if (soil(j)%phys(i_sep(j))%tmp > 0.) call sep_biozone     
        endif

        !! compute plant community partitions
        call pl_community

        soil_prof_labp = 0.
        do ly = 1, soil(j)%nly
          soil_prof_labp = soil_prof_labp + soil1(j)%mp(ly)%lab
        end do
        !! compute plant biomass, leaf, root and seed growth
        call pl_grow

        !! compute total parms for all plants in the community
        strsw_av = 0.; strsa_av = 0.; strsn_av = 0.; strsp_av = 0.; strstmp_av = 0.
        npl_gro = 0
        do ipl = 1, pcom(j)%npl
          if (pcom(j)%plcur(ipl)%gro == 'y' .and. pcom(j)%plcur(ipl)%idorm == 'n'       &
                                            .and. pcom(j)%plcur(ipl)%phuacc <= 1.) then
            npl_gro = npl_gro + 1
            strsw_av = strsw_av + (1. - pcom(j)%plstr(ipl)%strsw)
            strsa_av = strsa_av + (1. - pcom(j)%plstr(ipl)%strsa)
            strsn_av = strsn_av + (1. - pcom(j)%plstr(ipl)%strsn)
            strsp_av = strsp_av + (1. - pcom(j)%plstr(ipl)%strsp)
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
        
        !! compute total surface residue
        rsd1(j)%tot_com = orgz
        do ipl = 1, pcom(j)%npl
          rsd1(j)%tot_com = rsd1(j)%tot_com + rsd1(j)%tot(ipl)
        end do
        
        !! compute actual ET for day in HRU
        etday = ep_day + es_day + canev
        es_day = es_day

        !rtb gwflow
        if(gwflow_flag.eq.1) then
          etremain(j) = pet_day - etday
          etactual(j) = etday
        endif
 
        !! compute pesticide washoff   
        if (w%precip >= 2.54) call pest_washp

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

		  if (bsn_cc%cswat == 0) then
			call nut_orgn
	      end if
	      if (bsn_cc%cswat == 1) then	    
		    call nut_orgnc
		  end if
		  
		  !! Add by zhang
		  !! ====================
		  if (bsn_cc%cswat == 2) then
		    call nut_orgnc2
		  end if
		  !! Add by zhang
		  !! ====================

            call nut_psed
          end if
        end if

        !! add nitrate in rainfall to soil profile
        call nut_nrain

        !! compute nitrate movement leaching
        call nut_nlch

        !! compute phosphorus movement
        call nut_solp

        !! compute chl-a, CBOD and dissolved oxygen loadings
        call swr_subwq

        !! compute pathogen transport
        if (cs_db%num_paths > 0.) then
          call path_ls_swrouting
          call path_ls_runoff
          call path_ls_process
        end if

        !! compute loadings from urban areas
        if (hru(j)%luse%urb_lu > 0) then
	     if(time%step == 0) then
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

        !! compute water yield for HRU
        qdr(j) = qday + latq(j) + qtile
        
        !! ht2%flo is outflow from wetland or total saturation excess if no wetland

        if(ht2%flo > 0.) then
          wet_outflow = ht2%flo / hru(j)%area_ha / 10.   !! mm = m3/ha *ha/10000m2 *1000mm/m
          qday = qday + wet_outflow
          qdr(j) = qdr(j) + wet_outflow
          surfq(j) = surfq(j) + wet_outflow
          ht2%flo = 0.
        end if

        if (qdr(j) < 0.) qdr(j) = 0.

        xx = sed_con(j) + soln_con(j) + solp_con(j) + orgn_con(j) + orgp_con(j)
        if (xx > 1.e-6) then
          call hru_urb_bmp
        end if
      
      ! update total residue on surface
      rsd1(j)%tot_com = orgz
      do ipl = 1, pcom(j)%npl
        rsd1(j)%tot_com = rsd1(j)%tot_com + rsd1(j)%tot(ipl)
      end do

      ! compute outflow objects (flow to channels, reservoirs, or landscape)
      ! if flow from hru is directly routed
      iob_out = iob
      ! if the hru is part of a ru and it is routed
      if (ob(iob)%ru_tot > 0) then
        iob_out = sp_ob1%ru + ob(iob)%ru(1) - 1
      end if
      
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
        hwb_d(j)%surq_cont = surfq(j)
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
        hwb_d(j)%latq_runon = latqrunon !/ (10. * hru(j)%area_ha) 
        ! hwb_d(j)%overbank = over_flow     !overbank is not added yet

        hwb_d(j)%gwtran = gwtranq(j) !rtb gwflow - groundwater transferred to soil profile
        hwb_d(j)%satex = satexq(j) !rtb gwflow - saturation excess generated from high water table
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
        hnb_d(j)%gwtrann = gwtrann(j) !rtb gwflow
        hnb_d(j)%gwtranp = gwtranp(j) !rtb gwflow

      ! output_plantweather
        hpw_d(j)%lai = pcom(j)%lai_sum
        hpw_d(j)%bioms = pl_mass(j)%tot_com%m
        hpw_d(j)%residue = rsd1(j)%tot_com%m
        hpw_d(j)%yield = pl_yield%m
        pl_yield = plt_mass_z
        hpw_d(j)%sol_tmp =  soil(j)%phys(2)%tmp
        hpw_d(j)%strsw = strsw_av
        hpw_d(j)%strsa = strsa_av
        hpw_d(j)%strstmp = strstmp_av
        hpw_d(j)%strsn = strsn_av      
        hpw_d(j)%strsp = strsp_av
        hpw_d(j)%nplnt = nplnt(j)
        hpw_d(j)%percn = percn(j)
        !rtb gwflow: store nitrate leaching concentration for gwflow module
        if (gwflow_flag == 1 .and. gw_transport_flag == 1) then
          gwflow_percn(j) = percn(j)
        endif
        hpw_d(j)%pplnt = pplnt(j)
        hpw_d(j)%tmx = w%tmax
        hpw_d(j)%tmn = w%tmin
        hpw_d(j)%tmpav = w%tave
        hpw_d(j)%solrad = w%solrad
        hpw_d(j)%phubase0 = phubase(j)

      ! output_losses
        hls_d(j)%sedyld = sedyld(j) !/ hru(j)%area_ha
        if (j == 1087 .and. time%day == 165) then
          jj = 1
        end if
        hls_d(j)%sedorgn = sedorgn(j)
        hls_d(j)%sedorgp = sedorgp(j)
        hls_d(j)%surqno3 = surqno3(j)
        hls_d(j)%latno3 = latno3(j)
        hls_d(j)%surqsolp = surqsolp(j)
        hls_d(j)%usle = usle
        hls_d(j)%sedmin = sedminpa(j) + sedminps(j)
        hls_d(j)%tileno3 = tileno3(j)

      !! set hydrographs for direct routing or landscape unit
      call hru_hyds
 
      return
      end subroutine hru_control