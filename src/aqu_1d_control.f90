      subroutine aqu_1d_control 
    
      use aquifer_module
      use time_module
      use hydrograph_module
      use climate_module, only : wst
      use maximum_data_module
      use constituent_mass_module
      use pesticide_data_module
      use aqu_pesticide_module
      use salt_module
      use salt_aquifer
      use cs_aquifer
      use ch_pesticide_module
      
      implicit none
      
      integer :: iaq            !none       |counter
      integer :: iaqdb          !           |
      integer :: icha           !           |
      integer :: iob_out        !           !object type out
      integer :: iout           !none       |counter
      integer :: ii             !none       |counter
      integer :: icontrib       !none       |counter
      integer :: ipest          !none       |counter
      integer :: ipest_db       !none       |pesticide number from pesticide data base
      integer :: ipseq          !none       |sequential basin pesticide number
      integer :: ipdb           !none       |seqential pesticide number of daughter pesticide
      integer :: imeta          !none       |pesticide metabolite counter
      real :: mol_wt_rto        !ratio      |molecular weight ratio of duaghter to parent pesticide
      real :: stor_init         !           |
      real :: conc_no3          !           |
      real :: step              !           |
      real :: contrib_len
      real :: contrib_len_left
      real :: pest_init         !kg/ha      |amount of pesticide present at beginning of day
      real :: no3_init          !kg/ha      |amount of nitrate present at beginning of day
      real :: flow_mm           !mm         |total flow through aquifer - return flow + seepage
      real :: pest_kg           !kg         |soluble pesticide moving with flow
      real :: conc              !kg/mm      |concentration of pesticide in flow
      real :: zdb1              !mm         |kd - flow factor for pesticide transport
      real :: kd                !(mg/kg)/(mg/L) |koc * carbon
      real :: gw_volume         !m3         |m3 of groundwater in aquifer
      real :: salt_recharge     !kg         |kg of salt in recharge water
      real :: gw_discharge      !m3         |m3 of groundwater discharging to channels
      real :: salt_discharge    !kg         |kg of salt in groundwater discharge
      real :: gw_seep           !m3         |m3 of groundwater seeping from the aquifer
      real :: salt_seep         !kg         |kg of salt in groundwater seepage 
      real :: cs_recharge,cs_discharge,cs_seep !rtb cs
      integer :: m !rtb salt
      integer :: ics   !rtb cs
      
      !! set pointers to aquifer database and weather station
      iaq = ob(icmd)%num
      iaqdb = ob(icmd)%props
      iwst = ob(icmd)%wst
      stor_init = aqu_d(iaq)%stor
      
      ob(icmd)%hd(1) = hz
      ob(icmd)%hd(2) = hz
      if (cs_db%num_tot > 0) then
        obcs(icmd)%hd(1) = hin_csz
        obcs(icmd)%hd(2) = hin_csz
      end if

      !convert from m^3 to mm
      aqu_d(iaq)%rchrg = ob(icmd)%hin%flo / (10. * ob(icmd)%area_ha)
      
      !rtb salt
      !calculate salt ion concentrations in groundwater, using salt equilibrium chemistry
      if (cs_db%num_salts > 0) then
        call salt_chem_aqu
      endif
      
      !rtb cs
      !calculate changes in constituent concentration in groundwater due to chemical reactions and sorption
      if (cs_db%num_cs > 0) then
        call cs_rctn_aqu
        call cs_sorb_aqu
      endif
      
      !! lag recharge from bottom of soil to water table ** disabled
      !aqu_d(iaq)%rchrg = (1. - aqu_prm(iaq)%delay_e) * aqu_d(iaq)%rchrg + aqu_prm(iaq)%delay_e * aqu_st(iaq)%rchrg_prev
      
      aqu_prm(iaq)%rchrg_prev = aqu_d(iaq)%rchrg
      
      !! add recharge to aquifer storage
      aqu_d(iaq)%stor = aqu_d(iaq)%stor + aqu_d(iaq)%rchrg
      
      !! compute groundwater depth from surface
      aqu_d(iaq)%dep_wt = aqu_dat(iaq)%dep_bot - (aqu_d(iaq)%stor / (1000. * aqu_dat(iaq)%spyld))
      aqu_d(iaq)%dep_wt = max (0., aqu_d(iaq)%dep_wt)

      !! compute flow and substract from storage
      if (aqu_d(iaq)%dep_wt <= aqu_dat(iaq)%flo_min) then
        aqu_d(iaq)%flo = aqu_d(iaq)%flo * aqu_prm(iaq)%alpha_e + aqu_d(iaq)%rchrg * (1. - aqu_prm(iaq)%alpha_e)
        aqu_d(iaq)%flo = Max (0., aqu_d(iaq)%flo)
        aqu_d(iaq)%flo = Min (aqu_d(iaq)%stor, aqu_d(iaq)%flo)
        aqu_d(iaq)%stor = aqu_d(iaq)%stor - aqu_d(iaq)%flo
      else
        aqu_d(iaq)%flo = 0.
      endif

      !! set hydrograph flow from aquifer- convert mm to m3
      ob(icmd)%hd(1)%flo = 10. * aqu_d(iaq)%flo * ob(icmd)%area_ha
      
      !! compute seepage through aquifer and subtract from storage
      aqu_d(iaq)%seep = aqu_d(iaq)%rchrg * aqu_dat(iaq)%seep
      aqu_d(iaq)%seep = amin1 (aqu_d(iaq)%seep, aqu_d(iaq)%stor)
      ob(icmd)%hd(2)%flo = 10. * aqu_d(iaq)%seep * ob(icmd)%area_ha
      
      aqu_d(iaq)%stor = aqu_d(iaq)%stor - aqu_d(iaq)%seep
      
      !! compute revap (deep root uptake from aquifer) and subtract from storage
      if (aqu_d(iaq)%dep_wt < aqu_dat(iaq)%revap_min) then
        aqu_d(iaq)%revap = wst(iwst)%weat%pet * aqu_dat(iaq)%revap_co
        aqu_d(iaq)%revap = amin1 (aqu_d(iaq)%revap, aqu_d(iaq)%stor)
        aqu_d(iaq)%stor = aqu_d(iaq)%stor - aqu_d(iaq)%revap
      else
        aqu_d(iaq)%revap = 0.
      end if

      !! compute nitrate recharge into the aquifer
      aqu_d(iaq)%no3_rchg = ob(icmd)%hin%no3 / ob(icmd)%area_ha
      aqu_d(iaq)%no3_st = aqu_d(iaq)%no3_st + aqu_d(iaq)%no3_rchg
      aqu_prm(iaq)%rchrgn_prev = aqu_d(iaq)%no3_rchg
      
      !! compute nitrate return flow out of aquifer
      if (aqu_d(iaq)%stor > 1.e-6) then
        !! (kg/ha / mm)
        conc_no3 = aqu_d(iaq)%no3_st / aqu_d(iaq)%stor
      else
        conc_no3 = 0.
      endif
      !! kg = (kg/ha / mm) * mm * ha
      ob(icmd)%hd(1)%no3 = (conc_no3 * aqu_d(iaq)%flo) * ob(icmd)%area_ha
      ob(icmd)%hd(1)%no3 = amin1(ob(icmd)%hd(1)%no3, (aqu_d(iaq)%no3_st * ob(icmd)%area_ha))
      aqu_d(iaq)%no3_lat = ob(icmd)%hd(1)%no3 / ob(icmd)%area_ha
      aqu_d(iaq)%no3_st = aqu_d(iaq)%no3_st - aqu_d(iaq)%no3_lat
      
      !revapno3 = conc * revap -- dont include nitrate uptake by plant
      
      !! compute NO3 lost in the aquifer
      no3_init = aqu_d(iaq)%no3_st
      aqu_d(iaq)%no3_st = aqu_d(iaq)%no3_st * aqu_prm(iaq)%nloss
      aqu_d(iaq)%no3_loss = no3_init - aqu_d(iaq)%no3_st
      
      !! compute nitrate seepage out of aquifer
      !! kg/ha = (kg/ha / mm) * mm
      aqu_d(iaq)%no3_seep = conc_no3 * aqu_d(iaq)%seep
      aqu_d(iaq)%no3_seep = amin1(aqu_d(iaq)%no3_seep, aqu_d(iaq)%no3_st)
      aqu_d(iaq)%no3_st = aqu_d(iaq)%no3_st - aqu_d(iaq)%no3_seep
      ob(icmd)%hd(2)%no3 = aqu_d(iaq)%no3_seep * ob(icmd)%area_ha
      
      !rtb salt
      !compute salt recharge into the aquifer
      do m=1,cs_db%num_salts
        salt_recharge = obcs(icmd)%hin(1)%salt(m) !kg
        cs_aqu(iaq)%salt(m) = cs_aqu(iaq)%salt(m) + salt_recharge !kg
        asaltb_d(iaq)%salt(m)%rchrg = salt_recharge
      enddo
      !compute groundwater salt loading and seepage
      do m=1,cs_db%num_salts
        !calculate new concentration of salt ion in groundwater
        gw_volume = (aqu_d(iaq)%stor/1000.) * (ob(icmd)%area_ha*10000.) !m3 of groundwater
        if(gw_volume.gt.0) then
          cs_aqu(iaq)%saltc(m) = (cs_aqu(iaq)%salt(m) * 1000.) / gw_volume !g/m3 = mg/L
        else
          cs_aqu(iaq)%saltc(m) = 0.
        endif
        !compute salt loading to streams
        gw_discharge = (aqu_d(iaq)%flo/1000.) * (ob(icmd)%area_ha*10000.) !mm --> m3
        salt_discharge = (cs_aqu(iaq)%saltc(m)*gw_discharge) / 1000. !kg
        !if loading is more than salt in aquifer, decrease accordingly and set storage = 0
        if(salt_discharge .gt. cs_aqu(iaq)%salt(m)) then
          salt_discharge = cs_aqu(iaq)%salt(m)
        endif
        cs_aqu(iaq)%salt(m) = cs_aqu(iaq)%salt(m) - salt_discharge !kg (update salt storage)
        obcs(icmd)%hd(1)%salt(m) = salt_discharge !kg (store salt loading, for input to streams)
        if (db_mx%aqu2d > 0) then !save to distribute on following day (if aqu2d method is used, with "aqu_cha.lin" file)
          aq_chcs(iaq)%hd(1)%salt(m) = obcs(icmd)%hd(1)%salt(m) !save to distribute on following day
        endif
        asaltb_d(iaq)%salt(m)%saltgw = obcs(icmd)%hd(1)%salt(m) !kg (store for daily output)
        !compute salt seepage out of aquifer
        gw_seep = (aqu_d(iaq)%seep/1000.) * (ob(icmd)%area_ha*10000.) !m3 of groundwater
        salt_seep = (cs_aqu(iaq)%saltc(m) * gw_seep) / 1000. !kg
        if(salt_seep.gt.cs_aqu(iaq)%salt(m)) then
          salt_seep = cs_aqu(iaq)%salt(m)
        endif
        cs_aqu(iaq)%salt(m) = cs_aqu(iaq)%salt(m) - salt_seep !kg (update salt storage)
        asaltb_d(iaq)%salt(m)%seep = salt_seep !kg (store for daily output)
        obcs(icmd)%hd(2)%salt(m) = asaltb_d(iaq)%salt(m)%seep
        !update salt ion concentration in groundwater        
        if(gw_volume.gt.0) then
          cs_aqu(iaq)%saltc(m) = (cs_aqu(iaq)%salt(m) * 1000.) / gw_volume !g/m3 = mg/L
        else
          cs_aqu(iaq)%saltc(m) = 0.
        endif
        asaltb_d(iaq)%salt(m)%mass = cs_aqu(iaq)%salt(m) !store mass for output
        asaltb_d(iaq)%salt(m)%conc = cs_aqu(iaq)%saltc(m) !store concentration for output
      enddo

      !rtb cs
      !compute constituent mass recharge into the aquifer
      do ics=1,cs_db%num_cs
        cs_recharge = obcs(icmd)%hin(1)%cs(ics) !kg
        cs_aqu(iaq)%cs(ics) = cs_aqu(iaq)%cs(ics) + cs_recharge !kg
        acsb_d(iaq)%cs(ics)%rchrg = cs_recharge
      enddo
      !compute groundwater constituent loading and seepage
      do ics=1,cs_db%num_cs
        !calculate new concentration of constituent in groundwater
        gw_volume = (aqu_d(iaq)%stor/1000.) * (ob(icmd)%area_ha*10000.) !m3 of groundwater
        if(gw_volume.gt.0) then
          cs_aqu(iaq)%csc(ics) = (cs_aqu(iaq)%cs(ics) * 1000.) / gw_volume !g/m3 = mg/L
        else
          cs_aqu(iaq)%csc(ics) = 0.
        endif
        !compute constituent loading to streams
        gw_discharge = (aqu_d(iaq)%flo/1000.) * (ob(icmd)%area_ha*10000.) !mm --> m3
        cs_discharge = (cs_aqu(iaq)%csc(ics)*gw_discharge) / 1000. !kg
        !if loading is more than constituent mass in aquifer, decrease accordingly and set storage = 0
        if(cs_discharge .gt. cs_aqu(iaq)%cs(ics)) then
          cs_discharge = cs_aqu(iaq)%cs(ics)
        endif
        cs_aqu(iaq)%cs(ics) = cs_aqu(iaq)%cs(ics) - cs_discharge !kg (update constituent mass storage)
        obcs(icmd)%hd(1)%cs(ics) = cs_discharge !kg (store constituent loading, for input to streams)
        if (db_mx%aqu2d > 0) then !save to distribute on following day (if aqu2d method is used, with "aqu_cha.lin" file)
          aq_chcs(iaq)%hd(1)%cs(ics) = obcs(icmd)%hd(1)%cs(ics) 
        endif
        acsb_d(iaq)%cs(ics)%csgw = obcs(icmd)%hd(1)%cs(ics) !kg (store for daily output)
        !compute constituent mass seepage out of aquifer
        gw_seep = (aqu_d(iaq)%seep/1000.) * (ob(icmd)%area_ha*10000.) !m3 of groundwater
        cs_seep = (cs_aqu(iaq)%csc(ics) * gw_seep) / 1000. !kg
        if(cs_seep.gt.cs_aqu(iaq)%cs(ics)) then
          cs_seep = cs_aqu(iaq)%cs(ics)
        endif
        cs_aqu(iaq)%cs(ics) = cs_aqu(iaq)%cs(ics) - cs_seep !kg (update constituent mass storage)
        acsb_d(iaq)%cs(ics)%seep = cs_seep !kg (store for daily output)
        obcs(icmd)%hd(2)%cs(ics) = acsb_d(iaq)%cs(ics)%seep
        !update constituent concentration in groundwater        
        if(gw_volume.gt.0) then
          cs_aqu(iaq)%csc(ics) = (cs_aqu(iaq)%cs(ics) * 1000.) / gw_volume !g/m3 = mg/L
        else
          cs_aqu(iaq)%csc(ics) = 0.
        endif
        acsb_d(iaq)%cs(ics)%mass = cs_aqu(iaq)%cs(ics)  !store mass for output
        acsb_d(iaq)%cs(ics)%conc = cs_aqu(iaq)%csc(ics) !store concentration for output
      enddo
      
      !! compute mineral p flow (constant concentration) from aquifer - m^3 * ppm * 1000 kg/m^3 = 1/1000
      aqu_d(iaq)%minp = ob(icmd)%hin%flo * aqu_dat(iaq)%minp / 1000.
      ob(icmd)%hd(1)%solp = aqu_d(iaq)%minp
      
      !! temperature of aquifer flow
      ob(icmd)%hd(1)%temp = w_temp%gw

      !! compute fraction of flow to each channel in the aquifer
      !! if connected to aquifer - add flow
      if (db_mx%aqu2d > 0) then
        contrib_len = aq_ch(iaq)%len_tot * aqu_d(iaq)%flo / aqu_dat(iaq)%bf_max
      
        !! find the first channel contributing
        icontrib = 0
        do icha = 1, aq_ch(iaq)%num_tot
          if (contrib_len >= aq_ch(iaq)%ch(icha)%len_left) then
            icontrib = icha
            contrib_len_left = aq_ch(iaq)%ch(icha)%len_left + aq_ch(iaq)%ch(icha)%len
            exit
          end if
        end do
        !! set fractions for flow to each channel
        do icha = 1, aq_ch(iaq)%num_tot
          if (icha >= icontrib .and. icontrib > 0) then
            aq_ch(iaq)%ch(icha)%flo_fr = aq_ch(iaq)%ch(icha)%len / contrib_len_left
          else
            aq_ch(iaq)%ch(icha)%flo_fr = 0.
          end if
        end do
        !! save hydrographs to distribute on following day
        aq_ch(iaq)%hd = ob(icmd)%hd(1)
      end if

      !! compute pesticide transport and decay
      do ipest = 1, cs_db%num_pests
        ipest_db = cs_db%pest_num(ipest)
        
        !! set initial pesticide at start of day
        pest_init = cs_aqu(iaq)%pest(ipest)
        
        !! add incoming pesticide to storage
        cs_aqu(iaq)%pest(ipest) = cs_aqu(iaq)%pest(ipest) + obcs(icmd)%hin(1)%pest(ipest)
        
        !! compute pesticide decay in the aquifer
        aqupst_d(iaq)%pest(ipest)%react = 0.
        if (cs_aqu(iaq)%pest(ipest) > 1.e-12) then
          aqupst_d(iaq)%pest(ipest)%react = cs_aqu(iaq)%pest(ipest) * (1. - pestcp(ipest_db)%decay_s)
          cs_aqu(iaq)%pest(ipest) =  cs_aqu(iaq)%pest(ipest) * pestcp(ipest_db)%decay_s
          !! add decay to daughter pesticides
          do imeta = 1, pestcp(ipest_db)%num_metab
            ipseq = pestcp(ipest_db)%daughter(imeta)%num
            ipdb = cs_db%pest_num(ipseq)
            mol_wt_rto = pestdb(ipdb)%mol_wt / pestdb(ipest_db)%mol_wt
            aqupst_d(iaq)%pest(ipseq)%metab = aqupst_d(iaq)%pest(ipseq)%metab + aqupst_d(iaq)%pest(ipest)%react *     &
                                           pestcp(ipest_db)%daughter(imeta)%soil_fr * mol_wt_rto
            cs_aqu(iaq)%pest(ipseq) = cs_aqu(iaq)%pest(ipseq) + aqupst_d(iaq)%pest(ipseq)%metab
          end do
        end if
            
        !! compute pesticide in aquifer flow
        kd = pestdb(ipest_db)%koc * aqu_dat(iaq)%cbn / 100.
        !! assume specific yield = upper limit (effective vs total porosity) 
        !! and bulk density of 2.0 (ave of rock and soil - 2.65 and 1.35)
        !! mm = (mm/mm + (m^3/ton)*(ton/m^3)) * m * 1000.
        zdb1 = (aqu_dat(iaq)%spyld + kd * 2.0) * aqu_dat(iaq)%flo_dist * 1000.

        !! compute volume of flow through the layer - mm
        flow_mm = aqu_d(iaq)%flo + aqu_d(iaq)%seep
        obcs(icmd)%hd(1)%pest(ipest) = 0.
        obcs(icmd)%hd(2)%pest(ipest) = 0.

        !! compute concentration in the flow
        if (cs_aqu(iaq)%pest(ipest) >= 1.e-12 .and. flow_mm > 0.) then
          pest_kg =  cs_aqu(iaq)%pest(ipest) * (1. - Exp(-flow_mm / (zdb1 + 1.e-6)))
          conc = pest_kg / flow_mm
          conc = Min (pestdb(ipest_db)%solub / 100., conc)      ! check solubility
          pest_kg = conc * flow_mm
          if (pest_kg >  cs_aqu(iaq)%pest(ipest)) pest_kg = cs_aqu(iaq)%pest(ipest)
          
          !! return flow (1) and deep seepage (2)  kg = kg/mm * mm
          obcs(icmd)%hd(1)%pest(ipest) = pest_kg * aqu_d(iaq)%flo / flow_mm
          obcs(icmd)%hd(2)%pest(ipest) = pest_kg * aqu_d(iaq)%seep / flow_mm
          cs_aqu(iaq)%pest(ipest) =  cs_aqu(iaq)%pest(ipest) - pest_kg
        endif
      
        !! set pesticide output variables - kg
        aqupst_d(iaq)%pest(ipest)%tot_in = obcs(icmd)%hin(1)%pest(ipest)
        !! assume frsol = 1 (all soluble)
        aqupst_d(iaq)%pest(ipest)%sol_flo = obcs(icmd)%hd(1)%pest(ipest)
        aqupst_d(iaq)%pest(ipest)%sor_flo = 0.      !! all soluble - may add later
        aqupst_d(iaq)%pest(ipest)%sol_perc = obcs(icmd)%hd(2)%pest(ipest)
        aqupst_d(iaq)%pest(ipest)%stor_ave = cs_aqu(iaq)%pest(ipest)
        aqupst_d(iaq)%pest(ipest)%stor_init = pest_init
        aqupst_d(iaq)%pest(ipest)%stor_final = cs_aqu(iaq)%pest(ipest)
      end do
        
      !! compute outflow objects (flow to channels, reservoirs, or aquifer)
      !! if flow from hru is directly routed
      iob_out = icmd
      aqu_d(iaq)%flo_cha = 0.
      aqu_d(iaq)%flo_res = 0.
      aqu_d(iaq)%flo_ls = 0.
      do iout = 1, ob(iob_out)%src_tot
        !! sum outflow to channels, reservoirs and other aquifers
        if (ob(iob_out)%htyp_out(iout) == "tot") then
          select case (ob(iob_out)%obtyp_out(iout))
          case ("cha")
            aqu_d(iaq)%flo_cha = aqu_d(iaq)%flo_cha + aqu_d(iaq)%flo * ob(iob_out)%frac_out(iout)
          case ("sdc")
            aqu_d(iaq)%flo_cha = aqu_d(iaq)%flo_cha + aqu_d(iaq)%flo * ob(iob_out)%frac_out(iout)
          case ("res")
            aqu_d(iaq)%flo_res = aqu_d(iaq)%flo_res + aqu_d(iaq)%flo * ob(iob_out)%frac_out(iout)
          case ("aqu")
            aqu_d(iaq)%flo_ls = aqu_d(iaq)%flo_ls + aqu_d(iaq)%flo * ob(iob_out)%frac_out(iout)
          end select
        end if
      end do

      !! total ingoing and outgoing (retuen flow + percolation) for output to SWIFT
      ob(icmd)%hin_tot = ob(icmd)%hin_tot + ob(icmd)%hin
      ob(icmd)%hout_tot = ob(icmd)%hout_tot + ob(icmd)%hd(1) + ob(icmd)%hd(2)
        
      !if (time%step > 0) then
        do ii = 1, time%step
          step = real(time%step)
          ob(icmd)%ts(1,ii) = ob(icmd)%hd(1) / step
        end do
      !end if

      return
      end subroutine aqu_1d_control