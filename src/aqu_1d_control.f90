      subroutine aqu_1d_control 
    
      use aquifer_module
      use time_module
      use hydrograph_module
      use climate_module, only : wst
      use maximum_data_module
      use constituent_mass_module
      use pesticide_data_module
      use aqu_pesticide_module
      
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
      real :: flow_mm           !mm         |total flow through aquifer - return flow + seepage
      real :: pest_kg           !kg         |soluble pesticide moving with flow
      real :: conc              !kg/mm      |concentration of pesticide in flow
      real :: zdb1              !mm         |kd - flow factor for pesticide transport
      real :: kd                !(mg/kg)/(mg/L) |koc * carbon

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
      
      !! lag recharge from bottom of soil to water table ** disabled
      !aqu_d(iaq)%rchrg = (1. - aqu_prm(iaq)%delay_e) * aqu_d(iaq)%rchrg + aqu_prm(iaq)%delay_e * aqu_st(iaq)%rchrg_prev
      
      aqu_prm(iaq)%rchrg_prev = aqu_d(iaq)%rchrg
      
      !! add recharge to aquifer storage
      aqu_d(iaq)%stor = aqu_d(iaq)%stor + aqu_d(iaq)%rchrg
      
      !! compute groundwater depth from surface
      aqu_d(iaq)%dep_wt = aqudb(iaqdb)%dep_bot - (aqu_d(iaq)%stor / (1000. * aqu_prm(iaq)%spyld))
      aqu_d(iaq)%dep_wt = amax1 (0., aqu_d(iaq)%dep_wt)

      !! compute flow and substract from storage
      if (aqu_d(iaq)%dep_wt <= aqu_prm(iaq)%flo_min) then
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
      aqu_d(iaq)%seep = aqu_d(iaq)%rchrg * aqu_prm(iaq)%seep
      aqu_d(iaq)%seep = amin1 (aqu_d(iaq)%seep, aqu_d(iaq)%stor)
      ob(icmd)%hd(2)%flo = 10. * aqu_d(iaq)%seep * ob(icmd)%area_ha
      
      aqu_d(iaq)%stor = aqu_d(iaq)%stor - aqu_d(iaq)%seep
      
      !! compute revap (deep root uptake from aquifer) and subtract from storage
      if (aqu_d(iaq)%dep_wt < aqu_prm(iaq)%revap_min) then
        aqu_d(iaq)%revap = wst(iwst)%weat%pet * aqu_prm(iaq)%revap_co
        aqu_d(iaq)%revap = amin1 (aqu_d(iaq)%revap, aqu_d(iaq)%stor)
        aqu_d(iaq)%stor = aqu_d(iaq)%stor - aqu_d(iaq)%revap
      else
        aqu_d(iaq)%revap = 0.
      end if

      !! compute nitrate recharge into the aquifer
      aqu_d(iaq)%rchrg_n = ob(icmd)%hin%no3 / (10. * ob(icmd)%area_ha)
      if (ob(icmd)%hin%no3 > 1.) then
        ii = 1
      end if
      aqu_d(iaq)%no3 = aqu_d(iaq)%no3 + aqu_d(iaq)%rchrg_n
      aqu_prm(iaq)%rchrgn_prev = aqu_d(iaq)%rchrg_n
      
      !! compute nitrate return flow out of aquifer
      if (aqu_d(iaq)%stor > 1.e-6) then
        conc_no3 = aqu_d(iaq)%no3 / aqu_d(iaq)%stor
      else
        conc_no3 = 0.
      endif
      ob(icmd)%hd(1)%no3 = conc_no3 * aqu_d(iaq)%flo
      ob(icmd)%hd(1)%no3 = amin1(ob(icmd)%hd(1)%no3, aqu_d(iaq)%no3)
      aqu_d(iaq)%no3 = aqu_d(iaq)%no3 - ob(icmd)%hd(1)%no3
      aqu_d(iaq)%no3gw = ob(icmd)%hd(1)%no3
      
      !revapno3 = conc * revap -- dont include nitrate uptake by plant
      
      !! compute nitrate seepage out of aquifer
      aqu_d(iaq)%seepno3 = conc_no3 * aqu_d(iaq)%seep
      aqu_d(iaq)%seepno3 = amin1(aqu_d(iaq)%seepno3, aqu_d(iaq)%no3)
      aqu_d(iaq)%no3 = aqu_d(iaq)%no3 - aqu_d(iaq)%seepno3
      ob(icmd)%hd(2)%no3 = aqu_d(iaq)%seepno3
      
      !! compute mineral p flow from aquifer - m^3 * ppm * 1000 kg/m^3 = 1/1000
      aqu_d(iaq)%minp = ob(icmd)%hin%flo * aqudb(iaqdb)%minp / 1000.
      !! set hydrograph soluble p from aquifer- convert kg/ha to m3
      ob(icmd)%hd(1)%solp = 10. * aqu_d(iaq)%minp * ob(icmd)%area_ha
      ob(icmd)%hd(1)%solp = ob(icmd)%hin%flo * aqudb(iaqdb)%minp / 1000.

      !! compute fraction of flow to each channel in the aquifer
      !! if connected to aquifer - add flow
      if (db_mx%aqu2d > 0) then
        contrib_len = aq_ch(iaq)%len_tot * aqu_d(iaq)%flo / aqu_prm(iaq)%bf_max
      
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
        cs_aqu(iaq)%pest(ipest) = cs_aqu(iaq)%pest(ipest) + obcs(icmd)%hin%pest(ipest)
        
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
        kd = pestdb(ipest_db)%koc * aqu_d(iaq)%cbn / 100.
        !! assume specific yield = upper limit (effective vs total porosity) 
        !! and bulk density of 2.0 (ave of rock and soil - 2.65 and 1.35)
        !! mm = (mm/mm + (m^3/ton)*(ton/m^3)) * m * 1000.
        zdb1 = (aqu_prm(iaq)%spyld + kd * 2.0) * aqudb(iaqdb)%flo_dist * 1000.

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
        aqupst_d(iaq)%pest(ipest)%tot_in = obcs(icmd)%hin%pest(ipest)
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

      if (time%step > 0) then
        do ii = 1, time%step
          step = real(time%step)
          ob(icmd)%ts(1,ii) = ob(icmd)%hd(1) / step
        end do
      end if

      return
      end subroutine aqu_1d_control