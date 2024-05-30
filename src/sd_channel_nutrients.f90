      subroutine sd_channel_nutrients
    
      use channel_data_module
      use topography_data_module
      use hydrograph_module
      use sd_channel_module
      
      implicit none

      real :: no3_conc              !              |
      integer :: inut               !none          |counter
      integer :: istop              !              |
      real :: ch_len_inc            !              |
      real :: ch_len                !              |
      real :: denit                 !              |
      real :: sed_reduc             !              |
      real :: srp_reduc             !              |
      real :: sed_conc              !mg/L
      real :: tp_conc               !              |
      real :: nd_overb              !              | 
      real :: area_m2               !              | 
      real :: tss_conc              !              |
      real :: sedp_reduc            !              |
     
      inut = 0
      istop = 0
      ch_len_inc = rte_nut(inut)%len_inc
      ch_len = 0.
      denit = 0.
      sed_reduc = 0.
      tp_reduc = 0.
      srp_reduc = 0.
      no3_conc = ht1%no3
      sed_conc = ht1%sed
      tp_conc = ht1%sedp + ht1%solp
      
      !! loop for channel increment
      do while (istop == 0)
        ch_len = ch_len + ch_len_inc
        if (ch_len > 1000. * sd_ch(ich)%chl) then
          ch_len_inc = ch_len - sd_ch(ich)%chl
          istop = 1
        end if
        if (ob(icmd)%props > 0) then   
          !! 2-stage ditch
          if (sd_ch(ich)%overbank == "ob") then
            nd_overb = nd_overb + 1
            !! over-bank full flow
            area_m2 = ch_len_inc * 5.   !assume 5 m - should be hru(j)%field%wid
            if (no3_conc > rte_nut(inut)%no3_min_conc .and. no3_conc > 1.e-6) then
              !alog_no3 = alog(no3_conc)
              !denit_log = rte_nut(inut)%no3_slp_ob * alog(no3_conc) + rte_nut(inut)%no3_int_ob
              denit = 10. ** (rte_nut(inut)%no3_slp_ob * alog(no3_conc) + rte_nut(inut)%no3_int_ob)  !mg-N/m^2/hr
              denit = denit * area_m2 * 24. / 1.e6     !kg/d = mg/m2/h * m2 * hr/d)
              ht2%no3 = ht2%no3 - denit
              ht2%no3 = max(0., ht2%no3)
            end if
            no3_conc = 1000. * ht2%no3 / ht2%flo
            !turbid = ht1%sed / rte_nut(inut)%turb_tss_slp
            !turbid_reduc = - (rte_nut(inut)%turb_slp * turbid + rte_nut(inut)%turb_int) * area_m2
            !turbid_reduc = Max (0., turbid_reduc)
            
            if (tss_conc > rte_nut(inut)%tss_min_conc .and. tss_conc > 1.e-6) then
              sed_reduc = (rte_nut(inut)%tss_slp * tss_conc + rte_nut(inut)%tss_int)        !mg/L/m^2
              sed_reduc = sed_reduc * area_m2 * ht2%flo / 1000000.     !ton/d = mg/L/m^2 * m2 * (t / 1000000000. mg) * flo(m3/d) 1000. L/m3
              ht2%sed = ht2%sed - sed_reduc
              ht2%sed = max(0., ht2%sed)
            end if
            sed_conc = 1000000. * ht2%sed / ht2%flo
            
            if (tp_conc > rte_nut(inut)%tp_min_conc .and. tp_conc > 1.e-6) then
              tp_reduc = (rte_nut(inut)%tp_slp * (tp_conc) + rte_nut(inut)%tp_int)
              tp_reduc = tp_reduc * area_m2 * ht2%flo / 1000.          !kg/d = mg/L/m^2 * m2 * (kg / 1000000. mg) * flo(m3/d) 1000. L/m3
              
               srp_reduc = 0.10 * tp_reduc
               ht2%solp = ht2%solp - srp_reduc
               ht2%solp = max(0., ht2%solp)
            
               sedp_reduc = 0.90 * tp_reduc
               ht2%sedp = ht2%sedp - sedp_reduc
               ht2%sedp = max(0., ht2%sedp)
            end if
            tp_conc = 1000. * (ht1%sedp + ht1%solp) / ht2%flo
            
            ! if (srp_conc > rte_nut(inut)%tp_min_conc) then
            !  srp_reduc = srp_reduc + exp(rte_nut(inut)%srp_slp * alog(tp_reduc) + rte_nut(inut)%srp_int)
            ! srp_reduc = 1000. * srp_reduc / ht2%flo !kg =  1000 *mg/L / flow
            ! ht2%solp = ht2%solp - srp_reduc
            ! end if
            !srp_conc = 1000. * ht2%srp / ht2%flo
            
          else
              
            !! under-bank full flow
            if (no3_conc > rte_nut(inut)%no3_min_conc .and. no3_conc > 1.e-6) then
              denit = 10. ** (rte_nut(inut)%no3_slp_ub * alog(no3_conc) + rte_nut(inut)%no3_int_ub)  !mg-N/m^2/hr
              denit = denit * area_m2 * 24. / 1.e6     !kg/d = mg/m2/h * m2 * hr/d)
              ht2%no3 = ht2%no3 - denit
              ht2%no3 = max(0., ht2%no3)
            end if
            no3_conc = 1000. * ht2%no3 / ht2%flo
          end if
        else
          !! single stage ditch
        
          if (no3_conc > rte_nut(inut)%no3_min_conc .and. no3_conc > 1.e-6) then
            denit = 10. ** (rte_nut(inut)%no3_slp * alog(no3_conc) + rte_nut(inut)%no3_int) !mg-N/m^2/hr
            denit = denit * area_m2 * 24. / 1.e6     !kg/d = mg/m2/h * m2 * hr/d)
            ht2%no3 = ht2%no3 - denit
            ht2%no3 = max(0., ht2%no3)
          end if
          no3_conc = 1000. * ht2%no3 / ht2%flo
        end if

      end do
        
      
      return
      
      end subroutine sd_channel_nutrients