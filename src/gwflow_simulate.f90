      subroutine gwflow_simulate

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates new groundwater storage and solute mass for each gwflow grid cell;
!!    also, computes and write out daily/annual/average annual fluxes and mass balance error

      use gwflow_module
      use hydrograph_module
      use hru_module
      use sd_channel_module
      use time_module
      use soil_module

        implicit none

        external :: gwflow_canal_ext, gwflow_chem, gwflow_gwet, &
                    gwflow_pump_ext, gwflow_rech, gwflow_phreatophyte, &
                    gwflow_pond, gwflow_output_day, gwflow_output_mon, &
                    gwflow_output_yr, gwflow_output_aa

        !counters and general information
        !counters and general
        integer :: i = 0
        integer :: j = 0
        integer :: k = 0
        integer :: s = 0
        integer :: dum = 0
        integer :: cell_id = 0
        integer :: num_months = 0
        real :: sum = 0.
        real :: gw_storage = 0.
        real :: gw_heat = 0.
        real :: gw_temp = 0.
        real :: gwsw_sum(100) = 0.
        real, allocatable, save :: obs_vals(:)
        !tile drainage outflow
        real :: sum_tile(50) = 0.
        real :: sum_mass(50,100) = 0.
        real :: c_tile(50,100) = 0.
        !flow time stepping
        integer :: num_ts = 0
        integer :: count = 0
        real :: depth_wt_avg = 0.
        real :: temp_avg = 0.
        !solute transport (needed for beginning-of-day prep)
        real :: gw_trans_time_step = 0.
        real :: mass_adv(100) = 0.
        real :: mass_dsp(100) = 0.
        real :: mass_sorb(100) = 0.




      !record file
      write(out_gw,*) 'gwflow subroutine called:',time%yrc,time%day


      !1. calculate the available volume of groundwater (m3) in each cell ---------------------------------------------
      do i=1,ncell
        if(gw_state(i)%stat.gt.0) then
          if(gw_state(i)%head > gw_state(i)%botm) then
            gw_state(i)%stor = ((gw_state(i)%head - gw_state(i)%botm) * gw_state(i)%area) * gw_state(i)%spyd
          else
            gw_state(i)%stor = 0.
          endif
        endif
      enddo
      !available heat (J) in each cell
      if(gw_heat_flag == 1) then
        do i=1,ncell
          gw_temp = gwheat_state(i)%temp
          gw_storage = gw_state(i)%stor
          gw_heat = gwheat_state(i)%temp * gw_rho * gw_cp * gw_state(i)%stor !J
          gwheat_state(i)%stor = gwheat_state(i)%temp * gw_rho * gw_cp * gw_state(i)%stor !J
        enddo
      endif



      !2. calculate groundwater sources and sinks (m3) for each cell --------------------------------------------------

      !recharge ---------------------------------------------------------------
      call gwflow_rech

      !groundwater ET ---------------------------------------------------------
      call gwflow_gwet

      !phreatophyte transpiration ---------------------------------------------
      call gwflow_phreatophyte

      !groundwater-channel exchange -------------------------------------------
      !gwflow_gwsw called in sd_channel_control

      !groundwater saturation excess flow -------------------------------------
      !gwflow_satx called in sd_channel_control

      !groundwater-->soil exchange --------------------------------------------
      !gwflow_soil called in swr_percmain

      !groundwater pumping (irrigation) ---------------------------------------
      !gwflow_ppag called in wallo_withdraw
      !track and print out pumping for HRUs
      do i=1,sp_ob%hru
        hru_pump_mo(i) = hru_pump_mo(i) + hru_pump(i)
        hru_pump_yr(i) = hru_pump_yr(i) + hru_pump(i)
      enddo
      if(hru_pump_flag == 1 .and. gwflag_pump == 1) then
        do i=1,num_hru_pump_obs
          hru_pump_obs(i) = hru_pump(hru_pump_ids(i))
        enddo
        write(out_hru_pump_obs,119) time%yrc,time%day, &
                                   (hru_pump_obs(i),i=1,num_hru_pump_obs)
      endif
      hru_pump = 0.

      !groundwater pumping (external use) -------------------------------------
      call gwflow_pump_ext

      !discharge from groundwater to tile drains ------------------------------
      !gwflow_tile called in sd_channel_control
      !retrieve information for tile cell groups
      if(gw_tile_flag == 1) then
        !compute flow rate and solute concentration for the tile cell groups
        if(gw_tile_group_flag.eq.1) then
          do i=1,gw_tile_num_group
            sum_tile(i) = 0.
            do j=1,num_tile_cells(i)
              sum_tile(i) = sum_tile(i) + gw_hyd_ss(gw_tile_groups(i,j))%tile !m3
            enddo
            sum_tile(i) = (sum_tile(i)*(-1)) / 86400. !m3 --> m3/sec
            if(gw_solute_flag == 1) then
              do s=1,gw_nsolute !loop through the solutes
                sum_mass(i,s) = 0.
              enddo
              do j=1,num_tile_cells(i)
                do s=1,gw_nsolute !loop through the solutes
                  sum_mass(i,s) = sum_mass(i,s) + &
                                  gwsol_ss(gw_tile_groups(i,j))%solute(s)%tile !g
                enddo
              enddo
              if(sum_tile(i).ne.0) then
                do s=1,gw_nsolute !loop through the solutes
                  c_tile(i,s) = sum_mass(i,s) / sum_tile(i) !g/m3
                enddo
              else
                do s=1,gw_nsolute !loop through the solutes
                  c_tile(i,s) = 0. !g/m3
                enddo
              endif
            endif
          enddo
          if(gwflag_flux == 1) then
            if(gw_solute_flag == 1) then
              write(out_tile_cells,8130) time%day,time%mo,time%day_mo,time%yrc, &
                                       (sum_tile(i),i=1,gw_tile_num_group), &
                                       (c_tile(i,1),i=1,gw_tile_num_group), &
                                       (c_tile(i,2),i=1,gw_tile_num_group)
            else
              write(out_tile_cells,8130) time%day,time%mo,time%day_mo,time%yrc, &
                                       (sum_tile(i),i=1,gw_tile_num_group)
            endif
          endif
        endif
      endif

      !groundwater exchange with reservoirs -----------------------------------
      !gwflow_resv called in res_control

      !groundwater-wetland exchange -------------------------------------------
      !gwflow_wetl called in wetland_control

      !groundwater-channel exchange from floodplain cells ---------------------
      !gwflow_fpln called in sd_channel_control

      !seepage from irrigation canals to groundwater --------------------------
      !gwflow_canl called in sd_channel_control

      !seepage from irrigation canals to groundwater --------------------------
      !(for canals that originate outside the model boundary)
      call gwflow_canal_ext

      !for canal diversions: determine daily volume and daily return
      !only operative if ponds and/or canals are active
      do i=1,gw_ncanal
        !current day's diversion volume for the canal
        if(gw_canl_div_info(i)%divr > 0) then
          gw_canl_div_info(i)%div = recall(gw_canl_div_info(i)%divr)%hd(time%day,time%yrs)%flo * (-1) !m3; make positive
        else
          gw_canl_div_info(i)%div = 0.
        endif
        gw_canl_div_info(i)%stor = gw_canl_div_info(i)%div
        !zero out for daily water balance
        gw_canl_div_info(i)%out_pond = 0.
        gw_canl_div_info(i)%out_seep = 0.
      enddo

      !seepage from recharge ponds to groundwater -----------------------------
      call gwflow_pond

      !seepage from irrigation canals to groundwater --------------------------
      !(for canals associated with point source diversions)
      call gwflow_canal_div

      !3. sum sources/sinks for each grid cell ------------------------------------------------------------------------
      !m3 for water
      do i=1,ncell
        if(gw_state(i)%stat == 1) then
          !do not include gwsw and swsw (already accounted for in gwflow_gwsw)
          gw_hyd_ss(i)%totl = gw_hyd_ss(i)%rech + gw_hyd_ss(i)%gwet + & !gw_hyd_ss(i)%gwsw + gw_hyd_ss(i)%swgw
                               gw_hyd_ss(i)%satx + gw_hyd_ss(i)%soil + &
                               gw_hyd_ss(i)%ppag + gw_hyd_ss(i)%ppex + gw_hyd_ss(i)%tile + &
                               gw_hyd_ss(i)%resv + gw_hyd_ss(i)%wetl + gw_hyd_ss(i)%canl + &
                               gw_hyd_ss(i)%fpln + gw_hyd_ss(i)%pond + gw_hyd_ss(i)%phyt
        endif
      enddo

      !write out gwsw cell groups (sum of gw-channel exchange for each channel cell group)
      if(gw_gwsw_group_flag == 1) then
        gwsw_sum = 0.
        do i=1,gw_gwsw_ngroup
          do j=1,gw_gwsw_ncell(i)
            cell_id = gw_gwsw_group(i,j)
            gwsw_sum(i) = gwsw_sum(i) + gw_hyd_ss(cell_id)%gwsw + gw_hyd_ss(cell_id)%swgw
          enddo
        enddo
        if(gwflag_flux == 1) then
          write(out_gwsw_groups,8130) time%day,time%mo,time%day_mo,time%yrc,(gwsw_sum(i),i=1,gw_gwsw_ngroup)
        endif
      endif

      !write out channel cell values, for specified channel cells
      if(gw_chan_obs_flag == 1) then
        if(.not.allocated(obs_vals)) allocate(obs_vals(gw_chan_nobs))
        !flow
        do i=1,gw_chan_nobs
          cell_id = gw_chan_obs_cell(i)
          obs_vals(i) = gw_hyd_ss(cell_id)%gwsw + gw_hyd_ss(cell_id)%swgw + gw_hyd_ss(cell_id)%satx
        enddo
        if(gwflag_flux == 1) then
          write(out_gwsw_chanobs_flow,8130) time%day,time%mo,time%day_mo,time%yrc,(obs_vals(i),i=1,gw_chan_nobs)
          if(gw_solute_flag == 1) then
            do i=1,gw_chan_nobs
              cell_id = gw_chan_obs_cell(i)
              obs_vals(i) = gwsol_ss(cell_id)%solute(1)%gwsw + gwsol_ss(cell_id)%solute(1)%swgw + gwsol_ss(cell_id)%solute(1)%satx
            enddo
            write(out_gwsw_chanobs_no3,8130) time%day,time%mo,time%day_mo,time%yrc,(obs_vals(i),i=1,gw_chan_nobs)
          endif
        endif
      endif

      !J for heat
      if(gw_heat_flag == 1) then
        do i=1,ncell
          if(gw_state(i)%stat == 1) then
            gw_heat_ss(i)%totl = gw_heat_ss(i)%rech + & !recharge heat
                                 gw_heat_ss(i)%gwet + & !gwet heat
                                 gw_heat_ss(i)%gwsw + & !groundwater-->channel heat exchange
                                 gw_heat_ss(i)%swgw + & !channel-->groundwater heat exchange
                                 gw_heat_ss(i)%satx + & !saturation excess flow heat
                                 gw_heat_ss(i)%soil + & !groundwater-->soil heat transfer
                                 gw_heat_ss(i)%ppag + & !heat in pumping for irrigation
                                 gw_heat_ss(i)%ppex + & !heat in user-defined pumping
                                 gw_heat_ss(i)%tile + & !heat in tile drainage water
                                 gw_heat_ss(i)%resv + & !heat in groundwater-reservoir exchange water
                                 gw_heat_ss(i)%wetl + & !heat in groundwater inflow to wetlands
                                 gw_heat_ss(i)%canl + & !heat in groundwater-canal exchange
                                 gw_heat_ss(i)%fpln + & !heat in groundwater-floodplain exchange
                                 gw_heat_ss(i)%pond     !heat in recharge pond water
          endif
        enddo
      endif

      !g for solutes
      if(gw_solute_flag == 1) then
        do i=1,ncell
          if(gw_state(i)%stat == 1) then
            do s=1,gw_nsolute !loop through the solutes
              gwsol_ss(i)%solute(s)%totl = gwsol_ss(i)%solute(s)%rech + & !recharge
                                           gwsol_ss(i)%solute(s)%gwsw + & !gw-->channel
                                           gwsol_ss(i)%solute(s)%swgw + & !channel-->gw
                                           gwsol_ss(i)%solute(s)%satx + & !gw-->channel
                                           gwsol_ss(i)%solute(s)%soil + & !gw-->soil
                                           gwsol_ss(i)%solute(s)%ppag + & !pumping (irrigation)
                                           gwsol_ss(i)%solute(s)%ppex + & !pumping (external)
                                           gwsol_ss(i)%solute(s)%tile + & !tile outflow
                                           gwsol_ss(i)%solute(s)%resv + & !reservoir exchange
                                           gwsol_ss(i)%solute(s)%wetl + & !wetland
                                           gwsol_ss(i)%solute(s)%canl + & !canal exchange
                                           gwsol_ss(i)%solute(s)%fpln + & !floodplain exchange
                                           gwsol_ss(i)%solute(s)%pond     !recharge pond
            enddo
          endif
        enddo
      endif


      ! 4. calculate new groundwater storage and head for each grid cell ----------------------------------------------

      !compute groundwater volume and solute mass at beginning of day
      do i=1,ncell
        if(gw_state(i)%stat == 1) then
          gw_state(i)%vbef = (gw_state(i)%head-gw_state(i)%botm) * gw_state(i)%area * gw_state(i)%spyd !m3
          gw_storage = gw_state(i)%vbef
        endif
      enddo
      !compute cell heat at the beginning of the day
      if(gw_heat_flag == 1) then
        do i=1,ncell
          if(gw_state(i)%stat == 1) then
            if(gw_state(i)%vbef > 0.) then
              gwheat_state(i)%hbef = gwheat_state(i)%temp * gw_rho * gw_cp * gw_state(i)%vbef !J
            else
              gwheat_state(i)%hbef = 0.
            endif
            gw_heat = gwheat_state(i)%hbef
            gw_storage = gw_state(i)%vbef
          endif
        enddo
      endif
      !compute cell solute mass at the beginning of the day
      if(gw_solute_flag == 1) then
        do i=1,ncell
          if(gw_state(i)%stat == 1) then
            do s=1,gw_nsolute !loop through the solutes
              gwsol_state(i)%solute(s)%mbef = gwsol_state(i)%solute(s)%mass
            enddo
          endif
        enddo
      endif

      !determine number of flow time steps; determine size of transport time step
      num_ts = int(1./gw_time_step)
      if(gw_solute_flag == 1) then
        gw_trans_time_step = gw_time_step / num_ts_transport
      endif

      !prepare arrays
      do i=1,ncell
        gw_state(i)%hnew = 0.
        gw_state(i)%hold = 0.
      enddo
      if(gw_heat_flag == 1) then
          do i=1,ncell
            gwheat_state(i)%tnew = 0.
          enddo
      endif
      if(gw_solute_flag == 1) then
        do i=1,ncell
          do s=1,gw_nsolute
            gwsol_state(i)%solute(s)%cnew = 0.
          enddo
        enddo
      endif

      !calculate new storage, head, heat, and solute concentrations via lateral flow
      call gwflow_lateral

      !transit time update (deferred)


      !5. save/write out head and solute concentrations ---------------------------------------------------------------

      !save head, temperature, and solute concentration values for monthly and annual averages output
      do i=1,ncell
        gw_state(i)%hdmo = gw_state(i)%hdmo + gw_state(i)%head
        gw_state(i)%hdyr = gw_state(i)%hdyr + gw_state(i)%head
      enddo
      if(gw_heat_flag == 1) then
        do i=1,ncell
          gwheat_state(i)%tpmo = gwheat_state(i)%tpmo + gwheat_state(i)%temp
          gwheat_state(i)%tpyr = gwheat_state(i)%tpyr + gwheat_state(i)%temp
        enddo
      endif
      if(gw_solute_flag == 1) then
        do i=1,ncell
          do s=1,gw_nsolute !loop through the solutes
            gwsol_state(i)%solute(s)%cnmo = gwsol_state(i)%solute(s)%cnmo + gwsol_state(i)%solute(s)%conc
            gwsol_state(i)%solute(s)%cnyr = gwsol_state(i)%solute(s)%cnyr + gwsol_state(i)%solute(s)%conc
          enddo
        enddo
      endif

      !(specified-time head/temp/conc grid dump removed -- cell data now in long-format output)
      if(gw_output_index.le.gw_num_output) then
      if(gw_output_yr(gw_output_index).eq.time%yrc .and. gw_output_day(gw_output_index).eq.time%day) then
        gw_output_index = gw_output_index + 1
      endif
      endif

      !calculate the average depth to water table
      sum = 0.
      count = 0
      do i=1,ncell
        if(gw_state(i)%stat == 1) then
          sum = sum + (gw_state(i)%elev - gw_state(i)%head)
          count = count + 1
        endif
      enddo
      depth_wt_avg = sum / count

      !calculate the average groundwater temperature
      if(gw_heat_flag == 1) then
        sum = 0.
        count = 0
        do i=1,ncell
          if(gw_state(i)%stat == 1) then
            sum = sum + gwheat_state(i)%temp
            count = count + 1
          endif
        enddo
        temp_avg = sum / count
      endif

      !print out head values and solute concentration values for observation cells (each time step)
      do k=1,gw_num_obs_wells
        gw_obs_head(k) = gw_state(gw_obs_cells(k))%head
        if(gw_heat_flag == 1) then
          gw_obs_temp(k) = gwheat_state(gw_obs_cells(k))%temp
        endif
        if(gw_solute_flag == 1) then
          do s=1,gw_nsolute !loop through the solutes
            gw_obs_solute(k,s) = gwsol_state(gw_obs_cells(k))%solute(s)%conc
          enddo
        endif
      enddo
      !(obs well writes moved to gwflow_output_day)

      !compute groundwater volumes at the end of the day
      do i=1,ncell
        if(gw_state(i)%stat == 1) then
          gw_state(i)%vaft = ((gw_state(i)%head - gw_state(i)%botm) * gw_state(i)%area) * gw_state(i)%spyd
        endif
      enddo
      !compute groundwater heat at the end of the day
      if(gw_heat_flag == 1) then
        do i=1,ncell
          if(gw_state(i)%stat == 1) then
            gwheat_state(i)%haft = gwheat_state(i)%stor
          endif
        enddo
      endif
      !compute solute mass at the end of the day
      if(gw_solute_flag == 1) then
        do i=1,ncell
          if(gw_state(i)%stat == 1) then
            do s=1,gw_nsolute !loop through the solutes
              gwsol_state(i)%solute(s)%maft = gwsol_state(i)%solute(s)%mass !g
            enddo
          endif
        enddo
      endif


      !6. compute and write out daily fluxes and mass balance error
      call gwflow_output_day


      !7. monthly output
      if(time%end_mo == 1) call gwflow_output_mon


      !8. yearly output
      if(time%end_yr == 1) call gwflow_output_yr


      !9. last day of the simulation reached: print out average annual values
      if(time%yrc == time%yrc_end .and. time%day == time%day_end) then

        call gwflow_output_aa

      endif



      !10. zero out flux arrays for next day
      !flow -----------------------------------------------
      do i=1,ncell
        gw_hyd_ss(i)%rech = 0.
        gw_hyd_ss(i)%gwet = 0.
        gw_hyd_ss(i)%gwsw = 0.
        gw_hyd_ss(i)%swgw = 0.
        gw_hyd_ss(i)%satx = 0.
        gw_hyd_ss(i)%soil = 0.
        gw_hyd_ss(i)%latl = 0.
        gw_hyd_ss(i)%bndr = 0.
        gw_hyd_ss(i)%ppag = 0.
        gw_hyd_ss(i)%ppdf = 0.
        gw_hyd_ss(i)%ppex = 0.
        gw_hyd_ss(i)%tile = 0.
        gw_hyd_ss(i)%resv = 0.
        gw_hyd_ss(i)%wetl = 0.
        gw_hyd_ss(i)%canl = 0.
        gw_hyd_ss(i)%fpln = 0.
        gw_hyd_ss(i)%pond = 0.
        gw_hyd_ss(i)%phyt = 0.
        gw_hyd_ss(i)%totl = 0.
      enddo
      satx_count = 0
      !heat -----------------------------------------------
      if(gw_heat_flag == 1) then
        do i=1,ncell
          gw_heat_ss(i)%rech = 0.
          gw_heat_ss(i)%gwet = 0.
          gw_heat_ss(i)%gwsw = 0.
          gw_heat_ss(i)%swgw = 0.
          gw_heat_ss(i)%satx = 0.
          gw_heat_ss(i)%soil = 0.
          gw_heat_ss(i)%latl = 0.
          gw_heat_ss(i)%disp = 0.
          gw_heat_ss(i)%bndr = 0.
          gw_heat_ss(i)%ppag = 0.
          gw_heat_ss(i)%ppex = 0.
          gw_heat_ss(i)%tile = 0.
          gw_heat_ss(i)%resv = 0.
          gw_heat_ss(i)%wetl = 0.
          gw_heat_ss(i)%canl = 0.
          gw_heat_ss(i)%fpln = 0.
          gw_heat_ss(i)%pond = 0.
          gw_heat_ss(i)%totl = 0.
        enddo
      endif
      !solutes --------------------------------------------
      if(gw_solute_flag == 1) then
        do i=1,ncell
          do s=1,2 !only for no3 and p
            gwsol_ss(i)%solute(s)%rech = 0.
            gwsol_ss(i)%solute(s)%gwsw = 0.
            gwsol_ss(i)%solute(s)%swgw = 0.
            gwsol_ss(i)%solute(s)%satx = 0.
            gwsol_ss(i)%solute(s)%soil = 0.
            gwsol_ss(i)%solute(s)%ppag = 0.
            gwsol_ss(i)%solute(s)%ppex = 0.
            gwsol_ss(i)%solute(s)%tile = 0.
            gwsol_ss(i)%solute(s)%resv = 0.
            gwsol_ss(i)%solute(s)%wetl = 0.
            gwsol_ss(i)%solute(s)%canl = 0.
            gwsol_ss(i)%solute(s)%fpln = 0.
            gwsol_ss(i)%solute(s)%pond = 0.
            gwsol_ss(i)%solute(s)%advn = 0.
            gwsol_ss(i)%solute(s)%disp = 0.
            gwsol_ss(i)%solute(s)%rcti = 0.
            gwsol_ss(i)%solute(s)%rcto = 0.
            gwsol_ss(i)%solute(s)%minl = 0.
            gwsol_ss(i)%solute(s)%sorb = 0.
            gwsol_ss(i)%solute(s)%totl = 0.
          enddo
          do s=3,gw_nsolute !for salt and cs: do not zero out for recharge, reactions, or sorption (needed in salt_balance and cs_balance)
            !gwsol_ss(i)%solute(s)%rech = 0.
            gwsol_ss(i)%solute(s)%gwsw = 0.
            gwsol_ss(i)%solute(s)%swgw = 0.
            gwsol_ss(i)%solute(s)%satx = 0.
            gwsol_ss(i)%solute(s)%soil = 0.
            gwsol_ss(i)%solute(s)%ppag = 0.
            gwsol_ss(i)%solute(s)%ppex = 0.
            gwsol_ss(i)%solute(s)%tile = 0.
            gwsol_ss(i)%solute(s)%resv = 0.
            gwsol_ss(i)%solute(s)%wetl = 0.
            gwsol_ss(i)%solute(s)%canl = 0.
            gwsol_ss(i)%solute(s)%fpln = 0.
            gwsol_ss(i)%solute(s)%pond = 0.
            gwsol_ss(i)%solute(s)%advn = 0.
            gwsol_ss(i)%solute(s)%disp = 0.
            !gwsol_ss(i)%solute(s)%rcti = 0.
            !gwsol_ss(i)%solute(s)%rcto = 0.
            !gwsol_ss(i)%solute(s)%minl = 0.
            !gwsol_ss(i)%solute(s)%sorb = 0.
            gwsol_ss(i)%solute(s)%totl = 0.
          enddo
        enddo
        mass_adv = 0.
        mass_dsp = 0.
        mass_rct = 0.
        mass_sorb = 0.
      endif

      !read channel depths for next day
      if(gw_chan_dep_flag == 1) then
        read(1421,*) dum,dum,(gw_chan_dep(j),j=1,gw_chan_ndpzn)
      endif



100   format(10000(f12.3))
101   format(10000(e12.3))
102   format(i8,i8,f10.3,e16.7,e16.7,1000(e13.4))
103   format(i8,i8,i8,i8,i8,i8,i8,50(f15.3))
104   format(10000(f12.2))
105   format(i8,1000(e13.4))
106   format(i8,i8,i8,50(f12.3))
108   format(i8,2x,50(e12.4))
109   format(i8,i8,1000(e12.3))
110   format(i8,f20.1,i8,f12.3,f12.3,f12.3)
111   format(f20.1,f12.3,f12.3,i8)
112   format(f15.1,50(e13.4))
113   format(i8,1000(f12.3))
114   format(i8,i8,1000(e12.3))
115   format(f12.3,f12.3,f12.3,f12.3,i8,i8)
116   format(f20.1,f12.3,f12.3,f12.3,f12.3,i8,i8)
118   format(i8,i8,1000(f12.3))
119   format(i8,i8,i8,1000(f12.3))
130   format(i8,i8,i8,1000(e13.4))
131   format(i8,i8,1000(e13.4))
      !wide-format with standard 4-int time prefix (jday,mon,day,yr)
8130  format(4i6,1000(e13.4))

125   format(3x,i8,2x,i8,7x,f15.1,50(e13.4))
126   format(i8,i8,e18.9,e18.9,1000(e18.9))
127   format(i8,i8,f10.3,e18.9,e18.9,1000(e18.9))


      return
      end subroutine gwflow_simulate
