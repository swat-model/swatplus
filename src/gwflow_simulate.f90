      !This subroutine performs the following operations:
      !  1. Convert SWAT+ variables to grid-based variables
      !  2. Calculate new groundwater head for each grid cell     
      !  3. Transfer groundwater-surface water exchange rates to SWAT+
      !  4. Write out results (heads, groundwater balance, gw-sw exchange rates)
  
      !  Prepared by: Ryan Bailey, Colorado State University
      !  January-April 2020
  
      subroutine gwflow_simulate
      
      use gwflow_module
      use hydrograph_module
      use hru_module
      use sd_channel_module
      use time_module
      use soil_module
      
      implicit none
      
      !flow variables
      integer  i,j,k,n,ob_num,ob_num_aqu,num_hru,row,col,chan_num,chan_gis,num_ts,dum,chan_ob_num
      integer  ts_decrease
      integer  riv_num,riv_ob_num,count,cell_row,cell_col,cell_count
      real     max_ts,thick_west,thick_east,thick_north,thick_south
      real     recharge
      real     max_gwet,et_surface,et_bottom,gw_head,gwet
      real     chan_depth,chan_width,chan_length,chan_stage,flow_area,head_diff,Q
      real     sat_thick,ts_stable
      real     Q_west,Q_east,Q_north,Q_south,face_K,face_sat,gradient,head_change,sat_thick1,sat_thick2
      real     storage_change,ss_rech,ss_et,ss_gw,ss_sw,ss_satex,ss_tran,ss_pumpag,ss_pumpex,ss_Q,ss_tile, &
               ss_lake,mass_error,sum_ss
      real     gw_cell_Q_total,gw_cell_ss_rech_total,gw_cell_ss_et_total,gw_cell_ss_gwsw_total,gw_cell_ss_swgw_total, &
               gw_cell_ss_satex_total,gw_cell_ss_tran_total,gw_cell_ss_pumpag_total,gw_cell_ss_pumpex_total, &
               gw_cell_ss_tile_total,gw_cell_ss_lake_total
			real     tile_elev,ksat
      real     gwet_volume,rech_volume,cell_rech_volume,sum
      real     satex_depth,satex_volume,frac_sat,depth_wt_avg
      real     sum_tile(50),sum_massn(50),sum_massp(50),cn_tile(50),cp_tile(50)
      real     sub_recharge,sub_nmass,sub_pmass
      real     hru_total,hru_cell_total,huc12_cell_total
      double precision gw_volume_before,gw_volume_after
      !groundwater-soil transfer variables
      integer  transfer_flag
      real     tran_volume,tran_depth,dist_above,hru_area_m2,water_depth_tot,layer_fraction,layer_transfer,sol_thick,gwsum1,gwsum2
      real     tran_massn,tran_massp
      real     hru_soilz(sp_ob%hru),gwvol_hru(sp_ob%hru),vadose_hru(sp_ob%hru),vadose_thick(grid_nrow,grid_ncol),water_depth(10)
      real     gwmassn_hru(sp_ob%hru),gwmassp_hru(sp_ob%hru)
      !pumping
      integer  pumpex_start_date,pumpex_end_date
      !transport variables
      integer  t
      real     gw_trans_time_step,time_fraction
      real     gw_volume_old,gw_volume_new,gw_volume_inter
      !no3
      real     recharge_n,rech_nmass,cell_rech_nmass,nmass,chan_cn,nmass_west,nmass_east,nmass_north,nmass_south,cn_change,satex_nmass
      real     ss_rechn,ss_gwn,ss_swn,ss_satexn,ss_advn,ss_dspn,ss_rctn,ss_pumpagn,ss_pumpexn,ss_tilen,ss_trann,ss_laken
      real     gw_cell_ss_rechn_total,gw_cell_ss_gwswn_total,gw_cell_ss_swgwn_total,gw_cell_ss_satexn_total, &
               gw_cell_advn_total,gw_cell_dspn_total,gw_cell_rctn_total,gw_cell_ss_pumpagn_total,gw_cell_ss_pumpexn_total, &
               gw_cell_ss_tilen_total,gw_cell_ss_trann_total,gw_cell_ss_laken_total
      real     mn_change
      double precision gw_nmass_before,gw_nmass_after
      !p
      real     recharge_p,rech_pmass,cell_rech_pmass,pmass,chan_cp,pmass_west,pmass_east,pmass_north,pmass_south,cp_change,satex_pmass
      real     ss_rechp,ss_gwp,ss_swp,ss_satexp,ss_advp,ss_dspp,ss_rctp,ss_pumpagp,ss_pumpexp,ss_tilep,ss_tranp,ss_lakep
      real     gw_cell_ss_rechp_total,gw_cell_ss_gwswp_total,gw_cell_ss_swgwp_total,gw_cell_ss_satexp_total,gw_cell_advp_total, &
               gw_cell_dspp_total,gw_cell_rctp_total,gw_cell_ss_pumpagp_total,gw_cell_ss_pumpexp_total,gw_cell_ss_tilep_total, &
               gw_cell_ss_tranp_total,gw_cell_ss_lakep_total
      real     mp_change
      double precision gw_pmass_before,gw_pmass_after
      !hru
      integer  hru_id
      real     sum10

      
      !number of HRUs
      num_hru = sp_ob%hru

      !calculate the available volume of groundwater in each cell
      do i=1,grid_nrow
        do j=1,grid_ncol
          if(gw_cell_status(i,j).gt.0) then
            gw_avail(i,j) = ((gw_cell_head(i,j)-gw_cell_bot(i,j)) * (cell_size * cell_size)) * gw_cell_Sy(i,j) !m3 of groundwater
          endif
        enddo
      enddo
      

      
      ! 1) groundwater sources and sinks ----------------------------------------------------------------------------------------
      
      !calculate recharge and nitrate to the water table, based on the percolation from the soil profile
      if(gw_rech_flag.eq.0) then
        gwflow_perc = 0.
			endif
      do k=1,num_hru
        recharge = gw_rech(k)
        gw_rech(k) = 0.
        gw_rech(k) = ((1.-gw_delay(k))*gwflow_perc(k)) + (gw_delay(k)*recharge)
        if (gw_rech(k) < 1.e-6) gw_rech(k) = 0.
        if(gw_transport_flag.eq.1) then
          recharge_n = gw_rechn(k)
          gw_rechn(k) = ((1.-gw_delay(k))*gwflow_percn(k)) + (gw_delay(k)*recharge_n)
          recharge_p = gw_rechp(k)
          gw_rechp(k) = ((1.-gw_delay(k))*gwflow_percp(k)) + (gw_delay(k)*recharge_p)
        endif
      enddo
      
      !map the recharge from the HRUs to the grid cells
      if(nat_model) then !national model application
        !loop through the HUC12 subwatersheds
        cell_received = 0
        ob_num = sp_ob1%hru  !object number of first HRU
        hru_total = 0.
        hru_cell_total = 0.
        huc12_cell_total = 0.
        do n=1,sp_ob%outlet
          !loop through the HRUs in the subwatershed - assign recharge to any connected cells
          sub_recharge = 0.
          sub_nmass = 0.
          sub_pmass = 0.
          do k=1,huc12_nhru(n)
            hru_id = huc12_hrus(n,k)
            rech_volume = (gw_rech(hru_id)/1000.) * (ob(ob_num)%area_ha * 10000.) !m * m2 = m3
            hru_total = hru_total + rech_volume
            rech_nmass = gw_rechn(hru_id) * ob(ob_num)%area_ha * 1000. !g of no3-n
            rech_pmass = gw_rechp(hru_id) * ob(ob_num)%area_ha * 1000. !g of p
            if(hrus_connected(hru_id).eq.1) then
              do i=1,hru_num_cells(hru_id)
                cell_row = hru_cells(hru_id,i,1)
                cell_col = hru_cells(hru_id,i,2)
                cell_received(cell_row,cell_col) = 1
                cell_rech_volume = rech_volume * hru_cells_fract(hru_id,i)
                hru_cell_total = hru_cell_total + cell_rech_volume
                gw_cell_ss_rech(cell_row,cell_col) = gw_cell_ss_rech(cell_row,cell_col) + cell_rech_volume
                gwflow_rech_sum(cell_row,cell_col) = gwflow_rech_sum(cell_row,cell_col) + cell_rech_volume
                if(gw_transport_flag.eq.1) then
                  cell_rech_nmass = rech_nmass * hru_cells_fract(hru_id,i)
                  cell_rech_pmass = rech_pmass * hru_cells_fract(hru_id,i)
                  gw_cell_ss_rechn(cell_row,cell_col) = gw_cell_ss_rechn(cell_row,cell_col) + cell_rech_nmass
                  gwflow_rechn_sum(cell_row,cell_col) = gwflow_rechn_sum(cell_row,cell_col) + cell_rech_nmass
                  gw_cell_ss_rechp(cell_row,cell_col) = gw_cell_ss_rechp(cell_row,cell_col) + cell_rech_pmass
                  gwflow_rechp_sum(cell_row,cell_col) = gwflow_rechp_sum(cell_row,cell_col) + cell_rech_pmass
                endif
              enddo      
            else
              sub_recharge = sub_recharge + rech_volume
              sub_nmass = sub_nmass + rech_nmass
              sub_pmass = sub_pmass + rech_pmass
            endif
            ob_num = ob_num + 1
          enddo
          !loop through the cells in the subwatershed - assign remaining recharge to unconnected cells
          !first: count the number of cells in each subwatershed that did not receive recharge from an HRU
          cell_count = 0
          do k=1,huc12_ncell(n)
            cell_row = huc12_cells(n,k,1)
            cell_col = huc12_cells(n,k,2)
            if(cell_received(cell_row,cell_col).eq.0) then !has not been given recharge from an HRU
              cell_count = cell_count + 1
            endif
          enddo
          !second: calculate the recharge that should go to each grid cell
          cell_rech_volume = sub_recharge / cell_count
          cell_rech_nmass = sub_nmass / cell_count
          cell_rech_pmass = sub_pmass / cell_count
          !third: assign the average cell recharge to each cell
          do k=1,huc12_ncell(n)
            cell_row = huc12_cells(n,k,1)
            cell_col = huc12_cells(n,k,2)
            if(cell_received(cell_row,cell_col).eq.0) then !has not been given recharge from an HRU
              gw_cell_ss_rech(cell_row,cell_col) = gw_cell_ss_rech(cell_row,cell_col) + cell_rech_volume
              huc12_cell_total = huc12_cell_total + cell_rech_volume
              gwflow_rech_sum(cell_row,cell_col) = gwflow_rech_sum(cell_row,cell_col) + cell_rech_volume    
              if(gw_transport_flag.eq.1) then
                gw_cell_ss_rechn(cell_row,cell_col) = gw_cell_ss_rechn(cell_row,cell_col) + cell_rech_nmass
                gwflow_rechn_sum(cell_row,cell_col) = gwflow_rechn_sum(cell_row,cell_col) + cell_rech_nmass
                gw_cell_ss_rechp(cell_row,cell_col) = gw_cell_ss_rechp(cell_row,cell_col) + cell_rech_pmass
                gwflow_rechp_sum(cell_row,cell_col) = gwflow_rechp_sum(cell_row,cell_col) + cell_rech_pmass
              endif            
            endif
          enddo
        enddo      
        open(8822,file='gwflow_rech_comparison')
        write(8822,*) hru_total,hru_cell_total,huc12_cell_total !hr_total should be equal to (hru_cell_total + huc12_cell_total)
        
      else !proceed with usual mapping routine
        ob_num = sp_ob1%hru  !object number of first HRU
        do k=1,num_hru
          rech_volume = (gw_rech(k)/1000.) * (ob(ob_num)%area_ha * 10000.) !m * m2 = m3
          rech_nmass = gw_rechn(k) * ob(ob_num)%area_ha * 1000. !g of no3-n
          rech_pmass = gw_rechp(k) * ob(ob_num)%area_ha * 1000. !g of p
          do i=1,hru_num_cells(k)
            cell_row = hru_cells(k,i,1)
            cell_col = hru_cells(k,i,2)
            cell_rech_volume = rech_volume * hru_cells_fract(k,i)
            gw_cell_ss_rech(cell_row,cell_col) = gw_cell_ss_rech(cell_row,cell_col) + cell_rech_volume
            gwflow_rech_sum(cell_row,cell_col) = gwflow_rech_sum(cell_row,cell_col) + cell_rech_volume
            !gw_avail(cell_row,cell_col) = gw_avail(cell_row,cell_col) + cell_rech_volume !update available groundwater in the cell
            if(gw_transport_flag.eq.1) then
              cell_rech_nmass = rech_nmass * hru_cells_fract(k,i)
              cell_rech_pmass = rech_pmass * hru_cells_fract(k,i)
              gw_cell_ss_rechn(cell_row,cell_col) = gw_cell_ss_rechn(cell_row,cell_col) + cell_rech_nmass
              gwflow_rechn_sum(cell_row,cell_col) = gwflow_rechn_sum(cell_row,cell_col) + cell_rech_nmass
              gw_cell_ss_rechp(cell_row,cell_col) = gw_cell_ss_rechp(cell_row,cell_col) + cell_rech_pmass
              gwflow_rechp_sum(cell_row,cell_col) = gwflow_rechp_sum(cell_row,cell_col) + cell_rech_pmass
            endif
          enddo
          ob_num = ob_num + 1
        enddo
      endif
      
      
      
      !remaining ET from HRUs
      ob_num = sp_ob1%hru  !object number of first HRU
      if(gw_et_flag.eq.1) then
      do k=1,num_hru
        max_gwet = etremain(k) !maximum ET rate from the water table
        do i=1,hru_num_cells(k)
					max_gwet = max_gwet * hru_cells_fract(k,i)
          cell_row = hru_cells(k,i,1)
          cell_col = hru_cells(k,i,2)
          if(gw_cell_et(cell_row,cell_col).gt.0) then
            gwet = gw_cell_et(cell_row,cell_col)
          else
            et_surface = gw_cell_top(cell_row,cell_col) !ground surface
            et_bottom = et_surface - gw_cell_exdp(cell_row,cell_col) !lower elevation bound for ET to occur
            gw_head = gw_cell_head(cell_row,cell_col)
            gwet = 0.
            if(gw_head.le.et_bottom) then
              gwet = 0. !below the extinction depth
            elseif(gw_head.ge.et_surface) then
              gwet = max_gwet
            else
              if(gw_cell_exdp(cell_row,cell_col).ne.0) then
                gwet = max_gwet * (gw_head - et_bottom) / (et_surface - et_bottom) !vary ET linearly
              else
                gwet = 0.
              endif
            endif
          endif
          gwet_volume = (gwet/1000.) * (ob(ob_num)%area_ha * 10000.) !m3 of groundwater
          !check for available groundwater in the cell - can only remove what is there
          if(gw_cell_head(cell_row,cell_col).gt.gw_cell_bot(cell_row,cell_col)) then
            if(gwet_volume.ge.gw_avail(cell_row,cell_col)) then
              gwet_volume =  gw_avail(cell_row,cell_col)
            endif
          else
            gwet_volume = 0.
          endif
          gw_cell_ss_et(cell_row,cell_col) = gw_cell_ss_et(cell_row,cell_col) + (gwet_volume*(-1)) !(negative --> leaving the aquifer)
          gwflow_et_sum(cell_row,cell_col) = gwflow_et_sum(cell_row,cell_col) + (gwet_volume*(-1))
          gw_avail(cell_row,cell_col) = gw_avail(cell_row,cell_col) - gwet_volume 
        enddo
        ob_num = ob_num + 1
      enddo
      endif
      
      

      !gw/sw exchange between aquifer and streams; loop through the river cells
      chan_Q = 0.
      ob_num_aqu = sp_ob1%gwflow  !object number of first river cell
      do k=1,sp_ob%gwflow
        
        !connected SWAT+ channel
        chan_ob_num = ob(ob_num_aqu)%obj_out(1)
        chan_num = ob(chan_ob_num)%num
        chan_gis = ob(chan_ob_num)%gis_id
        chan_depth = sd_ch(chan_num)%chd !depth (m) of water in channel
        chan_width = sd_ch(chan_num)%chw !width (m) of channel

        !characteristics of stream channel in the river cell
        chan_length = gw_riv_len(k) !length (m) of channel
        chan_stage = gw_riv_elev(k) + chan_depth !stage (m) of water in channel
        
        !gw-sw exchange flow area
        flow_area = chan_width * chan_length
        
        !grid row and column of the river cell
        row = gw_riv_row(k)
        col = gw_riv_col(k)

        !only proceed if the cell is active and not a boundary; otherwise, it will be included here but not in the head calculations below
        if(gw_cell_status(row,col).eq.1) then 
        
          !calculate flow exchange rate (m3/day)
          !head difference --> head gradient --> flow rate
          gw_head = gw_cell_head(row,col)
          Q = 0.
          if(gw_head.lt.gw_riv_elev(k)) then
            head_diff = chan_depth
            Q =  gw_riv_K(k) * (head_diff / gw_riv_thick(k)) * flow_area !stream leakage (positive Q: entering aquifer)
          elseif (gw_head.gt.chan_stage) then
            head_diff = gw_head - chan_stage
            Q = gw_riv_K(k) * (head_diff / gw_riv_thick(k)) * flow_area * (-1) !gw discharge (negative Q: leaving aquifer)
          elseif (gw_head.gt.gw_riv_elev(k) .and. gw_head.lt.chan_stage) then
            head_diff = chan_stage - gw_head 
            Q = gw_riv_K(k) * (head_diff / gw_riv_thick(k)) * flow_area !stream leakage (positive Q: entering aquifer)
          endif
          
          !store values in source/sink arrays
          if(Q.lt.0) then
            if((Q*-1).ge.gw_avail(row,col)) then !can only remove what is there
              Q = gw_avail(row,col) * (-1)
            endif
            gw_cell_ss_gwsw(row,col) = gw_cell_ss_gwsw(row,col) + Q
            gw_avail(row,col) = gw_avail(row,col) + Q !update available groundwater in the cell
          else
            gw_cell_ss_swgw(row,col) = gw_cell_ss_swgw(row,col) + Q
          endif
          chan_Q(chan_num) = chan_Q(chan_num) + Q !store in array for writing out total channel exchange rate
          gwflow_gwsw_sum(row,col) = gwflow_gwsw_sum(row,col) + Q
          
          !calculate solute mass (g/day) transported to/from cell
          if(gw_transport_flag.eq.1) then
            chan_cn = 0.
            chan_cp = 0.
            nmass = 0.
            pmass = 0.
            if(Q.lt.0) then !mass leaving the cell
              nmass = Q * gw_cell_cn(row,col) !g
              gw_cell_ss_gwswn(row,col) = nmass
              pmass = Q * gw_cell_cp(row,col) !g
              gw_cell_ss_gwswp(row,col) = pmass
            else !mass entering cell from channel
              if(ob(chan_ob_num)%hd(1)%flo.gt.10) then
                if(ob(chan_ob_num)%hd(1)%no3.gt.0) chan_cn = (ob(chan_ob_num)%hd(1)%no3 * 1000.) / &
                                                              ob(chan_ob_num)%hd(1)%flo !g/m3 in channel
                if(ob(chan_ob_num)%hd(1)%solp.gt.0) chan_cp = (ob(chan_ob_num)%hd(1)%solp * 1000.) /  &
																															 ob(chan_ob_num)%hd(1)%flo !g/m3 in channel
						  endif
              nmass = Q * chan_cn !g
              pmass = Q * chan_cp !g
              gw_cell_ss_swgwn(row,col) = nmass
              gw_cell_ss_swgwp(row,col) = pmass
            endif
            gwflow_gwswn_sum(row,col) = gwflow_gwswn_sum(row,col) + nmass
            gwflow_gwswp_sum(row,col) = gwflow_gwswp_sum(row,col) + pmass
          endif

          !store values in object (this will be added to the channel object in the "command" subroutine)
          Q = Q * (-1) !positive value now is discharge to stream (adding flow to stream), for SWAT+ channel routing
          if(gw_transport_flag.eq.1) then
            nmass = nmass * (-1)
            pmass = pmass * (-1)
          else
            nmass = 0.
            pmass = 0.
          endif
          ob(ob_num_aqu)%hd(1)%flo = 0
          if(ob(ob_num_aqu)%typ == "gwflow") then
            ob(ob_num_aqu)%hd(1)%flo = Q
            ob(ob_num_aqu)%hd(1)%no3 = nmass / 1000. !kg of no3-n
            ob(ob_num_aqu)%hd(1)%solp = pmass / 1000. !kg of p
            if(Q.gt.0) then !hydrograph separation
              ob(ob_num_aqu)%hdsep%flo_gwsw = Q
            else
              ob(ob_num_aqu)%hdsep%flo_swgw = Q
            endif
          endif
          
        endif !cell status
        ob_num_aqu = ob_num_aqu + 1
      enddo !go to next river cell object
      !write(out_gwsw_chan,104) (chan_Q(i),i=1,sp_ob%chandeg)

      
      
      
      !groundwater pumping (specified by user)
      !groundwater pumping (specified pumping, for groundwater that leaves the hydrologic system)
      if(gw_pumpex_flag.eq.1) then
        !loop through pumps and specified pumping periods; apply rate to the associated grid cells
        do i=1,gw_npumpex
          row = gw_pumpex_cell_row(i)
          col = gw_pumpex_cell_col(i)
          if(gw_cell_status(row,col).gt.0) then
          do j=1,gw_pumpex_nperiods(i)
            !determine if the current day of the simulation is within the pumping period; if so, apply the pumping rate to the cell
            pumpex_start_date = gw_pumpex_dates(i,1,j)
            pumpex_end_date = gw_pumpex_dates(i,2,j)
            if(gw_daycount.ge.pumpex_start_date .and. gw_daycount.le.pumpex_end_date) then
              !check to make sure there is enough groundwater to satisfy the pumping rate
              Q = gw_pumpex_rates(i,j)
              if(Q.ge.gw_avail(row,col)) then
                Q = gw_avail(row,col)
                gw_avail(row,col) = gw_avail(row,col) - Q
							endif
              gw_cell_ss_pumpex(row,col) = gw_cell_ss_pumpex(row,col) - Q !negative = leaving the aquifer
              gwflow_pumpex_sum(row,col) = gwflow_pumpex_sum(row,col) - Q 
              !if chemical transport simulated, calculate the mass of N and P removed via pumping
              if(gw_transport_flag.eq.1) then
                nmass = Q * gw_cell_cn(row,col)
                pmass = Q * gw_cell_cp(row,col)
                gw_cell_ss_pumpexn(row,col) = gw_cell_ss_pumpexn(row,col) - nmass 
                gwflow_pumpexn_sum(row,col) = gwflow_pumpexn_sum(row,col) - nmass
                gw_cell_ss_pumpexp(row,col) = gw_cell_ss_pumpexp(row,col) - pmass 
                gwflow_pumpexp_sum(row,col) = gwflow_pumpexp_sum(row,col) - pmass
              endif
            endif
          enddo
          endif
        enddo
      endif
      
      
      
      
      !groundwater pumping (agricultural irrigation)
      !gw_cell_ss_pumpag is populated in the "actions" subroutine, when groundwater irrigation is simulated
      !sum up the volume of groundwater pumped during the current day
      sum10 = 0.
      do i=1,grid_nrow
        do j=1,grid_ncol
          gwflow_pumpag_sum(i,j) = gwflow_pumpag_sum(i,j) + gw_cell_ss_pumpag(i,j)
          sum10 = sum10 + gw_cell_ss_pumpag(i,j)
        enddo
      enddo
      if(gw_transport_flag.eq.1) then
        do i=1,grid_nrow
          do j=1,grid_ncol
            nmass = gw_cell_ss_pumpag(i,j) * gw_cell_cn(i,j) !m3/day * g/m3 = g/day
            pmass = gw_cell_ss_pumpag(i,j) * gw_cell_cp(i,j)
            !add to solute source/sink arrays
            gw_cell_ss_pumpagn(i,j) = nmass
            gwflow_pumpagn_sum(i,j) = gwflow_pumpagn_sum(i,j) + nmass
            gw_cell_ss_pumpagp(i,j) = pmass
            gwflow_pumpagp_sum(i,j) = gwflow_pumpagp_sum(i,j) + pmass
          enddo
        enddo  
      endif
      
      
      
      !groundwater exchange with lakes (lateral exchange only)	
      if(gw_lake_flag.eq.1) then	
        do i=1,grid_nrow	
          do j=1,grid_ncol	
            if(gw_cell_status(i,j).eq.1) then 	
              Q = 0.	
              !current cell has lake water (vertical exchange)	
              if(gw_cell_lake(i,j).eq.1) then	
                !calculate flow exchange rate (m3/day)	
                !head difference --> head gradient --> flow rate	
                if(gw_cell_head(i,j).lt.gw_cell_lake_bed(i,j)) then	
                  head_diff = gw_cell_lake_stage(i,j) - gw_cell_lake_bed(i,j)	
                  Q = lake_K * (head_diff / lake_thick) * (cell_size*cell_size) !lake leakage (positive Q: entering aquifer)	
                endif	
                gw_cell_ss_lake(i,j) = Q	
              !current cell is adjacent to a cell that has lake water (horizontal exchange)  	
              elseif(gw_cell_lake(i,j).eq.0 .and. gw_cell_lake(i+1,j).eq.1) then !lake to the south of the cell	
                head_diff = gw_cell_lake_stage(i+1,j) - gw_cell_head(i,j)	
                Q = lake_K * (head_diff / lake_thick) * (gw_cell_lake_stage(i+1,j) - gw_cell_lake_bed(i+1,j))	
                if(Q.lt.0) then !gw leaving the cell --> check against available groundwater in the cell	
                  if((Q*-1).ge.gw_avail(i,j)) then	
                    Q = gw_avail(i,j) * (-1)	
                    gw_avail(i,j) = gw_avail(i,j) + Q	
									endif	
                endif	
                gw_cell_ss_lake(i,j) = Q	
              elseif(gw_cell_lake(i,j).eq.0 .and. gw_cell_lake(i-1,j).eq.1) then !lake to the north of the cell	
                head_diff = gw_cell_lake_stage(i-1,j) - gw_cell_head(i,j)	
                Q = lake_K * (head_diff / lake_thick) * (gw_cell_lake_stage(i-1,j) - gw_cell_lake_bed(i-1,j))	
                if(Q.lt.0) then !gw leaving the cell --> check against available groundwater in the cell	
                  if((Q*-1).ge.gw_avail(i,j)) then	
                    Q = gw_avail(i,j) * (-1)	
                    gw_avail(i,j) = gw_avail(i,j) + Q	
									endif	
                endif	
                gw_cell_ss_lake(i,j) = Q	
              elseif(gw_cell_lake(i,j).eq.0 .and. gw_cell_lake(i,j+1).eq.1) then !lake to the east of the cell	
                head_diff = gw_cell_lake_stage(i,j+1) - gw_cell_head(i,j)	
                Q = lake_K * (head_diff / lake_thick) * (gw_cell_lake_stage(i,j+1) - gw_cell_lake_bed(i,j+1))	
                if(Q.lt.0) then !gw leaving the cell --> check against available groundwater in the cell	
                  if((Q*-1).ge.gw_avail(i,j)) then	
                    Q = gw_avail(i,j) * (-1)	
                    gw_avail(i,j) = gw_avail(i,j) + Q	
									endif	
                endif	
                gw_cell_ss_lake(i,j) = Q	
              elseif(gw_cell_lake(i,j).eq.0 .and. gw_cell_lake(i,j-1).eq.1) then !lake to the west of the cell	
                head_diff = gw_cell_lake_stage(i,j-1) - gw_cell_head(i,j)	
                Q = lake_K * (head_diff / lake_thick) * (gw_cell_lake_stage(i,j-1) - gw_cell_lake_bed(i,j-1))	
                if(Q.lt.0) then !gw leaving the cell --> check against available groundwater in the cell	
                  if((Q*-1).ge.gw_avail(i,j)) then	
                    Q = gw_avail(i,j) * (-1)	
                    gw_avail(i,j) = gw_avail(i,j) + Q	
									endif	
                endif	
                gw_cell_ss_lake(i,j) = Q	
              else	
                gw_cell_ss_lake(i,j) = 0.	
              endif	
              gwflow_lake_sum(i,j) = gwflow_lake_sum(i,j) + Q  	
            endif	
					enddo	
				enddo	
        if(gw_transport_flag.eq.1) then
          do i=1,grid_nrow
            do j=1,grid_ncol
              Q = gw_cell_ss_lake(i,j)
              if(Q.lt.0) then !mass is leaving the cell --> lake
                nmass = Q * gw_cell_cn(row,col) !g
                pmass = Q * gw_cell_cp(row,col) !g
              else
                nmass = Q * lake_no3 !g
                pmass = Q * lake_p !g
							endif
              gw_cell_ss_laken(row,col) = nmass
              gw_cell_ss_lakep(row,col) = pmass
              gwflow_laken_sum(i,j) = gwflow_laken_sum(i,j) + nmass
              gwflow_lakep_sum(i,j) = gwflow_lakep_sum(i,j) + pmass
            enddo
          enddo  
        endif
			endif	
      	

      
      !groundwater discharge and solute mass to tile drains; loop through the tile cells
      if(gw_tile_flag.eq.1) then
        ob_num_aqu = sp_ob1%gwflow  !object number of first river cell
        do i=1,grid_nrow
          do j=1,grid_ncol
            if(gw_cell_status(i,j).eq.1 .and. gw_cell_tile(i,j).eq.1) then
              
              !get elevation of the subsurface drain
              tile_elev = gw_cell_top(i,j) - gw_tile_depth    
              
              !only proceed if groundwater head is higher than the drain
              if(gw_cell_head(i,j).gt.tile_elev) then 
                
                !calculate flow rate using Darcy's Law
                !head_diff = gw_cell_head(i,j) - tile_elev
                !Q = gw_tile_drain_area * gw_tile_K * (head_diff) !m3/day
                
                !option #2: remove all groundwater above the drain, at the same time (TESTING...)
                Q = (gw_cell_head(i,j)-tile_elev) * cell_size * cell_size * gw_cell_Sy(i,j) !m3/day
                !check for available groundwater in the cell - can only remove what is there
                if(Q.ge.gw_avail(i,j)) then
                  Q = gw_avail(i,j)
                endif
                gw_avail(i,j) = gw_avail(i,j) - Q !update available groundwater in the cell
                
                !add to source/sink arrays
                gw_cell_ss_tile(i,j) = Q * (-1) !leaving aquifer
                gwflow_tile_sum(i,j) = gwflow_tile_sum(i,j) + (Q*(-1)) !leaving aquifer
                
                !add water to river cell object (that will eventually go to the SWAT+ channel)
                riv_num = gw_tilecell_rivcell(i,j)
                riv_ob_num = ob_num_aqu + riv_num - 1
                ob(riv_ob_num)%hd(1)%flo = ob(riv_ob_num)%hd(1)%flo + Q
                ob(riv_ob_num)%hdsep%flo_tile = ob(riv_ob_num)%hdsep%flo_tile + Q !hydrograph separation
                
                !solutes
                if(gw_transport_flag.eq.1) then
                  !calculate mass removed with tile water
                  nmass = Q * gw_cell_cn(i,j)
                  pmass = Q * gw_cell_cp(i,j)
                  !add to solute source/sink arrays
                  gw_cell_ss_tilen(i,j) = nmass * (-1)
                  gwflow_tilen_sum(i,j) = gwflow_tilen_sum(i,j) + (nmass*(-1))
                  gw_cell_ss_tilep(i,j) = pmass * (-1)
                  gwflow_tilep_sum(i,j) = gwflow_tilep_sum(i,j) + (pmass*(-1))
                  !add solute mass to river cell object (that will eventually go to the SWAT+ channel)
                  ob(riv_ob_num)%hd(1)%no3 = ob(riv_ob_num)%hd(1)%no3 + (nmass/1000.) !kg of no3-n
                  ob(riv_ob_num)%hd(1)%solp = ob(riv_ob_num)%hd(1)%solp + (pmass/1000.) !kg of p
                endif
                
              endif
            endif
          enddo
        enddo
        !computer flow rate and nutrient concentration for the tile cell groups, and write out to file
        if(gw_tile_group_flag.eq.1) then
          do i=1,gw_tile_num_group
            sum_tile(i) = 0.
            do j=1,num_tile_cells(i)
              sum_tile(i) = sum_tile(i) + gw_cell_ss_tile(gw_tile_groups(i,j,1),gw_tile_groups(i,j,2)) !m3 
            enddo
            sum_tile(i) = (sum_tile(i)*(-1)) / 86400. !m3 --> m3/sec, change to positive value
            if(gw_transport_flag.eq.1) then
              sum_massn(i) = 0.
              sum_massp(i) = 0.
              do j=1,num_tile_cells(i)
                sum_massn(i) = sum_massn(i) + gw_cell_ss_tilen(gw_tile_groups(i,j,1),gw_tile_groups(i,j,2)) !g
                sum_massp(i) = sum_massp(i) + gw_cell_ss_tilep(gw_tile_groups(i,j,1),gw_tile_groups(i,j,2)) !g
              enddo
              if(sum_tile(i).ne.0) then
                cn_tile(i) = sum_massn(i) / sum_tile(i) !g/m3
                cp_tile(i) = sum_massp(i) / sum_tile(i) !g/m3
						  else
						    cn_tile(i) = 0.
                cp_tile(i) = 0.
              endif
            endif
          enddo
          if(gw_transport_flag.eq.1) then
            write(out_tile_cells,102) time%day,time%yrc,(sum_tile(i),i=1,gw_tile_num_group), &
                                     (cn_tile(i),i=1,gw_tile_num_group),(cp_tile(i),i=1,gw_tile_num_group)
          else
            write(out_tile_cells,102) time%day,time%yrc,(sum_tile(i),i=1,gw_tile_num_group)
          endif
        endif
        !compute HRU-composite tile drain flow volumes (m3) and loads (g)
        !do i=1,grid_nrow
        !  do j=1,grid_ncol
        !    do k=1,cell_num_hrus(i,j)
        !      hru_id = cell_hrus(i,j,k) !HRU connected to the current grid cell
        !      !add up for annual totals
        !      tile_hru_yr(hru_id,1) = tile_hru_yr(hru_id,1) + (gw_cell_ss_tile(i,j) * cell_hrus_fract(i,j,k)) 
        !      tile_hru_yr(hru_id,2) = tile_hru_yr(hru_id,2) + (gw_cell_ss_tilen(i,j) * cell_hrus_fract(i,j,k))
        !      tile_hru_yr(hru_id,3) = tile_hru_yr(hru_id,3) + (gw_cell_ss_tilep(i,j) * cell_hrus_fract(i,j,k))
        !    enddo
        !  enddo
        !enddo 
        
			endif
        
      
      
      !calculate total volume of groundwater sources/sinks for each grid cell
      do i=1,grid_nrow
        do j=1,grid_ncol
          if(gw_cell_status(i,j).eq.1) then  
            gw_cell_ss(i,j) = gw_cell_ss_rech(i,j) + gw_cell_ss_et(i,j) + gw_cell_ss_gwsw(i,j) + gw_cell_ss_swgw(i,j) +  &
                              gw_cell_ss_pumpag(i,j) + gw_cell_ss_pumpex(i,j) + gw_cell_ss_tile(i,j) + gw_cell_ss_lake(i,j)
          endif
        enddo
      enddo     
      !calculate total solute mass of sources/sinks for each grid cell
      if(gw_transport_flag.eq.1) then
        do i=1,grid_nrow
          do j=1,grid_ncol
            if(gw_cell_status(i,j).eq.1) then  
              gw_cell_ssn(i,j) = gw_cell_ss_rechn(i,j) + gw_cell_ss_gwswn(i,j) + gw_cell_ss_swgwn(i,j) + gw_cell_ss_pumpagn(i,j) + &
                                 gw_cell_ss_pumpexn(i,j) + gw_cell_ss_tilen(i,j) + gw_cell_ss_laken(i,j)
              gw_cell_ssp(i,j) = gw_cell_ss_rechp(i,j) + gw_cell_ss_gwswp(i,j) + gw_cell_ss_swgwp(i,j) + gw_cell_ss_pumpagp(i,j) + &
                                 gw_cell_ss_pumpexp(i,j) + gw_cell_ss_tilep(i,j) + gw_cell_ss_lakep(i,j)
            endif
          enddo
        enddo
      endif
      
      
      
      ! 2) calculate new groundwater head for each grid cell ----------------------------------------------------------------------------------------
      
      !compute cell groundwater volume at the beginning of the day
      do i=1,grid_nrow
        do j=1,grid_ncol
          if(gw_cell_status(i,j).gt.0) then
            gw_volume_before_cell(i,j) = ((gw_cell_head(i,j)-gw_cell_bot(i,j)) * (cell_size * cell_size)) * gw_cell_Sy(i,j) !m3
          endif
        enddo
      enddo
      !compute cell solute mass at the beginning of the day
      if(gw_transport_flag.eq.1) then
        do i=1,grid_nrow
          do j=1,grid_ncol
            if(gw_cell_status(i,j).gt.0) then
              gw_nmass_before_cell(i,j) = gw_cell_mn(i,j) !g
              gw_pmass_before_cell(i,j) = gw_cell_mp(i,j) !g
            endif
          enddo
        enddo
      endif
      
      !determine number of flow time steps; determine size of transport time step
      num_ts = int(1./gw_time_step)
      if(gw_transport_flag.eq.1) then
        gw_trans_time_step = gw_time_step / num_ts_transport
      endif
      
      !calculate new head value for each cell ---------------------------------------------------------------------------------------------
      !(if mass transport calculated, also calculate new concentrations)
      head_new = 0.
      head_old = 0.
      Q_lateral = 0.
      if(gw_transport_flag.eq.1) then
        cn_new = 0.
        cp_new = 0.
      endif
      ss_Q = 0.
      gw_cell_Q = 0.
      do n=1,num_ts
        do i=1,grid_nrow
          do j=1,grid_ncol
            if(gw_cell_status(i,j).gt.0) then !only proceed if the cell is active
              if(gw_cell_status(i,j).eq.1) then !interior cell
                
                !flow across west face (flow from west --> east)
                thick_west = 0.
                if(j.eq.1) then
                  Q_west = 0.
                elseif(gw_cell_status(i,j-1).eq.0) then
                  Q_west = 0.
								elseif(gw_cell_status(i,j-1).eq.2 .and. bc_type.eq.2) then
                  Q_west = 0.
                else
                  face_K = cell_size / (((cell_size/2.)/gw_cell_K(i,j-1)) + ((cell_size/2.)/gw_cell_K(i,j))) !K at the interface (harmonic mean)
                  sat_thick1 = gw_cell_head(i,j-1) - gw_cell_bot(i,j-1)
                  sat_thick2 = gw_cell_head(i,j) - gw_cell_bot(i,j)
                  face_sat = (sat_thick1 + sat_thick2) / 2.   !saturated thickness at the interface
                  gradient = (gw_cell_head(i,j-1) - gw_cell_head(i,j)) / cell_size
                  flow_area = face_sat * cell_size
                  Q_west = face_K * gradient * flow_area !Darcy's Law
                  thick_west = face_sat
                  if(Q_west.lt.0) then !check against available groundwater in current cell
                    if((Q_west*-1).ge.gw_avail(i,j)) then
                      Q_west = gw_avail(i,j) * (-1)
                      gw_avail(i,j) = gw_avail(i,j) + Q_west
                    endif
                  else !check against available groundwater in adjacent cell that provides the flow
                    if(Q_west.ge.gw_avail(i,j-1)) then
                      Q_west = gw_avail(i,j-1)
                      gw_avail(i,j-1) = gw_avail(i,j-1) - Q_west
                    endif
                  endif
                endif
                
                !flow across east face
                thick_east = 0.
                if(j.eq.grid_ncol) then
                  Q_east = 0.
                elseif(gw_cell_status(i,j+1).eq.0) then
                  Q_east = 0.
                elseif(gw_cell_status(i,j+1).eq.2 .and. bc_type.eq.2) then
                  Q_east = 0.
                else
                  face_K = cell_size / (((cell_size/2.)/gw_cell_K(i,j+1)) + ((cell_size/2.)/gw_cell_K(i,j))) !K at the interface (harmonic mean)
                  sat_thick1 = gw_cell_head(i,j+1) - gw_cell_bot(i,j+1)
                  sat_thick2 = gw_cell_head(i,j) - gw_cell_bot(i,j)
                  face_sat = (sat_thick1 + sat_thick2) / 2.   !saturated thickness at the interface
                  gradient = (gw_cell_head(i,j+1) - gw_cell_head(i,j)) / cell_size
                  flow_area = face_sat * cell_size
                  Q_east = face_K * gradient * flow_area !Darcy's Law
                  thick_east = face_sat
                  if(Q_east.lt.0) then !check against available groundwater in the cell
                    if((Q_east*-1).ge.gw_avail(i,j)) then
                      Q_east = gw_avail(i,j) * (-1)
                      gw_avail(i,j) = gw_avail(i,j) + Q_east
                    endif
                  else !check against available groundwater in adjacent cell that provides the flow
                    if(Q_east.ge.gw_avail(i,j+1)) then
                      Q_east = gw_avail(i,j+1)
                      gw_avail(i,j-1) = gw_avail(i,j-1) - Q_east
                    endif
                  endif
                endif
                
                !flow across north face
                thick_north = 0.
                if(i.eq.1) then
                  Q_north = 0.
                elseif(gw_cell_status(i-1,j).eq.0) then
                  Q_north = 0.
                elseif(gw_cell_status(i-1,j).eq.2 .and. bc_type.eq.2) then
                  Q_north = 0.
                else
                  face_K = cell_size / (((cell_size/2.)/gw_cell_K(i-1,j)) + ((cell_size/2.)/gw_cell_K(i,j))) !K at the interface (harmonic mean)
                  sat_thick1 = gw_cell_head(i-1,j) - gw_cell_bot(i-1,j)
                  sat_thick2 = gw_cell_head(i,j) - gw_cell_bot(i,j)
                  face_sat = (sat_thick1 + sat_thick2) / 2.   !saturated thickness at the interface
                  gradient = (gw_cell_head(i-1,j) - gw_cell_head(i,j)) / cell_size
                  flow_area = face_sat * cell_size
                  Q_north = face_K * gradient * flow_area !Darcy's Law
                  thick_north = face_sat
                  if(Q_north.lt.0) then !check against available groundwater in the cell
                    if((Q_north*-1).ge.gw_avail(i,j)) then
                      Q_north = gw_avail(i,j) * (-1)
                      gw_avail(i,j) = gw_avail(i,j) + Q_north
                    endif
                  else !check against available groundwater in adjacent cell that provides the flow
                    if(Q_north.ge.gw_avail(i-1,j)) then
                      Q_north = gw_avail(i-1,j)
                      gw_avail(i,j-1) = gw_avail(i,j-1) - Q_north
                    endif
                  endif
                endif

                !flow across south face
                thick_south = 0.
                if(i.eq.grid_nrow) then
                  Q_south = 0.
                elseif(gw_cell_status(i+1,j).eq.0) then
                  Q_south = 0.
                elseif(gw_cell_status(i+1,j).eq.2 .and. bc_type.eq.2) then
                  Q_south = 0.
								else
                  face_K = cell_size / (((cell_size/2.)/gw_cell_K(i+1,j)) + ((cell_size/2.)/gw_cell_K(i,j))) !K at the interface (harmonic mean)
                  sat_thick1 = gw_cell_head(i+1,j) - gw_cell_bot(i+1,j)
                  sat_thick2 = gw_cell_head(i,j) - gw_cell_bot(i,j)
                  face_sat = (sat_thick1 + sat_thick2) / 2.   !saturated thickness at the interface
                  gradient = (gw_cell_head(i+1,j) - gw_cell_head(i,j)) / cell_size
                  flow_area = face_sat * cell_size
                  Q_south = face_K * gradient * flow_area !Darcy's Law
                  thick_south = face_sat
                  if(Q_south.lt.0) then !check against available groundwater in the cell
                    if((Q_south*-1).ge.gw_avail(i,j)) then
                      Q_south = gw_avail(i,j) * (-1)
                      gw_avail(i,j) = gw_avail(i,j) + Q_south
                    endif
                  else !check against available groundwater in adjacent cell that provides the flow
                    if(Q_south.ge.gw_avail(i+1,j)) then
                      Q_south = gw_avail(i+1,j)
                      gw_avail(i,j-1) = gw_avail(i,j-1) - Q_south
                    endif
                  endif
                endif                
                
                !calculate change in head
                head_change = (Q_west + Q_east + Q_north + Q_south + gw_cell_ss(i,j)) * (gw_time_step/(gw_cell_Sy(i,j) * cell_size * cell_size))
                
                !calculate new head value
                head_new(i,j) = gw_cell_head(i,j) + head_change
                if(head_new(i,j).lt.gw_cell_bot(i,j)) then
                  head_new(i,j) = gw_cell_bot(i,j)
                endif
                
                !store inflow/outflow for each cell
                gw_cell_Q(i,j) = gw_cell_Q(i,j) + ((Q_west + Q_east + Q_north + Q_south)*gw_time_step)
                ss_Q = ss_Q + ((Q_west + Q_east + Q_north + Q_south)*gw_time_step)
                gwflow_lateral_sum(i,j) = gwflow_lateral_sum(i,j) + ((Q_west + Q_east + Q_north + Q_south)*gw_time_step)
                
                !store flows for transport calculations
                Q_lateral(i,j,1) = Q_west
                Q_lateral(i,j,2) = Q_east
                Q_lateral(i,j,3) = Q_north
                Q_lateral(i,j,4) = Q_south
                
                !store saturated thickness for transport calculations
                sat_west(i,j)  = thick_west
                sat_east(i,j)  = thick_east
                sat_north(i,j) = thick_north
                sat_south(i,j) = thick_south
                
              elseif(gw_cell_status(i,j).eq.2) then !constant head cell                 
                head_new(i,j) = gw_cell_inithead(i,j)
              endif
			        ! ---------------------
            endif
          enddo !next column
        enddo !next row
        
        !store new head values into regular head array
        head_old = gw_cell_head
        gw_cell_head = head_new
        gw_avail = (gw_cell_head-gw_cell_bot)*(cell_size*cell_size)*gw_cell_Sy !update groundwater volume in each cell
        
        !simulate fate and transport of solutes - calculate new concentrations
        if(gw_transport_flag.eq.1) then
          do t=1,num_ts_transport
            do i=1,grid_nrow
              do j=1,grid_ncol
                if(gw_cell_status(i,j).eq.1) then !interior cell
                  
                  !groundwater volume in the cell (m3)
                  gw_volume_old = cell_size * cell_size * (head_old(i,j)-gw_cell_bot(i,j)) * gw_cell_Sy(i,j)
                  gw_volume_new = cell_size * cell_size * (gw_cell_head(i,j)-gw_cell_bot(i,j)) * gw_cell_Sy(i,j)
                  
                  !calculate groundwater volume for the current transport time step (via interpolation)
								  time_fraction = real(t)/real(num_ts_transport)
                  gw_volume_inter = gw_volume_old + ((gw_volume_new-gw_volume_old)*time_fraction)
                  
                  !advection transport
								  !transport across west face
                  if(Q_lateral(i,j,1).gt.0) then
                    nmass_west = Q_lateral(i,j,1) * gw_cell_cn(i,j-1) !entering cell
                    pmass_west = Q_lateral(i,j,1) * gw_cell_cp(i,j-1)
                  else
                    nmass_west = Q_lateral(i,j,1) * gw_cell_cn(i,j) !leaving cell
                    pmass_west = Q_lateral(i,j,1) * gw_cell_cp(i,j)
                  endif
                  !transport across east face
                  if(Q_lateral(i,j,2).gt.0) then
                    nmass_east = Q_lateral(i,j,2) * gw_cell_cn(i,j+1) !entering cell
                    pmass_east = Q_lateral(i,j,2) * gw_cell_cp(i,j+1)
                  else
                    nmass_east = Q_lateral(i,j,2) * gw_cell_cn(i,j) !leaving cell
                    pmass_east = Q_lateral(i,j,2) * gw_cell_cp(i,j)
                  endif
                  !transport across north face
                  if(Q_lateral(i,j,3).gt.0) then
                    nmass_north = Q_lateral(i,j,3) * gw_cell_cn(i-1,j) !entering cell
                    pmass_north = Q_lateral(i,j,3) * gw_cell_cp(i-1,j)
                  else
                    nmass_north = Q_lateral(i,j,3) * gw_cell_cn(i,j) !leaving cell
                    pmass_north = Q_lateral(i,j,3) * gw_cell_cp(i,j)
                  endif
                  !transport across south face
                  if(Q_lateral(i,j,4).gt.0) then
                    nmass_south = Q_lateral(i,j,4) * gw_cell_cn(i+1,j) !entering cell
                    pmass_south = Q_lateral(i,j,4) * gw_cell_cp(i+1,j)
                  else
                    nmass_south = Q_lateral(i,j,4) * gw_cell_cn(i,j) !leaving cell
                    pmass_south = Q_lateral(i,j,4) * gw_cell_cp(i,j)
                  endif
                  nmass_adv(i,j) = nmass_west + nmass_east + nmass_north + nmass_south !total advectice mass in/out of the cell
                  pmass_adv(i,j) = pmass_west + pmass_east + pmass_north + pmass_south
                  
                  !dispersion transport
                  if(gw_cell_status(i,j).eq.1) then
                    nmass_west =  gw_long_disp * ((gw_cell_cn(i,j-1)-gw_cell_cn(i,j))/cell_size) * sat_west(i,j)
                    nmass_east =  gw_long_disp * ((gw_cell_cn(i,j+1)-gw_cell_cn(i,j))/cell_size) * sat_east(i,j)
                    nmass_north = gw_long_disp * ((gw_cell_cn(i-1,j)-gw_cell_cn(i,j))/cell_size) * sat_north(i,j)
                    nmass_south = gw_long_disp * ((gw_cell_cn(i+1,j)-gw_cell_cn(i,j))/cell_size) * sat_south(i,j)
                    pmass_west =  gw_long_disp * ((gw_cell_cp(i,j-1)-gw_cell_cp(i,j))/cell_size) * sat_west(i,j)
                    pmass_east =  gw_long_disp * ((gw_cell_cp(i,j+1)-gw_cell_cp(i,j))/cell_size) * sat_east(i,j)
                    pmass_north = gw_long_disp * ((gw_cell_cp(i-1,j)-gw_cell_cp(i,j))/cell_size) * sat_north(i,j)
                    pmass_south = gw_long_disp * ((gw_cell_cp(i+1,j)-gw_cell_cp(i,j))/cell_size) * sat_south(i,j)
                  endif
                  nmass_dsp(i,j) = nmass_west + nmass_east + nmass_north + nmass_south !total dispersive mass in/out of the cell
                  pmass_dsp(i,j) = pmass_west + pmass_east + pmass_north + pmass_south
                  
                  !denitrification for no3 (g/day)
                  nmass_rct(i,j) = gw_cell_cn(i,j) * gw_volume_inter * gw_lambda_no3
                  
                  !calculate change in mass (g)
                  mn_change = (nmass_adv(i,j) + nmass_dsp(i,j) + nmass_rct(i,j) + gw_cell_ssn(i,j)) * (gw_trans_time_step / gw_reta_no3)
                  mp_change = (pmass_adv(i,j) + pmass_dsp(i,j) +                  gw_cell_ssp(i,j)) * (gw_trans_time_step / gw_reta_p)
                  
                  !calculate new mass in the cell (g)
                  gw_cell_mn(i,j) = gw_cell_mn(i,j) + mn_change
                  gw_cell_mp(i,j) = gw_cell_mp(i,j) + mp_change
                  if(gw_cell_mn(i,j).lt.0) gw_cell_mn(i,j) = 0.
                  if(gw_cell_mp(i,j).lt.0) gw_cell_mp(i,j) = 0.
                  
                  !calculate new concentration (g/m3)
                  if(gw_volume_inter.gt.0) then
                    cn_new(i,j) = gw_cell_mn(i,j) / gw_volume_inter
                    cp_new(i,j) = gw_cell_mp(i,j) / gw_volume_inter
									else
                    cn_new(i,j) = 0.
                    cp_new(i,j) = 0.
                    gw_cell_mn(i,j) = 0.
                    gw_cell_mp(i,j) = 0.
                  endif
                  
                  !store values for mass budget analysis
                  gw_cell_advn(i,j) = gw_cell_advn(i,j) + (nmass_adv(i,j)*(gw_trans_time_step / gw_reta_no3))
                  gw_cell_dspn(i,j) = gw_cell_dspn(i,j) + (nmass_dsp(i,j)*(gw_trans_time_step / gw_reta_no3))
                  gw_cell_rctn(i,j) = gw_cell_rctn(i,j) + (nmass_rct(i,j)*(gw_trans_time_step / gw_reta_no3))
                  gw_cell_advp(i,j) = gw_cell_advp(i,j) + (pmass_adv(i,j)*(gw_trans_time_step / gw_reta_p))
                  gw_cell_dspp(i,j) = gw_cell_dspp(i,j) + (pmass_dsp(i,j)*(gw_trans_time_step / gw_reta_p))
                  gw_cell_rctp(i,j) = gw_cell_rctp(i,j) + (pmass_rct(i,j)*(gw_trans_time_step / gw_reta_p))
                  
                elseif(gw_cell_status(i,j).eq.2) then !constant concentration cell
                  cn_new(i,j) = gw_cell_initcn(i,j)
                  cp_new(i,j) = gw_cell_initcp(i,j)
                endif
					    enddo
						enddo
            
            gw_cell_cn = cn_new !store new concentration values into regular array
            gw_cell_cp = cp_new
            
          enddo !next transport time step
        endif
        
      enddo !next flow time step ----------------------------------------------------------------------------------------------------------

      
      !calculate groundwater saturation excess flow (routing to nearby streams)
      !(this occurs if the water table rises above the ground surface during the day)
      if(gw_satexcess_flag.eq.1) then
      ob_num_aqu = sp_ob1%gwflow  !object number of first river cell
      count = 0
      do i=1,grid_nrow
        do j=1,grid_ncol
          if(gw_cell_status(i,j).eq.1) then
            if(gw_cell_head(i,j).gt.gw_cell_top(i,j)) then
            
              !calculate volume of saturation excess flow that is routed to streams
              satex_depth = gw_cell_head(i,j) - gw_cell_top(i,j)
              satex_volume = (cell_size * cell_size * satex_depth) * gw_cell_Sy(i,j) !m3 of groundwater
              count = count + 1
              
              !add water to the River Cell object closest to the current cell
              riv_num = gw_cell_rivcell(i,j)
              riv_ob_num = ob_num_aqu + riv_num - 1
              ob(riv_ob_num)%hd(1)%flo = ob(riv_ob_num)%hd(1)%flo + satex_volume
              ob(riv_ob_num)%hdsep%flo_satex = ob(riv_ob_num)%hdsep%flo_satex + satex_volume !hydrograph separation
              
              !sum up water volume for total watershed
              gw_cell_satex(i,j) = satex_volume
              gwflow_satex_sum(i,j) = gwflow_satex_sum(i,j) + satex_volume
              
              !set groundwater head to the ground surface
              gw_cell_head(i,j) = gw_cell_top(i,j)
              
              !solutes
              if(gw_transport_flag.eq.1) then
                !calculate solute mass leaving with saturation excess flow; add to river cell object
                satex_nmass = satex_volume * gw_cell_cn(i,j) !g of no3-n
                satex_pmass = satex_volume * gw_cell_cp(i,j) !g of p
                ob(riv_ob_num)%hd(1)%no3 = ob(riv_ob_num)%hd(1)%no3 + (satex_nmass/1000.) !kg of no3-n
                ob(riv_ob_num)%hd(1)%solp = ob(riv_ob_num)%hd(1)%solp + (satex_pmass/1000.) !kg of p
                !sum up solute mass for total watershed
                gw_cell_satexn(i,j) = satex_nmass
                gwflow_satexn_sum(i,j) = gwflow_satexn_sum(i,j) + satex_nmass
                gw_cell_satexp(i,j) = satex_pmass
                gwflow_satexp_sum(i,j) = gwflow_satexp_sum(i,j) + satex_pmass
                !calculate new solute concentrations
                gw_cell_mn(i,j) = gw_cell_mn(i,j) - satex_nmass
                gw_cell_cn(i,j) = gw_cell_mn(i,j) / ((gw_cell_head(i,j)-gw_cell_bot(i,j))*cell_size*cell_size*gw_cell_Sy(i,j))
                gw_cell_mp(i,j) = gw_cell_mp(i,j) - satex_pmass
                gw_cell_cp(i,j) = gw_cell_mp(i,j) / ((gw_cell_head(i,j)-gw_cell_bot(i,j))*cell_size*cell_size*gw_cell_Sy(i,j))
              endif
              
            endif
          endif
        enddo
      enddo
      endif
      frac_sat = real(count) / real(num_active) !calculate the fraction of active grid cells that are fully saturated
      
      
      !print out new head values and nutrient concentration values, if requested
      if(gw_output_yr(gw_output_index).eq.time%yrc .and. gw_output_day(gw_output_index).eq.time%day) then
        write(out_gwheads,*) 'Groundwater Head for:',time%yrc,time%day
        do i=1,grid_nrow
          write(out_gwheads,100) (gw_cell_head(i,j),j=1,grid_ncol)
        enddo
        write(out_gwheads,*)
        if(gw_transport_flag.eq.1) then
          write(out_gwconc,*) 'NO3-N Concentration for:',time%yrc,time%day
          do i=1,grid_nrow
            write(out_gwconc,100) (gw_cell_cn(i,j),j=1,grid_ncol)
          enddo
          write(out_gwconc,*) 'P Concentration for:',time%yrc,time%day
          do i=1,grid_nrow
            write(out_gwconc,100) (gw_cell_cp(i,j),j=1,grid_ncol)
          enddo
          write(out_gwconc,*)
        endif
        gw_output_index = gw_output_index + 1
      endif
      
      !calculate the average depth to water table
      sum = 0.
      do i=1,grid_nrow
        do j=1,grid_ncol
          if(gw_cell_status(i,j).eq.1) then
            sum = sum + (gw_cell_top(i,j) - gw_cell_head(i,j))
          endif
        enddo
      enddo
      depth_wt_avg = sum / num_active
      
      !print out head values and nutrient concentration values for observation cells (each time step)
      do k=1,gw_num_obs_wells
        gw_obs_head(k) = gw_cell_head(gw_obs_cells_row(k),gw_obs_cells_col(k))
        if(gw_transport_flag.eq.1) then
          gw_obs_cn(k) = gw_cell_cn(gw_obs_cells_row(k),gw_obs_cells_col(k))
          gw_obs_cp(k) = gw_cell_cp(gw_obs_cells_row(k),gw_obs_cells_col(k))
        endif
      enddo
      if(gw_transport_flag.eq.1) then
        write(out_gwobs,102) time%yrc,time%day,(gw_obs_head(k),k=1,gw_num_obs_wells), &
                            (gw_obs_cn(k),k=1,gw_num_obs_wells),(gw_obs_cp(k),k=1,gw_num_obs_wells)
      else
        write(out_gwobs,102) time%yrc,time%day,(gw_obs_head(k),k=1,gw_num_obs_wells)
      endif
      
      !write out channel flow rates (new output file, to more easily view hydrographs)
      do i=1,sp_ob%chandeg
        channel_flow(i) = ch_out_d(i)%flo        
      enddo
      write(out_gw_chan,104) (channel_flow(i),i=1,sp_ob%chandeg)
      
      
			!calculate groundwater that should be transferred to SWAT+ HRU soil profiles (i.e. groundwater is within the soil profile)
      !retrieve soil thickness for each HRU, and store in DHRUs
      if(gw_transfer_flag.eq.1) then
      do k=1,num_hru
        hru_soilz(k) = soil(k)%phys(soil(k)%nly)%d !mm
      enddo
      hru_soilz = hru_soilz / 1000. !mm --> m
      !determine the thickness of the vadose zone (distance from the ground surface to the water table) for each grid cell
      do i=1,grid_nrow
        do j=1,grid_ncol
          if(gw_cell_status(i,j).eq.1) then
            vadose_thick(i,j) = gw_cell_top(i,j) - gw_cell_head(i,j)
          endif
        enddo
      enddo
      !for each cells, test whether the water table is in the soil profile of the HRU
      !if so, then calculate the volume of water to transfer to the soil profile
      gwvol_hru = 0.
      gw_cell_tran = 0.
      hru_gwtran = 0.
      if(gw_transport_flag.eq.1) then
        gwmassn_hru = 0.
        gw_cell_trann = 0.
        hru_ntran = 0.
        gwmassp_hru = 0.
        gw_cell_tranp = 0.
        hru_ptran = 0.
      endif
      do i=1,grid_nrow
        do j=1,grid_ncol
          if(gw_cell_status(i,j).eq.1) then
            tran_volume = 0.
            tran_massn = 0.
            tran_massp = 0.
            !loop through the HRUs that intersect the grid cell
            do k=1,cell_num_hrus(i,j)
              hru_id = cell_hrus(i,j,k) !HRU ID
              hru_area_m2 = cell_hrus_fract(i,j,k) * (cell_size * cell_size) !area (m2) of the HRU within the grid cell
              if(vadose_thick(i,j) .lt. hru_soilz(hru_id)) then              
                dist_above = hru_soilz(hru_id) - vadose_thick(i,j) !m
                vadose_hru(hru_id) = vadose_thick(i,j)
                tran_volume = dist_above * hru_area_m2 * gw_cell_Sy(i,j) !m3 of groundwater to transfer to the soil profile 
                gwvol_hru(hru_id) = gwvol_hru(hru_id) + tran_volume
                gw_cell_tran(i,j) = gw_cell_tran(i,j) + tran_volume
                gwflow_tran_sum(i,j) = gwflow_tran_sum(i,j) + tran_volume
                if(gw_transport_flag.eq.1) then
							    tran_massn = tran_volume * gw_cell_cn(i,j) !g of nitrate to transfer to the soil profile
                  gwmassn_hru(hru_id) = gwmassn_hru(hru_id) + tran_massn
                  gw_cell_trann(i,j) = gw_cell_trann(i,j) + tran_massn
                  gwflow_trann_sum(i,j) = gwflow_trann_sum(i,j) + tran_massn
                  tran_massp = tran_volume * gw_cell_cp(i,j) !g of P to transfer to the soil profile
                  gwmassp_hru(hru_id) = gwmassp_hru(hru_id) + tran_massp
                  gw_cell_tranp(i,j) = gw_cell_tranp(i,j) + tran_massp
                  gwflow_tranp_sum(i,j) = gwflow_tranp_sum(i,j) + tran_massp
                endif
              endif
            enddo !go to next intersected HRU
					endif
				enddo
			enddo
      !calculate new head for each cell
      do i=1,grid_nrow
        do j=1,grid_ncol
          if(gw_cell_status(i,j).eq.1) then
            head_change =  gw_cell_tran(i,j) / (cell_size * cell_size * gw_cell_Sy(i,j)) !m
            gw_cell_head(i,j) = gw_cell_head(i,j) - head_change
          endif
				enddo
			enddo
      !calculate new solute concentrations for each cell
      if(gw_transport_flag.eq.1) then
        do i=1,grid_nrow
          do j=1,grid_ncol
            if(gw_cell_status(i,j).eq.1) then
              gw_cell_mn(i,j) = gw_cell_mn(i,j) - gw_cell_trann(i,j)
              if((gw_cell_head(i,j)-gw_cell_bot(i,j)).gt.0) then
                gw_cell_cn(i,j) = gw_cell_mn(i,j) / ((gw_cell_head(i,j)-gw_cell_bot(i,j))*cell_size*cell_size*gw_cell_Sy(i,j))
							else
                gw_cell_cn(i,j) = 0.
              endif
              gw_cell_mp(i,j) = gw_cell_mp(i,j) - gw_cell_tranp(i,j)
              if((gw_cell_head(i,j)-gw_cell_bot(i,j)).gt.0) then
                gw_cell_cp(i,j) = gw_cell_mp(i,j) / ((gw_cell_head(i,j)-gw_cell_bot(i,j))*cell_size*cell_size*gw_cell_Sy(i,j))
							else
                gw_cell_cp(i,j) = 0.
              endif
            endif
				  enddo
			  enddo
      endif
      !transfer water to HRU soil layers
      ob_num = sp_ob1%hru  !object number of first HRU
      do k=1,num_hru
        hru_area_m2 = ob(ob_num)%area_ha * 10000. !area of the HRU in m2      
        if(gwvol_hru(k).gt.0) then !water is transferred
          !determine which HRU soil layers are to receive groundwater
          water_depth = 0.
          water_depth_tot = 0.
          do j=1,soil(k)%nly
            sol_thick = soil(k)%phys(j)%thick / 1000. !mm --> m
            if((soil(k)%phys(j)%d/1000.).gt.vadose_hru(k)) then
              if(j.eq.1) then !top layer
                water_depth(j) = (soil(k)%phys(j)%d/1000.) - vadose_hru(k)
              else
                if(vadose_hru(k).gt.(soil(k)%phys(j-1)%d/1000.)) then !water table is within this layer
                  water_depth(j) = (soil(k)%phys(j)%d/1000.) - vadose_hru(k)
                else !water table is above this layer
                  water_depth(j) = sol_thick
                endif
              endif
              water_depth_tot = water_depth_tot + water_depth(j)
            endif
          enddo
          !transfer the water and solute mass
          do j=1,soil(k)%nly
            if(water_depth_tot.gt.0) then
              layer_fraction = water_depth(j) / water_depth_tot
            else
              layer_fraction = 0.
            endif
            layer_transfer = gwvol_hru(k) * layer_fraction !m3
            layer_transfer = layer_transfer / hru_area_m2 !m
            layer_transfer = layer_transfer * 1000. !mm
            hru_gwtran(k,j) = hru_gwtran(k,j) + layer_transfer
            if(gw_transport_flag.eq.1) then
              !NO3
              layer_transfer = gwmassn_hru(k) * layer_fraction !g
              layer_transfer = layer_transfer / 1000. !kg
              layer_transfer = layer_transfer / ob(ob_num)%area_ha !kg/ha
              hru_ntran(k,j) = hru_ntran(k,j) + layer_transfer
              !P
              layer_transfer = gwmassp_hru(k) * layer_fraction !g
              layer_transfer = layer_transfer / 1000. !kg
              layer_transfer = layer_transfer / ob(ob_num)%area_ha !kg/ha
              hru_ptran(k,j) = hru_ptran(k,j) + layer_transfer
            endif
          enddo
        endif
        ob_num = ob_num + 1
			enddo
      endif

      
      !compute groundwater volumes at the end of the day
      do i=1,grid_nrow
        do j=1,grid_ncol
          if(gw_cell_status(i,j).gt.0) then
            gw_volume_after_cell(i,j) = ((gw_cell_head(i,j)-gw_cell_bot(i,j)) * (cell_size * cell_size)) * gw_cell_Sy(i,j)  
          endif
        enddo
      enddo
      !compute nutrient mass at the end of the day
      if(gw_transport_flag.eq.1) then
        do i=1,grid_nrow
          do j=1,grid_ncol
            if(gw_cell_status(i,j).gt.0) then 
              gw_nmass_after_cell(i,j) = gw_cell_mn(i,j) !g
              gw_pmass_after_cell(i,j) = gw_cell_mp(i,j) !g
            endif
          enddo
        enddo
      endif
      
      
      !print out source/sink information for a specified cell
      gw_cell_obs_ss_vals(1) = gw_cell_head(gw_cell_obs_ss_row,gw_cell_obs_ss_col) 
      gw_cell_obs_ss_vals(2) = gw_volume_before_cell(gw_cell_obs_ss_row,gw_cell_obs_ss_col)
      gw_cell_obs_ss_vals(3) = gw_volume_after_cell(gw_cell_obs_ss_row,gw_cell_obs_ss_col)
      gw_cell_obs_ss_vals(4) = gw_cell_ss_rech(gw_cell_obs_ss_row,gw_cell_obs_ss_col)
      gw_cell_obs_ss_vals(5) = gw_cell_ss_et(gw_cell_obs_ss_row,gw_cell_obs_ss_col)
      gw_cell_obs_ss_vals(6) = gw_cell_ss_gwsw(gw_cell_obs_ss_row,gw_cell_obs_ss_col)
      gw_cell_obs_ss_vals(7) = gw_cell_ss_swgw(gw_cell_obs_ss_row,gw_cell_obs_ss_col)
      gw_cell_obs_ss_vals(8) = gw_cell_satex(gw_cell_obs_ss_row,gw_cell_obs_ss_col)
      gw_cell_obs_ss_vals(9) = gw_cell_Q(gw_cell_obs_ss_row,gw_cell_obs_ss_col)
      gw_cell_obs_ss_vals(10) = gw_cell_ss_pumpag(gw_cell_obs_ss_row,gw_cell_obs_ss_col)
      gw_cell_obs_ss_vals(11) = gw_cell_ss_pumpex(gw_cell_obs_ss_row,gw_cell_obs_ss_col)
      gw_cell_obs_ss_vals(12) = gw_cell_ss_tile(gw_cell_obs_ss_row,gw_cell_obs_ss_col)
      gw_cell_obs_ss_vals(13) = gw_cell_tran(gw_cell_obs_ss_row,gw_cell_obs_ss_col)
      gw_cell_obs_ss_vals(14) = gw_cell_ss_lake(gw_cell_obs_ss_row,gw_cell_obs_ss_col)
      write(out_gwobs_ss,102)  time%yrc,time%day,(gw_cell_obs_ss_vals(i),i=1,14)     

      !compute change in groundwater storage and mass balance error (as %)
      gw_volume_before = 0.
      gw_volume_after = 0.
      gw_cell_ss_rech_total = 0.
      gw_cell_ss_et_total = 0.
      gw_cell_ss_gwsw_total = 0.
      gw_cell_ss_swgw_total = 0.
      gw_cell_ss_satex_total = 0.
      gw_cell_ss_tran_total = 0.
      gw_cell_Q_total = 0.
      gw_cell_ss_pumpag_total = 0.
      gw_cell_ss_pumpex_total = 0.
      gw_cell_ss_tile_total = 0.
      gw_cell_ss_lake_total = 0.
      do i=1,grid_nrow
        do j=1,grid_ncol
          if(gw_cell_status(i,j).eq.1) then
            gw_volume_before = gw_volume_before +  gw_volume_before_cell(i,j)
            gw_volume_after = gw_volume_after +  gw_volume_after_cell(i,j)
            gw_cell_ss_rech_total = gw_cell_ss_rech_total + gw_cell_ss_rech(i,j)
            gw_cell_ss_et_total = gw_cell_ss_et_total + gw_cell_ss_et(i,j)
            gw_cell_ss_gwsw_total = gw_cell_ss_gwsw_total + gw_cell_ss_gwsw(i,j)
            gw_cell_ss_swgw_total = gw_cell_ss_swgw_total + gw_cell_ss_swgw(i,j)
            gw_cell_ss_satex_total = gw_cell_ss_satex_total + gw_cell_satex(i,j)
            gw_cell_ss_tran_total = gw_cell_ss_tran_total + gw_cell_tran(i,j)
            gw_cell_Q_total = gw_cell_Q_total + gw_cell_Q(i,j)
            gw_cell_ss_pumpag_total = gw_cell_ss_pumpag_total + gw_cell_ss_pumpag(i,j)
            gw_cell_ss_pumpex_total = gw_cell_ss_pumpex_total + gw_cell_ss_pumpex(i,j)
            gw_cell_ss_tile_total = gw_cell_ss_tile_total + gw_cell_ss_tile(i,j)
            gw_cell_ss_lake_total = gw_cell_ss_lake_total + gw_cell_ss_lake(i,j)
          endif
        enddo
      enddo
      mass_error = (1-((gw_volume_before + gw_cell_ss_rech_total + gw_cell_ss_et_total + gw_cell_ss_gwsw_total + &
                        gw_cell_ss_swgw_total - gw_cell_ss_satex_total - gw_cell_ss_tran_total + gw_cell_Q_total + &
                        gw_cell_ss_pumpag_total + gw_cell_ss_pumpex_total + gw_cell_ss_tile_total + gw_cell_ss_lake_total) &
                        /gw_volume_after)) * 100 
           
      !print out daily information (time step, water balance) in mm (normalized to watershed area)
      gw_volume_before = (gw_volume_before / watershed_area) * 1000. !m3 --> mm of water
      gw_volume_after = (gw_volume_after / watershed_area) * 1000.
      ss_rech = (gw_cell_ss_rech_total / watershed_area) * 1000.
      ss_et = (gw_cell_ss_et_total / watershed_area) * 1000.
      ss_gw = (gw_cell_ss_gwsw_total / watershed_area) * 1000.
      ss_sw = (gw_cell_ss_swgw_total / watershed_area) * 1000.
      ss_satex = (gw_cell_ss_satex_total / watershed_area) * 1000. * (-1) !leaving the aquifer
      ss_tran = (gw_cell_ss_tran_total / watershed_area) * 1000. * (-1) !aquifer --> soil profile
      ss_Q = (gw_cell_Q_total / watershed_area) * 1000.
      ss_pumpag = (gw_cell_ss_pumpag_total / watershed_area) * 1000.
      ss_pumpex = (gw_cell_ss_pumpex_total / watershed_area) * 1000.
      ss_tile = (gw_cell_ss_tile_total / watershed_area) * 1000.
      ss_lake = (gw_cell_ss_lake_total / watershed_area) * 1000.
      if(gwflag_day.eq.1) then
        write(out_gwbal,102) time%yrc,time%day,gw_time_step,gw_volume_before,gw_volume_after,ss_rech,ss_et,ss_gw,ss_sw, &
                             ss_satex,ss_tran,ss_Q,ss_pumpag,ss_pumpex,ss_tile,ss_lake,mass_error,frac_sat,depth_wt_avg
      endif
      
      !add daily water balance volumes to yearly and total values
      vol_change_yr = vol_change_yr + (gw_volume_after-gw_volume_before)
      ss_rech_yr = ss_rech_yr + ss_rech
      ss_et_yr = ss_et_yr + ss_et
      ss_gw_yr = ss_gw_yr + ss_gw
      ss_sw_yr = ss_sw_yr + ss_sw
      ss_satex_yr = ss_satex_yr + ss_satex
      ss_tran_yr = ss_tran_yr + ss_tran
      ss_Q_yr = ss_Q_yr + ss_Q
      ss_pumpag_yr = ss_pumpag_yr + ss_pumpag
      ss_pumpex_yr = ss_pumpex_yr + ss_pumpex
      ss_tile_yr = ss_tile_yr + ss_tile
      ss_lake_yr = ss_lake_yr + ss_lake
      vol_change_total = vol_change_total + (gw_volume_after-gw_volume_before)
      ss_rech_total = ss_rech_total + ss_rech
      ss_et_total = ss_et_total + ss_et
      ss_gw_total = ss_gw_total + ss_gw
      ss_sw_total = ss_sw_total + ss_sw
      ss_satex_total = ss_satex_total + ss_satex
      ss_tran_total = ss_tran_total + ss_tran
      ss_Q_total = ss_Q_total + ss_Q
      ss_pumpag_total = ss_pumpag_total + ss_pumpag
      ss_pumpex_total = ss_pumpex_total + ss_pumpex
      ss_tile_total = ss_tile_total + ss_tile
      ss_lake_total = ss_lake_total + ss_lake
      
      if(gw_transport_flag.eq.1) then
      !no3: compute change in mass storage and mass balance error (as %)
      gw_nmass_before = 0.
      gw_nmass_after = 0.
      gw_cell_ss_rechn_total = 0.
      gw_cell_ss_gwswn_total = 0.
      gw_cell_ss_swgwn_total = 0.
      gw_cell_ss_satexn_total = 0.
      gw_cell_advn_total = 0.
      gw_cell_dspn_total = 0.
      gw_cell_rctn_total = 0.
      gw_cell_ss_pumpagn_total = 0.
      gw_cell_ss_pumpexn_total = 0.
      gw_cell_ss_tilen_total = 0.
      gw_cell_ss_trann_total = 0.
      gw_cell_ss_laken_total = 0.
      do i=1,grid_nrow
        do j=1,grid_ncol
          if(gw_cell_status(i,j).eq.1) then
						gw_nmass_before = gw_nmass_before +  gw_nmass_before_cell(i,j)
            gw_nmass_after = gw_nmass_after +  gw_nmass_after_cell(i,j)
            gw_cell_ss_rechn_total = gw_cell_ss_rechn_total + gw_cell_ss_rechn(i,j)
            gw_cell_ss_gwswn_total = gw_cell_ss_gwswn_total + gw_cell_ss_gwswn(i,j)
            gw_cell_ss_swgwn_total = gw_cell_ss_swgwn_total + gw_cell_ss_swgwn(i,j)
            gw_cell_ss_satexn_total = gw_cell_ss_satexn_total + gw_cell_satexn(i,j)
            gw_cell_advn_total = gw_cell_advn_total + gw_cell_advn(i,j)
            gw_cell_dspn_total = gw_cell_dspn_total + gw_cell_dspn(i,j)
            gw_cell_rctn_total = gw_cell_rctn_total + gw_cell_rctn(i,j)
            gw_cell_ss_pumpagn_total = gw_cell_ss_pumpagn_total + gw_cell_ss_pumpagn(i,j)
            gw_cell_ss_pumpexn_total = gw_cell_ss_pumpexn_total + gw_cell_ss_pumpexn(i,j)
            gw_cell_ss_tilen_total = gw_cell_ss_tilen_total + gw_cell_ss_tilen(i,j)
            gw_cell_ss_trann_total = gw_cell_ss_trann_total + gw_cell_trann(i,j)
            gw_cell_ss_laken_total = gw_cell_ss_laken_total + gw_cell_ss_laken(i,j)
          endif
        enddo
      enddo
      gw_cell_ss_swgwn_total = gw_cell_ss_swgwn_total * (-1) !leaving the aquifer
      gw_cell_ss_satexn_total = gw_cell_ss_satexn_total * (-1) !leaving the aquifer
      gw_cell_ss_trann_total = gw_cell_ss_trann_total * (-1) !leaving the aquifer
      if(gw_nmass_after.gt.0) then
        mass_error = (1-((gw_nmass_before + gw_cell_ss_rechn_total + gw_cell_ss_gwswn_total + gw_cell_ss_swgwn_total + &
                          gw_cell_ss_satexn_total + gw_cell_advn_total + gw_cell_dspn_total + gw_cell_rctn_total + &
                          gw_cell_ss_pumpagn_total + gw_cell_ss_pumpexn_total + gw_cell_ss_tilen_total + &
                          gw_cell_ss_trann_total + gw_cell_ss_laken_total) &
                          /gw_nmass_after)) * 100 
      endif

      !no3: print out daily information (time step, mass balance) in kg
      gw_nmass_before = gw_nmass_before / 1000. !g --> kg
      gw_nmass_after = gw_nmass_after / 1000. 
      ss_rechn = gw_cell_ss_rechn_total / 1000.  
      ss_gwn = gw_cell_ss_gwswn_total / 1000. 
      ss_swn = gw_cell_ss_swgwn_total / 1000. 
      ss_satexn = gw_cell_ss_satexn_total / 1000.
      ss_advn = gw_cell_advn_total / 1000.
      ss_dspn = gw_cell_dspn_total / 1000.
      ss_rctn = gw_cell_rctn_total / 1000.
      ss_pumpagn = gw_cell_ss_pumpagn_total / 1000.
      ss_pumpexn = gw_cell_ss_pumpexn_total / 1000.
      ss_tilen = gw_cell_ss_tilen_total / 1000.
      ss_trann = gw_cell_ss_trann_total / 1000.
      ss_laken = gw_cell_ss_laken_total / 1000.
      if(gwflag_day.eq.1) then
        write(out_gwbaln,107) time%yrc, time%day, gw_time_step, gw_nmass_before, gw_nmass_after,  &
            ss_rechn, ss_gwn, ss_swn, ss_satexn, ss_trann, ss_advn, ss_dspn, ss_rctn, ss_pumpagn, &
            ss_pumpexn, ss_tilen, ss_laken, mass_error
      endif
 
      !no3: add daily mass values to yearly and total values
      nmass_change_yr = nmass_change_yr + (gw_nmass_after-gw_nmass_before)
      ss_rechn_yr = ss_rechn_yr + ss_rechn
      ss_gwn_yr = ss_gwn_yr + ss_gwn
      ss_swn_yr = ss_swn_yr + ss_swn
      ss_satexn_yr = ss_satexn_yr + ss_satexn
      ss_advn_yr = ss_advn_yr + ss_advn
      ss_dspn_yr = ss_dspn_yr + ss_dspn
      ss_rctn_yr = ss_rctn_yr + ss_rctn
      ss_pumpagn_yr = ss_pumpagn_yr + ss_pumpagn
      ss_pumpexn_yr = ss_pumpexn_yr + ss_pumpexn
      ss_tilen_yr = ss_tilen_yr + ss_tilen
      ss_trann_yr = ss_trann_yr + ss_trann
      ss_laken_yr = ss_laken_yr + ss_laken
      nmasschange_total = nmasschange_total + (gw_nmass_after-gw_nmass_before)
      ss_rechn_total = ss_rechn_total + ss_rechn
      ss_gwn_total = ss_gwn_total + ss_gwn
      ss_swn_total = ss_swn_total + ss_swn
      ss_satexn_total = ss_satexn_total + ss_satexn
      ss_advn_total = ss_advn_total + ss_advn
      ss_dspn_total = ss_dspn_total + ss_dspn
      ss_rctn_total = ss_rctn_total + ss_rctn
      ss_pumpagn_total = ss_pumpagn_total + ss_pumpagn
      ss_pumpexn_total = ss_pumpexn_total + ss_pumpexn
      ss_tilen_total = ss_tilen_total + ss_tilen
      ss_trann_total = ss_trann_total + ss_trann
      ss_laken_total = ss_laken_total + ss_laken
      
      !p: compute change in mass storage and mass balance error (as %)
      gw_pmass_before = 0.
      gw_pmass_after = 0.
      gw_cell_ss_rechp_total = 0.
      gw_cell_ss_gwswp_total = 0.
      gw_cell_ss_swgwp_total = 0.
      gw_cell_ss_satexp_total = 0.
      gw_cell_advp_total = 0.
      gw_cell_dspp_total = 0.
      gw_cell_rctp_total = 0.
      gw_cell_ss_pumpagp_total = 0.
      gw_cell_ss_pumpexp_total = 0.
      gw_cell_ss_tilep_total = 0.
      gw_cell_ss_tranp_total = 0.
      gw_cell_ss_lakep_total = 0.
      do i=1,grid_nrow
        do j=1,grid_ncol
          if(gw_cell_status(i,j).eq.1) then
            gw_pmass_before = gw_pmass_before +  gw_pmass_before_cell(i,j)
            gw_pmass_after = gw_pmass_after +  gw_pmass_after_cell(i,j)
            gw_cell_ss_rechp_total = gw_cell_ss_rechp_total + (gw_cell_ss_rechp(i,j)/gw_reta_p)
            gw_cell_ss_gwswp_total = gw_cell_ss_gwswp_total + (gw_cell_ss_gwswp(i,j)/gw_reta_p)
            gw_cell_ss_swgwp_total = gw_cell_ss_swgwp_total + (gw_cell_ss_swgwp(i,j)/gw_reta_p)
            gw_cell_ss_satexp_total = gw_cell_ss_satexp_total + gw_cell_satexp(i,j)
            gw_cell_advp_total = gw_cell_advp_total + (gw_cell_advp(i,j)/gw_reta_p)
            gw_cell_dspp_total = gw_cell_dspp_total + (gw_cell_dspp(i,j)/gw_reta_p)
            gw_cell_rctp_total = gw_cell_rctp_total + gw_cell_rctp(i,j)
            gw_cell_ss_pumpagp_total = gw_cell_ss_pumpagp_total + (gw_cell_ss_pumpagp(i,j)/gw_reta_p)
            gw_cell_ss_pumpexp_total = gw_cell_ss_pumpexp_total + (gw_cell_ss_pumpexp(i,j)/gw_reta_p)
            gw_cell_ss_tilep_total = gw_cell_ss_tilep_total + (gw_cell_ss_tilep(i,j)/gw_reta_p)
            gw_cell_ss_tranp_total = gw_cell_ss_tranp_total + (gw_cell_tranp(i,j)/gw_reta_p)
            gw_cell_ss_lakep_total = gw_cell_ss_lakep_total + (gw_cell_ss_lakep(i,j)/gw_reta_p)
          endif
        enddo
      enddo
      gw_cell_ss_swgwp_total = gw_cell_ss_swgwp_total * (-1) !leaving the aquifer
      gw_cell_ss_satexp_total = gw_cell_ss_satexp_total * (-1) !leaving the aquifer
      gw_cell_ss_tranp_total = gw_cell_ss_tranp_total * (-1) !leaving the aquifer
      if(gw_pmass_after.gt.0) then
        mass_error = (1-((gw_pmass_before + gw_cell_ss_rechp_total + gw_cell_ss_gwswp_total + gw_cell_ss_swgwp_total + &
													gw_cell_ss_satexp_total + gw_cell_advp_total + gw_cell_dspp_total + gw_cell_rctp_total + &
                          gw_cell_ss_pumpagp_total + gw_cell_ss_pumpexp_total + gw_cell_ss_tilep_total + &
                          gw_cell_ss_tranp_total + gw_cell_ss_lakep_total) &
                          /gw_pmass_after)) * 100 
      endif

      !p: print out daily information (time step, mass balance) in kg
      gw_pmass_before = gw_pmass_before / 1000. !g --> kg
      gw_pmass_after = gw_pmass_after / 1000. 
      ss_rechp = gw_cell_ss_rechp_total / 1000.  
      ss_gwp = gw_cell_ss_gwswp_total / 1000. 
      ss_swp = gw_cell_ss_swgwp_total / 1000. 
      ss_satexp = gw_cell_ss_satexp_total / 1000.
      ss_advp = gw_cell_advp_total / 1000.
      ss_dspp = gw_cell_dspp_total / 1000.
      ss_rctp = gw_cell_rctp_total / 1000.
      ss_pumpagp = gw_cell_ss_pumpagp_total / 1000.
      ss_pumpexp = gw_cell_ss_pumpexp_total / 1000.
      ss_tilep = gw_cell_ss_tilep_total / 1000.
      ss_tranp = gw_cell_ss_tranp_total / 1000.
      ss_lakep = gw_cell_ss_lakep_total / 1000.
      if(gwflag_day.eq.1) then
        write(out_gwbalp,107) time%yrc,time%day,gw_time_step,gw_pmass_before,gw_pmass_after,ss_rechp,ss_gwp,ss_swp, &
                              ss_satexp,ss_tranp,ss_advp,ss_dspp,ss_rctp,ss_pumpagp,ss_pumpexp,ss_tilep,ss_lakep,mass_error
      endif
 
      !p: add daily mass values to yearly and total values
      pmass_change_yr = pmass_change_yr + (gw_pmass_after-gw_pmass_before)
      ss_rechp_yr = ss_rechp_yr + ss_rechp
      ss_gwp_yr = ss_gwp_yr + ss_gwp
      ss_swp_yr = ss_swp_yr + ss_swp
      ss_satexp_yr = ss_satexp_yr + ss_satexp
      ss_advp_yr = ss_advp_yr + ss_advp
      ss_dspp_yr = ss_dspp_yr + ss_dspp
      ss_rctp_yr = ss_rctp_yr + ss_rctp
      ss_pumpagp_yr = ss_pumpagp_yr + ss_pumpagp
      ss_pumpexp_yr = ss_pumpexp_yr + ss_pumpexp
      ss_tilep_yr = ss_tilep_yr + ss_tilep
      ss_tranp_yr = ss_tranp_yr + ss_tranp
      ss_lakep_yr = ss_lakep_yr + ss_lakep
      pmasschange_total = pmasschange_total + (gw_pmass_after-gw_pmass_before)
      ss_rechp_total = ss_rechp_total + ss_rechp
      ss_gwp_total = ss_gwp_total + ss_gwp
      ss_swp_total = ss_swp_total + ss_swp
      ss_satexp_total = ss_satexp_total + ss_satexp
      ss_advp_total = ss_advp_total + ss_advp
      ss_dspp_total = ss_dspp_total + ss_dspp
      ss_rctp_total = ss_rctp_total + ss_rctp
      ss_pumpagp_total = ss_pumpagp_total + ss_pumpagp
      ss_pumpexp_total = ss_pumpexp_total + ss_pumpexp
      ss_tilep_total = ss_tilep_total + ss_tilep
      ss_tranp_total = ss_tranp_total + ss_tranp
      ss_lakep_total = ss_lakep_total + ss_lakep
      endif

      !if end of year: write out annual recharge, groundwater ET, gwsw rates, and saturation excess flow values and corresponding nutrient mass flux
      if(time%day == 365) then
        !recharge
        gwflow_rech_sum = gwflow_rech_sum / 365.
        write(out_gw_rech,*) 'Recharge for year (m3/day):',time%yrc
        do i=1,grid_nrow
          write(out_gw_rech,101) (gwflow_rech_sum(i,j),j=1,grid_ncol)
        enddo
        gwflow_rech_sum = 0.
        write(out_gw_rech,*)
        if(gw_transport_flag.eq.1) then
          gwflow_rechn_sum = (gwflow_rechn_sum/1000.) / 365. !g --> kg
          write(out_gw_rech,*) 'Recharge NO3-N for year (kg/day):',time%yrc
          do i=1,grid_nrow
            write(out_gw_rech,101) (gwflow_rechn_sum(i,j),j=1,grid_ncol)
          enddo
          gwflow_rechn_sum = 0.
          write(out_gw_rech,*)
          gwflow_rechp_sum = (gwflow_rechp_sum/1000.) / 365. !g --> kg
          write(out_gw_rech,*) 'Recharge P for year (kg/day):',time%yrc
          do i=1,grid_nrow
            write(out_gw_rech,101) (gwflow_rechp_sum(i,j),j=1,grid_ncol)
          enddo
          gwflow_rechp_sum = 0.
          write(out_gw_rech,*)
        endif
        !groundwater ET
        if(gw_et_flag.eq.1) then
        gwflow_et_sum = gwflow_et_sum / 365.
        write(out_gw_et,*) 'Groundwater ET for year:',time%yrc
        do i=1,grid_nrow
          write(out_gw_et,101) (gwflow_et_sum(i,j),j=1,grid_ncol)
        enddo  
        gwflow_et_sum = 0
        write(out_gw_et,*)
        endif
        !gw-sw exchange rates
        gwflow_gwsw_sum = gwflow_gwsw_sum / 365.
        write(out_gwsw,*) 'GW-SW Exchange Rates for year:',time%yrc
        do i=1,grid_nrow
          write(out_gwsw,101) (gwflow_gwsw_sum(i,j),j=1,grid_ncol)
        enddo  
        gwflow_gwsw_sum = 0
        write(out_gwsw,*)
        if(gw_transport_flag.eq.1) then
          gwflow_gwswn_sum = (gwflow_gwswn_sum/1000.) / 365. !g --> kg
          write(out_gwsw,*) 'GW-SW NO3-N mass for year (kg/day):',time%yrc
          do i=1,grid_nrow
            write(out_gwsw,101) (gwflow_gwswn_sum(i,j),j=1,grid_ncol)
          enddo  
          gwflow_gwswn_sum = 0
          write(out_gwsw,*)
          gwflow_gwswp_sum = (gwflow_gwswp_sum/1000.) / 365. !g --> kg
          write(out_gwsw,*) 'GW-SW P mass for year (kg/day):',time%yrc
          do i=1,grid_nrow
            write(out_gwsw,101) (gwflow_gwswp_sum(i,j),j=1,grid_ncol)
          enddo  
          gwflow_gwswp_sum = 0
          write(out_gwsw,*)
        endif
        !saturation excess flow
        if(gw_satexcess_flag.eq.1) then
        gwflow_satex_sum = gwflow_satex_sum / 365.
        write(out_gw_satex,*) 'Saturation Excess Volumes for:',time%yrc
        do i=1,grid_nrow
          write(out_gw_satex,101) (gwflow_satex_sum(i,j),j=1,grid_ncol)
        enddo  
        gwflow_satex_sum = 0
        write(out_gw_satex,*)
        if(gw_transport_flag.eq.1) then
          gwflow_satexn_sum = (gwflow_satexn_sum/1000.) / 365. !g --> kg
          write(out_gw_satex,*) 'NO3-N mass transport for (kg/day):',time%yrc
          do i=1,grid_nrow
            write(out_gw_satex,101) (gwflow_satexn_sum(i,j),j=1,grid_ncol)
          enddo  
          gwflow_satexn_sum = 0
          write(out_gw_satex,*)
          gwflow_satexp_sum = (gwflow_satexp_sum/1000.) / 365. !g --> kg
          write(out_gw_satex,*) 'P mass transport for (kg/day):',time%yrc
          do i=1,grid_nrow
            write(out_gw_satex,101) (gwflow_satexp_sum(i,j),j=1,grid_ncol)
          enddo  
          gwflow_satexp_sum = 0
          write(out_gw_satex,*)
        endif
        endif
        !groundwater --> soil transfer
        if(gw_transfer_flag.eq.1) then
        gwflow_tran_sum = gwflow_tran_sum / 365.
        write(out_gw_tran,*) 'Groundwater --> Soil Transfer for:',time%yrc
        do i=1,grid_nrow
          write(out_gw_tran,101) (gwflow_tran_sum(i,j),j=1,grid_ncol)
        enddo  
        gwflow_tran_sum = 0
        write(out_gw_tran,*)
        if(gw_transport_flag.eq.1) then
          gwflow_trann_sum = (gwflow_trann_sum/1000.) / 365. !g --> kg
          write(out_gw_tran,*) 'NO3-N mass transport for (kg/day):',time%yrc
          do i=1,grid_nrow
            write(out_gw_tran,101) (gwflow_trann_sum(i,j),j=1,grid_ncol)
          enddo  
          gwflow_trann_sum = 0
          write(out_gw_tran,*)
          gwflow_tranp_sum = (gwflow_tranp_sum/1000.) / 365. !g --> kg
          write(out_gw_tran,*) 'P mass transport for (kg/day):',time%yrc
          do i=1,grid_nrow
            write(out_gw_tran,101) (gwflow_tranp_sum(i,j),j=1,grid_ncol)
          enddo  
          gwflow_tranp_sum = 0
          write(out_gw_tran,*)
        endif
        endif
        !lateral flow
        gwflow_lateral_sum = gwflow_lateral_sum / 365.
        write(out_lateral,*) 'Lateral flow for year:',time%yrc
        do i=1,grid_nrow
          write(out_lateral,101) (gwflow_lateral_sum(i,j),j=1,grid_ncol)
        enddo
        gwflow_lateral_sum = 0.
        write(out_lateral,*)
        !tile drain flow
        if(gw_tile_flag.eq.1) then
        gwflow_tile_sum = gwflow_tile_sum / 365.
        write(out_gw_tile,*) 'Tile Drain Outflow Volumes for:',time%yrc
        do i=1,grid_nrow
          write(out_gw_tile,101) (gwflow_tile_sum(i,j),j=1,grid_ncol)
        enddo  
        gwflow_tile_sum = 0
        write(out_gw_tile,*)
        if(gw_transport_flag.eq.1) then
          gwflow_tilen_sum = (gwflow_tilen_sum/1000.) / 365. !g --> kg
          write(out_gw_tile,*) 'Tile NO3-N mass for year (kg/day):',time%yrc
          do i=1,grid_nrow
            write(out_gw_tile,101) (gwflow_tilen_sum(i,j),j=1,grid_ncol)
          enddo  
          gwflow_tilen_sum = 0
          write(out_gw_tile,*)
          gwflow_tilep_sum = (gwflow_tilep_sum/1000.) / 365. !g --> kg
          write(out_gw_tile,*) 'Tile P mass for year (kg/day):',time%yrc
          do i=1,grid_nrow
            write(out_gw_tile,101) (gwflow_tilep_sum(i,j),j=1,grid_ncol)
          enddo  
          gwflow_tilep_sum = 0
          write(out_gw_tile,*)
        endif
        endif
        !pumping (irrigation)
        gwflow_pumpag_sum = gwflow_pumpag_sum / 365.
        write(out_gw_pumpag,*) 'Pumping rate for year (m3/day):',time%yrc
        do i=1,grid_nrow
          write(out_gw_pumpag,101) (gwflow_pumpag_sum(i,j),j=1,grid_ncol)
        enddo
        gwflow_pumpag_sum = 0.
        write(out_gw_pumpag,*)
        if(gw_transport_flag.eq.1) then
          gwflow_pumpagn_sum = (gwflow_pumpagn_sum/1000.) / 365. !g --> kg
          write(out_gw_pumpag,*) 'Pumped NO3-N for year (kg/day):',time%yrc
          do i=1,grid_nrow
            write(out_gw_pumpag,101) (gwflow_pumpagn_sum(i,j),j=1,grid_ncol)
          enddo
          gwflow_pumpagn_sum = 0.
          write(out_gw_pumpag,*)
          gwflow_pumpagp_sum = (gwflow_pumpagp_sum/1000.) / 365. !g --> kg
          write(out_gw_pumpag,*) 'Pumped P for year (kg/day):',time%yrc
          do i=1,grid_nrow
            write(out_gw_pumpag,101) (gwflow_pumpagp_sum(i,j),j=1,grid_ncol)
          enddo
          gwflow_pumpagp_sum = 0.
          write(out_gw_pumpag,*)
        endif
        !pumping (irrigation)
        if(gw_pumpex_flag.eq.1) then
        gwflow_pumpex_sum = gwflow_pumpex_sum / 365.
        write(out_gw_pumpex,*) 'Pumping rate for year (m3/day):',time%yrc
        do i=1,grid_nrow
          write(out_gw_pumpex,101) (gwflow_pumpex_sum(i,j),j=1,grid_ncol)
        enddo
        gwflow_pumpex_sum = 0.
        write(out_gw_pumpex,*)
        if(gw_transport_flag.eq.1) then
          gwflow_pumpexn_sum = (gwflow_pumpexn_sum/1000.) / 365. !g --> kg
          write(out_gw_pumpex,*) 'Pumped NO3-N for year (kg/day):',time%yrc
          do i=1,grid_nrow
            write(out_gw_pumpex,101) (gwflow_pumpexn_sum(i,j),j=1,grid_ncol)
          enddo
          gwflow_pumpexn_sum = 0.
          write(out_gw_pumpex,*)
          gwflow_pumpexp_sum = (gwflow_pumpexp_sum/1000.) / 365. !g --> kg
          write(out_gw_pumpex,*) 'Pumped P for year (kg/day):',time%yrc
          do i=1,grid_nrow
            write(out_gw_pumpex,101) (gwflow_pumpexp_sum(i,j),j=1,grid_ncol)
          enddo
          gwflow_pumpexp_sum = 0.
          write(out_gw_pumpex,*)
        endif
        endif
        !groundwater-lake exchange
        if(gw_lake_flag.eq.1) then
        gwflow_lake_sum = gwflow_lake_sum / 365.	
        write(out_gw_lake,*) 'Groundwater-Lake Exchange Volumes for:',time%yrc	
        do i=1,grid_nrow	
          write(out_gw_lake,101) (gwflow_lake_sum(i,j),j=1,grid_ncol)	
        enddo  	
        gwflow_lake_sum = 0	
        write(out_gw_lake,*)
        if(gw_transport_flag.eq.1) then
          gwflow_laken_sum = (gwflow_laken_sum/1000.) / 365. !g --> kg
          write(out_gw_lake,*) 'Lake NO3-N mass for year (kg/day):',time%yrc
          do i=1,grid_nrow
            write(out_gw_lake,101) (gwflow_laken_sum(i,j),j=1,grid_ncol)
          enddo  
          gwflow_laken_sum = 0
          write(out_gw_lake,*)
          gwflow_lakep_sum = (gwflow_lakep_sum/1000.) / 365. !g --> kg
          write(out_gw_lake,*) 'Lake P mass for year (kg/day):',time%yrc
          do i=1,grid_nrow
            write(out_gw_lake,101) (gwflow_lakep_sum(i,j),j=1,grid_ncol)
          enddo  
          gwflow_lakep_sum = 0
          write(out_gw_lake,*)
        endif
        endif
        !yearly water balance
        if(gwflag_yr.eq.1) then
          write(out_gwbal_yr,105) time%yrc,vol_change_yr,ss_rech_yr,ss_et_yr,ss_gw_yr,ss_sw_yr, &
                                  ss_satex_yr,ss_tran_yr,ss_Q_yr,ss_pumpag_yr,ss_pumpex_yr,ss_tile_yr,ss_lake_yr
        endif
        vol_change_yr = 0.
        ss_rech_yr = 0.
        ss_et_yr = 0.
        ss_gw_yr = 0.
        ss_sw_yr = 0.
        ss_satex_yr = 0.
        ss_tran_yr = 0.
        ss_Q_yr = 0.
        ss_pumpag_yr = 0.
        ss_pumpex_yr = 0.
        ss_tile_yr = 0.
        ss_lake_yr = 0.
        if(gw_transport_flag.eq.1) then
          !yearly NO3-N mass balance
          if(gwflag_yr.eq.1) then
            write(out_gwbaln_yr,108) time%yrc,nmass_change_yr,ss_rechn_yr,ss_gwn_yr,ss_swn_yr,ss_satexn_yr, &
																		 ss_trann_yr,ss_advn_yr,ss_dspn_yr,ss_rctn_yr,ss_pumpagn_yr,ss_pumpexn_yr,&
                                     ss_tilen_yr,ss_laken_yr
          endif
          nmass_change_yr = 0.
          ss_rechn_yr = 0.
          ss_gwn_yr = 0.
          ss_swn_yr = 0.
          ss_satexn_yr = 0.
          ss_advn_yr = 0.
          ss_dspn_yr = 0.
          ss_rctn_yr = 0.
          ss_pumpagn_yr = 0.
          ss_pumpexn_yr = 0.
          ss_tilen_yr = 0.
          ss_trann_yr = 0.
          ss_laken_yr = 0.
          !yearly P mass balance
          if(gwflag_yr.eq.1) then
            write(out_gwbalp_yr,108) time%yrc,nmass_change_yr,ss_rechp_yr,ss_gwp_yr,ss_swp_yr,ss_satexp_yr,ss_tranp_yr, &
                                     ss_advp_yr,ss_dspp_yr,ss_rctp_yr,ss_pumpagp_yr,ss_pumpexp_yr,ss_tilep_yr,ss_lakep_yr
          endif
          pmass_change_yr = 0.
          ss_rechp_yr = 0.
          ss_gwp_yr = 0.
          ss_swp_yr = 0.
          ss_satexp_yr = 0.
          ss_advp_yr = 0.
          ss_dspp_yr = 0.
          ss_rctp_yr = 0.
          ss_pumpagp_yr = 0.
          ss_pumpexp_yr = 0.
          ss_tilep_yr = 0.
          ss_tranp_yr = 0.
          ss_lakep_yr = 0.
        endif
        !hru tile drain output
        !write(out_gwtile_hru,*) 'Year, HRU, Flow (m3), NO3 (kg), P (kg)'
        !do k=1,num_hru
        !  tile_hru_yr(k,1) = tile_hru_yr(k,1) * (-1)
        !  tile_hru_yr(k,2) = tile_hru_yr(k,2) * (-1) / 1000. !g --> kg
        !  tile_hru_yr(k,3) = tile_hru_yr(k,3) * (-1) / 1000. !g --> kg
        !  write(out_gwtile_hru,109) time%yrc,k,tile_hru_yr(k,1),tile_hru_yr(k,2),tile_hru_yr(k,3)
        !enddo
        !write(out_gwtile_hru,*)
        !tile_hru_yr = 0. !zero out for following year
      endif
      
      !add to the total source/sinks arrays
      ss_rech_cell_total = ss_rech_cell_total + gw_cell_ss_rech
      ss_et_cell_total = ss_et_cell_total + gw_cell_ss_et
      ss_gwsw_cell_total = ss_gwsw_cell_total + gw_cell_ss_gwsw + gw_cell_ss_swgw
      ss_satex_cell_total = ss_satex_cell_total + gw_cell_satex
      ss_tran_cell_total = ss_tran_cell_total + gw_cell_tran
      ss_Q_cell_total = ss_Q_cell_total + gw_cell_Q
      ss_tile_cell_total = ss_tile_cell_total + gw_cell_ss_tile
      ss_lake_cell_total = ss_lake_cell_total + gw_cell_ss_lake
      ss_pumpag_cell_total = ss_pumpag_cell_total + gw_cell_ss_pumpag
      ss_pumpex_cell_total = ss_pumpex_cell_total + gw_cell_ss_pumpex
      !if last day of the simulation is reached: print out total source/sink values and average annual values
      if(time%yrc == time%yrc_end .and. time%day == time%day_end) then
        write(out_gw_rech,*) 'Total for entire Simulation'
        write(out_gw_et,*) 'Total for entire Simulation'
        write(out_gwsw,*) 'Total for entire Simulation'
        write(out_gw_satex,*) 'Total for entire Simulation'
        write(out_gw_tran,*) 'Total for entire Simulation'
        write(out_lateral,*) 'Total for entire Simulation'
        write(out_gw_tile,*) 'Total for entire Simulation'
        write(out_gw_pumpag,*) 'Total for entire Simulation'
        write(out_gw_pumpex,*) 'Total for entire Simulation'
        write(out_gw_lake,*) 'Total for entire Simulation'
        do i=1,grid_nrow
          write(out_gw_rech,101) (ss_rech_cell_total(i,j),j=1,grid_ncol)
          write(out_gw_et,101) (ss_et_cell_total(i,j),j=1,grid_ncol)
          write(out_gwsw,101) (ss_gwsw_cell_total(i,j),j=1,grid_ncol)
          write(out_gw_satex,101) (ss_satex_cell_total(i,j),j=1,grid_ncol)
          write(out_gw_tran,101) (ss_tran_cell_total(i,j),j=1,grid_ncol)
          write(out_lateral,101) (ss_Q_cell_total(i,j),j=1,grid_ncol)
          write(out_gw_tile,101) (ss_tile_cell_total(i,j),j=1,grid_ncol)
          write(out_gw_pumpag,101) (ss_pumpag_cell_total(i,j),j=1,grid_ncol)
          write(out_gw_pumpex,101) (ss_pumpex_cell_total(i,j),j=1,grid_ncol)
          write(out_gw_lake,101) (ss_lake_cell_total(i,j),j=1,grid_ncol)
        enddo 
        !average annual water balance
        vol_change_total = vol_change_total + (gw_volume_after-gw_volume_before)
        ss_rech_total = ss_rech_total / time%nbyr
        ss_et_total = ss_et_total / time%nbyr
        ss_gw_total = ss_gw_total / time%nbyr
        ss_sw_total = ss_sw_total / time%nbyr
        ss_satex_total = ss_satex_total / time%nbyr
        ss_tran_total = ss_tran_total / time%nbyr
        ss_Q_total = ss_Q_total / time%nbyr
        ss_pumpag_total = ss_pumpag_total / time%nbyr
        ss_pumpex_total = ss_pumpex_total / time%nbyr
        ss_tile_total = ss_tile_total / time%nbyr
        ss_lake_total = ss_lake_total / time%nbyr
        if(gwflag_aa.eq.1) then
          write(out_gwbal_aa,105) time%yrc,vol_change_total,ss_rech_total,ss_et_total,ss_gw_total,ss_sw_total, &
                                  ss_satex_total,ss_tran_total,ss_Q_total,ss_pumpag_total,ss_pumpex_total, &
                                  ss_tile_total,ss_lake_total
        endif
      endif
      
      if(gw_transport_flag.eq.1) then
      !no3: add to the total source/sink mass arrays - print out if last day of the simulation is reached
      ss_rechn_cell_total = ss_rechn_cell_total + gw_cell_ss_rechn
      ss_gwswn_cell_total = ss_gwswn_cell_total + gw_cell_ss_gwswn + gw_cell_ss_swgwn
      ss_satexn_cell_total = ss_satexn_cell_total + gw_cell_satexn
      ss_tilen_cell_total = ss_tilen_cell_total + gw_cell_ss_tilen
      ss_trann_cell_total = ss_trann_cell_total + gw_cell_trann
      ss_laken_cell_total = ss_laken_cell_total + gw_cell_ss_laken
      ss_pumpagn_cell_total = ss_pumpagn_cell_total + gw_cell_ss_pumpagn
      ss_pumpexn_cell_total = ss_pumpexn_cell_total + gw_cell_ss_pumpexn
      if(time%yrc == time%yrc_end .and. time%day == time%day_end) then
        write(out_gw_rech,*) 'Total NO3-N (kg) for entire Simulation'
        write(out_gwsw,*) 'Total NO3-N (kg) for entire Simulation'
        write(out_gw_satex,*) 'Total NO3-N (kg) for entire Simulation'
        write(out_gw_tile,*) 'Total NO3-N (kg) for entire Simulation'
        write(out_gw_pumpag,*) 'Total NO3-N (kg) for entire Simulation'
        write(out_gw_pumpex,*) 'Total NO3-N (kg) for entire Simulation'
        do i=1,grid_nrow
          write(out_gw_rech,101) (ss_rechn_cell_total(i,j),j=1,grid_ncol)
          write(out_gwsw,101) (ss_gwswn_cell_total(i,j),j=1,grid_ncol)
          write(out_gw_satex,101) (ss_satexn_cell_total(i,j),j=1,grid_ncol)
          write(out_gw_tile,101) (ss_tilen_cell_total(i,j),j=1,grid_ncol)
          write(out_gw_pumpag,101) (ss_pumpagn_cell_total(i,j),j=1,grid_ncol)
          write(out_gw_pumpex,101) (ss_pumpexn_cell_total(i,j),j=1,grid_ncol)
          write(out_gw_lake,101) (ss_laken_cell_total(i,j),j=1,grid_ncol)
        enddo 
        !average annual mass balance
        nmasschange_total = nmasschange_total + (gw_nmass_after-gw_nmass_before)
        ss_rechn_total = ss_rechn_total / time%nbyr
        ss_gwn_total = ss_gwn_total / time%nbyr
        ss_swn_total = ss_swn_total / time%nbyr
        ss_satexn_total = ss_satexn_total / time%nbyr
        ss_advn_total = ss_advn_total / time%nbyr
        ss_dspn_total = ss_dspn_total / time%nbyr
        ss_rctn_total = ss_rctn_total / time%nbyr
        ss_pumpagn_total = ss_pumpagn_total / time%nbyr
        ss_pumpexn_total = ss_pumpexn_total / time%nbyr
        ss_tilen_total = ss_tilen_total / time%nbyr
        ss_trann_total = ss_trann_total / time%nbyr
        ss_laken_total = ss_laken_total / time%nbyr
        if(gwflag_aa.eq.1) then
          write(out_gwbaln_aa,105) time%yrc,nmasschange_total,ss_rechn_total,ss_gwn_total,ss_swn_total,ss_satexn_total, &
                                   ss_trann_total,ss_advn_total,ss_dspn_total,ss_rctn_total,ss_pumpagn_total, &
                                   ss_pumpexn_total,ss_tilen_total,ss_laken_total
        endif
      endif
      
      !p: add to the total source/sink mass arrays - print out if last day of the simulation is reached
      ss_rechp_cell_total = ss_rechp_cell_total + gw_cell_ss_rechp
      ss_gwswp_cell_total = ss_gwswp_cell_total + gw_cell_ss_gwswp + gw_cell_ss_swgwp
      ss_satexp_cell_total = ss_satexp_cell_total + gw_cell_satexp
      ss_tilep_cell_total = ss_tilep_cell_total + gw_cell_ss_tilep
      ss_tranp_cell_total = ss_tranp_cell_total + gw_cell_tranp
      ss_pumpagp_cell_total = ss_pumpagp_cell_total + gw_cell_ss_pumpagp
      ss_pumpexp_cell_total = ss_pumpexp_cell_total + gw_cell_ss_pumpexp
      ss_lakep_cell_total = ss_lakep_cell_total + gw_cell_ss_lakep
      if(time%yrc == time%yrc_end .and. time%day == time%day_end) then
        write(out_gw_rech,*) 'Total P (kg) for entire Simulation'
        write(out_gwsw,*) 'Total P (kg) for entire Simulation'
        write(out_gw_satex,*) 'Total P (kg) for entire Simulation'
        write(out_gw_tile,*) 'Total P (kg) for entire Simulation'
        write(out_gw_pumpag,*) 'Total P (kg) for entire Simulation'
        write(out_gw_pumpex,*) 'Total P (kg) for entire Simulation'
        do i=1,grid_nrow
          write(out_gw_rech,101) (ss_rechp_cell_total(i,j),j=1,grid_ncol)
          write(out_gwsw,101) (ss_gwswp_cell_total(i,j),j=1,grid_ncol)
          write(out_gw_satex,101) (ss_satexp_cell_total(i,j),j=1,grid_ncol)
          write(out_gw_tile,101) (ss_tilep_cell_total(i,j),j=1,grid_ncol)
          write(out_gw_pumpag,101) (ss_pumpagp_cell_total(i,j),j=1,grid_ncol)
          write(out_gw_pumpex,101) (ss_pumpexp_cell_total(i,j),j=1,grid_ncol)
          write(out_gw_lake,101) (ss_lakep_cell_total(i,j),j=1,grid_ncol)
        enddo 
        !average annual mass balance
        pmasschange_total = pmasschange_total + (gw_pmass_after-gw_pmass_before)
        ss_rechp_total = ss_rechp_total / time%nbyr
        ss_gwp_total = ss_gwp_total / time%nbyr
        ss_swp_total = ss_swp_total / time%nbyr
        ss_satexp_total = ss_satexp_total / time%nbyr
        ss_advp_total = ss_advp_total / time%nbyr
        ss_dspp_total = ss_dspp_total / time%nbyr
        ss_rctp_total = ss_rctp_total / time%nbyr
        ss_pumpagp_total = ss_pumpagp_total / time%nbyr
        ss_pumpexp_total = ss_pumpexp_total / time%nbyr
        ss_tilep_total = ss_tilep_total / time%nbyr
        ss_lakep_total = ss_lakep_total / time%nbyr
        ss_tranp_total = ss_tranp_total / time%nbyr
        if(gwflag_aa.eq.1) then
          write(out_gwbalp_aa,105) time%yrc,pmasschange_total,ss_rechp_total,ss_gwp_total,ss_swp_total,ss_satexp_total, &
                                   ss_tranp_total,ss_advp_total,ss_dspp_total,ss_rctp_total,ss_pumpagp_total, &
                                   ss_pumpexp_total,ss_tilep_total,ss_lakep_total
        endif
      endif
      endif
      
      !zero out arrays for next day (flow)
      gw_cell_ss_rech = 0.
      gw_cell_ss_et = 0.
      gw_cell_ss_gwsw = 0.
      gw_cell_ss_swgw = 0.
      gw_cell_satex = 0.
      gw_cell_tran = 0.
      gw_cell_ss_pumpag = 0.
      gw_cell_ss_pumpex = 0.
      gw_cell_ss_tile = 0.
      gw_cell_ss_lake = 0.
      gw_cell_ss = 0.
      gw_cell_Q = 0.
      if(gw_transport_flag.eq.1) then
        !zero out arrays for next day (NO3-N)
        gw_cell_ss_rechn = 0.
        gw_cell_ss_gwswn = 0.
        gw_cell_ss_swgwn = 0.
        gw_cell_satexn = 0.
        gw_cell_trann = 0.
        nmass_adv = 0.
        nmass_dsp = 0.
        nmass_rct = 0.
        gw_cell_advn = 0.
        gw_cell_dspn = 0.
        gw_cell_rctn = 0.
        gw_cell_ss_pumpagn = 0.
        gw_cell_ss_pumpexn = 0.
        gw_cell_ss_tilen = 0.
        gw_cell_ss_laken = 0.
        gw_cell_ssn = 0.
        !zero out arrays for next day (P)
        gw_cell_ss_rechp = 0.
        gw_cell_ss_gwswp = 0.
        gw_cell_ss_swgwp = 0.
        gw_cell_satexp = 0.
        gw_cell_tranp = 0.
        pmass_adv = 0.
        pmass_dsp = 0.
        pmass_rct = 0.
        gw_cell_advp = 0.
        gw_cell_dspp = 0.
        gw_cell_rctp = 0.
        gw_cell_ss_pumpagp = 0.
        gw_cell_ss_pumpexp = 0.
        gw_cell_ss_tilep = 0.
        gw_cell_ss_lakep = 0.
        gw_cell_ssp = 0.
      endif
        
      
100   format(1000(f12.3))
101   format(1000(e12.3))
102   format(i8,i8,1000(e13.4))
103   format(i8,i8,i8,i8,i8,i8,i8,50(f15.3))
104   format(10000(f12.2))
105   format(i8,50(e13.4))
106   format(i8,i8,i8,50(f12.3))
107   format(i8,i8,f10.2,2x,e12.6,2x,e12.6,50(f12.2))
108   format(i8,2x,50(e12.4))
109   format(i8,i8,1000(e12.3))

      return
      end subroutine gwflow_simulate