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
      use water_body_module, only: res_wat_d
      use maximum_data_module, only : db_mx
      use calibration_data_module, only : lsu_out
      
      implicit none
      
      !counters and general information
      integer  i                         !           |counter
      integer  j                         !           |counter
      integer  k                         !           |counter
      integer  n                         !           |counter
      integer  s                         !           |solute counter
      integer  dum                       !           |dummy variable
      integer  num_ts                    !           |number of flow time steps during the daily time step
      integer :: cell_id                 !           |id of gwflow cell
      real :: sum                        !           |general summation
      !lateral flow calculations
      real :: area1                   !m2         |spatial area of first connected cell
      real :: area2                   !m2         |spatial area of second connected cell
      real :: area                    !m2         |smaller of the two areas
      real :: conn_length             !m          |length of connection between two adjacent cells
      real :: dist_x                  !m          |x distance between centroids of adjacent cells
      real :: dist_y                  !m          |y distance between centroids of adjacent cells
      real :: grad_distance           !m          |distance between centroids of two connected cells
      real :: Q_cell                  !m3         |groundater flow between two cells
      real :: face_K                  !m/day      |hydraulic conductivity at cell interface
      real :: sat_thick1              !m          |saturated thickness of connected cell
      real :: sat_thick2              !m          |saturated thickness of cell
      real :: face_sat                !m          |saturated thickness at cell interface
      !storage calculations
      real :: stor_change             !m3         |daily change in groundwater storage for a cell
      real :: sat_change              !m          |daily change in saturated thickness for a cell
      real :: flow_area               !m2         |groundwater flow area between cells
      real :: Q                       !m3         |total flow in/out of cell
      real :: gradient                !m/m        |groundwater gradient between cells
      !groundwater conditions
      real :: frac_sat                !           |fraction of cells that are saturated (i.e., water table at ground surface)
      real :: depth_wt_avg            !m          |average water table depth from all grid cells
      !water balance analysis       
      real :: mass_error              !           |mass error in groundwater balance and solute mass balance
      !tile drainage outflow
      real :: sum_tile(50)            !m3         |summation of flow from tile cell groups
      real :: sum_mass(50,100)        !g          |total solute mass in tile cell groups
      real :: c_tile(50,100)          !g/m3       |average concentration of solute mass in tile cell groups
      !grid totals of fluxes for day (m3, mm)
      real*8   vbef_grid,vaft_grid,rech_grid,gwet_grid,gwsw_grid,swgw_grid,satx_grid,soil_grid,latl_grid, &
               bndr_grid,ppag_grid,ppdf_grid,ppex_grid,tile_grid,resv_grid,wetl_grid,canl_grid,fpln_grid        			 
      !groundwater solutes
      integer  t                      !           |counter for solute transport time steps
      real :: gw_trans_time_step      !days       |length of solute transport time steps
      real :: time_fraction           !           |fraction of flow time step     
      real :: gw_volume_old           !m3         |cell groundwater volume from previous flow time step
      real :: gw_volume_new           !m3         |cell groundwater volume from current flow time step
      real :: gw_volume_inter         !m3         |interpolated cell groundwater volume for the current transport time step
      real :: mass_adv(100)           !g          |solute mass advected into/out of cell
      real :: mass_dsp(100)           !g          |solute mass dispersed into/out of cell
      real :: m_change(100)           !g          |change in cell's solute mass, for one transport time step
      real :: del_no_sorp             !g          |change in cell's solute mass, without sorption
      real :: mass_sorb(100)          !g          |solute mass sorbed
      !total grid values for solute mass (kg)
      real     sol_grid_mbef,sol_grid_maft,sol_grid_rech,sol_grid_gwsw,sol_grid_swgw,sol_grid_satx, &
               sol_grid_advn,sol_grid_disp,sol_grid_rcti,sol_grid_rcto,sol_grid_minl, &
               sol_grid_sorb,sol_grid_ppag,sol_grid_ppex, &
               sol_grid_tile,sol_grid_soil,sol_grid_resv,sol_grid_wetl,sol_grid_canl,sol_grid_fpln
      !usgs observation wells
      real     head_sum,head_avg,head_mae,error_sum,head_residual,error_sum_well,head_mae_well(1000,2),sat_div_well(1000,2)
      integer  write_yr,usgs_yr,val_count,val_count_well,num_yrs_calb,num_yrs_test
      real     head_mae_calb,head_mae_test,sat_sum,sat_avg,sum_sat,sum_sat_well,sat_div,sat_div_calb,sat_div_test
      integer  num_gw_meas,num_gw_meas_calb,num_gw_meas_test,num_gw_meas_well(500,2)
      !stream observations
      integer  chan_count,month_count,month_count_calb,month_count_test
      real     sum_obs_flow,sum_sim_flow,mean_obs_flow,mean_sim_flow
      real     sum_resi_nse,sum_diff_nse,sum_resi_nse1,sum_diff_nse1,sum_num,sum_den,sum_den1,sum_den2
      real     cc,sum_sim_sd,sum_obs_sd,sim_sd,obs_sd
      integer  num_months_calb,num_months_test
      
      
      
                     
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
      
      

      !2. calculate groundwater sources and sinks (m3) for each cell --------------------------------------------------
      
      !recharge ---------------------------------------------------------------
      call gwflow_rech
      
      !groundwater ET ---------------------------------------------------------
      call gwflow_gwet
      
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
      if (hru_pump_flag == 1) then !pumping output for specified HRUs
        do i=1,num_hru_pump_obs    
          hru_pump_obs(i) = hru_pump(hru_pump_ids(i))
        enddo
        write(out_hru_pump_obs,119) time%yrc,time%day, &
                                   (hru_pump_obs(i),i=1,num_hru_pump_obs)
      endif
      hru_pump = 0.
      
      !groundwater pumping (external use) -------------------------------------
      call gwflow_ppex
      
      !discharge from groundwater to tile drains ------------------------------
      !gwflow_tile called in sd_channel_control
      !retrieve information for tile cell groups
      if (gw_tile_flag == 1) then
        !computer flow rate and solute concentration for the tile cell groups
        if(gw_tile_group_flag == 1) then
          do i=1,gw_tile_num_group
            sum_tile(i) = 0.
            do j=1,num_tile_cells(i)
              sum_tile(i) = sum_tile(i) + gw_ss(gw_tile_groups(i,j))%tile !m3 
            enddo
            sum_tile(i) = (sum_tile(i)*(-1)) / 86400. !m3 --> m3/sec
            if (gw_solute_flag == 1) then
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
          if (gw_solute_flag == 1) then
            write(out_tile_cells,102) time%day,time%yrc, &
                                     (sum_tile(i),i=1,gw_tile_num_group), &
                                     (c_tile(i,1),i=1,gw_tile_num_group), &
                                     (c_tile(i,2),i=1,gw_tile_num_group) !only no3 and p
          else
            write(out_tile_cells,102) time%day,time%yrc, &
                                     (sum_tile(i),i=1,gw_tile_num_group)
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
      call gwflow_canl_out
      

      !3. sum sources/sinks for each grid cell ------------------------------------------------------------------------
      !m3 for water; g for solutes
      do i=1,ncell 
        if(gw_state(i)%stat == 1) then  
          gw_ss(i)%totl = gw_ss(i)%rech + gw_ss(i)%gwet + gw_ss(i)%gwsw + gw_ss(i)%swgw + &
          gw_ss(i)%satx + gw_ss(i)%soil + &
          gw_ss(i)%ppag + gw_ss(i)%ppex + gw_ss(i)%tile + &
          gw_ss(i)%resv + gw_ss(i)%wetl + gw_ss(i)%canl + gw_ss(i)%fpln  
        endif
      enddo     
      if (gw_solute_flag == 1) then
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
                                           gwsol_ss(i)%solute(s)%fpln     !floodplain exchange
            enddo
          endif
        enddo
      endif
      
      
      
      ! 4. calculate new groundwater storage and head for each grid cell ----------------------------------------------
      
      !compute groundwater volume and solute mass at beginning of day
      do i=1,ncell
        if(gw_state(i)%stat == 1) then
          gw_state(i)%vbef = (gw_state(i)%head-gw_state(i)%botm) * gw_state(i)%area * gw_state(i)%spyd !m3
        endif
      enddo
      !compute cell solute mass at the beginning of the day
      if (gw_solute_flag == 1) then
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
      if (gw_solute_flag == 1) then
        gw_trans_time_step = gw_time_step / num_ts_transport
      endif
      
      !prepare arrays
      do i=1,ncell
        gw_state(i)%hnew = 0.
        gw_state(i)%hold = 0.
      enddo
      if (gw_solute_flag == 1) then
        do i=1,ncell
          do s=1,gw_nsolute
            gwsol_state(i)%solute(s)%cnew = 0.
          enddo
        enddo
      endif
      
      !calculate new storage and head for each cell
      do n=1,num_ts
        do i=1,ncell
          !only proceed if the cell is active
          if(gw_state(i)%stat > 0) then
            
            !if the cell is interior (not a boundry cell)
            if(gw_state(i)%stat == 1) then
              
              !loop through the cells connected to the current cell
              Q = 0.
              do k=1,gw_state(i)%ncon
                !id of the connected cell
                cell_id = cell_con(i)%cell_id(k) 
                Q_cell = 0.
                !calculate groundwater flow between the cells, using Darcy's Law
                if(gw_state(cell_id)%stat == 0) then
                  Q_cell = 0.
                 elseif(gw_state(cell_id)%stat == 2 .and. bc_type == 2) then !boundary cell
                  Q_cell = 0.
                 else
                  !length of connection between the two cells
                  area1 = gw_state(cell_id)%area !area of connected cell
                  area2 = gw_state(i)%area !area of current cell
                  area = min(area1,area2) !smaller of the two
                  conn_length = sqrt(area)
                  !K along the interface (harmonic mean)
                  face_K = conn_length / (((conn_length/2.)/gw_state(cell_id)%hydc) + ((conn_length/2.)/gw_state(i)%hydc))
                  !saturated thickness of connected cell
                  if(gw_state(cell_id)%head > gw_state(cell_id)%botm) then
                    sat_thick1 = gw_state(cell_id)%head - gw_state(cell_id)%botm
                  else
                    sat_thick1 = 0.
                  endif
                  !saturated thickness of current cell
                  if(gw_state(i)%head > gw_state(i)%botm) then
                    sat_thick2 = gw_state(i)%head - gw_state(i)%botm
                  else
                    sat_thick2 = 0.
                  endif
                  !saturated thickness at the interface (m)
                  face_sat = (sat_thick1 + sat_thick2) / 2.
                  !groundwater hydraulic gradient (m/m)
                  dist_x = gw_state(i)%xcrd - gw_state(cell_id)%xcrd !m
                  dist_y = gw_state(i)%ycrd - gw_state(cell_id)%ycrd !m
                  grad_distance = sqrt((dist_x)**2 + (dist_y)**2)
                  gradient = (gw_state(cell_id)%head - gw_state(i)%head) / grad_distance
                  !groundwater flow area (m2)
                  flow_area = face_sat * conn_length
                  !groundwater flow rate (m3/day)
                  Q_cell = face_K * gradient * flow_area !Darcy's Law
                  !store for solute transport mass balance calculations
                  cell_con(i)%latl(k) = Q_cell
                  cell_con(i)%sat(k) = face_sat
                endif
                !store for cell water balance
                if(gw_state(cell_id)%stat == 2) then !boundary flow
                  gw_ss(i)%bndr = gw_ss(i)%bndr + (Q_cell*gw_time_step)
                else
                  gw_ss(i)%latl = gw_ss(i)%latl + (Q_cell*gw_time_step)
                  gw_ss_sum(i)%latl = gw_ss_sum(i)%latl + (Q_cell*gw_time_step)
                endif
                !sum total flow to/from current cell
                Q = Q + Q_cell
              enddo !go to next connected cell

              !update storage and head for the cell
              stor_change = (Q + gw_ss(i)%totl) * gw_time_step !change in storage (m3)
              gw_state(i)%stor = gw_state(i)%stor + stor_change !new storage (m3)
              sat_change = stor_change / (gw_state(i)%spyd * gw_state(i)%area) !change in saturated thickness (m3)
              gw_state(i)%hnew = gw_state(i)%head + sat_change !new groundwater head (m)
              
            elseif(gw_state(i)%stat == 2) then !constant head cell                 
              gw_state(i)%hnew = gw_state(i)%init
              gw_state(i)%stor = ((gw_state(i)%hnew - gw_state(i)%botm) * gw_state(i)%area) * gw_state(i)%spyd
            endif

          endif !check if cell is active
        enddo !go to next cell
        
        !store new head values into regular head array
        do i=1,ncell
          gw_state(i)%hold = gw_state(i)%head
          gw_state(i)%head = gw_state(i)%hnew
        enddo
        
        !simulate fate and transport of solutes - calculate new concentrations
        if (gw_solute_flag == 1) then
          do t=1,num_ts_transport
            do i=1,ncell
              if(gw_state(i)%stat == 1) then !interior cell

                !calculate old and new groundwater volume in the cell (m3)
                if(gw_state(i)%hold > gw_state(i)%botm) then
                  gw_volume_old = gw_state(i)%area * (gw_state(i)%hold - gw_state(i)%botm) * gw_state(i)%spyd
                else
                  gw_volume_old = 0.
                endif
                if(gw_state(i)%head > gw_state(i)%botm) then
                  gw_volume_new = gw_state(i)%area * (gw_state(i)%head - gw_state(i)%botm) * gw_state(i)%spyd
                else
                  gw_volume_new = 0.
                endif
                  
                !calculate groundwater volume for the current transport time step (via interpolation)
                time_fraction = real(t)/real(num_ts_transport)
                gw_volume_inter = gw_volume_old + ((gw_volume_new-gw_volume_old)*time_fraction)
                  
                !advection transport
                mass_adv = 0.
                do k=1,gw_state(i)%ncon !loop through the connected cells
                  cell_id = cell_con(i)%cell_id(k) !id of the connected cell
                  Q_cell = cell_con(i)%latl(k) !m3/day flow between cell and connected cell
                  if(Q_cell > 0) then !mass entering cell
                    do s=1,gw_nsolute
                      mass_adv(s) = mass_adv(s) + (Q_cell * gwsol_state(cell_id)%solute(s)%conc) !g
                    enddo
                  else !mass leaving cell
                  do s=1,gw_nsolute
                      mass_adv(s) = mass_adv(s) + (Q_cell * gwsol_state(i)%solute(s)%conc) !g
                    enddo
                 endif
                enddo !go to next connected cell
                
                !dispersion transport
                mass_dsp = 0.
                do k=1,gw_state(i)%ncon !loop through the connected cells
                  cell_id = cell_con(i)%cell_id(k) !id of the connected cell
                  face_sat = cell_con(i)%sat(k) !m saturated thickness at interface between cells
                  area1 = gw_state(cell_id)%area !area of connected cell
                  area2 = gw_state(i)%area !area of current cell
                  area = min(area1,area2) !smaller of the two
                  conn_length = sqrt(area)
                  do s=1,gw_nsolute !loop through the solutes
                    mass_dsp(s) = mass_dsp(s) + (gw_long_disp * ((gwsol_state(cell_id)%solute(s)%conc -   &
                       gwsol_state(i)%solute(s)%conc)/conn_length) * face_sat) !g
                  enddo
                enddo !go to next connected cell
                
                !chemical reactions --> fill in mass_rct and mass_min
                cell_id = i
                mass_rct = 0.
                mass_min = 0.
                call gwflow_chem(cell_id,gw_volume_inter)
                
                !calculate change in mass (g)
                do s=1,gw_nsolute !loop through the solutes
                  m_change(s) = (mass_adv(s) + mass_dsp(s) + mass_rct(s) + mass_min(s) + gwsol_ss(i)%solute(s)%totl) * &
                     (gw_trans_time_step/gwsol_sorb(s))    
                enddo
                  
                !calculate mass removed due to sorption (g)
                do s=1,gw_nsolute !loop through the solutes
                  del_no_sorp = (mass_adv(s) + mass_dsp(s) + mass_rct(s) + mass_min(s) + gwsol_ss(i)%solute(s)%totl) * &
                     gw_trans_time_step
                  mass_sorb(s) = del_no_sorp - m_change(s)     
                enddo
                  
                !calculate new mass in the cell (g)
                do s=1,gw_nsolute !loop through the solutes
                  gwsol_state(i)%solute(s)%mass = gwsol_state(i)%solute(s)%mass + m_change(s)
                  if(gwsol_state(i)%solute(s)%mass < 0) then
                    gwsol_state(i)%solute(s)%mass = 0.
                  endif
                enddo
                  
                !calculate new concentration (g/m3)
                if(gw_volume_inter > 0) then
                  do s=1,gw_nsolute !loop through the solutes
                    gwsol_state(i)%solute(s)%cnew = gwsol_state(i)%solute(s)%mass / gw_volume_inter
                  enddo
                 else
                  do s=1,gw_nsolute !loop through the solutes
                    gwsol_state(i)%solute(s)%cnew = 0.
                    gwsol_state(i)%solute(s)%mass = 0.
                  enddo
                endif
                  
                !store values for mass budget analysis
                do s=1,gw_nsolute !loop through the solutes
                  gwsol_ss(i)%solute(s)%advn = gwsol_ss(i)%solute(s)%advn + (mass_adv(s)*(gw_trans_time_step/gwsol_sorb(s)))
                  gwsol_ss(i)%solute(s)%disp = gwsol_ss(i)%solute(s)%disp + (mass_dsp(s)*(gw_trans_time_step/gwsol_sorb(s)))
                  if(mass_rct(s) > 0) then
                    gwsol_ss(i)%solute(s)%rcti = gwsol_ss(i)%solute(s)%rcti + (mass_rct(s)*(gw_trans_time_step/gwsol_sorb(s))) !produced
                  else
                    gwsol_ss(i)%solute(s)%rcto = gwsol_ss(i)%solute(s)%rcto + (mass_rct(s)*(gw_trans_time_step/gwsol_sorb(s))) !consumed
                  endif
                  gwsol_ss(i)%solute(s)%minl = gwsol_ss(i)%solute(s)%minl + (mass_min(s)*(gw_trans_time_step/gwsol_sorb(s)))
                  gwsol_ss(i)%solute(s)%sorb = gwsol_ss(i)%solute(s)%sorb + mass_sorb(s)
                 if(mass_sorb(3) > 0) then
                  dum = 10
                 endif
                enddo
                  
                !track for annual write-out
                do s=1,gw_nsolute !loop through the solutes
                  if(mass_rct(s) > 0) then
                    gwsol_ss_sum(i)%solute(s)%rcti = gwsol_ss_sum(i)%solute(s)%rcti + &
                       (mass_rct(s)*(gw_trans_time_step/gwsol_sorb(s))) !produced
                  else
                    gwsol_ss_sum(i)%solute(s)%rcto = gwsol_ss_sum(i)%solute(s)%rcto + &
                       (mass_rct(s)*(gw_trans_time_step/gwsol_sorb(s))) !consumed
                  endif
                  gwsol_ss_sum(i)%solute(s)%minl = gwsol_ss_sum(i)%solute(s)%minl + &
                     (mass_min(s)*(gw_trans_time_step/gwsol_sorb(s)))
                  gwsol_ss_sum(i)%solute(s)%sorb = gwsol_ss_sum(i)%solute(s)%sorb + mass_sorb(s)
                enddo

              elseif(gw_state(i)%stat == 2) then !constant concentration cell
                do s=1,gw_nsolute
                  gwsol_state(i)%solute(s)%cnew = 0.
                enddo
              endif

            enddo !go to next cell
            
            !store new concentration values into regular array
            do i=1,ncell
              do s=1,gw_nsolute !loop through the solutes
                gwsol_state(i)%solute(s)%conc = gwsol_state(i)%solute(s)%cnew
              enddo
            enddo

          enddo !go to next transport time step
        endif !check if solute transport is being simulated 
        
      enddo !next flow time step --------------------------------------------------------------------------------------

            
      
      !5. save/write out head and solute concentrations ---------------------------------------------------------------      
      
      !save head values and solute concentration values for monthly and annual averages
      do i=1,ncell
        gw_state(i)%hdmo = gw_state(i)%hdmo + gw_state(i)%head
        gw_state(i)%hdyr = gw_state(i)%hdyr + gw_state(i)%head
      enddo
      if (gw_solute_flag == 1) then
        do i=1,ncell
          do s=1,gw_nsolute !loop through the solutes
            gwsol_state(i)%solute(s)%cnmo = gwsol_state(i)%solute(s)%cnmo + gwsol_state(i)%solute(s)%conc
            gwsol_state(i)%solute(s)%cnyr = gwsol_state(i)%solute(s)%cnyr + gwsol_state(i)%solute(s)%conc
          enddo  
        enddo
      endif
      

      !open(96622,file='gwflow_head_daily')
      !if(grid_type == "structured") then
   !     grid_val = 0.
   !     do i=1,grid_nrow
   !       do j=1,grid_ncol
   !         if(cell_id_usg(i,j) > 0) then
   !           grid_val(i,j) = gw_state(cell_id_usg(i,j))%head
   !         endif
   !       enddo
   !     enddo
   !     do i=1,grid_nrow
   !       write(96622,100) (grid_val(i,j),j=1,grid_ncol)
   !     enddo
   !   endif
       !write(96622,*) 



      !print out new head values and solute concentration values, if requested
      if(gw_output_index.le.gw_num_output) then
      if(gw_output_yr(gw_output_index).eq.time%yrc .and. gw_output_day(gw_output_index).eq.time%day) then
        write(out_gwheads,*) 'Groundwater Head for:',time%yrc,time%day
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_state(cell_id_usg(i,j))%head
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_gwheads,100) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_gwheads,120) (gw_state(i)%head,i=1,ncell)
        endif
        write(out_gwheads,*)
        if (gw_solute_flag == 1) then
          do s=1,gw_nsolute !loop through the solutes
            write(out_gwconc,*) gwsol_nm(s),'concentration for:',time%yrc,time%day
            if(grid_type == "structured") then
              grid_val = 0.
              do i=1,grid_nrow
                do j=1,grid_ncol
                  if(cell_id_usg(i,j) > 0) then
                    grid_val(i,j) = gwsol_state(cell_id_usg(i,j))%solute(s)%conc
                  endif
                enddo
              enddo
              do i=1,grid_nrow
                write(out_gwconc,101) (grid_val(i,j),j=1,grid_ncol)
              enddo
            else
              write(out_gwconc,120) (gwsol_state(i)%solute(s)%conc,i=1,ncell)
            endif
            write(out_gwconc,*)
          enddo      
        endif
        gw_output_index = gw_output_index + 1
      endif
      endif
      
      !calculate the average depth to water table
      sum = 0.
      do i=1,ncell
        if(gw_state(i)%stat == 1) then
          sum = sum + (gw_state(i)%elev - gw_state(i)%head)
        endif
      enddo
      depth_wt_avg = sum / num_active
      
      !print out head values and solute concentration values for observation cells (each time step)
      do k=1,gw_num_obs_wells
        gw_obs_head(k) = gw_state(gw_obs_cells(k))%head
        if (gw_solute_flag == 1) then
          do s=1,gw_nsolute !loop through the solutes
            gw_obs_solute(k,s) = gwsol_state(gw_obs_cells(k))%solute(s)%conc
          enddo 
        endif
        !is usgs wells, store for end-of-year analysis
        if(usgs_obs == 1) then
          gw_obs_head_annual(k,time%day) = gw_state(gw_obs_cells(k))%head
          gw_obs_sat_annual(k,time%day) = gw_state(gw_obs_cells(k))%head - &
                                          gw_state(gw_obs_cells(k))%botm
        endif
      enddo
      write(out_gwobs,119) time%yrc,time%day,(gw_obs_head(k),k=1,gw_num_obs_wells)
      if (gw_solute_flag == 1) then
        write(out_gwobs_sol,119) time%yrc,time%day,(gw_obs_solute(k,1),k=1,gw_num_obs_wells), &
                                                   (gw_obs_solute(k,2),k=1,gw_num_obs_wells)
      !need to continue if there are more solutes...
      endif
      
      !if the end of the month has been reached, then store flow rates for specified channels
      if (stream_obs == 1) then
        if(gw_num_obs_chan.gt.0) then
          if (time%end_mo == 1) then
            do chan_count=1,gw_num_obs_chan 
              sim_flow_vals(chan_count,sim_month) = ch_out_m(obs_channels(chan_count))%flo / time%day_mo
            enddo
            sim_month = sim_month + 1
          endif
        endif
      endif
            
      !compute groundwater volumes at the end of the day
      do i=1,ncell
        if(gw_state(i)%stat == 1) then
          gw_state(i)%vaft = ((gw_state(i)%head - gw_state(i)%botm) * gw_state(i)%area) * gw_state(i)%spyd  
        endif
      enddo
      !compute solute mass at the end of the day
      if (gw_solute_flag == 1) then
        do i=1,ncell
          if(gw_state(i)%stat == 1) then
            do s=1,gw_nsolute !loop through the solutes
              gwsol_state(i)%solute(s)%maft = gwsol_state(i)%solute(s)%mass !g
            enddo
          endif
        enddo
      endif
      
      !print out source/sink information for a specified cell
      !gw_cell_obs_ss_vals(1) = gw_state(gw_cell_obs_ss)%head 
      !gw_cell_obs_ss_vals(2) = gw_state(gw_cell_obs_ss)%vbef
      !gw_cell_obs_ss_vals(3) = gw_state(gw_cell_obs_ss)%vaft
      !gw_cell_obs_ss_vals(4) = gw_ss(gw_cell_obs_ss)%rech
      !gw_cell_obs_ss_vals(5) = gw_ss(gw_cell_obs_ss)%gwet
      !gw_cell_obs_ss_vals(6) = gw_ss(gw_cell_obs_ss)%gwsw
      !gw_cell_obs_ss_vals(7) = gw_ss(gw_cell_obs_ss)%swgw
      !gw_cell_obs_ss_vals(8) = gw_ss(gw_cell_obs_ss)%satx
      !gw_cell_obs_ss_vals(9) = gw_ss(gw_cell_obs_ss)%soil
      !gw_cell_obs_ss_vals(10) = gw_ss(gw_cell_obs_ss)%latl
      !gw_cell_obs_ss_vals(11) = gw_ss(gw_cell_obs_ss)%ppag
      !gw_cell_obs_ss_vals(12) = gw_ss(gw_cell_obs_ss)%ppex
      !gw_cell_obs_ss_vals(13) = gw_ss(gw_cell_obs_ss)%tile
      !gw_cell_obs_ss_vals(14) = gw_ss(gw_cell_obs_ss)%resv
      !gw_cell_obs_ss_vals(15) = gw_ss(gw_cell_obs_ss)%wetl
      !gw_cell_obs_ss_vals(16) = gw_ss(gw_cell_obs_ss)%canl
      !gw_cell_obs_ss_vals(17) = gw_ss(gw_cell_obs_ss)%fpln
      !write(out_gwobs_ss,102)  time%yrc,time%day,(gw_cell_obs_ss_vals(i),i=1,17)     
      
      !sum groundwater budget terms for each HUC12 (if national model mode)
      if (nat_model == 1) then
        do n=1,sp_ob%outlet !loop through the HUC12 catchments
          do k=1,huc12_ncell(n) !loop through the cells within each HUC12 catchment
            cell_id = huc12_cells(n,k)
            if(gw_state(cell_id)%stat.eq.1) then
              gw_huc12_wb(1,n) = gw_huc12_wb(1,n) + gw_ss(cell_id)%rech
              gw_huc12_wb(2,n) = gw_huc12_wb(2,n) + gw_ss(cell_id)%gwet
              gw_huc12_wb(3,n) = gw_huc12_wb(3,n) + gw_ss(cell_id)%gwsw
              gw_huc12_wb(4,n) = gw_huc12_wb(4,n) + gw_ss(cell_id)%swgw
              gw_huc12_wb(5,n) = gw_huc12_wb(5,n) + gw_ss(cell_id)%satx
              gw_huc12_wb(6,n) = gw_huc12_wb(6,n) + gw_ss(cell_id)%soil
              gw_huc12_wb(7,n) = gw_huc12_wb(7,n) + gw_ss(cell_id)%latl
              gw_huc12_wb(8,n) = gw_huc12_wb(8,n) + gw_ss(cell_id)%ppag
              gw_huc12_wb(9,n) = gw_huc12_wb(9,n) + gw_ss(cell_id)%ppex
              gw_huc12_wb(10,n) = gw_huc12_wb(10,n) + gw_ss(cell_id)%tile
              gw_huc12_wb(11,n) = gw_huc12_wb(11,n) + gw_ss(cell_id)%resv  
              gw_huc12_wb(12,n) = gw_huc12_wb(12,n) + gw_ss(cell_id)%wetl
              gw_huc12_wb(13,n) = gw_huc12_wb(13,n) + gw_ss(cell_id)%canl
              gw_huc12_wb(14,n) = gw_huc12_wb(14,n) + gw_ss(cell_id)%fpln
              gw_huc12_wb(15,n) = gw_huc12_wb(15,n) + gw_ss(cell_id)%ppdf
              gw_huc12_wb_mo(1,n) = gw_huc12_wb_mo(1,n) + gw_ss(cell_id)%rech
              gw_huc12_wb_mo(2,n) = gw_huc12_wb_mo(2,n) + gw_ss(cell_id)%gwet
              gw_huc12_wb_mo(3,n) = gw_huc12_wb_mo(3,n) + gw_ss(cell_id)%gwsw
              gw_huc12_wb_mo(4,n) = gw_huc12_wb_mo(4,n) + gw_ss(cell_id)%swgw
              gw_huc12_wb_mo(5,n) = gw_huc12_wb_mo(5,n) + gw_ss(cell_id)%satx
              gw_huc12_wb_mo(6,n) = gw_huc12_wb_mo(6,n) + gw_ss(cell_id)%soil
              gw_huc12_wb_mo(7,n) = gw_huc12_wb_mo(7,n) + gw_ss(cell_id)%latl
              gw_huc12_wb_mo(8,n) = gw_huc12_wb_mo(8,n) + gw_ss(cell_id)%ppag
              gw_huc12_wb_mo(9,n) = gw_huc12_wb_mo(9,n) + gw_ss(cell_id)%ppex
              gw_huc12_wb_mo(10,n) = gw_huc12_wb_mo(10,n) + gw_ss(cell_id)%tile
              gw_huc12_wb_mo(11,n) = gw_huc12_wb_mo(11,n) + gw_ss(cell_id)%resv  
              gw_huc12_wb_mo(12,n) = gw_huc12_wb_mo(12,n) + gw_ss(cell_id)%wetl
              gw_huc12_wb_mo(13,n) = gw_huc12_wb_mo(13,n) + gw_ss(cell_id)%canl
              gw_huc12_wb_mo(14,n) = gw_huc12_wb_mo(14,n) + gw_ss(cell_id)%fpln
              gw_huc12_wb_mo(15,n) = gw_huc12_wb_mo(15,n) + gw_ss(cell_id)%ppdf
            endif
          enddo
        enddo
      endif
      
      
      
      !6. compute and write out daily fluxes and mass balance error

      !calculate values for entire grid (all cells)
      vbef_grid = 0.
      vaft_grid = 0.
      rech_grid = 0.
      gwet_grid = 0.
      gwsw_grid = 0.
      swgw_grid = 0.
      satx_grid = 0.
      soil_grid = 0.
      latl_grid = 0.
      bndr_grid = 0.
      ppag_grid = 0.
      ppdf_grid = 0.
      ppex_grid = 0.
      tile_grid = 0.
      resv_grid = 0.
      wetl_grid = 0.
      canl_grid = 0.
      fpln_grid = 0.
      !open(1356,file='gwflow_daily_cell_balance')
      do i=1,ncell
        if(gw_state(i)%stat == 1) then
          vbef_grid = vbef_grid + gw_state(i)%vbef
          vaft_grid = vaft_grid + gw_state(i)%vaft
          rech_grid = rech_grid + gw_ss(i)%rech
          gwet_grid = gwet_grid + gw_ss(i)%gwet
          gwsw_grid = gwsw_grid + gw_ss(i)%gwsw
          swgw_grid = swgw_grid + gw_ss(i)%swgw
          satx_grid = satx_grid + gw_ss(i)%satx
          soil_grid = soil_grid + gw_ss(i)%soil
          latl_grid = latl_grid + gw_ss(i)%latl
          bndr_grid = bndr_grid + gw_ss(i)%bndr
          ppag_grid = ppag_grid + gw_ss(i)%ppag
          ppdf_grid = ppdf_grid + gw_ss(i)%ppdf
          ppex_grid = ppex_grid + gw_ss(i)%ppex
          tile_grid = tile_grid + gw_ss(i)%tile
          resv_grid = resv_grid + gw_ss(i)%resv
          wetl_grid = wetl_grid + gw_ss(i)%wetl
          canl_grid = canl_grid + gw_ss(i)%canl
          fpln_grid = fpln_grid + gw_ss(i)%fpln
        !write(1356,100) gw_state(i)%vbef,gw_state(i)%vaft,gw_ss(i)%rech,gw_ss(i)%gwet,gw_ss(i)%gwsw, &
        !                                                  gw_ss(i)%swgw,gw_ss(i)%satx,gw_ss(i)%soil, &
        !                                                  gw_ss(i)%latl,gw_ss(i)%bndr,gw_ss(i)%ppag, &
        !                                                  gw_ss(i)%ppdf,gw_ss(i)%ppex,gw_ss(i)%tile, &
        !                                                  gw_ss(i)%resv,gw_ss(i)%wetl,gw_ss(i)%canl, &
        !                                                  gw_ss(i)%fpln
        endif
      enddo
      !write(1356,*)
      mass_error = (1-((vbef_grid + rech_grid + gwet_grid + gwsw_grid + swgw_grid - satx_grid - soil_grid + &
                        latl_grid + bndr_grid + ppag_grid + ppex_grid + tile_grid + resv_grid + wetl_grid + &
                        canl_grid + fpln_grid) &
                       /vaft_grid)) * 100 
               
      !print out daily information
      !first, normalize volumes to the watershed area (m3 --> mm)
      vbef_grid = (vbef_grid / (bsn%area_tot_ha*10000.)) * 1000.
      vaft_grid = (vaft_grid / (bsn%area_tot_ha*10000.)) * 1000.
      rech_grid = (rech_grid / (bsn%area_tot_ha*10000.)) * 1000.
      gwet_grid = (gwet_grid / (bsn%area_tot_ha*10000.)) * 1000.
      gwsw_grid = (gwsw_grid / (bsn%area_tot_ha*10000.)) * 1000.
      swgw_grid = (swgw_grid / (bsn%area_tot_ha*10000.)) * 1000.
      satx_grid = (satx_grid / (bsn%area_tot_ha*10000.)) * 1000.
      soil_grid = (soil_grid / (bsn%area_tot_ha*10000.)) * 1000.
      latl_grid = (latl_grid / (bsn%area_tot_ha*10000.)) * 1000.
      bndr_grid = (bndr_grid / (bsn%area_tot_ha*10000.)) * 1000.
      ppag_grid = (ppag_grid / (bsn%area_tot_ha*10000.)) * 1000.
      ppdf_grid = (ppdf_grid / (bsn%area_tot_ha*10000.)) * 1000.
      ppex_grid = (ppex_grid / (bsn%area_tot_ha*10000.)) * 1000.
      tile_grid = (tile_grid / (bsn%area_tot_ha*10000.)) * 1000.
      resv_grid = (resv_grid / (bsn%area_tot_ha*10000.)) * 1000.
      wetl_grid = (wetl_grid / (bsn%area_tot_ha*10000.)) * 1000.
      canl_grid = (canl_grid / (bsn%area_tot_ha*10000.)) * 1000.
      fpln_grid = (fpln_grid / (bsn%area_tot_ha*10000.)) * 1000.
      frac_sat = real(satx_count) / real(num_active) !calculate the fraction of active grid cells that are fully saturated
      if(gwflag_day.eq.1) then
        write(out_gwbal,102) time%yrc,time%day,gw_time_step,vbef_grid,vaft_grid,rech_grid,gwet_grid,gwsw_grid,swgw_grid, &
                                                            satx_grid,soil_grid,latl_grid,bndr_grid,ppag_grid,ppex_grid, &
                                                            tile_grid,resv_grid,wetl_grid,canl_grid,fpln_grid, &
                                                            mass_error,frac_sat,depth_wt_avg,ppdf_grid
      endif
      
      !add daily water balance volumes to yearly values
      ss_grid_yr%chng = ss_grid_yr%chng + (vaft_grid-vbef_grid)
      ss_grid_yr%rech = ss_grid_yr%rech + rech_grid
      ss_grid_yr%gwet = ss_grid_yr%gwet + gwet_grid
      ss_grid_yr%gwsw = ss_grid_yr%gwsw + gwsw_grid
      ss_grid_yr%swgw = ss_grid_yr%swgw + swgw_grid
      ss_grid_yr%satx = ss_grid_yr%satx + satx_grid
      ss_grid_yr%soil = ss_grid_yr%soil + soil_grid
      ss_grid_yr%latl = ss_grid_yr%latl + latl_grid
      ss_grid_yr%bndr = ss_grid_yr%bndr + bndr_grid
      ss_grid_yr%ppag = ss_grid_yr%ppag + ppag_grid
      ss_grid_yr%ppdf = ss_grid_yr%ppdf + ppdf_grid
      ss_grid_yr%ppex = ss_grid_yr%ppex + ppex_grid
      ss_grid_yr%tile = ss_grid_yr%tile + tile_grid
      ss_grid_yr%resv = ss_grid_yr%resv + resv_grid
      ss_grid_yr%wetl = ss_grid_yr%wetl + wetl_grid
      ss_grid_yr%canl = ss_grid_yr%canl + canl_grid
      ss_grid_yr%fpln = ss_grid_yr%fpln + fpln_grid
      !add daily water balance volumes to total values
      ss_grid_tt%chng = ss_grid_tt%chng + (vaft_grid-vbef_grid)
      ss_grid_tt%rech = ss_grid_tt%rech + rech_grid
      ss_grid_tt%gwet = ss_grid_tt%gwet + gwet_grid
      ss_grid_tt%gwsw = ss_grid_tt%gwsw + gwsw_grid
      ss_grid_tt%swgw = ss_grid_tt%swgw + swgw_grid
      ss_grid_tt%satx = ss_grid_tt%satx + satx_grid
      ss_grid_tt%soil = ss_grid_tt%soil + soil_grid
      ss_grid_tt%latl = ss_grid_tt%latl + latl_grid
      ss_grid_tt%bndr = ss_grid_tt%bndr + bndr_grid
      ss_grid_tt%ppag = ss_grid_tt%ppag + ppag_grid
      ss_grid_tt%ppdf = ss_grid_tt%ppdf + ppdf_grid
      ss_grid_tt%ppex = ss_grid_tt%ppex + ppex_grid
      ss_grid_tt%tile = ss_grid_tt%tile + tile_grid
      ss_grid_tt%resv = ss_grid_tt%resv + resv_grid
      ss_grid_tt%wetl = ss_grid_tt%wetl + wetl_grid
      ss_grid_tt%canl = ss_grid_tt%canl + canl_grid
      ss_grid_tt%fpln = ss_grid_tt%fpln + fpln_grid
      
      if (gw_solute_flag == 1) then !if solutes are simulated
                                
        !loop through the solutes
        do s=1,gw_nsolute
          sol_grid_mbef = 0.
          sol_grid_maft = 0.
          sol_grid_rech = 0.
          sol_grid_gwsw = 0.
          sol_grid_swgw = 0.
          sol_grid_satx = 0.
          sol_grid_soil = 0.
          sol_grid_advn = 0.
          sol_grid_disp = 0.
          sol_grid_rcti = 0.
          sol_grid_rcto = 0.
          sol_grid_minl = 0.
          sol_grid_sorb = 0.
          sol_grid_ppag = 0.
          sol_grid_ppex = 0.
          sol_grid_tile = 0.
          sol_grid_soil = 0.
          sol_grid_resv = 0.
          sol_grid_wetl = 0.
          sol_grid_canl = 0.
          sol_grid_fpln = 0.
          !add up mass for the grid (convert g-->kg)
          do i=1,ncell
            if(gw_state(i)%stat == 1) then
              sol_grid_mbef = sol_grid_mbef + (gwsol_state(i)%solute(s)%mbef / 1000.)
              sol_grid_maft = sol_grid_maft + (gwsol_state(i)%solute(s)%maft / 1000.)
              sol_grid_rech = sol_grid_rech + (gwsol_ss(i)%solute(s)%rech / 1000.)
              sol_grid_gwsw = sol_grid_gwsw + (gwsol_ss(i)%solute(s)%gwsw / 1000.)
              sol_grid_swgw = sol_grid_swgw + (gwsol_ss(i)%solute(s)%swgw / 1000.)
              sol_grid_satx = sol_grid_satx + (gwsol_ss(i)%solute(s)%satx / 1000.)
              sol_grid_advn = sol_grid_advn + (gwsol_ss(i)%solute(s)%advn / 1000.)
              sol_grid_disp = sol_grid_disp + (gwsol_ss(i)%solute(s)%disp / 1000.)
              sol_grid_rcti = sol_grid_rcti + (gwsol_ss(i)%solute(s)%rcti / 1000.)
              sol_grid_rcto = sol_grid_rcto + (gwsol_ss(i)%solute(s)%rcto / 1000.)
              sol_grid_minl = sol_grid_minl + (gwsol_ss(i)%solute(s)%minl / 1000.)
              sol_grid_sorb = sol_grid_sorb + (gwsol_ss(i)%solute(s)%sorb / 1000.)
              sol_grid_ppag = sol_grid_ppag + (gwsol_ss(i)%solute(s)%ppag / 1000.)
              sol_grid_ppex = sol_grid_ppex + (gwsol_ss(i)%solute(s)%ppex / 1000.)
              sol_grid_tile = sol_grid_tile + (gwsol_ss(i)%solute(s)%tile / 1000.)
              sol_grid_soil = sol_grid_soil + (gwsol_ss(i)%solute(s)%soil / 1000.)
              sol_grid_resv = sol_grid_resv + (gwsol_ss(i)%solute(s)%resv / 1000.)
              sol_grid_wetl = sol_grid_wetl + (gwsol_ss(i)%solute(s)%wetl / 1000.)
              sol_grid_canl = sol_grid_canl + (gwsol_ss(i)%solute(s)%canl / 1000.)
              sol_grid_fpln = sol_grid_fpln + (gwsol_ss(i)%solute(s)%fpln / 1000.)
            endif
          enddo
          sol_grid_sorb = sol_grid_sorb * (-1) !leaving groundwater (sorbing to aquifer material)
          !calculate mass error
          if(sol_grid_maft > 0) then
            mass_error = (1- ((sol_grid_mbef + sol_grid_rech + sol_grid_gwsw + sol_grid_swgw + &
                               sol_grid_satx + sol_grid_advn + sol_grid_disp + &
                               sol_grid_rcti + sol_grid_rcto + sol_grid_minl + &
                               sol_grid_ppag + sol_grid_ppex + sol_grid_tile + sol_grid_soil + &
                               sol_grid_resv + sol_grid_wetl + sol_grid_canl + sol_grid_fpln) / sol_grid_maft)) * 100 
          endif
          !print out daily values for the solute
          if(gwflag_day == 1) then
            write(out_solbal_dy+s,102) time%yrc,time%day,gw_time_step, &
                                       sol_grid_mbef,sol_grid_maft,sol_grid_rech,sol_grid_gwsw,sol_grid_swgw, &
                                       sol_grid_satx,sol_grid_soil,sol_grid_advn,sol_grid_disp, &
                                       sol_grid_rcti,sol_grid_rcto,sol_grid_minl, &
                                       sol_grid_sorb,sol_grid_ppag,sol_grid_ppex,sol_grid_tile,sol_grid_resv, &
                                       sol_grid_wetl,sol_grid_canl,sol_grid_fpln, &
                                       mass_error
          endif
          !add grid values to yearly and total mass values
          !yearly (kg)
          sol_grid_chng_yr(s) = sol_grid_chng_yr(s) + (sol_grid_maft-sol_grid_mbef)
          sol_grid_rech_yr(s) = sol_grid_rech_yr(s) + sol_grid_rech
          sol_grid_gwsw_yr(s) = sol_grid_gwsw_yr(s) + sol_grid_gwsw
          sol_grid_swgw_yr(s) = sol_grid_swgw_yr(s) + sol_grid_swgw
          sol_grid_satx_yr(s) = sol_grid_satx_yr(s) + sol_grid_satx
          sol_grid_advn_yr(s) = sol_grid_advn_yr(s) + sol_grid_advn
          sol_grid_disp_yr(s) = sol_grid_disp_yr(s) + sol_grid_disp
          sol_grid_rcti_yr(s) = sol_grid_rcti_yr(s) + sol_grid_rcti
          sol_grid_rcto_yr(s) = sol_grid_rcto_yr(s) + sol_grid_rcto
          sol_grid_minl_yr(s) = sol_grid_minl_yr(s) + sol_grid_minl
          sol_grid_sorb_yr(s) = sol_grid_sorb_yr(s) + sol_grid_sorb
          sol_grid_ppag_yr(s) = sol_grid_ppag_yr(s) + sol_grid_ppag
          sol_grid_ppex_yr(s) = sol_grid_ppex_yr(s) + sol_grid_ppex
          sol_grid_tile_yr(s) = sol_grid_tile_yr(s) + sol_grid_tile
          sol_grid_soil_yr(s) = sol_grid_soil_yr(s) + sol_grid_soil
          sol_grid_resv_yr(s) = sol_grid_resv_yr(s) + sol_grid_resv
          sol_grid_wetl_yr(s) = sol_grid_wetl_yr(s) + sol_grid_wetl
          sol_grid_canl_yr(s) = sol_grid_canl_yr(s) + sol_grid_canl
          sol_grid_fpln_yr(s) = sol_grid_fpln_yr(s) + sol_grid_fpln
          !total (kg)
          sol_grid_chng_tt(s) = sol_grid_chng_tt(s) + (sol_grid_maft-sol_grid_mbef)
          sol_grid_rech_tt(s) = sol_grid_rech_tt(s) + sol_grid_rech
          sol_grid_gwsw_tt(s) = sol_grid_gwsw_tt(s) + sol_grid_gwsw
          sol_grid_swgw_tt(s) = sol_grid_swgw_tt(s) + sol_grid_swgw
          sol_grid_satx_tt(s) = sol_grid_satx_tt(s) + sol_grid_satx
          sol_grid_advn_tt(s) = sol_grid_advn_tt(s) + sol_grid_advn
          sol_grid_disp_tt(s) = sol_grid_disp_tt(s) + sol_grid_disp
          sol_grid_rcti_tt(s) = sol_grid_rcti_tt(s) + sol_grid_rcti
          sol_grid_rcto_tt(s) = sol_grid_rcto_tt(s) + sol_grid_rcto
          sol_grid_minl_tt(s) = sol_grid_minl_tt(s) + sol_grid_minl
          sol_grid_sorb_tt(s) = sol_grid_sorb_tt(s) + sol_grid_sorb
          sol_grid_ppag_tt(s) = sol_grid_ppag_tt(s) + sol_grid_ppag
          sol_grid_ppex_tt(s) = sol_grid_ppex_tt(s) + sol_grid_ppex
          sol_grid_tile_tt(s) = sol_grid_tile_tt(s) + sol_grid_tile
          sol_grid_soil_tt(s) = sol_grid_soil_tt(s) + sol_grid_soil
          sol_grid_resv_tt(s) = sol_grid_resv_tt(s) + sol_grid_resv
          sol_grid_wetl_tt(s) = sol_grid_wetl_tt(s) + sol_grid_wetl
          sol_grid_canl_tt(s) = sol_grid_canl_tt(s) + sol_grid_canl
          sol_grid_fpln_tt(s) = sol_grid_fpln_tt(s) + sol_grid_fpln
        enddo !go to next solute
      endif !check for solute transport
      
      

      !7. end of month: average head, average solute concentration, sum pumped groundwater (m3) for HRUs
      if (time%end_mo == 1) then
      !huc12 values, if national model is in use
        if(nat_model == 1) then
          do n=1,sp_ob%outlet !loop through the huc12 catchments
            write(out_huc12wb_mo,125) time%yrc,time%mo,huc12(n),(gw_huc12_wb_mo(i,n),i=1,15)
          enddo
            gw_huc12_wb_mo = 0.
        endif
        !monthly average groundwater head
        do i=1,ncell
          gw_state(i)%hdmo = gw_state(i)%hdmo / time%day_mo 
        enddo
        write(out_head_mo,*) time%yrc,time%mo
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_state(cell_id_usg(i,j))%hdmo
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_head_mo,100) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_head_mo,121) (gw_state(i)%hdmo,i=1,ncell)  
        endif
        write(out_head_mo,*)
        !zero out for next month
        do i=1,ncell
          gw_state(i)%hdmo = 0.
        enddo
        !monthly average solute concentration        
        if(gw_solute_flag == 1) then
          write(out_conc_mo,*) time%yrc,time%mo
          do s=1,gw_nsolute
            !calculate average concentration
            do i=1,ncell
              gwsol_state(i)%solute(s)%cnmo = gwsol_state(i)%solute(s)%cnmo / time%day_mo  
            enddo
            !write out
            write(out_conc_mo,*) gwsol_nm(s) !solute name
            if(grid_type == "structured") then
              grid_val = 0.
              do i=1,grid_nrow
                do j=1,grid_ncol
                  if(cell_id_usg(i,j) > 0) then
                    grid_val(i,j) = gwsol_state(cell_id_usg(i,j))%solute(s)%cnmo
                  endif
                enddo
              enddo
              do i=1,grid_nrow
                write(out_conc_mo,101) (grid_val(i,j),j=1,grid_ncol)
              enddo
            else
              write(out_conc_mo,121) (gwsol_state(i)%solute(s)%cnmo,i=1,ncell)    
            endif
            !zero out for next month
            do i=1,ncell
              gwsol_state(i)%solute(s)%cnmo = 0.
            enddo
          enddo !next solute
          write(out_conc_mo,*)
        endif
        !pumping (irrigation) (for HRUs)
        do i=1,sp_ob%hru
          hru_pump_mo_all(i,((time%yrs-1)*12)+time%mo) = hru_pump_mo(i) 
        enddo
        hru_pump_mo = 0.
      endif
      
      
      
      !8. end of year: write out annual flux values
      if(time%end_yr == 1) then
          
        !annual average groundwater head
        do i=1,ncell
          gw_state(i)%hdyr = gw_state(i)%hdyr / time%day
        enddo
        write(out_head_yr,*) time%yrc
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_state(cell_id_usg(i,j))%hdyr
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_head_yr,100) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_head_yr,121) (gw_state(i)%hdyr,i=1,ncell)   
        endif
        write(out_head_yr,*)
        !zero out for next year
        do i=1,ncell
          gw_state(i)%hdyr = 0.
        enddo
        !annual average solute concentration        
        if (gw_solute_flag == 1) then
          write(out_conc_yr,*) time%yrc
          do s=1,gw_nsolute
            !calculate average concentration
            do i=1,ncell
              gwsol_state(i)%solute(s)%cnyr = gwsol_state(i)%solute(s)%cnyr / time%day
            enddo
            !write out
            write(out_conc_yr,*) gwsol_nm(s) !solute name
            if(grid_type == "structured") then
              grid_val = 0.
              do i=1,grid_nrow
                do j=1,grid_ncol
                  if(cell_id_usg(i,j) > 0) then
                    grid_val(i,j) = gwsol_state(cell_id_usg(i,j))%solute(s)%cnyr
                  endif
                enddo
              enddo
              do i=1,grid_nrow
                write(out_conc_yr,101) (grid_val(i,j),j=1,grid_ncol)
              enddo
            else
              write(out_conc_yr,121) (gwsol_state(i)%solute(s)%cnyr,i=1,ncell)  
            endif
            !zero out for next year
            do i=1,ncell
              gwsol_state(i)%solute(s)%cnyr = 0.
            enddo
          enddo !next solute
          write(out_conc_yr,*)
        endif !check for solutes   
        
        !compute average daily groundwater fluxes (m3/day) for the year
        do i=1,ncell
          gw_ss_sum(i)%rech = gw_ss_sum(i)%rech / time%day_end_yr
          gw_ss_sum(i)%gwet = gw_ss_sum(i)%gwet / time%day_end_yr
          gw_ss_sum(i)%gwsw = gw_ss_sum(i)%gwsw / time%day_end_yr
          gw_ss_sum(i)%swgw = gw_ss_sum(i)%swgw / time%day_end_yr
          gw_ss_sum(i)%satx = gw_ss_sum(i)%satx / time%day_end_yr
          gw_ss_sum(i)%soil = gw_ss_sum(i)%soil / time%day_end_yr
          gw_ss_sum(i)%latl = gw_ss_sum(i)%latl / time%day_end_yr
          gw_ss_sum(i)%bndr = gw_ss_sum(i)%bndr / time%day_end_yr
          gw_ss_sum(i)%ppag = gw_ss_sum(i)%ppag / time%day_end_yr 
          gw_ss_sum(i)%ppdf = gw_ss_sum(i)%ppdf / time%day_end_yr
          gw_ss_sum(i)%ppex = gw_ss_sum(i)%ppex / time%day_end_yr
          gw_ss_sum(i)%tile = gw_ss_sum(i)%tile / time%day_end_yr
          gw_ss_sum(i)%resv = gw_ss_sum(i)%resv / time%day_end_yr
          gw_ss_sum(i)%wetl = gw_ss_sum(i)%wetl / time%day_end_yr
          gw_ss_sum(i)%fpln = gw_ss_sum(i)%fpln / time%day_end_yr
          gw_ss_sum(i)%canl = gw_ss_sum(i)%canl / time%day_end_yr
        enddo

        !compute average daily solute fluxes (kg/day) for the year 
        if (gw_solute_flag == 1) then
          do i=1,ncell
            do s=1,gw_nsolute
              gwsol_ss_sum(i)%solute(s)%rech = (gwsol_ss_sum(i)%solute(s)%rech/1000.) / time%day_end_yr !g --> kg
              gwsol_ss_sum(i)%solute(s)%gwsw = (gwsol_ss_sum(i)%solute(s)%gwsw/1000.) / time%day_end_yr !g --> kg
              gwsol_ss_sum(i)%solute(s)%swgw = (gwsol_ss_sum(i)%solute(s)%swgw/1000.) / time%day_end_yr !g --> kg
              gwsol_ss_sum(i)%solute(s)%soil = (gwsol_ss_sum(i)%solute(s)%soil/1000.) / time%day_end_yr !g --> kg
              gwsol_ss_sum(i)%solute(s)%satx = (gwsol_ss_sum(i)%solute(s)%satx/1000.) / time%day_end_yr !g --> kg
              gwsol_ss_sum(i)%solute(s)%ppex = (gwsol_ss_sum(i)%solute(s)%ppex/1000.) / time%day_end_yr !g --> kg
              gwsol_ss_sum(i)%solute(s)%tile = (gwsol_ss_sum(i)%solute(s)%tile/1000.) / time%day_end_yr !g --> kg
              gwsol_ss_sum(i)%solute(s)%resv = (gwsol_ss_sum(i)%solute(s)%resv/1000.) / time%day_end_yr !g --> kg
              gwsol_ss_sum(i)%solute(s)%wetl = (gwsol_ss_sum(i)%solute(s)%wetl/1000.) / time%day_end_yr !g --> kg
              gwsol_ss_sum(i)%solute(s)%fpln = (gwsol_ss_sum(i)%solute(s)%fpln/1000.) / time%day_end_yr !g --> kg
              gwsol_ss_sum(i)%solute(s)%canl = (gwsol_ss_sum(i)%solute(s)%canl/1000.) / time%day_end_yr !g --> kg
              gwsol_ss_sum(i)%solute(s)%advn = (gwsol_ss_sum(i)%solute(s)%advn/1000.) / time%day_end_yr !g --> kg
              gwsol_ss_sum(i)%solute(s)%disp = (gwsol_ss_sum(i)%solute(s)%disp/1000.) / time%day_end_yr !g --> kg
              gwsol_ss_sum(i)%solute(s)%rcti = (gwsol_ss_sum(i)%solute(s)%rcti/1000.) / time%day_end_yr !g --> kg
              gwsol_ss_sum(i)%solute(s)%rcto = (gwsol_ss_sum(i)%solute(s)%rcto/1000.) / time%day_end_yr !g --> kg
              gwsol_ss_sum(i)%solute(s)%minl = (gwsol_ss_sum(i)%solute(s)%minl/1000.) / time%day_end_yr !g --> kg
              gwsol_ss_sum(i)%solute(s)%sorb = (gwsol_ss_sum(i)%solute(s)%sorb/1000.) / time%day_end_yr !g --> kg
            enddo
          enddo
        endif
        
        !recharge
        write(out_gw_rech,*) 'Recharge for year (m3/day):',time%yrc
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_ss_sum(cell_id_usg(i,j))%rech
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_gw_rech,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_gw_rech,121) (gw_ss_sum(i)%rech,i=1,ncell)
        endif
        write(out_gw_rech,*)
        if (gw_solute_flag == 1) then !solute mass flux
          do s=1,gw_nsolute
            write(out_sol_rech,*) gwsol_nm(s),'recharge flux for year (kg/day):',time%yrc
            if(grid_type == "structured") then
              grid_val = 0.
              do i=1,grid_nrow
                do j=1,grid_ncol
                  if(cell_id_usg(i,j) > 0) then
                    grid_val(i,j) = gwsol_ss_sum(cell_id_usg(i,j))%solute(s)%rech
                  endif
                enddo
              enddo
              do i=1,grid_nrow
                write(out_sol_rech,101) (grid_val(i,j),j=1,grid_ncol)
              enddo
            else
              write(out_sol_rech,121) (gwsol_ss_sum(i)%solute(s)%rech,i=1,ncell)
            endif
            write(out_sol_rech,*)  
          enddo
        endif
        !groundwater ET
        write(out_gw_et,*) 'Groundwater ET for year:',time%yrc
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_ss_sum(cell_id_usg(i,j))%gwet
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_gw_et,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_gw_et,121) (gw_ss_sum(i)%gwet,i=1,ncell)
        endif
        write(out_gw_et,*)
        !gw-sw exchange rates
        write(out_gwsw,*) 'GW-SW Exchange Rates for year:',time%yrc
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_ss_sum(cell_id_usg(i,j))%gwsw
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_gwsw,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_gwsw,121) (gw_ss_sum(i)%gwsw,i=1,ncell)
        endif
        write(out_gwsw,*)
        if (gw_solute_flag == 1) then !solute mass flux
          do s=1,gw_nsolute
            write(out_sol_gwsw,*) gwsol_nm(s),'gw-channel flux for year (kg/day):',time%yrc
            if(grid_type == "structured") then
              grid_val = 0.
              do i=1,grid_nrow
                do j=1,grid_ncol
                  if(cell_id_usg(i,j) > 0) then
                    grid_val(i,j) = gwsol_ss_sum(cell_id_usg(i,j))%solute(s)%gwsw
                  endif
                enddo
              enddo
              do i=1,grid_nrow
                write(out_sol_gwsw,101) (grid_val(i,j),j=1,grid_ncol)
              enddo
            else
              write(out_sol_gwsw,121) (gwsol_ss_sum(i)%solute(s)%gwsw,i=1,ncell)
            endif
            write(out_sol_gwsw,*)  
          enddo
        endif
        !saturation excess flow
        if(gw_satx_flag.eq.1) then
        write(out_gw_satex,*) 'Saturation Excess Volumes for:',time%yrc
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_ss_sum(cell_id_usg(i,j))%satx
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_gw_satex,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_gw_satex,121) (gw_ss_sum(i)%satx,i=1,ncell)
        endif
        write(out_gw_satex,*)
        if (gw_solute_flag == 1) then !solute mass flux
          do s=1,gw_nsolute
            write(out_sol_satx,*) gwsol_nm(s),'sat. excess flux for year (kg/day):',time%yrc
            if(grid_type == "structured") then
              grid_val = 0.
              do i=1,grid_nrow
                do j=1,grid_ncol
                  if(cell_id_usg(i,j) > 0) then
                    grid_val(i,j) = gwsol_ss_sum(cell_id_usg(i,j))%solute(s)%satx
                  endif
                enddo
              enddo
              do i=1,grid_nrow
                write(out_sol_satx,101) (grid_val(i,j),j=1,grid_ncol)
              enddo
            else
              write(out_sol_satx,121) (gwsol_ss_sum(i)%solute(s)%satx,i=1,ncell)
            endif
            write(out_sol_satx,*)  
          enddo
        endif
        endif
        !groundwater --> soil transfer
        if(gw_soil_flag.eq.1) then
        write(out_gw_soil,*) 'Groundwater --> Soil Transfer for:',time%yrc
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_ss_sum(cell_id_usg(i,j))%soil
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_gw_soil,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_gw_soil,121) (gw_ss_sum(i)%soil,i=1,ncell) 
        endif
        write(out_gw_soil,*)
        if (gw_solute_flag == 1) then !solute mass flux
          do s=1,gw_nsolute
            write(out_sol_soil,*) gwsol_nm(s),'gw-->soil flux for year (kg/day):',time%yrc
            if(grid_type == "structured") then
              grid_val = 0.
              do i=1,grid_nrow
                do j=1,grid_ncol
                  if(cell_id_usg(i,j) > 0) then
                    grid_val(i,j) = gwsol_ss_sum(cell_id_usg(i,j))%solute(s)%soil
                  endif
                enddo
              enddo
              do i=1,grid_nrow
                write(out_sol_soil,101) (grid_val(i,j),j=1,grid_ncol)
              enddo
            else
              write(out_sol_soil,121) (gwsol_ss_sum(i)%solute(s)%soil,i=1,ncell)
            endif
            write(out_sol_soil,*)  
          enddo
        endif
        endif
        !lateral flow
        write(out_lateral,*) 'Lateral flow for year:',time%yrc
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_ss_sum(cell_id_usg(i,j))%latl
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_lateral,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_lateral,121) (gw_ss_sum(i)%latl,i=1,ncell)
        endif
        write(out_lateral,*)
        !tile drain flow
        if(gw_tile_flag == 1) then
        write(out_gw_tile,*) 'Tile Drain Outflow Volumes for:',time%yrc
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_ss_sum(cell_id_usg(i,j))%tile
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_gw_tile,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_gw_tile,121) (gw_ss_sum(i)%tile,i=1,ncell)  
        endif
        write(out_gw_tile,*)
        if (gw_solute_flag == 1) then !solute mass flux
          do s=1,gw_nsolute
            write(out_sol_tile,*) gwsol_nm(s),'tile drain flux for year (kg/day):',time%yrc
            if(grid_type == "structured") then
              grid_val = 0.
              do i=1,grid_nrow
                do j=1,grid_ncol
                  if(cell_id_usg(i,j) > 0) then
                    grid_val(i,j) = gwsol_ss_sum(cell_id_usg(i,j))%solute(s)%tile
                  endif
                enddo
              enddo
              do i=1,grid_nrow
                write(out_sol_tile,101) (grid_val(i,j),j=1,grid_ncol)
              enddo
            else
              write(out_sol_tile,121) (gwsol_ss_sum(i)%solute(s)%tile,i=1,ncell)
            endif
            write(out_sol_tile,*)  
          enddo
        endif
        endif
        !pumping (irrigation)
        write(out_gw_pumpag,*) 'Pumping rate for year (m3/day):',time%yrc
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_ss_sum(cell_id_usg(i,j))%ppag
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_gw_pumpag,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_gw_pumpag,121) (gw_ss_sum(i)%ppag,i=1,ncell)
        endif
        write(out_gw_pumpag,*)
        if (gw_solute_flag == 1) then !solute mass flux
          do s=1,gw_nsolute
            write(out_sol_ppag,*) gwsol_nm(s),'ag pumping flux for year (kg/day):',time%yrc
            if(grid_type == "structured") then
              grid_val = 0.
              do i=1,grid_nrow
                do j=1,grid_ncol
                  if(cell_id_usg(i,j) > 0) then
                    grid_val(i,j) = gwsol_ss_sum(cell_id_usg(i,j))%solute(s)%ppag
                  endif
                enddo
              enddo
              do i=1,grid_nrow
                write(out_sol_ppag,101) (grid_val(i,j),j=1,grid_ncol)
              enddo
            else
              write(out_sol_ppag,121) (gwsol_ss_sum(i)%solute(s)%ppag,i=1,ncell)
            endif
            write(out_sol_ppag,*)  
          enddo
        endif
        !pumping (irrigation) (for HRUs)
        do i=1,sp_ob%hru
          hru_pump_yr_all(i,time%yrs) = hru_pump_yr(i) 
        enddo
        hru_pump_yr = 0.
        !pumping deficit (not satisfied) (irrigation)
        write(out_gw_pumpdef,*) 'Pumping rate not satisfied for year (m3/day):',time%yrc
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_ss_sum(cell_id_usg(i,j))%ppdf
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_gw_pumpdef,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_gw_pumpdef,121) (gw_ss_sum(i)%ppdf,i=1,ncell)
        endif
        write(out_gw_pumpdef,*)
        !pumping (user specified)
        if (gw_pumpex_flag == 1) then
        write(out_gw_pumpex,*) 'Pumping rate for year (m3/day):',time%yrc
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_ss_sum(cell_id_usg(i,j))%ppex
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_gw_pumpex,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_gw_pumpex,121) (gw_ss_sum(i)%ppex,i=1,ncell)
        endif
        write(out_gw_pumpex,*)
        if (gw_solute_flag == 1) then !solute mass flux
          do s=1,gw_nsolute
            write(out_sol_ppex,*) gwsol_nm(s),'ex pumping flux for year (kg/day):',time%yrc
            if(grid_type == "structured") then
              grid_val = 0.
              do i=1,grid_nrow
                do j=1,grid_ncol
                  if(cell_id_usg(i,j) > 0) then
                    grid_val(i,j) = gwsol_ss_sum(cell_id_usg(i,j))%solute(s)%ppex
                  endif
                enddo
              enddo
              do i=1,grid_nrow
                write(out_sol_ppex,101) (grid_val(i,j),j=1,grid_ncol)
              enddo
            else
              write(out_sol_ppex,121) (gwsol_ss_sum(i)%solute(s)%ppex,i=1,ncell)
            endif
            write(out_sol_ppex,*)  
          enddo
        endif
        endif
        !groundwater-reservoir exchange
        if (gw_res_flag == 1) then
        write(out_gw_res,*) 'Groundwater-Reservoir Exchange Volumes for:',time%yrc
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_ss_sum(cell_id_usg(i,j))%resv
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_gw_res,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_gw_res,121) (gw_ss_sum(i)%resv,i=1,ncell)	  	
        endif
        write(out_gw_res,*)
        if (gw_solute_flag == 1) then !solute mass flux
          do s=1,gw_nsolute
            write(out_sol_resv,*) gwsol_nm(s),'gw-reservoir flux for year (kg/day):',time%yrc
            if(grid_type == "structured") then
              grid_val = 0.
              do i=1,grid_nrow
                do j=1,grid_ncol
                  if(cell_id_usg(i,j) > 0) then
                    grid_val(i,j) = gwsol_ss_sum(cell_id_usg(i,j))%solute(s)%resv
                  endif
                enddo
              enddo
              do i=1,grid_nrow
                write(out_sol_resv,101) (grid_val(i,j),j=1,grid_ncol)
              enddo
            else
              write(out_sol_resv,121) (gwsol_ss_sum(i)%solute(s)%resv,i=1,ncell)
            endif
            write(out_sol_resv,*)  
          enddo
        endif
        endif
        !groundwater-wetland exchange
        if (gw_wet_flag == 1) then
        write(out_gw_wet,*) 'Groundwater outflow to wetlands for:',time%yrc	
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_ss_sum(cell_id_usg(i,j))%wetl
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_gw_wet,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_gw_wet,121) (gw_ss_sum(i)%wetl,i=1,ncell)	 	
        endif
        write(out_gw_wet,*)
        if (gw_solute_flag == 1) then !solute mass flux
          do s=1,gw_nsolute
            write(out_sol_wetl,*) gwsol_nm(s),'gw-wetland flux for year (kg/day):',time%yrc
            if(grid_type == "structured") then
              grid_val = 0.
              do i=1,grid_nrow
                do j=1,grid_ncol
                  if(cell_id_usg(i,j) > 0) then
                    grid_val(i,j) = gwsol_ss_sum(cell_id_usg(i,j))%solute(s)%wetl
                  endif
                enddo
              enddo
              do i=1,grid_nrow
                write(out_sol_wetl,101) (grid_val(i,j),j=1,grid_ncol)
              enddo
            else
              write(out_sol_wetl,121) (gwsol_ss_sum(i)%solute(s)%wetl,i=1,ncell)
            endif
            write(out_sol_wetl,*)  
          enddo
        endif
        endif
        !groundwater-canal exchange
        if (gw_canal_flag == 1) then	
        write(out_gw_canal,*) 'Groundwater-Canal Exchange Volumes for:',time%yrc	
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_ss_sum(cell_id_usg(i,j))%canl
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_gw_canal,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_gw_canal,121) (gw_ss_sum(i)%canl,i=1,ncell)	 	
        endif
        write(out_gw_canal,*)
        if (gw_solute_flag == 1) then !solute mass flux
          do s=1,gw_nsolute
            write(out_sol_canl,*) gwsol_nm(s),'gw-canal flux for year (kg/day):',time%yrc
            if(grid_type == "structured") then
              grid_val = 0.
              do i=1,grid_nrow
                do j=1,grid_ncol
                  if(cell_id_usg(i,j) > 0) then
                    grid_val(i,j) = gwsol_ss_sum(cell_id_usg(i,j))%solute(s)%canl
                  endif
                enddo
              enddo
              do i=1,grid_nrow
                write(out_sol_canl,101) (grid_val(i,j),j=1,grid_ncol)
              enddo
            else
              write(out_sol_canl,121) (gwsol_ss_sum(i)%solute(s)%canl,i=1,ncell)
            endif
            write(out_sol_canl,*)  
          enddo
        endif
        endif
        !floodplain exchange
        if (gw_fp_flag == 1) then
        write(out_gw_fp,*) 'Floodplain Exchange Volumes for:',time%yrc	
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_ss_sum(cell_id_usg(i,j))%fpln
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_gw_fp,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_gw_fp,121) (gw_ss_sum(i)%fpln,i=1,ncell)		
        endif
        write(out_gw_fp,*)
        if (gw_solute_flag == 1) then !solute mass flux
          do s=1,gw_nsolute
            write(out_sol_fpln,*) gwsol_nm(s),'gw-floodplain flux for year (kg/day):',time%yrc
            if(grid_type == "structured") then
              grid_val = 0.
              do i=1,grid_nrow
                do j=1,grid_ncol
                  if(cell_id_usg(i,j) > 0) then
                    grid_val(i,j) = gwsol_ss_sum(cell_id_usg(i,j))%solute(s)%fpln
                  endif
                enddo
              enddo
              do i=1,grid_nrow
                write(out_sol_fpln,101) (grid_val(i,j),j=1,grid_ncol)
              enddo
            else
              write(out_sol_fpln,121) (gwsol_ss_sum(i)%solute(s)%fpln,i=1,ncell)
            endif
            write(out_sol_fpln,*)  
          enddo
        endif
        endif
        !chemical reaction (produced = positive values)
        if (gw_solute_flag == 1) then !solute mass flux
          do s=1,gw_nsolute
            write(out_sol_rcti,*) gwsol_nm(s),'chem. reaction flux for year (kg/day):',time%yrc
            if(grid_type == "structured") then
              grid_val = 0.
              do i=1,grid_nrow
                do j=1,grid_ncol
                  if(cell_id_usg(i,j) > 0) then
                    grid_val(i,j) = gwsol_ss_sum(cell_id_usg(i,j))%solute(s)%rcti
                  endif
                enddo
              enddo
              do i=1,grid_nrow
                write(out_sol_rcti,101) (grid_val(i,j),j=1,grid_ncol)
              enddo
            else
              write(out_sol_rcti,121) (gwsol_ss_sum(i)%solute(s)%rcti,i=1,ncell)
            endif
            write(out_sol_rcti,*)  
          enddo
        endif
        !chemical reaction (consumed = negative values)
        if(gw_solute_flag == 1) then !solute mass flux
          do s=1,gw_nsolute
            write(out_sol_rcto,*) gwsol_nm(s),'chem. reaction flux for year (kg/day):',time%yrc
            if(grid_type == "structured") then
              grid_val = 0.
              do i=1,grid_nrow
                do j=1,grid_ncol
                  if(cell_id_usg(i,j) > 0) then
                    grid_val(i,j) = gwsol_ss_sum(cell_id_usg(i,j))%solute(s)%rcto
                  endif
                enddo
              enddo
              do i=1,grid_nrow
                write(out_sol_rcto,101) (grid_val(i,j),j=1,grid_ncol)
              enddo
            else
              write(out_sol_rcto,121) (gwsol_ss_sum(i)%solute(s)%rcto,i=1,ncell)
            endif
            write(out_sol_rcto,*)  
          enddo
        endif
        !precipitation-dissolution
        if(gw_solute_flag == 1) then !solute mass flux
          do s=1,gw_nsolute
            write(out_sol_minl,*) gwsol_nm(s),'mineral dissolved mass for year (kg/day):',time%yrc
            if(grid_type == "structured") then
              grid_val = 0.
              do i=1,grid_nrow
                do j=1,grid_ncol
                  if(cell_id_usg(i,j) > 0) then
                    grid_val(i,j) = gwsol_ss_sum(cell_id_usg(i,j))%solute(s)%minl
                  endif
                enddo
              enddo
              do i=1,grid_nrow
                write(out_sol_minl,101) (grid_val(i,j),j=1,grid_ncol)
              enddo
            else
              write(out_sol_minl,121) (gwsol_ss_sum(i)%solute(s)%minl,i=1,ncell)
            endif
            write(out_sol_minl,*)  
          enddo
        endif
        !sorption
        if (gw_solute_flag == 1) then !solute mass flux
          do s=1,gw_nsolute
            write(out_sol_sorb,*) gwsol_nm(s),'sorption flux for year (kg/day):',time%yrc
            if(grid_type == "structured") then
              grid_val = 0.
              do i=1,grid_nrow
                do j=1,grid_ncol
                  if(cell_id_usg(i,j) > 0) then
                    grid_val(i,j) = gwsol_ss_sum(cell_id_usg(i,j))%solute(s)%sorb
                  endif
                enddo
              enddo
              do i=1,grid_nrow
                write(out_sol_sorb,101) (grid_val(i,j),j=1,grid_ncol)
              enddo
            else
              write(out_sol_sorb,121) (gwsol_ss_sum(i)%solute(s)%sorb,i=1,ncell)
            endif
            write(out_sol_sorb,*)  
          enddo
        endif
        !zero out flux sums to prepare for the next year
        !flow
        do i=1,ncell
          gw_ss_sum(i)%rech = 0.
          gw_ss_sum(i)%gwet = 0.
          gw_ss_sum(i)%gwsw = 0.
          gw_ss_sum(i)%swgw = 0.
          gw_ss_sum(i)%satx = 0.
          gw_ss_sum(i)%soil = 0.
          gw_ss_sum(i)%latl = 0.
          gw_ss_sum(i)%bndr = 0.
          gw_ss_sum(i)%ppag = 0.
          gw_ss_sum(i)%ppdf = 0.
          gw_ss_sum(i)%ppex = 0.
          gw_ss_sum(i)%tile = 0.
          gw_ss_sum(i)%resv = 0.
          gw_ss_sum(i)%wetl = 0.
          gw_ss_sum(i)%fpln = 0.
          gw_ss_sum(i)%canl = 0.
        enddo
        !solute
        do i=1,ncell
          do s=1,gw_nsolute
            gwsol_ss_sum(i)%solute(s)%rech = 0.
            gwsol_ss_sum(i)%solute(s)%gwsw = 0.
            gwsol_ss_sum(i)%solute(s)%swgw = 0.
            gwsol_ss_sum(i)%solute(s)%satx = 0.
            gwsol_ss_sum(i)%solute(s)%soil = 0.
            gwsol_ss_sum(i)%solute(s)%ppag = 0.
            gwsol_ss_sum(i)%solute(s)%ppex = 0.
            gwsol_ss_sum(i)%solute(s)%tile = 0.
            gwsol_ss_sum(i)%solute(s)%resv = 0.
            gwsol_ss_sum(i)%solute(s)%wetl = 0.
            gwsol_ss_sum(i)%solute(s)%fpln = 0.
            gwsol_ss_sum(i)%solute(s)%canl = 0.
            gwsol_ss_sum(i)%solute(s)%advn = 0.
            gwsol_ss_sum(i)%solute(s)%disp = 0.
            gwsol_ss_sum(i)%solute(s)%rcti = 0.
            gwsol_ss_sum(i)%solute(s)%rcto = 0.
            gwsol_ss_sum(i)%solute(s)%minl = 0.
            gwsol_ss_sum(i)%solute(s)%sorb = 0.
          enddo
        enddo
        !yearly water balance
        if(gwflag_yr.eq.1) then
          write(out_gwbal_yr,105) time%yrc, & 
                                  ss_grid_yr%chng,ss_grid_yr%rech,ss_grid_yr%gwet,ss_grid_yr%gwsw,ss_grid_yr%swgw, &
                                  ss_grid_yr%satx,ss_grid_yr%soil,ss_grid_yr%latl,ss_grid_yr%bndr,ss_grid_yr%ppag, &
                                  ss_grid_yr%ppex,ss_grid_yr%tile,ss_grid_yr%resv,ss_grid_yr%wetl,ss_grid_yr%canl, &
                                  ss_grid_yr%fpln,ss_grid_yr%ppdf
        endif
        !if usgs wells are used, calculate and store annual average values for usgs wells and observation cells
        if (usgs_obs == 1) then
          do k=1,gw_num_obs_wells
            !compute the average head value for the year, for the current observation cell 
            !also, compute the average saturated thickness for the year
            head_sum = 0.
            sat_sum = 0.
            do i=1,time%day
              head_sum = head_sum + gw_obs_head_annual(k,i)
              sat_sum = sat_sum + gw_obs_sat_annual(k,i)
            enddo
            head_avg = head_sum / time%day
            sim_head_vals(k,time%yrs) = head_avg
            sat_avg = sat_sum / time%day
            sim_sat_vals(k,time%yrs) = sat_avg
          enddo
        endif
        !zero out annual arrays
        ss_grid_yr%chng = 0.
        ss_grid_yr%rech = 0.
        ss_grid_yr%gwet = 0.
        ss_grid_yr%gwsw = 0.
        ss_grid_yr%swgw = 0.
        ss_grid_yr%satx = 0.
        ss_grid_yr%soil = 0.
        ss_grid_yr%latl = 0.
        ss_grid_yr%bndr = 0.
        ss_grid_yr%ppag = 0.
        ss_grid_yr%ppdf = 0.
        ss_grid_yr%ppex = 0.
        ss_grid_yr%tile = 0.
        ss_grid_yr%resv = 0.
        ss_grid_yr%wetl = 0.
        ss_grid_yr%canl = 0.
        ss_grid_yr%fpln = 0.
        !solute mass values
        if (gw_solute_flag == 1) then
          do s=1,gw_nsolute !loop through the solutes
            !write out annual values
            if(gwflag_yr.eq.1) then
              write(out_solbal_yr+s,105) time%yrc, &
                                         sol_grid_chng_yr,sol_grid_rech_yr,sol_grid_gwsw_yr,sol_grid_swgw_yr,sol_grid_satx_yr, &
														 		         sol_grid_soil_yr,sol_grid_advn_yr,sol_grid_disp_yr, &
                                         sol_grid_rcti_yr,sol_grid_rcto_yr,sol_grid_minl_yr,sol_grid_sorb_yr, &
                                         sol_grid_ppag_yr,sol_grid_ppex_yr,sol_grid_tile_yr,sol_grid_resv_yr,sol_grid_wetl_yr, &
                                         sol_grid_canl_yr,sol_grid_fpln_yr
            endif
            !zero out values for next year
            sol_grid_chng_yr = 0.
            sol_grid_rech_yr = 0.
            sol_grid_gwsw_yr = 0.
            sol_grid_swgw_yr = 0.
            sol_grid_satx_yr = 0.
            sol_grid_soil_yr = 0.
            sol_grid_advn_yr = 0.
            sol_grid_disp_yr = 0.
            sol_grid_rcti_yr = 0.
            sol_grid_rcto_yr = 0.
            sol_grid_minl_yr = 0.
            sol_grid_sorb_yr = 0.
            sol_grid_ppag_yr = 0.
            sol_grid_ppex_yr = 0.
            sol_grid_tile_yr = 0.
            sol_grid_resv_yr = 0.
            sol_grid_wetl_yr = 0.
            sol_grid_canl_yr = 0.
            sol_grid_fpln_yr = 0.
          enddo !go to next solute
        endif
      endif
      
      
      
      !9. last day of the simulationreached: print out average annual values; calculate head and flow metrics
      if(time%yrc == time%yrc_end .and. time%day == time%day_end) then

        !pumping for HRUs
        num_months = time%nbyr * 12
        do i=1,sp_ob%hru
           write(out_hru_pump_mo,105) i,(hru_pump_mo_all(i,j),j=1,num_months) 
           write(out_hru_pump_yr,105) i,(hru_pump_yr_all(i,j),j=1,time%nbyr)
        enddo
        
        !average annual water balance
        ss_grid_tt%chng = ss_grid_tt%chng + (vaft_grid-vbef_grid)
        ss_grid_tt%rech = ss_grid_tt%rech / time%nbyr
        ss_grid_tt%gwet = ss_grid_tt%gwet / time%nbyr
        ss_grid_tt%gwsw = ss_grid_tt%gwsw / time%nbyr
        ss_grid_tt%swgw = ss_grid_tt%swgw / time%nbyr
        ss_grid_tt%satx = ss_grid_tt%satx / time%nbyr
        ss_grid_tt%soil = ss_grid_tt%soil / time%nbyr
        ss_grid_tt%latl = ss_grid_tt%latl / time%nbyr
        ss_grid_tt%bndr = ss_grid_tt%bndr / time%nbyr
        ss_grid_tt%ppag = ss_grid_tt%ppag / time%nbyr
        ss_grid_tt%ppdf = ss_grid_tt%ppdf / time%nbyr
        ss_grid_tt%ppex = ss_grid_tt%ppex / time%nbyr
        ss_grid_tt%tile = ss_grid_tt%tile / time%nbyr
        ss_grid_tt%resv = ss_grid_tt%resv / time%nbyr
        ss_grid_tt%wetl = ss_grid_tt%wetl / time%nbyr
        ss_grid_tt%canl = ss_grid_tt%canl / time%nbyr
        ss_grid_tt%fpln = ss_grid_tt%fpln / time%nbyr
        if(gwflag_aa.eq.1) then
          write(out_gwbal_aa,105) time%yrc, &          
                                  ss_grid_tt%chng,ss_grid_tt%rech,ss_grid_tt%gwet,ss_grid_tt%gwsw,ss_grid_tt%swgw, &
                                  ss_grid_tt%satx,ss_grid_tt%soil,ss_grid_tt%latl,ss_grid_tt%bndr,ss_grid_tt%ppag, &
                                  ss_grid_tt%ppex,ss_grid_tt%tile,ss_grid_tt%resv,ss_grid_tt%wetl,ss_grid_tt%canl, &
                                  ss_grid_tt%fpln,ss_grid_tt%ppdf
        endif
        
        !average annual solute values
        if (gw_solute_flag == 1) then
          do s=1,gw_nsolute
            sol_grid_chng_tt(s) = sol_grid_chng_tt(s) + (sol_grid_maft-sol_grid_mbef)
            sol_grid_rech_tt(s) = sol_grid_rech_tt(s) / time%nbyr
            sol_grid_gwsw_tt(s) = sol_grid_gwsw_tt(s) / time%nbyr
            sol_grid_swgw_tt(s) = sol_grid_swgw_tt(s) / time%nbyr
            sol_grid_satx_tt(s) = sol_grid_satx_tt(s) / time%nbyr
            sol_grid_advn_tt(s) = sol_grid_advn_tt(s) / time%nbyr
            sol_grid_disp_tt(s) = sol_grid_disp_tt(s) / time%nbyr
            sol_grid_rcti_tt(s) = sol_grid_rcti_tt(s) / time%nbyr
            sol_grid_rcto_tt(s) = sol_grid_rcto_tt(s) / time%nbyr
            sol_grid_minl_tt(s) = sol_grid_minl_tt(s) / time%nbyr
            sol_grid_sorb_tt(s) = sol_grid_sorb_tt(s) / time%nbyr
            sol_grid_ppag_tt(s) = sol_grid_ppag_tt(s) / time%nbyr
            sol_grid_ppex_tt(s) = sol_grid_ppex_tt(s) / time%nbyr
            sol_grid_tile_tt(s) = sol_grid_tile_tt(s) / time%nbyr
            sol_grid_soil_tt(s) = sol_grid_soil_tt(s) / time%nbyr
            sol_grid_resv_tt(s) = sol_grid_resv_tt(s) / time%nbyr
            sol_grid_wetl_tt(s) = sol_grid_wetl_tt(s) / time%nbyr
            sol_grid_canl_tt(s) = sol_grid_canl_tt(s) / time%nbyr
            sol_grid_fpln_tt(s) = sol_grid_fpln_tt(s) / time%nbyr
            if(gwflag_aa.eq.1) then
              write(out_solbal_aa+s,105) time%yrc, &
                                        sol_grid_chng_tt(s),sol_grid_rech_tt(s),sol_grid_gwsw_tt(s),sol_grid_swgw_tt(s),  &
                                        sol_grid_satx_tt(s),sol_grid_soil_tt(s),sol_grid_advn_tt(s),sol_grid_disp_tt(s),  &
                                        sol_grid_rcti_tt(s),sol_grid_rcto_tt(s),sol_grid_minl_tt(s),sol_grid_sorb_tt(s),  &
                                        sol_grid_ppag_tt(s),sol_grid_ppex_tt(s),sol_grid_tile_tt(s),sol_grid_resv_tt(s),  &
                                        sol_grid_wetl_tt(s),sol_grid_canl_tt(s),sol_grid_fpln_tt(s)
            endif
          enddo !next solute
        endif
        
        !write out observed and simulated annual averaged gw head values at USGS well locations
        if (usgs_obs == 1) then
          if(gw_flow_cal.eq.0) then
            !first, calculate mean absolute error (MAE) (meters) for gw head values
            val_count = 0
            error_sum = 0.
            sum_sat = 0.
            do k=1,gw_num_obs_wells
              usgs_yr = time%yrc_start - 1920 + 1 !get the right year from the USGS data set
              val_count_well = 0
              error_sum_well = 0.
              sum_sat_well = 0.
              do j=1,time%nbyr
                if(usgs_head_vals(k,usgs_yr).ne.(-9.99)) then
                  head_residual = usgs_head_vals(k,usgs_yr) - sim_head_vals(k,j)
                  sum_sat = sum_sat + sim_sat_vals(k,j)
                  sum_sat_well = sum_sat_well + sim_sat_vals(k,j)
                  error_sum = error_sum + abs(head_residual)
                  error_sum_well = error_sum_well + head_residual
                  val_count = val_count + 1
                  val_count_well = val_count_well + 1
                endif
                usgs_yr = usgs_yr + 1
              enddo
              if(val_count_well.gt.0) then
                head_mae_well(k,1) = error_sum_well / val_count_well
                sat_div_well(k,1) = abs((sum_sat_well/val_count_well) / head_mae_well(k,1))
                num_gw_meas_well(k,1) = val_count_well
              else
                head_mae_well(k,1) = 0.
                sat_div_well(k,1) = 0.
                num_gw_meas_well(k,1) = 0
              endif
            enddo
            if(val_count.gt.0) then
              head_mae = error_sum / val_count
              sat_div = (sum_sat/val_count) / head_mae
              num_gw_meas = val_count
            else
              head_mae = 0.
              sat_div = 0.
              num_gw_meas = 0
            endif
            !write out results
            write(out_gwobs_usgs,*) 'MAE (m), SAT/MAE, #Meas'
            write(out_gwobs_usgs,*) head_mae,sat_div,num_gw_meas
            write(out_gwobs_usgs,*)
            write(out_gwobs_usgs,*) 'MAE (m), SAT/MAE, #Meas per well'
            write(out_gwobs_usgs,*) 'Observed values - Simulated values'
            do k=1,gw_num_obs_wells
              write(out_gwobs_usgs,111) usgs_id(k),head_mae_well(k,1),sat_div_well(k,1),num_gw_meas_well(k,1)
            enddo
          else
            num_yrs_calb = gw_flow_cal_yrs
            num_yrs_test = time%nbyr - num_yrs_calb
            !calibration period
            !first, calculate mean absolute error (MAE) (meters) for gw head values
            val_count = 0
            error_sum = 0.
            sum_sat = 0.
            do k=1,gw_num_obs_wells
              usgs_yr = time%yrc_start - 1920 + 1 !get the right year from the USGS data set
              val_count_well = 0
              error_sum_well = 0.
              sum_sat_well = 0.
              do j=1,num_yrs_calb
                if(usgs_head_vals(k,usgs_yr).ne.(-9.99)) then
                  head_residual = usgs_head_vals(k,usgs_yr) - sim_head_vals(k,j)
                  sum_sat = sum_sat + sim_sat_vals(k,j)
                  sum_sat_well = sum_sat_well + sim_sat_vals(k,j)
                  error_sum = error_sum + abs(head_residual)
                  error_sum_well = error_sum_well + head_residual
                  val_count = val_count + 1
                  val_count_well = val_count_well + 1
                endif
                usgs_yr = usgs_yr + 1
              enddo
              if(val_count_well.gt.0) then
                head_mae_well(k,1) = error_sum_well / val_count_well
                sat_div_well(k,1) = abs((sum_sat_well/val_count_well) / head_mae_well(k,1))
                num_gw_meas_well(k,1) = val_count_well
              else
                head_mae_well(k,1) = 0.
                sat_div_well(k,1) = 0.
                num_gw_meas_well(k,1) = 0
              endif
            enddo
            if(val_count.gt.0) then
              head_mae_calb = error_sum / val_count
              sat_div_calb = (sum_sat/val_count) / head_mae_calb
              num_gw_meas_calb = val_count
            else
              head_mae_calb = 0.
              sat_div_calb = 0.
              num_gw_meas_calb = 0
            endif
            !testing period
            val_count = 0
            error_sum = 0.
            sum_sat = 0.
            do k=1,gw_num_obs_wells
              usgs_yr = time%yrc_start - 1920 + 1 + num_yrs_calb !get the right year from the USGS data set
              val_count_well = 0
              error_sum_well = 0.
              sum_sat_well = 0.
              do j=1,num_yrs_test
                if(usgs_head_vals(k,usgs_yr).ne.(-9.99)) then
                  head_residual = usgs_head_vals(k,usgs_yr) - sim_head_vals(k,j+num_yrs_calb)
                  sum_sat = sum_sat + sim_sat_vals(k,j+num_yrs_calb)
                  sum_sat_well = sum_sat_well + sim_sat_vals(k,j+num_yrs_calb)
                  error_sum = error_sum + abs(head_residual)
                  error_sum_well = error_sum_well + head_residual
                  val_count = val_count + 1
                  val_count_well = val_count_well + 1
                endif
                usgs_yr = usgs_yr + 1
              enddo
              if(val_count_well.gt.0) then
                head_mae_well(k,2) = error_sum_well / val_count_well
                sat_div_well(k,2) = abs((sum_sat_well/val_count_well) / head_mae_well(k,1))
                num_gw_meas_well(k,2) = val_count_well
              else
                head_mae_well(k,2) = 0.
                sat_div_well(k,2) = 0.
                num_gw_meas_well(k,2) = 0
              endif
            enddo
            if(val_count.gt.0) then
              head_mae_test = error_sum / val_count
              sat_div_test = (sum_sat/val_count) / head_mae_test
              num_gw_meas_test = val_count
            else
              head_mae_test = 0.
              sat_div_test = 0.
              num_gw_meas_test = 0
            endif
            !write out results
            write(out_gwobs_usgs,*) 'MAE_calib,MAE_test,SAT/MAE_calib,SAT_MAE_test,#calib,#test'
            write(out_gwobs_usgs,115) head_mae_calb,head_mae_test,sat_div_calb,sat_div_test,num_gw_meas_calb,num_gw_meas_test
            write(out_gwobs_usgs,*)
            write(out_gwobs_usgs,*) 'MAE (m), SAT/MAE per well, #Meas per well'
            write(out_gwobs_usgs,*) 'Observed values - Simulated values'
            do k=1,gw_num_obs_wells
              write(out_gwobs_usgs,116) usgs_id(k),head_mae_well(k,1),head_mae_well(k,2),sat_div_well(k,1),sat_div_well(k,2),  &
                 num_gw_meas_well(k,1),num_gw_meas_well(k,2)
            enddo
          endif
          write(out_gwobs_usgs,*)
          write(out_gwobs_usgs,*) 'Year, Well, Row, Col, Obs, Sim, SatThick'
          do k=1,gw_num_obs_wells
            write_yr = time%yrc_start
            usgs_yr = time%yrc_start - 1920 + 1 !get the right year from the USGS data set
            do j=1,time%nbyr
              write(out_gwobs_usgs,110) write_yr,usgs_id(k),gw_obs_cells(k),usgs_head_vals(k,usgs_yr),  &
                 sim_head_vals(k,j),sim_sat_vals(k,j)
              write_yr = write_yr + 1
              usgs_yr = usgs_yr + 1
            enddo
          enddo
        endif
        
        !write out monthly flow rates for specified channels
        if (stream_obs == 1) then
          if(gw_num_obs_chan.gt.0) then
          !first, calculate Nash-Sutcliffe model efficiciency coefficient (NSE) for each specified channel
          allocate(stream_nse(gw_num_obs_chan,2))
          allocate(stream_nse1(gw_num_obs_chan,2))
          allocate(stream_nnse(gw_num_obs_chan,2))
          allocate(stream_kg(gw_num_obs_chan,2))
          allocate(stream_pbias(gw_num_obs_chan,2))
          stream_nse = -9.99
          stream_nse1 = -9.99
          stream_nnse = -9.99
          stream_kg = -9.99
          stream_pbias = -9.99
          if(gw_flow_cal.eq.0) then
            do i=1,gw_num_obs_chan
              !calculate mean of observed discharges; also count the number of months with streamflow data    
              sum_obs_flow = 0.
              sum_sim_flow = 0.
              month_count = 0
              do j=1,num_months
                if(obs_flow_vals(i,j).gt.0) then
                  sum_obs_flow = sum_obs_flow + obs_flow_vals(i,j)
                  sum_sim_flow = sum_sim_flow + sim_flow_vals(i,j)
                  month_count = month_count + 1
                endif
              enddo
              if(month_count.gt.30) then !only calculate if number of data points > 30
                mean_obs_flow = sum_obs_flow / month_count
                mean_sim_flow = sum_sim_flow / month_count
                !calculate NSE, NNSE, and NSE1
                sum_resi_nse = 0.
                sum_diff_nse = 0.
                sum_resi_nse1 = 0.
                sum_diff_nse1 = 0.
                do j=1,num_months
                  if(obs_flow_vals(i,j).gt.0) then
                    sum_resi_nse = sum_resi_nse + (obs_flow_vals(i,j) - sim_flow_vals(i,j))**2
                    sum_diff_nse = sum_diff_nse + (obs_flow_vals(i,j) - mean_obs_flow)**2
                    sum_resi_nse1 = sum_resi_nse1 + abs(obs_flow_vals(i,j) - sim_flow_vals(i,j))
                    sum_diff_nse1 = sum_diff_nse1 + abs(obs_flow_vals(i,j) - mean_obs_flow)
                  endif
                enddo
                if(sum_diff_nse.ne.0) then
                  stream_nse(i,1) = 1 - (sum_resi_nse / sum_diff_nse)
                  stream_nse1(i,1) = 1 - (sum_resi_nse1 / sum_diff_nse1)
                  stream_nnse(i,1) = 1 / (2 - stream_nse(i,1))
              endif
                !calculate PBIAS
                sum_num = 0.
                sum_den = 0.
                do j=1,num_months
                  if(obs_flow_vals(i,j).gt.0) then
                    sum_num = sum_num + (obs_flow_vals(i,j) - sim_flow_vals(i,j))
                    sum_den = sum_den + obs_flow_vals(i,j)
                  endif
                enddo
                if(sum_den.ne.0) then
                  stream_pbias(i,1) = (sum_num*100.) / sum_den
                endif
                !calculate KLING-GUPTA efficiency
                sum_num = 0.
                sum_den1 = 0.
                sum_den2 = 0.
                do j=1,num_months
                  if(obs_flow_vals(i,j).gt.0) then
                    sum_num = sum_num + ((obs_flow_vals(i,j)-mean_obs_flow)*(sim_flow_vals(i,j)-mean_sim_flow))
                    sum_den1 = sum_den1 + (obs_flow_vals(i,j) - mean_obs_flow)**2
                    sum_den2 = sum_den2 + (sim_flow_vals(i,j) - mean_sim_flow)**2
                  endif
                enddo
                if((sum_den1+sum_den2).ne.0) then
                  cc = sum_num / (sqrt(sum_den1)*sqrt(sum_den2))
                endif
                sum_sim_sd = 0.
                sum_obs_sd = 0.
                do j=1,num_months
                  if(obs_flow_vals(i,j).gt.0) then
                    sum_sim_sd = sum_sim_sd + (sim_flow_vals(i,j)-mean_sim_flow)**2
                    sum_obs_sd = sum_obs_sd + (obs_flow_vals(i,j)-mean_obs_flow)**2
                  endif
                enddo
                sim_sd = sqrt(sum_sim_sd/month_count)
                obs_sd = sqrt(sum_obs_sd/month_count)
                stream_kg(i,1) = 1 - sqrt((cc-1)**2 + ((sim_sd/obs_sd)-1)**2 + ((mean_sim_flow/mean_obs_flow)-1)**2)
              endif
            enddo
            !write out results
            write(out_strobs,*) 'Statistics for specified channels'
            write(out_strobs,*) 'Channel, count, NSE, NSE1, NNSE, PBIAS, KGE'
            do i=1,gw_num_obs_chan
              write(out_strobs,114) obs_channels(i),month_count,stream_nse(i,1),stream_nse1(i,1),stream_nnse(i,1),  &
                 stream_pbias(i,1),stream_kg(i,1)
            enddo
          else !calculate statistics for calibration and testing periods
            num_months_calb = 12 * gw_flow_cal_yrs
            num_months_test = num_months - num_months_calb
            do i=1,gw_num_obs_chan
              !calibration period
              !calculate mean of observed discharges    
              sum_obs_flow = 0.
              sum_sim_flow = 0.
              month_count_calb = 0
              do j=1,num_months_calb
                if(obs_flow_vals(i,j).gt.0) then
                  sum_obs_flow = sum_obs_flow + obs_flow_vals(i,j)
                  sum_sim_flow = sum_sim_flow + sim_flow_vals(i,j)
                  month_count_calb = month_count_calb + 1
                endif
              enddo
              if(month_count_calb.gt.30) then
                mean_obs_flow = sum_obs_flow / month_count_calb
                mean_sim_flow = sum_sim_flow / month_count_calb
                !calculate NSE, NNSE, and NSE1
                sum_resi_nse = 0.
                sum_diff_nse = 0.
                sum_resi_nse1 = 0.
                sum_diff_nse1 = 0.
                do j=1,num_months_calb
                  if(obs_flow_vals(i,j).gt.0) then
                    sum_resi_nse = sum_resi_nse + (obs_flow_vals(i,j) - sim_flow_vals(i,j))**2
                    sum_diff_nse = sum_diff_nse + (obs_flow_vals(i,j) - mean_obs_flow)**2
                    sum_resi_nse1 = sum_resi_nse1 + abs(obs_flow_vals(i,j) - sim_flow_vals(i,j))
                    sum_diff_nse1 = sum_diff_nse1 + abs(obs_flow_vals(i,j) - mean_obs_flow)
                  endif
                enddo
                if(sum_diff_nse.ne.0) then
                  stream_nse(i,1) = 1 - (sum_resi_nse / sum_diff_nse)
                  stream_nse1(i,1) = 1 - (sum_resi_nse1 / sum_diff_nse1)
                  stream_nnse(i,1) = 1 / (2 - stream_nse(i,1))
                endif
              endif
              !calculate PBIAS
              sum_num = 0.
              sum_den = 0.
              do j=1,num_months
                if(obs_flow_vals(i,j).gt.0) then
                  sum_num = sum_num + (obs_flow_vals(i,j) - sim_flow_vals(i,j))
                  sum_den = sum_den + obs_flow_vals(i,j)
                endif
              enddo
              if(sum_den.ne.0) then
                stream_pbias(i,1) = (sum_num*100.) / sum_den
              endif
              !calculate KLING-GUPTA efficiency
              sum_num = 0.
              sum_den1 = 0.
              sum_den2 = 0.
              do j=1,num_months
                if(obs_flow_vals(i,j).gt.0) then
                  sum_num = sum_num + ((obs_flow_vals(i,j)-mean_obs_flow)*(sim_flow_vals(i,j)-mean_sim_flow))
                  sum_den1 = sum_den1 + (obs_flow_vals(i,j) - mean_obs_flow)**2
                  sum_den2 = sum_den2 + (sim_flow_vals(i,j) - mean_sim_flow)**2
                endif
              enddo
              if((sum_den1+sum_den2).ne.0) then
                cc = sum_num / (sqrt(sum_den1)*sqrt(sum_den2))
              endif
              sum_sim_sd = 0.
              sum_obs_sd = 0.
              do j=1,num_months
                if(obs_flow_vals(i,j).gt.0) then
                  sum_sim_sd = sum_sim_sd + (sim_flow_vals(i,j)-mean_sim_flow)**2
                  sum_obs_sd = sum_obs_sd + (obs_flow_vals(i,j)-mean_obs_flow)**2
                endif
              enddo
              sim_sd = sqrt(sum_sim_sd/month_count_calb)
              obs_sd = sqrt(sum_obs_sd/month_count_calb)
              stream_kg(i,1) = 1 - sqrt((cc-1)**2 + ((sim_sd/obs_sd)-1)**2 + ((mean_sim_flow/mean_obs_flow)-1)**2)
              !testing period
              !calculate mean of observed discharges    
              sum_obs_flow = 0.
              sum_sim_flow = 0.
              month_count_test = 0
              do j=1,num_months_test
                if(obs_flow_vals(i,j+num_months_calb).gt.0) then  
                  sum_obs_flow = sum_obs_flow + obs_flow_vals(i,j+num_months_calb)
                  sum_sim_flow = sum_sim_flow + sim_flow_vals(i,j+num_months_calb)
                  month_count_test = month_count_test + 1
                endif
              enddo
              if(month_count_test.gt.30) then
                mean_obs_flow = sum_obs_flow / month_count_test
                mean_sim_flow = sum_sim_flow / month_count_test
                !calculate NSE, NNSE, and NSE1
                sum_resi_nse = 0.
                sum_diff_nse = 0.
                sum_resi_nse1 = 0.
                sum_diff_nse1 = 0.
                do j=1,num_months_test
                  if(obs_flow_vals(i,j+num_months_calb).gt.0) then
                    sum_resi_nse = sum_resi_nse + (obs_flow_vals(i,j+num_months_calb) - sim_flow_vals(i,j+num_months_calb))**2
                    sum_diff_nse = sum_diff_nse + (obs_flow_vals(i,j+num_months_calb) - mean_obs_flow)**2
                    sum_resi_nse1 = sum_resi_nse1 + abs(obs_flow_vals(i,j+num_months_calb) - sim_flow_vals(i,j+num_months_calb))
                    sum_diff_nse1 = sum_diff_nse1 + abs(obs_flow_vals(i,j+num_months_calb) - mean_obs_flow)
                  endif
                enddo
                if(sum_diff_nse.ne.0) then
                  stream_nse(i,2) = 1 - (sum_resi_nse / sum_diff_nse)
                  stream_nse1(i,2) = 1 - (sum_resi_nse1 / sum_diff_nse1)
                  stream_nnse(i,2) = 1 / (2 - stream_nse(i,1))
                endif
                !calculate PBIAS
                sum_num = 0.
                sum_den = 0.
                do j=1,num_months
                  if(obs_flow_vals(i,j+num_months_calb).gt.0) then
                    sum_num = sum_num + (obs_flow_vals(i,j+num_months_calb) - sim_flow_vals(i,j+num_months_calb))
                    sum_den = sum_den + obs_flow_vals(i,j+num_months_calb)
                  endif
                enddo
                if(sum_den.ne.0) then
                  stream_pbias(i,2) = (sum_num*100.) / sum_den
                endif
                !calculate KLING-GUPTA efficiency
                sum_num = 0.
                sum_den1 = 0.
                sum_den2 = 0.
                do j=1,num_months
                  if(obs_flow_vals(i,j+num_months_calb).gt.0) then
                    sum_num = sum_num + ((obs_flow_vals(i,j+num_months_calb)-mean_obs_flow)*  &
                       (sim_flow_vals(i,j+num_months_calb)-mean_sim_flow))
                    sum_den1 = sum_den1 + (obs_flow_vals(i,j+num_months_calb) - mean_obs_flow)**2
                    sum_den2 = sum_den2 + (sim_flow_vals(i,j+num_months_calb) - mean_sim_flow)**2
                  endif
                enddo
                if((sum_den1+sum_den2).ne.0) then
                  cc = sum_num / (sqrt(sum_den1)*sqrt(sum_den2))
                endif
                sum_sim_sd = 0.
                sum_obs_sd = 0.
                do j=1,num_months
                  if(obs_flow_vals(i,j+num_months_calb).gt.0) then
                    sum_sim_sd = sum_sim_sd + (sim_flow_vals(i,j+num_months_calb)-mean_sim_flow)**2
                    sum_obs_sd = sum_obs_sd + (obs_flow_vals(i,j+num_months_calb)-mean_obs_flow)**2
                  endif
                enddo
                sim_sd = sqrt(sum_sim_sd/month_count_test)
                obs_sd = sqrt(sum_obs_sd/month_count_test)
                stream_kg(i,2) = 1 - sqrt((cc-1)**2 + ((sim_sd/obs_sd)-1)**2 + ((mean_sim_flow/mean_obs_flow)-1)**2)
              endif
            enddo
            !write out results
            write(out_strobs,*) 'Statistics for specified channels (calib/testing)'
            write(out_strobs,*) 'Channel, count, NSE, NSE1, NNSE, PBIAS, KGE'
            do i=1,gw_num_obs_chan
              write(out_strobs,114) obs_channels(i),month_count_calb,stream_nse(i,1),stream_nse1(i,1),stream_nnse(i,1),  &
                 stream_pbias(i,1),stream_kg(i,1)
              write(out_strobs,114) obs_channels(i),month_count_test,stream_nse(i,2),stream_nse1(i,2),stream_nnse(i,2),  &
                 stream_pbias(i,2),stream_kg(i,2)
            enddo         
          endif
          write(out_strobs,*)
          write(out_strobs,*) 'Monthly values for each specified channel'
          do i=1,gw_num_obs_chan
            write(out_strobs,*) 'Channel:',obs_channels(i)
            write(out_strobs,*) 'Month     Observed     Simulated'
            do j=1,num_months
              write(out_strobs,113) j,obs_flow_vals(i,j),sim_flow_vals(i,j) 
            enddo
            write(out_strobs,*)
          enddo
          endif
        endif    

        !write out average annual groundwater budgets for each huc12 catchment
        if(nat_model == 1) then
          do n=1,sp_ob%outlet !loop through the huc12 catchments
            write(out_huc12wb,112) huc12(n),(gw_huc12_wb(i,n),i=1,15)
          enddo
        endif
        
        !if soft calibration, prepare for next simulation 
        sim_month = 1
        if(stream_obs == 1) then
          deallocate(stream_nse)
          deallocate(stream_nse1)
          deallocate(stream_nnse)
          deallocate(stream_kg)
          deallocate(stream_pbias)
        endif
        
      endif !end of simulation check

      
      
      !10. zero out flux arrays for next day
      !flow
      do i=1,ncell
        gw_ss(i)%rech = 0.
        gw_ss(i)%gwet = 0.
        gw_ss(i)%gwsw = 0.
        gw_ss(i)%swgw = 0.
        gw_ss(i)%satx = 0.
        gw_ss(i)%soil = 0.
        gw_ss(i)%latl = 0.
        gw_ss(i)%bndr = 0.
        gw_ss(i)%ppag = 0.
        gw_ss(i)%ppdf = 0.
        gw_ss(i)%ppex = 0.
        gw_ss(i)%tile = 0.
        gw_ss(i)%resv = 0.
        gw_ss(i)%wetl = 0.
        gw_ss(i)%fpln = 0.
        gw_ss(i)%canl = 0.
        gw_ss(i)%totl = 0.
      enddo   
      satx_count = 0
      !solutes
      if (gw_solute_flag == 1) then
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
            gwsol_ss(i)%solute(s)%fpln = 0.
            gwsol_ss(i)%solute(s)%canl = 0.
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
            gwsol_ss(i)%solute(s)%fpln = 0.
            gwsol_ss(i)%solute(s)%canl = 0.
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
       
      
      
100   format(10000(f12.3))
101   format(10000(e12.3))
102   format(i8,i8,f10.3,e16.7,e16.7,1000(e13.4))
103   format(i8,i8,i8,i8,i8,i8,i8,50(f15.3))
104   format(10000(f12.2))
105   format(i8,50(e13.4))
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
119   format(i8,i8,1000(f12.3))

!120   format(<out_cols>(f12.3))
!121   format(<out_cols>(e12.3))
120   format(f12.3)
121   format(e12.3)
125	  format(3x,i8,2x,i8,7x,f15.1,50(e13.4)) 
      

      return
      end subroutine gwflow_simulate
			