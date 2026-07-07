
      !This subroutine performs the following operations:
      !  1. Read in grid cell information
      !  2. Prepare the grid cells for the groundwater model
      !  3. Connect SWAT+ objects to grid cells
  
      !  Prepared by: Ryan Bailey, Colorado State University
      !  January-April 2020
  
      subroutine gwflow_read
      
      use gwflow_module
      use hydrograph_module
      use sd_channel_module
      
      implicit none
      
      character*10 b(3) 
      character(len=13) :: gwflow_hdr_day(19),gwflow_hdr_yr(13),gwflow_hdr_aa(13)
      character(len=13) :: gwflown_hdr_day(18),gwflown_hdr_yr(14),gwflown_hdr_aa(14)
      character(len=13) :: gwflowp_hdr_day(18),gwflowp_hdr_yr(14),gwflowp_hdr_aa(14)
      character(len=16) :: hydsep_hdr(10)
      integer  date_time(8)  
      integer  in_gw,in_hru_cell,in_cell_hru,in_huc_cell,hru,num_unique,cell,hru_count,hru_cell,nhru_connected
      integer  i,j,k,l,row,col,ob_num,num_hru,hru_id,dum,wt_init_type
      integer  nzones_aquK,nzones_aquSy,nzones_aqun,nzones_strK,nzones_strbed,K_zone,Sy_zone,n_zone,bed_zone
      integer  hru_read,hru_cell_row,hru_cell_col,cell_count,obs_cell_row,obs_cell_col
      integer  first_found,first_cell,last_cell,found
      integer  already_written,counter,riv_num,count,obs_cell_ID,cell_id,num_connected_hrus, &
               num_cells,cell_row,cell_col,pumpex_cell_ID
      integer  line_array(5000)
      integer  dum1,dum2,dum3,dum7,dum8
      integer  channel_id,num_hydsep
      real     dum4,dum5,dum6
      real(8)  huc12_id
      real     hru_area,poly_area,cell_area
      real     dist_x,dist_y,min_dist,distance,hru_avg_area,hru_avg_side
      real     wt_start,delay,thickness,bed_change
      real     remainder
      real     gw_cell_volume
      real, dimension (:), allocatable :: zones_aquK,zones_aquSy,zones_aqun,zones_strK,zones_strbed
      integer, dimension (:), allocatable :: chan_num_array,chan_gis_array
      

      !write message to screen
      call DATE_AND_TIME (b(1), b(2), b(3), date_time)
      write (*,111) "reading from groundwater file      ", date_time(5), date_time(6), date_time(7)
      
      !set flag to 1
      gwflow_flag = 1
      
      !integers for input and output files
      in_gw = 1230
      in_hru_cell = 1231
      in_cell_hru = 1232
      in_huc_cell = 1233
      out_gwobs = 1240
      out_gwconnect = 1241
      out_gwheads = 1242
      out_gwbal = 1243
      out_gwsw_chan = 1245
      out_gw_chan = 1246
      out_gw_rech = 1247
      out_gw_et = 1248
      out_gw_grid = 1249
      out_gw_satex = 1251
      out_gwsw = 1252
      out_lateral = 1253
      out_gw_etact = 1254
      out_gw_tile = 1255
      out_gwbal_yr = 1256
      out_gwbal_aa = 1257
      out_hyd_sep = 1258
      out_tile_cells = 1259
      out_gwconc = 1260
      out_gwbaln = 1261
      out_gwbaln_yr = 1262
      out_gwbaln_aa = 1263
      out_gwbalp = 1264
      out_gwbalp_yr = 1265
      out_gwbalp_aa = 1266
      out_gwtile_hru = 1267
      out_gwobs_ss = 1268
      out_gw_tran = 1270
      out_gw_lake = 1271
      out_gw_pumpag = 1272 
      out_gw_pumpex = 1273
      
      !number of HRUs in the simulation
      num_hru = sp_ob%hru
      
      !calculate watershed domain area (m2) (used for connecting HRUs to grid cells, and for water balance calculations)
      ob_num = sp_ob1%hru  !object number of first HRU
      watershed_area = 0.
      do i=1,num_hru
        watershed_area = watershed_area + (ob(i)%area_ha * 10000.) !m2
        ob_num = ob_num + 1
      enddo
      
      
      !read in gwflow module information from gwflow.input ------------------------------------------------------------------------------------------
      open(in_gw,file='gwflow.input')
      read(in_gw,*)
      read(in_gw,*)
      
      !basic information
      read(in_gw,*) cell_size
      read(in_gw,*) grid_nrow,grid_ncol
      read(in_gw,*) wt_init_type !water table initiation flag
      read(in_gw,*) wt_start !water table initiation
      read(in_gw,*) bc_type !boundary condition type
      read(in_gw,*) gw_transfer_flag !flag to simulate groundwater-soil interactions
      read(in_gw,*) gw_satexcess_flag !flag to simulate saturation excess routing
      read(in_gw,*) gw_et_flag
      read(in_gw,*) gw_tile_flag !flag to simulate groundwater-tile drain exchange	
      read(in_gw,*) gw_lake_flag !flag to simulate groundwater-lake exchange	
      read(in_gw,*) gw_pumpex_flag !flag to simulate specified groundwater pumping	
      read(in_gw,*) gw_rech_flag !flag to simulate recharge
      allocate(gw_delay(num_hru)) !groundwater delay
      allocate(gw_rech(num_hru))
      allocate(gw_rechn(num_hru))
      allocate(gw_rechp(num_hru))
      read(in_gw,*) delay
      gw_delay = Exp(-1./(delay + 1.e-6))
      gw_rech = 0.
      read(in_gw,*) gw_transport_flag
      if (gw_transport_flag == 1) then
        gw_rechn = 0.
        gw_rechp = 0.
      endif
      read(in_gw,*) gw_time_step !user-specified time step
      read(in_gw,*) gwflag_day,gwflag_yr,gwflag_aa
      
      !Aquifer and streambed parameter zones
      read(in_gw,*)
      read(in_gw,*)
      read(in_gw,*)
      read(in_gw,*) nzones_aquK
      allocate(zones_aquK(nzones_aquK))
      do i=1,nzones_aquK
        read(in_gw,*) dum,zones_aquK(i)
      enddo
      read(in_gw,*)
      read(in_gw,*) nzones_aquSy
      allocate(zones_aquSy(nzones_aquSy))
      do i=1,nzones_aquSy
        read(in_gw,*) dum,zones_aquSy(i)
      enddo
      read(in_gw,*)
      read(in_gw,*) nzones_aqun
      allocate(zones_aqun(nzones_aqun))
      do i=1,nzones_aqun
        read(in_gw,*) dum,zones_aqun(i)
      enddo
      read(in_gw,*)
      read(in_gw,*) nzones_strK
      allocate(zones_strK(nzones_strK))
      do i=1,nzones_strK
        read(in_gw,*) dum,zones_strK(i)
      enddo
      read(in_gw,*)
      read(in_gw,*) nzones_strbed
      allocate(zones_strbed(nzones_strbed))
      do i=1,nzones_strbed
        read(in_gw,*) dum,zones_strbed(i)
      enddo

      !grid cell information
      allocate(gw_cell_id(grid_nrow,grid_ncol))
      allocate(gw_cell_top(grid_nrow,grid_ncol))
      allocate(gw_cell_bot(grid_nrow,grid_ncol))
      allocate(gw_cell_K(grid_nrow,grid_ncol))
      allocate(gw_cell_Sy(grid_nrow,grid_ncol))
      allocate(gw_cell_por(grid_nrow,grid_ncol))
      allocate(gw_cell_inithead(grid_nrow,grid_ncol))
      allocate(gw_cell_status(grid_nrow,grid_ncol))
      allocate(gw_cell_exdp(grid_nrow,grid_ncol))
      allocate(gw_cell_et(grid_nrow,grid_ncol))
      allocate(gw_cell_tile(grid_nrow,grid_ncol))
      allocate(gw_cell_initcn(grid_nrow,grid_ncol))
      allocate(gw_cell_initcp(grid_nrow,grid_ncol))
      if(gw_lake_flag.eq.1) then	
        allocate(gw_cell_lake(grid_nrow,grid_ncol))	
        allocate(gw_cell_lake_bed(grid_nrow,grid_ncol))	
        allocate(gw_cell_lake_stage(grid_nrow,grid_ncol))	
      endif	
      gw_cell_inithead = 0.
      gw_cell_initcn = 0.
      gw_cell_initcp = 0.
      gw_cell_K = 0.
      gw_cell_Sy = 0.
      gw_cell_por = 0.
      read(in_gw,*)
      read(in_gw,*)
      read(in_gw,*)
      do i=1,grid_nrow
        do j=1,grid_ncol
          read(in_gw,*) gw_cell_id(i,j),gw_cell_status(i,j),gw_cell_top(i,j),thickness,K_zone,Sy_zone,n_zone, &
                        gw_cell_exdp(i,j),gw_cell_et(i,j),gw_cell_tile(i,j),gw_cell_initcn(i,j),gw_cell_initcp(i,j), &
                        gw_cell_lake(i,j),gw_cell_lake_bed(i,j),gw_cell_lake_stage(i,j)
          gw_cell_bot(i,j) = gw_cell_top(i,j) - thickness
          if(gw_cell_status(i,j).gt.0 .and. wt_init_type.lt.3) then !only proceed if initial head should be calculated
            if(wt_init_type.eq.1) then  
              gw_cell_inithead(i,j) = gw_cell_top(i,j) - wt_start
            elseif(wt_init_type.eq.2) then
              gw_cell_inithead(i,j) = gw_cell_bot(i,j) + ((gw_cell_top(i,j) - gw_cell_bot(i,j)) * wt_start)
            endif
          endif
          if(gw_cell_status(i,j).gt.0) then
            gw_cell_K(i,j) = zones_aquK(K_zone)
            gw_cell_Sy(i,j) = zones_aquSy(Sy_zone)
            gw_cell_por(i,j) = zones_aqun(n_zone)
          endif
        enddo
      enddo
      !if specified, read cell-by-cell initial head values
      if(wt_init_type.eq.3) then
        read(in_gw,*)
        do i=1,grid_nrow   
          read(in_gw,*) (gw_cell_inithead(i,j),j=1,grid_ncol)
        enddo
			endif
      !verify that initial head values are at or above the bedrock 
      do i=1,grid_nrow
        do j=1,grid_ncol
          if(gw_cell_status(i,j).gt.0) then
            if(gw_cell_inithead(i,j).lt.gw_cell_bot(i,j)) then
              gw_cell_inithead(i,j) = gw_cell_bot(i,j)
            endif
          endif
        enddo
			enddo
      !write out cell-by-cell values
      open(out_gw_grid,file='gwflow_grid_arrays')
      write(out_gw_grid,*) 'Cell Status: 0=inactive; 1=active; 2=boundary'
      do i=1,grid_nrow
        write(out_gw_grid,102) (gw_cell_status(i,j),j=1,grid_ncol)
      enddo
      write(out_gw_grid,*)
      write(out_gw_grid,*) 'Ground surface elevation (m)'
      do i=1,grid_nrow
        write(out_gw_grid,101) (gw_cell_top(i,j),j=1,grid_ncol)
      enddo
      write(out_gw_grid,*)
      write(out_gw_grid,*) 'Groundwater Initial Head (m)'
      do i=1,grid_nrow
        write(out_gw_grid,101) (gw_cell_inithead(i,j),j=1,grid_ncol)
      enddo
      write(out_gw_grid,*)
      write(out_gw_grid,*) 'Bedrock elevation (m)'
      do i=1,grid_nrow
        write(out_gw_grid,101) (gw_cell_bot(i,j),j=1,grid_ncol)
      enddo
      write(out_gw_grid,*)
      write(out_gw_grid,*) 'Hydraulic conductivity (m/day)'
      do i=1,grid_nrow
        write(out_gw_grid,101) (gw_cell_K(i,j),j=1,grid_ncol)
      enddo
      write(out_gw_grid,*)
      write(out_gw_grid,*) 'Specific yield'
      do i=1,grid_nrow
        write(out_gw_grid,101) (gw_cell_Sy(i,j),j=1,grid_ncol)
      enddo
      write(out_gw_grid,*)
      
      !count the number of active cells
      num_active = 0
      do i=1,grid_nrow
        do j=1,grid_ncol
          if(gw_cell_status(i,j).eq.1) then
            num_active = num_active + 1
          endif
        enddo
      enddo
      
      !groundwater output times (times at which groundwater head for each cell will be output)
      read(in_gw,*)
      read(in_gw,*) gw_num_output
      allocate(gw_output_yr(gw_num_output))
      allocate(gw_output_day(gw_num_output))
      do i=1,gw_num_output
        read(in_gw,*) gw_output_yr(i),gw_output_day(i)
      enddo
      
      !read in row and columns of cells for daily output (i.e. observation wells)
      read(in_gw,*)
      read(in_gw,*) gw_num_obs_wells
      allocate(gw_obs_cells_row(gw_num_obs_wells))
      allocate(gw_obs_cells_col(gw_num_obs_wells))
      do k=1,gw_num_obs_wells
        read(in_gw,*) obs_cell_ID
        remainder = mod(obs_cell_ID,grid_ncol)
        if(remainder.eq.0) then
          gw_obs_cells_row(k) = obs_cell_ID / grid_ncol !row
        else
          gw_obs_cells_row(k) = int(obs_cell_ID/grid_ncol) + 1 !row
        endif
        gw_obs_cells_col(k) = obs_cell_ID - ((gw_obs_cells_row(k)-1)*grid_ncol) !column
      enddo
      allocate(gw_obs_head(gw_num_obs_wells))
      gw_obs_head = 0.
      if(gw_transport_flag.eq.1) then
        allocate(gw_obs_cn(gw_num_obs_wells))
        allocate(gw_obs_cp(gw_num_obs_wells))
        gw_obs_cn = 0.
        gw_obs_cp = 0.
      endif
      open(out_gwobs,file='gwflow_state_obs')
      if(gw_transport_flag.eq.1) then
        write(out_gwobs,*) 'Daily head (m) and nutrient concentration (mg/L) values for observation wells'
      else
        write(out_gwobs,*) 'Daily head (m) values for observation wells'
      endif
      write(out_gwobs,*)
      !cell for detailed daily groundwater source/sink output
      read(in_gw,*)
      read(in_gw,*)
      read(in_gw,*) gw_cell_obs_ss_row,gw_cell_obs_ss_col
      open(out_gwobs_ss,file='gwflow_cell_ss')
      write(out_gwobs_ss,*) 'Daily sources and sinks for cell'
      write(out_gwobs_ss,*) 'Row:',gw_cell_obs_ss_row,'Column:',gw_cell_obs_ss_col
      write(out_gwobs_ss,*) 'head,volume_before,volume_after,rech,et,gw-->sw,sw-->gw,satex,lateral,pumpag,pumpex,tile,gw-->soil,lake'  
      allocate(gw_cell_obs_ss_vals(14))
      
      !river cell information (cells that are connected to SWAT+ chandeg channels)
      !(most of the river cell information was already read in the gwflow.riv file)
      num_rivcells = sp_ob%gwflow
      allocate(gw_riv_row(num_rivcells))
      allocate(gw_riv_col(num_rivcells))
      allocate(gw_riv_K(num_rivcells))
      allocate(gw_riv_thick(num_rivcells))
      do i=1,num_rivcells
        remainder = mod(gw_riv_id(i),grid_ncol)
        if(remainder.eq.0) then
          gw_riv_row(i) = gw_riv_id(i) / grid_ncol !row
        else
          gw_riv_row(i) = int(gw_riv_id(i)/grid_ncol) + 1 !row
        endif
        gw_riv_col(i) = gw_riv_id(i) - ((gw_riv_row(i)-1)*grid_ncol) !column
        gw_riv_K(i) = zones_strK(gw_riv_zone(i))
        gw_riv_thick(i) = zones_strbed(gw_riv_zone(i))
      enddo
      read(in_gw,*)
      read(in_gw,*) bed_change !vertical distance correction for streambed elevation
      
      !hydrograph separation - specify which channels should be output
      read(in_gw,*)
      read(in_gw,*) num_hydsep
      allocate(hydsep_flag(sp_ob%chandeg))
      do i=1,num_hydsep
        read(in_gw,*) channel_id
        hydsep_flag(channel_id) = 1
      enddo
      
      !tile drain cell information
      if(gw_tile_flag.eq.1) then
        allocate(gw_tilecell_rivcell(grid_nrow,grid_ncol))
        read(in_gw,*)
        read(in_gw,*) gw_tile_depth
        read(in_gw,*) gw_tile_drain_area
        read(in_gw,*) gw_tile_K
        read(in_gw,*) gw_tile_group_flag
        if(gw_tile_group_flag.eq.1) then
          read(in_gw,*) gw_tile_num_group
          allocate(gw_tile_groups(gw_tile_num_group,5000,2))
          do i=1,gw_tile_num_group
            read(in_gw,*)
            read(in_gw,*) num_tile_cells(i)
            do j=1,num_tile_cells(i)
              read(in_gw,*) cell_ID
              remainder = mod(cell_ID,grid_ncol)
              if(remainder.eq.0) then
                gw_tile_groups(i,j,1) = cell_ID / grid_ncol !row
              else
                gw_tile_groups(i,j,1) = int(cell_ID/grid_ncol) + 1 !row
              endif
              gw_tile_groups(i,j,2) = cell_ID - ((gw_tile_groups(i,j,1)-1)*grid_ncol) !column
            enddo
          enddo
          open(out_tile_cells,file='gwflow_tile_cell_groups')
          write(out_tile_cells,*) 'Total tile flow (m3/sec), nutrient conc. (g/m3) for cell groups'
        endif
        !find the closest river cell for tile drain flow
        do i=1,grid_nrow
          do j=1,grid_ncol
            if(gw_cell_status(i,j).eq.1 .and. gw_cell_tile(i,j).eq.1) then
              !find the closest river cell
              min_dist = 1000000.
              do k=1,num_rivcells
                dist_x = (gw_riv_row(k) - i)
                dist_y = (gw_riv_col(k) - j)
                distance = sqrt((dist_x)**2 + (dist_y)**2)
                if(distance.lt.min_dist) then
                  min_dist = distance
                  riv_num = k
                endif
              enddo
              gw_tilecell_rivcell(i,j) = riv_num  
            endif
          enddo
        enddo
        open(out_gw_tile,file='gwflow_flux_tile')
        if(gw_transport_flag.eq.1) then
          write(out_gw_tile,*) 'Annual tile flows (m3/day) and nutrient mass (kg/day)'
        else
          write(out_gw_tile,*) 'Annual tile flows (m3/day)'
        endif
        !open(out_gwtile_hru,file='gwflow_tile_hru')
        !write(out_gwtile_hru,*) 'Annual flow (m3), NO3 mass (kg), and P mass (kg) from tile drains'
        !write(out_gwtile_hru,*)
      endif
      allocate(gwflow_tile_sum(grid_nrow,grid_ncol))
      allocate(ss_tile_cell_total(grid_nrow,grid_ncol))
      allocate(gwflow_tilen_sum(grid_nrow,grid_ncol))
      allocate(ss_tilen_cell_total(grid_nrow,grid_ncol))
      allocate(gwflow_tilep_sum(grid_nrow,grid_ncol))
      allocate(ss_tilep_cell_total(grid_nrow,grid_ncol))
      gwflow_tile_sum = 0.
      ss_tile_cell_total = 0.
      gwflow_tilen_sum = 0.
      ss_tilen_cell_total = 0.
      gwflow_tilep_sum = 0.
      ss_tilep_cell_total = 0.
      
      !lake information
      if(gw_lake_flag.eq.1) then
        open(out_gw_lake,file='gwflow_flux_lake')
        if(gw_transport_flag.eq.1) then
          write(out_gw_lake,*) 'Annual groundwater-lake exchange (m3/day) and nutrient mass (kg/day)'
        else
          write(out_gw_lake,*) 'Annual groundwater-lake exchange (m3/day)'  
        endif
        read(in_gw,*)
        read(in_gw,*) lake_thick
        read(in_gw,*) lake_K
        read(in_gw,*) lake_no3
        read(in_gw,*) lake_p
      endif
      allocate(gwflow_lake_sum(grid_nrow,grid_ncol))
      allocate(ss_lake_cell_total(grid_nrow,grid_ncol))
      allocate(gwflow_laken_sum(grid_nrow,grid_ncol))
      allocate(ss_laken_cell_total(grid_nrow,grid_ncol))
      allocate(gwflow_lakep_sum(grid_nrow,grid_ncol))
      allocate(ss_lakep_cell_total(grid_nrow,grid_ncol))
      gwflow_lake_sum = 0.
      ss_lake_cell_total = 0.
      gwflow_laken_sum = 0.
      ss_laken_cell_total = 0.
      gwflow_lakep_sum = 0.
      ss_lakep_cell_total = 0.
      
      !pumping information (specified pumping, for groundwater that leaves the hydrologic system)
      if(gw_pumpex_flag.eq.1) then
        read(in_gw,*)
        read(in_gw,*)
        read(in_gw,*) gw_npumpex !number of pumps
        allocate(gw_pumpex_cell_row(gw_npumpex))
        allocate(gw_pumpex_cell_col(gw_npumpex))
        allocate(gw_pumpex_nperiods(gw_npumpex))
        allocate(gw_pumpex_dates(gw_npumpex,2,1000))
        allocate(gw_pumpex_rates(gw_npumpex,1000))
        gw_pumpex_nperiods = 0
        gw_pumpex_rates = 0.
        do i=1,gw_npumpex !read in the information for each pump
          read(in_gw,*) 
          read(in_gw,*) pumpex_cell_ID,gw_pumpex_nperiods(i)
          remainder = mod(pumpex_cell_ID,grid_ncol)
          if(remainder.eq.0) then
            gw_pumpex_cell_row(i) = pumpex_cell_ID / grid_ncol !row
          else
            gw_pumpex_cell_row(i) = int(pumpex_cell_ID/grid_ncol) + 1 !row
          endif
          gw_pumpex_cell_col(i) = pumpex_cell_ID - ((gw_pumpex_cell_row(i)-1)*grid_ncol) !column
          do j=1,gw_pumpex_nperiods(i)
            read(in_gw,*) gw_pumpex_dates(i,1,j),gw_pumpex_dates(i,2,j),gw_pumpex_rates(i,j)
          enddo
        enddo
      endif
      
      !chemical transport information
      if(gw_transport_flag.eq.1) then
        read(in_gw,*)
        read(in_gw,*) gw_lambda_no3
        read(in_gw,*) gw_long_disp
        read(in_gw,*) gw_reta_no3
        read(in_gw,*) gw_reta_p
        read(in_gw,*) num_ts_transport
      endif
      

      
      
      !allocate other arrays for gwflow module ------------------------------------------------------------------------------------------------------
      !arrays for groundwater head and saturated thickness
      allocate(gw_cell_head(grid_nrow,grid_ncol))
      allocate(head_new(grid_nrow,grid_ncol))
      allocate(head_old(grid_nrow,grid_ncol))
      allocate(gw_avail(grid_nrow,grid_ncol))
      allocate(gw_cell_satthick(grid_nrow,grid_ncol))
      !arrays for groundwater sources and sinks
      allocate(gw_cell_ss_rech(grid_nrow,grid_ncol))
      allocate(gw_cell_ss_et(grid_nrow,grid_ncol))
      allocate(gw_cell_ss_etact(grid_nrow,grid_ncol))
      allocate(gw_cell_ss_gwsw(grid_nrow,grid_ncol))
      allocate(gw_cell_ss_swgw(grid_nrow,grid_ncol))
      allocate(gw_cell_satex(grid_nrow,grid_ncol))
      allocate(gw_cell_tran(grid_nrow,grid_ncol))
      allocate(hru_gwtran(num_hru,20))
      allocate(gw_cell_ss_pumpag(grid_nrow,grid_ncol))
      allocate(gw_cell_ss_pumpex(grid_nrow,grid_ncol))
      allocate(gw_cell_ss_tile(grid_nrow,grid_ncol))
      allocate(gw_cell_ss_lake(grid_nrow,grid_ncol))
      allocate(gw_cell_ss(grid_nrow,grid_ncol))
      allocate(gw_cell_Q(grid_nrow,grid_ncol))
      allocate(gw_volume_before_cell(grid_nrow,grid_ncol))
      allocate(gw_volume_after_cell(grid_nrow,grid_ncol))
      allocate(gwflow_perc(num_hru))
      allocate(etremain(num_hru))
      allocate(etactual(num_hru))
      gw_cell_ss_rech = 0.
      gw_cell_ss_et = 0.
      gw_cell_ss_etact = 0.
      gw_cell_ss_gwsw = 0.
      gw_cell_ss_swgw = 0.
      gw_cell_satex = 0.
      gw_cell_tran = 0.
      hru_gwtran = 0.
      gw_cell_ss_pumpag = 0.
      gw_cell_ss_pumpex = 0.
      gw_cell_ss_tile = 0.
      gw_cell_ss_lake = 0.
      gw_cell_ss = 0.
      gw_cell_Q = 0.
      gwflow_perc = 0.
      etremain = 0.
      etactual = 0.
      gw_volume_before_cell = 0.
      gw_volume_after_cell = 0.
      gw_cell_satthick = 0.
      gw_output_index = 1
      !allocate arrays for chemical transport
      allocate(Q_lateral(grid_nrow,grid_ncol,4))
      allocate(sat_west(grid_nrow,grid_ncol))
      allocate(sat_east(grid_nrow,grid_ncol))
      allocate(sat_north(grid_nrow,grid_ncol))
      allocate(sat_south(grid_nrow,grid_ncol))
      Q_lateral = 0.
      sat_west = 0.
      sat_east = 0.
      sat_north = 0.
      sat_south = 0.
      !no3
      if(gw_transport_flag.eq.1) then
      allocate(gw_cell_mn(grid_nrow,grid_ncol))
      allocate(gw_cell_cn(grid_nrow,grid_ncol))
      allocate(cn_new(grid_nrow,grid_ncol))
      allocate(gwflow_percn(num_hru))     
      allocate(gw_cell_ss_rechn(grid_nrow,grid_ncol))
      allocate(gw_cell_ss_gwswn(grid_nrow,grid_ncol))
      allocate(gw_cell_ss_swgwn(grid_nrow,grid_ncol))
      allocate(gw_cell_satexn(grid_nrow,grid_ncol))
      allocate(gw_cell_ss_pumpagn(grid_nrow,grid_ncol))
      allocate(gw_cell_ss_pumpexn(grid_nrow,grid_ncol))
      allocate(gw_cell_ss_tilen(grid_nrow,grid_ncol))
      allocate(gw_cell_trann(grid_nrow,grid_ncol))
      allocate(gw_cell_ss_laken(grid_nrow,grid_ncol))
      allocate(nmass_adv(grid_nrow,grid_ncol))
      allocate(nmass_dsp(grid_nrow,grid_ncol))
      allocate(nmass_rct(grid_nrow,grid_ncol))
      allocate(gw_cell_advn(grid_nrow,grid_ncol))
      allocate(gw_cell_dspn(grid_nrow,grid_ncol))
      allocate(gw_cell_rctn(grid_nrow,grid_ncol))
      allocate(gw_cell_ssn(grid_nrow,grid_ncol))
      allocate(gw_nmass_before_cell(grid_nrow,grid_ncol))
      allocate(gw_nmass_after_cell(grid_nrow,grid_ncol))
      allocate(hru_ntran(num_hru,20))
      allocate(hru_ptran(num_hru,20))
      gw_cell_ss_rechn = 0.
      gw_cell_ss_gwswn = 0.
      gw_cell_ss_swgwn = 0.
      gw_cell_satexn = 0.
      gw_cell_ss_pumpagn = 0.
      gw_cell_ss_pumpexn = 0.
      gw_cell_ss_tilen = 0.
      gw_cell_trann = 0.
      gw_cell_ss_laken = 0.
      nmass_adv = 0.
      nmass_dsp = 0.
      nmass_rct = 0.
      gw_cell_advn = 0
      gw_cell_dspn = 0.
      gw_cell_rctn = 0.
      gw_cell_ssn = 0.
      gw_nmass_before_cell = 0.
      gw_nmass_after_cell = 0.
      gwflow_percn = 0.
      hru_ntran = 0.
      hru_ptran = 0.
      !p
      allocate(gw_cell_mp(grid_nrow,grid_ncol))
      allocate(gw_cell_cp(grid_nrow,grid_ncol))
      allocate(cp_new(grid_nrow,grid_ncol))
      allocate(gwflow_percp(num_hru))     
      allocate(gw_cell_ss_rechp(grid_nrow,grid_ncol))
      allocate(gw_cell_ss_gwswp(grid_nrow,grid_ncol))
      allocate(gw_cell_ss_swgwp(grid_nrow,grid_ncol))
      allocate(gw_cell_satexp(grid_nrow,grid_ncol))
      allocate(gw_cell_ss_pumpagp(grid_nrow,grid_ncol))
      allocate(gw_cell_ss_pumpexp(grid_nrow,grid_ncol))
      allocate(gw_cell_ss_tilep(grid_nrow,grid_ncol))
      allocate(gw_cell_tranp(grid_nrow,grid_ncol))
      allocate(gw_cell_ss_lakep(grid_nrow,grid_ncol))
      allocate(pmass_adv(grid_nrow,grid_ncol))
      allocate(pmass_dsp(grid_nrow,grid_ncol))
      allocate(pmass_rct(grid_nrow,grid_ncol))
      allocate(gw_cell_advp(grid_nrow,grid_ncol))
      allocate(gw_cell_dspp(grid_nrow,grid_ncol))
      allocate(gw_cell_rctp(grid_nrow,grid_ncol))
      allocate(gw_cell_ssp(grid_nrow,grid_ncol))
      allocate(gw_pmass_before_cell(grid_nrow,grid_ncol))
      allocate(gw_pmass_after_cell(grid_nrow,grid_ncol))
      gw_cell_ss_rechp = 0.
      gw_cell_ss_gwswp = 0.
      gw_cell_ss_swgwp = 0.
      gw_cell_satexp = 0.
      gw_cell_ss_pumpagp = 0.
      gw_cell_ss_pumpexp = 0.
      gw_cell_ss_tilep = 0.
      gw_cell_tranp = 0.
      gw_cell_ss_lakep = 0.
      pmass_adv = 0.
      pmass_dsp = 0.
      pmass_rct = 0.
      gw_cell_advp = 0
      gw_cell_dspp = 0.
      gw_cell_rctp = 0.
      gw_cell_ssp = 0.
      gw_pmass_before_cell = 0.
      gw_pmass_after_cell = 0.
      gwflow_percp = 0.
      endif
      !hru
      allocate(tile_hru_yr(num_hru,3))
      tile_hru_yr = 0.
      

      
      !read in connection information between HRUs and grid cells  ----------------------------------------------------------------------------------
      
      !for normal gwflow applications, the Cell-HRU connection will be used, using the gwflow.cellhru file. However, for applications
      !with the national agroecosystem model, the Cell-HUC12 connection will be used. If the gwflow.huc12cell file is present in the folder,
      !then the national model approach will be used.
      inquire(file='gwflow.huc12cell',exist=nat_model)
      if(nat_model) then
		    !read in the HUC12 subwatersheds
        open(5100,file='out.key')
        allocate(huc12(sp_ob%outlet))
        read(5100,*)
        read(5100,*)
        do k=1,sp_ob%outlet
          read(5100,*) dum1,huc12(k)
        enddo
        !open up and read the HRUs listed for each HUC12 subwatershed
        open(5101,file='hru.con')
        allocate(huc12_hrus(sp_ob%outlet,5000))
        allocate(huc12_nhru(sp_ob%outlet))
        huc12_nhru = 0
        read(5101,*)
        read(5101,*)
        do k=1,sp_ob%outlet
          read(5101,*) dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8,huc12_id
          backspace(5101)
          hru_count = 0
          do while (huc12_id.eq.huc12(k))
            hru_count = hru_count + 1
            read(5101,*) hru_id
            huc12_hrus(k,hru_count) = hru_id
            huc12_nhru(k) = hru_count
            read(5101,*,end=15) dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8,huc12_id
            backspace(5101)
          enddo
15      enddo
			endif
      
      !HRU-cell connection
      open(in_hru_cell,file='gwflow.hrucell')
      read(in_hru_cell,*)
      read(in_hru_cell,*)
      read(in_hru_cell,*)
      !read in list of HRUs that are spatially connected to grid cells - these are cultivated fields within the NAM
      read(in_hru_cell,*) nhru_connected !number of HRUs spatially connected to grid cells
      allocate(hrus_connected(num_hru))
      hrus_connected = 0
      do i=1,nhru_connected
        read(in_hru_cell,*) hru_id
        hrus_connected(hru_id) = 1
      enddo
      !read in the HRU-cell connection information
      read(in_hru_cell,*)
      read(in_hru_cell,*)
      allocate(hru_num_cells(num_hru))
      allocate(hru_cells(num_hru,1500,2))
      allocate(hru_cells_fract(num_hru,1500))
      hru_num_cells = 0
      hru_cells = 0
      hru_cells_fract = 0.
      do k=1,num_hru
        if(hrus_connected(k).eq.1) then
        hru = k
        cell_count = 0
        do while (hru.eq.k)
          cell_count = cell_count + 1
          read(in_hru_cell,*) hru,hru_area,cell_ID,poly_area
          remainder = mod(cell_ID,grid_ncol)
          if(remainder.eq.0) then
            hru_cells(k,cell_count,1) = cell_ID / grid_ncol !row
          else
            hru_cells(k,cell_count,1) = int(cell_ID/grid_ncol) + 1 !row
          endif
          hru_cells(k,cell_count,2) = cell_ID - ((hru_cells(k,cell_count,1)-1)*grid_ncol) !column
          hru_cells_fract(k,cell_count) = poly_area / hru_area
          read(in_hru_cell,*,end=10) hru
          backspace(in_hru_cell)
        enddo
10      hru_num_cells(k) = cell_count
        endif
      enddo
      
      !for normal gwflow applications, the Cell-HRU connection will be used, using the gwflow.cellhru file. However, for applications
      !with the national agroecosystem model, the Cell-HUC12 connection will be used. If the gwflow.huc12cell file is present in the folder,
      !then the national model approach will be used.
      if(nat_model) then
        !read in the list of grid cells for each HUC12
        open(in_huc_cell,file='gwflow.huc12cell')
        read(in_huc_cell,*)
        read(in_huc_cell,*)
        read(in_huc_cell,*)
        allocate(huc12_cells(sp_ob%outlet,5000,2))
        allocate(huc12_ncell(sp_ob%outlet))
        huc12_ncell = 0
        do k=1,sp_ob%outlet
          read(in_huc_cell,*) huc12_id
          backspace(in_huc_cell)
          cell_count = 0
          do while (huc12_id.eq.huc12(k))
            cell_count = cell_count + 1
            read(in_huc_cell,*) huc12_id,cell_ID
            remainder = mod(cell_ID,grid_ncol)
            if(remainder.eq.0) then
              huc12_cells(k,cell_count,1) = cell_ID / grid_ncol !row
            else
              huc12_cells(k,cell_count,1) = int(cell_ID/grid_ncol) + 1 !row
            endif
            huc12_cells(k,cell_count,2) = cell_ID - ((huc12_cells(k,cell_count,1)-1)*grid_ncol) !column
            huc12_ncell(k) = cell_count
            read(in_huc_cell,*,end=20) huc12_id
            backspace(in_huc_cell)
          enddo
  20    enddo      
        allocate(cell_received(grid_nrow,grid_ncol))
        cell_received = 0  
      else !read in Cell-HRU connection information
      allocate(cell_num_hrus(grid_nrow,grid_ncol))
      allocate(cell_hrus(grid_nrow,grid_ncol,100))
      allocate(cell_hrus_fract(grid_nrow,grid_ncol,100))
      cell_num_hrus = 0
      cell_hrus = 0
      cell_hrus_fract = 0.
      open(in_cell_hru,file='gwflow.cellhru')
      read(in_cell_hru,*)
      read(in_cell_hru,*)
      read(in_cell_hru,*) num_unique !number of cells that intersect HRUs
      read(in_cell_hru,*)
      do k=1,num_unique
        read(in_cell_hru,*) hru_cell
        cell_ID = hru_cell
        backspace(in_cell_hru)
        hru_count = 0
        do while (cell_ID.eq.hru_cell)
          hru_count = hru_count + 1
          read(in_cell_hru,*) cell_ID,hru,cell_area,poly_area
          remainder = mod(cell_ID,grid_ncol)
          if(remainder.eq.0) then
            cell_row = cell_ID / grid_ncol !row
          else
            cell_row = int(cell_ID/grid_ncol) + 1 !row
          endif
          cell_col = cell_ID - ((cell_row-1)*grid_ncol) !column
          cell_hrus(cell_row,cell_col,hru_count) = hru
          cell_hrus_fract(cell_row,cell_col,hru_count) = poly_area / cell_area
          read(in_cell_hru,*,end=30) cell_ID
          backspace(in_cell_hru)
        enddo
30      cell_num_hrus(cell_row,cell_col) = hru_count
      enddo
      endif
      
      
      !initialize groundwater balance ---------------------------------------------------------------------------------------------------------------
      
      !open file to track daily groundwater water balance
      if(gwflag_day.eq.1) then
      open(out_gwbal,file='gwflow_balance_gw_day')
      write(out_gwbal,*) 'Groundwater watershed-wide fluxes for each day'
      write(out_gwbal,*)
      write(out_gwbal,*) 'watershed area (m2):',watershed_area
      write(out_gwbal,*)
      write(out_gwbal,*) 'Positive value: groundwater added to aquifer'
      write(out_gwbal,*) 'Negative value: groundwater removed from aquifer'
      write(out_gwbal,*)
      write(out_gwbal,*) 'ts:            days time step used for groundwater storage calculations'
      write(out_gwbal,*) 'vol_bef:       mm   total groundwater volume at the beginning of the day'
      write(out_gwbal,*) 'vol_aft:       mm   total groundwater volume at the end of the day'
      write(out_gwbal,*) 'rech:          mm   soil water added to groundwater'
      write(out_gwbal,*) 'gwet:          mm   groundwater removed by evapotranspiration'
      write(out_gwbal,*) 'gwsw:          mm   groundwater discharge to streams'
      write(out_gwbal,*) 'swgw:          mm   stream water seepage to groundwater'
      write(out_gwbal,*) 'satex:         mm   saturation excess flow (water table above ground)'
      write(out_gwbal,*) 'gwsoil:        mm   groundwater transferred to HRU soil profile'
      write(out_gwbal,*) 'bound:         mm   groundwater added/removed at watershed boundary'
      write(out_gwbal,*) 'pump_ag:       mm   groundwater pumped for irrigation'
      write(out_gwbal,*) 'pump_ex:       mm   groundwater pumping specified by user'
      write(out_gwbal,*) 'tile:          mm   groundwater removed via tile drains'
      write(out_gwbal,*) 'lake:          mm   groundwater exchanged with lakes'	
      write(out_gwbal,*) 'error:         --   water balance error for aquifer'
      write(out_gwbal,*) 'satfr:              fraction of cells that have water table at ground'
      write(out_gwbal,*) 'wtdep:         m    average depth to water table for watershed'
      write(out_gwbal,*)
      gwflow_hdr_day = (/ character(len=16) :: "  year","   day","ts","vol_bef","vol_aft","rech","gwet","gwsw","swgw","satex","gwsoil", &
                         "bound","pump_ag","pump_ex","tile","lake","error","satfr","wtdepth"/)
      write(out_gwbal,119) (gwflow_hdr_day(j),j=1,19)
      endif

      !open file to track yearly groundwater water balance
      if(gwflag_yr.eq.1) then 
      open(out_gwbal_yr,file='gwflow_balance_gw_yr')
      write(out_gwbal_yr,*) 'Groundwater watershed-wide fluxes for each year'
      write(out_gwbal_yr,*)
      write(out_gwbal_yr,*) 'watershed area (m2):',watershed_area
      write(out_gwbal_yr,*)
      write(out_gwbal_yr,*) 'Positive value: groundwater added to aquifer'
      write(out_gwbal_yr,*) 'Negative value: groundwater removed from aquifer'
      write(out_gwbal_yr,*)
      write(out_gwbal_yr,*) 'delvol:        mm   change in groundwater volume during the year'
      write(out_gwbal_yr,*) 'rech:          mm   soil water added to groundwater'
      write(out_gwbal_yr,*) 'gwet:          mm   groundwater removed by evapotranspiration'
      write(out_gwbal_yr,*) 'gwsw:          mm   groundwater discharge to streams'
      write(out_gwbal_yr,*) 'swgw:          mm   stream water seepage to groundwater'
      write(out_gwbal_yr,*) 'satex:         mm   saturation excess flow (water table above ground)'
      write(out_gwbal_yr,*) 'gwsoil:        mm   groundwater transferred to HRU soil profile'
      write(out_gwbal_yr,*) 'bound:         mm   groundwater added/removed at watershed boundary'
      write(out_gwbal_yr,*) 'pump_ag:       mm   groundwater pumped for irrigation'
      write(out_gwbal_yr,*) 'pump_ex:       mm   groundwater pumping specified by user'
      write(out_gwbal_yr,*) 'tile:          mm   groundwater removed via tile drains'
      write(out_gwbal_yr,*) 'lake:          mm   groundwater exchanged with lakes'
      write(out_gwbal_yr,*)
      gwflow_hdr_yr = (/ character(len=16) :: "  year","delvol","rech","gwet","gwsw","swgw","satex","gwsoil","bound","pump_ag","pump_ex","tile","lake"/)
      write(out_gwbal_yr,120) (gwflow_hdr_yr(j),j=1,13)
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
      ss_lake_yr = 0.
      endif
      
      !open file to write out average annual groundwater water balance
      if(gwflag_aa.eq.1) then
      open(out_gwbal_aa,file='gwflow_balance_gw_aa')
      write(out_gwbal_aa,*) 'Average annual groundwater watershed-wide fluxes'
      write(out_gwbal_aa,*)
      write(out_gwbal_aa,*) 'watershed area (m2):',watershed_area
      write(out_gwbal_aa,*)
      write(out_gwbal_aa,*) 'Positive value: groundwater added to aquifer'
      write(out_gwbal_aa,*) 'Negative value: groundwater removed from aquifer'
      write(out_gwbal_aa,*)
      write(out_gwbal_aa,*) 'delvol:        mm   total change in groundwater volume across all years'
      write(out_gwbal_aa,*) 'rech:          mm   soil water added to groundwater'
      write(out_gwbal_aa,*) 'gwet:          mm   groundwater removed by evapotranspiration'
      write(out_gwbal_aa,*) 'gwsw:          mm   groundwater discharge to streams'
      write(out_gwbal_aa,*) 'swgw:          mm   stream water seepage to groundwater'
      write(out_gwbal_aa,*) 'satex:         mm   saturation excess flow (water table above ground)'
      write(out_gwbal_aa,*) 'gwsoil:        mm   groundwater transferred to HRU soil profile'
      write(out_gwbal_aa,*) 'bound:         mm   groundwater added/removed at watershed boundary'
      write(out_gwbal_aa,*) 'pump_ag:       mm   groundwater pumped for irrigation'
      write(out_gwbal_aa,*) 'pump_ex:       mm   groundwater pumping specified by user'
      write(out_gwbal_aa,*) 'tile:          mm   groundwater removed via tile drains'
      write(out_gwbal_aa,*) 'lake:          mm   groundwater exchanged with lakes'
      write(out_gwbal_aa,*)
      gwflow_hdr_aa = (/ character(len=16) :: "  year","delvol","rech","gwet","gwsw","swgw","satex","gwsoil","bound","pump_ag","pump_ex","tile","lake"/)
      write(out_gwbal_aa,120) (gwflow_hdr_aa(j),j=1,13)
      vol_change_total = 0.
      ss_rech_total = 0.
      ss_et_total = 0.
      ss_gw_total = 0.
      ss_sw_total = 0.
      ss_satex_total = 0.
      ss_tran_total = 0.
      ss_Q_total = 0.
      ss_pumpag_total = 0.
      ss_pumpex_total = 0.
      ss_tile_total = 0.
      ss_lake_total = 0.
      endif
      
      !initialize nutrient mass balance ---------------------------------------------------------------------------------------------------------------
      
      if(gw_transport_flag.eq.1) then
        !open file to track daily nutrient mass balance
        if(gwflag_day.eq.1) then
        open(out_gwbaln,file='gwflow_balance_no3_day')
        write(out_gwbaln,*) 'Groundwater watershed-wide NO3 loads for each day'
        write(out_gwbaln,*)
        write(out_gwbaln,*) 'Positive value: NO3 mass added to aquifer'
        write(out_gwbaln,*) 'Negative value: NO3 mass removed from aquifer'
        write(out_gwbaln,*)
        write(out_gwbaln,*) 'ts:            days time step used for groundwater nutrient calculations'
        write(out_gwbaln,*) 'massbef:       kg   total groundwater NO3 mass at the beginning of the day'
        write(out_gwbaln,*) 'massaft:       kg   total groundwater NO3 mass at the end of the day'
        write(out_gwbaln,*) 'rech:          kg   NO3 mass in recharge water'
        write(out_gwbaln,*) 'gwsw:          kg   NO3 mass loaded to streams'
        write(out_gwbaln,*) 'swgw:          kg   NO3 mass loaded from streams'
        write(out_gwbaln,*) 'satex:         kg   NO3 mass loaded to streams by saturation excess flow'
        write(out_gwbaln,*) 'gwsoil:        kg   NO3 mass loaded to HRU soil profiles'
        write(out_gwbaln,*) 'adv:           kg   NO3 mass transported by advection'
        write(out_gwbaln,*) 'dsp:           kg   NO3 mass transported by dispersion'
        write(out_gwbaln,*) 'rct:           kg   NO3 mass removed by denitrification'
        write(out_gwbaln,*) 'pump_ag:       kg   NO3 mass removed by groundwater pumping for irrigation'
        write(out_gwbaln,*) 'pump_ex:       kg   NO3 mass removed by groundwater pumping specified by user'
        write(out_gwbaln,*) 'tile:          kg   NO3 mass removed by tile drains'
        write(out_gwbaln,*) 'lake:          kg   NO3 mass loaded to/from lakes'
        write(out_gwbaln,*) 'error:         --   mass balance error for aquifer'
        write(out_gwbaln,*)
        gwflown_hdr_day = (/ character(len=16) :: "  year","   day","ts","massbef","massaft","rech","gwsw","swgw","satex","gwsoil","adv","dsp","rct", &
                            "pump_ag","pump_ex","tile","lake","error"/)
        write(out_gwbaln,119) (gwflown_hdr_day(j),j=1,18)
        open(out_gwbalp,file='gwflow_balance_p_day')
        write(out_gwbalp,*) 'Groundwater watershed-wide P loads for each day'
        write(out_gwbalp,*)
        write(out_gwbalp,*) 'Positive value: P mass added to aquifer'
        write(out_gwbalp,*) 'Negative value: P mass removed from aquifer'
        write(out_gwbalp,*)
        write(out_gwbalp,*) 'ts:            days time step used for groundwater nutrient calculations'
        write(out_gwbalp,*) 'massbef:       kg   total groundwater P mass at the beginning of the day'
        write(out_gwbalp,*) 'massaft:       kg   total groundwater P mass at the end of the day'
        write(out_gwbalp,*) 'rech:          kg   P mass in recharge water'
        write(out_gwbalp,*) 'gwsw:          kg   P mass loaded to streams'
        write(out_gwbalp,*) 'swgw:          kg   P mass loaded from streams'
        write(out_gwbalp,*) 'satex:         kg   P mass loaded to streams by saturation excess flow'
        write(out_gwbalp,*) 'gwsoil:        kg   P mass loaded to HRU soil profiles'
        write(out_gwbalp,*) 'adv:           kg   P mass transported by advection'
        write(out_gwbalp,*) 'dsp:           kg   P mass transported by dispersion'
        write(out_gwbalp,*) 'rct:           kg   P mass removed by denitrification'
        write(out_gwbalp,*) 'pump_ag:       kg   P mass removed by groundwater pumping for irrigation'
        write(out_gwbalp,*) 'pump_ex:       kg   P mass removed by groundwater pumping specified by user'
        write(out_gwbalp,*) 'tile:          kg   P mass removed by tile drains'
        write(out_gwbalp,*) 'lake:          kg   P mass loaded to/from lakes'
        write(out_gwbalp,*) 'error:         --   mass balance error for aquifer'
        write(out_gwbalp,*)
        gwflowp_hdr_day = (/ character(len=16) :: "  year","   day","ts","massbef","massaft","rech","gwsw","swgw","satex","gwsoil","adv","dsp","rct", &
                            "pump_ag","pump_ex","tile","lake","error"/)
        write(out_gwbalp,119) (gwflowp_hdr_day(j),j=1,18)
        endif
        
        !open file to track yearly nutrient mass balance
        if(gwflag_yr.eq.1) then
        open(out_gwbaln_yr,file='gwflow_balance_no3_yr')
        write(out_gwbaln_yr,*) 'Groundwater watershed-wide NO3 loads for each year'
        write(out_gwbaln_yr,*)
        write(out_gwbaln_yr,*) 'Positive value: NO3 mass added to aquifer'
        write(out_gwbaln_yr,*) 'Negative value: NO3 mass removed from aquifer'
        write(out_gwbaln_yr,*)
        write(out_gwbaln_yr,*) 'delmass:       kg   change in groundwater NO3 mass during the year'
        write(out_gwbaln_yr,*) 'rech:          kg   NO3 mass in recharge water'
        write(out_gwbaln_yr,*) 'gwsw:          kg   NO3 mass loaded to streams'
        write(out_gwbaln_yr,*) 'swgw:          kg   NO3 mass loaded from streams'
        write(out_gwbaln_yr,*) 'satex:         kg   NO3 mass loaded to streams by saturation excess flow'
        write(out_gwbaln_yr,*) 'gwsoil:        kg   NO3 mass loaded to HRU soil profiles'
        write(out_gwbaln_yr,*) 'adv:           kg   NO3 mass transported by advection'
        write(out_gwbaln_yr,*) 'dsp:           kg   NO3 mass transported by dispersion'
        write(out_gwbaln_yr,*) 'rct:           kg   NO3 mass removed by denitrification'
        write(out_gwbaln_yr,*) 'pump_ag:       kg   NO3 mass removed by groundwater pumping for irrigation'
        write(out_gwbaln_yr,*) 'pump_ex:       kg   NO3 mass removed by groundwater pumping specified by user'
        write(out_gwbaln_yr,*) 'tile:          kg   NO3 mass removed by tile drains'
        write(out_gwbaln_yr,*) 'lake:          kg   NO3 mass loaded to/from lakes'
        write(out_gwbaln_yr,*)
        gwflown_hdr_yr = (/ character(len=16) :: "  year","delmass","rech","gwsw","swgw","satex","gwsoil","adv","dsp","rct","pump_ag","pump_ex","tile","lake"/)
        write(out_gwbaln_yr,120) (gwflown_hdr_yr(j),j=1,14)
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
        ss_laken_yr = 0.
        open(out_gwbalp_yr,file='gwflow_balance_p_yr')
        write(out_gwbalp_yr,*) 'Groundwater watershed-wide P loads for each year'
        write(out_gwbalp_yr,*)
        write(out_gwbalp_yr,*) 'Positive value: P mass added to aquifer'
        write(out_gwbalp_yr,*) 'Negative value: P mass removed from aquifer'
        write(out_gwbalp_yr,*)
        write(out_gwbalp_yr,*) 'delmass:       kg   change in groundwater P mass during the year'
        write(out_gwbalp_yr,*) 'rech:          kg   P mass in recharge water'
        write(out_gwbalp_yr,*) 'gwsw:          kg   P mass loaded to streams'
        write(out_gwbalp_yr,*) 'swgw:          kg   P mass loaded from streams'
        write(out_gwbalp_yr,*) 'satex:         kg   P mass loaded to streams by saturation excess flow'
        write(out_gwbalp_yr,*) 'gwsoil:        kg   P mass loaded to HRU soil profiles'
        write(out_gwbalp_yr,*) 'adv:           kg   P mass transported by advection'
        write(out_gwbalp_yr,*) 'dsp:           kg   P mass transported by dispersion'
        write(out_gwbalp_yr,*) 'rct:           kg   P mass removed by denitrification'
        write(out_gwbalp_yr,*) 'pump_ag:       kg   P mass removed by groundwater pumping for irrigation'
        write(out_gwbalp_yr,*) 'pump_ex:       kg   P mass removed by groundwater pumping specified by user'
        write(out_gwbalp_yr,*) 'tile:          kg   P mass removed by tile drains'
        write(out_gwbalp_yr,*) 'lake:          kg   P mass loaded to/from lakes'
        write(out_gwbalp_yr,*)
        gwflowp_hdr_yr = (/ character(len=16) :: "  year","delmass","rech","gwsw","swgw","satex","gwsoil","adv","dsp","rct","pump_ag","pump_ex","tile","lake"/)
        write(out_gwbalp_yr,120) (gwflowp_hdr_yr(j),j=1,14)
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
        ss_lakep_yr = 0.
        endif
        
        !open file to write out average annual nutrient mass balance
        if(gwflag_aa.eq.1) then
        open(out_gwbaln_aa,file='gwflow_balance_no3_aa')
        write(out_gwbaln_aa,*) 'Average annual groundwater watershed-wide NO3 loads'
        write(out_gwbaln_aa,*)
        write(out_gwbaln_aa,*) 'Positive value: NO3 mass added to aquifer'
        write(out_gwbaln_aa,*) 'Negative value: NO3 mass removed from aquifer'
        write(out_gwbaln_aa,*)
        write(out_gwbaln_aa,*) 'delmass:       kg   total change in groundwater NO3 mass across all years'
        write(out_gwbaln_aa,*) 'rech:          kg   NO3 mass in recharge water'
        write(out_gwbaln_aa,*) 'gwsw:          kg   NO3 mass loaded to streams'
        write(out_gwbaln_aa,*) 'swgw:          kg   NO3 mass loaded from streams'
        write(out_gwbaln_aa,*) 'satex:         kg   NO3 mass loaded to streams by saturation excess flow'
        write(out_gwbaln_aa,*) 'gwsoil:        kg   NO3 mass loaded to HRU soil profiles'
        write(out_gwbaln_aa,*) 'adv:           kg   NO3 mass transported by advection'
        write(out_gwbaln_aa,*) 'dsp:           kg   NO3 mass transported by dispersion'
        write(out_gwbaln_aa,*) 'rct:           kg   NO3 mass removed by denitrification'
        write(out_gwbaln_aa,*) 'pump_ag:       kg   NO3 mass removed by groundwater pumping for irrigation'
        write(out_gwbaln_aa,*) 'pump_ex:       kg   NO3 mass removed by groundwater pumping specified by user'
        write(out_gwbaln_aa,*) 'tile:          kg   NO3 mass removed by tile drains'
        write(out_gwbaln_aa,*) 'lake:          kg   NO3 mass loaded to/from lakes'
        write(out_gwbaln_aa,*)
        gwflown_hdr_aa = (/ character(len=16) :: "  year","delmass","rech","gwsw","swgw","satex","gwsoil","adv","dsp","rct","pump_ag","pump_ex","tile","lake"/)
        write(out_gwbaln_aa,120) (gwflown_hdr_aa(j),j=1,14)
        nmasschange_total = 0.
        ss_rechn_total = 0.
        ss_gwn_total = 0.
        ss_swn_total = 0.
        ss_satexn_total = 0.
        ss_advn_total = 0.
        ss_dspn_total = 0.
        ss_rctn_total = 0.
        ss_pumpagn_total = 0.
        ss_pumpexn_total = 0.
        ss_tilen_total  = 0.
        ss_laken_total  = 0.
        open(out_gwbalp_aa,file='gwflow_balance_p_aa')
        write(out_gwbalp_aa,*) 'Average annual groundwater watershed-wide P loads'
        write(out_gwbalp_aa,*)
        write(out_gwbalp_aa,*) 'Positive value: P mass added to aquifer'
        write(out_gwbalp_aa,*) 'Negative value: P mass removed from aquifer'
        write(out_gwbalp_aa,*)
        write(out_gwbalp_aa,*) 'delmass:       kg   total change in groundwater P mass across all years'
        write(out_gwbalp_aa,*) 'rech:          kg   P mass in recharge water'
        write(out_gwbalp_aa,*) 'gwsw:          kg   P mass loaded to streams'
        write(out_gwbalp_aa,*) 'swgw:          kg   P mass loaded from streams'
        write(out_gwbalp_aa,*) 'satex:         kg   P mass loaded to streams by saturation excess flow'
        write(out_gwbalp_aa,*) 'gwsoil:        kg   P mass loaded to HRU soil profiles'
        write(out_gwbalp_aa,*) 'adv:           kg   P mass transported by advection'
        write(out_gwbalp_aa,*) 'dsp:           kg   P mass transported by dispersion'
        write(out_gwbalp_aa,*) 'rct:           kg   P mass removed by denitrification'
        write(out_gwbalp_aa,*) 'pump_ag:       kg   P mass removed by groundwater pumping for irrigation'
        write(out_gwbalp_aa,*) 'pump_ex:       kg   P mass removed by groundwater pumping specified by user'
        write(out_gwbalp_aa,*) 'tile:          kg   P mass removed by tile drains'
        write(out_gwbalp_aa,*) 'lake:          kg   P mass loaded to/from lakes'
        write(out_gwbalp_aa,*)
        gwflowp_hdr_aa = (/ character(len=16) :: "  year","delmass","rech","gwsw","swgw","satex","gwsoil","adv","dsp","rct","pump_ag","pump_ex","tile","lake"/)
        write(out_gwbalp_aa,120) (gwflowp_hdr_aa(j),j=1,14)
        pmasschange_total = 0.
        ss_rechp_total = 0.
        ss_gwp_total = 0.
        ss_swp_total = 0.
        ss_satexp_total = 0.
        ss_advp_total = 0.
        ss_dspp_total = 0.
        ss_rctp_total = 0.
        ss_pumpagp_total = 0.
        ss_pumpexp_total = 0.
        ss_tilep_total  = 0.
        ss_lakep_total  = 0.
        endif
      endif
      
      
      !prepare files for writing groundwater balance terms -----------------------------------------------------------------------
      !recharge
      open(out_gw_rech,file='gwflow_flux_recharge')
      if(gw_transport_flag.eq.1) then
        write(out_gw_rech,*) 'Annual recharge flow (m3/day) and nutrient mass (kg/day)'
      else
        write(out_gw_rech,*) 'Annual recharge flow (m3/day)'
      endif
      allocate(gwflow_rech_sum(grid_nrow,grid_ncol))
      allocate(ss_rech_cell_total(grid_nrow,grid_ncol))
      gwflow_rech_sum = 0.
      ss_rech_cell_total = 0.
      if(gw_transport_flag.eq.1) then
        allocate(gwflow_rechn_sum(grid_nrow,grid_ncol))
        allocate(ss_rechn_cell_total(grid_nrow,grid_ncol))
        allocate(gwflow_rechp_sum(grid_nrow,grid_ncol))
        allocate(ss_rechp_cell_total(grid_nrow,grid_ncol))
        gwflow_rechn_sum = 0.
        ss_rechn_cell_total = 0.
        gwflow_rechp_sum = 0.
        ss_rechp_cell_total = 0.
      endif
      !gwet
      if(gw_et_flag.eq.1) then
        open(out_gw_et,file='gwflow_flux_gwet')
        write(out_gw_et,*) 'Annual groundwater ET rates (m3/day)'
        allocate(gwflow_et_sum(grid_nrow,grid_ncol))
        allocate(ss_et_cell_total(grid_nrow,grid_ncol))
        gwflow_et_sum = 0.
        ss_et_cell_total = 0.
      endif
      !gwsw
      open(out_gwsw,file='gwflow_flux_gwsw')
      if(gw_transport_flag.eq.1) then
        write(out_gwsw,*) 'Annual GW-SW Exchange flows (m3/day) and nutrient mass (kg/day)'
      else
        write(out_gwsw,*) 'Annual GW-SW Exchange flows (m3/day)'
      endif
      allocate(gwflow_gwsw_sum(grid_nrow,grid_ncol))
      allocate(ss_gwsw_cell_total(grid_nrow,grid_ncol))
      gwflow_gwsw_sum = 0.
      ss_gwsw_cell_total = 0.
      if(gw_transport_flag.eq.1) then
        allocate(gwflow_gwswn_sum(grid_nrow,grid_ncol))
        allocate(ss_gwswn_cell_total(grid_nrow,grid_ncol))
        allocate(gwflow_gwswp_sum(grid_nrow,grid_ncol))
        allocate(ss_gwswp_cell_total(grid_nrow,grid_ncol))
        gwflow_gwswn_sum = 0.
        ss_gwswn_cell_total = 0.
        gwflow_gwswp_sum = 0.
        ss_gwswp_cell_total = 0.
      endif
      !lateral flow
      open(out_lateral,file='gwflow_flux_lateral')
      write(out_lateral,*) 'Annual Lateral flows (m3/day)'
      allocate(gwflow_lateral_sum(grid_nrow,grid_ncol))
      allocate(ss_Q_cell_total(grid_nrow,grid_ncol))
      gwflow_lateral_sum = 0.
      ss_Q_cell_total = 0.
      !groundwater pumping (irrigation)
      open(out_gw_pumpag,file='gwflow_flux_pumping_ag')
      if(gw_transport_flag.eq.1) then
        write(out_gw_pumpag,*) 'Annual pumping rate (m3/day) and nutrient mass (kg/day) (irrigation)'
      else
        write(out_gw_pumpag,*) 'Annual pumping rate (m3/day) (irrigation)'
      endif
      allocate(gwflow_pumpag_sum(grid_nrow,grid_ncol))
      allocate(ss_pumpag_cell_total(grid_nrow,grid_ncol))
      gwflow_pumpag_sum = 0.
      ss_pumpag_cell_total = 0.
      if(gw_transport_flag.eq.1) then
        allocate(gwflow_pumpagn_sum(grid_nrow,grid_ncol))
        allocate(ss_pumpagn_cell_total(grid_nrow,grid_ncol))
        allocate(gwflow_pumpagp_sum(grid_nrow,grid_ncol))
        allocate(ss_pumpagp_cell_total(grid_nrow,grid_ncol))
        gwflow_pumpagn_sum = 0.
        ss_pumpagn_cell_total = 0.
        gwflow_pumpagp_sum = 0.
        ss_pumpagp_cell_total = 0.
      endif
      !groundwater pumping (specified by user)
      if(gw_pumpex_flag.eq.1) then
      open(out_gw_pumpex,file='gwflow_flux_pumping_ex')
      if(gw_transport_flag.eq.1) then
        write(out_gw_pumpex,*) 'Annual pumping rate (m3/day) and nutrient mass (kg/day) (specified)'
      else
        write(out_gw_pumpex,*) 'Annual pumping rate (m3/day) (specified)'
      endif
      allocate(gwflow_pumpex_sum(grid_nrow,grid_ncol))
      allocate(ss_pumpex_cell_total(grid_nrow,grid_ncol))
      gwflow_pumpex_sum = 0.
      ss_pumpex_cell_total = 0.
      if(gw_transport_flag.eq.1) then
        allocate(gwflow_pumpexn_sum(grid_nrow,grid_ncol))
        allocate(ss_pumpexn_cell_total(grid_nrow,grid_ncol))
        allocate(gwflow_pumpexp_sum(grid_nrow,grid_ncol))
        allocate(ss_pumpexp_cell_total(grid_nrow,grid_ncol))
        gwflow_pumpexn_sum = 0.
        ss_pumpexn_cell_total = 0.
        gwflow_pumpexp_sum = 0.
        ss_pumpexp_cell_total = 0.
      endif
      endif

      !prepare files for writing groundwater-surface water interaction ------------------------------------------------------------------------------ 
      
      !open files to store gw-sw exchange rates (for each channel)      
      !open(out_gwsw_chan,file='gwflow_gwsw_chanQ')
      !write(out_gwsw_chan,*) 'Information for gw/sw flow rate exchange: each chandeg channel'
      !write(out_gwsw_chan,*) 'Positive value = stream seepage to groundwater'
      !write(out_gwsw_chan,*) 'Negative value = groundwater discharge to stream'
      allocate(chan_num_array(sp_ob%chandeg))
      allocate(chan_gis_array(sp_ob%chandeg))
      allocate(chan_Q(sp_ob%chandeg))
      ob_num = sp_ob1%chandeg  !object number of first chandeg channel
      do i=1,sp_ob%chandeg
        chan_num_array(i) = ob(ob_num)%num
        chan_gis_array(i) = ob(ob_num)%gis_id
        ob_num = ob_num + 1
      enddo
      !write(out_gwsw_chan,103) (chan_num_array(i),i=1,sp_ob%chandeg)
      !write(out_gwsw_chan,103) (chan_gis_array(i),i=1,sp_ob%chandeg)
      
      !open file to write out channel flow rates (m3/s)
      open(out_gw_chan,file='gwflow_chanQ')
      write(out_gw_chan,103) (chan_num_array(i),i=1,sp_ob%chandeg)
      write(out_gw_chan,103) (chan_gis_array(i),i=1,sp_ob%chandeg)
      allocate(channel_flow(sp_ob%chandeg))
      
      
      
      
      !prepare saturation excess routing ------------------------------------------------------------------------------------------------------------
      
      !for each grid cell: find the nearest River Cell (when saturation excess flow occurs, the water is routed to the River Cell; 
      !the water is then added to the stream channel that is connected to the River Cell)
      if(gw_satexcess_flag.eq.1) then
      allocate(gw_cell_rivcell(grid_nrow,grid_ncol))
      do i=1,grid_nrow
        do j=1,grid_ncol
          if(gw_cell_status(i,j).gt.0) then
          !loop through the River Cells
          min_dist = 1000000.
          do k=1,num_rivcells
            dist_x = (gw_riv_row(k) - i)
            dist_y = (gw_riv_col(k) - j)
            distance = sqrt((dist_x)**2 + (dist_y)**2)
            if(distance.lt.min_dist) then
              min_dist = distance
              riv_num = k
            endif
          enddo
          gw_cell_rivcell(i,j) = riv_num
          endif
        enddo
      enddo
      open(out_gw_satex,file='gwflow_flux_satex')
      if(gw_transport_flag.eq.1) then
        write(out_gw_satex,*) 'Annual saturation excess flows (m3/day) and nutrient mass (kg/day)'
      else
        write(out_gw_satex,*) 'Annual saturation excess flows (m3/day)'
      endif
      allocate(gwflow_satex_sum(grid_nrow,grid_ncol))
      allocate(ss_satex_cell_total(grid_nrow,grid_ncol))
      gwflow_satex_sum = 0.
      ss_satex_cell_total = 0.
      if(gw_transport_flag.eq.1) then
        allocate(gwflow_satexn_sum(grid_nrow,grid_ncol))
        allocate(ss_satexn_cell_total(grid_nrow,grid_ncol))
        allocate(gwflow_satexp_sum(grid_nrow,grid_ncol))
        allocate(ss_satexp_cell_total(grid_nrow,grid_ncol))
        gwflow_satexn_sum = 0.
        ss_satexn_cell_total = 0.
        gwflow_satexp_sum = 0.
        ss_satexp_cell_total = 0.
      endif
      endif
      
      
      
      !prepare groundwater --> soil transfers ------------------------------------------------------------------------------------------------------------
      
      !for each grid cell: find the nearest River Cell (when saturation excess flow occurs, the water is routed to the River Cell; 
      !the water is then added to the stream channel that is connected to the River Cell)
      open(out_gw_tran,file='gwflow_flux_gwsoil')
      if(gw_transport_flag.eq.1) then
        write(out_gw_tran,*) 'Annual groundwater-->soil transfers (m3/day) and nutrient mass (kg/day)'
      else
        write(out_gw_tran,*) 'Annual groundwater-->soil transfers (m3/day)'
      endif
      allocate(gwflow_tran_sum(grid_nrow,grid_ncol))
      allocate(ss_tran_cell_total(grid_nrow,grid_ncol))
      gwflow_tran_sum = 0.
      ss_tran_cell_total = 0.
      if(gw_transport_flag.eq.1) then
        allocate(gwflow_trann_sum(grid_nrow,grid_ncol))
        allocate(ss_trann_cell_total(grid_nrow,grid_ncol))
        gwflow_trann_sum = 0.
        ss_trann_cell_total = 0.
        allocate(gwflow_tranp_sum(grid_nrow,grid_ncol))
        allocate(ss_tranp_cell_total(grid_nrow,grid_ncol))
        gwflow_tranp_sum = 0.
        ss_tranp_cell_total = 0.
      endif
      
      
      !to start the simulation, set gw head and nutrient concentration values to the initial values -----------------------------------------------------------------------
      gw_cell_head = gw_cell_inithead
      do i=1,grid_nrow
        do j=1,grid_ncol
          if(gw_cell_status(i,j).gt.0) then
					  gw_cell_volume = cell_size * cell_size * (gw_cell_head(i,j)-gw_cell_bot(i,j)) * gw_cell_Sy(i,j) !m3 of groundwater
          endif
        enddo
      enddo
      if(gw_transport_flag.eq.1) then
        gw_cell_cn = gw_cell_initcn
        gw_cell_cp = gw_cell_initcp
        do i=1,grid_nrow
          do j=1,grid_ncol
            if(gw_cell_status(i,j).gt.0) then
              gw_cell_mn(i,j) = gw_cell_volume * gw_cell_cn(i,j) !m3 * g/m3 = g
              gw_cell_mp(i,j) = gw_cell_volume * gw_cell_cp(i,j) !m3 * g/m3 = g
            endif
          enddo
        enddo
      endif
      
      !write out initial head and nutrient concentration for each grid cell
      open(out_gwheads,file='gwflow_state_head')
      write(out_gwheads,*) 'Initial head values (m)'
      do i=1,grid_nrow
        write(out_gwheads,101) (gw_cell_head(i,j),j=1,grid_ncol)
      enddo
      write(out_gwheads,*)
      if(gw_transport_flag.eq.1) then
        open(out_gwconc,file='gwflow_state_conc')
        write(out_gwconc,*) 'Initial concentration values (mg/L)'
        write(out_gwconc,*) 'NO3'
        do i=1,grid_nrow
          write(out_gwconc,101) (gw_cell_cn(i,j),j=1,grid_ncol)
        enddo
        write(out_gwconc,*) 'P'
        do i=1,grid_nrow
          write(out_gwconc,101) (gw_cell_cp(i,j),j=1,grid_ncol)
        enddo
        write(out_gwconc,*)
      endif
      
      
      
      !prepare hydrograph separation array -------------------------------------------------------------------------------------------------------
      allocate(chan_hyd_sep(sp_ob%chandeg,6))
      chan_hyd_sep = 0.
      open(out_hyd_sep,file='gwflow_state_hydsep')
      write(out_hyd_sep,*) 'Hydrograph Separation (m3/sec) for each channel'
      write(out_hyd_sep,*)
      write(out_hyd_sep,*) 'chan_surf:     channel flow contributed from surface runoff'
      write(out_hyd_sep,*) 'chan_lat:      channel flow contributed from soil lateral flow'
      write(out_hyd_sep,*) 'chan_gwsw:     channel flow contributed from groundwater discharge'
      write(out_hyd_sep,*) 'chan_swgw:     channel flow seeped from channel to aquifer'
      write(out_hyd_sep,*) 'chan_satexgw:  channel flow contributed from groundwater saturation excess'
      write(out_hyd_sep,*) 'chan_satexsw:  channel flow contributed from saturation excess runoff' 
      write(out_hyd_sep,*) 'chan_tile:     channel flow contributed from tile drain flow' 
      write(out_hyd_sep,*)
      hydsep_hdr = (/ character(len=16) :: "  year","   day","channel","chan_surf","chan_lat","chan_gwsw","chan_swgw","chan_satexgw","chan_satexsw","chan_tile"/)
      write(out_hyd_sep,121) (hydsep_hdr(j),j=1,10)      
      
      

      return
      
100   format(i6,i6,10(f10.2))
101   format(1000(f12.4))
102   format(1000(i4))
103   format(10000(i8))
111   format(1x,a, 5x,"Time",2x,i2,":",i2,":",i2)
119   format(a8,a11,50(a13))
120   format(a8,50(a13))
121   format(50(a16))

      end subroutine gwflow_read
      
           
      