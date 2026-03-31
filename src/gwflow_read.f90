
      !This subroutine performs the following operations:
      !  1. Read in grid cell information
      !  2. Prepare the grid cells for the groundwater model
      !  3. Connect SWAT+ objects to grid cells

      !  Prepared by: Ryan Bailey, Colorado State University (beginning 2020)
      !  Adapted for gwFlowMerge: unified SS type, renamed variables, NAM/USGS removed

      subroutine gwflow_read

      use gwflow_module
      use hydrograph_module
      use sd_channel_module
      use maximum_data_module
      use hru_module, only : hru
      use reservoir_data_module, only : wet_dat
      use cs_data_module
      use constituent_mass_module, only : cs_db

      implicit none

      character*10 b(3),irrig_type
      character*4 aString
      !water balance and solute balance output file headers
      character(len=13) :: gwflow_hdr(19) = ""
      character(len=13) :: gwflow_hdr_day(26) = ""
      character(len=13) :: gwflow_hdr_mon(21) = ""
      character(len=13) :: gwflow_hdr_yr(20) = ""
      character(len=13) :: gwflow_hdr_aa(20) = ""
      character(len=13) :: gwflow_hdr_day_grp(25) = ""
      character(len=13) :: gwflow_hdr_canal(10) = ""
      character(len=13) :: gwflow_hdr_canal_sol(11) = ""
      character(len=13) :: gwflow_hdr_pond(12) = ""
      character(len=13) :: gwflow_hdr_pond_sol(10) = ""
      character(len=13) :: sol_hdr_day(25) = ""
      character(len=13) :: sol_hdr_mo(22) = ""
      character(len=13) :: sol_hdr_yr(21) = ""
      character(len=13) :: sol_hdr_aa(21) = ""
      character(len=13) :: heat_hdr_day(24) = ""
      character(len=13) :: heat_hdr_yr(22) = ""
      character(len=13) :: heat_hdr_aa(22) = ""
      character(len=16) :: hydsep_hdr(10) = ""
      !general variables
      character(len=13) :: header = ""
      character(len=30) :: read_type = ""
      character*100 file_name(50)
      character(len=13) :: cs_names(20) = ""
      character(len=13) :: name = ""
      logical  i_exist
      logical  i_exist2
      integer :: date_time(8) = 0
      integer :: i = 0
      integer :: j = 0
      integer :: k = 0
      integer :: m = 0
      integer :: n = 0
      integer :: s = 0
      integer :: isalt = 0
      integer :: count = 0
      integer :: cell_num = 0
      integer :: sol_index = 0
      integer :: div = 0
      integer :: channel = 0
      integer :: chan_cell = 0
      integer :: ob_num = 0
      integer :: dum_id = 0
      integer :: active_cell = 0
      real :: cell_size = 0.
      real :: x_coord = 0.
      real :: y_coord = 0.
      integer :: num_conn = 0
      real :: sum = 0.
      real :: dist_x = 0.
      real :: dist_y = 0.
      real :: min_dist = 0.
      real :: distance = 0.
      real :: gw_cell_volume = 0.
      !input file numbers
      integer :: in_gw = 0
      integer :: in_wtdepth = 0
      integer :: in_hru_cell = 0
      integer :: in_cell_hru = 0
      integer :: in_res_cell = 0
      integer :: in_canal_cell = 0
      integer :: in_gw_minl = 0
      !aquifer and streambed properties
      integer :: K_zone = 0
      integer :: Sy_zone = 0
      integer :: nzones_aquK = 0
      integer :: nzones_aquSy = 0
      integer :: nzones_strK = 0
      integer :: nzones_strbed = 0
      real, dimension (:), allocatable :: zones_aquK
      real, dimension (:), allocatable :: zones_aquSy
      real, dimension (:), allocatable :: zones_strK
      real, dimension (:), allocatable :: zones_strbed
      real, dimension (:), allocatable :: zones_Kt
      !water table depth for initial head
      integer :: nzones_wt = 0
      real, dimension (:), allocatable :: zones_wt
      !external pumping information
      integer :: pumpex_cell = 0
      !tile drain information
      real :: tile_depth_val = 0.
      real :: tile_drain_area_val = 0.
      real :: tile_K_val = 0.
      !reservoir information
      integer :: res_cell = 0
      integer :: res_id = 0
      real :: res_stage = 0.
      !canal information
      integer :: canal_out(5000) = 0
      integer :: canal_div(5000) = 0
      integer :: day_beg = 0
      integer :: day_end = 0
      integer :: canal = 0
      real :: thick = 0.
      real :: depth = 0.
      real :: width = 0.
      real :: bed_K = 0.
      real :: length = 0.
      real :: frc_ret = 0
      real :: stage = 0.
      real :: fld_ro = 0.
      real :: spk_ro = 0.
      real :: drp_ro = 0.
      !recharge pond information
      integer :: month_days(12) = 0
      integer :: yr_start = 0
      integer :: mo_start = 0
      integer :: dy_start = 0
      integer :: num_yr = 0
      integer :: num_dy = 0
      !HRU-cell, LSU-cell linkage
      integer :: num_unique = 0
      integer :: cell = 0
      integer :: hru_count = 0
      integer :: hru_cell = 0
      integer :: nhru_connected = 0
      integer :: num_hru = 0
      real :: hru_area = 0.
      integer :: hru_id = 0
      integer :: lsu = 0
      integer :: nlsu = 0
      integer :: nlsu_connected = 0
      integer :: lsu_id = 0
      integer :: cell_count = 0
      real :: poly_area = 0.
      real :: cell_area = 0.
      real :: lsu_area = 0.
      !time-varying boundary conditions
      integer :: bc_type_int = 0
      integer :: in_tvh = 0
      integer :: cell_id = 0
      !groundwater transit time
      integer :: in_transit_time = 0
      integer :: cell_transit = 0
      !observation well cell IDs (local, before USG conversion)
      integer, dimension (:), allocatable :: gw_obs_cells_init
      integer :: obs_cell_id = 0
      !dummy variables for reading
      integer :: dum = 0
      integer :: dum1 = 0
      integer :: dum2 = 0
      integer :: dum3 = 0
      integer :: dum7 = 0
      integer :: dum8 = 0
      real :: dum4 = 0.
      real :: dum5 = 0.
      real :: dum6 = 0.
      real :: single_value = 0.
      integer :: max_num = 0
      integer :: wb_cell = 0
      real :: group_area = 0.


      !write message to screen
      call DATE_AND_TIME (b(1), b(2), b(3), date_time)
      write (*,111) "reading from groundwater file      ", date_time(5), date_time(6), date_time(7)

      !write message to record file
      write(out_gw,*)
      write(out_gw,*) 'reading from file gwflow.input...'

      !integers for input files
      in_gw = 1230
      in_hru_cell = 1231
      in_cell_hru = 1232
      in_res_cell = 1236
      in_canal_cell = 1237
      in_hru_pump_obs = 1238
      in_lsu_cell = 1229
      in_gw_minl = 1220
      in_transit_time = 1235
      in_tvh = 1420

      !number of HRUs in the simulation
      num_hru = sp_ob%hru


      !read in gwflow module information from gwflow.input -----------------------------------------------
      open(in_gw,file='gwflow.input')
      read(in_gw,*) header
      read(in_gw,*) header

      !basic information
      write(out_gw,*) '     reading basic information...'
      read(in_gw,*) grid_type                       !structured or unstructured
      if(grid_type == "structured") then
        read(in_gw,*) cell_size                     !size (m) of each grid cell
        read(in_gw,*) grid_nrow,grid_ncol           !number of rows and columns in the gwflow grid
      elseif(grid_type == "unstructured") then
        read(in_gw,*) ncell                         !number of gwflow cells
      endif
      read(in_gw,*) bc_type_int                     !boundary condition type
      read(in_gw,*) conn_type                       !connection type (HRU or LSU)
      read(in_gw,*) gw_soil_flag                    !flag to simulate groundwater-soil interactions
      read(in_gw,*) gw_satx_flag                    !flag to simulate saturation excess routing
      read(in_gw,*) gw_pumpex_flag                  !flag to simulate specified groundwater pumping
      read(in_gw,*) gw_tile_flag                    !flag to simulate tile drainage outflow
      read(in_gw,*) gw_res_flag                     !flag to simulate groundwater-reservoir exchange
      read(in_gw,*) gw_wet_flag                     !flag to simulate groundwater-wetland exchange
      read(in_gw,*) gw_fp_flag                      !flag to simulate groundwater-floodplain exchange
      read(in_gw,*) gw_canal_flag                   !flag to simulate canal seepage to groundwater
      read(in_gw,*) gw_solute_flag                  !flag to simulate solute transport in groundwater
      read(in_gw,*) gw_time_step                    !user-specified time step
      read(in_gw,*) gwflag_day,gwflag_mon,gwflag_yr,gwflag_aa  !flags for writing balance files
      read(in_gw,*) out_cols                        !number of columns in output files

      !map bc type to all cells
      if(grid_type == "structured") then
        allocate(bc_type_array(grid_nrow*grid_ncol))
        count = 1
        do i=1,grid_nrow
          do j=1,grid_ncol
            bc_type_array(count) = bc_type_int
            count = count + 1
          enddo
        enddo
      elseif(grid_type == "unstructured") then
        allocate(bc_type_array(ncell))
        do i=1,ncell
          bc_type_array(i) = bc_type_int
        enddo
      endif

      !check connections (HRU-cell or LSU-cell) -----------------------------------------------------------------------
      write(out_gw,*) '     checking for connection (HRU, LSU) files...'
      if(conn_type == 1) then !HRU-cell
        inquire(file='gwflow.hrucell',exist=i_exist)
        if(i_exist) then
          hru_cells_link = 1
          lsu_cells_link = 0
          write(out_gw,*) '          found gwflow.hrucell: proceed'
        else
          hru_cells_link = 0
          inquire(file='gwflow.lsucell',exist=i_exist) !try LSU-cell connection instead
          if(i_exist) then
            lsu_cells_link = 1
            gw_soil_flag = 0 !gw-->soil transfer can occur only for HRU-cell connection
            gw_wet_flag = 0 !wetland transfer can occur only for HRU-cell connection
            write(out_gw,*) '          gwflow.hrucell not found: using gwflow.lsucell'
            write(out_gw,*) '          gwflow.lsucell: gw-->soil transfer not simulated'
            write(out_gw,*) '          gwflow.lsucell: gw-->wetland transfer not simulated'
          endif
        endif
      elseif(conn_type == 2) then !LSU-cell
        inquire(file='gwflow.lsucell',exist=i_exist)
        if(i_exist) then
          lsu_cells_link = 1
          hru_cells_link = 0
          gw_soil_flag = 0 !gw-->soil transfer cannot occur if LSU-cell linkage is active
          gw_wet_flag = 0 !wetland transfer can occur only for HRU-cell connection
          write(out_gw,*) '          found gwflow.lsucell: proceed'
          write(out_gw,*) '          gwflow.lsucell: gw-->soil transfer not simulated'
          write(out_gw,*) '          gwflow.lsucell: gw-->wetland transfer not simulated'
        else
          lsu_cells_link = 0
          inquire(file='gwflow.hrucell',exist=i_exist) !try HRU-cell connection instead
          if(i_exist) then
            hru_cells_link = 1
            write(out_gw,*) '          gwflow.lsucell not found: using gwflow.hrucell'
          endif
        endif
      else !cannot find connection file; write out message and stop
        write(out_gw,*) '          neither gwflow.hrucell or gwflow.lsucell found; stop simulation'
        stop
      endif

      !aquifer and streambed parameters -------------------------------------------------------------------------------
      write(out_gw,*) '     reading aquifer and streambed parameters...'

      !aquifer hydraulic conductivity (m/day)
      write(out_gw,*) '          reading aquifer hydraulic conductivity'
      read(in_gw,*) header
      read(in_gw,*) header
      read(in_gw,*) nzones_aquK
      allocate(zones_aquK(nzones_aquK), source = 0.)
      do i=1,nzones_aquK
        read(in_gw,*) dum,zones_aquK(i)
      enddo

      !aquifer specific yield
      write(out_gw,*) '          reading aquifer specific yield'
      read(in_gw,*) header
      read(in_gw,*) nzones_aquSy
      allocate(zones_aquSy(nzones_aquSy), source = 0.)
      do i=1,nzones_aquSy
        read(in_gw,*) dum,zones_aquSy(i)
      enddo

      !streambed hydraulic conductivity (m/day)
      write(out_gw,*) '          reading streambed hydraulic conductivity'
      read(in_gw,*) header
      read(in_gw,*) nzones_strK
      allocate(zones_strK(nzones_strK), source = 0.)
      do i=1,nzones_strK
        read(in_gw,*) dum,zones_strK(i)
      enddo

      !streambed thickness (m)
      write(out_gw,*) '          reading streambed thickness'
      read(in_gw,*) header
      read(in_gw,*) nzones_strbed
      allocate(zones_strbed(nzones_strbed), source = 0.)
      do i=1,nzones_strbed
        read(in_gw,*) dum,zones_strbed(i)
      enddo

      !grid cell information ------------------------------------------------------------------------------------------
      write(out_gw,*) '     reading grid cell information...'

      !if a structured grid, read in structured cell data -------------------------------------------------------------
      !(then, convert to usg arrays)
      if(grid_type == "structured") then
      read(in_gw,*) header

      !cell status
      read(in_gw,*) header
      allocate(grid_status(grid_nrow,grid_ncol), source = 0)
      read(in_gw,*) ((grid_status(i,j),j=1,grid_ncol),i=1,grid_nrow)

      !determine connections using cell status
      allocate(cell_id_usg(grid_nrow,grid_ncol), source = 0)
      allocate(cell_id_list(grid_nrow*grid_ncol), source = 0)
      cell_id_usg = 0
      ncell = 0
      count = 0
      !first: determine the new cell id of each gwflow cell
      do i=1,grid_nrow
        do j=1,grid_ncol
          count = count + 1
          if(grid_status(i,j) > 0) then
            ncell = ncell + 1
            cell_id_usg(i,j) = ncell
            cell_id_list(count) = ncell
          endif
        enddo
      enddo
      !second: allocate general array of cell attributes
      allocate(gw_state(ncell))
      !third: determine the cells connected to each cell
      allocate(cell_con(ncell))
      do i=1,grid_nrow
        do j=1,grid_ncol
          if(cell_id_usg(i,j) > 0) then
            !first: determine the number of cells connected to this cell
            num_conn = 0
            if(i > 1) then
              if(grid_status(i-1,j) > 0) num_conn = num_conn + 1
            endif
            if(j < grid_ncol) then
              if(grid_status(i,j+1) > 0) num_conn = num_conn + 1
            endif
            if(i < grid_nrow) then
              if(grid_status(i+1,j) > 0) num_conn = num_conn + 1
            endif
            if(j > 1) then
              if(grid_status(i,j-1) > 0) num_conn = num_conn + 1
            endif
            !second: store the id of each connected cell
            allocate(cell_con(cell_id_usg(i,j))%cell_id(num_conn))
            num_conn = 0
            if(i > 1) then
              if(grid_status(i-1,j) > 0) then
                num_conn = num_conn + 1
                cell_con(cell_id_usg(i,j))%cell_id(num_conn) = cell_id_usg(i-1,j)
              endif
            endif
            if(j < grid_ncol) then
              if(grid_status(i,j+1) > 0) then
                num_conn = num_conn + 1
                cell_con(cell_id_usg(i,j))%cell_id(num_conn) = cell_id_usg(i,j+1)
              endif
            endif
            if(i < grid_nrow) then
              if(grid_status(i+1,j) > 0) then
                num_conn = num_conn + 1
                cell_con(cell_id_usg(i,j))%cell_id(num_conn) = cell_id_usg(i+1,j)
              endif
            endif
            if(j > 1) then
              if(grid_status(i,j-1) > 0) then
                num_conn = num_conn + 1
                cell_con(cell_id_usg(i,j))%cell_id(num_conn) = cell_id_usg(i,j-1)
              endif
            endif
            gw_state(cell_id_usg(i,j))%ncon = num_conn
          endif
        enddo
      enddo
      !establish xy coordinates (at centroid) of each cell
      y_coord = (grid_nrow*cell_size) - (cell_size/2)
      do i=1,grid_nrow
        x_coord = cell_size / 2
        do j=1,grid_ncol
          if(cell_id_usg(i,j) > 0) then
            gw_state(cell_id_usg(i,j))%xcrd = x_coord
            gw_state(cell_id_usg(i,j))%ycrd = y_coord
          endif
          x_coord = x_coord + cell_size
        enddo
        y_coord = y_coord - cell_size
      enddo
      !store area (m2) of each cell
      do i=1,grid_nrow
        do j=1,grid_ncol
          if(cell_id_usg(i,j) > 0) then
            gw_state(cell_id_usg(i,j))%area = cell_size * cell_size
          endif
        enddo
      enddo
      !cell status
      do i=1,grid_nrow
        do j=1,grid_ncol
          if(cell_id_usg(i,j) > 0) then
            gw_state(cell_id_usg(i,j))%stat = grid_status(i,j)
          endif
        enddo
      enddo
      !read in other cell values; map to arrays
      allocate(grid_val(grid_nrow,grid_ncol), source = 0.)
      allocate(grid_int(grid_nrow,grid_ncol), source = 0)
      !ground surface elevation
      read(in_gw,*) header
      read(in_gw,*) ((grid_val(i,j),j=1,grid_ncol),i=1,grid_nrow)
      do i=1,grid_nrow
        do j=1,grid_ncol
          if(cell_id_usg(i,j) > 0) then
            gw_state(cell_id_usg(i,j))%elev = grid_val(i,j)
          endif
        enddo
      enddo
      !aquifer thickness
      read(in_gw,*) header
      read(in_gw,*) ((grid_val(i,j),j=1,grid_ncol),i=1,grid_nrow)
      do i=1,grid_nrow
        do j=1,grid_ncol
          if(cell_id_usg(i,j) > 0) then
            gw_state(cell_id_usg(i,j))%botm = gw_state(cell_id_usg(i,j))%elev - grid_val(i,j)
          endif
        enddo
      enddo
      !hydraulic conductivity zone
      read(in_gw,*) header
      read(in_gw,*) ((grid_int(i,j),j=1,grid_ncol),i=1,grid_nrow)
      do i=1,grid_nrow
        do j=1,grid_ncol
          if(grid_int(i,j) == 0) grid_int(i,j) = 1
          if(cell_id_usg(i,j) > 0) then
            gw_state(cell_id_usg(i,j))%hydc = zones_aquK(grid_int(i,j))
            gw_state(cell_id_usg(i,j))%zone = grid_int(i,j)
          endif
        enddo
      enddo
      !specific yield zone
      read(in_gw,*) header
      read(in_gw,*) ((grid_int(i,j),j=1,grid_ncol),i=1,grid_nrow)
      do i=1,grid_nrow
        do j=1,grid_ncol
          if(grid_int(i,j) == 0) grid_int(i,j) = 1
          if(cell_id_usg(i,j) > 0) then
            gw_state(cell_id_usg(i,j))%spyd = zones_aquSy(grid_int(i,j))
          endif
        enddo
      enddo
      !recharge delay
      allocate(delay(ncell), source = 0.)
      read(in_gw,*) header
      read(in_gw,*) ((grid_val(i,j),j=1,grid_ncol),i=1,grid_nrow)
      do i=1,grid_nrow
        do j=1,grid_ncol
          if(cell_id_usg(i,j) > 0) then
            delay(cell_id_usg(i,j)) = grid_val(i,j)
          endif
        enddo
      enddo
      !groundwater extinction depth
      read(in_gw,*) header
      read(in_gw,*) ((grid_val(i,j),j=1,grid_ncol),i=1,grid_nrow)
      do i=1,grid_nrow
        do j=1,grid_ncol
          if(cell_id_usg(i,j) > 0) then
            gw_state(cell_id_usg(i,j))%exdp = grid_val(i,j)
          endif
        enddo
      enddo
      !initial groundwater head
      read(in_gw,*) header
      read(in_gw,*) ((grid_val(i,j),j=1,grid_ncol),i=1,grid_nrow)
      do i=1,grid_nrow
        do j=1,grid_ncol
          if(cell_id_usg(i,j) > 0) then
            gw_state(cell_id_usg(i,j))%init = grid_val(i,j)
          endif
        enddo
      enddo
      !initial groundwater head - by zone (only if gwflow.wtdepth is present)
      inquire(file='gwflow.wtdepth',exist=i_exist)
      if(i_exist) then
        in_wtdepth = 1600
        open(in_wtdepth,file='gwflow.wtdepth')
        read(in_wtdepth,*) header
        read(in_wtdepth,*) header
        read(in_wtdepth,*) nzones_wt
        allocate(zones_wt(nzones_wt), source = 0.)
        do i=1,nzones_wt
          read(in_wtdepth,*) dum,zones_wt(i)
        enddo
        read(in_wtdepth,*) header
        read(in_wtdepth,*) ((grid_int(i,j),j=1,grid_ncol),i=1,grid_nrow)
        do i=1,grid_nrow
          do j=1,grid_ncol
            if(grid_int(i,j) == 0) grid_int(i,j) = 1
            if(cell_id_usg(i,j) > 0) then
              gw_state(cell_id_usg(i,j))%init = gw_state(cell_id_usg(i,j))%elev - zones_wt(grid_int(i,j))
            endif
          enddo
        enddo
        close(1600)
      endif

      !over-write parameters with specified cell-by-cell values (if provided) -----------------------------------------

      !read hydraulic conductivity (K) by array, if file is present
      inquire(file='gwflow.array_K',exist=i_exist)
      if(i_exist) then
        open(1600,file='gwflow.array_K')
        read(1600,*) ((grid_val(i,j),j=1,grid_ncol),i=1,grid_nrow)
        do i=1,grid_nrow
          do j=1,grid_ncol
            if(cell_id_usg(i,j) > 0) then
              gw_state(cell_id_usg(i,j))%hydc = grid_val(i,j)
            endif
          enddo
        enddo
        close(1600)
      endif

      !read specific yield (Sy) by array, if file is present
      inquire(file='gwflow.array_Sy',exist=i_exist)
      if(i_exist) then
        open(1600,file='gwflow.array_Sy')
        read(1600,*) ((grid_val(i,j),j=1,grid_ncol),i=1,grid_nrow)
        do i=1,grid_nrow
          do j=1,grid_ncol
            if(cell_id_usg(i,j) > 0) then
              gw_state(cell_id_usg(i,j))%spyd = grid_val(i,j)
            endif
          enddo
        enddo
        close(1600)
      endif

      !read recharge delay by array, if file is present
      inquire(file='gwflow.array_rdel',exist=i_exist)
      if(i_exist) then
        open(1600,file='gwflow.array_rdel')
        read(1600,*) ((grid_val(i,j),j=1,grid_ncol),i=1,grid_nrow)
        do i=1,grid_nrow
          do j=1,grid_ncol
            if(cell_id_usg(i,j) > 0) then
              delay(cell_id_usg(i,j)) = grid_val(i,j)
            endif
          enddo
        enddo
        close(1600)
      endif

      !read groundwater extinction depth by array, if file is present
      inquire(file='gwflow.array_exdp',exist=i_exist)
      if(i_exist) then
        open(1600,file='gwflow.array_exdp')
        read(1600,*) ((grid_val(i,j),j=1,grid_ncol),i=1,grid_nrow)
        do i=1,grid_nrow
          do j=1,grid_ncol
            if(cell_id_usg(i,j) > 0) then
              gw_state(cell_id_usg(i,j))%exdp = grid_val(i,j)
            endif
          enddo
        enddo
        close(1600)
      endif

      !read groundwater initial head by array, if file is present
      inquire(file='gwflow.array_hinit',exist=i_exist)
      if(i_exist) then
        open(1600,file='gwflow.array_hinit')
        read(1600,*) ((grid_val(i,j),j=1,grid_ncol),i=1,grid_nrow)
        do i=1,grid_nrow
          do j=1,grid_ncol
            if(cell_id_usg(i,j) > 0) then
              gw_state(cell_id_usg(i,j))%init = grid_val(i,j)
            endif
          enddo
        enddo
        close(1600)
      endif

      !read boundary condition for each boundary cell
      inquire(file='gwflow.array_bc',exist=i_exist)
      if(i_exist) then
        open(1600,file='gwflow.array_bc')
        read(1600,*)
        read(1600,*)
        read(1600,*)
        read(1600,*) ((grid_int(i,j),j=1,grid_ncol),i=1,grid_nrow)
        do i=1,grid_nrow
          do j=1,grid_ncol
            if(cell_id_usg(i,j) > 0) then
              bc_type_array(cell_id_usg(i,j)) = grid_int(i,j)
            endif
          enddo
        enddo
        close(1600)
      endif

      !if the grid type was specified as unstructured (usg) -----------------------------------------------------------
      elseif(grid_type == "unstructured") then

      !allocate general array of cell attributes
      allocate(gw_state(ncell))

      !read in cell information
      allocate(delay(ncell), source = 0.)
      allocate(cell_con(ncell))
      do i=1,13
        read(in_gw,*) header
      enddo
      do i=1,ncell
        !read line first time to determine the number of connected cells
        read(in_gw,*) dum1,gw_state(i)%stat,gw_state(i)%elev,gw_state(i)%thck,K_zone,Sy_zone,delay(i), &
                           gw_state(i)%exdp,gw_state(i)%init,gw_state(i)%xcrd, &
                           gw_state(i)%ycrd,gw_state(i)%area,gw_state(i)%ncon
        !read line second time to read in the list of connected cells
        backspace(in_gw)
        allocate(cell_con(i)%cell_id(gw_state(i)%ncon))
        read(in_gw,*) dum1,gw_state(i)%stat,gw_state(i)%elev,gw_state(i)%thck,K_zone,Sy_zone,delay(i), &
                           gw_state(i)%exdp,gw_state(i)%init,gw_state(i)%xcrd, &
                           gw_state(i)%ycrd,gw_state(i)%area,gw_state(i)%ncon, &
                          (cell_con(i)%cell_id(j),j=1,gw_state(i)%ncon)
        !additional calculations
        gw_state(i)%zone = K_zone
        gw_state(i)%botm = gw_state(i)%elev - gw_state(i)%thck
        gw_state(i)%hydc = zones_aquK(K_zone)
        gw_state(i)%spyd = zones_aquSy(Sy_zone)
        if(gw_state(i)%init < gw_state(i)%botm) then
          gw_state(i)%init = gw_state(i)%botm
        endif
      enddo
      endif !test for grid type


      !cell operations ------------------------------------------------------------------------------------------------
      !count the number of active cells
      write(out_gw,*) '     counting number of active cells'
      num_active = 0
      do i=1,ncell
        if(gw_state(i)%stat > 0) then
          num_active = num_active + 1
        endif
      enddo

      !calculate active area of the watershed, for the gwflow grid
      gwflow_area = 0.
      do i=1,ncell
        if(gw_state(i)%stat == 1) then
          gwflow_area = gwflow_area + gw_state(i)%area
        endif
      enddo

      !for boundary cells: store id of the closest active cell
      allocate(gw_bound_near(ncell), source = 0)
      allocate(gw_bound_dist(ncell), source = 0.)
      do i=1,ncell
        if(gw_state(i)%stat == 2) then
          min_dist = 1000000.
          do j=1,ncell
            if(gw_state(j)%stat == 1) then
              dist_x = gw_state(i)%xcrd - gw_state(j)%xcrd
              dist_y = gw_state(i)%ycrd - gw_state(j)%ycrd
              distance = sqrt((dist_x)**2 + (dist_y)**2)
              if(distance.lt.min_dist) then
                min_dist = distance
                active_cell = j
              endif
            endif
          enddo
          gw_bound_near(i) = active_cell
          gw_bound_dist(i) = min_dist
        endif
      enddo

      !groundwater output times (times at which groundwater head for each cell will be output) ------------------------
      write(out_gw,*) '     reading groundwater output times'
      read(in_gw,*)
      read(in_gw,*) gw_num_output
      allocate(gw_output_yr(gw_num_output), source = 0)
      allocate(gw_output_day(gw_num_output), source = 0)
      do i=1,gw_num_output
        read(in_gw,*) gw_output_yr(i),gw_output_day(i)
      enddo


      !read in cells for daily output (i.e. observation wells) --------------------------------------------------------
      write(out_gw,*) '     reading observation cells'
      read(in_gw,*)
      read(in_gw,*) gw_num_obs_wells
      allocate(gw_obs_cells_init(gw_num_obs_wells), source = 0)
      allocate(gw_obs_cells(gw_num_obs_wells), source = 0)
      !loop through the observation well locations
      do k=1,gw_num_obs_wells
        read(in_gw,*) gw_obs_cells_init(k)
      enddo
      allocate(gw_obs_head(gw_num_obs_wells), source = 0.)
      !open file for writing out daily head values
      open(out_gwobs,file='gwflow_cell_obs_head_day.txt')
      write(out_gwobs,*) 'Daily head (m) values for observation wells'
      write(out_gwobs,123) 'cell:',(gw_obs_cells_init(k),k=1,gw_num_obs_wells)
      write(out_gwobs,*)
      gw_output_index = 1
      !if structured grid, then convert cell ids
      if(grid_type == "structured") then
        do k=1,gw_num_obs_wells
          gw_obs_cells(k) = cell_id_list(gw_obs_cells_init(k))
        enddo
      else
        do k=1,gw_num_obs_wells
          gw_obs_cells(k) = gw_obs_cells_init(k)
        enddo
      endif

      !cell for detailed daily groundwater source/sink output (currently disabled) ------------------------------------
      read(in_gw,*) header
      read(in_gw,*) gw_cell_obs_ss


      !channel cell information (cells that are connected to SWAT+ chandeg channels) ----------------------------------
      !(channel cell information was already read: gwflow_chan_read subroutine)
      write(out_gw,*)
      write(out_gw,*) 'additional gwflow preparation...'
      write(out_gw,*)
      write(out_gw,*) '     processing channel-cell information'
      allocate(gw_chan_cell(sp_ob%gwflow))
      allocate(gw_chan_K(sp_ob%gwflow))
      allocate(gw_chan_thick(sp_ob%gwflow))
      do i=1,sp_ob%gwflow
        if(grid_type == "structured") then
          gw_chan_cell(i) = cell_id_list(gw_chan_id(i))
        elseif(grid_type == "unstructured") then
          gw_chan_cell(i) = gw_chan_id(i)
        endif
        gw_chan_K(i) = zones_strK(gw_chan_zone(i))
        gw_chan_thick(i) = zones_strbed(gw_chan_zone(i))
      enddo
      read(in_gw,*)
      read(in_gw,*) gw_bed_change !vertical distance correction for streambed elevation
      close(in_gw)

      !read channel bed conductivity by array, if file is present
      inquire(file='gwflow.array_strK',exist=i_exist)
      if(i_exist) then
        open(1600,file='gwflow.array_strK')
        do i=1,sp_ob%gwflow
          read(1600,*) gw_chan_K(i)
        enddo
        close(1600)
      endif

      !read channel bed thickness by array, if file is present
      inquire(file='gwflow.array_strthick',exist=i_exist)
      if(i_exist) then
        open(1600,file='gwflow.array_strthick')
        do i=1,sp_ob%gwflow
          read(1600,*) gw_chan_thick(i)
        enddo
        close(1600)
      endif


      !prepare source-sink arrays and options -------------------------------------------------------------------------
      write(out_gw,*) '     allocate and prepare source-sink arrays...'

      !allocate cell source/sink arrays
      allocate(gw_hyd_ss(ncell))
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
        gw_hyd_ss(i)%fpln = 0.
        gw_hyd_ss(i)%canl = 0.
        gw_hyd_ss(i)%pond = 0.
        gw_hyd_ss(i)%phyt = 0.
        gw_hyd_ss(i)%totl = 0.
      enddo

      !allocate grid source/sink arrays
      !annual
      allocate(gw_hyd_ss_yr(ncell))
      do i=1,ncell
        gw_hyd_ss_yr(i)%rech = 0.
        gw_hyd_ss_yr(i)%gwet = 0.
        gw_hyd_ss_yr(i)%gwsw = 0.
        gw_hyd_ss_yr(i)%swgw = 0.
        gw_hyd_ss_yr(i)%satx = 0.
        gw_hyd_ss_yr(i)%soil = 0.
        gw_hyd_ss_yr(i)%latl = 0.
        gw_hyd_ss_yr(i)%bndr = 0.
        gw_hyd_ss_yr(i)%ppag = 0.
        gw_hyd_ss_yr(i)%ppdf = 0.
        gw_hyd_ss_yr(i)%ppex = 0.
        gw_hyd_ss_yr(i)%tile = 0.
        gw_hyd_ss_yr(i)%resv = 0.
        gw_hyd_ss_yr(i)%wetl = 0.
        gw_hyd_ss_yr(i)%fpln = 0.
        gw_hyd_ss_yr(i)%canl = 0.
        gw_hyd_ss_yr(i)%pond = 0.
        gw_hyd_ss_yr(i)%phyt = 0.
      enddo
      !monthly
      allocate(gw_hyd_ss_mo(ncell))
      do i=1,ncell
        gw_hyd_ss_mo(i)%rech = 0.
        gw_hyd_ss_mo(i)%gwet = 0.
        gw_hyd_ss_mo(i)%gwsw = 0.
        gw_hyd_ss_mo(i)%swgw = 0.
        gw_hyd_ss_mo(i)%satx = 0.
        gw_hyd_ss_mo(i)%soil = 0.
        gw_hyd_ss_mo(i)%latl = 0.
        gw_hyd_ss_mo(i)%bndr = 0.
        gw_hyd_ss_mo(i)%ppag = 0.
        gw_hyd_ss_mo(i)%ppdf = 0.
        gw_hyd_ss_mo(i)%ppex = 0.
        gw_hyd_ss_mo(i)%tile = 0.
        gw_hyd_ss_mo(i)%resv = 0.
        gw_hyd_ss_mo(i)%wetl = 0.
        gw_hyd_ss_mo(i)%fpln = 0.
        gw_hyd_ss_mo(i)%canl = 0.
        gw_hyd_ss_mo(i)%pond = 0.
        gw_hyd_ss_mo(i)%phyt = 0.
      enddo

      !recharge
      write(out_gw,*) '          recharge from soil'
      !flux output file
      open(out_gw_rech,file='gwflow_cell_wb_rech_yr.txt')
      write(out_gw_rech,*) 'Annual recharge flow (m3/day)'
      open(out_gw_rech_mo,file='gwflow_cell_wb_rech_mon.txt')
      write(out_gw_rech_mo,*) 'Monthly recharge flow (m3/day)'
      allocate(gwflow_perc(sp_ob%hru))
      gwflow_perc = 0.

      !gwet
      write(out_gw,*) '          groundwater ET'
      !flux output file
      open(out_gw_gwet,file='gwflow_cell_wb_gwet_yr.txt')
      write(out_gw_gwet,*) 'Annual groundwater ET rates (m3/day)'
      open(out_gw_gwet_mo,file='gwflow_cell_wb_gwet_mon.txt')
      write(out_gw_gwet_mo,*) 'Monthly groundwater ET rates (m3/day)'
      allocate(etremain(sp_ob%hru))
      etremain = 0.

      !lateral flow within aquifer ----------------------------------------------------------------
      do i=1,ncell
        allocate(cell_con(i)%latl(gw_state(i)%ncon))
        allocate(cell_con(i)%sat(gw_state(i)%ncon))
        do j=1,gw_state(i)%ncon
          cell_con(i)%latl(j) = 0.
          cell_con(i)%sat(j) = 0.
        enddo
      enddo

      !groundwater-channel exchange ---------------------------------------------------------------
      write(out_gw,*) '          groundwater-channel exchange'
      !flux output file
      open(out_gw_gwsw,file='gwflow_cell_wb_gwsw_yr.txt')
      write(out_gw_gwsw,*) 'Annual groundwater-channel Exchange flows (m3/day)'
      open(out_gw_gwsw_mo,file='gwflow_cell_wb_gwsw_mon.txt')
      write(out_gw_gwsw_mo,*) 'Monthly groundwater-channel Exchange flows (m3/day)'
      !determine the number of cells that are linked to each channel
      allocate(gw_chan_info(sp_ob%chandeg))
      do i=1,sp_ob%gwflow
        channel = gw_chan_chan(i) !channel connected to cell
        gw_chan_info(channel)%ncon = gw_chan_info(channel)%ncon + 1
      enddo
      !allocate arrays holding the cell information
      do i=1,sp_ob%chandeg
        allocate(gw_chan_info(i)%cells(gw_chan_info(i)%ncon))
        allocate(gw_chan_info(i)%leng(gw_chan_info(i)%ncon))
        allocate(gw_chan_info(i)%elev(gw_chan_info(i)%ncon))
        allocate(gw_chan_info(i)%hydc(gw_chan_info(i)%ncon))
        allocate(gw_chan_info(i)%thck(gw_chan_info(i)%ncon))
        allocate(gw_chan_info(i)%dpzn(gw_chan_info(i)%ncon))
        gw_chan_info(i)%ncon = 0
      enddo
      !populate the array holding the cell numbers for each channel
      do i=1,sp_ob%gwflow
        channel = gw_chan_chan(i) !channel connected to cell
        gw_chan_info(channel)%ncon = gw_chan_info(channel)%ncon + 1
        gw_chan_info(channel)%cells(gw_chan_info(channel)%ncon) = gw_chan_cell(i)
        gw_chan_info(channel)%leng(gw_chan_info(channel)%ncon) = gw_chan_len(i)
        gw_chan_info(channel)%elev(gw_chan_info(channel)%ncon) = gw_chan_elev(i)
        gw_chan_info(channel)%hydc(gw_chan_info(channel)%ncon) = gw_chan_K(i)
        gw_chan_info(channel)%thck(gw_chan_info(channel)%ncon) = gw_chan_thick(i)
        if(gw_chan_dep_flag == 1) then !depth zone for daily channel depth
          gw_chan_info(channel)%dpzn(gw_chan_info(channel)%ncon) = gw_chan_dpzn(i)
        endif
      enddo

      !groundwater-->soil transfer ----------------------------------------------------------------
      if(gw_soil_flag == 1) then
        write(out_gw,*) '          groundwater-->soil transfer'
        !flux output file
        open(out_gw_soil,file='gwflow_cell_wb_soil_yr.txt')
        write(out_gw_soil,*) 'Annual groundwater-->soil transfers (m3/day)'
        open(out_gw_soil_mo,file='gwflow_cell_wb_soil_mon.txt')
        write(out_gw_soil_mo,*) 'Monthly groundwater-->soil transfers (m3/day)'
      endif

      !general method: find closest channel for each grid cell
      !(this is used for saturation excess flow, tile drains)
      allocate(cell_channel(ncell))
      cell_channel = 0
      do i=1,ncell
        if(gw_state(i)%stat > 0) then
          !find the closest channel cell
          min_dist = 1000000.
          chan_cell = 0
          do k=1,sp_ob%gwflow
            dist_x = gw_state(i)%xcrd - gw_state(gw_chan_cell(k))%xcrd !m
            dist_y = gw_state(i)%ycrd - gw_state(gw_chan_cell(k))%ycrd !m
            distance = sqrt((dist_x)**2 + (dist_y)**2)
            if(distance.lt.min_dist) then
              min_dist = distance
              chan_cell = k
            endif
          enddo
          channel = gw_chan_chan(chan_cell) !channel connected to channel cell
          cell_channel(i) = channel
        endif
      enddo

      !groundwater saturation excess flow ---------------------------------------------------------
      !for each grid cell: find the nearest channel cell
      !saturation excess water is then added to the stream channel that is connected to the channel cell
      if(gw_satx_flag == 1) then
        write(out_gw,*) '          groundwater saturation excess flow'
        allocate(gw_satx_info(sp_ob%chandeg))
        !count the cells connected to each channel
        do i=1,ncell
          if(gw_state(i)%stat > 0) then
            channel = cell_channel(i) !channel connected to cell
            gw_satx_info(channel)%ncon = gw_satx_info(channel)%ncon + 1
          endif
        enddo
        !allocate array for the set of cells connected to each channel
        do i=1,sp_ob%chandeg
          allocate(gw_satx_info(i)%cells(gw_satx_info(i)%ncon))
          gw_satx_info(i)%ncon = 0
        enddo
        !for each cell - tie to a channel
        do i=1,ncell
          if(gw_state(i)%stat > 0) then
            channel = cell_channel(i) !channel connected to cell
            gw_satx_info(channel)%ncon = gw_satx_info(channel)%ncon + 1
            gw_satx_info(channel)%cells(gw_satx_info(channel)%ncon) = i
          endif
        enddo
        satx_count = 0
        !flux output file
        open(out_gw_satx,file='gwflow_cell_wb_satx_yr.txt')
        write(out_gw_satx,*) 'Annual saturation excess flows (m3/day)'
        open(out_gw_satx_mo,file='gwflow_cell_wb_satx_mon.txt')
        write(out_gw_satx_mo,*) 'Monthly saturation excess flows (m3/day)'
      endif

      !groundwater pumping (irrigation) -----------------------------------------------------------
      write(out_gw,*) '          groundwater pumping for irrigation'
      !flux output file
      open(out_gw_ppag,file='gwflow_cell_wb_ppag_yr.txt')
      write(out_gw_ppag,*) 'Annual pumping rate (m3/day) (irrigation)'
      open(out_gw_ppag_mo,file='gwflow_cell_wb_ppag_mon.txt')
      write(out_gw_ppag_mo,*) 'Monthly pumping rate (m3/day) (irrigation)'
      !pumping deficiencies (unmet demand)
      open(out_gw_pumpdef,file='gwflow_cell_wb_ppag_deficient_yr.txt')
      write(out_gw_pumpdef,*) 'Annual rate (m3/day) not satisfied (irrigation)'
      !track pumped volume for each irrigated HRU
      allocate(hru_pump(sp_ob%hru))
      allocate(hru_pump_mo(sp_ob%hru))
      allocate(hru_pump_yr(sp_ob%hru))
      allocate(hru_pump_mo_all(sp_ob%hru,time%nbyr*12))
      allocate(hru_pump_yr_all(sp_ob%hru,time%nbyr))
      hru_pump = 0.
      hru_pump_mo = 0.
      hru_pump_yr = 0.
      hru_pump_mo_all = 0.
      hru_pump_yr_all = 0.
      open(out_hru_pump_yr,file='gwflow_cell_wb_ppag_hru_yr.txt')
      write(out_hru_pump_yr,*) 'Annual pumped volume (m3) (irrigation) for HRUs'
      write(out_hru_pump_yr,*) 'Columns: each year of the simulation'
      open(out_hru_pump_mo,file='gwflow_cell_wb_ppag_hru_mon.txt')
      write(out_hru_pump_mo,*) 'Monthly pumped volume (m3) (irrigation) for HRUs'
      write(out_hru_pump_mo,*) 'Columns: each month of the simulation'
      inquire(file='gwflow.hru_pump_observe',exist=i_exist)
      if(hru_pump_flag == 1) then
        open(in_hru_pump_obs,file='gwflow.hru_pump_observe')
        read(in_hru_pump_obs,*)
        read(in_hru_pump_obs,*) num_hru_pump_obs
        allocate(hru_pump_ids(num_hru_pump_obs))
        do i=1,num_hru_pump_obs
          read(in_hru_pump_obs,*) hru_pump_ids(i)
        enddo
        allocate(hru_pump_obs(num_hru_pump_obs))
        hru_pump_obs = 0.
        open(out_hru_pump_obs,file='gwflow_cell_wb_ppag_obs_day.txt')
        write(out_hru_pump_obs,*) 'Daily groundwater pumping (m3) for specified HRUs'
        write(out_hru_pump_obs,*) 'Columns = HRUs (same order as in gwflow.hru_pump_observe)'
        write(out_hru_pump_obs,*) 'Time(day)    Daily pumping (m3)'
      endif

      !groundwater pumping (specified) ------------------------------------------------------------

      if(gw_pumpex_flag == 1) then
      inquire(file='gwflow.pumpex',exist=i_exist)
      if(i_exist) then
        write(out_gw,*) '          groundwater pumping external (gwflow.pumpex found)'
        open(in_gw,file='gwflow.pumpex')
        read(in_gw,*) header
        read(in_gw,*) gw_npumpex !number of pumps
        allocate(gw_pumpex_cell(gw_npumpex))
        allocate(gw_pumpex_nperiods(gw_npumpex))
        allocate(gw_pumpex_dates(gw_npumpex,2,1000))
        allocate(gw_pumpex_rates(gw_npumpex,1000))
        gw_pumpex_cell = 0
        gw_pumpex_nperiods = 0
        gw_pumpex_rates = 0.
        do i=1,gw_npumpex !read in the information for each pump
          read(in_gw,*) header
          read(in_gw,*) pumpex_cell,gw_pumpex_nperiods(i)
          if(grid_type == "structured") then
            gw_pumpex_cell(i) = cell_id_list(pumpex_cell)
          elseif(grid_type == "unstructured") then
            gw_pumpex_cell(i) = pumpex_cell
          endif
          do j=1,gw_pumpex_nperiods(i)
            read(in_gw,*) gw_pumpex_dates(i,1,j),gw_pumpex_dates(i,2,j),gw_pumpex_rates(i,j)
          enddo
        enddo
        close(in_gw)
        gw_daycount = 1
        !flux output file
        open(out_gw_ppex,file='gwflow_cell_wb_ppex_yr.txt')
        write(out_gw_ppex,*) 'Annual pumping rate (m3/day) (specified)'
        open(out_gw_ppex_mo,file='gwflow_cell_wb_ppex_mon.txt')
        write(out_gw_ppex_mo,*) 'Monthly pumping rate (m3/day) (specified)'
      else
        write(out_gw,*) '          gwflow.pumpex not found; pumping not simulated'
      endif
      endif !end specified pumping

      !tile drainage outflow ----------------------------------------------------------------------
      !tile drain cell information
      if(gw_tile_flag == 1) then
      inquire(file='gwflow.tiles',exist=i_exist)
      if(i_exist) then
        write(out_gw,*) '          groundwater-tile drainage outflow (gwflow.tiles found)'
        open(in_gw,file='gwflow.tiles')
        read(in_gw,*) header
        !read in tile parameters
        allocate(gw_tile_depth(ncell))
        allocate(gw_tile_drain_area(ncell))
        allocate(gw_tile_K(ncell))
        read(in_gw,*) tile_depth_val
        read(in_gw,*) tile_drain_area_val
        read(in_gw,*) tile_K_val
        do i=1,ncell
          gw_tile_depth(i) = tile_depth_val
          gw_tile_drain_area(i) = tile_drain_area_val
          gw_tile_K(i) = tile_K_val
        enddo
        !determine if cell-by-cell values should be read in
        !tile depth
        inquire(file='gwflow.array_tiledepth',exist=i_exist)
        if(i_exist) then
          open(1600,file='gwflow.array_tiledepth')
          read(1600,*) ((grid_val(i,j),j=1,grid_ncol),i=1,grid_nrow)
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                gw_tile_depth(cell_id_usg(i,j)) = grid_val(i,j)
              endif
            enddo
          enddo
          close(1600)
        endif
        !tile drain area
        inquire(file='gwflow.array_tilearea',exist=i_exist)
        if(i_exist) then
          open(1600,file='gwflow.array_tilearea')
          read(1600,*) ((grid_val(i,j),j=1,grid_ncol),i=1,grid_nrow)
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                gw_tile_drain_area(cell_id_usg(i,j)) = grid_val(i,j)
              endif
            enddo
          enddo
          close(1600)
        endif
        !tile K
        inquire(file='gwflow.array_tileK',exist=i_exist)
        if(i_exist) then
          open(1600,file='gwflow.array_tileK')
          read(1600,*) ((grid_val(i,j),j=1,grid_ncol),i=1,grid_nrow)
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                gw_tile_K(cell_id_usg(i,j)) = grid_val(i,j)
              endif
            enddo
          enddo
          close(1600)
        endif
        !read in tile cell groups (if any)
        read(in_gw,*) gw_tile_group_flag
        if(gw_tile_group_flag.eq.1) then
          read(in_gw,*) gw_tile_num_group
          allocate(gw_tile_groups(gw_tile_num_group,5000))
          do i=1,gw_tile_num_group
            read(in_gw,*)
            read(in_gw,*) num_tile_cells(i)
            do j=1,num_tile_cells(i)
              read(in_gw,*) gw_tile_groups(i,j)
            enddo
          enddo
          open(out_tile_cells,file='gwflow_tile_cell_groups')
          write(out_tile_cells,*) 'Total tile flow (m3/sec)'
        endif
        !read in tile cell flag (0=no tile; 1=tiles are present)
        read(in_gw,*) header
        if(grid_type == "structured") then
          do i=1,grid_nrow
            read(in_gw,*) (grid_int(i,j),j=1,grid_ncol)
          enddo
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                gw_state(cell_id_usg(i,j))%tile = grid_int(i,j)
              endif
            enddo
          enddo
        elseif(grid_type == "unstructured") then
          do i=1,ncell
            read(in_gw,*) gw_state(i)%tile
          enddo
        endif
        close(in_gw)
        !determine the number of tile cells that are linked to each channel
        allocate(gw_tile_info(sp_ob%chandeg))
        do i=1,ncell
          if(gw_state(i)%stat.eq.1 .and. gw_state(i)%tile.eq.1) then
            channel = cell_channel(i) !channel connected to cell
            gw_tile_info(channel)%ncon = gw_tile_info(channel)%ncon + 1
          endif
        enddo
        !allocate array for cell information
        do i=1,sp_ob%chandeg
          allocate(gw_tile_info(i)%cells(gw_tile_info(i)%ncon))
          gw_tile_info(i)%ncon = 0
        enddo
        !populate the array holding the cell numbers for each channel
        do i=1,ncell
          if(gw_state(i)%stat == 1 .and. gw_state(i)%tile == 1) then
            channel = cell_channel(i) !channel connected to cell
            gw_tile_info(channel)%ncon = gw_tile_info(channel)%ncon + 1
            gw_tile_info(channel)%cells(gw_tile_info(channel)%ncon) = i
          endif
        enddo
        !flux output file
        open(out_gw_tile,file='gwflow_cell_wb_tile_yr.txt')
        write(out_gw_tile,*) 'Annual tile flows (m3/day)'
        open(out_gw_tile_mo,file='gwflow_cell_wb_tile_mon.txt')
        write(out_gw_tile_mo,*) 'Monthly tile flows (m3/day)'
      else
        write(out_gw,*) '          gwflow.tiles not found; tile drainage not simulated'
        gw_tile_flag = 0.
      endif
      endif !end tile drainage

      !aquifer-reservoir exchange -----------------------------------------------------------------
      if(gw_res_flag == 1) then
      inquire(file='gwflow.rescells',exist=i_exist)
      if(i_exist) then
        write(out_gw,*) '          groundwater-reservoir exchange (gwflow.rescells found)'
        open(in_res_cell,file='gwflow.rescells')
        read(in_res_cell,*) header
        read(in_res_cell,*) header
        !read in reservoir bed conductivity (m/day) and thickness (m)
        read(in_res_cell,*) res_thick
        read(in_res_cell,*) res_K
        !number of cells that are linked to each reservoir
        allocate(gw_resv_info(sp_ob%res))
        !track the number of cells connected with each reservoir
        read(in_res_cell,*) num_res_cells
        read(in_res_cell,*) header
        do i=1,num_res_cells
          read(in_res_cell,*) res_cell,res_id,res_stage
          if(res_id > 0) then
            gw_resv_info(res_id)%ncon = gw_resv_info(res_id)%ncon + 1
          endif
        enddo
        !allocate arrays and re-read information
        do i=1,sp_ob%res
          allocate(gw_resv_info(i)%cells(gw_resv_info(i)%ncon))
          allocate(gw_resv_info(i)%elev(gw_resv_info(i)%ncon))
          allocate(gw_resv_info(i)%hydc(gw_resv_info(i)%ncon))
          allocate(gw_resv_info(i)%thck(gw_resv_info(i)%ncon))
          gw_resv_info(i)%ncon = 0
        enddo
        rewind(in_res_cell)
        read(in_res_cell,*) header
        read(in_res_cell,*) header
        read(in_res_cell,*) res_thick
        read(in_res_cell,*) res_K
        read(in_res_cell,*) num_res_cells
        read(in_res_cell,*) header
        do i=1,num_res_cells
          read(in_res_cell,*) res_cell,res_id,res_stage
          if(res_id > 0) then
            if(grid_type == "structured") then
              res_cell = cell_id_list(res_cell)
            endif
            gw_resv_info(res_id)%ncon = gw_resv_info(res_id)%ncon + 1
            gw_resv_info(res_id)%cells(gw_resv_info(res_id)%ncon) = res_cell
            gw_resv_info(res_id)%elev(gw_resv_info(res_id)%ncon) = gw_state(res_cell)%elev
            gw_resv_info(res_id)%hydc(gw_resv_info(res_id)%ncon) = res_K
            gw_resv_info(res_id)%thck(gw_resv_info(res_id)%ncon) = res_thick
          endif
        enddo
        !flux output file
        open(out_gw_resv,file='gwflow_cell_wb_resv_yr.txt')
        write(out_gw_resv,*) 'Annual groundwater-reservoir exchange (m3/day)'
        open(out_gw_resv_mo,file='gwflow_cell_wb_resv_mon.txt')
        write(out_gw_resv_mo,*) 'Monthly groundwater-reservoir exchange (m3/day)'
      else
        write(out_gw,*) '          gwflow.rescells not found (groundwater-res exchange not simulated)'
      endif
      endif !end reservoir exchange

      !aquifer-wetland exchange -------------------------------------------------------------------
      if(gw_wet_flag == 1) then
        write(out_gw,*) '          groundwater-->wetland exchange'
        !wetland bed thickness for each wetland object is read in wet_read_hyd; set default value here
        allocate(wet_thick(sp_ob%hru))
        wet_thick = 0.25
        !flux output file
        open(out_gw_wetl,file='gwflow_cell_wb_wetl_yr.txt')
        write(out_gw_wetl,*) 'Annual groundwater outflow to wetlands (m3/day)'
        open(out_gw_wetl_mo,file='gwflow_cell_wb_wetl_mon.txt')
        write(out_gw_wetl_mo,*) 'Monthly groundwater outflow to wetlands (m3/day)'
      endif !end wetland exchange

      !aquifer-floodplain exchange ----------------------------------------------------------------
      allocate(flood_freq(sp_ob%chandeg))
      flood_freq = 0
      if(gw_fp_flag == 1) then
      inquire(file='gwflow.floodplain',exist=i_exist)
      if(i_exist) then
        write(out_gw,*) '          groundwater-floodplain exchange (gwflow.floodplain found)'
        open(in_fp_cell,file='gwflow.floodplain')
        read(in_fp_cell,*) header
        read(in_fp_cell,*) gw_fp_ncells !number of floodplain cells
        !number of cells that are linked to each channel
        allocate(gw_fpln_info(sp_ob%chandeg))
        !read in the attributes of each cell (ID, channel of connection, K, area of intersection with floodplain)
        allocate(gw_fp_cellid(gw_fp_ncells))
        allocate(gw_fp_chanid(gw_fp_ncells))
        allocate(gw_fp_K(gw_fp_ncells))
        allocate(gw_fp_area(gw_fp_ncells))
        read(in_fp_cell,*) header
        do i=1,gw_fp_ncells
          read(in_fp_cell,*) gw_fp_cellid(i),gw_fp_chanid(i),gw_fp_K(i),gw_fp_area(i)
          if(grid_type == "structured") then
            gw_fp_cellid(i) = cell_id_list(gw_fp_cellid(i))
          endif
          !limit the floodplain area to the size of the cell
          if(gw_fp_area(i).gt.(gw_state(gw_fp_cellid(i))%area)) then
            gw_fp_area(i) = gw_state(gw_fp_cellid(i))%area
          endif
          !track the number of cells for each channel
          channel = gw_fp_chanid(i) !channel connected to cell
          gw_fpln_info(channel)%ncon = gw_fpln_info(channel)%ncon + 1
        enddo
        !allocate arrays holding the cell information
        do i=1,sp_ob%chandeg
          allocate(gw_fpln_info(i)%cells(gw_fpln_info(i)%ncon))
          allocate(gw_fpln_info(i)%hydc(gw_fpln_info(i)%ncon))
          allocate(gw_fpln_info(i)%area(gw_fpln_info(i)%ncon))
          allocate(gw_fpln_info(i)%mtch(gw_fpln_info(i)%ncon))
          gw_fpln_info(i)%ncon = 0
        enddo
        !populate the array holding the cell numbers for each channel
        do i=1,gw_fp_ncells
          channel = gw_fp_chanid(i) !channel connected to cell
          gw_fpln_info(channel)%ncon = gw_fpln_info(channel)%ncon + 1
          gw_fpln_info(channel)%cells(gw_fpln_info(channel)%ncon) = gw_fp_cellid(i)
          gw_fpln_info(channel)%hydc(gw_fpln_info(channel)%ncon) = gw_fp_K(i)
          gw_fpln_info(channel)%area(gw_fpln_info(channel)%ncon) = gw_fp_area(i)
          gw_fpln_info(channel)%mtch(gw_fpln_info(channel)%ncon) = 0
          do k=1,sp_ob%gwflow !loop through channel cells - see if there is a match
            if(gw_fp_cellid(i) == gw_chan_id(k)) then
              gw_fpln_info(channel)%mtch(gw_fpln_info(channel)%ncon) = k
            endif
          enddo
        enddo
        !flux output file
        open(out_gw_fpln,file='gwflow_cell_wb_fpln_yr.txt')
        write(out_gw_fpln,*) 'Annual floodplain seepage (m3/day)'
        open(out_gw_fpln_mo,file='gwflow_cell_wb_fpln_mon.txt')
        write(out_gw_fpln_mo,*) 'Monthly floodplain seepage (m3/day)'
      else
        write(out_gw,*) '          gwflow.floodplain not found (groundwater-fp exchange not simulated)'
      endif
      endif !end floodplain exchange


      !groundwater seepage from canals ------------------------------------------------------------
      !canal seepage information (these are for cells that are connected to irrigation canals)
      if(gw_canal_flag == 1) then
      inquire(file='gwflow.canals',exist=i_exist)
      if(i_exist) then
        write(out_gw,*) '          canal-->groundwater seepage (gwflow.canals found)'
        open(in_canal_cell,file='gwflow.canals')
        read(in_canal_cell,*) header
        !read in the number of canals for each channel
        allocate(gw_chan_canl_info(sp_ob%chandeg)) !number of canals for each channel
        canal_out = 0
        read(in_canal_cell,*) gw_ncanal
        do i=1,11
          read(in_canal_cell,*)
        enddo
        allocate(gw_canl_div_info(gw_ncanal))
        allocate(canal_out_info(gw_ncanal,6))
        canal_out_info = 0.
        do i=1,gw_ncanal
          read(in_canal_cell,*) canal,div,channel,width,depth,thick,bed_K,day_beg,day_end,frc_ret
          if(div > 0) then !canal water from point source diversion
            canal_div(i) = 1
            gw_canl_div_info(i)%canal_id = canal
            gw_canl_div_info(i)%divr = div
            gw_canl_div_info(i)%width = width
            gw_canl_div_info(i)%depth = depth
            gw_canl_div_info(i)%thick = thick
            gw_canl_div_info(i)%bed_K = bed_K
            gw_canl_div_info(i)%frc_ret = frc_ret
          elseif(div == 0 .and. channel == 0) then !canal water originates from outside the model domain
            canal_out(i) = 1
            canal_out_info(i,1) = width
            canal_out_info(i,2) = depth
            canal_out_info(i,3) = thick
            canal_out_info(i,4) = day_beg
            canal_out_info(i,5) = day_end
            canal_out_info(i,6) = bed_K
          else
            gw_chan_canl_info(channel)%ncanal = gw_chan_canl_info(channel)%ncanal + 1
          endif
        enddo
        !allocate arrays for canal attributes
        do i=1,sp_ob%chandeg
          allocate(gw_chan_canl_info(i)%canals(gw_chan_canl_info(i)%ncanal))
          allocate(gw_chan_canl_info(i)%wdth(gw_chan_canl_info(i)%ncanal))
          allocate(gw_chan_canl_info(i)%dpth(gw_chan_canl_info(i)%ncanal))
          allocate(gw_chan_canl_info(i)%thck(gw_chan_canl_info(i)%ncanal))
          allocate(gw_chan_canl_info(i)%hydc(gw_chan_canl_info(i)%ncanal))
          allocate(gw_chan_canl_info(i)%dayb(gw_chan_canl_info(i)%ncanal))
          allocate(gw_chan_canl_info(i)%daye(gw_chan_canl_info(i)%ncanal))
          gw_chan_canl_info(i)%ncanal = 0
        enddo
        !read in and store canal attributes
        rewind(in_canal_cell)
        read(in_canal_cell,*) header
        read(in_canal_cell,*) gw_ncanal
        do i=1,11
          read(in_canal_cell,*)
        enddo
        do i=1,gw_ncanal
          read(in_canal_cell,*) canal,div,channel,width,depth,thick,bed_K,day_beg,day_end,frc_ret
          if(canal_out(canal) == 0 .and. div == 0) then !channel source; no diversion
            gw_chan_canl_info(channel)%ncanal = gw_chan_canl_info(channel)%ncanal + 1
            gw_chan_canl_info(channel)%canals(gw_chan_canl_info(channel)%ncanal) = canal
            gw_chan_canl_info(channel)%wdth(gw_chan_canl_info(channel)%ncanal) = width
            gw_chan_canl_info(channel)%dpth(gw_chan_canl_info(channel)%ncanal) = depth
            gw_chan_canl_info(channel)%thck(gw_chan_canl_info(channel)%ncanal) = thick
            gw_chan_canl_info(channel)%hydc(gw_chan_canl_info(channel)%ncanal) = bed_K
            gw_chan_canl_info(channel)%dayb(gw_chan_canl_info(channel)%ncanal) = day_beg
            gw_chan_canl_info(channel)%daye(gw_chan_canl_info(channel)%ncanal) = day_end
          endif
        enddo
        !read the runoff fraction for each irrigation type
        read(in_canal_cell,*) header
        read(in_canal_cell,*) irrig_type,fld_ro !flood irrigation
        read(in_canal_cell,*) irrig_type,spk_ro !sprinkler irrigation
        read(in_canal_cell,*) irrig_type,drp_ro !drip irrigation
        !read the HRUs serviced (irrigated) by each canal, with the accompanying irrigation type
        read(in_canal_cell,*) header
        do i=1,gw_ncanal
          read(in_canal_cell,*) header
          read(in_canal_cell,*) num_hru
          gw_canl_div_info(i)%nhru = num_hru
          if(num_hru > 0) then
            allocate(gw_canl_div_info(i)%hrus(gw_canl_div_info(i)%nhru))
            allocate(gw_canl_div_info(i)%hru_ro(gw_canl_div_info(i)%nhru))
            do j=1,num_hru
              read(in_canal_cell,*) hru_id,irrig_type
              gw_canl_div_info(i)%hrus(j) = hru_id
              if(irrig_type == "flood") then
                gw_canl_div_info(i)%hru_ro(j) = fld_ro
              elseif(irrig_type == "sprinkler") then
                gw_canl_div_info(i)%hru_ro(j) = spk_ro
              elseif(irrig_type == "drip") then
                gw_canl_div_info(i)%hru_ro(j) = drp_ro
              else
                gw_canl_div_info(i)%hru_ro(j) = 0.
              endif
            enddo
          endif
        enddo !go to next canal
        !determine the number of cells that are connected to canals that receive outside water; that receive water from diversions
        gw_canal_ncells_div = 0
        gw_canal_ncells_out = 0
        read(in_canal_cell,*) header
        read(in_canal_cell,*) gw_canal_ncells
        read(in_canal_cell,*) header
        do i=1,gw_canal_ncells
          read(in_canal_cell,*) cell_num,canal
          if(canal_div(canal) == 1) then
            gw_canal_ncells_div = gw_canal_ncells_div + 1
          endif
          if(canal_out(canal) == 1) then
            gw_canal_ncells_out = gw_canal_ncells_out + 1
          endif
        enddo
        allocate(gw_canl_div_cell(gw_canal_ncells_div))
        allocate(gw_canl_out_info(gw_canal_ncells_out))
        !rewind file to beginning line
        rewind(in_canal_cell)
        read(in_canal_cell,*) header
        read(in_canal_cell,*) gw_ncanal
        do i=1,11
          read(in_canal_cell,*)
        enddo
        do i=1,gw_ncanal
          read(in_canal_cell,*)
        enddo
        do i=1,4
          read(in_canal_cell,*)
        enddo
        read(in_canal_cell,*) header
        do i=1,gw_ncanal
          read(in_canal_cell,*) header
          read(in_canal_cell,*)
          do j=1,gw_canl_div_info(i)%nhru
            read(in_canal_cell,*)
          enddo
        enddo
        !read in the number of cells for each canal
        read(in_canal_cell,*) header
        allocate(gw_canl_info(gw_ncanal)) !allocate number of cells connected to each canal
        read(in_canal_cell,*) gw_canal_ncells
        read(in_canal_cell,*) header
        do i=1,gw_canal_ncells
          read(in_canal_cell,*) cell_num,canal,length,stage
          if(canal_out(canal) == 0) then
            gw_canl_info(canal)%ncon = gw_canl_info(canal)%ncon + 1
          endif
        enddo
        !allocate arrays holding cell attributes
        do i=1,gw_ncanal
          allocate(gw_canl_info(i)%cells(gw_canl_info(i)%ncon))
          allocate(gw_canl_info(i)%leng(gw_canl_info(i)%ncon))
          allocate(gw_canl_info(i)%elev(gw_canl_info(i)%ncon))
          allocate(gw_canl_info(i)%hydc(gw_canl_info(i)%ncon))
          gw_canl_info(i)%ncon = 0
        enddo
        !read in and store cell attributes (canal length, stage, K)
        rewind(in_canal_cell)
        read(in_canal_cell,*) header
        read(in_canal_cell,*) gw_ncanal
        do i=1,11
          read(in_canal_cell,*)
        enddo
        do i=1,gw_ncanal
          read(in_canal_cell,*)
        enddo
        do i=1,4
          read(in_canal_cell,*)
        enddo
        read(in_canal_cell,*) header
        do i=1,gw_ncanal
          read(in_canal_cell,*) header
          read(in_canal_cell,*)
          do j=1,gw_canl_div_info(i)%nhru
            read(in_canal_cell,*)
          enddo
        enddo
        read(in_canal_cell,*) header
        read(in_canal_cell,*) gw_canal_ncells
        read(in_canal_cell,*) header
        gw_canal_ncells_div = 0
        gw_canal_ncells_out = 0
        do i=1,gw_canal_ncells
          read(in_canal_cell,*) cell_num,canal,length,stage
          if(grid_type == "structured") then
            cell_num = cell_id_list(cell_num)
          endif
          if(cell_num > 0) then
            if(canal_div(canal) == 1) then !canal water from a point source diversion
              gw_canal_ncells_div = gw_canal_ncells_div + 1
              gw_canl_div_cell(gw_canal_ncells_div)%cell_id = cell_num
              gw_canl_div_cell(gw_canal_ncells_div)%canal_id = canal
              gw_canl_div_cell(gw_canal_ncells_div)%leng = length
              gw_canl_div_cell(gw_canal_ncells_div)%elev = stage
            elseif(canal_out(canal) == 1) then !canal water from outside the model domain
              gw_canal_ncells_out = gw_canal_ncells_out + 1
              gw_canl_out_info(gw_canal_ncells_out)%cell_id = cell_num
              gw_canl_out_info(gw_canal_ncells_out)%wdth = canal_out_info(canal,1)
              gw_canl_out_info(gw_canal_ncells_out)%dpth = canal_out_info(canal,2)
              gw_canl_out_info(gw_canal_ncells_out)%thck = canal_out_info(canal,3)
              gw_canl_out_info(gw_canal_ncells_out)%leng = length
              gw_canl_out_info(gw_canal_ncells_out)%elev = stage
              gw_canl_out_info(gw_canal_ncells_out)%hydc = canal_out_info(canal,6)
              gw_canl_out_info(gw_canal_ncells_out)%dayb = canal_out_info(canal,4)
              gw_canl_out_info(gw_canal_ncells_out)%daye = canal_out_info(canal,5)
            else
              gw_canl_info(canal)%ncon = gw_canl_info(canal)%ncon + 1
              gw_canl_info(canal)%cells(gw_canl_info(canal)%ncon) = cell_num
              gw_canl_info(canal)%leng(gw_canl_info(canal)%ncon) = length
              gw_canl_info(canal)%elev(gw_canl_info(canal)%ncon) = stage
              gw_canl_info(canal)%hydc(gw_canl_info(canal)%ncon) = canal_out_info(canal,6)
            endif
          endif
        enddo !go to next canal cell
        !flux output file
        open(out_gw_canl,file='gwflow_cell_wb_canl_yr.txt')
        write(out_gw_canl,*) 'Annual groundwater-canal exchange (m3/day)'
        open(out_gw_canl_mo,file='gwflow_cell_wb_canl_mon.txt')
        write(out_gw_canl_mo,*) 'Monthly groundwater-canal exchange (m3/day)'
        !canal water balance file (daily)
        open(out_canal_bal,file='gwflow_canal_water_balance')
        write(out_canal_bal,*) 'daily water balance for recharge ponds'
        write(out_canal_bal,*)
        write(out_canal_bal,*) 'div:       m3   water diverted into canal'
        write(out_canal_bal,*) 'stor:      m3   water storage at end of day'
        write(out_canal_bal,*) 'pond:      m3   water transferred to recharge ponds'
        write(out_canal_bal,*) 'seep:      m3   water seeped to aquifer'
        write(out_canal_bal,*) 'irrg:      m3   water transferred to irrigated fields'
        write(out_canal_bal,*) 'retn:      m3   water left in canal, to return to river'
        write(out_canal_bal,*)
        gwflow_hdr_canal = (/"year","month","day","canal","div","stor","pond","seep","irrg","retn"/)
        write(out_canal_bal,131) (gwflow_hdr_canal(j),j=1,10)
        !canal solute mass balance file (daily)
        open(out_canal_sol,file='gwflow_canal_mass_balance')
        write(out_canal_sol,*) 'daily solute mass balance for recharge ponds'
        write(out_canal_sol,*)
        write(out_canal_sol,*) 'div:       kg   solute mass diverted into canal'
        write(out_canal_sol,*) 'stor:      kg   solute mass at end of day'
        write(out_canal_sol,*) 'pond:      kg   solute mass transferred to recharge ponds'
        write(out_canal_sol,*) 'seep:      kg   solute mass leached to aquifer'
        write(out_canal_sol,*) 'irrg:      kg   solute mass transferred to irrigated fields'
        write(out_canal_sol,*) 'retn:      kg   solute mass left in canal, to return to river'
        write(out_canal_sol,*)
        gwflow_hdr_canal_sol = (/"year","month","day","canal","solute","div","stor","pond","seep","irrg","retn"/)
        write(out_canal_sol,131) (gwflow_hdr_canal_sol(j),j=1,11)
      else
        write(out_gw,*) '          gwflow.canals not found (canal seepage not simulated)'
      endif
      endif !end canal seepage


      !phreatophyte transpiration ---------------------------------------------------------------------------
      gw_phyt_flag = 0
      inquire(file='gwflow.phreatophytes',exist=i_exist)
      if(i_exist) then
        gw_phyt_flag = 1
        open(in_gw,file='gwflow.phreatophytes')
        read(in_gw,*) header
        read(in_gw,*) header
        !read in points that define depth-ET relationship
        read(in_gw,*) gw_phyt_npts
        read(in_gw,*) header
        allocate(gw_phyt_dep(gw_phyt_npts))
        allocate(gw_phyt_rate(gw_phyt_npts))
        do i=1,gw_phyt_npts
          read(in_gw,*) gw_phyt_dep(i),gw_phyt_rate(i)
        enddo
        !read in cells that have phreatophytes
        read(in_gw,*) gw_phyt_ncells
        allocate(gw_phyt_ids(gw_phyt_ncells))
        allocate(gw_phyt_area(gw_phyt_ncells))
        read(in_gw,*) header
        do i=1,gw_phyt_ncells
          read(in_gw,*) gw_phyt_ids(i),gw_phyt_area(i)
          if(grid_type == "structured") then
            gw_phyt_ids(i) = cell_id_list(gw_phyt_ids(i))
          endif
        enddo
        close(in_gw)
        !flux output file
        open(out_gw_phyt,file='gwflow_cell_wb_phyt_yr.txt')
        write(out_gw_phyt,*) 'Annual phreatophyte transpiration (m3/day)'
        open(out_gw_phyt_mo,file='gwflow_cell_wb_phyt_mon.txt')
        write(out_gw_phyt_mo,*) 'Monthly phreatophyte transpiration (m3/day)'
      endif

      !time-varying boundary conditions ---------------------------------------------------------------------
      gw_tvh_flag = 0
      inquire(file='gwflow.tvheads',exist=i_exist)
      if(i_exist) then
        gw_tvh_flag = 1
        open(in_tvh,file='gwflow.tvheads')
        read(in_tvh,*) header
        read(in_tvh,*) header
        read(in_tvh,*) gw_ntvh
        read(in_tvh,*) header
        allocate(gw_tvh_ids(gw_ntvh))
        allocate(gw_tvh_vals(gw_ntvh,time%nbyr))
        do i=1,gw_ntvh
          read(in_tvh,*) cell_id,(gw_tvh_vals(i,j),j=1,time%nbyr)
          if(grid_type == "structured") then
            gw_tvh_ids(i) = cell_id_list(cell_id)
          endif
        enddo
      endif


      !groundwater heat transport option --------------------------------------------------------------------
      inquire(file='gwflow.heat',exist=i_exist)
      if(i_exist) then
        gw_heat_flag = 1
        open(in_gw,file='gwflow.heat')
        read(in_gw,*) header
        !allocate groundwater head cell features
        allocate(gwheat_state(ncell))
        !read in thermal conductivity for each aquifer zone; assign to cells
        read(in_gw,*) header
        allocate(zones_Kt(nzones_aquK))
        do i=1,nzones_aquK
          read(in_gw,*) zones_Kt(i)
        enddo
        if(grid_type == "structured") then
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                gwheat_state(cell_id_usg(i,j))%thmc = zones_Kt(gw_state(cell_id_usg(i,j))%zone)
              endif
            enddo
          enddo
        elseif(grid_type == "unstructured") then
          do i=1,ncell
            gwheat_state(i)%thmc = zones_Kt(gw_state(i)%zone)
          enddo
        endif
        !read in initial groundwater temperature
        read(in_gw,*) header
        if(grid_type == "structured") then
          read(in_gw,*) read_type
          if(read_type == "single") then
            read(in_gw,*) single_value
            grid_val = single_value
          elseif(read_type == "array") then
            do i=1,grid_nrow
              read(in_gw,*) (grid_val(i,j),j=1,grid_ncol)
            enddo
          endif
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                gwheat_state(cell_id_usg(i,j))%temp = grid_val(i,j) !temperature (deg C)
              endif
            enddo
          enddo
        elseif(grid_type == "unstructured") then
          !read one cell at a time
          do i=1,ncell
            read(in_gw,*) gwheat_state(i)%temp
          enddo
        endif
        close(in_gw)

        !allocate cell source/sink arrays
        allocate(gw_heat_ss(ncell))
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
          gw_heat_ss(i)%fpln = 0.
          gw_heat_ss(i)%canl = 0.
          gw_heat_ss(i)%totl = 0.
        enddo

        !allocate grid source/sink arrays
        !annual
        allocate(gw_heat_ss_yr(ncell))
        do i=1,ncell
          gw_heat_ss_yr(i)%rech = 0.
          gw_heat_ss_yr(i)%gwet = 0.
          gw_heat_ss_yr(i)%gwsw = 0.
          gw_heat_ss_yr(i)%swgw = 0.
          gw_heat_ss_yr(i)%satx = 0.
          gw_heat_ss_yr(i)%soil = 0.
          gw_heat_ss_yr(i)%latl = 0.
          gw_heat_ss_yr(i)%disp = 0.
          gw_heat_ss_yr(i)%bndr = 0.
          gw_heat_ss_yr(i)%ppag = 0.
          gw_heat_ss_yr(i)%ppex = 0.
          gw_heat_ss_yr(i)%tile = 0.
          gw_heat_ss_yr(i)%resv = 0.
          gw_heat_ss_yr(i)%wetl = 0.
          gw_heat_ss_yr(i)%fpln = 0.
          gw_heat_ss_yr(i)%canl = 0.
        enddo

        !temperature at observation cells
        allocate(gw_obs_temp(gw_num_obs_wells))
        gw_obs_temp = 0.
        !open file for writing out daily head values
        open(out_gwobs_temp,file='gwflow_cell_obs_temp_day.txt')
        write(out_gwobs_temp,*) 'Daily temperature (deg C) values for observation wells'
        write(out_gwobs_temp,123) 'cell:',(gw_obs_cells(k),k=1,gw_num_obs_wells)
        write(out_gwobs_temp,*)

        !open output files for each source/sink type
        !recharge
        open(out_heat_rech,file='gwflow_heat_rech')
        write(out_heat_rech,*) 'Annual recharge heat flux (MJ/day)'
        allocate(gw_rechheat(sp_ob%hru))
        gw_rechheat = 0.
        !groundwater ET
        open(out_heat_gwet,file='gwflow_heat_gwet')
        write(out_heat_gwet,*) 'Annual recharge heat flux (MJ/day)'
        !groundwater-channel exchange
        open(out_heat_gwsw,file='gwflow_heat_gwsw')
        write(out_heat_gwsw,*) 'Annual gw-channel exchange heat flux (MJ/day)'
        !ground-->soil transfer
        if(gw_soil_flag.eq.1) then
          open(out_heat_soil,file='gwflow_heat_soil')
          write(out_heat_soil,*) 'Annual groundwater-->soil heat flux (MJ/day)'
        endif
        !saturation excess flow
        if(gw_satx_flag.eq.1) then
          open(out_heat_satx,file='gwflow_heat_satx')
          write(out_heat_satx,*) 'Annual saturation excess flow heat flux (MJ/day)'
        endif
        !irrigation pumping
        open(out_heat_ppag,file='gwflow_heat_ppag')
        write(out_heat_ppag,*) 'Annual heat flux in pumping (MJ/day) (irrigation)'
        !specified pumping
        if(gw_pumpex_flag == 1) then
          open(out_heat_ppex,file='gwflow_heat_ppex')
          write(out_heat_ppex,*) 'Annual heat flux in pumping (MJ/day) (specified)'
        endif
        !tile drainage
        if(gw_tile_flag == 1) then
          open(out_heat_tile,file='gwflow_heat_tile')
          write(out_heat_tile,*) 'Annual heat flux in tile flow (MJ/day)'
        endif
        !reservoir
        if(gw_res_flag == 1) then
          open(out_heat_resv,file='gwflow_heat_resv')
          write(out_heat_resv,*) 'Annual groundwater-reservoir exchange heat flux (MJ/day)'
        endif
        !wetland exchange
        if(gw_wet_flag == 1) then
          open(out_heat_wetl,file='gwflow_heat_wetl')
          write(out_heat_wetl,*) 'Annual groundwater-wetland exchange heat flux (MJ/day)'
        endif
        !floodplain exchange
        if(gw_fp_flag == 1) then
          open(out_heat_fpln,file='gwflow_heat_fpln')
          write(out_heat_fpln,*) 'Annual groundwater-floodplain exchange heat flux (MJ/day)'
        endif
        !canal seepage
        if(gw_canal_flag == 1) then
          open(out_heat_canl,file='gwflow_heat_canl')
          write(out_heat_canl,*) 'Annual groundwater-canal exchange heat flux (MJ/day)'
        endif
        allocate(heat_cell(ncell))
        heat_cell = 0.

      endif



      !groundwater solute transport option ------------------------------------------------------------------
      write(out_gw,*)

      gw_nsolute = 0
      if(gw_solute_flag == 1) then
      inquire(file='gwflow.solutes',exist=i_exist)
      if(i_exist) then

        !open the file
        open(in_gw,file='gwflow.solutes')

        !include no3 and p (default)
        gw_nsolute = 2
        gwsol_nm(1) = 'no3'
        gwsol_nm(2) = 'p'

        !determine which other solutes should be included
        inquire(file="constituents.cs", exist=i_exist2)
        if(i_exist2) then
          if(cs_db%num_salts > 0) then
            gwsol_salt = 1
            do i=1,cs_db%num_salts
              gw_nsolute = gw_nsolute + 1
              gwsol_nm(gw_nsolute) = cs_db%salts(i)
            enddo
          else
            gwsol_salt = 0
          endif
          !other constituents
          if(cs_db%num_cs > 0) then
            gwsol_cons = 1
            do i=1,cs_db%num_cs
              gw_nsolute = gw_nsolute + 1
              gwsol_nm(gw_nsolute) = cs_db%cs(i)
            enddo
          else
            gwsol_cons = 0
          endif
        else !no constituents specified; only simulate no3 and p
          gwsol_salt = 0
          gwsol_cons = 0
        endif

        !reading gwflow.solutes file
        write(out_gw,*) '     groundwater solute transport preparation...'
        write(out_gw,*) '          groundwater solute being simulated (gwflow.solutes found)'
        !general parameters
        read(in_gw,*) header
        read(in_gw,*) header
        read(in_gw,*) num_ts_transport
        read(in_gw,*) gw_long_disp
        !read in solute parameters
        allocate(canal_out_conc(gw_nsolute))
        canal_out_conc = 0.
        read(in_gw,*) header
        do s=1,gw_nsolute
          read(in_gw,*) name,gwsol_sorb(s),gwsol_rctn(s),canal_out_conc(s)
        enddo
        !read in initial solute concentrations (store in general concentration array)
        !allocate state array
        allocate(gwsol_state(ncell))
        do i=1,ncell
          allocate(gwsol_state(i)%solute(gw_nsolute))
        enddo
        !read in concentrations
        read(in_gw,*) header
        if(grid_type == "structured") then
          !read one array at a time
          do s=1,gw_nsolute
            read(in_gw,*) header
            read(in_gw,*) read_type
            if(read_type == "single") then
              read(in_gw,*) single_value
              grid_val = single_value
            elseif(read_type == "array") then
              do i=1,grid_nrow
                read(in_gw,*) (grid_val(i,j),j=1,grid_ncol)
              enddo
            endif
            do i=1,grid_nrow
              do j=1,grid_ncol
                if(cell_id_usg(i,j) > 0) then
                  gwsol_state(cell_id_usg(i,j))%solute(s)%conc = grid_val(i,j)
                endif
              enddo
            enddo
          enddo
        elseif(grid_type == "unstructured") then
          !read one cell at a time (solutes across columns)
          read(in_gw,*) header
          do i=1,ncell
            read(in_gw,*) (gwsol_state(i)%solute(s)%conc,s=1,gw_nsolute)
          enddo
        endif
        !if salts active: read in salt mineral data (if provided)
        if(gwsol_salt == 1) then
          inquire(file='gwflow.solutes.minerals',exist=gwsol_minl)
          if(gwsol_minl) then
            open(in_gw_minl,file='gwflow.solutes.minerals')
            read(in_gw_minl,*) header
            read(in_gw_minl,*) gw_nminl
            !allocate arrays based on number of salt minerals
            allocate(gwsol_minl_state(ncell))
            do i=1,ncell
              allocate(gwsol_minl_state(i)%fract(gw_nminl))
            enddo
            !read in initial salt minerals fractions
            read(in_gw_minl,*) header
            if(grid_type == "structured") then
            !read one mineral at a time
            do m=1,gw_nminl
              read(in_gw_minl,*) header
              read(in_gw_minl,*) read_type
              if(read_type == "single") then
                read(in_gw_minl,*) single_value
                grid_val = single_value
              elseif(read_type == "array") then
                do i=1,grid_nrow
                  read(in_gw_minl,*) (grid_val(i,j),j=1,grid_ncol)
                enddo
              endif
            enddo
          elseif(grid_type == "unstructured") then
            !read one cell at a time (solutes across columns)
            do i=1,ncell
              read(in_gw_minl,*) (gwsol_minl_state(i)%fract(m),m=1,gw_nminl)
            enddo
          endif
          endif
        endif
        !if constituents active: read in reaction group and shale fractions
        if(gwsol_cons == 1) then
          !allocate solute chem array
          allocate(gwsol_chem(ncell))
          do i=1,ncell
            gwsol_chem(i)%ino3 = 0.
            gwsol_chem(i)%oxyg = 0.
            gwsol_chem(i)%kd_seo4 = 0.
            gwsol_chem(i)%kd_seo3 = 0.
            gwsol_chem(i)%kd_boron = 0.
            gwsol_chem(i)%kseo4 = 0.
            gwsol_chem(i)%bed_o2a= 0.
            gwsol_chem(i)%bed_no3a = 0.
          enddo
          !read reaction group for each cell; use to assign chem values to cells
          read(in_gw,*)
          if(grid_type == "unstructured") then
            allocate(cell_int(ncell))
            read(in_gw,*) (cell_int(i),i=1,ncell)
            do i=1,ncell
              gwsol_chem(i)%ino3 = rct(1,cell_int(i)) !inhibition term
              gwsol_chem(i)%oxyg = rct(3,cell_int(i)) !o2 concentration in groundwater
              gwsol_chem(i)%kd_seo4 = rct(4,cell_int(i)) !seo4 sorption coefficient
              gwsol_chem(i)%kd_seo3 = rct(5,cell_int(i)) !seo3 sorption coefficient
              gwsol_chem(i)%kd_boron = rct(6,cell_int(i)) !boron sorption coefficient
              gwsol_chem(i)%kseo4 = rct(8,cell_int(i)) !seo4 microbial reduction rate constant
              gwsol_chem(i)%kseo3 = rct(10,cell_int(i)) !seo3 microbial reduction rate constant
            enddo
          elseif(grid_type == "structured") then
            do i=1,grid_nrow
              read(in_gw,*) (grid_int(i,j),j=1,grid_ncol)
            enddo
            do i=1,grid_nrow
              do j=1,grid_ncol
                if(cell_id_usg(i,j) > 0) then
                  gwsol_chem(cell_id_usg(i,j))%ino3 = rct(1,grid_int(i,j)) !inhibition term
                  gwsol_chem(cell_id_usg(i,j))%oxyg = rct(3,grid_int(i,j)) !o2 concentration in groundwater
                  gwsol_chem(cell_id_usg(i,j))%kd_seo4 = rct(4,grid_int(i,j)) !seo4 sorption coefficient
                  gwsol_chem(cell_id_usg(i,j))%kd_seo3 = rct(5,grid_int(i,j)) !seo3 sorption coefficient
                  gwsol_chem(cell_id_usg(i,j))%kd_boron = rct(6,grid_int(i,j)) !boron sorption coefficient
                  gwsol_chem(cell_id_usg(i,j))%kseo4 = rct(8,grid_int(i,j)) !seo4 microbial reduction rate constant
                  gwsol_chem(cell_id_usg(i,j))%kseo3 = rct(10,grid_int(i,j)) !seo4 microbial reduction rate constant
                endif
              enddo
            enddo
          endif
          !read number of shale groups, and shale value (0 or 1) for each cell
          do i=1,ncell
            gwsol_chem(i)%nshale = num_geol_shale
            allocate(gwsol_chem(i)%shale(num_geol_shale))
            allocate(gwsol_chem(i)%shale_sseratio(num_geol_shale))
            allocate(gwsol_chem(i)%shale_o2a(num_geol_shale))
            allocate(gwsol_chem(i)%shale_no3a(num_geol_shale))
          enddo
          do n=1,num_geol_shale
            read(in_gw,*) header
            if(grid_type == "unstructured") then
              read(in_gw,*) (cell_int(i),i=1,ncell)
              do i=1,ncell
                if(cell_int(i) > 0) then !shale is present in the cell
                  gwsol_chem(i)%shale(n) = 1
                  gwsol_chem(i)%shale_sseratio(n) = rct_shale(n,1)
                  gwsol_chem(i)%shale_o2a(n) = rct_shale(n,2)
                  gwsol_chem(i)%shale_no3a(n) = rct_shale(n,3)
                else
                  gwsol_chem(i)%shale(n) = 0
                  gwsol_chem(i)%shale_sseratio(n) = 0.
                  gwsol_chem(i)%shale_o2a(n) = 0.
                  gwsol_chem(i)%shale_no3a(n) = 0.
                endif
              enddo
            elseif(grid_type == "structured") then
              do i=1,grid_nrow
                read(in_gw,*) (grid_int(i,j),j=1,grid_ncol)
              enddo
              do i=1,grid_nrow
                do j=1,grid_ncol
                  if(cell_id_usg(i,j) > 0) then
                    if(grid_int(i,j) > 0) then !shale is present in the cell
                      gwsol_chem(cell_id_usg(i,j))%shale(n) = 1
                      gwsol_chem(cell_id_usg(i,j))%shale_sseratio(n) = rct_shale(n,1)
                      gwsol_chem(cell_id_usg(i,j))%shale_o2a(n) = rct_shale(n,2)
                      gwsol_chem(cell_id_usg(i,j))%shale_no3a(n) = rct_shale(n,3)
                    else
                      gwsol_chem(cell_id_usg(i,j))%shale(n) = 0
                      gwsol_chem(cell_id_usg(i,j))%shale_sseratio(n) = 0.
                      gwsol_chem(cell_id_usg(i,j))%shale_o2a(n) = 0.
                      gwsol_chem(cell_id_usg(i,j))%shale_no3a(n) = 0.
                    endif
                  endif
                enddo
              enddo
            endif
          enddo !go to next shale formation
          !read in shale ID for bedrock material underlying the unconfined aquifer
          read(in_gw,*) header
          if(grid_type == "unstructured") then
            read(in_gw,*) (cell_int(i),i=1,ncell)
            do i=1,ncell
              if(cell_int(i) > 0) then !shale is present in the cell
                gwsol_chem(i)%bed_flag = 1
                gwsol_chem(i)%bed_sse = rct_shale(cell_int(i),1)
                gwsol_chem(i)%bed_o2a = rct_shale(cell_int(i),2)
                gwsol_chem(i)%bed_no3a = rct_shale(cell_int(i),3)
              else
                gwsol_chem(i)%bed_flag = 0
                gwsol_chem(i)%bed_sse = 0.
                gwsol_chem(i)%bed_o2a = 0.
                gwsol_chem(i)%bed_no3a = 0.
              endif
            enddo
          elseif(grid_type == "structured") then
            do i=1,grid_nrow
              read(in_gw,*) (grid_int(i,j),j=1,grid_ncol)
            enddo
            do i=1,grid_nrow
              do j=1,grid_ncol
                if(cell_id_usg(i,j) > 0) then
                  if(grid_int(i,j) > 0) then !shale is present in the cell
                    gwsol_chem(cell_id_usg(i,j))%bed_flag = 1
                    gwsol_chem(cell_id_usg(i,j))%bed_sse = rct_shale(grid_int(i,j),1)
                    gwsol_chem(cell_id_usg(i,j))%bed_o2a = rct_shale(grid_int(i,j),2)
                    gwsol_chem(cell_id_usg(i,j))%bed_no3a = rct_shale(grid_int(i,j),3)
                  else
                    gwsol_chem(cell_id_usg(i,j))%bed_flag = 0
                    gwsol_chem(cell_id_usg(i,j))%bed_sse = 0.
                    gwsol_chem(cell_id_usg(i,j))%bed_o2a = 0.
                    gwsol_chem(cell_id_usg(i,j))%bed_no3a = 0.
                  endif
                endif
              enddo
            enddo
          endif
        endif !if constituents are active
        close(in_gw) !close the file
        !copy to initial concentrations
        do i=1,ncell
          do s=1,gw_nsolute !loop through the solutes
            gwsol_state(i)%solute(s)%init = gwsol_state(i)%solute(s)%conc
          enddo
        enddo
        !allocate all mass arrays for sources and sinks
        allocate(gwsol_ss(ncell))
        do i=1,ncell
          allocate(gwsol_ss(i)%solute(gw_nsolute))
        enddo
        !allocate all mass arrays for sums of sources and sinks
        allocate(gwsol_ss_sum(ncell))
        allocate(gwsol_ss_sum_mo(ncell))
        do i=1,ncell
          allocate(gwsol_ss_sum(i)%solute(gw_nsolute))
          allocate(gwsol_ss_sum_mo(i)%solute(gw_nsolute))
        enddo
        !open output files for each source/sink type
        !recharge
        open(out_sol_rech,file='gwflow_cell_sol_rech_yr.txt')
        write(out_sol_rech,*) 'Annual recharge mass (kg/day)'
        open(out_sol_rech_mo,file='gwflow_cell_sol_rech_mon.txt')
        write(out_sol_rech_mo,*) 'Monthly recharge mass (kg/day)'
        allocate(gw_rechsol(sp_ob%hru,gw_nsolute))
        allocate(gwflow_percsol(sp_ob%hru,gw_nsolute))
        gw_rechsol = 0.
        gwflow_percsol = 0.
        !groundwater-channel exchange
        open(out_sol_gwsw,file='gwflow_cell_sol_gwsw_yr.txt')
        write(out_sol_gwsw,*) 'Annual gw-channel exchange mass (kg/day)'
        open(out_sol_gwsw_mo,file='gwflow_cell_sol_gwsw_mon.txt')
        write(out_sol_gwsw_mo,*) 'Monthly gw-channel exchange mass (kg/day)'
        !ground-->soil transfer
        if(gw_soil_flag.eq.1) then
          open(out_sol_soil,file='gwflow_cell_sol_soil_yr.txt')
          write(out_sol_soil,*) 'Annual groundwater-->soil mass transfer (kg/day)'
          open(out_sol_soil_mo,file='gwflow_cell_sol_soil_mon.txt')
          write(out_sol_soil_mo,*) 'Monthly groundwater-->soil mass transfer (kg/day)'
        endif
        allocate(hru_soil(sp_ob%hru,20,gw_nsolute))
        hru_soil = 0.
        !saturation excess flow
        if(gw_satx_flag.eq.1) then
          open(out_sol_satx,file='gwflow_cell_sol_satx_yr.txt')
          write(out_sol_satx,*) 'Annual saturation excess flow mass (kg/day)'
          open(out_sol_satx_mo,file='gwflow_cell_sol_satx_mon.txt')
          write(out_sol_satx_mo,*) 'Monthly saturation excess flow mass (kg/day)'
        endif
        !irrigation pumping
        open(out_sol_ppag,file='gwflow_cell_sol_ppag_yr.txt')
        write(out_sol_ppag,*) 'Annual mass in pumping (kg/day) (irrigation)'
        open(out_sol_ppag_mo,file='gwflow_cell_sol_ppag_mon.txt')
        write(out_sol_ppag_mo,*) 'Monthly mass in pumping (kg/day) (irrigation)'
        !specified pumping
        if(gw_pumpex_flag == 1) then
          open(out_sol_ppex,file='gwflow_cell_sol_ppex_yr.txt')
          write(out_sol_ppex,*) 'Annual mass in pumping (kg/day) (specified)'
          open(out_sol_ppex_mo,file='gwflow_cell_sol_ppex_mon.txt')
          write(out_sol_ppex_mo,*) 'Monthly mass in pumping (kg/day) (specified)'
        endif
        !tile drainage
        if(gw_tile_flag == 1) then
          open(out_sol_tile,file='gwflow_cell_sol_tile_yr.txt')
          write(out_sol_tile,*) 'Annual mass in tile flow (kg/day)'
          open(out_sol_tile_mo,file='gwflow_cell_sol_tile_mon.txt')
          write(out_sol_tile_mo,*) 'Monthly mass in tile flow (kg/day)'
        endif
        !reservoir
        if(gw_res_flag == 1) then
          open(out_sol_resv,file='gwflow_cell_sol_resv_yr.txt')
          write(out_sol_resv,*) 'Annual groundwater-reservoir exchange mass (kg/day)'
          open(out_sol_resv_mo,file='gwflow_cell_sol_resv_mon.txt')
          write(out_sol_resv_mo,*) 'Monthly groundwater-reservoir exchange mass (kg/day)'
        endif
        !wetland exchange
        if(gw_wet_flag == 1) then
          open(out_sol_wetl,file='gwflow_cell_sol_wetl_yr.txt')
          write(out_sol_wetl,*) 'Annual groundwater-wetland exchange mass (kg/day)'
          open(out_sol_wetl_mo,file='gwflow_cell_sol_wetl_mon.txt')
          write(out_sol_wetl_mo,*) 'Monthly groundwater-wetland exchange mass (kg/day)'
        endif
        !floodplain exchange
        if(gw_fp_flag == 1) then
          open(out_sol_fpln,file='gwflow_cell_sol_fpln_yr.txt')
          write(out_sol_fpln,*) 'Annual groundwater-floodplain exchange mass (kg/day)'
          open(out_sol_fpln_mo,file='gwflow_cell_sol_fpln_mon.txt')
          write(out_sol_fpln_mo,*) 'Monthly groundwater-floodplain exchange mass (kg/day)'
        endif
        !canal seepage
        if(gw_canal_flag == 1) then
          open(out_sol_canl,file='gwflow_cell_sol_canl_yr.txt')
          write(out_sol_canl,*) 'Annual groundwater-canal exchange mass (kg/day)'
          open(out_sol_canl_mo,file='gwflow_cell_sol_canl_mon.txt')
          write(out_sol_canl_mo,*) 'Monthly groundwater-canal exchange mass (kg/day)'
        endif
        !chemical reactions (inputs and outputs)
        if(gw_solute_flag == 1) then !solute mass flux
          open(out_sol_rcti,file='gwflow_cell_sol_rcti_yr.txt')
          write(out_sol_rcti,*) 'Annual mass (kg/day) of input reactions'
          open(out_sol_rcti_mo,file='gwflow_cell_sol_rcti_mon.txt')
          write(out_sol_rcti_mo,*) 'Monthly mass (kg/day) of input reactions'
          open(out_sol_rcto,file='gwflow_cell_sol_rcto_yr.txt')
          write(out_sol_rcto,*) 'Annual mass (kg/day) of output reactions'
          open(out_sol_rcto_mo,file='gwflow_cell_sol_rcto_mon.txt')
          write(out_sol_rcto_mo,*) 'Monthly mass (kg/day) of output reactions'
        endif
        !open file for writing observation values; allocate arrays for observation cells
        open(out_gwobs_sol,file='gwflow_cell_obs_conc_day.txt')
        write(out_gwobs_sol,*) 'Daily solute concentration (mg/L) values for observation wells'
        write(out_gwobs_sol,123) 'cell:',(gw_obs_cells(k),k=1,gw_num_obs_wells)
        write(out_gwobs_sol,*)
        allocate(gw_obs_solute(gw_num_obs_wells,gw_nsolute))
        gw_obs_solute = 0.
      else
        gw_solute_flag = 0 !turn off transport option
      endif
      endif !end solute transport


      !recharge pond seepage ----------------------------------------------------------------------
      !(do this here, because depends on the number of groundwater solutes)
      gw_pond_flag = 0
      inquire(file='gwflow.ponds',exist=i_exist)
      if(i_exist) then
        gw_pond_flag = 1
        !number of days in each month
        month_days(1) = 31
        month_days(2) = 28
        month_days(3) = 31
        month_days(4) = 30
        month_days(5) = 31
        month_days(6) = 30
        month_days(7) = 31
        month_days(8) = 31
        month_days(9) = 30
        month_days(10) = 31
        month_days(11) = 30
        month_days(12) = 31
        open(in_ponds,file='gwflow.ponds')
        read(in_ponds,*) header
        read(in_ponds,*) gw_npond
        allocate(gw_pond_info(gw_npond))
        !read in the attributes of each recharge pond
        read(in_ponds,*) header
        do i=1,gw_npond
          allocate(gw_pond_info(i)%unl_conc(gw_nsolute))
          read(in_ponds,*) gw_pond_info(i)%id, &
                           gw_pond_info(i)%area, &
                           gw_pond_info(i)%chan, &
                           gw_pond_info(i)%canal, &
                           gw_pond_info(i)%unl, &
                           gw_pond_info(i)%bed_k, &
                           gw_pond_info(i)%ncell, &
                           gw_pond_info(i)%wsta, &
                           gw_pond_info(i)%evap_co, &
                           yr_start,mo_start,dy_start, &
                           (gw_pond_info(i)%unl_conc(j),j=1,gw_nsolute)
          !determine starting day for each recharge pond
          if(yr_start < time%yrc) then
            num_dy = 1
          else
            num_dy = (yr_start-time%yrc) * 365
            do j=1,mo_start-1
              num_dy = num_dy + month_days(j)
            enddo
            num_dy = num_dy + dy_start
          endif
          gw_pond_info(i)%dy_start = num_dy
          allocate(gw_pond_info(i)%cells(gw_pond_info(i)%ncell))
          allocate(gw_pond_info(i)%conn_area(gw_pond_info(i)%ncell))
          allocate(gw_pond_info(i)%sol_mass(gw_nsolute))
          allocate(gw_pond_info(i)%sol_conc(gw_nsolute))
          do j=1,gw_nsolute
            gw_pond_info(i)%sol_mass(j) = 0.
            gw_pond_info(i)%sol_conc(j) = 0.
          enddo
        enddo
        !read in the connection information between recharge ponds and cells
        read(in_ponds,*)
        read(in_ponds,*)
        do i=1,gw_npond
          do j=1,gw_pond_info(i)%ncell
            read(in_ponds,*) dum_id,cell_num,gw_pond_info(i)%conn_area(j)
            if(grid_type == "structured") then
              cell_num = cell_id_list(cell_num)
            endif
            gw_pond_info(i)%cells(j) = cell_num
          enddo
        enddo !go to next recharge pond
        read(in_ponds,*) header
        read(in_ponds,*) header
        !flux output file
        open(out_gw_pond,file='gwflow_cell_wb_pond_yr.txt')
        write(out_gw_pond,*) 'Annual recharge pond seepage (m3/day)'
        open(out_gw_pond_mo,file='gwflow_cell_wb_pond_mon.txt')
        write(out_gw_pond_mo,*) 'Monthly recharge pond seepage (m3/day)'
        !pond water balance file
        open(out_pond_bal,file='gwflow_pond_water_balance')
        write(out_pond_bal,*) 'daily water balance for recharge ponds'
        write(out_pond_bal,*)
        write(out_pond_bal,*) 'area:      m2   recharge pond surface area'
        write(out_pond_bal,*) 'stor:      m3   recharge pond water storage'
        write(out_pond_bal,*) 'rain:      m3   rain added to recharge pond'
        write(out_pond_bal,*) 'div_added: m3   diversion water added to recharge pond'
        write(out_pond_bal,*) 'evap:      m3   evaporation from recharge pond'
        write(out_pond_bal,*) 'recharge:  m3   recharge from pond to aquifer'
        write(out_pond_bal,*) 'div_spec:  m3   specified diversion water'
        write(out_pond_bal,*) 'div_uns:   m3   unsatisfied diversion water'
        write(out_pond_bal,*)
        gwflow_hdr_pond = (/"year","month","day","pond_id","area","stor","rain","div_add","evap","recharge","div_spec","div_uns"/)
        write(out_pond_bal,131) (gwflow_hdr_pond(j),j=1,12)
        !pond solute mass balance file
        open(out_pond_sol,file='gwflow_pond_mass_balance')
        write(out_pond_sol,*) 'daily solute mass balance for recharge ponds'
        write(out_pond_sol,*)
        write(out_pond_sol,*) 'area:      m2   recharge pond surface area'
        write(out_pond_sol,*) 'stor:      m3   recharge pond water storage'
        write(out_pond_sol,*) 'solute:    --   name of solute'
        write(out_pond_sol,*) 'mass:      kg   recharge pond solute mass storage'
        write(out_pond_sol,*) 'div_added: kg   solute mass added to recharge pond via diversion'
        write(out_pond_sol,*) 'recharge:  m3   solute mass leaching from pond to aquifer'
        write(out_pond_sol,*)
        gwflow_hdr_pond_sol = (/"year","month","day","pond_id","area","stor","solute","mass","div_add","recharge"/)
        write(out_pond_sol,131) (gwflow_hdr_pond_sol(j),j=1,10)
        !pond mass for each day
        open(out_pond_mass,file='gwflow_pond_daily_mass')
        write(out_pond_mass,*) 'daily mass (kg) in each recharge pond'
        write(out_pond_mass,*) 'written in order of ponds (see gwflow.ponds)'
        write(out_pond_mass,*)
        !pond concentration for each day
        open(out_pond_conc,file='gwflow_pond_daily_conc')
        write(out_pond_conc,*) 'daily concentration (g/m3) in each recharge pond'
        write(out_pond_conc,*) 'written in order of ponds (see gwflow.ponds)'
        write(out_pond_conc,*)
      endif


      !read in connection information between SWAT+ objects (LSUs or HRUs) and grid cells  ------------------------------------------------
      !if LSU-cell connection is active (i.e., file is provided), it supercedes HRU-cell connection
      write(out_gw,*)
      write(out_gw,*) '     read and prepare connection (HRU-cell or LSU-cell)'
      if(lsu_cells_link) then
        write(out_gw,*) '          LSU-cell connections (gwflow.lsucell)'
        open(in_lsu_cell,file='gwflow.lsucell')
        read(in_lsu_cell,*) header
        !read in list of LSUs that are spatially connected to grid cells
        read(in_lsu_cell,*) nlsu !number of LSUs in the model
        read(in_lsu_cell,*) nlsu_connected !number of LSUs spatially connected to grid cells
        allocate(lsus_connected(nlsu))
        lsus_connected = 0
        do i=1,nlsu_connected
          read(in_lsu_cell,*) lsu_id
          lsus_connected(lsu_id) = 1
        enddo
        !read in the LSU-cell connection information
        read(in_lsu_cell,*) header
        read(in_lsu_cell,*) header
        allocate(lsu_num_cells(nlsu))
        allocate(lsu_cells(nlsu,5000))
        allocate(lsu_cells_fract(nlsu,5000))
        lsu_num_cells = 0
        lsu_cells = 0
        lsu_cells_fract = 0.
        do k=1,nlsu
          if(lsus_connected(k).eq.1) then
          lsu = k
          cell_count = 0
          do while (lsu.eq.k)
            cell_count = cell_count + 1
            read(in_lsu_cell,*) lsu,lsu_area,lsu_cells(k,cell_count),poly_area
            if(grid_type == "structured") then
              if(cell_id_list(lsu_cells(k,cell_count)) > 0) then
                lsu_cells(k,cell_count) = cell_id_list(lsu_cells(k,cell_count))
                lsu_cells_fract(k,cell_count) = poly_area / lsu_area
              endif
            else
              lsu_cells_fract(k,cell_count) = poly_area / lsu_area
            endif
            read(in_lsu_cell,*,end=25) lsu
            backspace(in_lsu_cell)
          enddo
  25      lsu_num_cells(k) = cell_count
          endif
        enddo

      else !LSU-cell connection not present; proceed with HRU-cell connection

      !HRU-cell connection
      write(out_gw,*) '          HRU-cell connections (gwflow.hrucell)'
      open(in_hru_cell,file='gwflow.hrucell')
      read(in_hru_cell,*)
      read(in_hru_cell,*)
      read(in_hru_cell,*)
      !read in list of HRUs that are spatially connected to grid cells
      read(in_hru_cell,*) nhru_connected !number of HRUs spatially connected to grid cells
      allocate(hrus_connected(sp_ob%hru))
      hrus_connected = 0
      do i=1,nhru_connected
        read(in_hru_cell,*) hru_id
        hrus_connected(hru_id) = 1
      enddo
      !read in the HRU-cell connection information
      read(in_hru_cell,*)
      read(in_hru_cell,*)
      allocate(hru_num_cells(sp_ob%hru))
      allocate(hru_cells(sp_ob%hru,20000))
      allocate(hru_cells_fract(sp_ob%hru,20000))
      allocate(cells_fract(sp_ob%hru,20000))
      hru_num_cells = 0
      hru_cells = 0
      hru_cells_fract = 0.
      do k=1,sp_ob%hru
        if(hrus_connected(k).eq.1) then
        hru_id = k
        cell_count = 0
        do while (hru_id.eq.k)
          cell_count = cell_count + 1
          read(in_hru_cell,*) hru_id,hru_area,hru_cells(k,cell_count),poly_area
          if(grid_type == "structured") then
            if(cell_id_list(hru_cells(k,cell_count)) > 0) then
              hru_cells(k,cell_count) = cell_id_list(hru_cells(k,cell_count))
              hru_cells_fract(k,cell_count) = poly_area / hru_area !fraction of HRU area
              cells_fract(k,cell_count) = poly_area / gw_state(hru_cells(k,cell_count))%area !fraction of cell area
            else
              cell_count = cell_count - 1
            endif
          else
            hru_cells_fract(k,cell_count) = poly_area / hru_area !fraction of HRU area
            cells_fract(k,cell_count) = poly_area / gw_state(hru_cells(k,cell_count))%area !fraction of cell area
          endif
          read(in_hru_cell,*,end=10) hru_id
          backspace(in_hru_cell)
        enddo
10      hru_num_cells(k) = cell_count
        endif
      enddo

      !read Cell-HRU connection information (gwflow.cellhru) -- optional
      inquire(file='gwflow.cellhru',exist=i_exist)
      if(i_exist) then
      write(out_gw,*) '          HRU-cell connections (gwflow.cellhru)'
      allocate(cell_num_hrus(ncell), source = 0)
      allocate(cell_hrus(ncell,100), source = 0)
      allocate(cell_hrus_fract(ncell,100), source = 0.)
      open(in_cell_hru,file='gwflow.cellhru')
      read(in_cell_hru,*)
      read(in_cell_hru,*)
      read(in_cell_hru,*) num_unique !number of cells that intersect HRUs
      read(in_cell_hru,*)
      do k=1,num_unique
        read(in_cell_hru,*) hru_cell
        if(grid_type == "structured") then
          hru_cell = cell_id_list(hru_cell)
        endif
        cell_num = hru_cell
        backspace(in_cell_hru)
        hru_count = 0
        do while (cell_num.eq.hru_cell)
          hru_count = hru_count + 1
          read(in_cell_hru,*) cell_num,hru_id,cell_area,poly_area
          if(grid_type == "structured") then
            cell_num = cell_id_list(cell_num)
          endif
          cell_hrus(cell_num,hru_count) = hru_id
          cell_hrus_fract(cell_num,hru_count) = poly_area / cell_area
          read(in_cell_hru,*,end=30) cell_num
          if(grid_type == "structured") then
            cell_num = cell_id_list(cell_num)
          endif
          backspace(in_cell_hru)
        enddo
30      cell_num_hrus(cell_num) = hru_count
      enddo
      else
        write(out_gw,*) '          gwflow.cellhru not found; cell-HRU reverse mapping not available'
      endif !check for gwflow.cellhru

      endif !check for LSU-cell connection




      !output file initialization (extracted to gwflow_output.f90)
      call gwflow_output_init

      !prepare additional arrays for start of simulation ----------------------------------------------------

      !map cell groundwater delay terms to each HRU
      !default value (days) = average of all grid cell values
      write(out_gw,*)
      write(out_gw,*) '     final preparations...'
      write(out_gw,*) '          map recharge delay to HRUs'
      allocate(gw_delay(sp_ob%hru))
      allocate(gw_rech(sp_ob%hru))
      sum = 0.
      do i=1,ncell
        sum = sum + delay(i)
      enddo
      gw_delay = Exp(-1./((sum/(ncell)) + 1.e-6))
      !assign value to HRU based on connected grid cells
      if(lsu_cells_link == 0) then !if LSU-cell connection: keep average of all grid cell values
        do k=1,sp_ob%hru
          do i=1,hru_num_cells(k)
            cell_num = hru_cells(k,i)
            gw_delay(k) = Exp(-1./(delay(cell_num) + 1.e-6))
          enddo
        enddo
      endif
      gw_rech = 0.

      !set groundwater head to initial head, for each grid cell
      do i=1,ncell
        gw_state(i)%head = gw_state(i)%init
      enddo

      !set solute mass for each grid cell
      if(gw_solute_flag == 1) then
        do i=1,ncell
          if(gw_state(i)%stat.gt.0) then
            if(gw_state(i)%head > gw_state(i)%botm) then
              gw_cell_volume = gw_state(i)%area * (gw_state(i)%head-gw_state(i)%botm) * gw_state(i)%spyd !m3 of groundwater
            else
              gw_cell_volume = 0.
            endif
            do s=1,gw_nsolute !loop through solutes
              gwsol_state(i)%solute(s)%mass = gw_cell_volume * gwsol_state(i)%solute(s)%conc !m3 * g/m3 = g
            enddo
          endif
        enddo
      endif

      !prepare output files for groundwater head (at output times)
      open(out_gwheads,file='gwflow_cell_head_specified.txt')
      write(out_gwheads,*) 'Initial head values (m)'
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
          write(out_gwheads,130) (grid_val(i,j),j=1,grid_ncol)
        enddo
      else
        write(out_gwheads,101) (gw_state(i)%head,i=1,ncell)
      endif
      write(out_gwheads,*)
      !heat
      if(gw_heat_flag == 1) then
        open(out_gwtemps,file='gwflow_cell_temp_specified.txt')
        write(out_gwtemps,*) 'Initial groundwater temperature (deg C)'
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gwheat_state(cell_id_usg(i,j))%temp
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_gwtemps,130) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_gwtemps,101) (gwheat_state(i)%temp,i=1,ncell)
        endif
        write(out_gwtemps,*)
      endif
      !solutes
      if(gw_solute_flag == 1) then
        open(out_gwconc,file='gwflow_cell_conc_specified.txt')
        write(out_gwconc,*) 'Initial concentration values (mg/L)'
        do s=1,gw_nsolute !loop through the solutes
          write(out_gwconc,*) 'solute:',gwsol_nm(s)
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
              write(out_gwconc,130) (grid_val(i,j),j=1,grid_ncol)
            enddo
          else
            write(out_gwconc,101) (gwsol_state(i)%solute(s)%conc,i=1,ncell)
          endif
        enddo
        write(out_gwconc,*)
      endif

      !prepare output files and arrays for monthly and annual average groundwater head
      write(out_gw,*) '          prepare output files for monthly and annual average groundwater head'
      open(out_head_mo,file='gwflow_cell_head_mon.txt')
      open(out_head_yr,file='gwflow_cell_head_yr.txt')
      write(out_head_mo,*) 'Monthly average groundwater head (m) for each grid cell'
      write(out_head_mo,*)
      write(out_head_yr,*) 'Annual average groundwater head (m) for each grid cell'
      write(out_head_yr,*)

      !prepare output files and arrays for monthly and annual average groundwater temperature
      if(gw_heat_flag == 1) then
        write(out_gw,*) '          prepare output files for monthly and annual average groundwater temperature'
        open(out_temp_mo,file='gwflow_cell_temp_mon.txt')
        open(out_temp_yr,file='gwflow_cell_temp_yr.txt')
        write(out_temp_mo,*) 'Monthly average groundwater temperature (deg C) for each grid cell'
        write(out_temp_mo,*)
        write(out_temp_yr,*) 'Annual average groundwater temperature (deg C) for each grid cell'
        write(out_temp_yr,*)
      endif

      !prepare output file and arrays for monthly and annual average solute concentration
      if(gw_solute_flag == 1) then
        write(out_gw,*) '          prepare output files for monthly and annual average solute conc.'
        open(out_conc_mo,file='gwflow_cell_conc_mon.txt')
        open(out_conc_yr,file='gwflow_cell_conc_yr.txt')
        write(out_conc_mo,*) 'Monthly average solute concentration (g/m3) for each grid cell'
        write(out_conc_mo,*)
        write(out_conc_yr,*) 'Annual average solute concentration (g/m3) for each grid cell'
        write(out_conc_yr,*)
      endif


      !read in cells for groundwater transit time output
      if(grid_type == "structured") then !only for structured grids
      inquire(file='gwflow.transit',exist=i_exist)
      if(i_exist) then
        gw_ttime = 1
        !allocate arrays
        allocate(gw_transit(ncell))
        do i=1,ncell
          gw_transit(i)%x = gw_state(i)%xcrd !start at cell centroid
          gw_transit(i)%y = gw_state(i)%ycrd !start at cell centroid
          gw_transit(i)%t = 0.
          gw_transit(i)%cell = i !tracks cell where groundwater is located (at current time step)
          gw_transit(i)%t_chan = 0. !time for groundwater to reach channel
        enddo
        !read in cells for output
        open(in_transit_time,file='gwflow.transit')
        read(in_transit_time,*) header
        read(in_transit_time,*) header
        read(in_transit_time,*) gw_transit_num
        allocate(gw_transit_cells(gw_transit_num))
        do i=1,gw_transit_num
          read(in_transit_time,*) cell_transit
          if(grid_type == "structured") then
            gw_transit_cells(i) = cell_id_list(cell_transit)
          endif
        enddo
        close(in_transit_time)
        !open up output file
        open(out_gw_transit,file='gwflow_transit_cell')
        write(out_gw_transit,*) 'groundwater location and transit times'
        write(out_gw_transit,*) 'results (t, x, y, cell) for each cell listed in gwflow.transit'
        write(out_gw_transit,*) 't = days'
        write(out_gw_transit,*) 'x = meters'
        write(out_gw_transit,*) 'y = meters'
        write(out_gw_transit,*) 'cell = current location'
        !prepare: transit time to channels
        open(out_gw_transit_chan,file='gwflow_transit_chan')
        write(out_gw_transit_chan,*) 'time (days) to a channel for each grid cell'
        allocate(gw_cell_chan_time(ncell))
        allocate(gw_cell_chan_flag(ncell)) !array: 0=no channel; 1=channel is present in cell
        gw_cell_chan_time = 0.
        gw_cell_chan_flag = 0
        do i=1,sp_ob%gwflow
          gw_cell_chan_flag(gw_chan_cell(i)) = 1
        enddo
        if(gw_tile_flag == 1) then
          open(out_gw_transit_tile,file='gwflow_transit_tile')
          write(out_gw_transit_tile,*) 'time (days) to a tile for each grid cell'
          allocate(gw_cell_tile_time(ncell))
          gw_cell_tile_time = 0.
        endif
      endif
      endif


      !read in groups of channel cells, to write out daily reach gw-channel exchange
      inquire(file='gwflow.gwsw_groups',exist=i_exist)
      if(i_exist) then
        gw_gwsw_group_flag = 1
        open(1235,file='gwflow.gwsw_groups')
        read(1235,*)
        read(1235,*)
        !read cell groups
        read(1235,*)
        read(1235,*) gw_gwsw_ngroup
        read(1235,*) gw_gwsw_max
        allocate(gw_gwsw_ncell(gw_gwsw_ngroup))
        allocate(gw_gwsw_group(gw_gwsw_ngroup,gw_gwsw_max))
        gw_gwsw_group = 0
        do i=1,gw_gwsw_ngroup
          read(1235,*)
          read(1235,*) gw_gwsw_ncell(i)
          read(1235,*)
          do j=1,gw_gwsw_ncell(i)
            read(1235,*) gw_gwsw_group(i,j)
            if(grid_type == "structured") then
              gw_gwsw_group(i,j) = cell_id_list(gw_gwsw_group(i,j))
            endif
          enddo
        enddo
        open(out_gwsw_groups,file='gwflow_gwsw_groups')
        write(out_gwsw_groups,*) 'daily volume (m3) of aquifer-channel exchange'
        write(out_gwsw_groups,*) 'summed for each cell group, as specified in gwflow.gwsw_groups'
        write(out_gwsw_groups,*)
        write(out_gwsw_groups,*) 'year,month,day,volume...'
        close(1235)
      endif

      !read in channel cells for daily output (flow, nutrient mass)
      inquire(file='gwflow.chancells_obs',exist=i_exist)
      if(i_exist) then
        gw_chan_obs_flag = 1
        open(1235,file='gwflow.chancells_obs')
        read(1235,*) header
        read(1235,*) header
        read(1235,*) gw_chan_nobs
        read(1235,*) header
        allocate(gw_chan_obs_cell(gw_chan_nobs))
        gw_chan_obs_cell = 0.
        do i=1,gw_chan_nobs
          read(1235,*) cell_id
          if(grid_type == "structured") then
            gw_chan_obs_cell(i) = cell_id_list(cell_id)
          endif
        enddo
        close(1235)
        open(out_gwsw_chanobs_flow,file='gwflow_chan_obs_flow')
        write(out_gwsw_chanobs_flow,*) 'daily volume (m3) of aquifer-channel exchange'
        write(out_gwsw_chanobs_flow,*) 'negative values = groundwater contribution to channel'
        write(out_gwsw_chanobs_flow,*) 'year,month,day,volume (same order as in gwflow.chancells_obs...)'
        if(gw_solute_flag == 1) then
          open(out_gwsw_chanobs_no3,file='gwflow_chan_obs_no3')
          write(out_gwsw_chanobs_no3,*) 'daily nitrate mass (g) of aquifer-channel exchange'
          write(out_gwsw_chanobs_no3,*) 'negative values = groundwater contribution to channel'
          write(out_gwsw_chanobs_no3,*) 'year,month,day,volume (same order as in gwflow.chancells_obs...)'
        endif
      endif

      !prepare hydrograph separation array
      allocate(hydsep_flag(sp_ob%chandeg))
      hydsep_flag = 0
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
      hydsep_hdr = (/"  year","   day","channel","chan_surf","chan_lat","chan_gwsw","chan_swgw","chan_satexgw","chan_satexsw","chan_tile"/)
      write(out_hyd_sep,121) (hydsep_hdr(j),j=1,10)

      !gwflow record file (skip line)
      write(out_gw,*)
      write(out_gw,*)
      write(out_gw,*) 'record of daily gwflow use...'
      write(out_gw,*)



      open(225577,file='gwflow_heat_cell_balance')




      return


100   format(i6,i6,10(f10.2))
      !output files for all cells
101   format(<out_cols>(f12.4))
102   format(<out_cols>(i4))
      !other formats
103   format(10000(i8))
111   format(1x,a, 5x,"Time",2x,i2,":",i2,":",i2)
119   format(4x,a8,a8,a10,a16,a19,50(a13))
120   format(a8,7x,50(a13))
121   format(50(a16))
122   format(a20,50(a13))
123   format(a10,1000(i12))
130   format(10000(f12.3))
131   format(2x,a8,a8,a8,a8,1x,50(a15))
132   format(4x,a8,a8,50(a13))
133   format(4x,a8,a8,a10,a16,a19,50(a18))

      end subroutine gwflow_read


