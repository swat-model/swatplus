
      !This subroutine performs the following operations:
      !  1. Read in grid cell information
      !  2. Prepare the grid cells for the groundwater model
      !  3. Connect SWAT+ objects to grid cells
  
      !  Prepared by: Ryan Bailey, Colorado State University (beginning 2020)
  
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
      
      character*10 b(3) 
      !water balance and solute balance output file headers
      character(len=13) :: gwflow_hdr(19)
      character(len=13) :: gwflow_hdr_day(24)
      character(len=13) :: gwflow_hdr_yr(18)
      character(len=13) :: gwflow_hdr_aa(18)
      character(len=13) :: gwflow_hdr_huc12(16)
      character(len=13) :: gwflow_hdr_huc12_mo(18)
      character(len=13) :: sol_hdr_day(24)
      character(len=13) :: sol_hdr_yr(20)
      character(len=13) :: sol_hdr_aa(20)
      character(len=16) :: hydsep_hdr(10)
      !general variables
      character(len=13) :: header
      character(len=30) :: read_type
      character*100 file_name
      character(len=13) :: cs_names(20),name
      logical  i_exist                !          |
      logical  i_exist2               !          |
      integer  date_time(8)  
      integer :: i                    !          |
      integer :: j                    !          |
      integer :: k                    !          |
      integer :: m                    !          |
      integer :: n                    !          |
      integer :: s                    !          |
      integer :: count                !          |
      integer :: cell_num             !          |id of cell (in input files)
      integer :: sol_index            !          |index to keep track of number of solutes
      integer :: channel              !          |
      integer :: chan_cell            !          |
      integer :: ob_num               !          |
      integer :: active_cell          !          |
      real :: cell_size               !          |size of cells in structured grid
      real :: x_coord                 !          |variable to track x coordinate of cell in structured grid
      real :: y_coord                 !          |variable to track y coordinate of cell in structured grid
      integer :: num_conn             !          |number of cells connected to cell, in structured grid
      real :: sum                     !          |
      real :: dist_x                  !          |
      real :: dist_y                  !          |
      real :: min_dist                !          |
      real :: distance                !          |
      real :: gw_cell_volume          !          |
      !input file numbers
      integer :: in_gw                !          |
      integer :: in_hru_cell          !          |
      integer :: in_cell_hru          !          |
      integer :: in_huc_cell          !          |
      integer :: in_res_cell          !          |
      integer :: in_canal_cell        !          |
      integer :: in_gw_minl           !          |
      !aquifer and streambed properties
      integer :: K_zone               !          |
      integer :: Sy_zone              !          |
      integer :: nzones_aquK          !          |
      integer :: nzones_aquSy         !          |
      integer :: nzones_strK          !          |
      integer :: nzones_strbed        !          |
      real, dimension (:), allocatable :: zones_aquK
      real, dimension (:), allocatable :: zones_aquSy
      real, dimension (:), allocatable :: zones_strK
      real, dimension (:), allocatable :: zones_strbed
      !external pumping information
      integer :: pumpex_cell
      !reservoir information
      integer :: res_cell             !          |
      integer :: res_id               !          |
      real :: res_stage               !          |
      !canal information
      integer :: canal_out(5000)      !          |flag (0,1) indicating if canal receives water from outside the model domain
      integer :: day_beg              !          |beginning day (of year)  of active canals
      integer :: day_end              !          |ending day (of year) of active canals
      integer :: canal     !          |id of canal that intersects grid cells
      real :: thick                   !m         | |thickness of canal bed sediments
      real :: depth                   !m         |depth of canal water
      real :: width                   !m         |width of canal
      real :: length                  !m         |length of canal in cell
      real :: stage                   !m         |stage of canal
      !HRU-cell, LSU-cell linkage
      integer :: num_unique           !          |
      integer :: cell                 !          |
      integer :: hru_count            !          |
      integer :: hru_cell             !          |
      integer :: nhru_connected       !          |
      integer :: num_hru              !          |
      real :: hru_area                !          |
      integer :: hru_id               !          |
      integer :: lsu                  !          |
      integer :: nlsu                 !          |
      integer :: nlsu_connected       !          |
      integer :: lsu_id               !          |
      integer :: cell_count           !          |
      real :: poly_area               !          |
      real :: cell_area               !          |
      real :: lsu_area                !          |
      !testing with usgs data
      integer :: in_usgs_head         !          |
      integer :: num_usgs_wells       !          |
      integer :: in_str_obs           !          |
      real :: head_vals(101)          !          |
      real :: usgs_lat                !          |
      real :: usgs_long               !          |
      real(8) :: usgs_site_id         !          |
      !national model variables
      integer :: huc12_connect(1000)  !          |huc12 catchments that are in connection with cells
      real(8) :: huc12_id             !          |
      real(8) :: huc12_dum            !          |
      !dummy variables for reading
      integer :: dum,dum1,dum2,dum3,dum7,dum8
      real :: dum4,dum5,dum6
      real :: single_value

      
      

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
      in_huc_cell = 1233
      in_usgs_head = 1234
      in_str_obs = 1235
      in_res_cell = 1236
      in_canal_cell = 1237
      in_hru_pump_obs = 1238
      in_lsu_cell = 1229
      in_gw_minl = 1220
      
      !number of HRUs in the simulation
      num_hru = sp_ob%hru

     
      !read in gwflow module information from gwflow.input --------------------------------------------------------------------------------
      open(in_gw,file='gwflow.input')
      read(in_gw,*) header
      read(in_gw,*) header
      
      !basic information
      write(out_gw,*) '     reading basic information...'
      read(in_gw,*) grid_type                       !structured or unstructured
      if (grid_type == "structured") then
        read(in_gw,*) cell_size                     !area (m2) of each grid cell
        read(in_gw,*) grid_nrow,grid_ncol           !number of rows and columns in the gwflow grid
      else if (grid_type == "unstructured") then
        read(in_gw,*) ncell                         !number of gwflow cells
      endif
      read(in_gw,*) bc_type                         !boundary condition type
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
      read(in_gw,*) gwflag_day,gwflag_yr,gwflag_aa  !flags for writing balance files
      read(in_gw,*) out_cols                        !number of columns in output files
      
      
      !check connections (HRU-cell or LSU-cell) -----------------------------------------------------------------------
      write(out_gw,*) '     checking for connection (HRU, LSU) files...'
      if (conn_type == 1) then !HRU-cell
        inquire(file='gwflow.hrucell',exist=i_exist)
        if (i_exist) then
          hru_cells_link = 1
          lsu_cells_link = 0
          write(out_gw,*) '          found gwflow.hrucell: proceed'
        else
          hru_cells_link = 0
          inquire(file='gwflow.lsucell',exist=i_exist) !try LSU-cell connection instead
          if (i_exist) then
            lsu_cells_link = 1
            gw_soil_flag = 0 !gw-->soil transfer can occur only for HRU-cell connection
            gw_wet_flag = 0 !wetland transfer can occur only for HRU-cell connection
            write(out_gw,*) '          gwflow.hrucell not found: using gwflow.lsucell'
            write(out_gw,*) '          gwflow.lsucell: gw-->soil transfer not simulated'
            write(out_gw,*) '          gwflow.lsucell: gw-->wetland transfer not simulated'
          endif
        endif
      elseif (conn_type == 2) then !LSU-cell
        inquire(file='gwflow.lsucell',exist=i_exist)
        if (i_exist) then
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
          if (i_exist) then
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
      allocate(zones_aquK(nzones_aquK))
      do i=1,nzones_aquK
        read(in_gw,*) dum,zones_aquK(i)
      enddo
      
      !aquifer specific yield
      write(out_gw,*) '          reading aquifer specific yield'
      read(in_gw,*) header
      read(in_gw,*) nzones_aquSy
      allocate(zones_aquSy(nzones_aquSy))
      do i=1,nzones_aquSy
        read(in_gw,*) dum,zones_aquSy(i)
      enddo
      
      !streambed hydraulic conductivity (m/day)
      write(out_gw,*) '          reading streambed hydraulic conductivity'
      read(in_gw,*) header
      read(in_gw,*) nzones_strK
      allocate(zones_strK(nzones_strK))
      do i=1,nzones_strK
        read(in_gw,*) dum,zones_strK(i)
      enddo
      
      !streambed thickness (m)
      write(out_gw,*) '          reading streambed thickness'
      read(in_gw,*) header
      read(in_gw,*) nzones_strbed
      allocate(zones_strbed(nzones_strbed))
      do i=1,nzones_strbed
        read(in_gw,*) dum,zones_strbed(i)
      enddo

      

      !grid cell information ------------------------------------------------------------------------------------------
      write(out_gw,*) '     reading grid cell information...'
      
      !if a structured grid, read in structured cell data -------------------------------------------------------------
      !(then, convert to usg arrays)
      if (grid_type == "structured") then
      read(in_gw,*) header
      
      !cell status
      read(in_gw,*) header
      allocate(grid_status(grid_nrow,grid_ncol))
      read(in_gw,*) ((grid_status(i,j),j=1,grid_ncol),i=1,grid_nrow)
      
      !determine connections using cell status
      allocate(cell_id_usg(grid_nrow,grid_ncol))
      allocate(cell_id_list(grid_nrow*grid_ncol))
      cell_id_usg = 0
      ncell = 0
      count = 0
      !first: determine the new cell id of each gwflow cell
      do i=1,grid_nrow
        do j=1,grid_ncol
          count = count + 1
          if (grid_status(i,j) > 0) then
            !add to cell list; set new cell id
            ncell = ncell + 1
            cell_id_usg(i,j) = ncell
            cell_id_list(count) = ncell
          endif
        enddo
      enddo
      !second: !allocate general array of cell attributes
      allocate(gw_state(ncell))
      do i=1,ncell
        gw_state(i)%elev = 0.
        gw_state(i)%thck = 0.
        gw_state(i)%botm = 0.
        gw_state(i)%xcrd = 0.
        gw_state(i)%ycrd = 0.
        gw_state(i)%area = 0.
        gw_state(i)%init = 0.	
        gw_state(i)%head = 0.
        gw_state(i)%hydc = 0.
        gw_state(i)%spyd = 0.
        gw_state(i)%exdp = 0.
        gw_state(i)%stat = 0
        gw_state(i)%ncon = 0
        gw_state(i)%tile = 0
        gw_state(i)%hnew = 0.
        gw_state(i)%hold = 0.
        gw_state(i)%stor = 0.
        gw_state(i)%vbef = 0.
        gw_state(i)%vaft = 0.
        gw_state(i)%hdmo = 0.
        gw_state(i)%hdyr = 0.
      enddo
      !third: determine the cells connected to each cell
      allocate(cell_con(ncell))
      do i=1,grid_nrow
        do j=1,grid_ncol
          if(cell_id_usg(i,j) > 0) then
            !first: determine the number of cells connected to this cell
            num_conn = 0
            !north
            if(i > 1) then
              if(grid_status(i-1,j) > 0) then
                num_conn = num_conn + 1    
              endif
            endif
            !east
            if(j < grid_ncol) then
              if(grid_status(i,j+1) > 0) then
                num_conn = num_conn + 1    
              endif
            endif  
            !south
            if(i < grid_nrow) then
              if(grid_status(i+1,j) > 0) then
                num_conn = num_conn + 1    
              endif
            endif   
            !west
            if(j > 1) then
              if(grid_status(i,j-1) > 0) then
                num_conn = num_conn + 1    
              endif
            endif
            !second: go back through, storing the id of each connected cell
            allocate(cell_con(cell_id_usg(i,j))%cell_id(num_conn))
            num_conn = 0 !number of cells connected to this cell
            !north
            if(i > 1) then
              if(grid_status(i-1,j) > 0) then
                num_conn = num_conn + 1  
                cell_con(cell_id_usg(i,j))%cell_id(num_conn) = cell_id_usg(i-1,j)
              endif
            endif
            !east
            if(j < grid_ncol) then
              if(grid_status(i,j+1) > 0) then
                num_conn = num_conn + 1    
                cell_con(cell_id_usg(i,j))%cell_id(num_conn) = cell_id_usg(i,j+1)
              endif
            endif  
            !south
            if(i < grid_nrow) then
              if(grid_status(i+1,j) > 0) then
                num_conn = num_conn + 1    
                cell_con(cell_id_usg(i,j))%cell_id(num_conn) = cell_id_usg(i+1,j)
              endif
            endif   
            !west
            if(j > 1) then
              if(grid_status(i,j-1) > 0) then
                num_conn = num_conn + 1    
                cell_con(cell_id_usg(i,j))%cell_id(num_conn) = cell_id_usg(i,j-1)
              endif
            endif
            !store number of connected cells
            gw_state(cell_id_usg(i,j))%ncon = num_conn
          endif !if cell is active
        enddo
      enddo
      !establish xy coordinates (at centroid) of each cell
      y_coord = (grid_nrow*cell_size) - (cell_size/2) !m
      do i=1,grid_nrow
        x_coord = cell_size / 2 !m
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
      allocate(grid_val(grid_nrow,grid_ncol))
      allocate(grid_int(grid_nrow,grid_ncol))
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
          if(grid_int(i,j) == 0) then
            grid_int(i,j) = 1 !give default, in case of missing cell value
          endif
          if(cell_id_usg(i,j) > 0) then
            gw_state(cell_id_usg(i,j))%hydc = zones_aquK(grid_int(i,j))
          endif
        enddo
      enddo
      !specific yield zone
      read(in_gw,*) header
      read(in_gw,*) ((grid_int(i,j),j=1,grid_ncol),i=1,grid_nrow)
      do i=1,grid_nrow
        do j=1,grid_ncol
          if(grid_int(i,j) == 0) then
            grid_int(i,j) = 1 !give default, in case of missing cell value
          endif
          if(cell_id_usg(i,j) > 0) then
            gw_state(cell_id_usg(i,j))%spyd = zones_aquSy(grid_int(i,j))
          endif
        enddo
      enddo
      !recharge delay
      allocate(delay(ncell))
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
            if(gw_state(cell_id_usg(i,j))%init < gw_state(cell_id_usg(i,j))%botm) then !correct initial head, if necessary
              gw_state(cell_id_usg(i,j))%init = gw_state(cell_id_usg(i,j))%botm  
            endif
          endif    
        enddo
      enddo
      
      
      !if the grid type was specified as unstructured (usg) -----------------------------------------------------------
      elseif(grid_type == "unstructured") then
      
      !allocate general array of cell attributes
      allocate(gw_state(ncell))
      do i=1,ncell
        gw_state(i)%elev = 0.
        gw_state(i)%thck = 0.
        gw_state(i)%botm = 0.
        gw_state(i)%xcrd = 0.
        gw_state(i)%ycrd = 0.
        gw_state(i)%area = 0.
        gw_state(i)%init = 0.	
        gw_state(i)%head = 0.
        gw_state(i)%hydc = 0.
        gw_state(i)%spyd = 0.
        gw_state(i)%exdp = 0.
        gw_state(i)%stat = 0
        gw_state(i)%ncon = 0
        gw_state(i)%tile = 0
        gw_state(i)%hnew = 0.
        gw_state(i)%hold = 0.
        gw_state(i)%stor = 0.
        gw_state(i)%vbef = 0.
        gw_state(i)%vaft = 0.
        gw_state(i)%hdmo = 0.
        gw_state(i)%hdyr = 0.
      enddo
      
      
      !read in cell information ---------------------------------------------------------------------------------------
      allocate(delay(ncell))
      allocate(cell_con(ncell))
      do i=1,13 !definitions
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
        gw_state(i)%botm = gw_state(i)%elev - gw_state(i)%thck !aquifer bottom (m)
        gw_state(i)%hydc = zones_aquK(K_zone) !hydraulic conductivity (m/day)
        gw_state(i)%spyd = zones_aquSy(Sy_zone) !specific yield
        if(gw_state(i)%init < gw_state(i)%botm) then !correct initial head, if necessary
          gw_state(i)%init = gw_state(i)%botm  
        endif
      enddo !go to next cell
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
        if (gw_state(i)%stat == 1) then
          gwflow_area = gwflow_area + gw_state(i)%area
        endif
      enddo

      !for boundary cells: store id of the closest active cell
      allocate(gw_bound_near(ncell))
      allocate(gw_bound_dist(ncell))
      gw_bound_near = 0
      gw_bound_dist = 0.
      do i=1,ncell
        if (gw_state(i)%stat == 2) then
          min_dist = 1000000. 
          do j=1,ncell
            if(gw_state(j)%stat == 1) then
              dist_x = gw_state(i)%xcrd - gw_state(j)%xcrd !m
              dist_y = gw_state(i)%ycrd - gw_state(j)%ycrd !m
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
      allocate(gw_output_yr(gw_num_output))
      allocate(gw_output_day(gw_num_output))
      do i=1,gw_num_output
        read(in_gw,*) gw_output_yr(i),gw_output_day(i)
      enddo
      
      
      !read in cells for daily output (i.e. observation wells) --------------------------------------------------------
      write(out_gw,*) '     reading observation cells'
      read(in_gw,*)
      read(in_gw,*) gw_num_obs_wells
      allocate(gw_obs_cells(gw_num_obs_wells))
      !check to see if there are USGS well names (for the national model)
      inquire(file='usgs_annual_head',exist=i_exist)
      if (usgs_obs == 1) then
        allocate(usgs_id(gw_num_obs_wells))
      endif
      !loop through the observation well locations
      do k=1,gw_num_obs_wells
        if(usgs_obs == 1) then
          read(in_gw,*) gw_obs_cells(k),usgs_id(k)
        else
          read(in_gw,*) gw_obs_cells(k)
        endif
      enddo
      allocate(gw_obs_head(gw_num_obs_wells))
      gw_obs_head = 0.
      !open file for writing out daily head values
      open(out_gwobs,file='gwflow_state_obs_head')
      write(out_gwobs,*) 'Daily head (m) values for observation wells'
      write(out_gwobs,123) 'cell:',(gw_obs_cells(k),k=1,gw_num_obs_wells)
      write(out_gwobs,*)
      gw_output_index = 1 !start output index at 1
      !if structured grid, then convert cell ids
      do k=1,gw_num_obs_wells
        if(grid_type == "structured") then
          gw_obs_cells(k) = cell_id_list(gw_obs_cells(k))
        endif
      enddo

      
      !cell for detailed daily groundwater source/sink output ---------------------------------------------------------
      read(in_gw,*) header
      read(in_gw,*) gw_cell_obs_ss
      !if(grid_type == "structured") then
      !  gw_cell_obs_ss = cell_id_list(gw_cell_obs_ss)
      !endif
      !open(out_gwobs_ss,file='gwflow_cell_ss')
      !write(out_gwobs_ss,*) 'Daily sources and sinks for cell'
      !write(out_gwobs_ss,*) 'cell:',gw_cell_obs_ss
      !gwflow_hdr = (/"year","day","head","vol_bef","vol_aft","rech","gwet","gwsw","swgw","satex","gwsoil", &
      !                   "lateral","pump_ag","pump_ex","tile","res","wet","canal","fplain"/)
      !write(out_gwobs_ss,119) (gwflow_hdr(j),j=1,19)
      !allocate(gw_cell_obs_ss_vals(17))
      
      
      !if usgs observation wells, read in annual head data from national data set -------------------------------------
      if (usgs_obs == 1) then
        open(in_usgs_head,file='usgs_annual_head')
        read(in_usgs_head,*) 
        num_usgs_wells = 356785 
        allocate(usgs_head_vals(gw_num_obs_wells,101))
        do i=1,num_usgs_wells
          read(in_usgs_head,*) usgs_site_id,usgs_lat,usgs_long,(head_vals(j),j=1,101) 
          !determine if the well is a match with a well from the list of observation cells
          do k=1,gw_num_obs_wells
            if(usgs_site_id.eq.usgs_id(k)) then
              do j=1,101    
                usgs_head_vals(k,j) = head_vals(j)
              enddo
            endif
          enddo
        enddo
        open(out_gwobs_usgs,file='gwflow_state_obs_head_usgs')
        write(out_gwobs_usgs,*) 'Average annual heads: USGS observed vs. simulated'
        !prepare arrays for simulation
        allocate(gw_obs_head_annual(gw_num_obs_wells,366))
        allocate(sim_head_vals(gw_num_obs_wells,101))
        allocate(gw_obs_sat_annual(gw_num_obs_wells,366))
        allocate(sim_sat_vals(gw_num_obs_wells,101))
        gw_obs_head_annual = 0.
        sim_head_vals = 0.
        gw_obs_sat_annual = 0.
        sim_sat_vals = 0.
      endif
      
      
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
      
      
      !prepare source-sink arrays and options -------------------------------------------------------------------------
      write(out_gw,*) '     allocate and prepare source-sink arrays...'
      
      !allocate cell source/sink arrays
      allocate(gw_ss(ncell))
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
      
      !allocate grid source/sink arrays
      allocate(gw_ss_sum(ncell))
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
      
      !recharge
      write(out_gw,*) '          recharge from soil'
      !flux output file
      open(out_gw_rech,file='gwflow_flux_rech')
      write(out_gw_rech,*) 'Annual recharge flow (m3/day)'
      allocate(gwflow_perc(num_hru))
      gwflow_perc = 0.

      !gwet
      write(out_gw,*) '          groundwater ET'
      !flux output file
      open(out_gw_et,file='gwflow_flux_gwet')
      write(out_gw_et,*) 'Annual groundwater ET rates (m3/day)'
      allocate(etremain(num_hru))
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
      open(out_gwsw,file='gwflow_flux_gwsw')
      write(out_gwsw,*) 'Annual GW-SW Exchange flows (m3/day)'
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
      enddo
      
      !groundwater-->soil transfer ----------------------------------------------------------------
      if (gw_soil_flag == 1) then
        write(out_gw,*) '          groundwater-->soil transfer'
        !flux output file
        open(out_gw_soil,file='gwflow_flux_soil')
        write(out_gw_soil,*) 'Annual groundwater-->soil transfers (m3/day)'
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
      if (gw_satx_flag == 1) then
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
        open(out_gw_satex,file='gwflow_flux_satx')
        write(out_gw_satex,*) 'Annual saturation excess flows (m3/day)'
      endif
      
      !groundwater pumping (irrigation) -----------------------------------------------------------
      write(out_gw,*) '          groundwater pumping for irrigation'
      !flux output file
      open(out_gw_pumpag,file='gwflow_flux_ppag')
      write(out_gw_pumpag,*) 'Annual pumping rate (m3/day) (irrigation)'
      !pumping deficiencies (unmet demand)
      open(out_gw_pumpdef,file='gwflow_flux_pumping_deficient')
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
      open(out_hru_pump_yr,file='gwflow_flux_pumping_hru_yr')
      write(out_hru_pump_yr,*) 'Annual pumped volume (m3) (irrigation) for HRUs'
      write(out_hru_pump_yr,*) 'Columns: each year of the simulation'
      open(out_hru_pump_mo,file='gwflow_flux_pumping_hru_mo')
      write(out_hru_pump_mo,*) 'Monthly pumped volume (m3) (irrigation) for HRUs'
      write(out_hru_pump_mo,*) 'Columns: each month of the simulation'
      inquire(file='gwflow.hru_pump_observe',exist=i_exist)
      if (hru_pump_flag == 1) then
        open(in_hru_pump_obs,file='gwflow.hru_pump_observe')
        read(in_hru_pump_obs,*)
        read(in_hru_pump_obs,*) num_hru_pump_obs
        allocate(hru_pump_ids(num_hru_pump_obs))
        do i=1,num_hru_pump_obs
          read(in_hru_pump_obs,*) hru_pump_ids(i)
        enddo
        allocate(hru_pump_obs(num_hru_pump_obs))
        hru_pump_obs = 0.
        open(out_hru_pump_obs,file='gwflow_flux_pumping_hru_obs')
        write(out_hru_pump_obs,*) 'Daily groundwater pumping (m3) for specified HRUs'
        write(out_hru_pump_obs,*) 'Columns = HRUs (same order as in gwflow.hru_pump_observe)'
        write(out_hru_pump_obs,*) 'Time(day)    Daily pumping (m3)'
      endif
      
      !groundwater pumping (specified) ------------------------------------------------------------
      if (gw_pumpex_flag == 1) then
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
          read(in_gw,*) 
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
        open(out_gw_pumpex,file='gwflow_flux_ppex')
        write(out_gw_pumpex,*) 'Annual pumping rate (m3/day) (specified)'
      else
        write(out_gw,*) '          gwflow.pumpex not found; pumping not simulated'
      endif 
      endif !end specified pumping
      
      !tile drainage outflow ----------------------------------------------------------------------
      !tile drain cell information
      if (gw_tile_flag == 1) then
      inquire(file='gwflow.tiles',exist=i_exist)
      if(i_exist) then
        write(out_gw,*) '          groundwater-tile drainage outflow (gwflow.tiles found)'
        open(in_gw,file='gwflow.tiles')
        read(in_gw,*) header
        !read in tile parameters
        read(in_gw,*) gw_tile_depth
        read(in_gw,*) gw_tile_drain_area
        read(in_gw,*) gw_tile_K
        read(in_gw,*) gw_tile_group_flag
        !read in tile cell groups (if any)
        if(gw_tile_group_flag == 1) then
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
        open(out_gw_tile,file='gwflow_flux_tile')
        write(out_gw_tile,*) 'Annual tile flows (m3/day)'
        else
        write(out_gw,*) '          gwflow.tiles not found; tile drainage not simulated'
      endif
      endif !end tile drainage

      !aquifer-reservoir exchange -----------------------------------------------------------------
      if (gw_res_flag == 1) then
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
        open(out_gw_res,file='gwflow_flux_resv')
        write(out_gw_res,*) 'Annual groundwater-reservoir exchange (m3/day)' 
        else
        write(out_gw,*) '          gwflow.rescells not found (groundwater-res exchange not simulated)'
      endif
      endif !end reservoir exchange
      
      !aquifer-wetland exchange -------------------------------------------------------------------
      if (gw_wet_flag == 1) then
        write(out_gw,*) '          groundwater-->wetland exchange'
        !wetland bed thickness for each wetland object is read in wet_read_hyd; set default value here
        allocate(wet_thick(sp_ob%hru))
        wet_thick = 0.25
        !flux output file
        open(out_gw_wet,file='gwflow_flux_wetland')
        write(out_gw_wet,*) 'Annual groundwater outflow to wetlands (m3/day)' 
      endif !end wetland exchange

      !aquifer-floodplain exchange ----------------------------------------------------------------
      allocate(flood_freq(sp_ob%chandeg))
      flood_freq = 0
      if (gw_fp_flag == 1) then
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
        open(out_gw_fp,file='gwflow_flux_floodplain')
        write(out_gw_fp,*) 'Annual floodplain seepage (m3/day)'  
        else
        write(out_gw,*) '          gwflow.floodplain not found (groundwater-fp exchange not simulated)'
      endif
      endif !end floodplain exchange
      
      !groundwater seepage from canals ------------------------------------------------------------
      !canal seepage information (these are for cells that are connected to irrigation canals)
      if (gw_canal_flag == 1) then
      inquire(file='gwflow.canals',exist=i_exist)
      if(i_exist) then
        write(out_gw,*) '          canal-->groundwater seepage (gwflow.canals found)'
        open(in_canal_cell,file='gwflow.canals')
        read(in_canal_cell,*) header
        !read in the number of canals for each channel
        allocate(gw_chan_canl_info(sp_ob%chandeg)) !number of canals for each channel
        canal_out = 0
        read(in_canal_cell,*) gw_ncanal 
        read(in_canal_cell,*)
        allocate(canal_out_info(gw_ncanal,5))
        canal_out_info = 0.
        do i=1,gw_ncanal
          read(in_canal_cell,*) canal,channel,width,depth,thick,day_beg,day_end
          if(channel == 0) then !canal water originates from outside the model domain
            canal_out(i) = 1
            canal_out_info(i,1) = width
            canal_out_info(i,2) = depth
            canal_out_info(i,3) = thick
            canal_out_info(i,4) = day_beg
            canal_out_info(i,5) = day_end
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
          allocate(gw_chan_canl_info(i)%dayb(gw_chan_canl_info(i)%ncanal)) 
          allocate(gw_chan_canl_info(i)%daye(gw_chan_canl_info(i)%ncanal)) 
          gw_chan_canl_info(i)%ncanal = 0
        enddo
        !read in and store canal attributes
        rewind(in_canal_cell)
        read(in_canal_cell,*) header
        read(in_canal_cell,*) gw_ncanal
        read(in_canal_cell,*) 
        do i=1,gw_ncanal
          read(in_canal_cell,*) canal,channel,width,depth,thick,day_beg,day_end 
          if(canal_out(canal) == 0) then
            gw_chan_canl_info(channel)%ncanal = gw_chan_canl_info(channel)%ncanal + 1
            gw_chan_canl_info(channel)%canals(gw_chan_canl_info(channel)%ncanal) = canal
            gw_chan_canl_info(channel)%wdth(gw_chan_canl_info(channel)%ncanal) = width
            gw_chan_canl_info(channel)%dpth(gw_chan_canl_info(channel)%ncanal) = depth
            gw_chan_canl_info(channel)%thck(gw_chan_canl_info(channel)%ncanal) = thick
            gw_chan_canl_info(channel)%dayb(gw_chan_canl_info(channel)%ncanal) = day_beg
            gw_chan_canl_info(channel)%daye(gw_chan_canl_info(channel)%ncanal) = day_end
          endif
        enddo
        !read in the canal bed K values for each zone
        read(in_canal_cell,*) header
        read(in_canal_cell,*) num_canalK_zones
        allocate(canalK_zones(num_canalK_zones))
        do i=1,num_canalK_zones
          read(in_canal_cell,*) dum1,canalK_zones(i)
        enddo
        !determine the number of cells that are connected to canals that receive outside water
        gw_canal_ncells_out = 0
        read(in_canal_cell,*) header
        read(in_canal_cell,*) gw_canal_ncells
        read(in_canal_cell,*) header
        do i=1,gw_canal_ncells
          read(in_canal_cell,*) cell_num,canal
          if(canal_out(canal) == 1) then
            gw_canal_ncells_out = gw_canal_ncells_out + 1
          endif
        enddo
        allocate(gw_canl_out_info(gw_canal_ncells_out))
        !rewind file to beginning line
        rewind(in_canal_cell)                
        read(in_canal_cell,*) header
        read(in_canal_cell,*) gw_ncanal
        read(in_canal_cell,*) header
        do i=1,gw_ncanal
          read(in_canal_cell,*)
        enddo
        read(in_canal_cell,*) header
        read(in_canal_cell,*) num_canalK_zones
        do i=1,num_canalK_zones
          read(in_canal_cell,*)
        enddo
        !read in the number of cells for each canal
        read(in_canal_cell,*) header
        allocate(gw_canl_info(gw_ncanal)) !allocate number of cells connected to each canal
        read(in_canal_cell,*) gw_canal_ncells 
        read(in_canal_cell,*) header
        do i=1,gw_canal_ncells
          read(in_canal_cell,*) cell_num,canal,length,stage,K_zone
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
        read(in_canal_cell,*) header
        do i=1,gw_ncanal
          read(in_canal_cell,*)
        enddo
        read(in_canal_cell,*) header
        read(in_canal_cell,*) num_canalK_zones
        do i=1,num_canalK_zones
          read(in_canal_cell,*)
        enddo
        read(in_canal_cell,*) header
        read(in_canal_cell,*) gw_canal_ncells
        read(in_canal_cell,*) header
        gw_canal_ncells_out = 0
        do i=1,gw_canal_ncells
          read(in_canal_cell,*) cell_num,canal,length,stage,K_zone
          if(grid_type == "structured") then
            cell_num = cell_id_list(cell_num)
          endif
          if(cell_num > 0) then
            if(canal_out(canal) == 1) then !canal water from outside the model domain
              gw_canal_ncells_out = gw_canal_ncells_out + 1
              gw_canl_out_info(gw_canal_ncells_out)%cell_id = cell_num
              gw_canl_out_info(gw_canal_ncells_out)%wdth = canal_out_info(canal,1)
              gw_canl_out_info(gw_canal_ncells_out)%dpth = canal_out_info(canal,2)
              gw_canl_out_info(gw_canal_ncells_out)%thck = canal_out_info(canal,3)
              gw_canl_out_info(gw_canal_ncells_out)%leng = length
              gw_canl_out_info(gw_canal_ncells_out)%elev = stage
              gw_canl_out_info(gw_canal_ncells_out)%hydc = canalK_zones(K_zone)
              gw_canl_out_info(gw_canal_ncells_out)%dayb = canal_out_info(canal,4)
              gw_canl_out_info(gw_canal_ncells_out)%daye = canal_out_info(canal,5)
            else
              gw_canl_info(canal)%ncon = gw_canl_info(canal)%ncon + 1
              gw_canl_info(canal)%cells(gw_canl_info(canal)%ncon) = cell_num
              gw_canl_info(canal)%leng(gw_canl_info(canal)%ncon) = length
              gw_canl_info(canal)%elev(gw_canl_info(canal)%ncon) = stage
              gw_canl_info(canal)%hydc(gw_canl_info(canal)%ncon) = canalK_zones(K_zone)
            endif
          endif
        enddo !go to next canal cell
        !flux output file
        open(out_gw_canal,file='gwflow_flux_canl')
        write(out_gw_canal,*) 'Annual canal seepage (m3/day)' 
        else
        write(out_gw,*) '          gwflow.canals not found (canal seepage not simulated)'
      endif
      endif !end canal seepage
           


      !groundwater solute transport option ------------------------------------------------------------------
      write(out_gw,*)
      
      if (gw_solute_flag == 1) then
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
          do i=1,ncell
            read(in_gw,*) (gwsol_state(i)%solute(s)%conc,s=1,gw_nsolute)
          enddo
        endif
        !if salts active: read in salt mineral data (if provided)
        if(gwsol_salt == 1) then
          inquire(file='gwflow.solutes.minerals',exist=i_exist)
          if(gwsol_minl == 1) then
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
              do i=1,grid_nrow
                do j=1,grid_ncol
                  if(cell_id_usg(i,j) > 0) then
                    gwsol_minl_state(cell_id_usg(i,j))%fract(m) = grid_val(i,j)
                  endif 
                enddo
              enddo
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
        if (gwsol_cons == 1) then 
          !allocate solute chem array
          allocate(gwsol_chem(ncell))
          do i=1,ncell
            gwsol_chem(i)%ino3 = 0.
            gwsol_chem(i)%oxyg = 0.
            gwsol_chem(i)%kd_seo4 = 0.
            gwsol_chem(i)%kd_seo3 = 0.
            gwsol_chem(i)%kd_boron = 0.
            gwsol_chem(i)%kseo4 = 0.
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
        do i=1,ncell
          allocate(gwsol_ss_sum(i)%solute(gw_nsolute))
        enddo
        !open output files for each source/sink type
        !recharge
        open(out_sol_rech,file='gwflow_mass_rech')
        write(out_sol_rech,*) 'Annual recharge mass (kg/day)'
        allocate(gw_rechsol(sp_ob%hru,gw_nsolute))
        allocate(gwflow_percsol(num_hru,gw_nsolute))
        gw_rechsol = 0.
        gwflow_percsol = 0.
        !groundwater-channel exchange
        open(out_sol_gwsw,file='gwflow_mass_gwsw')
        write(out_sol_gwsw,*) 'Annual gw-channel exchange mass (kg/day)'
        !ground-->soil transfer
        if(gw_soil_flag == 1) then
          open(out_sol_soil,file='gwflow_mass_soil')
          write(out_sol_soil,*) 'Annual groundwater-->soil mass transfer (kg/day)'
        endif
        allocate(hru_soil(num_hru,20,gw_nsolute))
        hru_soil = 0.
        !saturation excess flow 
        if (gw_satx_flag == 1) then
          open(out_sol_satx,file='gwflow_mass_satx')
          write(out_sol_satx,*) 'Annual saturation excess flow mass (kg/day)'  
        endif
        !irrigation pumping
        open(out_sol_ppag,file='gwflow_mass_ppag')
        write(out_sol_ppag,*) 'Annual mass in pumping (kg/day) (irrigation)'
        !specified pumping
        if (gw_pumpex_flag == 1) then
          open(out_sol_ppex,file='gwflow_mass_ppex')
          write(out_sol_ppex,*) 'Annual mass in pumping (kg/day) (specified)'
        endif
        !tile drainage
        if (gw_tile_flag == 1) then
          open(out_sol_tile,file='gwflow_mass_tile')
          write(out_sol_tile,*) 'Annual mass in tile flow (kg/day)'
        endif
        !reservoir
        if (gw_res_flag == 1) then
          open(out_sol_resv,file='gwflow_mass_resv')
          write(out_sol_resv,*) 'Annual groundwater-reservoir exchange mass (kg/day)' 
        endif
        !wetland exchange
        if (gw_wet_flag == 1) then
          open(out_sol_wetl,file='gwflow_mass_wetl')
          write(out_sol_wetl,*) 'Annual groundwater-wetland exchange mass (kg/day)' 
        endif
        !floodplain exchange
        if (gw_fp_flag == 1) then
          open(out_sol_fpln,file='gwflow_mass_fpln')
          write(out_sol_fpln,*) 'Annual floodplain seepage mass (kg/day)'  
        endif
        !canal seepage
        if (gw_canal_flag == 1) then
          open(out_sol_canl,file='gwflow_mass_canl')
          write(out_sol_canl,*) 'Annual canal seepage mass (kg/day)' 
        endif
        !reactions and sorption
        open(out_gw_chem,file='gwflow_flux_reaction')
        write(out_gw_chem,*) 'Annual denitrifiction and sorption mass (kg/day)'
        !open file for writing observation values; allocate arrays for observation cells
        open(out_gwobs_sol,file='gwflow_state_obs_conc')
        write(out_gwobs_sol,*) 'Daily solute concentration (mg/L) values for observation wells'
        write(out_gwobs_sol,123) 'cell:',(gw_obs_cells(k),k=1,gw_num_obs_wells)
        write(out_gwobs_sol,*)
        allocate(gw_obs_solute(gw_num_obs_wells,gw_nsolute))
        gw_obs_solute = 0.
      else
        gw_solute_flag = 0 !turn off transport option
      endif
      endif !end solute transport
      

      
      !read in connection information between SWAT+ objects (LSUs or HRUs) and grid cells  ------------------------------------------------
      !if LSU-cell connection is active (i.e., file is provided), it supercedes HRU-cell connection
      write(out_gw,*)
      write(out_gw,*) '     read and prepare connection (HRU-cell or LSU-cell)'
      if (lsu_cells_link == 1) then
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
      
      !for normal gwflow applications, the Cell-HRU connection will be used, using the gwflow.cellhru file. However, for applications
      !with the national agroecosystem model, the Cell-HUC12 connection will be used. If the gwflow.huc12cell file is present in the folder,
      !then the national model approach will be used.
      inquire(file='gwflow.huc12cell',exist=i_exist)
      if (nat_model == 1) then
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
      write(out_gw,*) '          HRU-cell connections (gwflow.hrucell)'
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
      allocate(hru_cells(num_hru,5000))
      allocate(hru_cells_fract(num_hru,5000))
      allocate(cells_fract(num_hru,5000))
      hru_num_cells = 0
      hru_cells = 0
      hru_cells_fract = 0.
      do k=1,num_hru
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
      
      !for normal gwflow applications, the Cell-HRU connection will be used, using the gwflow.cellhru file. However, for applications
      !with the NAM, the Cell-HUC12 connection will be used. If the gwflow.huc12cell file is present in the folder,
      !then the national model approach will be used.
      if (nat_model == 1) then
        !read in the list of grid cells for each HUC12
        open(in_huc_cell,file='gwflow.huc12cell')
        read(in_huc_cell,*)
        read(in_huc_cell,*)
        !read in the list of HUC12 catchments that have connections with grid cells
        read(in_huc_cell,*)
        do k=1,sp_ob%outlet
          read(in_huc_cell,*) huc12_dum,huc12_connect(k)
        enddo
        allocate(huc12_cells(sp_ob%outlet,50000))
        allocate(huc12_ncell(sp_ob%outlet))
        allocate(cell_included_huc12(ncell)) !check for cell inclusion - cell should only be in one HUC12
        huc12_ncell = 0
        cell_included_huc12 = 0
        read(in_huc_cell,*)
        read(in_huc_cell,*)
        do k=1,sp_ob%outlet
          if(huc12_connect(k).eq.1) then
            read(in_huc_cell,*) huc12_id
            backspace(in_huc_cell)
            cell_count = 0
            do while (huc12_id.eq.huc12(k))
              read(in_huc_cell,*) huc12_id,cell_num
              if(grid_type == "structured") then
                if(cell_id_list(cell_num) > 0) then
                  cell_num = cell_id_list(cell_num)
                  if(cell_included_huc12(cell_num) == 1) then !cell already included in a HUC12
                    dum = 10
                  else !proceed
                    cell_count = cell_count + 1
                    cell_included_huc12(cell_num) = 1
                    huc12_cells(k,cell_count) = cell_num
                    huc12_ncell(k) = cell_count
                  endif
                endif
              else
                if(cell_included_huc12(cell_num) == 1) then !cell already included in a HUC12
                dum = 10
                else !proceed
                  cell_count = cell_count + 1
                  cell_included_huc12(cell_num) = 1
                  huc12_cells(k,cell_count) = cell_num
                  huc12_ncell(k) = cell_count
                endif
              endif
              read(in_huc_cell,*,end=20) huc12_id
              backspace(in_huc_cell)
            enddo
          endif
  20    enddo      
        allocate(cell_received(ncell))
        cell_received = 0  
      else !read in Cell-HRU connection information
      write(out_gw,*) '          HRU-cell connections (gwflow.cellhru)'
      allocate(cell_num_hrus(ncell))
      allocate(cell_hrus(ncell,100))
      allocate(cell_hrus_fract(ncell,100))
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
      endif
      
      endif !check for LSU-cell connection
      
      
      
      !initialize groundwater balance ---------------------------------------------------------------------------------------------------------------
      write(out_gw,*) 
      write(out_gw,*) '     initialize groundwater balance files and arrays'
      
      !open file to track daily groundwater water balance
      if(gwflag_day.eq.1) then
        open(out_gwbal,file='gwflow_balance_gw_day')
        write(out_gwbal,*) 'Groundwater watershed-wide fluxes for each day'
        write(out_gwbal,*)
        write(out_gwbal,*) 'watershed area (m2):',(bsn%area_tot_ha*10000.)
        write(out_gwbal,*)
        write(out_gwbal,*) 'Positive value: groundwater added to aquifer'
        write(out_gwbal,*) 'Negative value: groundwater removed from aquifer'
        write(out_gwbal,*)
        write(out_gwbal,*) 'ts:           days time step used for groundwater storage calculations'
        write(out_gwbal,*) 'vbef:         mm   total groundwater volume at the beginning of the day'
        write(out_gwbal,*) 'vaft:         mm   total groundwater volume at the end of the day'
        write(out_gwbal,*) 'rech:         mm   soil water added to groundwater'
        write(out_gwbal,*) 'gwet:         mm   groundwater removed by evapotranspiration'
        write(out_gwbal,*) 'gwsw:         mm   groundwater discharge to streams'
        write(out_gwbal,*) 'swgw:         mm   stream water seepage to groundwater'
        write(out_gwbal,*) 'satx:         mm   saturation excess flow (water table above ground)'
        write(out_gwbal,*) 'soil:         mm   groundwater transferred to HRU soil profile'
        write(out_gwbal,*) 'latl:         mm   groundwater transferred between cells'
        write(out_gwbal,*) 'bndr:         mm   groundwater added/removed at watershed boundary'
        write(out_gwbal,*) 'ppag:         mm   groundwater pumped for irrigation'
        write(out_gwbal,*) 'ppex:         mm   groundwater pumping specified by user'
        write(out_gwbal,*) 'tile:         mm   groundwater removed via tile drains'
        write(out_gwbal,*) 'resv:         mm   groundwater exchanged with reservoirs'
        write(out_gwbal,*) 'wetl:         mm   groundwater outflow to wetlands'	
        write(out_gwbal,*) 'canl:         mm   canal seepage to groundwater'
        write(out_gwbal,*) 'fpln:         mm   floodplain exchange'
        write(out_gwbal,*) 'error:        --   water balance error for aquifer'
        write(out_gwbal,*) 'satfr:        --   fraction of cells that have water table at ground'
        write(out_gwbal,*) 'wtdep:        m    average depth to water table for watershed'
        write(out_gwbal,*) 'ppdf:         mm   groundwater demand not satisfied for irrigation'
        write(out_gwbal,*)
        gwflow_hdr_day = [character(len=24) :: "year","day","ts","vbef","vaft","rech","gwet","gwsw","swgw","satx","soil", &
                                             "latl","bndr","ppag","ppex","tile","resv","wetl","canl", &
                                             "fpln","error","satfr","wtdepth","ppdf"]
        write(out_gwbal,119) (gwflow_hdr_day(j),j=1,24)
      endif

      !open file to track yearly groundwater water balance
      if(gwflag_yr.eq.1) then 
        open(out_gwbal_yr,file='gwflow_balance_gw_yr')
        write(out_gwbal_yr,*) 'Groundwater watershed-wide fluxes for each year'
        write(out_gwbal_yr,*)
        write(out_gwbal_yr,*) 'watershed area (m2):',(bsn%area_tot_ha*10000.)
        write(out_gwbal_yr,*)
        write(out_gwbal_yr,*) 'Positive value: groundwater added to aquifer'
        write(out_gwbal_yr,*) 'Negative value: groundwater removed from aquifer'
        write(out_gwbal_yr,*)
        write(out_gwbal_yr,*) 'dvol:      mm   change in groundwater volume during the year'
        write(out_gwbal_yr,*) 'rech:      mm   soil water added to groundwater'
        write(out_gwbal_yr,*) 'gwet:      mm   groundwater removed by evapotranspiration'
        write(out_gwbal_yr,*) 'gwsw:      mm   groundwater discharge to streams'
        write(out_gwbal_yr,*) 'swgw:      mm   stream water seepage to groundwater'
        write(out_gwbal_yr,*) 'satx:      mm   saturation excess flow (water table above ground)'
        write(out_gwbal_yr,*) 'soil:      mm   groundwater transferred to HRU soil profile'
        write(out_gwbal_yr,*) 'latl:      mm   groundwater transferred between cells'
        write(out_gwbal_yr,*) 'bndr:      mm   groundwater added/removed at watershed boundary'
        write(out_gwbal_yr,*) 'ppag:      mm   groundwater pumped for irrigation'
        write(out_gwbal_yr,*) 'ppex:      mm   groundwater pumping specified by user'
        write(out_gwbal_yr,*) 'tile:      mm   groundwater removed via tile drains'
        write(out_gwbal_yr,*) 'resv:      mm   groundwater exchanged with reservoirs'
        write(out_gwbal_yr,*) 'wetl:      mm   groundwater outflow to wetlands'
        write(out_gwbal_yr,*) 'canl:      mm   canal seepage to groundwater'
        write(out_gwbal_yr,*) 'fpln:      mm   floodplain exchange'
        write(out_gwbal_yr,*) 'ppdf:      mm   groundwater demand not satisfied for irrigation'
        write(out_gwbal_yr,*)
        gwflow_hdr_yr = [character(len=18) :: "  year","dvol","rech","gwet","gwsw","swgw","satx","soil","latl","bndr",  &
              "ppag","ppex","tile","resv","wetl","canl","fpln","ppdf"]
        write(out_gwbal_yr,120) (gwflow_hdr_yr(j),j=1,18)
      endif
      
      !open file to write out average annual groundwater water balance
        if(gwflag_aa.eq.1) then
        open(out_gwbal_aa,file='gwflow_balance_gw_aa')
        write(out_gwbal_aa,*) 'Average annual groundwater watershed-wide fluxes'
        write(out_gwbal_aa,*)
        write(out_gwbal_aa,*) 'watershed area (m2):',(bsn%area_tot_ha*10000.)
        write(out_gwbal_aa,*)
        write(out_gwbal_aa,*) 'Positive value: groundwater added to aquifer'
        write(out_gwbal_aa,*) 'Negative value: groundwater removed from aquifer'
        write(out_gwbal_aa,*)
        write(out_gwbal_aa,*) 'dvol:      mm   change in groundwater volume during the year'
        write(out_gwbal_aa,*) 'rech:      mm   soil water added to groundwater'
        write(out_gwbal_aa,*) 'gwet:      mm   groundwater removed by evapotranspiration'
        write(out_gwbal_aa,*) 'gwsw:      mm   groundwater discharge to streams'
        write(out_gwbal_aa,*) 'swgw:      mm   stream water seepage to groundwater'
        write(out_gwbal_aa,*) 'satx:      mm   saturation excess flow (water table above ground)'
        write(out_gwbal_aa,*) 'soil:      mm   groundwater transferred to HRU soil profile'
        write(out_gwbal_aa,*) 'latl:      mm   groundwater transferred between cells'
        write(out_gwbal_aa,*) 'bndr:      mm   groundwater added/removed at watershed boundary'
        write(out_gwbal_aa,*) 'ppag:      mm   groundwater pumped for irrigation'
        write(out_gwbal_aa,*) 'ppex:      mm   groundwater pumping specified by user'
        write(out_gwbal_aa,*) 'tile:      mm   groundwater removed via tile drains'
        write(out_gwbal_aa,*) 'resv:      mm   groundwater exchanged with reservoirs'
        write(out_gwbal_aa,*) 'wetl:      mm   groundwater outflow to wetlands'
        write(out_gwbal_aa,*) 'canl:      mm   canal seepage to groundwater'
        write(out_gwbal_aa,*) 'fpln:      mm   floodplain exchange'
        write(out_gwbal_aa,*) 'ppdf:      mm   groundwater demand not satisfied for irrigation'
        write(out_gwbal_aa,*)
        gwflow_hdr_aa = [character(len=18) :: "  year","dvol","rech","gwet","gwsw","swgw","satx","soil",  &
                     "latl","bndr","ppag","ppex","tile","resv","wetl","canl","fpln","ppdf"]
        write(out_gwbal_aa,120) (gwflow_hdr_aa(j),j=1,18)
      endif
      
      !open file to write out average annual groundwater water balance for each HUC12 catchment
      !average annual results
      if (nat_model == 1) then
      open(out_huc12wb,file='gwflow_balance_huc12')
      write(out_huc12wb,*) 'Total groundwater fluxes for each HUC12'
      write(out_huc12wb,*)
      write(out_huc12wb,*) 'Positive value: groundwater added to aquifer'
      write(out_huc12wb,*) 'Negative value: groundwater removed from aquifer'
      write(out_huc12wb,*)
      write(out_huc12wb,*) 'rech:          mm   soil water added to groundwater'
      write(out_huc12wb,*) 'gwet:          mm   groundwater removed by evapotranspiration'
      write(out_huc12wb,*) 'gwsw:          mm   groundwater discharge to streams'
      write(out_huc12wb,*) 'swgw:          mm   stream water seepage to groundwater'
      write(out_huc12wb,*) 'satex:         mm   saturation excess flow (water table above ground)'
      write(out_huc12wb,*) 'gwsoil:        mm   groundwater transferred to HRU soil profile'
      write(out_huc12wb,*) 'lateral:       mm   groundwater added/removed via lateral flow'
      write(out_huc12wb,*) 'pump_ag:       mm   groundwater pumped for irrigation'
      write(out_huc12wb,*) 'pump_ex:       mm   groundwater pumping specified by user'
      write(out_huc12wb,*) 'tile:          mm   groundwater removed via tile drains'
      write(out_huc12wb,*) 'res:           mm   groundwater exchanged with reservoirs'
      write(out_huc12wb,*) 'wet:           mm   groundwater outflow to wetlands'
      write(out_huc12wb,*) 'canal:         mm   canal seepage to groundwater'
      write(out_huc12wb,*) 'fplain:        mm   floodplain exchange'
      write(out_huc12wb,*) 'pump_def:      mm   groundwater demand not satisfied for irrigation'
      write(out_huc12wb,*)
      gwflow_hdr_huc12 = [character(len=16) :: "  HUC12","rech","gwet","gwsw","swgw","satex","gwsoil","lateral","pump_ag",  &
              "pump_ex","tile","res","wet","canal","fplain","pump_def"]
      write(out_huc12wb,122) (gwflow_hdr_huc12(j),j=1,16)
      allocate(gw_huc12_wb(15,sp_ob%outlet))
      gw_huc12_wb = 0.
      !monthly results
      open(out_huc12wb_mo,file='gwflow_balance_huc12_mon')
      write(out_huc12wb_mo,*) 'Monthly total groundwater fluxes for each HUC12'
      write(out_huc12wb_mo,*)
      write(out_huc12wb_mo,*) 'Positive value: groundwater added to aquifer'
      write(out_huc12wb_mo,*) 'Negative value: groundwater removed from aquifer'
      write(out_huc12wb_mo,*)
      write(out_huc12wb_mo,*) 'rech:          mm   soil water added to groundwater'
      write(out_huc12wb_mo,*) 'gwet:          mm   groundwater removed by evapotranspiration'
      write(out_huc12wb_mo,*) 'gwsw:          mm   groundwater discharge to streams'
      write(out_huc12wb_mo,*) 'swgw:          mm   stream water seepage to groundwater'
      write(out_huc12wb_mo,*) 'satex:         mm   saturation excess flow (water table above ground)'
      write(out_huc12wb_mo,*) 'gwsoil:        mm   groundwater transferred to HRU soil profile'
      write(out_huc12wb_mo,*) 'lateral:       mm   groundwater added/removed via lateral flow'
      write(out_huc12wb_mo,*) 'pump_ag:       mm   groundwater pumped for irrigation'
      write(out_huc12wb_mo,*) 'pump_ex:       mm   groundwater pumping specified by user'
      write(out_huc12wb_mo,*) 'tile:          mm   groundwater removed via tile drains'
      write(out_huc12wb_mo,*) 'res:           mm   groundwater exchanged with reservoirs'
      write(out_huc12wb_mo,*) 'wet:           mm   groundwater outflow to wetlands'
      write(out_huc12wb_mo,*) 'pump_def:      mm   groundwater demand not satisfied for irrigation'
      write(out_huc12wb_mo,*) 'canal:         mm   canal seepage to groundwater'
      write(out_huc12wb_mo,*) 'fplain:        mm   floodplain exchange'
      write(out_huc12wb_mo,*)
      gwflow_hdr_huc12_mo = [character(len=18) :: "year","month","  HUC12","rech","gwet","gwsw","swgw","satex",  &
          "gwsoil","lateral","pump_ag","pump_ex","tile","res","wet","canal","fplain","pump_def"]
      write(out_huc12wb_mo,122) (gwflow_hdr_huc12_mo(j),j=1,18)
      allocate(gw_huc12_wb_mo(15,sp_ob%outlet))
      gw_huc12_wb_mo = 0.
      endif
      
      
      !initialize solute mass balance ---------------------------------------------------------------------------------------------------------------
      if (gw_solute_flag == 1) then
      
        !allocate yearly and total arrays
        allocate(sol_grid_chng_yr(gw_nsolute))
        allocate(sol_grid_rech_yr(gw_nsolute))
        allocate(sol_grid_gwsw_yr(gw_nsolute))
        allocate(sol_grid_swgw_yr(gw_nsolute))
        allocate(sol_grid_satx_yr(gw_nsolute))
        allocate(sol_grid_advn_yr(gw_nsolute))
        allocate(sol_grid_disp_yr(gw_nsolute))
        allocate(sol_grid_rcti_yr(gw_nsolute))
        allocate(sol_grid_rcto_yr(gw_nsolute))
        allocate(sol_grid_minl_yr(gw_nsolute))
        allocate(sol_grid_sorb_yr(gw_nsolute))
        allocate(sol_grid_ppag_yr(gw_nsolute))
        allocate(sol_grid_ppex_yr(gw_nsolute))
        allocate(sol_grid_tile_yr(gw_nsolute))
        allocate(sol_grid_soil_yr(gw_nsolute))
        allocate(sol_grid_resv_yr(gw_nsolute))
        allocate(sol_grid_wetl_yr(gw_nsolute))
        allocate(sol_grid_canl_yr(gw_nsolute))
        allocate(sol_grid_fpln_yr(gw_nsolute))
        allocate(sol_grid_chng_tt(gw_nsolute))
        allocate(sol_grid_rech_tt(gw_nsolute))
        allocate(sol_grid_gwsw_tt(gw_nsolute))
        allocate(sol_grid_swgw_tt(gw_nsolute))
        allocate(sol_grid_satx_tt(gw_nsolute))
        allocate(sol_grid_advn_tt(gw_nsolute))
        allocate(sol_grid_disp_tt(gw_nsolute))
        allocate(sol_grid_rcti_tt(gw_nsolute))
        allocate(sol_grid_rcto_tt(gw_nsolute))
        allocate(sol_grid_minl_tt(gw_nsolute))
        allocate(sol_grid_sorb_tt(gw_nsolute))
        allocate(sol_grid_ppag_tt(gw_nsolute))
        allocate(sol_grid_ppex_tt(gw_nsolute))
        allocate(sol_grid_tile_tt(gw_nsolute))
        allocate(sol_grid_soil_tt(gw_nsolute))
        allocate(sol_grid_resv_tt(gw_nsolute))
        allocate(sol_grid_wetl_tt(gw_nsolute))
        allocate(sol_grid_canl_tt(gw_nsolute))
        allocate(sol_grid_fpln_tt(gw_nsolute))
        
        !loop through the solutes
        do n=1,gw_nsolute
        
          !daily solute mass balance
          if(gwflag_day.eq.1) then
            file_name = 'gwflow_balance_day_'//gwsol_nm(n)
            open(out_solbal_dy+n,file=file_name)
            write(out_solbal_dy+n,*) 'Solute:',gwsol_nm(n)
            write(out_solbal_dy+n,*) 'Groundwater watershed-wide solute loads for each day'
            write(out_solbal_dy+n,*)
            write(out_solbal_dy+n,*) 'Positive value: solute mass added to aquifer'
            write(out_solbal_dy+n,*) 'Negative value: solute mass removed from aquifer'
            write(out_solbal_dy+n,*)
            write(out_solbal_dy+n,*) 'ts:        days time step used for groundwater solute calculations'
            write(out_solbal_dy+n,*) 'mbef:      kg   total groundwater solute mass at the beginning of the day'
            write(out_solbal_dy+n,*) 'maft:      kg   total groundwater solute mass at the end of the day'
            write(out_solbal_dy+n,*) 'rech:      kg   solute mass in recharge water'
            write(out_solbal_dy+n,*) 'gwsw:      kg   solute mass loaded to streams'
            write(out_solbal_dy+n,*) 'swgw:      kg   solute mass loaded from streams'
            write(out_solbal_dy+n,*) 'satx:      kg   solute mass loaded to streams by saturation excess flow'
            write(out_solbal_dy+n,*) 'soil:      kg   solute mass loaded to HRU soil profiles'
            write(out_solbal_dy+n,*) 'advn:      kg   solute mass transported by advection'
            write(out_solbal_dy+n,*) 'disp:      kg   solute mass transported by dispersion'
            write(out_solbal_dy+n,*) 'rcti:      kg   solute mass produced by kinetic reaction'
            write(out_solbal_dy+n,*) 'rcto:      kg   solute mass consumed by kinetic reaction'
            write(out_solbal_dy+n,*) 'minl:      kg   solute mass added by mineral dissolution'            
            write(out_solbal_dy+n,*) 'sorb:      kg   solute mass removed by sorption'
            write(out_solbal_dy+n,*) 'ppag:      kg   solute mass removed by groundwater pumping for irrigation'
            write(out_solbal_dy+n,*) 'ppex:      kg   solute mass removed by groundwater pumping specified by user'
            write(out_solbal_dy+n,*) 'tile:      kg   solute mass removed by tile drains'
            write(out_solbal_dy+n,*) 'resv:      kg   solute mass loaded to/from reservoirs'
            write(out_solbal_dy+n,*) 'wetl:      kg   solute mass loaded to/from wetlands'
            write(out_solbal_dy+n,*) 'canl:      kg   solute mass loaded to groundwater from canal seepage'
            write(out_solbal_dy+n,*) 'fpln:      kg   solute mass in floodplain exchange'
            write(out_solbal_dy+n,*) 'error:     --   mass balance error for aquifer'
            write(out_solbal_dy+n,*)
            sol_hdr_day = [character(len=24) :: "  year","   day","ts","mbef","maft","rech","gwsw","swgw","satx","soil","advn", &
                            "disp","rcti","rcto","minl","sorb","ppag","ppex","tile","resv","wetl","canl","fpln","error"]
            write(out_solbal_dy+n,119) (sol_hdr_day(j),j=1,24)
          endif

          !yearly solute mass balance
          if(gwflag_yr.eq.1) then
            file_name = 'gwflow_balance_yr_'//gwsol_nm(n)
            open(out_solbal_yr+n,file=file_name)
            write(out_solbal_yr+n,*) 'Solute:',gwsol_nm(n)
            write(out_solbal_yr+n,*) 'Groundwater watershed-wide solute loads for each year'
            write(out_solbal_yr+n,*)
            write(out_solbal_yr+n,*) 'Positive value: solute mass added to aquifer'
            write(out_solbal_yr+n,*) 'Negative value: solute mass removed from aquifer'
            write(out_solbal_yr+n,*)
            write(out_solbal_yr+n,*) 'delm:     kg   change in groundwater solute mass during the year'
            write(out_solbal_yr+n,*) 'rech:     kg   solute mass in recharge water'
            write(out_solbal_yr+n,*) 'gwsw:     kg   solute mass loaded to streams'
            write(out_solbal_yr+n,*) 'swgw:     kg   solute mass loaded from streams'
            write(out_solbal_yr+n,*) 'satx:     kg   solute mass loaded to streams by saturation excess flow'
            write(out_solbal_yr+n,*) 'soil:     kg   solute mass loaded to HRU soil profiles'
            write(out_solbal_yr+n,*) 'advn:     kg   solute mass transported by advection'
            write(out_solbal_yr+n,*) 'disp:     kg   solute mass transported by dispersion'
            write(out_solbal_yr+n,*) 'rcti:      kg   solute mass produced by kinetic reaction'
            write(out_solbal_yr+n,*) 'rcto:      kg   solute mass consumed by kinetic reaction'
            write(out_solbal_yr+n,*) 'minl:      kg   solute mass added by mineral dissolution' 
            write(out_solbal_yr+n,*) 'sorb:     kg   solute mass removed by denitrification'
            write(out_solbal_yr+n,*) 'ppag:     kg   solute mass removed by groundwater pumping for irrigation'
            write(out_solbal_yr+n,*) 'ppex:     kg   solute mass removed by groundwater pumping specified by user'
            write(out_solbal_yr+n,*) 'tile:     kg   solute mass removed by tile drains'
            write(out_solbal_yr+n,*) 'resv:     kg   solute mass loaded to/from reservoirs'
            write(out_solbal_yr+n,*) 'wetl:     kg   solute mass loaded to/from wetlands'
            write(out_solbal_yr+n,*) 'canl:     kg   solute mass loaded to groundwater from canal seepage'
            write(out_solbal_yr+n,*) 'fpln:     kg   solute mass in floodplain exchange'
            write(out_solbal_yr+n,*)
            sol_hdr_yr = [character(len=20) :: "  year","delm","rech","gwsw","swgw","satx","soil","advn","disp",  &
                "rcti","rcto","minl","sorb","ppag","ppex","tile","resv","wetl","canl","fpln"]
            write(out_solbal_yr+n,120) (sol_hdr_yr(j),j=1,20)
            !zero out yearly arrays
            sol_grid_chng_yr(n) = 0.
            sol_grid_rech_yr(n) = 0.
            sol_grid_gwsw_yr(n) = 0.
            sol_grid_swgw_yr(n) = 0.
            sol_grid_satx_yr(n) = 0.
            sol_grid_advn_yr(n) = 0.
            sol_grid_disp_yr(n) = 0.
            sol_grid_rcti_yr(n) = 0.
            sol_grid_rcto_yr(n) = 0.
            sol_grid_minl_yr(n) = 0.
            sol_grid_sorb_yr(n) = 0.
            sol_grid_ppag_yr(n) = 0.
            sol_grid_ppex_yr(n) = 0.
            sol_grid_tile_yr(n) = 0.
            sol_grid_soil_yr(n) = 0.
            sol_grid_resv_yr(n) = 0.
            sol_grid_wetl_yr(n) = 0.
            sol_grid_canl_yr(n) = 0.
            sol_grid_fpln_yr(n) = 0.
          endif
          
          !average annual solute mass balance
          if(gwflag_aa.eq.1) then
            file_name = 'gwflow_balance_aa_'//gwsol_nm(n)
            open(out_solbal_aa+n,file=file_name)
            write(out_solbal_aa+n,*) 'Solute:',gwsol_nm(n)
            write(out_solbal_aa+n,*) 'Average annual groundwater watershed-wide solute loads'
            write(out_solbal_aa+n,*)
            write(out_solbal_aa+n,*) 'Positive value: solute mass added to aquifer'
            write(out_solbal_aa+n,*) 'Negative value: solute mass removed from aquifer'
            write(out_solbal_aa+n,*)
            write(out_solbal_aa+n,*) 'delm:      kg   total change in groundwater solute mass across all years'
            write(out_solbal_aa+n,*) 'rech:      kg   solute mass in recharge water'
            write(out_solbal_aa+n,*) 'gwsw:      kg   solute mass loaded to streams'
            write(out_solbal_aa+n,*) 'swgw:      kg   solute mass loaded from streams'
            write(out_solbal_aa+n,*) 'satx:      kg   solute mass loaded to streams by saturation excess flow'
            write(out_solbal_aa+n,*) 'soil:      kg   solute mass loaded to HRU soil profiles'
            write(out_solbal_aa+n,*) 'advn:      kg   solute mass transported by advection'
            write(out_solbal_aa+n,*) 'disp:      kg   solute mass transported by dispersion'
            write(out_solbal_aa+n,*) 'rcti:      kg   solute mass produced by kinetic reaction'
            write(out_solbal_aa+n,*) 'rcto:      kg   solute mass consumed by kinetic reaction'
            write(out_solbal_aa+n,*) 'minl:      kg   solute mass added by mineral dissolution' 
            write(out_solbal_aa+n,*) 'sorb:      kg   solute mass removed via sorption'
            write(out_solbal_aa+n,*) 'ppag:      kg   solute mass removed by groundwater pumping for irrigation'
            write(out_solbal_aa+n,*) 'ppex:      kg   solute mass removed by groundwater pumping specified by user'
            write(out_solbal_aa+n,*) 'tile:      kg   solute mass removed by tile drains'
            write(out_solbal_aa+n,*) 'resv:      kg   solute mass loaded to/from reservoirs'
            write(out_solbal_aa+n,*) 'wetl:      kg   solute mass loaded to/from wetlands'
            write(out_solbal_aa+n,*) 'canl:      kg   solute mass loaded to groundwater from canal seepage'
            write(out_solbal_aa+n,*) 'fpln:      kg   solute mass in floodplain exchange'
            write(out_solbal_aa+n,*)
            sol_hdr_aa = [character(len=20) :: "  year","delm","rech","gwsw","swgw","satx","soil","advn","disp",  &
                "rcti","rcto","minl","sorb","ppag","ppex","tile","resv","wetl","canl","fpln"]
            write(out_solbal_aa+n,120) (sol_hdr_aa(j),j=1,20)
            !zero out yearly arrays
            sol_grid_chng_tt(n) = 0.
            sol_grid_rech_tt(n) = 0.
            sol_grid_gwsw_tt(n) = 0.
            sol_grid_swgw_tt(n) = 0.
            sol_grid_satx_tt(n) = 0.
            sol_grid_advn_tt(n) = 0.
            sol_grid_disp_tt(n) = 0.
            sol_grid_rcti_tt(n) = 0.
            sol_grid_rcto_tt(n) = 0.
            sol_grid_minl_tt(n) = 0.
            sol_grid_sorb_tt(n) = 0.
            sol_grid_ppag_tt(n) = 0.
            sol_grid_ppex_tt(n) = 0.
            sol_grid_tile_tt(n) = 0.
            sol_grid_soil_tt(n) = 0.
            sol_grid_resv_tt(n) = 0.
            sol_grid_wetl_tt(n) = 0.
            sol_grid_canl_tt(n) = 0.
            sol_grid_fpln_tt(n) = 0.
          endif
          
        enddo !go to next solute
        
      endif !check for solutes
      

      
      !prepare additional arrays for start of simulation ----------------------------------------------------
      
      !map cell groundwater delay terms to each HRU
      !default value (days) = average of all grid cell values
      write(out_gw,*)
      write(out_gw,*) '     final preparations...'
      write(out_gw,*) '          map recharge delay to HRUs' 
      allocate(gw_delay(num_hru))
      allocate(gw_rech(num_hru))
      sum = 0.
      do i=1,ncell
        sum = sum + delay(i)    
      enddo
      gw_delay = Exp(-1./((sum/(ncell)) + 1.e-6))
      !assign value to HRU based on connected grid cells
      if(lsu_cells_link == 0) then !if LSU-cell connection: keep average of all grid cell values
        do k=1,num_hru
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
      if (gw_solute_flag == 1) then
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
      open(out_gwheads,file='gwflow_state_head')
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
      if (gw_solute_flag == 1) then
        open(out_gwconc,file='gwflow_state_conc')
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
      open(out_head_mo,file='gwflow_state_head_mo')
      open(out_head_yr,file='gwflow_state_head_yr')
      write(out_head_mo,*) 'Monthly average groundwater head (m) for each grid cell'
      write(out_head_mo,*)
      write(out_head_yr,*) 'Annual average groundwater head (m) for each grid cell'
      write(out_head_yr,*)
      
      !prepare output file and arrays for monthly and annual average solute concentration
      if (gw_solute_flag == 1) then
        write(out_gw,*) '          prepare output files for monthly and annual average solute conc.'
        open(out_conc_mo,file='gwflow_state_conc_mo')
        open(out_conc_yr,file='gwflow_state_conc_yr')
        write(out_conc_mo,*) 'Monthly average solute concentration (g/m3) for each grid cell'
        write(out_conc_mo,*)
        write(out_conc_yr,*) 'Annual average solute concentration (g/m3) for each grid cell'
        write(out_conc_yr,*)
      endif
      
      
      !read in monthly streamflow data (if available)
      inquire(file='gwflow.streamobs',exist=i_exist)
      if (stream_obs == 1) then
        open(in_str_obs,file='gwflow.streamobs')
        open(out_strobs,file='gwflow_state_obs_flow')
        write(out_strobs,*) 'Channels: observed vs. simulated monthly values'
        read(in_str_obs,*)
        read(in_str_obs,*) gw_num_obs_chan
        if(gw_num_obs_chan.gt.0) then
          num_months = time%nbyr * 12
          allocate(obs_channels(gw_num_obs_chan))
          allocate(obs_flow_vals(gw_num_obs_chan,num_months))
          allocate(sim_flow_vals(gw_num_obs_chan,num_months))
          !read in the observation values
          do i=1,gw_num_obs_chan
            read(in_str_obs,*) obs_channels(i)
          enddo
          read(in_str_obs,*)
          read(in_str_obs,*) gw_flow_cal !flag for dividing years into calibration vs. testing
          if(gw_flow_cal.eq.1) then
            read(in_str_obs,*) gw_flow_cal_yrs !number of years for calibration
          endif
          read(in_str_obs,*)
          do i=1,num_months
            read(in_str_obs,*) (obs_flow_vals(j,i),j=1,gw_num_obs_chan)
          enddo
          sim_month = 1 !start month counter for the simulation
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
      hydsep_hdr = [character(len=10) :: "  year","   day","channel","chan_surf","chan_lat","chan_gwsw","chan_swgw",  &
               "chan_satexgw","chan_satexsw","chan_tile"]
      write(out_hyd_sep,121) (hydsep_hdr(j),j=1,10)      
      
      !gwflow record file (skip line)
      write(out_gw,*)
      write(out_gw,*)
      write(out_gw,*) 'record of daily gwflow use...'
      write(out_gw,*)
      
      return
      
       
100   format(i6,i6,10(f10.2))
!output files for all cells
!101   format(<out_cols>(f12.4))
!102   format(<out_cols>(i4))
101   format(f12.4)
102   format(i4)
    !other formats
103   format(10000(i8))
111   format(1x,a, 5x,"Time",2x,i2,":",i2,":",i2)
119   format(4x,a8,a8,a10,a16,a19,50(a13))
120   format(a8,50(a13))
121   format(50(a16))
122   format(a20,50(a13))
123   format(a10,1000(i12))   
130   format(10000(f12.3))

      end subroutine gwflow_read
      
           
      