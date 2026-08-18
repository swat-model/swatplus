
      !This subroutine reads in channel-cell connection information from chancell.gw.
      !gwflow.con is produced by the SWAT+ editor and consumed downstream by hyd_read_connect;
      !no longer regenerated here.

      !  Prepared by: Ryan Bailey, Colorado State University

      subroutine gwflow_chan_read

      use gwflow_module
      use hydrograph_module
      use utils, only : split_line

      implicit none

      character*30 :: header
      character(len=2500) :: line_buf = ''
      character(len=50) :: fields(40) = ''
      integer :: nf = 0
      integer :: k = 0
      integer :: cell_id = 0
      integer :: channel = 0
      integer :: chan_zone = 0
      integer :: dep_zone = 0
      integer :: obs = 0
      integer :: nobs = 0
      real :: bed_elev = 0.
      real :: chan_length = 0.
      logical :: i_exist

      !message to record file
      write(out_gw,*) 'reading cell-channel connections in chancell.gw...'

      open(1280,file='chancell.gw')

      !number of cells that intersect with channels
      num_chancells = sp_ob%gwflow

      !allocate global arrays for channel cells
      allocate (gw_chan_id(num_chancells), source = 0)
      allocate (gw_chan_chan(num_chancells), source = 0)
      allocate (gw_chan_len(num_chancells), source = 0.)
      allocate (gw_chan_elev(num_chancells), source = 0.)
      allocate (gw_chan_zone(num_chancells), source = 0)
      allocate (gw_chan_dpzn(num_chancells), source = 0)
      allocate (gw_chan_obs(num_chancells), source = 0)
      gw_chan_len = 0.

      !read channel-cell connections. columns: cell_id elev_m channel riv_length_m zone [dep_zone] [obs]
      !dep_zone (col 6) and obs (col 7) are optional trailing columns (merged from the former
      !gwflow.chancells_depth zone part and gwflow.chancells_obs)
      read(1280,*)                                  !meta line
      read(1280,*)                                  !column header
      nobs = 0
      do k=1,num_chancells
        read(1280,'(a)') line_buf
        call split_line(line_buf, fields, nf)
        read(fields(1),*) cell_id
        read(fields(2),*) bed_elev
        read(fields(3),*) channel
        read(fields(4),*) chan_length
        read(fields(5),*) chan_zone
        dep_zone = 0
        obs = 0
        if(nf >= 6) read(fields(6),*) dep_zone
        if(nf >= 7) read(fields(7),*) obs
        gw_chan_id(k) = cell_id
        gw_chan_elev(k) = bed_elev
        gw_chan_chan(k) = channel
        gw_chan_len(k) = chan_length
        gw_chan_zone(k) = chan_zone
        gw_chan_dpzn(k) = dep_zone
        gw_chan_obs(k) = obs
        if(obs > 0) nobs = nobs + 1
      enddo
      close(1280)

      !channel observation cells (was gwflow.chancells_obs): the obs flag column marks which cells
      !emit daily gw-channel exchange output. The active-cell ids (gw_chan_obs_cell) are filled in
      !gwflow_read once gw_chan_cell (the structured->active mapping) is built.
      gw_chan_nobs = nobs
      if(nobs > 0) gw_chan_obs_flag = 1

      !daily channel depths per depth zone (was the time-series part of gwflow.chancells_depth)
      !now a separate flat file chan_depth.gw: meta + header + one row per sim day (jday yr depth_zone1..)
      inquire(file='chan_depth.gw',exist=i_exist)
      if(i_exist) then
        gw_chan_dep_flag = 1
        open(1421,file='chan_depth.gw')
        read(1421,*) header                         !meta line
        read(1421,*) header                         !column header
        gw_chan_ndpzn = maxval(gw_chan_dpzn(1:num_chancells))
        allocate(gw_chan_dep(gw_chan_ndpzn), source = 0.)
        !daily depth rows (jday yr depth per zone) are read during the simulation (gwflow_simulate)
      endif

      end subroutine gwflow_chan_read
