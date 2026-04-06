      subroutine gwflow_lateral

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates lateral groundwater flow between adjacent cells
!!    using Darcy's Law, then updates head and storage
!!    (extracted from gwflow_simulate)

      use gwflow_module
      use time_module

      implicit none

      !local variables - flow calculation
      integer :: i, j, k, n, cell_id
      integer :: num_ts
      real :: area1, area2, area
      real :: conn_length, dist_x, dist_y, grad_distance
      real :: Q_cell, Q
      real :: face_K, sat_thick1, sat_thick2, face_sat
      real :: stor_change, sat_change, flow_area, gradient

      !local variables - transit time
      real :: qs, vs, ds, dist_frac, q_dir_x, q_dir_y
      real :: cell_length, x_min, x_max, y_min, y_max

      !local variables - transit time output
      integer :: cell_transit, line_num
      real, allocatable, save :: line_vals(:)


      !determine number of flow time steps
      num_ts = int(1./gw_time_step)

      !prepare arrays
      do i=1,ncell
        gw_state(i)%hnew = 0.
        gw_state(i)%hold = 0.
      enddo

      !calculate new storage and head for each cell
      do n=1,num_ts
        do i=1,ncell

          !only proceed if the cell is active
          if(gw_state(i)%stat > 0) then

            !if the cell is interior (not a boundary cell)
            if(gw_state(i)%stat == 1) then

              !loop through the cells connected to the current cell
              Q = 0.
              gw_state(i)%delx = 0.
              gw_state(i)%dely = 0.
              do k=1,gw_state(i)%ncon
                !id of the connected cell
                cell_id = cell_con(i)%cell_id(k)
                Q_cell = 0.
                !calculate groundwater flow between the cells, using Darcy's Law
                if(gw_state(cell_id)%stat == 0) then
                  Q_cell = 0.
                elseif(gw_state(cell_id)%stat == 2 .and. bc_type_array(cell_id) == 2) then !boundary cell
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
                  !travel time calculations
                  if(gw_ttime == 1) then
                    if(Q_cell.ne.0) then
                      qs = face_K * gradient !specific discharge (m/day)
                      vs = qs / gw_state(cell_id)%spyd !linear velocity (m/day)
                      ds = abs(vs * gw_time_step) !distance of travel (m) along the flow path
                      dist_frac = ds / grad_distance !fraction of distance between the cell centroids
                      !determine direction (x axis)
                      if(gw_state(cell_id)%xcrd >= gw_state(i)%xcrd) then
                        if(Q_cell > 0) then
                          q_dir_x = -1
                        else
                          q_dir_x = 1
                        endif
                      else
                        if(Q_cell > 0) then
                          q_dir_x = 1
                        else
                          q_dir_x = -1
                        endif
                      endif
                      !determine direction (y axis)
                      if(gw_state(cell_id)%ycrd >= gw_state(i)%ycrd) then
                        if(Q_cell > 0) then
                          q_dir_y = -1
                        else
                          q_dir_y = 1
                        endif
                      else
                        if(Q_cell > 0) then
                          q_dir_y = 1
                        else
                          q_dir_y = -1
                        endif
                      endif
                      !cumulative for the cell (for this time step)
                      gw_state(i)%delx = gw_state(i)%delx + (dist_frac * abs((gw_state(i)%xcrd-gw_state(cell_id)%xcrd)) * q_dir_x) !m change in x
                      gw_state(i)%dely = gw_state(i)%dely + (dist_frac * abs((gw_state(i)%ycrd-gw_state(cell_id)%ycrd)) * q_dir_y) !m change in y
                    endif
                  endif
                endif
                !store for cell water balance
                if(gw_state(cell_id)%stat == 2) then !boundary flow
                  gw_hyd_ss(i)%bndr = gw_hyd_ss(i)%bndr + (Q_cell*gw_time_step)
                else
                  gw_hyd_ss(i)%latl = gw_hyd_ss(i)%latl + (Q_cell*gw_time_step)
                  gw_hyd_ss_yr(i)%latl = gw_hyd_ss_yr(i)%latl + (Q_cell*gw_time_step)
                endif
                !sum total flow to/from current cell
                Q = Q + Q_cell
              enddo !go to next connected cell

              !update storage and head for the cell
              stor_change = (Q + gw_hyd_ss(i)%totl) * gw_time_step !change in storage (m3)
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

        !heat transport (within each flow time step)
        if(gw_heat_flag == 1) then
          call gwflow_heat
        endif

        !solute transport (within each flow time step)
        if(gw_solute_flag == 1) then
          call gwflow_solute
        endif

      enddo !go to next flow time step

      !update groundwater travel time; write out xy coordinates and cumulative time for selected cells
      if(gw_ttime == 1) then
        !determine cell, xy coordinates of groundwater
        do i=1,ncell
          !determine the cell in which groundwater is located
          !first, check if is in the same cell
          cell_id = gw_transit(i)%cell
          cell_length = sqrt(gw_state(cell_id)%area)
          x_min = gw_state(cell_id)%xcrd - (cell_length/2)
          x_max = gw_state(cell_id)%xcrd + (cell_length/2)
          y_min = gw_state(cell_id)%ycrd - (cell_length/2)
          y_max = gw_state(cell_id)%ycrd + (cell_length/2)
          if(gw_transit(i)%x > x_min .and. gw_transit(i)%x < x_max) then
            if(gw_transit(i)%y > y_min .and. gw_transit(i)%y < y_max) then
              !gw_transit(i)%cell = stays the same
              !update location (x,y) of groundwater
              gw_transit(i)%x = gw_transit(i)%x + gw_state(cell_id)%delx
              gw_transit(i)%y = gw_transit(i)%y + gw_state(cell_id)%dely
              goto 10
            endif
          endif
          !if not, search other cells
          do j=1,ncell
            cell_length = sqrt(gw_state(j)%area)
            x_min = gw_state(j)%xcrd - (cell_length/2)
            x_max = gw_state(j)%xcrd + (cell_length/2)
            y_min = gw_state(j)%ycrd - (cell_length/2)
            y_max = gw_state(j)%ycrd + (cell_length/2)
            if(gw_transit(i)%x > x_min .and. gw_transit(i)%x < x_max) then
              if(gw_transit(i)%y > y_min .and. gw_transit(i)%y < y_max) then
                gw_transit(i)%cell = j !cell where groundwater is located
                !update location (x,y) of groundwater
                gw_transit(i)%x = gw_transit(i)%x + gw_state(j)%delx
                gw_transit(i)%y = gw_transit(i)%y + gw_state(j)%dely
                goto 10
              endif
            endif
          enddo
10        continue
          gw_transit(i)%t = gw_transit(i)%t + gw_time_step !days
          !determine if the current cell is a channel cell
          cell_id = gw_transit(i)%cell
          if(gw_cell_chan_flag(cell_id) == 1) then !channel cell
            if(gw_cell_chan_time(i) == 0.) then !only if not already stored
              gw_cell_chan_time(i) = gw_transit(i)%t
            endif
          endif
          !determine if the current cell is a tile cell, and if there is drainage to tile
          if(gw_tile_flag == 1) then
            cell_id = gw_transit(i)%cell
            if(gw_state(cell_id)%tile == 1) then !tile cell
              if(gw_cell_tile_time(i) == 0.) then !only if not already stored
                gw_cell_tile_time(i) = gw_transit(i)%t
              endif
            endif
          endif
        enddo
        !write out results for specified cells
        if(.not.allocated(line_vals)) allocate(line_vals(gw_transit_num*4), source=0.)
        line_vals = 0.
        line_num = 1
        do i=1,gw_transit_num
          cell_id = gw_transit_cells(i)
          line_vals(line_num) = gw_transit(cell_id)%t
          line_vals(line_num+1) = gw_transit(cell_id)%x
          line_vals(line_num+2) = gw_transit(cell_id)%y
          line_vals(line_num+3) = gw_transit(cell_id)%cell
          line_num = line_num + 4
        enddo
        line_num = line_num - 1
        write(out_gw_transit,100) (line_vals(i),i=1,line_num)
      endif

      return
100   format(99999(f20.4))
      end subroutine gwflow_lateral
