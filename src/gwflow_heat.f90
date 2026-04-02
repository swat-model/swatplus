      subroutine gwflow_heat !rtb gwflow

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates heat advection and dispersion for groundwater
!!    heat transport. Called from gwflow_lateral once per flow time step, after
!!    the Darcy head update. Uses cell_con%latl and cell_con%sat populated
!!    during the flow calculation in the same time step.

      use gwflow_module
      use time_module

      implicit none

      integer :: i = 0                    !       |cell counter
      integer :: k = 0                    !       |connected-cell counter
      integer :: cell_id = 0              !       |id of connected cell
      real :: heat_adv = 0.               !J/day  |total advective heat flux for current cell
      real :: cell_adv = 0.               !J/day  |advective heat flux from one connection
      real :: heat_dsp = 0.               !J/day  |total dispersive heat flux for current cell
      real :: Q_heat = 0.                 !J/day  |dispersive heat flux from one connection
      real :: heat_change = 0.            !J      |net change in heat storage for current cell
      real :: face_thmc = 0.              !J/d-m-T|harmonic mean thermal conductivity at interface
      real :: Q_cell = 0.                 !m3/day |lateral flow between cells
      real :: dist_x = 0.                 !m      |x distance between cell centroids
      real :: dist_y = 0.                 !m      |y distance between cell centroids
      real :: grad_distance = 0.          !m      |distance between cell centroids
      real :: gradient = 0.               !T/m    |temperature gradient
      real :: area1 = 0.                  !m2     |area of connected cell
      real :: area2 = 0.                  !m2     |area of current cell
      real :: area = 0.                   !m2     |smaller of the two areas
      real :: conn_length = 0.            !m      |connection length
      real :: face_sat = 0.               !m      |saturated thickness at interface
      real :: flow_area = 0.              !m2     |cross-section area at interface

      !store current heat (J) - used to track heat storage during cell loop
      do i=1,ncell
        heat_cell(i) = gwheat_state(i)%stor !J
      enddo

      !loop through cells - update heat and temperature
      do i=1,ncell
        if(gw_state(i)%stat == 1) then !interior cell

          !starting heat in the cell
          heat_cell(i) = gwheat_state(i)%stor !J

          !advection heat transport (transported with groundwater flow)
          heat_adv = 0.
          do k=1,gw_state(i)%ncon !loop through the connected cells
            cell_id = cell_con(i)%cell_id(k) !id of the connected cell
            Q_cell = cell_con(i)%latl(k) !m3/day flow between cell and connected cell
            if(Q_cell > 0) then !heat entering cell
              cell_adv = Q_cell * gw_rho * gw_cp * gwheat_state(cell_id)%temp !J
              !check against available heat in contributing cell
              if(cell_adv > heat_cell(cell_id)) then
                cell_adv = heat_cell(cell_id)
                heat_cell(cell_id) = heat_cell(cell_id) - cell_adv
              endif
            else !heat leaving cell
              cell_adv = Q_cell * gw_rho * gw_cp * gwheat_state(i)%temp !J
              if((cell_adv*-1) > heat_cell(i)) then
                cell_adv = heat_cell(i) * -1 !can only take what is there
              endif
            endif
            heat_adv = heat_adv + cell_adv !J
            heat_cell(i) = heat_cell(i) + cell_adv
            if(gw_state(cell_id)%stat == 2) then !boundary flow
              gw_heat_ss(i)%bndr = gw_heat_ss(i)%bndr + (cell_adv*gw_time_step)
            else
              gw_heat_ss(i)%latl = gw_heat_ss(i)%latl + (cell_adv*gw_time_step)
              gw_heat_ss_yr(i)%latl = gw_heat_ss_yr(i)%latl + (cell_adv*gw_time_step)
            endif
          enddo !go to next connected cell

          !dispersive heat transport (Fourier's Law)
          heat_dsp = 0.
          do k=1,gw_state(i)%ncon !loop through the connected cells
            cell_id = cell_con(i)%cell_id(k) !id of the connected cell
            !temperature gradient (del_T/del_m)
            dist_x = gw_state(i)%xcrd - gw_state(cell_id)%xcrd !m
            dist_y = gw_state(i)%ycrd - gw_state(cell_id)%ycrd !m
            grad_distance = sqrt((dist_x)**2 + (dist_y)**2)
            gradient = (gwheat_state(cell_id)%temp - gwheat_state(i)%temp) / grad_distance
            !interface cross-section area (m2)
            area1 = gw_state(cell_id)%area !area of connected cell
            area2 = gw_state(i)%area !area of current cell
            area = min(area1,area2) !smaller of the two
            conn_length = sqrt(area) !connection length
            face_sat = cell_con(i)%sat(k) !m saturated thickness at interface between cells
            flow_area = face_sat * conn_length
            !heat flux (J/day) (harmonic mean of connected cells)
            face_thmc = conn_length / (((conn_length/2.)/gwheat_state(cell_id)%thmc) + &
                        ((conn_length/2.)/gwheat_state(i)%thmc))
            Q_heat = flow_area * face_thmc * gradient !m2 * J/d-m-T * T/m = J/d
            if(Q_heat > 0) then
              !check against available heat in contributing cell
              if(Q_heat > heat_cell(cell_id)) then
                Q_heat = heat_cell(cell_id)
                heat_cell(cell_id) = heat_cell(cell_id) - Q_heat
              endif
            endif
            if(Q_heat < 0) then
              !check against available heat in current cell
              if((Q_heat*-1) > heat_cell(i)) then
                Q_heat = heat_cell(i) * -1 !can only take what is there
              endif
            endif
            heat_dsp = heat_dsp + Q_heat
            heat_cell(i) = heat_cell(i) + Q_heat
          enddo !go to next connected cell
          gw_heat_ss(i)%disp = heat_dsp * gw_time_step
          gw_heat_ss_yr(i)%disp = gw_heat_ss_yr(i)%disp + (heat_dsp*gw_time_step)

          !change in heat (J); new heat and temperature
          heat_change = (heat_adv + heat_dsp + gw_heat_ss(i)%totl) * gw_time_step
          gwheat_state(i)%stor = gwheat_state(i)%stor + heat_change
          if(gw_state(i)%stor > 0) then
            gwheat_state(i)%tnew = gwheat_state(i)%stor / (gw_rho * gw_cp * gw_state(i)%stor) !deg C
          else
            gwheat_state(i)%tnew = 0.
          endif

        endif !check for interior cell

      enddo !go to the next cell

      !store new temperature values into regular array
      do i=1,ncell
        gwheat_state(i)%told = gwheat_state(i)%temp
        gwheat_state(i)%temp = gwheat_state(i)%tnew
      enddo

      return
      end subroutine gwflow_heat
