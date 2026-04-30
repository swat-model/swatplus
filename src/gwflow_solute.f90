      subroutine gwflow_solute !rtb gwflow

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates solute advection, dispersion, chemical
!!    reactions, and sorption for groundwater solute transport. Called from
!!    gwflow_lateral once per flow time step. Contains its own sub-timestep
!!    loop (num_ts_transport steps). Uses cell_con%latl and cell_con%sat
!!    populated during the Darcy flow calculation in the same time step.

      use gwflow_module
      use time_module

      implicit none

      integer :: i = 0                    !       |cell counter
      integer :: j = 0                    !       |general counter
      integer :: k = 0                    !       |connected-cell counter
      integer :: s = 0                    !       |solute counter
      integer :: t = 0                    !       |transport time step counter
      integer :: cell_id = 0              !       |id of connected cell
      real :: gw_trans_time_step = 0.     !days   |length of transport sub-timestep
      real :: time_fraction = 0.          !       |fraction of flow step for interpolation
      real :: gw_volume_old = 0.          !m3     |gw volume at start of flow step
      real :: gw_volume_new = 0.          !m3     |gw volume at end of flow step
      real :: gw_volume_inter = 0.        !m3     |interpolated gw volume at transport step
      real :: mass_adv(100) = 0.          !g/day  |advective mass flux per solute
      real :: mass_dsp(100) = 0.          !g/day  |dispersive mass flux per solute
      real :: m_change(100) = 0.          !g      |mass change per solute
      real :: del_no_sorp = 0.            !g      |mass change without sorption
      real :: mass_sorb(100) = 0.         !g      |mass removed by sorption per solute
      real :: mass_rct_local(100) = 0.    !g/day  |mass from reactions (unused local; module mass_rct used)
      real :: Q_cell = 0.                 !m3/day |lateral flow between cells
      real :: face_sat = 0.               !m      |saturated thickness at cell interface
      real :: area1 = 0.                  !m2     |area of connected cell
      real :: area2 = 0.                  !m2     |area of current cell
      real :: area = 0.                   !m2     |smaller of the two areas
      real :: conn_length = 0.            !m      |connection length

      !compute transport sub-timestep size
      gw_trans_time_step = gw_time_step / num_ts_transport

      !transport sub-timestep loop
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
                mass_dsp(s) = mass_dsp(s) + (gw_long_disp * &
                    ((gwsol_state(cell_id)%solute(s)%conc - gwsol_state(i)%solute(s)%conc)/conn_length) * face_sat) !g
              enddo
            enddo !go to next connected cell

            !chemical reactions --> fill in mass_rct and mass_min
            cell_id = i
            mass_rct = 0.
            mass_min = 0.
            call gwflow_chem(cell_id,gw_volume_inter)

            !calculate change in mass (g)
            do s=1,gw_nsolute !loop through the solutes
              m_change(s) = (mass_adv(s) + mass_dsp(s) + mass_rct(s) + mass_min(s) + &
                  gwsol_ss(i)%solute(s)%totl) * (gw_trans_time_step/gwsol_sorb(s))
            enddo

            !calculate mass removed due to sorption (g)
            do s=1,gw_nsolute !loop through the solutes
              del_no_sorp = (mass_adv(s) + mass_dsp(s) + mass_rct(s) + mass_min(s) + &
                  gwsol_ss(i)%solute(s)%totl) * gw_trans_time_step
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
              gwsol_ss(i)%solute(s)%advn = gwsol_ss(i)%solute(s)%advn + &
                  (mass_adv(s)*(gw_trans_time_step/gwsol_sorb(s)))
              gwsol_ss(i)%solute(s)%disp = gwsol_ss(i)%solute(s)%disp + &
                  (mass_dsp(s)*(gw_trans_time_step/gwsol_sorb(s)))
              if(mass_rct(s) > 0) then
                gwsol_ss(i)%solute(s)%rcti = gwsol_ss(i)%solute(s)%rcti + &
                    (mass_rct(s)*(gw_trans_time_step/gwsol_sorb(s))) !produced
              else
                gwsol_ss(i)%solute(s)%rcto = gwsol_ss(i)%solute(s)%rcto + &
                    (mass_rct(s)*(gw_trans_time_step/gwsol_sorb(s))) !consumed
              endif
              gwsol_ss(i)%solute(s)%minl = gwsol_ss(i)%solute(s)%minl + &
                  (mass_min(s)*(gw_trans_time_step/gwsol_sorb(s)))
              gwsol_ss(i)%solute(s)%sorb = gwsol_ss(i)%solute(s)%sorb + mass_sorb(s)
            enddo

            !track for annual and monthly write-out
            do s=1,gw_nsolute !loop through the solutes
              if(mass_rct(s) > 0) then
                gwsol_ss_sum(i)%solute(s)%rcti = gwsol_ss_sum(i)%solute(s)%rcti + &
                    (mass_rct(s)*(gw_trans_time_step/gwsol_sorb(s))) !produced
                gwsol_ss_sum_mo(i)%solute(s)%rcti = gwsol_ss_sum_mo(i)%solute(s)%rcti + &
                    (mass_rct(s)*(gw_trans_time_step/gwsol_sorb(s))) !produced
              else
                gwsol_ss_sum(i)%solute(s)%rcto = gwsol_ss_sum(i)%solute(s)%rcto + &
                    (mass_rct(s)*(gw_trans_time_step/gwsol_sorb(s))) !consumed
                gwsol_ss_sum_mo(i)%solute(s)%rcto = gwsol_ss_sum_mo(i)%solute(s)%rcto + &
                    (mass_rct(s)*(gw_trans_time_step/gwsol_sorb(s))) !consumed
              endif
              gwsol_ss_sum(i)%solute(s)%minl = gwsol_ss_sum(i)%solute(s)%minl + &
                  (mass_min(s)*(gw_trans_time_step/gwsol_sorb(s)))
              gwsol_ss_sum_mo(i)%solute(s)%minl = gwsol_ss_sum_mo(i)%solute(s)%minl + &
                  (mass_min(s)*(gw_trans_time_step/gwsol_sorb(s)))
              gwsol_ss_sum(i)%solute(s)%sorb = gwsol_ss_sum(i)%solute(s)%sorb + mass_sorb(s)
              gwsol_ss_sum_mo(i)%solute(s)%sorb = gwsol_ss_sum_mo(i)%solute(s)%sorb + mass_sorb(s)
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

      return
      end subroutine gwflow_solute
