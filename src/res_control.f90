      subroutine res_control (jres)
      
      use basin_module
      use reservoir_data_module 
      use time_module
      use reservoir_module
      use climate_module
      use hydrograph_module
      use conditional_module
      use water_body_module
      use constituent_mass_module  !! added nbs
      
      implicit none

      integer :: ii = 0               !none          |counter
      integer :: jres                 !none          |reservoir number
      integer :: idat = 0             !              |
      integer :: irel = 0             !              |
      integer :: iob = 0              !none          |counter
      integer :: ictbl = 0
      integer :: icon = 0              !! nbs
      real :: pvol_m3 = 0.
      real :: evol_m3 = 0.
      real :: dep = 0.
      real :: weir_hgt = 0.
      real :: alpha_up = 0.
      real :: alpha_down = 0.

      integer :: dom            = 0                                           !           |Day of month
      integer :: mon            = 0                                           !           |Month of year
      integer :: end_of_mo      = 0                                           !           |End of month flag
      integer :: n_days         = 0                                           !           |Number of days in current month
      real    :: daily_inflow   = 0.                                          !m3         |Daily inflow from the past day
      real, dimension(:), allocatable :: temp_array                           !           |Temporary to store new values
      real :: daily_demand      = 0.                                          !m3         |Daily irrigation demand
      integer :: irrig_track_b  = 0                                           !none       |Tracker to update daily irrigation demand

      ht1 = ob(icmd)%hin    !! set incoming flow
      ht2 = resz            !! zero outgoing flow, sediment and nutrients
      hcs2 = hin_csz        !! zero outgoing constituents

      if (time%yrc > res_hyd(jres)%iyres .or. (time%mo >= res_hyd(jres)%mores   &
                                   .and. time%yrc == res_hyd(jres)%iyres)) then
        iob = res_ob(jres)%ob
        iwst = ob(iob)%wst
      
        !! adjust precip and temperature for elevation using lapse rates
        w = wst(iwst)%weat
        if (bsn_cc%lapse == 1) call cli_lapse
        wst(iwst)%weat = w
      
        !! set water body pointer to res
        wbody => res(jres)
        wbody_wb => res_wat_d(jres)
        wbody_prm => res_prm(jres)
      
        !! add incoming flow to reservoir
        res(jres) = res(jres) + ht1

        dom          = time%day_mo
        mon          = time%mo
        end_of_mo    = time%end_mo
        daily_inflow = ht1%flo

        if (irrig_track_b == res_ob(jres)%irrig_track) then
            daily_demand = 0

        else
            irrig_track_b = res_ob(jres)%irrig_track
            daily_demand = res_ob(jres)%d_irrig_day

        end if

        !! Store values in daily inflow array for the month and reset if month has ended
        if (dom == 1) then
            if (allocated(res_ob(jres)%daily_inflow_array)) then                                    ! Deallocate array to start over
                deallocate(res_ob(jres)%daily_inflow_array)
            end if

            if (allocated(res_ob(jres)%daily_demand_array)) then                                    ! Deallocate array to start over
                deallocate(res_ob(jres)%daily_demand_array)
            end if

            allocate(res_ob(jres)%daily_inflow_array(1))
            res_ob(jres)%daily_inflow_array(1) = daily_inflow                                       ! Store first inflow of the month

            allocate(res_ob(jres)%daily_demand_array(1))
            res_ob(jres)%daily_demand_array(1) = daily_demand                                       ! Store first irrigation demand of the month

        else
        !! Append inflow of current day
            n_days = size(res_ob(jres)%daily_inflow_array)

            allocate(temp_array(n_days+1))
            temp_array(1:n_days)   = res_ob(jres)%daily_inflow_array(1:n_days)
            temp_array(n_days +1)  = daily_inflow

            call move_alloc(temp_array, res_ob(jres)%daily_inflow_array)                            !Replace original array with move_alloc

        !! Append irrigaton demand of current day
            allocate(temp_array(n_days+1))
            temp_array(1:n_days)   = res_ob(jres)%daily_demand_array(1:n_days)
            temp_array(n_days +1)  = daily_demand

            call move_alloc(temp_array, res_ob(jres)%daily_demand_array)                            !Replace original array with move_alloc

        end if


        !! Get mean and store in reservoir's memory if end of month
        if (end_of_mo == 1) then
            ! Shift rolling window to the left
            res_ob(jres)%I_mon_past(1:12*(res_ob(jres)%N_memory)-1)     = res_ob(jres)%I_mon_past(2:12*(res_ob(jres)%N_memory))
            res_ob(jres)%I_mon_past(12*(res_ob(jres)%N_memory))         = sum(res_ob(jres)%daily_inflow_array) / real(size(res_ob(jres)%daily_inflow_array), kind=8)

            ! Do the same for irrigation
            res_ob(jres)%d_mon_past(1:12*(res_ob(jres)%N_memory)-1)     = res_ob(jres)%d_mon_past(2:12*(res_ob(jres)%N_memory))
            res_ob(jres)%d_mon_past(12*(res_ob(jres)%N_memory))         = sum(res_ob(jres)%daily_demand_array) / real(size(res_ob(jres)%daily_demand_array), kind=8)

        end if

        !! perform reservoir water/sediment balance
        idat = res_ob(jres)%props
        if(res_ob(jres)%rel_tbl == "d") then
          !! determine reservoir outflow
          irel = res_dat(idat)%release
          d_tbl => dtbl_res(irel)
          pvol_m3 = res_ob(jres)%pvol
          evol_m3 = res_ob(jres)%evol
          if (res_wat_d(jres)%area_ha > 1.e-6) then
            dep = wbody%flo / res_wat_d(jres)%area_ha / 10000.     !m = m3 / ha / 10000m2/ha
          else
            dep = 0.
          end if
          weir_hgt = res_ob(jres)%weir_hgt
          call conditions (jres, irel)

                  !! Retrospective information -> Inflow and irrigation demand memory of the reservoir

          call res_hydro (jres, irel, pvol_m3, evol_m3)
          
          !! new lag to smooth condition jumps (volume or month conditions)
          alpha_up = Exp(-res_ob(jres)%lag_up)
          alpha_down = Exp(-res_ob(jres)%lag_down)
          !! lag outflow when flows are receding
          if (res_ob(jres)%prev_flo < ht2%flo) then
            ht2%flo = ht2%flo * alpha_up + res_ob(jres)%prev_flo * (1. - alpha_up)
          else
            ht2%flo = ht2%flo * alpha_down + res_ob(jres)%prev_flo * (1. - alpha_down)
          end if
          res_ob(jres)%prev_flo = ht2%flo
            
          call res_sediment
        else
          ictbl = res_dat(idat)%release                              !! Osvaldo
          call res_rel_conds (ictbl, res(jres)%flo, ht1%flo, 0.)
          
        endif 
        
        !! calculate water balance for day
        res_wat_d(jres)%evap = 10. * res_hyd(jres)%evrsv * wst(iwst)%weat%pet * res_wat_d(jres)%area_ha
        res_wat_d(jres)%precip = 10. * wst(iwst)%weat%precip * res_wat_d(jres)%area_ha
        if(bsn_cc%gwflow == 0) then !if gwflow active, seepage calculated in gwflow_simulate (rtb gwflow)
          res_wat_d(jres)%seep = 240. * res_hyd(jres)%k * res_wat_d(jres)%area_ha
        endif

        !! add precip to reservoir storage
        res(jres)%flo = res(jres)%flo + res_wat_d(jres)%precip

        !! subtract outflow from reservoir storage
        res(jres)%flo = res(jres)%flo - ht2%flo
        if (res(jres)%flo < 0.) then
          ht2%flo = ht2%flo + res(jres)%flo
          res(jres)%flo = 0.
        end if

        !! subtract evaporation from reservoir storage
        res(jres)%flo = res(jres)%flo - res_wat_d(jres)%evap
        if (res(jres)%flo < 0.) then
          res_wat_d(jres)%evap = res_wat_d(jres)%evap + res(jres)%flo
          res(jres)%flo = 0.
        end if
      
        !! subtract seepage from reservoir storage
        res(jres)%flo = res(jres)%flo - res_wat_d(jres)%seep
        if (res(jres)%flo < 0.) then
          res_wat_d(jres)%seep = res_wat_d(jres)%seep + res(jres)%flo
          res(jres)%flo = 0.
        end if

        !! update surface area
        if (res(jres)%flo > 0.) then
          res_wat_d(jres)%area_ha = res_ob(jres)%br1 * res(jres)%flo ** res_ob(jres)%br2
        else
          res_wat_d(jres)%area_ha = 0.
        end if

        !! subtract sediment leaving from reservoir
        !res(jres)%sed = max (0., res(jres)%sed - ht2%sed)
        !res(jres)%sil = max (0., res(jres)%sil - ht2%sil)
        !res(jres)%cla = max (0., res(jres)%cla - ht2%cla)
          
        !! perform reservoir nutrient balance
        call res_nutrient (iob)

        !! perform reservoir pesticide transformations
        if (cs_db%num_pests > 0) then
          call res_pest (jres)
          obcs(icmd)%hd(1)%pest = hcs2%pest
        end if

        !! perform reservoir salt process (rtb salt)
        if (cs_db%num_salts > 0) then
          call res_salt(jres)
          obcs(icmd)%hd(1)%salt = hcs2%salt
        endif
        
        !! perform reservoir constituent process (rtb cs)
        if(cs_db%num_cs > 0) then
          icon = res_dat(idat)%cs
          call res_cs(jres, icon, iob)
          obcs(icmd)%hd(1)%cs = hcs2%cs
        endif
        
        !! set values for outflow variables
        ob(icmd)%hd(1) = ht2
        if (cs_db%num_tot > 0) then
          obcs(icmd)%hd(1) = hcs2
        end if

        !! total incoming to output to SWIFT
        ob(icmd)%hin_tot = ob(icmd)%hin_tot + ob(icmd)%hin     
        !! total outgoing to output to SWIFT
        ob(icmd)%hout_tot = ob(icmd)%hout_tot + ht2
        
        if (time%step > 1) then
          do ii = 1, time%step
            ob(icmd)%ts(1,ii) = ht2 / real(time%step)
          end do
        end if

        !! set inflow and outflow variables for reservoir_output
        if (time%yrs > pco%nyskip) then
          res_in_d(jres) = ht1 
          res_out_d(jres) = ht2
          !res_in_d(jres)%flo = res_in_d(jres)%flo / 10000.          !m^3 -> ha-m
          !res_out_d(jres)%flo = res_out_d(jres)%flo / 10000.        !m^3 -> ha-m
          !res_wat_d(jres)%evap = res_wat_d(jres)%evap / 10000.      !m^3 -> ha-m
          !res_wat_d(jres)%seep = res_wat_d(jres)%seep / 10000.      !m^3 -> ha-m
          !res_wat_d(jres)%precip = res_wat_d(jres)%precip / 10000.  !m^3 -> ha-m
        end if             
        
      else
        !! reservoir has not been constructed yet
        ob(icmd)%hd(1) = ob(icmd)%hin
        if (cs_db%num_tot > 0) obcs(icmd)%hd(1) = obcs(icmd)%hin(1)
      end if



      return
      end subroutine res_control
