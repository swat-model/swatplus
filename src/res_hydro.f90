      subroutine res_hydro (jres, id, pvol_m3, evol_m3)

      use reservoir_data_module
      use reservoir_module
      use conditional_module
      use climate_module
      use time_module
      use hydrograph_module
      use water_body_module
      use soil_module
      use hru_module
      use water_allocation_module
      
      implicit none
      
      real,  intent (in) :: pvol_m3
      real,  intent (in) :: evol_m3
      integer,  intent (in) :: jres             !none      |hru number
      integer :: iweir = 0         !none      |weir ID 
      integer :: nstep = 0        !none      |counter
      integer :: tstep = 0        !none      |hru number
      integer :: iac = 0          !none      |counter 
      integer :: ic = 0          !none      |counter
      !integer :: weir_flg=0        !none      |counter
      integer,  intent (in) :: id               !none      |hru number
      integer :: ial = 0          !none      |counter
      integer :: irel = 0         !          |
      integer :: iob = 0          !none      |hru or wro number
      real :: vol = 0.            !          |
      real :: vol_above = 0.            !          |
      real :: b_lo = 0.           !          |
      character(len=1) :: action = ""!          |
      real :: res_h = 0.          !m         |water depth
      real :: demand = 0.         !m3        |irrigation demand by hru or wro
      real :: wsa1 = 0.           !m2        |water surface area 
      real :: qout = 0.                !m3        |weir discharge during short time step
      real :: hgt = 0.                 !m         |height of bottom of weir above bottom of impoundment
      real :: hgt_above = 0.      !m         |height of water above the above bottom of weir
      real :: sto_max = 0.             !m3        |maximum storage volume at the bank top


      !! Jose T 2025 |  Doell method
      real :: sto               = 0.                                          !m3         |Current lake storage 
      real :: smax              = 0.                                          !m3         |Maximum lake stirage 
      real :: so                = 0.                                          !m3         |Dead lake storage    
      real :: kr                = 0.                                          !1/d        |Release coefficient  
      real :: alpha             = 0.                                          !none       |Exponent             
      
      !! Jose T 2025 |  Hanazaki method
      integer :: dom            = 0                                           !           |Day of month
      integer :: mon            = 0                                           !           |Month of year
      integer :: end_of_mo      = 0                                           !           |End of month flag
      integer :: n_days         = 0                                           !           |Number of days in current month
      real    :: daily_inflow   = 0.                                          !m3         |Daily inflow from the past day
      real, dimension(:), allocatable :: temp_array                           !           |Temporary to store new values
      real :: er                = 1.00                                        !           |Release rate
      real :: I_mon             = 0.00                                        !m3         |Monthly inflow
      real :: d_mon             = 0.00                                        !m3         |Monthly demand
      real :: beta              = 0.10                                        !none       |Environmental flow req coefficient
      real :: target_rel        = 0.                                          !m3         |Target release
      real :: daily_demand      = 0.                                          !m3         |Daily irrigation demand
      integer :: irrig_track_b  = 0                                           !none       |Tracker to update daily irrigation demand
      
      !! Jose T 2025 |  HYPE model for HP method
      real :: pi                = 3.14159265358979323846                      !Pi :)
      real :: a_amp             = 0.                                          !none       |Amplitude of the sine function
      real :: b_phase           = 0.                                          !none       |Phase of the sine function
      real :: s_min_hype        = 0.                                          !m3         |Storage at turbine intake (no release below this)
      real :: s_lim_hype        = 0.                                          !m3         |Storage at which energy production starts being limited (below 'desired' hydraulic head for turbines)
      real :: F_sin             = 0.                                          !none       |Seasonal demand factor
      real :: F_lin             = 0.                                           !none       |Storage limiting factor
      
      !! store initial values
      vol = wbody%flo
      nstep = 1
      wsa1 = wbody_wb%area_ha * 10000. !m2
      if (time%step>0) then  !Jaehak 2024
        nstep = time%step
      else
        nstep = 1
      end if
      
      do tstep = 1, nstep

        !! Retrospective information -> Inflow and irrigation demand memory of the reservoir   
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

        !calc release from decision table
        do iac = 1, d_tbl%acts
          action = "n"
          if (d_tbl%alts == 0) then
            action = "y"
          else
            do ial = 1, dtbl_res(id)%alts
              if (d_tbl%act_hit(ial) == "y" .and. d_tbl%act_outcomes(iac,ial) == "y") then
                action = "y"
                exit
              end if
            end do
          end if
          
          !condition is met - set the release rate
          if (action == "y") then
            select case (d_tbl%act(iac)%option)
            case ("rate")
              !! release at constant rate
              ht2%flo = ht2%flo + d_tbl%act(iac)%const * 86400.
              
            case ("rate_pct")
              !! release at percentage of principal volume
              ht2%flo = ht2%flo + d_tbl%act(iac)%const * pvol_m3 / 100.
              
            case ("inflo_rate")
              !! JK: added functionality to use const2 to reduce/increase inflow variable - const is max release
              ht2%flo = ht2%flo + max (ht1%flo + dtbl_res(id)%act(iac)%const2 * 86400., dtbl_res(id)%act(iac)%const * 86400.)
              
            case ("inflo_frac")
              !! release at fraction of inflow
              ht2%flo = ht2%flo + ht1%flo * dtbl_res(id)%act(iac)%const
              
            case ("ab_emer")
              !! release all volume above emergency
              if (wbody%flo > evol_m3) ht2%flo = ht2%flo + (wbody%flo - evol_m3)
              
            case ("days")
              !! release based on drawdown days
              select case (dtbl_res(id)%act(iac)%file_pointer)
                case ("null")
                  b_lo = 0.
                case ("pvol")
                  b_lo = pvol_m3 * d_tbl%act(iac)%const2
                case ("evol")
                  b_lo = evol_m3 * d_tbl%act(iac)%const2
              end select
              ht2%flo = ht2%flo + (wbody%flo - b_lo) / d_tbl%act(iac)%const / nstep
              ht2%flo = max(0.,ht2%flo)
              wbody%flo = max(0.,wbody%flo - ht2%flo)    ! & jga-jj 6-3-2025
              vol = wbody%flo
              
            case ("dyrt")
              !! release based on drawdown days + percentage of principal volume
              select case (dtbl_res(id)%act(iac)%file_pointer)
                case ("null")
                  b_lo = 0.
                case ("pvol")
                  b_lo = pvol_m3
                case ("evol")
                  b_lo = evol_m3
              end select
              b_lo = max (0., b_lo)
              ht2%flo = ht2%flo + (wbody%flo - b_lo) / d_tbl%act(iac)%const +           &
                                         d_tbl%act(iac)%const2 * pvol_m3 / 100. / nstep
              ht2%flo = max(0.,ht2%flo)
              
            case ("dyrt1")
              !for base volume for drawdown days, use condition associated with action
              select case (d_tbl%act(iac)%file_pointer)
                case ("con1")
                  ic = 5    !NAM setup
                case ("con2")
                  ic = 4    !NAM setup
                case ("con3")
                  ic = 3    !NAM setup
              end select
              !perform operation on target variable to get target
              select case ((d_tbl%cond(ic)%lim_op))
              case ('=') 
                b_lo = pvol_m3 + (evol_m3 - pvol_m3) * d_tbl%cond(ic)%lim_const
              case ("*")
                b_lo = (evol_m3 - pvol_m3) * d_tbl%cond(ic)%lim_const
              case ("+")
                b_lo = (evol_m3 - pvol_m3) + d_tbl%cond(ic)%lim_const
              case ("-")
                b_lo = (evol_m3 - pvol_m3) - d_tbl%cond(ic)%lim_const
              case ("/")
                b_lo = (evol_m3 - pvol_m3) / d_tbl%cond(ic)%lim_const
              end select
              b_lo = max (0., b_lo)
              ht2%flo = ht2%flo + (wbody%flo - b_lo) / d_tbl%act(iac)%const +           &
                                         d_tbl%act(iac)%const2 * pvol_m3 / 100. / nstep
              ht2%flo = max(0.,ht2%flo)
              
            case ("inflo_targ")
              !! release inflow + all volume over target (pvol_m3), use condition associated with action
              ic = int (d_tbl%act(iac)%const)
              b_lo = pvol_m3 * d_tbl%cond(ic)%lim_const
              
              ht2%flo = ht2%flo + ht1%flo + (wbody%flo - b_lo)
              ht2%flo = max(0.,ht2%flo)
              
            case ("irrig_dmd")
              !! release based on irrigation demand of hru or water rights object
              iob = Int(d_tbl%act(iac)%const2)
              select case (d_tbl%act(iac)%file_pointer)
              case ("wro")    !demand from water rights object
                demand = wallo(iob)%tot%demand
              case ("hru")    !demand from single hru
                demand = irrig(iob)%demand
              end select
              !! const allows a fraction (usually > 1.0) of the demand (m3) released
              ht2%flo = ht2%flo + demand * d_tbl%act(iac)%const / nstep
              ht2%flo = max(0.,ht2%flo)
                 
            case ("weir")
              !! release based on weir equation
              iweir = d_tbl%act_typ(iac)
              res_h = vol / wsa1     !m
              hgt_above = max(0., res_h - wet_ob(jres)%weir_hgt)    !m
              if (nstep>1) then !subdaily time interval Jaehak 2025
                ht2%flo = res_weir(iweir)%c * res_weir(iweir)%w * hgt_above ** res_weir(iweir)%k !m3/s
                ht2%flo = max(0.,86400. / nstep * ht2%flo) !m3
                vol = vol - ht2%flo
              else
                do ic = 1, 24
                  vol_above = hgt_above * wsa1 !m3 water volume above weir height
                  qout = res_weir(iweir)%c * res_weir(iweir)%w * hgt_above ** res_weir(iweir)%k !m3/s
                  qout = 3600. * qout !m3
                  if (qout > vol_above) then
                    ht2%flo = ht2%flo + vol_above !weir discharge volume for the day, m3
                    vol = vol - vol_above
                  else
                    ht2%flo = ht2%flo + qout 
                    vol = vol - qout
                  end if
                  res_h = vol / wsa1 !m
                  hgt_above = max(0.,res_h - wet_ob(jres)%weir_hgt)  !m Jaehak 2022
                  if (vol_above<=0.001.or.hgt_above<=0.0001) exit
                end do
              endif
              res_h = vol / wsa1 !m
              wbody%flo = vol !m3
              !iweir = d_tbl%act_typ(iac)
              !ht2%flo = ht2%flo + res_weir(iweir)%c * res_weir(iweir)%w * hgt_above ** res_weir(iweir)%k / nstep   !m3/s
              !ht2%flo = ht2%flo + max(0.,ht2%flo)
              
            case ("meas")
              !! measured outflow or release
              irel = int(d_tbl%act_typ(iac))
              select case (recall(irel)%typ)
              case (1)    !daily
                ht2%flo = ht2%flo + recall(irel)%hd(time%day,time%yrs)%flo / nstep
              case (2)    !monthly
                ht2%flo = ht2%flo + recall(irel)%hd(time%mo,time%yrs)%flo / nstep
              case (3)    !annual
                ht2%flo = ht2%flo + recall(irel)%hd(1,time%yrs)%flo / nstep
              end select
              ht2%flo = max(0.,ht2%flo)

            case ("doell")
                !! Jose T | release as natural lake based on Doell (2003) formilation
                
                so    = dtbl_res(id)%act(iac)%const * pvol_m3                                   ! Inactive storage (% of Pvol)
                smax  = pvol_m3                                                                 ! Max lake storage coefficient (% of Pvol) - defined on dtl
                sto   = vol - so                                                                ! Current effective storage
                kr    = dtbl_res(id)%act(iac)%const2                                            !0.01 ! 1/d  Release coefficient
                alpha = 1.50                                                                    ! Exponent
              
                ht2%flo = ht2%flo + ((sto/(smax - so)) ** alpha) * sto * kr

            case ("hanazaki_06_gen")
                !! Jose T | Time-variant parametric scheme (Hanazaki et al. 2006) for non-irrigation reservoirs
                !! release as non-irrigation reservoir
                smax                 = max(pvol_m3,evol_m3)
                alpha                = dtbl_res(id)%act(iac)%const
                I_mon                = res_ob(jres)%I_mon_past(12*(res_ob(jres)%N_memory))            ! m3/day
                res_ob(jres)%I_mean  = sum(res_ob(jres)%I_mon_past)/size(res_ob(jres)%I_mon_past)     ! Long term mean inflow from rolling window (m3/day)
                    
                res_ob(jres)%c_ratio = smax/(res_ob(jres)%I_mean*365.25)                              ! Update capacity ratio
                   
                !! Verify if new operational year is starting
                if (dom == 1) then                                                                    ! This update only occurs at the beginning of the month
                    if (I_mon<res_ob(jres)%I_mean) then
                        res_ob(jres)%S_ini = vol                                                      ! Current storage is now S_ini
                    end if
                       
                    !! If storage getting close to pvol, update S_ini to increase release rate (Not in the original method)
                    if (vol > pvol_m3) then
                        res_ob(jres)%S_ini = vol
                    end if
                       
                end if
                    
                !! Update release rate
                er = res_ob(jres)%S_ini/(alpha*smax)
                    
                !! Define release target
                target_rel =  res_ob(jres)%I_mean
                    
                !! Calculate release
                if (res_ob(jres)%c_ratio >= 0.5) then
                    ht2%flo = ht2%flo + er * target_rel
                else
                    ht2%flo = er*target_rel*(2*res_ob(jres)%c_ratio)**2 + (1-(2*res_ob(jres)%c_ratio)**2)*I_mon
                end if
                    
            case ("hanazaki_06_irr")
                !! Jose T | Time-variant parametric scheme (Hanazaki et al. 2006) for irrigation reservoirs
                !! release as irrigation reservoir
                smax                 = max(pvol_m3,evol_m3)
                alpha                = dtbl_res(id)%act(iac)%const
                beta                 = dtbl_res(id)%act(iac)%const2
                I_mon                = res_ob(jres)%I_mon_past(12*(res_ob(jres)%N_memory))                ! m3/day
                d_mon                = res_ob(jres)%d_mon_past(12*(res_ob(jres)%N_memory))                ! m3/day
                res_ob(jres)%I_mean  = sum(res_ob(jres)%I_mon_past)/size(res_ob(jres)%I_mon_past)         ! Long term mean inflow from rolling window (m3/day)
                res_ob(jres)%d_mean  = sum(res_ob(jres)%d_mon_past)/size(res_ob(jres)%d_mon_past)         ! Long term mean demand from rolling window (m3/day)
                res_ob(jres)%c_ratio = smax/(res_ob(jres)%I_mean*365.25)                                  ! Update capacity ratio
                !! Verify if new operational year is starting
                if (dom == 1) then                                                                        ! This update only occurs at the beginning of the month
                    if (I_mon<res_ob(jres)%I_mean) then
                        res_ob(jres)%S_ini = vol                                                          ! Current storage is now S_ini
                    end if
                    !! If storage getting close to pvol, update S_ini to increase release rate (Not in the original method)
                    if (vol > pvol_m3) then
                        res_ob(jres)%S_ini = vol
                    end if
                end if
                
                !! Update release rate
                er = res_ob(jres)%S_ini/(alpha*smax)
                    
                !! Determine release target (Condition depends on wether the irrigation demand surpassess environmental flow requirements)
                if (res_ob(jres)%d_mean >= beta*res_ob(jres)%I_mean) then
                    target_rel = 0.10*res_ob(jres)%I_mean + 0.9*(d_mon/res_ob(jres)%d_mean)
                        
                else
                    target_rel = res_ob(jres)%I_mean + d_mon - res_ob(jres)%d_mean
                        
                end if
                    
                !! Calculate release
                if (res_ob(jres)%c_ratio >= 0.5) then
                    ht2%flo = ht2%flo + er * target_rel
                else
                    ht2%flo = er*target_rel*(2*res_ob(jres)%c_ratio)**2 + (1-(2*res_ob(jres)%c_ratio)**2)*I_mon
                end if
                    
            case ("hype_hp")
                !! HYPE Model Hydroelectric Power Reservoir release scheme (Scheme by Arheimer et al, 2019 and implementation by Gahari et al, 2024)
                a_amp       = dtbl_res(id)%act(iac)%const
                b_phase     = dtbl_res(id)%act(iac)%const2
                s_min_hype  = 0.20 * pvol_m3
                s_lim_hype  = 0.50 * pvol_m3
                res_ob(jres)%I_mean  = sum(res_ob(jres)%I_mon_past)/size(res_ob(jres)%I_mon_past)           ! Long term mean inflow from rolling window (m3/day)
                 
                F_lin  = min(1.,max(0.,(vol-s_min_hype)/(s_lim_hype-s_min_hype)))                           ! Linear factor based on storage limitations for energy
                F_sin  = max(0.,1.0 + a_amp *((2*pi*time%day + b_phase)/(365.)))                            ! Sinusoidal factor based on demand per day of the year
                
                target_rel =  res_ob(jres)%I_mean
                
                ht2%flo = ht2%flo + F_lin*F_sin*target_rel
              
            end select
          
          end if    ! if action hit
          
        end do      ! iac - actions loop
      end do    !tstep loop

      return
      end subroutine res_hydro