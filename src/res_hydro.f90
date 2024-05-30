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
      integer :: iweir             !none      |weir ID 
      integer :: nstep            !none      |counter
      integer :: tstep            !none      |hru number
      integer :: iac              !none      |counter 
      integer :: ic              !none      |counter
      integer,  intent (in) :: id               !none      |hru number
      integer :: ial              !none      |counter
      integer :: irel             !          |
      integer :: iob              !none      |hru or wro number
      real :: vol                 !          |
      real :: b_lo                !          |
      character(len=1) :: action  !          |
      real :: res_h               !m         |water depth
      real :: demand              !m3        |irrigation demand by hru or wro
      real :: wsa1                !m2        |water surface area 
      real :: hgt_above           !m         |height of water above the above bottom of weir
      real :: alpha_e
      
      !! store initial values
      vol = wbody%flo
      nstep = 1
      wsa1 = wbody_wb%area_ha * 10000. !m2
      
      do tstep = 1, nstep

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
              res_h = vol / (wbody_wb%area_ha * 10000.)     !m
              hgt_above = max(0., res_h - wet_ob(jres)%weir_hgt)    !m
              iweir = d_tbl%act_typ(iac)
              ht2%flo = ht2%flo + res_weir(iweir)%c * res_weir(iweir)%w * hgt_above ** res_weir(iweir)%k / nstep   !m3/s
              ht2%flo = ht2%flo + max(0.,ht2%flo)
              
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
              
            end select
          
          end if    ! if action hit
          
        end do      ! iac - actions loop
      end do    !tstep loop

      return
      end subroutine res_hydro