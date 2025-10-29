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
              
            case ("irrig_trn")
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
              
            end select
          
          end if    ! if action hit
          
        end do      ! iac - actions loop
      end do    !tstep loop

      return
      end subroutine res_hydro