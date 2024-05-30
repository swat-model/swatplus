      subroutine cal_conditions

      use maximum_data_module
      use calibration_data_module
      use conditional_module
      use hru_lte_module
      use hru_module, only : hru
      use soil_module
      use plant_module
      use time_module
      use climate_module, only : pcp, tmp
      
      implicit none
           
      character(len=16) :: chg_parm                           !                |               
      character(len=16) :: chg_typ                            !variable        |type of change (absval, abschg, pctchg)
      character(len=1) :: cond_met                            !                |       
      character(len=1) :: pl_find                             !                |       
      integer :: lyr                                          !none            |counter
      integer :: iyr                                          !                |
      integer :: ichg_par                                     !none            |counter
      integer :: ispu                                         !none            |counter
      integer :: ielem                                        !none            |counter
      real :: chg_val                                         !                |
      real :: absmin                                          !                |minimum range for variable
      real :: absmax                                          !                |maximum change for variable
      integer :: num_db                                       !                |
      integer :: ic                                           !none            |counter
      integer :: ipg                                          !                |
      integer :: ipl                                          !                |
      integer :: iyear                                        !none            |counter
      real :: val_cur                                         !variable        |current parameter value
                                                              !                |the standard temperature (20 degrees C)
      real :: chg_par                                         !variable        |type of change (absval, abschg, pctchg)
      integer :: iday                                         !none            |counter
      integer :: ig                                           !                |
      integer :: nvar                                         !                |number of plant cal variables (1=lai_pot, 2=harv_idx)
      integer :: cal_lyr1, cal_lyr2, iplant
         
      do ichg_par = 1, db_mx%cal_upd
        do ispu = 1, cal_upd(ichg_par)%num_elem
          ielem = cal_upd(ichg_par)%num(ispu)
          chg_parm = cal_upd(ichg_par)%name
          chg_typ = cal_upd(ichg_par)%chg_typ
          chg_val = cal_upd(ichg_par)%val
          absmin = cal_parms(cal_upd(ichg_par)%num_db)%absmin
          absmax = cal_parms(cal_upd(ichg_par)%num_db)%absmax
          num_db = cal_upd(ichg_par)%num_db
          
          !check to see if conditions are met
          cond_met = "y"
          do ic = 1, cal_upd(ichg_par)%conds
            select case (cal_upd(ichg_par)%cond(ic)%var)
            case ("hsg")
              if (cal_upd(ichg_par)%cond(ic)%targc /= soil(ielem)%hydgrp) then
                cond_met = "n"
              end if
            case ("res_typ")
              if (cal_upd(ichg_par)%cond(ic)%targc /= dtbl_res(ielem)%name) then
                cond_met = "n"
              end if
            case ("texture")
              if (cal_upd(ichg_par)%cond(ic)%targc /= soil(ielem)%texture) then
                cond_met = "n"
              end if
            case ("plant")      !for hru
              do ipl = 1, pcom(ielem)%npl
                pl_find = "n"
                if (cal_upd(ichg_par)%cond(ic)%targc == pcom(ielem)%pl(ipl)) then
                  pl_find = "y"
                end if
                  if (pl_find == "n") cond_met = "n"
                  exit
              end do
            case ("landuse")    !for hru
              if (cal_upd(ichg_par)%cond(ic)%targc /= hru(ielem)%land_use_mgt_c) then 
                cond_met = "n"
                exit
              end if
            case ("region")     !for hru    
              if (cal_upd(ichg_par)%cond(ic)%targc /= hru(ielem)%region) then 
                cond_met = "n"
                exit
              end if
            case ("region_lte")     !for hru    
              if (cal_upd(ichg_par)%cond(ic)%targc /= hru(ielem)%region) then 
                cond_met = "n"
                exit
              end if
            end select
          end do    ! ic - conditions

          if (cond_met == "y") then
            if (cal_parms(num_db)%ob_typ /= "sol" .and. cal_parms(num_db)%ob_typ /= "cli" .and. &
              cal_parms(num_db)%ob_typ /= "plt" .and. cal_parms(num_db)%ob_typ /= "rdt") then
              call cal_parm_select (ielem, lyr, chg_parm, chg_typ, chg_val, absmin, absmax, num_db)
            end if
            select case (cal_parms(num_db)%ob_typ)
            case ("sol")
              !! check layers for soil variables
              cal_lyr1 = cal_upd(ichg_par)%lyr1
              cal_lyr1 = Max (cal_lyr1, 1)
              cal_lyr2 = cal_upd(ichg_par)%lyr2
              if (cal_lyr2 <= 0) cal_lyr2 = soil(ielem)%nly
              cal_lyr2 = Min (cal_lyr2, soil(ielem)%nly)
              do lyr = cal_lyr1, cal_lyr2
                call cal_parm_select (ielem, lyr, chg_parm, chg_typ, chg_val, absmin, absmax, num_db)
              end do

            case ("rdt")    !reservoir decision table
              !! check storage zones and flood season
              
              !! exclusive flood control storage
              if (cal_upd(ichg_par)%lyr1 /= 0) then
                lyr = 1
                select case (chg_parm)
                case ("drawdown_days")
                  call cal_parm_select (ielem, lyr, chg_parm, chg_typ, chg_val, absmin, absmax, num_db)
                case ("withdraw_rate")
                  call cal_parm_select (ielem, lyr, chg_parm, chg_typ, chg_val, absmin, absmax, num_db)
                end select
              end if
              !! seasonal flood control storage - non flood season
              if (cal_upd(ichg_par)%lyr2 /= 0) then
                lyr = 2
                select case (chg_parm)
                case ("drawdown_days")
                  call cal_parm_select (ielem, lyr, chg_parm, chg_typ, chg_val, absmin, absmax, num_db)
                case ("withdraw_rate")
                  call cal_parm_select (ielem, lyr, chg_parm, chg_typ, chg_val, absmin, absmax, num_db)
                end select
              end if
              !! seasonal flood control storage - flood season
              if (cal_upd(ichg_par)%year1 /= 0) then
                lyr = 3
                select case (chg_parm)
                case ("drawdown_days")
                  call cal_parm_select (ielem, lyr, chg_parm, chg_typ, chg_val, absmin, absmax, num_db)
                case ("withdraw_rate")
                  call cal_parm_select (ielem, lyr, chg_parm, chg_typ, chg_val, absmin, absmax, num_db)
                end select
              end if
              !! multiple use storage - non flood season
              if (cal_upd(ichg_par)%year2 /= 0) then
                lyr = 4
                select case (chg_parm)
                case ("drawdown_days")
                  call cal_parm_select (ielem, lyr, chg_parm, chg_typ, chg_val, absmin, absmax, num_db)
                case ("withdraw_rate")
                  call cal_parm_select (ielem, lyr, chg_parm, chg_typ, chg_val, absmin, absmax, num_db)
                end select
              end if
              !! multiple use storage - non flood season
              if (cal_upd(ichg_par)%day1 /= 0) then
                lyr = 5
                select case (chg_parm)
                case ("drawdown_days")
                  call cal_parm_select (ielem, lyr, chg_parm, chg_typ, chg_val, absmin, absmax, num_db)
                case ("withdraw_rate")
                  call cal_parm_select (ielem, lyr, chg_parm, chg_typ, chg_val, absmin, absmax, num_db)
                end select
              end if

            case ("plt")
              nvar = 2
              select case (cal_upd(ichg_par)%name)
                  
              case ("phu_mat")
                do ipl = 1, pcom(ielem)%npl
                  do ic = 1, cal_upd(ichg_par)%conds
                    if (cal_upd(ichg_par)%cond(ic)%targc == pcom(ielem)%pl(ipl)) then
                      pcom(ielem)%plcur(ipl)%phumat = chg_par (pcom(ielem)%plcur(ipl)%phumat, chg_typ, chg_val, &
                            absmin, absmax)
                    end if
                  end do
                end do
                
              case ("epco")
                iplant = 0
                !! check to see if conditioned on the plant and only update that plant
                do ic = 1, cal_upd(ichg_par)%conds
                    do ipl = 1, pcom(ielem)%npl
                      if (cal_upd(ichg_par)%cond(ic)%var == "plant") then
                        pcom(ielem)%plcur(ipl)%lai_pot = chg_par (pcom(ielem)%plcur(ipl)%lai_pot, chg_typ, chg_val, &
                            absmin, absmax)
                        iplant = 1
                      end if
                    end do
                end do
                if (iplant == 0) then
                  !! not conditioned on plant - change all plants
                  do ipl = 1, pcom(ielem)%npl
                    pcom(ielem)%plcur(ipl)%lai_pot = chg_par (pcom(ielem)%plcur(ipl)%lai_pot, chg_typ, chg_val, &
                            absmin, absmax)
                  end do
                end if
                    
              case ("lai_pot")
                do ipl = 1, pcom(ielem)%npl
                  do ic = 1, cal_upd(ichg_par)%conds
                    if (cal_upd(ichg_par)%cond(ic)%targc == pcom(ielem)%pl(ipl)) then
                      pcom(ielem)%plcur(ipl)%lai_pot = chg_par (pcom(ielem)%plcur(ipl)%lai_pot, chg_typ, chg_val, &
                            absmin, absmax)
                    end if
                  end do
                end do
                
              case ("harv_idx")
                do ipl = 1, pcom(ielem)%npl
                  do ic = 1, cal_upd(ichg_par)%conds
                    if (cal_upd(ichg_par)%cond(ic)%targc == pcom(ielem)%pl(ipl)) then
                      pcom(ielem)%plcur(ipl)%harv_idx = chg_par (pcom(ielem)%plcur(ipl)%harv_idx, chg_typ, chg_val, &
                            absmin, absmax)
                    end if
                  end do
                end do
              end select
 
            case ("cli")
            !! check dates for climate variable
              select case (cal_upd(ichg_par)%name)
              case ("precip")
                do ielem = 1, cal_upd(ichg_par)%num_elem
                  ipg = cal_upd(ichg_par)%num(ielem)
                  do iyear = cal_upd(ichg_par)%year1, cal_upd(ichg_par)%year2
                    iyr = iyear - time%yrc + 1
                    do iday = cal_upd(ichg_par)%day1, cal_upd(ichg_par)%day2
                      val_cur = pcp(ipg)%ts(iday,iyr)
                      pcp(ipg)%ts(iday,iyr) = chg_par (val_cur, chg_typ, chg_val, absmin, absmax)
                    end do
                  end do
                end do
                
              case ("temp")
                do ielem = 1, cal_upd(ichg_par)%num_elem
                  ipg = cal_upd(ichg_par)%num(ielem)
                  do iyear = cal_upd(ichg_par)%year1, cal_upd(ichg_par)%year2
                    iyr = iyear - time%yrc + 1
                    do iday = cal_upd(ichg_par)%day1, cal_upd(ichg_par)%day2
                      val_cur = tmp(ig)%ts(iday,iyr)
                      tmp(ig)%ts(iday,iyr) = chg_par (val_cur, chg_typ, chg_val, absmin, absmax)
                    end do
                  end do
                end do

              end select
              
            end select
          end if

        end do        ! ispu
      end do          ! ichg_par
      
      return
      end subroutine cal_conditions