      subroutine cal_conditions

      use maximum_data_module
      use calibration_data_module
      use hru_lte_module
      use hru_module, only : hru, cn2
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
      integer :: cal_lyr1, cal_lyr2, ireg, ilum
         
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
              cal_parms(num_db)%ob_typ /= "plt") then
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

            case ("plt")
              nvar = 2
              select case (cal_upd(ichg_par)%name)
              case ("phu_mat")
                do ipl = 1, pcom(ielem)%npl
                  do ic = 1, cal_upd(ichg_par)%conds
                    if (cal_upd(ichg_par)%cond(ic)%targc == pcom(ielem)%pl(ipl)) then
                      ireg = hru(ielem)%crop_reg
                      do ilum = 1, plcal(ireg)%lum_num
                        if (pl_prms(1)%prm(ilum)%name == pcom(ielem)%pl(ipl)) then
                          absmin = pl_prms(1)%prm(ilum)%lo
                          absmax = pl_prms(1)%prm(ilum)%up
                          pcom(ielem)%plcur(ipl)%phumat = chg_par (pcom(ielem)%plcur(ipl)%phumat, ielem, chg_typ, chg_val, &
                            absmin, absmax, num_db)
                        end if
                      end do
                    end if
                  end do
                end do
              case ("lai_pot")
                do ipl = 1, pcom(ielem)%npl
                  do ic = 1, cal_upd(ichg_par)%conds
                    if (cal_upd(ichg_par)%cond(ic)%targc == pcom(ielem)%pl(ipl)) then
                      ireg = hru(ielem)%crop_reg
                      do ilum = 1, plcal(ireg)%lum_num
                        if (pl_prms(1)%prm(ilum)%name == pcom(ielem)%pl(ipl)) then
                          absmin = pl_prms(1)%prm(ilum)%lo
                          absmax = pl_prms(1)%prm(ilum)%up
                          pcom(ielem)%plcur(ipl)%lai_pot = chg_par (pcom(ielem)%plcur(ipl)%lai_pot, ielem, chg_typ, chg_val, &
                            absmin, absmax, num_db)
                        end if
                      end do
                    end if
                  end do
                end do
              case ("harv_idx")
                do ipl = 1, pcom(ielem)%npl
                  do ic = 1, cal_upd(ichg_par)%conds
                    if (cal_upd(ichg_par)%cond(ic)%targc == pcom(ielem)%pl(ipl)) then
                      ireg = hru(ielem)%crop_reg
                      do ilum = 1, plcal(ireg)%lum_num
                        if (pl_prms(1)%prm(ilum)%name == pcom(ielem)%pl(ipl)) then
                          absmin = pl_prms(1)%prm(ilum+nvar)%lo
                          absmax = pl_prms(1)%prm(ilum+nvar)%up
                          pcom(ielem)%plcur(ipl)%harv_idx = chg_par (pcom(ielem)%plcur(ipl)%harv_idx, ielem, chg_typ, chg_val, &
                            absmin, absmax, num_db)
                        end if
                      end do
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
                      pcp(ipg)%ts(iday,iyr) = chg_par (val_cur, ielem, chg_typ, chg_val, absmin, absmax, num_db)
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
                      tmp(ig)%ts(iday,iyr) = chg_par (val_cur, ielem, chg_typ, chg_val, absmin, absmax, num_db)
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