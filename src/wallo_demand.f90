      subroutine wallo_demand (iwallo, idmd)
      
      use water_allocation_module
      use hru_module
      use hydrograph_module
      use conditional_module
      
      implicit none 

      integer, intent (in) :: iwallo            !water allocation object number
      integer, intent (in)  :: idmd             !water demand object number
      integer :: j                  !none       |hru number
      integer :: id                 !none       |flo_con decision table number
      integer :: isrc               !none       |source object number
      integer :: irec               !none       |recall database number

      !! zero total demand for each object
      wallod_out(iwallo)%dmd(idmd)%dmd_tot = 0.
      
      !! compute total demand from each demand object
      select case (wallo(iwallo)%dmd(idmd)%ob_typ)
        !! minicipal demand
        case ("muni")
          if (wallo(iwallo)%dmd(idmd)%withdr == "ave_day") then
            wallod_out(iwallo)%dmd(idmd)%dmd_tot = wallo(iwallo)%dmd(idmd)%amount  
          else
            !! use recall object for demand
            irec = wallo(iwallo)%dmd(idmd)%rec_num
            select case (recall(irec)%typ)
            case (1)    !daily
              wallod_out(iwallo)%dmd(idmd)%dmd_tot = recall(irec)%hd(time%day,time%yrs)%flo
            case (2)    !monthly
              wallod_out(iwallo)%dmd(idmd)%dmd_tot = recall(irec)%hd(time%mo,time%yrs)%flo
            case (3)    !annual
              wallod_out(iwallo)%dmd(idmd)%dmd_tot = recall(irec)%hd(1,time%yrs)%flo
            end select
          end if
        
        !! diversion demand
        case ("divert")
          !! use average daily or a flow control decision table
          if (wallo(iwallo)%dmd(idmd)%withdr == "ave_day") then
            wallod_out(iwallo)%dmd(idmd)%dmd_tot = wallo(iwallo)%dmd(idmd)%amount  
          else
            !! use decision table for flow control - water allocation
            id = wallo(iwallo)%dmd(idmd)%rec_num   !dtbl_flo(i)%act_typ(iac)
            ich = 0     !use number in decision table for divert
            !icmd is source channel object number
            icmd = sp_ob1%chandeg + wallo(iwallo)%cha - 1
            d_tbl => dtbl_flo(id)
            call conditions (ich, id)
            call actions (ich, icmd, id)
            wallod_out(iwallo)%dmd(idmd)%dmd_tot = trans_m3
          end if

        !! irrigation demand
        case ("hru")
          j = wallo(iwallo)%dmd(idmd)%ob_num
          !! if there is demand, use amount from water allocation file
          if (irrig(j)%demand > 0.) then
            wallod_out(iwallo)%dmd(idmd)%dmd_tot = wallo(iwallo)%dmd(idmd)%amount * hru(j)%area_ha * 10. !m3 = mm * ha * 10.
          else
            wallod_out(iwallo)%dmd(idmd)%dmd_tot = 0.
          end if
      end select

      !! initialize unmet to total demand and subtract as water is withdrawn
      wallo(iwallo)%dmd(idmd)%unmet_m3 = wallod_out(iwallo)%dmd(idmd)%dmd_tot
      
      
      !! compute demand from each source object
      do isrc = 1, wallo(iwallo)%dmd(idmd)%dmd_src_obs
        wallod_out(iwallo)%dmd(idmd)%src(isrc)%demand = wallo(iwallo)%dmd(idmd)%src(isrc)%frac *      &
                                                                wallod_out(iwallo)%dmd(idmd)%dmd_tot
      end do
       
      return
      end subroutine wallo_demand