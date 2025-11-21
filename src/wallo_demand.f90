      subroutine wallo_demand (iwallo, itrn)
      
      use water_allocation_module
      use hru_module
      use hydrograph_module
      use conditional_module
      
      implicit none 

      integer, intent (in) :: iwallo            !water allocation object number
      integer, intent (in) :: itrn              !water demand object number
      integer :: j = 0              !none       |hru number
      integer :: id = 0             !none       |flo_con decision table number
      integer :: irec = 0           !none       |recall database number
      integer :: isrc = 0           !none       |transfer object number

      !! compute total transfer from each transfer object
      select case (wallo(iwallo)%trn(itrn)%trn_typ)
          
      !! outflow from the source object - only 1 source object for outflow
      case ("outflo")
        isrc = wallo(iwallo)%trn(itrn)%src(1)%num
        !! only one source object for outflow transfer
        select case (wallo(iwallo)%trn(itrn)%src(1)%typ)
        !! source object is an out of basin source
        case ("osrc")
          wallod_out(iwallo)%trn(itrn)%trn_flo = osrc_om_out(isrc)%flo
        !! source object is a water treatment plant
        case ("wtp")
          wallod_out(iwallo)%trn(itrn)%trn_flo = wtp_om_out(isrc)%flo
        !! source object is a domestic, industrial, or commercial use
        case ("use")
          wallod_out(iwallo)%trn(itrn)%trn_flo = wuse_om_out(isrc)%flo
        !! source object is a water storage tank
        !case ("stor") 
          !wallod_out(iwallo)%trn(itrn)%trn_tot =
        !! source object is a water storage tank
        !case ("canal") 
          !wallod_out(iwallo)%trn(itrn)%trn_tot =
      end select
            
      !! average daily transfer
      case ("ave_day")
        !! input ave daily m3/s and convert to m3/day
        wallod_out(iwallo)%trn(itrn)%trn_flo = 86400. * wallo(iwallo)%trn(itrn)%amount
          
      !! use recall object for transfer
      case ("rec")
        !! use recall object for transfer
        irec = wallo(iwallo)%trn(itrn)%rec_num
        select case (recall(irec)%typ)
          case (1)    !daily
            wallod_out(iwallo)%trn(itrn)%trn_flo = recall(irec)%hd(time%day,time%yrs)%flo
          case (2)    !monthly
            wallod_out(iwallo)%trn(itrn)%trn_flo = recall(irec)%hd(time%mo,time%yrs)%flo
          case (3)    !annual
            wallod_out(iwallo)%trn(itrn)%trn_flo = recall(irec)%hd(1,time%yrs)%flo
          end select
        
      !! for wallo transfer amount, source available, and source and receiving allocating
      case ("dtbl_con")
        id = wallo(iwallo)%trn(itrn)%rec_num
        d_tbl => dtbl_flo(id)
        j = 0
        icmd = 0   !check to make sure we don't need icmd -- res_ob(j)%ob
        call conditions (j, id)
        call actions (j, icmd, id)
        wallod_out(iwallo)%trn(itrn)%trn_flo = trn_m3

      !! for hru irrigation
      case ("dtbl_lum")
        j = wallo(iwallo)%trn(itrn)%num
        !! if there is demand, use amount from water allocation file
        if (irrig(j)%demand > 0.) then
          if (hru(j)%irr_hmax > 0.) then
            !! Irrigation demand (m3 = mm * ha * 10.) based on paddy/wetland target ponding depth Jaehak 2023
            wallod_out(iwallo)%trn(itrn)%trn_flo = irrig(j)%demand
          else
            wallod_out(iwallo)%trn(itrn)%trn_flo = wallo(iwallo)%trn(itrn)%amount * hru(j)%area_ha * 10.
          endif
        else
          wallod_out(iwallo)%trn(itrn)%trn_flo = 0.
        end if
      end select

      return
      end subroutine wallo_demand