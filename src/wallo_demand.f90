subroutine wallo_demand (iwallo, itrn)
      
      use water_allocation_module
      use hru_module
      use hydrograph_module
      use conditional_module
      use recall_module
      use exco_module
      
      implicit none 

      integer, intent (in) :: iwallo            !water allocation object number
      integer, intent (in) :: itrn              !water demand object number
      integer :: j = 0              !none       |hru number
      integer :: id = 0             !none       |flo_con decision table number
      integer :: iom = 0            !none       |recall organic/mineral number
      integer :: isrc = 0           !none       |source object number

      !! compute total transfer from each transfer object
      select case (wallo(iwallo)%trn(itrn)%trn_typ)
          
      !! outflow from the source object - only 1 source object for outflow
      case ("outflo")
        isrc = wallo(iwallo)%trn(itrn)%src(1)%num
        !! only one source object for outflow transfer
        select case (wallo(iwallo)%trn(itrn)%src(1)%typ)
            
        !! source object is an out of basin flowing source - measured flow or SWAT+ output
        case ("osrc")
        !! use recall object for transfer
        iom = recall_db(isrc)%iorg_min
        select case (recall_db(iom)%org_min%tstep)
          case ("day")    !daily
            wallod_out(iwallo)%trn(itrn)%trn_flo = recall(iom)%hd(time%day,time%yrs)%flo
          case ("mo")    !monthly
            wallod_out(iwallo)%trn(itrn)%trn_flo = recall(iom)%hd(time%mo,time%yrs)%flo
          case ("yr")    !yearly
            wallod_out(iwallo)%trn(itrn)%trn_flo = recall(iom)%hd(1,time%yrs)%flo
        end select
        
        !! source object is an out of basin flowing source - measured flow or SWAT+ output
        case ("osrc_a")
        !! use recall object for transfer
        iom = wallo(iwallo)%trn(itrn)%osrc(1)%aa
        wallod_out(iwallo)%trn(itrn)%trn_flo = exco(iom)%flo
        
        !! source object is a water treatment plant
        case ("wtp")
          isrc = wallo(iwallo)%trn(itrn)%src(1)%num
          wallod_out(iwallo)%trn(itrn)%trn_flo = wtp_om_out(isrc)%flo
          
        !! source object is a domestic, industrial, or commercial use
        case ("use")
          isrc = wallo(iwallo)%trn(itrn)%src(1)%num
          wallod_out(iwallo)%trn(itrn)%trn_flo = wuse_om_out(isrc)%flo
          
        !! source object is a water storage tank
        !case ("stor") 
          isrc = wallo(iwallo)%trn(itrn)%src(1)%num
          !wallod_out(iwallo)%trn(itrn)%trn_flo =
          
        !! source object is channel
        case ("cha") 
          isrc = wallo(iwallo)%trn(itrn)%src(1)%num
          !! trn3 is channel flow - calling from sd_channel_control3
          wallod_out(iwallo)%trn(itrn)%trn_flo = trn_m3
          
        !! source object is a canal
        case ("can") 
          isrc = wallo(iwallo)%trn(itrn)%src(1)%num
          wallod_out(iwallo)%trn(itrn)%trn_flo = canal_om_out(isrc)%flo
          
      end select
            
      !! average daily transfer
      case ("ave_day")
        !! input ave daily m3/s and convert to m3/day
        wallod_out(iwallo)%trn(itrn)%trn_flo = 86400. * wallo(iwallo)%trn(itrn)%amount
          
      !! divert and leave minimum flow in channel - only used for channel
      case ("div_min")
        !! flow in channel - minimum
        wallod_out(iwallo)%trn(itrn)%trn_flo = Max (0., trn_m3 - (86400. * wallo(iwallo)%trn(itrn)%amount))
          
      !! diver fraction of flow in channel - only used for channel
      case ("div_frac")
        !! flow in channel - minimum
        wallod_out(iwallo)%trn(itrn)%trn_flo = trn_m3 * wallo(iwallo)%trn(itrn)%amount
          
      !! for wallo transfer amount, source available, and source and receiving allocating
      case ("dtbl_con")
        id = wallo(iwallo)%trn(itrn)%dtbl_num
        d_tbl => dtbl_flo(id)
        j = 0
        call conditions (j, id)
        call actions (j, icmd, id)
        wallod_out(iwallo)%trn(itrn)%trn_flo = trn_m3

      !! for hru irrigation
      case ("dtbl_lum")
        j = wallo(iwallo)%trn(itrn)%rcv%num
        id = wallo(iwallo)%trn(itrn)%dtbl_lum
        d_tbl => dtbl_lum(id)
        call conditions (j, id)
        call actions (j, icmd, id)
        
        !! if there is demand, use amount from water allocation file
        if (irrig(j)%demand > 0.) then
          wallod_out(iwallo)%trn(itrn)%trn_flo = irrig(j)%demand
        else
          wallod_out(iwallo)%trn(itrn)%trn_flo = 0.
        end if
      end select

      return
      end subroutine wallo_demand