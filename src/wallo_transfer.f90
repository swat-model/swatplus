      subroutine wallo_transfer (iwallo, idmd)
      
      use water_allocation_module
      use hydrograph_module
      use sd_channel_module
      use aquifer_module
      use reservoir_module
      use time_module
      
      implicit none 

      integer, intent (in):: iwallo         !water allocation object number
      integer, intent (in) :: idmd          !water demand object number
      integer :: j                  !none       |object number of specific type (cha, res, aqu, etc)
      integer :: iob                !none       |object number (ob)

      !! check if water is available from each source - set withdrawal and unmet
      select case (wallo(iwallo)%dmd(idmd)%rcv_ob)
      !! divert flowing water from channel source
      case ("cha")
        j = wallo(iwallo)%dmd(idmd)%rcv_num
        iob = sd_ch(j)%obj_no
        ob(iob)%trans = wallo(iwallo)%dmd(idmd)%hd
            
      !! reservor source
      case ("res") 
        j = wallo(iwallo)%dmd(idmd)%rcv_num
        res(j) = res(j) + wallo(iwallo)%dmd(idmd)%hd
            
      !! aquifer source
      case ("aqu") 
        j = wallo(iwallo)%dmd(idmd)%rcv_num
        aqu_d(j)%stor = aqu_d(j)%stor + (wallo(iwallo)%dmd(idmd)%hd%flo / (10. * aqu_prm(j)%area_ha))  !mm = m3/(10.*ha)
        aqu_d(j)%no3_st = aqu_d(j)%no3_st + wallo(iwallo)%dmd(idmd)%hd%no3
        aqu_d(j)%minp = aqu_d(j)%minp + wallo(iwallo)%dmd(idmd)%hd%solp
            
      end select
 
      return
      end subroutine wallo_transfer