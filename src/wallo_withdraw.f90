      subroutine wallo_withdraw (iwallo, itrn, isrc)
      
      use water_allocation_module
      use hydrograph_module
      use aquifer_module
      use reservoir_module
      use time_module
      use basin_module, only : bsn_cc
      
      implicit none 

      integer, intent (in):: iwallo         !water allocation object number
      integer, intent (in) :: itrn          !water demand object number
      integer, intent (in) :: isrc          !source object number
      integer :: isrc_wallo = 0             !source object number
      integer :: j = 0              !none       |hru number
      integer :: dum = 0
      integer :: irec = 0           !           |recall id
      real :: res_min = 0.          !m3         |min reservoir volume for withdrawal
      real :: res_vol = 0.          !m3         |reservoir volume after withdrawal
      real :: cha_min = 0.          !m3         |minimum allowable flow in channel after withdrawal
      real :: cha_div = 0.          !m3         |maximum amount of flow that can be diverted
      real :: rto = 0.              !none       |ratio of channel withdrawal to determine hydrograph removed
      real :: avail = 0.            !m3         |water available to withdraw from an aquifer
      real :: extracted = 0.        !m3         |water extracted from the aquifer object (gwflow - rtb)
      real :: trn_unmet = 0.        !m3         |demand that is unmet (gwflow - rtb)
      real :: hru_demand = 0.   !m3         |demand (copy to pass into gwflow subroutine - rtb)
      real :: withdraw = 0.         !m3
      real :: unmet = 0.            !m3
      real :: total_trn = 0.        !m3
        
      !! zero withdrawal hyd for the demand source
      wdraw_om = hz

      !! check if water is available from each source - set withdrawal and unmet
      select case (wallo(iwallo)%trn(itrn)%src(isrc)%typ)
          
      !! outside the basin source
      case ("osrc")
        j = wallo(iwallo)%trn(itrn)%src(isrc)%num
        wallod_out(iwallo)%trn(itrn)%src(isrc)%withdr = osrc_om_out(j)%flo
        wallod_out(iwallo)%trn(itrn)%src(isrc)%unmet = 0.
        wal_omd(iwallo)%trn(itrn)%src(isrc)%hd = osrc_om_out(j)
        
      !! water treatment plant source
      case ("wtp")
        j = wallo(iwallo)%trn(itrn)%src(isrc)%num
        wallod_out(iwallo)%trn(itrn)%src(isrc)%withdr = wtp_om_out(j)%flo
        wallod_out(iwallo)%trn(itrn)%src(isrc)%unmet = 0.
        wal_omd(iwallo)%trn(itrn)%src(isrc)%hd = wtp_om_out(j)
        
      !! water use source (effluent)
      case ("use")
        j = wallo(iwallo)%trn(itrn)%src(isrc)%num
        wallod_out(iwallo)%trn(itrn)%src(isrc)%withdr = wuse_om_out(j)%flo
        wallod_out(iwallo)%trn(itrn)%src(isrc)%unmet = 0.
        wal_omd(iwallo)%trn(itrn)%src(isrc)%hd = wuse_om_out(j)
        
      !! water tower storage
      case ("stor")
        j = wallo(iwallo)%trn(itrn)%src(isrc)%num
        
        !! check if there is enough storage and compute outflow
        if (wtow_om_stor(j)%flo > trn_m3 .and. wtow_om_stor(j)%flo > 0.01) then
          wtow_om_out(j)%flo = trn_m3
          rto = Min(1., wtow_om_out(j)%flo / wtow_om_stor(j)%flo)
          wtow_om_out(j) = rto * wtow_om_stor(j)
          wal_omd(iwallo)%trn(itrn)%src(isrc)%hd = rto * wtow_om_stor(j)
          wtow_om_stor(j) = (1. - rto) * wtow_om_stor(j)
          wallod_out(iwallo)%trn(itrn)%src(isrc)%unmet = 0.
        else
          wtow_om_out(j) = wtow_om_stor(j)
          wal_omd(iwallo)%trn(itrn)%src(isrc)%hd = wtow_om_out(j)
          wtow_om_stor(j) = hz
          wallod_out(iwallo)%trn(itrn)%src(isrc)%unmet = trn_m3 - wtow_om_out(j)%flo
        end if
        wallod_out(iwallo)%trn(itrn)%src(isrc)%withdr = wtow_om_out(j)%flo
        
      !! divert flowing water from channel source
      case ("cha")
        j = wallo(iwallo)%trn(itrn)%src(isrc)%num
        isrc_wallo = wallo(iwallo)%trn(itrn)%src_wal(isrc)
        cha_min = wallo(iwallo)%src(isrc_wallo)%limit_mon(time%mo) * 86400.  !m3 = m3/s * 86400s/d
        !! amount that can be diverted without falling below low flow limit
        cha_div = ht2%flo - cha_min
        if (trn_m3 < cha_div) then
          rto = trn_m3 / ht2%flo
          wal_omd(iwallo)%trn(itrn)%src(isrc)%hd = rto * ht2
          ht2 = (1. - rto) * ht2
          wallod_out(iwallo)%trn(itrn)%src(isrc)%withdr = wallod_out(iwallo)%trn(itrn)%src(isrc)%withdr + trn_m3
        else
          wallod_out(iwallo)%trn(itrn)%src(isrc)%unmet = wallod_out(iwallo)%trn(itrn)%src(isrc)%unmet + trn_m3
          wallod_out(iwallo)%trn(itrn)%src(isrc)%unmet = Min (wallod_out(iwallo)%trn(itrn)%src(isrc)%unmet,      &
                                                                wallod_out(iwallo)%trn(itrn)%src(isrc)%demand)
        end if
            
        !! reservoir source
        case ("res") 
          j = wallo(iwallo)%trn(itrn)%src(isrc)%num
          isrc_wallo = wallo(iwallo)%trn(itrn)%src_wal(isrc)
          res_min = wallo(iwallo)%src(isrc_wallo)%limit_mon(time%mo) * res_ob(j)%pvol
          res_vol = res(j)%flo - trn_m3
          if (res_vol > res_min) then
            rto = trn_m3 / res(j)%flo
            wal_omd(iwallo)%trn(itrn)%src(isrc)%hd = rto * res(j)
            res(j) = (1. - rto) * res(j)
            wallod_out(iwallo)%trn(itrn)%src(isrc)%withdr = wallod_out(iwallo)%trn(itrn)%src(isrc)%withdr + trn_m3
            
          else
            wallod_out(iwallo)%trn(itrn)%src(isrc)%unmet = wallod_out(iwallo)%trn(itrn)%src(isrc)%unmet + trn_m3
            wallod_out(iwallo)%trn(itrn)%src(isrc)%unmet = Min (wallod_out(iwallo)%trn(itrn)%src(isrc)%unmet,      &
                                                                wallod_out(iwallo)%trn(itrn)%src(isrc)%demand)
          end if
         
        !! aquifer source
        case ("aqu") 
          if(bsn_cc%gwflow == 0) then !proceed with original code
          j = wallo(iwallo)%trn(itrn)%src(isrc)%num
          isrc_wallo = wallo(iwallo)%trn(itrn)%src_wal(isrc)
          avail = (wallo(iwallo)%src(isrc_wallo)%limit_mon(time%mo) - aqu_d(j)%dep_wt)  * aqu_dat(j)%spyld
          avail = avail * 10000. * aqu_prm(j)%area_ha     !m3 = 10,000*ha*m
          if (trn_m3 < avail) then
            !! only have flow, no3, and minp(solp) for aquifer
            wal_omd(iwallo)%trn(itrn)%src(isrc)%hd%flo = trn_m3
            aqu_d(j)%stor = aqu_d(j)%stor - (trn_m3 / (10. * aqu_prm(j)%area_ha))  !mm = m3/(10.*ha)
            rto =  (trn_m3 / (10. * aqu_prm(j)%area_ha)) / aqu_d(j)%stor  !mm
            wal_omd(iwallo)%trn(itrn)%src(isrc)%hd%no3 = rto * aqu_d(j)%no3_st
            aqu_d(j)%no3_st = (1. - rto) * aqu_d(j)%no3_st
            wal_omd(iwallo)%trn(itrn)%src(isrc)%hd%solp = rto * aqu_d(j)%minp
            aqu_d(j)%minp = (1. - rto) * aqu_d(j)%minp
            wallod_out(iwallo)%trn(itrn)%src(isrc)%withdr = wallod_out(iwallo)%trn(itrn)%src(isrc)%withdr + trn_m3
          else
            wallod_out(iwallo)%trn(itrn)%src(isrc)%unmet = wallod_out(iwallo)%trn(itrn)%src(isrc)%unmet + trn_m3
            wallod_out(iwallo)%trn(itrn)%src(isrc)%unmet = Min (wallod_out(iwallo)%trn(itrn)%src(isrc)%unmet,      &
                                                                wallod_out(iwallo)%trn(itrn)%src(isrc)%demand)
          end if
          elseif(bsn_cc%gwflow == 1) then !gwflow is active; determine pumping amounts from grid cells
            extracted = 0.
            trn_unmet = 0.
            hru_demand = trn_m3
            call gwflow_ppag(wallo(iwallo)%trn(itrn)%num,trn_m3,extracted,trn_unmet)
            wallod_out(iwallo)%trn(itrn)%src(isrc)%withdr = wallod_out(iwallo)%trn(itrn)%src(isrc)%withdr + extracted
            wallod_out(iwallo)%trn(itrn)%src(isrc)%unmet = wallod_out(iwallo)%trn(itrn)%src(isrc)%unmet + trn_unmet
            wallod_out(iwallo)%trn(itrn)%src(isrc)%unmet = Min (wallod_out(iwallo)%trn(itrn)%src(isrc)%unmet,      &
                                                                wallod_out(iwallo)%trn(itrn)%src(isrc)%demand)
          endif
        
            !store values
            wallod_out(iwallo)%trn(itrn)%src(isrc)%withdr = wallod_out(iwallo)%trn(itrn)%src(isrc)%withdr + withdraw
            wallod_out(iwallo)%trn(itrn)%src(isrc)%unmet = wallod_out(iwallo)%trn(itrn)%src(isrc)%unmet + unmet
            wallod_out(iwallo)%trn(itrn)%src(isrc)%unmet = Min (wallod_out(iwallo)%trn(itrn)%src(isrc)%unmet,      &
                                                                wallod_out(iwallo)%trn(itrn)%src(isrc)%demand)
            
          !! unlimited source
          case ("unl")
            wal_omd(iwallo)%trn(itrn)%src(isrc)%hd%flo = trn_m3
            wallod_out(iwallo)%trn(itrn)%src(isrc)%withdr = wallod_out(iwallo)%trn(itrn)%src(isrc)%withdr + trn_m3
          end select
          
          !! add source withdrawal hyd to get total withdrawal hyd for the demand object
          wal_omd(iwallo)%trn(itrn)%h_tot = wal_omd(iwallo)%trn(itrn)%h_tot + wal_omd(iwallo)%trn(itrn)%src(isrc)%hd
          
          !! subtract withdrawal from unmet
          wallo(iwallo)%trn(itrn)%unmet_m3 = wallo(iwallo)%trn(itrn)%unmet_m3 - wallod_out(iwallo)%trn(itrn)%src(isrc)%withdr
          
      return
    end subroutine wallo_withdraw