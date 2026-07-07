      subroutine ru_control
      
!!    ~ ~ ~ PURPOSE ~ ~ ~

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!

      use hru_module, only : hhsurfq, ihru, qday, latq
      use ru_module
      use hydrograph_module
      use time_module
      use constituent_mass_module
      use output_landscape_module
      
      implicit none 
      
      character (len=3) :: ihtyp         !            |
      integer :: iday                    !            |
      integer :: ihdmx                   !            |
      real :: sumfrac                    !            |
      real :: sumarea                    !            |
      integer :: ielem                   !none        |counter
      integer :: ise                     !none        |counter
      integer :: iob                     !            |
      integer :: iday_cur                !            |
      integer :: isd
      integer :: ihtypno                 !            |
      real :: ef                         !            | 
      real :: cnv_m3                     !            |
      real :: cnv                        !            |hru conversion from mm to m3 with del ratio applied
      integer :: ii                      !none        |counter
      integer :: iday_prev               !            | 
      real :: ts_flo_mm                  !mm          |total flow during the time step
      integer :: kk                      !            |
      real :: sumflo                     !            |
      integer :: itot                    !            |
      integer :: ib                      !none        |counter
      real :: rto                        !none        |cloud cover factor
      integer :: iadj                    !            | 
      integer :: istep                   !            |
      integer :: ipest                   !            |
      real :: sumflo_day                 !            |
      real :: ratio                      !frac        |fraction change in precipitation due to 
                                         !            |elevation changes
      real :: wtmp                       !deg C       |temperature of water in reach
      integer :: istep_bak           !none          |counter
      integer :: day_cur             !none          |counter
      integer :: day_next            !none          |counter
      integer :: tinc                !none          |
      integer :: inext_step
           
      iday = 1
      ihdmx = 2
      cnv_m3 = 1000. * ru(iru)%da_km2

      ru_d(iru) = hz
      ob(icmd)%hd(1) = hz
      ob(icmd)%hd(2) = hz
      ob(icmd)%hd(3) = hz
      ob(icmd)%hd(4) = hz
      ob(icmd)%hd(5) = hz
      if (cs_db%num_tot > 0) then
        obcs(icmd)%hd(1) = hin_csz
        obcs(icmd)%hd(2) = hin_csz
        obcs(icmd)%hd(3) = hin_csz
        obcs(icmd)%hd(4) = hin_csz
        obcs(icmd)%hd(5) = hin_csz
      end if
      
      sumfrac = 0.
      sumarea = 0.
      
      do ielem = 1, ru_def(iru)%num_tot
        ise = ru_def(iru)%num(ielem)
        iob = ru_elem(ise)%obj
        ihru = ru_elem(ise)%obtypno
        ht1 = hz
        ht2 = hz
        ht3 = hz
        ht4 = hz
        ht5 = hz
        delrto = hz
        
        sumfrac = sumfrac + ru_elem(ise)%frac
        sumarea = sumarea + ob(iob)%area_ha
        
        !define delivery ratio - all variables are hyd_output type

        !calculated dr = f(tconc element/ tconc sub)
        delrto = ru_elem(ise)%dr

        if (ru_elem(ise)%obtyp == "exc") then
          !! compute hyds for export coefficients-ht1==surface,ht2==groundwater
          ht1 = exco(ob(iob)%props) ** delrto
          ht2 = hz
          if (ob(iob)%area_ha > .01) then
            !per area units - mm, t/ha, kg/ha (ie hru, apex)
            ef = ru_elem(ise)%frac * ru(iru)%da_km2 / (ob(iob)%area_ha / 100.)
            ht1 = ef * ht1
          end if
          
        else
          !! for routing units, channels, reservoir, and recall objects use fraction
          ef = ru_elem(ise)%frac
          !! for hru's use define expansion factor to surface/soil and recharge
          if (ob(iob)%typ == "hru" .or. ob(iob)%typ == "hru_lte") then
            !! if frac is 1.0, then the hru is not part of ru and use entire hru output
            if (ef < .99999) then
              ef = ef * ru(iru)%da_km2 / (ob(iob)%area_ha / 100.)
            end if
          end if
          
          !compute all hyd"s needed for routing
          do ihtypno = 1, ob(iob)%nhyds
            if (ihtypno /=2) then
              !! apply dr to tot, surf, lat and tile
              ht1 = ob(iob)%hd(ihtypno) ** delrto
              do ipest = 1, cs_db%num_pests
                hcs1%pest(ipest) = obcs(iob)%hd(ihtypno)%pest(ipest)    !* delrto - don't apply dr to pests
              end do
            else
              !! don't apply dr to recharge
              ht1 = ob(iob)%hd(ihtypno)
              !! set constituents
              do ipest = 1, cs_db%num_pests
                hcs1%pest(ipest) = obcs(iob)%hd(ihtypno)%pest(ipest)
              end do
            end if
            ht1 = ef * ht1
            ob(icmd)%hd(ihtypno) = ob(icmd)%hd(ihtypno) + ht1
            ru_d(iru) = ru_d(iru) + ht1
            !! set constituents
            do ipest = 1, cs_db%num_pests
              obcs(icmd)%hd(ihtypno)%pest(ipest) = obcs(icmd)%hd(ihtypno)%pest(ipest) + hcs1%pest(ipest)
            end do
          end do
          
        end if      !ru_elem(ise)%obtyp == "exc"
  
        !! sum subdaily hydrographs using subdaily precip and green and ampt runoff
        if (time%step > 0 .and. bsn_cc%gampt == 1) then
          select case (ru_elem(ise)%obtyp)
          case ("hru")
            do ii = 1, time%step
              !! conversion from mm to m3 and apply delivery ratio
              cnv = ru_elem(ise)%frac * ob(icmd)%area_ha * delrto%flo
              ts_flo_mm = cnv * hhsurfq(ihru,ii)
              iday_cur = ob(icmd)%day_cur
              !! hru hyd_flo in m3
              rto = (ru_elem(ise)%frac * ob(icmd)%area_ha) / ob(iob)%area_ha
              ob(icmd)%hyd_flo(iday_cur,ii) = ob(iob)%hyd_flo(iday_cur,ii) + ts_flo_mm
            end do
            case ("res")
            do ii = 1, time%step
              !ts_flo_mm = hhsurfq(ihru,ii) + (latq(ihru) + hwb_d(ihru)%qtile)  / time%step
              !iday_cur = ob(icmd)%day_cur
              !! res hyd_flo in m3
              !ob(icmd)%hyd_flo(iday_cur,ii) = ob(iob)%hyd_flo(iday_cur,ii) + ts_flo_mm      &
              !                                           * ru_elem(ise)%frac * delrto%flo
            end do
          end select
          
          !! set previous and next days for adding previous and translating to next
          day_cur = ob(icmd)%day_cur
          day_next = day_cur + 1
          if (day_next > ob(icmd)%day_max) day_next = 1
          
          !! translate the hydrogrpah by time of concentration - no attenuation
          ob(icmd)%hyd_flo(day_next,:) = 0.
          ru_tc(iru) = 2.
          if (ru_tc(iru) * 60. > time%dtm) then
            tinc = int (ru_tc(iru) * 60. / time%dtm)
            !! move to next days hydrograph
            do istep = 1, tinc
              inext_step = time%step - tinc + istep
              ob(icmd)%hyd_flo(day_next,istep) = ob(icmd)%hyd_flo(day_cur,inext_step)
            end do
            !! shift current day hydrograph
            do istep = 1, time%step
              istep_bak = time%step - istep + 1
              if (istep_bak <= tinc) then
                ob(icmd)%hyd_flo(day_cur,istep_bak) = 0.
              else
                ob(icmd)%hyd_flo(day_cur,istep_bak) = ob(icmd)%hyd_flo(day_cur,istep_bak-tinc)
              end if
            end do
          end if
        end if

        ! rtb gwflow - hydrograph separation --> add volumes from the current HRU
        ob(icmd)%hdsep%flo_surq = ob(icmd)%hdsep%flo_surq + ob(iob)%hdsep%flo_surq !surface runoff (m3)
        ob(icmd)%hdsep%flo_latq = ob(icmd)%hdsep%flo_latq + ob(iob)%hdsep%flo_latq !lateral flow (m3)
        

      end do  !element loop
      
      !! set subdaily ru hydrographs using daily runoff and ru unit hydrograph
      if (time%step > 0 .and. bsn_cc%gampt == 0) then
        call flow_hyd_ru_hru (ob(icmd)%day_cur, ob(icmd)%hd(3)%flo, ob(icmd)%hd(4)%flo,     &
                                        ob(icmd)%hd(5)%flo, ob(icmd)%uh, ob(icmd)%hyd_flo)
      end if

	return

      end subroutine ru_control