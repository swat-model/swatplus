      subroutine rcurv_interp_dep (icha, flow_dep)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine interpolates between points on a rating curve given flow rate

      use sd_channel_module
      
      implicit none
      
      integer, intent (in) :: icha         !none       !channel number
      real, intent (in) :: flow_dep     !m          !flow depth to interpolate rating curve
      integer :: ielev                  !none       |counter for point on the rating curve
      real :: rto                       !ratio      |storage time constant for the reach on
      
        !! interpolate rating curve using inflow rates
        do ielev = 1, ch_rcurv(icha)%npts
          if (flow_dep < ch_rcurv(icha)%elev(ielev)%dep) then
            if (ielev == 1) then
              rto = flow_dep / ch_rcurv(icha)%elev(ielev)%dep
              rcurv = ch_rcurv(icha)%elev(ielev) * rto
              rcurv%ttime = ch_rcurv(icha)%elev(ielev)%ttime
              exit
            end if
            if (ielev > 1) then
              rto = (flow_dep - ch_rcurv(icha)%elev(ielev-1)%dep) /     &
                (ch_rcurv(icha)%elev(ielev)%dep - ch_rcurv(icha)%elev(ielev-1)%dep)
              call chrc_interp (ch_rcurv(icha)%elev(ielev-1), ch_rcurv(icha)%elev(ielev), rto, rcurv)
              exit
            end if
          end if
          if (ielev == 4) then
            rto = 1. + (flow_dep - ch_rcurv(icha)%elev(ielev)%dep) / ch_rcurv(icha)%elev(ielev)%dep
            rcurv = ch_rcurv(icha)%elev(ielev) * rto
            !! keep max travel time at max bankfull
            rcurv%ttime = rcurv%ttime / rto
            exit
          end if
          
        end do

      return
      end subroutine rcurv_interp_dep