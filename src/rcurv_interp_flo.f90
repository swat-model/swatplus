      subroutine rcurv_interp_flo (icha, flo_rate)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine interpolates between points on a rating curve given flow rate

      use sd_channel_module
      
      implicit none
      
      integer, intent (in) :: icha      !none       !channel number
      real, intent (in) :: flo_rate     !m3/s       !flow rate to interpolate rating curve
      integer :: ielev                  !none       |counter for point on the rating curve
      real :: rto                       !ratio      |storage time constant for the reach on
      
        !! interpolate rating curve using inflow rates
        do ielev = 1, ch_rcurv(icha)%npts
          if (flo_rate < ch_rcurv(icha)%elev(ielev)%flo_rate) then
            if (ielev == 1) then
              rto = flo_rate / ch_rcurv(icha)%elev(ielev)%flo_rate
              rcurv = ch_rcurv(icha)%elev(ielev) * rto
              rcurv%ttime = ch_rcurv(icha)%elev(ielev)%ttime
              exit
            end if
            if (ielev > 1) then
              rto = (flo_rate - ch_rcurv(icha)%elev(ielev-1)%flo_rate) /     &
                (ch_rcurv(icha)%elev(ielev)%flo_rate - ch_rcurv(icha)%elev(ielev-1)%flo_rate)
              call chrc_interp (ch_rcurv(icha)%elev(ielev-1), ch_rcurv(icha)%elev(ielev), rto, rcurv)
              exit
            end if
          end if
          if (ielev == 4) then
            rto = 1. + (flo_rate - ch_rcurv(icha)%elev(ielev)%flo_rate) / ch_rcurv(icha)%elev(ielev)%flo_rate
            rcurv = ch_rcurv(icha)%elev(ielev) * rto
            !! keep max travel time at max bankfull
            rcurv%ttime = rcurv%ttime / rto
            exit
          end if
          
        end do

      return
      end subroutine rcurv_interp_flo