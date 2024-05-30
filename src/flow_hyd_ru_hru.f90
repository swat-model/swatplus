      subroutine flow_hyd_ru_hru (iday_start, surfq, latq, tileq, uh, hyd_flo)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine determines the subdaily flow hydrographs for hru's, ru's and inflow fractions

      use hydrograph_module
      use time_module
      use basin_module
      
      implicit none
  
      integer, intent (in) :: iday_start    !           |current start day in hydrograph
      real, intent (in) :: surfq            !m3         |surface runoff
      real, intent (in) :: latq             !m3         |lateral soil flow
      real, intent (in) :: tileq            !m3         |tile flow
      real, intent (in), dimension(bsn_prm%day_lag_mx,time%step) :: uh              !unit hydrograph
      real, intent (in out), dimension(bsn_prm%day_lag_mx,time%step) :: hyd_flo     !surface runoff hydrograph
      integer :: istep                      !none       |counter  
      integer :: iday                       !           | 
      integer :: iday_prev                  !           |
      integer :: iday_cur                   !           |
      real :: ssq                           !           |
      real :: sq                            !           |
      real :: sumflo                        !           |

      !! set subdaily hydrographs
      iday_cur = iday_start
      iday_prev = iday_start - 1
      if (iday_prev < 1) iday_prev = bsn_prm%day_lag_mx

      !! subsurface flow = lateral + tile --> assume uniform throughout the day
      ssq = (latq + tileq)  / time%step

      !! sum flow in case hydrograph exceeds max days
      sumflo = 0.
      
      !!zero previous day flow hydrograph and total hydrographs
      hyd_flo(iday_prev,:) = 0.     !iob is incoming
        
      !! use unit hydrograph to compute subdaily flow hydrographs
      do iday = 1, bsn_prm%day_lag_mx
        !! only add subsurface flow today - already lagged and assumed uniform for the day
        if (iday > 1) ssq = 0.
        do istep = 1, time%step
          sq = uh(iday,istep) * surfq
          hyd_flo(iday_cur,istep) = hyd_flo(iday_cur,istep) + sq
        end do
        
        !! set current and previous days
        iday_cur = iday_cur + 1
        if (iday_cur > bsn_prm%day_lag_mx) iday_cur = 1
        iday_prev = iday_prev + 1
        if (iday_prev > bsn_prm%day_lag_mx) iday_prev = 1
      end do

      !sumflo_day = 0.
      !iday = ob(iob)%day_cur
      !do istep = 1, time%step
      !  ob(iob)%ts(iday,istep)%flo = ob(iob)%ts(iday,istep)%flo + ssq
      !  sumflo_day = sumflo_day + ob(iob)%ts(iday,istep)%flo
      !end do

      return   
      end subroutine flow_hyd_ru_hru