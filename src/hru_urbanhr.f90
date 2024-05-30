      subroutine hru_urbanhr
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes loadings from urban areas using the
!!    a build-up/wash-off algorithm at subdaily time intervals

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dirtmx(:)    |kg/curb km     |maximum amount of solids allowed to
!!                                 |build up on impervious surfaces
!!    hru_km(:)    |km^2           |area of HRU in square kilometers
!!    ihru         |none           |HRU number
!!    isweep(:,:,:)|julian date    |date of street sweeping operation
!!    surfq(:)     |mm H2O         |surface runoff for the day in HRU
!!    thalf(:)     |days           |time for the amount of solids on
!!                                 |impervious areas to build up to 1/2
!!                                 |the maximum level
!!    tnconc(:)    |mg N/kg sed    |concentration of total nitrogen in
!!                                 |suspended solid load from impervious
!!                                 |areas
!!    tno3conc(:)  |mg NO3-N/kg sed|concentration of NO3-N in suspended
!!                                 |solid load from impervious areas
!!    tpconc(:)    |mg P/kg sed    |concentration of total phosphorus in
!!                                 |suspended solid load from impervious
!!                                 |areas
!!    twash(:)     |days           |time that solids have built-up on streets
!!    urbcoef(:)   |1/mm           |wash-off coefficient for removal of
!!                                 |constituents from an impervious surface
!!    ubntss(:)    |metric tons    |TSS loading from urban impervious cover
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    twash(:)    |
!!    ubntss(:)    |metric tons    |TSS loading from urban impervious cover
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Max, Log
!!    SWAT: Regres, sweep

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use hru_module, only : hru, ihru, ulu, twash, surfq, ubntss, surqno3, sedorgn, sedorgp, init_abstrc,  &
         surqsolp, ubnrunoff, isweep, phusw, phubase, etday, ipl  
      use plant_module
      use urban_data_module
      use climate_module
      use time_module
      
      implicit none

      real :: sus_sol      !kg            |suspended solid loading in surface runoff
                           !              |from urban area
      real :: tn           !kg            |total nitrogen in surface runoff from
                           !              |urban area

      real :: tp           !kg            |total phosphorus in surface runoff from 
                           !              |urban area
      real :: urbk         !1/hr          | 
      real :: dirto        !kg/ha         |amount of solids built up on impervious
                           !              |surfaces at the beginning of time step
      integer :: j         !none          |HRU number
      real :: qdt          !              |
	  real*8 :: dirt       !kg/curb km    |amount of solids built up on impervious
                           !              |surfaces
      integer :: k         !none          |counter 
      integer :: tno3      !              |

      j = ihru
      ulu = hru(j)%luse%urb_lu

	  do k = 1, time%step

          !! build-up/wash-off algorithm

          !! rainy day: no build-up, street cleaning allowed
		  
		   qdt = ubnrunoff(k) * 60./ real(time%dtm) !urban runoff in mm/hr
	      if (qdt > 0.025 .and. surfq(j) > 0.1) then   ! SWMM : 0.001 in/hr (=0.0254mm/hr)
       
          !! calculate amount of dirt on streets prior to wash-off
              dirt = 0.
              dirto = 0.
              dirto = urbdb(ulu)%dirtmx * twash(j) / (urbdb(ulu)%thalf + twash(j))

          !! calculate wash-off of solids
              urbk = 0.				! qp_cms -> hhqday for subdaily time steps 6/19/09 JJ
              urbk = urbdb(ulu)%urbcoef * qdt  
                                     
              dirt=dirto * Exp (- urbk * real(time%dtm) / 60.)
              if (dirt < 1.e-6) dirt = 0.0

          !! set time to correspond to lower amount of dirt
              twash(j) = 0.
              twash(j) = urbdb(ulu)%thalf * dirt / (urbdb(ulu)%dirtmx - dirt)
              sus_sol = 0.
              tn = 0.
              tp = 0.
              tno3 = 0.
          !! amounts are kg/ha
              sus_sol = Max(0., (dirto - dirt) * urbdb(ulu)%curbden)
              tn = urbdb(ulu)%tnconc * sus_sol / 1.e6
              tp = urbdb(ulu)%tpconc * sus_sol / 1.e6
              tno3 = urbdb(ulu)%tno3conc * sus_sol / 1.e6

              ubntss(k) = (.001*sus_sol*hru(j)%area_ha)*urbdb(ulu)%fimp
              surqno3(j) = tno3 * urbdb(ulu)%fimp + surqno3(j) *           &      
                                             (1. - urbdb(ulu)%fimp)
              sedorgn(j) = (tn - tno3) * urbdb(ulu)%fimp + sedorgn(j) *    &
                                             (1. - urbdb(ulu)%fimp)
              sedorgp(j) = .75 * tp * urbdb(ulu)%fimp + sedorgp(j) *       &
                                             (1. - urbdb(ulu)%fimp)
              surqsolp(j) = .25 * tp * urbdb(ulu)%fimp + surqsolp(j) *     &
                                             (1. - urbdb(ulu)%fimp)
	      else
          !! no surface runoff
              twash(j) = twash(j) + time%dtm / 1440.
 
          !! perform street sweeping
          if (isweep(j) > 0 .and. time%day >= isweep(j)) then
            call hru_sweep
          else if (phusw(j) > 0.0001) then
            if (pcom(j)%plcur(ipl)%gro == "n") then
              if (phubase(j) > phusw(j)) call hru_sweep
            else 
              if (pcom(j)%plcur(1)%phuacc > phusw(j)) call hru_sweep
            end if
          end if
        end if

	  sus_sol=0
	  
	  ! Compute evaporation of water (initial abstraction) from impervious cover
	  init_abstrc(j) = init_abstrc(j) - etday / time%step
	  init_abstrc(j) = max(0.,init_abstrc(j))
	end do

      !! perform street sweeping
      if(surfq(j) < 0.1) then
	  if (isweep(j) > 0 .and. time%day >= isweep(j)) then
                call hru_sweep
        else if (phusw(j) > 0.0001) then
          if (pcom(j)%plcur(ipl)%gro == "n") then
            if (phubase(j) > phusw(j)) then
		    call hru_sweep
	      endif
          else 
            if (pcom(j)%plcur(1)%phuacc > phusw(j)) then
		    call hru_sweep
	      endif
          end if
        end if
      end if

      return
      end subroutine hru_urbanhr