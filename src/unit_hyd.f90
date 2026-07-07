      subroutine unit_hyd (tc, uh)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine computes variables related to the watershed hydrology:
!!    the time of concentration for the subbasins, lagged surface runoff,
!!    the coefficient for the peak runoff rate equation, and lateral flow travel
!!    time.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~1
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    ch_n(1,:)   |none          |Manning"s "n" value for the tributary channels
!!    ch_s(1,:)   |m/m           |average slope of tributary channels
!!    gdrain(:)   |hrs           |drain tile lag time: the amount of time
!!                               |between the transfer of water from the soil
!!                               |to the drain tile and the release of the
!!                               |water from the drain tile to the reach.
!!    hru_km(:)   |km2           |area of HRU in square kilometers
!!    lat_ttime(:)|days          |lateral flow travel time
!!    slsoil(:)   |m             |slope length for lateral subsurface flow
!!    slsubbsn(:) |m             |average slope length for subbasin
!!    tconc(:)     |hr           |time of concentration
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 


!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    lat_ttime(:)|none          |Exponential of the lateral flow travel time
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    l           |none          |counter
!!    scmx        |mm/hr         |maximum soil hydraulic conductivity
!!    t_ch        |hr            |time for flow entering the farthest upstream 
!!                               |channel to reach the subbasin outlet
!!    xx          |none          |variable to hold calculation result
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~  
!!    SWAT: Ttcoef

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      !use hru_module, only : itb
      use basin_module
      use time_module
      use hydrograph_module, only : sp_ob
      
      implicit none
      
      integer :: j              !none       |counter
      real :: ql                !           | 
      real :: sumq              !           |
      real :: tb                !           |
      real :: tp                !           |
      integer :: int            !           |
      integer :: i              !none       |counter
      real :: xi                !           |
      real :: q                 !           |
      integer :: max            !           |
      integer :: itb            !           |
      integer :: istep          !none       |time step that corresponds to time%step for routing
      integer :: iday           !none       |current day in the unit hydrograph
      real :: t_inc             !hr         |time increment to sum the unit hyd
      real :: t_tot             !hr         |total time in the hydrograph
      
      real, intent (in)  :: tc
      real, intent (out), dimension(bsn_prm%day_lag_mx,time%step) :: uh

      !! compute unit hydrograph for computing hydrograph from direct runoff
      ql = 0.
      sumq = 0.
      uh = 0.
      tb = .5 + .6 * tc + bsn_prm%tb_adj    !baseflow time, hr
      if (tb > 48.) tb = 48.			    !maximum 48hrs
      tp = .375 * tb                        ! time to peak flow

      !! sum 20 points on the unit hydrograph to get sum for the time%step
	  itb = 20 
      t_inc = tb / 20.
      t_tot = 0.
      istep = 1
      iday = 1
        
	  !! Triangular Unit Hydrograph
	  if (bsn_cc%uhyd == 0) then
        !! increment for number of points to estimate uh
	    do i = 1, itb
          t_tot = t_tot + t_inc
          if (t_tot > float(istep * time%step / 24)) then
            istep = istep + 1
          end if
          if (t_tot > float(iday * 24)) then
            iday = iday + 1
            t_tot = 0
          end if
          
 	      if (t_tot < tp) then
            !! rising limb of hydrograph
            q = t_tot / tp
          else
            !! falling limb of hydrograph
            q = (tb - t_tot) / (tb - tp)
          end if
          q = Max(0.,q)
          uh(iday,istep) = uh(iday,istep) + (q + ql) / 2.
          sumq = sumq + (q + ql) / 2.
          ql = q
        end do

	  ! Gamma Function Unit Hydrograph
	  else if (bsn_cc%uhyd == 1) then
        i = 1; q = 1.
		do while (q > 0.0001)
          t_tot = t_tot + t_inc
          if (t_tot > float(istep * time%step / 24)) then
            istep = istep + 1
          end if
          if (t_tot > float(iday * 24)) then
            iday = iday + 1
            t_tot = 0
          end if
          
          xi = float(i)
          q = (xi / tp) ** bsn_prm%uhalpha * exp((1. - xi / tp) * bsn_prm%uhalpha)
          q = Max(0.,q)
          uh(iday,istep) = (q + ql) / 2.
          ql = q
          sumq = sumq + uh(iday,istep)
	      i = i + 1
	      if (i > 3. * time%step) exit
        end do

	  end if 
          
      do i = 1, iday
        do istep = 1, time%step
          uh(i,istep) = uh(i, istep) / sumq
        end do
      end do
	  
      return
      end subroutine unit_hyd