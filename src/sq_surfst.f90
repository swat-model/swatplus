      subroutine sq_surfst

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine determines the net surface runoff reaching the 
!!    main channel on a given day. The net amount of water reaching
!!    the main channel can include water in surface runoff from the 
!!    previous day and will exclude surface runoff generated on the
!!    current day which takes longer than one day to reach the main
!!    channel

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    brt(:)      |none          |fraction of surface runoff that takes
!!                               |one day or less to reach the subbasin
!!                               |outlet
!!    surf_bs(1,:)|mm H2O        |amount of surface runoff lagged over one
!!                               |day
!!    surfq(:)    |mm H2O        |surface runoff generated in HRU on the
!!                               |current day 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bsprev      |mm H2O        |surface runoff lagged from prior day
!!    surf_bs(1,:)|mm H2O        |amount of surface runoff lagged over one
!!                               |day 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module
      use time_module
      use hru_module, only : surf_bs, surfq, brt, hhsurf_bs, hhsurfq, ihru, bsprev, qday 
      
      implicit none

      integer :: j           !none          |HRU number
      integer :: k           !none          |counter

      j = ihru

      if (bsn_cc%gampt == 0) then	
        bsprev = surf_bs(1,j)
	    surf_bs(1,j) = Max(1.e-6, surf_bs(1,j) + surfq(j))
        qday = surf_bs(1,j) * brt(j)
        surf_bs(1,j) = surf_bs(1,j) - qday
	  else
		bsprev = hhsurf_bs(1,j,time%step)		! lag from previous day J.Jeong 4/06/2009
        qday = 0.
	    do k=1,time%step
	      !! Left-over (previous timestep) + inflow (current  timestep)
          hhsurf_bs(1,j,k) = Max(0., bsprev + hhsurfq(j,k))
   	
	      !! new estimation of runoff and sediment reaching the main channel
	      hhsurfq(j,k) = hhsurf_bs(1,j,k) * brt(j)
	      hhsurf_bs(1,j,k) = hhsurf_bs(1,j,k) - hhsurfq(j,k)
   	  
	      !! lagged at the end of time step  
	      bsprev = hhsurf_bs(1,j,k)
          
          !! daily total yield from the HRU
	      qday = qday + hhsurfq(j,k)
	    end do
	  end if
     
      return
      end subroutine sq_surfst