      subroutine soil_phys_init (isol)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine initializes soil physical properties

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ddrain(:)     |mm            |depth to the sub-surface drain
!!    i             |none          |HRU number
!!    rock(:)       |%             |percent of rock fragments in soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    rock(:)       |none          |exponential value that is a function of
!!                                 |percent rock
!!    sol_st(:,:)   |mm H2O        |amount of water stored in the soil layer
!!                                 |on any given day (less wp water)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Sqrt
!!    SWAT: Curno

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use soil_module
      use basin_module
      use time_module
      
      implicit none

      integer :: j            !none          |counter
      integer :: nly          !none          |number of soil layers
      real :: sumpor          !mm            |porosity of profile
      real :: pormm           !mm            |porosity in mm depth
      real :: nota            !              |
      real :: a               !m^2           |cross-sectional area of channel
      real :: b               !m             |bottom width of channel
      real :: c               !none          |inverse of channel side slope
      real :: d               !m             |depth of flow
      integer :: isol         !              |   
      real :: drpor           !              |
      real :: sa              !ha            |surface area of impounded water body
      real :: cl              !              |
      real :: si              !m/n           |slope of main channel
      real :: depth_prev      !              |
      

      if (sol(isol)%s%alb < 0.1) sol(isol)%s%alb  = 0.1
      if (sol(isol)%s%anion_excl<=1.e-6) sol(isol)%s%anion_excl = 0.5
      if (sol(isol)%s%anion_excl >= 1.) sol(isol)%s%anion_excl = 0.99

      nly = sol(isol)%s%nly
      do j = 1, nly
        a = 50.0
        b = 20.0
        c = 5.0
        d = 2.0           
        nota = 10.
        if (sol(isol)%phys(j)%k <= 0.0) then 
          if (sol(isol)%s%hydgrp == "A") then
            sol(isol)%phys(j)%k = a
	    else
          if (sol(isol)%s%hydgrp == "B") then
            sol(isol)%phys(j)%k = b
	    else
          if (sol(isol)%s%hydgrp == "C") then
            sol(isol)%phys(j)%k = c
	    else
          if (sol(isol)%s%hydgrp == "D") then
            sol(isol)%phys(j)%k = d          !Claire 12/2/09
          else 
           sol(isol)%phys(j)%k = nota
          endif
          endif
          endif
          endif
        endif

        if (sol(isol)%phys(j)%bd <= 1.e-6) sol(isol)%phys(j)%bd = 1.3
        if (sol(isol)%phys(j)%bd > 2.) sol(isol)%phys(j)%bd = 2.0
        if (sol(isol)%phys(j)%awc <= 1.e-6) sol(isol)%phys(j)%awc = .005
        if (sol(isol)%phys(j)%awc >= .8) sol(isol)%phys(j)%awc = .8
        if (sol(isol)%phys(j)%rock > 98.0) sol(isol)%phys(j)%rock= 98.0
        
        !! Defaults for ph and calcium mjw average of 20,000 SSURGO soils mjw rev 490
        if (sol(isol)%ly(j)%cal <= 1.e-6) sol(isol)%ly(j)%cal = 2.8
        if (sol(isol)%ly(j)%ph<= 1.e-6) sol(isol)%ly(j)%ph = 6.5
      end do

      nly = sol(isol)%s%nly

!!    calculate water content of soil at -1.5 MPa and -0.033 MPa
      do j = 1, nly
        sol(isol)%phys(j)%wp = 0.4 * sol(isol)%phys(j)%clay * sol(isol)%phys(j)%bd / 100.
        if (sol(isol)%phys(j)%wp <= 0.) sol(isol)%phys(j)%wp = .005
          sol(isol)%phys(j)%up = sol(isol)%phys(j)%wp + sol(isol)%phys(j)%awc
          sol(isol)%phys(j)%por = 1. - sol(isol)%phys(j)%bd / 2.65
        if (sol(isol)%phys(j)%up >= sol(isol)%phys(j)%por) then
           sol(isol)%phys(j)%up = sol(isol)%phys(j)%por - .05
           sol(isol)%phys(j)%wp = sol(isol)%phys(j)%up - sol(isol)%phys(j)%awc
        if (sol(isol)%phys(j)%wp <= 0.) then
          sol(isol)%phys(j)%up = sol(isol)%phys(j)%por * .75
          sol(isol)%phys(j)%wp = sol(isol)%phys(j)%por * .25
        end if
        end if
        !! compute drainable porosity and variable water table factor - Daniel
        drpor = sol(isol)%phys(j)%por - sol(isol)%phys(j)%up
        sol(isol)%ly(j)%vwt=(437.13*drpor**2)-(95.08 * drpor)+8.257
       end do

      sa = sol(isol)%phys(1)%sand / 100.
      cl = sol(isol)%phys(1)%clay  / 100.
      si = sol(isol)%phys(1)%silt / 100.
!!    determine detached sediment size distribution
!!    typical for mid-western soils in USA (Foster et al., 1980)
!!    Based on SWRRB
       sol(isol)%s%det_san = sa * (1. - cl)** 2.49   !! Sand fraction
       sol(isol)%s%det_sil = 0.13 * si               !! Silt fraction
       sol(isol)%s%det_cla = 0.20 * cl               !! Clay fraction   
       if (cl < .25) then
         sol(isol)%s%det_sag = 2.0 * cl              !! Small aggregate fraction                    
       else if (cl > .5) then
         sol(isol)%s%det_sag = .57
       else
         sol(isol)%s%det_sag = .28 * (cl - .25) + .5
       end if

       sol(isol)%s%det_lag = 1. - sol(isol)%s%det_san -                 &                
          sol(isol)%s%det_sil - sol(isol)%s%det_cla - sol(isol)%s%det_sag  !! Large Aggregate fraction

!!	Error check. May happen for soils with more sand
!!    Soil not typical of mid-western USA
!!    The fraction wont add upto 1.0
	if (sol(isol)%s%det_lag < 0.) then
	  sol(isol)%s%det_san = sol(isol)%s%det_san/(1 - sol(isol)%s%det_lag) 
	  sol(isol)%s%det_sil = sol(isol)%s%det_sil/(1 - sol(isol)%s%det_lag) 
	  sol(isol)%s%det_cla = sol(isol)%s%det_cla/(1 - sol(isol)%s%det_lag) 
	  sol(isol)%s%det_sag = sol(isol)%s%det_sag/(1 - sol(isol)%s%det_lag) 
	  sol(isol)%s%det_lag = 0.
      end if

!!    initialize water/drainage coefs for each soil layer
      depth_prev = 0.
      sumpor = 0.
      do j = 1, nly
        sol(isol)%phys(j)%thick = sol(isol)%phys(j)%d - depth_prev
        pormm = sol(isol)%phys(j)%por * sol(isol)%phys(j)%thick
        sumpor = sumpor + pormm
        sol(isol)%phys(j)%ul = (sol(isol)%phys(j)%por -                  &
           sol(isol)%phys(j)%wp) * sol(isol)%phys(j)%thick
        sol(isol)%s%sumul = sol(isol)%s%sumul + sol(isol)%phys(j)%ul
        sol(isol)%phys(j)%fc = sol(isol)%phys(j)%thick * (sol(isol)%phys(j)%up -            &
                                              sol(isol)%phys(j)%wp)
        sol(isol)%s%sumfc = sol(isol)%s%sumfc + sol(isol)%phys(j)%fc
        sol(isol)%phys(j)%st = sol(isol)%phys(j)%fc * sol(isol)%s%ffc
        sol(isol)%phys(j)%hk = (sol(isol)%phys(j)%ul -                 &
           sol(isol)%phys(j)%fc) / sol(isol)%phys(j)%k
        if (sol(isol)%phys(j)%hk < 1.) sol(isol)%phys(j)%hk = 1.
        sol(isol)%s%sw = sol(isol)%s%sw + sol(isol)%phys(j)%st
        sol(isol)%phys(j)%wpmm = sol(isol)%phys(j)%wp * sol(isol)%phys(j)%thick
        sol(isol)%s%sumwp = sol(isol)%s%sumwp + sol(isol)%phys(j)%wpmm
        sol(isol)%phys(j)%crdep = sol(isol)%s%crk * 0.916 *            &
           Exp(-.0012 * sol(isol)%phys(j)%d) * sol(isol)%phys(j)%thick
        sol(isol)%ly(j)%volcr = sol(isol)%phys(j)%crdep *              &
          (sol(isol)%phys(j)%fc - sol(isol)%phys(j)%st) /              &
          (sol(isol)%phys(j)%fc)
        depth_prev = sol(isol)%phys(j)%d
      end do
      !! initialize water table depth and soil water for Daniel
      sol(isol)%s%swpwt = sol(isol)%s%sw
      if (sol(isol)%s%ffc > 1.) then
        sol(isol)%s%wat_tbl = (sol(isol)%s%det_lag - sol(isol)%s%ffc *   &
          sol(isol)%s%sumfc) / sol(isol)%phys(nly)%d
      else
        sol(isol)%s%wat_tbl = 0.
      end if
      sol(isol)%s%avpor = sumpor / sol(isol)%phys(nly)%d
      sol(isol)%s%avbd = 2.65 * (1. - sol(isol)%s%avpor)

!!    calculate infiltration parameters for subdaily time step
      if (time%step > 1) then
        sol(isol)%phys(1)%sand = 100. - sol(isol)%phys(1)%clay - sol(isol)%phys(1)%silt
      end if

      return
      end subroutine soil_phys_init