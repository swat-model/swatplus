      subroutine swr_percmain
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine is the master soil percolation component.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    new water table depth  equations   01/2009
!!	  c			  |none		     |a factor used to convert airvol to wtd
!!    dg          |mm            |soil layer thickness in HRU
!!    new water table depth  equations   01/2009
!!    latq(:)     |mm H2O        |total lateral flow in soil profile for the 
!!                               |day in HRU
!!    lyrtile     |mm H2O        |drainage tile flow in soil layer for day
!!    new water table depth  equations   01/2009
!!	  ne_p		  |mm/hr         |effective porosity in HRU for all soil profile layers 
!!	  ne_w		  |mm/hr         |effective porosity in HRU for soil layers above wtd 
!!    new water table depth  equations   01/2009
!!    qtile       |mm H2O        |drainage tile flow in soil profile for the day
!!    sepday      |mm H2O        |micropore percolation from soil layer
!!    sepbtm(:)   |mm H2O        |percolation from bottom of soil profile for
!!                               |the day in HRU
!!    sw_excess   |mm H2O        |amount of water in excess of field capacity
!!                               |stored in soil layer on the current day
!!    new water table depth  equations   01/2009
!!    new water table depth  equations   01/2009
!!    wt_shall    |mm H2O        |shallow water table height above bottom of soil profile
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!	  w2          |mm            |
!!	  y1          |mm 		     |dummy variable for wat
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

      use hru_module, only : hru, ihru, i_sep, inflpcp, isep, latlyr, latq, lyrtile, qstemm, sepbtm, sepcrktot, sepday,   &
         sw_excess, wt_shall, qtile, gwsoilq !rtb gwflow
      use soil_module
      use septic_data_module
      use hydrograph_module
      use gwflow_module, only : gw_soil_flag,gw_solute_flag !rtb gwflow
      use basin_module
      
      implicit none
      
      integer :: j           !none       |HRU number
      integer :: j1          !none       |counter
      integer :: ires        !none       |counter
      real :: slug           !           | 
      real :: sep_left       !           |
      real :: por_air        !           |
      real :: d              !           |
      real :: yy             !           |
      real :: xx             !           |
      real :: wat            !mm H2O     |shallow water table depth below the soil surface to up to impervious layer
      real :: sw_del         !           |
      real :: wt_del         !           |
      real :: sumqtile       !           | 
    
      j = ihru
      ires =  hru(j)%dbs%surf_stor !Jaehak 2022

      !rtb gwflow: add groundwater transferred to soil profile
      if(bsn_cc%gwflow.eq.1) then
        call gwflow_soil(j)
      endif

      !! initialize water entering first soil layer
      !! ht1%flo is infiltration from overland flow routing
      if (ires==0) then
        sepday = inflpcp + irrig(j)%applied + ht1%flo !mm
      else
        sepday = inflpcp + ht1%flo / (hru(j)%area_ha * 10.)
      endif
      
      !hru(j)%water_seep = 0.

      !! calculate crack flow 
      if (bsn_cc%crk == 1) then 
	    call swr_percmacro
	    sepday = sepday - sepcrktot
	  endif

      !back to 4 mm slug for soil routing- keeps moisture above fc
      slug = 1000.  !4.  !1000.   !this should be an input in parameters.bsn
      sep_left = sepday
      do                  !slug loop
        sepday = amin1(sep_left, slug)
        sep_left = sep_left - sepday
        sep_left = max(0., sep_left)
      do j1 = 1, soil(j)%nly
        !! add water moving into soil layer from overlying layer
        soil(j)%phys(j1)%st = soil(j)%phys(j1)%st + sepday
        
 	  !! septic tank inflow to biozone layer  J.Jeong
	  ! STE added to the biozone layer if soil temp is above zero. 
	  if (j1 == i_sep(j) .and. soil(j)%phys(j1)%tmp > 0. .and.          &
            sep(isep)%opt  /= 0) then
		soil(j)%phys(j1)%st = soil(j)%phys(j1)%st + qstemm(j)  ! in mm
        end if

       !! determine gravity drained water in layer
        sw_excess = soil(j)%phys(j1)%st - soil(j)%phys(j1)%fc

        !! initialize variables for current layer
        sepday = 0.
        latlyr = 0.
        lyrtile = 0.

        if (sw_excess > 1.e-5) then
          !! calculate tile flow (lyrtile), lateral flow (latlyr) and
          !! percolation (sepday)
          call swr_percmicro(j1)

          soil(j)%phys(j1)%st = soil(j)%phys(j1)%st - sepday - latlyr - lyrtile
          soil(j)%phys(j1)%st = Max(1.e-6, soil(j)%phys(j1)%st)
        end if

        !! summary calculations
        if (j1 == soil(j)%nly) then
          sepbtm(j) = sepbtm(j) + sepday
        endif
        latq(j) = latq(j) + latlyr
        qtile = qtile + lyrtile
        soil(j)%ly(j1)%flat = latlyr + lyrtile
        soil(j)%ly(j1)%prk = soil(j)%ly(j1)%prk + sepday
	    if (latq(j) < 1.e-6) latq(j) = 0.
        if (qtile < 1.e-6) qtile = 0.
        if (soil(j)%ly(j1)%flat < 1.e-6) soil(j)%ly(j1)%flat = 0.
      end do
      if (sep_left <= 0.) exit
      end do                    !slug loop

      !! redistribute soil water if above saturation (high water table)
      call swr_satexcess
      
      !! update soil profile water
      soil(j)%sw = 0.
      do j1 = 1, soil(j)%nly
        soil(j)%sw = soil(j)%sw + soil(j)%phys(j1)%st
      end do

      !! compute shallow water table depth and tile flow
      qtile = 0.
      wt_shall = soil(j)%zmx
      !! drainmod tile equations   08/11/2006
      if (soil(j)%phys(2)%tmp > 0.) then   !Daniel 1/29/09
        por_air = 0.9
        d = soil(j)%zmx - hru(j)%lumv%sdr_dep   !height of tiles above bottom of soil profile
        !! drainmod wt_shall equations   10/23/2006
        if (bsn_cc%wtdn == 0) then !compute wt_shall using original eq-Daniel 10/23/06
          if (soil(j)%sw > soil(j)%sumfc) then
            yy = soil(j)%sumul * por_air
            if (yy < 1.1 * soil(j)%sumfc) then
              yy = 1.1 * soil(j)%sumfc
            end if
            xx = (soil(j)%sw - soil(j)%sumfc) / (yy - soil(j)%sumfc)
            if (xx > 1.) xx = 1.
            wt_shall = xx * soil(j)%zmx
		    wat = soil(j)%zmx - wt_shall
			if(wat > soil(j)%zmx) wat = soil(j)%zmx
          end if
        else
          !compute water table depth using Daniel"s modifications
          do j1 = 1, soil(j)%nly
            if (soil(j)%wat_tbl < soil(j)%phys(j1)%d) then
              sw_del = soil(j)%swpwt - soil(j)%sw
              wt_del = sw_del * soil(j)%ly(j1)%vwt
              soil(j)%wat_tbl = soil(j)%wat_tbl + wt_del
	          if(soil(j)%wat_tbl > soil(j)%zmx)  soil(j)%wat_tbl = soil(j)%zmx
	          wt_shall = soil(j)%zmx - soil(j)%wat_tbl
	          soil(j)%swpwt = soil(j)%sw
	          exit
	        end if
	      end do
        end if
        !! drainmod wt_shall equations   10/23/2006
        
        if (hru(j)%tiledrain > 0) then
        if (hru(j)%lumv%sdr_dep > 0.) then
          if (wt_shall <= d) then
            qtile = 0.
          else
            !! Start Daniel"s tile equations modifications  01/2006
            if (bsn_cc%tdrn == 1) then
              !! drainmod tile equations
              call swr_drains           !! compute tile flow using drainmod tile equations 
            else                        
              call swr_origtile(d)      !! compute tile flow using existing tile equations 
	          if(qtile < 0.) qtile = 0.
            end if 
          end if
        end if
        end if
      end if
      !! End Daniel"s tile equations modifications  01/2006

      if (qtile > 0.) then
        !! update soil profile water after tile drainage
        sumqtile = qtile
        do j1 = 1, soil(j)%nly
          xx = soil(j)%phys(j1)%st - soil(j)%phys(j1)%fc
          if (xx > 0.) then
            if (xx > sumqtile) then
              soil(j)%phys(j1)%st = soil(j)%phys(j1)%st - sumqtile
              sumqtile = 0.
            else
              sumqtile = sumqtile - xx
              soil(j)%phys(j1)%st = soil(j)%phys(j1)%fc
            end if
          end if
        end do
        if (sumqtile > 0.) then
          qtile = qtile - sumqtile
          qtile = Max(0., qtile)
        end if
      end if

      !! update soil profile water
      soil(j)%sw = 0.
      do j1 = 1, soil(j)%nly
        soil(j)%sw = soil(j)%sw + soil(j)%phys(j1)%st
      end do

      return
      end subroutine swr_percmain