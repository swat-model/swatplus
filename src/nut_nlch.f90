      subroutine nut_nlch
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine simulates the loss of nitrate via surface runoff, 
!!    lateral flow, tile flow, and percolation out of the profile

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    nperco      |none          |nitrate percolation coefficient (0-1)
!!                               |0:concentration of nitrate in surface runoff
!!                               |  is zero
!!                               |1:surface runoff has same concentration of
!!                               |  nitrate as percolate
!!    surfq(:)    |mm H2O        |surface runoff generated on day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Max, Min

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module
      use organic_mineral_mass_module
      use hru_module, only : hru, latno3, percn, surqno3, tileno3, surfq, ihru, qtile, gwsoiln
      use gwflow_module, only : gw_soil_flag,gw_solute_flag,hru_soil
      use soil_module
      
      implicit none 

      integer :: j         !none          |HRU number
      integer :: jj        !none          |counter 
      integer :: jlo       !none          |counter for taking tile no3 from lower layers
      real :: sro          !mm H2O        |surface runoff 
      real :: ssfnlyr      !kg N/ha       |nitrate transported in lateral flow from layer
      real :: percnlyr     !kg N/ha       |nitrate leached to next lower layer with
                           !              |percolation
      real :: vv           !mm H2O        |water mixing with nutrient in layer
      real :: vno3         !              |
      real :: co           !kg N/mm       |concentration of nitrate in solution
      real :: ww           !varies        |variable to hold intermediate calculation
      real :: ul_sum       !mm            |sum of porosity in tile layer to lowest layer
      real :: no3_sum      !kg/ha         |sum of no3 in tile layer to lowest layer
      real :: tileno3_left !kg/ha         |remaining no3 if not available from layers below tile
      real :: st_sum

      j = ihru

      
      !rtb gwflow: add nitrate mass transferred to soil profile from the aquifer
      if (gw_soil_flag == 1 .and. gw_solute_flag == 1) then
        do jj = 1, soil(j)%nly
          soil1(j)%mn(jj)%no3 = soil1(j)%mn(jj)%no3 + hru_soil(j,jj,1) !kg/ha
          gwsoiln(j) = gwsoiln(j) + hru_soil(j,jj,1) !HRU total
        enddo
      endif
      
      percnlyr = 0.

      do jj = 1, soil(j)%nly

        !! add nitrate leached from layer above
        soil1(j)%mn(jj)%no3 = soil1(j)%mn(jj)%no3 + percnlyr
	    if (soil1(j)%mn(jj)%no3 < 1.e-6) soil1(j)%mn(jj)%no3 = 0.0

        !! determine concentration of nitrate in mobile water
        if (jj == 1) then
          sro = surfq(j)
        else
          sro = 0.
        end if
        vv = soil(j)%ly(jj)%prk + sro + soil(j)%ly(jj)%flat + 1.e-10
        if (hru(j)%lumv%ldrain == jj) vv = vv + qtile
        ww = -vv / ((1. - soil(j)%anion_excl) * soil(j)%phys(jj)%ul)
        vno3 = soil1(j)%mn(jj)%no3 * (1. - Exp(ww))
        co = Max(vno3 / vv, 0.)     !kg/ha/mm (if * 100 = ppm)

        !! calculate nitrate in surface runoff
        if (jj == 1) then
          surqno3(j) = surfq(j) * bsn_prm%nperco * co
          surqno3(j) = Min(surqno3(j), soil1(j)%mn(jj)%no3)
          soil1(j)%mn(jj)%no3 = max(0.0001,soil1(j)%mn(jj)%no3 - surqno3(j))
        endif

        !! calculate nitrate in tile flow 
        if (hru(j)%lumv%ldrain == jj .and. qtile > 0.) then
          !! take no3 from tile layer and all lower layers
          !! assume rising water table will move nitrates up
          ul_sum = 0.
          no3_sum = 0.
          st_sum = 0.
          do jlo = jj, soil(j)%nly
            ul_sum = ul_sum + soil(j)%phys(jlo)%ul
            no3_sum = no3_sum + soil1(j)%mn(jlo)%no3
            st_sum = st_sum + soil(j)%phys(jlo)%st
          end do
          vv = qtile
          ww = -vv / ((1. - soil(j)%anion_excl) * ul_sum)
          vno3 = no3_sum * (1. - Exp(ww))
          co = Max(vno3 / vv, 0.)     !kg/ha/mm (if * 100 = ppm)
          tileno3(j) = co * bsn_prm%nperco_lchtile  * qtile
          !! subtract tile no3 from soil layers
          tileno3_left = tileno3(j)
          do jlo = jj, soil(j)%nly
            soil1(j)%mn(jlo)%no3 = soil1(j)%mn(jlo)%no3 - tileno3_left
            if (soil1(j)%mn(jlo)%no3 < 0.) then
              tileno3_left = - soil1(j)%mn(jlo)%no3
              soil1(j)%mn(jlo)%no3 = 0.
            else
              tileno3_left = 0.
            end if
          end do
          tileno3(j) = tileno3(j) - tileno3_left 
        end if

        !! calculate nitrate in lateral flow
        if (jj == 1) then
          ssfnlyr = co * soil(j)%ly(jj)%flat
        else
          ssfnlyr = co * soil(j)%ly(jj)%flat
        end if
        ssfnlyr = Min(ssfnlyr, soil1(j)%mn(jj)%no3)
        latno3(j) = latno3(j) + ssfnlyr
        soil1(j)%mn(jj)%no3 = max(0.0001,soil1(j)%mn(jj)%no3 - ssfnlyr)

        !! calculate nitrate in percolate
        percnlyr = co * soil(j)%ly(jj)%prk
        percnlyr = Min(percnlyr, soil1(j)%mn(jj)%no3)
        soil1(j)%mn(jj)%no3 = max(0.0001,soil1(j)%mn(jj)%no3 - percnlyr)
        
        !! last layer leaches from soil profile
        if (jj == soil(j)%nly) then
          percn(j) = percnlyr
        end if
      end do
      
      !nloss = (2.18 * hru(j)%topo%dis_stream - 8.63) / 100.
      !nloss = Max(0.,nloss)
      !nloss = Amin1(1.,nloss)
      !latno3(j) = (1. - nloss) * latno3(j)

      return
      end subroutine nut_nlch