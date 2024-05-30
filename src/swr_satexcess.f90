      subroutine swr_satexcess
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine moves water to upper layers if saturated and can't perc

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sep         |mm H2O        |micropore percolation from soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max
!!    SWAT: percmacro, percmicro

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use hru_module, only : hru, ihru, surfq, satexq  !rtb gwflow
      use soil_module
      use hydrograph_module
      use basin_module
      use organic_mineral_mass_module
      use gwflow_module, only : gw_soil_flag !rtb gwflow
      use reservoir_module
      
      implicit none

      integer :: j                 !none          |HRU number
      real:: ul_excess             !              |
      real :: rto                  !              |
      integer :: nn                !none          |number of soil layers
      integer :: ly                !none          |counter
      integer :: ly1               !none          |counter
      integer :: ires                !none          |counter

      j = ihru
      ires =  hru(j)%dbs%surf_stor

      nn = soil(j)%nly
      do ly = nn, 1, -1
        !! bottom layers - move water above upper limit to next layer up
        if (ly > 1) then
          if (soil(j)%phys(ly)%st > soil(j)%phys(ly)%ul) then
            ul_excess = soil(j)%phys(ly)%st - soil(j)%phys(ly)%ul
            soil(j)%phys(ly)%st = soil(j)%phys(ly)%ul
            soil(j)%phys(ly-1)%st = soil(j)%phys(ly-1)%st + ul_excess
          else
            ul_excess = 0.
          end if
        else
          !! top layer 
          !! if no depressional storage (wetland), add to surface runoff
          ul_excess = soil(j)%phys(1)%st - soil(j)%phys(1)%ul
          if (ul_excess > 0.) then
            soil(j)%phys(1)%st = soil(j)%phys(1)%ul
            !! check if entire profile is saturated - could get excess in first layer if irrigating on frozen soil
            do ly1 = 2, nn
              soil(j)%phys(ly1)%st = soil(j)%phys(ly1)%st + ul_excess
              if (soil(j)%phys(ly1)%st > soil(j)%phys(ly1)%ul) then
                ul_excess = soil(j)%phys(ly1)%st - soil(j)%phys(ly1)%ul
                soil(j)%phys(ly1)%st = soil(j)%phys(ly1)%ul
              else
                ul_excess = 0.
                exit
              end if
            end do
          end if
          !! if still saturated
          if (ul_excess > 0.) then
            !! if depressional storage, add to ponded water 
            !! if no depressional storage, add to surface runoff
            if (ires == 0) then
              surfq(j) = surfq(j) + ul_excess
              !! rtb gwflow: add ul_excess to runoff storage
              if(gw_soil_flag.eq.1) then                                             !!!!!!  Ryan please check; 
                satexq(j) = satexq(j) + ul_excess !saturation excess (mm) leaving HRU soil profile on current day
              end if
            else
              !! move water and nutrient upward and add to wetland storage Jaehak 2022
              !! this is not actual upward movement of water and nutrient, but a process computationally 
              !! rebalancing water and mass balance in the soil profile
              wet(j)%flo = wet(j)%flo + ul_excess * 10. * hru(ihru)%area_ha   !m3=mm*10*ha)
              wet_ob(j)%depth = wet(j)%flo / hru(j)%area_ha / 10000. !m
              
              !! add ratio of nutrients to be reallocated to ponding water
              if (hru(j)%water_seep > 1.e-6) then
                rto = ul_excess / hru(j)%water_seep           
                rto = amin1 (1., rto)
                hru(j)%water_seep = rto * hru(j)%water_seep     !updated infiltration volume of the standing water
                !! substract the fraction of nutrient in the top soil layer
                soil1(j)%mn(1)%no3 = soil1(j)%mn(1)%no3 - wet_seep_day(j)%no3 * rto / hru(j)%area_ha !kg/ha
                soil1(j)%mn(1)%nh4 = soil1(j)%mn(1)%nh4 - wet_seep_day(j)%nh3 * rto / hru(j)%area_ha !kg/ha
                soil1(j)%mp(1)%act = soil1(j)%mp(1)%act - wet_seep_day(j)%solp * rto / hru(j)%area_ha !kg/ha
                soil1(j)%water(1)%n = soil1(j)%water(1)%n - wet_seep_day(j)%orgn * rto / hru(j)%area_ha !kg/ha
                soil1(j)%water(1)%p = soil1(j)%water(1)%p - wet_seep_day(j)%sedp * rto / hru(j)%area_ha !kg/ha
                !! add to the wetland water nutrient storage
                wet(j)%no3 = wet(ihru)%no3 + wet_seep_day(j)%no3 * rto  !kg
                wet(j)%nh3 = wet(ihru)%nh3 + wet_seep_day(j)%nh3 * rto  !kg
                wet(j)%orgn = wet(ihru)%orgn + wet_seep_day(j)%orgn * rto !kg
                wet(j)%solp = wet(ihru)%solp + wet_seep_day(j)%solp * rto  !kg
                wet(j)%sedp = wet(ihru)%sedp + wet_seep_day(j)%sedp * rto  !kg
              end if
            end if  !ul_excess of 1st layer > 0.
          end if    !if ponded water
        end if      !first layer or lower layers
            
      end do        !soil layer loop

      return
      end subroutine swr_satexcess