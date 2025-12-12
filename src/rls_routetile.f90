      subroutine rls_routetile (iob, tile_fr_surf)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!
      use hru_module, only : ihru, hru
      use soil_module
      use hydrograph_module
      use organic_mineral_mass_module
      
      implicit none
 
      integer, intent (in) :: iob   !           |unused object number
      integer :: j = 0              !           |hru number
      integer :: lyr = 0            !           |tile soil layer
      real, intent (in)  :: tile_fr_surf    !m3     |overland tile flow

      !! suppress unused variable warning
      if (iob < 0) continue

      j = ihru

      !! add tile inflow and nitrate to soil layer the tile is in
      !! if exceeds saturation, it will be redistributed in swr_satexcess
      lyr = hru(j)%sb%sb_db%lyr
      soil(j)%phys(lyr)%st = soil(j)%phys(lyr)%st + hru(j)%sb%inflo * (1. - tile_fr_surf)
      soil1(j)%mn(lyr)%no3 = soil1(j)%mn(lyr)%no3 + hru(j)%sb%no3 * (1. - tile_fr_surf)
      hru(j)%sb%inflo = 0.
      hru(j)%sb%no3 = 0.

      return
      end subroutine rls_routetile