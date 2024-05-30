      subroutine rls_routesoil (iob)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!
      use hru_module, only : ihru, latqrunon
      use soil_module
      use hydrograph_module
      
      implicit none

      real :: latqlyr               !mm         |lateral flow into layer 
      integer :: j                  !           |hru number
      integer, intent (in) :: iob   !           |object number
      integer :: lyr                !           |layer number

      j = ihru
      
      latqrunon = ob(iob)%hin_lat%flo
      if (latqrunon > 1.e-9) then
      !!put in soil layers - weighted by depth of soil layer
        do lyr = 1, soil(j)%nly
          latqlyr = (soil(j)%phys(lyr)%thick / soil(j)%phys(soil(j)%nly)%d) * latqrunon
          soil(j)%phys(lyr)%st = soil(j)%phys(lyr)%st + latqlyr
        end do
        !! excess above ul is added to surface storage in saturation excess routine
      end if

      return
      end subroutine rls_routesoil