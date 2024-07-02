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

      real :: latqlyr               !           |
      real :: xslat                 !           | 
      integer :: j                  !           |
      integer, intent (in) :: iob   !           | 
      integer :: dep                !           |  
      integer :: lyr                !none       |counter

      j = ihru
      
      latqrunon = ob(iob)%hin_lat%flo
      if (latqrunon > 1.e-9) then
      !!put in soil layers - weighted by depth of soil layer
        dep = 0.
        xslat = 0.
        do lyr = 1, soil(j)%nly
          latqlyr = ((soil(j)%phys(lyr)%d - dep) / soil(j)%phys(soil(j)%nly)%d) * latqrunon
          dep = soil(j)%phys(lyr)%d
          soil(j)%phys(lyr)%st = soil(j)%phys(lyr)%st + latqlyr
          !if (soil(j)%phys(lyr)%st > soil(j)%phys(lyr)%ul) then
          !  xslat = xslat + (soil(j)%phys(lyr)%st - soil(j)%phys(lyr)%ul)
          !  soil(j)%phys(lyr)%st = soil(j)%phys(lyr)%ul
          !end if
        end do
        !add excess to surface storage
      end if

      return
      end subroutine rls_routesoil