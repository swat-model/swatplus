      subroutine layersplit(dep_new)

      use hru_module, only : ihru, isep_ly
      use soil_module
      use organic_mineral_mass_module
      use constituent_mass_module
      
      implicit none
      
      integer :: nly               !none         |end of loop
      integer :: nly1              !             |
      integer :: lyn               !none         |counter
      integer :: j                 !             |
      integer :: ly                !none         |counter
      real :: dif                  !             |
	  real, intent(in):: dep_new   !             |
      
	  nly = soil(j)%nly

      allocate (layer1(nly))
      do ly = 1, nly
        layer1(ly) = soil(ihru)%ly(ly)
      end do
      
      do ly = 2, nly 
        dif = Abs(dep_new - soil(ihru)%phys(ly)%d)
        !! if values are within 10 mm of one another, reset boundary
        if (dif < 10.) then
          soil(ihru)%phys(ly)%d = dep_new
          exit
        end if

        !! set a soil layer at dep_new and adjust all lower layers                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         (ihru)%ly)
        deallocate (soil(ihru)%phys)
        !!!deallocate (soil(ihru)%nut)
        deallocate (soil(ihru)%ly)
        deallocate (cs_soil(ihru)%ly)
        nly1 = soil(ihru)%nly + 1                                                                                                         
        allocate (soil(ihru)%ly(nly1))
        allocate (cs_soil(ihru)%ly(nly1))
        allocate (soil(ihru)%phys(nly1))
        allocate (soil1(ihru)%tot(nly1))
        allocate (soil1_init(ihru)%tot(nly1))
        if (soil(ihru)%phys(ly)%d > dep_new) then                                                                                                     
          isep_ly = ly
          soil(ihru)%phys(ly)%d = dep_new
          do lyn = ly, nly
            soil(ihru)%ly(lyn+1) = layer1(lyn)
          end do
        end if
      end do
      
      deallocate (layer1)
	  return
      end        