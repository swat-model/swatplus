      subroutine layersplit(dep_new)

      use hru_module, only : ihru
      use soil_module
      use organic_mineral_mass_module
      use constituent_mass_module
      
      implicit none
      
      integer :: nly               !none         |end of loop
      integer :: nly1              !             |
      integer :: lyn               !none         |counter
      integer :: ly                !none         |counter
	  real, intent(in):: dep_new   !             |
      
	  nly = soil(ihru)%nly

      allocate (layer1(nly))
      allocate (phys1(nly))
      do ly = 1, nly
        layer1(ly) = soil(ihru)%ly(ly)
        phys1(ly) = soil(ihru)%phys(ly)
      end do
      
      deallocate (soil(ihru)%phys)
      deallocate (soil(ihru)%ly)
      do ly = 2, nly
        !! set a soil layer at dep_new and adjust all lower layer
        if (phys1(ly)%d > dep_new) then
          soil(ihru)%nly = soil(ihru)%nly + 1
          nly1 = soil(ihru)%nly
          allocate (soil(ihru)%ly(nly1))
          allocate (soil(ihru)%phys(nly1))
          do lyn = 1, ly
            soil(ihru)%ly(lyn) = layer1(lyn)
            soil(ihru)%phys(lyn) = phys1(lyn)
            if (lyn == ly) then
              soil(ihru)%phys(lyn)%d = dep_new
              soil(ihru)%phys(lyn)%thick = dep_new - soil(ihru)%phys(lyn-1)%d
            end if
          end do
          do lyn = ly, nly
            soil(ihru)%ly(lyn+1) = layer1(lyn)
            soil(ihru)%phys(lyn+1) = phys1(lyn)
            if (lyn == ly) then
              soil(ihru)%phys(lyn+1)%thick = soil(ihru)%phys(lyn+1)%d - dep_new
            end if
          end do
          exit
        end if
      end do
      
      deallocate (layer1)
      deallocate (phys1)
	  return
      end        