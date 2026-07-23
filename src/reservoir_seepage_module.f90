      module reservoir_seepage_module

      use soil_module, only : soil
      use hru_module, only : hru

      implicit none

      contains

      real function store_reservoir_seepage_in_hru(ihru, offered_m3, &
        water_surface_depth_mm, reservoir_bottom_depth_mm) &
        result(accepted_m3)

      integer, intent(in) :: ihru
      real, intent(in) :: offered_m3
      real, intent(in) :: water_surface_depth_mm
      real, intent(in) :: reservoir_bottom_depth_mm

      integer :: ly
      integer :: iter
      integer :: nly

      real :: area_conversion
      real :: offered_mm
      real :: accepted_target_mm
      real :: remaining_mm
      real :: distributed_mm
      real :: weight_sum
      real :: share_mm
      real :: layer_add_mm
      real :: actual_added_mm

      real :: layer_top_mm
      real :: layer_bottom_mm
      real :: contacted_thickness_mm
      real :: contacted_fraction
      real :: layer_capacity_mm
      real :: available_layer_capacity_mm
      real :: dep

      real, dimension(:), allocatable :: contact_mm
      real, dimension(:), allocatable :: capacity_mm
      real, dimension(:), allocatable :: added_mm

      accepted_m3 = 0.

      if (offered_m3 <= 0.) return
      if (.not. allocated(soil)) return
      if (.not. allocated(hru)) return

      if (ihru < 1 .or. ihru > size(soil)) return
      if (ihru > size(hru)) return

      if (.not. allocated(soil(ihru)%phys)) return
      if (hru(ihru)%area_ha <= 0.) return

      if (reservoir_bottom_depth_mm <= water_surface_depth_mm) return

      nly = soil(ihru)%nly

      if (nly <= 0) return

      allocate(contact_mm(nly))
      allocate(capacity_mm(nly))
      allocate(added_mm(nly))

      contact_mm = 0.
      capacity_mm = 0.
      added_mm = 0.

      !! Conversion between soil-water depth and volume:
      !! 1 mm over 1 ha equals 10 m3.
      area_conversion = hru(ihru)%area_ha * 10.
      offered_mm = offered_m3 / area_conversion

      !! ------------------------------------------------------------------
      !! Determine the overlap between each soil layer and the wetted
      !! reservoir interval:
      !!
      !!   water_surface_depth_mm to reservoir_bottom_depth_mm
      !!
      !! Soil-layer depths are measured downward from the HRU soil surface.
      !! ------------------------------------------------------------------

      do ly = 1, nly

        if (soil(ihru)%phys(ly)%thick <= 0.) cycle

        layer_bottom_mm = soil(ihru)%phys(ly)%d
        layer_top_mm = layer_bottom_mm - soil(ihru)%phys(ly)%thick

        contacted_thickness_mm = max(0., &
          min(layer_bottom_mm, reservoir_bottom_depth_mm) - &
          max(layer_top_mm, water_surface_depth_mm))

        if (contacted_thickness_mm <= 0.) cycle

        contacted_fraction = contacted_thickness_mm / &
                             soil(ihru)%phys(ly)%thick

        contacted_fraction = max(0., min(1., contacted_fraction))

        layer_capacity_mm = max(0., &
          soil(ihru)%phys(ly)%ul - soil(ihru)%phys(ly)%st)

        !! Only the contacted fraction of the layer storage is available
        !! for direct reservoir seepage.
        capacity_mm(ly) = layer_capacity_mm * contacted_fraction
        contact_mm(ly) = contacted_thickness_mm

      end do

      if (sum(contact_mm) <= 0.) then
        deallocate(contact_mm)
        deallocate(capacity_mm)
        deallocate(added_mm)
        return
      end if

      if (sum(capacity_mm) <= 0.) then
        deallocate(contact_mm)
        deallocate(capacity_mm)
        deallocate(added_mm)
        return
      end if

      accepted_target_mm = min(offered_mm, sum(capacity_mm))
      remaining_mm = accepted_target_mm

      !! ------------------------------------------------------------------
      !! Iteratively distribute water proportional to contacted thickness.
      !!
      !! When a layer reaches capacity, its rejected share is redistributed
      !! among the other contacted layers that still have free capacity.
      !! ------------------------------------------------------------------

      do iter = 1, nly + 2

        if (remaining_mm <= 1.e-10) exit

        weight_sum = 0.

        do ly = 1, nly

          available_layer_capacity_mm = max(0., &
            capacity_mm(ly) - added_mm(ly))

          if (contact_mm(ly) > 0. .and. &
              available_layer_capacity_mm > 1.e-10) then

            weight_sum = weight_sum + contact_mm(ly)

          end if

        end do

        if (weight_sum <= 0.) exit

        distributed_mm = 0.

        do ly = 1, nly

          available_layer_capacity_mm = max(0., &
            capacity_mm(ly) - added_mm(ly))

          if (contact_mm(ly) <= 0.) cycle
          if (available_layer_capacity_mm <= 1.e-10) cycle

          share_mm = remaining_mm * contact_mm(ly) / weight_sum

          layer_add_mm = min(share_mm, &
                             available_layer_capacity_mm)

          layer_add_mm = max(0., layer_add_mm)

          added_mm(ly) = added_mm(ly) + layer_add_mm
          distributed_mm = distributed_mm + layer_add_mm

        end do

        if (distributed_mm <= 1.e-10) exit

        remaining_mm = max(0., remaining_mm - distributed_mm)

      end do

      !! ------------------------------------------------------------------
      !! Write the accepted seepage to the contacted soil layers.
      !! ------------------------------------------------------------------

      actual_added_mm = 0.

      do ly = 1, nly

        if (added_mm(ly) <= 0.) cycle

        soil(ihru)%phys(ly)%st = soil(ihru)%phys(ly)%st + &
                                 added_mm(ly)

        actual_added_mm = actual_added_mm + added_mm(ly)

      end do

      !! Recalculate total profile soil-water storage.
      soil(ihru)%sw = 0.

      do ly = 1, nly
        soil(ihru)%sw = soil(ihru)%sw + soil(ihru)%phys(ly)%st
      end do

      !! Recalculate soil-water storage to 300 mm depth using the existing
      !! SWAT+ convention.
      soil(ihru)%sw_300 = 0.

      do ly = 1, nly

        if (ly == 1) then
          dep = 0.
        else
          dep = soil(ihru)%phys(ly-1)%d
        end if

        if (dep >= 300.) exit

        if (soil(ihru)%phys(ly)%d >= 300.) then

          soil(ihru)%sw_300 = soil(ihru)%sw_300 + &
            soil(ihru)%phys(ly)%st * &
            max(0., 300. - dep) / soil(ihru)%phys(ly)%thick

          exit

        else

          soil(ihru)%sw_300 = soil(ihru)%sw_300 + &
                              soil(ihru)%phys(ly)%st

        end if

      end do

      !! Return the volume actually written to the HRU soil state.
      accepted_m3 = actual_added_mm * area_conversion

      deallocate(contact_mm)
      deallocate(capacity_mm)
      deallocate(added_mm)

      end function store_reservoir_seepage_in_hru

      end module reservoir_seepage_module
