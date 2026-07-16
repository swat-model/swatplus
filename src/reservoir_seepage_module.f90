      module reservoir_seepage_module

      use soil_module, only : soil
      use hru_module, only : hru

      implicit none

      real, parameter :: seepage_depth_limit_mm = 1000.

      contains

      real function hru_soil_capacity_1m_m3(ihru) result(capacity_m3)

      integer, intent(in) :: ihru

      integer :: ly
      real :: layer_top_mm
      real :: active_thickness_mm
      real :: active_fraction
      real :: layer_capacity_mm
      real :: total_capacity_mm

      capacity_m3 = 0.
      total_capacity_mm = 0.

      if (.not. allocated(soil)) return
      if (.not. allocated(hru)) return
      if (ihru < 1 .or. ihru > size(soil)) return
      if (ihru > size(hru)) return
      if (.not. allocated(soil(ihru)%phys)) return

      do ly = 1, soil(ihru)%nly

        layer_top_mm = soil(ihru)%phys(ly)%d - &
                       soil(ihru)%phys(ly)%thick

        active_thickness_mm = max(0., &
          min(soil(ihru)%phys(ly)%d, seepage_depth_limit_mm) - &
          layer_top_mm)

        if (active_thickness_mm <= 0.) cycle
        if (soil(ihru)%phys(ly)%thick <= 0.) cycle

        active_fraction = active_thickness_mm / &
                          soil(ihru)%phys(ly)%thick

        layer_capacity_mm = max(0., &
          soil(ihru)%phys(ly)%ul - soil(ihru)%phys(ly)%st)

        total_capacity_mm = total_capacity_mm + &
                            layer_capacity_mm * active_fraction

      end do

      !! 1 mm over 1 ha equals 10 m3.
      capacity_m3 = total_capacity_mm * hru(ihru)%area_ha * 10.

      end function hru_soil_capacity_1m_m3

      end module reservoir_seepage_module
