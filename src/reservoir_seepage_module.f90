       module reservoir_seepage_module

       use soil_module, only : soil
       use hru_module, only : hru

       implicit none

       real, parameter :: seepage_depth_limit_mm = 1000.


        !! Seepage accepted on the current day and applied to the
        !! receiving HRU at the beginning of its next simulation day.
        real, dimension(:), allocatable :: pending_reservoir_seepage_m3

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



        subroutine ensure_pending_reservoir_seepage_size()

        if (.not. allocated(hru)) return

        if (.not. allocated(pending_reservoir_seepage_m3)) then
          allocate(pending_reservoir_seepage_m3(size(hru)))
          pending_reservoir_seepage_m3 = 0.
        end if

        end subroutine ensure_pending_reservoir_seepage_size


        real function queue_reservoir_seepage_for_hru(ihru, offered_m3) &
          result(accepted_m3)

        integer, intent(in) :: ihru
        real, intent(in) :: offered_m3

        real :: current_capacity_m3
        real :: available_capacity_m3

        accepted_m3 = 0.

        if (offered_m3 <= 0.) return
        if (.not. allocated(hru)) return
        if (ihru < 1 .or. ihru > size(hru)) return

        call ensure_pending_reservoir_seepage_size()

        if (.not. allocated(pending_reservoir_seepage_m3)) return

        current_capacity_m3 = hru_soil_capacity_1m_m3(ihru)

        available_capacity_m3 = max(0., current_capacity_m3 - &
          pending_reservoir_seepage_m3(ihru))

        accepted_m3 = min(offered_m3, available_capacity_m3)
        accepted_m3 = max(0., accepted_m3)

        pending_reservoir_seepage_m3(ihru) = &
          pending_reservoir_seepage_m3(ihru) + accepted_m3

        if (accepted_m3 > 0.) then
          write(*,'(a,i0,2(a,es16.8))') &
            "QUEUE_SEEP hru=", ihru, &
            " accepted_m3=", accepted_m3, &
            " pending_m3=", pending_reservoir_seepage_m3(ihru)
        end if

        end function queue_reservoir_seepage_for_hru


        subroutine apply_pending_reservoir_seepage_to_hru(ihru)

        integer, intent(in) :: ihru

        real :: pending_m3
        real :: stored_m3

        if (.not. allocated(hru)) return
        if (ihru < 1 .or. ihru > size(hru)) return

        call ensure_pending_reservoir_seepage_size()

        if (.not. allocated(pending_reservoir_seepage_m3)) return

        pending_m3 = pending_reservoir_seepage_m3(ihru)

        if (pending_m3 <= 0.) return

        stored_m3 = store_reservoir_seepage_in_hru(ihru, pending_m3)

        write(*,'(a,i0,4(a,es16.8))') &
          "APPLY_SEEP hru=", ihru, &
          " pending_m3=", pending_m3, &
          " stored_m3=", stored_m3, &
          " soil_sw=", soil(ihru)%sw, &
          " remaining_m3=", max(0., pending_m3 - stored_m3)

        pending_reservoir_seepage_m3(ihru) = &
          max(0., pending_m3 - stored_m3)

        end subroutine apply_pending_reservoir_seepage_to_hru

       real function store_reservoir_seepage_in_hru(ihru, offered_m3) &
         result(accepted_m3)

       integer, intent(in) :: ihru
       real, intent(in) :: offered_m3

       integer :: ly
       integer :: last_active_layer
       real :: area_conversion
       real :: layer_top_mm
       real :: active_thickness_mm
       real :: active_fraction
       real :: layer_capacity_mm
       real :: active_capacity_mm
       real :: total_capacity_mm
       real :: offered_mm
       real :: accepted_mm
       real :: remaining_mm
       real :: layer_add_mm
       real :: actual_added_mm
       real :: dep

       accepted_m3 = 0.

       if (offered_m3 <= 0.) return
       if (.not. allocated(soil)) return
       if (.not. allocated(hru)) return
       if (ihru < 1 .or. ihru > size(soil)) return
       if (ihru > size(hru)) return
       if (.not. allocated(soil(ihru)%phys)) return
       if (hru(ihru)%area_ha <= 0.) return

       !! Conversion between soil-water depth and volume:
       !! 1 mm over 1 ha equals 10 m3.
       area_conversion = hru(ihru)%area_ha * 10.
       offered_mm = offered_m3 / area_conversion

       total_capacity_mm = 0.
       last_active_layer = 0

       !! First pass: determine total available storage in the upper 1 m.
       do ly = 1, soil(ihru)%nly

         if (soil(ihru)%phys(ly)%thick <= 0.) cycle

         layer_top_mm = soil(ihru)%phys(ly)%d - &
                        soil(ihru)%phys(ly)%thick

         active_thickness_mm = max(0., &
           min(soil(ihru)%phys(ly)%d, seepage_depth_limit_mm) - &
           layer_top_mm)

         if (active_thickness_mm <= 0.) cycle

         active_fraction = active_thickness_mm / &
                           soil(ihru)%phys(ly)%thick

         layer_capacity_mm = max(0., &
           soil(ihru)%phys(ly)%ul - soil(ihru)%phys(ly)%st)

         active_capacity_mm = layer_capacity_mm * active_fraction

         if (active_capacity_mm > 0.) then
           total_capacity_mm = total_capacity_mm + active_capacity_mm
           last_active_layer = ly
         end if

       end do

       if (total_capacity_mm <= 0.) return

       accepted_mm = min(offered_mm, total_capacity_mm)
       remaining_mm = accepted_mm
       actual_added_mm = 0.

       !! Second pass: distribute accepted water proportional to the
       !! available storage of each active layer.
       do ly = 1, soil(ihru)%nly

         if (soil(ihru)%phys(ly)%thick <= 0.) cycle

         layer_top_mm = soil(ihru)%phys(ly)%d - &
                        soil(ihru)%phys(ly)%thick

         active_thickness_mm = max(0., &
           min(soil(ihru)%phys(ly)%d, seepage_depth_limit_mm) - &
           layer_top_mm)

         if (active_thickness_mm <= 0.) cycle

         active_fraction = active_thickness_mm / &
                           soil(ihru)%phys(ly)%thick

         layer_capacity_mm = max(0., &
           soil(ihru)%phys(ly)%ul - soil(ihru)%phys(ly)%st)

         active_capacity_mm = layer_capacity_mm * active_fraction

         if (active_capacity_mm <= 0.) cycle

         if (ly == last_active_layer) then
           !! Assign the floating-point remainder to the last active layer.
           layer_add_mm = min(remaining_mm, active_capacity_mm)
         else
           layer_add_mm = accepted_mm * active_capacity_mm / &
                          total_capacity_mm
           layer_add_mm = min(layer_add_mm, active_capacity_mm)
           layer_add_mm = min(layer_add_mm, remaining_mm)
         end if

         layer_add_mm = max(0., layer_add_mm)

         soil(ihru)%phys(ly)%st = soil(ihru)%phys(ly)%st + &
                                  layer_add_mm

         remaining_mm = max(0., remaining_mm - layer_add_mm)
         actual_added_mm = actual_added_mm + layer_add_mm

       end do

       !! Recalculate total profile soil-water storage.
       soil(ihru)%sw = 0.
       do ly = 1, soil(ihru)%nly
         soil(ihru)%sw = soil(ihru)%sw + soil(ihru)%phys(ly)%st
       end do

       !! Recalculate soil-water storage to 300 mm depth using the
       !! existing SWAT+ calculation.
       soil(ihru)%sw_300 = 0.
       do ly = 1, soil(ihru)%nly

         if (ly == 1) then
           dep = 0.
         else
           dep = soil(ihru)%phys(ly-1)%d
         end if

         if (soil(ihru)%phys(ly)%d >= 300.) then
           soil(ihru)%sw_300 = soil(ihru)%sw_300 + &
             soil(ihru)%phys(ly)%st * &
             (300. - dep) / soil(ihru)%phys(ly)%thick
           exit
         else
           soil(ihru)%sw_300 = soil(ihru)%sw_300 + &
                               soil(ihru)%phys(ly)%st
         end if

       end do

       !! Return the volume actually written to the soil state.
       accepted_m3 = actual_added_mm * area_conversion

       end function store_reservoir_seepage_in_hru

       end module reservoir_seepage_module
