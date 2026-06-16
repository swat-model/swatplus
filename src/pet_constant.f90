subroutine pet_constant (pet_day)

!! ~ ~ ~ PURPOSE ~ ~ ~
!! Assigns a basin-wide constant as potential ET. Used when
!! bsn_cc%pet == 4. The constant is read from parameters.bsn
!! via bsn_prm%pet_const (mm/day).

    use basin_module

    implicit none

    real, intent(out) :: pet_day !! mm/day -- potential ET

    pet_day = max (0., bsn_prm%pet_const)

    return
end subroutine pet_constant