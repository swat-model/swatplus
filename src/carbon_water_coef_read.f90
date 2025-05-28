subroutine carbon_water_coef_read
  ! read carbon water coefficients from file carbon_water_coef.cbn
  ! 
    use carbon_module
    use basin_module
    
    implicit none

    integer :: eof = 0                !           |end of file
    logical :: i_exist = .false.      !           |true if file exists
    character (len=80) :: titldum = ""!           |title of file
    character (len=80) :: header = "" !           |header of file
    
    
    if (bsn_cc%cswat == 2) then
        inquire (file='carbon_water_coef.cbn', exist=i_exist)
        if (i_exist) then
          open (107,file='carbon_water_coef.cbn',recl = 1500)
          read (107,*,iostat=eof) titldum
          read (107,*,iostat=eof) header
          read (107,*,iostat=eof) cb_wtr_coef
          close (107)
        endif
    endif
    return
end subroutine                         