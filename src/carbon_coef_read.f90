subroutine carbon_coef_read

    use carbon_module
    use basin_module
    
    implicit none

    integer :: eof = 0                !           |end of file
    logical :: i_exist = .false.      !           |true if file exists
    character (len=80) :: titldum = ""!           |title of file
    character (len=80) :: header = "" !           |header of file
    
    
    if (bsn_cc%cswat == 2) then
        inquire (file='carbon_coef.cbn', exist=i_exist)
        if (i_exist) then
          open (107,file='carbon_coef.cbn',recl = 1500)
          read (107,*,iostat=eof) titldum
          read (107,*,iostat=eof) header
          read (107,*,iostat=eof) carbdb%hp_rate, carbdb%hs_rate, carbdb%microb_rate, &
                carbdb%meta_rate, carbdb%str_rate, carbdb%microb_top_rate, carbdb%hs_hp
          carbdb%carbon_coef_file = .true.
        endif
    endif
    return
end subroutine                         