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
          read (107,*,iostat=eof) carbdb(1)%hp_rate, carbdb(1)%hs_rate, carbdb(1)%microb_rate, &
                carbdb(1)%meta_rate, carbdb(1)%str_rate, carbdb(1)%microb_top_rate, carbdb(1)%hs_hp, &
                org_allo(1)%a1co2, org_allo(1)%asco2, org_allo(1)%apco2 
          read (107,*,iostat=eof) carbdb(2)%hp_rate, carbdb(2)%hs_rate, carbdb(2)%microb_rate, &
                carbdb(2)%meta_rate, carbdb(2)%str_rate, carbdb(2)%microb_top_rate, carbdb(2)%hs_hp, &
                org_allo(2)%a1co2, org_allo(2)%asco2, org_allo(2)%apco2 
          close (107)
          carbon_coef_file = .true.
        endif
    endif
    return
end subroutine                         