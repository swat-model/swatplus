subroutine carbon_coef_read

    use carbon_module
    use basin_module
    use tillage_data_module 
    
    implicit none

    integer :: eof = 0                !           |end of file
    logical :: i_exist = .false.      !           |true if file exists
    character (len=80) :: titldum = ""!           |title of file
    character (len=80) :: header = "" !           |header of file
    character (len=16) :: var_name = "" !           
    
    ! if (bsn_cc%cswat == 2) then
    !     inquire (file='carbon_coef.cbn', exist=i_exist)
    !     if (i_exist) then
    !       open (107,file='carbon_coef.cbn',recl = 1500)
    !       read (107,*,iostat=eof) titldum
    !       read (107,*,iostat=eof) header
    !       read (107,*,iostat=eof) carbdb(1)%hp_rate, carbdb(1)%hs_rate, carbdb(1)%microb_rate, &
    !             carbdb(1)%meta_rate, carbdb(1)%str_rate, carbdb(1)%microb_top_rate, carbdb(1)%hs_hp, &
    !             org_allo(1)%a1co2, org_allo(1)%asco2, org_allo(1)%apco2, org_allo(1)%abco2  
    !       read (107,*,iostat=eof) carbdb(2)%hp_rate, carbdb(2)%hs_rate, carbdb(2)%microb_rate, &
    !             carbdb(2)%meta_rate, carbdb(2)%str_rate, carbdb(2)%microb_top_rate, carbdb(2)%hs_hp, &
    !             org_allo(2)%a1co2, org_allo(2)%asco2, org_allo(2)%apco2, org_allo(2)%abco2 
    !       close (107)
    !       carbon_coef_file = .true.
    !     endif
    ! endif

    if (bsn_cc%cswat == 2) then
        inquire (file='carb_coefs.cbn', exist=i_exist)
        if (i_exist) then
          open (107,file='carb_coefs.cbn', iostat=eof)
          do
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            do while (eof >= 0)
              read (107,*,iostat=eof) var_name
              if (eof < 0) exit
              if (var_name(1:1) == "#") cycle  ! allow comment lines in file that begin with #
              select case(var_name) 
                case("hp_rate")
                    backspace (107)
                    read (107,*,iostat=eof) var_name, carbdb(1)%hp_rate,carbdb(2)%hp_rate
                case("hs_rate")
                    backspace (107)
                    read (107,*,iostat=eof) var_name, carbdb(1)%hs_rate,carbdb(2)%hs_rate
                case("microb_rate")
                    backspace (107)
                    read (107,*,iostat=eof) var_name, carbdb(1)%microb_rate,carbdb(2)%microb_rate
                case("meta_rate")
                    backspace (107)
                    read (107,*,iostat=eof) var_name, carbdb(1)%meta_rate,carbdb(2)%meta_rate
                case("str_rate")
                    backspace (107)
                    read (107,*,iostat=eof) var_name, carbdb(1)%str_rate,carbdb(2)%str_rate
                case("microb_top_rate")
                    backspace (107)
                    read (107,*,iostat=eof) var_name, carbdb(1)%microb_top_rate,carbdb(2)%microb_top_rate
                case("hs_hp")
                    backspace (107)
                    read (107,*,iostat=eof) var_name, carbdb(1)%hs_hp,carbdb(2)%hs_hp
                case("a1co2")
                    backspace (107)
                    read (107,*,iostat=eof) var_name, org_allo(1)%a1co2,org_allo(2)%a1co2
                case("asco2")
                    backspace (107)
                    read (107,*,iostat=eof) var_name, org_allo(1)%asco2,org_allo(2)%asco2
                case("apco2")
                    backspace (107)
                    read (107,*,iostat=eof) var_name, org_allo(1)%apco2,org_allo(2)%apco2
                case("abco2")
                    backspace (107)
                    read (107,*,iostat=eof) var_name, org_allo(1)%abco2,org_allo(2)%abco2
                case("prmt_21")
                    backspace (107)
                    read (107,*,iostat=eof) var_name, cb_wtr_coef%prmt_21
                case("prmt_44")
                    backspace (107)
                    read (107,*,iostat=eof) var_name, cb_wtr_coef%prmt_44
                case("till_eff_days")
                    backspace (107)
                    read (107,*,iostat=eof) var_name, till_eff_days
                case("rtof")
                    backspace (107)
                    read (107,*,iostat=eof) var_name, man_coef%rtof
                case("man_to_c")
                    backspace (107)
                    read (107,*,iostat=eof) var_name, man_coef%man_to_c
                case default
                    write(*, fmt="(a,a,a)", advance="yes") "Error: The variable ", var_name, "in the input file carb_coefs.cbn is not a recognized variable."
                    write(*, fmt="(a)") "       and cannot be processed."
                    print*
                    error stop
              end select
            enddo
            carbon_coef_file = .true.
            close (107)
            exit
          enddo
        endif
    endif
    return
end subroutine                         