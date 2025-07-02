subroutine carbon_coef_read

    use carbon_module
    use basin_module
    use tillage_data_module 
    use soil_module
    
    implicit none

    integer :: eof = 0                !           |end of file
    integer :: cbn_test_cntr  = 0     !           |counter for carbon test, cannot exceed nmbr_cbn_tests 
    logical :: i_exist = .false.      !           |true if file exists
    character (len=80) :: titldum = ""!           |title of file
    character (len=80) :: header = "" !           |header of file
    character (len=24) :: var_name = "" !
    
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

    nmbr_cbn_tests = 0
    cbn_test_cntr  = 0

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
                case("nmbr_cbn_tests")
                    backspace (107)
                    read (107,*,iostat=eof) var_name, nmbr_cbn_tests
                    allocate(sol_cbn_test(nmbr_cbn_tests))
                case("cbn_test")
                    cbn_test_cntr = cbn_test_cntr + 1
                    if (nmbr_cbn_tests == 0) then
                        write(*, fmt="(a)", advance="yes") "Error: The number of carbon tests (nmbr_cbn_tests) has not been specified in the input file carb_coefs.cbn."
                        write(*, fmt="(a)")                "       The cbn_test cannot be processed."
                        print*
                        error stop
                    endif
                    if (cbn_test_cntr > nmbr_cbn_tests) then
                        write(*, fmt="(a,i3,a)", advance="yes") "Error: The number of carbon tests exceeds the input nmbr_cbn_tests ", nmbr_cbn_tests, " in the input file carb_coefs.cbn."
                        write(*, fmt="(a)")                     "       The cbn_test cannot be processed."
                        print*
                        error stop
                    endif
                    backspace (107)
                    read (107,*,iostat=eof) var_name, sol_cbn_test(cbn_test_cntr)%snam,  &
                                                      sol_cbn_test(cbn_test_cntr)%d,     &
                                                      sol_cbn_test(cbn_test_cntr)%cbn
                case default
                    write(*, fmt="(a,a,a)", advance="yes") "Error: The variable ", var_name, "in the input file carb_coefs.cbn is not a recognized variable."
                    write(*, fmt="(a)")                    "       and cannot be processed."
                    print*
                    error stop
              end select
            enddo
            ! if (sol_cbn_test%d > 0.000001 .and. sol_cbn_test%cbn >= 0.000001 ) print*, "call soil_cbn_adjust"
            carbon_coef_file = .true.
            close (107)
            exit
          enddo
          if (nmbr_cbn_tests > 0 .and. nmbr_cbn_tests /= cbn_test_cntr ) then
            write(*, fmt="(a,I3,a,i3)", advance="yes")   "Error: The number of carbon tests",  nmbr_cbn_tests, " does not match the number of cbn_tests of", cbn_test_cntr
            write(*, fmt="(a,I3,a,i3,a)", advance="yes") "       in the input file carb_coefs.cbn."
            write(*, fmt="(a)")                          "       The cbn_test cannot be processed."
            print*
            error stop
          endif
        endif
    endif
    return
end subroutine                         