subroutine carbon_coef_read

    ! Purpose: Read in variables for calibration purposes.

    use carbon_module
    use basin_module
    use tillage_data_module 
    use soil_module
    use organic_mineral_mass_module
    
    implicit none

    integer :: eof = 0                !           |end of file
    integer :: soil_test_cntr  = 0    !           |counter for soil test, cannot exceed nmbr_soil_tests 
    logical :: i_exist = .false.      !           |true if file exists
    character (len=80) :: titldum = ""!           |title of file
    character (len=80) :: header = "" !           |header of file
    character (len=24) :: var_name = "" !
    
    nmbr_soil_tests = 0     ! comes from soil module
    soil_test_cntr  = 0     ! local variable
    ! soil_phys_test_cntr  = 0

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
                case("meta_frac")
                    backspace (107)
                    read (107,*,iostat=eof) var_name, meta_frac 
                case("str_frac")
                    backspace (107)
                    read (107,*,iostat=eof) var_name, str_frac 
                case("lig_frac")
                    backspace (107)
                    read (107,*,iostat=eof) var_name, lig_frac 
                case("nmbr_soil_tests")
                    backspace (107)
                    read (107,*,iostat=eof) var_name, nmbr_soil_tests
                    allocate(sol_test(nmbr_soil_tests))
                case("soil_test")
                    soil_test_cntr = soil_test_cntr + 1
                    if (nmbr_soil_tests == 0) then
                        write(*, fmt="(a)", advance="yes") "Error: The number of soil tests (nmbr_soil_tests) has not been specified"
                        write(*, fmt="(a)", advance="yes") "       in the input file carb_coefs.cbn."
                        write(*, fmt="(a)")                "       The soil_test values cannot be processed."
                        print*
                        error stop
                    endif
                    if (soil_test_cntr > nmbr_soil_tests) then
                        write(*, fmt="(a,i3,a)", advance="yes") "Error: The number of soil_test exceeds the input nmbr_soil_tests ", nmbr_soil_tests
                        write(*, fmt="(a)", advance="yes")      "       in the input file carb_coefs.cbn."
                        write(*, fmt="(a)")                     "       The soil_test values cannot be processed."
                        print*
                        error stop
                    endif
                    backspace (107)
                    read (107,*,iostat=eof) var_name, sol_test(soil_test_cntr)%snam,    &
                                                      sol_test(soil_test_cntr)%d,       &
                                                      sol_test(soil_test_cntr)%bd,      &
                                                      sol_test(soil_test_cntr)%cbn,     &
                                                      sol_test(soil_test_cntr)%sand,    &
                                                      sol_test(soil_test_cntr)%silt,    &
                                                      sol_test(soil_test_cntr)%clay   
                case default
                    write(*, fmt="(a,a,a)", advance="yes") "Error: The variable ", var_name, "in the input file carb_coefs.cbn is not a recognized variable."
                    write(*, fmt="(a)")                    "       and cannot be processed."
                    print*
                    error stop
              end select
            enddo
            carbon_coef_file = .true.
            close (107)
            exit
          enddo
          if (nmbr_soil_tests > 0 .and. nmbr_soil_tests /= soil_test_cntr ) then
            write(*, fmt="(a,I3,a,i3)", advance="yes")   "Error: The nmbr_soil_tests of",  nmbr_soil_tests, " does not match the input number of soil_tests of", soil_test_cntr
            write(*, fmt="(a)", advance="yes")           "       in the input file carb_coefs.soil."
            write(*, fmt="(a)")                          "       The soil_test values cannot be processed."
            print*
            error stop
          endif
        endif
    endif
    return
end subroutine                         