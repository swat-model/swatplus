      subroutine soil_carbvar_write(out_freq)
      !!    ~ ~ ~ PURPOSE ~ ~ ~
      !!    this subroutine writes soil carbon output.
      !!
      !!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
      !!    name          |units         |definition
      !!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      !!    out_freq      |              |output frequency (d=daily, m=monthly, y=yearly, a=average annual)
      !!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

        use basin_module
        use carbon_module
        use hydrograph_module
        use organic_mineral_mass_module
        use calibration_data_module
        use soil_module

        implicit none

        character(len=1), intent(in) :: out_freq   ! Output frequency (d, m, y, a)
        character (len=6) :: freq_label
        integer :: j  ! hydrologic unit number
        integer :: k  ! soil layer 
        integer :: iob  ! soil layer 

        ! print*, "In soil_carbvar_write.f90 ", out_freq
        select case(out_freq)
            case ("d")
            freq_label = "day"
            case ("m")
            freq_label = "mon"
            case ("y")
            freq_label = "year"
            case ("a")
            freq_label = "av_ann"
        end select

        
        ! Carbon variable output file = hru_carbvar.(txt/csv)
        if (bsn_cc%cswat == 2) then
            do j = 1, sp_ob%hru
                iob = sp_ob1%hru + j - 1
                do k = 1, soil(j)%nly
                    write (4574,*) freq_label, k, int(soil(j)%phys(k)%d), time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                                    soil1(j)%org_con_lr(k)%sut, soil1(j)%org_con_lr(k)%till_eff, soil1(j)%org_con_lr(k)%cdg, &
                                    soil1(j)%org_con_lr(k)%ox, soil1(j)%org_con_lr(k)%cs, soil1(j)%org_con_lr(k)%no3, &
                                    soil1(j)%org_con_lr(k)%nh4, soil1(j)%org_con_lr(k)%resp, soil(j)%phys(k)%tmp 
                    if (pco%csvout == "y") then
                    write (4575,'(*(G0.7,:,","))') freq_label, k, int(soil(j)%phys(k)%d), time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, &
                                    soil1(j)%org_con_lr(k)%sut, soil1(j)%org_con_lr(k)%till_eff, soil1(j)%org_con_lr(k)%cdg, &
                                    soil1(j)%org_con_lr(k)%ox, soil1(j)%org_con_lr(k)%cs, soil1(j)%org_con_lr(k)%no3, &
                                    soil1(j)%org_con_lr(k)%nh4, soil1(j)%org_con_lr(k)%resp, soil(j)%phys(k)%tmp
                    end if
                end do
            end do
        end if

        ! Carbon organinic allocation variable output file = hru_org_allo_vars.(txt/csv)
        if (bsn_cc%cswat == 2) then
            do j = 1, sp_ob%hru
                iob = sp_ob1%hru + j - 1
                do k = 1, soil(j)%nly
                    write (4576,*) freq_label, k, int(soil(j)%phys(k)%d), time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, soil1(j)%org_allo_lr(k)
                    if (pco%csvout == "y") then
                    write (4577,'(*(G0.7,:,","))') freq_label, k, int(soil(j)%phys(k)%d), time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, soil1(j)%org_allo_lr(k)
                    end if
                end do
            end do
        end if

        ! Carbon organinic ratio variable output file = hru_org_ratio_vars.(txt/csv)
        if (bsn_cc%cswat == 2) then
            do j = 1, sp_ob%hru
                iob = sp_ob1%hru + j - 1
                do k = 1, soil(j)%nly
                    write (4578,*) freq_label, k, int(soil(j)%phys(k)%d), time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, soil1(j)%org_ratio_lr(k)
                    if (pco%csvout == "y") then
                    write (4579,'(*(G0.7,:,","))') freq_label, k, int(soil(j)%phys(k)%d), time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, soil1(j)%org_ratio_lr(k)
                    end if
                end do
            end do
        end if

        ! Potential organinic transformation values file = hru_org_tran_vars.(txt/csv)
        if (bsn_cc%cswat == 2) then
            do j = 1, sp_ob%hru
                iob = sp_ob1%hru + j - 1
                do k = 1, soil(j)%nly
                    write (4580,*) freq_label, k, int(soil(j)%phys(k)%d), time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, soil1(j)%org_tran_lr(k)
                    if (pco%csvout == "y") then
                    write (4581,'(*(G0.7,:,","))') freq_label, k, int(soil(j)%phys(k)%d), time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, soil1(j)%org_tran_lr(k)
                    end if
                end do
            end do
        end if
      return
      end subroutine soil_carbvar_write