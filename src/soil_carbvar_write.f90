      subroutine soil_carbvar_write(out_freq)
      !! writes per-layer carbon driver variables (hru_carb_drv) and
      !! per-layer carbon pool dynamics (hru_carb_dyn) in wide-per-layer format,
      !! one row per HRU per timestep, with first cb_n_layers depth columns
      !! followed by each variable's per-layer values.

        use basin_module
        use carbon_module
        use hydrograph_module
        use organic_mineral_mass_module
        use calibration_data_module
        use soil_module
        use time_module

        implicit none

        character(len=2), intent(in) :: out_freq   !! d / m / y / a
        integer :: j, k, iob, n_use
        integer :: u_drv_txt, u_drv_csv, u_dyn_txt, u_dyn_csv
        real :: buf(cb_n_layers)
        character(len=1) :: drv_gate, dyn_gate

        select case(out_freq)
          case (" d")
            u_drv_txt = 4582; u_drv_csv = 4586; u_dyn_txt = 4590; u_dyn_csv = 4594
            drv_gate = pco%cb_drv_hru%d; dyn_gate = pco%cb_dyn_hru%d
          case (" m")
            u_drv_txt = 4583; u_drv_csv = 4587; u_dyn_txt = 4591; u_dyn_csv = 4595
            drv_gate = pco%cb_drv_hru%m; dyn_gate = pco%cb_dyn_hru%m
          case (" y")
            u_drv_txt = 4584; u_drv_csv = 4588; u_dyn_txt = 4592; u_dyn_csv = 4596
            drv_gate = pco%cb_drv_hru%y; dyn_gate = pco%cb_dyn_hru%y
          case (" a")
            u_drv_txt = 4585; u_drv_csv = 4589; u_dyn_txt = 4593; u_dyn_csv = 4597
            drv_gate = pco%cb_drv_hru%a; dyn_gate = pco%cb_dyn_hru%a
          case default; return
        end select

        if (drv_gate /= "y" .and. dyn_gate /= "y") return

        do j = 1, sp_ob%hru
          iob = sp_ob1%hru + j - 1
          n_use = soil(j)%nly

          if (drv_gate == "y") then
            call cv_emit_row_id_txt(u_drv_txt, j, iob)
            buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = real(soil(j)%phys(k)%d); end do
            call cb_write_depth_row(u_drv_txt, buf, n_use, .false., "no")
            call cv_drv_blocks(u_drv_txt, .false., j, n_use, buf)
            if (pco%csvout == "y") then
              call cv_emit_row_id_csv(u_drv_csv, j, iob)
              buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = real(soil(j)%phys(k)%d); end do
              call cb_write_depth_row(u_drv_csv, buf, n_use, .true., "no")
              call cv_drv_blocks(u_drv_csv, .true., j, n_use, buf)
            end if
          end if

          if (dyn_gate == "y") then
            call cv_emit_row_id_txt(u_dyn_txt, j, iob)
            buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = real(soil(j)%phys(k)%d); end do
            call cb_write_depth_row(u_dyn_txt, buf, n_use, .false., "no")
            call cv_dyn_blocks(u_dyn_txt, .false., j, n_use, buf)
            if (pco%csvout == "y") then
              call cv_emit_row_id_csv(u_dyn_csv, j, iob)
              buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = real(soil(j)%phys(k)%d); end do
              call cb_write_depth_row(u_dyn_csv, buf, n_use, .true., "no")
              call cv_dyn_blocks(u_dyn_csv, .true., j, n_use, buf)
            end if
          end if
        end do

      return

      contains

        subroutine cv_emit_row_id_txt(unit_no, hru_j, hru_iob)
          integer, intent(in) :: unit_no, hru_j, hru_iob
          write (unit_no, '(5i12,2x,i12,2x,a16)', advance='no') &
            time%day, time%mo, time%day_mo, time%yrc, hru_j, ob(hru_iob)%gis_id, ob(hru_iob)%name
        end subroutine cv_emit_row_id_txt

        subroutine cv_emit_row_id_csv(unit_no, hru_j, hru_iob)
          integer, intent(in) :: unit_no, hru_j, hru_iob
          write (unit_no, '(5(i0,a),i0,a,a)', advance='no') &
            time%day, ",", time%mo, ",", time%day_mo, ",", time%yrc, ",", hru_j, ",", &
            ob(hru_iob)%gis_id, ",", trim(ob(hru_iob)%name)
        end subroutine cv_emit_row_id_csv

        subroutine cv_drv_blocks(u, is_csv, j_in, n_use_in, buf_in)
          integer, intent(in) :: u, j_in, n_use_in
          logical, intent(in) :: is_csv
          real, intent(inout) :: buf_in(:)
          integer :: kk
          buf_in = 0.0; do kk = 1, min(cb_n_layers, n_use_in); buf_in(kk) = soil1(j_in)%org_con_lr(kk)%sut; end do
          call cb_write_var_block(u, buf_in, n_use_in, is_csv, "no")
          buf_in = 0.0; do kk = 1, min(cb_n_layers, n_use_in); buf_in(kk) = soil(j_in)%ly(kk)%tillagef; end do
          call cb_write_var_block(u, buf_in, n_use_in, is_csv, "no")
          buf_in = 0.0; do kk = 1, min(cb_n_layers, n_use_in); buf_in(kk) = soil(j_in)%ly(kk)%bmix; end do
          call cb_write_var_block(u, buf_in, n_use_in, is_csv, "no")
          buf_in = 0.0; do kk = 1, min(cb_n_layers, n_use_in); buf_in(kk) = soil(j_in)%ly(kk)%tillagef_biomix; end do
          call cb_write_var_block(u, buf_in, n_use_in, is_csv, "no")
          buf_in = 0.0; do kk = 1, min(cb_n_layers, n_use_in); buf_in(kk) = soil(j_in)%ly(kk)%tillagef_tillmix; end do
          call cb_write_var_block(u, buf_in, n_use_in, is_csv, "no")
          buf_in = 0.0; do kk = 1, min(cb_n_layers, n_use_in); buf_in(kk) = soil1(j_in)%org_con_lr(kk)%till_eff; end do
          call cb_write_var_block(u, buf_in, n_use_in, is_csv, "no")
          buf_in = 0.0; do kk = 1, min(cb_n_layers, n_use_in); buf_in(kk) = soil1(j_in)%org_con_lr(kk)%cdg; end do
          call cb_write_var_block(u, buf_in, n_use_in, is_csv, "no")
          buf_in = 0.0; do kk = 1, min(cb_n_layers, n_use_in); buf_in(kk) = soil1(j_in)%org_con_lr(kk)%ox; end do
          call cb_write_var_block(u, buf_in, n_use_in, is_csv, "no")
          buf_in = 0.0; do kk = 1, min(cb_n_layers, n_use_in); buf_in(kk) = soil1(j_in)%org_con_lr(kk)%cs; end do
          call cb_write_var_block(u, buf_in, n_use_in, is_csv, "no")
          buf_in = 0.0; do kk = 1, min(cb_n_layers, n_use_in); buf_in(kk) = soil1(j_in)%org_con_lr(kk)%no3; end do
          call cb_write_var_block(u, buf_in, n_use_in, is_csv, "no")
          buf_in = 0.0; do kk = 1, min(cb_n_layers, n_use_in); buf_in(kk) = soil1(j_in)%org_con_lr(kk)%nh4; end do
          call cb_write_var_block(u, buf_in, n_use_in, is_csv, "no")
          buf_in = 0.0; do kk = 1, min(cb_n_layers, n_use_in); buf_in(kk) = soil1(j_in)%org_con_lr(kk)%resp; end do
          call cb_write_var_block(u, buf_in, n_use_in, is_csv, "no")
          buf_in = 0.0; do kk = 1, min(cb_n_layers, n_use_in); buf_in(kk) = soil(j_in)%phys(kk)%tmp; end do
          call cb_write_var_block(u, buf_in, n_use_in, is_csv, "no")
          buf_in = 0.0; do kk = 1, min(cb_n_layers, n_use_in); buf_in(kk) = soil1(j_in)%emix(kk); end do
          call cb_write_var_block(u, buf_in, n_use_in, is_csv, "yes")
        end subroutine cv_drv_blocks

        subroutine cv_dyn_blocks(u, is_csv, j_in, n_use_in, buf_in)
          integer, intent(in) :: u, j_in, n_use_in
          logical, intent(in) :: is_csv
          real, intent(inout) :: buf_in(:)
          integer :: kk
          !! allocations (6)
          buf_in = 0.0; do kk = 1, min(cb_n_layers, n_use_in); buf_in(kk) = soil1(j_in)%org_allo_lr(kk)%asp; end do
          call cb_write_var_block(u, buf_in, n_use_in, is_csv, "no")
          buf_in = 0.0; do kk = 1, min(cb_n_layers, n_use_in); buf_in(kk) = soil1(j_in)%org_allo_lr(kk)%abp; end do
          call cb_write_var_block(u, buf_in, n_use_in, is_csv, "no")
          buf_in = 0.0; do kk = 1, min(cb_n_layers, n_use_in); buf_in(kk) = soil1(j_in)%org_allo_lr(kk)%abco2; end do
          call cb_write_var_block(u, buf_in, n_use_in, is_csv, "no")
          buf_in = 0.0; do kk = 1, min(cb_n_layers, n_use_in); buf_in(kk) = soil1(j_in)%org_allo_lr(kk)%a1co2; end do
          call cb_write_var_block(u, buf_in, n_use_in, is_csv, "no")
          buf_in = 0.0; do kk = 1, min(cb_n_layers, n_use_in); buf_in(kk) = soil1(j_in)%org_allo_lr(kk)%asco2; end do
          call cb_write_var_block(u, buf_in, n_use_in, is_csv, "no")
          buf_in = 0.0; do kk = 1, min(cb_n_layers, n_use_in); buf_in(kk) = soil1(j_in)%org_allo_lr(kk)%apco2; end do
          call cb_write_var_block(u, buf_in, n_use_in, is_csv, "no")
          !! N:C ratios (3)
          buf_in = 0.0; do kk = 1, min(cb_n_layers, n_use_in); buf_in(kk) = soil1(j_in)%org_ratio_lr(kk)%ncbm; end do
          call cb_write_var_block(u, buf_in, n_use_in, is_csv, "no")
          buf_in = 0.0; do kk = 1, min(cb_n_layers, n_use_in); buf_in(kk) = soil1(j_in)%org_ratio_lr(kk)%nchp; end do
          call cb_write_var_block(u, buf_in, n_use_in, is_csv, "no")
          buf_in = 0.0; do kk = 1, min(cb_n_layers, n_use_in); buf_in(kk) = soil1(j_in)%org_ratio_lr(kk)%nchs; end do
          call cb_write_var_block(u, buf_in, n_use_in, is_csv, "no")
          !! transformations (12)
          buf_in = 0.0; do kk = 1, min(cb_n_layers, n_use_in); buf_in(kk) = soil1(j_in)%org_tran_lr(kk)%bmctp; end do
          call cb_write_var_block(u, buf_in, n_use_in, is_csv, "no")
          buf_in = 0.0; do kk = 1, min(cb_n_layers, n_use_in); buf_in(kk) = soil1(j_in)%org_tran_lr(kk)%bmntp; end do
          call cb_write_var_block(u, buf_in, n_use_in, is_csv, "no")
          buf_in = 0.0; do kk = 1, min(cb_n_layers, n_use_in); buf_in(kk) = soil1(j_in)%org_tran_lr(kk)%hsctp; end do
          call cb_write_var_block(u, buf_in, n_use_in, is_csv, "no")
          buf_in = 0.0; do kk = 1, min(cb_n_layers, n_use_in); buf_in(kk) = soil1(j_in)%org_tran_lr(kk)%hsntp; end do
          call cb_write_var_block(u, buf_in, n_use_in, is_csv, "no")
          buf_in = 0.0; do kk = 1, min(cb_n_layers, n_use_in); buf_in(kk) = soil1(j_in)%org_tran_lr(kk)%hpctp; end do
          call cb_write_var_block(u, buf_in, n_use_in, is_csv, "no")
          buf_in = 0.0; do kk = 1, min(cb_n_layers, n_use_in); buf_in(kk) = soil1(j_in)%org_tran_lr(kk)%hpntp; end do
          call cb_write_var_block(u, buf_in, n_use_in, is_csv, "no")
          buf_in = 0.0; do kk = 1, min(cb_n_layers, n_use_in); buf_in(kk) = soil1(j_in)%org_tran_lr(kk)%lmctp; end do
          call cb_write_var_block(u, buf_in, n_use_in, is_csv, "no")
          buf_in = 0.0; do kk = 1, min(cb_n_layers, n_use_in); buf_in(kk) = soil1(j_in)%org_tran_lr(kk)%lmntp; end do
          call cb_write_var_block(u, buf_in, n_use_in, is_csv, "no")
          buf_in = 0.0; do kk = 1, min(cb_n_layers, n_use_in); buf_in(kk) = soil1(j_in)%org_tran_lr(kk)%lsctp; end do
          call cb_write_var_block(u, buf_in, n_use_in, is_csv, "no")
          buf_in = 0.0; do kk = 1, min(cb_n_layers, n_use_in); buf_in(kk) = soil1(j_in)%org_tran_lr(kk)%lslctp; end do
          call cb_write_var_block(u, buf_in, n_use_in, is_csv, "no")
          buf_in = 0.0; do kk = 1, min(cb_n_layers, n_use_in); buf_in(kk) = soil1(j_in)%org_tran_lr(kk)%lslnctp; end do
          call cb_write_var_block(u, buf_in, n_use_in, is_csv, "no")
          buf_in = 0.0; do kk = 1, min(cb_n_layers, n_use_in); buf_in(kk) = soil1(j_in)%org_tran_lr(kk)%lsntp; end do
          call cb_write_var_block(u, buf_in, n_use_in, is_csv, "yes")
        end subroutine cv_dyn_blocks

      end subroutine soil_carbvar_write
