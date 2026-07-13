      subroutine soil_nutcarb_write(out_freq)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes soil carbon output.
!!
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    out_freq      |              |output frequency (d=daily, m=monthly, y=yearly, a=average annual)

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use soil_module
      use organic_mineral_mass_module
      use hydrograph_module
      use calibration_data_module
      use carbon_module
      use basin_module
      use plant_module
      
      implicit none
      
      character(len=2), intent(in) :: out_freq   ! Output frequency (d, m, y, a)
      integer :: ly = 0         !none        |counter
      real :: const = 0.        !none        |counter
      real :: tot_lyr_n
      real :: tot_lyr_p
      real :: tot_prof_n
      real :: tot_prof_p
      real :: prf_swc = 0.0     !mm/mm       |average profile soil water content including wilting point moisture content.
      real :: prf_depth = 0.0   !mm          |depth of soil profile.
      real :: frac_above_300 = 1.0
      real :: soil_prof_lig_n
      real :: soil_prof_nonlig_n
      real :: soil_prof_lig_p
      real :: soil_prof_nonlig_p
      integer :: iihru = 0      !none        |counter
      integer :: j = 0          !none        |counter
      integer :: ipl = 0        !none        |counter
      integer :: iob = 0
      integer :: profile_depth
      character (len=7) :: freq_label
      logical :: layer_output
      real :: root_frac_ly = 0.

      layer_output = .false.

      select case(out_freq)
        case (" d")
          freq_label = "day"
        case ("dl")
          freq_label = "day"
          layer_output = .true.
        case (" m")
          freq_label = "mon"
        case ("ml")
          freq_label = "mon"
          layer_output = .true.
        case (" y")
          freq_label = "year"
        case ("yl")
          freq_label = "year"
          layer_output = .true.
        case (" a")
          freq_label = "av_ann"
        case ("al")
          freq_label = "av_ann"
          layer_output = .true.
        case (" b")
          freq_label = "begsim"
        case (" e")
          freq_label = "endsim"
      end select
           
      !! basin output - zero daily basin outputs before summing
      bsn_org_soil = soil_org_z
      bsn_org_pl = soil_org_z
      bsn_org_rsd = soil_org_z

      if (freq_label == "begsim" .or. freq_label == "endsim" ) then
        !! hru_soil_snap_tot gated by pco%cb_snap_hru%a.
        !! BOTH stages write soil1(j)%tot(ly)%c (kg C / ha) for comparability.
        if (pco%cb_snap_hru%a == "y") then
          do j = 1, sp_ob%hru
            iob = sp_ob1%hru + j - 1
            call cb_soil_snap_emit(4601, 4605, freq_label, j, iob)
          enddo
        endif
        return
      else
        !! sum the output for the entire soil profile
        do j = 1, sp_ob%hru
          iob = sp_ob1%hru + j - 1
          soil1(j)%tot_org = soil_org_z
          soil1(j)%seq_org = soil_org_z
          soil1(j)%surf_org = soil_org_z
          soil_prof_hact = soil_org_z
          soil_prof_hsta = soil_org_z
          soil_prof_rsd = soil_org_z
          soil_prof_root = soil_org_z
          soil_prof_root_frac = 0.0
          soil_prof_str = soil_org_z
          soil_prof_lig = soil_org_z
          soil_prof_nonlig = soil_org_z
          soil_prof_meta = soil_org_z
          soil_prof_man = soil_org_z
          soil_prof_seq_hs = soil_org_z
          soil_prof_seq_hp = soil_org_z
          soil_prof_seq_microb = soil_org_z
          soil_prof_hs = soil_org_z
          soil_prof_hp = soil_org_z
          soil_prof_microb = soil_org_z
          soil_prof_water = soil_org_z
          soil_prof_lig_n = 0.
          soil_prof_nonlig_n = 0.
          soil_prof_lig_p = 0.
          soil_prof_nonlig_p = 0.
          do ly = 1, soil(j)%nly
            soil_prof_seq_hs = soil_prof_seq_hs + soil1(j)%hs(ly)
            soil_prof_seq_hp = soil_prof_seq_hp + soil1(j)%hp(ly)
            soil_prof_seq_microb = soil_prof_seq_microb + soil1(j)%microb(ly)
            soil1(j)%rsd_tot(ly) = orgz
            soil1(j)%root_tot(ly) = orgz
            do ipl = 1, pcom(j)%npl
              soil1(j)%rsd_tot(ly) = soil1(j)%rsd_tot(ly) + soil1(j)%pl(ipl)%rsd(ly)
              soil1(j)%root_tot(ly)%m = soil1(j)%root_tot(ly)%m + pcom(j)%plg(ipl)%rtfr(ly) * pl_mass(j)%root(ipl)%m 
            end do
            root_frac_ly = 0.
            if (pcom(j)%npl > 0) then
              do ipl = 1, pcom(j)%npl
                root_frac_ly = root_frac_ly + pcom(j)%plg(ipl)%rtfr(ly)
              end do
              root_frac_ly = root_frac_ly / real(pcom(j)%npl)
            end if
            soil_prof_rsd = soil_prof_rsd + soil1(j)%rsd_tot(ly)
            soil_prof_root = soil_prof_root + soil1(j)%root_tot(ly)
            soil_prof_root_frac = soil_prof_root_frac + root_frac_ly
            soil_prof_str = soil_prof_str + soil1(j)%str(ly)
            soil_prof_hact = soil_prof_hact + soil1(j)%hact(ly)
            soil_prof_hsta = soil_prof_hsta + soil1(j)%hsta(ly)
            soil_prof_man = soil_prof_man + soil1(j)%man(ly)
            soil_prof_hs = soil_prof_hs + soil1(j)%hs(ly)
            soil_prof_hp = soil_prof_hp + soil1(j)%hp(ly)
            soil_prof_meta = soil_prof_meta + soil1(j)%meta(ly)
            soil_prof_lig = soil_prof_lig +  soil1(j)%lig(ly)
            soil_prof_nonlig = soil_prof_nonlig +  soil1(j)%nonlig(ly)
            soil_prof_lig_n = soil_prof_lig_n + soil1(j)%lig(ly)%n
            soil_prof_nonlig_n = soil_prof_nonlig_n +  soil1(j)%nonlig(ly)%n
            soil_prof_lig_p = soil_prof_lig_p +  soil1(j)%lig(ly)%p
            soil_prof_nonlig_p = soil_prof_nonlig_p +  soil1(j)%nonlig(ly)%p
            soil_prof_microb = soil_prof_microb + soil1(j)%microb(ly)
            soil_prof_water = soil_prof_water + soil1(j)%water(ly)
          end do

          ! Total org all layers
          soil1(j)%tot_org = soil_prof_hs + soil_prof_hp + soil_prof_microb + soil_prof_meta &
                            + soil_prof_str + soil_prof_water + soil_prof_man

          ! Sequestered org for soil layers greater than 1
          soil1(j)%seq_org = soil_prof_seq_hs + soil_prof_seq_hp + soil_prof_seq_microb
          
          ! Surface org, just layer 1 excluding residue
          soil1(j)%surf_org = soil1(j)%meta(1) + soil1(j)%str(1) + soil1(j)%microb(1) &
                              + soil1(j)%hs(1) + soil1(j)%man(1) + soil1(j)%water(1)
          
          !write total carbon by soil layer, file = "hru_cbn_lyr.txt"
          ! print header with soil layer depths
          ! calc total C above or equal to 300mm
          soil1(j)%tot_300_c = 0.0
          do ly = 1, soil(j)%nly
            if (soil(j)%phys(ly)%d <= 300.0) then
              frac_above_300 = 1.0
            else if (ly == 1) then
              frac_above_300 = 300.0 / soil(j)%phys(ly)%d 
            else if (soil(j)%phys(ly-1)%d < 300.0) then
              frac_above_300 = (300.0 -  soil(j)%phys(ly-1)%d)/ (soil(j)%phys(ly)%d - soil(j)%phys(ly-1)%d) 
            else
              frac_above_300 = 0.0
            endif
            if (bsn_cc%cswat == 0) then
              soil1(j)%tot_300_c = soil1(j)%tot_300_c + (soil1(j)%hact(ly)%c + soil1(j)%hsta(ly)%c + soil1(j)%microb(ly)%c) * frac_above_300
            endif
            if(bsn_cc%cswat == 2 ) then
              soil1(j)%tot_300_c = soil1(j)%tot_300_c + soil1(j)%tot(ly)%c * frac_above_300 
            endif
          end do
          !! compute cswat==0 totals if needed (cswat==2 already has soil1%tot and soil1%seq populated)
          if (bsn_cc%cswat == 0 ) then
            do ly = 1, soil(j)%nly
              soil1(j)%tot(ly)%c = soil1(j)%hact(ly)%c + soil1(j)%hsta(ly)%c + soil1(j)%microb(ly)%c
            end do
          end if

          !! compute total sequestered C above or equal to 300mm (was previously written to hru_seq_lyr; now folded into hru_cbn_lyr)
          soil1(j)%seq_tot_300_c = 0.0
          do ly = 1, soil(j)%nly
            if (soil(j)%phys(ly)%d <= 300.0) then
              frac_above_300 = 1.0
            else if (ly == 1) then
              frac_above_300 = 300.0 / soil(j)%phys(ly)%d
            else if (soil(j)%phys(ly-1)%d < 300.0) then
              frac_above_300 = (300.0 -  soil(j)%phys(ly-1)%d)/ (soil(j)%phys(ly)%d - soil(j)%phys(ly-1)%d)
            else
              frac_above_300 = 0.0
            endif
            if (bsn_cc%cswat == 0) then
              soil1(j)%seq_tot_300_c = soil1(j)%seq_tot_300_c + (soil1(j)%hact(ly)%c + soil1(j)%hsta(ly)%c + soil1(j)%microb(ly)%c) * frac_above_300
              soil1(j)%seq(ly)%c = soil1(j)%hact(ly)%c + soil1(j)%hsta(ly)%c + soil1(j)%microb(ly)%c
            endif
            if(bsn_cc%cswat == 2 ) then
              soil1(j)%seq_tot_300_c = soil1(j)%seq_tot_300_c + soil1(j)%seq(ly)%c * frac_above_300
            endif
          end do

          !! hru_cbn_lyr_{day,mon,yr,aa} - depth-prefixed columns; header written once at file open.
          call cb_cbn_lyr_emit(out_freq, j, iob)

          !! hru_n_p_pool_stat_{day,mon,yr,aa} - wide-per-layer, 18 vars
          call cb_n_p_pool_emit(out_freq, j, iob)
          
          !! per-family gating happens inside each emit subroutine (no more cbn_diagnostics wrap).
          call cb_plc_stat_emit(out_freq, j, iob)
          call cb_soil_snap_period(out_freq, j, iob)
          if (bsn_cc%cswat == 2) then
            call cb_cflux_stat_emit(out_freq, j, iob)
            call cb_cpool_stat_emit(out_freq, j, iob)
          endif

        end do    !! hru loop
        
        !! summing hru output for the basin
        do j = 1, sp_ob%hru
          iihru = lsu_elem(j)%obtypno
          if (lsu_elem(iihru)%bsn_frac > 1.e-12) then
            const = lsu_elem(iihru)%bsn_frac
            if (lsu_elem(iihru)%obtyp == "hru") then
              bsn_org_soil = bsn_org_soil + const * soil1(iihru)%tot_org
              bsn_org_pl = bsn_org_pl + const * pl_mass(iihru)%tot_com
              bsn_org_rsd = bsn_org_rsd + const * soil_prof_rsd + pl_mass(iihru)%rsd_tot
              bsn_mn = bsn_mn + const * soil1(iihru)%tot_mn
              bsn_mp = bsn_mp + const * soil1(iihru)%tot_mp
            end if
          end if
        end do
                
        !! basin_carbon_all.txt (unit 4566) removed; basin yearly sums reconstructable from HRU files

      endif      

      return

      contains

      subroutine cb_cpool_stat_emit(freq_in, hru_j, hru_iob)
        !! emit one wide-per-layer row to the cpool_stat file matching freq_in.
        !! state-variable file: each call writes whatever the pool is at that timestep boundary.
        character(len=2), intent(in) :: freq_in
        integer, intent(in) :: hru_j, hru_iob

        integer :: u_txt, u_csv, k, n_use
        real :: buf(cb_n_layers)

        select case(freq_in)
          case (" d"); u_txt = 4538; u_csv = 4542; if (pco%cb_cpool_hru%d /= "y") return
          case (" m"); u_txt = 4539; u_csv = 4543; if (pco%cb_cpool_hru%m /= "y") return
          case (" y"); u_txt = 4540; u_csv = 4544; if (pco%cb_cpool_hru%y /= "y") return
          case (" a"); u_txt = 4541; u_csv = 4545; if (pco%cb_cpool_hru%a /= "y") return
          case default; return
        end select

        n_use = soil(hru_j)%nly

        !! txt
        write (u_txt, '(5i12,2x,i12,2x,a16)', advance='no') &
          time%day, time%mo, time%day_mo, time%yrc, hru_j, ob(hru_iob)%gis_id, ob(hru_iob)%name
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = real(soil(hru_j)%phys(k)%d); end do
        call cb_write_depth_row(u_txt, buf, n_use, .false., "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%rsd_tot(k)%c; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%str(k)%c; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%meta(k)%c; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%hs(k)%c; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%hp(k)%c; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%microb(k)%c; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%lig(k)%c; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%nonlig(k)%c; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%root_tot(k)%m; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil(hru_j)%phys(k)%tot_sw; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "yes")

        !! csv
        if (pco%csvout == "y") then
          call cb_emit_row_id_csv(u_csv, hru_j, hru_iob)
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = real(soil(hru_j)%phys(k)%d); end do
          call cb_write_depth_row(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%rsd_tot(k)%c; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%str(k)%c; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%meta(k)%c; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%hs(k)%c; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%hp(k)%c; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%microb(k)%c; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%lig(k)%c; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%nonlig(k)%c; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%root_tot(k)%m; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil(hru_j)%phys(k)%tot_sw; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "yes")
        end if
      end subroutine cb_cpool_stat_emit

      subroutine cb_emit_row_id_txt(unit_no, hru_j, hru_iob)
        integer, intent(in) :: unit_no, hru_j, hru_iob
        write (unit_no, '(5i12,2x,i12,2x,a16)', advance='no') &
          time%day, time%mo, time%day_mo, time%yrc, hru_j, ob(hru_iob)%gis_id, ob(hru_iob)%name
      end subroutine cb_emit_row_id_txt

      subroutine cb_emit_row_id_csv(unit_no, hru_j, hru_iob)
        integer, intent(in) :: unit_no, hru_j, hru_iob
        write (unit_no, '(5(i0,a),i0,a,a)', advance='no') &
          time%day, ",", time%mo, ",", time%day_mo, ",", time%yrc, ",", hru_j, ",", &
          ob(hru_iob)%gis_id, ",", trim(ob(hru_iob)%name)
      end subroutine cb_emit_row_id_csv

      subroutine cb_n_p_pool_emit(freq_in, hru_j, hru_iob)
        !! 18 N+P pool vars per layer, wide format
        character(len=2), intent(in) :: freq_in
        integer, intent(in) :: hru_j, hru_iob
        integer :: u_txt, u_csv, k, n_use
        real :: buf(cb_n_layers)

        select case(freq_in)
          case (" d"); u_txt = 4566; u_csv = 4570; if (pco%cb_npool_hru%d /= "y") return
          case (" m"); u_txt = 4567; u_csv = 4571; if (pco%cb_npool_hru%m /= "y") return
          case (" y"); u_txt = 4568; u_csv = 4572; if (pco%cb_npool_hru%y /= "y") return
          case (" a"); u_txt = 4569; u_csv = 4573; if (pco%cb_npool_hru%a /= "y") return
          case default; return
        end select
        n_use = soil(hru_j)%nly

        call cb_emit_row_id_txt(u_txt, hru_j, hru_iob)
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = real(soil(hru_j)%phys(k)%d); end do
        call cb_write_depth_row(u_txt, buf, n_use, .false., "no")
        !! N pools
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%rsd_tot(k)%n + soil1(hru_j)%str(k)%n + soil1(hru_j)%meta(k)%n + soil1(hru_j)%hs(k)%n + soil1(hru_j)%hp(k)%n + soil1(hru_j)%microb(k)%n + soil1(hru_j)%water(k)%n + soil1(hru_j)%man(k)%n; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%rsd_tot(k)%n; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%str(k)%n; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%meta(k)%n; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%hs(k)%n; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%hp(k)%n; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%microb(k)%n; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%lig(k)%n; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%nonlig(k)%n; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "no")
        !! P pools
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%rsd_tot(k)%p + soil1(hru_j)%str(k)%p + soil1(hru_j)%meta(k)%p + soil1(hru_j)%hs(k)%p + soil1(hru_j)%hp(k)%p + soil1(hru_j)%microb(k)%p + soil1(hru_j)%water(k)%p + soil1(hru_j)%man(k)%p; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%rsd_tot(k)%p; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%str(k)%p; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%meta(k)%p; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%hs(k)%p; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%hp(k)%p; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%microb(k)%p; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%lig(k)%p; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%nonlig(k)%p; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "yes")

        if (pco%csvout == "y") then
          call cb_emit_row_id_csv(u_csv, hru_j, hru_iob)
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = real(soil(hru_j)%phys(k)%d); end do
          call cb_write_depth_row(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%rsd_tot(k)%n + soil1(hru_j)%str(k)%n + soil1(hru_j)%meta(k)%n + soil1(hru_j)%hs(k)%n + soil1(hru_j)%hp(k)%n + soil1(hru_j)%microb(k)%n + soil1(hru_j)%water(k)%n + soil1(hru_j)%man(k)%n; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%rsd_tot(k)%n; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%str(k)%n; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%meta(k)%n; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%hs(k)%n; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%hp(k)%n; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%microb(k)%n; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%lig(k)%n; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%nonlig(k)%n; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%rsd_tot(k)%p + soil1(hru_j)%str(k)%p + soil1(hru_j)%meta(k)%p + soil1(hru_j)%hs(k)%p + soil1(hru_j)%hp(k)%p + soil1(hru_j)%microb(k)%p + soil1(hru_j)%water(k)%p + soil1(hru_j)%man(k)%p; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%rsd_tot(k)%p; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%str(k)%p; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%meta(k)%p; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%hs(k)%p; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%hp(k)%p; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%microb(k)%p; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%lig(k)%p; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%nonlig(k)%p; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "yes")
        end if
      end subroutine cb_n_p_pool_emit

      subroutine cb_plc_stat_emit(freq_in, hru_j, hru_iob)
        !! 7 plant carbon vars, no soil layers
        character(len=2), intent(in) :: freq_in
        integer, intent(in) :: hru_j, hru_iob
        integer :: u_txt, u_csv

        select case(freq_in)
          case (" d"); u_txt = 4574; u_csv = 4578; if (pco%cb_plt_hru%d /= "y") return
          case (" m"); u_txt = 4575; u_csv = 4579; if (pco%cb_plt_hru%m /= "y") return
          case (" y"); u_txt = 4576; u_csv = 4580; if (pco%cb_plt_hru%y /= "y") return
          case (" a"); u_txt = 4577; u_csv = 4581; if (pco%cb_plt_hru%a /= "y") return
          case default; return
        end select

        write (u_txt, '(5i12,2x,i12,2x,a16,7(1x,g16.7))') &
          time%day, time%mo, time%day_mo, time%yrc, hru_j, ob(hru_iob)%gis_id, ob(hru_iob)%name, &
          pl_mass(hru_j)%tot_com%c, pl_mass(hru_j)%ab_gr_com%c, pl_mass(hru_j)%leaf_com%c, &
          pl_mass(hru_j)%stem_com%c, pl_mass(hru_j)%seed_com%c, pl_mass(hru_j)%root_com%c, pl_mass(hru_j)%rsd_tot%c

        if (pco%csvout == "y") then
          write (u_csv, '(5(i0,a),i0,a,a,7(a,g0.7))') &
            time%day, ",", time%mo, ",", time%day_mo, ",", time%yrc, ",", hru_j, ",", &
            ob(hru_iob)%gis_id, ",", trim(ob(hru_iob)%name), &
            ",", pl_mass(hru_j)%tot_com%c, ",", pl_mass(hru_j)%ab_gr_com%c, ",", pl_mass(hru_j)%leaf_com%c, &
            ",", pl_mass(hru_j)%stem_com%c, ",", pl_mass(hru_j)%seed_com%c, ",", pl_mass(hru_j)%root_com%c, &
            ",", pl_mass(hru_j)%rsd_tot%c
        end if
      end subroutine cb_plc_stat_emit

      subroutine cb_cflux_stat_emit(freq_in, hru_j, hru_iob)
        !! 37 flux vars per layer, wide format.
        !! For _aa file (out_freq=" a"/"al"): values come from soil1%org_flx_cum_lr / yrs_prt (true annual average).
        !! For _day/_mon/_yr files: values come from soil1%org_flx_lr (instantaneous daily flux at the timestep boundary).
        character(len=2), intent(in) :: freq_in
        integer, intent(in) :: hru_j, hru_iob
        integer :: u_txt, u_csv, k, n_use
        real :: buf(cb_n_layers)
        logical :: use_aa

        select case(freq_in)
          case (" d"); u_txt = 4558; u_csv = 4562; use_aa = .false.; if (pco%cb_flux_hru%d /= "y") return
          case (" m"); u_txt = 4559; u_csv = 4563; use_aa = .false.; if (pco%cb_flux_hru%m /= "y") return
          case (" y"); u_txt = 4560; u_csv = 4564; use_aa = .false.; if (pco%cb_flux_hru%y /= "y") return
          case (" a"); u_txt = 4561; u_csv = 4565; use_aa = .true.; if (pco%cb_flux_hru%a /= "y") return
          case default; return
        end select
        n_use = soil(hru_j)%nly

        call cb_emit_row_id_txt(u_txt, hru_j, hru_iob)
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = real(soil(hru_j)%phys(k)%d); end do
        call cb_write_depth_row(u_txt, buf, n_use, .false., "no")
        !! all 37 organic_flux fields (matches cflux_vars order)
        call cb_cflux_emit_blocks(u_txt, .false., hru_j, n_use, buf, use_aa)

        if (pco%csvout == "y") then
          call cb_emit_row_id_csv(u_csv, hru_j, hru_iob)
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = real(soil(hru_j)%phys(k)%d); end do
          call cb_write_depth_row(u_csv, buf, n_use, .true., "no")
          call cb_cflux_emit_blocks(u_csv, .true., hru_j, n_use, buf, use_aa)
        end if
      end subroutine cb_cflux_stat_emit

      subroutine cb_cflux_emit_blocks(u, is_csv, hru_j, n_use, buf, use_aa)
        integer, intent(in) :: u, hru_j, n_use
        logical, intent(in) :: is_csv, use_aa
        real, intent(inout) :: buf(:)
        integer :: k
        real :: yrs

        yrs = max(time%yrs_prt, 1.0)
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = merge(soil1(hru_j)%org_flx_cum_lr(k)%cfmets1/yrs, soil1(hru_j)%org_flx_lr(k)%cfmets1, use_aa); end do
        call cb_write_var_block(u, buf, n_use, is_csv, "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = merge(soil1(hru_j)%org_flx_cum_lr(k)%cfstrs1/yrs, soil1(hru_j)%org_flx_lr(k)%cfstrs1, use_aa); end do
        call cb_write_var_block(u, buf, n_use, is_csv, "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = merge(soil1(hru_j)%org_flx_cum_lr(k)%cfstrs2/yrs, soil1(hru_j)%org_flx_lr(k)%cfstrs2, use_aa); end do
        call cb_write_var_block(u, buf, n_use, is_csv, "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = merge(soil1(hru_j)%org_flx_cum_lr(k)%efmets1/yrs, soil1(hru_j)%org_flx_lr(k)%efmets1, use_aa); end do
        call cb_write_var_block(u, buf, n_use, is_csv, "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = merge(soil1(hru_j)%org_flx_cum_lr(k)%efstrs1/yrs, soil1(hru_j)%org_flx_lr(k)%efstrs1, use_aa); end do
        call cb_write_var_block(u, buf, n_use, is_csv, "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = merge(soil1(hru_j)%org_flx_cum_lr(k)%efstrs2/yrs, soil1(hru_j)%org_flx_lr(k)%efstrs2, use_aa); end do
        call cb_write_var_block(u, buf, n_use, is_csv, "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = merge(soil1(hru_j)%org_flx_cum_lr(k)%immmets1/yrs, soil1(hru_j)%org_flx_lr(k)%immmets1, use_aa); end do
        call cb_write_var_block(u, buf, n_use, is_csv, "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = merge(soil1(hru_j)%org_flx_cum_lr(k)%immstrs1/yrs, soil1(hru_j)%org_flx_lr(k)%immstrs1, use_aa); end do
        call cb_write_var_block(u, buf, n_use, is_csv, "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = merge(soil1(hru_j)%org_flx_cum_lr(k)%immstrs2/yrs, soil1(hru_j)%org_flx_lr(k)%immstrs2, use_aa); end do
        call cb_write_var_block(u, buf, n_use, is_csv, "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = merge(soil1(hru_j)%org_flx_cum_lr(k)%mnrmets1/yrs, soil1(hru_j)%org_flx_lr(k)%mnrmets1, use_aa); end do
        call cb_write_var_block(u, buf, n_use, is_csv, "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = merge(soil1(hru_j)%org_flx_cum_lr(k)%mnrstrs1/yrs, soil1(hru_j)%org_flx_lr(k)%mnrstrs1, use_aa); end do
        call cb_write_var_block(u, buf, n_use, is_csv, "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = merge(soil1(hru_j)%org_flx_cum_lr(k)%mnrstrs2/yrs, soil1(hru_j)%org_flx_lr(k)%mnrstrs2, use_aa); end do
        call cb_write_var_block(u, buf, n_use, is_csv, "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = merge(soil1(hru_j)%org_flx_cum_lr(k)%co2fmet/yrs, soil1(hru_j)%org_flx_lr(k)%co2fmet, use_aa); end do
        call cb_write_var_block(u, buf, n_use, is_csv, "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = merge(soil1(hru_j)%org_flx_cum_lr(k)%co2fstr/yrs, soil1(hru_j)%org_flx_lr(k)%co2fstr, use_aa); end do
        call cb_write_var_block(u, buf, n_use, is_csv, "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = merge(soil1(hru_j)%org_flx_cum_lr(k)%cfs1s2/yrs, soil1(hru_j)%org_flx_lr(k)%cfs1s2, use_aa); end do
        call cb_write_var_block(u, buf, n_use, is_csv, "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = merge(soil1(hru_j)%org_flx_cum_lr(k)%cfs1s3/yrs, soil1(hru_j)%org_flx_lr(k)%cfs1s3, use_aa); end do
        call cb_write_var_block(u, buf, n_use, is_csv, "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = merge(soil1(hru_j)%org_flx_cum_lr(k)%cfs2s1/yrs, soil1(hru_j)%org_flx_lr(k)%cfs2s1, use_aa); end do
        call cb_write_var_block(u, buf, n_use, is_csv, "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = merge(soil1(hru_j)%org_flx_cum_lr(k)%cfs2s3/yrs, soil1(hru_j)%org_flx_lr(k)%cfs2s3, use_aa); end do
        call cb_write_var_block(u, buf, n_use, is_csv, "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = merge(soil1(hru_j)%org_flx_cum_lr(k)%cfs3s1/yrs, soil1(hru_j)%org_flx_lr(k)%cfs3s1, use_aa); end do
        call cb_write_var_block(u, buf, n_use, is_csv, "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = merge(soil1(hru_j)%org_flx_cum_lr(k)%efs1s2/yrs, soil1(hru_j)%org_flx_lr(k)%efs1s2, use_aa); end do
        call cb_write_var_block(u, buf, n_use, is_csv, "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = merge(soil1(hru_j)%org_flx_cum_lr(k)%efs1s3/yrs, soil1(hru_j)%org_flx_lr(k)%efs1s3, use_aa); end do
        call cb_write_var_block(u, buf, n_use, is_csv, "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = merge(soil1(hru_j)%org_flx_cum_lr(k)%efs2s1/yrs, soil1(hru_j)%org_flx_lr(k)%efs2s1, use_aa); end do
        call cb_write_var_block(u, buf, n_use, is_csv, "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = merge(soil1(hru_j)%org_flx_cum_lr(k)%efs2s3/yrs, soil1(hru_j)%org_flx_lr(k)%efs2s3, use_aa); end do
        call cb_write_var_block(u, buf, n_use, is_csv, "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = merge(soil1(hru_j)%org_flx_cum_lr(k)%efs3s1/yrs, soil1(hru_j)%org_flx_lr(k)%efs3s1, use_aa); end do
        call cb_write_var_block(u, buf, n_use, is_csv, "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = merge(soil1(hru_j)%org_flx_cum_lr(k)%imms1s2/yrs, soil1(hru_j)%org_flx_lr(k)%imms1s2, use_aa); end do
        call cb_write_var_block(u, buf, n_use, is_csv, "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = merge(soil1(hru_j)%org_flx_cum_lr(k)%imms1s3/yrs, soil1(hru_j)%org_flx_lr(k)%imms1s3, use_aa); end do
        call cb_write_var_block(u, buf, n_use, is_csv, "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = merge(soil1(hru_j)%org_flx_cum_lr(k)%imms2s1/yrs, soil1(hru_j)%org_flx_lr(k)%imms2s1, use_aa); end do
        call cb_write_var_block(u, buf, n_use, is_csv, "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = merge(soil1(hru_j)%org_flx_cum_lr(k)%imms2s3/yrs, soil1(hru_j)%org_flx_lr(k)%imms2s3, use_aa); end do
        call cb_write_var_block(u, buf, n_use, is_csv, "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = merge(soil1(hru_j)%org_flx_cum_lr(k)%imms3s1/yrs, soil1(hru_j)%org_flx_lr(k)%imms3s1, use_aa); end do
        call cb_write_var_block(u, buf, n_use, is_csv, "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = merge(soil1(hru_j)%org_flx_cum_lr(k)%mnrs1s2/yrs, soil1(hru_j)%org_flx_lr(k)%mnrs1s2, use_aa); end do
        call cb_write_var_block(u, buf, n_use, is_csv, "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = merge(soil1(hru_j)%org_flx_cum_lr(k)%mnrs1s3/yrs, soil1(hru_j)%org_flx_lr(k)%mnrs1s3, use_aa); end do
        call cb_write_var_block(u, buf, n_use, is_csv, "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = merge(soil1(hru_j)%org_flx_cum_lr(k)%mnrs2s1/yrs, soil1(hru_j)%org_flx_lr(k)%mnrs2s1, use_aa); end do
        call cb_write_var_block(u, buf, n_use, is_csv, "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = merge(soil1(hru_j)%org_flx_cum_lr(k)%mnrs2s3/yrs, soil1(hru_j)%org_flx_lr(k)%mnrs2s3, use_aa); end do
        call cb_write_var_block(u, buf, n_use, is_csv, "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = merge(soil1(hru_j)%org_flx_cum_lr(k)%mnrs3s1/yrs, soil1(hru_j)%org_flx_lr(k)%mnrs3s1, use_aa); end do
        call cb_write_var_block(u, buf, n_use, is_csv, "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = merge(soil1(hru_j)%org_flx_cum_lr(k)%co2fs1/yrs, soil1(hru_j)%org_flx_lr(k)%co2fs1, use_aa); end do
        call cb_write_var_block(u, buf, n_use, is_csv, "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = merge(soil1(hru_j)%org_flx_cum_lr(k)%co2fs2/yrs, soil1(hru_j)%org_flx_lr(k)%co2fs2, use_aa); end do
        call cb_write_var_block(u, buf, n_use, is_csv, "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = merge(soil1(hru_j)%org_flx_cum_lr(k)%co2fs3/yrs, soil1(hru_j)%org_flx_lr(k)%co2fs3, use_aa); end do
        call cb_write_var_block(u, buf, n_use, is_csv, "yes")
      end subroutine cb_cflux_emit_blocks

      subroutine cb_cbn_lyr_emit(freq_in, hru_j, hru_iob)
        !! 2 SOC vars per layer (tot + seq), plus 300mm sums; wide-per-layer
        character(len=2), intent(in) :: freq_in
        integer, intent(in) :: hru_j, hru_iob
        integer :: u_txt, u_csv, k, n_use
        real :: buf(cb_n_layers)

        select case(freq_in)
          case (" d"); u_txt = 4530; u_csv = 4534; if (pco%cb_lyr_hru%d /= "y") return
          case (" m"); u_txt = 4531; u_csv = 4535; if (pco%cb_lyr_hru%m /= "y") return
          case (" y"); u_txt = 4532; u_csv = 4536; if (pco%cb_lyr_hru%y /= "y") return
          case (" a"); u_txt = 4533; u_csv = 4537; if (pco%cb_lyr_hru%a /= "y") return
          case default; return
        end select
        n_use = soil(hru_j)%nly

        call cb_emit_row_id_txt(u_txt, hru_j, hru_iob)
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = real(soil(hru_j)%phys(k)%d); end do
        call cb_write_depth_row(u_txt, buf, n_use, .false., "no")
        write (u_txt, '(1x,g22.7)', advance='no') soil1(hru_j)%tot_300_c/1000.
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%tot(k)%c/1000.; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "no")
        write (u_txt, '(1x,g22.7)', advance='no') soil1(hru_j)%seq_tot_300_c/1000.
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%seq(k)%c/1000.; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "yes")

        if (pco%csvout == "y") then
          call cb_emit_row_id_csv(u_csv, hru_j, hru_iob)
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = real(soil(hru_j)%phys(k)%d); end do
          call cb_write_depth_row(u_csv, buf, n_use, .true., "no")
          write (u_csv, '(a,g0.7)', advance='no') ",", soil1(hru_j)%tot_300_c/1000.
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%tot(k)%c/1000.; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "no")
          write (u_csv, '(a,g0.7)', advance='no') ",", soil1(hru_j)%seq_tot_300_c/1000.
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%seq(k)%c/1000.; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "yes")
        end if
      end subroutine cb_cbn_lyr_emit

      subroutine cb_soil_snap_period(freq_in, hru_j, hru_iob)
        !! end-of-period soil property snapshot per layer; routed to hru_soil_snap_{day,mon,yr}.
        character(len=2), intent(in) :: freq_in
        integer, intent(in) :: hru_j, hru_iob
        integer :: u_txt, u_csv

        select case(freq_in)
          case (" d"); u_txt = 4598; u_csv = 4602; if (pco%cb_snap_hru%d /= "y") return
          case (" m"); u_txt = 4599; u_csv = 4603; if (pco%cb_snap_hru%m /= "y") return
          case (" y"); u_txt = 4600; u_csv = 4604; if (pco%cb_snap_hru%y /= "y") return
          case default; return    !! aa slot rolls into _tot file via " b"/" e" calls
        end select
        call cb_soil_snap_emit(u_txt, u_csv, "period", hru_j, hru_iob)
      end subroutine cb_soil_snap_period

      subroutine cb_soil_snap_emit(u_txt, u_csv, stage, hru_j, hru_iob)
        !! emit one wide-per-layer snapshot row to (u_txt, u_csv).
        !! `stage` is "begsim", "endsim", or "period". For period rows we still emit identical layout.
        !! tot_c column uses soil1(hru_j)%tot(ly)%c for ALL stages (kg C / ha).
        integer, intent(in) :: u_txt, u_csv, hru_j, hru_iob
        character(len=*), intent(in) :: stage
        integer :: k, n_use
        real :: buf(cb_n_layers)

        n_use = soil(hru_j)%nly

        call cb_emit_row_id_txt(u_txt, hru_j, hru_iob)
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = real(soil(hru_j)%phys(k)%d); end do
        call cb_write_depth_row(u_txt, buf, n_use, .false., "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil(hru_j)%phys(k)%bd; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil(hru_j)%phys(k)%awc; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil(hru_j)%phys(k)%k; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%tot(k)%c; end do  !! %c at all stages
        call cb_write_var_block(u_txt, buf, n_use, .false., "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil(hru_j)%phys(k)%clay; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil(hru_j)%phys(k)%silt; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil(hru_j)%phys(k)%sand; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil(hru_j)%phys(k)%rock; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil(hru_j)%ly(k)%alb; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil(hru_j)%ly(k)%usle_k; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil(hru_j)%ly(k)%ec; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil(hru_j)%ly(k)%cal; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "no")
        buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil(hru_j)%ly(k)%ph; end do
        call cb_write_var_block(u_txt, buf, n_use, .false., "yes")

        if (pco%csvout == "y") then
          call cb_emit_row_id_csv(u_csv, hru_j, hru_iob)
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = real(soil(hru_j)%phys(k)%d); end do
          call cb_write_depth_row(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil(hru_j)%phys(k)%bd; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil(hru_j)%phys(k)%awc; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil(hru_j)%phys(k)%k; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil1(hru_j)%tot(k)%c; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil(hru_j)%phys(k)%clay; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil(hru_j)%phys(k)%silt; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil(hru_j)%phys(k)%sand; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil(hru_j)%phys(k)%rock; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil(hru_j)%ly(k)%alb; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil(hru_j)%ly(k)%usle_k; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil(hru_j)%ly(k)%ec; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil(hru_j)%ly(k)%cal; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "no")
          buf = 0.0; do k = 1, min(cb_n_layers, n_use); buf(k) = soil(hru_j)%ly(k)%ph; end do
          call cb_write_var_block(u_csv, buf, n_use, .true., "yes")
        end if
      end subroutine cb_soil_snap_emit

      end subroutine soil_nutcarb_write