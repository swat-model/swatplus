      subroutine carbon_bsn_read

      !! reads carbon parameters from carbon.bsn and carbon_lyr.bsn.
      !!
      !! carbon.bsn: title line, column header line, single data row with
      !!   28 values (22 original scalars, 5 residue-decomp tunables
      !!   n_act_frac, cnr_cap, cnr_ref, cpr_cap, cpr_ref, and the
      !!   mathers_method 0/1 flag for humus-slow pool initialization).
      !! legacy cbn_diag column (retired output flag) removed entirely.
      !! Example files no longer carry it.
      !!
      !! carbon_lyr.bsn: title line, column header line, then one data
      !!   row per layer group. each data row starts with an integer
      !!   layer id (1 = top, 2 = subsurface) followed by 11 coefficients
      !!   for that layer. layer id is the array index into carbdb(:) and
      !!   org_allo(:), so future expansion past dimension(2) needs no
      !!   change to this reader.
      !!
      !! both files are required when bsn_cc%cswat == 2; the routine aborts via
      !! error stop if either file is missing, or if its data line(s) are missing
      !! or malformed. the leading title and column-header lines are optional and
      !! may be blank. no-op when carbon is off.

      use carbon_module
      use basin_module
      use tillage_data_module
      use plant_data_module
      use input_file_module, only : in_basin

      implicit none

      character (len=80)  :: titldum = ""
      character (len=500) :: header  = ""
      integer             :: eof     = 0
      logical             :: i_exist = .false.
      integer             :: layer_id  = 0
      integer             :: max_lyr   = 0
      integer             :: rows_read = 0
      character (len=32)  :: carbon_lyr = ""   ! per-layer file, derived from carbon.bsn
      integer             :: idot = 0
      real                :: r_hp_rate, r_hs_rate, r_microb_rate
      real                :: r_meta_rate, r_str_rate, r_microb_top_rate, r_hs_hp
      real                :: r_a1co2, r_asco2, r_apco2, r_abco2
      integer             :: mathers_int = 0   ! 0/1 flag for org_frac%mathers_method

      if (bsn_cc%cswat /= 2) return

      !! carbon.bsn (scalars)

      inquire (file=in_basin%carbon_bsn, exist=i_exist)
      if (.not. i_exist) then
        write (*,*) "ERROR: ", trim(in_basin%carbon_bsn), " is required when carbon is enabled (codes.bsn carbon = 2)"
        write (9001,*) "ERROR: ", trim(in_basin%carbon_bsn), " is required when carbon is enabled (codes.bsn carbon = 2)"
        error stop
      end if

      open (107, file=in_basin%carbon_bsn, iostat=eof)
      if (eof /= 0) then
        write (*,*) "ERROR: ", trim(in_basin%carbon_bsn), " could not be opened"
        write (9001,*) "ERROR: ", trim(in_basin%carbon_bsn), " could not be opened"
        error stop
      end if

      !! comment and column-header lines are optional (may be blank); read with
      !! '(a)' so a blank line consumes exactly one record instead of skipping it.
      read (107, '(a)', iostat=eof) titldum
      read (107, '(a)', iostat=eof) header

      read (107, *, iostat=eof)                                   &
        org_frac%frac_seq,         org_frac%frac_hum_microb,      &
        org_frac%frac_hum_slow,    org_frac%frac_hum_passive,     &
        cb_wtr_coef%prmt_21,       cb_wtr_coef%prmt_44,           &
        till_eff_days,             man_coef%rtof,                 &
        bio_consf,                 till_consf,                    &
        org_con%tmpf,              org_con%watf,                  &
        org_con%tn, org_con%top,   org_con%tx,                    &
        bmix_a, bmix_b, bmix_c,                                   &
        tillmix_a, tillmix_b, tillmix_c,                          &
        photo_degrade_factor,                                     &
        n_act_frac, cnr_cap, cnr_ref, cpr_cap, cpr_ref,           &
        mathers_int

      if (eof /= 0) then
        write (*,*) "ERROR: ", trim(in_basin%carbon_bsn), " data/values line is missing or could not be parsed (expected 28 values)"
        write (9001,*) "ERROR: ", trim(in_basin%carbon_bsn), " data/values line is missing or could not be parsed (expected 28 values)"
        close (107)
        error stop
      end if

      !! mathers_method: 1 = use the Mathers humus-slow init in soil_nutcarb_init, 0 = original method
      org_frac%mathers_method = (mathers_int == 1)

      close (107)

      !! carbon_lyr.bsn (per-layer table)

      !! the per-layer file name is built from carbon.bsn so it tracks any rename in
      !! file.cio: carbon.bsn -> carbon_lyr.bsn, foo.bsn -> foo_lyr.bsn. it travels
      !! with carbon.bsn and is not separately listed in file.cio.
      idot = index(trim(in_basin%carbon_bsn), ".", back=.true.)
      if (idot > 1) then
        carbon_lyr = in_basin%carbon_bsn(1:idot-1) // "_lyr" // trim(in_basin%carbon_bsn(idot:))
      else
        carbon_lyr = trim(in_basin%carbon_bsn) // "_lyr.bsn"
      end if

      inquire (file=carbon_lyr, exist=i_exist)
      if (.not. i_exist) then
        write (*,*) "ERROR: ", trim(carbon_lyr), " is required when carbon is enabled (codes.bsn carbon = 2)"
        write (9001,*) "ERROR: ", trim(carbon_lyr), " is required when carbon is enabled (codes.bsn carbon = 2)"
        error stop
      end if

      open (107, file=carbon_lyr, iostat=eof)
      if (eof /= 0) then
        write (*,*) "ERROR: ", trim(carbon_lyr), " could not be opened"
        write (9001,*) "ERROR: ", trim(carbon_lyr), " could not be opened"
        error stop
      end if

      read (107, '(a)', iostat=eof) titldum
      read (107, '(a)', iostat=eof) header

      max_lyr = size(carbdb)

      do
        read (107, *, iostat=eof)                                  &
          layer_id,                                                &
          r_hp_rate, r_hs_rate, r_microb_rate,                     &
          r_meta_rate, r_str_rate, r_microb_top_rate, r_hs_hp,     &
          r_a1co2, r_asco2, r_apco2, r_abco2
        if (eof /= 0) exit

        if (layer_id < 1 .or. layer_id > max_lyr) then
          write (9001,*) trim(carbon_lyr), ": layer id ", layer_id,    &
                         " is out of range (1 to ", max_lyr, "); row ignored"
          cycle
        end if

        carbdb(layer_id)%hp_rate         = r_hp_rate
        carbdb(layer_id)%hs_rate         = r_hs_rate
        carbdb(layer_id)%microb_rate     = r_microb_rate
        carbdb(layer_id)%meta_rate       = r_meta_rate
        carbdb(layer_id)%str_rate        = r_str_rate
        carbdb(layer_id)%microb_top_rate = r_microb_top_rate
        carbdb(layer_id)%hs_hp           = r_hs_hp

        org_allo(layer_id)%a1co2 = r_a1co2
        org_allo(layer_id)%asco2 = r_asco2
        org_allo(layer_id)%apco2 = r_apco2
        org_allo(layer_id)%abco2 = r_abco2

        rows_read = rows_read + 1
      end do

      if (rows_read < max_lyr) then
        write (*,*) "ERROR: ", trim(carbon_lyr), " provided ", rows_read,  &
                    " row(s); expected ", max_lyr
        write (9001,*) "ERROR: ", trim(carbon_lyr), " provided ", rows_read,  &
                       " row(s); expected ", max_lyr
        close (107)
        error stop
      end if

      close (107)
      return
      end subroutine carbon_bsn_read
