      subroutine output_landscape_init

      use hydrograph_module
      use channel_module
      use sd_channel_module
      use basin_module
      use maximum_data_module
      use calibration_data_module
      use aquifer_module
      use output_landscape_module
      use time_module
      use carbon_module
      use output_path_module
      use soil_module
      use carbon_legacy_module, only: carbon_legacy_open

      implicit none

      external :: soil_nutcarb_write

      integer :: jhru

      !! if carbon_layers.prt did not set the per-layer output count explicitly,
      !! default it to the largest soil layer count across all HRUs so every
      !! layer is written and no real layer is truncated.
      if (.not. cb_n_layers_explicit) then
        cb_n_layers = 0
        do jhru = 1, sp_ob%hru
          if (soil(jhru)%nly > cb_n_layers) cb_n_layers = soil(jhru)%nly
        end do
        if (cb_n_layers < 1) cb_n_layers = 7
      end if

      if (sp_ob%hru > 0) then
!!!  HRU - Water balance
        if (pco%wb_hru%d == "y") then
          call open_output_file(2000, "hru_wb_day.txt", 1500)
          write (2000,*)  bsn%name, prog
          write (2000,*) wb_hdr  !! hru
          write (2000,*) wb_hdr_units
          write (9000,*) "HRU                       hru_wb_day.txt"
          !write (9000,*) "HRU                 waterbal_day_hru.txt"
            if (pco%csvout == "y") then
              call open_output_file(2004, "hru_wb_day.csv", 1500)
              write (2004,*)  bsn%name, prog
              write (2004,'(*(G0.6,:,","))') wb_hdr  !! hru
              write (2004,'(*(G0.6,:,","))') wb_hdr_units
              write (9000,*) "HRU                       hru_wb_day.csv"
              !write (9000,*) "HRU                 waterbal_day_hru.csv"              
            end if 
        endif
 
        if (pco%wb_hru%m == "y") then 
          call open_output_file(2001, "hru_wb_mon.txt", 1500)
          write (2001,*)  bsn%name, prog
          write (2001,*) wb_hdr   !! hru
          write (2001,*) wb_hdr_units
           write (9000,*) "HRU                       hru_wb_mon.txt"
          !write (9000,*) "HRU                 waterbal_mon_hru.txt"
          if (pco%csvout == "y") then
            call open_output_file(2005, "hru_wb_mon.csv", 1500)
            write (2005,*)  bsn%name, prog
            write (2005,'(*(G0.6,:,","))') wb_hdr   !! hru
            write (2005,'(*(G0.6,:,","))') wb_hdr_units
            write (9000,*) "HRU                       hru_wb_mon.csv"
            !write (9000,*) "HRU                 waterbal_mon_hru.csv"
          end if
        end if 

        if (pco%wb_hru%y == "y") then
          call open_output_file(2002, "hru_wb_yr.txt", 1500)
          write (2002,*)  bsn%name, prog
          write (2002,*) wb_hdr  !! hru
          write (2002,*) wb_hdr_units
          write (9000,*) "HRU                       hru_wb_yr.txt"
            if (pco%csvout == "y") then
              call open_output_file(2006, "hru_wb_yr.csv", 1500)
              write (2006,*)  bsn%name, prog
              write (2006,'(*(G0.6,:,","))') wb_hdr  !! hru
              write (2006,'(*(G0.6,:,","))') wb_hdr_units
              write (9000,*) "HRU                       hru_wb_yr.csv"
              !write (9000,*) "HRU                 waterbal_yr_hru.csv"
            end if 
        endif
        
        if (pco%wb_hru%a == "y") then
          call open_output_file(2003, "hru_wb_aa.txt", 1500)
          write (2003,*)  bsn%name, prog
          write (2003,*) wb_hdr   !! hru
          write (2003,*) wb_hdr_units
          write (9000,*) "HRU                       hru_wb_aa.txt"
          if (pco%csvout == "y") then
            call open_output_file(2007, "hru_wb_aa.csv", 1500)
            write (2007,*)  bsn%name, prog
            write (2007,'(*(G0.6,:,","))') wb_hdr   !! hru
            write (2007,'(*(G0.6,:,","))') wb_hdr_units
            write (9000,*) "HRU                       hru_wb_aa.csv"
          end if
        end if 

!!!  HRU - Nutrient balance
        if (pco%nb_hru%d == "y") then
          call open_output_file(2020, "hru_nb_day.txt", 1500)
          write (2020,*)  bsn%name, prog
          write (2020,*) nb_hdr
          write (2020,*) nb_hdr_units
          write (9000,*) "HRU                       hru_nb_day.txt"
            if (pco%csvout == "y") then
              call open_output_file(2024, "hru_nb_day.csv", 1500)
              write (2024,*)  bsn%name, prog
              write (2024,'(*(G0.6,:,","))') nb_hdr
              write (2024,'(*(G0.6,:,","))') nb_hdr_units
              write (9000,*) "HRU                       hru_nb_day.csv"
            end if
        endif
        
!!!  HRU - NEW Nutrient cycling output
        if (pco%nb_hru%d == "y") then
          call open_output_file(3333, "hru_ncycle_day.txt", 1500)
          write (3333,*)  bsn%name, prog
          write (3333,*) nb_hdr1
          write (3333,*) nb_hdr_units1
          write (9000,*) "HRU                       hru_ncycle_day.txt"
            if (pco%csvout == "y") then
              call open_output_file(3334, "hru_ncycle_day.csv", 1500)
              write (3334,*)  bsn%name, prog
              write (3334,'(*(G0.6,:,","))') nb_hdr1
              write (3334,'(*(G0.6,:,","))') nb_hdr_units1
              write (9000,*) "HRU                       hru_ncycle_day.csv"
            end if
        endif
        
        if (pco%nb_hru%m == "y") then
          call open_output_file(3335, "hru_ncycle_mon.txt", 1500)
          write (3335,*)  bsn%name, prog
          write (3335,*) nb_hdr1
          write (3335,*) nb_hdr_units1
          write (9000,*) "HRU                       hru_ncycle_mon.txt"
            if (pco%csvout == "y") then
              call open_output_file(3336, "hru_ncycle_mon.csv", 1500)
              write (3336,*)  bsn%name, prog
              write (3336,'(*(G0.6,:,","))') nb_hdr1
              write (3336,'(*(G0.6,:,","))') nb_hdr_units1
              write (9000,*) "HRU                       hru_ncycle_mon.csv"
            end if
        endif
        
     if (pco%nb_hru%y == "y") then
          call open_output_file(3337, "hru_ncycle_yr.txt", 1500)
          write (3337,*)  bsn%name, prog
          write (3337,*) nb_hdr1
          write (3337,*) nb_hdr_units1
          write (9000,*) "HRU                       hru_ncycle_yr.txt"
            if (pco%csvout == "y") then
              call open_output_file(3338, "hru_ncycle_yr.csv", 1500)
              write (3338,*)  bsn%name, prog
              write (3338,'(*(G0.6,:,","))') nb_hdr1
              write (3338,'(*(G0.6,:,","))') nb_hdr_units1
              write (9000,*) "HRU                       hru_ncycle_yr.csv"
            end if
        endif
        
        if (pco%nb_hru%a == "y") then
          call open_output_file(3339, "hru_ncycle_aa.txt", 1500)
          write (3339,*)  bsn%name, prog
          write (3339,*) nb_hdr1
          write (3339,*) nb_hdr_units1
          write (9000,*) "HRU                       hru_ncycle_aa.txt"
            if (pco%csvout == "y") then
              call open_output_file(3340, "hru_ncycle_aa.csv", 1500)
              write (3340,*)  bsn%name, prog
              write (3340,'(*(G0.6,:,","))') nb_hdr1
              write (3340,'(*(G0.6,:,","))') nb_hdr_units1
              write (9000,*) "HRU                       hru_ncycle_aa.csv"
            end if
        endif
!!!  HRU - NEW Nutrient cycling output
        
       if (pco%nb_hru%m == "y") then
        call open_output_file(2021, "hru_nb_mon.txt", 1500)
          write (2021,*) bsn%name, prog
          write (2021,*) nb_hdr
          write (2021,*) nb_hdr_units
          write (9000,*) "HRU                       hru_nb_mon.txt"
        if (pco%csvout == "y") then
          call open_output_file(2025, "hru_nb_mon.csv", 1500)
          write (2025,*) bsn%name, prog
          write (2025,'(*(G0.6,:,","))') nb_hdr
          write (2025,'(*(G0.6,:,","))') nb_hdr_units
          write (9000,*) "HRU                       hru_nb_mon.csv"
        end if
       end if

        if (pco%nb_hru%y == "y") then
          call open_output_file(2022, "hru_nb_yr.txt", 1500)
          write (2022,*) bsn%name, prog
          write (2022,*) nb_hdr
          write (2022,*) nb_hdr_units
          write (9000,*) "HRU                       hru_nb_yr.txt"
            if (pco%csvout == "y") then
              call open_output_file(2026, "hru_nb_yr.csv", 1500)
              write (2026,*) bsn%name, prog
              write (2026,'(*(G0.6,:,","))') nb_hdr
              write (2026,'(*(G0.6,:,","))') nb_hdr_units
              write (9000,*) "HRU                       hru_nb_yr.csv" 
            end if
        endif
        
       if (pco%nb_hru%a == "y") then 
        call open_output_file(2023, "hru_nb_aa.txt", 1500)
          write (2023,*) bsn%name, prog
          write (2023,*) nb_hdr
          write (2023,*) nb_hdr_units
          write (9000,*) "HRU                       hru_nb_aa.txt"
        if (pco%csvout == "y") then
          call open_output_file(2027, "hru_nb_aa.csv", 1500)
          write (2027,*) bsn%name, prog
          write (2027,'(*(G0.6,:,","))') nb_hdr
          write (2027,'(*(G0.6,:,","))') nb_hdr_units
          write (9000,*) "HRU                       hru_nb_aa.csv"
        end if
       end if
               
!!! hru_carb_gl gated by pco%cb_gl_hru (was riding hru_nb).
        if (pco%cb_gl_hru%d == "y") then
          call open_output_file(4520, "hru_carb_gl_day.txt", 8000)
          write (4520,*)  bsn%name, prog
          write (4520,*) carb_gl_hdr
          write (4520,*) carb_gl_hdr_units
          write (9000,*) "HRU                       hru_carb_gl_day.txt"
          if (pco%csvout == "y") then
            call open_output_file(4524, "hru_carb_gl_day.csv", 8000)
            write (4524,*)  bsn%name, prog
            write (4524,'(*(G0.6,:,","))') carb_gl_hdr
            write (4524,'(*(G0.6,:,","))') carb_gl_hdr_units
            write (9000,*) "HRU                       hru_carb_gl_day.csv"
          end if
        endif

        if (pco%cb_gl_hru%m == "y") then
          call open_output_file(4521, "hru_carb_gl_mon.txt", 8000)
          write (4521,*)  bsn%name, prog
          write (4521,*) carb_gl_hdr
          write (4521,*) carb_gl_hdr_units
          write (9000,*) "HRU                       hru_carb_gl_mon.txt"
          if (pco%csvout == "y") then
            call open_output_file(4525, "hru_carb_gl_mon.csv", 8000)
            write (4525,*)  bsn%name, prog
            write (4525,'(*(G0.6,:,","))') carb_gl_hdr
            write (4525,'(*(G0.6,:,","))') carb_gl_hdr_units
            write (9000,*) "HRU                       hru_carb_gl_mon.csv"
          end if
        endif

        if (pco%cb_gl_hru%y == "y") then
          call open_output_file(4522, "hru_carb_gl_yr.txt", 8000)
          write (4522,*)  bsn%name, prog
          write (4522,*) carb_gl_hdr
          write (4522,*) carb_gl_hdr_units
          write (9000,*) "HRU                       hru_carb_gl_yr.txt"
          if (pco%csvout == "y") then
            call open_output_file(4526, "hru_carb_gl_yr.csv", 8000)
            write (4526,*)  bsn%name, prog
            write (4526,'(*(G0.6,:,","))') carb_gl_hdr
            write (4526,'(*(G0.6,:,","))') carb_gl_hdr_units
            write (9000,*) "HRU                       hru_carb_gl_yr.csv"
          end if
        endif

        if (pco%cb_gl_hru%a == "y") then
          call open_output_file(4523, "hru_carb_gl_aa.txt", 8000)
          write (4523,*)  bsn%name, prog
          write (4523,*) carb_gl_hdr
          write (4523,*) carb_gl_hdr_units
          write (9000,*) "HRU                       hru_carb_gl_aa.txt"
          if (pco%csvout == "y") then
            call open_output_file(4527, "hru_carb_gl_aa.csv", 8000)
            write (4527,*)  bsn%name, prog
            write (4527,'(*(G0.6,:,","))') carb_gl_hdr
            write (4527,'(*(G0.6,:,","))') carb_gl_hdr_units
            write (9000,*) "HRU                       hru_carb_gl_aa.csv"
          end if
        endif


       
!!!NEW SOIL TRANSFORMATIONS CARBON OUTPUT
        if (pco%cb_trf_hru%d == "y") then
          call open_output_file(4550, "hru_scf_day.txt", 8000)
          write (4550,*)  bsn%name, prog
          write (4550,*) hscf_hdr
          write (4550,*) hscf_hdr_units
              write (9000,*) "HRU                       hru_scf_day.txt"          
             if (pco%csvout == "y") then
              call open_output_file(4554, "hru_scf_day.csv", 8000)
              write (4554,*)  bsn%name, prog
              write (4554,'(*(G0.6,:,","))') hscf_hdr
              write (4554,'(*(G0.6,:,","))') hscf_hdr_units
              write (9000,*) "HRU                       hru_scf_day.csv"
            end if
        endif
        
        if (pco%cb_trf_hru%m == "y") then
          call open_output_file(4551, "hru_scf_mon.txt", 8000)
          write (4551,*)  bsn%name, prog
          write (4551,*) hscf_hdr
          write (4551,*) hscf_hdr_units
          write (9000,*) "HRU                       hru_scf_mon.txt"
            if (pco%csvout == "y") then
              call open_output_file(4555, "hru_scf_mon.csv", 8000)
              write (4555,*)  bsn%name, prog
              write (4555,'(*(G0.6,:,","))') hscf_hdr
              write (4555,'(*(G0.6,:,","))') hscf_hdr_units
              write (9000,*) "HRU                       hru_scf_mon.csv"
            end if
        endif
        
     if (pco%cb_trf_hru%y == "y") then
          call open_output_file(4552, "hru_scf_yr.txt", 8000)
          write (4552,*)  bsn%name, prog
          write (4552,*) hscf_hdr
          write (4552,*) hscf_hdr_units
          write (9000,*) "HRU                       hru_scf_yr.txt"
            if (pco%csvout == "y") then
              call open_output_file(4556, "hru_scf_yr.csv", 8000)
              write (4556,*)  bsn%name, prog
              write (4556,'(*(G0.6,:,","))') hscf_hdr
              write (4556,'(*(G0.6,:,","))') hscf_hdr_units
              write (9000,*) "HRU                       hru_scf_yr.csv"
            end if
        endif
        
        if (pco%cb_trf_hru%a == "y") then
          call open_output_file(4553, "hru_scf_aa.txt", 8000)
          write (4553,*)  bsn%name, prog
          write (4553,*) hscf_hdr
          write (4553,*) hscf_hdr_units
          write (9000,*) "HRU                       hru_scf_aa.txt"
            if (pco%csvout == "y") then
              call open_output_file(4557, "hru_scf_aa.csv", 8000)
              write (4557,*)  bsn%name, prog
              write (4557,'(*(G0.6,:,","))') hscf_hdr
              write (4557,'(*(G0.6,:,","))') hscf_hdr_units
              write (9000,*) "HRU                       hru_scf_aa.csv"
            end if
        endif
        
 !!!NEW SOIL TRANSFORMATIONS CARBON OUTPUT     
       
!!! NEW SOILC_STAT/RESC_STAT/PLC_STAT CARBON OUTPUT FILES

        !! per-family carbon stat files, each with own print.prt flag (10 rows in hru_cb_* namespace).
        !! Unit allocation (txt then csv):
        !!   hru_carb_gl        4520-4523 / 4524-4527  (HRU C gain/loss; 21 vars, no layers)
        !!   hru_cbn_lyr        4530-4533 / 4534-4537  (per-layer SOC totals + sequestered)
        !!   hru_cpool_stat     4538-4541 / 4542-4545  (per-layer C pools; 10 vars)
        !!   hru_scf            4550-4553 / 4554-4557  (HRU C transformations; 13 vars, no layers)
        !!   hru_cflux_stat     4558-4561 / 4562-4565  (per-layer C and N fluxes; 37 vars)
        !!   hru_n_p_pool_stat  4566-4569 / 4570-4573  (per-layer N+P pools; 18 vars)
        !!   hru_plc_stat       4574-4577 / 4578-4581  (plant carbon state; 7 vars, no layers)
        !!   hru_carb_drv       4582-4585 / 4586-4589  (per-layer drivers; 14 vars)
        !!   hru_carb_dyn       4590-4593 / 4594-4597  (per-layer dynamics; 21 vars)
        !!   hru_soil_snap      4598-4601 / 4602-4605  (per-layer soil snapshot; _tot has begsim+endsim rows)
        !! Within each block, offsets are: day, mon, yr, aa.

        !! hru_cbn_lyr: per-layer SOC totals + sequestered. Header written once at file open.
        if (pco%cb_lyr_hru%d == "y") call open_cb_banner_pair(4530, 4534, "hru_cbn_lyr_day.txt", "hru_cbn_lyr_day.csv", "total soil carbon (Mg/ha) by layer depth in mm")
        if (pco%cb_lyr_hru%m == "y") call open_cb_banner_pair(4531, 4535, "hru_cbn_lyr_mon.txt", "hru_cbn_lyr_mon.csv", "total soil carbon (Mg/ha) by layer depth in mm")
        if (pco%cb_lyr_hru%y == "y") call open_cb_banner_pair(4532, 4536, "hru_cbn_lyr_yr.txt",  "hru_cbn_lyr_yr.csv",  "total soil carbon (Mg/ha) by layer depth in mm")
        if (pco%cb_lyr_hru%a == "y") call open_cb_banner_pair(4533, 4537, "hru_cbn_lyr_aa.txt",  "hru_cbn_lyr_aa.csv",  "total soil carbon (Mg/ha) by layer depth in mm")

        !! hru_n_p_pool_stat: per-layer N and P content of carbon pools.
        if (pco%cb_npool_hru%d == "y") call open_cb_wide_pair(4566, 4570, "hru_n_p_pool_stat_day.txt", "hru_n_p_pool_stat_day.csv", n_p_pool_vars)
        if (pco%cb_npool_hru%m == "y") call open_cb_wide_pair(4567, 4571, "hru_n_p_pool_stat_mon.txt", "hru_n_p_pool_stat_mon.csv", n_p_pool_vars)
        if (pco%cb_npool_hru%y == "y") call open_cb_wide_pair(4568, 4572, "hru_n_p_pool_stat_yr.txt",  "hru_n_p_pool_stat_yr.csv",  n_p_pool_vars)
        if (pco%cb_npool_hru%a == "y") call open_cb_wide_pair(4569, 4573, "hru_n_p_pool_stat_aa.txt",  "hru_n_p_pool_stat_aa.csv",  n_p_pool_vars)

        !! hru_soil_snap: per-layer soil properties. _day/_mon/_yr emit end-of-period rows; _tot emits begsim+endsim.
        if (pco%cb_snap_hru%d == "y") call open_cb_wide_pair(4598, 4602, "hru_soil_snap_day.txt", "hru_soil_snap_day.csv", soil_snap_vars)
        if (pco%cb_snap_hru%m == "y") call open_cb_wide_pair(4599, 4603, "hru_soil_snap_mon.txt", "hru_soil_snap_mon.csv", soil_snap_vars)
        if (pco%cb_snap_hru%y == "y") call open_cb_wide_pair(4600, 4604, "hru_soil_snap_yr.txt",  "hru_soil_snap_yr.csv",  soil_snap_vars)
        !! _tot file gated specifically by cb_snap_hru%a (begsim + endsim rows).
        if (pco%cb_snap_hru%a == "y") then
          call open_cb_wide_pair(4601, 4605, "hru_soil_snap_tot.txt", "hru_soil_snap_tot.csv", soil_snap_vars)
          if (bsn_cc%cswat == 2) call soil_nutcarb_write(" b")  !! emit begsim row to hru_soil_snap_tot
        end if

        !! hru_plc_stat: HRU-level plant carbon state (no layers).
        if (pco%cb_plt_hru%d == "y") call open_cb_flat_pair(4574, 4578, "hru_plc_stat_day.txt", "hru_plc_stat_day.csv", ["total_c    ","ab_gr_c    ","leaf_c     ","stem_c     ","seed_c     ","root_c     ","surf_rsd_c "])
        if (pco%cb_plt_hru%m == "y") call open_cb_flat_pair(4575, 4579, "hru_plc_stat_mon.txt", "hru_plc_stat_mon.csv", ["total_c    ","ab_gr_c    ","leaf_c     ","stem_c     ","seed_c     ","root_c     ","surf_rsd_c "])
        if (pco%cb_plt_hru%y == "y") call open_cb_flat_pair(4576, 4580, "hru_plc_stat_yr.txt",  "hru_plc_stat_yr.csv",  ["total_c    ","ab_gr_c    ","leaf_c     ","stem_c     ","seed_c     ","root_c     ","surf_rsd_c "])
        if (pco%cb_plt_hru%a == "y") call open_cb_flat_pair(4577, 4581, "hru_plc_stat_aa.txt",  "hru_plc_stat_aa.csv",  ["total_c    ","ab_gr_c    ","leaf_c     ","stem_c     ","seed_c     ","root_c     ","surf_rsd_c "])

        !! The remaining 4 families require the carbon model (cswat==2).
        if (bsn_cc%cswat == 2) then
          !! hru_cflux_stat: per-layer C and N fluxes (37 vars)
          if (pco%cb_flux_hru%d == "y") call open_cb_wide_pair(4558, 4562, "hru_cflux_stat_day.txt", "hru_cflux_stat_day.csv", cflux_vars)
          if (pco%cb_flux_hru%m == "y") call open_cb_wide_pair(4559, 4563, "hru_cflux_stat_mon.txt", "hru_cflux_stat_mon.csv", cflux_vars)
          if (pco%cb_flux_hru%y == "y") call open_cb_wide_pair(4560, 4564, "hru_cflux_stat_yr.txt",  "hru_cflux_stat_yr.csv",  cflux_vars)
          if (pco%cb_flux_hru%a == "y") call open_cb_wide_pair(4561, 4565, "hru_cflux_stat_aa.txt",  "hru_cflux_stat_aa.csv",  cflux_vars)

          !! hru_cpool_stat: per-layer C pools (10 vars)
          if (pco%cb_cpool_hru%d == "y") call open_cb_wide_pair(4538, 4542, "hru_cpool_stat_day.txt", "hru_cpool_stat_day.csv", cpool_vars)
          if (pco%cb_cpool_hru%m == "y") call open_cb_wide_pair(4539, 4543, "hru_cpool_stat_mon.txt", "hru_cpool_stat_mon.csv", cpool_vars)
          if (pco%cb_cpool_hru%y == "y") call open_cb_wide_pair(4540, 4544, "hru_cpool_stat_yr.txt",  "hru_cpool_stat_yr.csv",  cpool_vars)
          if (pco%cb_cpool_hru%a == "y") call open_cb_wide_pair(4541, 4545, "hru_cpool_stat_aa.txt",  "hru_cpool_stat_aa.csv",  cpool_vars)

          !! hru_carb_drv: per-layer environmental drivers (14 vars)
          if (pco%cb_drv_hru%d == "y") call open_cb_wide_pair(4582, 4586, "hru_carb_drv_day.txt", "hru_carb_drv_day.csv", carb_drv_vars)
          if (pco%cb_drv_hru%m == "y") call open_cb_wide_pair(4583, 4587, "hru_carb_drv_mon.txt", "hru_carb_drv_mon.csv", carb_drv_vars)
          if (pco%cb_drv_hru%y == "y") call open_cb_wide_pair(4584, 4588, "hru_carb_drv_yr.txt",  "hru_carb_drv_yr.csv",  carb_drv_vars)
          if (pco%cb_drv_hru%a == "y") call open_cb_wide_pair(4585, 4589, "hru_carb_drv_aa.txt",  "hru_carb_drv_aa.csv",  carb_drv_vars)

          !! hru_carb_dyn: per-layer pool dynamics (21 vars)
          if (pco%cb_dyn_hru%d == "y") call open_cb_wide_pair(4590, 4594, "hru_carb_dyn_day.txt", "hru_carb_dyn_day.csv", carb_dyn_vars)
          if (pco%cb_dyn_hru%m == "y") call open_cb_wide_pair(4591, 4595, "hru_carb_dyn_mon.txt", "hru_carb_dyn_mon.csv", carb_dyn_vars)
          if (pco%cb_dyn_hru%y == "y") call open_cb_wide_pair(4592, 4596, "hru_carb_dyn_yr.txt",  "hru_carb_dyn_yr.csv",  carb_dyn_vars)
          if (pco%cb_dyn_hru%a == "y") call open_cb_wide_pair(4593, 4597, "hru_carb_dyn_aa.txt",  "hru_carb_dyn_aa.csv",  carb_dyn_vars)
        end if

        !! legacy CSU carbon files (hru_cb / hru_cb_vars rows in print.prt).
        !! opens the old fixed-column files and writes the begsim soil snapshot.
        !! will be removed in revision 63.
        call carbon_legacy_open


!! basin_carbon_all.txt removed: yearly-only basin sum reconstructable from HRU-level files
        
!!!  HRU - Losses
        if (pco%ls_hru%d == "y") then
          call open_output_file(2030, "hru_ls_day.txt", 1500)
          write (2030,*) bsn%name, prog
          write (2030,*) ls_hdr    !! hru
          write (2030,*) ls_hdr_units
          write (9000,*) "HRU                       hru_ls_day.txt"
            if (pco%csvout == "y") then
              call open_output_file(2034, "hru_ls_day.csv", 1500)
              write (2034,*) bsn%name, prog
              write (2034,'(*(G0.6,:,","))') ls_hdr    !! hru
              write (2034,'(*(G0.6,:,","))') ls_hdr_units
              write (9000,*) "HRU                       hru_ls_day.csv"
            end if 
        endif
        
                
!!  hru_nut_carb_gl_* files removed: header-only files with no data writes; redundant with hru_ls and the dedicated carbon output files

       if (pco%ls_hru%m == "y") then
        call open_output_file(2031, "hru_ls_mon.txt", 1500)
        write (2031,*) bsn%name, prog
        write (2031,*) ls_hdr  !! hru 
        write (2031,*) ls_hdr_units
        write (9000,*) "HRU                       hru_ls_mon.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2035, "hru_ls_mon.csv", 1500)
            write (2035,*) bsn%name, prog
            write (2035,'(*(G0.6,:,","))') ls_hdr  !! hru
            write (2035,'(*(G0.6,:,","))') ls_hdr_units
            write (9000,*) "HRU                       hru_ls_mon.csv"
          end if
       endif
          
        if (pco%ls_hru%y == "y") then
          call open_output_file(2032, "hru_ls_yr.txt", 1500)
          write (2032,*) bsn%name, prog
          write (2032,*) ls_hdr    !! hru
          write (2032,*) ls_hdr_units
          write (9000,*) "HRU                       hru_ls_yr.txt"
            if (pco%csvout == "y") then
              call open_output_file(2036, "hru_ls_yr.csv", 1500)
              write (2036,*) bsn%name, prog
              write (2036,'(*(G0.6,:,","))') ls_hdr    !! hru
              write (2036,'(*(G0.6,:,","))') ls_hdr_units
              write (9000,*) "HRU                       hru_ls_yr.csv"
            end if 
        endif
        
       if (pco%ls_hru%a == "y") then
        call open_output_file(2033, "hru_ls_aa.txt", 1500)
        write (2033,*) bsn%name, prog
        write (2033,*) ls_hdr  !! hru
        write (2033,*) ls_hdr_units
        write (9000,*) "HRU                       hru_ls_aa.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2037, "hru_ls_aa.csv", 1500)
            write (2037,*) bsn%name, prog
            write (2037,'(*(G0.6,:,","))') ls_hdr  !! hru
            write (2037,'(*(G0.6,:,","))') ls_hdr_units
            write (9000,*) "HRU                       hru_ls_aa.csv"
          end if 
       end if

!!!  HRU - Plant/Weather
        if (pco%pw_hru%d == "y") then
          call open_output_file(2040, "hru_pw_day.txt", 1500)
          write (2040,*) bsn%name, prog
          write (2040,*) pw_hdr  !! hru 
          write (2040,*) pw_hdr_units
          write (9000,*) "HRU                       hru_pw_day.txt"
            if (pco%csvout == "y") then 
              call open_output_file(2044, "hru_pw_day.csv", 1500)
              write (2044,*) bsn%name, prog
              write (2044,'(*(G0.6,:,","))') pw_hdr  !! hru
              write (2044,'(*(G0.6,:,","))') pw_hdr_units
              write (9000,*) "HRU                       hru_pw_day.csv"
            end if 
        endif
        
      if (pco%pw_hru%m == "y") then
        call open_output_file(2041, "hru_pw_mon.txt", 1500)
        write (2041,*) bsn%name, prog
        write (2041,*) pw_hdr  !! hru
        write (2041,*) pw_hdr_units
        write (9000,*) "HRU                       hru_pw_mon.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2045, "hru_pw_mon.csv", 1500)
            write (2045,*) bsn%name, prog
            write (2045,'(*(G0.6,:,","))') pw_hdr  !! hru
            write (2045,'(*(G0.6,:,","))') pw_hdr_units
            write (9000,*) "HRU                       hru_pw_mon.csv"
          end if 
      endif
      
        if (pco%pw_hru%y == "y") then
          call open_output_file(2042, "hru_pw_yr.txt", 1500)
          write (2042,*) bsn%name, prog                  
          write (2042,*) pw_hdr  !! hru
          write (2042,*) pw_hdr_units
          write (9000,*) "HRU                       hru_pw_yr.txt"
            if (pco%csvout == "y") then 
              call open_output_file(2046, "hru_pw_yr.csv", 1500)
              write (2046,*) bsn%name, prog
              write (2046,'(*(G0.6,:,","))') pw_hdr  !! hru
              write (2046,'(*(G0.6,:,","))') pw_hdr_units
              write (9000,*) "HRU                       hru_pw_yr.csv"
            end if 
        endif
        
       if (pco%pw_hru%a == "y") then
        call open_output_file(2043, "hru_pw_aa.txt", 1500)
        write (2043,*) bsn%name, prog
        write (2043,*) pw_hdr  !! hru
        write (2043,*) pw_hdr_units
        write (9000,*) "HRU                       hru_pw_aa.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2047, "hru_pw_aa.csv", 1500)
            write (2047,*) bsn%name, prog
            write (2047,'(*(G0.6,:,","))') pw_hdr  !! hru
            write (2047,'(*(G0.6,:,","))') pw_hdr_units
            write (9000,*) "HRU                       hru_pw_aa.csv"
          end if 
       endif
      end if 

 !!! SWAT-DEG - Water Balance 
  
     if (sp_ob%hru_lte > 0) then
        if (pco%wb_sd%d == "y") then
          call open_output_file(2300, "hru-lte_wb_day.txt", 1500)
          write (2300,*) bsn%name, prog
          write (2300,*) wb_hdr  !! swat-deg
          write (2300,*) wb_hdr_units
          write (9000,*) "SWAT-DEG                  hru-lte_wb_day.txt"
            if (pco%csvout == "y") then 
              call open_output_file(2304, "hru-lte_wb_day.csv", 1500)
              write (2304,*) bsn%name, prog
              write (2304,'(*(G0.6,:,","))') wb_hdr  !! swat-deg
              write (2304,'(*(G0.6,:,","))') wb_hdr_units
              write (9000,*) "SWAT-DEG                  hru-lte_wb_day.csv"
            end if 
        endif

                
      if (pco%wb_sd%m == "y") then
      call open_output_file(2301, "hru-lte_wb_mon.txt", 1500)
        write (2301,*) bsn%name, prog
        write (2301,*) wb_hdr   !! swat deg 
        write (2301,*) wb_hdr_units
        write (9000,*) "SWAT-DEG                  hru-lte_wb_mon.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2305, "hru-lte_wb_mon.csv", 1500)
            write (2305,*) bsn%name, prog
            write (2305,'(*(G0.6,:,","))') wb_hdr   !! swat deg
            write (2305,'(*(G0.6,:,","))') wb_hdr_units
            write (9000,*) "SWAT-DEG                  hru-lte_wb_mon.csv"
          end if
      end if
          
       
     if (sp_ob%hru_lte > 0) then   
        if (pco%wb_sd%y == "y") then
          call open_output_file(2302, "hru-lte_wb_yr.txt", 1500)
          write (2302,*) bsn%name, prog
          write (2302,*) wb_hdr  !! swat-deg
          write (2302,*) wb_hdr_units
          write (9000,*) "SWAT-DEG                  hru-lte_wb_yr.txt"
            if (pco%csvout == "y") then 
              call open_output_file(2306, "hru-lte_wb_yr.csv", 1500)
              write (2306,*) bsn%name, prog
              write (2306,'(*(G0.6,:,","))') wb_hdr  !! swat-deg
              write (2306,'(*(G0.6,:,","))') wb_hdr_units
              write (9000,*) "SWAT-DEG                  hru-lte_wb_yr.csv"
            end if 
        endif
      end if 
        
        
      if (pco%wb_sd%a == "y") then
        call open_output_file(2303, "hru-lte_wb_aa.txt", 1500)
        write (2303,*) bsn%name, prog
        write (2303,*) wb_hdr   !! swat deg
        write (2303,*) wb_hdr_units
        write (9000,*) "SWAT-DEG                  hru-lte_wb_aa.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2307, "hru-lte_wb_aa.csv", 1500)
            write (2307,*) bsn%name, prog
            write (2307,'(*(G0.6,:,","))') wb_hdr   !! swat deg
            write (2307,'(*(G0.6,:,","))') wb_hdr_units
            write (9000,*) "SWAT-DEG                  hru-lte_wb_aa.csv"
          end if
      end if

!!!  SWAT-DEG - Nutrient Balance
!       open (4101,file="nutbal.sd", recl = 1500)  !! no nuts in SWAT-DEG
!       write (4101,*) nb_hdr
!       open (4105,file="nutbal_aa.sd", recl = 1500)
!       write (4105,*) nb_hdr
!       if (pco%csvout == "y") then 
!         call open_output_file(4025, "nutbal_sd.csv", 1500)  !! no nuts in SWAT-DEG
!         write (4025,*) nb_hdr
!         call open_output_file(4026, "nutbal_aa_sd.csv", 1500)
!         write (4026,*) nb_hdr
!       end if 

!!!  SWAT-DEG - Losses
        if (pco%ls_sd%d == "y") then
          call open_output_file(2440, "hru-lte_ls_day.txt", 1500)
          write (2440,*) bsn%name, prog
          write (2440,*) ls_hdr    !! swat-deg
          write (2440,*) ls_hdr_units
          write (9000,*) "SWAT-DEG                  hru-lte_ls_day.txt"
            if (pco%csvout == "y") then 
              call open_output_file(2444, "hru-lte_ls_day.csv", 1500)
              write (2444,*) bsn%name, prog
              write (2444,'(*(G0.6,:,","))') ls_hdr    !! swat-deg 
              write (2444,'(*(G0.6,:,","))') ls_hdr_units
              write (9000,*) "SWAT-DEG                  hru-lte_ls_day.csv"
            end if 
        endif
        
      if (pco%ls_sd%m == "y") then
        call open_output_file(2441, "hru-lte_ls_mon.txt", 1500)
        write (2441,*) bsn%name, prog
        write (2441,*) ls_hdr  !! swat-deg
        write (2441,*) ls_hdr_units
        write (9000,*) "SWAT-DEG                  hru-lte_ls_mon.txt"
        if (pco%csvout == "y") then 
          call open_output_file(2445, "hru-lte_ls_mon.csv", 1500)
          write (2445,*) bsn%name, prog
          write (2445,'(*(G0.6,:,","))') ls_hdr  !! swat-deg
          write (2445,'(*(G0.6,:,","))') ls_hdr_units
          write (9000,*) "SWAT-DEG                  hru-lte_ls_mon.csv"
        end if
      end if
        
        if (pco%ls_sd%y == "y") then
          call open_output_file(2442, "hru-lte_ls_yr.txt", 1500)
          write (2442,*) bsn%name, prog
          write (2442,*) ls_hdr    !! swat-deg
          write (2442,*) ls_hdr_units
          write (9000,*) "SWAT-DEG                  hru-lte_ls_yr.txt"
            if (pco%csvout == "y") then 
              call open_output_file(2446, "hru-lte_ls_yr.csv", 1500)
              write (2446,*) bsn%name, prog
              write (2446,'(*(G0.6,:,","))') ls_hdr    !! swat-deg 
              write (2446,'(*(G0.6,:,","))') ls_hdr_units
              write (9000,*) "SWAT-DEG                  hru-lte_ls_yr.csv"
            end if 
        endif
        
      if (pco%ls_sd%a == "y") then
         call open_output_file(2443, "hru-lte_ls_aa.txt", 1500)
         write (2443,*) bsn%name, prog
         write (2443,*) ls_hdr  !! swat-deg
         write (2443,*) ls_hdr_units
         write (9000,*) "SWAT-DEG                  hru-lte_ls_aa.txt"
        if (pco%csvout == "y") then 
          call open_output_file(2447, "hru-lte_ls_aa.csv", 1500)
          write (2447,*) bsn%name, prog
          write (2447,'(*(G0.6,:,","))') ls_hdr  !! swat-deg
          write (2447,'(*(G0.6,:,","))') ls_hdr_units
          write (9000,*) "SWAT-DEG                  hru-lte_ls_aa.csv"
        end if
      end if 
        
        
!!!  SWAT-DEG - Plant/Weather
        if (pco%pw_sd%d == "y") then
          call open_output_file(2460, "hru-lte_pw_day.txt", 1500)
          write (2460,*) bsn%name, prog
          write (2460,*) pw_hdr  !! swat-deg
          write (2460,*) pw_hdr_units
          write (9000,*) "SWAT-DEG                  hru-lte_pw_day.txt"
           if (pco%csvout == "y") then 
             call open_output_file(2464, "hru-lte_pw_day.csv", 1500)
             write (2464,*) bsn%name, prog
             write (2464,'(*(G0.6,:,","))') pw_hdr  !! swat-deg
             write (2464,'(*(G0.6,:,","))') pw_hdr_units
             write (9000,*) "SWAT-DEG                  hru-lte_pw_day.csv"
           end if
        endif
        
        if (pco%pw_sd%m == "y") then
          call open_output_file(2461, "hru-lte_pw_mon.txt", 1500)
          write (2461,*) bsn%name, prog
          write (2461,*) pw_hdr !! swat-deg
          write (2461,*) pw_hdr_units
          write (9000,*) "SWAT-DEG                  hru-lte_pw_mon.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2465, "hru-lte_pw_mon.csv", 1500)
            write (2465,*) bsn%name, prog
            write (2465,'(*(G0.6,:,","))') pw_hdr !! swat-deg
            write (2465,'(*(G0.6,:,","))') pw_hdr_units
            write (9000,*) "SWAT-DEG                  hru-lte_pw_mon.csv"
          end if
        end if

       if (pco%pw_sd%y == "y") then
          call open_output_file(2462, "hru-lte_pw_yr.txt", 1500)
          write (2462,*) bsn%name, prog
          write (2462,*) pw_hdr  !! swat-deg
          write (2462,*) pw_hdr_units
          write (9000,*) "SWAT-DEG                  hru-lte_pw_yr.txt"
           if (pco%csvout == "y") then 
             call open_output_file(2466, "hru-lte_pw_yr.csv", 1500)
             write (2466,*) bsn%name, prog
             write (2466,'(*(G0.6,:,","))') pw_hdr  !! swat-deg
             write (2466,'(*(G0.6,:,","))') pw_hdr_units
             write (9000,*) "SWAT-DEG                  hru-lte_pw_yr.csv"
           end if
       endif
        
      if (pco%pw_sd%a == "y") then    !!!
        call open_output_file(2463, "hru-lte_pw_aa.txt", 1500)
        write (2463,*) bsn%name, prog
        write (2463,*) pw_hdr !! swat-deg
        write (2463,*) pw_hdr_units
        write (9000,*) "SWAT-DEG                  hru-lte_pw_aa.txt"
         if (pco%csvout == "y") then 
          call open_output_file(2467, "hru-lte_pw_aa.csv", 1500)
          write (2467,*) bsn%name, prog
          write (2467,'(*(G0.6,:,","))') pw_hdr !! swat-deg
          write (2467,'(*(G0.6,:,","))') pw_hdr_units
          write (9000,*) "SWAT-DEG                  hru-lte_pw_aa.csv"
        end if 
      endif
      end if  
      
!!!  Water Balance
      if (db_mx%lsu_out > 0) then   !! Water Balance  
        if (pco%wb_lsu%d == "y") then
          call open_output_file(2140, "lsunit_wb_day.txt", 1500)
          write (2140,*) bsn%name, prog
          write (2140,*) wb_hdr  !! subbasin
          write (2140,*) wb_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_wb_day.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2144, "lsunit_wb_day.csv", 1500)
            write (2144,*) bsn%name, prog
            write (2144,'(*(G0.6,:,","))') wb_hdr  !! subbasin
            write (2144,'(*(G0.6,:,","))') wb_hdr_units
            write (9000,*) "ROUTING_UNIT              lsunit_wb_day.csv"
          end if 
        endif
    
        
       if (pco%wb_lsu%m == "y") then
        call open_output_file(2141, "lsunit_wb_mon.txt", 1500)
        write (2141,*) bsn%name, prog
        write (2141,*) wb_hdr  !! subbasin
        write (2141,*) wb_hdr_units
        write (9000,*) "ROUTING_UNIT              lsunit_wb_mon.txt"
          if (pco%csvout == "y") then
            call open_output_file(2145, "lsunit_wb_mon.csv", 1500)
            write (2145,*) bsn%name, prog
            write (2145,'(*(G0.6,:,","))') wb_hdr   !! subbasin
            write (2145,'(*(G0.6,:,","))') wb_hdr_units
            write (9000,*) "ROUTING_UNIT              lsunit_wb_mon.csv"
          end if
        end if 

     if (sp_ob%ru > 0) then   
        if (pco%wb_lsu%y == "y") then
          call open_output_file(2142, "lsunit_wb_yr.txt", 1500)
          write (2142,*) bsn%name, prog
          write (2142,*) wb_hdr  !! subbasin
          write (2142,*) wb_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_wb_yr.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2146, "lsunit_wb_yr.csv", 1500)
            write (2146,*) bsn%name, prog
            write (2146,'(*(G0.6,:,","))') wb_hdr  !! subbasin
            write (2146,'(*(G0.6,:,","))') wb_hdr_units
            write (9000,*) "ROUTING_UNIT              lsunit_wb_yr.csv"
          end if 
        endif
     end if
        
       if (pco%wb_lsu%a == "y") then
         call open_output_file(2143, "lsunit_wb_aa.txt", 1500)
         write (2143,*) bsn%name, prog
         write (2143,*) wb_hdr   !! subbasin
         write (2143,*) wb_hdr_units
         write (9000,*) "ROUTING_UNIT              lsunit_wb_aa.txt"
          if (pco%csvout == "y") then
           call open_output_file(2147, "lsunit_wb_aa.csv", 1500)
           write (2147,*) bsn%name, prog
           write (2147,'(*(G0.6,:,","))') wb_hdr   !! subbasin
           write (2147,'(*(G0.6,:,","))') wb_hdr_units
           write (9000,*) "ROUTING_UNIT              lsunit_wb_aa.csv"
          end if
       end if
        
!!!  Nutrient Balance
        if (pco%nb_lsu%d == "y") then
          call open_output_file(2150, "lsunit_nb_day.txt", 1500)
          write (2150,*) bsn%name, prog
          write (2150,*) nb_hdr
          write (2150,*) nb_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_nb_day.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2154, "lsunit_nb_day.csv", 1500)
            write (2154,*) bsn%name, prog
            write (2154,'(*(G0.6,:,","))') nb_hdr
            write (2154,'(*(G0.6,:,","))') nb_hdr_units
            write (9000,*) "ROUTING_UNIT              lsunit_nb_day.csv"
          end if 
        endif
        
        if (pco%nb_lsu%m == "y") then
        call open_output_file(2151, "lsunit_nb_mon.txt", 1500)
        write (2151,*) bsn%name, prog
        write (2151,*) nb_hdr
        write (2151,*) nb_hdr_units
        write (9000,*) "ROUTING_UNIT              lsunit_nb_mon.txt"
          if (pco%csvout == "y") then
            call open_output_file(2155, "lsunit_nb_mon.csv", 1500)
            write (2155,*) bsn%name, prog
            write (2155,'(*(G0.6,:,","))') nb_hdr
            write (2155,'(*(G0.6,:,","))') nb_hdr_units
            write (9000,*) "ROUTING_UNIT              lsunit_nb_mon.csv"
          end if
        end if
        
        if (pco%nb_lsu%y == "y") then
          call open_output_file(2152, "lsunit_nb_yr.txt", 1500)
          write (2152,*) bsn%name, prog
          write (2152,*) nb_hdr
          write (2152,*) nb_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_nb_yr.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2156, "lsunit_nb_yr.csv", 1500)
            write (2156,*) bsn%name, prog
            write (2156,'(*(G0.6,:,","))') nb_hdr
            write (2156,'(*(G0.6,:,","))') nb_hdr_units
            write (9000,*) "ROUTING_UNIT              lsunit_nb_yr.csv"
          end if 
        endif
        
        if (pco%nb_lsu%a == "y") then
        call open_output_file(2153, "lsunit_nb_aa.txt", 1500)
        write (2153,*) bsn%name, prog
        write (2153,*) nb_hdr
        write (2153,*) nb_hdr_units
        write (9000,*) "ROUTING_UNIT              lsunit_nb_aa.txt"
          if (pco%csvout == "y") then
            call open_output_file(2157, "lsunit_nb_aa.csv", 1500)
            write (2157,*) bsn%name, prog
            write (2157,'(*(G0.6,:,","))') nb_hdr
            write (2157,'(*(G0.6,:,","))') nb_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_nb_aa.csv"
          end if 
        end if 

!!!  Losses
        if (pco%ls_lsu%d == "y") then
          call open_output_file(2160, "lsunit_ls_day.txt", 1500)
          write (2160,*) bsn%name, prog
          write (2160,*) ls_hdr 
          write (2160,*) ls_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_ls_day.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2164, "lsunit_ls_day.csv", 1500)
            write (2164,*) bsn%name, prog
            write (2164,'(*(G0.6,:,","))') ls_hdr    !! subbasin
            write (2164,'(*(G0.6,:,","))') ls_hdr_units
            write (9000,*) "ROUTING_UNIT              lsunit_ls_day.csv"
          end if 
        endif
        
      if (pco%ls_lsu%m == "y") then
        call open_output_file(2161, "lsunit_ls_mon.txt", 1500)
        write (2161,*) bsn%name, prog
        write (2161,*) ls_hdr 
        write (2161,*) ls_hdr_units
        write (9000,*) "ROUTING_UNIT              lsunit_ls_mon.txt"
        if (pco%csvout == "y") then 
          call open_output_file(2165, "lsunit_ls_mon.csv", 1500)
          write (2165,*) bsn%name, prog
          write (2165,'(*(G0.6,:,","))') ls_hdr  !! subbasin 
          write (2165,'(*(G0.6,:,","))') ls_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_ls_mon.csv"
        end if 
      end if 
        
       if (pco%ls_lsu%y == "y") then
          call open_output_file(2162, "lsunit_ls_yr.txt", 1500)
          write (2162,*) bsn%name, prog
          write (2162,*) ls_hdr 
          write (2162,*) ls_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_ls_yr.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2166, "lsunit_ls_yr.csv", 1500)
            write (2166,*) bsn%name, prog
            write (2166,'(*(G0.6,:,","))') ls_hdr
            write (2166,'(*(G0.6,:,","))') ls_hdr_units
            write (9000,*) "ROUTING_UNIT              lsunit_ls_yr.csv"
          end if 
       endif
        
       if (pco%ls_lsu%a == "y") then
       call open_output_file(2163, "lsunit_ls_aa.txt", 1500)
        write (2163,*) bsn%name, prog
        write (2163,*) ls_hdr 
        write (2163,*) ls_hdr_units
        write (9000,*) "ROUTING_UNIT              lsunit_ls_aa.txt"
        if (pco%csvout == "y") then 
          call open_output_file(2167, "lsunit_ls_aa.csv", 1500)
          write (2167,*) bsn%name, prog
          write (2167,'(*(G0.6,:,","))') ls_hdr
          write (2167,'(*(G0.6,:,","))') ls_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_ls_aa.csv"
        end if 
       end if

!!!  Plant/Weather
        if (pco%pw_lsu%d == "y") then
          call open_output_file(2170, "lsunit_pw_day.txt", 1500)
          write (2170,*) bsn%name, prog
          write (2170,*) pw_hdr
          write (2170,*) pw_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_pw_day.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2174, "lsunit_pw_day.csv", 1500)
            write (2174,*) bsn%name, prog
            write (2174,'(*(G0.6,:,","))') pw_hdr 
            write (2174,'(*(G0.6,:,","))') pw_hdr_units
            write (9000,*) "ROUTING_UNIT              lsunit_pw_day.csv"
          end if 
        end if 
    
      
      if (pco%pw_lsu%m == "y") then
       call open_output_file(2171, "lsunit_pw_mon.txt", 1500)
        write (2171,*) bsn%name, prog
        write (2171,*) pw_hdr
        write (2171,*) pw_hdr_units
        write (9000,*) "ROUTING_UNIT              lsunit_pw_mon.txt"
        if (pco%csvout == "y") then 
          call open_output_file(2175, "lsunit_pw_mon.csv", 1500)
          write (2175,*) bsn%name, prog
          write (2175,'(*(G0.6,:,","))') pw_hdr
          write (2175,'(*(G0.6,:,","))') pw_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_pw_mon.csv"
        end if
       end if
        
        if (pco%pw_lsu%y == "y") then
          call open_output_file(2172, "lsunit_pw_yr.txt", 1500)
          write (2172,*) bsn%name, prog
          write (2172,*) pw_hdr
          write (2172,*) pw_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_pw_yr.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2176, "lsunit_pw_yr.csv", 1500)
            write (2176,*) bsn%name, prog
            write (2176, '(*(G0.6,:,","))')pw_hdr
            write (2176,'(*(G0.6,:,","))') pw_hdr_units
            write (9000,*) "ROUTING_UNIT              lsunit_pw_yr.csv"
          end if 
        end if 
      
     if (pco%pw_lsu%a == "y") then
       call open_output_file(2173, "lsunit_pw_aa.txt", 1500)
        write (2173,*) bsn%name, prog
        write (2173,*) pw_hdr
        write (2173,*) pw_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_pw_aa.txt"
        if (pco%csvout == "y") then 
          call open_output_file(2177, "lsunit_pw_aa.csv", 1500)
          write (2177,*) bsn%name, prog
          write (2177,'(*(G0.6,:,","))') pw_hdr
          write (2177,'(*(G0.6,:,","))') pw_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_pw_aa.csv"
        end if
     end if
      end if
      
!!!  BASIN - Water balance 
        if (pco%wb_bsn%d == "y") then
          call open_output_file(2050, "basin_wb_day.txt", 1500)
          write (2050,*) bsn%name, prog
          write (2050,*) wb_hdr  !! bsn
          write (2050,*) wb_hdr_units
          write (9000,*) "BASIN                     basin_wb_day.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2054, "basin_wb_day.csv", 1500)
            write (2054,*) bsn%name, prog
            write (2054,'(*(G0.6,:,","))') wb_hdr !! bsn
            write (2054,'(*(G0.6,:,","))') wb_hdr_units
            write (9000,*) "BASIN                     basin_wb_day.csv"
          end if 
        endif
            
       if (pco%wb_bsn%m == "y") then 
        call open_output_file(2051, "basin_wb_mon.txt", 1500)
        write (2051,*) bsn%name, prog
        write (2051,*) wb_hdr  !! bsn
        write (2051,*) wb_hdr_units
        write (9000,*) "BASIN                     basin_wb_mon.txt"
        if (pco%csvout == "y") then 
          call open_output_file(2055, "basin_wb_mon.csv", 1500)
          write (2055,*) bsn%name, prog
          write (2055,'(*(G0.6,:,","))') wb_hdr !! bsn
          write (2055,'(*(G0.6,:,","))') wb_hdr_units
          write (9000,*) "BASIN                     basin_wb_mon.csv"
        end if
       end if 

        if (pco%wb_bsn%y == "y") then
          call open_output_file(2052, "basin_wb_yr.txt", 1500)
          write (2052,*) bsn%name, prog
          write (2052,*) wb_hdr  !! bsn
          write (2052,*) wb_hdr_units
          write (9000,*) "BASIN                     basin_wb_yr.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2056, "basin_wb_yr.csv", 1500)
            write (2056,*) bsn%name, prog
            write (2056,'(*(G0.6,:,","))') wb_hdr !! bsn
            write (2056,'(*(G0.6,:,","))') wb_hdr_units
            write (9000,*) "BASIN                     basin_wb_yr.csv"
          end if 
        endif
        
       if (pco%wb_bsn%a == "y") then 
        call open_output_file(2053, "basin_wb_aa.txt", 1500)
        write (2053,*) bsn%name, prog
        write (2053,*) wb_hdr  !! bsn
        write (2053,*) wb_hdr_units
        write (9000,*) "BASIN                     basin_wb_aa.txt"
        if (pco%csvout == "y") then 
          call open_output_file(2057, "basin_wb_aa.csv", 1500)
          write (2057,*) bsn%name, prog
          write (2057,'(*(G0.6,:,","))') wb_hdr !! bsn
          write (2057,'(*(G0.6,:,","))') wb_hdr_units
          write (9000,*) "BASIN                     basin_wb_aa.csv"
        end if
       end if 

!!!  BASIN - Nutrient balance    
        if (pco%nb_bsn%d == "y") then
          call open_output_file(2060, "basin_nb_day.txt", 1500)
          write (2060,*) bsn%name, prog
          write (2060,*) nb_hdr
          write (2060,*) nb_hdr_units
          write (9000,*) "BASIN                     basin_nb_day.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2064, "basin_nb_day.csv", 1500)
            write (2064,*) bsn%name, prog
            write (2064,'(*(G0.6,:,","))') nb_hdr
            write (2064,'(*(G0.6,:,","))') nb_hdr_units
            write (9000,*) "BASIN                     basin_nb_day.csv"
          end if 
        endif
        
       if (pco%nb_bsn%m == "y") then 
        call open_output_file(2061, "basin_nb_mon.txt", 1500)
        write (2061,*) bsn%name, prog
        write (2061,*) nb_hdr
        write (2061,*) nb_hdr_units
        write (9000,*) "BASIN                     basin_nb_mon.txt"
        if (pco%csvout == "y") then 
          call open_output_file(2065, "basin_nb_mon.csv", 1500)
          write (2065,*) bsn%name, prog
          write (2065,'(*(G0.6,:,","))') nb_hdr
          write (2065,'(*(G0.6,:,","))') nb_hdr_units
          write (9000,*) "BASIN                     basin_nb_mon.csv"
        end if
       end if 

        if (pco%nb_bsn%y == "y") then
          call open_output_file(2062, "basin_nb_yr.txt", 1500)
          write (2062,*) bsn%name, prog
          write (2062,*) nb_hdr
          write (2062,*) nb_hdr_units
          write (9000,*) "BASIN                     basin_nb_yr.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2066, "basin_nb_yr.csv", 1500)
            write (2066,*) bsn%name, prog
            write (2066,'(*(G0.6,:,","))') nb_hdr
            write (2066,'(*(G0.6,:,","))') nb_hdr_units
            write (9000,*) "BASIN                     basin_nb_yr.csv"
          end if 
        endif
        
       if (pco%nb_bsn%a == "y") then 
        call open_output_file(2063, "basin_nb_aa.txt", 1500)
        write (2063,*) bsn%name, prog
        write (2063,*) nb_hdr
        write (2063,*) nb_hdr_units
        write (9000,*) "BASIN                     basin_nb_aa.txt"
        if (pco%csvout == "y") then 
          call open_output_file(2067, "basin_nb_aa.csv", 1500)
          write (2067,*) bsn%name, prog
          write (2067,'(*(G0.6,:,","))') nb_hdr
          write (2067,'(*(G0.6,:,","))') nb_hdr_units
          write (9000,*) "BASIN                     basin_nb_aa.csv"
        end if
       end if 
                
!!!  BASIN - Losses
        if (pco%ls_bsn%d == "y") then
          call open_output_file(2070, "basin_ls_day.txt", 1500)
          write (2070,*) bsn%name, prog
          write (2070,*) ls_hdr    !! bsn
          write (2070,*) ls_hdr_units
          write (9000,*) "BASIN                     basin_ls_day.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2074, "basin_ls_day.csv", 1500)
            write (2074,*) bsn%name, prog
            write (2074,'(*(G0.6,:,","))') ls_hdr    !! bsn
            write (2074,'(*(G0.6,:,","))') ls_hdr_units
            write (9000,*) "BASIN                     basin_ls_day.csv"
          end if 
        endif
        
       if (pco%ls_bsn%m == "y") then
        call open_output_file(2071, "basin_ls_mon.txt", 1500)
        write (2071,*) bsn%name, prog
        write (2071,*) ls_hdr     !! bsn
        write (2071,*) ls_hdr_units
        write (9000,*) "BASIN                     basin_ls_mon.txt"
        if (pco%csvout == "y") then 
          call open_output_file(2075, "basin_ls_mon.csv", 1500)
          write (2075,*) bsn%name, prog
          write (2075,'(*(G0.6,:,","))') ls_hdr     !! bsn
          write (2075,'(*(G0.6,:,","))') ls_hdr_units
          write (9000,*) "BASIN                     basin_ls_mon.csv"
        end if
       end if

        if (pco%ls_bsn%y == "y") then
          call open_output_file(2072, "basin_ls_yr.txt", 1500)
          write (2072,*) bsn%name, prog
          write (2072,*) ls_hdr    !! bsn
          write (2072,*) ls_hdr_units
          write (9000,*) "BASIN                     basin_ls_yr.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2076, "basin_ls_yr.csv", 1500)
            write (2076,*) bsn%name, prog
            write (2076,'(*(G0.6,:,","))') ls_hdr    !! bsn
            write (2076,'(*(G0.6,:,","))') ls_hdr_units
            write (9000,*) "BASIN                     basin_ls_yr.csv"
          end if 
        endif
        
       if (pco%ls_bsn%a == "y") then
        call open_output_file(2073, "basin_ls_aa.txt", 1500)
        write (2073,*) bsn%name, prog
        write (2073,*) ls_hdr     !! bsn
        write (2073,*) ls_hdr_units
        write (9000,*) "BASIN                     basin_ls_aa.txt"
        if (pco%csvout == "y") then 
          call open_output_file(2077, "basin_ls_aa.csv", 1500)
          write (2077,*) bsn%name, prog
          write (2077,'(*(G0.6,:,","))') ls_hdr     !! bsn
          write (2077,'(*(G0.6,:,","))') ls_hdr_units
          write (9000,*) "BASIN                     basin_ls_aa.csv"
        end if
       end if
        
!!!  BASIN - Plant/Weather
        if (pco%pw_bsn%d == "y") then
          call open_output_file(2080, "basin_pw_day.txt", 1500)
          write (2080,*) bsn%name, prog
          write (2080,*) pw_hdr  !! bsn
          write (2080,*) pw_hdr_units
          write (9000,*) "BASIN                     basin_pw_day.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2084, "basin_pw_day.csv", 1500)
            write (2084,*) bsn%name, prog
            write (2084,'(*(G0.6,:,","))') pw_hdr  !! bsn
            write (2084,'(*(G0.6,:,","))') pw_hdr_units
            write (9000,*) "BASIN                     basin_pw_day.csv"
          end if
        endif
        
       if (pco%pw_bsn%m == "y") then
        call open_output_file(2081, "basin_pw_mon.txt", 1500) 
        write (2081,*) bsn%name, prog
        write (2081,*) pw_hdr  !! bsn
        write (2081,*) pw_hdr_units
        write (9000,*) "BASIN                     basin_pw_mon.txt"
       if (pco%csvout == "y") then 
          call open_output_file(2085, "basin_pw_mon.csv", 1500)
          write (2085,*) bsn%name, prog
          write (2085,'(*(G0.6,:,","))') pw_hdr     !! bsn
          write (2085,'(*(G0.6,:,","))') pw_hdr_units
          write (9000,*) "BASIN                     basin_pw_mon.csv"
       end if
      end if

        if (pco%pw_bsn%y == "y") then
          call open_output_file(2082, "basin_pw_yr.txt", 1500)
          write (2082,*) bsn%name, prog
          write (2082,*) pw_hdr  !! bsn
          write (2082,*) pw_hdr_units
          write (9000,*) "BASIN                     basin_pw_yr.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2086, "basin_pw_yr.csv", 1500)
            write (2086,*) bsn%name, prog
            write (2086,'(*(G0.6,:,","))') pw_hdr  !! bsn
            write (2086,'(*(G0.6,:,","))') pw_hdr_units
            write (9000,*) "BASIN                     basin_pw_yr.csv"
          end if
        endif
        
       if (pco%pw_bsn%a == "y") then
        call open_output_file(2083, "basin_pw_aa.txt", 1500) 
        write (2083,*) bsn%name, prog
        write (2083,*) pw_hdr  !! bsn
        write (2083,*) pw_hdr_units
        write (9000,*) "BASIN                     basin_pw_aa.txt"
       if (pco%csvout == "y") then 
          call open_output_file(2087, "basin_pw_aa.csv", 1500)
          write (2087,*) bsn%name, prog
          write (2087,'(*(G0.6,:,","))') pw_hdr     !! bsn
          write (2087,'(*(G0.6,:,","))') pw_hdr_units
          write (9000,*) "BASIN                     basin_pw_aa.csv"
       end if
       end if
       
!!! CROP YIELDS - output file only written for yearly or annual timesteps; "b" = both files written;
      if (pco%crop_yld == "y" .or. pco%crop_yld == "b") then
        !! headers for yearly crop yields
        call open_output_file(4010, "crop_yld_yr.txt", 1500)
        write (4010,*) bsn%name, prog
        write (4010,1000)
        write (9000,*) "CROP                      crop_yld_yr.txt"
        if (pco%csvout == "y") then
            call open_output_file(4011, "crop_yld_yr.csv")
            write (4011,*) bsn%name, prog
            write (4011,'(*(G0.6,:,","))') "jday","mon","day","year","unit","plantnm","yield"
            write (9000,*) "CROP                      crop_yld_yr.csv"
        end if
      end if
              
      !! headers for annual crop yields
      if (pco%crop_yld == "a" .or. pco%crop_yld == "b") then
        call open_output_file(4008, "crop_yld_aa.txt", 1500)
        write (4008,*) bsn%name, prog
        write (4008,1000)
        write (9000,*) "CROP                      crop_yld_aa.txt"
        if (pco%csvout == "y") then
            call open_output_file(4009, "crop_yld_aa.csv")
            write (4009,*) bsn%name, prog
            write (4009,'(*(G0.6,:,","))') "jday","mon","day","year","unit","plantnm","yield"
            write (9000,*) "CROP                      crop_yld_aa.csv"
        end if
      end if
      
1000    format (76x,"--YIELD (kg/ha)--",/,1x," jday",1x,"  mon",1x,"  day",1x,"   yr",1x,"   unit", 1x,"PLANTNM",   &
                 18x,"       MASS","          C", "           N","           P")

      !! LSU-level carbon output files (only when project has LSUs configured)
      !! Unit allocation: lsu_carb_gl 4750-4757, lsu_scf 4758-4765, lsu_plc_stat 4766-4773.
      !! Reuses HRU header types (carb_gl_hdr, hscf_hdr) since columns are identical; lsu_plc_stat uses runtime header.

      if (db_mx%lsu_out > 0) then

      !! lsu_carb_gl_*
      if (pco%cb_gl_lsu%d == "y") then
        call open_output_file(4750, "lsu_carb_gl_day.txt", 8000)
        write (4750,*) bsn%name, prog
        write (4750,*) carb_gl_hdr
        write (4750,*) carb_gl_hdr_units
        write (9000,*) "LSU                       lsu_carb_gl_day.txt"
        if (pco%csvout == "y") then
          call open_output_file(4754, "lsu_carb_gl_day.csv", 8000)
          write (4754,*) bsn%name, prog
          write (4754,'(*(G0.6,:,","))') carb_gl_hdr
          write (4754,'(*(G0.6,:,","))') carb_gl_hdr_units
          write (9000,*) "LSU                       lsu_carb_gl_day.csv"
        end if
      end if
      if (pco%cb_gl_lsu%m == "y") then
        call open_output_file(4751, "lsu_carb_gl_mon.txt", 8000)
        write (4751,*) bsn%name, prog
        write (4751,*) carb_gl_hdr
        write (4751,*) carb_gl_hdr_units
        write (9000,*) "LSU                       lsu_carb_gl_mon.txt"
        if (pco%csvout == "y") then
          call open_output_file(4755, "lsu_carb_gl_mon.csv", 8000)
          write (4755,*) bsn%name, prog
          write (4755,'(*(G0.6,:,","))') carb_gl_hdr
          write (4755,'(*(G0.6,:,","))') carb_gl_hdr_units
          write (9000,*) "LSU                       lsu_carb_gl_mon.csv"
        end if
      end if
      if (pco%cb_gl_lsu%y == "y") then
        call open_output_file(4752, "lsu_carb_gl_yr.txt", 8000)
        write (4752,*) bsn%name, prog
        write (4752,*) carb_gl_hdr
        write (4752,*) carb_gl_hdr_units
        write (9000,*) "LSU                       lsu_carb_gl_yr.txt"
        if (pco%csvout == "y") then
          call open_output_file(4756, "lsu_carb_gl_yr.csv", 8000)
          write (4756,*) bsn%name, prog
          write (4756,'(*(G0.6,:,","))') carb_gl_hdr
          write (4756,'(*(G0.6,:,","))') carb_gl_hdr_units
          write (9000,*) "LSU                       lsu_carb_gl_yr.csv"
        end if
      end if
      if (pco%cb_gl_lsu%a == "y") then
        call open_output_file(4753, "lsu_carb_gl_aa.txt", 8000)
        write (4753,*) bsn%name, prog
        write (4753,*) carb_gl_hdr
        write (4753,*) carb_gl_hdr_units
        write (9000,*) "LSU                       lsu_carb_gl_aa.txt"
        if (pco%csvout == "y") then
          call open_output_file(4757, "lsu_carb_gl_aa.csv", 8000)
          write (4757,*) bsn%name, prog
          write (4757,'(*(G0.6,:,","))') carb_gl_hdr
          write (4757,'(*(G0.6,:,","))') carb_gl_hdr_units
          write (9000,*) "LSU                       lsu_carb_gl_aa.csv"
        end if
      end if

      !! lsu_scf_*
      if (pco%cb_trf_lsu%d == "y") then
        call open_output_file(4758, "lsu_scf_day.txt", 8000)
        write (4758,*) bsn%name, prog
        write (4758,*) hscf_hdr
        write (4758,*) hscf_hdr_units
        write (9000,*) "LSU                       lsu_scf_day.txt"
        if (pco%csvout == "y") then
          call open_output_file(4762, "lsu_scf_day.csv", 8000)
          write (4762,*) bsn%name, prog
          write (4762,'(*(G0.6,:,","))') hscf_hdr
          write (4762,'(*(G0.6,:,","))') hscf_hdr_units
          write (9000,*) "LSU                       lsu_scf_day.csv"
        end if
      end if
      if (pco%cb_trf_lsu%m == "y") then
        call open_output_file(4759, "lsu_scf_mon.txt", 8000)
        write (4759,*) bsn%name, prog
        write (4759,*) hscf_hdr
        write (4759,*) hscf_hdr_units
        write (9000,*) "LSU                       lsu_scf_mon.txt"
        if (pco%csvout == "y") then
          call open_output_file(4763, "lsu_scf_mon.csv", 8000)
          write (4763,*) bsn%name, prog
          write (4763,'(*(G0.6,:,","))') hscf_hdr
          write (4763,'(*(G0.6,:,","))') hscf_hdr_units
          write (9000,*) "LSU                       lsu_scf_mon.csv"
        end if
      end if
      if (pco%cb_trf_lsu%y == "y") then
        call open_output_file(4760, "lsu_scf_yr.txt", 8000)
        write (4760,*) bsn%name, prog
        write (4760,*) hscf_hdr
        write (4760,*) hscf_hdr_units
        write (9000,*) "LSU                       lsu_scf_yr.txt"
        if (pco%csvout == "y") then
          call open_output_file(4764, "lsu_scf_yr.csv", 8000)
          write (4764,*) bsn%name, prog
          write (4764,'(*(G0.6,:,","))') hscf_hdr
          write (4764,'(*(G0.6,:,","))') hscf_hdr_units
          write (9000,*) "LSU                       lsu_scf_yr.csv"
        end if
      end if
      if (pco%cb_trf_lsu%a == "y") then
        call open_output_file(4761, "lsu_scf_aa.txt", 8000)
        write (4761,*) bsn%name, prog
        write (4761,*) hscf_hdr
        write (4761,*) hscf_hdr_units
        write (9000,*) "LSU                       lsu_scf_aa.txt"
        if (pco%csvout == "y") then
          call open_output_file(4765, "lsu_scf_aa.csv", 8000)
          write (4765,*) bsn%name, prog
          write (4765,'(*(G0.6,:,","))') hscf_hdr
          write (4765,'(*(G0.6,:,","))') hscf_hdr_units
          write (9000,*) "LSU                       lsu_scf_aa.csv"
        end if
      end if

      !! lsu_plc_stat_* (single column: lsu_plt_c, area-weighted plant carbon mass)
      if (pco%cb_plt_lsu%d == "y") then
        call open_output_file(4766, "lsu_plc_stat_day.txt", 8000)
        write (4766,*) bsn%name, prog
        write (4766,*) "        jday         mon         day          yr        unit                gis_id    name              lsu_plt_c"
        write (9000,*) "LSU                       lsu_plc_stat_day.txt"
        if (pco%csvout == "y") then
          call open_output_file(4770, "lsu_plc_stat_day.csv", 8000)
          write (4770,*) bsn%name, prog
          write (4770,*) "jday,mon,day,yr,unit,gis_id,name,lsu_plt_c"
          write (9000,*) "LSU                       lsu_plc_stat_day.csv"
        end if
      end if
      if (pco%cb_plt_lsu%m == "y") then
        call open_output_file(4767, "lsu_plc_stat_mon.txt", 8000)
        write (4767,*) bsn%name, prog
        write (4767,*) "        jday         mon         day          yr        unit                gis_id    name              lsu_plt_c"
        write (9000,*) "LSU                       lsu_plc_stat_mon.txt"
        if (pco%csvout == "y") then
          call open_output_file(4771, "lsu_plc_stat_mon.csv", 8000)
          write (4771,*) bsn%name, prog
          write (4771,*) "jday,mon,day,yr,unit,gis_id,name,lsu_plt_c"
          write (9000,*) "LSU                       lsu_plc_stat_mon.csv"
        end if
      end if
      if (pco%cb_plt_lsu%y == "y") then
        call open_output_file(4768, "lsu_plc_stat_yr.txt", 8000)
        write (4768,*) bsn%name, prog
        write (4768,*) "        jday         mon         day          yr        unit                gis_id    name              lsu_plt_c"
        write (9000,*) "LSU                       lsu_plc_stat_yr.txt"
        if (pco%csvout == "y") then
          call open_output_file(4772, "lsu_plc_stat_yr.csv", 8000)
          write (4772,*) bsn%name, prog
          write (4772,*) "jday,mon,day,yr,unit,gis_id,name,lsu_plt_c"
          write (9000,*) "LSU                       lsu_plc_stat_yr.csv"
        end if
      end if
      if (pco%cb_plt_lsu%a == "y") then
        call open_output_file(4769, "lsu_plc_stat_aa.txt", 8000)
        write (4769,*) bsn%name, prog
        write (4769,*) "        jday         mon         day          yr        unit                gis_id    name              lsu_plt_c"
        write (9000,*) "LSU                       lsu_plc_stat_aa.txt"
        if (pco%csvout == "y") then
          call open_output_file(4773, "lsu_plc_stat_aa.csv", 8000)
          write (4773,*) bsn%name, prog
          write (4773,*) "jday,mon,day,yr,unit,gis_id,name,lsu_plt_c"
          write (9000,*) "LSU                       lsu_plc_stat_aa.csv"
        end if
      end if

      end if  !! db_mx%lsu_out > 0

      return

      contains

        subroutine open_cb_wide_pair(u_txt, u_csv, fname_txt, fname_csv, var_names)
          !! opens a wide-per-layer carbon stat file pair (txt always, csv if pco%csvout == "y").
          integer, intent(in) :: u_txt, u_csv
          character(len=*), intent(in) :: fname_txt, fname_csv
          character(len=*), intent(in) :: var_names(:)
          integer :: rl
          !! record length must hold id columns + depth block + one block per
          !! variable, each block cb_n_layers wide; sized so any layer count fits.
          rl = 512 + (1 + size(var_names)) * cb_n_layers * 24
          call open_output_file(u_txt, fname_txt, rl)
          write (u_txt, *) bsn%name, prog
          call cb_write_wide_header(u_txt, var_names, .false.)
          write (9000, *) "HRU                       " // trim(fname_txt)
          if (pco%csvout == "y") then
            call open_output_file(u_csv, fname_csv, rl)
            write (u_csv, *) bsn%name, prog
            call cb_write_wide_header(u_csv, var_names, .true.)
            write (9000, *) "HRU                       " // trim(fname_csv)
          end if
        end subroutine open_cb_wide_pair

        subroutine open_cb_flat_pair(u_txt, u_csv, fname_txt, fname_csv, var_names)
          !! opens a non-layered carbon stat file pair (no _lyrK suffix on labels).
          integer, intent(in) :: u_txt, u_csv
          character(len=*), intent(in) :: fname_txt, fname_csv
          character(len=*), intent(in) :: var_names(:)
          call open_output_file(u_txt, fname_txt, 8000)
          write (u_txt, *) bsn%name, prog
          call cb_write_flat_header(u_txt, var_names, .false.)
          write (9000, *) "HRU                       " // trim(fname_txt)
          if (pco%csvout == "y") then
            call open_output_file(u_csv, fname_csv, 8000)
            write (u_csv, *) bsn%name, prog
            call cb_write_flat_header(u_csv, var_names, .true.)
            write (9000, *) "HRU                       " // trim(fname_csv)
          end if
        end subroutine open_cb_flat_pair

        subroutine open_cb_banner_pair(u_txt, u_csv, fname_txt, fname_csv, banner_msg)
          !! opens hru_cbn_lyr files: banner row, then the column header (written once).
          integer, intent(in) :: u_txt, u_csv
          character(len=*), intent(in) :: fname_txt, fname_csv, banner_msg
          integer :: rl
          !! id columns + depth block + tot block + seq block (each cb_n_layers
          !! wide) plus the two 300 mm scalar sums; sized so any layer count fits.
          rl = 512 + (3 * cb_n_layers + 2) * 24
          call open_output_file(u_txt, fname_txt, rl)
          write (u_txt, *) bsn%name, prog, banner_msg
          call cb_write_cbn_lyr_header(u_txt, .false.)
          write (9000, *) "HRU                       " // trim(fname_txt)
          if (pco%csvout == "y") then
            call open_output_file(u_csv, fname_csv, rl)
            write (u_csv, *) bsn%name, prog, banner_msg
            call cb_write_cbn_lyr_header(u_csv, .true.)
            write (9000, *) "HRU                       " // trim(fname_csv)
          end if
        end subroutine open_cb_banner_pair

      end subroutine output_landscape_init