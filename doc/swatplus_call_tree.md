# SWAT+ Call Tree from `main.f90`

Focused on the `cswat = 1` (CENTURY-based carbon model) code path.
Items in `[brackets]` are loop constructs or conditions, not subroutine calls.

---

## Top-Level Structure

```
main
├── ── INITIALIZATION ──────────────────────────────────────────────
│   ├── proc_bsn              basin-level setup (reads cswat=1 flag)
│   ├── proc_db               read parameter databases
│   ├── proc_read             read spatial input files
│   ├── hyd_connect           build routing network graph
│   ├── recalldb_read
│   ├── exco_db_read
│   └── dr_db_read
│
├── ── OBJECT SETUP ────────────────────────────────────────────────
│   ├── cli_lapse
│   ├── object_read_output
│   ├── om_water_init
│   ├── pest_cha_res_read / path_cha_res_read
│   ├── salt_cha_read / cs_cha_read
│   ├── lsu_read_elements
│   ├── proc_hru              HRU initialisation (carbon pools set here)
│   ├── proc_cha              channel initialisation
│   ├── proc_aqu              aquifer initialisation
│   ├── dtbl_lum_read / hru_lte_read / proc_cond
│   ├── res_read_weir / dtbl_res_read / dtbl_scen_read / cal_cond_read
│   ├── manure_allocation_read / dtbl_flocon_read
│   ├── om_treat_read / om_use_read / om_osrc_read
│   ├── water_treatment_read / water_use_read / water_tower_read
│   ├── water_pipe_read / water_canal_read / water_allocation_read
│   ├── hru_dtbl_actions_init
│   ├── proc_res / wet_read_hyd / wet_read / wet_read_salt_cs
│   ├── wet_all_initial / wet_fp_init
│   ├── [loop HRUs] soil_nutcarb_init   ← initialise CENTURY pool masses from SOC%
│   ├── proc_cal              calibration setup
│   ├── proc_open             open output files, write headers
│   ├── unit_hyd_ru_hru / dr_ru
│   └── hyd_connect_out
│
└── ── SIMULATION ──────────────────────────────────────────────────
    └── time_control
        ├── [year loop]
        │   └── [day loop]
        │       ├── xmon / sim_initday / climate_control
        │       ├── conditions / actions / mallo_control
        │       └── command              ← daily object loop
        │
        ├── [if cal_soft] calsoft_control
        └── [if cal_hard] calhard_control
```

---

## `proc_bsn`

```
proc_bsn
├── readcio_read             read file.cio
├── basin_read_cc            read basins.bsn — sets bsn_cc%cswat = 1
├── basin_read_objs
├── time_read
├── basin_read_prm
├── basin_prm_default
├── basin_print_codes_read   read print.prt — sets cb_hru, cb_vars_hru codes
├── co2_read
└── carbon_coef_read         read carbon_coef.cbn if present (overrides CENTURY rates)
```

---

## `proc_db`

```
proc_db
├── plant_parm_read          plants.plt
├── plantparm_init
├── plant_transplant_read
├── till_parm_read           tillage.til  (needed for till_eff in cbn_zhang2)
├── pest_parm_read
├── fert_parm_read
├── manure_orgmin_read / manure_db_read
├── urban_parm_read
├── path_parm_read / septic_parm_read
├── mgt_read_irrops / mgt_read_chemapp / mgt_read_harvops
├── mgt_read_grazeops / mgt_read_sweepops / mgt_read_fireops
├── mgt_read_mgtops / mgt_read_puddle
├── sdr_read / sep_read
├── scen_read_grwway / scen_read_filtstrip / scen_read_bmpuser / sat_buff_read
├── readpcom
├── cntbl_read / cons_prac_read / overland_n_read
└── landuse_read
```

---

## `proc_read`

```
proc_read
├── ch_read_temp / cli_read_atmodep / cli_staread
├── constit_db_read / pest_metabolite_read
├── soil_plant_init / solt_db_read
├── pest_hru_aqu_read / path_hru_aqu_read / hmet_hru_aqu_read
├── salt_hru_read / salt_aqu_read / salt_irr_read / salt_plant_read
├── cli_read_atmodep_salt / salt_roadsalt_read / salt_uptake_read
├── salt_urban_read / salt_fert_read
├── cs_hru_read / cs_aqu_read / cli_read_atmodep_cs / cs_irr_read
├── cs_plant_read / cs_uptake_read / cs_reactions_read / cs_urban_read / cs_fert_read
├── topo_read / field_read / hydrol_read / shade_factor_read
├── snowdb_read
├── soil_db_read             soils.sol — organic C% used to init CENTURY pools
└── soil_lte_db_read
```

---

## `proc_hru` — HRU Initialisation (carbon pools set here)

```
proc_hru
├── hru_allo / hru_read / hrudb_init
├── hru_lum_init_all / topohyd_init / hru_output_allo
├── carbon_read              read initial carbon state (organic C%, litter)
├── [loop HRUs] structure_set_parms("septic")
├── soils_init               compute FC, WP, BD for each layer
├── structure_init / plant_all_init / cn2_init_all / hydro_init
├── pesticide_init / pathogen_init / salt_hru_init / cs_hru_init
└── rte_read_nut
```

After `proc_hru` returns, `main` calls `soil_nutcarb_init` for each HRU:

```
[loop ihru = 1, sp_ob%hru]
    soil_nutcarb_init(isol)
        ├── Partition initial SOC% into CENTURY pool masses:
        │     microb(k)%c  = frac_hum_microb × total_C
        │     hs(k)%c      = frac_hum_slow    × total_C
        │     hp(k)%c      = frac_hum_passive × total_C   (subsurface only)
        │     str(k)%c, meta(k)%c  initialised from litter fraction
        └── Initialise corresponding N pools from pool N:C ratios
```

---

## `proc_cha`

```
proc_cha
├── ch_read_init / ch_read_init_cs
├── sd_hydsed_read / ch_read_hyd / ch_read_sed / ch_read_nut
├── ch_read / sd_channel_read / sd_hydsed_init
├── aqu2d_init
├── [loop channels] ch_ttcoef / ch_initial
├── overbank_read / sd_channel_surf_link / time_conc_init
```

---

## `proc_aqu`

```
proc_aqu
├── aqu_read / aqu_initial
├── aqu_read_init / aqu_read_init_cs
```

---

## `proc_cal`

```
proc_cal
├── cal_parm_read / cal_parmchg_read
├── pl_read_regions_cal / pl_read_parms_cal / cal_conditions
├── calsoft_read_codes / lcu_read_softcal / ls_read_lsparms_cal
├── aqu_read_elements / ch_read_elements / res_read_elements / rec_read_elements
├── ch_read_orders_cal / ch_read_parms_cal
└── cal_allo_init
```

---

## `proc_open`

```
proc_open
├── output_landscape_init
├── header_channel / header_aquifer / header_sd_channel / header_mgt
├── header_lu_change / header_yield / header_hyd / header_reservoir / header_wetland
├── header_water_allocation / header_pest / header_path
├── header_salt / header_const
└── header_write
```

---

## `time_control` — Year and Day Loops

```
time_control
├── [year loop: curyr = 1 to time%nbyr]
│   │
│   ├── [day loop: julian_day = day_start to day_end_yr]
│   │   ├── xmon                  day-of-year → month, day-of-month
│   │   ├── basin_sw_init         (once, at nyskip year)
│   │   ├── aqu_pest_output_init
│   │   ├── sim_initday           zero daily HRU accumulators
│   │   ├── climate_control       read/generate weather
│   │   ├── cli_atmodep_time_control
│   │   ├── [condition/action loop]  conditions, actions
│   │   ├── [manure allocation loop] mallo_control
│   │   └── command               ← MAIN DAILY OBJECT LOOP (see below)
│   │
│   └── [end-of-year]
│       ├── calsoft_sum_output
│       ├── [crop yield accounting]
│       └── [channel morphology: ebank, downcutting, fp deposition]
│
├── calsoft_ave_output
└── [channel morph summary output: ch_order_sed.txt, ch_budget.txt]
```

---

## `command` — Daily Object Routing Loop

Walks a singly-linked list (`ob(:)%cmd_next`) of spatial objects in routing order.

```
command
├── [linked-list loop: icmd walks ob(:)%cmd_next until icmd == 0]
│   │
│   ├── [pre-object: wallo_control — water allocation without channel source]
│   │
│   ├── [accumulate incoming hydrographs from all upstream objects]
│   │
│   ├── select case (ob(icmd)%typ)
│   │   ├── "hru"      → hru_control     ← land-phase simulation (see below)
│   │   │                hyddep_output
│   │   ├── "hru_lte"  → hru_lte_control
│   │   ├── "ru"       → ru_control
│   │   │                hyddep_output
│   │   ├── "gwflow"   → gwflow_simulate
│   │   ├── "aqu"      → aqu_1d_control
│   │   ├── "res"      → res_control
│   │   ├── "recall"   → [read recall hydrograph]
│   │   │                recall_nut / recall_salt / recall_cs
│   │   ├── "dr"       → [apply delivery ratio]
│   │   ├── "outlet"   → [pass-through]
│   │   └── "chandeg"  → sd_channel_control3
│   │
│   └── [post-object: wallo_control, flow_dur_curve]
│
└── [output section — after all objects processed each day]
    ├── obj_output
    │
    ├── [loop ihru = 1, sp_ob%hru]
    │   ├── hru_output
    │   ├── hru_carbon_output
    │   ├── wetland_output / wet_salt_output / wet_cs_output
    │   ├── hru_pesticide_output / hru_pathogen_output
    │   ├── hru_salt_output / hru_cs_output
    │   │
    │   ├── soil_nutcarb_write    (if pco%cb_hru%d/m/y enabled in print.prt)
    │   │     writes hru_cb.csv — per-HRU carbon balance
    │   │
    │   └── soil_carbvar_write    (if pco%cb_vars_hru%d/m/y enabled AND cswat==1)
    │         writes hru_cb_vars.csv — per-layer pool masses and fluxes
    │
    ├── [loop aqu]  aquifer_output / aqu_salt_output / aqu_cs_output / aqu_pesticide_output
    ├── [loop chan]  channel_output
    ├── [loop chandeg]  sd_chanmorph_output / sd_chanbud_output / sd_channel_output / ...
    ├── [loop res]  reservoir_output / res_pesticide_output / res_salt_output / res_cs_output
    ├── [loop ru]   ru_output / ru_salt_output / ru_cs_output
    ├── [loop recall] recall_output
    ├── hydin_output
    ├── basin_* output routines
    └── salt_balance / cs_balance
```

---

## `hru_control` — HRU Daily Simulation

```
hru_control
│
├── varinit                  zero daily variables
├── albedo
├── [if salts] salt_chem_hru
├── [if cs] cs_rctn_hru / cs_sorb_hru
│
├── stmp_solt                soil temperature profile
├── sq_canopyint             canopy interception
├── sq_snom                  snow accumulation and melt
│
├── rls_routesurf            surface runon from upstream HRU
├── rls_routesoil            lateral inflow from upstream HRU
├── rls_routetile            tile inflow from upstream HRU
├── rls_routeaqu             aquifer inflow
├── [if crk] sq_crackvol     shrink-swell crack volume
│
├── et_pot                   potential evapotranspiration
├── et_act                   actual ET
│
├── [if yr_skip==0] mgt_operatn    scheduled management operations
│   ├── mgt_harvest / mgt_kill
│   ├── mgt_fert             fertilizer/manure application
│   ├── mgt_irrig
│   ├── mgt_tillage → mgt_newtillmix_cswat1
│   │     (mixes/redistributes CENTURY pool masses between soil layers)
│   ├── mgt_graze
│   ├── mgt_burn
│   └── mgt_sweepop
│
├── surface                  surface runoff generation
│   ├── sq_dailycn           SCS curve number
│   └── sq_greenampt         (if enabled)
│
├── wet_irrp / wetland_control
│
├── swr_percmain             percolation
│   ├── swr_percmicro
│   └── swr_percmacro
│
├── [if grazing] pl_graze
│
│   ┌─ CARBON MODEL (cswat == 1) ──────────────────────────────────────────┐
│   │                                                                      │
│   ├── cbn_surfrsd_decomp                                                 │
│   │     Surface residue (pl_mass(j)%rsd) → metabolic + structural litter │
│   │     in surface layer (k=1).                                          │
│   │     Split determined by lignin:N ratio of residue.                   │
│   │                                                                      │
│   ├── cbn_rsd_transfer                                                   │
│   │     Root death + incorporated residue → meta, str, lig pools         │
│   │     distributed across soil layers by root fraction.                 │
│   │                                                                      │
│   └── cbn_zhang2           CENTURY 5-pool C/N model                      │
│         [soil layer loop k = 1 to nly]                                   │
│         │                                                                │
│         ├── Compute sut    soil water factor                             │
│         ├── Compute till_eff tillage disturbance factor                  │
│         │     → mgt_newtillmix_cswat1 sets tillage_switch/days/depth     │
│         ├── Compute cdg    temperature factor: fcgd(stemp)               │
│         ├── Compute ox     oxygen/depth factor                           │
│         ├── cs = min(15, sqrt(cdg×sut) × 0.9 × ox × till_eff)            │
│         │                                                                │
│         ├── Denitrification: NO3 loss → wdn                              │
│         │                                                                │
│         ├── Potential transformations (rate × cs × pool mass)            │
│         │   ├── str:    lsctp, lslctp, lslnctp (lignin/non-lignin split) │
│         │   ├── meta:   lmctp                                            │
│         │   ├── microb: bmctp (scaled by xbm texture factor)             │
│         │   ├── hs:     hsctp                                            │
│         │   └── hp:     hpctp  (zero in surface layer)                   │
│         │                                                                │
│         ├── N supply–demand → reduc factor                               │
│         ├── Actual transformations = potential × reduc                   │
│         │                                                                │
│         ├── C pool updates (microb, hs, hp, str, meta, lig, nonlig)      │
│         │                                                                │
│         ├── N flux accounting via nut_np_flow:                           │
│         │   str→microb, meta→microb, microb→hs, microb→hp,               │
│         │   hs→microb, hs→hp, hp→microb                                  │
│         │   → updates meta%n, str%n, microb%n, hs%n, hp%n, NO3, NH4      │
│         │                                                                │
│         ├── CO2 respiration (rspc) → hsc_d(j)%rsp_c                      │
│         │                                                                │
│         └── Save per-layer diagnostics:                                  │
│               soil1(j)%org_flx_lr(k)   — all C/N fluxes                  │
│               soil1(j)%org_con_lr(k)   — sut, cdg, ox, cs, till_eff      │
│               soil1(j)%org_allo_lr(k)  — allocation fractions            │
│               soil1(j)%org_tran_lr(k)  — potential transformation rates  │
│         [end layer loop]                                                 │
│                                                                          │
│   └─ END CARBON MODEL ───────────────────────────────────────────────────┘
│
├── nut_nitvol               N volatilization
├── nut_pminrl2 / nut_pminrl P mineralization
├── [if septic] sep_biozone
│
├── pl_community             plant community decisions
├── pl_grow                  daily plant growth (LAI, biomass, root, N/P uptake)
│
├── pest_washp / pest_pl_up / pest_decay / pest_lch / pest_soil_tot
├── pest_enrsb / pest_pesty
│
├── nut_orgn / nut_orgnc2    organic N transport
├── nut_psed                 P with sediment
├── nut_nrain / nut_nlch / nut_solp
│
├── salt_rain / salt_roadsalt / salt_lch
├── cs_rain / cs_lch
├── path_ls_swrouting / path_ls_runoff / path_ls_process
├── [if urban] hru_urban / hru_urbanhr
│
├── swr_latsed / stor_surfstor / swr_substor
├── smp_filter / smp_buffer / smp_grass_wway / smp_bmpfixed
├── sq_surfst
├── [conditional management] conditions, actions
├── swr_subwq
└── hru_hyds                 assemble outflow hydrographs
```

---

## Carbon Pool Flow in `cbn_zhang2` (one soil layer)

```
                        ┌─────────────────────────────────────────┐
                        │         INPUTS  (from cbn_*_transfer)   │
                        └──────┬──────────────────────────────────┘
                               │ litter added to meta(k) and str(k)
              ┌────────────────┴────────────────────────┐
              ▼                                          ▼
      METABOLIC LITTER                        STRUCTURAL LITTER
      meta(k)%c, %n                     str(k)%c = nonlig(k)%c + lig(k)%c

       │ a1co2 → CO2                    │ 0.3 × lig → CO2
       │ a1 ────────────────────────────┤ a1 × nonlig ─────────────────────────┐
       │                                │ 0.7 × lig ──────► SLOW HUMUS (hs)    │
       │                                │                    hs(k)%c, %n       │
       └───────────────────────────────►│                         │            │
                                 MICROBIAL (microb)               │            │
                                 microb(k)%c, %n ◄────────────────┘ asx        │
                                        │                         │            │
                            ┌───────────┼───────────┐   asco2→CO2 │            │
                            │ abco2     │ abp       │   asp ─────►│            │
                            ▼           ▼           ▼        PASSIVE (hp)      │
                           CO2    PASSIVE (hp)   SLOW (hs) ◄───────────────────┘
                               hp(k)%c, %n   hs(k)%c, %n
                                    │               │
                              apco2 → CO2     asco2 → CO2
                              apx ──────────────────────► MICROBIAL (microb)
                                    │ asp ──────────────► PASSIVE (hp)

  All CO2 terms summed into hsc_d(j)%rsp_c  (soil respiration output)
  seq(k)%c = microb(k)%c + hs(k)%c + hp(k)%c   (sequestered C total)
  tot(k)%c = seq(k)%c + meta(k)%c + str(k)%c   (all organic C)
```

---

## Tillage Mixing — `mgt_newtillmix_cswat1`

Called from `mgt_tillage` within `mgt_operatn`. Redistributes CENTURY pool masses between soil layers according to tillage depth, simulating physical mixing of SOM:

```
mgt_operatn
└── mgt_tillage
    └── mgt_newtillmix_cswat1
          For each layer pair within tillage depth:
          ├── Mix microb(k)%c, microb(k)%n
          ├── Mix hs(k)%c, hs(k)%n
          ├── Mix hp(k)%c, hp(k)%n
          ├── Mix str(k)%c, str(k)%n
          ├── Mix meta(k)%c, meta(k)%n
          └── Mix lig(k)%c, lig(k)%m
          Sets: tillage_switch(j)=1, tillage_days(j)=0, tillage_depth(j)
          → till_eff will be 1.6 for the next 30 days in cbn_zhang2
```

---

## Erosion and Sediment Yield — Output Files and `print.prt` Controls

### Daily accumulation into output structure (`hru_control.f90:874–881`)

After MUSLE and USLE are computed each day, results are normalised per-hectare and packed into the daily landscape-losses structure:

```fortran
hls_d(j)%sedyld  = sedyld(j) / hru(j)%area_ha   ! t/ha  (MUSLE sediment yield)
hls_d(j)%usle    = usle                           ! t/ha  (USLE erosion estimate)
hls_d(j)%sedorgn = sedorgn(j)                     ! kg/ha
hls_d(j)%sedorgp = sedorgp(j)                     ! kg/ha
hls_d(j)%sedminp = sedminpa(j) + sedminps(j)      ! kg/ha
```

`hls_d` accumulates into `hls_m` (monthly) and `hls_y` (yearly) in `hru_output.f90:43,105,174`.

---

### HRU-level output files

Controlled by the `hru_ls` row in `print.prt` → reads into `pco%ls_hru%d/m/y/a` (`basin_print_codes_read.f90:380`).

| Timestep | `print.prt` flag | `.txt` unit | `.csv` unit | File names                          |
| -------- | ---------------- | ----------- | ----------- | ----------------------------------- |
| Daily    | `pco%ls_hru%d`   | 2030        | 2034        | `hru_ls_day.txt` / `hru_ls_day.csv` |
| Monthly  | `pco%ls_hru%m`   | 2031        | 2035        | `hru_ls_mon.txt` / `hru_ls_mon.csv` |
| Yearly   | `pco%ls_hru%y`   | 2032        | 2036        | `hru_ls_yr.txt`  / `hru_ls_yr.csv`  |
| Avg ann  | `pco%ls_hru%a`   | 2033        | 2037        | `hru_ls_aa.txt`  / `hru_ls_aa.csv`  |

Writes driven by `hru_output.f90:81–86` (daily), `:138–143` (monthly), `:209–214` (yearly), `:275` (avg annual).

Key erosion/sediment columns in these files:

| Column    | Variable                   | Units | Source            |
| --------- | -------------------------- | ----- | ----------------- |
| `sedyld`  | MUSLE sediment leaving HRU | t/ha  | `ero_ysed.f90:55` |
| `usle`    | USLE erosion estimate      | t/ha  | `ero_ysed.f90:81` |
| `sedorgn` | Organic N in sediment      | kg/ha | `nut_orgn.f90`    |
| `sedorgp` | Organic P in sediment      | kg/ha | `nut_psed.f90`    |
| `sedminp` | Mineral P in sediment      | kg/ha | `nut_psed.f90`    |

---

### Basin-level output files

Controlled by the `basin_ls` row in `print.prt` → reads into `pco%ls_bsn%d/m/y/a` (`basin_print_codes_read.f90:204`).

| Timestep | `print.prt` flag | `.txt` unit | `.csv` unit | File names                              |
| -------- | ---------------- | ----------- | ----------- | --------------------------------------- |
| Daily    | `pco%ls_bsn%d`   | 2070        | 2074        | `basin_ls_day.txt` / `basin_ls_day.csv` |
| Monthly  | `pco%ls_bsn%m`   | 2071        | 2075        | `basin_ls_mon.txt` / `basin_ls_mon.csv` |
| Yearly   | `pco%ls_bsn%y`   | 2072        | 2076        | `basin_ls_yr.txt`  / `basin_ls_yr.csv`  |
| Avg ann  | `pco%ls_bsn%a`   | 2073        | —           | `basin_ls_aa.txt`                       |

Aggregated from HRU values in `basin_output.f90`.

---

### `print.prt` row format

Each losses row in `print.prt` has four y/n flags for daily, monthly, yearly, and annual-average:

```
hru_ls    y  y  y  y      ← enables all four HRU losses timesteps
basin_ls  n  n  y  y      ← basin losses: yearly + avg annual only
```

File opened via `output_landscape_init.f90:640–759` (HRU) and `:1402–1452` (basin); headers written by the same routine using the `output_losses_header` type defined in `output_landscape_module.f90`.
