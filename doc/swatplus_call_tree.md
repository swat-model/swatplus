# SWAT+ Call Tree from `main.f90`

Full call tree without cswat constraints — both the `cswat = 0` (static model) and `cswat = 1` (CENTURY) paths are shown.
Branches prefixed with `if` or `case` are conditional — the subroutine is only called when the condition is met.
Click any linked name to jump to that routine's own call tree below.

---

## main

<pre>
main
├── INITIALIZATION ─────────────────────────────────────────────────────
│   ├── <a href="#proc_bsn">proc_bsn</a>              basin-level setup
│   ├── proc_date_time
│   ├── <a href="#proc_db">proc_db</a>               read parameter databases
│   ├── <a href="#proc_read">proc_read</a>             read spatial input files
│   ├── hyd_connect           build routing network graph
│   ├── recalldb_read
│   ├── exco_db_read
│   └── dr_db_read
│
├── OBJECT SETUP ───────────────────────────────────────────────────────
│   ├── cli_lapse
│   ├── object_read_output
│   ├── om_water_init
│   ├── pest_cha_res_read / path_cha_res_read
│   ├── salt_cha_read / cs_cha_read
│   ├── lsu_read_elements
│   ├── <a href="#proc_hru">proc_hru</a>              HRU initialisation
│   ├── <a href="#proc_cha">proc_cha</a>              channel initialisation
│   ├── <a href="#proc_aqu">proc_aqu</a>              aquifer initialisation
│   ├── dtbl_lum_read
│   ├── hru_lte_read / proc_cond
│   ├── res_read_weir / dtbl_res_read / dtbl_scen_read / cal_cond_read
│   ├── manure_allocation_read / dtbl_flocon_read
│   ├── om_treat_read / om_use_read / om_osrc_read
│   ├── water_treatment_read / water_use_read / water_tower_read
│   ├── water_pipe_read / water_canal_read / water_allocation_read
│   ├── hru_dtbl_actions_init
│   ├── proc_res / wet_read_hyd / wet_read / wet_read_salt_cs
│   ├── [if db_mx%wet_dat > 0]  wet_all_initial
│   ├── wet_fp_init
│   ├── [loop ihru = 1, sp_ob%hru]  <a href="#soil_nutcarb_init">soil_nutcarb_init</a>
│   ├── <a href="#proc_cal">proc_cal</a>              calibration setup
│   ├── <a href="#proc_open">proc_open</a>             open output files, write headers
│   ├── unit_hyd_ru_hru / dr_ru
│   └── hyd_connect_out
│
└── SIMULATION ─────────────────────────────────────────────────────────
    ├── [if time%step &lt; 0]  <a href="#command">command</a>         export-coefficient (average annual) mode
    └── [else]              <a href="#time_control">time_control</a>    standard year/day simulation loop
          [after loop] [if cal_soft == "y"]  calsoft_control
                       [if cal_hard == "y"]  cal_parmchg_read → calhard_control
                       [if bsn_cc%swift_out == 1]  swift_output
</pre>

---

## proc_bsn

<pre>
proc_bsn
├── readcio_read             read file.cio
├── basin_read_cc            read basins.bsn  (sets bsn_cc%cswat, %crk, %lapse, etc.)
├── basin_read_objs
├── time_read
├── basin_read_prm
├── basin_prm_default
├── basin_print_codes_read   read print.prt  → sets all output flags
├── co2_read
└── carbon_coef_read         read carbon_coef.cbn if present (overrides CENTURY rates)
</pre>

---

## proc_db

<pre>
proc_db
├── plant_parm_read          plants.plt
├── plantparm_init
├── plant_transplant_read
├── till_parm_read           tillage.til  (till_eff used in cbn_zhang2)
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
</pre>

---

## proc_read

<pre>
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
├── soil_db_read             soils.sol — organic C% used to initialise carbon pools
└── soil_lte_db_read
</pre>

---

## proc_hru

<pre>
proc_hru
├── hru_allo / hru_read / hrudb_init
├── hru_lum_init_all / topohyd_init / hru_output_allo
├── carbon_read              read initial carbon state (organic C%, litter)
├── [loop HRUs]  structure_set_parms("septic")
├── soils_init               compute FC, WP, BD for each layer
├── structure_init / plant_all_init / cn2_init_all / hydro_init
├── pesticide_init / pathogen_init / salt_hru_init / cs_hru_init
└── rte_read_nut
</pre>

---

## soil_nutcarb_init

Called from `main` in a loop over every HRU after `proc_hru` returns.

<pre>
[loop ihru = 1, sp_ob%hru]
    <a href="#soil_nutcarb_init">soil_nutcarb_init</a>(isol)
        └── [loop k = 1, nly]  Partition initial SOC% into pool masses:
                microb(k)%c  = frac_hum_microb × total_C
                hs(k)%c      = frac_hum_slow    × total_C
                hp(k)%c      = frac_hum_passive × total_C   (subsurface layers only)
                str(k)%c, meta(k)%c  initialised from litter fraction
                N pools initialised from pool C:N ratios
</pre>

---

## proc_cha

<pre>
proc_cha
├── ch_read_init / ch_read_init_cs
├── sd_hydsed_read / ch_read_hyd / ch_read_sed / ch_read_nut
├── ch_read / sd_channel_read / sd_hydsed_init
├── aqu2d_init
├── [loop channels]  ch_ttcoef / ch_initial
└── overbank_read / sd_channel_surf_link / time_conc_init
</pre>

---

## proc_aqu

<pre>
proc_aqu
├── aqu_read / aqu_initial
└── aqu_read_init / aqu_read_init_cs
</pre>

---

## proc_cal

<pre>
proc_cal
├── cal_parm_read / cal_parmchg_read
├── pl_read_regions_cal / pl_read_parms_cal / cal_conditions
├── calsoft_read_codes / lcu_read_softcal / ls_read_lsparms_cal
├── aqu_read_elements / ch_read_elements / res_read_elements / rec_read_elements
├── ch_read_orders_cal / ch_read_parms_cal
└── cal_allo_init
</pre>

---

## proc_open

<pre>
proc_open
├── output_landscape_init
├── header_channel / header_aquifer / header_sd_channel / header_mgt
├── header_lu_change / header_yield / header_hyd / header_reservoir / header_wetland
├── header_water_allocation / header_pest / header_path
├── header_salt / header_const
└── header_write
</pre>

---

## time_control

<pre>
time_control
│
├── cli_precip_control (0)   generate first-day precipitation
│
├── [year loop: curyr = 1 to time%nbyr]
│   │
│   ├── [if pco%sw_init == "n" and yrs > nyskip]
│   │   ├── basin_sw_init
│   │   └── aqu_pest_output_init
│   │
│   ├── [day loop: julian_day = day_start to day_end_yr]
│   │   ├── xmon                          day-of-year → month, day-of-month
│   │   ├── sim_initday                   zero daily HRU accumulators
│   │   ├── climate_control               read/generate weather
│   │   ├── cli_atmodep_time_control      set atmospheric deposition array counter
│   │   ├── [loop db_mx%cond_up]  conditions / actions   conditional land-use reset
│   │   ├── [if db_mx%mallo_db > 0]  mallo_control        manure allocation
│   │   └── <a href="#command">command</a>                       daily object routing loop
│   │
│   └── [end-of-year]
│       ├── calsoft_sum_output
│       ├── [crop yield accounting — basin and regional totals]
│       ├── [loop sp_ob%hru]
│       │   ├── [if bsn_cc%cswat == 0 and biomix > 1e-6]
│       │   │     mgt_newtillmix_cswat0 (j, biomix, 0)   annual biological mixing
│       │   │     (cswat==1 biological mixing occurs daily in hru_control via mgt_biomix)
│       │   ├── [update perennial plant maturity ages]
│       │   └── [reset phubase, yr_skip; advance schedule past "skip" ops]
│       └── [reset yearly HRU/channel/LTE output accumulators]
│
├── calsoft_ave_output
└── [channel morphology summary: ch_order_sed.txt, ch_budget.txt]
    [reservoir trap efficiencies]
</pre>

---

## command

Walks a singly-linked list (`ob(:)%cmd_next`) of all spatial objects in routing order each day.

<pre>
command
│
├── [linked-list loop: icmd walks ob(:)%cmd_next until icmd == 0]
│   │
│   ├── [pre-object]  wallo_control   water allocation without channel source
│   │
│   ├── [accumulate incoming hydrographs from all upstream objects]
│   │   (surface, lateral, tile, aquifer components routed separately for HRUs)
│   │
│   ├── select case (ob(icmd)%typ)
│   │   │
│   │   ├── case "hru"
│   │   │   ├── <a href="#hru_control">hru_control</a>               land-phase daily simulation
│   │   │   └── [if ob%rcv_tot > 0]  hyddep_output
│   │   │
│   │   ├── case "hru_lte"
│   │   │   └── hru_lte_control
│   │   │
│   │   ├── case "ru"
│   │   │   ├── ru_control
│   │   │   └── [if ob%rcv_tot > 0]  hyddep_output
│   │   │
│   │   ├── case "gwflow"
│   │   │   └── gwflow_simulate
│   │   │
│   │   ├── case "aqu"
│   │   │   └── [if dfn_tot == 0]  aqu_1d_control   (1-D BF recession)
│   │   │
│   │   ├── case "res"
│   │   │   └── [if rcv_tot > 0]  res_control (ires)
│   │   │
│   │   ├── case "recall"   ── point-source / reach input ───────────────────
│   │   │   ├── select case (recall_db%org_min%tstep)
│   │   │   │   ├── "sub"  — load subdaily hydrograph array
│   │   │   │   ├── "day"  — load daily hydrograph
│   │   │   │   │     [if flow &lt; 0]  recall_nut   (nutrient diversion)
│   │   │   │   ├── "mo"   — load monthly hydrograph
│   │   │   │   └── "yr"   — load yearly hydrograph
│   │   │   ├── [if cs_db%num_salts > 0]  recall_salt
│   │   │   └── [if cs_db%num_cs > 0]    recall_cs
│   │   │
│   │   ├── case "dr"
│   │   │   └── [if cs_db%num_tot > 0]  constit_hyd_mult   (apply delivery ratio to constituents)
│   │   │
│   │   ├── case "outlet"
│   │   │   └── [pass-through: hd(1) = hin]
│   │   │
│   │   └── case "chandeg"   ── SWAT-Deg channel ─────────────────────────────
│   │       ├── [if chl > 1e-3]  sd_channel_control3
│   │       └── [else: artificial channel (length≈0) — pass-through, zero morphology outputs]
│   │
│   └── [post-object]  wallo_control / [if pco%fdcout == "y"] flow_dur_curve
│
└── OUTPUT SECTION  (daily, after all objects processed)
    │
    ├── obj_output
    │
    ├── [loop ihru = 1, sp_ob%hru]
    │   ├── hru_output
    │   ├── hru_carbon_output
    │   ├── [if surf_stor > 0]
    │   │   ├── wetland_output
    │   │   ├── [if num_salts > 0]  wet_salt_output
    │   │   └── [if num_cs > 0]    wet_cs_output
    │   ├── [if num_tot > 0]    hru_pesticide_output / hru_pathogen_output
    │   ├── [if num_salts > 0]  hru_salt_output
    │   ├── [if num_cs > 0]     hru_cs_output
    │   ├── [if pco%cb_hru%d/m/y/l set]       soil_nutcarb_write    → hru_cb.csv
    │   └── [if bsn_cc%cswat == 1]
    │         [if pco%cb_vars_hru%d/m/y/l set] soil_carbvar_write   → hru_cb_vars.csv
    │
    ├── [loop iaq]    aquifer_output / aqu_salt_output / aqu_cs_output / aqu_pesticide_output
    ├── [loop chan]   channel_output
    ├── [loop chandeg]  sd_chanmorph_output / sd_chanbud_output / sd_channel_output
    │                   cha_pesticide_output / ch_salt_output / ch_cs_output
    ├── [loop res]    reservoir_output / res_pesticide_output / res_salt_output / res_cs_output
    ├── [loop ru]     ru_output / ru_salt_output / ru_cs_output
    ├── [loop recall] recall_output
    ├── hydin_output
    ├── basin_output / basin_aquifer_output / basin_reservoir_output
    ├── basin_channel_output / basin_chanmorph_output / basin_chanbud_output
    ├── basin_sdchannel_output / basin_recall_output
    ├── basin_ch_pest_output / basin_res_pest_output / basin_ls_pest_output / basin_aqu_pest_output
    ├── lsu_output
    ├── [if num_salts > 0]  salt_balance
    └── [if num_cs > 0]     cs_balance
</pre>

---

## hru_control

Daily land-phase simulation for one HRU. Called from `command` for each `"hru"` object.

<pre>
hru_control
│
├── [lapse rate adjustment]
│   └── [if bsn_cc%lapse == 1]
│         w%precip, w%tmax, w%tmin adjusted for HRU elevation
│
├── [tillage effect decay — CENTURY path only]
│   └── [if bsn_cc%cswat == 1 and tillage_switch(ihru) == 1]
│         increment tillage_days(ihru)
│         [if tillage_days >= till_eff_days]  reset switch=0, days=0, tillagef=0
│
├── varinit                   zero daily output variables
│
├── [auto-operations from decision table]
│   └── [loop sched%num_autos]
│         conditions / actions
│         [if future fertilizer scheduled today]  pl_fert
│
├── albedo                    compute daily albedo
│
├── [salt equilibrium chemistry]
│   └── [if cs_db%num_salts > 0]  salt_chem_hru
│
├── [constituent reactions and sorption]
│   └── [if cs_db%num_cs > 0]
│         ├── cs_rctn_hru
│         └── cs_sorb_hru
│
├── stmp_solt                 soil temperature profile
├── sq_canopyint              canopy interception
├── sq_snom                   snow accumulation and melt
│
├── [surface and subsurface runon routing]
│   ├── [if hin_sur%flo > 1e-6 and ires == 0]  rls_routesurf
│   ├── [if hin_sur%flo > 1e-6 and ires > 0]   add to wetland (ht1)
│   ├── [if hin_lat%flo > 0]                    rls_routesoil
│   ├── [if this HRU is a saturated-buffer receiver]  rls_routetile
│   └── [if hin_aqu%flo > 0]                    rls_routeaqu
│
├── [shrink-swell cracking]
│   └── [if bsn_cc%crk == 1]  sq_crackvol
│
├── et_pot                    potential evapotranspiration
├── et_act                    actual evapotranspiration
│
├── [scheduled management operations]
│   └── [if yr_skip(j) == 0]  <a href="#mgt_operatn">mgt_operatn</a>
│
├── [surface runoff generation]
│   ├── [if ires == 0]   surface
│   │     ├── sq_dailycn       SCS curve number update
│   │     └── [if CN method == Green-Ampt]  sq_greenampt
│   └── [if ires > 0]   (wetland present — surfq=0, sedyld=0; no surface calculation)
│
├── [paddy continuous irrigation]
│   └── [if hru%paddy_irr > 0 and wet_ob%depth &lt; irr_hmin]  wet_irrp
│
├── [wetland/paddy processes]
│   ├── [if ires > 0]  wetland_control
│   └── [if ires == 0]  pass saturation excess to qday directly
│
├── swr_percmain              percolation through soil profile
│   ├── swr_percmicro         micropore (matrix) flow
│   └── swr_percmacro         macropore (bypass) flow
│
├── [grazing]
│   └── [if igrz(j) == 1]  pl_graze
│         igrz set to 1 by mgt_sched case "graz"; counts down grz_days then resets
│
├── ─── DECOMPOSITION AND CARBON CYCLING ───────────────────────────────────
│   │
│   ├── [if bsn_cc%cswat == 0]   ── static model ───────────────────────────
│   │   ├── rsd_decomp            surface residue decomposition
│   │   └── nut_nminrl            N and P mineralisation from soil residue
│   │
│   └── [if bsn_cc%cswat == 1]   ── CENTURY 5-pool model ───────────────────
│       ├── [if bmix_eff > 1e-6]  mgt_biomix (ihru, bmix_eff)   biological mixing
│       ├── cbn_surfrsd_decomp    surface residue → meta/str litter in layer 1
│       ├── cbn_rsd_transfer      root death + incorporated residue → soil pools
│       └── <a href="#cbn_zhang2">cbn_zhang2</a>            CENTURY 5-pool C/N transformations (layer loop)
│
├── ─── (both cswat paths continue here) ──────────────────────────────────
│
├── nut_nitvol                N volatilization
│
├── [phosphorus mineralisation]
│   ├── [if bsn_cc%sol_P_model == 1]  nut_pminrl2
│   └── [else]                        nut_pminrl
│
├── [septic biozone]
│   └── [if sep%opt != 0 and yrc >= sep%yr and soil_temp > 0]  sep_biozone
│
├── pl_community              plant community phenology decisions
├── pl_grow                   daily plant growth (LAI, biomass, root, N/P uptake)
│
├── [pesticide processes]
│   ├── [if w%precip >= 2.54]  pest_washp      pesticide washoff by rain
│   ├── pest_pl_up                             pesticide plant uptake
│   ├── pest_decay                             pesticide degradation
│   ├── pest_lch                               pesticide leaching
│   └── pest_soil_tot                          sum pesticide in soil
│
├── [nutrient/pollutant transport with surface runoff]
│   └── [if surfq > 0 and peak rate > 0 and precip_eff > 0]
│         ├── pest_enrsb                       enrichment ratio
│         ├── [if sedyld > 0]  pest_pesty      pesticide with sediment
│         ├── [if bsn_cc%cswat == 0]  nut_orgn      organic N in runoff (static)
│         ├── [if bsn_cc%cswat == 1]  nut_orgnc2    organic N in runoff (CENTURY)
│         └── nut_psed                         P with sediment
│
├── nut_nrain                 NO3 in rainfall added to soil
├── nut_nlch                  nitrate leaching
├── nut_solp                  soluble P movement
│
├── [salt processes]
│   └── [if cs_db%num_salts > 0]
│         ├── [if salt_atmo == "y"]  salt_rain       atmospheric deposition
│         ├── salt_roadsalt                          road salt application
│         └── salt_lch                               salt leaching
│
├── [constituent (cs) processes]
│   └── [if cs_db%num_cs > 0]
│         ├── [if cs_atmo == "y"]  cs_rain
│         └── cs_lch
│
├── [pathogen processes]
│   └── [if cs_db%num_paths > 0]
│         ├── path_ls_swrouting
│         ├── path_ls_runoff
│         └── path_ls_process
│
├── [urban area loadings]
│   └── [if hru%luse%urb_lu > 0]
│         ├── [if time%step == 1]  hru_urban    (daily simulation)
│         └── [if time%step > 1]  hru_urbanhr  (subdaily simulation)
│
├── swr_latsed                sediment in lateral flow
├── stor_surfstor             lag surface runoff and pollutants
├── swr_substor               lag subsurface flow and nitrate
│
├── [edge-of-field filter strip]
│   └── [if lumv%vfsi > 0]
│         ├── smp_filter
│         └── [if filterw(j) > 0]  smp_buffer
│
├── [in-field grass waterway]
│   └── [if lumv%grwat_i == 1]  smp_grass_wway
│
├── [fixed BMP efficiency]
│   └── [if lumv%bmp_flag == 1]  smp_bmpfixed
│
├── [saturated buffer — source HRU side]
│   └── [if sb%hru_src == j]  conditions / actions   (tile diversion decision table)
│
├── sq_surfst                 store/release surface runoff
├── swr_subwq                 chl-a, CBOD, dissolved O₂
├── [if sed/nut concentrations > 0]  hru_urb_bmp
└── hru_hyds                  assemble outflow hydrographs (hd arrays)
</pre>

---

## mgt_operatn

Called from `hru_control` when `yr_skip(j) == 0`.

<pre>
mgt_operatn
│
├── [read current scheduled operation for this HRU: sched(isched)%mgt_ops(cur_op)]
│
├── [while mgt%mon == time%mo AND mgt%day == time%day_mo]   date-triggered
│   └── <a href="#mgt_sched">mgt_sched</a>(isched)
│
└── [while mgt%husc > 0 AND aphu > mgt%husc]               heat-unit triggered
    └── <a href="#mgt_sched">mgt_sched</a>(isched)
          [skip "skip" operation immediately: call mgt_sched again]
</pre>

---

## mgt_sched

Dispatches each management operation type. Called from `mgt_operatn`.
After each call, advances `hru%cur_op` to the next scheduled operation.

<pre>
mgt_sched(isched)
│
└── select case (mgt%op)
    │
    ├── case "plnt" ── plant ──────────────────────────────────────────────────
    │   └── [loop ipl = 1, pcom%npl]
    │         [if crop name matches and crop NOT already growing]
    │         ├── mgt_plantop              initialise plant state variables
    │         └── [if mgt%op3 > 0 and transplant record found]
    │               ├── mgt_transplant(itr)
    │               └── log "TRANSPLANT" to mgt.out
    │         [if crop is already growing]
    │             log "PLANT_ALREADY_GROWING" to mgt.out (no action)
    │
    ├── case "mons" ── monsoon season control ──────────────────────────────────
    │   └── [loop ipl]  for plants with trig == "moisture_gro":
    │         [if mgt%op3 == 0 and not yet triggered during monsoon]
    │           reset phuacc=0, gro="y", idorm="n", mseas="n"
    │         [if mgt%op3 == 1]
    │           set mseas="y"  (begin monsoon season; growth triggered by P/PET in hru_control)
    │
    ├── case "harv" ── harvest only ──────────────────────────────────────────
    │   └── [loop ipl = 1, pcom%npl]
    │         [if gro == "y" and crop matches and biomass > bm_min]
    │         │   select case (harvop_db(iharvop)%typ)
    │         │   ├── "biomass"  → mgt_harvbiomass(j, ipl, iharvop)
    │         │   ├── "grain"    → mgt_harvgrain  (j, ipl, iharvop)
    │         │   ├── "residue"  → mgt_harvresidue(j, harveff, iharvop)
    │         │   ├── "tree"     → mgt_harvbiomass(j, ipl, iharvop)
    │         │   ├── "tuber"    → mgt_harvtuber  (j, ipl, iharvop)
    │         │   ├── "peanuts"  → mgt_harvtuber  (j, ipl, iharvop)
    │         │   ├── "stripper" → mgt_harvbiomass(j, ipl, iharvop)
    │         │   └── "picker"   → mgt_harvgrain  (j, ipl, iharvop)
    │         [if crop NOT growing]
    │             select case (harvop_db%typ)
    │             └── "residue"  → mgt_harvresidue(j, harveff, iharvop)
    │
    ├── case "kill" ── kill ───────────────────────────────────────────────────
    │   └── [loop ipl = 1, pcom%npl]
    │         [if gro == "y" and crop matches]
    │         ├── mgt_killop(j, ipl)   move biomass to residue, zero plant state
    │         └── pcom%plcur(ipl)%phuacc = 0
    │
    ├── case "hvkl" ── harvest and kill ─────────────────────────────────────
    │   └── [loop ipl = 1, pcom%npl]
    │         [if gro == "y" and crop matches and biomass > bm_min]
    │         │   select case (harvop_db(iharvop)%typ)
    │         │   ├── "biomass"  → mgt_harvbiomass(j, ipl, iharvop)
    │         │   ├── "grain"    → mgt_harvgrain  (j, ipl, iharvop)
    │         │   ├── "residue"  → mgt_harvresidue(j, harveff, iharvop)
    │         │   ├── "tuber"    → mgt_harvtuber  (j, ipl, iharvop)
    │         │   ├── "peanuts"  → mgt_harvtuber  (j, ipl, iharvop)
    │         │   ├── "stripper" → mgt_harvgrain  (j, ipl, iharvop)
    │         │   └── "picker"   → mgt_harvgrain  (j, ipl, iharvop)
    │         └── mgt_killop(j, ipl)   always called after harvest portion
    │         [if NOT growing]
    │             select case (harvop_db%typ)
    │             └── "residue"  → mgt_harvresidue(j, harveff, iharvop)
    │
    ├── case "till" ── tillage ────────────────────────────────────────────────
    │   ├── [if bsn_cc%cswat == 1]  mgt_newtillmix_cswat1(j, 0., idtill)
    │   │     mixes CENTURY pool masses between layers; sets tillage_switch=1,
    │   │     tillage_days=0 → till_eff=1.6 in cbn_zhang2 for till_eff_days
    │   └── [if bsn_cc%cswat == 0]  mgt_newtillmix_cswat0(j, 0., idtill)
    │         mixes residue and humus variables between layers
    │
    ├── case "irrm" ── date-scheduled irrigation ─────────────────────────────
    │   └── [no subroutine called]
    │         sets irrig(j)%applied and %runoff from irrop_db
    │         water enters soil profile via swr_percmain in hru_control
    │
    ├── case "fert" ── fertilizer application ────────────────────────────────
    │   ├── [if wet(j)%flo > 0]  standing water present (paddy/wetland)
    │   │   ├── pl_fert_wet(ifrt, frt_kg)         add nutrients to ponded water
    │   │   ├── salt_fert_wet(j, ifrt, frt_kg)
    │   │   └── cs_fert_wet(j, ifrt, frt_kg)
    │   └── [else]  dry soil
    │       └── pl_fert(ifrt, frt_kg, ifertop)    add nutrients to soil profile
    │
    ├── case "manu" ── manure application ────────────────────────────────────
    │   ├── pl_manure(ifrt, frt_kg, ifertop)      add organic/mineral N and P
    │   ├── salt_fert(j, ifrt, frt_kg, ifertop)
    │   └── cs_fert(j, ifrt, frt_kg, ifertop)
    │
    ├── case "pest" ── pesticide application ─────────────────────────────────
    │   └── pest_apply(j, ipest, pest_kg, ipestop)
    │
    ├── case "graz" ── grazing initiation ────────────────────────────────────
    │   └── [no subroutine called]
    │         sets igrz(j)=1, grz_days(j)=Int(mgt%op3)
    │         pl_graze is called per-day in hru_control while igrz(j)==1
    │         igrz resets to 0 after grz_days days
    │
    ├── case "cnup" ── curve number update ───────────────────────────────────
    │   ├── chg_par(cn2(j), ...)                  modify CN2 (relative or absolute)
    │   └── curno(cn2(j), j)                      recompute CN lookup table
    │
    ├── case "burn" ── prescribed burn ────────────────────────────────────────
    │   └── pl_burnop(j, iburn)                   combust biomass/residue fraction
    │
    ├── case "swep" ── street sweeping ────────────────────────────────────────
    │   └── [no subroutine called]
    │         sets sweepop parameters (sweepeff, fr_curb)
    │         used in hru_urban / hru_urbanhr on the same day
    │
    ├── case "dwm" ── drainage water management ──────────────────────────────
    │   └── [no subroutine called]
    │         sets hru%lumv%sdr_dep and %ldrain
    │         ldrain controls which soil layer is the tile drain target
    │         used in swr_percmain
    │
    ├── case "weir" ── weir height adjustment ────────────────────────────────
    │   └── [no subroutine called]
    │         sets wet_ob(j)%weir_hgt, %pvol, %iweir
    │         new weir height takes effect in wetland_control next call
    │
    ├── case "irrp" ── paddy continuous irrigation ────────────────────────────
    │   └── [no subroutine called]
    │         sets irrig%eff, %frac_surq, %salt, %no3
    │         ├── [if mgt%op3 &lt; 0]   use irrop_db%amt_mm as target (manual)
    │         │     paddy_irr=1; irr_hmax from irrop_db
    │         ├── [if mgt%op3 > 0]   use mgt%op3 as target ponding depth
    │         │     paddy_irr=1; irr_hmax=mgt%op3
    │         └── [if mgt%op3 == 0]  stop paddy irrigation
    │               paddy_irr=0; irr_hmin=0
    │         wet_irrp called daily in hru_control when depth &lt; irr_hmin
    │
    ├── case "irpm" ── date-scheduled rice field irrigation ───────────────────
    │   └── [no subroutine called]
    │         same mechanics as "irrm" but intended for paddy fields
    │         sets irrig(j)%applied and %runoff
    │
    ├── case "pudl" ── puddling (paddy soil preparation) ─────────────────────
    │   ├── [no subroutine for soil adjustment]
    │   │     sets hru%wet_hc from pudl_db; sets wet%sed from pudl_db%sed
    │   └── tillage mixing
    │         ├── [if wet_ob(j)%depth > 0.001]   mgt_newtillmix_wet(j, idtill)
    │         ├── [else if bsn_cc%cswat == 1]    mgt_newtillmix_cswat1(j, 0., idtill)
    │         └── [else if bsn_cc%cswat == 0]    mgt_newtillmix_cswat0(j, 0., idtill)
    │
    └── case "skip" ── skip a simulation year ────────────────────────────────
          └── [no subroutine called]
                sets yr_skip(j) = 1
                mgt_operatn exits immediately on yr_skip
                hru_control skips mgt_operatn for the rest of this calendar year
                yr_skip reset to 0 at year end in time_control
</pre>

---

## cbn_zhang2

CENTURY 5-pool C/N model. Called once per day per HRU when `bsn_cc%cswat == 1`.
Litter inputs arrive from `cbn_surfrsd_decomp` (surface) and `cbn_rsd_transfer` (soil layers).

<pre>
cbn_zhang2
│
└── [soil layer loop k = 1, nly]
    │
    ├── Rate scalars
    │   ├── sut      soil water scalar
    │   ├── cdg      temperature scalar  fcgd(stemp(k))
    │   ├── ox       oxygen/depth scalar  (reduced below ~30 cm)
    │   ├── till_eff tillage disturbance scalar
    │   │     = 1.6 for till_eff_days days after mgt_newtillmix_cswat1
    │   │     = 1.0 otherwise
    │   └── cs = min(15,  sqrt(cdg × sut) × 0.9 × ox × till_eff)
    │         composite rate modifier applied to all transformations
    │
    ├── Denitrification
    │   └── NO3 → N₂ (wdn);  removed from soil1%mn(k)%no3
    │
    ├── Potential C transformations  (= rate_constant × cs × pool_mass)
    │   ├── str (structural litter)
    │   │     nonlig fraction → microb (a1) + CO2 (a1co2)
    │   │     lig   fraction  → slow humus 0.7×lig (lslctp) + CO2 0.3×lig
    │   ├── meta (metabolic litter)
    │   │     → microb (a1) + CO2 (a1co2)
    │   ├── microb (microbial biomass)   scaled by texture factor xbm
    │   │     → passive hp (abp) + slow hs (abs) + CO2 (abco2)
    │   ├── hs (slow humus)
    │   │     → microb (asx) + passive hp (asp) + CO2 (asco2)
    │   └── hp (passive humus, k > 1 only — zero in surface layer)
    │         → microb (apx) + CO2 (apco2)
    │
    ├── N supply–demand balance → reduction factor (reduc)
    │     (if N demand for immobilisation exceeds mineral N supply, reduc &lt; 1)
    │
    ├── Actual transformations = potential × reduc
    │
    ├── Pool mass updates
    │   microb(k)%c, hs(k)%c, hp(k)%c, str(k)%c, meta(k)%c, lig(k)%c, nonlig(k)%c
    │
    ├── N flux accounting via nut_np_flow for each pool-to-pool transfer
    │   → updates meta%n, str%n, microb%n, hs%n, hp%n, NO3, NH4
    │
    ├── CO2 respiration summed into hsc_d(j)%rsp_c  (soil respiration output)
    │
    └── Save per-layer diagnostics
          soil1(j)%org_flx_lr(k)   all C/N fluxes
          soil1(j)%org_con_lr(k)   sut, cdg, ox, cs, till_eff
          soil1(j)%org_allo_lr(k)  allocation fractions (a1, abp, abs, …)
          soil1(j)%org_tran_lr(k)  potential transformation rates

[end layer loop]
</pre>

---

## Carbon Pool Flow in `cbn_zhang2`

```
       ┌── INPUTS ──────────────────────────────────────────────────────┐
       │  cbn_surfrsd_decomp  →  meta(1), str(1)  (surface layer)      │
       │  cbn_rsd_transfer    →  meta(k), str(k)  (all layers)         │
       └────────────────────────────────────────────────────────────────┘

                  ┌──────────────────────────┐
                  ▼                          ▼
          METABOLIC LITTER          STRUCTURAL LITTER
          meta(k)%c, %n         str(k)%c = nonlig(k)%c + lig(k)%c

           │ a1co2→CO2            │ 0.3×lig → CO2
           │ a1                   │ nonlig → MICROBIAL (a1)
           └──────────────────────┤ 0.7×lig → SLOW HUMUS (hs)
                                  ▼
                           MICROBIAL  microb(k)%c, %n
                           ◄─── hs (asx) ◄─── hp (apx)
                                  │
                  ┌───────────────┼──────────────┐
                  ▼ abco2         ▼ abp           ▼ abs
                 CO2         PASSIVE (hp)     SLOW (hs)
                          hp(k)%c, %n      hs(k)%c, %n
                               │                │
                         apco2→CO2        asco2→CO2
                         apx →──────────────────────► MICROBIAL
                         asp →──────────────────────► PASSIVE (hp)

All CO2 terms accumulated in hsc_d(j)%rsp_c (soil respiration)
seq(k)%c = microb(k)%c + hs(k)%c + hp(k)%c    (sequestered C)
tot(k)%c = seq(k)%c + meta(k)%c + str(k)%c    (total organic C)
```

---

## Tillage Mixing — `mgt_newtillmix_cswat1`

Called from `mgt_sched` case `"till"` and case `"pudl"` (dry soil path) when `bsn_cc%cswat == 1`.

```
mgt_sched case "till" (cswat==1)
└── mgt_newtillmix_cswat1 (j, 0., idtill)
      [loop over layer pairs within tillage depth]
      ├── Mix microb(k)%c, microb(k)%n
      ├── Mix hs(k)%c,    hs(k)%n
      ├── Mix hp(k)%c,    hp(k)%n
      ├── Mix str(k)%c,   str(k)%n
      ├── Mix meta(k)%c,  meta(k)%n
      └── Mix lig(k)%c,   lig(k)%m
      Sets: tillage_switch(j)=1, tillage_days(j)=0, tillage_depth(j)
      → till_eff = 1.6 in cbn_zhang2 for the next till_eff_days days
```

---

## HRU Output Files Controlled by `print.prt`

### Carbon balance output

| File | `print.prt` flag | Condition | Contents |
|---|---|---|---|
| `hru_cb.csv` | `cb_hru` d/m/y/l | always | per-HRU carbon balance totals |
| `hru_cb_vars.csv` | `cb_vars_hru` d/m/y/l | `cswat==1` only | per-layer pool masses and fluxes |

### Landscape losses output

| Timestep | Flag | File |
|---|---|---|
| Daily | `pco%ls_hru%d` | `hru_ls_day.csv` |
| Monthly | `pco%ls_hru%m` | `hru_ls_mon.csv` |
| Yearly | `pco%ls_hru%y` | `hru_ls_yr.csv` |
| Avg ann | `pco%ls_hru%a` | `hru_ls_aa.csv` |

Key erosion columns accumulated in `hru_control` (`hls_d`):

| Column | Variable | Units | Source |
|---|---|---|---|
| `sedyld` | MUSLE sediment leaving HRU | t/ha | `ero_ysed.f90` |
| `usle` | USLE erosion estimate | t/ha | `ero_ysed.f90` |
| `sedorgn` | Organic N with sediment | kg/ha | `nut_orgn.f90` |
| `sedorgp` | Organic P with sediment | kg/ha | `nut_psed.f90` |
| `sedminp` | Mineral P with sediment | kg/ha | `nut_psed.f90` |
