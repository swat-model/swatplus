# SWAT+ Call Tree from `main.f90`

Full Swatplus call tree.  Click any linked name to jump to that routine's own call tree below.

---

## main

<pre>
main
├── INITIALIZATION ─────────────────────────────────────────────────────
│   ├── <a href="#proc_bsn">proc_bsn</a>              basin-level setup
│   ├── <a href="#proc_date_time">proc_date_time</a>
│   ├── <a href="#proc_db">proc_db</a>               read parameter databases
│   ├── <a href="#proc_read">proc_read</a>             read spatial input files
│   ├── <a href="#hyd_connect">hyd_connect</a>           build routing network graph
│   ├── <a href="#recalldb_read">recalldb_read</a>
│   ├── <a href="#exco_db_read">exco_db_read</a>
│   └── <a href="#dr_db_read">dr_db_read</a>
│
├── OBJECT SETUP ───────────────────────────────────────────────────────
│   ├── cli_lapse
│   ├── object_read_output
│   ├── om_water_init
│   ├── pest_cha_res_read / path_cha_res_read
│   ├── salt_cha_read / cs_cha_read
│   ├── <a href="#lsu_read_elements">lsu_read_elements</a>
│   ├── <a href="#proc_hru">proc_hru</a>              HRU initialisation
│   ├── <a href="#proc_cha">proc_cha</a>              channel initialisation
│   ├── <a href="#proc_aqu">proc_aqu</a>              aquifer initialisation
│   ├── dtbl_lum_read
│   ├── <a href="#hru_lte_read">hru_lte_read</a> / proc_cond
│   ├── res_read_weir / dtbl_res_read / dtbl_scen_read / cal_cond_read
│   ├── manure_allocation_read / dtbl_flocon_read
│   ├── om_treat_read / om_use_read / om_osrc_read
│   ├── water_treatment_read / water_use_read / water_tower_read
│   ├── water_pipe_read / water_canal_read / water_allocation_read
│   ├── hru_dtbl_actions_init
│   ├── <a href="#proc_res">proc_res</a> / wet_read_hyd / wet_read / wet_read_salt_cs
│   ├── [if db_mx%wet_dat > 0]  <a href="#wet_all_initial">wet_all_initial</a>
│   ├── wet_fp_init
│   ├── [loop ihru = 1, sp_ob%hru]  <a href="#soil_nutcarb_init">soil_nutcarb_init</a>
│   ├── <a href="#proc_cal">proc_cal</a>              calibration setup
│   ├── <a href="#proc_open">proc_open</a>             open output files, write headers
│   ├── <a href="#unit_hyd_ru_hru">unit_hyd_ru_hru</a> / dr_ru
│   └── hyd_connect_out
│
└── SIMULATION ─────────────────────────────────────────────────────────
    ├── [if time%step &lt; 0]  <a href="#command">command</a>         export-coefficient (average annual) mode
    └── [else]              <a href="#time_control">time_control</a>    standard year/day simulation loop
          [after loop] [if cal_soft == "y"]  <a href="#calsoft_control">calsoft_control</a>
                       [if cal_hard == "y"]  cal_parmchg_read → calhard_control
                       [if bsn_cc%swift_out == 1]  <a href="#swift_output">swift_output</a>
</pre>

---

## proc_bsn


**Called from:** [`main`](#main)

<pre>
proc_bsn
├── <a href="#readcio_read">readcio_read</a>             read file.cio
├── basin_read_cc            read basins.bsn  (sets bsn_cc%cswat, %crk, %lapse, etc.)
├── basin_read_objs
├── <a href="#time_read">time_read</a>
├── basin_read_prm
├── basin_prm_default
├── basin_print_codes_read   read print.prt  → sets all output flags
├── <a href="#co2_read">co2_read</a>
├── carbon_coef_read         read carbon_coef.cbn if present (overrides CENTURY rates)
└── open_output_file         open files.out, diagnostics.out, area_calc.out
</pre>

---

## proc_db


**Called from:** [`main`](#main)

<pre>
proc_db
├── plant_parm_read          plants.plt
├── <a href="#plantparm_init">plantparm_init</a>
├── plant_transplant_read
├── till_parm_read           tillage.til  (till_eff used in cbn_zhang2)
├── pest_parm_read
├── fert_parm_read
├── manure_orgmin_read / manure_db_read
├── urban_parm_read
├── path_parm_read / septic_parm_read
├── mgt_read_irrops / mgt_read_chemapp / mgt_read_harvops
├── mgt_read_grazeops / mgt_read_sweepops / mgt_read_fireops
├── <a href="#mgt_read_mgtops">mgt_read_mgtops</a> / mgt_read_puddle
├── sdr_read / sep_read
├── scen_read_grwway / scen_read_filtstrip / scen_read_bmpuser / sat_buff_read
├── readpcom
├── cntbl_read / cons_prac_read / overland_n_read
└── landuse_read
</pre>

---

## proc_read


**Called from:** [`main`](#main)

<pre>
proc_read
├── ch_read_temp / cli_read_atmodep / <a href="#cli_staread">cli_staread</a>
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


**Called from:** [`main`](#main)

<pre>
proc_hru
├── hru_allo / <a href="#hru_read">hru_read</a> / hrudb_init
├── <a href="#hru_lum_init_all">hru_lum_init_all</a> / <a href="#topohyd_init">topohyd_init</a> / hru_output_allo
├── carbon_read              read initial carbon state (organic C%, litter)
├── [loop HRUs]  <a href="#structure_set_parms">structure_set_parms</a>("septic")
├── <a href="#soils_init">soils_init</a>               compute FC, WP, BD for each layer
├── <a href="#structure_init">structure_init</a> / <a href="#plant_all_init">plant_all_init</a> / <a href="#cn2_init_all">cn2_init_all</a> / hydro_init
├── pesticide_init / pathogen_init / salt_hru_init / cs_hru_init
├── rte_read_nut
└── open_output_file         open erosion.out, checker.out
</pre>

---

## soil_nutcarb_init

Called from `main` in a loop over every HRU after `proc_hru` returns.

**Called from:** [`main`](#main)


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


**Called from:** [`main`](#main)

<pre>
proc_cha
├── ch_read_init / ch_read_init_cs
├── sd_hydsed_read / ch_read_hyd / ch_read_sed / ch_read_nut
├── ch_read / sd_channel_read / <a href="#sd_hydsed_init">sd_hydsed_init</a>
├── aqu2d_init
├── [loop channels]  ch_ttcoef / ch_initial
└── overbank_read / sd_channel_surf_link / time_conc_init
</pre>

---

## proc_aqu


**Called from:** [`main`](#main)

<pre>
proc_aqu
├── aqu_read / aqu_initial
└── aqu_read_init / aqu_read_init_cs
</pre>

---

## proc_cal


**Called from:** [`main`](#main)

<pre>
proc_cal
├── cal_parm_read / <a href="#cal_parmchg_read">cal_parmchg_read</a>
├── <a href="#pl_read_regions_cal">pl_read_regions_cal</a> / <a href="#pl_read_parms_cal">pl_read_parms_cal</a> / <a href="#cal_conditions">cal_conditions</a>
├── calsoft_read_codes / lcu_read_softcal / ls_read_lsparms_cal
├── <a href="#aqu_read_elements">aqu_read_elements</a> / <a href="#ch_read_elements">ch_read_elements</a> / <a href="#res_read_elements">res_read_elements</a> / <a href="#rec_read_elements">rec_read_elements</a>
├── ch_read_orders_cal / ch_read_parms_cal
└── cal_allo_init
</pre>

---

## proc_open


**Called from:** [`main`](#main)

<pre>
proc_open
├── <a href="#output_landscape_init">output_landscape_init</a>
├── <a href="#header_channel">header_channel</a> / <a href="#header_aquifer">header_aquifer</a> / <a href="#header_sd_channel">header_sd_channel</a> / <a href="#header_mgt">header_mgt</a>
├── <a href="#header_lu_change">header_lu_change</a> / <a href="#header_yield">header_yield</a> / <a href="#header_hyd">header_hyd</a> / <a href="#header_reservoir">header_reservoir</a> / <a href="#header_wetland">header_wetland</a>
├── <a href="#header_water_allocation">header_water_allocation</a> / <a href="#header_pest">header_pest</a> / <a href="#header_path">header_path</a>
├── <a href="#header_salt">header_salt</a> / <a href="#header_const">header_const</a>
└── <a href="#header_write">header_write</a>
</pre>

---

## time_control


**Called from:** [`calhard_control`](#calhard_control), [`main`](#main)

<pre>
time_control
│
├── <a href="#cli_precip_control">cli_precip_control</a> (0)   generate first-day precipitation
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
│   │   ├── <a href="#climate_control">climate_control</a>               read/generate weather
│   │   ├── cli_atmodep_time_control      set atmospheric deposition array counter
│   │   ├── [loop db_mx%cond_up]  <a href="#conditions">conditions</a> / <a href="#actions">actions</a>   conditional land-use reset
│   │   ├── [if db_mx%mallo_db > 0]  <a href="#mallo_control">mallo_control</a>        manure allocation
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

**Called from:** [`main`](#main), [`time_control`](#time_control)


<pre>
command
│
├── [linked-list loop: icmd walks ob(:)%cmd_next until icmd == 0]
│   │
│   ├── [pre-object]  <a href="#wallo_control">wallo_control</a>   water allocation without channel source
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
│   │   │   └── <a href="#hru_lte_control">hru_lte_control</a>
│   │   │
│   │   ├── case "ru"
│   │   │   ├── <a href="#ru_control">ru_control</a>
│   │   │   └── [if ob%rcv_tot > 0]  hyddep_output
│   │   │
│   │   ├── case "gwflow"
│   │   │   └── <a href="#gwflow_simulate">gwflow_simulate</a>
│   │   │
│   │   ├── case "aqu"
│   │   │   └── [if dfn_tot == 0]  <a href="#aqu_1d_control">aqu_1d_control</a>   (1-D BF recession)
│   │   │
│   │   ├── case "res"
│   │   │   └── [if rcv_tot > 0]  <a href="#res_control">res_control</a> (ires)
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
│   │       ├── [if chl > 1e-3]  <a href="#sd_channel_control3">sd_channel_control3</a>
│   │       └── [else: artificial channel (length≈0) — pass-through, zero morphology outputs]
│   │
│   └── [post-object]  <a href="#wallo_control">wallo_control</a> / [if pco%fdcout == "y"] flow_dur_curve
│
└── OUTPUT SECTION  (daily, after all objects processed)
    │
    ├── obj_output
    │
    ├── [loop ihru = 1, sp_ob%hru]
    │   ├── <a href="#hru_output">hru_output</a>
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

**Called from:** [`command`](#command)


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
│         <a href="#conditions">conditions</a> / <a href="#actions">actions</a>
│         [if future fertilizer scheduled today]  pl_fert
│
├── albedo                    compute daily albedo
│
├── [salt equilibrium chemistry]
│   └── [if cs_db%num_salts > 0]  <a href="#salt_chem_hru">salt_chem_hru</a>
│
├── [constituent reactions and sorption]
│   └── [if cs_db%num_cs > 0]
│         ├── <a href="#cs_rctn_hru">cs_rctn_hru</a>
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
│   ├── [if ires == 0]   <a href="#surface">surface</a>
│   │     ├── sq_dailycn       SCS curve number update
│   │     └── [if CN method == Green-Ampt]  sq_greenampt
│   └── [if ires > 0]   (wetland present — surfq=0, sedyld=0; no surface calculation)
│
├── [paddy continuous irrigation]
│   └── [if hru%paddy_irr > 0 and wet_ob%depth &lt; irr_hmin]  wet_irrp
│
├── [wetland/paddy processes]
│   ├── [if ires > 0]  <a href="#wetland_control">wetland_control</a>
│   └── [if ires == 0]  pass saturation excess to qday directly
│
├── <a href="#swr_percmain">swr_percmain</a>              percolation through soil profile
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
│       ├── [if bmix_eff > 1e-6]  <a href="#mgt_biomix">mgt_biomix</a> (ihru, bmix_eff)   biological mixing
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
├── <a href="#pl_community">pl_community</a>              plant community phenology decisions
├── <a href="#pl_grow">pl_grow</a>                   daily plant growth (LAI, biomass, root, N/P uptake)
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
│         └── [if time%step > 1]  <a href="#hru_urbanhr">hru_urbanhr</a>  (subdaily simulation)
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
│   └── [if sb%hru_src == j]  <a href="#conditions">conditions</a> / <a href="#actions">actions</a>   (tile diversion decision table)
│
├── sq_surfst                 store/release surface runoff
├── swr_subwq                 chl-a, CBOD, dissolved O₂
├── [if sed/nut concentrations > 0]  hru_urb_bmp
└── <a href="#hru_hyds">hru_hyds</a>                  assemble outflow hydrographs (hd arrays)
</pre>

---

## mgt_operatn

Called from `hru_control` when `yr_skip(j) == 0`.

**Called from:** [`hru_control`](#hru_control)

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

**Called from:** [`mgt_operatn`](#mgt_operatn)


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
    │               ├── <a href="#mgt_transplant">mgt_transplant</a>(itr)
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
    │         │   ├── "tuber"    → <a href="#mgt_harvtuber">mgt_harvtuber</a>  (j, ipl, iharvop)
    │         │   ├── "peanuts"  → <a href="#mgt_harvtuber">mgt_harvtuber</a>  (j, ipl, iharvop)
    │         │   ├── "stripper" → mgt_harvbiomass(j, ipl, iharvop)
    │         │   └── "picker"   → mgt_harvgrain  (j, ipl, iharvop)
    │         [if crop NOT growing]
    │             select case (harvop_db%typ)
    │             └── "residue"  → mgt_harvresidue(j, harveff, iharvop)
    │
    ├── case "kill" ── kill ───────────────────────────────────────────────────
    │   └── [loop ipl = 1, pcom%npl]
    │         [if gro == "y" and crop matches]
    │         ├── <a href="#mgt_killop">mgt_killop</a>(j, ipl)   move biomass to residue, zero plant state
    │         └── pcom%plcur(ipl)%phuacc = 0
    │
    ├── case "hvkl" ── harvest and kill ─────────────────────────────────────
    │   └── [loop ipl = 1, pcom%npl]
    │         [if gro == "y" and crop matches and biomass > bm_min]
    │         │   select case (harvop_db(iharvop)%typ)
    │         │   ├── "biomass"  → mgt_harvbiomass(j, ipl, iharvop)
    │         │   ├── "grain"    → mgt_harvgrain  (j, ipl, iharvop)
    │         │   ├── "residue"  → mgt_harvresidue(j, harveff, iharvop)
    │         │   ├── "tuber"    → <a href="#mgt_harvtuber">mgt_harvtuber</a>  (j, ipl, iharvop)
    │         │   ├── "peanuts"  → <a href="#mgt_harvtuber">mgt_harvtuber</a>  (j, ipl, iharvop)
    │         │   ├── "stripper" → mgt_harvgrain  (j, ipl, iharvop)
    │         │   └── "picker"   → mgt_harvgrain  (j, ipl, iharvop)
    │         └── <a href="#mgt_killop">mgt_killop</a>(j, ipl)   always called after harvest portion
    │         [if NOT growing]
    │             select case (harvop_db%typ)
    │             └── "residue"  → mgt_harvresidue(j, harveff, iharvop)
    │
    ├── case "till" ── tillage ────────────────────────────────────────────────
    │   ├── [if bsn_cc%cswat == 1]  <a href="#mgt_newtillmix_cswat1">mgt_newtillmix_cswat1</a>(j, 0., idtill)
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
    │   └── <a href="#curno">curno</a>(cn2(j), j)                      recompute CN lookup table
    │
    ├── case "burn" ── prescribed burn ────────────────────────────────────────
    │   └── <a href="#pl_burnop">pl_burnop</a>(j, iburn)                   combust biomass/residue fraction
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
    │         ├── [else if bsn_cc%cswat == 1]    <a href="#mgt_newtillmix_cswat1">mgt_newtillmix_cswat1</a>(j, 0., idtill)
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

**Called from:** [`hru_control`](#hru_control)


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
    ├── nut_np_flow              N flux accounting for each pool-to-pool transfer
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

**Called from:** [`mgt_sched`](#mgt_sched)


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

---

## proc_date_time

Called from `main` during INITIALIZATION; reads all measured weather time series into memory before the simulation loop.

**Called from:** [`main`](#main)


<pre>
proc_date_time
├── cli_petmeas          read PET measurement files
├── cli_pmeas            read precipitation measurement files
├── cli_tmeas            read temperature measurement files
├── cli_smeas            read solar radiation measurement files
├── cli_hmeas            read relative humidity measurement files
├── cli_wmeas            read wind speed measurement files
└── cli_wgnread          read weather generator parameter files
</pre>

---

## hyd_connect

Reads routing network configuration files and builds the object connectivity graph used by `command`.

**Called from:** [`main`](#main)


<pre>
hyd_connect
│
├── [if sp_ob%hru > 0]      hyd_read_connect(hru.con)
├── [if sp_ob%hru_lte > 0]  hyd_read_connect(hru_lte.con)
├── [if sp_ob%ru > 0]
│   ├── hyd_read_connect(rout_unit.con)
│   ├── <a href="#ru_read_elements">ru_read</a>              read routing unit definitions
│   └── <a href="#ru_read_elements">ru_read_elements</a>     map HRUs/elements to routing units
├── [if sp_ob%aqu > 0]
│   ├── <a href="#hyd_read_connect">hyd_read_connect</a>(aquifer.con)
│   └── <a href="#aqu2d_read">aqu2d_read</a>           read 2-D aquifer connections
├── [if sp_ob%chan > 0]
│   ├── <a href="#hyd_read_connect">hyd_read_connect</a>(channel.con)
│   └── overbank_read        read overbank floodplain linkages
├── [if sp_ob%res > 0]      <a href="#hyd_read_connect">hyd_read_connect</a>(reservoir.con)
├── [if sp_ob%recall > 0]   <a href="#hyd_read_connect">hyd_read_connect</a>(recall.con)
├── [if sp_ob%exco > 0]     <a href="#hyd_read_connect">hyd_read_connect</a>(exco.con)
├── [if sp_ob%dr > 0]
│   ├── <a href="#hyd_read_connect">hyd_read_connect</a>(delivratio.con)
│   └── <a href="#dr_db_read">dr_db_read</a>           read delivery ratio database
├── [if sp_ob%outlet > 0]   <a href="#hyd_read_connect">hyd_read_connect</a>(outlet.con)
├── [if sp_ob%chandeg > 0]  <a href="#hyd_read_connect">hyd_read_connect</a>(chandeg.con)
└── [if sp_ob%gwflow > 0]
    ├── gwflow_chan_read      read gwflow–channel cell linkage
    ├── <a href="#hyd_read_connect">hyd_read_connect</a>(gwflow.con)
    └── <a href="#gwflow_read">gwflow_read</a>          read gwflow spatial parameters
</pre>

---

## recalldb_read

Called from `main` after `hyd_connect`; loops over all recall objects and loads each recall input time series.

**Called from:** [`main`](#main)


<pre>
recalldb_read
└── [loop i = 1, sp_ob%recall]  recall_read(i)
</pre>

---

## exco_db_read

Called from `main`; reads export-coefficient object parameters.

**Called from:** [`main`](#main)


<pre>
exco_db_read
├── exco_read_om
├── [if cs_db%num_pests > 0]   exco_read_pest
├── [if cs_db%num_paths > 0]   exco_read_path
├── [if cs_db%num_hmet > 0]    exco_read_hmet
└── [if cs_db%num_salts > 0]   exco_read_salt
</pre>

---

## dr_db_read

Called from `main` (and conditionally from `hyd_connect` when `sp_ob%dr > 0`); reads delivery-ratio database parameters.

**Called from:** [`hyd_connect`](#hyd_connect), [`main`](#main)


<pre>
dr_db_read
├── dr_read_om
├── [if cs_db%num_pests > 0]   dr_read_pest
├── [if cs_db%num_paths > 0]   dr_path_read
├── [if cs_db%num_hmet > 0]    dr_read_hmet
└── [if cs_db%num_salts > 0]   dr_read_salt
</pre>

---

## climate_control

Called daily from `time_control` at the start of each day loop. Reads measured data or calls weather generators for each weather station (`iwst`). Each variable falls back to its generator when the measured value is missing or out of bounds.

**Called from:** [`time_control`](#time_control)


<pre>
climate_control
│
├── cli_precip_control (1)      read/generate today's precipitation for all gages
│
└── [loop iwst = 1, db_mx%wst]  per weather station:
    ├── cli_weatgn(iwgn)         update WGN state for this station/month
    ├── [temperature]
    │   ├── [if simulated]    cli_tgen(iwgn)      generate Tmax, Tmin
    │   └── [if measured]     read tmp(:)%ts; cli_tgen on missing/out-of-bounds
    │                         cli_bounds_check (tmp bounds)
    ├── [solar radiation]
    │   ├── cli_clgen(iwgn)       update sub-daily storm parameters
    │   ├── [if simulated]    cli_slrgen(iwgn)    generate solar radiation
    │   └── [if measured]     read slr(:)%ts; cli_slrgen on missing
    │                         cli_bounds_check (slr bounds)
    ├── [relative humidity]
    │   ├── [if simulated]    cli_rhgen(iwgn)     generate relative humidity
    │   └── [if measured]     read hmd(:)%ts; cli_rhgen on missing
    │                         cli_bounds_check (hmd bounds)
    ├── [wind speed]
    │   ├── [if simulated]    cli_wndgen(iwgn)    generate wind speed
    │   └── [if measured]     read wnd(:)%ts; cli_wndgen on missing
    │                         cli_bounds_check (wnd bounds)
    ├── [PET — if measured gage configured]
    │   └── read petm(:)%ts; Hargreaves method as fallback on missing values
    │       cli_bounds_check (petm bounds)
    ├── [CMI / 30-day P–PET rolling sum — inline, no sub-call]
    ├── [half-hour rainfall fraction — inline via Atri() function]
    └── [climate change adjustments — inline scaling of precip, T, solrad, rhum]
</pre>

---

## Subsystem Subroutines

The following sections document all SWAT+ subroutines reachable from `main` that call at least one other SWAT+ subroutine. Sections are ordered approximately by call depth (BFS order from `main`).

---

## cal_parmchg_read

this function computes new parameter value based on

**Called from:** [`main`](#main), [`proc_cal`](#proc_cal)


Source: `cal_parmchg_read.f90`

<pre>
cal_parmchg_read
└── define_unit_elements                save the object number of each defining unit
</pre>

---

## calhard_control

re-initialize all objects

**Called from:** [`main`](#main)


Source: `calhard_control.f90`

<pre>
calhard_control
├── re_initialize                       reset basin soil water for next simulation
└── <a href="#time_control">time_control</a>                        this subroutine contains the loops governing the modeling of processes
</pre>

---

## hru_lte_read

Source: `hru_lte_read.f90`

**Called from:** [`main`](#main)


<pre>
hru_lte_read
└── ascrv                               this subroutine computes shape parameters x5 and x6 for the S curve
</pre>

---

## lsu_read_elements

read landscape cataloging unit definitions for output (old subbasin output file)

**Called from:** [`main`](#main)


Source: `lsu_read_elements.f90`

<pre>
lsu_read_elements
└── define_unit_elements                save the object number of each defining unit
</pre>

---

## proc_res

allocate and initialize reservoir variables

**Called from:** [`main`](#main)


Source: `proc_res.f90`

<pre>
proc_res
├── res_allo
├── <a href="#res_initial">res_initial</a>                         set initial volumes for res and hru types and convert units
├── res_objects                         set reservoir object numbers for reservoir objects
├── res_read                            read reservoir.res
├── res_read_conds
├── res_read_csdb                       this subroutine reads reservoir water quality parameters for constituents
├── res_read_hyd
├── res_read_init                       read init
├── res_read_nut                        this subroutine reads data from the lake water quality input file (.lwq).
├── res_read_salt_cs                    read reservoir.res_cs
├── res_read_saltdb                     this subroutine reads reservoir water quality parameters for salt ions
└── res_read_sed                        this subroutine reads data from the lake water quality input file (.lwq).
</pre>

---

## unit_hyd_ru_hru

compute unit hydrographs for all hru and ru

**Called from:** [`main`](#main)


Source: `unit_hyd_ru_hru.f90`

<pre>
unit_hyd_ru_hru
└── unit_hyd                            This subroutine computes variables related to the watershed hydrology:
</pre>

---

## aqu_1d_control

set pointers to aquifer database and weather station

**Called from:** [`command`](#command)


Source: `aqu_1d_control.f90`

<pre>
aqu_1d_control
├── <a href="#cs_rctn_aqu">cs_rctn_aqu</a>                         this subroutine updates constituent concentrations based on chemical reactions in groundwater
├── cs_sorb_aqu                         this subroutine updates constituent concentrations based on sorption in the aquifer
└── <a href="#salt_chem_aqu">salt_chem_aqu</a>                       this subroutine calculates salt ion concentrations based on equilibrium chemical reactions, for an HRU
</pre>

---

## gwflow_simulate

this subroutine calculates new groundwater storage and solute mass for each gwflow grid cell;

**Called from:** [`command`](#command)


Source: `gwflow_simulate.f90`

<pre>
gwflow_simulate
├── gwflow_canal_div                    this subroutine calculates the water exchange volume between irrigation canals and connected grid cells
├── gwflow_canal_ext                    this subroutine calculates the water exchange volume between irrigation canals and connected grid cells
├── gwflow_gwet                         this subroutine determines the volume of groundwater that is removed from the
├── <a href="#gwflow_lateral">gwflow_lateral</a>                      this subroutine calculates lateral groundwater flow between adjacent cells
├── <a href="#gwflow_output_aa">gwflow_output_aa</a>                    this subroutine writes average annual gwflow output in SWAT+ long format:
├── gwflow_output_day                   this subroutine computes and writes daily gwflow output:
├── gwflow_output_mon                   writes monthly gwflow output
├── gwflow_output_yr                    writes yearly gwflow output
├── gwflow_phreatophyte                 this subroutine calculates the water removed from the aquifer via phreatophyte extraction
├── gwflow_pond                         this subroutine calculates the volume of seepage from recharge ponds;
├── gwflow_pump_ext                     this subroutine determines the volume of groundwater that is extracted
└── gwflow_rech                         this subroutine determines the volume of groundwater that is added to the aquifer via recharge (soil percolation)
</pre>

---

## hru_lte_control

Source: `hru_lte_control.f90`

**Called from:** [`command`](#command)


<pre>
hru_lte_control
├── <a href="#actions">actions</a>
└── <a href="#conditions">conditions</a>                          current conditions include: w_stress, n_stress, phu_plant, phu_base0, soil_water, jday, month, vol
</pre>

---

## hru_output

this subroutine outputs HRU variables on daily, monthly and annual time steps

**Called from:** [`command`](#command)


Source: `hru_output.f90`

<pre>
hru_output
└── soil_nutcarb_write                  this subroutine writes soil carbon output.
</pre>

---

## res_control

Source: `res_control.f90`

**Called from:** [`command`](#command)


<pre>
res_control
├── [if bsn_cc%lapse == 1]  cli_lapse   lapse-rate weather adjustment for reservoir elevation
├── <a href="#conditions">conditions</a>                          current conditions include: w_stress, n_stress, phu_plant, phu_base0, soil_water, jday, month, vol
├── gwflow_reservoir                    this subroutine calculates the water exchange volume between the reservoir and the connected grid cells
├── res_cs                              this subroutine computes the reservoir constituent mass balance
├── res_hydro                           Jose T 2025 |  Doell method
├── res_nutrient                        if reservoir volume less than 1 m^3, set all nutrient levels to
├── res_pest                            this subroutine computes the lake hydrologic pesticide balance.
├── <a href="#res_rel_conds">res_rel_conds</a>                       ictbl = 1     nbs
├── res_salt                            this subroutine computes the reservoir salt ion balance
├── res_sediment                        reservoir is empty
│
[+ 1 calls to external/intrinsic routines not in src/]
</pre>

---

## ru_control

name        |units         |definition

**Called from:** [`command`](#command)


Source: `ru_control.f90`

<pre>
ru_control
└── flow_hyd_ru_hru                     this subroutine determines the subdaily flow hydrographs for hru's, ru's and inflow fractions
</pre>

---

## sd_channel_control3

rtb floodplain

**Called from:** [`command`](#command)


Source: `sd_channel_control3.f90`

<pre>
sd_channel_control3
├── [if bsn_cc%lapse == 1]  cli_lapse   lapse-rate weather adjustment
├── wallo_control               water allocation for channel objects
├── <a href="#ch_rtmusk">ch_rtmusk</a>                           this subroutine routes a daily flow through a reach using the
├── ch_rtpath                           this subroutine routes bacteria through the stream network
├── ch_rtpest                           this subroutine computes the daily stream pesticide balance
├── ch_temp                             parameters for temperature model
├── <a href="#ch_watqual4">ch_watqual4</a>                         this subroutine performs in-stream nutrient transformations and water
├── gwflow_canal                        this subroutine calculates the water exchange volume between irrigation canals and connected grid cells
├── gwflow_channel_exch                 this subroutine calculates the water exchange volume between the channel and the connected grid cells
├── gwflow_satexcess                    this subroutine calculates the groundwater volume that enters the channel via saturation excess flow
├── gwflow_tile                         this subroutine calculates the water exchange volume between irrigation canals and connected grid cells
├── <a href="#rcurv_interp_flo">rcurv_interp_flo</a>                    this subroutine interpolates between points on a rating curve given flow rate
└── <a href="#sd_channel_sediment3">sd_channel_sediment3</a>
</pre>

---

## aqu2d_read

read data for aquifer elements for 2-D groundwater model

**Called from:** [`hyd_connect`](#hyd_connect)


Source: `aqu2d_read.f90`

<pre>
aqu2d_read
└── define_unit_elements                save the object number of each defining unit
</pre>

---

## gwflow_read

water balance and solute balance output file headers

**Called from:** [`hyd_connect`](#hyd_connect)


Source: `gwflow_read.f90`

<pre>
gwflow_read
└── <a href="#gwflow_output_init">gwflow_output_init</a>                  this subroutine opens all gwflow output files and writes headers
</pre>

---

## hyd_read_connect

con_file ==> connect file for spatial object

**Called from:** [`hyd_connect`](#hyd_connect)


Source: `hyd_read_connect.f90`

<pre>
hyd_read_connect
└── search
</pre>

---

## ru_read_elements

read data for each element in all subbasins

**Called from:** [`hyd_connect`](#hyd_connect)


Source: `ru_read_elements.f90`

<pre>
ru_read_elements
└── define_unit_elements                save the object number of each defining unit
</pre>

---

## co2_read

output annual CO2

**Called from:** [`proc_bsn`](#proc_bsn)


Source: `co2_read.f90`

<pre>
co2_read
└── open_output_file                    Get full path
</pre>

---

## readcio_read

read file.cio

**Called from:** [`proc_bsn`](#proc_bsn)


Source: `readcio_read.f90`

<pre>
readcio_read
└── init_output_path                    Detect OS - Runtime check is more robust if preprocessor fails
</pre>

---

## time_read

read weather codes

**Called from:** [`proc_bsn`](#proc_bsn)


Source: `time_read.f90`

<pre>
time_read
└── xmon                                this subroutine determines the month, given the julian date and leap
</pre>

---

## aqu_read_elements

Source: `aqu_read_elements.f90`

**Called from:** [`proc_cal`](#proc_cal)


<pre>
aqu_read_elements
└── define_unit_elements                save the object number of each defining unit
</pre>

---

## cal_conditions

Source: `cal_conditions.f90`

**Called from:** [`proc_cal`](#proc_cal)


<pre>
cal_conditions
└── <a href="#cal_parm_select">cal_parm_select</a>                     this subroutine finds the current parameter value based on
</pre>

---

## ch_read_elements

Source: `ch_read_elements.f90`

**Called from:** [`proc_cal`](#proc_cal)


<pre>
ch_read_elements
└── define_unit_elements                save the object number of each defining unit
</pre>

---

## pl_read_parms_cal

Source: `pl_read_parms_cal.f90`

**Called from:** [`proc_cal`](#proc_cal)


<pre>
pl_read_parms_cal
└── define_unit_elements                save the object number of each defining unit
</pre>

---

## pl_read_regions_cal

Source: `pl_read_regions_cal.f90`

**Called from:** [`proc_cal`](#proc_cal)


<pre>
pl_read_regions_cal
└── define_unit_elements                save the object number of each defining unit
</pre>

---

## rec_read_elements

Source: `rec_read_elements.f90`

**Called from:** [`proc_cal`](#proc_cal)


<pre>
rec_read_elements
└── define_unit_elements                save the object number of each defining unit
</pre>

---

## res_read_elements

Source: `res_read_elements.f90`

**Called from:** [`proc_cal`](#proc_cal)


<pre>
res_read_elements
└── define_unit_elements                save the object number of each defining unit
</pre>

---

## sd_hydsed_init

Source: `sd_hydsed_init.f90`

**Called from:** [`proc_cha`](#proc_cha)


<pre>
sd_hydsed_init
├── hyd_convert_conc_to_mass            m3/s to m3
├── <a href="#rcurv_interp_dep">rcurv_interp_dep</a>                    this subroutine interpolates between points on a rating curve given flow rate
└── sd_rating_curve                     use hydrograph_module
</pre>

---

## cli_tmeas

read all measured daily temperature data

**Called from:** [`proc_date_time`](#proc_date_time)


Source: `cli_tmeas.f90`

<pre>
cli_tmeas
└── xmon                                this subroutine determines the month, given the julian date and leap
</pre>

---

## cli_wgnread

read weather generator data from weather_generator.dat - wgn parameters

**Called from:** [`proc_date_time`](#proc_date_time)


Source: `cli_wgnread.f90`

<pre>
cli_wgnread
├── cli_initwgn                         this subroutine initializes the HRU weather generator parameters from the
└── gcycl                               This subroutine initializes the random number seeds. If the user
</pre>

---

## mgt_read_mgtops

read mgtops.dat file

**Called from:** [`proc_db`](#proc_db)


Source: `mgt_read_mgtops.f90`

<pre>
mgt_read_mgtops
└── read_mgtops
</pre>

---

## plantparm_init

set default values

**Called from:** [`proc_db`](#proc_db)


Source: `plantparm_init.f90`

<pre>
plantparm_init
└── ascrv                               this subroutine computes shape parameters x5 and x6 for the S curve
</pre>

---

## cn2_init_all

assign topography and hyd parameters

**Called from:** [`proc_hru`](#proc_hru)


Source: `cn2_init_all.f90`

<pre>
cn2_init_all
└── <a href="#cn2_init">cn2_init</a>                            assign cn2
</pre>

---

## hru_lum_init_all

Source: `hru_lum_init_all.f90`

**Called from:** [`proc_hru`](#proc_hru)


<pre>
hru_lum_init_all
└── hru_lum_init                        assign land use pointers for the hru
</pre>

---

## hru_read

Source: `hru_read.f90`

**Called from:** [`proc_hru`](#proc_hru)


<pre>
hru_read
└── <a href="#allocate_parms">allocate_parms</a>                      this subroutine allocates array sizes
</pre>

---

## plant_all_init

assign land use pointers for the hru

**Called from:** [`proc_hru`](#proc_hru)


Source: `plant_all_init.f90`

<pre>
plant_all_init
└── <a href="#plant_init">plant_init</a>                          use hru_lte_module
</pre>

---

## soils_init

Section 1

**Called from:** [`proc_hru`](#proc_hru)


Source: `soils_init.f90`

<pre>
soils_init
├── soil_phys_init                      this subroutine initializes soil physical properties
├── soils_test_adjust                   Adjust the input soil values based input soil test values.
└── layersplit                          split soil layer for thickness adjustments (called twice)
</pre>

---

## structure_init

set parameters for structural land use/management

**Called from:** [`proc_hru`](#proc_hru)


Source: `structure_init.f90`

<pre>
structure_init
└── <a href="#structure_set_parms">structure_set_parms</a>                 this subroutine controls the simulation of the land phase of the
</pre>

---

## structure_set_parms

this subroutine controls the simulation of the land phase of the

**Called from:** [`actions`](#actions), [`proc_hru`](#proc_hru), [`structure_init`](#structure_init)


Source: `structure_set_parms.f90`

<pre>
structure_set_parms
└── ttcoef_wway                         this subroutine computes travel time coefficients for routing
</pre>

---

## topohyd_init

assign topography and hyd parameters

**Called from:** [`proc_hru`](#proc_hru)


Source: `topohyd_init.f90`

<pre>
topohyd_init
└── ascrv                               this subroutine computes shape parameters x5 and x6 for the S curve
</pre>

---

## header_aquifer

AQUIFER

**Called from:** [`proc_open`](#proc_open)


Source: `header_aquifer.f90`

<pre>
header_aquifer
└── open_output_file                    Get full path
</pre>

---

## header_channel

CHANNEL

**Called from:** [`proc_open`](#proc_open)


Source: `header_channel.f90`

<pre>
header_channel
└── open_output_file                    Get full path
</pre>

---

## header_const

Source: `header_const.f90`

**Called from:** [`proc_open`](#proc_open)


<pre>
header_const
└── open_output_file                    Get full path
</pre>

---

## header_hyd

HYDCON (no headers)

**Called from:** [`proc_open`](#proc_open)


Source: `header_hyd.f90`

<pre>
header_hyd
└── open_output_file                    Get full path
</pre>

---

## header_lu_change

open lu_change output file

**Called from:** [`proc_open`](#proc_open)


Source: `header_lu_change.f90`

<pre>
header_lu_change
└── open_output_file                    Get full path
</pre>

---

## header_mgt

open mgt.out file

**Called from:** [`proc_open`](#proc_open)


Source: `header_mgt.f90`

<pre>
header_mgt
└── open_output_file                    Get full path
</pre>

---

## header_path

HRU_PATHOGEN - daily

**Called from:** [`proc_open`](#proc_open)


Source: `header_path.f90`

<pre>
header_path
└── open_output_file                    Get full path
</pre>

---

## header_pest

HRU_PESTICIDE - daily

**Called from:** [`proc_open`](#proc_open)


Source: `header_pest.f90`

<pre>
header_pest
└── open_output_file                    Get full path
</pre>

---

## header_reservoir

use hydrograph_module, only : res, sp_ob

**Called from:** [`proc_open`](#proc_open)


Source: `header_reservoir.f90`

<pre>
header_reservoir
└── open_output_file                    Get full path
</pre>

---

## header_salt

Source: `header_salt.f90`

**Called from:** [`proc_open`](#proc_open)


<pre>
header_salt
└── open_output_file                    Get full path
</pre>

---

## header_sd_channel

SWAT-DEG CHANNEL - SUBDAILY OUTPUT

**Called from:** [`proc_open`](#proc_open)


Source: `header_sd_channel.f90`

<pre>
header_sd_channel
└── open_output_file                    Get full path
</pre>

---

## header_water_allocation

Water Allocation Output

**Called from:** [`proc_open`](#proc_open)


Source: `header_water_allocation.f90`

<pre>
header_water_allocation
└── open_output_file                    Get full path
</pre>

---

## header_wetland

RESERVOIR/WETLAND - DAILY

**Called from:** [`proc_open`](#proc_open)


Source: `header_wetland.f90`

<pre>
header_wetland
└── open_output_file                    Get full path
</pre>

---

## header_write

Source: `header_write.f90`

**Called from:** [`proc_open`](#proc_open)


<pre>
header_write
└── open_output_file                    Get full path
</pre>

---

## header_yield

yield biomass file

**Called from:** [`proc_open`](#proc_open)


Source: `header_yield.f90`

<pre>
header_yield
└── open_output_file                    Get full path
</pre>

---

## output_landscape_init

HRU - Water balance

**Called from:** [`proc_open`](#proc_open)


Source: `output_landscape_init.f90`

<pre>
output_landscape_init
├── open_output_file                    Get full path
└── soil_nutcarb_write                  this subroutine writes soil carbon output.
</pre>

---

## res_initial

set initial volumes for res and hru types and convert units

**Called from:** [`proc_res`](#proc_res)


Source: `res_initial.f90`

<pre>
res_initial
└── res_convert_mass
</pre>

---

## actions

Source: `actions.f90`

**Called from:** [`hru_control`](#hru_control), [`hru_lte_control`](#hru_lte_control), [`mallo_control`](#mallo_control), [`time_control`](#time_control)


<pre>
actions
├── <a href="#cn2_init">cn2_init</a>                            assign cn2
├── cs_fert                             this subroutine adds constituent fertilizer to the soil profile
├── <a href="#curno">curno</a>                               this subroutine determines the curve numbers for moisture conditions
├── hru_fr_change                       read data for each element in all routing units
├── hru_lum_init                        assign land use pointers for the hru
├── mgt_harvbiomass                     this subroutine performs the harvest operation for above ground biomass (no kill)
├── mgt_harvgrain                       this subroutine performs the harvest grain only operation
├── mgt_harvresidue                     this subroutine performs the harvest residue operation
├── <a href="#mgt_harvtuber">mgt_harvtuber</a>                       this subroutine performs the harvest grain only operation
├── <a href="#mgt_killop">mgt_killop</a>                          this subroutine performs the kill operation
├── mgt_newtillmix_cswat0               this subroutine mixes residue and nutrients during tillage and
├── <a href="#mgt_newtillmix_cswat1">mgt_newtillmix_cswat1</a>               this subroutine mixes residue and nutrients during tillage and
├── mgt_newtillmix_wet                  this subroutine mixes residue and nutrients in soil layers and ponding water during tillage
├── <a href="#mgt_transplant">mgt_transplant</a>                      set initial heat units and other data
├── pest_apply                          this subroutine applies pesticide
├── <a href="#pl_burnop">pl_burnop</a>                           this subroutine performs all management operations
├── pl_fert                             this subroutine applies N and P specified by date and
├── pl_fert_wet                         this subroutine applies N and P specified by date and
├── pl_graze                            graze only if adequate biomass in HRU
├── pl_manure                           this subroutine applies N and P specified by date and
├── <a href="#plant_init">plant_init</a>                          use hru_lte_module
├── salt_fert                           this subroutine adds salt fertilizer to the soil profile
├── <a href="#structure_set_parms">structure_set_parms</a>                 this subroutine controls the simulation of the land phase of the
└── <a href="#wet_initial">wet_initial</a>                         check if hru can store surface water
</pre>

---

## cli_precip_control

this subroutine controls weather inputs to SWAT. Precipitation and

**Called from:** [`climate_control`](#climate_control), [`time_control`](#time_control)


Source: `cli_precip_control.f90`

<pre>
cli_precip_control
├── cli_bounds_check                    this subroutine checks to see if climate data is in current simulation day
├── cli_pgen                            this subroutine generates precipitation data when the user chooses to
└── cli_pgenhr                          this subroutine distributes daily rainfall exponentially within the day
</pre>

---

## conditions

current conditions include: w_stress, n_stress, phu_plant, phu_base0, soil_water, jday, month, vol

**Called from:** [`hru_control`](#hru_control), [`hru_lte_control`](#hru_lte_control), [`mallo_control`](#mallo_control), [`res_control`](#res_control), [`time_control`](#time_control), [`wetland_control`](#wetland_control)


Source: `conditions.f90`

<pre>
conditions
├── cond_integer                        current conditions include: w_stress, n_stress, phu_plant, phu_base0, soil_water, jday, month, vol
└── cond_real                           current conditions include: w_stress, n_stress, phu_plant, phu_base0, soil_water, jday, month, vol
</pre>

---

## mallo_control

zero demand, withdrawal, and unmet for entire allocation object

**Called from:** [`time_control`](#time_control)


Source: `mallo_control.f90`

<pre>
mallo_control
├── <a href="#actions">actions</a>
├── <a href="#conditions">conditions</a>                          current conditions include: w_stress, n_stress, phu_plant, phu_base0, soil_water, jday, month, vol
└── pl_fert                             this subroutine applies N and P specified by date and
</pre>

---

## cs_rctn_aqu

this subroutine updates constituent concentrations based on chemical reactions in groundwater

**Called from:** [`aqu_1d_control`](#aqu_1d_control)


Source: `cs_rctn_aqu.f90`

<pre>
cs_rctn_aqu
└── se_reactions_aquifer                get concentration of SeO4 and SeO3
</pre>

---

## salt_chem_aqu

this subroutine calculates salt ion concentrations based on equilibrium chemical reactions, for an HRU

**Called from:** [`aqu_1d_control`](#aqu_1d_control)


Source: `salt_chem_aqu.f90`

<pre>
salt_chem_aqu
├── activity_coefficient
├── caco3
├── caso4
├── cationexchange                      CEC selected based on soil type; for simplicity, for now used one value based on the sandy-loam soil type
├── ionic_strength
├── mgco3
├── mgso4
└── nacl
</pre>

---

## gwflow_lateral

this subroutine calculates lateral groundwater flow between adjacent cells

**Called from:** [`gwflow_simulate`](#gwflow_simulate)


Source: `gwflow_lateral.f90`

<pre>
gwflow_lateral
├── gwflow_heat                         this subroutine calculates heat advection and dispersion for groundwater
└── <a href="#gwflow_solute">gwflow_solute</a>                       this subroutine calculates solute advection, dispersion, chemical
</pre>

---

## gwflow_output_aa

this subroutine writes average annual gwflow output in SWAT+ long format:

**Called from:** [`gwflow_simulate`](#gwflow_simulate)


Source: `gwflow_output.f90`

<pre>
gwflow_output_aa
└── gwflow_write_cell_array             Writes active cell values as a single row.
</pre>

---

## cs_rctn_hru

this subroutine updates constituent concentrations based on chemical reactions and sorption in the soil profile

**Called from:** [`hru_control`](#hru_control)


Source: `cs_rctn_hru.f90`

<pre>
cs_rctn_hru
└── se_reactions_soil                   suppress unused variable warning
</pre>

---

## hru_hyds

this subroutine summarizes data for subbasins with multiple HRUs and

**Called from:** [`hru_control`](#hru_control)


Source: `hru_hyds.f90`

<pre>
hru_hyds
└── flow_hyd_ru_hru                     this subroutine determines the subdaily flow hydrographs for hru's, ru's and inflow fractions
</pre>

---

## hru_urbanhr

this subroutine computes loadings from urban areas using the

**Called from:** [`hru_control`](#hru_control)


Source: `hru_urbanhr.f90`

<pre>
hru_urbanhr
└── hru_sweep                           the subroutine performs the street sweeping operation
</pre>

---

## pl_community

this subroutine predicts daily potential growth of total plant

**Called from:** [`hru_control`](#hru_control)


Source: `pl_community.f90`

<pre>
pl_community
└── <a href="#pl_waterup">pl_waterup</a>                          this subroutine distributes potential plant evaporation through
</pre>

---

## pl_grow

Source: `pl_grow.f90`

**Called from:** [`hru_control`](#hru_control)


<pre>
pl_grow
├── <a href="#pl_biomass_gro">pl_biomass_gro</a>
├── pl_dormant                          this subroutine checks the dormant status of the different plant types
├── [if time%end_yr == 1]  pl_mortality  annual plant mortality
├── pl_leaf_gro                         this subroutine adjusts plant biomass, leaf area index, and canopy height
├── pl_leaf_senes                       lai decline for annuals - if dlai < phuacc < 1
├── <a href="#pl_nut_demand">pl_nut_demand</a>                       this subroutine predicts daily potential growth of total plant
├── pl_partition                        update plant mass for daily biomass/c increase
├── <a href="#pl_root_gro">pl_root_gro</a>                         calculate root depth
└── pl_seed_gro                         calculate plant ET values when heat units exceed 0.5
</pre>

---

## salt_chem_hru

this subroutine calculates salt ion concentrations based on equilibrium chemical reactions, for an HRU

**Called from:** [`hru_control`](#hru_control)


Source: `salt_chem_hru.f90`

<pre>
salt_chem_hru
├── activity_coefficient
├── caco3
├── caso4
├── cationexchange                      CEC selected based on soil type; for simplicity, for now used one value based on the sandy-loam soil type
├── ionic_strength
├── mgco3
├── mgso4
└── nacl
</pre>

---

## surface

this subroutine models surface hydrology at any desired time step

**Called from:** [`hru_control`](#hru_control)


Source: `surface.f90`

<pre>
surface
├── ero_cfactor                         this subroutine predicts daily soil loss caused by water erosion
├── ero_eiusle                          This subroutine computes the USLE erosion index (EI)
├── ero_ovrsed                          this subroutine computes splash erosion by raindrop impact and flow erosion by overland flow
├── ero_pkq                             this subroutine computes the peak runoff rate for each HRU
├── ero_ysed                            this subroutine predicts daily soil loss caused by water erosion
├── sq_dailycn                          Calculates curve number for the day in the HRU
├── [if surfq > 0 and bsn_cc%crk == 1]  sq_crackflow   route surface runoff into soil cracks
└── <a href="#sq_volq">sq_volq</a>                             Call subroutines to calculate the current day"s CN for the HRU and
</pre>

---

## swr_percmain

this subroutine is the master soil percolation component.

**Called from:** [`hru_control`](#hru_control)


Source: `swr_percmain.f90`

<pre>
swr_percmain
├── gwflow_soil                         this subroutine calculates the water exchange volume between the aquifer and the soil profile
├── <a href="#swr_drains">swr_drains</a>                          this subroutine finds the effective lateral hydraulic conductivity
├── swr_origtile                        this subroutine computes tile drainage using basic tile equations
├── swr_percmacro                       this surboutine computes percolation by crack flow
├── swr_percmicro                       this subroutine computes percolation and lateral subsurface flow
└── swr_satexcess                       this subroutine moves water to upper layers if saturated and can't perc
</pre>

---

## wetland_control

Source: `wetland_control.f90`

**Called from:** [`hru_control`](#hru_control)


<pre>
wetland_control
├── <a href="#conditions">conditions</a>                          current conditions include: w_stress, n_stress, phu_plant, phu_base0, soil_water, jday, month, vol
├── ero_cfactor                         this subroutine predicts daily soil loss caused by water erosion
├── gwflow_wetland                      this subroutine determines the volume of groundwater exchanged with wetlands
├── res_hydro                           Jose T 2025 |  Doell method
├── res_nutrient                        if reservoir volume less than 1 m^3, set all nutrient levels to
├── res_sediment                        reservoir is empty
├── res_weir_release                    suppress unused variable warning
├── wet_cs                              this subroutine computes the wetland constituent mass balance
└── wet_salt                            this subroutine computes the wetland salt ion mass balance
</pre>

---

## res_rel_conds

ictbl = 1     nbs

**Called from:** [`res_control`](#res_control)


Source: `res_rel_conds.f90`

<pre>
res_rel_conds
├── cond_integer_c
└── cond_real_c
</pre>

---

## ch_rtmusk

this subroutine routes a daily flow through a reach using the

**Called from:** [`sd_channel_control3`](#sd_channel_control3)


Source: `ch_rtmusk.f90`

<pre>
ch_rtmusk
└── <a href="#rcurv_interp_flo">rcurv_interp_flo</a>                    this subroutine interpolates between points on a rating curve given flow rate
</pre>

---

## ch_watqual4

this subroutine performs in-stream nutrient transformations and water

**Called from:** [`sd_channel_control3`](#sd_channel_control3)


Source: `ch_watqual4.f90`

<pre>
ch_watqual4
└── <a href="#rcurv_interp_flo">rcurv_interp_flo</a>                    this subroutine interpolates between points on a rating curve given flow rate
</pre>

---

## rcurv_interp_flo

this subroutine interpolates between points on a rating curve given flow rate

**Called from:** [`ch_rtmusk`](#ch_rtmusk), [`ch_watqual4`](#ch_watqual4), [`sd_channel_control3`](#sd_channel_control3), [`sd_channel_sediment3`](#sd_channel_sediment3)


Source: `rcurv_interp_flo.f90`

<pre>
rcurv_interp_flo
└── chrc_interp
</pre>

---

## sd_channel_sediment3

Source: `sd_channel_sediment3.f90`

**Called from:** [`sd_channel_control3`](#sd_channel_control3)


<pre>
sd_channel_sediment3
├── gwflow_floodplain                   this subroutine calculates the water exchange volume between the floodplain and the connected grid cells
└── <a href="#rcurv_interp_flo">rcurv_interp_flo</a>                    this subroutine interpolates between points on a rating curve given flow rate
</pre>

---

## gwflow_output_init

this subroutine opens all gwflow output files and writes headers

**Called from:** [`gwflow_read`](#gwflow_read)


Source: `gwflow_output.f90`

<pre>
gwflow_output_init
└── gwflow_write_celldef                Writes gwflow_cell_definition.txt once during initialization.
</pre>

---

## cal_parm_select

this subroutine finds the current parameter value based on

**Called from:** [`cal_conditions`](#cal_conditions)


Source: `cal_parm_select.f90`

<pre>
cal_parm_select
├── <a href="#curno">curno</a>                               this subroutine determines the curve numbers for moisture conditions
├── soil_awc_init                       this subroutine initializes soil parameters based on awc
└── soil_text_init                      this subroutine initializes soil parameters based on awc
</pre>

---

## rcurv_interp_dep

this subroutine interpolates between points on a rating curve given flow rate

**Called from:** [`sd_hydsed_init`](#sd_hydsed_init)


Source: `rcurv_interp_dep.f90`

<pre>
rcurv_interp_dep
└── chrc_interp
</pre>

---

## cn2_init

assign cn2

**Called from:** [`actions`](#actions), [`cn2_init_all`](#cn2_init_all)


Source: `cn2_init.f90`

<pre>
cn2_init
└── <a href="#curno">curno</a>                               this subroutine determines the curve numbers for moisture conditions
</pre>

---

## allocate_parms

this subroutine allocates array sizes

**Called from:** [`hru_read`](#hru_read)


Source: `allocate_parms.f90`

<pre>
allocate_parms
├── zero0                               this subroutine initializes the values for some of the arrays
├── zero1                               this subroutine initializes the values for some of the arrays
├── zero2                               this subroutine zeros all array values
└── zeroini                             this subroutine zeros values for single array variables
</pre>

---

## plant_init

use hru_lte_module

**Called from:** [`actions`](#actions), [`plant_all_init`](#plant_all_init)


Source: `plant_init.f90`

<pre>
plant_init
├── pl_partition                        update plant mass for daily biomass/c increase
├── <a href="#pl_root_gro">pl_root_gro</a>                         calculate root depth
├── pl_rootfr                           This subroutine distributes dead root mass through the soil profile
├── pl_seed_gro                         calculate plant ET values when heat units exceed 0.5
└── xmon                                this subroutine determines the month, given the julian date and leap
</pre>

---

## curno

this subroutine determines the curve numbers for moisture conditions

**Called from:** [`actions`](#actions), [`cal_parm_select`](#cal_parm_select), [`cn2_init`](#cn2_init), [`mgt_sched`](#mgt_sched), [`pl_burnop`](#pl_burnop)


Source: `curno.f90`

<pre>
curno
└── ascrv                               this subroutine computes shape parameters x5 and x6 for the S curve
</pre>

---

## mgt_harvtuber

this subroutine performs the harvest grain only operation

**Called from:** [`actions`](#actions), [`mgt_sched`](#mgt_sched)


Source: `mgt_harvtuber.f90`

<pre>
mgt_harvtuber
└── pl_rootfr                           This subroutine distributes dead root mass through the soil profile
</pre>

---

## mgt_killop

this subroutine performs the kill operation

**Called from:** [`actions`](#actions), [`mgt_sched`](#mgt_sched)


Source: `mgt_killop.f90`

<pre>
mgt_killop
├── pl_rootfr                           This subroutine distributes dead root mass through the soil profile
└── plg_zero
</pre>

---

## mgt_newtillmix_cswat1

this subroutine mixes residue and nutrients during tillage and

**Called from:** [`actions`](#actions), [`mgt_sched`](#mgt_sched)


Source: `mgt_newtillmix_cswat1.f90`

<pre>
mgt_newtillmix_cswat1
└── mgt_tillfactor                      Armen 16 January 2008
</pre>

---

## mgt_transplant

set initial heat units and other data

**Called from:** [`actions`](#actions), [`mgt_sched`](#mgt_sched)


Source: `mgt_transplant.f90`

<pre>
mgt_transplant
├── pl_partition                        update plant mass for daily biomass/c increase
├── <a href="#pl_root_gro">pl_root_gro</a>                         calculate root depth
└── pl_seed_gro                         calculate plant ET values when heat units exceed 0.5
</pre>

---

## pl_burnop

this subroutine performs all management operations

**Called from:** [`actions`](#actions), [`mgt_sched`](#mgt_sched)


Source: `pl_burnop.f90`

<pre>
pl_burnop
└── <a href="#curno">curno</a>                               this subroutine determines the curve numbers for moisture conditions
</pre>

---

## wet_initial

check if hru can store surface water

**Called from:** [`actions`](#actions)


Source: `wet_initial.f90`

<pre>
wet_initial
└── res_convert_mass
</pre>

---

## gwflow_solute

this subroutine calculates solute advection, dispersion, chemical

**Called from:** [`gwflow_lateral`](#gwflow_lateral)


Source: `gwflow_solute.f90`

<pre>
gwflow_solute
└── <a href="#gwflow_chem">gwflow_chem</a>                         this subroutine calculates chemical reactions in gwflow cells.
</pre>

---

## pl_waterup

this subroutine distributes potential plant evaporation through

**Called from:** [`pl_community`](#pl_community)


Source: `pl_waterup.f90`

<pre>
pl_waterup
└── <a href="#salt_chem_soil_single">salt_chem_soil_single</a>               this subroutine calculates salt ion concentrations based on equilibrium chemical reactions
</pre>

---

## pl_biomass_gro

Source: `pl_biomass_gro.f90`

**Called from:** [`pl_grow`](#pl_grow)


<pre>
pl_biomass_gro
├── <a href="#pl_nup">pl_nup</a>                              This subroutine calculates plant nitrogen uptake
├── <a href="#pl_pup">pl_pup</a>                              this subroutine calculates plant phosphorus uptake
├── pl_tstr                             computes temperature stress for crop growth - strstmp
├── salt_uptake                         this subroutine simulates salt ion uptake in the root zone
└── [if cs_db%num_cs > 0]  cs_uptake   constituent uptake by plants
</pre>

---

## pl_nut_demand

this subroutine predicts daily potential growth of total plant

**Called from:** [`pl_grow`](#pl_grow)


Source: `pl_nut_demand.f90`

<pre>
pl_nut_demand
├── pl_nupd                             This subroutine calculates plant nitrogen demand
└── pl_pupd                             this subroutine calculates plant phosphorus demand
</pre>

---

## pl_root_gro

calculate root depth

**Called from:** [`mgt_transplant`](#mgt_transplant), [`pl_grow`](#pl_grow), [`plant_init`](#plant_init)


Source: `pl_root_gro.f90`

<pre>
pl_root_gro
└── pl_rootfr                           This subroutine distributes dead root mass through the soil profile
</pre>

---

## sq_volq

Call subroutines to calculate the current day"s CN for the HRU and

**Called from:** [`surface`](#surface)


Source: `sq_volq.f90`

<pre>
sq_volq
├── sq_daycn                            Predicts daily runoff given daily precipitation and snow melt
└── sq_greenampt                        Predicts daily runoff given breakpoint precipitation and snow melt
</pre>

---

## swr_drains

this subroutine finds the effective lateral hydraulic conductivity

**Called from:** [`swr_percmain`](#swr_percmain)


Source: `swr_drains.f90`

<pre>
swr_drains
└── swr_depstor                         this subroutine computes maximum surface depressional storage depth based on
</pre>

---

## gwflow_chem

this subroutine calculates chemical reactions in gwflow cells.

**Called from:** [`gwflow_solute`](#gwflow_solute)


Source: `gwflow_chem.f90`

<pre>
gwflow_chem
└── gwflow_minl                         this subroutine calculates salt mineral precipitation-dissolution
</pre>

---

## salt_chem_soil_single

this subroutine calculates salt ion concentrations based on equilibrium chemical reactions

**Called from:** [`pl_waterup`](#pl_waterup)


Source: `salt_chem_soil_single.f90`

<pre>
salt_chem_soil_single
├── activity_coefficient
├── caco3
├── caso4
├── ionic_strength
├── mgco3
├── mgso4
└── nacl
</pre>

---

## pl_nup

This subroutine calculates plant nitrogen uptake

**Called from:** [`pl_biomass_gro`](#pl_biomass_gro)


Source: `pl_nup.f90`

<pre>
pl_nup
├── nuts                                this function calculates the plant stress factor caused by limited
└── pl_nfix                             this subroutine estimates nitrogen fixation by legumes
</pre>

---

## pl_pup

this subroutine calculates plant phosphorus uptake

**Called from:** [`pl_biomass_gro`](#pl_biomass_gro)


Source: `pl_pup.f90`

<pre>
pl_pup
└── nuts                                this function calculates the plant stress factor caused by limited
</pre>

---

---

## wet_all_initial

Initialises all wetland/pond objects before the simulation loop.

**Called from:** [`main`](#main)

<pre>
wet_all_initial
└── [loop iihru = 1, sp_ob%hru]  wet_initial(iihru)
</pre>

---

## mgt_biomix

Performs daily biological mixing of CENTURY carbon pools within the tillage depth.
Called only when `bsn_cc%cswat == 1` and `bmix_eff > 1e-6`.

**Called from:** [`hru_control`](#hru_control)

<pre>
mgt_biomix
└── mgt_tillfactor           compute tillage factor for biological mixing efficiency
</pre>

---

## calsoft_control

Top-level soft-calibration controller. Runs repeated simulations adjusting parameters
to minimise objective function residuals.

**Called from:** [`main`](#main)

<pre>
calsoft_control
├── <a href="#calsoft_hyd">calsoft_hyd</a>            calibrate hydrologic components (surface, lateral, perc, ET)
├── <a href="#calsoft_hyd_bfr">calsoft_hyd_bfr</a>        calibrate baseflow recession parameters
├── <a href="#caltsoft_hyd">caltsoft_hyd</a>           calibrate time-of-travel (storage-coefficient) routing
├── <a href="#calsoft_plant">calsoft_plant</a>          calibrate plant growth parameters
├── <a href="#calsoft_sed">calsoft_sed</a>            calibrate sediment parameters
└── pl_write_parms_cal     write calibrated plant parameters
</pre>

---

## calsoft_hyd

Calibrates hydrologic parameters (CN2, ESCO, etc.) by iterating the full simulation.

**Called from:** [`calsoft_control`](#calsoft_control)

<pre>
calsoft_hyd
├── re_initialize          reset all state variables to initial conditions
├── <a href="#time_control">time_control</a>           run full simulation with current parameters
└── curno                  recompute CN lookup table after CN2 adjustment
</pre>

---

## calsoft_hyd_bfr

Calibrates baseflow parameters by iterating sub-objective functions for each component.

**Called from:** [`calsoft_control`](#calsoft_control)

<pre>
calsoft_hyd_bfr
├── <a href="#calsoft_hyd_bfr_pet">calsoft_hyd_bfr_pet</a>    calibrate PET-related baseflow parameters
├── <a href="#calsoft_hyd_bfr_et">calsoft_hyd_bfr_et</a>     calibrate actual ET baseflow contribution
├── <a href="#calsoft_hyd_bfr_surq">calsoft_hyd_bfr_surq</a>   calibrate surface-runoff influence on baseflow
├── <a href="#calsoft_hyd_bfr_latq">calsoft_hyd_bfr_latq</a>   calibrate lateral-flow influence on baseflow
└── <a href="#calsoft_hyd_bfr_perc">calsoft_hyd_bfr_perc</a>   calibrate percolation influence on baseflow
</pre>

---

## calsoft_hyd_bfr_pet

Calibrates PET parameter contribution to baseflow objective.

**Called from:** [`calsoft_hyd_bfr`](#calsoft_hyd_bfr)

<pre>
calsoft_hyd_bfr_pet
├── re_initialize
└── <a href="#time_control">time_control</a>
</pre>

---

## calsoft_hyd_bfr_et

Calibrates actual ET parameter contribution to baseflow objective.

**Called from:** [`calsoft_hyd_bfr`](#calsoft_hyd_bfr)

<pre>
calsoft_hyd_bfr_et
├── re_initialize
└── <a href="#time_control">time_control</a>
</pre>

---

## calsoft_hyd_bfr_surq

Calibrates surface-runoff influence on baseflow; adjusts CN2 before each run.

**Called from:** [`calsoft_hyd_bfr`](#calsoft_hyd_bfr)

<pre>
calsoft_hyd_bfr_surq
├── curno
├── re_initialize
└── <a href="#time_control">time_control</a>
</pre>

---

## calsoft_hyd_bfr_latq

Calibrates lateral-flow influence on baseflow.

**Called from:** [`calsoft_hyd_bfr`](#calsoft_hyd_bfr)

<pre>
calsoft_hyd_bfr_latq
├── re_initialize
└── <a href="#time_control">time_control</a>
</pre>

---

## calsoft_hyd_bfr_perc

Calibrates percolation influence on baseflow.

**Called from:** [`calsoft_hyd_bfr`](#calsoft_hyd_bfr)

<pre>
calsoft_hyd_bfr_perc
├── re_initialize
└── <a href="#time_control">time_control</a>
</pre>

---

## calsoft_plant

Calibrates plant growth parameters by iterating the full simulation.

**Called from:** [`calsoft_control`](#calsoft_control)

<pre>
calsoft_plant
├── calsoft_plant_zero     zero plant calibration accumulators
├── re_initialize
└── <a href="#time_control">time_control</a>
</pre>

---

## calsoft_sed

Calibrates sediment parameters by iterating the full simulation.

**Called from:** [`calsoft_control`](#calsoft_control)

<pre>
calsoft_sed
├── re_initialize
└── <a href="#time_control">time_control</a>
</pre>

---

## caltsoft_hyd

Calibrates time-of-travel (storage-coefficient) routing parameters.

**Called from:** [`calsoft_control`](#calsoft_control)

<pre>
caltsoft_hyd
├── ascrv                  compute storage-coefficient routing curve
└── <a href="#time_control">time_control</a>
</pre>

---

## swift_output

Writes SWIFT-format output files after the simulation completes.

**Called from:** [`main`](#main)

<pre>
swift_output
├── [loop output files]  copy_file(src, "SWIFT/" // file_name)
└── hyd_convert_mass_to_conc    convert hydrograph mass fluxes to concentrations
</pre>

---

## wallo_control

Water allocation controller; processes all demand, withdrawal, transfer, and treatment
operations for one allocation object on the current day.

**Called from:** [`command`](#command), [`sd_channel_control3`](#sd_channel_control3)

<pre>
wallo_control
├── <a href="#wallo_demand">wallo_demand</a>           determine water demand for each use type
├── <a href="#wallo_withdraw">wallo_withdraw</a>         withdraw water from source (stream, reservoir, gwflow)
├── wallo_canal            route water through irrigation canal
├── wallo_transfer         transfer withdrawn water to destination object
├── <a href="#wallo_treatment">wallo_treatment</a>        apply water treatment processes
├── <a href="#wallo_use">wallo_use</a>              apply water to HRU/crop use
├── [if num_salts > 0]  salt_irrig   add salt load to irrigation water
├── [if num_cs > 0]     cs_irrig     add constituent load to irrigation water
└── [if src == reservoir]  <a href="#res_control">res_control</a>  update reservoir state after withdrawal
</pre>

---

## wallo_demand

Determines water demand for each use object by evaluating conditions and actions.

**Called from:** [`wallo_control`](#wallo_control)

<pre>
wallo_demand
├── <a href="#conditions">conditions</a>    evaluate demand trigger conditions
└── <a href="#actions">actions</a>       set demand amounts based on triggered actions
</pre>

---

## wallo_withdraw

Withdraws water from the allocated source (stream, reservoir, or groundwater).

**Called from:** [`wallo_control`](#wallo_control)

<pre>
wallo_withdraw
└── [if gwflow source]  gwflow_pump_allo   extract water from gwflow grid cell
</pre>

---

## wallo_treatment

Applies water treatment processes to withdrawn water before use.

**Called from:** [`wallo_control`](#wallo_control)

<pre>
wallo_treatment
├── hyd_convert_conc_to_mass   convert concentrations to mass in hydrograph
├── hyd_min                    apply minimum-flow constraint
└── hydcsout_conc_mass         compute constituent mass from outflow concentration
</pre>

---

## wallo_use

Applies allocated water to the HRU or crop use object.

**Called from:** [`wallo_control`](#wallo_control)

<pre>
wallo_use
├── hyd_convert_conc_to_mass   convert concentrations to mass in hydrograph
└── hydcsout_conc_mass         compute constituent mass from outflow concentration
</pre>


---

## cli_staread

Reads weather station configuration and maps each station to its gage files (WGN, precipitation,
temperature, solar radiation, humidity, wind, PET, atmospheric deposition).

**Called from:** [`proc_read`](#proc_read)

<pre>
cli_staread
└── [loop weather stations, per gage type if db_mx%*files > 0]
    └── search    look up gage name in array and return its index
</pre>

