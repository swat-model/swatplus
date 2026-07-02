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
│   ├── <a href="#cli_lapse">cli_lapse</a>
│   ├── <a href="#object_read_output">object_read_output</a>
│   ├── <a href="#om_water_init">om_water_init</a>
│   ├── <a href="#pest_cha_res_read">pest_cha_res_read</a> / <a href="#path_cha_res_read">path_cha_res_read</a>
│   ├── <a href="#salt_cha_read">salt_cha_read</a> / <a href="#cs_cha_read">cs_cha_read</a>
│   ├── <a href="#lsu_read_elements">lsu_read_elements</a>
│   ├── <a href="#proc_hru">proc_hru</a>              HRU initialisation
│   ├── <a href="#proc_cha">proc_cha</a>              channel initialisation
│   ├── <a href="#proc_aqu">proc_aqu</a>              aquifer initialisation
│   ├── <a href="#dtbl_lum_read">dtbl_lum_read</a>
│   ├── <a href="#hru_lte_read">hru_lte_read</a> / <a href="#proc_cond">proc_cond</a>
│   ├── <a href="#res_read_weir">res_read_weir</a> / <a href="#dtbl_res_read">dtbl_res_read</a> / <a href="#dtbl_scen_read">dtbl_scen_read</a> / <a href="#cal_cond_read">cal_cond_read</a>
│   ├── <a href="#manure_allocation_read">manure_allocation_read</a> / <a href="#dtbl_flocon_read">dtbl_flocon_read</a>
│   ├── <a href="#om_treat_read">om_treat_read</a> / <a href="#om_use_read">om_use_read</a> / <a href="#om_osrc_read">om_osrc_read</a>
│   ├── <a href="#water_treatment_read">water_treatment_read</a> / <a href="#water_use_read">water_use_read</a> / <a href="#water_tower_read">water_tower_read</a>
│   ├── <a href="#water_pipe_read">water_pipe_read</a> / <a href="#water_canal_read">water_canal_read</a> / <a href="#water_allocation_read">water_allocation_read</a>
│   ├── <a href="#hru_dtbl_actions_init">hru_dtbl_actions_init</a>
│   ├── <a href="#proc_res">proc_res</a> / <a href="#wet_read_hyd">wet_read_hyd</a> / <a href="#wet_read">wet_read</a> / <a href="#wet_read_salt_cs">wet_read_salt_cs</a>
│   ├── [if db_mx%wet_dat > 0]  <a href="#wet_all_initial">wet_all_initial</a>
│   ├── <a href="#wet_fp_init">wet_fp_init</a>
│   ├── [loop ihru = 1, sp_ob%hru]  <a href="#soil_nutcarb_init">soil_nutcarb_init</a>
│   ├── <a href="#proc_cal">proc_cal</a>              calibration setup
│   ├── <a href="#proc_open">proc_open</a>             open output files, write headers
│   ├── <a href="#unit_hyd_ru_hru">unit_hyd_ru_hru</a> / <a href="#dr_ru">dr_ru</a>
│   └── <a href="#hyd_connect_out">hyd_connect_out</a>
│
└── SIMULATION ─────────────────────────────────────────────────────────
    ├── [if time%step &lt; 0]  <a href="#command">command</a>         export-coefficient (average annual) mode
    └── [else]              <a href="#time_control">time_control</a>    standard year/day simulation loop
          [after loop] [if cal_soft == "y"]  <a href="#calsoft_control">calsoft_control</a>
                       [if cal_hard == "y"]  <a href="#cal_parmchg_read">cal_parmchg_read</a> → <a href="#calhard_control">calhard_control</a>
                       [if bsn_cc%swift_out == 1]  <a href="#swift_output">swift_output</a>
</pre>

---

## proc_bsn


**Called from:** [`main`](#main)

<pre>
proc_bsn
├── <a href="#readcio_read">readcio_read</a>             read file.cio
├── <a href="#basin_read_cc">basin_read_cc</a>            read basins.bsn  (sets bsn_cc%cswat, %crk, %lapse, etc.)
├── <a href="#basin_read_objs">basin_read_objs</a>
├── <a href="#time_read">time_read</a>
├── <a href="#basin_read_prm">basin_read_prm</a>
├── <a href="#basin_prm_default">basin_prm_default</a>
├── <a href="#basin_print_codes_read">basin_print_codes_read</a>   read print.prt  → sets all output flags
├── <a href="#co2_read">co2_read</a>
├── <a href="#carbon_coef_read">carbon_coef_read</a>         read carbon_coef.cbn if present (overrides CENTURY rates)
└── <a href="#open_output_file">open_output_file</a>         open files.out, diagnostics.out, area_calc.out
</pre>

---

## proc_db


**Called from:** [`main`](#main)

<pre>
proc_db
├── <a href="#plant_parm_read">plant_parm_read</a>          plants.plt
├── <a href="#plantparm_init">plantparm_init</a>
├── <a href="#plant_transplant_read">plant_transplant_read</a>
├── <a href="#till_parm_read">till_parm_read</a>           tillage.til  (till_eff used in <a href="#cbn_zhang2">cbn_zhang2</a>)
├── <a href="#pest_parm_read">pest_parm_read</a>
├── <a href="#fert_parm_read">fert_parm_read</a>
├── <a href="#manure_orgmin_read">manure_orgmin_read</a> / <a href="#manure_db_read">manure_db_read</a>
├── <a href="#urban_parm_read">urban_parm_read</a>
├── <a href="#path_parm_read">path_parm_read</a> / <a href="#septic_parm_read">septic_parm_read</a>
├── <a href="#mgt_read_irrops">mgt_read_irrops</a> / <a href="#mgt_read_chemapp">mgt_read_chemapp</a> / <a href="#mgt_read_harvops">mgt_read_harvops</a>
├── <a href="#mgt_read_grazeops">mgt_read_grazeops</a> / <a href="#mgt_read_sweepops">mgt_read_sweepops</a> / <a href="#mgt_read_fireops">mgt_read_fireops</a>
├── <a href="#mgt_read_mgtops">mgt_read_mgtops</a> / <a href="#mgt_read_puddle">mgt_read_puddle</a>
├── <a href="#sdr_read">sdr_read</a> / <a href="#sep_read">sep_read</a>
├── <a href="#scen_read_grwway">scen_read_grwway</a> / <a href="#scen_read_filtstrip">scen_read_filtstrip</a> / <a href="#scen_read_bmpuser">scen_read_bmpuser</a> / <a href="#sat_buff_read">sat_buff_read</a>
├── <a href="#readpcom">readpcom</a>
├── <a href="#cntbl_read">cntbl_read</a> / <a href="#cons_prac_read">cons_prac_read</a> / <a href="#overland_n_read">overland_n_read</a>
└── <a href="#landuse_read">landuse_read</a>
</pre>

---

## proc_read


**Called from:** [`main`](#main)

<pre>
proc_read
├── <a href="#ch_read_temp">ch_read_temp</a> / <a href="#cli_read_atmodep">cli_read_atmodep</a> / <a href="#cli_staread">cli_staread</a>
├── <a href="#constit_db_read">constit_db_read</a> / <a href="#pest_metabolite_read">pest_metabolite_read</a>
├── <a href="#soil_plant_init">soil_plant_init</a> / <a href="#solt_db_read">solt_db_read</a>
├── <a href="#pest_hru_aqu_read">pest_hru_aqu_read</a> / <a href="#path_hru_aqu_read">path_hru_aqu_read</a> / <a href="#hmet_hru_aqu_read">hmet_hru_aqu_read</a>
├── <a href="#salt_hru_read">salt_hru_read</a> / <a href="#salt_aqu_read">salt_aqu_read</a> / <a href="#salt_irr_read">salt_irr_read</a> / <a href="#salt_plant_read">salt_plant_read</a>
├── <a href="#cli_read_atmodep_salt">cli_read_atmodep_salt</a> / <a href="#salt_roadsalt_read">salt_roadsalt_read</a> / <a href="#salt_uptake_read">salt_uptake_read</a>
├── <a href="#salt_urban_read">salt_urban_read</a> / <a href="#salt_fert_read">salt_fert_read</a>
├── <a href="#cs_hru_read">cs_hru_read</a> / <a href="#cs_aqu_read">cs_aqu_read</a> / <a href="#cli_read_atmodep_cs">cli_read_atmodep_cs</a> / <a href="#cs_irr_read">cs_irr_read</a>
├── <a href="#cs_plant_read">cs_plant_read</a> / <a href="#cs_uptake_read">cs_uptake_read</a> / <a href="#cs_reactions_read">cs_reactions_read</a> / <a href="#cs_urban_read">cs_urban_read</a> / <a href="#cs_fert_read">cs_fert_read</a>
├── <a href="#topo_read">topo_read</a> / <a href="#field_read">field_read</a> / <a href="#hydrol_read">hydrol_read</a> / <a href="#shade_factor_read">shade_factor_read</a>
├── <a href="#snowdb_read">snowdb_read</a>
├── <a href="#soil_db_read">soil_db_read</a>             soils.sol — organic C% used to initialise carbon pools
└── <a href="#soil_lte_db_read">soil_lte_db_read</a>
</pre>

---

## proc_hru


**Called from:** [`main`](#main)

<pre>
proc_hru
├── <a href="#hru_allo">hru_allo</a> / <a href="#hru_read">hru_read</a> / <a href="#hrudb_init">hrudb_init</a>
├── <a href="#hru_lum_init_all">hru_lum_init_all</a> / <a href="#topohyd_init">topohyd_init</a> / <a href="#hru_output_allo">hru_output_allo</a>
├── <a href="#carbon_read">carbon_read</a>              read initial carbon state (organic C%, litter)
├── [loop HRUs]  <a href="#structure_set_parms">structure_set_parms</a>("septic")
├── <a href="#soils_init">soils_init</a>               compute FC, WP, BD for each layer
├── <a href="#structure_init">structure_init</a> / <a href="#plant_all_init">plant_all_init</a> / <a href="#cn2_init_all">cn2_init_all</a> / <a href="#hydro_init">hydro_init</a>
├── <a href="#pesticide_init">pesticide_init</a> / <a href="#pathogen_init">pathogen_init</a> / <a href="#salt_hru_init">salt_hru_init</a> / <a href="#cs_hru_init">cs_hru_init</a>
├── <a href="#rte_read_nut">rte_read_nut</a>
└── <a href="#open_output_file">open_output_file</a>         open erosion.out, checker.out
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
├── <a href="#ch_read_init">ch_read_init</a> / <a href="#ch_read_init_cs">ch_read_init_cs</a>
├── <a href="#sd_hydsed_read">sd_hydsed_read</a> / <a href="#ch_read_hyd">ch_read_hyd</a> / <a href="#ch_read_sed">ch_read_sed</a> / <a href="#ch_read_nut">ch_read_nut</a>
├── <a href="#ch_read">ch_read</a> / <a href="#sd_channel_read">sd_channel_read</a> / <a href="#sd_hydsed_init">sd_hydsed_init</a>
├── <a href="#aqu2d_init">aqu2d_init</a>
├── [loop channels]  <a href="#ch_ttcoef">ch_ttcoef</a> / <a href="#ch_initial">ch_initial</a>
└── <a href="#overbank_read">overbank_read</a> / <a href="#sd_channel_surf_link">sd_channel_surf_link</a> / <a href="#time_conc_init">time_conc_init</a>
</pre>

---

## proc_aqu


**Called from:** [`main`](#main)

<pre>
proc_aqu
├── <a href="#aqu_read">aqu_read</a> / <a href="#aqu_initial">aqu_initial</a>
└── <a href="#aqu_read_init">aqu_read_init</a> / <a href="#aqu_read_init_cs">aqu_read_init_cs</a>
</pre>

---

## proc_cal


**Called from:** [`main`](#main)

<pre>
proc_cal
├── <a href="#cal_parm_read">cal_parm_read</a> / <a href="#cal_parmchg_read">cal_parmchg_read</a>
├── <a href="#pl_read_regions_cal">pl_read_regions_cal</a> / <a href="#pl_read_parms_cal">pl_read_parms_cal</a> / <a href="#cal_conditions">cal_conditions</a>
├── <a href="#calsoft_read_codes">calsoft_read_codes</a> / <a href="#lcu_read_softcal">lcu_read_softcal</a> / <a href="#ls_read_lsparms_cal">ls_read_lsparms_cal</a>
├── <a href="#aqu_read_elements">aqu_read_elements</a> / <a href="#ch_read_elements">ch_read_elements</a> / <a href="#res_read_elements">res_read_elements</a> / <a href="#rec_read_elements">rec_read_elements</a>
├── <a href="#ch_read_orders_cal">ch_read_orders_cal</a> / <a href="#ch_read_parms_cal">ch_read_parms_cal</a>
└── <a href="#cal_allo_init">cal_allo_init</a>
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
│   │   ├── <a href="#basin_sw_init">basin_sw_init</a>
│   │   └── <a href="#aqu_pest_output_init">aqu_pest_output_init</a>
│   │
│   ├── [day loop: julian_day = day_start to day_end_yr]
│   │   ├── <a href="#xmon">xmon</a>                          day-of-year → month, day-of-month
│   │   ├── <a href="#sim_initday">sim_initday</a>                   zero daily HRU accumulators
│   │   ├── <a href="#climate_control">climate_control</a>               read/generate weather
│   │   ├── <a href="#cli_atmodep_time_control">cli_atmodep_time_control</a>      set atmospheric deposition array counter
│   │   ├── [loop db_mx%cond_up]  <a href="#conditions">conditions</a> / <a href="#actions">actions</a>   conditional land-use reset
│   │   ├── [if db_mx%mallo_db > 0]  <a href="#mallo_control">mallo_control</a>        manure allocation
│   │   └── <a href="#command">command</a>                       daily object routing loop
│   │
│   └── [end-of-year]
│       ├── <a href="#calsoft_sum_output">calsoft_sum_output</a>
│       ├── [crop yield accounting — basin and regional totals]
│       ├── [loop sp_ob%hru]
│       │   ├── [if bsn_cc%cswat == 0 and biomix > 1e-6]
│       │   │     <a href="#mgt_newtillmix_cswat0">mgt_newtillmix_cswat0</a> (j, biomix, 0)   annual biological mixing
│       │   │     (cswat==1 biological mixing occurs daily in <a href="#hru_control">hru_control</a> via <a href="#mgt_biomix">mgt_biomix</a>)
│       │   ├── [update perennial plant maturity ages]
│       │   └── [reset phubase, yr_skip; advance schedule past "skip" ops]
│       └── [reset yearly HRU/channel/LTE output accumulators]
│
├── <a href="#calsoft_ave_output">calsoft_ave_output</a>
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
│   │   (<a href="#surface">surface</a>, lateral, tile, aquifer components routed separately for HRUs)
│   │
│   ├── select case (ob(icmd)%typ)
│   │   │
│   │   ├── case "hru"
│   │   │   ├── <a href="#hru_control">hru_control</a>               land-phase daily simulation
│   │   │   └── [if ob%rcv_tot > 0]  <a href="#hyddep_output">hyddep_output</a>
│   │   │
│   │   ├── case "hru_lte"
│   │   │   └── <a href="#hru_lte_control">hru_lte_control</a>
│   │   │
│   │   ├── case "ru"
│   │   │   ├── <a href="#ru_control">ru_control</a>
│   │   │   └── [if ob%rcv_tot > 0]  <a href="#hyddep_output">hyddep_output</a>
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
│   │   │   │   │     [if flow &lt; 0]  <a href="#recall_nut">recall_nut</a>   (nutrient diversion)
│   │   │   │   ├── "mo"   — load monthly hydrograph
│   │   │   │   └── "yr"   — load yearly hydrograph
│   │   │   ├── [if cs_db%num_salts > 0]  <a href="#recall_salt">recall_salt</a>
│   │   │   └── [if cs_db%num_cs > 0]    <a href="#recall_cs">recall_cs</a>
│   │   │
│   │   ├── case "dr"
│   │   │   └── [if cs_db%num_tot > 0]  <a href="#constit_hyd_mult">constit_hyd_mult</a>   (apply delivery ratio to constituents)
│   │   │
│   │   ├── case "outlet"
│   │   │   └── [pass-through: hd(1) = hin]
│   │   │
│   │   └── case "chandeg"   ── SWAT-Deg channel ─────────────────────────────
│   │       ├── [if chl > 1e-3]  <a href="#sd_channel_control3">sd_channel_control3</a>
│   │       └── [else: artificial channel (length≈0) — pass-through, zero morphology outputs]
│   │
│   ├── [post-object]  <a href="#wallo_control">wallo_control</a> / [if pco%fdcout == "y"] <a href="#flow_dur_curve">flow_dur_curve</a>
│   └── [if ob%src_tot > 0]  <a href="#hydout_output">hydout_output</a>        write object-to-object hydrograph output
│
└── OUTPUT SECTION  (daily, after all objects processed)
    │
    ├── <a href="#obj_output">obj_output</a>
    │
    ├── [loop iwro]  <a href="#wallo_allo_output">wallo_allo_output</a> / <a href="#wallo_trn_output">wallo_trn_output</a> / <a href="#wallo_treat_output">wallo_treat_output</a> / <a href="#wallo_use_output">wallo_use_output</a>
    ├── [loop mallo] <a href="#manure_source_output">manure_source_output</a> / <a href="#manure_demand_output">manure_demand_output</a>
    ├── [loop hru_lte]  <a href="#hru_lte_output">hru_lte_output</a>
    │
    ├── [loop ihru = 1, sp_ob%hru]
    │   ├── <a href="#hru_output">hru_output</a>
    │   ├── <a href="#hru_carbon_output">hru_carbon_output</a>
    │   ├── [if surf_stor > 0]
    │   │   ├── <a href="#wetland_output">wetland_output</a>
    │   │   ├── [if num_salts > 0]  <a href="#wet_salt_output">wet_salt_output</a>
    │   │   └── [if num_cs > 0]    <a href="#wet_cs_output">wet_cs_output</a>
    │   ├── [if num_tot > 0]    <a href="#hru_pesticide_output">hru_pesticide_output</a> / <a href="#hru_pathogen_output">hru_pathogen_output</a>
    │   ├── [if num_salts > 0]  <a href="#hru_salt_output">hru_salt_output</a>
    │   ├── [if num_cs > 0]     <a href="#hru_cs_output">hru_cs_output</a>
    │   ├── [if pco%cb_hru%d/m/y/l set]       <a href="#soil_nutcarb_write">soil_nutcarb_write</a>    → hru_cb.csv
    │   └── [if bsn_cc%cswat == 1]
    │         [if pco%cb_vars_hru%d/m/y/l set] <a href="#soil_carbvar_write">soil_carbvar_write</a>   → hru_cb_vars.csv
    │
    ├── [loop iaq]    <a href="#aquifer_output">aquifer_output</a> / <a href="#aqu_salt_output">aqu_salt_output</a> / <a href="#aqu_cs_output">aqu_cs_output</a> / <a href="#aqu_pesticide_output">aqu_pesticide_output</a>
    ├── [loop chan]   <a href="#channel_output">channel_output</a>
    ├── [loop chandeg]  <a href="#sd_chanmorph_output">sd_chanmorph_output</a> / <a href="#sd_chanbud_output">sd_chanbud_output</a> / <a href="#sd_channel_output">sd_channel_output</a>
    │                   <a href="#cha_pesticide_output">cha_pesticide_output</a> / <a href="#ch_salt_output">ch_salt_output</a> / <a href="#ch_cs_output">ch_cs_output</a>
    │                   [if num_cs > 0]  <a href="#cs_str_output">cs_str_output</a>
    ├── [loop res]    <a href="#reservoir_output">reservoir_output</a> / <a href="#res_pesticide_output">res_pesticide_output</a> / <a href="#res_salt_output">res_salt_output</a> / <a href="#res_cs_output">res_cs_output</a>
    ├── [loop ru]     <a href="#ru_output">ru_output</a> / <a href="#ru_salt_output">ru_salt_output</a> / <a href="#ru_cs_output">ru_cs_output</a>
    ├── [loop recall] <a href="#recall_output">recall_output</a>
    ├── <a href="#hydin_output">hydin_output</a>
    ├── <a href="#basin_output">basin_output</a> / <a href="#basin_aquifer_output">basin_aquifer_output</a> / <a href="#basin_reservoir_output">basin_reservoir_output</a>
    ├── <a href="#basin_channel_output">basin_channel_output</a> / <a href="#basin_chanmorph_output">basin_chanmorph_output</a> / <a href="#basin_chanbud_output">basin_chanbud_output</a>
    ├── <a href="#basin_sdchannel_output">basin_sdchannel_output</a> / <a href="#basin_recall_output">basin_recall_output</a>
    ├── <a href="#basin_ch_pest_output">basin_ch_pest_output</a> / <a href="#basin_res_pest_output">basin_res_pest_output</a> / <a href="#basin_ls_pest_output">basin_ls_pest_output</a> / <a href="#basin_aqu_pest_output">basin_aqu_pest_output</a>
    ├── <a href="#lsu_output">lsu_output</a>
    ├── [if num_salts > 0]  <a href="#salt_balance">salt_balance</a>
    └── [if num_cs > 0]     <a href="#cs_balance">cs_balance</a>
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
├── <a href="#varinit">varinit</a>                   zero daily output variables
│
├── [auto-operations from decision table]
│   └── [loop sched%num_autos]
│         <a href="#conditions">conditions</a> / <a href="#actions">actions</a>
│         [if future fertilizer scheduled today]  <a href="#pl_fert">pl_fert</a>
│
├── <a href="#albedo">albedo</a>                    compute daily <a href="#albedo">albedo</a>
│
├── [salt equilibrium chemistry]
│   └── [if cs_db%num_salts > 0]  <a href="#salt_chem_hru">salt_chem_hru</a>
│
├── [constituent reactions and sorption]
│   └── [if cs_db%num_cs > 0]
│         ├── <a href="#cs_rctn_hru">cs_rctn_hru</a>
│         └── <a href="#cs_sorb_hru">cs_sorb_hru</a>
│
├── <a href="#stmp_solt">stmp_solt</a>                 soil temperature profile
├── <a href="#sq_canopyint">sq_canopyint</a>              canopy interception
├── <a href="#sq_snom">sq_snom</a>                   snow accumulation and melt
│
├── [surface and subsurface runon routing]
│   ├── [if hin_sur%flo > 1e-6 and ires == 0]  <a href="#rls_routesurf">rls_routesurf</a>
│   ├── [if hin_sur%flo > 1e-6 and ires > 0]   add to wetland (ht1)
│   ├── [if hin_lat%flo > 0]                    <a href="#rls_routesoil">rls_routesoil</a>
│   ├── [if this HRU is a saturated-buffer receiver]  <a href="#rls_routetile">rls_routetile</a>
│   └── [if hin_aqu%flo > 0]                    <a href="#rls_routeaqu">rls_routeaqu</a>
│
├── [shrink-swell cracking]
│   └── [if bsn_cc%crk == 1]  <a href="#sq_crackvol">sq_crackvol</a>
│
├── <a href="#et_pot">et_pot</a>                    potential evapotranspiration
├── <a href="#et_act">et_act</a>                    actual evapotranspiration
│
├── [scheduled management operations]
│   └── [if yr_skip(j) == 0]  <a href="#mgt_operatn">mgt_operatn</a>
│
├── [surface runoff generation]
│   ├── [if ires == 0]   <a href="#surface">surface</a>
│   │     ├── <a href="#sq_dailycn">sq_dailycn</a>       SCS curve number update
│   │     └── [if CN method == Green-Ampt]  <a href="#sq_greenampt">sq_greenampt</a>
│   └── [if ires > 0]   (wetland present — surfq=0, sedyld=0; no <a href="#surface">surface</a> calculation)
│
├── [paddy continuous irrigation]
│   └── [if hru%paddy_irr > 0 and wet_ob%depth &lt; irr_hmin]  <a href="#wet_irrp">wet_irrp</a>
│
├── [wetland/paddy processes]
│   ├── [if ires > 0]  <a href="#wetland_control">wetland_control</a>
│   └── [if ires == 0]  pass saturation excess to qday directly
│
├── <a href="#swr_percmain">swr_percmain</a>              percolation through soil profile
│   ├── <a href="#swr_percmicro">swr_percmicro</a>         micropore (matrix) flow
│   └── <a href="#swr_percmacro">swr_percmacro</a>         macropore (bypass) flow
│
├── [grazing]
│   └── [if igrz(j) == 1]  <a href="#pl_graze">pl_graze</a>
│         igrz set to 1 by <a href="#mgt_sched">mgt_sched</a> case "graz"; counts down grz_days then resets
│
├── ─── DECOMPOSITION AND CARBON CYCLING ───────────────────────────────────
│   │
│   ├── [if bsn_cc%cswat == 0]   ── static model ───────────────────────────
│   │   ├── <a href="#rsd_decomp">rsd_decomp</a>            <a href="#surface">surface</a> residue decomposition
│   │   └── <a href="#nut_nminrl">nut_nminrl</a>            N and P mineralisation from soil residue
│   │
│   └── [if bsn_cc%cswat == 1]   ── CENTURY 5-pool model ───────────────────
│       ├── [if bmix_eff > 1e-6]  <a href="#mgt_biomix">mgt_biomix</a> (ihru, bmix_eff)   biological mixing
│       ├── <a href="#cbn_surfrsd_decomp">cbn_surfrsd_decomp</a>    <a href="#surface">surface</a> residue → meta/str litter in layer 1
│       ├── <a href="#cbn_rsd_transfer">cbn_rsd_transfer</a>      root death + incorporated residue → soil pools
│       └── <a href="#cbn_zhang2">cbn_zhang2</a>            CENTURY 5-pool C/N transformations (layer loop)
│
├── ─── (both cswat paths continue here) ──────────────────────────────────
│
├── <a href="#nut_nitvol">nut_nitvol</a>                N volatilization
│
├── [phosphorus mineralisation]
│   ├── [if bsn_cc%sol_P_model == 1]  <a href="#nut_pminrl2">nut_pminrl2</a>
│   └── [else]                        <a href="#nut_pminrl">nut_pminrl</a>
│
├── [septic biozone]
│   └── [if sep%opt != 0 and yrc >= sep%yr and soil_temp > 0]  <a href="#sep_biozone">sep_biozone</a>
│
├── <a href="#pl_community">pl_community</a>              plant community phenology decisions
├── <a href="#pl_grow">pl_grow</a>                   daily plant growth (LAI, biomass, root, N/P uptake)
│
├── [pesticide processes]
│   ├── [if w%precip >= 2.54]  <a href="#pest_washp">pest_washp</a>      pesticide washoff by rain
│   ├── <a href="#pest_pl_up">pest_pl_up</a>                             pesticide plant uptake
│   ├── <a href="#pest_decay">pest_decay</a>                             pesticide degradation
│   ├── <a href="#pest_lch">pest_lch</a>                               pesticide leaching
│   └── <a href="#pest_soil_tot">pest_soil_tot</a>                          sum pesticide in soil
│
├── [nutrient/pollutant transport with surface runoff]
│   └── [if surfq > 0 and peak rate > 0 and precip_eff > 0]
│         ├── <a href="#pest_enrsb">pest_enrsb</a>                       enrichment ratio
│         ├── [if sedyld > 0]  <a href="#pest_pesty">pest_pesty</a>      pesticide with sediment
│         ├── [if bsn_cc%cswat == 0]  <a href="#nut_orgn">nut_orgn</a>      organic N in runoff (static)
│         ├── [if bsn_cc%cswat == 1]  <a href="#nut_orgnc2">nut_orgnc2</a>    organic N in runoff (CENTURY)
│         └── <a href="#nut_psed">nut_psed</a>                         P with sediment
│
├── <a href="#nut_nrain">nut_nrain</a>                 NO3 in rainfall added to soil
├── <a href="#nut_nlch">nut_nlch</a>                  nitrate leaching
├── <a href="#nut_solp">nut_solp</a>                  soluble P movement
│
├── [salt processes]
│   └── [if cs_db%num_salts > 0]
│         ├── [if salt_atmo == "y"]  <a href="#salt_rain">salt_rain</a>       atmospheric deposition
│         ├── <a href="#salt_roadsalt">salt_roadsalt</a>                          road salt application
│         └── <a href="#salt_lch">salt_lch</a>                               salt leaching
│
├── [constituent (cs) processes]
│   └── [if cs_db%num_cs > 0]
│         ├── [if cs_atmo == "y"]  <a href="#cs_rain">cs_rain</a>
│         └── <a href="#cs_lch">cs_lch</a>
│
├── [pathogen processes]
│   └── [if cs_db%num_paths > 0]
│         ├── <a href="#path_ls_swrouting">path_ls_swrouting</a>
│         ├── <a href="#path_ls_runoff">path_ls_runoff</a>
│         └── <a href="#path_ls_process">path_ls_process</a>
│
├── [urban area loadings]
│   └── [if hru%luse%urb_lu > 0]
│         ├── [if time%step == 1]  <a href="#hru_urban">hru_urban</a>    (daily simulation)
│         └── [if time%step > 1]  <a href="#hru_urbanhr">hru_urbanhr</a>  (subdaily simulation)
│
├── <a href="#swr_latsed">swr_latsed</a>                sediment in lateral flow
├── <a href="#stor_surfstor">stor_surfstor</a>             lag <a href="#surface">surface</a> runoff and pollutants
├── <a href="#swr_substor">swr_substor</a>               lag subsurface flow and nitrate
│
├── [edge-of-field filter strip]
│   └── [if lumv%vfsi > 0]
│         ├── <a href="#smp_filter">smp_filter</a>
│         └── [if filterw(j) > 0]  <a href="#smp_buffer">smp_buffer</a>
│
├── [in-field grass waterway]
│   └── [if lumv%grwat_i == 1]  <a href="#smp_grass_wway">smp_grass_wway</a>
│
├── [fixed BMP efficiency]
│   └── [if lumv%bmp_flag == 1]  <a href="#smp_bmpfixed">smp_bmpfixed</a>
│
├── [saturated buffer — source HRU side]
│   └── [if sb%hru_src == j]  <a href="#conditions">conditions</a> / <a href="#actions">actions</a>   (tile diversion decision table)
│
├── <a href="#sq_surfst">sq_surfst</a>                 store/release <a href="#surface">surface</a> runoff
├── <a href="#swr_subwq">swr_subwq</a>                 chl-a, CBOD, dissolved O₂
├── [if sed/nut concentrations > 0]  <a href="#hru_urb_bmp">hru_urb_bmp</a>
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
    │         ├── <a href="#mgt_plantop">mgt_plantop</a>              initialise plant state variables
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
    │           set mseas="y"  (begin monsoon season; growth triggered by P/PET in <a href="#hru_control">hru_control</a>)
    │
    ├── case "harv" ── harvest only ──────────────────────────────────────────
    │   └── [loop ipl = 1, pcom%npl]
    │         [if gro == "y" and crop matches and biomass > bm_min]
    │         │   select case (harvop_db(iharvop)%typ)
    │         │   ├── "biomass"  → <a href="#mgt_harvbiomass">mgt_harvbiomass</a>(j, ipl, iharvop)
    │         │   ├── "grain"    → <a href="#mgt_harvgrain">mgt_harvgrain</a>  (j, ipl, iharvop)
    │         │   ├── "residue"  → <a href="#mgt_harvresidue">mgt_harvresidue</a>(j, harveff, iharvop)
    │         │   ├── "tree"     → <a href="#mgt_harvbiomass">mgt_harvbiomass</a>(j, ipl, iharvop)
    │         │   ├── "tuber"    → <a href="#mgt_harvtuber">mgt_harvtuber</a>  (j, ipl, iharvop)
    │         │   ├── "peanuts"  → <a href="#mgt_harvtuber">mgt_harvtuber</a>  (j, ipl, iharvop)
    │         │   ├── "stripper" → <a href="#mgt_harvbiomass">mgt_harvbiomass</a>(j, ipl, iharvop)
    │         │   └── "picker"   → <a href="#mgt_harvgrain">mgt_harvgrain</a>  (j, ipl, iharvop)
    │         [if crop NOT growing]
    │             select case (harvop_db%typ)
    │             └── "residue"  → <a href="#mgt_harvresidue">mgt_harvresidue</a>(j, harveff, iharvop)
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
    │         │   ├── "biomass"  → <a href="#mgt_harvbiomass">mgt_harvbiomass</a>(j, ipl, iharvop)
    │         │   ├── "grain"    → <a href="#mgt_harvgrain">mgt_harvgrain</a>  (j, ipl, iharvop)
    │         │   ├── "residue"  → <a href="#mgt_harvresidue">mgt_harvresidue</a>(j, harveff, iharvop)
    │         │   ├── "tuber"    → <a href="#mgt_harvtuber">mgt_harvtuber</a>  (j, ipl, iharvop)
    │         │   ├── "peanuts"  → <a href="#mgt_harvtuber">mgt_harvtuber</a>  (j, ipl, iharvop)
    │         │   ├── "stripper" → <a href="#mgt_harvgrain">mgt_harvgrain</a>  (j, ipl, iharvop)
    │         │   └── "picker"   → <a href="#mgt_harvgrain">mgt_harvgrain</a>  (j, ipl, iharvop)
    │         └── <a href="#mgt_killop">mgt_killop</a>(j, ipl)   always called after harvest portion
    │         [if NOT growing]
    │             select case (harvop_db%typ)
    │             └── "residue"  → <a href="#mgt_harvresidue">mgt_harvresidue</a>(j, harveff, iharvop)
    │
    ├── case "till" ── tillage ────────────────────────────────────────────────
    │   ├── [if bsn_cc%cswat == 1]  <a href="#mgt_newtillmix_cswat1">mgt_newtillmix_cswat1</a>(j, 0., idtill)
    │   │     mixes CENTURY pool masses between layers; sets tillage_switch=1,
    │   │     tillage_days=0 → till_eff=1.6 in <a href="#cbn_zhang2">cbn_zhang2</a> for till_eff_days
    │   └── [if bsn_cc%cswat == 0]  <a href="#mgt_newtillmix_cswat0">mgt_newtillmix_cswat0</a>(j, 0., idtill)
    │         mixes residue and humus variables between layers
    │
    ├── case "irrm" ── date-scheduled irrigation ─────────────────────────────
    │   └── [no subroutine called]
    │         sets irrig(j)%applied and %runoff from irrop_db
    │         water enters soil profile via <a href="#swr_percmain">swr_percmain</a> in <a href="#hru_control">hru_control</a>
    │
    ├── case "fert" ── fertilizer application ────────────────────────────────
    │   ├── [if wet(j)%flo > 0]  standing water present (paddy/wetland)
    │   │   ├── <a href="#pl_fert_wet">pl_fert_wet</a>(ifrt, frt_kg)         add nutrients to ponded water
    │   │   ├── <a href="#salt_fert_wet">salt_fert_wet</a>(j, ifrt, frt_kg)
    │   │   └── <a href="#cs_fert_wet">cs_fert_wet</a>(j, ifrt, frt_kg)
    │   └── [else]  dry soil
    │       └── <a href="#pl_fert">pl_fert</a>(ifrt, frt_kg, ifertop)    add nutrients to soil profile
    │
    ├── case "manu" ── manure application ────────────────────────────────────
    │   ├── <a href="#pl_manure">pl_manure</a>(ifrt, frt_kg, ifertop)      add organic/mineral N and P
    │   ├── <a href="#salt_fert">salt_fert</a>(j, ifrt, frt_kg, ifertop)
    │   └── <a href="#cs_fert">cs_fert</a>(j, ifrt, frt_kg, ifertop)
    │
    ├── case "pest" ── pesticide application ─────────────────────────────────
    │   └── <a href="#pest_apply">pest_apply</a>(j, ipest, pest_kg, ipestop)
    │
    ├── case "graz" ── grazing initiation ────────────────────────────────────
    │   └── [no subroutine called]
    │         sets igrz(j)=1, grz_days(j)=Int(mgt%op3)
    │         <a href="#pl_graze">pl_graze</a> is called per-day in <a href="#hru_control">hru_control</a> while igrz(j)==1
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
    │         used in <a href="#hru_urban">hru_urban</a> / <a href="#hru_urbanhr">hru_urbanhr</a> on the same day
    │
    ├── case "dwm" ── drainage water management ──────────────────────────────
    │   └── [no subroutine called]
    │         sets hru%lumv%sdr_dep and %ldrain
    │         ldrain controls which soil layer is the tile drain target
    │         used in <a href="#swr_percmain">swr_percmain</a>
    │
    ├── case "weir" ── weir height adjustment ────────────────────────────────
    │   └── [no subroutine called]
    │         sets wet_ob(j)%weir_hgt, %pvol, %iweir
    │         new weir height takes effect in <a href="#wetland_control">wetland_control</a> next call
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
    │         <a href="#wet_irrp">wet_irrp</a> called daily in <a href="#hru_control">hru_control</a> when depth &lt; irr_hmin
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
    │         ├── [if wet_ob(j)%depth > 0.001]   <a href="#mgt_newtillmix_wet">mgt_newtillmix_wet</a>(j, idtill)
    │         ├── [else if bsn_cc%cswat == 1]    <a href="#mgt_newtillmix_cswat1">mgt_newtillmix_cswat1</a>(j, 0., idtill)
    │         └── [else if bsn_cc%cswat == 0]    <a href="#mgt_newtillmix_cswat0">mgt_newtillmix_cswat0</a>(j, 0., idtill)
    │
    └── case "skip" ── skip a simulation year ────────────────────────────────
          └── [no subroutine called]
                sets yr_skip(j) = 1
                <a href="#mgt_operatn">mgt_operatn</a> exits immediately on yr_skip
                <a href="#hru_control">hru_control</a> skips <a href="#mgt_operatn">mgt_operatn</a> for the rest of this calendar year
                yr_skip reset to 0 at year end in <a href="#time_control">time_control</a>
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
    │   │     = 1.6 for till_eff_days days after <a href="#mgt_newtillmix_cswat1">mgt_newtillmix_cswat1</a>
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
    │   └── hp (passive humus, k > 1 only — zero in <a href="#surface">surface</a> layer)
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
    ├── <a href="#nut_np_flow">nut_np_flow</a>              N flux accounting for each pool-to-pool transfer
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
├── <a href="#cli_petmeas">cli_petmeas</a>          read PET measurement files
├── <a href="#cli_pmeas">cli_pmeas</a>            read precipitation measurement files
├── <a href="#cli_tmeas">cli_tmeas</a>            read temperature measurement files
├── <a href="#cli_smeas">cli_smeas</a>            read solar radiation measurement files
├── <a href="#cli_hmeas">cli_hmeas</a>            read relative humidity measurement files
├── <a href="#cli_wmeas">cli_wmeas</a>            read wind speed measurement files
└── <a href="#cli_wgnread">cli_wgnread</a>          read weather generator parameter files
</pre>

---

## hyd_connect

Reads routing network configuration files and builds the object connectivity graph used by `command`.

**Called from:** [`main`](#main)


<pre>
hyd_connect
│
├── [if sp_ob%hru > 0]      <a href="#hyd_read_connect">hyd_read_connect</a>(hru.con)
├── [if sp_ob%hru_lte > 0]  <a href="#hyd_read_connect">hyd_read_connect</a>(hru_lte.con)
├── [if sp_ob%ru > 0]
│   ├── <a href="#hyd_read_connect">hyd_read_connect</a>(rout_unit.con)
│   ├── <a href="#ru_read_elements">ru_read</a>              read routing unit definitions
│   └── <a href="#ru_read_elements">ru_read_elements</a>     map HRUs/elements to routing units
├── [if sp_ob%aqu > 0]
│   ├── <a href="#hyd_read_connect">hyd_read_connect</a>(aquifer.con)
│   └── <a href="#aqu2d_read">aqu2d_read</a>           read 2-D aquifer connections
├── [if sp_ob%chan > 0]
│   ├── <a href="#hyd_read_connect">hyd_read_connect</a>(channel.con)
│   └── <a href="#overbank_read">overbank_read</a>        read overbank floodplain linkages
├── [if sp_ob%res > 0]      <a href="#hyd_read_connect">hyd_read_connect</a>(reservoir.con)
├── [if sp_ob%recall > 0]   <a href="#hyd_read_connect">hyd_read_connect</a>(recall.con)
├── [if sp_ob%exco > 0]     <a href="#hyd_read_connect">hyd_read_connect</a>(exco.con)
├── [if sp_ob%dr > 0]
│   ├── <a href="#hyd_read_connect">hyd_read_connect</a>(delivratio.con)
│   └── <a href="#dr_db_read">dr_db_read</a>           read delivery ratio database
├── [if sp_ob%outlet > 0]   <a href="#hyd_read_connect">hyd_read_connect</a>(outlet.con)
├── [if sp_ob%chandeg > 0]  <a href="#hyd_read_connect">hyd_read_connect</a>(chandeg.con)
└── [if sp_ob%gwflow > 0]
    ├── <a href="#gwflow_chan_read">gwflow_chan_read</a>      read gwflow–channel cell linkage
    ├── <a href="#hyd_read_connect">hyd_read_connect</a>(gwflow.con)
    └── <a href="#gwflow_read">gwflow_read</a>          read gwflow spatial parameters
</pre>

---

## recalldb_read

Called from `main` after `hyd_connect`; loops over all recall objects and loads each recall input time series.

**Called from:** [`main`](#main)


<pre>
recalldb_read
└── [loop i = 1, sp_ob%recall]  <a href="#recall_read">recall_read</a>(i)
</pre>

---

## exco_db_read

Called from `main`; reads export-coefficient object parameters.

**Called from:** [`main`](#main)


<pre>
exco_db_read
├── <a href="#exco_read_om">exco_read_om</a>
├── [if cs_db%num_pests > 0]   <a href="#exco_read_pest">exco_read_pest</a>
├── [if cs_db%num_paths > 0]   <a href="#exco_read_path">exco_read_path</a>
├── [if cs_db%num_hmet > 0]    <a href="#exco_read_hmet">exco_read_hmet</a>
└── [if cs_db%num_salts > 0]   <a href="#exco_read_salt">exco_read_salt</a>
</pre>

---

## dr_db_read

Called from `main` (and conditionally from `hyd_connect` when `sp_ob%dr > 0`); reads delivery-ratio database parameters.

**Called from:** [`hyd_connect`](#hyd_connect), [`main`](#main)


<pre>
dr_db_read
├── <a href="#dr_read_om">dr_read_om</a>
├── [if cs_db%num_pests > 0]   <a href="#dr_read_pest">dr_read_pest</a>
├── [if cs_db%num_paths > 0]   <a href="#dr_path_read">dr_path_read</a>
├── [if cs_db%num_hmet > 0]    <a href="#dr_read_hmet">dr_read_hmet</a>
└── [if cs_db%num_salts > 0]   <a href="#dr_read_salt">dr_read_salt</a>
</pre>

---

## climate_control

Called daily from `time_control` at the start of each day loop. Reads measured data or calls weather generators for each weather station (`iwst`). Each variable falls back to its generator when the measured value is missing or out of bounds.

**Called from:** [`time_control`](#time_control)


<pre>
climate_control
│
├── <a href="#cli_precip_control">cli_precip_control</a> (1)      read/generate today's precipitation for all gages
│
└── [loop iwst = 1, db_mx%wst]  per weather station:
    ├── <a href="#cli_weatgn">cli_weatgn</a>(iwgn)         update WGN state for this station/month
    ├── [temperature]
    │   ├── [if simulated]    <a href="#cli_tgen">cli_tgen</a>(iwgn)      generate Tmax, Tmin
    │   └── [if measured]     read tmp(:)%ts; <a href="#cli_tgen">cli_tgen</a> on missing/out-of-bounds
    │                         <a href="#cli_bounds_check">cli_bounds_check</a> (tmp bounds)
    ├── [solar radiation]
    │   ├── <a href="#cli_clgen">cli_clgen</a>(iwgn)       update sub-daily storm parameters
    │   ├── [if simulated]    <a href="#cli_slrgen">cli_slrgen</a>(iwgn)    generate solar radiation
    │   └── [if measured]     read slr(:)%ts; <a href="#cli_slrgen">cli_slrgen</a> on missing
    │                         <a href="#cli_bounds_check">cli_bounds_check</a> (slr bounds)
    ├── [relative humidity]
    │   ├── [if simulated]    <a href="#cli_rhgen">cli_rhgen</a>(iwgn)     generate relative humidity
    │   └── [if measured]     read hmd(:)%ts; <a href="#cli_rhgen">cli_rhgen</a> on missing
    │                         <a href="#cli_bounds_check">cli_bounds_check</a> (hmd bounds)
    ├── [wind speed]
    │   ├── [if simulated]    <a href="#cli_wndgen">cli_wndgen</a>(iwgn)    generate wind speed
    │   └── [if measured]     read wnd(:)%ts; <a href="#cli_wndgen">cli_wndgen</a> on missing
    │                         <a href="#cli_bounds_check">cli_bounds_check</a> (wnd bounds)
    ├── [PET — if measured gage configured]
    │   └── read petm(:)%ts; Hargreaves method as fallback on missing values
    │       <a href="#cli_bounds_check">cli_bounds_check</a> (petm bounds)
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
└── <a href="#define_unit_elements">define_unit_elements</a>                save the object number of each defining unit
</pre>

---

## calhard_control

re-initialize all objects

**Called from:** [`main`](#main)


Source: `calhard_control.f90`

<pre>
calhard_control
├── <a href="#re_initialize">re_initialize</a>                       reset basin soil water for next simulation
└── <a href="#time_control">time_control</a>                        this subroutine contains the loops governing the modeling of processes
</pre>

---

## hru_lte_read

Source: `hru_lte_read.f90`

**Called from:** [`main`](#main)


<pre>
hru_lte_read
└── <a href="#ascrv">ascrv</a>                               this subroutine computes shape parameters x5 and x6 for the S curve
</pre>

---

## lsu_read_elements

read landscape cataloging unit definitions for output (old subbasin output file)

**Called from:** [`main`](#main)


Source: `lsu_read_elements.f90`

<pre>
lsu_read_elements
└── <a href="#define_unit_elements">define_unit_elements</a>                save the object number of each defining unit
</pre>

---

## proc_res

allocate and initialize reservoir variables

**Called from:** [`main`](#main)


Source: `proc_res.f90`

<pre>
proc_res
├── <a href="#res_allo">res_allo</a>
├── <a href="#res_initial">res_initial</a>                         set initial volumes for res and hru types and convert units
├── <a href="#res_objects">res_objects</a>                         set reservoir object numbers for reservoir objects
├── <a href="#res_read">res_read</a>                            read reservoir.res
├── <a href="#res_read_conds">res_read_conds</a>
├── <a href="#res_read_csdb">res_read_csdb</a>                       this subroutine reads reservoir water quality parameters for constituents
├── <a href="#res_read_hyd">res_read_hyd</a>
├── <a href="#res_read_init">res_read_init</a>                       read init
├── <a href="#res_read_nut">res_read_nut</a>                        this subroutine reads data from the lake water quality input file (.lwq).
├── <a href="#res_read_salt_cs">res_read_salt_cs</a>                    read reservoir.res_cs
├── <a href="#res_read_saltdb">res_read_saltdb</a>                     this subroutine reads reservoir water quality parameters for salt ions
└── <a href="#res_read_sed">res_read_sed</a>                        this subroutine reads data from the lake water quality input file (.lwq).
</pre>

---

## unit_hyd_ru_hru

compute unit hydrographs for all hru and ru

**Called from:** [`main`](#main)


Source: `unit_hyd_ru_hru.f90`

<pre>
unit_hyd_ru_hru
└── <a href="#unit_hyd">unit_hyd</a>                            This subroutine computes variables related to the watershed hydrology:
</pre>

---

## aqu_1d_control

set pointers to aquifer database and weather station

**Called from:** [`command`](#command)


Source: `aqu_1d_control.f90`

<pre>
aqu_1d_control
├── <a href="#cs_rctn_aqu">cs_rctn_aqu</a>                         this subroutine updates constituent concentrations based on chemical reactions in groundwater
├── <a href="#cs_sorb_aqu">cs_sorb_aqu</a>                         this subroutine updates constituent concentrations based on sorption in the aquifer
└── <a href="#salt_chem_aqu">salt_chem_aqu</a>                       this subroutine calculates salt ion concentrations based on equilibrium chemical reactions, for an HRU
</pre>

---

## gwflow_simulate

this subroutine calculates new groundwater storage and solute mass for each gwflow grid cell;

**Called from:** [`command`](#command)


Source: `gwflow_simulate.f90`

<pre>
gwflow_simulate
├── <a href="#gwflow_canal_div">gwflow_canal_div</a>                    this subroutine calculates the water exchange volume between irrigation canals and connected grid cells
├── <a href="#gwflow_canal_ext">gwflow_canal_ext</a>                    this subroutine calculates the water exchange volume between irrigation canals and connected grid cells
├── <a href="#gwflow_gwet">gwflow_gwet</a>                         this subroutine determines the volume of groundwater that is removed from the
├── <a href="#gwflow_lateral">gwflow_lateral</a>                      this subroutine calculates lateral groundwater flow between adjacent cells
├── <a href="#gwflow_output_aa">gwflow_output_aa</a>                    this subroutine writes average annual gwflow output in SWAT+ long format:
├── <a href="#gwflow_output_day">gwflow_output_day</a>                   this subroutine computes and writes daily gwflow output:
├── <a href="#gwflow_output_mon">gwflow_output_mon</a>                   writes monthly gwflow output
├── <a href="#gwflow_output_yr">gwflow_output_yr</a>                    writes yearly gwflow output
├── <a href="#gwflow_phreatophyte">gwflow_phreatophyte</a>                 this subroutine calculates the water removed from the aquifer via phreatophyte extraction
├── <a href="#gwflow_pond">gwflow_pond</a>                         this subroutine calculates the volume of seepage from recharge ponds;
├── <a href="#gwflow_pump_ext">gwflow_pump_ext</a>                     this subroutine determines the volume of groundwater that is extracted
└── <a href="#gwflow_rech">gwflow_rech</a>                         this subroutine determines the volume of groundwater that is added to the aquifer via recharge (soil percolation)
</pre>

---

## hru_lte_control

Source: `hru_lte_control.f90`

**Called from:** [`command`](#command)


<pre>
hru_lte_control
├── <a href="#actions">actions</a>
└── <a href="#conditions">conditions</a>                          current <a href="#conditions">conditions</a> include: w_stress, n_stress, phu_plant, phu_base0, soil_water, jday, month, vol
</pre>

---

## hru_output

this subroutine outputs HRU variables on daily, monthly and annual time steps

**Called from:** [`command`](#command)


Source: `hru_output.f90`

<pre>
hru_output
└── <a href="#soil_nutcarb_write">soil_nutcarb_write</a>                  this subroutine writes soil carbon output.
</pre>

---

## res_control

Source: `res_control.f90`

**Called from:** [`command`](#command)


<pre>
res_control
├── [if bsn_cc%lapse == 1]  <a href="#cli_lapse">cli_lapse</a>   lapse-rate weather adjustment for reservoir elevation
├── <a href="#conditions">conditions</a>                          current <a href="#conditions">conditions</a> include: w_stress, n_stress, phu_plant, phu_base0, soil_water, jday, month, vol
├── <a href="#gwflow_reservoir">gwflow_reservoir</a>                    this subroutine calculates the water exchange volume between the reservoir and the connected grid cells
├── <a href="#res_cs">res_cs</a>                              this subroutine computes the reservoir constituent mass balance
├── <a href="#res_hydro">res_hydro</a>                           Jose T 2025 |  Doell method
├── <a href="#res_nutrient">res_nutrient</a>                        if reservoir volume less than 1 m^3, set all nutrient levels to
├── <a href="#res_pest">res_pest</a>                            this subroutine computes the lake hydrologic pesticide balance.
├── <a href="#res_rel_conds">res_rel_conds</a>                       ictbl = 1     nbs
├── <a href="#res_salt">res_salt</a>                            this subroutine computes the reservoir salt ion balance
├── <a href="#res_sediment">res_sediment</a>                        reservoir is empty
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
└── <a href="#flow_hyd_ru_hru">flow_hyd_ru_hru</a>                     this subroutine determines the subdaily flow hydrographs for hru's, ru's and inflow fractions
</pre>

---

## sd_channel_control3

rtb floodplain

**Called from:** [`command`](#command)


Source: `sd_channel_control3.f90`

<pre>
sd_channel_control3
├── [if bsn_cc%lapse == 1]  <a href="#cli_lapse">cli_lapse</a>   lapse-rate weather adjustment
├── <a href="#wallo_control">wallo_control</a>               water allocation for channel objects
├── <a href="#ch_rtmusk">ch_rtmusk</a>                           this subroutine routes a daily flow through a reach using the
├── <a href="#ch_rtpath">ch_rtpath</a>                           this subroutine routes bacteria through the stream network
├── <a href="#ch_rtpest">ch_rtpest</a>                           this subroutine computes the daily stream pesticide balance
├── <a href="#ch_temp">ch_temp</a>                             parameters for temperature model
├── <a href="#ch_watqual4">ch_watqual4</a>                         this subroutine performs in-stream nutrient transformations and water
├── <a href="#gwflow_canal">gwflow_canal</a>                        this subroutine calculates the water exchange volume between irrigation canals and connected grid cells
├── <a href="#gwflow_channel_exch">gwflow_channel_exch</a>                 this subroutine calculates the water exchange volume between the channel and the connected grid cells
├── <a href="#gwflow_satexcess">gwflow_satexcess</a>                    this subroutine calculates the groundwater volume that enters the channel via saturation excess flow
├── <a href="#gwflow_tile">gwflow_tile</a>                         this subroutine calculates the water exchange volume between irrigation canals and connected grid cells
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
└── <a href="#define_unit_elements">define_unit_elements</a>                save the object number of each defining unit
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
└── <a href="#search">search</a>
</pre>

---

## ru_read_elements

read data for each element in all subbasins

**Called from:** [`hyd_connect`](#hyd_connect)


Source: `ru_read_elements.f90`

<pre>
ru_read_elements
└── <a href="#define_unit_elements">define_unit_elements</a>                save the object number of each defining unit
</pre>

---

## co2_read

output annual CO2

**Called from:** [`proc_bsn`](#proc_bsn)


Source: `co2_read.f90`

<pre>
co2_read
└── <a href="#open_output_file">open_output_file</a>                    Get full path
</pre>

---

## readcio_read

read file.cio

**Called from:** [`proc_bsn`](#proc_bsn)


Source: `readcio_read.f90`

<pre>
readcio_read
└── <a href="#init_output_path">init_output_path</a>                    Detect OS - Runtime check is more robust if preprocessor fails
</pre>

---

## time_read

read weather codes

**Called from:** [`proc_bsn`](#proc_bsn)


Source: `time_read.f90`

<pre>
time_read
└── <a href="#xmon">xmon</a>                                this subroutine determines the month, given the julian date and leap
</pre>

---

## aqu_read_elements

Source: `aqu_read_elements.f90`

**Called from:** [`proc_cal`](#proc_cal)


<pre>
aqu_read_elements
└── <a href="#define_unit_elements">define_unit_elements</a>                save the object number of each defining unit
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
└── <a href="#define_unit_elements">define_unit_elements</a>                save the object number of each defining unit
</pre>

---

## pl_read_parms_cal

Source: `pl_read_parms_cal.f90`

**Called from:** [`proc_cal`](#proc_cal)


<pre>
pl_read_parms_cal
└── <a href="#define_unit_elements">define_unit_elements</a>                save the object number of each defining unit
</pre>

---

## pl_read_regions_cal

Source: `pl_read_regions_cal.f90`

**Called from:** [`proc_cal`](#proc_cal)


<pre>
pl_read_regions_cal
└── <a href="#define_unit_elements">define_unit_elements</a>                save the object number of each defining unit
</pre>

---

## rec_read_elements

Source: `rec_read_elements.f90`

**Called from:** [`proc_cal`](#proc_cal)


<pre>
rec_read_elements
└── <a href="#define_unit_elements">define_unit_elements</a>                save the object number of each defining unit
</pre>

---

## res_read_elements

Source: `res_read_elements.f90`

**Called from:** [`proc_cal`](#proc_cal)


<pre>
res_read_elements
└── <a href="#define_unit_elements">define_unit_elements</a>                save the object number of each defining unit
</pre>

---

## sd_hydsed_init

Source: `sd_hydsed_init.f90`

**Called from:** [`proc_cha`](#proc_cha)


<pre>
sd_hydsed_init
├── <a href="#hyd_convert_conc_to_mass">hyd_convert_conc_to_mass</a>            m3/s to m3
├── <a href="#rcurv_interp_dep">rcurv_interp_dep</a>                    this subroutine interpolates between points on a rating curve given flow rate
└── <a href="#sd_rating_curve">sd_rating_curve</a>                     use hydrograph_module
</pre>

---

## cli_tmeas

read all measured daily temperature data

**Called from:** [`proc_date_time`](#proc_date_time)


Source: `cli_tmeas.f90`

<pre>
cli_tmeas
└── <a href="#xmon">xmon</a>                                this subroutine determines the month, given the julian date and leap
</pre>

---

## cli_wgnread

read weather generator data from weather_generator.dat - wgn parameters

**Called from:** [`proc_date_time`](#proc_date_time)


Source: `cli_wgnread.f90`

<pre>
cli_wgnread
├── <a href="#cli_initwgn">cli_initwgn</a>                         this subroutine initializes the HRU weather generator parameters from the
└── <a href="#gcycl">gcycl</a>                               This subroutine initializes the random number seeds. If the user
</pre>

---

## mgt_read_mgtops

read mgtops.dat file

**Called from:** [`proc_db`](#proc_db)


Source: `mgt_read_mgtops.f90`

<pre>
mgt_read_mgtops
└── <a href="#read_mgtops">read_mgtops</a>
</pre>

---

## plantparm_init

set default values

**Called from:** [`proc_db`](#proc_db)


Source: `plantparm_init.f90`

<pre>
plantparm_init
└── <a href="#ascrv">ascrv</a>                               this subroutine computes shape parameters x5 and x6 for the S curve
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
└── <a href="#hru_lum_init">hru_lum_init</a>                        assign land use pointers for the hru
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
├── <a href="#soil_phys_init">soil_phys_init</a>                      this subroutine initializes soil physical properties
├── <a href="#soils_test_adjust">soils_test_adjust</a>                   Adjust the input soil values based input soil test values.
└── <a href="#layersplit">layersplit</a>                          split soil layer for thickness adjustments (called twice)
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
└── <a href="#ttcoef_wway">ttcoef_wway</a>                         this subroutine computes travel time coefficients for routing
</pre>

---

## topohyd_init

assign topography and hyd parameters

**Called from:** [`proc_hru`](#proc_hru)


Source: `topohyd_init.f90`

<pre>
topohyd_init
└── <a href="#ascrv">ascrv</a>                               this subroutine computes shape parameters x5 and x6 for the S curve
</pre>

---

## header_aquifer

AQUIFER

**Called from:** [`proc_open`](#proc_open)


Source: `header_aquifer.f90`

<pre>
header_aquifer
└── <a href="#open_output_file">open_output_file</a>                    Get full path
</pre>

---

## header_channel

CHANNEL

**Called from:** [`proc_open`](#proc_open)


Source: `header_channel.f90`

<pre>
header_channel
└── <a href="#open_output_file">open_output_file</a>                    Get full path
</pre>

---

## header_const

Source: `header_const.f90`

**Called from:** [`proc_open`](#proc_open)


<pre>
header_const
└── <a href="#open_output_file">open_output_file</a>                    Get full path
</pre>

---

## header_hyd

HYDCON (no headers)

**Called from:** [`proc_open`](#proc_open)


Source: `header_hyd.f90`

<pre>
header_hyd
└── <a href="#open_output_file">open_output_file</a>                    Get full path
</pre>

---

## header_lu_change

open lu_change output file

**Called from:** [`proc_open`](#proc_open)


Source: `header_lu_change.f90`

<pre>
header_lu_change
└── <a href="#open_output_file">open_output_file</a>                    Get full path
</pre>

---

## header_mgt

open mgt.out file

**Called from:** [`proc_open`](#proc_open)


Source: `header_mgt.f90`

<pre>
header_mgt
└── <a href="#open_output_file">open_output_file</a>                    Get full path
</pre>

---

## header_path

HRU_PATHOGEN - daily

**Called from:** [`proc_open`](#proc_open)


Source: `header_path.f90`

<pre>
header_path
└── <a href="#open_output_file">open_output_file</a>                    Get full path
</pre>

---

## header_pest

HRU_PESTICIDE - daily

**Called from:** [`proc_open`](#proc_open)


Source: `header_pest.f90`

<pre>
header_pest
└── <a href="#open_output_file">open_output_file</a>                    Get full path
</pre>

---

## header_reservoir

use hydrograph_module, only : res, sp_ob

**Called from:** [`proc_open`](#proc_open)


Source: `header_reservoir.f90`

<pre>
header_reservoir
└── <a href="#open_output_file">open_output_file</a>                    Get full path
</pre>

---

## header_salt

Source: `header_salt.f90`

**Called from:** [`proc_open`](#proc_open)


<pre>
header_salt
└── <a href="#open_output_file">open_output_file</a>                    Get full path
</pre>

---

## header_sd_channel

SWAT-DEG CHANNEL - SUBDAILY OUTPUT

**Called from:** [`proc_open`](#proc_open)


Source: `header_sd_channel.f90`

<pre>
header_sd_channel
└── <a href="#open_output_file">open_output_file</a>                    Get full path
</pre>

---

## header_water_allocation

Water Allocation Output

**Called from:** [`proc_open`](#proc_open)


Source: `header_water_allocation.f90`

<pre>
header_water_allocation
└── <a href="#open_output_file">open_output_file</a>                    Get full path
</pre>

---

## header_wetland

RESERVOIR/WETLAND - DAILY

**Called from:** [`proc_open`](#proc_open)


Source: `header_wetland.f90`

<pre>
header_wetland
└── <a href="#open_output_file">open_output_file</a>                    Get full path
</pre>

---

## header_write

Source: `header_write.f90`

**Called from:** [`proc_open`](#proc_open)


<pre>
header_write
└── <a href="#open_output_file">open_output_file</a>                    Get full path
</pre>

---

## header_yield

yield biomass file

**Called from:** [`proc_open`](#proc_open)


Source: `header_yield.f90`

<pre>
header_yield
└── <a href="#open_output_file">open_output_file</a>                    Get full path
</pre>

---

## output_landscape_init

HRU - Water balance

**Called from:** [`proc_open`](#proc_open)


Source: `output_landscape_init.f90`

<pre>
output_landscape_init
├── <a href="#open_output_file">open_output_file</a>                    Get full path
└── <a href="#soil_nutcarb_write">soil_nutcarb_write</a>                  this subroutine writes soil carbon output.
</pre>

---

## res_initial

set initial volumes for res and hru types and convert units

**Called from:** [`proc_res`](#proc_res)


Source: `res_initial.f90`

<pre>
res_initial
└── <a href="#res_convert_mass">res_convert_mass</a>
</pre>

---

## actions

Source: `actions.f90`

**Called from:** [`hru_control`](#hru_control), [`hru_lte_control`](#hru_lte_control), [`mallo_control`](#mallo_control), [`time_control`](#time_control)


<pre>
actions
├── <a href="#cn2_init">cn2_init</a>                            assign cn2
├── <a href="#cs_fert">cs_fert</a>                             this subroutine adds constituent fertilizer to the soil profile
├── <a href="#curno">curno</a>                               this subroutine determines the curve numbers for moisture <a href="#conditions">conditions</a>
├── <a href="#hru_fr_change">hru_fr_change</a>                       read data for each element in all routing units
├── <a href="#hru_lum_init">hru_lum_init</a>                        assign land use pointers for the hru
├── <a href="#mgt_harvbiomass">mgt_harvbiomass</a>                     this subroutine performs the harvest operation for above ground biomass (no kill)
├── <a href="#mgt_harvgrain">mgt_harvgrain</a>                       this subroutine performs the harvest grain only operation
├── <a href="#mgt_harvresidue">mgt_harvresidue</a>                     this subroutine performs the harvest residue operation
├── <a href="#mgt_harvtuber">mgt_harvtuber</a>                       this subroutine performs the harvest grain only operation
├── <a href="#mgt_killop">mgt_killop</a>                          this subroutine performs the kill operation
├── <a href="#mgt_newtillmix_cswat0">mgt_newtillmix_cswat0</a>               this subroutine mixes residue and nutrients during tillage and
├── <a href="#mgt_newtillmix_cswat1">mgt_newtillmix_cswat1</a>               this subroutine mixes residue and nutrients during tillage and
├── <a href="#mgt_newtillmix_wet">mgt_newtillmix_wet</a>                  this subroutine mixes residue and nutrients in soil layers and ponding water during tillage
├── <a href="#mgt_transplant">mgt_transplant</a>                      set initial heat units and other data
├── <a href="#pest_apply">pest_apply</a>                          this subroutine applies pesticide
├── <a href="#pl_burnop">pl_burnop</a>                           this subroutine performs all management operations
├── <a href="#pl_fert">pl_fert</a>                             this subroutine applies N and P specified by date and
├── <a href="#pl_fert_wet">pl_fert_wet</a>                         this subroutine applies N and P specified by date and
├── <a href="#pl_graze">pl_graze</a>                            graze only if adequate biomass in HRU
├── <a href="#pl_manure">pl_manure</a>                           this subroutine applies N and P specified by date and
├── <a href="#plant_init">plant_init</a>                          use hru_lte_module
├── <a href="#salt_fert">salt_fert</a>                           this subroutine adds salt fertilizer to the soil profile
├── <a href="#structure_set_parms">structure_set_parms</a>                 this subroutine controls the simulation of the land phase of the
└── <a href="#wet_initial">wet_initial</a>                         check if hru can store <a href="#surface">surface</a> water
</pre>

---

## cli_precip_control

this subroutine controls weather inputs to SWAT. Precipitation and

**Called from:** [`climate_control`](#climate_control), [`time_control`](#time_control)


Source: `cli_precip_control.f90`

<pre>
cli_precip_control
├── <a href="#cli_bounds_check">cli_bounds_check</a>                    this subroutine checks to see if climate data is in current simulation day
├── <a href="#cli_pgen">cli_pgen</a>                            this subroutine generates precipitation data when the user chooses to
└── <a href="#cli_pgenhr">cli_pgenhr</a>                          this subroutine distributes daily rainfall exponentially within the day
</pre>

---

## conditions

current conditions include: w_stress, n_stress, phu_plant, phu_base0, soil_water, jday, month, vol

**Called from:** [`hru_control`](#hru_control), [`hru_lte_control`](#hru_lte_control), [`mallo_control`](#mallo_control), [`res_control`](#res_control), [`time_control`](#time_control), [`wetland_control`](#wetland_control)


Source: `conditions.f90`

<pre>
conditions
├── <a href="#cond_integer">cond_integer</a>                        current <a href="#conditions">conditions</a> include: w_stress, n_stress, phu_plant, phu_base0, soil_water, jday, month, vol
└── <a href="#cond_real">cond_real</a>                           current <a href="#conditions">conditions</a> include: w_stress, n_stress, phu_plant, phu_base0, soil_water, jday, month, vol
</pre>

---

## mallo_control

zero demand, withdrawal, and unmet for entire allocation object

**Called from:** [`time_control`](#time_control)


Source: `mallo_control.f90`

<pre>
mallo_control
├── <a href="#actions">actions</a>
├── <a href="#conditions">conditions</a>                          current <a href="#conditions">conditions</a> include: w_stress, n_stress, phu_plant, phu_base0, soil_water, jday, month, vol
└── <a href="#pl_fert">pl_fert</a>                             this subroutine applies N and P specified by date and
</pre>

---

## cs_rctn_aqu

this subroutine updates constituent concentrations based on chemical reactions in groundwater

**Called from:** [`aqu_1d_control`](#aqu_1d_control)


Source: `cs_rctn_aqu.f90`

<pre>
cs_rctn_aqu
└── <a href="#se_reactions_aquifer">se_reactions_aquifer</a>                get concentration of SeO4 and SeO3
</pre>

---

## salt_chem_aqu

this subroutine calculates salt ion concentrations based on equilibrium chemical reactions, for an HRU

**Called from:** [`aqu_1d_control`](#aqu_1d_control)


Source: `salt_chem_aqu.f90`

<pre>
salt_chem_aqu
├── <a href="#activity_coefficient">activity_coefficient</a>
├── <a href="#caco3">caco3</a>
├── <a href="#caso4">caso4</a>
├── <a href="#cationexchange">cationexchange</a>                      CEC selected based on soil type; for simplicity, for now used one value based on the sandy-loam soil type
├── <a href="#ionic_strength">ionic_strength</a>
├── <a href="#mgco3">mgco3</a>
├── <a href="#mgso4">mgso4</a>
└── <a href="#nacl">nacl</a>
</pre>

---

## gwflow_lateral

this subroutine calculates lateral groundwater flow between adjacent cells

**Called from:** [`gwflow_simulate`](#gwflow_simulate)


Source: `gwflow_lateral.f90`

<pre>
gwflow_lateral
├── <a href="#gwflow_heat">gwflow_heat</a>                         this subroutine calculates heat advection and dispersion for groundwater
└── <a href="#gwflow_solute">gwflow_solute</a>                       this subroutine calculates solute advection, dispersion, chemical
</pre>

---

## gwflow_output_aa

this subroutine writes average annual gwflow output in SWAT+ long format:

**Called from:** [`gwflow_simulate`](#gwflow_simulate)


Source: `gwflow_output.f90`

<pre>
gwflow_output_aa
└── <a href="#gwflow_write_cell_array">gwflow_write_cell_array</a>             Writes active cell values as a single row.
</pre>

---

## cs_rctn_hru

this subroutine updates constituent concentrations based on chemical reactions and sorption in the soil profile

**Called from:** [`hru_control`](#hru_control)


Source: `cs_rctn_hru.f90`

<pre>
cs_rctn_hru
└── <a href="#se_reactions_soil">se_reactions_soil</a>                   suppress unused variable warning
</pre>

---

## hru_hyds

this subroutine summarizes data for subbasins with multiple HRUs and

**Called from:** [`hru_control`](#hru_control)


Source: `hru_hyds.f90`

<pre>
hru_hyds
└── <a href="#flow_hyd_ru_hru">flow_hyd_ru_hru</a>                     this subroutine determines the subdaily flow hydrographs for hru's, ru's and inflow fractions
</pre>

---

## hru_urbanhr

this subroutine computes loadings from urban areas using the

**Called from:** [`hru_control`](#hru_control)


Source: `hru_urbanhr.f90`

<pre>
hru_urbanhr
└── <a href="#hru_sweep">hru_sweep</a>                           the subroutine performs the street sweeping operation
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
├── <a href="#pl_dormant">pl_dormant</a>                          this subroutine checks the dormant status of the different plant types
├── [if time%end_yr == 1]  <a href="#pl_mortality">pl_mortality</a>  annual plant mortality
├── <a href="#pl_leaf_gro">pl_leaf_gro</a>                         this subroutine adjusts plant biomass, leaf area index, and canopy height
├── <a href="#pl_leaf_senes">pl_leaf_senes</a>                       lai decline for annuals - if dlai < phuacc < 1
├── <a href="#pl_nut_demand">pl_nut_demand</a>                       this subroutine predicts daily potential growth of total plant
├── <a href="#pl_partition">pl_partition</a>                        update plant mass for daily biomass/c increase
├── <a href="#pl_root_gro">pl_root_gro</a>                         calculate root depth
└── <a href="#pl_seed_gro">pl_seed_gro</a>                         calculate plant ET values when heat units exceed 0.5
</pre>

---

## salt_chem_hru

this subroutine calculates salt ion concentrations based on equilibrium chemical reactions, for an HRU

**Called from:** [`hru_control`](#hru_control)


Source: `salt_chem_hru.f90`

<pre>
salt_chem_hru
├── <a href="#activity_coefficient">activity_coefficient</a>
├── <a href="#caco3">caco3</a>
├── <a href="#caso4">caso4</a>
├── <a href="#cationexchange">cationexchange</a>                      CEC selected based on soil type; for simplicity, for now used one value based on the sandy-loam soil type
├── <a href="#ionic_strength">ionic_strength</a>
├── <a href="#mgco3">mgco3</a>
├── <a href="#mgso4">mgso4</a>
└── <a href="#nacl">nacl</a>
</pre>

---

## surface

this subroutine models surface hydrology at any desired time step

**Called from:** [`hru_control`](#hru_control)


Source: `surface.f90`

<pre>
surface
├── <a href="#ero_cfactor">ero_cfactor</a>                         this subroutine predicts daily soil loss caused by water erosion
├── <a href="#ero_eiusle">ero_eiusle</a>                          This subroutine computes the USLE erosion index (EI)
├── <a href="#ero_ovrsed">ero_ovrsed</a>                          this subroutine computes splash erosion by raindrop impact and flow erosion by overland flow
├── <a href="#ero_pkq">ero_pkq</a>                             this subroutine computes the peak runoff rate for each HRU
├── <a href="#ero_ysed">ero_ysed</a>                            this subroutine predicts daily soil loss caused by water erosion
├── <a href="#sq_dailycn">sq_dailycn</a>                          Calculates curve number for the day in the HRU
├── [if surfq > 0 and bsn_cc%crk == 1]  <a href="#sq_crackflow">sq_crackflow</a>   route <a href="#surface">surface</a> runoff into soil cracks
└── <a href="#sq_volq">sq_volq</a>                             Call subroutines to calculate the current day"s CN for the HRU and
</pre>

---

## swr_percmain

this subroutine is the master soil percolation component.

**Called from:** [`hru_control`](#hru_control)


Source: `swr_percmain.f90`

<pre>
swr_percmain
├── <a href="#gwflow_soil">gwflow_soil</a>                         this subroutine calculates the water exchange volume between the aquifer and the soil profile
├── <a href="#swr_drains">swr_drains</a>                          this subroutine finds the effective lateral hydraulic conductivity
├── <a href="#swr_origtile">swr_origtile</a>                        this subroutine computes tile drainage using basic tile equations
├── <a href="#swr_percmacro">swr_percmacro</a>                       this surboutine computes percolation by crack flow
├── <a href="#swr_percmicro">swr_percmicro</a>                       this subroutine computes percolation and lateral subsurface flow
└── <a href="#swr_satexcess">swr_satexcess</a>                       this subroutine moves water to upper layers if saturated and can't perc
</pre>

---

## wetland_control

Source: `wetland_control.f90`

**Called from:** [`hru_control`](#hru_control)


<pre>
wetland_control
├── <a href="#conditions">conditions</a>                          current <a href="#conditions">conditions</a> include: w_stress, n_stress, phu_plant, phu_base0, soil_water, jday, month, vol
├── <a href="#ero_cfactor">ero_cfactor</a>                         this subroutine predicts daily soil loss caused by water erosion
├── <a href="#gwflow_wetland">gwflow_wetland</a>                      this subroutine determines the volume of groundwater exchanged with wetlands
├── <a href="#res_hydro">res_hydro</a>                           Jose T 2025 |  Doell method
├── <a href="#res_nutrient">res_nutrient</a>                        if reservoir volume less than 1 m^3, set all nutrient levels to
├── <a href="#res_sediment">res_sediment</a>                        reservoir is empty
├── <a href="#res_weir_release">res_weir_release</a>                    suppress unused variable warning
├── <a href="#wet_cs">wet_cs</a>                              this subroutine computes the wetland constituent mass balance
└── <a href="#wet_salt">wet_salt</a>                            this subroutine computes the wetland salt ion mass balance
</pre>

---

## res_rel_conds

ictbl = 1     nbs

**Called from:** [`res_control`](#res_control)


Source: `res_rel_conds.f90`

<pre>
res_rel_conds
├── <a href="#cond_integer_c">cond_integer_c</a>
└── <a href="#cond_real_c">cond_real_c</a>
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
└── <a href="#chrc_interp">chrc_interp</a>
</pre>

---

## sd_channel_sediment3

Source: `sd_channel_sediment3.f90`

**Called from:** [`sd_channel_control3`](#sd_channel_control3)


<pre>
sd_channel_sediment3
├── <a href="#gwflow_floodplain">gwflow_floodplain</a>                   this subroutine calculates the water exchange volume between the floodplain and the connected grid cells
└── <a href="#rcurv_interp_flo">rcurv_interp_flo</a>                    this subroutine interpolates between points on a rating curve given flow rate
</pre>

---

## gwflow_output_init

this subroutine opens all gwflow output files and writes headers

**Called from:** [`gwflow_read`](#gwflow_read)


Source: `gwflow_output.f90`

<pre>
gwflow_output_init
└── <a href="#gwflow_write_celldef">gwflow_write_celldef</a>                Writes gwflow_cell_definition.txt once during initialization.
</pre>

---

## cal_parm_select

this subroutine finds the current parameter value based on

**Called from:** [`cal_conditions`](#cal_conditions)


Source: `cal_parm_select.f90`

<pre>
cal_parm_select
├── <a href="#curno">curno</a>                               this subroutine determines the curve numbers for moisture <a href="#conditions">conditions</a>
├── <a href="#soil_awc_init">soil_awc_init</a>                       this subroutine initializes soil parameters based on awc
└── <a href="#soil_text_init">soil_text_init</a>                      this subroutine initializes soil parameters based on awc
</pre>

---

## rcurv_interp_dep

this subroutine interpolates between points on a rating curve given flow rate

**Called from:** [`sd_hydsed_init`](#sd_hydsed_init)


Source: `rcurv_interp_dep.f90`

<pre>
rcurv_interp_dep
└── <a href="#chrc_interp">chrc_interp</a>
</pre>

---

## cn2_init

assign cn2

**Called from:** [`actions`](#actions), [`cn2_init_all`](#cn2_init_all)


Source: `cn2_init.f90`

<pre>
cn2_init
└── <a href="#curno">curno</a>                               this subroutine determines the curve numbers for moisture <a href="#conditions">conditions</a>
</pre>

---

## allocate_parms

this subroutine allocates array sizes

**Called from:** [`hru_read`](#hru_read)


Source: `allocate_parms.f90`

<pre>
allocate_parms
├── <a href="#zero0">zero0</a>                               this subroutine initializes the values for some of the arrays
├── <a href="#zero1">zero1</a>                               this subroutine initializes the values for some of the arrays
├── <a href="#zero2">zero2</a>                               this subroutine zeros all array values
└── <a href="#zeroini">zeroini</a>                             this subroutine zeros values for single array variables
</pre>

---

## plant_init

use hru_lte_module

**Called from:** [`actions`](#actions), [`plant_all_init`](#plant_all_init)


Source: `plant_init.f90`

<pre>
plant_init
├── <a href="#pl_partition">pl_partition</a>                        update plant mass for daily biomass/c increase
├── <a href="#pl_root_gro">pl_root_gro</a>                         calculate root depth
├── <a href="#pl_rootfr">pl_rootfr</a>                           This subroutine distributes dead root mass through the soil profile
├── <a href="#pl_seed_gro">pl_seed_gro</a>                         calculate plant ET values when heat units exceed 0.5
└── <a href="#xmon">xmon</a>                                this subroutine determines the month, given the julian date and leap
</pre>

---

## curno

this subroutine determines the curve numbers for moisture conditions

**Called from:** [`actions`](#actions), [`cal_parm_select`](#cal_parm_select), [`cn2_init`](#cn2_init), [`mgt_sched`](#mgt_sched), [`pl_burnop`](#pl_burnop)


Source: `curno.f90`

<pre>
curno
└── <a href="#ascrv">ascrv</a>                               this subroutine computes shape parameters x5 and x6 for the S curve
</pre>

---

## mgt_harvtuber

this subroutine performs the harvest grain only operation

**Called from:** [`actions`](#actions), [`mgt_sched`](#mgt_sched)


Source: `mgt_harvtuber.f90`

<pre>
mgt_harvtuber
└── <a href="#pl_rootfr">pl_rootfr</a>                           This subroutine distributes dead root mass through the soil profile
</pre>

---

## mgt_killop

this subroutine performs the kill operation

**Called from:** [`actions`](#actions), [`mgt_sched`](#mgt_sched)


Source: `mgt_killop.f90`

<pre>
mgt_killop
├── <a href="#pl_rootfr">pl_rootfr</a>                           This subroutine distributes dead root mass through the soil profile
└── <a href="#plg_zero">plg_zero</a>
</pre>

---

## mgt_newtillmix_cswat1

this subroutine mixes residue and nutrients during tillage and

**Called from:** [`actions`](#actions), [`mgt_sched`](#mgt_sched)


Source: `mgt_newtillmix_cswat1.f90`

<pre>
mgt_newtillmix_cswat1
└── <a href="#mgt_tillfactor">mgt_tillfactor</a>                      Armen 16 January 2008
</pre>

---

## mgt_transplant

set initial heat units and other data

**Called from:** [`actions`](#actions), [`mgt_sched`](#mgt_sched)


Source: `mgt_transplant.f90`

<pre>
mgt_transplant
├── <a href="#pl_partition">pl_partition</a>                        update plant mass for daily biomass/c increase
├── <a href="#pl_root_gro">pl_root_gro</a>                         calculate root depth
└── <a href="#pl_seed_gro">pl_seed_gro</a>                         calculate plant ET values when heat units exceed 0.5
</pre>

---

## pl_burnop

this subroutine performs all management operations

**Called from:** [`actions`](#actions), [`mgt_sched`](#mgt_sched)


Source: `pl_burnop.f90`

<pre>
pl_burnop
└── <a href="#curno">curno</a>                               this subroutine determines the curve numbers for moisture <a href="#conditions">conditions</a>
</pre>

---

## wet_initial

check if hru can store surface water

**Called from:** [`actions`](#actions)


Source: `wet_initial.f90`

<pre>
wet_initial
└── <a href="#res_convert_mass">res_convert_mass</a>
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
├── <a href="#pl_tstr">pl_tstr</a>                             computes temperature stress for crop growth - strstmp
├── <a href="#salt_uptake">salt_uptake</a>                         this subroutine simulates salt ion uptake in the root zone
└── [if cs_db%num_cs > 0]  <a href="#cs_uptake">cs_uptake</a>   constituent uptake by plants
</pre>

---

## pl_nut_demand

this subroutine predicts daily potential growth of total plant

**Called from:** [`pl_grow`](#pl_grow)


Source: `pl_nut_demand.f90`

<pre>
pl_nut_demand
├── <a href="#pl_nupd">pl_nupd</a>                             This subroutine calculates plant nitrogen demand
└── <a href="#pl_pupd">pl_pupd</a>                             this subroutine calculates plant phosphorus demand
</pre>

---

## pl_root_gro

calculate root depth

**Called from:** [`mgt_transplant`](#mgt_transplant), [`pl_grow`](#pl_grow), [`plant_init`](#plant_init)


Source: `pl_root_gro.f90`

<pre>
pl_root_gro
└── <a href="#pl_rootfr">pl_rootfr</a>                           This subroutine distributes dead root mass through the soil profile
</pre>

---

## sq_volq

Call subroutines to calculate the current day"s CN for the HRU and

**Called from:** [`surface`](#surface)


Source: `sq_volq.f90`

<pre>
sq_volq
├── <a href="#sq_daycn">sq_daycn</a>                            Predicts daily runoff given daily precipitation and snow melt
└── <a href="#sq_greenampt">sq_greenampt</a>                        Predicts daily runoff given breakpoint precipitation and snow melt
</pre>

---

## swr_drains

this subroutine finds the effective lateral hydraulic conductivity

**Called from:** [`swr_percmain`](#swr_percmain)


Source: `swr_drains.f90`

<pre>
swr_drains
└── <a href="#swr_depstor">swr_depstor</a>                         this subroutine computes maximum <a href="#surface">surface</a> depressional storage depth based on
</pre>

---

## gwflow_chem

this subroutine calculates chemical reactions in gwflow cells.

**Called from:** [`gwflow_solute`](#gwflow_solute)


Source: `gwflow_chem.f90`

<pre>
gwflow_chem
└── <a href="#gwflow_minl">gwflow_minl</a>                         this subroutine calculates salt mineral precipitation-dissolution
</pre>

---

## salt_chem_soil_single

this subroutine calculates salt ion concentrations based on equilibrium chemical reactions

**Called from:** [`pl_waterup`](#pl_waterup)


Source: `salt_chem_soil_single.f90`

<pre>
salt_chem_soil_single
├── <a href="#activity_coefficient">activity_coefficient</a>
├── <a href="#caco3">caco3</a>
├── <a href="#caso4">caso4</a>
├── <a href="#ionic_strength">ionic_strength</a>
├── <a href="#mgco3">mgco3</a>
├── <a href="#mgso4">mgso4</a>
└── <a href="#nacl">nacl</a>
</pre>

---

## pl_nup

This subroutine calculates plant nitrogen uptake

**Called from:** [`pl_biomass_gro`](#pl_biomass_gro)


Source: `pl_nup.f90`

<pre>
pl_nup
├── <a href="#nuts">nuts</a>                                this function calculates the plant stress factor caused by limited
└── <a href="#pl_nfix">pl_nfix</a>                             this subroutine estimates nitrogen fixation by legumes
</pre>

---

## pl_pup

this subroutine calculates plant phosphorus uptake

**Called from:** [`pl_biomass_gro`](#pl_biomass_gro)


Source: `pl_pup.f90`

<pre>
pl_pup
└── <a href="#nuts">nuts</a>                                this function calculates the plant stress factor caused by limited
</pre>

---

---

## wet_all_initial

Initialises all wetland/pond objects before the simulation loop.

**Called from:** [`main`](#main)

<pre>
wet_all_initial
└── [loop iihru = 1, sp_ob%hru]  <a href="#wet_initial">wet_initial</a>(iihru)
</pre>

---

## mgt_biomix

Performs daily biological mixing of CENTURY carbon pools within the tillage depth.
Called only when `bsn_cc%cswat == 1` and `bmix_eff > 1e-6`.

**Called from:** [`hru_control`](#hru_control)

<pre>
mgt_biomix
└── <a href="#mgt_tillfactor">mgt_tillfactor</a>           compute tillage factor for biological mixing efficiency
</pre>

---

## calsoft_control

Top-level soft-calibration controller. Runs repeated simulations adjusting parameters
to minimise objective function residuals.

**Called from:** [`main`](#main)

<pre>
calsoft_control
├── <a href="#calsoft_hyd">calsoft_hyd</a>            calibrate hydrologic components (<a href="#surface">surface</a>, lateral, perc, ET)
├── <a href="#calsoft_hyd_bfr">calsoft_hyd_bfr</a>        calibrate baseflow recession parameters
├── <a href="#caltsoft_hyd">caltsoft_hyd</a>           calibrate time-of-travel (storage-coefficient) routing
├── <a href="#calsoft_plant">calsoft_plant</a>          calibrate plant growth parameters
├── <a href="#calsoft_sed">calsoft_sed</a>            calibrate sediment parameters
└── <a href="#pl_write_parms_cal">pl_write_parms_cal</a>     write calibrated plant parameters
</pre>

---

## calsoft_hyd

Calibrates hydrologic parameters (CN2, ESCO, etc.) by iterating the full simulation.

**Called from:** [`calsoft_control`](#calsoft_control)

<pre>
calsoft_hyd
├── <a href="#re_initialize">re_initialize</a>          reset all state variables to initial <a href="#conditions">conditions</a>
├── <a href="#time_control">time_control</a>           run full simulation with current parameters
└── <a href="#curno">curno</a>                  recompute CN lookup table after CN2 adjustment
</pre>

---

## calsoft_hyd_bfr

Calibrates baseflow parameters by iterating sub-objective functions for each component.

**Called from:** [`calsoft_control`](#calsoft_control)

<pre>
calsoft_hyd_bfr
├── <a href="#calsoft_hyd_bfr_pet">calsoft_hyd_bfr_pet</a>    calibrate PET-related baseflow parameters
├── <a href="#calsoft_hyd_bfr_et">calsoft_hyd_bfr_et</a>     calibrate actual ET baseflow contribution
├── <a href="#calsoft_hyd_bfr_surq">calsoft_hyd_bfr_surq</a>   calibrate <a href="#surface">surface</a>-runoff influence on baseflow
├── <a href="#calsoft_hyd_bfr_latq">calsoft_hyd_bfr_latq</a>   calibrate lateral-flow influence on baseflow
└── <a href="#calsoft_hyd_bfr_perc">calsoft_hyd_bfr_perc</a>   calibrate percolation influence on baseflow
</pre>

---

## calsoft_hyd_bfr_pet

Calibrates PET parameter contribution to baseflow objective.

**Called from:** [`calsoft_hyd_bfr`](#calsoft_hyd_bfr)

<pre>
calsoft_hyd_bfr_pet
├── <a href="#re_initialize">re_initialize</a>
└── <a href="#time_control">time_control</a>
</pre>

---

## calsoft_hyd_bfr_et

Calibrates actual ET parameter contribution to baseflow objective.

**Called from:** [`calsoft_hyd_bfr`](#calsoft_hyd_bfr)

<pre>
calsoft_hyd_bfr_et
├── <a href="#re_initialize">re_initialize</a>
└── <a href="#time_control">time_control</a>
</pre>

---

## calsoft_hyd_bfr_surq

Calibrates surface-runoff influence on baseflow; adjusts CN2 before each run.

**Called from:** [`calsoft_hyd_bfr`](#calsoft_hyd_bfr)

<pre>
calsoft_hyd_bfr_surq
├── <a href="#curno">curno</a>
├── <a href="#re_initialize">re_initialize</a>
└── <a href="#time_control">time_control</a>
</pre>

---

## calsoft_hyd_bfr_latq

Calibrates lateral-flow influence on baseflow.

**Called from:** [`calsoft_hyd_bfr`](#calsoft_hyd_bfr)

<pre>
calsoft_hyd_bfr_latq
├── <a href="#re_initialize">re_initialize</a>
└── <a href="#time_control">time_control</a>
</pre>

---

## calsoft_hyd_bfr_perc

Calibrates percolation influence on baseflow.

**Called from:** [`calsoft_hyd_bfr`](#calsoft_hyd_bfr)

<pre>
calsoft_hyd_bfr_perc
├── <a href="#re_initialize">re_initialize</a>
└── <a href="#time_control">time_control</a>
</pre>

---

## calsoft_plant

Calibrates plant growth parameters by iterating the full simulation.

**Called from:** [`calsoft_control`](#calsoft_control)

<pre>
calsoft_plant
├── <a href="#calsoft_plant_zero">calsoft_plant_zero</a>     zero plant calibration accumulators
├── <a href="#re_initialize">re_initialize</a>
└── <a href="#time_control">time_control</a>
</pre>

---

## calsoft_sed

Calibrates sediment parameters by iterating the full simulation.

**Called from:** [`calsoft_control`](#calsoft_control)

<pre>
calsoft_sed
├── <a href="#re_initialize">re_initialize</a>
└── <a href="#time_control">time_control</a>
</pre>

---

## caltsoft_hyd

Calibrates time-of-travel (storage-coefficient) routing parameters.

**Called from:** [`calsoft_control`](#calsoft_control)

<pre>
caltsoft_hyd
├── <a href="#ascrv">ascrv</a>                  compute storage-coefficient routing curve
└── <a href="#time_control">time_control</a>
</pre>

---

## swift_output

Writes SWIFT-format output files after the simulation completes.

**Called from:** [`main`](#main)

<pre>
swift_output
├── [loop output files]  <a href="#copy_file">copy_file</a>(src, "SWIFT/" // file_name)
└── <a href="#hyd_convert_mass_to_conc">hyd_convert_mass_to_conc</a>    convert hydrograph mass fluxes to concentrations
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
├── <a href="#wallo_canal">wallo_canal</a>            route water through irrigation canal
├── <a href="#wallo_transfer">wallo_transfer</a>         transfer withdrawn water to destination object
├── <a href="#wallo_treatment">wallo_treatment</a>        apply water treatment processes
├── <a href="#wallo_use">wallo_use</a>              apply water to HRU/crop use
├── [if num_salts > 0]  <a href="#salt_irrig">salt_irrig</a>   add salt load to irrigation water
├── [if num_cs > 0]     <a href="#cs_irrig">cs_irrig</a>     add constituent load to irrigation water
└── [if src == reservoir]  <a href="#res_control">res_control</a>  update reservoir state after withdrawal
</pre>

---

## wallo_demand

Determines water demand for each use object by evaluating conditions and actions.

**Called from:** [`wallo_control`](#wallo_control)

<pre>
wallo_demand
├── <a href="#conditions">conditions</a>    evaluate demand trigger <a href="#conditions">conditions</a>
└── <a href="#actions">actions</a>       set demand amounts based on triggered <a href="#actions">actions</a>
</pre>

---

## wallo_withdraw

Withdraws water from the allocated source (stream, reservoir, or groundwater).

**Called from:** [`wallo_control`](#wallo_control)

<pre>
wallo_withdraw
└── [if gwflow source]  <a href="#gwflow_pump_allo">gwflow_pump_allo</a>   extract water from gwflow grid cell
</pre>

---

## wallo_treatment

Applies water treatment processes to withdrawn water before use.

**Called from:** [`wallo_control`](#wallo_control)

<pre>
wallo_treatment
├── <a href="#hyd_convert_conc_to_mass">hyd_convert_conc_to_mass</a>   convert concentrations to mass in hydrograph
├── <a href="#hyd_min">hyd_min</a>                    apply minimum-flow constraint
└── <a href="#hydcsout_conc_mass">hydcsout_conc_mass</a>         compute constituent mass from outflow concentration
</pre>

---

## wallo_use

Applies allocated water to the HRU or crop use object.

**Called from:** [`wallo_control`](#wallo_control)

<pre>
wallo_use
├── <a href="#hyd_convert_conc_to_mass">hyd_convert_conc_to_mass</a>   convert concentrations to mass in hydrograph
└── <a href="#hydcsout_conc_mass">hydcsout_conc_mass</a>         compute constituent mass from outflow concentration
</pre>


---

## cli_staread

Reads weather station configuration and maps each station to its gage files (WGN, precipitation,
temperature, solar radiation, humidity, wind, PET, atmospheric deposition).

**Called from:** [`proc_read`](#proc_read)

<pre>
cli_staread
└── [loop weather stations, per gage type if db_mx%*files > 0]
    └── <a href="#search">search</a>    look up gage name in array and return its index
</pre>

Parsing source...
428 missing leaf sections to generate
  0/428...
  50/428...
  100/428...
  150/428...
  200/428...
  250/428...
## cli_lapse

this subroutine adjusts precip and temperature for elevation

**Called from:** [`main`](#main), [`res_control`](#res_control), [`sd_channel_control3`](#sd_channel_control3)

Source: `cli_lapse.f90`

<pre>
cli_lapse
│
│  !! set precip and temp lapse for each object
│
├── [loop iob = 1,]
│   │  iwst = ob(iob)%wst
│   
│   │  !! adjust precip and temperature for elevation using lapse rates
│   
│   ├── [if wst(iwst)%wco_c%pgage == "sim"]
│      │  iwgn = wst(iwst)%wco%wgn
│      │  ob(iob)%plaps = bsn_prm%plaps * (ob(iob)%elev - wgn(iwgn)%elev) / 1000.
│      │  igage = wst(iwst)%wco%pgage
│      │  ob(iob)%plaps = bsn_prm%plaps * (ob(iob)%elev - pcp(igage)%elev) / 1000.
│   
│   ├── [if wst(iwst)%wco_c%tgage == "sim"]
│      │  iwgn = wst(iwst)%wco%wgn
│      │  ob(iob)%tlaps = bsn_prm%tlaps * (wgn(iwgn)%elev - ob(iob)%elev) / 1000.
│      │  igage = wst(iwst)%wco%tgage
│      │  ob(iob)%tlaps = bsn_prm%tlaps * (tmp(igage)%elev - ob(iob)%elev) / 1000.
│
▼
</pre>


---

## object_read_output

Reads `object_output` input data from file.

**Called from:** [`main`](#main)

Source: `object_read_output.f90`


---

## om_water_init

**Called from:** [`main`](#main)

Source: `om_water_init.f90`

<pre>
om_water_init
│  inquire (file=in_init%om_water, exist=i_exist)
│
├── [if .not. i_exist .or. in_init%om_water == "null"]
│   │  open (105,file=in_init%om_water)
│   │  read (105,*,iostat=eof) titldum
│   │  read (105,*,iostat=eof) header
│   
│   ├── [do while (eof == 0)]
│      │  read (105,*,iostat=eof) titldum
│      │  imax = imax + 1
│   │  db_mx%om_water_init = imax
│   │  read (105,*,iostat=eof) titldum
│   │  read (105,*,iostat=eof) header
│   
│   ├── [loop ichi = 1,]
│      │  read (105,*,iostat=eof) titldum
│
▼
</pre>


---

## pest_cha_res_read

Reads `pest_cha_res` input data from file.

**Called from:** [`main`](#main)

Source: `pest_cha_res_read.f90`


---

## path_cha_res_read

Reads `path_cha_res` input data from file.

**Called from:** [`main`](#main)

Source: `path_cha_res_read.f90`


---

## salt_cha_read

Reads `salt_cha` input data from file.

**Called from:** [`main`](#main)

Source: `salt_cha_read.f90`


---

## cs_cha_read

Reads `cs_cha` input data from file.

**Called from:** [`main`](#main)

Source: `cs_cha_read.f90`


---

## dtbl_lum_read

Reads `dtbl_lum` input data from file.

**Called from:** [`main`](#main)

Source: `dtbl_lum_read.f90`


---

## proc_cond

**Called from:** [`main`](#main)

Source: `proc_cond.f90`

<pre>
proc_cond
│
│  !! set cross walk for auto management operations
│
├── [loop ihru = 1,]
│   │  isched = hru(ihru)%mgt_ops
│   
│   ├── [if sched(isched)%num_autos > 0]
│      │  sched(isched)%irr = 1
│      
│      │  !! crosswalk with conditional.ctl
│      
│      ├── [loop iauto = 1,]
│         
│         ├── [loop ictl = 1,]
│         
│         ├── [if sched(isched)%auto_name(iauto) == dtbl_lum(ict]
│         │  sched(isched)%num_db(iauto) = ictl
│
▼
</pre>


---

## res_read_weir

this subroutine reads data from the lake water quality input file (.lwq).

**Called from:** [`main`](#main)

Source: `res_read_weir.f90`


---

## dtbl_res_read

Reads `dtbl_res` input data from file.

**Called from:** [`main`](#main)

Source: `dtbl_res_read.f90`


---

## dtbl_scen_read

Reads `dtbl_scen` input data from file.

**Called from:** [`main`](#main)

Source: `dtbl_scen_read.f90`


---

## cal_cond_read

this function computes new parameter value based on

**Called from:** [`main`](#main)

Source: `cal_cond_read.f90`


---

## manure_allocation_read

Reads `manure_allocation` input data from file.

**Called from:** [`main`](#main)

Source: `manure_allocation_read.f90`


---

## dtbl_flocon_read

Reads `dtbl_flocon` input data from file.

**Called from:** [`main`](#main)

Source: `dtbl_flocon_read.f90`


---

## om_treat_read

Reads `om_treat` input data from file.

**Called from:** [`main`](#main)

Source: `om_treat_read.f90`


---

## om_use_read

Reads `om_use` input data from file.

**Called from:** [`main`](#main)

Source: `om_use_read.f90`


---

## om_osrc_read

Reads `om_osrc` input data from file.

**Called from:** [`main`](#main)

Source: `om_osrc_read.f90`


---

## water_treatment_read

Reads `water_treatment` input data from file.

**Called from:** [`main`](#main)

Source: `water_treatment_read.f90`


---

## water_use_read

Reads `water_use` input data from file.

**Called from:** [`main`](#main)

Source: `water_use_read.f90`


---

## water_tower_read

Reads `water_tower` input data from file.

**Called from:** [`main`](#main)

Source: `water_tower_read.f90`


---

## water_pipe_read

Reads `water_pipe` input data from file.

**Called from:** [`main`](#main)

Source: `water_pipe_read.f90`


---

## water_canal_read

Reads `water_canal` input data from file.

**Called from:** [`main`](#main)

Source: `water_canal_read.f90`


---

## water_allocation_read

Reads `water_allocation` input data from file.

**Called from:** [`main`](#main)

Source: `water_allocation_read.f90`


---

## hru_dtbl_actions_init

**Called from:** [`main`](#main)

Source: `hru_dtbl_actions_init.f90`

<pre>
hru_dtbl_actions_init
│
├── [loop iihru = 1,]
│   │  ihru = sp_ob1%hru + iihru - 1
│   │  isched = hru(ihru)%mgt_ops
│   │  m_autos = sched(isched)%num_autos
│   
│   │  !! add decision table for manure allocation demand
│   
│   ├── [if hru(ihru)%man_trn_dtbl > 0]
│      │  m_autos = m_autos + 1
│      │  hru(ihru)%man_trn_iauto = m_autos
│   
│   ├── [if m_autos > 0]
│      
│      ├── [loop iauto = 1,]
│         
│         ├── [if iauto /= hru(ihru)%irr_trn_iauto .and. iauto /]
│         │  id = sched(isched)%num_db(iauto)
│      
│      ├── [if iauto == hru(ihru)%man_trn_iauto]
│         
│         │  !! dtbl from water allocation for irrigation demand
│         │  id = hru(ihru)%man_trn_dtbl
│      │  allocate (pcom(ihru)%dtbl(iauto)%num_actions(dtbl_lum(id)%acts), source 
│      │  pcom(ihru)%dtbl(iauto)%num_actions = 1
│      │  allocate (pcom(ihru)%dtbl(iauto)%days_act(dtbl_lum(id)%acts), source = 0
│      
│      │  !! set variables for future fertilizer operations
│      
│      ├── [loop iac = 1,]
│      
│      ├── [if num_fut > 0]
│      
│      ├── [loop iac = 1,]
│         
│         ├── [if dtbl_lum(id)%act(iac)%typ == "fert_future"]
│         
│         ├── [loop idb = 1,]
│
▼
</pre>


---

## wet_read_hyd

Reads `wet_hyd` input data from file.

**Called from:** [`main`](#main)

Source: `wet_read_hyd.f90`


---

## wet_read

Reads `wet` input data from file.

**Called from:** [`main`](#main)

Source: `wet_read.f90`


---

## wet_read_salt_cs

**Called from:** [`main`](#main)

Source: `wet_read_salt_cs.f90`

<pre>
wet_read_salt_cs
│  inquire (file="wetland.wet_cs",exist=i_exist)
│
├── [if i_exist]
│   │  open(105,file="wetland.wet_cs")
│   
│   ├── [loop iwet = 1,]
│      │  read (105,*,iostat=eof) i
│      │  read (105,*,iostat=eof) k, wet_dat_c_cs(iwet)
│      
│      ├── [loop isalt = 1,]
│         
│         ├── [if res_salt_data(isalt)%name == wet_dat_c_cs(iwet]
│         │  wet_dat(iwet)%salt = isalt
│   
│   ├── [loop ics = 1,]
│      
│      ├── [if res_cs_data(ics)%name == wet_dat_c_cs(iwet)%cs]
│         │  wet_dat(iwet)%cs = ics
│  close(105)
│
▼
</pre>


---

## wet_fp_init

this subroutine routes computes the initial storage in flood plain wetlands

**Called from:** [`main`](#main)

Source: `wet_fp_init.f90`

<pre>
wet_fp_init
│
│  !! total wetland flood plain volume at start of simulation
│
├── [loop jrch = 1,]
│   │  wet_stor(jrch) = hz
│   
│   ├── [if sd_ch(jrch)%fp%hru_tot > 0]
│      
│      ├── [loop ihru = 1,]
│         │  wet_stor(jrch) = wet_stor(jrch) + wet(ihru)
│
▼
</pre>


---

## dr_ru

**Called from:** [`main`](#main)

Source: `dr_ru.f90`

<pre>
dr_ru
│
├── [loop iru = 1,]
│   
│   ├── [loop ii = 1,]
│      │  ielem = ru_def(iru)%num(ii)
│      │  ihru = ru_elem(ielem)%obtypno
│      
│      ├── [if ru_elem(ielem)%dr_name == "calc" .or. ru_elem(]
│         
│         ├── [select case (ru_elem(ielem)%obtyp)]
│         │  rto = tconc(ihru) / ru_tc(iru)
│         │  rto = (hlt_db(ihru)%tc / 3600.) / ru_tc(iru)
│      │  rto = amin1(1.0, rto ** .5)
│      │  ru_elem(ielem)%dr = rto .add. hz
│      │  ru_elem(ielem)%dr%flo = 1.
│   
│   ├── [if ru_elem(ielem)%dr_name == "full" .or. ru_elem(]
│      │  ru_elem(ielem)%dr = 1. .add. hz
│
▼
</pre>


---

## hyd_connect_out

Writes `hyd_connect` output to file.

**Called from:** [`main`](#main)

Source: `hyd_connect_out.f90`


---

## open_output_file

**Called from:** [`co2_read`](#co2_read), [`header_aquifer`](#header_aquifer), [`header_channel`](#header_channel), [`header_const`](#header_const), [`header_hyd`](#header_hyd), [`header_lu_change`](#header_lu_change), [`header_mgt`](#header_mgt), [`header_path`](#header_path), [`header_pest`](#header_pest), [`header_reservoir`](#header_reservoir), [`header_salt`](#header_salt), [`header_sd_channel`](#header_sd_channel), [`header_water_allocation`](#header_water_allocation), [`header_wetland`](#header_wetland), [`header_write`](#header_write), [`header_yield`](#header_yield), [`output_landscape_init`](#output_landscape_init), [`proc_bsn`](#proc_bsn), [`proc_hru`](#proc_hru)

Source: `output_path_module.f90`

<pre>
open_output_file
│  character(len=*), intent(in) :: filename
│  character(len=512) :: full_path
│
│  !! Get full path
│  full_path = get_output_filename(filename)
│
│  !! Open with or without recl
│
├── [if present(recl_val)]
│   │  open (iunit, file=trim(full_path), recl=recl_val)
│   │  open (iunit, file=trim(full_path))
│
▼
</pre>


---

## basin_read_cc

Reads `basin_cc` input data from file.

**Called from:** [`proc_bsn`](#proc_bsn)

Source: `basin_read_cc.f90`


---

## basin_read_objs

reads in the routing information from the watershed configuration

**Called from:** [`proc_bsn`](#proc_bsn)

Source: `basin_read_objs.f90`


---

## basin_read_prm

Reads `basin_prm` input data from file.

**Called from:** [`proc_bsn`](#proc_bsn)

Source: `basin_read_prm.f90`


---

## basin_prm_default

**Called from:** [`proc_bsn`](#proc_bsn)

Source: `basin_prm_default.f90`

<pre>
basin_prm_default
│
│  !! 0 = conc of nitrate in surface runoff is zero                                           
│
│  !! 0 = conc of sol P in surf runoff is zero                                                
│
│  !! time constant for the reach at bankfull depth
│
│  !! and outflow rate in determining storage on reach
│
│  !! leach from bottom layer
│  bsn_prm%petco_pmpt = (100. + bsn_prm%petco_pmpt) / 100.
│
│  !! set additional parameters
│  uptake%water_dis = 10.0
│  uptake%water_norm = 1. - exp(-uptake%water_dis)
│  uptake%n_norm = 1. - exp(-bsn_prm%n_updis)
│
▼
</pre>


---

## basin_print_codes_read

Reads `basin_print_codes` input data from file.

**Called from:** [`proc_bsn`](#proc_bsn)

Source: `basin_print_codes_read.f90`


---

## carbon_coef_read

Purpose: Read in variables for calibration purposes.

**Called from:** [`proc_bsn`](#proc_bsn)

Source: `carbon_coef_read.f90`


---

## cli_petmeas

**Called from:** [`proc_date_time`](#proc_date_time)

Source: `cli_petmeas.f90`

<pre>
cli_petmeas
│
│  !! read all measured daily pet data
│  inquire (file=in_cli%pet_cli, exist=i_exist)
│
├── [if .not. i_exist .or. in_cli%pet_cli == "null"]
│   │  open (107,file=in_cli%pet_cli)
│   │  read (107,*,iostat=eof) titldum
│   │  read (107,*,iostat=eof) header
│   
│   ├── [do while (eof == 0)]
│      │  read (107,*,iostat=eof) titldum
│      │  imax = imax + 1
│   │  read (107,*,iostat=eof) titldum
│   │  read (107,*,iostat=eof) header
│   
│   ├── [loop i = 1,]
│      │  read (107,*, iostat=eof) petm_n(i)
│   │  read (107,*,iostat=eof) titldum
│   
│   ├── [loop i = 1,]
│      
│      │  !! weather path code
│      
│      ├── [if in_path_pet%peti == "null"]
│
▼
</pre>


---

## cli_pmeas

**Called from:** [`proc_date_time`](#proc_date_time)

Source: `cli_pmeas.f90`

<pre>
cli_pmeas
│
│  !! read all measured daily precipitation data
│  inquire (file=in_cli%pcp_cli, exist=i_exist)
│
├── [if .not. i_exist .or. in_cli%pcp_cli == "null"]
│   │  open (107,file=in_cli%pcp_cli)
│   │  read (107,*,iostat=eof) titldum
│   │  read (107,*,iostat=eof) header
│   
│   ├── [do while (eof == 0)]
│      │  read (107,*,iostat = eof) titldum
│      │  imax = imax + 1
│   │  read (107,*,iostat=eof) titldum
│   │  read (107,*,iostat=eof) header
│   
│   ├── [loop i = 1,]
│      │  read (107,*,iostat = eof) pcp_n(i)
│   │  read (107,*,iostat=eof) titldum
│   
│   ├── [loop i = 1,]
│      
│      ├── [if in_path_pcp%pcp == "null"]
│
▼
</pre>


---

## cli_smeas

**Called from:** [`proc_date_time`](#proc_date_time)

Source: `cli_smeas.f90`

<pre>
cli_smeas
│
│  !! read all measured daily solar radiation data
│  inquire (file=in_cli%slr_cli, exist=i_exist)
│
├── [if .not. i_exist .or. in_cli%slr_cli == "null"]
│   │  open (107,file=in_cli%slr_cli)
│   │  read (107,*,iostat=eof) titldum
│   │  read (107,*,iostat=eof) header
│   
│   ├── [do while (eof == 0)]
│      │  read (107,*,iostat=eof) titldum
│      │  imax = imax + 1
│   │  read (107,*,iostat=eof) titldum
│   │  read (107,*,iostat=eof) header
│   
│   ├── [loop i = 1,]
│      │  read (107,*, iostat=eof) slr_n(i)
│   │  read (107,*,iostat=eof) titldum
│   
│   ├── [loop i = 1,]
│      
│      │  !! weather path code
│      
│      ├── [if in_path_slr%slr == "null"]
│
▼
</pre>


---

## cli_hmeas

**Called from:** [`proc_date_time`](#proc_date_time)

Source: `cli_hmeas.f90`

<pre>
cli_hmeas
│
│  !! read all measured daily relative humidity data
│  inquire (file=in_cli%hmd_cli, exist=i_exist)
│
├── [if .not. i_exist .or. in_cli%hmd_cli == "null"]
│   │  open (107,file=in_cli%hmd_cli)
│   │  read (107,*,iostat=eof) titldum
│   │  read (107,*,iostat=eof) header
│   
│   ├── [do while (eof == 0)]
│      │  read (107,*,iostat=eof) titldum
│      │  imax = imax + 1
│   │  read (107,*,iostat=eof) titldum
│   │  read (107,*,iostat=eof) header
│   
│   ├── [loop i = 1,]
│      │  read (107,*,iostat = eof) hmd_n(i)
│   │  read (107,*,iostat=eof) titldum
│   
│   ├── [loop i = 1,]
│      
│      │  !! weather path code
│      
│      ├── [if in_path_hmd%hmd == "null"]
│
▼
</pre>


---

## cli_wmeas

**Called from:** [`proc_date_time`](#proc_date_time)

Source: `cli_wmeas.f90`

<pre>
cli_wmeas
│
│  !! read all measured daily wind data
│  inquire (file=in_cli%wnd_cli, exist=i_exist)
│
├── [if .not. i_exist .or. in_cli%wnd_cli == "null"]
│   │  open (107,file=in_cli%wnd_cli)
│   │  read (107,*,iostat=eof) titldum
│   │  read (107,*,iostat=eof) header
│   
│   ├── [do while (eof == 0)]
│      │  read (107,*,iostat=eof) titldum
│      │  imax = imax + 1
│   │  read (107,*,iostat=eof) titldum
│   │  read (107,*,iostat=eof) header
│   
│   ├── [loop i = 1,]
│      │  read (107,*,iostat=eof) wnd_n(i)
│   │  read (107,*,iostat=eof) titldum
│   
│   ├── [loop i = 1,]
│      
│      │  !! weather path code
│      
│      ├── [if in_path_wnd%wnd == "null"]
│
▼
</pre>


---

## plant_parm_read

Reads `plant_parm` input data from file.

**Called from:** [`proc_db`](#proc_db)

Source: `plant_parm_read.f90`


---

## plant_transplant_read

Reads `plant_transplant` input data from file.

**Called from:** [`proc_db`](#proc_db)

Source: `plant_transplant_read.f90`


---

## till_parm_read

Reads `till_parm` input data from file.

**Called from:** [`proc_db`](#proc_db)

Source: `till_parm_read.f90`


---

## pest_parm_read

Reads `pest_parm` input data from file.

**Called from:** [`proc_db`](#proc_db)

Source: `pest_parm_read.f90`


---

## fert_parm_read

Reads `fert_parm` input data from file.

**Called from:** [`proc_db`](#proc_db)

Source: `fert_parm_read.f90`


---

## manure_orgmin_read

Reads `manure_orgmin` input data from file.

**Called from:** [`proc_db`](#proc_db)

Source: `manure_orgmin_read.f90`


---

## manure_db_read

Reads `manure` input data from file.

**Called from:** [`proc_db`](#proc_db)

Source: `manure_db_read.f90`


---

## urban_parm_read

Reads `urban_parm` input data from file.

**Called from:** [`proc_db`](#proc_db)

Source: `urban_parm_read.f90`


---

## path_parm_read

Reads `path_parm` input data from file.

**Called from:** [`proc_db`](#proc_db)

Source: `path_parm_read.f90`


---

## septic_parm_read

Reads `septic_parm` input data from file.

**Called from:** [`proc_db`](#proc_db)

Source: `septic_parm_read.f90`


---

## mgt_read_irrops

Reads `mgt_irrops` input data from file.

**Called from:** [`proc_db`](#proc_db)

Source: `mgt_read_irrops.f90`


---

## mgt_read_chemapp

Reads `mgt_chemapp` input data from file.

**Called from:** [`proc_db`](#proc_db)

Source: `mgt_read_chemapp.f90`


---

## mgt_read_harvops

Reads `mgt_harvops` input data from file.

**Called from:** [`proc_db`](#proc_db)

Source: `mgt_read_harvops.f90`


---

## mgt_read_grazeops

Reads `mgt_grazeops` input data from file.

**Called from:** [`proc_db`](#proc_db)

Source: `mgt_read_grazeops.f90`


---

## mgt_read_sweepops

Reads `mgt_sweepops` input data from file.

**Called from:** [`proc_db`](#proc_db)

Source: `mgt_read_sweepops.f90`


---

## mgt_read_fireops

Reads `mgt_fireops` input data from file.

**Called from:** [`proc_db`](#proc_db)

Source: `mgt_read_fireops.f90`


---

## mgt_read_puddle

Reads `mgt_puddle` input data from file.

**Called from:** [`proc_db`](#proc_db)

Source: `mgt_read_puddle.f90`


---

## sdr_read

Reads `sdr` input data from file.

**Called from:** [`proc_db`](#proc_db)

Source: `sdr_read.f90`


---

## sep_read

Reads `sep` input data from file.

**Called from:** [`proc_db`](#proc_db)

Source: `sep_read.f90`


---

## scen_read_grwway

Reads `scen_grwway` input data from file.

**Called from:** [`proc_db`](#proc_db)

Source: `scen_read_grwway.f90`


---

## scen_read_filtstrip

Reads `scen_filtstrip` input data from file.

**Called from:** [`proc_db`](#proc_db)

Source: `scen_read_filtstrip.f90`


---

## scen_read_bmpuser

Reads `scen_bmpuser` input data from file.

**Called from:** [`proc_db`](#proc_db)

Source: `scen_read_bmpuser.f90`


---

## sat_buff_read

Reads `sat_buff` input data from file.

**Called from:** [`proc_db`](#proc_db)

Source: `sat_buff_read.f90`


---

## readpcom

**Called from:** [`proc_db`](#proc_db)

Source: `readpcom.f90`

<pre>
readpcom
│
│  !! Open plant community data file
│  inquire (file=in_init%plant, exist=i_exist)
│
├── [if .not. i_exist .or. in_init%plant == "null"]
│   │  db_mx%plantcom = mcom + 1
│   │  open (113,file=in_init%plant)
│   │  read (113,*,iostat=eof) titldum
│   │  read (113,*,iostat=eof) header
│   
│   ├── [do while (eof == 0)]
│      │  read (113,*,iostat=eof) name, numb
│      
│      ├── [loop ii = 1,]
│         │  read (113,*,iostat=eof) name
│      │  imax = imax + 1
│   │  read (113,*,iostat=eof) titldum
│   │  read (113,*,iostat=eof) header
│   
│   ├── [loop icom = 1,]
│      
│      ├── [loop iplt = 1,]
│         
│         ├── [loop ipldb = 1,]
│
▼
</pre>


---

## cntbl_read

Reads `cntbl` input data from file.

**Called from:** [`proc_db`](#proc_db)

Source: `cntbl_read.f90`


---

## cons_prac_read

Reads `cons_prac` input data from file.

**Called from:** [`proc_db`](#proc_db)

Source: `cons_prac_read.f90`


---

## overland_n_read

Reads `overland_n` input data from file.

**Called from:** [`proc_db`](#proc_db)

Source: `overland_n_read.f90`


---

## landuse_read

Reads `landuse` input data from file.

**Called from:** [`proc_db`](#proc_db)

Source: `landuse_read.f90`


---

## ch_read_temp

**Called from:** [`proc_read`](#proc_read)

Source: `ch_read_temp.f90`

<pre>
ch_read_temp
│  inquire (file=in_cha%temp, exist=i_exist)
│
├── [if .not. i_exist .or. in_cha%temp == "null"]
│   │  open (105,file=in_cha%temp)
│   │  read (105,*,iostat=eof) titldum
│   │  read (105,*,iostat=eof) header
│   
│   ├── [do while (eof == 0)]
│      │  read (105,*,iostat=eof) titldum
│      │  imax = imax + 1
│   │  db_mx%w_temp = imax
│   │  read (105,*,iostat=eof) titldum
│   │  read (105,*,iostat=eof) header
│   
│   ├── [loop ich_temp = 1,]
│      │  read (105,*,iostat=eof) titldum
│
▼
</pre>


---

## cli_read_atmodep

**Called from:** [`proc_read`](#proc_read)

Source: `cli_read_atmodep.f90`

<pre>
cli_read_atmodep
│  inquire (file=in_cli%atmo_cli,exist=i_exist)
│
├── [if .not. i_exist .or. in_cli%atmo_cli == "null"]
│   
│   │  !! no filename
│   │  db_mx%atmodep = 0
│   │  open (127,file = in_cli%atmo_cli)
│   │  read (127,*,iostat=eof) titldum
│   │  read (127,*,iostat=eof) header
│   │  read (127,*,iostat=eof) atmodep_cont%num_sta, atmodep_cont%timestep, atm
│   │  iyrc_atmo = atmodep_cont%yr_init
│   │  imo_atmo = atmodep_cont%mo_init
│   
│   ├── [if atmodep_cont%timestep == "yr"]
│      
│      ├── [loop iyr = 1,]
│         
│         ├── [if iyrc_atmo == time%yrc_start]
│         │  atmodep_cont%ts = iyr
│         │  atmodep_cont%first = 0
│
├── [if atmodep_cont%timestep == "mo"]
│   
│   ├── [loop imo = 1,]
│      
│      ├── [if iyrc_atmo == time%yrc_start .and. imo_atmo == ]
│      
│      ├── [if imo_atmo > 12]
│
▼
</pre>


---

## constit_db_read

Reads `constit` input data from file.

**Called from:** [`proc_read`](#proc_read)

Source: `constit_db_read.f90`


---

## pest_metabolite_read

Reads `pest_metabolite` input data from file.

**Called from:** [`proc_read`](#proc_read)

Source: `pest_metabolite_read.f90`


---

## soil_plant_init

**Called from:** [`proc_read`](#proc_read)

Source: `soil_plant_init.f90`

<pre>
soil_plant_init
│  inquire (file=in_init%soil_plant_ini, exist=i_exist)
│
├── [if i_exist .or. in_init%soil_plant_ini /= "null"]
│   │  open (107,file=in_init%soil_plant_ini)
│   │  read (107,*,iostat=eof) titldum
│   │  read (107,*,iostat=eof) header
│   
│   ├── [do while (eof == 0)]
│      │  read (107,*,iostat=eof) titldum
│      │  imax = imax + 1
│   │  db_mx%sol_plt_ini = imax
│   │  read (107,*,iostat=eof) titldum
│   │  read (107,*,iostat=eof) header
│   
│   ├── [loop ii = 1,]
│      
│      ├── [if bsn_cc%nam1 == 0]
│         │  read (107,*,iostat=eof) sol_plt_ini(ii)%name, sol_plt_ini(ii)%sw_frac, s
│
▼
</pre>


---

## solt_db_read

Reads `solt` input data from file.

**Called from:** [`proc_read`](#proc_read)

Source: `solt_db_read.f90`


---

## pest_hru_aqu_read

Reads `pest_hru_aqu` input data from file.

**Called from:** [`proc_read`](#proc_read)

Source: `pest_hru_aqu_read.f90`


---

## path_hru_aqu_read

Reads `path_hru_aqu` input data from file.

**Called from:** [`proc_read`](#proc_read)

Source: `path_hru_aqu_read.f90`


---

## hmet_hru_aqu_read

Reads `hmet_hru_aqu` input data from file.

**Called from:** [`proc_read`](#proc_read)

Source: `hmet_hru_aqu_read.f90`


---

## salt_hru_read

Reads `salt_hru` input data from file.

**Called from:** [`proc_read`](#proc_read)

Source: `salt_hru_read.f90`


---

## salt_aqu_read

Reads `salt_aqu` input data from file.

**Called from:** [`proc_read`](#proc_read)

Source: `salt_aqu_read.f90`


---

## salt_irr_read

Reads `salt_irr` input data from file.

**Called from:** [`proc_read`](#proc_read)

Source: `salt_irr_read.f90`


---

## salt_plant_read

Reads `salt_plant` input data from file.

**Called from:** [`proc_read`](#proc_read)

Source: `salt_plant_read.f90`


---

## cli_read_atmodep_salt

read in wet and dry deposition values for salt ions

**Called from:** [`proc_read`](#proc_read)

Source: `cli_read_atmodep_salt.f90`

<pre>
cli_read_atmodep_salt
│
├── [if cs_db%num_salts > 0]
│   │  inquire (file='salt_atmo.cli',exist=i_exist)
│   
│   ├── [if i_exist]
│      │  salt_atmo = "y"
│      │  open(5050,file='salt_atmo.cli')
│      
│      ├── [loop iadep = 1,]
│         
│         ├── [if atmodep_cont%timestep == "aa"]
│         
│         ├── [loop isalt=1,cs_db%num_salts]
│      
│      ├── [loop isalt=1,cs_db%num_salts]
│   
│   ├── [if atmodep_cont%timestep == "mo"]
│      
│      ├── [loop isalt=1,cs_db%num_salts]
│         │  allocate (atmodep_salt(iadep)%salt(isalt)%rfmo(atmodep_cont%num), source
│         │  read(5050,*) salt_ion,(atmodep_salt(iadep)%salt(isalt)%rfmo(imo),imo=1,a
│      
│      ├── [loop isalt=1,cs_db%num_salts]
│         │  allocate (atmodep_salt(iadep)%salt(isalt)%drymo(atmodep_cont%num), sourc
│         │  read(5050,*) salt_ion,(atmodep_salt(iadep)%salt(isalt)%drymo(imo),imo=1,
│   
│   ├── [if atmodep_cont%timestep == "yr"]
│      
│      ├── [loop isalt=1,cs_db%num_salts]
│         │  allocate (atmodep_salt(iadep)%salt(isalt)%rfyr(atmodep_cont%num), source
│         │  read(5050,*) salt_ion,(atmodep_salt(iadep)%salt(isalt)%rfyr(iyr),iyr=1,a
│
▼
</pre>


---

## salt_roadsalt_read

read in road salt loadings (kg/ha)

**Called from:** [`proc_read`](#proc_read)

Source: `salt_roadsalt_read.f90`


---

## salt_uptake_read

read in specified salt uptake mass (kg/ha)

**Called from:** [`proc_read`](#proc_read)

Source: `salt_uptake_read.f90`


---

## salt_urban_read

Reads `salt_urban` input data from file.

**Called from:** [`proc_read`](#proc_read)

Source: `salt_urban_read.f90`


---

## salt_fert_read

this subroutine reads salt ion fertilizer loading (kg/ha) for various fertilizer types

**Called from:** [`proc_read`](#proc_read)

Source: `salt_fert_read.f90`


---

## cs_hru_read

Reads `cs_hru` input data from file.

**Called from:** [`proc_read`](#proc_read)

Source: `cs_hru_read.f90`


---

## cs_aqu_read

Reads `cs_aqu` input data from file.

**Called from:** [`proc_read`](#proc_read)

Source: `cs_aqu_read.f90`


---

## cli_read_atmodep_cs

read in wet and dry deposition values for constituents

**Called from:** [`proc_read`](#proc_read)

Source: `cli_read_atmodep_cs.f90`

<pre>
cli_read_atmodep_cs
│
├── [if cs_db%num_cs > 0]
│   │  inquire (file='cs_atmo.cli',exist=i_exist)
│   
│   ├── [if i_exist]
│      │  cs_atmo = "y"
│      │  open(5050,file='cs_atmo.cli')
│      
│      ├── [loop iadep = 1,]
│         
│         ├── [if atmodep_cont%timestep == "aa"]
│         
│         ├── [loop ics=1,cs_db%num_cs]
│      
│      ├── [loop ics=1,cs_db%num_cs]
│   
│   ├── [if atmodep_cont%timestep == "mo"]
│      
│      ├── [loop ics=1,cs_db%num_cs]
│         │  allocate (atmodep_cs(iadep)%cs(ics)%rfmo(atmodep_cont%num), source = 0.)
│      
│      ├── [loop ics=1,cs_db%num_cs]
│         │  read(5050,*) (atmodep_cs(iadep)%cs(ics)%rfmo(imo),imo=1,atmodep_cont%num
│      
│      ├── [loop ics=1,cs_db%num_cs]
│         │  allocate (atmodep_cs(iadep)%cs(ics)%drymo(atmodep_cont%num), source = 0.
│      
│      ├── [loop ics=1,cs_db%num_cs]
│         │  read(5050,*) (atmodep_cs(iadep)%cs(ics)%drymo(imo),imo=1,atmodep_cont%nu
│   
│   ├── [if atmodep_cont%timestep == "yr"]
│      
│      ├── [loop ics=1,cs_db%num_cs]
│         │  allocate (atmodep_cs(iadep)%cs(ics)%rfyr(atmodep_cont%num), source = 0.)
│
▼
</pre>


---

## cs_irr_read

Reads `cs_irr` input data from file.

**Called from:** [`proc_read`](#proc_read)

Source: `cs_irr_read.f90`


---

## cs_plant_read

Reads `cs_plant` input data from file.

**Called from:** [`proc_read`](#proc_read)

Source: `cs_plant_read.f90`


---

## cs_uptake_read

Reads `cs_uptake` input data from file.

**Called from:** [`proc_read`](#proc_read)

Source: `cs_uptake_read.f90`


---

## cs_reactions_read

Reads `cs_reactions` input data from file.

**Called from:** [`proc_read`](#proc_read)

Source: `cs_reactions_read.f90`


---

## cs_urban_read

Reads `cs_urban` input data from file.

**Called from:** [`proc_read`](#proc_read)

Source: `cs_urban_read.f90`


---

## cs_fert_read

this subroutine reads constituent fertilizer loading (kg/ha) for various fertilizer types

**Called from:** [`proc_read`](#proc_read)

Source: `cs_fert_read.f90`


---

## topo_read

Reads `topo` input data from file.

**Called from:** [`proc_read`](#proc_read)

Source: `topo_read.f90`


---

## field_read

Reads `field` input data from file.

**Called from:** [`proc_read`](#proc_read)

Source: `field_read.f90`


---

## hydrol_read

Reads `hydrol` input data from file.

**Called from:** [`proc_read`](#proc_read)

Source: `hydrol_read.f90`


---

## shade_factor_read

subroutine to read the shade factor input - todo: needs to be called in main (not sure if correct)

**Called from:** [`proc_read`](#proc_read)

Source: `shade_factor_read.f90`


---

## snowdb_read

Reads `snowdb` input data from file.

**Called from:** [`proc_read`](#proc_read)

Source: `snowdb_read.f90`


---

## soil_db_read

Reads `soil` input data from file.

**Called from:** [`proc_read`](#proc_read)

Source: `soil_db_read.f90`


---

## soil_lte_db_read

Reads `soil_lte` input data from file.

**Called from:** [`proc_read`](#proc_read)

Source: `soil_lte_db_read.f90`


---

## ru_read

Reads `ru` input data from file.

**Called from:** [`hyd_connect`](#hyd_connect)

Source: `ru_read.f90`


---

## overbank_read

Reads `overbank` input data from file.

**Called from:** [`hyd_connect`](#hyd_connect), [`proc_cha`](#proc_cha)

Source: `overbank_read.f90`


---

## gwflow_chan_read

Reads `gwflow_chan` input data from file.

**Called from:** [`hyd_connect`](#hyd_connect)

Source: `gwflow_chan_read.f90`


---

## recall_read

Reads `recall` input data from file.

**Called from:** [`recalldb_read`](#recalldb_read)

Source: `recall_read.f90`


---

## exco_read_om

**Called from:** [`exco_db_read`](#exco_db_read)

Source: `exco_read_om.f90`

<pre>
exco_read_om
│  inquire (file=in_exco%om, exist=i_exist)
│
├── [if i_exist .or. in_exco%om /= "null"]
│   │  open (107,file=in_exco%om)
│   │  read (107,*,iostat=eof) titldum
│   │  read (107,*,iostat=eof) header
│   │  read (107,*,iostat=eof) header
│   
│   ├── [do while (eof == 0)]
│      │  read (107,*,iostat=eof) titldum
│      │  imax = imax + 1
│   │  db_mx%exco_om = imax
│   │  allocate (exco_om_num(imax), source = 0)
│   │  read (107,*,iostat=eof) titldum
│   
│   ├── [loop ii = 1,]
│
▼
</pre>


---

## exco_read_pest

**Called from:** [`exco_db_read`](#exco_db_read)

Source: `exco_read_pest.f90`

<pre>
exco_read_pest
│  inquire (file=in_exco%pest, exist=i_exist)
│
├── [if i_exist .or. in_exco%pest /= "null"]
│   │  open (107,file=in_exco%pest)
│   │  read (107,*,iostat=eof) titldum
│   │  read (107,*,iostat=eof) header
│   
│   ├── [do while (eof == 0)]
│      │  read (107,*,iostat=eof) titldum
│      │  imax = imax + 1
│   │  db_mx%exco_pest = imax
│   
│   ├── [loop iexco_pest = 1,]
│      │  allocate (exco_pest(iexco_pest)%pest(cs_db%num_pests), source = 0.)
│   │  allocate (exco_pest_num(imax), source = 0)
│   │  read (107,*,iostat=eof) titldum
│   
│   ├── [loop ii = 1,]
│
├── [loop iexco = 1,]
│
▼
</pre>


---

## exco_read_path

**Called from:** [`exco_db_read`](#exco_db_read)

Source: `exco_read_path.f90`

<pre>
exco_read_path
│  inquire (file=in_exco%path, exist=i_exist)
│
├── [if i_exist .or. in_exco%path /= "null"]
│   │  open (107,file=in_exco%path)
│   │  read (107,*,iostat=eof) titldum
│   │  read (107,*,iostat=eof) header
│   
│   ├── [do while (eof == 0)]
│      │  read (107,*,iostat=eof) titldum
│      │  imax = imax + 1
│   │  db_mx%exco_path = imax
│   
│   ├── [loop iexco_path = 1,]
│      │  allocate (exco_path(iexco_path)%path(cs_db%num_paths), source = 0.)
│   │  allocate (exco_path_num(imax), source = 0)
│   │  read (107,*,iostat=eof) titldum
│   
│   ├── [loop ii = 1,]
│
├── [loop iexco = 1,]
│
▼
</pre>


---

## exco_read_hmet

**Called from:** [`exco_db_read`](#exco_db_read)

Source: `exco_read_hmet.f90`

<pre>
exco_read_hmet
│  inquire (file=in_exco%hmet, exist=i_exist)
│
├── [if i_exist .or. in_exco%hmet /= "null"]
│   │  open (107,file=in_exco%hmet)
│   │  read (107,*,iostat=eof) titldum
│   │  read (107,*,iostat=eof) header
│   
│   ├── [do while (eof == 0)]
│      │  read (107,*,iostat=eof) titldum
│      │  imax = imax + 1
│   │  db_mx%exco_hmet = imax
│   
│   ├── [loop iexco_hmet = 1,]
│      │  allocate (exco_hmet(iexco_hmet)%hmet(cs_db%num_metals), source = 0.)
│   │  allocate (exco_hmet_num(imax), source = 0)
│   │  read (107,*,iostat=eof) titldum
│   
│   ├── [loop ii = 1,]
│
├── [loop iexco = 1,]
│
▼
</pre>


---

## exco_read_salt

**Called from:** [`exco_db_read`](#exco_db_read)

Source: `exco_read_salt.f90`

<pre>
exco_read_salt
│  inquire (file=in_exco%salt, exist=i_exist)
│
├── [if i_exist .or. in_exco%salt /= "null"]
│   │  open (107,file=in_exco%salt)
│   │  read (107,*,iostat=eof) titldum
│   │  read (107,*,iostat=eof) header
│   
│   ├── [do while (eof == 0)]
│      │  read (107,*,iostat=eof) titldum
│      │  imax = imax + 1
│   │  db_mx%exco_salt = imax
│   
│   ├── [loop iexco_salt = 1,]
│      │  allocate (exco_salt(iexco_salt)%salt(cs_db%num_salts), source = 0.)
│   │  allocate (exco_salt_num(imax), source = 0)
│   │  read (107,*,iostat=eof) titldum
│   
│   ├── [loop ii = 1,]
│
├── [loop iexco = 1,]
│
▼
</pre>


---

## dr_read_om

**Called from:** [`dr_db_read`](#dr_db_read)

Source: `dr_read_om.f90`

<pre>
dr_read_om
│  inquire (file=in_delr%om, exist=i_exist)
│
├── [if i_exist .or. in_delr%om /= "null"]
│   │  open (107,file=in_delr%om)
│   │  read (107,*,iostat=eof) titldum
│   │  read (107,*,iostat=eof) header
│   
│   ├── [do while (eof == 0)]
│      │  read (107,*,iostat=eof) titldum
│      │  imax = imax + 1
│   │  db_mx%dr_om = imax
│   │  allocate (dr_om_num(0:imax), source = 0)
│   │  read (107,*,iostat=eof) titldum
│   │  read (107,*,iostat=eof) header
│   
│   ├── [loop ii = 1,]
│
├── [loop idr = 1,]
│   
│   ├── [loop idr_om = 1,]
│      
│      ├── [if dr_db(idr)%om_file == dr_om_name(idr_om)]
│
▼
</pre>


---

## dr_read_pest

**Called from:** [`dr_db_read`](#dr_db_read)

Source: `dr_read_pest.f90`

<pre>
dr_read_pest
│  inquire (file=in_delr%pest, exist=i_exist)
│
├── [if i_exist .or. in_delr%pest /= "null"]
│   │  open (107,file=in_delr%pest)
│   │  read (107,*,iostat=eof) titldum
│   │  read (107,*,iostat=eof) header
│   
│   ├── [do while (eof == 0)]
│      │  read (107,*,iostat=eof) titldum
│      │  imax = imax + 1
│   │  db_mx%dr_pest = imax
│   
│   ├── [loop idr_pest = 1,]
│      │  allocate (dr_pest(idr_pest)%pest(cs_db%num_pests), source = 0.)
│   │  allocate (dr_pest_num(imax), source = 0)
│   │  read (107,*,iostat=eof) titldum
│   
│   ├── [loop ii = 1,]
│
├── [loop idr = 1,]
│
▼
</pre>


---

## dr_path_read

Reads `dr_path` input data from file.

**Called from:** [`dr_db_read`](#dr_db_read)

Source: `dr_path_read.f90`


---

## dr_read_hmet

**Called from:** [`dr_db_read`](#dr_db_read)

Source: `dr_read_hmet.f90`

<pre>
dr_read_hmet
│  inquire (file=in_delr%hmet, exist=i_exist)
│
├── [if i_exist .or. in_delr%hmet /= "null"]
│   │  open (107,file=in_delr%hmet)
│   │  read (107,*,iostat=eof) titldum
│   │  read (107,*,iostat=eof) header
│   
│   ├── [do while (eof == 0)]
│      │  read (107,*,iostat=eof) titldum
│      │  imax = imax + 1
│   │  db_mx%dr_hmet = imax
│   
│   ├── [loop idr_hmet = 1,]
│      │  allocate (dr_hmet(idr_hmet)%hmet(cs_db%num_metals), source = 0.)
│   │  allocate (dr_hmet_num(imax), source = 0)
│   │  read (107,*,iostat=eof) titldum
│   
│   ├── [loop ii = 1,]
│
├── [loop idr = 1,]
│
▼
</pre>


---

## dr_read_salt

**Called from:** [`dr_db_read`](#dr_db_read)

Source: `dr_read_salt.f90`

<pre>
dr_read_salt
│  inquire (file=in_delr%salt, exist=i_exist)
│
├── [if i_exist .or. in_delr%salt /= "null"]
│   │  open (107,file=in_delr%salt)
│   │  read (107,*,iostat=eof) titldum
│   │  read (107,*,iostat=eof) header
│   
│   ├── [do while (eof == 0)]
│      │  read (107,*,iostat=eof) titldum
│      │  imax = imax + 1
│   │  db_mx%dr_salt = imax
│   
│   ├── [loop idr_salt = 1,]
│      │  allocate (dr_salt(idr_salt)%salt(cs_db%num_salts), source = 0.)
│   │  allocate (dr_salt_num(imax), source = 0)
│   │  read (107,*,iostat=eof) titldum
│   
│   ├── [loop ii = 1,]
│
├── [loop idr = 1,]
│
▼
</pre>


---

## define_unit_elements

**Called from:** [`aqu2d_read`](#aqu2d_read), [`aqu_read_elements`](#aqu_read_elements), [`cal_parmchg_read`](#cal_parmchg_read), [`ch_read_elements`](#ch_read_elements), [`lsu_read_elements`](#lsu_read_elements), [`pl_read_parms_cal`](#pl_read_parms_cal), [`pl_read_regions_cal`](#pl_read_regions_cal), [`rec_read_elements`](#rec_read_elements), [`res_read_elements`](#res_read_elements), [`ru_read_elements`](#ru_read_elements)

Source: `define_unit_elements.f90`

<pre>
define_unit_elements
│
│  !! save the object number of each defining unit
│
├── [do while (ii <= num_elem)]
│   
│   ├── [if ii == num_elem]
│      
│      ├── [if ii == 1]
│      │  ie1 = elem_cnt(ii-1)
│      │  ie2 = elem_cnt(ii)
│      
│      ├── [if ie2 > 0]
│         │  ielem = ielem + 1
│         │  ie2 = abs(ie2)
│         
│         ├── [loop ie = ie1,]
│         │  ielem = ielem + 1
│   │  ii = ii + 1
│   │  ie1 = elem_cnt(ii)
│   │  ie2 = elem_cnt(ii+1)
│   
│   ├── [if ie2 > 0]
│      │  ielem = ielem + 1
│      │  ii = ii + 1
│      
│      ├── [loop ie = ie1,]
│
├── [do while (ii <= num_elem)]
│
▼
</pre>


---

## hru_allo

**Called from:** [`proc_hru`](#proc_hru)

Source: `hru_allo.f90`

<pre>
hru_allo
│  imax = sp_ob%hru
│
├── [if imax == 0]
│
▼
</pre>


---

## hrudb_init

**Called from:** [`proc_hru`](#proc_hru)

Source: `hrudb_init.f90`

<pre>
hrudb_init
│
│  !! assign database pointers for the hru
│
├── [loop ihru = 1,]
│   │  iob = sp_ob1%hru + ihru - 1
│   │  ihru_db = ob(iob)%props
│   │  hru(ihru)%dbs = hru_db(ihru_db)%dbs
│   │  hru(ihru)%dbsc = hru_db(ihru_db)%dbsc
│   │  hru(ihru)%obj_no = sp_ob1%hru + ihru - 1
│   │  hru(ihru)%area_ha = ob(iob)%area_ha
│   │  hru(ihru)%km = ob(iob)%area_ha / 100.
│   │  hru(ihru)%land_use_mgt_c = hru_db(ihru_db)%dbsc%land_use_mgt
│   │  ilu = hru(ihru)%dbs%land_use_mgt
│   │  hru(ihru)%cal_group = lum(ilu)%cal_group
│
▼
</pre>


---

## hru_output_allo

**Called from:** [`proc_hru`](#proc_hru)

Source: `hru_output_allo.f90`

<pre>
hru_output_allo
│  mhru = sp_ob%hru
│
│  !! dimension hru output variables
│
│  !! new nut carb files
│
│  !! new carbon files
│
│  !! new nut carb files
│
▼
</pre>


---

## carbon_read

Reads `carbon` input data from file.

**Called from:** [`proc_hru`](#proc_hru)

Source: `carbon_read.f90`


---

## hydro_init

This subroutine computes variables related to the watershed hydrology:

**Called from:** [`proc_hru`](#proc_hru)

Source: `hydro_init.f90`

<pre>
hydro_init
│
│  !! SWAT: Ttcoef
│
├── [loop j = 1,]
│   │  iob = hru(j)%obj_no
│   │  iwst = ob(iob)%wst
│   │  iwgn = wst(iwst)%wco%wgn
│   
│   │  !! calculate composite usle value
│   │  rock = Exp(-.053 * soil(j)%phys(1)%rock)
│   │  hru(j)%lumv%usle_mult = rock * soil(j)%ly(1)%usle_k *                   
│   │  tsoil = (wgn(iwgn)%tmpmx(12) + wgn(iwgn)%tmpmx(12)) / 2.
│   
│   │  !! set fraction of field capacity in soil
│   
│   ├── [if bsn_prm%ffcb <= 0.]
│      │  sffc = wgn_pms(iwgn)%pcp_an / (wgn_pms(iwgn)%pcp_an + Exp(9.043 -       
│      │  sffc = bsn_prm%ffcb
│   
│   │  !! set initial soil water and temperature for each layer
│   │  nly = soil(j)%nly
│   │  soil(j)%sw = 0.
│   
│   ├── [loop k = 1,]
│   
│   │  !! set day length threshold for dormancy and initial dormancy
│   
│   ├── [if sdlat > 1.]
│   
│   ├── [loop ipl = 1,]
│      
│      ├── [if pcom(j)%plcur(ipl)%gro == "y" .and. daylength ]
│
▼
</pre>


---

## pesticide_init

this subroutine calls subroutines which read input data for the

**Called from:** [`proc_hru`](#proc_hru)

Source: `pesticide_init.f90`

<pre>
pesticide_init
│
│  !! allocate hru pesticides
│
├── [loop ihru = 1,]
│   │  npmx = cs_db%num_pests
│   
│   ├── [if npmx > 0]
│      │  nly = soil(ihru)%nly
│      │  npl = pcom(ihru)%npl
│      
│      ├── [loop ly = 1,]
│         │  allocate (cs_soil(ihru)%ly(ly)%pest(npmx), source = 0.)
│         │  cs_soil(ihru)%ly(ly)%pest = 0.
│      
│      ├── [loop ipl = 1,]
│         │  allocate (cs_pl(ihru)%pl_in(ipl)%pest(npmx), source = 0.)
│         │  cs_pl(ihru)%pl_in(ipl)%pest = 0.
│         │  allocate (cs_pl(ihru)%pl_on(ipl)%pest(npmx), source = 0.)
│         │  cs_pl(ihru)%pl_on(ipl)%pest = 0.
│         │  allocate (cs_pl(ihru)%pl_up(ipl)%pest(npmx), source = 0.)
│   
│   ├── [loop ipest = 1,]
│      
│      ├── [loop ipl = 1,]
│         
│         ├── [if pcom(ihru)%lai_sum > 1.e-6]
│   
│   ├── [loop ly = 1,]
│
▼
</pre>


---

## pathogen_init

this subroutine calls subroutines which read input data for the

**Called from:** [`proc_hru`](#proc_hru)

Source: `pathogen_init.f90`

<pre>
pathogen_init
│
│  !! SWAT: soil_chem, soil_phys, rteinit, h2omgt_init, hydro_init,
│
├── [loop ihru = 1,]
│   
│   │  !! allocate pathogens
│   │  mpath = cs_db%num_paths
│   
│   ├── [if mpath > 0]
│      
│      │  !! allocate pathogens associated with soil and plant
│      
│      ├── [loop ly = 1,]
│         │  allocate (cs_soil(ihru)%ly(ly)%path(mpath), source = 0.)
│      
│      ├── [loop ipl = 1,]
│         │  allocate (cs_pl(ihru)%pl_in(ipl)%path(mpath), source = 0.)
│         │  allocate (cs_pl(ihru)%pl_on(ipl)%path(mpath), source = 0.)
│         │  allocate (cs_pl(ihru)%pl_up(ipl)%path(mpath), source = 0.)
│   │  isp_ini = hru(ihru)%dbs%soil_plant_init
│   │  ipath_db = sol_plt_ini(isp_ini)%path
│   
│   ├── [if mpath > 0]
│      
│      ├── [loop ipath = 1,]
│         
│         ├── [loop ly = 1,]
│         
│         ├── [if ly == 1]
│         │  cs_soil(ihru)%ly(1)%path(ipath) = path_soil_ini(ipath_db)%soil(ipath)
│         │  cs_soil(ihru)%ly(1)%path(ipath) = 0.
│   │  hpath_bal(ihru)%path(ipath)%plant = path_soil_ini(ipath_db)%plt(ipath)
│
▼
</pre>


---

## salt_hru_init

this subroutine calls subroutines which read input data for the

**Called from:** [`proc_hru`](#proc_hru)

Source: `salt_hru_init.f90`

<pre>
salt_hru_init
│
│  !! allocate hru salts
│  npmx = cs_db%num_salts
│
├── [loop ihru = 1,]
│   
│   ├── [if npmx > 0]
│      
│      ├── [loop ly = 1,]
│         │  allocate (cs_soil(ihru)%ly(ly)%salt(npmx), source = 0.)
│         │  allocate (cs_soil(ihru)%ly(ly)%salt_min(5), source = 0.)
│         │  allocate (cs_soil(ihru)%ly(ly)%saltc(npmx), source = 0.)
│      │  allocate (cs_irr(ihru)%saltc(npmx), source = 0.)
│   │  isp_ini = hru(ihru)%dbs%soil_plant_init
│   │  isalt_db = sol_plt_ini(isp_ini)%salt
│   │  hru_area_m2 = hru(ihru)%area_ha * 10000.
│   
│   ├── [loop isalt=1,npmx]
│      
│      ├── [loop ly = 1,]
│         │  cs_soil(ihru)%ly(ly)%saltc(isalt) = salt_soil_ini(isalt_db)%soil(isalt)
│         │  water_volume = (soil(ihru)%phys(ly)%st/1000.) * hru_area_m2
│   
│   ├── [loop isalt = 1,5]
│      
│      ├── [loop ly = 1,soil(ihru)%nly]
│
▼
</pre>


---

## cs_hru_init

**Called from:** [`proc_hru`](#proc_hru)

Source: `cs_hru_init.f90`

<pre>
cs_hru_init
│
│  !! allocate hru cs
│  npmx = cs_db%num_cs
│
├── [loop ihru = 1,]
│   
│   ├── [if npmx > 0]
│      
│      ├── [loop ly = 1,]
│         │  allocate (cs_soil(ihru)%ly(ly)%cs(npmx), source = 0.)
│         │  allocate (cs_soil(ihru)%ly(ly)%csc(npmx), source = 0.)
│         │  allocate (cs_soil(ihru)%ly(ly)%cs_sorb(npmx), source = 0.)
│         │  allocate (cs_soil(ihru)%ly(ly)%csc_sorb(npmx), source = 0.)
│      │  allocate (cs_irr(ihru)%csc(npmx), source = 0.)
│   │  isp_ini = hru(ihru)%dbs%soil_plant_init
│   │  ics_db = sol_plt_ini(isp_ini)%cs
│   │  hru_area_m2 = hru(ihru)%area_ha * 10000.
│   
│   ├── [loop ics = 1,]
│      
│      ├── [loop ly = 1,]
│         │  cs_soil(ihru)%ly(ly)%csc(ics) = cs_soil_ini(ics_db)%soil(ics)
│
▼
</pre>


---

## rte_read_nut

this subroutine reads data from the lake water quality input file (.lwq).

**Called from:** [`proc_hru`](#proc_hru)

Source: `rte_read_nut.f90`

<pre>
rte_read_nut
│
│  !! apply to all reservoirs in the watershed.
│  inquire (file="nutrients.rte",exist=i_exist)
│
├── [if .not. i_exist]
│   │  open (105,file="nutrients.rte")
│   │  read (105,*,iostat=eof) titldum
│   │  read (105,*,iostat=eof) header
│   
│   ├── [do while (eof == 0)]
│      │  read (105,*,iostat=eof) titldum
│      │  imax = imax + 1
│   │  read (105,*,iostat=eof) titldum
│   │  read (105,*,iostat=eof) header
│   
│   ├── [loop ich = 1,]
│      │  read (105,*,iostat=eof) titldum
│      │  read (105,*,iostat=eof) rte_nut(ich)
│
▼
</pre>


---

## ch_read_init

Reads `ch_init` input data from file.

**Called from:** [`proc_cha`](#proc_cha)

Source: `ch_read_init.f90`


---

## ch_read_init_cs

Reads `ch_init_cs` input data from file.

**Called from:** [`proc_cha`](#proc_cha)

Source: `ch_read_init_cs.f90`


---

## sd_hydsed_read

Reads `sd_hydsed` input data from file.

**Called from:** [`proc_cha`](#proc_cha)

Source: `sd_hydsed_read.f90`


---

## ch_read_hyd

Reads `ch_hyd` input data from file.

**Called from:** [`proc_cha`](#proc_cha)

Source: `ch_read_hyd.f90`


---

## ch_read_sed

this subroutine reads data from the lake water quality input file (.lwq).

**Called from:** [`proc_cha`](#proc_cha)

Source: `ch_read_sed.f90`

<pre>
ch_read_sed
│
│  !! apply to all reservoirs in the watershed.
│  inquire (file=in_cha%sed,exist=i_exist)
│
├── [if .not. i_exist .or. in_cha%sed == "null"]
│   │  open (105,file=in_cha%sed)
│   │  read (105,*,iostat=eof) titldum
│   │  read (105,*,iostat=eof) header
│   
│   ├── [do while (eof == 0)]
│      │  read (105,*,iostat=eof) titldum
│      │  imax = imax + 1
│   │  db_mx%ch_sed = imax
│   │  read (105,*,iostat=eof) titldum
│   │  read (105,*,iostat=eof) header
│   
│   ├── [loop ich = 1,]
│      │  read (105,*,iostat=eof) titldum
│      
│      ├── [if ch_sed(ich)%eqn <= 0]
│
▼
</pre>


---

## ch_read_nut

this subroutine reads data from the lake water quality input file (.lwq).

**Called from:** [`proc_cha`](#proc_cha)

Source: `ch_read_nut.f90`

<pre>
ch_read_nut
│
│  !! apply to all reservoirs in the watershed.
│  inquire (file=in_cha%nut,exist=i_exist)
│
├── [if .not. i_exist .or. in_cha%nut == "null"]
│   │  open (105,file=in_cha%nut)
│   │  read (105,*,iostat=eof) titldum
│   │  read (105,*,iostat=eof) header
│   
│   ├── [do while (eof == 0)]
│      │  read (105,*,iostat=eof) titldum
│      │  imax = imax + 1
│   │  db_mx%ch_nut = imax
│   │  read (105,*,iostat=eof) titldum
│   │  read (105,*,iostat=eof) header
│   
│   ├── [loop ich = 1,]
│      │  read (105,*,iostat=eof) titldum
│      
│      │  !! set default values for undefined parameters
│
▼
</pre>


---

## ch_read

Reads `ch` input data from file.

**Called from:** [`proc_cha`](#proc_cha)

Source: `ch_read.f90`


---

## sd_channel_read

Reads `sd_channel` input data from file.

**Called from:** [`proc_cha`](#proc_cha)

Source: `sd_channel_read.f90`


---

## aqu2d_init

**Called from:** [`proc_cha`](#proc_cha)

Source: `aqu2d_init.f90`

<pre>
aqu2d_init
│
│  !! set parameters needed to distribute gwflow to channels using geomorphical model
│
├── [loop iaq = 1,]
│   
│   │  !! set channel drainage areas
│   │  allocate (next(aq_ch(iaq)%num_tot), source = 0.)
│   
│   ├── [loop icha = 1,]
│      │  ich = aq_ch(iaq)%num(icha)
│      │  sd_ch(ich)%aqu_link = iaq
│      │  sd_ch(ich)%aqu_link_ch = icha
│      │  iob = sp_ob1%chandeg + ich - 1
│      │  ichd = ob(iob)%props
│      │  aqu_cha(icha)%area = ob(iob)%area_ha
│      │  aqu_cha(icha)%len = sd_chd(ichd)%chl
│      │  sum_len = sum_len + sd_chd(ichd)%chl
│   
│   │  !! order channels by drainage area - set linked list
│   
│   ├── [loop icha = 2,]
│      │  next1 = mfe
│      
│      ├── [loop ipts = 1,]
│         
│         ├── [if aqu_cha(icha)%area < aqu_cha(next1)%area]
│         
│         ├── [if ipts == 1]
│
├── [if npts > 0 .and. ipts == npts + 1]
│
│  !! set the sorted object- aq_ch
│
├── [loop icha = 1,]
│
▼
</pre>


---

## ch_ttcoef

this subroutine computes travel time coefficients for routing

**Called from:** [`proc_cha`](#proc_cha)

Source: `ch_ttcoef.f90`

<pre>
ch_ttcoef
│
│  !! If it is main reach default side slope to 2:1 if it is a waterway default to 8:1
│
├── [if ch_hyd(k)%side <= 1.e-6]
│   │  chsslope = ch_hyd(k)%side
│  d = ch_hyd(k)%d
│  b = ch_hyd(k)%w - 2. * d * chsslope
│
│  !! check if bottom width (b) is < 0
│
├── [if b <= 0.]
│   │  b = .5 * ch_hyd(k)%w
│   │  b = Max(0., b)
│   │  chsslope = (ch_hyd(k)%w - b) / (2. * d)
│  ch_vel(k)%wid_btm = b
│  ch_vel(k)%dep_bf = d
│
│  !! compute flow and travel time at bankfull depth
│  p = b + 2. * d * Sqrt(chsslope * chsslope + 1.)
│  a = b * d + chsslope * d * d
│
│  !! compute flow and travel time at 1.2 bankfull depth
│
▼
</pre>


---

## ch_initial

**Called from:** [`proc_cha`](#proc_cha)

Source: `ch_initial.f90`

<pre>
ch_initial
│  ised = ch_dat(idat)%sed
│  bnksize = ch_sed(ised)%bnk_d50 / 1000.
│
│  !! Clayey bank
│
├── [if bnksize <= 0.005]
│   │  ch(irch)%bnk_cla = 0.65
│   │  ch(irch)%bnk_sil = 0.15
│   │  ch(irch)%bnk_san = 0.15
│   │  ch(irch)%bnk_gra = 0.05
│
├── [if bnksize > 0.005 .and. bnksize <= 0.05]
│   │  ch(irch)%bnk_sil = 0.65
│   │  ch(irch)%bnk_cla = 0.15
│   │  ch(irch)%bnk_san = 0.15
│   │  ch(irch)%bnk_gra = 0.05
│
├── [if bnksize > 0.05 .and. bnksize <= 2.]
│
│  !! Gravel bank
│
├── [if bnksize > 2.]
│
├── [if bedsize <= 0.005]
│
├── [if bedsize > 0.005 .and. bedsize <= 0.05]
│
├── [if bedsize > 0.05 .and. bedsize <= 2.]
│
▼
</pre>


---

## sd_channel_surf_link

**Called from:** [`proc_cha`](#proc_cha)

Source: `sd_channel_surf_link.f90`

<pre>
sd_channel_surf_link
│
├── [loop ics = 1,]
│   
│   ├── [if sd_ch(ics)%fp%obj_tot > 0]
│      
│      │  !! determine number of hru's
│      
│      ├── [loop ii = 1,]
│         │  iobtyp = sd_ch(ics)%fp%obtyp(ii)
│         
│         ├── [select case (iobtyp)]
│         │  ihru_tot = ihru_tot + 1
│         │  iru = sd_ch(ics)%fp%obtypno(ii)
│         │  ihru_tot = ihru_tot + ru_def(iru)%num_tot
│   │  allocate (sd_ch(ics)%fp%hru(ihru_tot), source = 0)
│   │  allocate (sd_ch(ics)%fp%hru_fr(ihru_tot), source = 0.)
│   
│   │  !! calculate total flood plain area and set hru numbers
│   │  sd_ch(ics)%fp%ha = 0.
│   
│   ├── [loop ii = 1,]
│      │  iobtyp = sd_ch(ics)%fp%obtyp(ii)
│      
│      ├── [select case (iobtyp)]
│         │  ihru_tot = ihru_tot + 1
│         │  ihru = sd_ch(ics)%fp%obtypno(ii)
│         
│         ├── [loop iihru = 1,]
│
├── [loop ihru = 1,]
│
▼
</pre>


---

## time_conc_init

**Called from:** [`proc_cha`](#proc_cha)

Source: `time_conc_init.f90`

<pre>
time_conc_init
│
├── [loop iru = 1,]
│   │  ru_n(iru) = 0.
│   
│   ├── [loop ii = 1,]
│      │  ielem = ru_def(iru)%num(ii)
│      
│      ├── [if ru_elem(ielem)%obtyp == "hru"]
│         │  ihru = ru_elem(ielem)%obtypno
│         │  ru_n(iru) = ru_n(iru) + hru(ihru)%luse%ovn * hru(ihru)%km
│         │  ru_n(iru) = 0.1
│
├── [loop iru = 1,]
│   │  iob = sp_ob1%ru + iru - 1
│   │  ru(iru)%da_km2 = ob(iob)%area_ha / 100.
│   │  ru_n(iru) = ru_n(iru) / ru(iru)%da_km2
│   │  ith = ru(iru)%dbs%toposub_db
│   │  tov = .0556 * (topo_db(ith)%slope_len * ru_n(iru)) ** .6 /              
│
│  !! compute time of concentration (sum of overland and channel times)
│
├── [loop ihru = 1,]
│   
│   │  !! assume channel begins at 1/2 of distance
│   
│   │  !! compute fraction of surface runoff that is reaching the main channel
│   
│   ├── [if time%step > 1]
│
▼
</pre>


---

## aqu_read

Reads `aqu` input data from file.

**Called from:** [`proc_aqu`](#proc_aqu)

Source: `aqu_read.f90`


---

## aqu_initial

**Called from:** [`proc_aqu`](#proc_aqu)

Source: `aqu_initial.f90`

<pre>
aqu_initial
│
├── [if cs_db%num_pests > 0]
│
├── [if cs_db%num_salts > 0]
│   
│   ├── [loop iaq = 1,sp_ob%aqu]
│      
│      ├── [loop isalt=1,cs_db%num_salts]
│         │  asaltb_m(iaq)%salt(isalt)%rchrg = 0.
│         │  asaltb_m(iaq)%salt(isalt)%seep = 0.
│         │  asaltb_m(iaq)%salt(isalt)%saltgw = 0.
│         │  asaltb_m(iaq)%salt(isalt)%conc = 0.
│         │  asaltb_m(iaq)%salt(isalt)%irr = 0.
│         │  asaltb_y(iaq)%salt(isalt)%rchrg = 0.
│         │  asaltb_y(iaq)%salt(isalt)%seep = 0.
│         │  asaltb_y(iaq)%salt(isalt)%saltgw = 0.
│         │  asaltb_y(iaq)%salt(isalt)%conc = 0.
│         │  asaltb_y(iaq)%salt(isalt)%irr = 0.
│
▼
</pre>


---

## aqu_read_init

Reads `aqu_init` input data from file.

**Called from:** [`proc_aqu`](#proc_aqu)

Source: `aqu_read_init.f90`


---

## aqu_read_init_cs

Reads `aqu_init_cs` input data from file.

**Called from:** [`proc_aqu`](#proc_aqu)

Source: `aqu_read_init_cs.f90`


---

## ascrv

this subroutine computes shape parameters x5 and x6 for the S curve

**Called from:** [`caltsoft_hyd`](#caltsoft_hyd), [`curno`](#curno), [`hru_lte_read`](#hru_lte_read), [`plantparm_init`](#plantparm_init), [`topohyd_init`](#topohyd_init)

Source: `ascrv.f90`

<pre>
ascrv
│
│  !! x) This subroutine is called from readbsn.f and readcrop.f
│  xx = Log(x3/x1 - x3)
│  x6 = (xx - Log(x4/x2 - x4)) / (x4 - x3)
│  x5 = xx + (x3 * x6)
│
▼
</pre>


---

## res_read_hyd

Reads `res_hyd` input data from file.

**Called from:** [`proc_res`](#proc_res)

Source: `res_read_hyd.f90`


---

## res_read_sed

this subroutine reads data from the lake water quality input file (.lwq).

**Called from:** [`proc_res`](#proc_res)

Source: `res_read_sed.f90`

<pre>
res_read_sed
│
│  !! apply to all reservoirs in the watershed.
│  inquire (file=in_res%sed_res,exist=i_exist)
│
├── [if .not. i_exist .or. in_res%sed_res == "null"]
│   │  open (105,file=in_res%sed_res)
│   │  read (105,*,iostat=eof) titldum
│   │  read (105,*,iostat=eof) header
│   
│   ├── [do while (eof == 0)]
│      │  read (105,*,iostat=eof) titldum
│      │  imax = imax + 1
│   │  db_mx%res_sed = imax
│   │  read (105,*,iostat=eof) titldum
│   │  read (105,*,iostat=eof) header
│   
│   ├── [loop ires = 1,]
│      │  read (105,*,iostat=eof) titldum
│
▼
</pre>


---

## res_read_nut

this subroutine reads data from the lake water quality input file (.lwq).

**Called from:** [`proc_res`](#proc_res)

Source: `res_read_nut.f90`

<pre>
res_read_nut
│
│  !! apply to all reservoirs in the watershed.
│  inquire (file=in_res%nut_res,exist=i_exist)
│
├── [if .not. i_exist .or. in_res%nut_res == "null"]
│   │  open (105,file=in_res%nut_res)
│   │  read (105,*,iostat=eof) titldum
│   │  read (105,*,iostat=eof) header
│   
│   ├── [do while (eof == 0)]
│      │  read (105,*,iostat=eof) titldum
│      │  imax = imax + 1
│   │  db_mx%res_nut = imax
│   │  read (105,*,iostat=eof) titldum
│   │  read (105,*,iostat=eof) header
│   
│   ├── [loop ires = 1,]
│      │  read (105,*,iostat=eof) titldum
│
│  !! convert units
│
├── [loop ires = 1,]
│
▼
</pre>


---

## res_read_init

Reads `res_init` input data from file.

**Called from:** [`proc_res`](#proc_res)

Source: `res_read_init.f90`


---

## res_read_saltdb

this subroutine reads reservoir water quality parameters for salt ions

**Called from:** [`proc_res`](#proc_res)

Source: `res_read_saltdb.f90`

<pre>
res_read_saltdb
│
│  !! this subroutine reads reservoir water quality parameters for salt ions
│  inquire (file="salt_res",exist=i_exist)
│
├── [if .not. i_exist .or. in_res%nut_res == "null"]
│   │  open (105,file="salt_res")
│   │  read (105,*,iostat=eof) titldum
│   │  read (105,*,iostat=eof) titldum
│   
│   ├── [loop i=1,8]
│   │  read (105,*,iostat=eof) header
│   
│   ├── [do while (eof == 0)]
│      │  read (105,*,iostat=eof) titldum
│      │  imax = imax + 1
│   │  db_mx%res_salt = imax
│   
│   ├── [loop isalti=1,imax]
│      │  allocate (res_salt_data(isalti)%c_init(cs_db%num_salts), source = 0.)
│   │  read (105,*,iostat=eof) titldum
│   
│   ├── [loop i=1,8]
│   
│   ├── [loop ires = 1,]
│
▼
</pre>


---

## res_read_csdb

this subroutine reads reservoir water quality parameters for constituents

**Called from:** [`proc_res`](#proc_res)

Source: `res_read_csdb.f90`

<pre>
res_read_csdb
│
│  !! this subroutine reads reservoir water quality parameters for constituents
│  inquire (file="cs_res",exist=i_exist)
│
├── [if .not. i_exist .or. in_res%nut_res == "null"]
│   │  open (105,file="cs_res")
│   │  read (105,*,iostat=eof) titldum
│   │  read (105,*,iostat=eof) titldum
│   
│   ├── [loop i=1,12]
│   │  read (105,*,iostat=eof) header
│   
│   ├── [do while (eof == 0)]
│      │  read (105,*,iostat=eof) titldum
│      │  imax = imax + 1
│   │  db_mx%res_cs = imax
│   │  read (105,*,iostat=eof) titldum
│   │  read (105,*,iostat=eof) titldum
│   
│   ├── [loop i=1,12]
│   
│   ├── [loop ires = 1,]
│
▼
</pre>


---

## res_read_conds

**Called from:** [`proc_res`](#proc_res)

Source: `res_read_conds.f90`

<pre>
res_read_conds
│  inquire (file="res_conds.dat", exist=i_exist)
│  open (100,file="res_conds.dat")
│  read (100,*,iostat=eof) title
│  read (100,*,iostat=eof) max_table
│  db_mx%ctbl_res = max_table
│
│  !! read data for each condition table
│
├── [loop ictbl = 1,]
│   
│   │  !! loop through all conditions
│   
│   ├── [loop ii = 1,]
│      
│      │  !! read total number of sub conditions
│      │  read (100,*) ctbl(ictbl)%conds(ii)%num_conds, (ctbl(ictbl)%conds(ii)%sco
│   
│   │  !! read each module
│   
│   ├── [loop imod = 1,]
│      │  ctbl(ictbl)%mods(imod)%num_conds = tnum_conds
│      
│      │  !! read all subconditions
│      
│      ├── [loop ii = 1,]
│         │  read (100,*) ctbl(ictbl)%mods(imod)%con(ii)%num_conds, (ctbl(ictbl)%mods
│
▼
</pre>


---

## res_allo

**Called from:** [`proc_res`](#proc_res)

Source: `res_allo.f90`

<pre>
res_allo
│  mres = sp_ob%res
│
├── [if cs_db%num_tot > 0]
│   
│   ├── [loop ires = 1,]
│      
│      ├── [if cs_db%num_pests > 0]
│         │  allocate (res_water(ires)%pest(cs_db%num_pests), source = 0.)
│         │  allocate (res_benthic(ires)%pest(cs_db%num_pests), source = 0.)
│         │  allocate (res_ob(ires)%aq_mix(cs_db%num_pests), source = 0.)
│
▼
</pre>


---

## res_objects

**Called from:** [`proc_res`](#proc_res)

Source: `res_objects.f90`

<pre>
res_objects
│
│  !! set reservoir object numbers for reservoir objects
│  iob1 = sp_ob1%res
│  iob2 = sp_ob1%res + sp_ob%res - 1
│
├── [loop i = iob1,]
│   │  ires = ires + 1
│   │  res_ob(ires)%ob = i
│   │  res_ob(ires)%props = ob(i)%props
│
▼
</pre>


---

## res_read

Reads `res` input data from file.

**Called from:** [`proc_res`](#proc_res)

Source: `res_read.f90`


---

## res_read_salt_cs

**Called from:** [`proc_res`](#proc_res)

Source: `res_read_salt_cs.f90`

<pre>
res_read_salt_cs
│  inquire (file="reservoir.res_cs",exist=i_exist)
│
├── [if i_exist]
│   │  open(105,file="reservoir.res_cs")
│   
│   ├── [loop i = 1,]
│      │  read (105,*,iostat=eof) ires
│      │  read (105,*,iostat=eof) k, res_dat_c_cs(ires)
│      
│      ├── [loop isalt = 1,]
│         
│         ├── [if res_salt_data(isalt)%name == res_dat_c_cs(ires]
│         │  res_dat(ires)%salt = isalt
│   
│   ├── [loop ics = 1,]
│      
│      ├── [if res_cs_data(ics)%name == res_dat_c_cs(ires)%cs]
│         │  res_dat(ires)%cs = ics
│  close(105)
│
▼
</pre>


---

## cal_parm_read

this function computes new parameter value based on

**Called from:** [`proc_cal`](#proc_cal)

Source: `cal_parm_read.f90`


---

## calsoft_read_codes

**Called from:** [`proc_cal`](#proc_cal)

Source: `calsoft_read_codes.f90`

<pre>
calsoft_read_codes
│  inquire (file=in_chg%codes_sft, exist=i_exist)
│
├── [if .not. i_exist .or. in_chg%codes_sft == "null"]
│   │  open (107,file=in_chg%codes_sft)
│   │  read (107,*,iostat=eof) titldum
│   │  read (107,*,iostat=eof) header
│   │  read (107,*,iostat=eof) cal_codes
│  close(107)
│
▼
</pre>


---

## lcu_read_softcal

**Called from:** [`proc_cal`](#proc_cal)

Source: `lcu_read_softcal.f90`

<pre>
lcu_read_softcal
│  inquire (file=in_chg%water_balance_sft, exist=i_exist)
│
├── [if .not. i_exist .or. in_chg%water_balance_sft ==]
│   │  open (107,file=in_chg%water_balance_sft)
│   │  read (107,*,iostat=eof) titldum
│   │  read (107,*,iostat=eof) mreg
│   │  read (107,*,iostat=eof) header
│   
│   │  !! allocate regional output files
│   │  db_mx%lsu_reg = mreg
│   
│   ├── [loop ireg = 1,]
│      │  read (107,*,iostat=eof) region(ireg)%name, region(ireg)%nlum
│      │  db_mx%landuse = region(ireg)%nlum
│      │  mlug = region(ireg)%nlum
│
▼
</pre>


---

## ls_read_lsparms_cal

**Called from:** [`proc_cal`](#proc_cal)

Source: `ls_read_parms_cal.f90`

<pre>
ls_read_lsparms_cal
│  inquire (file=in_chg%wb_parms_sft, exist=i_exist)
│
├── [if .not. i_exist .or. in_chg%wb_parms_sft == "nul]
│   │  open (107,file = in_chg%wb_parms_sft)
│   │  read (107,*,iostat=eof) titldum
│   │  read (107,*,iostat=eof) mlsp
│   │  read (107,*,iostat=eof) header
│  db_mx%lscal_prms = mlsp
│
├── [loop i = 1,]
│   │  read (107,*,iostat=eof) ls_prms(i)%name, ls_prms(i)%chg_typ, ls_prms(i)%
│  close(107)
│
▼
</pre>


---

## ch_read_orders_cal

**Called from:** [`proc_cal`](#proc_cal)

Source: `ch_read_orders_cal.f90`

<pre>
ch_read_orders_cal
│  inquire (file=in_chg%ch_sed_budget_sft, exist=i_exist)
│
├── [if .not. i_exist .or. in_chg%ch_sed_budget_sft ==]
│   │  open (107,file=in_chg%ch_sed_budget_sft)
│   │  read (107,*,iostat=eof) titldum
│   │  read (107,*,iostat=eof) mreg
│   │  read (107,*,iostat=eof) header
│   
│   ├── [loop i = 1,]
│      │  read (107,*,iostat=eof) chcal(i)%name, chcal(i)%ord_num, nspu
│      
│      ├── [if nspu > 0]
│         │  allocate (elem_cnt(nspu), source = 0)
│         │  read (107,*,iostat=eof) chcal(i)%name, chcal(i)%ord_num,  nspu, (elem_cn
│         
│         │  !! save the object number of each defining unit
│         
│         ├── [loop ii = 1,]
│         │  ie1 = elem_cnt(ii)
│         
│         ├── [if ii == nspu]
│         │  ielem = ielem + 1
│         
│         ├── [if elem_cnt(ii+1) < 0]
│         
│         ├── [loop ie = ie1,]
│
▼
</pre>


---

## ch_read_parms_cal

Reads `ch_parms_cal` input data from file.

**Called from:** [`proc_cal`](#proc_cal)

Source: `ch_read_parms_cal.f90`


---

## cal_allo_init

**Called from:** [`proc_cal`](#proc_cal)

Source: `cal_allo_init.f90`

<pre>
cal_allo_init
│
├── [loop iihru = 1,]
│   │  icom = hru(iihru)%plant_cov
│   │  nplt = pcomdb(icom)%plants_com
│   │  isched = hru(iihru)%mgt_ops
│   
│   ├── [loop iauto = 1,]
│      │  id = sched(isched)%num_db(iauto)
│      │  allocate (pcom_init(iihru)%dtbl(iauto)%num_actions(dtbl_lum(id)%acts), s
│      │  pcom_init(iihru)%dtbl(iauto)%num_actions = 1
│      │  allocate (pcom_init(iihru)%dtbl(iauto)%days_act(dtbl_lum(id)%acts), sour
│      │  pcom_init(iihru)%dtbl(iauto)%days_act = 0
│   │  nly1 = soil(iihru)%nly + 1
│
│  !! initialize all hru parameters
│
├── [if sp_ob%hru > 0]
│   │  hru_init = hru
│
▼
</pre>


---

## unit_hyd

This subroutine computes variables related to the watershed hydrology:

**Called from:** [`unit_hyd_ru_hru`](#unit_hyd_ru_hru)

Source: `unit_hyd.f90`

<pre>
unit_hyd
│
│  !! SWAT: Ttcoef
│
│  !! compute unit hydrograph for computing hydrograph from direct runoff
│  tb = .5 + .6 * tc + bsn_prm%tb_adj
│  tp = .375 * tb
│
│  !! sum 20 points on the unit hydrograph to get sum for the time%step
│  t_inc = tb / 20.
│  ts_base = int(tb / (time%dtm / 60.))
│  ts_base = max (1, ts_base)
│  t_inc = 20 / ts_base + 2
│  t_inc_hr = time%dtm / float(t_inc) / 60.
│
├── [loop iday = 1,]
│   
│   ├── [loop istep = 1,]
│      
│      │  !! increment within the time step to get accurate estimates near peak
│      
│      ├── [loop i = 1,]
│         │  t_tot = t_tot + t_inc_hr
│         
│         │  !! Triangular Unit Hydrograph
│         
│         ├── [if bsn_cc%uhyd == 0]
│         
│         ├── [if t_tot < tp]
│         
│         │  !! rising limb of hydrograph
│         │  q = t_tot / tp
│         
│         │  !! falling limb of hydrograph
│         │  q = (tb - t_tot) / (tb - tp)
│   
│   │  !! Gamma Function Unit Hydrograph
│   
│   ├── [if bsn_cc%uhyd == 1]
│
├── [loop i = 1,]
│   
│   ├── [loop istep = 1,]
│
▼
</pre>


---

## hyddep_output

this subroutine outputs hyd variables on daily, monthly and annual time steps

**Called from:** [`command`](#command)

Source: `hyddep_output.f90`


---

## recall_nut

**Called from:** [`command`](#command)

Source: `recall_nut.f90`

<pre>
recall_nut
│  ichan = ob(icmd)%obtypno_out(1)
│
├── [if ch_stor(ichan)%flo > 10.]
│   │  sol_conc = (ch_stor(ichan)%no3*1000.) / ch_stor(ichan)%flo
│   │  div_mass = (sol_conc * recall(irec)%hd(time%day,time%yrs)%flo) / 1000.
│   
│   ├── [if (div_mass*(-1)) > ch_stor(ichan)%no3]
│      │  div_mass = ch_stor(ichan)%no3 * (-1)
│   │  ch_stor(ichan)%no3 = ch_stor(ichan)%no3 + div_mass
│   │  sol_conc = (ch_stor(ichan)%solp*1000.) / ch_stor(ichan)%flo
│   │  div_mass = (sol_conc * recall(irec)%hd(time%day,time%yrs)%flo) / 1000.
│   
│   ├── [if (div_mass*(-1)) > ch_stor(ichan)%solp]
│      │  div_mass = ch_stor(ichan)%solp * (-1)
│   │  ch_stor(ichan)%solp = ch_stor(ichan)%solp + div_mass
│   │  sol_conc = (ch_stor(ichan)%nh3*1000.) / ch_stor(ichan)%flo
│   
│   ├── [if (div_mass*(-1)) > ch_stor(ichan)%nh3]
│   
│   ├── [if (div_mass*(-1)) > ch_stor(ichan)%no2]
│   
│   ├── [if (div_mass*(-1)) > ch_stor(ichan)%dox]
│   
│   ├── [if (div_mass*(-1)) > ch_stor(ichan)%orgn]
│
▼
</pre>


---

## recall_salt

**Called from:** [`command`](#command)

Source: `recall_salt.f90`

<pre>
recall_salt
│  obcs(icmd)%hd(1)%salt = 0.
│
├── [loop isalt=1,cs_db%num_salts]
│   │  div_conc_salt(isalt,irec) = 0.
│
├── [if cs_db%num_salts > 0]
│   
│   ├── [select case (rec_salt(irec)%typ)]
│      
│      ├── [if time%yrc >= recall(irec)%start_yr .and. time%y]
│         
│         ├── [loop isalt=1,cs_db%num_salts]
│         
│         ├── [if recall(irec)%hd(time%day,time%yrs)%flo < 0]
│         │  ichan = ob(icmd)%obtypno_out(1)
│         
│         ├── [if ch_stor(ichan)%flo > 10.]
│         │  salt_conc = (ch_water(ichan)%salt(isalt)*1000.) / ch_stor(ichan)%flo
│         │  div_conc_salt(isalt,irec) = salt_conc
│         │  div_mass = (salt_conc * recall(irec)%hd(time%day,time%yrs)%flo) / 1000.
│         
│         ├── [if (div_mass*(-1)) > ch_water(ichan)%salt(isalt)]
│         │  div_mass = ch_water(ichan)%salt(isalt) * (-1)
│      │  ch_water(ichan)%salt(isalt) = ch_water(ichan)%salt(isalt) + div_mass
│      │  chsalt_d(ichan)%salt(isalt)%div = div_mass
│   │  obcs(icmd)%hd(1)%salt(isalt) = rec_salt(irec)%hd_salt(time%day,time%yrs)
│
├── [if rec_salt(irec)%pts_type.eq.1]
│   
│   ├── [loop isalt=1,cs_db%num_salts]
│   
│   ├── [loop isalt=1,cs_db%num_salts]
│
├── [if time%yrc >= recall(irec)%start_yr .and. time%y]
│   
│   ├── [loop isalt=1,cs_db%num_salts]
│
▼
</pre>


---

## recall_cs

**Called from:** [`command`](#command)

Source: `recall_cs.f90`

<pre>
recall_cs
│  obcs(icmd)%hd(1)%cs = 0.
│
├── [loop ics=1,cs_db%num_cs]
│   │  div_conc_cs(ics,irec) = 0.
│
├── [if cs_db%num_cs > 0]
│   
│   ├── [select case (rec_cs(irec)%typ)]
│      
│      ├── [if time%yrc >= recall(irec)%start_yr .and. time%y]
│         
│         ├── [loop ics=1,cs_db%num_cs]
│         
│         ├── [if recall(irec)%hd(time%day,time%yrs)%flo < 0]
│         │  ichan = ob(icmd)%obtypno_out(1)
│         
│         ├── [if ch_stor(ichan)%flo > 10.]
│         │  cs_conc = (ch_water(ichan)%cs(ics)*1000.) / ch_stor(ichan)%flo
│         │  div_conc_cs(ics,irec) = cs_conc
│         │  div_mass = (cs_conc * recall(irec)%hd(time%day,time%yrs)%flo) / 1000.
│         
│         ├── [if (div_mass*(-1)) > ch_water(ichan)%cs(ics)]
│         │  div_mass = ch_water(ichan)%cs(ics) * (-1)
│      │  ch_water(ichan)%cs(ics) = ch_water(ichan)%cs(ics) + div_mass
│      │  chcs_d(ichan)%cs(ics)%div = div_mass
│   │  obcs(icmd)%hd(1)%cs(ics) = rec_cs(irec)%hd_cs(time%day,time%yrs)%cs(ics)
│
├── [if rec_cs(irec)%pts_type.eq.1]
│
├── [if time%yrc >= recall(irec)%start_yr .and. time%y]
│   
│   ├── [loop ics=1,cs_db%num_cs]
│   
│   ├── [if rec_cs(irec)%pts_type.eq.1]
│
▼
</pre>


---

## constit_hyd_mult

**Called from:** [`command`](#command)

Source: `constit_hyd_mult.f90`

<pre>
constit_hyd_mult
│  idr_pest = dr_pest_num(idr)
│
├── [loop ipest = 1,]
│   │  obcs(iob)%hd(1)%pest(ipest) =  obcs(iob)%hin(1)%pest(ipest) * dr_pest(id
│  idr_path = dr_path_num(idr)
│
├── [loop ipath = 1,]
│   │  obcs(iob)%hd(1)%path(ipath) =  obcs(iob)%hin(1)%path(ipath) * dr_path(id
│  idr_hmet = dr_hmet_num(idr)
│
├── [loop ihmet = 1,]
│   │  obcs(iob)%hd(1)%hmet(ihmet) =  obcs(iob)%hin(1)%hmet(ihmet) * dr_hmet(id
│  idr_salt = dr_salt_num(idr)
│
├── [loop isalt = 1,]
│   │  obcs(iob)%hd(1)%salt(isalt) =  obcs(iob)%hin(1)%salt(isalt) * dr_salt(id
│
▼
</pre>


---

## flow_dur_curve

**Called from:** [`command`](#command)

Source: `flow_dur_curve.f90`

<pre>
flow_dur_curve
│  ob(icmd)%fdc_ll(time%day)%val = ob(icmd)%hd(1)%flo
│  next = ob(icmd)%fdc%mfe
│  npts = time%day - 1
│
├── [loop ipts = 1,]
│   
│   ├── [if ob(icmd)%fdc_ll(time%day)%val >= ob(icmd)%fdc_]
│      │  ob(icmd)%fdc_ll(time%day)%next = next
│      
│      ├── [if ipts == 1]
│         │  ob(icmd)%fdc%mfe = time%day
│         │  ob(icmd)%fdc_ll(iprv)%next = time%day
│   │  iprv = next
│   │  next = ob(icmd)%fdc_ll(next)%next
│
├── [if npts > 0 .and. ipts == npts + 1]
│   │  mle = ob(icmd)%fdc%mle
│   │  ob(icmd)%fdc_ll(mle)%next = time%day
│
├── [if time%end_yr == 1]
│   
│   ├── [loop iday = 1,]
│      
│      ├── [if iday == fdc_days(nprob)]
│   
│   ├── [loop iday = 1,]
│
▼
</pre>


---

## hydout_output

this subroutine outputs hyd variables on daily, monthly and annual time steps

**Called from:** [`command`](#command)

Source: `hydout_output.f90`


---

## obj_output

Writes `obj` output to file.

**Called from:** [`command`](#command)

Source: `obj_output.f90`


---

## wallo_allo_output

Writes `wallo_allo` output to file.

**Called from:** [`command`](#command)

Source: `wallo_allo_output.f90`


---

## wallo_trn_output

Writes `wallo_trn` output to file.

**Called from:** [`command`](#command)

Source: `wallo_trn_output.f90`


---

## wallo_treat_output

Writes `wallo_treat` output to file.

**Called from:** [`command`](#command)

Source: `wallo_treat_output.f90`


---

## wallo_use_output

Writes `wallo_use` output to file.

**Called from:** [`command`](#command)

Source: `wallo_use_output.f90`


---

## manure_source_output

Writes `manure_source` output to file.

**Called from:** [`command`](#command)

Source: `manure_source_output.f90`


---

## manure_demand_output

Writes `manure_demand` output to file.

**Called from:** [`command`](#command)

Source: `manure_demand_output.f90`


---

## hru_lte_output

Writes `hru_lte` output to file.

**Called from:** [`command`](#command)

Source: `hru_lte_output.f90`


---

## hru_carbon_output

this subroutine outputs HRU variables on daily, monthly and annual time steps

**Called from:** [`command`](#command)

Source: `hru_carbon_output.f90`


---

## wetland_output

Writes `wetland` output to file.

**Called from:** [`command`](#command)

Source: `wetland_output.f90`


---

## wet_salt_output

this subroutine outputs salt ion mass in wetlands (by HRU)

**Called from:** [`command`](#command)

Source: `wet_salt_output.f90`


---

## wet_cs_output

this subroutine outputs constituent mass in wetlands (by HRU)

**Called from:** [`command`](#command)

Source: `wet_cs_output.f90`


---

## hru_pesticide_output

this subroutine outputs HRU variables on daily, monthly and annual time steps

**Called from:** [`command`](#command)

Source: `hru_pesticide_output.f90`


---

## hru_pathogen_output

this subroutine outputs HRU variables on daily, monthly and annual time steps

**Called from:** [`command`](#command)

Source: `hru_pathogen_output.f90`


---

## hru_salt_output

this subroutine outputs salt mass loadings and concentrations from HRUs

**Called from:** [`command`](#command)

Source: `hru_salt_output.f90`


---

## hru_cs_output

this subroutine outputs constituent mass loadings and concentrations from HRUs

**Called from:** [`command`](#command)

Source: `hru_cs_output.f90`


---

## soil_nutcarb_write

this subroutine writes soil carbon output.

**Called from:** [`command`](#command), [`hru_output`](#hru_output), [`output_landscape_init`](#output_landscape_init)

Source: `soil_nutcarb_write.f90`


---

## soil_carbvar_write

this subroutine writes soil carbon output.

**Called from:** [`command`](#command)

Source: `soil_carbvar_write.f90`


---

## aquifer_output

Writes `aquifer` output to file.

**Called from:** [`command`](#command)

Source: `aquifer_output.f90`


---

## aqu_salt_output

this subroutine outputs salt mass loadings and concentrations in aquifers

**Called from:** [`command`](#command)

Source: `aqu_salt_output.f90`


---

## aqu_cs_output

this subroutine outputs constituent mass loadings and concentrations in aquifers

**Called from:** [`command`](#command)

Source: `aqu_cs_output.f90`


---

## aqu_pesticide_output

this subroutine outputs HRU variables on daily, monthly and annual time steps

**Called from:** [`command`](#command)

Source: `aqu_pesticide_output.f90`


---

## channel_output

Writes `channel` output to file.

**Called from:** [`command`](#command)

Source: `channel_output.f90`


---

## sd_chanmorph_output

Writes `sd_chanmorph` output to file.

**Called from:** [`command`](#command)

Source: `sd_chanmorph_output.f90`


---

## sd_chanbud_output

Writes `sd_chanbud` output to file.

**Called from:** [`command`](#command)

Source: `sd_chanbud_output.f90`


---

## sd_channel_output

Writes `sd_channel` output to file.

**Called from:** [`command`](#command)

Source: `sd_channel_output.f90`


---

## cha_pesticide_output

this subroutine outputs HRU variables on daily, monthly and annual time steps

**Called from:** [`command`](#command)

Source: `cha_pesticide_output.f90`


---

## ch_salt_output

this subroutine outputs salt mass in channels

**Called from:** [`command`](#command)

Source: `ch_salt_output.f90`


---

## ch_cs_output

this subroutine outputs constituent mass in channels

**Called from:** [`command`](#command)

Source: `ch_cs_output.f90`


---

## cs_str_output

this subroutine prints out daily constituent data for specified channels

**Called from:** [`command`](#command)

Source: `cs_str_output.f90`


---

## reservoir_output

Writes `reservoir` output to file.

**Called from:** [`command`](#command)

Source: `reservoir_output.f90`


---

## res_pesticide_output

this subroutine outputs HRU variables on daily, monthly and annual time steps

**Called from:** [`command`](#command)

Source: `res_pesticide_output.f90`


---

## res_salt_output

this subroutine outputs salt mass in reservoirs

**Called from:** [`command`](#command)

Source: `res_salt_output.f90`


---

## res_cs_output

this subroutine outputs constituent mass in reservoirs

**Called from:** [`command`](#command)

Source: `res_cs_output.f90`


---

## ru_output

Writes `ru` output to file.

**Called from:** [`command`](#command)

Source: `ru_output.f90`


---

## ru_salt_output

this subroutine outputs salt mass loadings and concentrations from routing units

**Called from:** [`command`](#command)

Source: `ru_salt_output.f90`


---

## ru_cs_output

this subroutine outputs constituent mass loadings and concentrations from routing units

**Called from:** [`command`](#command)

Source: `ru_cs_output.f90`


---

## recall_output

this subroutine outputs SUBBASIN variables on daily, monthly and annual time steps

**Called from:** [`command`](#command)

Source: `recall_output.f90`


---

## hydin_output

this subroutine outputs hyd variables on daily, monthly and annual time steps

**Called from:** [`command`](#command)

Source: `hydin_output.f90`


---

## basin_ch_pest_output

this subroutine outputs HRU variables on daily, monthly and annual time steps

**Called from:** [`command`](#command)

Source: `basin_ch_pest_output.f90`


---

## basin_res_pest_output

this subroutine outputs HRU variables on daily, monthly and annual time steps

**Called from:** [`command`](#command)

Source: `basin_res_pest_output.f90`


---

## basin_ls_pest_output

this subroutine outputs HRU variables on daily, monthly and annual time steps

**Called from:** [`command`](#command)

Source: `basin_ls_pest_output.f90`


---

## basin_aqu_pest_output

this subroutine outputs HRU variables on daily, monthly and annual time steps

**Called from:** [`command`](#command)

Source: `basin_aqu_pest_output.f90`


---

## basin_output

Writes `basin` output to file.

**Called from:** [`command`](#command)

Source: `basin_output.f90`


---

## lsu_output

Writes `lsu` output to file.

**Called from:** [`command`](#command)

Source: `lsu_output.f90`


---

## basin_aquifer_output

Writes `basin_aquifer` output to file.

**Called from:** [`command`](#command)

Source: `basin_aquifer_output.f90`


---

## basin_reservoir_output

Writes `basin_reservoir` output to file.

**Called from:** [`command`](#command)

Source: `basin_reservoir_output.f90`


---

## basin_channel_output

Writes `basin_channel` output to file.

**Called from:** [`command`](#command)

Source: `basin_channel_output.f90`


---

## basin_chanmorph_output

Writes `basin_chanmorph` output to file.

**Called from:** [`command`](#command)

Source: `basin_chanmorph_output.f90`


---

## basin_chanbud_output

Writes `basin_chanbud` output to file.

**Called from:** [`command`](#command)

Source: `basin_chanbud_output.f90`


---

## basin_sdchannel_output

Writes `basin_sdchannel` output to file.

**Called from:** [`command`](#command)

Source: `basin_sdchannel_output.f90`


---

## basin_recall_output

Writes `basin_recall` output to file.

**Called from:** [`command`](#command)

Source: `basin_recall_output.f90`


---

## salt_balance

this subroutine calculates total salt in system and writes out data to perform

**Called from:** [`command`](#command)

Source: `salt_balance.f90`

<pre>
salt_balance
│
│  !! a system-wide salt mass balance.
│
├── [loop i=1,sp_ob%hru]
│   
│   ├── [loop m=1,cs_db%num_salts]
│      │  saltsum = saltsum + (hsaltb_d(i)%salt(m)%latq * hru(i)%area_ha)
│  salt_basin(1) = saltsum
│
├── [if bsn_cc%gwflow == 1]
│   
│   ├── [if gw_solute_flag == 1]
│      
│      ├── [loop i=1,ncell]
│         
│         ├── [loop m=1,cs_db%num_salts]
│         │  saltsum = saltsum + (gwsol_ss(i)%solute(2+m)%gwsw * (-1) / 1000.)
│         │  saltsum = saltsum + (gwsol_ss(i)%solute(2+m)%swgw * (-1) / 1000.)
│         │  saltsum = saltsum + (gwsol_ss(i)%solute(2+m)%satx * (-1) / 1000.)
│  ob_ctr = sp_ob1%aqu
│
├── [loop i=1,sp_ob%aqu]
│   
│   ├── [loop m=1,cs_db%num_salts]
│      │  saltsum = saltsum + asaltb_d(i)%salt(m)%saltgw
│   │  ob_ctr = ob_ctr + 1
│  salt_basin(2) = saltsum
│
├── [loop i=1,sp_ob%hru]
│   
│   ├── [loop m=1,cs_db%num_salts]
│      │  saltsum = saltsum + (hsaltb_d(i)%salt(m)%surq * hru(i)%area_ha)
│
├── [loop i=1,sp_ob%hru]
│   
│   ├── [loop m=1,cs_db%num_salts]
│
▼
</pre>


---

## cs_balance

this subroutine calculates total constituent mass in system and writes out data to perform

**Called from:** [`command`](#command)

Source: `cs_balance.f90`

<pre>
cs_balance
│
│  !! a system-wide constituent mass balance
│
├── [loop i=1,sp_ob%hru]
│   │  cssum1 = cssum1 + (hcsb_d(i)%cs(1)%latq * hru(i)%area_ha)
│   │  cssum2 = cssum2 + (hcsb_d(i)%cs(2)%latq * hru(i)%area_ha)
│   │  cssum3 = cssum3 + (hcsb_d(i)%cs(3)%latq * hru(i)%area_ha)
│  cs_basin(1) = cssum1
│  cs_basin(30) = cssum2
│  cs_basin(59) = cssum3
│
├── [loop i=1,sp_ob%hru]
│   │  cssum1 = cssum1 + (hcsb_d(i)%cs(1)%surq * hru(i)%area_ha)
│   │  cssum2 = cssum2 + (hcsb_d(i)%cs(2)%surq * hru(i)%area_ha)
│   │  cssum3 = cssum3 + (hcsb_d(i)%cs(3)%surq * hru(i)%area_ha)
│  cs_basin(2) = cssum1
│
├── [loop i=1,sp_ob%hru]
│
├── [loop i=1,sp_ob%hru]
│
▼
</pre>


---

## xmon

this subroutine determines the month, given the julian date and leap

**Called from:** [`cli_tmeas`](#cli_tmeas), [`plant_init`](#plant_init), [`time_control`](#time_control), [`time_read`](#time_read)

Source: `xmon.f90`

<pre>
xmon
│
│  !! name        |units         |definition
│
├── [loop i_mo = 1,]
│   │  m1 = i_mo + 1
│   │  nda = ndays(m1)
│   
│   ├── [if jd <= nda]
│      │  mo = i_mo
│      │  day_mo = jd - ndays(i_mo)
│
▼
</pre>


---

## basin_sw_init

**Called from:** [`time_control`](#time_control)

Source: `basin_sw_init.f90`

<pre>
basin_sw_init
│
├── [loop ihru = 1,]
│   │  hwb_d(ihru)%sw_init = soil(ihru)%sw
│   │  hwb_m(ihru)%sw_init = soil(ihru)%sw
│   │  hwb_y(ihru)%sw_init = soil(ihru)%sw
│   │  hwb_a(ihru)%sw_init = soil(ihru)%sw
│   │  hwb_d(ihru)%sno_init = hru(ihru)%sno_mm
│   │  hwb_m(ihru)%sno_init = hru(ihru)%sno_mm
│   │  hwb_y(ihru)%sno_init = hru(ihru)%sno_mm
│   │  hwb_a(ihru)%sno_init = hru(ihru)%sno_mm
│  bwb_d%sw_init = 0.
│  bwb_d%sno_init = 0.
│
├── [loop ihru = 1,]
│   
│   ├── [if lsu_elem(iihru)%bsn_frac > 1.e-12]
│      
│      ├── [if lsu_elem(iihru)%obtyp == "hru"]
│
├── [loop ihru = 1,]
│   
│   ├── [if lsu_elem(iihru)%bsn_frac > 1.e-12]
│      
│      ├── [if lsu_elem(iihru)%obtyp == "hlt"]
│
├── [loop ilsu = 1,]
│   
│   ├── [loop ielem = 1,]
│
▼
</pre>


---

## aqu_pest_output_init

Writes `aqu_pest_init` output to file.

**Called from:** [`time_control`](#time_control)

Source: `aqu_pest_output_init.f90`


---

## sim_initday

this subroutine initialized arrays at the beginning of the day

**Called from:** [`time_control`](#time_control)

Source: `sim_initday.f90`

<pre>
sim_initday
│
│  !! |hour in day in HRU
│
│  !! initialize variables at beginning of day
│
▼
</pre>


---

## cli_atmodep_time_control

**Called from:** [`time_control`](#time_control)

Source: `cli_atmodep_time_control.f90`

<pre>
cli_atmodep_time_control
│
├── [if atmodep_cont%num_sta > 0]
│   
│   ├── [if atmodep_cont%first == 1]
│      
│      ├── [if atmodep_cont%timestep == "yr"]
│         
│         ├── [if atmodep_cont%yr_init == time%yrc]
│         │  atmodep_cont%ts = 1
│         │  atmodep_cont%first = 0
│   
│   ├── [if atmodep_cont%timestep =="mo"]
│      
│      ├── [if atmodep_cont%yr_init == time%yrc .and. atmodep]
│         │  atmodep_cont%ts = 1
│         │  atmodep_cont%first = 0
│   
│   ├── [if atmodep_cont%timestep == "yr"]
│      
│      ├── [if time%end_yr == 1]
│         │  atmodep_cont%ts = atmodep_cont%ts + 1
│   
│   ├── [if atmodep_cont%timestep == "mo"]
│      
│      ├── [if time%end_mo == 1]
│         │  atmodep_cont%ts = atmodep_cont%ts + 1
│
▼
</pre>


---

## calsoft_sum_output

Writes `calsoft_sum` output to file.

**Called from:** [`time_control`](#time_control)

Source: `calsoft_sum_output.f90`


---

## mgt_newtillmix_cswat0

this subroutine mixes residue and nutrients during tillage and

**Called from:** [`actions`](#actions), [`mgt_sched`](#mgt_sched), [`time_control`](#time_control)

Source: `mgt_newtillmix_cswat0.f90`

<pre>
mgt_newtillmix_cswat0
│
│  !! Intrinsic: Min, Max
│  npmx = cs_db%num_pests
│  mix_mn = mnz
│  mix_mp = mpz
│  mix_org%tot = orgz
│  mix_org%rsd = orgz
│  mix_org%hact = orgz
│  mix_org%hsta = orgz
│  mix_org%hs = orgz
│  mix_org%hp = orgz
│  mix_org%microb = orgz
│
├── [if bmix > 1.e-6]
│
▼
</pre>


---

## calsoft_ave_output

Writes `calsoft_ave` output to file.

**Called from:** [`time_control`](#time_control)

Source: `calsoft_ave_output.f90`


---

## pl_write_parms_cal

**Called from:** [`calsoft_control`](#calsoft_control)

Source: `pl_write_parms_cal.f90`

<pre>
pl_write_parms_cal
│  open (107,file="plant_parms.cal",recl = 1500)
│  write (107,*) "Calibrated plant parameter file"
│  write (107,*) db_mx%plcal_reg
│  write (107,*) "header"
│
├── [loop i = 1,]
│   │  write (107,*) pl_prms(i)%name, pl_prms(i)%lum_num, pl_prms(i)%parms,
│   │  write (107,*) "header"
│   
│   │  !! read landscape soft calibration data for each land use and parameter
│   
│   ├── [if pl_prms(i)%lum_num > 0]
│      │  ilum_mx = pl_prms(i)%lum_num * pl_prms(i)%parms
│      
│      ├── [loop ilum = 1,]
│         
│         ├── [select case (pl_prms(i)%prm(ilum)%var)]
│         │  pl_prms(i)%prm(ilum)%init_val = pl_prms(i)%prm(ilum)%init_val + plcal(i)
│         │  pl_prms(i)%prm(ilum)%init_val = amin1 (pl_prms(i)%prm(ilum)%init_val, pl
│         │  pl_prms(i)%prm(ilum)%init_val = Max (pl_prms(i)%prm(ilum)%init_val, pl_p
│      
│      │  !! write parameters to file
│
▼
</pre>


---

## re_initialize

**Called from:** [`calhard_control`](#calhard_control), [`calsoft_hyd`](#calsoft_hyd), [`calsoft_hyd_bfr_et`](#calsoft_hyd_bfr_et), [`calsoft_hyd_bfr_latq`](#calsoft_hyd_bfr_latq), [`calsoft_hyd_bfr_perc`](#calsoft_hyd_bfr_perc), [`calsoft_hyd_bfr_pet`](#calsoft_hyd_bfr_pet), [`calsoft_hyd_bfr_surq`](#calsoft_hyd_bfr_surq), [`calsoft_plant`](#calsoft_plant), [`calsoft_sed`](#calsoft_sed)

Source: `re_initialize.f90`

<pre>
re_initialize
│
│  !! reset basin soil water for next simulation
│  pco%sw_init = "n"
│
│  !! re-initialize all hru parameters
│
├── [if sp_ob%hru > 0]
│   │  hru = hru_init
│   │  soil = soil_init
│   │  soil1 = soil1_init
│   │  pcom = pcom_init
│   │  pl_mass = pl_mass_init
│   │  wet = wet_om_init
│
│  !! re-initialize hru_lte parameters
│
├── [if sp_ob%hru_lte > 0]
│   │  hlt = hlt_init
│
│  !! re-initialize channel lte storage and dimensions
│
├── [if sp_ob%chandeg > 0]
│   │  sd_ch = sdch_init
│   │  ch_stor = ch_om_water_init
│
│  !! re-initialize reservoir storage
│
├── [if sp_ob%res > 0]
│
│  !! re-initialize aquifer storage
│
├── [if sp_ob%aqu > 0]
│
▼
</pre>


---

## copy_file

**Called from:** [`swift_output`](#swift_output)

Source: `copy_file.f90`

<pre>
copy_file
│  character(len=*), intent(in) :: source, destination
│  character(len=32000) :: line
│
│  !! Check if the source file exists
│  inquire (file=source, exist=i_exist)
│
├── [if .not. i_exist]
│
│  !! Open the source and destination files
│  open(unit=107, file=source, status='old', action='read')
│  open(unit=1007, file=destination, status='replace', action='write', recl
│
│  !! Copy the contents of the source file to the destination file
│  read(107, '(A)', iostat=eof) line
│  write(1007, '(A)') trim(line)
│  close(107)
│  close(1007)
│
▼
</pre>


---

## hyd_convert_mass_to_conc

**Called from:** [`swift_output`](#swift_output)

Source: `hydrograph_module.f90`

<pre>
hyd_convert_mass_to_conc
│  hyd1%flo = hyd1%flo
│
├── [if hyd1%flo > 0.01]
│   │  hyd1%sed = 1000000. * hyd1%sed / hyd1%flo
│   │  hyd1%orgn = 1000. * hyd1%orgn / hyd1%flo
│   │  hyd1%sedp = 1000. * hyd1%sedp / hyd1%flo
│   │  hyd1%no3 = 1000. * hyd1%no3 / hyd1%flo
│   │  hyd1%solp = 1000. * hyd1%solp / hyd1%flo
│   │  hyd1%chla = 1000. * hyd1%chla / hyd1%flo
│   │  hyd1%nh3 = 1000. * hyd1%nh3 / hyd1%flo
│   │  hyd1%no2 = 1000. * hyd1%no2 / hyd1%flo
│   │  hyd1%cbod = 1000. * hyd1%cbod / hyd1%flo
│
▼
</pre>


---

## init_output_path

**Called from:** [`readcio_read`](#readcio_read)

Source: `output_path_module.f90`

<pre>
init_output_path
│  character(len=*), intent(in) :: path_in
│  character(len=256) :: path_work, path_mkdir
│  character(len=512) :: cmd
│  character(len=32) :: os_env
│
│  !! Detect OS - Runtime check is more robust if preprocessor fails
│  is_windows = .false.
│  call get_environment_variable("OS", os_env, status=stat)
│
├── [if stat == 0 .and. index(os_env, "Windows") > 0]
│   │  is_windows = .true.
│
│  !! If null or empty, use current directory (no prefix needed)
│
├── [if trim(path_in) == "null" .or. trim(path_in) == ]
│   │  out_path = ""
│
│  !! Copy and process the path
│  path_work = trim(adjustl(path_in))
│  path_len = len_trim(path_work)
│
│  !! Handle path separators based on OS
│
├── [if is_windows]
│   
│   │  !! On Windows: convert forward slashes to backslashes
│   
│   ├── [loop i = 1,]
│   
│   │  !! If so, prepend C:
│   
│   ├── [if path_in(1:1) == '/']
│   
│   │  !! Remove trailing backslash(es) for consistent processing
│   
│   ├── [do while (path_len > 0 .and. path_work(path_len:path)]
│   
│   │  !! On Unix: convert backslashes to forward slashes
│   
│   ├── [loop i = 1,]
│   
│   │  !! Check if this looks like a Windows path on Unix
│   
│   ├── [if index(path_work, ':') > 0]
│   
│   │  !! Remove trailing slash(es) for consistent processing
│   
│   ├── [do while (path_len > 1 .and. path_work(path_len:path)]
│
▼
</pre>


---

## gcycl

This subroutine initializes the random number seeds. If the user

**Called from:** [`cli_wgnread`](#cli_wgnread)

Source: `gcycl.f90`

<pre>
gcycl
│
│  !! SWAT: Aunif
│
│  !! initialize random number array locator
│  idg = (/1,2,3,4,5,6,7,8,9/)
│
│  !! initialize random number seeds
│
├── [loop iwgn = 1,]
│   │  rndseed(1,iwgn) = 748932582
│   │  rndseed(2,iwgn) = 1985072130
│   │  rndseed(3,iwgn) = 1631331038
│   │  rndseed(4,iwgn) = 67377721
│   │  rndseed(5,iwgn) = 366304404
│   │  rndseed(6,iwgn) = 1094585182
│   │  rndseed(7,iwgn) = 1767585417
│   │  rndseed(8,iwgn) = 1980520317
│   │  rndseed(9,iwgn) = 392682216
│
├── [if bsn_prm%igen /= 0]
│   
│   │  !! assign new random number seeds
│   
│   ├── [loop j = 1,]
│      
│      ├── [loop k = 1,]
│   
│   │  !! assign random number for decision table conditional
│   
│   │  !! shuffle seeds randomly (Bratley, Fox, Schrage, p34)
│   
│   ├── [loop j = 9,]
│
│  !! in sub-daily pcp generator
│
├── [loop iwgn = 1,]
│
├── [loop iwgn = 1,]
│
▼
</pre>


---

## cli_initwgn

this subroutine initializes the HRU weather generator parameters from the

**Called from:** [`cli_wgnread`](#cli_wgnread)

Source: `cli_initwgn.f90`

<pre>
cli_initwgn
│
│  !! SWAT: Aunif, Dstn1
│  real, dimension (12) :: rain_hhsm = 0.
│
│  !! variables needed for radiation calcs.
│  xx = wgn(iwgn)%lat / 57.296
│  wgn_pms(iwgn)%latsin = Sin(xx)
│  wgn_pms(iwgn)%latcos = Cos(xx)
│  lattan = Tan(xx)
│
│  !! the angular velocity of the earth's rotation, omega, = 15 deg/hr or 0.2618 rad/hr and 2/
│  x1 = .4348 * Abs(lattan)
│  wgn_pms(iwgn)%daylmn = 7.6394 * x2
│
│  !! calculate day length threshold for dormancy
│
├── [if bsn_prm%dorm_hr < 1.e-6]
│   
│   ├── [if Abs(wgn(iwgn)%lat) > 40.]
│      │  dl = (Abs(wgn(iwgn)%lat) - 20.) / 20.
│   │  dl = bsn_prm%dorm_hr
│  wgn_pms(iwgn)%daylth = dl
│
│  !! calculate smoothed maximum 0.5hr rainfall amounts
│
├── [loop mon = 2,]
│
│  !! calculate missing values and additional parameters
│
├── [loop mon = 1,]
│
▼
</pre>


---

## read_mgtops

Reads `read_mgtops` input data from file.

**Called from:** [`mgt_read_mgtops`](#mgt_read_mgtops)

Source: `read_mgtops.f90`


---

## search

**Called from:** [`cli_staread`](#cli_staread), [`hyd_read_connect`](#hyd_read_connect)

Source: `search.f90`

<pre>
search
│  character(len=50) :: cfind
│  character(len=50), dimension(max) :: sch
│
├── [loop int = 1,]
│   │  nn = (nl - nf) / 2 + nf
│   
│   ├── [if nl - nf == 1]
│      
│      ├── [if sch(nl) == cfind]
│         │  iseq = nl
│      
│      ├── [if sch(nf) == cfind]
│         │  iseq = nf
│   
│   ├── [if sch(nn) == cfind]
│      │  iseq = nn
│   
│   ├── [if sch(nn) > cfind]
│   
│   ├── [if sch(nn) < cfind]
│
▼
</pre>


---

## hru_lum_init

**Called from:** [`actions`](#actions), [`hru_lum_init_all`](#hru_lum_init_all)

Source: `hru_lum_init.f90`

<pre>
hru_lum_init
│
│  !! assign land use pointers for the hru
│  ilu = hru(iihru)%land_use_mgt
│  pcom(iihru)%name = lum(ilu)%plant_cov
│  hru(iihru)%plant_cov = lum_str(ilu)%plant_cov
│  hru(iihru)%lum_group_c = lum(ilu)%cal_group
│
├── [loop ilug = 1,]
│   
│   ├── [if hru(iihru)%lum_group_c == lum_grp%name(ilu)]
│      │  hru(iihru)%lum_group =  ilug
│  iob = hru(iihru)%obj_no
│  iwst = ob(iob)%wst
│  iwgn = wst(iwst)%wco%wgn
│  isched = lum_str(ilu)%mgt_ops
│  hru(iihru)%mgt_ops = lum_str(ilu)%mgt_ops
│
▼
</pre>


---

## ttcoef_wway

this subroutine computes travel time coefficients for routing

**Called from:** [`structure_set_parms`](#structure_set_parms)

Source: `ttcoef_wway.f90`

<pre>
ttcoef_wway
│
│  !! If it is main reach default side slope to 2:1 if it is a waterway default to 8:1
│  d = hru(k)%lumv%grwat_d
│  b = hru(k)%lumv%grwat_w - 2. * d * chsslope
│
│  !! check if bottom width (b) is < 0
│
├── [if b <= 0.]
│   │  b = .5 * hru(k)%lumv%grwat_w
│   │  chsslope = (hru(k)%lumv%grwat_w - b) / (2. * d)
│  grwway_vel(k)%wid_btm = b
│  grwway_vel(k)%dep_bf = d
│
│  !! compute flow and travel time at bankfull depth
│  p = b + 2. * d * Sqrt(chsslope * chsslope + 1.)
│  a = b * d + chsslope * d * d
│  rh = a / p
│  grwway_vel(k)%area = a
│
│  !! compute flow and travel time at 1.2 bankfull depth
│
│  !! compute flow and travel time at 0.1 bankfull depth
│
▼
</pre>


---

## soils_test_adjust

Adjust the input soil values based input soil test values.

**Called from:** [`soils_init`](#soils_init)

Source: `soils_test_adjust.f90`

<pre>
soils_test_adjust
│  first_lr = .true.
│
├── [loop test = 1,]
│   
│   ├── [if sol_test(test)%snam == sol(isol)%s%snam]
│      
│      ├── [if first_lr]
│         │  first_lr = .false.
│         │  tot_soil_depth = soildb(isol)%ly(soildb(isol)%s%nly)%z
│         
│         ├── [loop j = 1,]
│         
│         ├── [loop i = prev_depth]
│         
│         ├── [if i <= soildb(isol)%ly(j)%z .and. i > prev_depth]
│         │  sol_mm_db(1)%ly(i)%z = i
│         │  sol_mm_db(1)%ly(i)%bd = soildb(isol)%ly(j)%bd
│         │  sol_mm_db(1)%ly(i)%awc = soildb(isol)%ly(j)%awc
│         │  sol_mm_db(1)%ly(i)%k = soildb(isol)%ly(j)%k
│         │  sol_mm_db(1)%ly(i)%cbn = soildb(isol)%ly(j)%cbn
│         │  sol_mm_db(1)%ly(i)%clay = soildb(isol)%ly(j)%clay
│         │  sol_mm_db(1)%ly(i)%silt = soildb(isol)%ly(j)%silt
│
▼
</pre>


---

## soil_phys_init

this subroutine initializes soil physical properties

**Called from:** [`soils_init`](#soils_init)

Source: `soil_phys_init.f90`

<pre>
soil_phys_init
│
│  !! SWAT: Curno
│  nly = sol(isol)%s%nly
│
├── [loop j = 1,]
│   
│   ├── [if sol(isol)%phys(j)%k <= 0.0]
│      
│      ├── [if sol(isol)%s%hydgrp == "A"]
│         │  sol(isol)%phys(j)%k = a
│         │  else if (sol(isol)%s%hydgrp == "B") then
│         │  sol(isol)%phys(j)%k = b
│         │  else if (sol(isol)%s%hydgrp == "C") then
│         │  sol(isol)%phys(j)%k = c
│         │  else if (sol(isol)%s%hydgrp == "D") then
│         │  sol(isol)%phys(j)%k = d
│         │  sol(isol)%phys(j)%k = nota
│   
│   │  !! Defaults for ph and calcium mjw average of 20,000 SSURGO soils mjw rev 490
│  nly = sol(isol)%s%nly
│
│  !! calculate water content of soil at -1.5 MPa and -0.033 MPa
│
├── [loop j = 1,]
│   
│   ├── [if sol(isol)%phys(j)%up >= sol(isol)%phys(j)%por]
│
▼
</pre>


---

## layersplit

**Called from:** [`soils_init`](#soils_init)

Source: `layersplit.f90`

<pre>
layersplit
│  nly = soil(ihru)%nly
│
├── [loop ly = 1,]
│   │  layer1(ly) = soil(ihru)%ly(ly)
│   │  phys1(ly) = soil(ihru)%phys(ly)
│
├── [loop ly = 2,]
│   
│   │  !! set a soil layer at dep_new and adjust all lower layer
│   
│   ├── [if phys1(ly)%d > dep_new]
│      │  soil(ihru)%nly = soil(ihru)%nly + 1
│      │  nly1 = soil(ihru)%nly
│      
│      ├── [loop lyn = 1,]
│         │  soil(ihru)%ly(lyn) = layer1(lyn)
│         │  soil(ihru)%phys(lyn) = phys1(lyn)
│         
│         ├── [if lyn == ly]
│         │  soil(ihru)%phys(lyn)%d = dep_new
│         │  soil(ihru)%phys(lyn)%thick = dep_new - soil(ihru)%phys(lyn-1)%d
│   
│   ├── [loop lyn = ly,]
│      │  soil(ihru)%ly(lyn+1) = layer1(lyn)
│      
│      ├── [if lyn == ly]
│
▼
</pre>


---

## sd_rating_curve

**Called from:** [`sd_hydsed_init`](#sd_hydsed_init)

Source: `sd_rating_curve.f90`

<pre>
sd_rating_curve
│  b = sd_ch(i)%chw - 2. * sd_ch(i)%chd * sd_ch(i)%chss
│
│  !! check if bottom width (b) is < 0
│
├── [if b <= 0.]
│   │  b = .5 * sd_ch(i)%chw
│   │  b = Max(0., b)
│   │  sd_ch(i)%chss = (sd_ch(i)%chw - b) / (2. * sd_ch(i)%chd)
│
│  !! compute rating curve at 0.1 and 1.0 times bankfull depth
│
├── [loop i_dep = 1,]
│   
│   │  !! c^2=a^2+b^2 - a=dep; a/b=slope; b^2=a^2/slope^2
│   │  p = b + 2. * Sqrt(dep ** 2 * (1. + 1. / (sd_ch(i)%chss ** 2)))
│   │  a = b * dep + dep / sd_ch(i)%chss
│   │  rh = a / p
│   │  ch_rcurv(i)%elev(i_dep)%dep = dep
│   │  ch_rcurv(i)%elev(i_dep)%wet_perim = p
│   │  ch_rcurv(i)%elev(i_dep)%xsec_area = a
│   
│   │  !! save bankfull depth and area for flood plain calculations
│   
│   ├── [if i_dep == 2]
│
│  !! compute rating curve at 1.2 and 2.0 times bankfull depth (flood plain)
│
├── [loop i_dep = 1,]
│   
│   │  !! dep = depth above bankfull
│   
│   │  !! flood plain perimeter - p^2 = dep^2 + width^2
│   
│   │  !! flood plain cross section area - dep*width = dep^2 / slope (slope = dep/width)
│
▼
</pre>


---

## hyd_convert_conc_to_mass

**Called from:** [`sd_hydsed_init`](#sd_hydsed_init), [`wallo_treatment`](#wallo_treatment), [`wallo_use`](#wallo_use)

Source: `hydrograph_module.f90`

<pre>
hyd_convert_conc_to_mass
│  hyd1%flo = hyd1%flo
│  hyd1%sed = hyd1%sed * hyd1%flo / 1000000.
│  hyd1%orgn = hyd1%orgn * hyd1%flo / 1000.
│  hyd1%sedp = hyd1%sedp * hyd1%flo / 1000.
│  hyd1%no3 = hyd1%no3 * hyd1%flo / 1000.
│  hyd1%solp = hyd1%solp * hyd1%flo / 1000.
│  hyd1%chla = hyd1%chla * hyd1%flo / 1000.
│  hyd1%nh3 = hyd1%nh3 * hyd1%flo / 1000.
│  hyd1%no2 = hyd1%no2 * hyd1%flo / 1000.
│  hyd1%cbod = hyd1%cbod * hyd1%flo / 1000.
│
▼
</pre>


---

## res_convert_mass

**Called from:** [`res_initial`](#res_initial), [`wet_initial`](#wet_initial)

Source: `hydrograph_module.f90`

<pre>
res_convert_mass
│  hyd1%flo = hyd1%flo * pvol
│  hyd1%sed = hyd1%sed * hyd1%flo / 1000000.
│  hyd1%orgn = hyd1%orgn * hyd1%flo / 1000.
│  hyd1%sedp = hyd1%sedp * hyd1%flo / 1000.
│  hyd1%no3 = hyd1%no3 * hyd1%flo / 1000.
│  hyd1%solp = hyd1%solp * hyd1%flo / 1000.
│  hyd1%chla = hyd1%chla * hyd1%flo / 1000.
│  hyd1%nh3 = hyd1%nh3 * hyd1%flo / 1000.
│  hyd1%no2 = hyd1%no2 * hyd1%flo / 1000.
│  hyd1%cbod = hyd1%cbod * hyd1%flo / 1000.
│
▼
</pre>


---

## wallo_transfer

**Called from:** [`wallo_control`](#wallo_control)

Source: `wallo_transfer.f90`

<pre>
wallo_transfer
│
│  !! transfer water to receiving object from each source
│
├── [loop isrc = 1,]
│   │  iconv = wallo(iwallo)%trn(itrn)%src(isrc)%conv_num
│   
│   ├── [select case (wallo(iwallo)%trn(itrn)%src(is)]
│      
│      │  !! organic hydrograph being transfered from the source to the receiving object
│      │  wal_omd(iwallo)%trn(itrn)%src(isrc)%hd = (1. - pipe(iconv)%loss_fr) *   
│      
│      │  !! include pump losses here
│
▼
</pre>


---

## salt_irrig

this subroutine adds salt mass from irrigation water into the soil profile, and removes salt mass

**Called from:** [`wallo_control`](#wallo_control)

Source: `salt_irrig.f90`

<pre>
salt_irrig
│
│  !! from the source object
│
├── [loop isrc=1,irrig_nsource]
│   │  irrig_type = wallo(iwallo)%trn(itrn)%src(isrc)%typ
│   │  irrig_ob = wallo(iwallo)%trn(itrn)%src(isrc)%num
│   │  irrig_volume = wallod_out(iwallo)%trn(itrn)%src(isrc)%withdr
│   
│   ├── [if irrig_volume > 0]
│      
│      ├── [if irrig_type.eq.'res']
│         │  ires = irrig_ob
│         
│         ├── [loop isalt=1,cs_db%num_salts]
│         │  ion_mass = (res_water(ires)%saltc(isalt)*irrig_volume) / 1000.
│         │  res_mass = res_water(ires)%salt(isalt)
│         
│         ├── [if ion_mass.gt.res_mass]
│         │  mass_diff = ion_mass - res_mass
│      │  ion_mass = ion_mass - mass_diff
│      │  if(ion_mass.lt.0) ion_mass = 0.
│      │  res_water(ires)%salt(isalt) = res_water(ires)%salt(isalt) - ion_mass
│      
│      ├── [if wetland > 0]
│   
│   ├── [loop isalt=1,cs_db%num_salts]
│      
│      ├── [if ion_mass.gt.cs_aqu(iaq)%salt(isalt)]
│
▼
</pre>


---

## cs_irrig

this subroutine adds constituent mass from irrigation water into the soil profile, and removes constituent mass

**Called from:** [`wallo_control`](#wallo_control)

Source: `cs_irrig.f90`

<pre>
cs_irrig
│
│  !! from the source object
│
├── [loop isrc=1,irrig_nsource]
│   │  irrig_type = wallo(iwallo)%trn(itrn)%src(isrc)%typ
│   │  irrig_ob = wallo(iwallo)%trn(itrn)%src(isrc)%num
│   │  irrig_volume = wallod_out(iwallo)%trn(itrn)%src(isrc)%withdr
│   
│   ├── [if irrig_volume > 0]
│      
│      ├── [if irrig_type.eq.'res']
│         │  ires = irrig_ob
│         
│         ├── [loop ics=1,cs_db%num_cs]
│         │  ion_mass = (res_water(ires)%csc(ics)*irrig_volume) / 1000.
│         │  res_mass = res_water(ires)%cs(ics)
│         
│         ├── [if ion_mass.gt.res_mass]
│         │  mass_diff = ion_mass - res_mass
│      │  ion_mass = ion_mass - mass_diff
│      │  if(ion_mass.lt.0) ion_mass = 0.
│      │  res_water(ires)%cs(ics) = res_water(ires)%cs(ics) - ion_mass
│      
│      ├── [if wetland > 0]
│   
│   ├── [loop ics=1,cs_db%num_cs]
│      
│      ├── [if ion_mass.gt.cs_aqu(iaq)%cs(ics)]
│
▼
</pre>


---

## wallo_canal

Routes water through a wallo canal: computes outflow, applies loss,

**Called from:** [`wallo_control`](#wallo_control)

Source: `wallo_canal.f90`

<pre>
wallo_canal
│
│  !! and distributes canal seepage to aquifer (gwflow grid cells or 1-D aquifer).
│
│  !! compute outflow from canal using decision table or simple lag
│
├── [if canal(ican)%dtbl == "null"]
│   
│   │  !! simple drawdown days
│   │  wallod_out(iwallo)%trn(itrn)%trn_flo = canal_om_stor(ican)%flo / canal(i
│   
│   │  !! decision table to condition outflow from canal
│
│  !! outflow is the fraction of the withdrawal from the canal
│  canal_om_out(ican) = (wallod_out(iwallo)%trn(itrn)%trn_flo / canal_om_st
│
│  !! subtract amount that is removed
│  canal_om_stor(ican) = canal_om_stor(ican) - canal_om_out(ican)
│
│  !! compute canal loss volume
│  canal_loss_vol = canal(ican)%loss_fr * canal_om_out(ican)%flo
│
│  !! outflow to receiving object (after loss)
│  outflo_om = (1. - canal(ican)%loss_fr) * canal_om_out(ican)
│
│  !! route canal loss to aquifer
│
├── [if canal_loss_vol > 0.]
│   
│   ├── [if bsn_cc%gwflow == 1 .and. gw_canal_flag == 1]
│      
│      │  !! cell connections read from gwflow.canals, indexed by canal_id matching ican
│      
│      ├── [loop ic = 1,]
│         
│         ├── [if gw_canl_div_cell(ic)%canal_id == ican]
│         │  total_length = total_length + gw_canl_div_cell(ic)%leng
│   
│   ├── [if total_length > 0.]
│      
│      ├── [loop ic = 1,]
│         
│         ├── [if gw_canl_div_cell(ic)%canal_id == ican]
│         │  cell_id = gw_canl_div_cell(ic)%cell_id
│         
│         ├── [if gw_state(cell_id)%stat == 1]
│         │  cell_frac = gw_canl_div_cell(ic)%leng / total_length
│         │  cell_seep = canal_loss_vol * cell_frac
│         
│         │  !! add seepage to cell storage and flux tracking
│         │  gw_state(cell_id)%stor = gw_state(cell_id)%stor + cell_seep
│
│  !! no cell connections for this canal -- fall back to 1-D aquifer
│
├── [loop iaq = 1,]
│   
│   ├── [if iaqu_ob > 0 .and. iaqu_ob <= sp_ob%aqu]
│
▼
</pre>


---

## varinit

this subroutine initializes variables for the daily simulation of the

**Called from:** [`hru_control`](#hru_control)

Source: `varinit.f90`

<pre>
varinit
│
│  !! name        |units         |definition
│
│  !! initialize hru variables - modular code
│
├── [loop ly = 1,]
│   │  soil(j)%ly(ly)%prk = 0.
│   │  soil(j)%ly(ly)%flat = 0.
│
│  !! initialize variables NUBS - all these need to be checked
│
▼
</pre>


---

## pl_fert

this subroutine applies N and P specified by date and

**Called from:** [`actions`](#actions), [`hru_control`](#hru_control), [`mallo_control`](#mallo_control), [`mgt_sched`](#mgt_sched)

Source: `pl_fert.f90`

<pre>
pl_fert
│  organic_flag = .false.
│  org_frt%m = 0.
│  org_frt%c = 0.
│  org_frt%n = 0.
│  org_frt%p = 0.
│  rtof = man_coef%rtof
│
│  !! calculate c:n ratio for manure applications for SWAT-C
│
├── [if bsn_cc%cswat == 1]
│   
│   ├── [if fertdb(ifrt)%forgn > 0. .or. fertdb(ifrt)%forg]
│      │  organic_flag = .true.
│
├── [if organic_flag]
│   │  org_frt%m = frt_kg
│   │  org_frt%c = fertdb(ifrt)%forgn * frt_kg * 10.0
│   │  org_frt%n = fertdb(ifrt)%forgn * frt_kg
│   
│   │  !! meta_fr is the fraction of fertilizer that is allocated to metabolic litter pool
│
├── [if meta_fr < 0.01]
│   
│   ├── [if meta_fr > .7]
│
│  !! add fertilizer to first and/or second layer
│
├── [loop l = 1,]
│   
│   ├── [if l == 1]
│   
│   │  !! add mineral n and p for all methods
│
▼
</pre>


---

## albedo

this subroutine calculates albedo in the HRU for the day

**Called from:** [`hru_control`](#hru_control)

Source: `albedo.f90`

<pre>
albedo
│
│  !! this subroutine calculates albedo in the HRU for the day
│
│  !! calculate albedo
│  cej = -5.e-5
│  cover = pl_mass(j)%ab_gr_com%m + pl_mass(j)%rsd_tot%m
│  eaj = Exp(cej * (cover + .1))
│
├── [if hru(j)%sno_mm <= .5]
│   
│   │  !! equation 2.2.14 in SWAT manual
│   │  albday = soil(j)%ly(1)%alb
│   
│   │  !! equation 2.2.15 in SWAT manual
│   
│   │  !! equation 2.2.13 in SWAT manual
│   │  albday = 0.8
│
▼
</pre>


---

## cs_sorb_hru

this subroutine updates constituent concentrations based on sorption in the soil profile

**Called from:** [`hru_control`](#hru_control)

Source: `cs_sorb_hru.f90`

<pre>
cs_sorb_hru
│
│  !! this subroutine updates constituent concentrations based on sorption in the soil profile
│  hru_area_m2 = hru(j)%area_ha * 10000.
│
├── [loop jj = 1,soil(j)%nly]
│   │  sol_thick = soil(j)%phys(jj)%thick
│   │  soil_volume = hru_area_m2 * (sol_thick/1000.)
│   │  soil_mass = soil_volume * (soil(j)%phys(jj)%bd*1000.)
│   │  sorbed_seo4 = cs_soil(j)%ly(jj)%cs_sorb(1)
│   │  sorbed_seo3 = cs_soil(j)%ly(jj)%cs_sorb(2)
│   │  sorbed_born = cs_soil(j)%ly(jj)%cs_sorb(3)
│   │  mass_seo4_sorb = sorbed_seo4 * 1.e6 * hru(j)%area_ha
│   │  mass_seo3_sorb = sorbed_seo3 * 1.e6 * hru(j)%area_ha
│   │  mass_born_sorb = sorbed_born * 1.e6 * hru(j)%area_ha
│
├── [loop jj = 1,soil(j)%nly]
│
▼
</pre>


---

## stmp_solt

this subroutine estimates daily average temperature at the bottom

**Called from:** [`hru_control`](#hru_control)

Source: `stmp_solt.f90`

<pre>
stmp_solt
│
│  !! Intrinsic: Exp, Log, Max, Min
│  tlag = 0.8
│
│  !! SWAT manual equation 2.3.6
│  f = soil(j)%avbd / (soil(j)%avbd + 686. * Exp(-5.63 *               soil
│  dp = 1000. + 2500. * f
│
│  !! SWAT manual equation 2.3.7
│  ww = .356 - .144 * soil(j)%avbd
│  wc = soil(j)%sw / (ww * soil(j)%phys(soil(j)%nly)%d)
│
│  !! SWAT manual equation 2.3.8
│  b = Log(500. / dp)
│  f = Exp(b * ((1. - wc) / (1. + wc))**2)
│  dd = f * dp
│
│  !! SWAT manual equation 2.3.11
│  cover = pl_mass(j)%ab_gr_com%m + pl_mass(j)%rsd_tot%m
│  bcv = cover / (cover + Exp(7.563 - 1.297e-4 * cover))
│
├── [if hru(j)%sno_mm /= 0.]
│   
│   ├── [if hru(j)%sno_mm <= 120.]
│
│  !! calculate temperature at soil surface
│
│  !! SWAT manual equation 2.3.10
│
│  !! SWAT manual equation 2.3.9
│
│  !! SWAT manual equation 2.3.12
│
│  !! in summer due to high biomass
│
│  !! calculate temperature for each layer on current day
│
├── [loop k = 1,]
│
▼
</pre>


---

## sq_canopyint

this subroutine computes canopy interception of rainfall

**Called from:** [`hru_control`](#hru_control)

Source: `sq_canopyint.f90`

<pre>
sq_canopyint
│  iob = hru(j)%obj_no
│  iwst = ob(iob)%wst
│
├── [if time%step > 1]
│   │  canstori = canstor(j)
│   │  canmxl = hru(j)%hyd%canmx * pcom(j)%lai_sum / pcom(j)%laimx_sum
│   
│   ├── [loop ii = 1,]
│      │  xx = w%ts(ii)
│      │  w%ts(ii) = w%ts(ii) - (canmxl - canstor(j))
│      
│      ├── [if w%ts(ii) < 0.]
│         │  canstor(j) = canstor(j) + xx
│         │  w%ts(ii) = 0.
│         │  canstor(j) = canmxl
│   
│   ├── [if canstor(j) > canstori]
│      
│      ├── [loop ii = 1,]
│         │  xx = wst(iwst)%weat%ts(ii)
│         
│         ├── [if w%ts(ii) < 0.]
│
│  !! check if precip_eff is less than remaining canopy storage
│
├── [if precip_eff < canmxl - canstor(j)]
│
▼
</pre>


---

## sq_snom

this subroutine predicts daily snom melt when the average air

**Called from:** [`hru_control`](#hru_control)

Source: `sq_snom.f90`

<pre>
sq_snom
│
│  !! Intrinsic: Real, Sin, Exp
│
│  !! estimate snow pack temperature
│  snotmp = snotmp * (1. - hru(j)%sno%timp) + w%tave * hru(j)%sno%timp
│
├── [if w%tave <= hru(j)%sno%falltmp]
│   
│   │  !! calculate snow fall
│   │  hru(j)%sno_mm = hru(j)%sno_mm + precip_eff
│   │  snofall = precip_eff
│   
│   │  !! set subdaily effective precip to zero
│
├── [if w%tmax > hru(j)%sno%melttmp .and. hru(j)%sno_m]
│   
│   │  !! adjust melt factor for time of year
│   │  smfac = (hru(j)%sno%meltmx + hru(j)%sno%meltmn) / 2. + Sin((time%day - 8
│   │  snomlt = smfac * (((snotmp + w%tmax)/2.) - hru(j)%sno%melttmp)
│   
│   │  !! adjust for areal extent of snow cover
│   
│   ├── [if hru(j)%sno_mm < hru(j)%sno%covmx]
│      │  rto_sno = hru(j)%sno_mm / hru(j)%sno%covmx
│      │  snocov = rto_sno / (rto_sno + Exp(hru(j)%snocov1 - hru(j)%snocov2 * rto_
│   │  snomlt = snomlt * snocov
│   │  hru(j)%sno_mm = hru(j)%sno_mm - snomlt
│   │  precip_eff = precip_eff + snomlt
│   
│   ├── [if time%step > 1]
│
▼
</pre>


---

## rls_routesurf

name        |units         |definition

**Called from:** [`hru_control`](#hru_control)

Source: `rls_routesurf.f90`

<pre>
rls_routesurf
│
│  !! ht2== outflow from inflow: added to hru generated flows
│  ifield = hru(j)%dbs%field
│
│  !! compute infiltration from surface runon and tile flow to next landscape unit
│  ls_overq = ob(iob)%hin_sur%flo + (ob(iob)%hin_til%flo * tile_fr_surf) / 
│  precip_eff = precip_eff + ls_overq
│
│  !! add surface runon to subdaily effective precip
│
├── [if time%step > 1]
│   │  w%ts(:) = w%ts(:) + ls_overq / time%step
│
│  !! sediment deposition across the landscape
│  sed = ob(iob)%hin_sur%sed / hru(j)%area_ha
│
│  !! use surface runoff (mm) for eiq - m3/(10 * ha) = mm
│  trancap = hru(j)%topo%dep_co * usle_cfac(j) * ls_overq *                
│
├── [if sed > trancap]
│   │  ht1%sed = (sed - trancap) * hru(j)%area_ha
│   │  ht2%sed = trancap * hru(j)%area_ha
│   │  ht1%sed = 0.
│   │  ht2%sed = sed * hru(j)%area_ha
│
│  !! less persistent bacteria not routed
│
▼
</pre>


---

## rls_routesoil

name        |units         |definition

**Called from:** [`hru_control`](#hru_control)

Source: `rls_routesoil.f90`

<pre>
rls_routesoil
│
│  !! name        |units         |definition
│  latqrunon = ob(iob)%hin_lat%flo
│
├── [if latqrunon > 1.e-9]
│   
│   │  !! put in soil layers - weighted by depth of soil layer
│   
│   ├── [loop lyr = 1,]
│      │  latqlyr = (soil(j)%phys(lyr)%thick / soil(j)%phys(soil(j)%nly)%d) * latq
│      │  soil(j)%phys(lyr)%st = soil(j)%phys(lyr)%st + latqlyr
│   
│   │  !! excess above ul is added to surface storage in saturation excess routine
│
▼
</pre>


---

## rls_routetile

name        |units         |definition

**Called from:** [`hru_control`](#hru_control)

Source: `rls_routetile.f90`

<pre>
rls_routetile
│
│  !! name        |units         |definition
│
│  !! suppress unused variable warning
│
│  !! if exceeds saturation, it will be redistributed in swr_satexcess
│  lyr = hru(j)%sb%sb_db%lyr
│  soil(j)%phys(lyr)%st = soil(j)%phys(lyr)%st + hru(j)%sb%inflo * (1. - ti
│  soil1(j)%mn(lyr)%no3 = soil1(j)%mn(lyr)%no3 + hru(j)%sb%no3 * (1. - tile
│  hru(j)%sb%inflo = 0.
│  hru(j)%sb%no3 = 0.
│
▼
</pre>


---

## rls_routeaqu

name        |units         |definition

**Called from:** [`hru_control`](#hru_control)

Source: `rls_routeaqu.f90`

<pre>
rls_routeaqu
│
│  !! name        |units         |definition
│
│  !! if exceeds saturation, it will be redistributed in swr_satexcess
│  lyr = soil(j)%nly
│  soil(j)%phys(lyr)%st = soil(j)%phys(lyr)%st + ob(iob)%hin_til%flo
│  soil1(j)%mn(lyr)%no3 = soil1(j)%mn(lyr)%no3 + ob(iob)%hin_til%no3
│
▼
</pre>


---

## sq_crackvol

this surboutine computes total crack volume for the soil profile and

**Called from:** [`hru_control`](#hru_control)

Source: `sq_crackvol.f90`

<pre>
sq_crackvol
│
│  !! name        |units         |definition
│
│  !! calculate volume of cracks in soil
│
├── [loop l = 1,]
│   │  volcrnew = soil(j)%phys(l)%crdep * (soil(j)%phys(l)%fc -                
│   
│   ├── [if soil(j)%sw < .90 * soil(j)%sumfc]
│      
│      ├── [if volcrnew > soil(j)%ly(l)%volcr]
│         │  crlag = crlagdry
│         │  crlag = crlagwet
│      │  crlag = crlagwet
│   │  soil(j)%ly(l)%volcr = crlag * soil(j)%ly(l)%volcr + (1. - crlag) *      
│   │  voltot = voltot + soil(j)%ly(l)%volcr + volcrmin
│
▼
</pre>


---

## et_pot

this subroutine calculates potential evapotranspiration using one

**Called from:** [`hru_control`](#hru_control)

Source: `et_pot.f90`

<pre>
et_pot
│
│  !! initialize local variables
│  tk = w%tave + 273.15
│
│  !! calculate mean barometric pressure
│  pb = 101.3 - hru(j)%topo%elev * (0.01152 - 0.544e-6 * hru(j)%topo%elev)
│
│  !! calculate latent heat of vaporization
│  xl = 2.501 - 2.361e-3 * w%tave
│
│  !! calculate psychrometric constant
│  gma = 1.013e-3 * pb / (0.622 * xl)
│
│  !! vapor pressure deficit
│  ea = Ee(w%tave)
│  ed = ea * w%rhum
│  vpd = ea - ed
│
│  !! calculate the slope of the saturation vapor pressure curve
│  dlt = 4098. * ea / (w%tave + 237.3)**2
│
│  !! DETERMINE POTENTIAL ET
│
├── [select case (bsn_cc%pet)]
│   
│   │  !! calculate net short-wave radiation for PET
│   
│   ├── [if hru(j)%sno_mm <= .5]
│      │  ralb = w%solrad * (1.0 - 0.23)
│      │  ralb = w%solrad * (1.0 - 0.8)
│   
│   │  !! net emissivity  equation 2.2.20 in SWAT manual
│   
│   │  !! cloud cover factor equation 2.2.19
│   
│   ├── [if w%solradmx < 1.e-4]
│   
│   │  !! net long-wave radiation equation 2.2.21
│   
│   │  !! calculate net radiation
│   
│   │  !! net radiation
│   
│   │  !! calculate net short-wave radiation for PET
│   
│   ├── [if hru(j)%sno_mm <= .5]
│   
│   │  !! calculate net short-wave radiation for max plant ET
│   
│   │  !! net emissivity  equation 2.2.20 in SWAT manual
│   
│   │  !! cloud cover factor equation 2.2.19
│   
│   ├── [if w%solradmx  < 1.e-4]
│
▼
</pre>


---

## et_act

this subroutine calculates potential plant transpiration for Priestley-

**Called from:** [`hru_control`](#hru_control)

Source: `et_act.f90`

<pre>
et_act
│
│  !! real, parameter :: esd = 500., etco = 0.80, effnup = 0.1
│  integer:: ires = 0
│  integer:: ihyd = 0
│  pet = pet_day
│
│  !! added statements for test of real statement above
│  etco = 0.80
│  effnup = 0.05
│  ires= hru(j)%dbs%surf_stor
│
│  !! CN methods, canstor will always equal zero.
│  pet = pet - canstor(j)
│
├── [if pet < 0.]
│   │  canstor(j) = -pet
│   │  canev = pet_day
│   │  canev = canstor(j)
│
├── [if pet > 1.0e-6]
│   
│   │  !! compute potential plant evap for methods other that Penman-Monteith
│   
│   ├── [if pcom(j)%lai_sum <= 3.0]
│   
│   │  !! compute potential soil evaporation
│   
│   ├── [if hru(j)%sno_mm >= 0.5]
│
▼
</pre>


---

## wet_irrp

this subroutine checks manual continuous irrigation (irrp) setting for wetland/paddy in the management.sch

**Called from:** [`hru_control`](#hru_control)

Source: `wet_irrp.f90`

<pre>
wet_irrp
│
│  !! apply irrigation depth based on target ponding depth and the current depth
│  wsa1 = hru(j)%area_ha * 10.
│
│  !! store initial values
│  irrig(j)%demand = max(0., hru(j)%irr_hmax - wet_ob(j)%depth*1000. - w%pr
│
├── [if .not. allocated(ob(j)%ru)]
│   │  hru(j)%irr_src = 'unlim'
│   │  iru = ob(j)%ru(1)
│   │  iob = sp_ob%hru + iru
│
├── [if hru(j)%irr_src == 'null']
│   │  hru(j)%irr_src = 'unlim'
│   
│   ├── [if hru(j)%irr_isc > 0]
│      │  isrc = hru(j)%irr_isc
│      
│      ├── [loop i = 1,]
│         
│         ├── [if hru(j)%irr_src == ob(iob)%obtyp_out(i)]
│         │  isrc = ob(iob)%obtypno_out(i)
│
├── [if hru(j)%irr_src == 'cha'.or. hru(j)%irr_src == ]
│   
│   ├── [if ubound(ch_stor,1) > 0 .and. isrc>0]
│      
│      ├── [if ch_stor(isrc)%flo > 0.001]
│         │  rto = min(0.99, irrig(j)%demand / ch_stor(isrc)%flo)
│      │  irrig(j)%water = rto * ch_stor(isrc)
│
▼
</pre>


---

## pl_graze

**Called from:** [`actions`](#actions), [`hru_control`](#hru_control)

Source: `pl_graze.f90`

<pre>
pl_graze
│
│  !! graze only if adequate biomass in HRU
│
├── [loop ipl = 1,]
│   
│   │  !! set initial biomass before eating and trampling
│   │  dmi = pl_mass(j)%ab_gr(ipl)%m
│   
│   │  !! later we can add preferences - by animal type or simply by n and p content
│   │  eat_plant =  graze%eat / pl_mass(j)%ab_gr_com%m
│   │  eat_plant = amin1 (eat_plant, 1.)
│   
│   │  !! update remaining plant organic pools
│   │  pl_mass(j)%seed(ipl) = pl_mass(j)%seed(ipl) - eat_plant * pl_mass(j)%see
│   │  pl_mass(j)%leaf(ipl) = pl_mass(j)%leaf(ipl) - eat_plant * pl_mass(j)%lea
│   │  pl_mass(j)%stem(ipl) = pl_mass(j)%stem(ipl) - eat_plant * pl_mass(j)%ste
│   │  pl_mass(j)%tot(ipl) = pl_mass(j)%tot(ipl) - eat_plant * pl_mass(j)%ab_gr
│   │  pl_mass(j)%ab_gr(ipl) = pl_mass(j)%ab_gr(ipl) - eat_plant * pl_mass(j)%a
│   
│   │  !! remove biomass trampled - assume evenly divided by biomass of plant
│   │  tramp_plant = graze%tramp / pl_mass(j)%ab_gr_com%m
│   
│   │  !! update remaining plant organic pools
│   │  pl_mass(j)%seed(ipl) = pl_mass(j)%seed(ipl) - tramp_plant * pl_mass(j)%s
│   
│   │  !! reset leaf area index and fraction of growing season
│   
│   ├── [if dmi > 1.]
│      
│      │  !! assume lai doesn't start decreasing until 2,500 kg/ha at 1.0 lai per 1000 kg/ha
│      
│      ├── [if dmi < 2500.]
│
│  !! apply manure
│
├── [if manure_kg > 0.]
│   
│   ├── [if bsn_cc%cswat == 0]
│
▼
</pre>


---

## rsd_decomp

this subroutine estimates daily nitrogen and phosphorus

**Called from:** [`hru_control`](#hru_control)

Source: `rsd_decomp.f90`

<pre>
rsd_decomp
│
│  !! Intrinsic: Max, Exp, Sqrt, Min, Abs
│
│  !! zero transformations for summing layers
│  hnb_d(j)%rsd_nitorg_n = 0.
│  hnb_d(j)%rsd_laborg_p = 0.
│
│  !! compute surface residue decomp and mineralization of fresh organic n and p of each layer
│
├── [loop ipl = 1,]
│   
│   │  !! mineralization can occur only if temp above 0 deg
│   
│   ├── [if soil(j)%phys(1)%tmp > 0.]
│      
│      ├── [if pl_mass(j)%rsd(ipl)%n > 1.e-4]
│         │  cnr = pl_mass(j)%rsd(ipl)%c / pl_mass(j)%rsd(ipl)%n
│         │  cnrf = Exp(-.693 * (cnr - 25.) / 25.)
│      
│      ├── [if pl_mass(j)%rsd(ipl)%p > 1.e-4]
│         │  cpr = pl_mass(j)%rsd(ipl)%c / pl_mass(j)%rsd(ipl)%p
│         │  cprf = Exp(-.693 * (cpr - 200.) / 200.)
│      
│      │  !! compute soil water factor
│      │  sut = .1 + .9 * Sqrt(soil(j)%phys(1)%st / soil(j)%phys(1)%fc)
│      │  sut = Max(.05, sut)
│      
│      │  !! compute soil temperature factor
│      │  xx = soil(j)%phys(1)%tmp
│      │  cdg = .9 * xx / (xx + Exp(9.93 - .312 * xx)) + .1
│      
│      │  !! compute combined factor
│      
│      │  !! compute residue decomp and mineralization for each plant
│      
│      │  !! apply decay to total carbon pool for both C models
│
▼
</pre>


---

## nut_nminrl

this subroutine estimates daily nitrogen and phosphorus

**Called from:** [`hru_control`](#hru_control)

Source: `nut_nminrl.f90`

<pre>
nut_nminrl
│
│  !! Intrinsic: Max, Exp, Sqrt, Min, Abs
│  nactfr = .02
│  hnb_d(j)%act_nit_n = 0.
│  hnb_d(j)%org_lab_p = 0.
│  hnb_d(j)%act_sta_n = 0.
│  hnb_d(j)%denit = 0.
│  hnb_d(j)%rsd_nitorg_n = 0.
│  hnb_d(j)%rsd_laborg_p = 0.
│
│  !! compute humus mineralization of organic soil pools
│
├── [loop k = 1,]
│   
│   ├── [if k == 1]
│   
│   │  !! mineralization can occur only if temp above 0 deg
│   
│   ├── [if soil(j)%phys(kk)%tmp > 0.]
│      
│      │  !! compute soil water factor
│      │  sut = .1 + .9 * Sqrt(soil(j)%phys(kk)%st / soil(j)%phys(kk)%fc)
│      │  sut = Max(.05, sut)
│      
│      │  !! compute soil temperature factor
│      │  xx = soil(j)%phys(kk)%tmp
│      
│      │  !! compute combined factor
│      
│      │  !! compute flow from active to stable pools- maintain fraction of active (nactfr)
│      
│      ├── [if rwn > 0.]
│      
│      │  !! compute humus mineralization on active organic n
│      
│      │  !! compute humus mineralization on active organic p
│      
│      ├── [if xx > 1.e-6]
│
▼
</pre>


---
  300/428...
  350/428...
  400/428...

## cbn_surfrsd_decomp

this subroutine estimates daily nitrogen and phosphorus

**Called from:** [`hru_control`](#hru_control)

Source: `cbn_surfrsd_decomp.f90`

<pre>
cbn_surfrsd_decomp
│
│  !! Intrinsic: Max, Exp, Sqrt, Min, Abs
│  nactfr = .02
│  hnb_d(j)%act_nit_n = 0.
│  hnb_d(j)%org_lab_p = 0.
│  hnb_d(j)%act_sta_n = 0.
│  hnb_d(j)%denit = 0.
│  hnb_d(j)%rsd_nitorg_n = 0.
│  hnb_d(j)%rsd_laborg_p = 0.
│
│  !! compute humus mineralization of organic soil pools
│
├── [loop ipl = 1,]
│   │  photo_decomp = photo_degrade_factor * pl_mass(j)%rsd(ipl)
│   │  pl_mass(j)%rsd(ipl) = pl_mass(j)%rsd(ipl) - photo_decomp
│   │  pl_mass(j)%rsd_tot = pl_mass(j)%rsd_tot - photo_decomp
│   
│   ├── [if soil(j)%phys(1)%tmp > 0.]
│      
│      │  !! compute soil water factor
│      
│      │  !! compute soil temperature factor
│      
│      │  !! compute combined factor
│      
│      │  !! compute residue decomp and mineralization of surface residue
│      
│      ├── [if pl_mass(j)%rsd(ipl)%n > 1.e-4]
│      
│      ├── [if pl_mass(j)%rsd(ipl)%p > 1.e-4]
│
▼
</pre>


---

## cbn_rsd_transfer

this subroutine estimates daily nitrogen and phosphorus

**Called from:** [`hru_control`](#hru_control)

Source: `cbn_rsd_transfer.f90`

<pre>
cbn_rsd_transfer
│
│  !! Intrinsic: Max, Exp, Sqrt, Min, Abs
│  nactfr = .02
│  hnb_d(j)%act_nit_n = 0.
│  hnb_d(j)%org_lab_p = 0.
│  hnb_d(j)%act_sta_n = 0.
│  hnb_d(j)%denit = 0.
│  hnb_d(j)%rsd_nitorg_n = 0.
│  hnb_d(j)%rsd_laborg_p = 0.
│
│  !! compute root and incorporated residue decomposition
│
├── [loop k = 1,]
│   
│   ├── [loop ipl = 1,]
│      
│      │  !! mineralization can occur only if temp above 0 deg
│      
│      ├── [if soil(j)%phys(k)%tmp > 0.]
│         │  idp = pcom(j)%plcur(ipl)%idplt
│         │  decr = 1.0
│         │  transfer = decr * soil1(j)%pl(ipl)%rsd(k)
│
▼
</pre>


---

## nut_nitvol

this subroutine estimates daily mineralization (NH3 to NO3)

**Called from:** [`hru_control`](#hru_control)

Source: `nut_nitvol.f90`

<pre>
nut_nitvol
│
│  !! and volatilization of NH3
│
├── [loop k = 1,]
│   │  tf = .41 * (soil(j)%phys(k)%tmp - 5.) / 10.
│   
│   ├── [if soil1(j)%mn(k)%nh4 > 0. .and. tf >= 0.001]
│      │  sw25 = soil(j)%phys(k)%wpmm + 0.25 * soil(j)%phys(k)%fc
│      │  swwp = soil(j)%phys(k)%wpmm + soil(j)%phys(k)%st
│      
│      ├── [if swwp < sw25]
│         │  swf=(swwp-soil(j)%phys(k)%wpmm)/(sw25-soil(j)%phys(k)%wpmm)
│      
│      ├── [if k == 1]
│         │  xx = soil(j)%phys(k-1)%d
│      │  dmidl = (soil(j)%phys(k)%d + xx) / 2.
│      │  dpf = 1. - dmidl / (dmidl + Exp(4.706 - .0305 * dmidl))
│      │  akn = tf * swf
│      │  akv = tf * dpf * cecf
│      │  rnv = soil1(j)%mn(k)%nh4 * (1. - Exp(-akn - akv))
│      
│      │  !! apply septic algorithm only to active septic systems
│      
│      ├── [if k/=i_sep(j).or.sep(isep)%opt /= 1]
│         
│         ├── [if rvol + rnit > 1.e-6]
│      
│      ├── [if soil1(j)%mn(k)%nh4 < 0.]
│
▼
</pre>


---

## nut_pminrl2

this subroutine computes p flux between the labile, active mineral

**Called from:** [`hru_control`](#hru_control)

Source: `nut_pminrl2.f90`

<pre>
nut_pminrl2
│
│  !! Intrinsic: Min
│  hnb_d(j)%lab_min_p = 0.
│  hnb_d(j)%act_sta_p = 0.
│
├── [loop l = 1,]
│   
│   │  !! make sure that no zero or negative pool values come in
│   
│   │  !! Convert kg/ha to ppm so that it is more meaningful to compare between soil layers
│   │  solp = soil1(j)%mp(l)%lab / soil(j)%phys(l)%conv_wt
│   │  actpp = soil1(j)%mp(l)%act / soil(j)%phys(l)%conv_wt
│   │  stap = soil1(j)%mp(l)%sta / soil(j)%phys(l)%conv_wt
│   
│   │  !! PSP = -0.045*log (% clay) + 0.001*(Solution P, mg kg-1) - 0.035*(% Organic C) + 0.43
│   
│   ├── [if soil(j)%phys(l)%clay > 0.]
│      │  psp = -0.045 * log(soil(j)%phys(l)%clay)+ (0.001 * solp)
│      │  psp = psp - (0.035  * soil1(j)%cbn(l)) + 0.43
│      │  psp = 0.4
│   
│   │  !! Limit PSP range
│   
│   │  !! Calculate smoothed PSP average
│   
│   ├── [if soil(j)%ly(l)%psp_store > 0.]
│      │  psp = (soil(j)%ly(l)%psp_store * 29. + psp * 1.) / 30.
│   
│   │  !! Store PSP for tomrrows smoothing calculation
│   │  soil(j)%ly(l)%psp_store = psp
│   
│   │  !! on day 1 just set to a value of zero
│   
│   ├── [if (time%day == 1) .and. (time%yrs == 1)]
│   
│   │  !! Calculate P balance
│   
│   │  !! Move P between the soluble and active pools based on Vadas et al., 2006
│   
│   ├── [if rmp1 >= 0.]
│      
│      │  !! Calculate Dynamic Coefficant
│      
│      ├── [if soil(j)%ly(l)%a_days >0]
│      
│      │  !! limit rate coeff from 0.05 to .5 helps on day 1 when a_days is zero
│
▼
</pre>


---

## nut_pminrl

this subroutine computes p flux between the labile, active mineral

**Called from:** [`hru_control`](#hru_control)

Source: `nut_pminrl.f90`

<pre>
nut_pminrl
│
│  !! Intrinsic: Min
│  real, parameter :: bk = 0.01
│  hnb_d(j)%lab_min_p = 0.
│  hnb_d(j)%act_sta_p = 0.
│  rto = hru(j)%nut%psp / (1. - hru(j)%nut%psp)
│
├── [loop l = 1,]
│   │  rmp1 = (soil1(j)%mp(l)%lab - soil1(j)%mp(l)%act * rto)
│   
│   │  !! mike changed/added per isabelle beaudin"s email from 01/21/09
│   │  rmp1 = Min(rmp1, soil1(j)%mp(l)%lab)
│   │  roc = bk * (4. * soil1(j)%mp(l)%act - soil1(j)%mp(l)%sta)
│   │  roc = Min(roc, soil1(j)%mp(l)%act)
│   │  soil1(j)%mp(l)%sta = soil1(j)%mp(l)%sta + roc
│   │  soil1(j)%mp(l)%act = soil1(j)%mp(l)%act - roc + rmp1
│
▼
</pre>


---

## sep_biozone

This subroutine conducts biophysical processes occurring

**Called from:** [`hru_control`](#hru_control)

Source: `sep_biozone.f90`

<pre>
sep_biozone
│
│  !! Septic algorithm adapted from Siegrist et al., 2005
│  real*8 :: bz_vol = 0.d0
│  real*8 :: rtrate = 0.d0
│  real*8 :: qin = 0.d0
│  real*8 :: qout = 0.d0
│  real*8 :: rplqm = 0.d0
│  real*8 :: ntr_rt = 0.d0
│  real*8 :: dentr_rt = 0.d0
│  real*8 :: bod_rt = 0.d0
│  real*8 :: fcoli_rt = 0.d0
│  real*8 :: rtof = 0.d0
│
▼
</pre>


---

## pest_washp

this subroutine calculates the amount of pesticide washed off the plant

**Called from:** [`hru_control`](#hru_control)

Source: `pest_washp.f90`

<pre>
pest_washp
│
│  !! foliage and onto the soil
│
├── [loop k = 1,]
│   │  ipest_db = cs_db%pest_num(k)
│   
│   │  !! adjust foliar pesticide for wash off
│   
│   ├── [loop ipl = 1,]
│      
│      ├── [if cs_pl(j)%pl_on(ipl)%pest(k) >= 0.]
│         
│         ├── [if ipest_db > 0]
│         │  pest_soil = pestdb(ipest_db)%washoff * cs_pl(j)%pl_on(ipl)%pest(k)
│         │  cs_soil(j)%ly(1)%pest(k) = cs_soil(j)%ly(1)%pest(k) + pest_soil
│         │  cs_pl(j)%pl_on(ipl)%pest(k) = cs_pl(j)%pl_on(ipl)%pest(k) - pest_soil
│         │  hpestb_d(j)%pest(k)%wash = pest_soil
│
▼
</pre>


---

## pest_pl_up

this subroutine calculates the amount of pesticide plant uptake

**Called from:** [`hru_control`](#hru_control)

Source: `pest_pl_up.f90`

<pre>
pest_pl_up
│
│  !! foliage and onto the soil
│
├── [loop k = 1,]
│   │  ipest_db = cs_db%pest_num(k)
│   │  hpestb_d(j)%pest(k)%pl_uptake = 0.
│   
│   │  !! adjust foliar pesticide for wash off
│   
│   ├── [loop ipl = 1,]
│      
│      ├── [if cs_pl(j)%pl_on(ipl)%pest(k) >= 0.0001]
│         
│         ├── [if ipest_db > 0]
│         
│         ├── [loop ly = 1,]
│         │  pest_up = pestdb(ipest_db)%pl_uptake * pcom(j)%plcur(ipl)%uptake(ly) / s
│         │  cs_soil(j)%ly(ly)%pest(k) = cs_soil(j)%ly(ly)%pest(k) - pest_up
│         │  cs_pl(j)%pl_in(ipl)%pest(k) = cs_pl(j)%pl_in(ipl)%pest(k) + pest_up
│         │  hpestb_d(j)%pest(k)%pl_uptake = hpestb_d(j)%pest(k)%pl_uptake + pest_up
│
▼
</pre>


---

## pest_decay

this subroutine calculates degradation of pesticide in the soil and on

**Called from:** [`hru_control`](#hru_control)

Source: `pest_decay.f90`

<pre>
pest_decay
│
├── [loop k = 1,]
│   │  hpestb_d(j)%pest(k)%decay_s = 0.
│   │  hpestb_d(j)%pest(k)%decay_f = 0.
│   │  ipest_db = cs_db%pest_num(k)
│   
│   ├── [if ipest_db > 0]
│      
│      │  !! calculate degradation in soil
│      
│      ├── [loop l = 1,]
│         │  pest_init = cs_soil(j)%ly(l)%pest(k)
│         
│         ├── [if pest_init > 0.]
│         │  pest_end = pest_init * pestcp(ipest_db)%decay_s
│         │  cs_soil(j)%ly(l)%pest(k) = pest_end
│         │  pst_decay = (pest_init - pest_end)
│         │  pst_decay_s = pst_decay_s + pst_decay
│         
│         │  !! add decay to daughter pesticides
│         
│         ├── [loop imeta = 1,]
│         │  ipseq = pestcp(ipest_db)%daughter(imeta)%num
│         │  ipdb = cs_db%pest_num(ipseq)
│
│  !! adjust foliar pesticide for wash off
│
├── [loop ipl = 1,]
│   
│   ├── [if pest_init > 0.]
│      
│      │  !! add decay to daughter pesticides
│      
│      ├── [loop imeta = 1,]
│
▼
</pre>


---

## pest_lch

this subroutine calculates pesticides leached through each layer,

**Called from:** [`hru_control`](#hru_control)

Source: `pest_lch.f90`

<pre>
pest_lch
│
│  !! transported with surface runoff
│
├── [if cs_db%num_pests /= 0]
│   
│   ├── [loop k = 1,]
│      │  hpestb_d(j)%pest(k)%perc = 0.
│      │  hpestb_d(j)%pest(k)%surq = 0.
│      │  hpestb_d(j)%pest(k)%latq = 0.
│   
│   ├── [loop ly = 1,]
│      
│      ├── [loop k = 1,]
│         │  ipest_db = cs_db%pest_num(k)
│         │  kd = pestdb(ipest_db)%koc * soil1(ihru)%cbn(ly) / 100.
│         │  zdb1 = soil(j)%phys(ly)%ul + kd * soil(j)%phys(1)%bd * soil(j)%phys(1)%t
│         
│         │  !! compute volume of flow through the layer
│         │  vf = soil(j)%ly(ly)%prk + soil(j)%ly(ly)%flat
│         
│         │  !! compute concentration in the flow
│         
│         ├── [if cs_soil(j)%ly(ly)%pest(k) >= 0.0001 .and. vf >]
│         │  xx =  cs_soil(j)%ly(ly)%pest(k) * (1. - Exp(-vf / (zdb1 + 1.e-6)))
│         │  co = xx / vf
│         │  co = Min(pestdb(ipest_db)%solub / 100., co)
│         
│         │  !! calculate pesticide lost in surface runoff
│         
│         ├── [if surfq(j) > 0. .and. ly == 1]
│      
│      │  !! calculate pesticide lost in tile flow
│      
│      ├── [if qtile > 0. .and. ly == hru(j)%lumv%ldrain]
│      
│      │  !! calculate pesticide lost in lateral flow - use surface runoff conc for first layer
│      
│      ├── [if ly == 1]
│
▼
</pre>


---

## pest_soil_tot

this subroutine calculates the total amount of pesticide in the soil

**Called from:** [`hru_control`](#hru_control)

Source: `pest_soil_tot.f90`

<pre>
pest_soil_tot
│
│  !! this subroutine calculates the total amount of pesticide in the soil
│
├── [loop k = 1,]
│   
│   ├── [loop ipl = 1,]
│      │  hpestb_d(j)%pest(k)%plant = cs_pl(j)%pl_on(ipl)%pest(k)
│      │  hpestb_d(j)%pest(k)%in_plant = cs_pl(j)%pl_in(ipl)%pest(k)
│   │  hpestb_d(j)%pest(k)%soil = 0.
│   
│   ├── [loop ly = 1,]
│      │  hpestb_d(j)%pest(k)%soil = hpestb_d(j)%pest(k)%soil + cs_soil(j)%ly(ly)%
│
▼
</pre>


---

## pest_enrsb

this subroutine calculates the enrichment ratio for nutrient and

**Called from:** [`hru_control`](#hru_control)

Source: `pest_enrsb.f90`

<pre>
pest_enrsb
│
│  !! pesticide transport with runoff
│
├── [if sedyld(j) < 1.e-4]
│   │  sedyld(j) = 0.0
│   │  sanyld(j) = 0.0
│   │  silyld(j) = 0.0
│   │  clayld(j) = 0.0
│   │  sagyld(j) = 0.0
│   │  lagyld(j) = 0.0
│
│  !! CREAMS method for calculating enrichment ratio
│  cy = .1 * sedyld(j) / (hru(j)%area_ha * surfq(j) + 1.e-6)
│
├── [if cy > 1.e-6]
│   │  enratio = .78 * cy ** (-.2468)
│
▼
</pre>


---

## pest_pesty

this subroutine calculates pesticide transported with suspended sediment

**Called from:** [`hru_control`](#hru_control)

Source: `pest_pesty.f90`

<pre>
pest_pesty
│
│  !! this subroutine calculates pesticide transported with suspended sediment
│
├── [loop k = 1,]
│   │  ipest_db = cs_db%pest_num(k)
│   
│   ├── [if ipest_db > 0]
│      │  pest_init = cs_soil(j)%ly(1)%pest(k)
│      
│      ├── [if pest_init >= .0001]
│         │  kd = pestdb(ipest_db)%koc * soil1(j)%tot(1)%c / 100.
│         │  zdb1 = soil(j)%phys(1)%ul + kd * soil(j)%phys(1)%bd * soil(j)%phys(1)%th
│         
│         │  !! units: mm + (m^3/ton)*(ton/m^3)*mm = mm
│         │  conc = 100. * kd * pest_init / (zdb1 + 1.e-10)
│         
│         ├── [if hru(j)%hyd%erorgn > .001]
│         │  er = hru(j)%hyd%erorgn
│         │  er = enratio
│      │  hpestb_d(j)%pest(k)%sed = .001* sedyld(j) * conc * er / hru(j)%area_ha
│      │  cs_soil(j)%ly(1)%pest(k) = pest_init - hpestb_d(j)%pest(k)%sed
│
▼
</pre>


---

## nut_orgn

this subroutine calculates the amount of organic nitrogen removed in

**Called from:** [`hru_control`](#hru_control)

Source: `nut_orgn.f90`

<pre>
nut_orgn
│
│  !! ihru          |none         |HRU number
│
│  !! HRU calculations
│  orgn_kgha = soil1(j)%hsta(1)%n + soil1(j)%hact(1)%n
│
│  !! kg/ha = t/m3 * mm * 10,000 m2/ha * m/1000 mm * 1000 kg/t
│  wt1 = 10000. * soil(j)%phys(1)%bd * soil(j)%phys(1)%d
│
├── [if hru(j)%hyd%erorgn > .001]
│   │  er = hru(j)%hyd%erorgn
│   │  er = enratio
│  frac = orgn_kgha * er / wt1
│
│  !! kg/ha = t / ha * 1000. kg/t
│  sedorgn(j) = 1000. * frac * sedyld(j) / hru(j)%area_ha
│
│  !! update soil nitrogen pools only for HRU calculations
│
├── [if orgn_kgha > 1.e-6]
│   │  soil1(j)%hact(1)%n = soil1(j)%hact(1)%n - sedorgn(j) * (soil1(j)%hact(1)
│   │  soil1(j)%hsta(1)%n = soil1(j)%hsta(1)%n - sedorgn(j) * (soil1(j)%hsta(1)
│   
│   ├── [if soil1(j)%hact(1)%n < 0.]
│      │  sedorgn(j) = sedorgn(j) + soil1(j)%hact(1)%n
│      │  soil1(j)%hact(1)%n = 0.
│   
│   ├── [if soil1(j)%hsta(1)%n < 0.]
│
▼
</pre>


---

## nut_orgnc2

this subroutine calculates the amount of organic nitrogen removed in

**Called from:** [`hru_control`](#hru_control)

Source: `nut_orgnc2.f90`

<pre>
nut_orgnc2
│
│  !! total carbon in surface residue and soil humus
│  c_ly1 = soil1(j)%hp(1)%n + soil1(j)%hs(1)%n + pl_mass(j)%rsd_tot%n
│
│  !! wt = sol_bd(1,j) * sol_z(1,j) * 10. (tons/ha) -> wt1 = wt/1000
│  wt1 = soil(j)%phys(1)%bd * soil(j)%phys(1)%d / 100.
│
├── [if hru(j)%hyd%erorgn > .001]
│   │  er = hru(j)%hyd%erorgn
│   │  er = enratio
│
│  !! organic n leaving hru
│  conc = c_ly1 * er / wt1
│  sedorgn(j) = .001 * conc * sedyld(j) / hru(j)%area_ha
│
│  !! update soil carbon organic nitrogen pools
│
├── [if c_ly1 > 1.e-6]
│   │  n_left_rto = (1. - sedorgn(j) / c_ly1)
│   │  soil1(j)%tot(1)%n = soil1(j)%tot(1)%n * n_left_rto
│   │  soil1(j)%hs(1)%n = soil1(j)%hs(1)%n * n_left_rto
│   │  soil1(j)%hp(1)%n = soil1(j)%hp(1)%n * n_left_rto
│   
│   ├── [loop ipl = 1,]
│
│  !! Calculate runoff and leached C&N from microbial biomass
│
├── [loop ipl = 1,]
│
▼
</pre>


---

## nut_psed

this subroutine calculates the amount of organic and mineral phosphorus

**Called from:** [`hru_control`](#hru_control)

Source: `nut_psed.f90`

<pre>
nut_psed
│
│  !! |runoff in HRU for the day
│
│  !! HRU sediment calculations
│  sedp_attach = soil1(j)%hsta(1)%p + soil1(j)%man(1)%p + soil1(j)%man(1)%p
│
├── [if sedp_attach > 1.e-9]
│   │  fr_orgp = (soil1(j)%hsta(1)%p + soil1(j)%man(1)%p  + soil1(j)%man(1)%p) 
│   │  fr_actmin = soil1(j)%mp(1)%sta / sedp_attach
│   │  fr_stamin = soil1(j)%mp(1)%act / sedp_attach
│
│  !! kg/ha = t/m3 * mm * 10,000 m2/ha * m/1000 mm * 1000 kg/t
│  wt1 = 10000. * soil(j)%phys(1)%bd * soil(j)%phys(1)%d
│
├── [if hru(j)%hyd%erorgp > .001]
│   │  er = hru(j)%hyd%erorgp
│   │  er = enratio
│  frac = sedp_attach * er / wt1
│
│  !! kg/ha = t / ha * 1000. kg/t
│  sedp = 1000. * frac * sedyld(j) / hru(j)%area_ha
│
├── [if sedp > 1.e-9]
│   │  sedorgp(j) = sedp * fr_orgp
│   
│   ├── [if sed_orgp > 1.e-6]
│
▼
</pre>


---

## nut_nrain

this subroutine adds nitrate from rainfall to the soil profile

**Called from:** [`hru_control`](#hru_control)

Source: `nut_nrain.f90`

<pre>
nut_nrain
│
│  !! j           |none          |HRU number
│  iob = hru(j)%obj_no
│  iwst = ob(iob)%wst
│  iadep = wst(iwst)%wco%atmodep
│  ist = atmodep_cont%ts
│
│  !! (mg/l*mm) * kg/1,000,000 mg *1,00 l/m3 * m3/1,000 mm * 10,000 m2/ha = 0.01
│
├── [if ist > 0 .and. ist <= atmodep_cont%num]
│   
│   ├── [if atmodep_cont%timestep == "mo"]
│      │  const = float (ndays(time%mo + 1) - ndays(time%mo))
│      │  hnb_d(j)%no3atmo = .01 * atmodep(iadep)%no3_rfmo(ist) * w%precip + atmod
│      │  soil1(j)%mn(1)%no3 = hnb_d(j)%no3atmo + soil1(j)%mn(1)%no3
│      │  hnb_d(j)%nh4atmo = .01 * atmodep(iadep)%nh4_rfmo(ist) * w%precip + atmod
│      │  soil1(j)%mn(1)%nh4 = soil1(j)%mn(1)%nh4 + hnb_d(j)%nh4atmo
│   
│   ├── [if atmodep_cont%timestep == "yr"]
│      │  hnb_d(j)%no3atmo = .01 * atmodep(iadep)%no3_rfyr(ist) * w%precip + atmod
│
├── [if atmodep_cont%timestep == "aa"]
│
▼
</pre>


---

## nut_nlch

this subroutine simulates the loss of nitrate via surface runoff,

**Called from:** [`hru_control`](#hru_control)

Source: `nut_nlch.f90`

<pre>
nut_nlch
│
│  !! Intrinsic: Exp, Max, Min
│
├── [if gw_soil_flag == 1 .and. gw_solute_flag == 1]
│   
│   ├── [loop jj = 1,]
│      │  soil1(j)%mn(jj)%no3 = soil1(j)%mn(jj)%no3 + hru_soil(j,jj,1)
│      │  gwsoiln(j) = gwsoiln(j) + hru_soil(j,jj,1)
│
├── [loop jj = 1,]
│   
│   │  !! add nitrate leached from layer above
│   │  soil1(j)%mn(jj)%no3 = soil1(j)%mn(jj)%no3 + percnlyr
│   
│   │  !! determine concentration of nitrate in mobile water
│   
│   ├── [if jj == 1]
│      │  sro = surfq(j)
│   │  vv = soil(j)%ly(jj)%prk + sro + soil(j)%ly(jj)%flat + 1.e-10
│   │  ww = -vv / ((1. - soil(j)%anion_excl) * soil(j)%phys(jj)%ul)
│   
│   ├── [if ww < -80.0]
│   │  vno3 = soil1(j)%mn(jj)%no3 * (1. - Exp(ww))
│   │  co = Max(vno3 / vv, 0.)
│   
│   │  !! calculate nitrate in surface runoff
│   
│   ├── [if jj == 1]
│      │  surqno3(j) = surfq(j) * hru(j)%nut%nperco * co
│      │  surqno3(j) = Min(surqno3(j), soil1(j)%mn(jj)%no3)
│   
│   │  !! calculate nitrate in tile flow
│   
│   ├── [if hru(j)%lumv%ldrain == jj .and. qtile > 0.]
│      
│      │  !! assume rising water table will move nitrates up
│      
│      ├── [loop jlo = jj,]
│
▼
</pre>


---

## nut_solp

this subroutine calculates the amount of phosphorus lost from the soil

**Called from:** [`hru_control`](#hru_control)

Source: `nut_solp.f90`

<pre>
nut_solp
│
│  !! Intrinsic: Min, Max
│
├── [if gw_soil_flag.eq.1 .and. gw_solute_flag == 1]
│   
│   ├── [loop jj = 1,soil(j)%nly]
│      │  soil1(j)%mp(jj)%lab = soil1(j)%mp(jj)%lab + hru_soil(j,jj,2)
│      │  gwsoilp(j) = gwsoilp(j) + hru_soil(j,jj,2)
│  hls_d(j)%surqsolp = 0.
│  hls_d(j)%lchlabp = 0.
│  hls_d(j)%tilelabp = 0.
│  soil1(j)%mp(1)%lab = soil1(j)%mp(1)%lab + ht1%solp
│
│  !! compute soluble P lost in surface runoff
│  xx = soil(j)%phys(1)%bd * soil(j)%phys(1)%d * hru(j)%nut%phoskd
│  surqsolp(j) = soil1(j)%mp(1)%lab  * surfq(j) / (xx + 1.)
│
│  !! units ==> surqsolp = [kg/ha * mm] / [t/m^3 * mm * m^3/t] = kg/ha
│  surqsolp(j) = Min(surqsolp(j), soil1(j)%mp(1)%lab)
│  surqsolp(j) = Max(surqsolp(j), 0.)
│
│  !! compute soluble P leaching
│
├── [loop ly = 1,]
│   
│   ├── [if ly /= i_sep(j)]
│      
│      ├── [if ly == soil(j)%nly]
│         
│         │  !! leach p from bottom layer
│         
│         │  !! perc p to next layer
│      
│      ├── [if ly == hru(j)%lumv%ldrain]
│   
│   ├── [if bsn_cc%gwflow == 1 .and. gw_solute_flag == 1]
│
▼
</pre>


---

## salt_rain

this subroutine adds salt from atmospheric deposition (rainfall, dry) to the soil profile

**Called from:** [`hru_control`](#hru_control)

Source: `salt_rain.f90`

<pre>
salt_rain
│
│  !! j           |none          |HRU number
│
├── [if cs_db%num_salts > 0]
│   │  iob = hru(j)%obj_no
│   │  iwst = ob(iob)%wst
│   │  iadep = wst(iwst)%wco%atmodep
│   │  ist = atmodep_cont%ts
│   
│   ├── [if ist > 0 .and. ist <= atmodep_cont%num]
│      
│      ├── [if atmodep_cont%timestep == "mo"]
│         │  const = float (ndays(time%mo + 1) - ndays(time%mo))
│         
│         ├── [loop isalt=1,cs_db%num_salts]
│         │  hsaltb_d(j)%salt(isalt)%rain = .01 * atmodep_salt(iadep)%salt(isalt)%rfm
│         │  hsaltb_d(j)%salt(isalt)%dryd = atmodep_salt(iadep)%salt(isalt)%drymo(ist
│         │  cs_soil(j)%ly(1)%salt(isalt) = cs_soil(j)%ly(1)%salt(isalt) + (hsaltb_d(
│   
│   ├── [if atmodep_cont%timestep == "yr"]
│      
│      ├── [loop isalt=1,cs_db%num_salts]
│         │  hsaltb_d(j)%salt(isalt)%rain = .01 * atmodep_salt(iadep)%salt(isalt)%rfy
│         │  hsaltb_d(j)%salt(isalt)%dryd = atmodep_salt(iadep)%salt(isalt)%dryyr(ist
│
├── [if atmodep_cont%timestep == "aa"]
│   
│   ├── [loop isalt=1,cs_db%num_salts]
│
▼
</pre>


---

## salt_roadsalt

this subroutine adds salt from applied road salt to the soil profile

**Called from:** [`hru_control`](#hru_control)

Source: `salt_roadsalt.f90`

<pre>
salt_roadsalt
│
│  !! j           |none          |HRU number
│
├── [if cs_db%num_salts > 0]
│   │  iob = hru(j)%obj_no
│   │  iwst = ob(iob)%wst
│   │  iadep = wst(iwst)%wco%atmodep
│   │  ist = atmodep_cont%ts
│   
│   ├── [if ist > 0 .and. ist <= atmodep_cont%num]
│      
│      ├── [if atmodep_cont%timestep == "mo"]
│         │  const = float (ndays(time%mo + 1) - ndays(time%mo))
│         
│         ├── [loop isalt=1,cs_db%num_salts]
│         │  hsaltb_d(j)%salt(isalt)%road = rdapp_salt(iadep)%salt(isalt)%roadmo(ist)
│         │  cs_soil(j)%ly(1)%salt(isalt) = cs_soil(j)%ly(1)%salt(isalt) + hsaltb_d(j
│   
│   ├── [if atmodep_cont%timestep == "yr"]
│      
│      ├── [loop isalt=1,cs_db%num_salts]
│         │  hsaltb_d(j)%salt(isalt)%road = rdapp_salt(iadep)%salt(isalt)%roadday(tim
│         │  cs_soil(j)%ly(1)%salt(isalt) = cs_soil(j)%ly(1)%salt(isalt) + hsaltb_d(j
│
├── [if atmodep_cont%timestep == "aa"]
│   
│   ├── [loop isalt=1,cs_db%num_salts]
│      │  hsaltb_d(j)%salt(isalt)%road = rdapp_salt(iadep)%salt(isalt)%road / 365.
│
▼
</pre>


---

## salt_lch

this subroutine simulates the loss of salt via surface runoff,

**Called from:** [`hru_control`](#hru_control)

Source: `salt_lch.f90`

<pre>
salt_lch
│
│  !! Intrinsic: Exp, Max, Min
│
├── [if gw_soil_flag == 1 .and. gw_solute_flag == 1]
│   
│   ├── [loop jj = 1,]
│      
│      ├── [loop isalt=1,cs_db%num_salts]
│         │  sol_index = sol_index + 1
│         │  cs_soil(j)%ly(jj)%salt(isalt) = cs_soil(j)%ly(jj)%salt(isalt) +         
│         │  gwupsalt(j,isalt) = gwupsalt(j,isalt) + hru_soil(j,jj,sol_index)
│
├── [loop isalt=1,cs_db%num_salts]
│   │  sol_index = sol_index + 1
│   
│   ├── [loop jj = 1,soil(j)%nly]
│      
│      │  !! add salt leached from layer above
│      │  cs_soil(j)%ly(jj)%salt(isalt) = cs_soil(j)%ly(jj)%salt(isalt) + percsalt
│      
│      ├── [if cs_soil(j)%ly(jj)%salt(isalt) < 1.e-6]
│         │  cs_soil(j)%ly(jj)%salt(isalt) = 0.0
│      
│      │  !! determine concentration of salt in mobile water
│      
│      ├── [if jj == 1]
│         │  sro = surfq(j)
│      │  vv = soil(j)%ly(jj)%prk + sro + soil(j)%ly(jj)%flat + 1.e-10
│      │  ww = -vv / ((1. - soil(j)%anion_excl) * soil(j)%phys(jj)%ul)
│      │  vsalt = cs_soil(j)%ly(jj)%salt(isalt) * (1. - Exp(ww))
│      
│      │  !! calculate salt ion mass in surface runoff
│      
│      ├── [if jj == 1]
│
▼
</pre>


---

## cs_rain

this subroutine adds constituent mass from atmospheric deposition (rainfall, dry) to the soil profile

**Called from:** [`hru_control`](#hru_control)

Source: `cs_rain.f90`

<pre>
cs_rain
│
│  !! j           |none          |HRU number
│
├── [if cs_db%num_cs > 0]
│   │  iob = hru(j)%obj_no
│   │  iwst = ob(iob)%wst
│   │  iadep = wst(iwst)%wco%atmodep
│   │  ist = atmodep_cont%ts
│   
│   ├── [if ist > 0 .and. ist <= atmodep_cont%num]
│      
│      ├── [if atmodep_cont%timestep == "mo"]
│         │  const = float (ndays(time%mo + 1) - ndays(time%mo))
│         
│         ├── [loop ics=1,cs_db%num_cs]
│         │  hcsb_d(j)%cs(ics)%rain = .01 * atmodep_cs(iadep)%cs(ics)%rfmo(ist) * w%p
│         │  hcsb_d(j)%cs(ics)%dryd = atmodep_cs(iadep)%cs(ics)%drymo(ist) / const
│         │  cs_soil(j)%ly(1)%cs(ics) = cs_soil(j)%ly(1)%cs(ics) + (hcsb_d(j)%cs(ics)
│   
│   ├── [if atmodep_cont%timestep == "yr"]
│      
│      ├── [loop ics=1,cs_db%num_cs]
│         │  hcsb_d(j)%cs(ics)%rain = .01 * atmodep_cs(iadep)%cs(ics)%rfyr(ist) * w%p
│         │  hcsb_d(j)%cs(ics)%dryd = atmodep_cs(iadep)%cs(ics)%dryyr(ist) / 365.
│
├── [if atmodep_cont%timestep == "aa"]
│   
│   ├── [loop ics=1,cs_db%num_cs]
│
▼
</pre>


---

## cs_lch

this subroutine simulates the loss of constituent mass via surface runoff,

**Called from:** [`hru_control`](#hru_control)

Source: `cs_lch.f90`

<pre>
cs_lch
│
│  !! Intrinsic: Exp, Max, Min
│
├── [if gw_soil_flag == 1 .and. gw_solute_flag == 1]
│   
│   ├── [loop jj = 1,]
│      │  sol_index = 2 + cs_db%num_salts
│      
│      ├── [loop ics=1,cs_db%num_cs]
│         │  sol_index = sol_index + 1
│         │  cs_soil(j)%ly(jj)%cs(ics) = cs_soil(j)%ly(jj)%cs(ics) +                 
│         │  gwupcs(j,ics) = gwupcs(j,ics) + hru_soil(j,jj,sol_index)
│  sol_index = 2 + cs_db%num_salts
│
├── [loop ics=1,cs_db%num_cs]
│   │  sol_index = sol_index + 1
│   
│   ├── [loop jj = 1,soil(j)%nly]
│      
│      │  !! add constituent mass leached from layer above
│      │  cs_soil(j)%ly(jj)%cs(ics) = cs_soil(j)%ly(jj)%cs(ics)                   
│      
│      │  !! determine concentration of constituent in mobile water
│      
│      ├── [if jj == 1]
│         │  sro = surfq(j)
│      │  vv = soil(j)%ly(jj)%prk + sro + soil(j)%ly(jj)%flat + 1.e-10
│      │  ww = -vv / ((1. - soil(j)%anion_excl) * soil(j)%phys(jj)%ul)
│      
│      │  !! calculate constituent mass in surface runoff
│      
│      ├── [if jj == 1]
│      
│      │  !! calculate constituent mass in tile flow
│      
│      ├── [if hru(j)%lumv%ldrain == jj]
│
▼
</pre>


---

## path_ls_swrouting

**Called from:** [`hru_control`](#hru_control)

Source: `path_ls_swrouting.f90`

<pre>
path_ls_swrouting
│
├── [loop ipath = 1,]
│   │  isp_ini = hru(ihru)%dbs%soil_plant_init
│   │  ipath_db = sol_plt_ini(isp_ini)%path
│   │  path_kd = path_db(ipath_db)%kd
│   
│   │  !! compute pathogen incorporated into the soil
│   │  hpath_bal(j)%path(ipath)%perc1 = path_kd * cs_soil(j)%ly(1)%path(ipath) 
│   │  hpath_bal(j)%path(ipath)%perc1 = Min(hpath_bal(j)%path(ipath)%perc1, cs_
│   │  hpath_bal(j)%path(ipath)%perc1 = Max(hpath_bal(j)%path(ipath)%perc1, 0.)
│   │  cs_soil(j)%ly(1)%path(ipath) = cs_soil(j)%ly(1)%path(ipath) - hpath_bal(
│
▼
</pre>


---

## path_ls_runoff

**Called from:** [`hru_control`](#hru_control)

Source: `path_ls_runoff.f90`

<pre>
path_ls_runoff
│
├── [loop ipath = 1,]
│   │  isp_ini = hru(ihru)%dbs%soil_plant_init
│   │  ipath_db = sol_plt_ini(isp_ini)%path
│   │  path_kd = path_db(ipath_db)%kd
│   
│   │  !! compute soluble bacteria in the surface runoff
│   │  hpath_bal(j)%path(ipath)%surq = path_kd * cs_soil(j)%ly(1)%path(ipath) *
│   │  hpath_bal(j)%path(ipath)%surq = Min(hpath_bal(j)%path(ipath)%surq, cs_so
│   │  hpath_bal(j)%path(ipath)%surq = Max(cs_soil(j)%ly(1)%path(ipath), 0.)
│   │  cs_soil(j)%ly(1)%path(ipath) = cs_soil(j)%ly(1)%path(ipath) - hpath_bal(
│   
│   │  !! compute bacteria transported with sediment
│   
│   ├── [if enratio > 0.]
│      │  cpath = (1. - path_kd) * cs_soil(j)%ly(1)%path(ipath) * enratio / soil(j
│      │  hpath_bal(j)%path(ipath)%sed = .0001 * cpath * sedyld(j) / (hru(j)%area_
│      │  hpath_bal(j)%path(ipath)%sed = Min(hpath_bal(j)%path(ipath)%sed, cs_soil
│
▼
</pre>


---

## path_ls_process

**Called from:** [`hru_control`](#hru_control)

Source: `path_ls_process.f90`

<pre>
path_ls_process
│
├── [loop ipath = 1,]
│   │  isp_ini = hru(ihru)%dbs%soil_plant_init
│   │  ipath_db = sol_plt_ini(isp_ini)%path
│   │  hpath_bal(j)%path(ipath)%wash = 0.
│   
│   ├── [loop ipl = 1,]
│      
│      │  !! compute pathogen wash off
│      
│      ├── [if w%precip >= 2.54]
│         │  wash_off = path_db(ipath_db)%washoff * cs_pl(j)%pl_on(ipl)%path(ipath)
│         │  cs_soil(j)%ly(1)%path(ipath) = cs_soil(j)%ly(1)%path(ipath) + wash_off
│         │  cs_pl(j)%pl_on(ipl)%path(ipath) = cs_pl(j)%pl_on(ipl)%path(ipath) - wash
│         │  hpath_bal(j)%path(ipath)%wash = hpath_bal(j)%path(ipath)%wash + wash_off
│      
│      │  !! compute pathogen die-off and re-growth on foilage
│      │  pl_ini = cs_pl(j)%pl_on(ipl)%path(ipath)
│      │  pl_die_gro = path_db(ipath_db)%do_plnt - path_db(ipath_db)%gr_plnt
│      │  cs_pl(j)%pl_on(ipl)%path(ipath) = cs_pl(j)%pl_on(ipl)%path(ipath) *     
│   
│   │  !! compute pathogen die-off and re-growth in surface soil layer
│   
│   │  !! net die_off - negative is regrowth
│
▼
</pre>


---

## hru_urban

this subroutine computes loadings from urban areas using the

**Called from:** [`hru_control`](#hru_control)

Source: `hru_urban.f90`

<pre>
hru_urban
│
│  !! SWAT: Regres, sweep
│  ulu = hru(j)%luse%urb_lu
│  iob = hru(j)%obj_no
│  iwst = ob(iob)%wst
│
├── [select case (hru(j)%luse%urb_ro)]
│   
│   ├── [if w%precip > .1 .and. surfq(j) > .1]
│      │  cod = Regres(1)
│      │  sus_sol = Regres(2)
│      │  tn = Regres(3)
│      │  tp = Regres(4)
│      │  sedyld(j) = (.001 * sus_sol) * urbdb(ulu)%fimp + sedyld(j)              
│      
│      │  !! to be all sitly particles
│      │  silyld(j) = (.001 * sus_sol) * urbdb(ulu)%fimp                          
│      │  sanyld(j) = sanyld(j) * (1. - urbdb(ulu)%fimp)
│   
│   ├── [if surfq(j) > 0.1]
│      
│      │  !! calculate amount of dirt on streets prior to wash-off
│      
│      │  !! calculate wash-off of solids
│      
│      │  !! expression in () qp_cms in mm/hr
│      
│      │  !! set time to correspond to lower amount of dirt
│      
│      │  !! amounts are kg/ha
│
▼
</pre>


---

## swr_latsed

this subroutine calculates the sediment load contributed in lateral flow

**Called from:** [`hru_control`](#hru_control)

Source: `swr_latsed.f90`

<pre>
swr_latsed
│
│  !! j           |none          |HRU number
│
│  !! add sediment from lateral and tile flow - t=ppm*ha*mm*10/100,000;  m3=ha*mm*10; ppm=t/m3
│  sedyld(j) = sedyld(j) + hru(j)%hyd%lat_sed * hru(j)%area_ha * (latq(j) +
│
│  !! mm * 1 t/m3 * 10,000 m2/ha * mm/1,000 m * 1/1,000,000 ppm = 1/10,000
│  sedyld(j)=sedyld(j) + (latq(j) + qtile) * hru(j)%hyd%lat_sed / 100000.
│  sanyld(j)=sanyld(j)+ latq(j) * hru(j)%km * hru(j)%hyd%lat_sed * soil(j)%
│  silyld(j)=silyld(j)+ latq(j) * hru(j)%km * hru(j)%hyd%lat_sed * soil(j)%
│  clayld(j)=clayld(j)+ latq(j) * hru(j)%km * hru(j)%hyd%lat_sed * soil(j)%
│  sagyld(j)=sagyld(j)+ latq(j) * hru(j)%km * hru(j)%hyd%lat_sed * soil(j)%
│  lagyld(j)=lagyld(j)+ latq(j) * hru(j)%km * hru(j)%hyd%lat_sed * soil(j)%
│
│  !! organic n and p in the lateral flow     - by J.Jeong BREC 2011
│  sedorgn(j) = sedorgn(j) + latq(j) * hru(j)%hyd%lat_orgn /10000.
│  sedorgp(j) = sedorgp(j) + latq(j) * hru(j)%hyd%lat_orgp /10000.
│
▼
</pre>


---

## stor_surfstor

this subroutine stores and lags sediment and nutrients in surface runoff

**Called from:** [`hru_control`](#hru_control)

Source: `stor_surfstor.f90`

<pre>
stor_surfstor
│
│  !! Intrinsic: Max
│
├── [if time%step == 1]
│   │  surf_bs(2,j) = Max(1.e-9, surf_bs(2,j) + sedyld(j))
│   │  sedyld(j) = surf_bs(2,j) * brt(j)
│   │  surf_bs(2,j) = surf_bs(2,j) - sedyld(j)
│   │  sedprev = hhsurf_bs(2,j,time%step)
│   
│   ├── [loop k=1,time%step]
│      
│      │  !! Left-over (previous timestep) + inflow (current  timestep)
│      │  hhsurf_bs(2,j,k) = Max(1.e-9, sedprev + hhsedy(j,k))
│      
│      │  !! new estimation of sediment reaching the main channel
│      │  hhsedy(j,k) = hhsurf_bs(2,j,k) * brt(j)
│      │  hhsurf_bs(2,j,k) = hhsurf_bs(2,j,k) - hhsedy(j,k)
│      
│      │  !! lagged at the end of time step
│      │  sedprev = hhsurf_bs(2,j,k)
│      │  surf_bs(2,j) = Max(1.e-9, surf_bs(2,j) + sedyld(j))
│   
│   │  !! daily total sediment yield from the HRU
│   │  sedyld(j) = sum(hhsedy(j,:))
│
├── [if cs_db%num_salts > 0]
│
▼
</pre>


---

## swr_substor

this subroutine stores and lags lateral soil flow and nitrate

**Called from:** [`hru_control`](#hru_control)

Source: `swr_substor.f90`

<pre>
swr_substor
│  bss(1,j) = bss(1,j) + latq(j)
│  bss(2,j) = bss(2,j) + latno3(j)
│  bss(3,j) = bss(3,j) + qtile
│  bss(4,j) = bss(4,j) + tileno3(j)
│  latq(j) = bss(1,j) * hru(j)%hyd%lat_ttime
│  latno3(j) = bss(2,j) * hru(j)%hyd%lat_ttime
│  qtile = bss(3,j) * hru(j)%lumv%tile_ttime
│  tileno3(j) = bss(4,j) * hru(j)%lumv%tile_ttime
│  bss(1,j) = bss(1,j) - latq(j)
│  bss(2,j) = bss(2,j) - latno3(j)
│
▼
</pre>


---

## smp_filter

this subroutine calculates the reduction of pollutants in surface runoff

**Called from:** [`hru_control`](#hru_control)

Source: `smp_filter.f90`

<pre>
smp_filter
│
│  !! |sediment in surface runoff in HRU for day
│
├── [if i == 100]
│
│  !! Filter only if there is some surface runoff
│
├── [if surfq(j) > .0001]
│   
│   │  !! Calculate drainage area of vfs 1 2 3 in ha
│   │  drain_vfs1 = (1. - hru(j)%lumv%vfscon)* hru(j)%area_ha
│   │  drain_vfs2 = ((1. - hru(j)%lumv%vfsch) * hru(j)%lumv%vfscon)* hru(j)%are
│   │  drain_vfs3 = hru(j)%lumv%vfscon * hru(j)%lumv%vfsch * hru(j)%area_ha
│   
│   │  !! Calculate area of vfs 1 and 2 in ha
│   │  area_vfs1 = hru(j)%area_ha * 0.9 / hru(j)%lumv%vfsratio
│   │  area_vfs2 = hru(j)%area_ha * 0.1 / hru(j)%lumv%vfsratio
│   
│   │  !! Calculate drainage area to vfs area ratio (unitless)
│   │  vfs_ratio1 = drain_vfs1/area_vfs1
│   │  vfs_ratio2 = drain_vfs2/area_vfs2
│   
│   │  !! calculate runoff depth over buffer area in mm
│   │  vfs_depth1 = vfs_ratio1 * surfq(j)
│   │  vfs_depth2 = vfs_ratio2 * surfq(j)
│   
│   │  !! calculate sediment loading over buffer area in kg/m^2
│   │  vfs_sed1 = (sedyld(j) / hru(j)%area_ha * 1000. * drain_vfs1) / (area_vfs
│   
│   │  !! Based on vfsmod simulations
│   
│   │  !! calculate sediment Removal - Based on measured data from literature
│   
│   ├── [if sedtrap <= lagyld(j)]
│      
│      ├── [if xrem <= sanyld(j)]
│
▼
</pre>


---

## smp_buffer

this subroutine calculates the reduction of nitrates through a riparian

**Called from:** [`hru_control`](#hru_control)

Source: `smp_buffer.f90`

<pre>
smp_buffer
│
│  !! |sediment in surface runoff in HRU for day
│
│  !! compute nitrate reduction as a function of distance to stream
│  reduc = 2.1661 * filterw(j) - 5.1302
│  latno3(j) = latno3(j) * (1. - reduc / 100.)
│
▼
</pre>


---

## smp_grass_wway

this subroutine controls the grass waterways

**Called from:** [`hru_control`](#hru_control)

Source: `smp_grass_wway.f90`

<pre>
smp_grass_wway
│
│  !! rcharea     |m^2           |cross-sectional area of flow
│
│  !! set variables
│
│  !! do this only if there is surface runoff this day
│
├── [if surfq(j) > 0.001]
│   
│   │  !! calculate average flow based on 3 hours of runoff
│   │  chflow_day = 1000. * surfq(j) * hru(ihru)%km
│   │  chflow_m3 = chflow_day / 10800
│   │  qp_cms = 2. * chflow_m3 / (1.5 * tc_gwat(j))
│   
│   │  !! if peak rate is greater than bankfull discharge
│   
│   ├── [if qp_cms > grwway_vel(j)%vel_bf]
│      │  rcharea = grwway_vel(j)%area
│      │  rchdep = hru(j)%lumv%grwat_d
│      
│      │  !! find the depth until the discharge rate is equal to volrt
│      
│      ├── [do while (sdti < qp_cms)]
│         │  rchdep = rchdep + 0.01
│         │  rcharea = (grwway_vel(j)%wid_btm + 8 * rchdep) * rchdep
│         │  p = grwway_vel(j)%wid_btm + 2. * rchdep * Sqrt(1. + 8 * 8)
│         │  rh = rcharea / p
│         │  sdti = Qman(rcharea, rh, hru(j)%lumv%grwat_n, hru(j)%lumv%grwat_s)
│   
│   │  !! Sediment yield (t) from fraction of area drained by waterway
│   
│   │  !! calculate area of sheeflow in m^2 assume *:1 side slope 8.06 = (8^2+1^2)^.5
│   
│   │  !! limit area of sheet flow to 10% of hru area
│   
│   │  !! handled by 10% of VFS area. Waterways likely even more concentrated Assume only 20% of s
│   
│   ├── [if sf_area > 1.e-6]
│      
│      │  !! calculate runoff depth over sheetflow area in mm
│      
│      │  !! Calculate sediment load on sheetflow area kg/m2
│      
│      │  !! Calculate runoff and sediment losses taken from mostly from filter.f
│   
│   ├── [if sf_area > 0.]
│      
│      │  !! Simpler form derived from vfsmod simulations. r2 = 0.57 Publication pending White and Ar
│
▼
</pre>


---

## smp_bmpfixed

this subroutine applies fixed removal eff. from the .ops to upland loads

**Called from:** [`hru_control`](#hru_control)

Source: `smp_bmpfixed.f90`

<pre>
smp_bmpfixed
│
│  !! set variables
│  sedyld(j) = sedyld(j) * (1. - hru(j)%lumv%bmp_sed)
│
│  !! Particulate Phosphorus
│  sedminpa(j) = sedminpa(j) * (1. - hru(j)%lumv%bmp_pp)
│  sedminps(j) = sedminps(j) * (1. - hru(j)%lumv%bmp_pp)
│  sedorgp(j) = sedorgp(j) * (1. - hru(j)%lumv%bmp_pp)
│
│  !! Soluble Phosphorus
│  surqsolp(j) = surqsolp(j) * (1. - hru(j)%lumv%bmp_sp)
│
│  !! Particulate Nitrogen
│  sedorgn(j) = sedorgn(j) * (1. - hru(j)%lumv%bmp_pn)
│
│  !! Soluble Nitrogen
│  surqno3(j) = surqno3(j) * (1. - hru(j)%lumv%bmp_sn)
│  latno3(j) = latno3(j) * (1. - hru(j)%lumv%bmp_sn)
│
▼
</pre>


---

## sq_surfst

this subroutine determines the net surface runoff reaching the

**Called from:** [`hru_control`](#hru_control)

Source: `sq_surfst.f90`

<pre>
sq_surfst
│
│  !! Intrinsic: Max
│
├── [if bsn_cc%gampt == 0]
│   │  bsprev = surf_bs(1,j)
│   │  surf_bs(1,j) = Max(1.e-6, surf_bs(1,j) + surfq(j))
│   │  qday = surf_bs(1,j) * brt(j)
│   │  surf_bs(1,j) = surf_bs(1,j) - qday
│   │  bsprev = hhsurf_bs(1,j,time%step)
│   
│   ├── [loop k=1,time%step]
│      
│      │  !! Left-over (previous timestep) + inflow (current  timestep)
│      │  hhsurf_bs(1,j,k) = Max(1.e-9, bsprev + hhsurfq(j,k))
│      
│      │  !! new estimation of runoff and sediment reaching the main channel
│      │  hhsurfq(j,k) = hhsurf_bs(1,j,k) * brt(j)
│      │  hhsurf_bs(1,j,k) = hhsurf_bs(1,j,k) - hhsurfq(j,k)
│      
│      │  !! lagged at the end of time step
│      │  bsprev = hhsurf_bs(1,j,k)
│      
│      │  !! daily total yield from the HRU
│      │  qday = qday + hhsurfq(j,k)
│
▼
</pre>


---

## swr_subwq

this subroutine computes HRU loadings of chlorophyll-a, CBOD,

**Called from:** [`hru_control`](#hru_control)

Source: `swr_subwq.f90`

<pre>
swr_subwq
│
│  !! Intrinsic: Exp
│
│  !! SWAT manual 2.3.13
│  wtmp = 5.0 + 0.75 * w%tave
│  wtmp = wtmp + 273.15
│
├── [if qdr(j) > 1.e-4]
│   │  tp = 100. * (sedorgn(j) + surqno3(j)) / qdr(j)
│   │  chl_a(j) = .1 * tp
│   
│   │  !! calculate organic carbon loading to main channel
│   │  org_c = (soil1(j)%cbn(1) / 100.) * enratio * sedyld(j) * 1000.
│   
│   │  !! ========================
│   
│   ├── [if bsn_cc%cswat == 1]
│      │  org_c = hsc_d(j)%sed_c * hru(j)%area_ha
│   
│   │  !! other equations - BOD5 = 2.9 * TOC; CBOD5 = 23.7 + 1.68 * TOC; BOD = 1.813(TOC)**0.4244
│   │  cbodu(j) = 2.7 * org_c / (qdr(j) * hru(j)%km) / 10000.
│   
│   │  !! QUAL2E equation III-29
│   │  ww = -139.34410 + (1.575701E05 / wtmp)
│   │  xx = 6.642308E07 / (wtmp**2)
│   │  yy = 1.243800E10 / (wtmp**3)
│   
│   │  !! calculate actual dissolved oxygen concentration
│
▼
</pre>


---

## hru_urb_bmp

~ ~ ~ PURPOSE ~ ~ ~

**Called from:** [`hru_control`](#hru_control)

Source: `hru_urb_bmp.f90`

<pre>
hru_urb_bmp
│
│  !! convert to ppm -> (kg/ha)*100./mm = ppm
│
├── [if qdr(j) > 0.1]
│   │  xx = 100. / qdr(j)
│   │  sedppm = 1000. * xx * sedyld(j) / hru(j)%area_ha
│   │  solnppm = xx * (surqno3(j) + latno3(j))
│   │  solpppm = xx * surqsolp(j)
│   │  sednppm = xx * sedorgn(j)
│   │  sedpppm = xx * (sedorgp(j) + sedminpa(j) + sedminps(j))
│   
│   ├── [if sedppm > sed_con (j)]
│      │  sedyld(j) = sed_con(j) * hru(j)%area_ha / xx / 1000.
│   
│   ├── [if solnppm > soln_con(j)]
│      │  surqno3(j) = soln_con(j) / xx
│      │  latno3(j) = soln_con(j) / xx
│   
│   ├── [if solpppm > solp_con(j)]
│      │  surqsolp(j) = solp_con(j) / xx
│   
│   ├── [if sednppm > orgn_con(j)]
│   
│   ├── [if sedpppm > orgp_con(j)]
│
▼
</pre>


---

## flow_hyd_ru_hru

this subroutine determines the subdaily flow hydrographs for hru's, ru's and inflow fractions

**Called from:** [`hru_hyds`](#hru_hyds), [`ru_control`](#ru_control)

Source: `flow_hyd_ru_hru.f90`

<pre>
flow_hyd_ru_hru
│
│  !! this subroutine determines the subdaily flow hydrographs for hru's, ru's and inflow frac
│
│  !! set subdaily hydrographs
│  iday_cur = iday_start
│  iday_prev = iday_start - 1
│
│  !! subsurface flow = lateral + tile --> assume uniform throughout the day
│  ssq = (latq + tileq)  / time%step
│
│  !! sum flow in case hydrograph exceeds max days
│
│  !! zero previous day flow hydrograph and total hydrographs
│  hyd_flo(iday_prev,:) = 0.
│
│  !! use unit hydrograph to compute subdaily flow hydrographs
│
├── [loop iday = 1,]
│   
│   │  !! only add subsurface flow today - already lagged and assumed uniform for the day
│   
│   ├── [loop istep = 1,]
│      │  sq = uh(iday,istep) * surfq
│      │  hyd_flo(iday_cur,istep) = hyd_flo(iday_cur,istep) + sq
│   
│   │  !! set current and previous days
│   │  iday_cur = iday_cur + 1
│   │  iday_prev = iday_prev + 1
│
▼
</pre>


---

## gwflow_rech

this subroutine determines the volume of groundwater that is added to the aquifer via recharge (soil percolation)

**Called from:** [`gwflow_simulate`](#gwflow_simulate)

Source: `gwflow_rech.f90`

<pre>
gwflow_rech
│
│  !! (recharge volumes are used in gwflow_simulate, in groundwater balance equations)
│
├── [loop k=1,sp_ob%hru]
│   │  ob_num = sp_ob1%hru + k - 1
│   │  recharge = gw_rech(k)
│   │  gw_rech(k) = 0.
│   │  gw_rech(k) = ((1.-gw_delay(k))*gwflow_perc(k)) + (gw_delay(k)*recharge)
│   
│   ├── [if gw_heat_flag == 1]
│      │  perc_volm = (gwflow_perc(k)/1000.) * (ob(ob_num)%area_ha * 10000.)
│      │  perc_temp = soil(k)%phys(soil(k)%nly)%tmp
│      │  perc_heat = perc_temp * gw_rho * gw_cp * perc_volm
│      │  recharge_heat = gw_rechheat(k)
│      │  gw_rechheat(k) = ((1.-gw_delay(k))*perc_heat) + (gw_delay(k)*recharge_he
│   
│   ├── [if gw_solute_flag == 1]
│      
│      ├── [loop s=1,gw_nsolute]
│         │  recharge_sol = gw_rechsol(k,s)
│
├── [if lsu_cells_link == 1]
│   
│   ├── [loop k=1,db_mx%lsu_out]
│      
│      ├── [if gw_solute_flag == 1]
│      
│      ├── [loop j=1,lsu_out(k)%num_tot]
│         
│         ├── [if gw_solute_flag == 1]
│         
│         ├── [loop s=1,gw_nsolute]
│
├── [loop i=1,lsu_num_cells(k)]
│   
│   ├── [if gw_state(cell_id)%stat == 2]
│
▼
</pre>


---

## gwflow_gwet

this subroutine determines the volume of groundwater that is removed from the

**Called from:** [`gwflow_simulate`](#gwflow_simulate)

Source: `gwflow_gwet.f90`

<pre>
gwflow_gwet
│
│  !! aquifer via ET
│
├── [if lsu_cells_link == 1]
│   
│   ├── [loop k=1,db_mx%lsu_out]
│      
│      ├── [loop j=1,lsu_out(k)%num_tot]
│         │  hru_id = lsu_out(k)%num(j)
│         │  ob_num = sp_ob1%hru + hru_id - 1
│         │  hru_gwet_volume = (etremain(hru_id)/1000.) * (ob(ob_num)%area_ha * 10000
│         │  lsu_gwet_volume = lsu_gwet_volume + hru_gwet_volume
│      
│      ├── [loop i=1,lsu_num_cells(k)]
│         │  max_gwet = lsu_gwet_volume * lsu_cells_fract(k,i)
│         │  cell_id = lsu_cells(k,i)
│         │  et_surface = gw_state(cell_id)%elev
│         │  et_bottom = et_surface - gw_state(cell_id)%exdp
│         │  gw_head = gw_state(cell_id)%head
│         
│         ├── [if gw_head < et_bottom]
│         │  gwet_volume = max_gwet
│         
│         ├── [if gw_state(cell_id)%exdp.ne.0]
│   
│   ├── [if gw_state(cell_id)%head.gt.gw_state(cell_id)%bo]
│      
│      ├── [if gwet_volume.ge.gw_state(cell_id)%stor]
│   
│   ├── [if gw_heat_flag == 1]
│
▼
</pre>


---

## gwflow_phreatophyte

this subroutine calculates the water removed from the aquifer via phreatophyte extraction

**Called from:** [`gwflow_simulate`](#gwflow_simulate)

Source: `gwflow_phreatophyte.f90`

<pre>
gwflow_phreatophyte
│
│  !! (extraction volumes are used in gwflow_simulate, in groundwater balance equations)
│
├── [if gw_phyt_flag == 1]
│   
│   ├── [loop k=1,gw_phyt_ncells]
│      │  cell_id = gw_phyt_ids(k)
│      │  wt_depth = gw_state(cell_id)%elev - gw_state(cell_id)%head
│      
│      ├── [loop i=1,gw_phyt_npts-1]
│         
│         ├── [if wt_depth >= gw_phyt_dep(i) .and. wt_depth <= g]
│         │  ratio = (wt_depth - gw_phyt_dep(i)) / (gw_phyt_dep(i+1)-gw_phyt_dep(i))
│         │  et_rate = gw_phyt_rate(i) - (ratio*(gw_phyt_rate(i)-gw_phyt_rate(i+1)))
│         │  et_Q = et_rate * gw_phyt_area(k)
│   
│   ├── [if et_Q > gw_state(cell_id)%stor]
│      │  et_Q = gw_state(cell_id)%stor
│   │  et_Q = et_Q * (-1)
│   │  gw_hyd_ss(cell_id)%phyt = gw_hyd_ss(cell_id)%phyt + et_Q
│   │  gw_hyd_ss_yr(cell_id)%phyt = gw_hyd_ss_yr(cell_id)%phyt + et_Q
│   │  gw_hyd_ss_mo(cell_id)%phyt = gw_hyd_ss_mo(cell_id)%phyt + et_Q
│
▼
</pre>


---

## gwflow_pump_ext

this subroutine determines the volume of groundwater that is extracted

**Called from:** [`gwflow_simulate`](#gwflow_simulate)

Source: `gwflow_pump_ext.f90`

<pre>
gwflow_pump_ext
│
│  !! (pumping volume are used in gwflow_simulate, in groundwater balance equations)
│
├── [if gw_pumpex_flag == 1]
│   
│   ├── [loop i=1,gw_npumpex]
│      │  cell_id = gw_pumpex_cell(i)
│      
│      ├── [if gw_state(cell_id)%stat == 1]
│         
│         ├── [loop j=1,gw_pumpex_nperiods(i)]
│         │  pumpex_start_date = gw_pumpex_dates(i,1,j)
│         │  pumpex_end_date = gw_pumpex_dates(i,2,j)
│         
│         ├── [if gw_daycount.ge.pumpex_start_date .and. gw_dayc]
│         │  Q = gw_pumpex_rates(i,j)
│         
│         ├── [if Q.ge.gw_state(cell_id)%stor]
│         │  Q = gw_state(cell_id)%stor
│         │  gw_state(cell_id)%stor = gw_state(cell_id)%stor - Q
│      │  gw_hyd_ss(cell_id)%ppex = gw_hyd_ss(cell_id)%ppex - Q
│      │  gw_hyd_ss_yr(cell_id)%ppex = gw_hyd_ss_yr(cell_id)%ppex - Q
│      │  gw_hyd_ss_mo(cell_id)%ppex = gw_hyd_ss_mo(cell_id)%ppex - Q
│      
│      ├── [if gw_heat_flag == 1]
│         │  heat_flux = gwheat_state(cell_id)%temp * gw_rho * gw_cp * Q
│         
│         ├── [if heat_flux >= gwheat_state(cell_id)%stor]
│   
│   ├── [if gw_solute_flag == 1]
│      
│      ├── [loop s=1,gw_nsolute]
│
▼
</pre>


---

## gwflow_canal_ext

this subroutine calculates the water exchange volume between irrigation canals and connected grid cells

**Called from:** [`gwflow_simulate`](#gwflow_simulate)

Source: `gwflow_canal_ext.f90`

<pre>
gwflow_canal_ext
│
│  !! for canals that divert water from a source outside of the model domain.
│
├── [if gw_canal_flag == 1]
│   
│   ├── [loop i=1,gw_canal_ncells_out]
│      │  cell_id = gw_canl_out_info(i)%cell_id
│      
│      ├── [if gw_state(cell_id)%stat == 1]
│         
│         ├── [if time%day.ge.gw_canl_out_info(i)%dayb .and. tim]
│         │  width = gw_canl_out_info(i)%wdth
│         │  depth = gw_canl_out_info(i)%dpth
│         │  thick = gw_canl_out_info(i)%thck
│         │  length = gw_canl_out_info(i)%leng
│         │  stage = gw_canl_out_info(i)%elev
│         │  bed_K = gw_canl_out_info(i)%hydc
│         │  flow_area = length * width
│         │  canal_bed = stage - depth
│         
│         ├── [if gw_state(cell_id)%head < canal_bed]
│         │  head_diff = depth
│      
│      ├── [if Q < 0]
│         
│         ├── [if -Q .ge.gw_state(cell_id)%stor]
│   
│   ├── [if gw_heat_flag == 1]
│      
│      ├── [if Q < 0]
│         
│         ├── [if -heat_flux >= gwheat_state(cell_id)%stor]
│
▼
</pre>


---

## gwflow_pond

this subroutine calculates the volume of seepage from recharge ponds;

**Called from:** [`gwflow_simulate`](#gwflow_simulate)

Source: `gwflow_pond.f90`

<pre>
gwflow_pond
│
│  !! writes out recharge pond water balance
│  character(len=18) :: pond_name = ''
│
├── [if gw_pond_flag == 1]
│   │  read(in_ponds,*) year,month,day,(gw_pond_info(r)%div,r=1,gw_npond)
│   
│   ├── [loop r=1,gw_npond]
│      │  gw_pond_info(r)%div_uns = 0.
│      
│      ├── [if gw_daycount .ge. gw_pond_info(r)%dy_start]
│         │  div_specified = gw_pond_info(r)%div
│         
│         ├── [if div_specified > 0]
│      
│      ├── [if gw_pond_info(r)%chan > 0]
│         │  chan_id = gw_pond_info(r)%chan
│         │  chan_volume = ch_stor(chan_id)%flo
│         │  div_added = div_specified
│         
│         ├── [if div_specified > ch_stor(chan_id)%flo]
│         │  div_added = ch_stor(chan_id)%flo
│      │  gw_pond_info(r)%div_uns = div_specified - div_added
│      │  ch_stor(chan_id)%flo = ch_stor(chan_id)%flo - div_added
│      
│      ├── [if gw_solute_flag == 1]
│         
│         ├── [if chan_volume > 10.]
│         
│         ├── [if sol_mass > ch_stor(chan_id)%no3]
│      
│      ├── [if sol_mass > ch_stor(chan_id)%solp]
│
▼
</pre>


---

## gwflow_canal_div

this subroutine calculates the water exchange volume between irrigation canals and connected grid cells

**Called from:** [`gwflow_simulate`](#gwflow_simulate)

Source: `gwflow_canal_div.f90`

<pre>
gwflow_canal_div
│
│  !! removed from the diverted water volume.
│  character(len=18) :: canal_name = ''
│
├── [if gw_canal_flag == 1]
│   
│   ├── [loop i=1,gw_canal_ncells_div]
│      │  canal_id = gw_canl_div_cell(i)%canal_id
│      
│      ├── [if gw_canl_div_info(canal_id)%stor > 0]
│         │  cell_id = gw_canl_div_cell(i)%cell_id
│         
│         ├── [if gw_state(cell_id)%stat == 1]
│         │  width = gw_canl_div_info(canal_id)%width
│         │  depth = gw_canl_div_info(canal_id)%depth
│         │  thick = gw_canl_div_info(canal_id)%thick
│         │  bed_K = gw_canl_div_info(canal_id)%bed_K
│         │  length = gw_canl_div_cell(i)%leng
│         │  canal_bed = gw_canl_div_cell(i)%elev
│         │  stage = canal_bed + depth
│         
│         ├── [if gw_state(cell_id)%head < canal_bed]
│      
│      ├── [if Q < 0]
│         
│         ├── [if -Q .ge.gw_state(cell_id)%stor]
│      
│      ├── [if Q > gw_canl_div_info(canal_id)%stor]
│
▼
</pre>


---

## gwflow_output_day

this subroutine opens all gwflow output files and writes headers

**Called from:** [`gwflow_simulate`](#gwflow_simulate)

Source: `gwflow_output.f90`

<pre>
gwflow_output_day
│
│  !! solute mass balance; accumulates to monthly/yearly/aa totals
│  character(len=16) :: obs_name
│
▼
</pre>


---

## gwflow_output_mon

this subroutine opens all gwflow output files and writes headers

**Called from:** [`gwflow_simulate`](#gwflow_simulate)

Source: `gwflow_output.f90`

<pre>
gwflow_output_mon
│
│  !! daily flow rates; basin-level water/heat/solute balance; HRU pumping
│  character(len=16) :: obs_name
│
├── [if time%end_mo == 1]
│   │  day_mo_r = real(time%day_mo)
│   
│   ├── [loop i=1,ncell]
│      │  gw_state(i)%hdmo = gw_state(i)%hdmo / day_mo_r
│   
│   ├── [if gw_heat_flag == 1]
│      
│      ├── [loop i=1,ncell]
│         │  gwheat_state(i)%tpmo = gwheat_state(i)%tpmo / day_mo_r
│      
│      ├── [loop i=1,ncell]
│         │  gwheat_state(i)%tpmo = 0.
│   
│   ├── [if gw_solute_flag == 1]
│      
│      ├── [loop s=1,gw_nsolute]
│         
│         ├── [loop i=1,ncell]
│         │  gwsol_state(i)%solute(s)%cnmo = gwsol_state(i)%solute(s)%cnmo / day_mo_r
│      
│      ├── [loop i=1,ncell]
│         │  gwsol_state(i)%solute(s)%cnmo = 0.
│
├── [if gwflag_pump == 1]
│   
│   ├── [loop i=1,sp_ob%hru]
│      
│      ├── [if hru_pump_mo(i) > 0.]
│         │  iob = sp_ob1%hru + i - 1
│         │  write(out_hru_pump_mo,8101) time%day,time%mo,time%day_mo,           
│
├── [loop i=1,ncell]
│   │  gw_hyd_ss_mo(i)%rech = gw_hyd_ss_mo(i)%rech / day_mo_r
│
▼
</pre>


---

## gwflow_output_yr

this subroutine opens all gwflow output files and writes headers

**Called from:** [`gwflow_simulate`](#gwflow_simulate)

Source: `gwflow_output.f90`

<pre>
gwflow_output_yr
│
│  !! daily flow rates; basin-level water/heat/solute balance; HRU pumping
│  character(len=16) :: obs_name
│  if(time%end_yr /= 1) return
│  day_yr_r = real(time%day_end_yr)
│
├── [loop i=1,ncell]
│   │  gw_state(i)%hdyr = gw_state(i)%hdyr / day_yr_r
│
├── [if gw_heat_flag == 1]
│   
│   ├── [loop i=1,ncell]
│      │  gwheat_state(i)%tpyr = gwheat_state(i)%tpyr / day_yr_r
│   
│   ├── [loop i=1,ncell]
│      │  gwheat_state(i)%tpyr = 0.
│
├── [if gw_solute_flag == 1]
│   
│   ├── [loop s=1,gw_nsolute]
│      
│      ├── [loop i=1,ncell]
│         │  gwsol_state(i)%solute(s)%cnyr = gwsol_state(i)%solute(s)%cnyr / day_yr_r
│      
│      ├── [loop i=1,ncell]
│         │  gwsol_state(i)%solute(s)%cnyr = 0.
│
├── [loop i=1,ncell]
│   │  gw_hyd_ss_yr(i)%rech = gw_hyd_ss_yr(i)%rech / day_yr_r
│   │  gw_hyd_ss_yr(i)%gwet = gw_hyd_ss_yr(i)%gwet / day_yr_r
│
▼
</pre>


---

## cs_sorb_aqu

this subroutine updates constituent concentrations based on sorption in the aquifer

**Called from:** [`aqu_1d_control`](#aqu_1d_control)

Source: `cs_sorb_aqu.f90`

<pre>
cs_sorb_aqu
│
│  !! this subroutine updates constituent concentrations based on sorption in the aquifer
│  iaq = ob(icmd)%num
│  gw_volume = (aqu_d(iaq)%stor/1000.)*(ob(icmd)%area_ha*10000.)
│  iaqdb = ob(icmd)%props
│  aqu_volume = (ob(icmd)%area_ha*10000.) * aqudb(iaqdb)%dep_bot * (1-aqu_d
│  aqu_mass = aqu_volume * aqu_bd
│  sorbed_seo4 = cs_aqu(iaq)%cs_sorb(1)
│  sorbed_seo3 = cs_aqu(iaq)%cs_sorb(2)
│  sorbed_born = cs_aqu(iaq)%cs_sorb(3)
│  mass_seo4_sorb = sorbed_seo4 * 1.e6 * ob(icmd)%area_ha
│  mass_seo3_sorb = sorbed_seo3 * 1.e6 * ob(icmd)%area_ha
│
├── [if gw_volume > 0]
│
▼
</pre>


---

## res_hydro

**Called from:** [`res_control`](#res_control), [`wetland_control`](#wetland_control)

Source: `res_hydro.f90`

<pre>
res_hydro
│  character(len=1) :: action = ""
│
│  !! Jose T 2025 |  HYPE model for HP method
│  dom          = time%day_mo
│  mon          = time%mo
│  end_of_mo    = time%end_mo
│
│  !! store initial values
│  vol = wbody%flo
│  wsa1 = wbody_wb%area_ha * 10000.
│
├── [if time%step>0]
│   │  nstep = time%step
│
├── [loop tstep = 1,]
│   
│   ├── [loop iac = 1,]
│      │  action = "n"
│      
│      ├── [if d_tbl%alts == 0]
│         │  action = "y"
│         
│         ├── [loop ial = 1,]
│         
│         ├── [if d_tbl%act_hit(ial) == "y" .and. d_tbl%act_outc]
│         │  action = "y"
│
├── [if action == "y"]
│   
│   ├── [select case (d_tbl%act(iac)%option)]
│      
│      │  !! release at constant rate
│      
│      │  !! release at percentage of principal volume
│      
│      │  !! JK: added functionality to use const2 to reduce/increase inflow variable - const is max 
│      
│      │  !! release at fraction of inflow
│
▼
</pre>


---

## res_sediment

**Called from:** [`res_control`](#res_control), [`wetland_control`](#wetland_control)

Source: `res_sediment.f90`

<pre>
res_sediment
│
├── [if wbody%flo < 1.e-6]
│   │  wbody = hz
│   │  wbody%sed = 0.
│   │  ht2%sed = 0.
│   │  sed_ppm = 1.e-6
│   │  sil_ppm = 1.e-6
│   │  cla_ppm = 1.e-6
│   
│   │  !! compute concentrations
│   │  sed_ppm = 1000000. * wbody%sed / wbody%flo
│   │  sed_ppm = Max(1.e-6, sed_ppm)
│   │  sil_ppm = 1000000. * wbody%sil / wbody%flo
│   │  sil_ppm = Max(1.e-6, sil_ppm)
│   
│   │  !! compute change in sediment concentration due to settling
│   
│   ├── [if sed_ppm > wbody_prm%sed%nsed]
│      
│      │  !! update wetland sediment after settling
│      
│      │  !! calculate sediment in the outflow and subtract from wetland
│      
│      │  !! assume all sand aggregates and gravel settles
│   
│   │  !! compute sediment leaving reservoir - ppm -> t
│
▼
</pre>


---

## gwflow_reservoir

this subroutine calculates the water exchange volume between the reservoir and the connected grid cells

**Called from:** [`res_control`](#res_control)

Source: `gwflow_reservoir.f90`

<pre>
gwflow_reservoir
│
│  !! (exchange volumes are used in gwflow_simulate, in groundwater balance equations)
│
├── [if gw_res_flag == 1]
│   │  res_volume = res(res_id)%flo
│   
│   ├── [loop k=1,gw_resv_info(res_id)%ncon]
│      │  res_cell_id = gw_resv_info(res_id)%cells(k)
│      
│      ├── [loop jj=1,gw_state(res_cell_id)%ncon]
│         │  cell_id = cell_con(res_cell_id)%cell_id(jj)
│         
│         ├── [if gw_state(cell_id)%stat == 1]
│         │  area_res_cell = gw_state(res_cell_id)%area
│         │  area_cell = gw_state(cell_id)%area
│         │  min_area = min(area_res_cell,area_cell)
│         │  conn_length = sqrt(min_area)
│         │  head_diff = gw_resv_info(res_id)%elev(k) - gw_state(cell_id)%head
│         │  res_K = gw_resv_info(res_id)%hydc(k)
│         │  res_thick = gw_resv_info(res_id)%thck(k)
│         
│         ├── [if Q > 0]
│         
│         ├── [if (Q+seep_total) > res(res_id)%flo]
│      
│      ├── [if -Q .ge.gw_state(cell_id)%stor]
│   
│   ├── [if gw_heat_flag == 1]
│      
│      ├── [if Q < 0]
│         
│         ├── [if -heat_flux >= gwheat_state(cell_id)%stor]
│
▼
</pre>


---

## res_nutrient

**Called from:** [`res_control`](#res_control), [`wetland_control`](#wetland_control)

Source: `res_nutrient.f90`

<pre>
res_nutrient
│
│  !! zero and perform no nutrient calculations
│
├── [if wbody%flo < 1.e-6]
│   │  wbody = resz
│
│  !! if reservoir volume greater than 1 m^3, perform nutrient calculations
│
├── [if time%mo >= wbody_prm%nut%ires1 .and. time%mo <]
│   │  nsetlr = wbody_prm%nut%nsetlr1
│   │  psetlr = wbody_prm%nut%psetlr1
│   │  nsetlr = wbody_prm%nut%nsetlr2
│   │  psetlr = wbody_prm%nut%psetlr2
│  nsolr = wbody_prm%nut%nsolr
│  psolr = wbody_prm%nut%psolr
│
│  !! n and p concentrations kg/m3 * kg/1000 t * 1000000 ppp = 1000
│  conc_n = 1000. * wbody%orgn / wbody%flo
│  conc_p = 1000. * wbody%sedp / wbody%flo
│  conc_soln = 1000. * (wbody%no3 + wbody%nh3 + wbody%no2) / wbody%flo
│
│  !! Ikenberry wetland eqs modified - not function of area - fraction of difference in concen
│
│  !! other part of equation 29.1.3 in SWAT manual
│
│  !! calculate chlorophyll-a and water clarity
│
▼
</pre>


---

## res_pest

this subroutine computes the lake hydrologic pesticide balance.

**Called from:** [`res_control`](#res_control)

Source: `res_pest.f90`

<pre>
res_pest
│
│  !! this subroutine computes the lake hydrologic pesticide balance.
│
├── [if res(jres)%flo > 1.]
│   
│   ├── [loop ipst = 1,]
│      │  icmd = res_ob(jres)%ob
│      │  idb = ob(icmd)%props
│      │  ipest_db = cs_db%pest_num(ipst)
│      │  jsed = res_dat(idb)%sed
│      │  respst_d(jres)%pest(ipst)%tot_in = obcs(icmd)%hin(1)%pest(ipst)
│      │  tpest1 = obcs(icmd)%hin(1)%pest(ipst) + res_water(jres)%pest(ipst)
│      │  tpest2 = res_benthic(jres)%pest(ipst)
│      
│      │  !! calculate average depth of reservoir
│      │  depth = res(jres)%flo / (res_wat_d(jres)%area_ha * 10000.)
│      
│      │  !! water column --> kg sed/L water = t/m3 = t / (m3 - (t * m3/t)) --> sedvol = sed/particle
│      │  sedmass_watervol = (res(jres)%sed) / (res(jres)%flo - (res(jres)%sed / 2
│      │  kd = pestdb(ipest_db)%koc * res_sed(jsed)%carbon / 100.
│      
│      │  !! benthic layer --> kg sed/L water = t/m3 = bd (t sed/m3 total) / por --> por*total gives 
│      
│      │  !! determine pesticide lost through reactions in water layer
│      
│      ├── [if pest_init > 1.e-12]
│         
│         │  !! add decay to daughter pesticides
│         
│         ├── [loop imeta = 1,]
│   
│   │  !! determine pesticide lost through volatilization
│   
│   ├── [if volatpst > tpest1]
│
▼
</pre>


---

## res_salt

this subroutine computes the reservoir salt ion balance

**Called from:** [`res_control`](#res_control)

Source: `res_salt.f90`

<pre>
res_salt
│
│  !! this subroutine computes the reservoir salt ion balance
│  icmd = res_ob(jres)%ob
│
├── [loop isalt=1,cs_db%num_salts]
│   │  ressalt_d(jres)%salt(isalt)%inflow = 0.
│   │  ressalt_d(jres)%salt(isalt)%outflow = 0.
│   │  ressalt_d(jres)%salt(isalt)%seep = 0.
│   │  ressalt_d(jres)%salt(isalt)%mass = 0.
│   │  ressalt_d(jres)%salt(isalt)%conc = 0.
│
├── [if res(jres)%flo > 1.]
│   
│   ├── [loop isalt=1,cs_db%num_salts]
│      │  salt_mass_beg = res_water(jres)%salt(isalt)
│      │  salt_conc_beg = res_water(jres)%saltc(isalt)
│      │  mass_avail = salt_mass_beg
│      │  salt_inflow = obcs(icmd)%hin(1)%salt(isalt)
│      
│      ├── [if salt_outflow > mass_avail]
│      
│      ├── [if salt_seep > mass_avail]
│
▼
</pre>


---

## res_cs

this subroutine computes the reservoir constituent mass balance

**Called from:** [`res_control`](#res_control)

Source: `res_cs.f90`

<pre>
res_cs
│
│  !! this subroutine computes the reservoir constituent mass balance
│  icmd = res_ob(jres)%ob
│
├── [loop ics=1,cs_db%num_cs]
│   │  rescs_d(jres)%cs(ics)%inflow = 0.
│   │  rescs_d(jres)%cs(ics)%outflow = 0.
│   │  rescs_d(jres)%cs(ics)%seep = 0.
│   │  rescs_d(jres)%cs(ics)%settle = 0.
│   │  rescs_d(jres)%cs(ics)%rctn = 0.
│   │  rescs_d(jres)%cs(ics)%irrig = 0.
│   │  rescs_d(jres)%cs(ics)%mass = 0.
│   │  rescs_d(jres)%cs(ics)%conc = 0.
│
├── [if res(jres)%flo > 1.]
│   
│   ├── [loop ics=1,cs_db%num_cs]
│      │  cs_mass_beg = res_water(jres)%cs(ics)
│      
│      ├── [if cs_outflow > mass_avail]
│      
│      ├── [if cs_seep > mass_avail]
│      
│      ├── [if ics == 1]
│      
│      ├── [if cs_settle > mass_avail]
│
▼
</pre>


---

## gwflow_channel_exch

this subroutine calculates the water exchange volume between the channel and the connected grid cells

**Called from:** [`sd_channel_control3`](#sd_channel_control3)

Source: `gwflow_channel_exch.f90`

<pre>
gwflow_channel_exch
│
│  !! (exchange volumes are used in gwflow_simulate, in groundwater balance equations)
│  chan_volume = ch_stor(chan_id)%flo
│  chan_depth = sd_ch(chan_id)%chd
│  chan_width = sd_ch(chan_id)%chw
│
├── [loop k=1,gw_chan_info(chan_id)%ncon]
│   │  cell_id = gw_chan_info(chan_id)%cells(k)
│   
│   ├── [if gw_state(cell_id)%stat == 1]
│      │  chan_length = gw_chan_info(chan_id)%leng(k)
│      │  bed_elev = gw_chan_info(chan_id)%elev(k) - gw_bed_change
│      │  bed_K = gw_chan_info(chan_id)%hydc(k)
│      │  bed_thick = gw_chan_info(chan_id)%thck(k)
│      
│      ├── [if gw_chan_dep_flag == 1]
│         │  chan_depth = gw_chan_dep(gw_chan_info(chan_id)%dpzn(k))
│      │  chan_stage = bed_elev + chan_depth
│      
│      ├── [if gw_head < bed_elev]
│      
│      ├── [if Q < 0]
│         
│         ├── [if -Q >= gw_state(cell_id)%stor]
│      
│      ├── [if Q > ch_stor(chan_id)%flo]
│
▼
</pre>


---

## gwflow_canal

this subroutine calculates the water exchange volume between irrigation canals and connected grid cells

**Called from:** [`sd_channel_control3`](#sd_channel_control3)

Source: `gwflow_canal.f90`

<pre>
gwflow_canal
│
│  !! that remove water from a specified channel.
│
├── [if gw_canal_flag == 1]
│   │  chan_volume = ch_stor(chan_id)%flo
│   
│   ├── [if chan_volume > 10.]
│      │  chan_csol(1) = (ch_stor(chan_id)%no3 * 1000.) / chan_volume
│      │  chan_csol(2) = (ch_stor(chan_id)%solp * 1000.) / chan_volume
│      
│      ├── [if gwsol_salt == 1]
│         
│         ├── [loop isalt=1,cs_db%num_salts]
│         │  sol_index = sol_index + 1
│         │  chan_csol(sol_index) = (ch_water(chan_id)%salt(isalt) * 1000.) / chan_vo
│   
│   ├── [if gwsol_cons  == 1]
│      
│      ├── [loop ics=1,cs_db%num_cs]
│         │  sol_index = sol_index + 1
│         │  chan_csol(sol_index) = (ch_water(chan_id)%cs(ics) * 1000.) / chan_volume
│   │  conc_nh3 = (ch_stor(chan_id)%nh3 * 1000.) / chan_volume
│   │  conc_no2 = (ch_stor(chan_id)%no2 * 1000.) / chan_volume
│   │  conc_dox = (ch_stor(chan_id)%dox * 1000.) / chan_volume
│
├── [loop c=1,gw_chan_canl_info(chan_id)%ncanal]
│   
│   ├── [if time%day.ge.day_beg .and. time%day.le.day_end]
│      
│      ├── [loop k=1,gw_canl_info(canal_id)%ncon]
│         
│         ├── [if gw_state(cell_id)%stat == 1]
│
▼
</pre>


---

## gwflow_tile

this subroutine calculates the water exchange volume between irrigation canals and connected grid cells

**Called from:** [`sd_channel_control3`](#sd_channel_control3)

Source: `gwflow_tile.f90`

<pre>
gwflow_tile
│
│  !! (exchange volumes are used in gwflow_simulate, in groundwater balance equations)
│
├── [if gw_tile_flag == 1]
│   │  chan_volume = ch_stor(chan_id)%flo
│   
│   ├── [loop k=1,gw_tile_info(chan_id)%ncon]
│      │  cell_id = gw_tile_info(chan_id)%cells(k)
│      
│      ├── [if gw_state(cell_id)%stat == 1]
│         │  tile_elev = gw_state(cell_id)%elev - gw_tile_depth(cell_id)
│         
│         ├── [if gw_state(cell_id)%head > tile_elev]
│         │  head_diff = gw_state(cell_id)%head - tile_elev
│         │  Q = gw_tile_drain_area(cell_id) * gw_tile_K(cell_id) * head_diff
│         
│         ├── [if Q > gw_state(cell_id)%stor]
│         │  Q = gw_state(cell_id)%stor
│      │  gw_state(cell_id)%stor = gw_state(cell_id)%stor - Q
│      │  gw_hyd_ss(cell_id)%tile = Q * (-1)
│      │  gw_hyd_ss_yr(cell_id)%tile = gw_hyd_ss_yr(cell_id)%tile + (Q*(-1))
│      │  gw_hyd_ss_mo(cell_id)%tile = gw_hyd_ss_mo(cell_id)%tile + (Q*(-1))
│      
│      ├── [if gw_heat_flag == 1]
│         
│         ├── [if heat_flux >= gwheat_state(cell_id)%stor]
│      
│      ├── [if ch_stor(chan_id)%flo > 0]
│   
│   ├── [if gw_solute_flag == 1]
│      
│      ├── [loop s=1,gw_nsolute]
│         
│         ├── [if solmass(s) > gwsol_state(cell_id)%solute(s)%ma]
│
▼
</pre>


---

## gwflow_satexcess

this subroutine calculates the groundwater volume that enters the channel via saturation excess flow

**Called from:** [`sd_channel_control3`](#sd_channel_control3)

Source: `gwflow_satexcess.f90`

<pre>
gwflow_satexcess
│
│  !! (exchange volumes are used in gwflow_simulate, in groundwater balance equations)
│
├── [if chan_id == 3]
│
├── [if gw_satx_flag == 1]
│   
│   ├── [loop k=1,gw_satx_info(chan_id)%ncon]
│      │  cell_id = gw_satx_info(chan_id)%cells(k)
│      
│      ├── [if gw_state(cell_id)%stat == 1]
│         
│         ├── [if gw_state(cell_id)%head > gw_state(cell_id)%ele]
│         │  satx_count = satx_count + 1
│         │  satx_depth = gw_state(cell_id)%head - gw_state(cell_id)%elev
│         │  satx_volume = (gw_state(cell_id)%area * satx_depth) * gw_state(cell_id)%
│         │  gw_hyd_ss(cell_id)%satx = satx_volume * (-1)
│         │  gw_hyd_ss_yr(cell_id)%satx = gw_hyd_ss_yr(cell_id)%satx + (satx_volume *
│         │  gw_hyd_ss_mo(cell_id)%satx = gw_hyd_ss_mo(cell_id)%satx + (satx_volume *
│         │  chan_flow = ch_stor(chan_id)%flo
│         │  ch_stor(chan_id)%flo = ch_stor(chan_id)%flo + satx_volume
│         
│         ├── [if gw_heat_flag == 1]
│         │  chan_temp = ch_stor(chan_id)%temp
│         
│         ├── [if heat_flux > gw_heat]
│      
│      ├── [if ch_stor(chan_id)%flo > 0]
│   
│   ├── [if gw_solute_flag == 1]
│      
│      ├── [loop s=1,gw_nsolute]
│
▼
</pre>


---

## ch_rtpest

this subroutine computes the daily stream pesticide balance

**Called from:** [`sd_channel_control3`](#sd_channel_control3)

Source: `ch_rtpest.f90`

<pre>
ch_rtpest
│
│  !! zero outputs
│  chpst_d(jrch) = chpstz
│
│  !! initialize depth of water for pesticide calculations
│  depth = rcurv%dep
│
├── [if depth < 0.01]
│   │  depth = .01
│
├── [loop ipest = 1,]
│   │  jpst = cs_db%pest_num(ipest)
│   
│   │  !! volume of water entering reach and stored in reach
│   │  wtrin = ht1%flo + ch_stor(jrch)%flo
│   
│   │  !! pesticide transported into reach during day
│   │  pstin = hcs1%pest(ipest)
│   
│   │  !! calculate mass of pesticide in reach
│   │  chpstmass = pstin + ch_water(jrch)%pest(ipest)
│   
│   │  !! calculate mass of pesticide in bed sediment
│   │  sedpstmass = ch_benthic(jrch)%pest(ipest)
│   
│   ├── [if chpstmass + sedpstmass < 1.e-12]
│      │  ch_water(jrch)%pest(ipest) = 0.
│      │  ch_benthic(jrch)%pest(ipest) = 0.
│   
│   │  !! in-stream processes
│   
│   ├── [if wtrin / 86400. > 1.e-9]
│      
│      │  !! calculate sediment concentration
│      
│      │  !! calculate fraction of soluble and sorbed pesticide
│      
│      │  !! ASSUME DENSITY=2.6E6; KD2=KD1
│      
│      │  !! calculate flow duration
│      
│      │  !! calculate amount of pesticide that undergoes chemical or biological degradation on day i
│      
│      ├── [if pest_init > 1.e-12]
│         
│         │  !! add decay to daughter pesticides
│         
│         ├── [loop imeta = 1,]
│   
│   │  !! calculate amount of pesticide that volatilizes from reach
│   
│   ├── [if chpst%pest(ipest)%volat > frsol * chpstmass]
│
▼
</pre>


---

## ch_rtpath

this subroutine routes bacteria through the stream network

**Called from:** [`sd_channel_control3`](#sd_channel_control3)

Source: `ch_rtpath.f90`

<pre>
ch_rtpath
│
│  !! SWAT: Theta
│
├── [if rtwtr > 0. .and. rchdep > 0.]
│   │  wtmp = 5.0 + 0.75 * wst(iwst)%weat%tave
│   
│   ├── [loop ipath = 1,]
│      
│      │  !! total pathogen mass in reach
│      │  path_tot = obcs(iob)%hd(1)%path(ipath) * ob(icmd)%hin%flo + ch(jrch)%bac
│      
│      │  !! compute pathogen die-off
│      │  tday = rttime / 24.0
│      │  path_tot = path_tot * Exp(-Theta(path_db(ipath)%do_stream, path_db(ipath
│      │  path_tot = Max(0., path_tot)
│      
│      │  !! new concentration
│      │  netwtr = ob(icmd)%hin%flo  + rchwtr
│      
│      │  !! change made by CS while running region 4; date 2 jan 2006
│      
│      ├── [if netwtr >= 1.]
│         │  ch_water(jrch)%path(ipath) = path_tot / netwtr
│         │  ch_water(jrch)%path(ipath) = 0.
│
▼
</pre>


---

## ch_temp

**Called from:** [`sd_channel_control3`](#sd_channel_control3)

Source: `ch_temp.f90`

<pre>
ch_temp
│  jday = time%day
│  iob = sp_ob1%chandeg + ich - 1
│  iwst = ob(iob)%wst
│  ig = wst(iwst)%wco%tgage
│  w = wst(iwst)%weat
│  tw_def = 5.0 + 0.75 * w%tave
│
├── [loop in = 1,]
│   
│   ├── [if allocated(ob(iob)%obtyp_in) .and. ob(iob)%obty]
│      │  ru_count = count(ob(iob)%obtyp_in == "ru")
│  ht1 = ob(iob)%hd(1)
│
├── [loop in = 1,]
│   
│   ├── [if ob(iob)%obtyp_in(in) == "chandeg"]
│      │  trib1_temp = ob(iob)%hin_d(in-1)%temp
│      │  trib2_temp = ob(iob)%hin_d(in)%temp
│      
│      ├── [if trib_flo < 1e-6]
│   
│   ├── [if ob(iob)%obtyp_in(in) == "ru"]
│
▼
</pre>


---

## cli_pgen

this subroutine generates precipitation data when the user chooses to

**Called from:** [`cli_precip_control`](#cli_precip_control)

Source: `cli_pgen.f90`

<pre>
cli_pgen
│
│  !! SWAT: Aunif, Dstn1
│  vv = Aunif(rndseed(idg(1),iwgn))
│
├── [if wst(iwst)%weat%precip_prior_day == "dry"]
│   │  xx = wgn(iwgn)%pr_wd(time%mo)
│   │  xx = wgn(iwgn)%pr_ww(time%mo)
│
├── [if vv > xx]
│   │  v8 = Aunif(rndseed(idg(3),iwgn))
│   
│   │  !! skewed rainfall distribution
│   │  r6 = wgn(iwgn)%pcpskw(time%mo) / 6.
│   │  xlv = (cli_Dstn1(rnd3(iwgn),v8) - r6) * r6 + 1.
│   │  xlv = (xlv**3 - 1.) * 2. / wgn(iwgn)%pcpskw(time%mo)
│   │  rnd3(iwgn) = v8
│   │  pcpgen = xlv * wgn(iwgn)%pcpstd(time%mo) + wgn_pms(iwgn)%pcpmean(time%mo
│   │  pcpgen = pcpgen * wgn_pms(iwgn)%pcf(time%mo)
│
│  !! precip for the next day
│
▼
</pre>


---

## cli_pgenhr

this subroutine distributes daily rainfall exponentially within the day

**Called from:** [`cli_precip_control`](#cli_precip_control)

Source: `cli_pgenhr.f90`

<pre>
cli_pgenhr
│
│  !! zero subdaily precip array
│  wst(iwst)%weat%ts = 0.
│
│  !! calculate peak rate using same method used for peak runoff
│  pkrr = 2. * wst(iwst)%weat%precip * wst(iwst)%weat%precip_half_hr
│
│  !! vv => time to peak expressed as fraction of total storm duration
│  blm = 0.05
│  qmn = 0.25
│  uplm = 0.95
│  vv = Atri(blm, qmn, uplm, k)
│
│  !! calculate storm duration
│  xk1 = vv / 4.605
│  xk2 = (1.- vv) / 4.605
│  dur = wst(iwst)%weat%precip / (pkrr * (xk1 + xk2))
│
├── [if dur > 24.0]
│   │  dur = 24.0
│
│  !! rainfall and time of peak rainfall in units of minutes
│
│  !! do while pt less than rtp
│
│  !! after peak rainfall and before end of storm
│
▼
</pre>


---

## cli_bounds_check

this subroutine checks to see if climate data is in current simulation day

**Called from:** [`cli_precip_control`](#cli_precip_control), [`climate_control`](#climate_control)

Source: `cli_bounds_check.f90`

<pre>
cli_bounds_check
│
│  !! this subroutine checks to see if climate data is in current simulation day
│  character(len=1) :: out_bounds
│
│  !! check id climate data starts before simulation
│
├── [if st_yr > time%yrc]
│   │  out_bounds = "y"
│   
│   ├── [if st_yr == time%yrc]
│      
│      ├── [if st_day > time%day]
│         │  out_bounds = "y"
│
│  !! check if climate data starts after simulation
│
├── [if end_yr < time%yrc]
│   │  out_bounds = "y"
│   
│   ├── [if end_yr == time%yrc]
│      
│      ├── [if end_day < time%day]
│         │  out_bounds = "y"
│
▼
</pre>


---

## cli_weatgn

this subroutine generates weather parameters used to simulate the impact

**Called from:** [`climate_control`](#climate_control)

Source: `cli_weatgn.f90`

<pre>
cli_weatgn
│
│  !! SWAT: Aunif, Dstn1
│  integer, dimension (2) :: zshape = 0
│  real, dimension (3,3) :: a = 0.
│  real, dimension (3,3) :: b = 0.
│  real, dimension (3) :: xx = 0.
│  real, dimension (3) :: e = 0.
│  zshape = (/3, 3/)
│  a = Reshape((/.567, .253, -.006, .086, .504, -.039, -.002, -.050,       
│  b = Reshape((/.781, .328, .238, 0., .637, -.341, 0., 0., .873/),        
│
│  !! set random number array values
│  v2 = Aunif(rndseed(idg(8),iwgn))
│  e(1) = cli_Dstn1(rnd8(iwgn),v2)
│
├── [loop n = 1,]
│   
│   ├── [loop l = 1,]
│
├── [loop n = 1,]
│
▼
</pre>


---

## cli_tgen

this subroutine generates temperature data when the user chooses to

**Called from:** [`climate_control`](#climate_control)

Source: `cli_tgen.f90`

<pre>
cli_tgen
│
│  !! Intrinsic: Abs
│  tamp = .5 * (wgn(iwgn)%tmpmx(time%mo) - wgn(iwgn)%tmpmn(time%mo))
│  txxm = wgn(iwgn)%tmpmx(time%mo) + tamp * wgn_pms(iwgn)%pr_wdays(time%mo)
│  tmxg = txxm + wgn(iwgn)%tmpstdmx(time%mo) * wgncur(1,iwgn)
│  tmng = (wgn(iwgn)%tmpmn(time%mo)) + wgn(iwgn)%tmpstdmn(time%mo) *  wgncu
│  wst(iwst)%weat%tmax = tmxg
│  wst(iwst)%weat%tmin = tmng
│
▼
</pre>


---

## cli_clgen

this subroutine calculates the daylength, distribution of

**Called from:** [`climate_control`](#climate_control)

Source: `cli_clgen.f90`

<pre>
cli_clgen
│
│  !! Reset prior day category for precipitation
│
├── [if wst(iwst)%weat%precip >= 0.1]
│   │  wst(iwst)%weat%precip_prior_day = "wet"
│   │  wst(iwst)%weat%precip_prior_day = "dry"
│
│  !! calculate solar declination: equation 2.1.2 in SWAT manual
│  sd = Asin(.4 * Sin((Real(time%day) - 82.) / 58.09))
│
│  !! calculate the relative distance of the earth from the sun the eccentricity of the orbit
│  dd = 1.0 + 0.033 * Cos(Real(time%day) / 58.09)
│
│  !! to 15 deg/hr or 0.2618 rad/hr and 2/0.2618 = 7.6374
│  sdlat = -wgn_pms(iwgn)%latsin * Tan(sd) / wgn_pms(iwgn)%latcos
│
├── [if sdlat > 1.]
│   │  elseif (sdlat >= -1.) then
│   │  h = Acos(sdlat)
│   │  h = 3.1416
│  wst(iwst)%weat%daylength = 7.6394 * h
│
│  !! equation 2.2.7 in SWAT manual
│  ys = wgn_pms(iwgn)%latsin * Sin(sd)
│
│  !! equation 2.2.10 in SWAT manual
│
├── [loop ii = 1,]
│   
│   │  !! in the morning and negative in the evening
│
├── [if totrho > 0.001]
│   
│   ├── [loop ii = 1,]
│
▼
</pre>


---

## cli_slrgen

this subroutine generates solar radiation

**Called from:** [`climate_control`](#climate_control)

Source: `cli_slrgen.f90`

<pre>
cli_slrgen
│
│  !! name        |units         |definition
│  rav = wgn(iwgn)%solarav(time%mo) / (1. - 0.5 * wgn_pms(iwgn)%pr_wdays(ti
│  rx = wst(iwst)%weat%solradmx - rav
│  wst(iwst)%weat%solrad = rav + wgncur(3,iwgn) * rx / 4.
│  wst(iwst)%weat%solrad = wgn(iwgn)%solarav(time%mo)
│
▼
</pre>


---

## cli_rhgen

this subroutine generates weather relative humidity

**Called from:** [`climate_control`](#climate_control)

Source: `cli_rhgen.f90`

<pre>
cli_rhgen
│
│  !! convert dewpoint to relative humidity (idewpt == 0)
│
├── [if wgn_pms(iwgn)%idewpt == 0]
│   │  tmpmean = (wgn(iwgn)%tmpmx(time%mo) + wgn(iwgn)%tmpmn(time%mo)) / 2.
│   │  rhmo = Ee(wgn(iwgn)%dewpt(time%mo)) / Ee(tmpmean)
│   │  rhmo = wgn(iwgn)%dewpt(time%mo)
│  yy = 0.9 * wgn_pms(iwgn)%pr_wdays(time%mo)
│  rhm = (rhmo - yy) / (1.0 - yy)
│  vv = rhm - 1.
│  uplm = rhm - vv * Exp(vv)
│  blm = rhm * (1.0 - Exp(-rhm))
│  wst(iwst)%weat%rhum = Atri(blm,rhm,uplm,rndseed(idg(7),iwgn))
│
▼
</pre>


---

## cli_wndgen

this subroutine generates wind speed

**Called from:** [`climate_control`](#climate_control)

Source: `cli_wndgen.f90`

<pre>
cli_wndgen
│
│  !! SWAT: Aunif
│  pi2 = 6.283185
│  mo = time%mo
│
│  !! Generate wind speed !!
│  v6 = Aunif(rndseed(idg(5),iwgn))
│  wst(iwst)%weat%windsp = wgn(iwgn)%windav(time%mo) * (-Log(v6)) ** 0.3
│
│  !! set to zero, no longer attempt to read in
│
├── [if iwndir > 0]
│   │  v7 = Aunif(rndseed(idg(5),iwgn))
│   
│   ├── [loop idir = 1,]
│      
│      ├── [if wnd_dir(iwndir)%dir(mo,idir) > v7]
│         │  idir1 = idir
│         │  idir2 = idir - 1
│   
│   ├── [if idir1 == 1]
│      │  g = v7 / wnd_dir(iwndir)%dir(mo,idir1)
│      │  g = (v7 - wnd_dir(iwndir)%dir(mo,idir2)) /            (wnd_dir(iwndir)%d
│   │  rdir2 = float (idir2)
│
▼
</pre>


---

## cond_real

current conditions include: w_stress, n_stress, phu_plant, phu_base0, soil_water, jday, month, vol

**Called from:** [`conditions`](#conditions)

Source: `cond_real.f90`

<pre>
cond_real
│
│  !! suppress unused variable warning
│
├── [loop ialt = 1,]
│   
│   ├── [if d_tbl%alt(ic,ialt) /= "-" .and. d_tbl%act_hit(]
│      
│      ├── [if d_tbl%alt(ic,ialt) == "<"]
│         
│         ├── [if var_cur >= var_tbl]
│         │  d_tbl%act_hit(ialt) = "n"
│   
│   ├── [if d_tbl%alt(ic,ialt) == ">"]
│      
│      ├── [if var_cur <= var_tbl]
│         │  d_tbl%act_hit(ialt) = "n"
│   
│   ├── [if d_tbl%alt(ic,ialt) == "<="]
│      
│      ├── [if var_cur > var_tbl]
│         │  d_tbl%act_hit(ialt) = "n"
│   
│   ├── [if d_tbl%alt(ic,ialt) == ">="]
│      
│      ├── [if var_cur < var_tbl]
│         │  d_tbl%act_hit(ialt) = "n"
│   
│   ├── [if d_tbl%alt(ic,ialt) == "="]
│      
│      ├── [if var_cur /= var_tbl]
│         │  d_tbl%act_hit(ialt) = "n"
│   
│   ├── [if d_tbl%alt(ic,ialt) == "/="]
│      
│      ├── [if var_cur == var_tbl]
│         │  d_tbl%act_hit(ialt) = "n"
│
▼
</pre>


---

## cond_integer

current conditions include: w_stress, n_stress, phu_plant, phu_base0, soil_water, jday, month, vol

**Called from:** [`conditions`](#conditions)

Source: `cond_integer.f90`

<pre>
cond_integer
│
├── [loop ialt = 1,]
│   
│   ├── [if d_tbl%alt(ic,ialt) /= "-" .and. d_tbl%act_hit(]
│      
│      ├── [if d_tbl%alt(ic,ialt) == "<"]
│         
│         ├── [if var_cur >= var_tbl]
│         │  d_tbl%act_hit(ialt) = "n"
│   
│   ├── [if d_tbl%alt(ic,ialt) == ">"]
│      
│      ├── [if var_cur <= var_tbl]
│         │  d_tbl%act_hit(ialt) = "n"
│   
│   ├── [if d_tbl%alt(ic,ialt) == "<="]
│      
│      ├── [if var_cur > var_tbl]
│         │  d_tbl%act_hit(ialt) = "n"
│   
│   ├── [if d_tbl%alt(ic,ialt) == ">="]
│      
│      ├── [if var_cur < var_tbl]
│         │  d_tbl%act_hit(ialt) = "n"
│   
│   ├── [if d_tbl%alt(ic,ialt) == "="]
│      
│      ├── [if var_cur /= var_tbl]
│         │  d_tbl%act_hit(ialt) = "n"
│   
│   ├── [if d_tbl%alt(ic,ialt) == "/="]
│      
│      ├── [if var_cur == var_tbl]
│         │  d_tbl%act_hit(ialt) = "n"
│
▼
</pre>


---

## pl_fert_wet

this subroutine applies N and P specified by date and

**Called from:** [`actions`](#actions), [`mgt_sched`](#mgt_sched)

Source: `pl_fert_wet.f90`

<pre>
pl_fert_wet
│  real, parameter :: rtof=0.5
│
│  !! ======================
│
├── [if bsn_cc%cswat == 1]
│   │  wet(j)%no3 = wet(j)%no3 + frt_kg * (1. - fertdb(ifrt)%fnh3n) * fertdb(if
│   │  wet(j)%nh3 = wet(j)%nh3 + frt_kg * fertdb(ifrt)%fnh3n * fertdb(ifrt)%fmi
│   │  wet(j)%solp = wet(j)%solp + frt_kg * fertdb(ifrt)%fminp
│   │  wet(j)%orgn = wet(j)%orgn + frt_kg * fertdb(ifrt)%forgn
│   │  wet(j)%sedp = wet(j)%sedp + frt_kg * fertdb(ifrt)%forgp
│
│  !! summary calculations
│  fertno3 = frt_kg * fertdb(ifrt)%fminn * (1. - fertdb(ifrt)%fnh3n)
│  fertnh3 = frt_kg * (fertdb(ifrt)%fminn * fertdb(ifrt)%fnh3n)
│  fertorgn = frt_kg * fertdb(ifrt)%forgn
│  fertsolp = frt_kg * fertdb(ifrt)%fminp
│
▼
</pre>


---

## pl_manure

this subroutine applies N and P specified by date and

**Called from:** [`actions`](#actions), [`mgt_sched`](#mgt_sched)

Source: `pl_manure.f90`

<pre>
pl_manure
│
│  !! ======================
│  rtof = man_coef%rtof
│
│  !! therefore: liquid = solids/(1 - frac_liquid) - solids
│  frac_solids = (1. - manure_om(ifrt)%frac_water)
│  liq_manure_kg = frt_kg/(frac_solids) - frt_kg
│  liq_manure_mm = liq_manure_kg * .0001
│
├── [loop l = 1,]
│   
│   ├── [if l == 1]
│      │  fr_ly = chemapp_db(fertop)%surf_frac
│      │  fr_ly = 1. - chemapp_db(fertop)%surf_frac
│   │  fr_mass = fr_ly * frt_kg
│   │  soil(j)%phys(l)%st = soil(j)%phys(l)%st + fr_ly * liq_manure_mm
│   │  soil1(j)%mn(l)%no3 = soil1(j)%mn(l)%no3 + fr_mass *             (1. - ma
│   
│   ├── [if bsn_cc%cswat == 0]
│      │  soil1(j)%tot(l)%n = soil1(j)%tot(l)%n + rtof * fr_mass *                
│   
│   ├── [if bsn_cc%cswat == 1]
│   
│   │  !! ===========================
│   
│   ├── [if bsn_cc%cswat == 1]
│      
│      │  !! allocate organic fertilizer to Slow N pool;
│      
│      ├── [if meta_fr < 0.01]
│
▼
</pre>


---

## salt_fert

this subroutine adds salt fertilizer to the soil profile

**Called from:** [`actions`](#actions), [`mgt_sched`](#mgt_sched)

Source: `salt_fert.f90`

<pre>
salt_fert
│
│  !! jj          |none          |HRU number
│  character(len=16) :: fert_type = ""
│
├── [if cs_db%num_salts > 0 .and. fert_salt_flag == 1]
│   
│   ├── [if ifrt > 0]
│      
│      ├── [loop l=1,2]
│         
│         ├── [if l == 1]
│         │  xx = chemapp_db(fertop)%surf_frac
│         │  xx = 1. - chemapp_db(fertop)%surf_frac
│      │  cs_soil(jj)%ly(l)%salt(1) = cs_soil(jj)%ly(l)%salt(1) + (xx * frt_kg * f
│      │  cs_soil(jj)%ly(l)%salt(2) = cs_soil(jj)%ly(l)%salt(2) + (xx * frt_kg * f
│      │  cs_soil(jj)%ly(l)%salt(3) = cs_soil(jj)%ly(l)%salt(3) + (xx * frt_kg * f
│      │  cs_soil(jj)%ly(l)%salt(4) = cs_soil(jj)%ly(l)%salt(4) + (xx * frt_kg * f
│      │  cs_soil(jj)%ly(l)%salt(5) = cs_soil(jj)%ly(l)%salt(5) + (xx * frt_kg * f
│      │  cs_soil(jj)%ly(l)%salt(6) = cs_soil(jj)%ly(l)%salt(6) + (xx * frt_kg * f
│      │  cs_soil(jj)%ly(l)%salt(7) = cs_soil(jj)%ly(l)%salt(7) + (xx * frt_kg * f
│      
│      ├── [if fert_type(1:1) == 'a']
│
▼
</pre>


---

## cs_fert

this subroutine adds constituent fertilizer to the soil profile

**Called from:** [`actions`](#actions), [`mgt_sched`](#mgt_sched)

Source: `cs_fert.f90`

<pre>
cs_fert
│
│  !! jj          |none          |HRU number
│
├── [if cs_db%num_cs > 0 .and. fert_cs_flag == 1]
│   
│   ├── [if ifrt > 0]
│      
│      ├── [loop l=1,2]
│         
│         ├── [if l == 1]
│         │  xx = chemapp_db(fertop)%surf_frac
│         │  xx = 1. - chemapp_db(fertop)%surf_frac
│      │  cs_soil(jj)%ly(1)%cs(1) = cs_soil(jj)%ly(1)%cs(1) + (xx * frt_kg * fert_
│      │  cs_soil(jj)%ly(1)%cs(2) = cs_soil(jj)%ly(1)%cs(2) + (xx * frt_kg * fert_
│      │  cs_soil(jj)%ly(1)%cs(3) = cs_soil(jj)%ly(1)%cs(3) + (xx * frt_kg * fert_
│      │  hcsb_d(jj)%cs(1)%fert = hcsb_d(jj)%cs(1)%fert + (xx * frt_kg * fert_cs(i
│      │  hcsb_d(jj)%cs(2)%fert = hcsb_d(jj)%cs(2)%fert + (xx * frt_kg * fert_cs(i
│      │  hcsb_d(jj)%cs(3)%fert = hcsb_d(jj)%cs(3)%fert + (xx * frt_kg * fert_cs(i
│
▼
</pre>


---

## mgt_harvbiomass

this subroutine performs the harvest operation for above ground biomass (no kill)

**Called from:** [`actions`](#actions), [`mgt_sched`](#mgt_sched)

Source: `mgt_harvbiomass.f90`

<pre>
mgt_harvbiomass
│
│  !! this subroutine performs the harvest operation for above ground biomass (no kill)
│  ipl = iplant
│  idp = pcom(j)%plcur(ipl)%idplt
│  hi_ovr = harvop_db(iharvop)%hi_ovr
│  harveff = harvop_db(iharvop)%eff
│
│  !! remove yield from seed, leaf, and stem - using hi_ovr for all parts
│  hi_tot = hi_ovr * harveff
│  harv_seed = hi_tot * pl_mass(j)%seed(ipl)
│  harv_leaf = hi_tot * pl_mass(j)%leaf(ipl)
│  harv_stem = hi_tot * pl_mass(j)%stem(ipl)
│  pl_yield = harv_seed + harv_leaf + harv_stem
│
│  !! check if above ground > minimum biomass to cut
│
├── [if pl_mass(j)%ab_gr(ipl)%m - pl_yield%m > harvop_]
│   
│   │  !! apply pest stress to harvest index - mass lost due to pests - don't add to residue
│   │  pl_yield = (1. - pcom(j)%plcur(ipl)%pest_stress) * pl_yield
│   
│   │  !! add plant carbon for printing
│   
│   │  !! adjust foliar and internal pesticide for plant removal
│   
│   ├── [loop k = 1,]
│      
│      │  !! calculate amount of pesticide removed with yield and clippings
│      
│      │  !! add pesticide in clippings to soil surface
│   
│   │  !! update remaining plant organic pools
│   
│   │  !! add clippings (biomass left behind) to surface residue pool
│   
│   │  !! update total residue pool
│   
│   ├── [loop npl = 1,]
│
▼
</pre>


---

## mgt_harvgrain

this subroutine performs the harvest grain only operation

**Called from:** [`actions`](#actions), [`mgt_sched`](#mgt_sched)

Source: `mgt_harvgrain.f90`

<pre>
mgt_harvgrain
│
│  !! name        |units         |definition
│  ipl = iplant
│  idp = pcom(j)%plcur(ipl)%idplt
│  harveff = harvop_db(iharvop)%eff
│
│  !! check for minimum harvest index
│  pcom(j)%plcur(ipl)%harv_idx = max (pcom(j)%plcur(ipl)%harv_idx, pldb(idp
│
│  !! remove seed mass from total plant mass and calculate yield
│  pl_mass(j)%tot(ipl) = pl_mass(j)%tot(ipl) - pl_mass(j)%seed(ipl)
│  pl_mass(j)%ab_gr(ipl) = pl_mass(j)%ab_gr(ipl) - pl_mass(j)%seed(ipl)
│  pl_yield = harveff * pl_mass(j)%seed(ipl)
│
│  !! apply pest stress to harvest index - mass lost due to pests - don't add to residue
│  pl_yield = (1. - pcom(j)%plcur(ipl)%pest_stress) * pl_yield
│
│  !! add plant carbon for printing
│  hpc_d(j)%harv_abgr_c = hpc_d(j)%harv_abgr_c + pl_yield%c
│
│  !! add seed mass from harveff to slow humus pool of soil - to preserve balances
│  harveff1 = 1. - harveff
│
│  !! zero seed mass
│
│  !! adjust foliar and internal pesticide for grain removal
│
├── [loop k = 1,]
│   
│   │  !! calculate amount of pesticide removed with yield
│
▼
</pre>


---

## mgt_harvresidue

this subroutine performs the harvest residue operation

**Called from:** [`actions`](#actions), [`mgt_sched`](#mgt_sched)

Source: `mgt_harvresidue.f90`

<pre>
mgt_harvresidue
│
│  !! name        |units         |definition
│
│  !! prevent the harvest efficiency from being too small
│
├── [if harveff < .00001]
│   │  eff = harvop_db(iharvop)%eff
│   │  eff = harveff
│  harv_idx = harvop_db(iharvop)%hi_ovr
│  bm_min = harvop_db(iharvop)%bm_min
│  net_eff = eff * harv_idx
│
│  !! zero stover harvest
│  hrc_d(j)%harv_stov_c = 0.
│
│  !! harvest plant surface residue
│  rsd_removed = orgz
│
├── [loop ipl = 1,]
│   
│   │  !! compute carbon in harvested residue
│   │  rsd_removed = net_eff * pl_mass(j)%rsd(ipl)
│   
│   ├── [if (pl_mass(j)%rsd(ipl)%m - rsd_removed%m) < bm_m]
│      │  reduction_frac = (pl_mass(j)%rsd(ipl)%m - bm_min) / pl_mass(j)%rsd(ipl)%
│      │  rsd_removed = reduction_frac * pl_mass(j)%rsd(ipl)
│   
│   ├── [if pl_mass(j)%rsd_tot%m < 1.e-6]
│
▼
</pre>


---

## pest_apply

this subroutine applies pesticide

**Called from:** [`actions`](#actions), [`mgt_sched`](#mgt_sched)

Source: `pest_apply.f90`

<pre>
pest_apply
│
│  !! this subroutine applies pesticide
│
│  !! calculate ground cover
│  gc = (1.99532 - erfc(1.333 * pcom(j)%lai_sum - 2.)) / 2.1
│
│  !! update pesticide levels on ground and foliage
│
├── [if pcom(j)%lai_sum > 1.e-6]
│   
│   ├── [loop ipl = 1,]
│      │  pl_frac = pcom(j)%plg(ipl)%lai / pcom(j)%lai_sum
│      │  cs_pl(j)%pl_on(ipl)%pest(ipest) = cs_pl(j)%pl_on(ipl)%pest(ipest) + gc *
│  surf_frac = chemapp_db(pestop)%surf_frac
│  cs_soil(j)%ly(1)%pest(ipest) = cs_soil(j)%ly(1)%pest(ipest) + (1. - gc) 
│  cs_soil(j)%ly(2)%pest(ipest) = cs_soil(j)%ly(2)%pest(ipest) + (1. - gc) 
│  hpestb_d(j)%pest(ipest)%apply_f = gc * pest_kg
│  hpestb_d(j)%pest(ipest)%apply_s = (1. - gc) * pest_kg
│
▼
</pre>


---

## mgt_newtillmix_wet

this subroutine mixes residue and nutrients in soil layers and ponding water during tillage

**Called from:** [`actions`](#actions), [`mgt_sched`](#mgt_sched)

Source: `mgt_newtillmix_wet.f90`

<pre>
mgt_newtillmix_wet
│
│  !! Intrinsic: Min, Max
│
│  !! =============
│  npmx = cs_db%num_pests
│
│  !! tillage operation
│  emix = tilldb(idtill)%effmix
│  dtil = tilldb(idtill)%deptil
│  tdep = wet_ob(jj)%depth * 1000. + dtil
│  smix(1) = smix(1) + wet(jj)%no3 / hru(jj)%area_ha
│  smix(3) = smix(3) + wet(jj)%nh3 / hru(jj)%area_ha
│  smix(4) = smix(4) + wet(jj)%solp / hru(jj)%area_ha
│  smix(8) = smix(8) + wet(jj)%orgn / hru(jj)%area_ha
│  smix(9) = smix(9) + wet(jj)%sedp / hru(jj)%area_ha
│
├── [loop l = 1,]
│   │  sol_mass(l) = (soil(jj)%phys(l)%thick / 1000.) * 10000. *             so
│
├── [if dtil > 0.]
│   
│   ├── [loop l = 1,]
│      
│      ├── [if soil(jj)%phys(l)%d <= dtil]
│         
│         │  !! msn = mass of soil not mixed for the layer
│
▼
</pre>


---

## hru_fr_change

**Called from:** [`actions`](#actions)

Source: `hru_fr_change.f90`

<pre>
hru_fr_change
│  character(len=25), intent (in) :: lsu_elem_upd
│  character(len=25), intent (in) :: ru_elem_upd
│
│  !! read data for each element in all routing units
│  inquire (file=ru_elem_upd, exist=i_exist)
│
├── [if i_exist .or. ru_elem_upd /= "null"]
│   │  open (107,file=ru_elem_upd)
│   │  read (107,*,iostat=eof) titldum
│   │  read (107,*,iostat=eof) header
│   
│   ├── [loop isp = 1,]
│      │  read (107,*,iostat=eof) i
│      │  read (107,*,iostat=eof) k, ru_elem(i)%name, ru_elem(i)%obtyp, ru_elem(i)
│      
│      ├── [loop idr = 1,]
│         
│         ├── [if ru_elem(i)%dr_name == dr_db(idr)%name]
│         
│         │  !! dr_om_num was previously xwalked with dr_db()%om_file
│         │  ru_elem(i)%dr = dr(dr_om_num(idr))
│  close (107)
│
│  !! read data for each element in all landscape cataloging units
│
├── [if i_exist .or. lsu_elem_upd /= "null"]
│   
│   ├── [loop isp = 1,]
│
▼
</pre>


---

## calsoft_plant_zero

**Called from:** [`calsoft_plant`](#calsoft_plant)

Source: `calsoft_plant_zero.f90`

<pre>
calsoft_plant_zero
│
├── [loop ireg = 1,]
│   
│   ├── [loop ilum = 1,]
│      │  plcal(ireg)%lum(ilum)%nbyr = 0
│      │  plcal(ireg)%lum(ilum)%precip_aa = 0.
│      │  plcal(ireg)%lum(ilum)%aa = plcal_z
│      │  plcal(ireg)%lum(ilum)%sim%yield = 0.
│      │  plcal(ireg)%lum(ilum)%ha = 0.
│
▼
</pre>


---

## gwflow_write_celldef

this subroutine opens all gwflow output files and writes headers

**Called from:** [`gwflow_output_init`](#gwflow_output_init)

Source: `gwflow_output.f90`

<pre>
gwflow_write_celldef
│
│  !! Maps cell index to spatial location for all output files.
│  open(out_gw_celldef, file='gwflow_cell_definition.txt')
│  write(out_gw_celldef,'(a)') 'cell_id  row  col        x_coord       
│
├── [loop i=1,ncell]
│   
│   ├── [if gw_state(i)%stat > 0]
│      
│      ├── [if grid_type == "structured"]
│         │  write(out_gw_celldef,'(i8,2i6,2f15.1,2i6,e15.4)')               i, c
│         │  write(out_gw_celldef,'(i8,2i6,2f15.1,2i6,e15.4)')               i, 0
│  close(out_gw_celldef)
│
▼
</pre>


---

## zero0

this subroutine initializes the values for some of the arrays

**Called from:** [`allocate_parms`](#allocate_parms)

Source: `zero0.f90`

<pre>
zero0
│
│  !! Green and Ampt storages for urban runoff
│
│  !! Initialization by balaji
│
│  !! drainmod tile equations   06/2006
│
▼
</pre>


---

## zero1

this subroutine initializes the values for some of the arrays

**Called from:** [`allocate_parms`](#allocate_parms)

Source: `zero1.f90`

<pre>
zero1
│
│  !! septic changes 6/07/10  jaehak
│
│  !! septic changes 1/29/09
│
▼
</pre>


---

## zero2

this subroutine zeros all array values

**Called from:** [`allocate_parms`](#allocate_parms)

Source: `zero2.f90`

<pre>
zero2
│
│  !! this subroutine zeros all array values
│
▼
</pre>


---

## zeroini

this subroutine zeros values for single array variables

**Called from:** [`allocate_parms`](#allocate_parms)

Source: `zeroini.f90`

<pre>
zeroini
│
│  !! this subroutine zeros values for single array variables
│
▼
</pre>


---

## pl_seed_gro

**Called from:** [`mgt_transplant`](#mgt_transplant), [`pl_grow`](#pl_grow), [`plant_init`](#plant_init)

Source: `pl_seed_gro.f90`

<pre>
pl_seed_gro
│  idp = pcom(j)%plcur(ipl)%idplt
│  iwst = ob(j)%wst
│  ajhi = pcom(j)%plcur(ipl)%harv_idx * 100. * pcom(j)%plcur(ipl)%phuacc / 
│
│  !! calculate plant ET values when heat units exceed 0.5
│
├── [if pcom(j)%plcur(ipl)%phuacc > 0.5]
│   │  pcom(j)%plg(ipl)%plet = pcom(j)%plg(ipl)%plet + ep_day
│   │  pcom(j)%plg(ipl)%plpet = pcom(j)%plg(ipl)%plpet + pet_day
│   
│   │  !! adjust harvest index for water stress
│   
│   ├── [if pcom(j)%plg(ipl)%plpet > 1.e-6]
│      │  etr = 100. * pcom(j)%plg(ipl)%plet / pcom(j)%plg(ipl)%plpet
│      │  ajhi_min = ajhi / 2.
│      │  ajhi = (ajhi - ajhi_min) * (etr / (etr + Exp(6.13 - .0883 * etr))) + ajh
│      
│      ├── [if ajhi >  pldb(idp)%hvsti]
│         │  ajhi = pldb(idp)%hvsti
│   
│   ├── [if j == 985]
│   
│   │  !! calc daily change in hi
│   │  dhi = ajhi - pcom(j)%plg(ipl)%hi_prev
│   
│   │  !! adjust harvest index for water stress
│   
│   │  !! adjust harvest index for temperature stress
│   
│   ├── [if temp_dif < 0. .and. pcom(j)%plcur(ipl)%phuacc ]
│
▼
</pre>


---

## pl_partition

**Called from:** [`mgt_transplant`](#mgt_transplant), [`pl_grow`](#pl_grow), [`plant_init`](#plant_init)

Source: `pl_partition.f90`

<pre>
pl_partition
│  idp = pcom(j)%plcur(ipl)%idplt
│
│  !! update plant mass for daily biomass/c increase
│  pl_mass(j)%tot(ipl)%m = pl_mass(j)%tot(ipl)%m + pl_mass_up%m
│  pl_mass(j)%tot(ipl)%c = pl_mass(j)%tot(ipl)%c + pl_mass_up%c
│
│  !! partition leaf and stem (stalk) and seed (grain) mass
│
├── [if pldb(idp)%typ == "perennial"]
│   │  leaf_frac_veg = 0.02
│   │  leaf_frac_veg = 0.30
│  leaf_mass_frac_veg = leaf_frac_veg * pcom(j)%plg(ipl)%lai / pcom(j)%plcu
│
│  !! partition root and above ground biomass for tuber crops
│
├── [if pldb(idp)%typ == "warm_annual_tuber" .or. pldb]
│   │  root_frac = pcom(j)%plg(ipl)%root_frac
│   
│   │  !! 1. = root + ab_gr + hi * ab_gr --> solve for ab_gr
│   │  ab_gr_frac = (1. - root_frac) / (1. + pcom(j)%plg(ipl)%hi_adj)
│   │  seed_mass_frac = 1. - root_frac - ab_gr_frac
│   │  leaf_mass_frac = leaf_mass_frac_veg * ab_gr_frac
│   
│   │  !! partition root and above ground biomass for all other annuals (above ground grain)
│
│  !! check if initializing
│
├── [if init == 0]
│   
│   │  !! first maintain root fraction - root mass/total mass
│   
│   ├── [if mass_act < mass_opt]
│   
│   │  !! next maintain harvest index on yield (seed/fruit) component
│   
│   ├── [if mass_act < mass_opt]
│
▼
</pre>


---

## pl_rootfr

This subroutine distributes dead root mass through the soil profile

**Called from:** [`mgt_harvtuber`](#mgt_harvtuber), [`mgt_killop`](#mgt_killop), [`pl_root_gro`](#pl_root_gro), [`plant_init`](#plant_init)

Source: `pl_rootfr.f90`

<pre>
pl_rootfr
│
│  !! March, 2009 further adjustments expected
│  pcom(j)%plg(ipl)%rtfr = 0.
│
├── [if pcom(j)%plg(ipl)%root_dep < 1.e-6]
│   │  pcom(j)%plg(ipl)%rtfr(1) = 1.
│  c = 0.022
│  d = 0.12029
│
├── [loop ly = 1,]
│   
│   ├── [if soil(j)%phys(ly)%d >= pcom(j)%plg(ipl)%root_de]
│      │  cum_rd = pcom(j)%plg(ipl)%root_dep
│      │  cum_rd = soil(j)%phys(ly)%d
│   │  x1 = (cum_rd - soil(j)%phys(ly)%thick) / pcom(j)%plg(ipl)%root_dep
│   │  x2 = cum_rd / pcom(j)%plg(ipl)%root_dep
│   │  xx1 = -b * x1
│   │  xx2 = -b * x2
│   
│   ├── [if cum_rf > 1.]
│
│  !! ensures that cumulative fractional root distribution = 1
│
├── [loop ly = 1,]
│
▼
</pre>


---

## chrc_interp

**Called from:** [`rcurv_interp_dep`](#rcurv_interp_dep), [`rcurv_interp_flo`](#rcurv_interp_flo)

Source: `sd_channel_module.f90`

<pre>
chrc_interp
│  rci%xsec_area = rc1%xsec_area + const * (rc2%xsec_area - rc1%xsec_area)
│  rci%surf_area = rc1%surf_area + const * (rc2%surf_area - rc1%surf_area)
│  rci%flo_rate = rc1%flo_rate + const * (rc2%flo_rate - rc1%flo_rate)
│  rci%dep = rc1%dep + const * (rc2%dep - rc1%dep)
│  rci%top_wid = rc1%top_wid + const * (rc2%top_wid - rc1%top_wid)
│  rci%vol = rc1%vol + const * (rc2%vol - rc1%vol)
│  rci%vol_ch = rc1%vol_ch + const * (rc2%vol_ch - rc1%vol_ch)
│  rci%vol_fp = rc1%vol_fp + const * (rc2%vol_fp - rc1%vol_fp)
│  rci%wet_perim = rc1%wet_perim + const * (rc2%wet_perim - rc1%wet_perim)
│  rci%ttime = rc1%ttime + const * (rc2%ttime - rc1%ttime)
│
▼
</pre>


---

## soil_awc_init

this subroutine initializes soil parameters based on awc

**Called from:** [`cal_parm_select`](#cal_parm_select)

Source: `soil_awc_init.f90`

<pre>
soil_awc_init
│
│  !! reset soil parameters based on awc
│  nly = soil(isol)%nly
│
├── [loop ly = 1,]
│   
│   │  !! calculate water content of soil at -1.5 MPa and -0.033 MPa
│   │  soil(isol)%phys(ly)%wp = 0.4 * soil(isol)%phys(ly)%clay * soil(isol)%phy
│   │  soil(isol)%phys(ly)%up = soil(isol)%phys(ly)%wp + soil(isol)%phys(ly)%aw
│   │  soil(isol)%phys(ly)%por = 1. - soil(isol)%phys(ly)%bd / 2.65
│   
│   ├── [if soil(isol)%phys(ly)%up >= soil(isol)%phys(ly)%]
│      │  soil(isol)%phys(ly)%up = soil(isol)%phys(ly)%por - .05
│      │  soil(isol)%phys(ly)%wp = soil(isol)%phys(ly)%up - soil(isol)%phys(ly)%aw
│      
│      ├── [if soil(isol)%phys(ly)%wp <= 0.]
│         │  soil(isol)%phys(ly)%up = soil(isol)%phys(ly)%por * .75
│         │  soil(isol)%phys(ly)%wp = soil(isol)%phys(ly)%por * .25
│   
│   │  !! compute drainable porosity and variable water table factor - Daniel
│   │  drpor = soil(isol)%phys(ly)%por - soil(isol)%phys(ly)%up
│   │  soil(isol)%ly(ly)%vwt = (437.13*drpor**2)-(95.08 * drpor)+8.257
│
│  !! initialize water/drainage coefs for each soil layer
│
├── [loop ly = 1,]
│
▼
</pre>


---

## soil_text_init

this subroutine initializes soil parameters based on awc

**Called from:** [`cal_parm_select`](#cal_parm_select)

Source: `soil_text_init.f90`

<pre>
soil_text_init
│
│  !! SWAT: Curno
│  sa = soil(isol)%phys(1)%sand / 100.
│  cl = soil(isol)%phys(1)%clay  / 100.
│  si = soil(isol)%phys(1)%silt / 100.
│
│  !! typical for mid-western soils in USA (Foster et al., 1980 0 Based on SWRRB
│  soil(isol)%det_san = sa * (1. - cl)** 2.49
│  soil(isol)%det_sil = 0.13 * si
│  soil(isol)%det_cla = 0.20 * cl
│
├── [if cl < .25]
│   │  soil(isol)%det_sag = 2.0 * cl
│   │  soil(isol)%det_sag = .57
│   │  soil(isol)%det_sag = .28 * (cl - .25) + .5
│  soil(isol)%det_lag = 1. - soil(isol)%det_san -           soil(isol)%det_
│
│  !! Soil not typical of mid-western USA The fraction won't add upto 1.0
│
├── [if soil(isol)%det_lag < 0.]
│
▼
</pre>


---

## gwflow_pump_allo

this subroutine determines the volume of groundwater that is extracted

**Called from:** [`wallo_withdraw`](#wallo_withdraw)

Source: `gwflow_pump_allo.f90`

<pre>
gwflow_pump_allo
│
│  !! adapted for swatplusGW water allocation (4-arg call from wallo_withdraw)
│  character*4 :: demand_type = "hru"
│
├── [if demand_type == "hru" .and. ob_id_src == 0]
│   │  hru_id = ob_id_dmd
│   
│   ├── [if hru_num_cells(hru_id) > 0]
│      │  cell_demand = demand_vol / hru_num_cells(hru_id)
│      
│      ├── [loop i=1,hru_num_cells(hru_id)]
│         │  cell_id = hru_cells(hru_id,i)
│         
│         ├── [if gw_state(cell_id)%head > gw_state(cell_id)%bot]
│         │  gwvol_avail = ((gw_state(cell_id)%head-gw_state(cell_id)%botm) * gw_stat
│      
│      ├── [if gwvol_avail < cell_demand]
│         │  gwvol_removed = gwvol_avail
│         │  gwvol_unmet = cell_demand - gwvol_avail
│         │  gwvol_removed = cell_demand
│      │  extracted = extracted + gwvol_removed
│      │  dmd_unmet = dmd_unmet + gwvol_unmet
│      
│      ├── [if gw_heat_flag == 1]
│         
│         ├── [if heat_flux >= gwheat_state(cell_id)%stor]
│
▼
</pre>


---

## hyd_min

**Called from:** [`wallo_treatment`](#wallo_treatment)

Source: `hydrograph_module.f90`

<pre>
hyd_min
│  hyd1%flo = hyd1%flo
│  hyd1%sed = amin1 (hyd1%sed, hyd2%sed)
│  hyd1%orgn = amin1 (hyd1%orgn, hyd2%orgn)
│  hyd1%sedp = amin1 (hyd1%sedp, hyd2%sedp)
│  hyd1%no3 = amin1 (hyd1%no3, hyd2%no3)
│  hyd1%solp = amin1 (hyd1%solp, hyd2%solp)
│  hyd1%chla = amin1 (hyd1%chla, hyd2%chla)
│  hyd1%nh3 = amin1 (hyd1%nh3, hyd2%nh3)
│  hyd1%no2 = amin1 (hyd1%no2, hyd2%no2)
│  hyd1%cbod = amin1 (hyd1%cbod, hyd2%cbod)
│
▼
</pre>


---

## hydcsout_conc_mass

**Called from:** [`wallo_treatment`](#wallo_treatment), [`wallo_use`](#wallo_use)

Source: `constituent_mass_module.f90`

<pre>
hydcsout_conc_mass
│  allocate (hydcs2%pest(cs_db%num_pests), source = 0.)
│  allocate (hydcs2%path(cs_db%num_paths), source = 0.)
│  allocate (hydcs2%hmet(cs_db%num_metals), source = 0.)
│  allocate (hydcs2%salt(cs_db%num_salts), source = 0.)
│  allocate (hydcs2%cs(cs_db%num_cs), source = 0.)
│
│  !! kg = ppm * m3 / 1000.
│
├── [loop ipest = 1,]
│   │  hydcs2%pest(ipest) =  vol_m3 * hydcs1%pest(ipest) / 1000.
│
├── [loop ipath = 1,]
│   │  hydcs2%path(ipath) =  vol_m3 * hydcs1%path(ipath) / 1000.
│
├── [loop ihmet = 1,]
│   │  hydcs2%hmet(ihmet) =  vol_m3 * hydcs1%hmet(ihmet) / 1000.
│
├── [loop isalt = 1,]
│   │  hydcs2%salt(isalt) =  vol_m3 * hydcs1%salt(isalt) / 1000.
│
├── [loop ics = 1,]
│   │  hydcs2%cs(ics) =  vol_m3 * hydcs1%cs(ics) / 1000.
│
▼
</pre>


---

## ionic_strength

this subroutine calculates salt ion concentrations based on equilibrium chemical reactions, for an HRU

**Called from:** [`salt_chem_aqu`](#salt_chem_aqu), [`salt_chem_hru`](#salt_chem_hru), [`salt_chem_soil_single`](#salt_chem_soil_single)

Source: `salt_chem_hru.f90`


---

## activity_coefficient

this subroutine calculates salt ion concentrations based on equilibrium chemical reactions, for an HRU

**Called from:** [`salt_chem_aqu`](#salt_chem_aqu), [`salt_chem_hru`](#salt_chem_hru), [`salt_chem_soil_single`](#salt_chem_soil_single)

Source: `salt_chem_hru.f90`

<pre>
activity_coefficient
│
├── [if I_Prep_in.LE.1e-1]
│   
│   ├── [loop ii = 1,7]
│      │  LAMDA(ii)= 10.0**(-A*CharBal(ii)**2.0               *(I_Prep_in**0.5/(1+
│   │  I_Prep_in = 0.5
│   
│   ├── [loop ii = 1,7]
│      │  LAMDA(ii)= 10.0**(-A*CharBal(ii)**2.0               *(I_Prep_in**0.5/(1+
│   
│   ├── [loop ii = 1,7]
│      │  LAMDA(ii)= 10.0**(-A*CharBal(ii)**2.0               *(I_Prep_in**0.5/(1+
│
▼
</pre>


---

## caco3

this subroutine calculates salt ion concentrations based on equilibrium chemical reactions, for an HRU

**Called from:** [`salt_chem_aqu`](#salt_chem_aqu), [`salt_chem_hru`](#salt_chem_hru), [`salt_chem_soil_single`](#salt_chem_soil_single)

Source: `salt_chem_hru.f90`

<pre>
caco3
│  M1 = Sol_CaCO3(c5)
│  M2 = Cal_Conc(c11)
│  M3 = Car_Conc(c22)
│  Ksp = salt_K1
│  Solv = 0.5*(-(M2+M3)+sqrt((M2+M3)**2-4*(M2*M3-Ksp)))
│  Trial_Ksp=M2*M3
│
├── [if Trial_Ksp.GT.Ksp]
│   │  PosSolv = abs(Solv)
│   │  CalCar_Prep = PosSolv
│   │  Solid_CaCO3 = M1+CalCar_Prep
│   │  Calcium_Conc = M2-CalCar_Prep
│
▼
</pre>


---

## mgco3

this subroutine calculates salt ion concentrations based on equilibrium chemical reactions, for an HRU

**Called from:** [`salt_chem_aqu`](#salt_chem_aqu), [`salt_chem_hru`](#salt_chem_hru), [`salt_chem_soil_single`](#salt_chem_soil_single)

Source: `salt_chem_hru.f90`

<pre>
mgco3
│  M1 = Sol_MgCO3(c5)
│  M2 = Mg_Conc(salt_c3)
│  M3 = Car_Conc(c22+1)
│  Ksp = salt_K2
│  Solv = 0.5*(-(M2+M3)+sqrt((M2+M3)**2-4*(M2*M3-Ksp)))
│  Trial_Ksp=M2*M3
│
├── [if Trial_Ksp.GT.Ksp]
│   │  PosSolv = abs(Solv)
│   │  MgCar_Prep = PosSolv
│   │  Solid_MgCO3 = M1+MgCar_Prep
│   │  Mag_Conc = M2-MgCar_Prep
│
▼
</pre>


---

## caso4

this subroutine calculates salt ion concentrations based on equilibrium chemical reactions, for an HRU

**Called from:** [`salt_chem_aqu`](#salt_chem_aqu), [`salt_chem_hru`](#salt_chem_hru), [`salt_chem_soil_single`](#salt_chem_soil_single)

Source: `salt_chem_hru.f90`

<pre>
caso4
│  M1 = Sol_CaSO4(c5)
│  M2 = Cal_Conc(c11+1)
│  M3 = Sul_Conc(salt_c4)
│  Ksp = salt_K3
│  Solv = 0.5*(-(M2+M3)+sqrt((M2+M3)**2-4*(M2*M3-Ksp)))
│  Trial_Ksp = M2 * M3
│
├── [if Trial_Ksp.gt.Ksp]
│   │  PosSolv = abs(Solv)
│   │  CalSul_Prep = PosSolv
│   │  Solid_CaSO4 = M1+CalSul_Prep
│   │  Calcium_Conc = M2-CalSul_Prep
│
▼
</pre>


---

## mgso4

this subroutine calculates salt ion concentrations based on equilibrium chemical reactions, for an HRU

**Called from:** [`salt_chem_aqu`](#salt_chem_aqu), [`salt_chem_hru`](#salt_chem_hru), [`salt_chem_soil_single`](#salt_chem_soil_single)

Source: `salt_chem_hru.f90`

<pre>
mgso4
│  M1 = Sol_MgSO4(c5)
│  M2 = Mg_Conc(salt_c3+1)
│  M3 = Sul_Conc(salt_c4+1)
│  Ksp = salt_K4
│  Solv = 0.5*(-(M2+M3)+sqrt((M2+M3)**2-4*(M2*M3-Ksp)))
│  Trial_Ksp=M2*M3
│
├── [if Trial_Ksp.GT.Ksp]
│   │  PosSolv = abs(Solv)
│   │  MgSul_Prep = PosSolv
│   │  Solid_MgSO4 = M1+MgSul_Prep
│   │  Mag_Conc = M2-MgSul_Prep
│
▼
</pre>


---

## nacl

this subroutine calculates salt ion concentrations based on equilibrium chemical reactions, for an HRU

**Called from:** [`salt_chem_aqu`](#salt_chem_aqu), [`salt_chem_hru`](#salt_chem_hru), [`salt_chem_soil_single`](#salt_chem_soil_single)

Source: `salt_chem_hru.f90`

<pre>
nacl
│  M1 = Sol_NaCl(c5)
│  M2 = Sod_Conc(c5)
│  M3 = Cl_Conc(c5)
│  Ksp = salt_K5
│  Solv = 0.5*(-(M2+M3)+sqrt((M2+M3)**2-4*(M2*M3-Ksp)))
│  Trial_Ksp=M2*M3
│
├── [if Trial_Ksp.GT.Ksp]
│   │  PosSolv = abs(Solv)
│   │  SodiumChloride_Prep = PosSolv
│   │  Solid_NaCl = M1+SodiumChloride_Prep
│   │  Sodium_Conc = M2-SodiumChloride_Prep
│
▼
</pre>


---

## cationexchange

this subroutine calculates salt ion concentrations based on equilibrium chemical reactions, for an HRU

**Called from:** [`salt_chem_aqu`](#salt_chem_aqu), [`salt_chem_hru`](#salt_chem_hru)

Source: `salt_chem_hru.f90`

<pre>
cationexchange
│  Con_Ca = upion2/40
│  Con_Mg = upion3/24
│  Con_Na = upion4/23
│  Con_K  = upion5/39
│
├── [if Con_Ca.gt.0 .and. Con_Mg.gt.0 .and.           ]
│   │  Sel_K1 = 0.7
│   │  Sel_K3 = 0.4
│   │  Sel_K4 = 0.2
│   │  XCAINI = 13.6
│   │  XMGINI = 0.17
│   │  XNAINI = 0.25
│   
│   ├── [if Con_Ca.LE.0]
│   
│   ├── [if Con_Mg.LE.0]
│   
│   ├── [if Con_Na.LE.0]
│   
│   ├── [if Con_K.LE.0]
│
▼
</pre>


---

## se_reactions_soil

**Called from:** [`cs_rctn_hru`](#cs_rctn_hru)

Source: `se_reactions_soil.f90`

<pre>
se_reactions_soil
│
│  !! suppress unused variable warning
│  cseo4 = conc_rg(1)
│  cseo3 = conc_rg(2)
│  cno3 = conc_rg(3)
│  o2 = cs_rct_soil(j)%oxy_soil
│  no3inhib = cs_rct_soil(j)%se_ino3 / (cs_rct_soil(j)%se_ino3 + cno3)
│  seo4red = cs_rct_soil(j)%kseo4 * cseo4 * no3inhib
│  seo3red = cs_rct_soil(j)%kseo3 * cseo3 * no3inhib
│  yseo4_o2 = 315.84 / 224.0
│  yseo4_no3 = 789.6 / 196.0
│
├── [loop kk=1,num_geol_shale]
│   │  ko2a = cs_rct_soil(j)%ko2a(kk)
│
▼
</pre>


---

## sq_dailycn

Calculates curve number for the day in the HRU

**Called from:** [`surface`](#surface)

Source: `sq_dailycn.f90`

<pre>
sq_dailycn
│
│  !! Intrinsic: Exp
│  sw_fac = wrt(1,j) - wrt(2,j) * soil(j)%sw
│
│  !! traditional CN method (function of soil water)
│
├── [if (soil(j)%sw + Exp(sw_fac)) > 0.001]
│   │  r2 = smx(j) * (1. - soil(j)%sw / (soil(j)%sw + Exp(sw_fac)))
│   │  r2 = smx(j)
│  r2 = Max(3.,r2)
│  cnday(j) = 25400. / (r2 + 254.)
│
▼
</pre>


---

## sq_crackflow

this surboutine modifies surface runoff to account for crack flow

**Called from:** [`surface`](#surface)

Source: `sq_crackflow.f90`

<pre>
sq_crackflow
│
│  !! surfq(:)    |mm H2O        |surface runoff in the HRU for the day
│
│  !! subtract crack flow from surface runoff
│
├── [if surfq(j) > voltot]
│   │  surfq(j) = surfq(j) - voltot
│   │  surfq(j) = 0.
│
├── [if time%step > 1]
│   │  voli = voltot
│   
│   ├── [loop ii = 1,]
│      
│      ├── [if hhqday(j,ii) > voli]
│         │  hhqday(j,ii) = hhqday(j,ii) - voli
│         │  voli = voli - hhqday(j,ii)
│         │  hhqday(j,ii) = 0.
│
▼
</pre>


---

## ero_pkq

this subroutine computes the peak runoff rate for each HRU

**Called from:** [`surface`](#surface)

Source: `ero_pkq.f90`

<pre>
ero_pkq
│
│  !! Intrinsic: Log, Exp
│  iob = hru(j)%obj_no
│  iwst = ob(iob)%wst
│
│  !! select method for peak rate calculation
│
├── [if bsn_cc%sed_det == 1]
│   
│   │  !! half hour rainfall intensity method
│   │  xx = (2. * tconc(j) * Log(1. - wst(iwst)%weat%precip_half_hr))
│   │  altc = 1. - exp(xx)
│   │  qp_cms = altc * qday / tconc(j)
│   │  qp_cms = qp_cms * hru(j)%km / 3.6
│   
│   │  !! convert ha-mm * mi2/259ha * in/25.4mm to mi2-in --> 1/6578.6
│   │  qp_cfs = bsn_prm%prf / 6578.6 * hru(j)%area_ha * qday / tconc(j)
│   │  qp_cms = qp_cfs / 35.3
│
▼
</pre>


---

## ero_eiusle

This subroutine computes the USLE erosion index (EI)

**Called from:** [`surface`](#surface)

Source: `ero_eiusle.f90`

<pre>
ero_eiusle
│
│  !! Intrinsic: Log, Log10
│  iob = hru(j)%obj_no
│  iwst = ob(iob)%wst
│
├── [if w%precip > 1.e-4]
│   │  xb = -2. * Log(1. - wst(iwst)%weat%precip_half_hr)
│   │  pkrf30 = 2. * w%precip * wst(iwst)%weat%precip_half_hr
│   │  pkrf = xb * w%precip
│   │  usle_ei = w%precip * (12.1 + 8.9 * (Log10(pkrf) - .4343)) * pkrf30 / 100
│   │  usle_eifac(j) = usle_ei
│
▼
</pre>


---

## ero_ovrsed

this subroutine computes splash erosion by raindrop impact and flow erosion by overland flow

**Called from:** [`surface`](#surface)

Source: `ero_ovrsed.f90`

<pre>
ero_ovrsed
│
│  !! Code developed by J. Jeong and N. Kannan, BRC.
│  ulu = hru(j)%luse%urb_lu
│
│  !! Fraction of sand
│  percent_clay = soil(j)%phys(1)%clay
│  percent_silt = soil(j)%phys(1)%silt
│  percent_sand = 100. - percent_clay - percent_silt
│
│  !! Soil detachability values adopted from EUROSEM User Guide (Table 1)
│
├── [if (percent_clay>=40.) .and. (percent_sand>=20.) ]
│   │  erod_k = 2.0
│   │  elseif ((percent_clay>=27.) .and. (percent_sand>=20.) .and.             
│   │  erod_k = 1.7
│   │  elseif ((percent_silt<=40.).and.(percent_sand<=20.)) then
│   │  erod_k = 2.0
│   │  elseif ((percent_silt>40.).and.(percent_clay>=40.)) then
│
│  !! canopy cover is assumed to be 100% if LAI>=1
│
├── [if pcom(j)%lai_sum >= 1.]
│
├── [if bsn_cc%gampt > 0]
│   
│   ├── [loop k = 1,]
│
▼
</pre>


---

## ero_cfactor

this subroutine predicts daily soil loss caused by water erosion

**Called from:** [`surface`](#surface), [`wetland_control`](#wetland_control)

Source: `ero_cfactor.f90`

<pre>
ero_cfactor
│
│  !! Intrinsic: Exp
│  bsn_cc%cfac = 1
│
│  !! HRU sediment calculations
│
├── [if bsn_cc%cfac == 0]
│   
│   │  !! old method using minimum c factor (average of each plant in community)
│   │  cover = pl_mass(j)%ab_gr_com%m + pl_mass(j)%rsd_tot%m
│   
│   ├── [if pcom(j)%npl > 0]
│      │  c = exp_w((-.2231 - cvm_com(j)) * Exp(-.00115 * cover) + cvm_com(j))
│      
│      ├── [if cover > 1.e-4]
│         │  c = exp_w(-.2231 * exp_w(-.00115 * cover))
│   
│   │  !! new method using residue and biomass cover - from APEX
│   │  rsd_sumfac = (pl_mass(j)%rsd_tot%m + 1.) / 1000.
│   │  rsd_covfact = exp_w(-bsn_prm%rsd_covco * rsd_sumfac)
│   │  ab_gr_t = pl_mass(j)%ab_gr_com%m / 1000.
│   │  grcov_frac = ab_gr_t / (ab_gr_t + exp_w(1.175 - 1.748 * ab_gr_t))
│   │  bio_covfact = 1. - grcov_frac * exp_w(-.328 * pcom(j)%cht_mx)
│   │  bio_covfact = Max(1.e-10, bio_covfact)
│   
│   │  !! erosion output variables
│
▼
</pre>


---

## ero_ysed

this subroutine predicts daily soil loss caused by water erosion

**Called from:** [`surface`](#surface)

Source: `ero_ysed.f90`

<pre>
ero_ysed
│
│  !! Intrinsic: Exp
│
│  !! initialize variables
│  cklsp(j) = usle_cfac(j) * hru(j)%lumv%usle_mult
│  rock = Exp(-.053 * soil(j)%phys(1)%rock)
│
│  !! compute sediment yield with musle - t
│  sedyld(j) = (10. * surfq(j) * qp_cms * hru(j)%area_ha) ** .56 * cklsp(j)
│
│  !! this is the form of MUSLE in APEX documentation - same results as swat equation above ! 
│
│  !! adjust sediment yield for protection of snow cover
│
├── [if hru(j)%sno_mm > 0.]
│   │  sedyld(j) = 0.
│   │  sedyld(j) = sedyld(j) / Exp(hru(j)%sno_mm * 3. / 25.4)
│
│  !! compute erosion with usle (written to output for comparison)
│  usle = 1.292 * usle_ei * cklsp(j) / 11.8
│
│  !! erosion output variables
│  ero_output(j)%ero_d%sedyld = sedyld(j)
│  ero_output(j)%ero_d%precip = w%precip
│  ero_output(j)%ero_d%surfq = surfq(j)
│  ero_output(j)%ero_d%peak = qp_cms
│
│  !! sum daily erosion output
│
▼
</pre>


---

## gwflow_wetland

this subroutine determines the volume of groundwater exchanged with wetlands

**Called from:** [`wetland_control`](#wetland_control)

Source: `gwflow_wetland.f90`

<pre>
gwflow_wetland
│
│  !! (exchange volumes are used in gwflow_simulate, in groundwater balance equations)
│
├── [if gw_wet_flag == 1]
│   │  ires = hru(hru_id)%dbs%surf_stor
│   
│   ├── [if hru_num_cells(hru_id) > 0]
│      
│      ├── [loop icell=1,hru_num_cells(hru_id)]
│         │  cell_id = hru_cells(hru_id,icell)
│         │  wt = gw_state(cell_id)%head
│         │  wet_depth = wet(hru_id)%flo / (wet_wat_d(hru_id)%area_ha * 10000.)
│         │  wet_stage = ob(sp_ob1%hru+hru_id-1)%elev + wet_depth
│         │  wet_k = hru(hru_id)%wet_hc * 24 / 1000.
│         │  wet_area = hru_cells_fract(hru_id,icell) * (wet_wat_d(hru_id)%area_ha * 
│         
│         ├── [if wt > wet_stage]
│         │  gw_inflow = wet_area * wet_k * ((wt-wet_stage)/wet_thick(ires))
│         
│         ├── [if gw_state(cell_id)%head > gw_state(cell_id)%bot]
│         │  gwvol_avail = ((gw_state(cell_id)%head - gw_state(cell_id)%botm) *      
│      
│      ├── [if gw_inflow > gwvol_avail]
│         │  gw_inflow = gwvol_avail
│      
│      ├── [if gw_heat_flag == 1]
│         
│         ├── [if heat_flux >= gwheat_state(cell_id)%stor]
│
▼
</pre>


---

## res_weir_release

**Called from:** [`wetland_control`](#wetland_control)

Source: `res_weir_release.f90`

<pre>
res_weir_release
│
│  !! suppress unused variable warning
│
│  !! store initial values
│  vol = wbody%flo
│  iweir = wet_ob(jres)%iweir
│
├── [if wet_hyd(ihyd)%name=='paddy']
│   │  wsa1 = hru(jres)%area_ha * 10000.
│   │  wsa1 = wbody_wb%area_ha * 10000.
│  hgt_above = max(0., dep - weir_hgt)
│
│  !! check if reservoir decision table has a weir discharge command
│
├── [loop tstep = 1,]
│   
│   │  !! calculate weir discharge from scheduled management
│   
│   ├── [if hgt_above > 0 .and. iweir > 0]
│      
│      ├── [if vol>evol_m3]
│         │  ht2%flo = ht2%flo + (wbody%flo - evol_m3)
│         │  ht2%flo = max(0.,ht2%flo)
│         │  vol = evol_m3
│         │  res_h = vol / wsa1
│         │  hgt_above = max(0.,res_h - weir_hgt)
│      
│      ├── [if nstep>1]
│         
│         ├── [if qout > vol_above]
│
▼
</pre>


---

## wet_salt

this subroutine computes the wetland salt ion mass balance

**Called from:** [`wetland_control`](#wetland_control)

Source: `wet_salt.f90`

<pre>
wet_salt
│
│  !! this subroutine computes the wetland salt ion mass balance
│
├── [loop isalt=1,cs_db%num_salts]
│   │  wetsalt_d(ihru)%salt(isalt)%inflow = 0.
│   │  wetsalt_d(ihru)%salt(isalt)%outflow = 0.
│   │  wetsalt_d(ihru)%salt(isalt)%seep = 0.
│   │  wetsalt_d(ihru)%salt(isalt)%mass = 0.
│   │  wetsalt_d(ihru)%salt(isalt)%conc = 0.
│
├── [loop isalt=1,cs_db%num_salts]
│   │  salt_mass_beg = wet_water(ihru)%salt(isalt)
│   
│   ├── [if wet(ihru)%flo > 0.]
│      │  salt_conc_beg = (salt_mass_beg * 1000.) / wet(ihru)%flo
│   │  mass_avail = salt_mass_beg
│   │  salt_inflow = obcs(icmd)%hin_sur(1)%salt(isalt)
│   │  mass_avail = mass_avail + salt_inflow
│   
│   ├── [if salt_outflow > mass_avail]
│   
│   ├── [if salt_seep > mass_avail]
│   
│   ├── [if wet(ihru)%flo > 0.]
│
▼
</pre>


---

## wet_cs

this subroutine computes the wetland constituent mass balance

**Called from:** [`wetland_control`](#wetland_control)

Source: `wet_cs.f90`

<pre>
wet_cs
│
│  !! this subroutine computes the wetland constituent mass balance
│
├── [loop ics=1,cs_db%num_cs]
│   │  wetcs_d(ihru)%cs(ics)%inflow = 0.
│   │  wetcs_d(ihru)%cs(ics)%outflow = 0.
│   │  wetcs_d(ihru)%cs(ics)%seep = 0.
│   │  wetcs_d(ihru)%cs(ics)%settle = 0.
│   │  wetcs_d(ihru)%cs(ics)%rctn = 0.
│   │  wetcs_d(ihru)%cs(ics)%irrig = 0.
│   │  wetcs_d(ihru)%cs(ics)%mass = 0.
│   │  wetcs_d(ihru)%cs(ics)%conc = 0.
│
├── [if wet(ihru)%flo > 1.]
│   
│   ├── [loop ics=1,cs_db%num_cs]
│      │  cs_mass_beg = wet_water(ihru)%cs(ics)
│      
│      ├── [if wet(ihru)%flo > 0.]
│         │  cs_conc_beg = (cs_mass_beg * 1000.) / wet(ihru)%flo
│      
│      ├── [if cs_outflow > mass_avail]
│      
│      ├── [if cs_seep > mass_avail]
│      
│      ├── [if ics == 1]
│
▼
</pre>


---

## gwflow_soil

this subroutine calculates the water exchange volume between the aquifer and the soil profile

**Called from:** [`swr_percmain`](#swr_percmain)

Source: `gwflow_soil.f90`

<pre>
gwflow_soil
│
│  !! (exchange volumes are used in gwflow_simulate, in groundwater balance equations)
│  hru_area_m2 = ob(hru_id)%area_ha * 10000.
│
├── [if gw_soil_flag == 1]
│   │  hru_soilz = soil(hru_id)%phys(soil(hru_id)%nly)%d / 1000.
│   
│   ├── [loop k=1,hru_num_cells(hru_id)]
│      │  cell_id = hru_cells(hru_id,k)
│      
│      ├── [if gw_state(cell_id)%stat == 1]
│         │  vadose_z = gw_state(cell_id)%elev - gw_state(cell_id)%head
│         
│         ├── [if vadose_z < hru_soilz]
│         │  poly_area = gw_state(cell_id)%area * cells_fract(hru_id,k)
│         │  hru_Q = (hru_soilz - vadose_z) * poly_area * gw_state(cell_id)%spyd
│         │  gw_hyd_ss(cell_id)%soil = gw_hyd_ss(cell_id)%soil + (hru_Q*(-1))
│         │  gw_hyd_ss_yr(cell_id)%soil = gw_hyd_ss_yr(cell_id)%soil + (hru_Q*(-1))
│         │  gw_hyd_ss_mo(cell_id)%soil = gw_hyd_ss_mo(cell_id)%soil + (hru_Q*(-1))
│         
│         ├── [if gw_heat_flag == 1]
│         │  heat_flux = gwheat_state(cell_id)%temp * gw_rho * gw_cp * hru_Q
│         
│         ├── [if heat_flux >= gwheat_state(cell_id)%stor]
│   
│   ├── [loop jj=1,soil(hru_id)%nly]
│      
│      ├── [if (soil(hru_id)%phys(jj)%d/1000.) > vadose_z]
│         
│         ├── [if jj == 1]
│         
│         ├── [if vadose_z > (soil(hru_id)%phys(jj-1)%d/1000.)]
│
▼
</pre>


---

## swr_percmacro

this surboutine computes percolation by crack flow

**Called from:** [`swr_percmain`](#swr_percmain)

Source: `swr_percmacro.f90`

<pre>
swr_percmacro
│
│  !! Intrinsic: Min
│  sepcrk = Min(voltot, inflpcp)
│  sepcrktot = sepcrk
│
├── [if sepcrk > 1.e-4]
│   
│   ├── [loop ly = soil(j)%nly,]
│      
│      ├── [if ly == soil(j)%nly]
│         │  crk = crklch*(soil(j)%ly(ly)%volcr/(soil(j)%phys(ly)%d -                
│         
│         ├── [if crk < sepcrk]
│         │  sepcrk = sepcrk - crk
│         │  sepbtm(j) = sepbtm(j) + crk
│         │  soil(j)%ly(ly)%prk = soil(j)%ly(ly)%prk + crk
│         │  sepbtm(j) = sepbtm(j) + sepcrk
│         │  soil(j)%ly(ly)%prk = soil(j)%ly(ly)%prk + sepcrk
│   │  xx = soil(j)%phys(ly)%fc - soil(j)%phys(ly)%st
│   
│   ├── [if xx > 0.]
│      │  crk = Min(sepcrk, xx)
│
│  !! crack flow, it is assumed to percolate out of bottom of profile
│
├── [if sepcrk > 1.e-4]
│
▼
</pre>


---
Done.

## swr_percmicro

this subroutine computes percolation and lateral subsurface flow

**Called from:** [`swr_percmain`](#swr_percmain)

Source: `swr_percmicro.f90`

<pre>
swr_percmicro
│
│  !! Intrinsic: Exp
│
│  !! there is no water flow
│
├── [if soil(j)%phys(ly1)%tmp <= 0.]
│
│  !! COMPUTE LATERAL FLOW USING HILLSLOPE STORAGE METHOD
│
├── [if soil(j)%phys(ly1)%ul - soil(j)%phys(ly1)%fc <=]
│   │  ho = 2. * sw_excess / ((soil(j)%phys(ly1)%ul - soil(j)%phys(ly1)%fc) / s
│
├── [if ly1 == 1]
│   │  latlyr = hru(j)%hyd%latq_co * ho * soil(j)%phys(ly1)%k * hru(j)%topo%slo
│  soil(j)%phys(ly1)%hk = (soil(j)%phys(ly1)%ul - soil(j)%phys(ly1)%fc) / s
│
│  !! septic changes 1/28/09
│
├── [if ly1 == i_sep(j)]
│   
│   ├── [if sep(isep)%opt  == 1]
│      │  sol_k_sep = soil(j)%phys(ly1)%k * (soil(j)%phys(ly1)%st - soil(j)%phys(l
│      │  sol_k_sep = Max(1.e-6, sol_k_sep)
│      │  sol_k_sep = Min(soil(j)%phys(ly1)%k, sol_k_sep)
│      │  soil(j)%phys(ly1)%hk = (soil(j)%phys(ly1)%hk - soil(j)%phys(ly1)%fc) / s
│      │  elseif (sep(isep)%opt  == 2) then
│      │  soil(j)%phys(ly1)%hk = 1.e10
│  soil(j)%phys(ly1)%hk = Max(2., soil(j)%phys(ly1)%hk)
│
│  !! compute seepage to the next layer
│
│  !! limit maximum seepage from biozone layer below potential perc amount
│
├── [if ly1 == i_sep(j).and.sep(isep)%opt ==1]
│
│  !! switched to linear relationship for dep_imp and seepage
│
├── [if ly1 == soil(j)%nly]
│
│  !! check mass balance
│
├── [if sepday + latlyr > sw_excess]
│
▼
</pre>


---

## swr_satexcess

this subroutine moves water to upper layers if saturated and can't perc

**Called from:** [`swr_percmain`](#swr_percmain)

Source: `swr_satexcess.f90`

<pre>
swr_satexcess
│
│  !! SWAT: percmacro, percmicro
│  real:: ul_excess = 0.
│  ires =  hru(j)%dbs%surf_stor
│  nn = soil(j)%nly
│
├── [loop ly = nn,]
│   
│   │  !! bottom layers - move water above upper limit to next layer up
│   
│   ├── [if ly > 1]
│      
│      ├── [if soil(j)%phys(ly)%st > soil(j)%phys(ly)%ul]
│         │  ul_excess = soil(j)%phys(ly)%st - soil(j)%phys(ly)%ul
│         │  soil(j)%phys(ly)%st = soil(j)%phys(ly)%ul
│         │  soil(j)%phys(ly-1)%st = soil(j)%phys(ly-1)%st + ul_excess
│      
│      │  !! if no depressional storage (wetland), add to surface runoff
│      │  ul_excess = soil(j)%phys(1)%st - soil(j)%phys(1)%ul
│      
│      ├── [if ul_excess > 0.]
│         │  soil(j)%phys(1)%st = soil(j)%phys(1)%ul
│         
│         │  !! check if entire profile is saturated - could get excess in first layer if irrigating on 
│         
│         ├── [loop ly1 = 2,]
│         │  soil(j)%phys(ly1)%st = soil(j)%phys(ly1)%st + ul_excess
│         
│         ├── [if soil(j)%phys(ly1)%st > soil(j)%phys(ly1)%ul]
│         │  ul_excess = soil(j)%phys(ly1)%st - soil(j)%phys(ly1)%ul
│
│  !! if still saturated
│
├── [if ul_excess > 0.]
│   
│   │  !! if no depressional storage, add to surface runoff
│   
│   ├── [if ires == 0]
│      
│      │  !! rtb gwflow: add ul_excess to runoff storage
│      
│      ├── [if gw_soil_flag.eq.1]
│      
│      │  !! rebalancing water and mass balance in the paddy water and soil profile
│      
│      ├── [if wet_ob(j)%weir_hgt < 0.001]
│
▼
</pre>


---

## swr_origtile

this subroutine computes tile drainage using basic tile equations

**Called from:** [`swr_percmain`](#swr_percmain)

Source: `swr_origtile.f90`

<pre>
swr_origtile
│
│  !! this subroutine computes tile drainage using basic tile equations
│
│  !! compute tile flow using the original tile equations
│
├── [if soil(j)%sw > soil(j)%sumfc]
│   │  sw_excess = (tile_above_btm / wt_shall) * (soil(j)%sw - soil(j)%sumfc)
│   
│   │  !! (wt_above_btm - tile_above_btm) / wt_above_btm * (sw - fc)
│   │  sw_excess = (wt_shall - tile_above_btm) / wt_shall * (soil(j)%sw - soil(
│   
│   ├── [if hru(j)%sdr%time < 1.]
│      │  qtile = sw_excess
│      │  qtile = sw_excess * (1. - Exp(-24. / hru(j)%sdr%time))
│   │  qtile = Min(qtile, hru(j)%sdr%drain_co)
│
▼
</pre>


---

## mgt_tillfactor

Armen 16 January 2008

**Called from:** [`mgt_biomix`](#mgt_biomix), [`mgt_newtillmix_cswat1`](#mgt_newtillmix_cswat1)

Source: `mgt_tillfactor.f90`

<pre>
mgt_tillfactor
│
├── [loop l = 1,]
│   
│   ├── [if soil(jj)%phys(l)%d <= dtil]
│      │  emix = emix
│      │  frac_mixed = 1.0
│      │  emix = emix * (dtil - soil(jj)%phys(l-1)%d) / soil(jj)%phys(l)%thick
│      │  frac_mixed = 1.0 - (dtil - soil(jj)%phys(l-1)%d) / soil(jj)%phys(l)%thic
│      │  frac_mixed = 0.0
│   
│   ├── [if bio_mix_event]
│      │  zz = zz_bmix_coef_a + (zz_bmix_coef_b)*exp(zz_bmix_coef_c*soil(jj)%phys(
│      │  zz = zz_emix_coef_a + (zz_emix_coef_b)*exp(zz_emix_coef_c*soil(jj)%phys(
│      │  yy = soil(jj)%ly(l)%tillagef_tillmix / zz
│   
│   ├── [if yy > 0.01]
│      │  xx1 = yy ** exp_w(-0.13 + 1.06 * yy)
│      │  xx2 = exp_w(0.64 + 0.64 * yy ** 10.)
│   
│   ├── [if soil(jj)%phys(l)%tmp <= 0. .and. bio_mix_event]
│      
│      ├── [if bio_mix_event]
│         
│         ├── [if soil(jj)%ly(l)%tillagef_tillmix > 0.]
│
▼
</pre>


---

## nut_np_flow

**Called from:** [`cbn_zhang2`](#cbn_zhang2)

Source: `nut_np_flow.f90`

<pre>
nut_np_flow
│  E_AtoB = E_A * (C_AtoB / C_A)
│
├── [if CO2fromA .LT. 0.]
│   │  EfromCO2 = E_A * (CO2fromA / C_A)
│
├── [if C_AtoB / E_AtoB .GT. CEtoB]
│   │  IMM_AtoB = C_AtoB / CEtoB - E_AtoB
│   │  MNR_AtoB = E_AtoB - C_AtoB / CEtoB
│   │  E_AtoB = E_AtoB - MNR_AtoB
│  MNR_AtoB = MNR_AtoB + EFCO2
│
▼
</pre>


---

## pl_dormant

this subroutine checks the dormant status of the different plant types

**Called from:** [`pl_grow`](#pl_grow)

Source: `pl_dormant.f90`

<pre>
pl_dormant
│
│  !! this subroutine checks the dormant status of the different plant types
│  idp = pcom(j)%plcur(ipl)%idplt
│  iob = hru(j)%obj_no
│  iwst = ob(iob)%wst
│  iwgn = wst(iwst)%wco%wgn
│
│  !! check for beginning of dormant season
│
├── [if pcom(j)%plcur(ipl)%idorm == "n" .and. wst(iwst]
│   
│   │  !! beginning of temperature based perennial dormant period - above ground senescence
│   
│   ├── [if pldb(idp)%typ == "perennial"]
│      │  pcom(j)%plcur(ipl)%idorm = "y"
│      
│      │  !! add dead stem mass to residue pool
│      │  stem_drop = rto * pl_mass(j)%stem(ipl)
│      
│      │  !! lower lai by same ratio
│      │  lai_init = pcom(j)%plg(ipl)%lai
│      │  pcom(j)%plg(ipl)%lai = rto * lai_init
│      
│      │  !! compute leaf biomass drop
│      │  leaf_drop%m = rto * pl_mass(j)%leaf(ipl)%m
│      │  leaf_drop%n = leaf_drop%m * pcom(j)%plm(ipl)%n_fr
│      
│      │  !! add all seed/fruit mass to residue pool
│      
│      ├── [if pl_mass(j)%tot_com%m < 0.]
│   
│   │  !! beginning of cool season annual dormant period
│   
│   ├── [if pldb(idp)%typ == "cold_annual"]
│      
│      ├── [if pcom(j)%plcur(ipl)%phuacc < 0.75]
│
▼
</pre>


---

## pl_leaf_gro

this subroutine adjusts plant biomass, leaf area index, and canopy height

**Called from:** [`pl_grow`](#pl_grow)

Source: `pl_leaf_gro.f90`

<pre>
pl_leaf_gro
│
│  !! SWAT: tstr, nup, npup, anfert
│  idp = pcom(j)%plcur(ipl)%idplt
│  f = pcom(j)%plcur(ipl)%phuacc / (pcom(j)%plcur(ipl)%phuacc +            
│  pcom(j)%plg(ipl)%laimxfr = amin1 (f, pcom(j)%plg(ipl)%laimxfr)
│  ff = f - pcom(j)%plg(ipl)%laimxfr
│  pcom(j)%plg(ipl)%laimxfr = f
│  exponent = plcp(idp)%leaf1 - plcp(idp)%leaf2 * pcom(j)%plcur(ipl)%phuacc
│  f_p = pcom(j)%plcur(ipl)%phuacc_p / (pcom(j)%plcur(ipl)%phuacc_p +      
│
│  !! calculate new leaf area index when phuacc < dlai
│
├── [if pcom(j)%plcur(ipl)%phuacc < pldb(idp)%dlai]
│   
│   ├── [if pldb(idp)%typ == "perennial"]
│      │  rto_lin = float(pcom(j)%plcur(ipl)%curyr_mat) / float(pldb(idp)%mat_yrs)
│      │  rto = alog10 (rto_lin)
│      │  lai_exp = rto * pldb(idp)%laixco_tree
│   
│   │  !! calculate new canopy height
│   
│   ├── [if pldb(idp)%typ == "perennial"]
│   
│   │  !! calculate fraction of above ground tree biomass that is leaf
│   
│   ├── [if pldb(idp)%typ == "perennial"]
│   
│   │  !! only apply water stress to lai
│   
│   │  !! adjust lai increment for plant competition
│   
│   ├── [loop jpl = 1,]
│   
│   ├── [if sumlaiht > 1.e-6]
│
▼
</pre>


---

## pl_leaf_senes

**Called from:** [`pl_grow`](#pl_grow)

Source: `pl_leaf_senes.f90`

<pre>
pl_leaf_senes
│  idp = pcom(j)%plcur(ipl)%idplt
│  leaf_drop%m = 0.
│
│  !! lai decline for annuals - if dlai < phuacc < 1
│
├── [if pldb(idp)%typ == "warm_annual" .or. pldb(idp)%]
│   
│   ├── [if pcom(j)%plcur(ipl)%phuacc > pldb(idp)%dlai .an]
│      │  lai_init = pcom(j)%plg(ipl)%lai
│      │  rto = (1. - pcom(j)%plcur(ipl)%phuacc) / (1. - pcom(j)%plg(ipl)%dphu)
│      │  pcom(j)%plg(ipl)%lai = pcom(j)%plg(ipl)%olai * rto ** pldb(idp)%dlai_rat
│      
│      │  !! compute leaf biomass drop
│
│  !! lai decline for temperature based perennials
│
├── [if pldb(idp)%typ == "perennial" .and. pldb(idp)%t]
│   
│   ├── [if pcom(j)%plcur(ipl)%phuacc > pldb(idp)%dlai .an]
│      │  iob = hru(j)%obj_no
│      │  iwst = ob(iob)%wst
│      │  lai_init = pcom(j)%plg(ipl)%lai
│      
│      │  !! use 15 day senescence period
│      │  pcom(j)%plg(ipl)%d_senes = pcom(j)%plg(ipl)%d_senes + 1.
│      │  rto = 1. - (pcom(j)%plg(ipl)%d_senes / 15.)
│      
│      │  !! compute leaf biomass drop
│      
│      ├── [if lai_init > 0.05]
│      
│      │  !! forest -- total tree n_conc = 1.75%; leaf  = 2.25%, falling leaf = 50%*2.25% = 1.12% -->
│
│  !! lai decline for moisture based perennials - use f(P/PET) to estimate drought stress
│
├── [if pldb(idp)%typ == "perennial" .and. pldb(idp)%t]
│   
│   │  !! linear lai decline based on soil moisture (max loss at p/pet=0.1, min loss at p/pet=0.5)
│   
│   ├── [if ppet < 0.5]
│
▼
</pre>


---

## pl_mortality

**Called from:** [`pl_grow`](#pl_grow)

Source: `pl_mortality.f90`

<pre>
pl_mortality
│  idp = pcom(j)%plcur(ipl)%idplt
│  bm_dieoff = (1. + pldb(idp)%bm_dieoff) * (pl_mass(j)%ab_gr(ipl)%m - (pld
│
├── [if bm_dieoff > 1.e-6 .and. pl_mass(j)%ab_gr(ipl)%]
│   
│   │  !! partition all plant components by above ground ratio
│   │  rto = bm_dieoff / pl_mass(j)%ab_gr(ipl)%m
│   │  rto = amin1 (1., rto)
│   
│   │  !! add dead material to residue
│   │  rto1 = 1. - rto
│   │  rto1 = max (0., rto1)
│   
│   │  !! add above ground biomass that has died off to surface residue pools
│   │  pl_mass(j)%rsd(ipl) = pl_mass(j)%rsd(ipl) + rto * pl_mass(j)%ab_gr(ipl)
│   │  pl_mass(j)%rsd_tot = pl_mass(j)%rsd_tot + rto * pl_mass(j)%ab_gr(ipl)
│   
│   │  !! add dead roots to soil residue pools
│   
│   ├── [loop ly = 1,]
│      │  soil1(j)%pl(ipl)%rsd(ly) = soil1(j)%pl(ipl)%rsd(ly) + rto * pcom(j)%plg(
│   │  pl_mass(j)%tot(ipl) = rto1 * pl_mass(j)%tot(ipl)
│
▼
</pre>


---

## hru_sweep

the subroutine performs the street sweeping operation

**Called from:** [`hru_urbanhr`](#hru_urbanhr)

Source: `hru_sweep.f90`

<pre>
hru_sweep
│
│  !! twash(:)     |days          |time that solids have built-up on streets
│
│  !! calculate amount of dirt on streets prior to sweeping
│  dirt = urbdb(ulu)%dirtmx * twash(j) / (urbdb(ulu)%thalf +twash(j))
│
│  !! calculate reduced amount of solid built up on impervious areas
│  dirt = dirt * (1. - fr_curb * sweepeff)
│
│  !! set time to correspond to lower amount of dirt
│  twash(j) = 0.
│  twash(j) = urbdb(ulu)%thalf * dirt / (urbdb(ulu)%dirtmx - dirt)
│
▼
</pre>


---

## gwflow_heat

this subroutine calculates heat advection and dispersion for groundwater

**Called from:** [`gwflow_lateral`](#gwflow_lateral)

Source: `gwflow_heat.f90`

<pre>
gwflow_heat
│
│  !! during the flow calculation in the same time step.
│
├── [loop i=1,ncell]
│   │  heat_cell(i) = gwheat_state(i)%stor
│
├── [loop i=1,ncell]
│   
│   ├── [if gw_state(i)%stat == 1]
│      │  heat_cell(i) = gwheat_state(i)%stor
│      
│      ├── [loop k=1,gw_state(i)%ncon]
│         │  cell_id = cell_con(i)%cell_id(k)
│         │  Q_cell = cell_con(i)%latl(k)
│         
│         ├── [if Q_cell > 0]
│         │  cell_adv = Q_cell * gw_rho * gw_cp * gwheat_state(cell_id)%temp
│         
│         ├── [if cell_adv > heat_cell(cell_id)]
│         │  cell_adv = heat_cell(cell_id)
│         │  heat_cell(cell_id) = heat_cell(cell_id) - cell_adv
│      │  cell_adv = Q_cell * gw_rho * gw_cp * gwheat_state(i)%temp
│      
│      ├── [if (cell_adv*-1) > heat_cell(i)]
│         │  cell_adv = heat_cell(i) * -1
│   │  heat_adv = heat_adv + cell_adv
│   
│   ├── [if gw_state(cell_id)%stat == 2]
│
├── [loop k=1,gw_state(i)%ncon]
│
▼
</pre>


---

## gwflow_write_cell_array

this subroutine opens all gwflow output files and writes headers

**Called from:** [`gwflow_output_aa`](#gwflow_output_aa)

Source: `gwflow_output.f90`

<pre>
gwflow_write_cell_array
│
│  !! fmt_code: 1=f12.3 (heads), 2=e12.3 (fluxes), 3=e12.6 (high precision)
│
├── [select case (fmt_code)]
│   │  write(iunit,101) (values(i), i=1,ncell_in)
│   │  write(iunit,102) (values(i), i=1,ncell_in)
│   │  write(iunit,103) (values(i), i=1,ncell_in)
│
▼
</pre>


---

## se_reactions_aquifer

**Called from:** [`cs_rctn_aqu`](#cs_rctn_aqu)

Source: `se_reactions_aquifer.f90`

<pre>
se_reactions_aquifer
│  cseo4 = conc_rg(1)
│  cseo3 = conc_rg(2)
│  cno3 = conc_rg(3)
│  o2 = cs_rct_aqu(iaq)%oxy_aqu
│  no3inhib = cs_rct_aqu(iaq)%se_ino3 / (cs_rct_aqu(iaq)%se_ino3 + cno3)
│  seo4red = cs_rct_aqu(iaq)%kseo4 * cseo4 * no3inhib
│  yseo4_o2 = 315.84 / 224.0
│  yseo4_no3 = 789.6 / 196.0
│
├── [loop kk=1,num_geol_shale]
│   │  ko2a = cs_rct_aqu(iaq)%ko2a(kk)
│   │  o2red = ko2a * o2 * cs_rct_aqu(iaq)%shale(kk)
│
▼
</pre>


---

## cond_real_c

**Called from:** [`res_rel_conds`](#res_rel_conds)

Source: `cond_real_c.f90`

<pre>
cond_real_c
│
├── [if op == "<"]
│   
│   ├── [if var_cur >= var_tbl]
│      │  hit = "n"
│
├── [if op == ">"]
│   
│   ├── [if var_cur <= var_tbl]
│      │  hit = "n"
│
├── [if op == "<="]
│   
│   ├── [if var_cur > var_tbl]
│      │  hit = "n"
│
├── [if op == ">="]
│   
│   ├── [if var_cur < var_tbl]
│      │  hit = "n"
│
├── [if op == "="]
│   
│   ├── [if var_cur /= var_tbl]
│      │  hit = "n"
│
├── [if op == "/="]
│   
│   ├── [if var_cur == var_tbl]
│      │  hit = "n"
│
▼
</pre>


---

## cond_integer_c

**Called from:** [`res_rel_conds`](#res_rel_conds)

Source: `cond_integer_c.f90`

<pre>
cond_integer_c
│
├── [if op == "<"]
│   
│   ├── [if var_cur >= var_tbl]
│      │  hit = "n"
│
├── [if op == ">"]
│   
│   ├── [if var_cur <= var_tbl]
│      │  hit = "n"
│
├── [if op == "<="]
│   
│   ├── [if var_cur > var_tbl]
│      │  hit = "n"
│
├── [if op == ">="]
│   
│   ├── [if var_cur < var_tbl]
│      │  hit = "n"
│
├── [if op == "="]
│   
│   ├── [if var_cur /= var_tbl]
│      │  hit = "n"
│
├── [if op == "/="]
│   
│   ├── [if var_cur == var_tbl]
│      │  hit = "n"
│
▼
</pre>


---

## gwflow_floodplain

this subroutine calculates the water exchange volume between the floodplain and the connected grid cells

**Called from:** [`sd_channel_sediment3`](#sd_channel_sediment3)

Source: `gwflow_floodplain.f90`

<pre>
gwflow_floodplain
│
│  !! (exchange volumes are used in gwflow_simulate, in groundwater balance equations)
│  chan_volume = ch_stor(chan_id)%flo
│
├── [if gw_fp_flag == 1]
│   
│   ├── [loop k=1,gw_fpln_info(chan_id)%ncon]
│      │  cell_id = gw_fpln_info(chan_id)%cells(k)
│      
│      ├── [if gw_state(cell_id)%stat.eq.1]
│         │  chan_depth = sd_ch(chan_id)%chd
│         │  bed_elev = gw_state(cell_id)%elev
│         │  bed_K = gw_fpln_info(chan_id)%hydc(k)
│         │  flow_area = gw_fpln_info(chan_id)%area(k)
│         │  chan_stage = bed_elev + chan_depth
│         
│         ├── [if gw_fpln_info(chan_id)%mtch(k) > 0]
│         │  chan_cell = gw_fpln_info(chan_id)%mtch(k)
│         │  riv_flow_area = sd_ch(chan_id)%chw * gw_chan_len(chan_cell)
│         │  flow_area = flow_area - riv_flow_area
│         
│         ├── [if flow_area.lt.0]
│   
│   ├── [if gw_state(cell_id)%head > chan_stage]
│   
│   ├── [if Q < 0]
│      
│      ├── [if (Q*-1) >= gw_state(cell_id)%stor]
│      
│      ├── [if Q > ch_stor(chan_id)%flo]
│
▼
</pre>


---

## plg_zero

**Called from:** [`mgt_killop`](#mgt_killop)

Source: `plant_module.f90`

<pre>
plg_zero
│  plg%cht = 0.
│  plg%lai = 0.
│  plg%plet = 0.
│  plg%plpet = 0.
│  plg%laimxfr = 0.
│  plg%laimxfr_p = 0.
│  plg%hi_adj = 0.
│  plg%hi_prev = 0.
│  plg%olai = 0.
│  plg%dphu = 0.
│
▼
</pre>


---

## mgt_plantop

this subroutine performs the plant operation

**Called from:** [`mgt_sched`](#mgt_sched)

Source: `mgt_plantop.f90`

<pre>
mgt_plantop
│
│  !! SWAT: curno
│  pcom(j)%plcur(ipl)%gro = "y"
│  pcom(j)%plcur(ipl)%idorm = "n"
│  pcom(j)%plcur(ipl)%phuacc = 0.
│  pl_mass(j)%tot(ipl) = orgz
│  pl_mass(j)%ab_gr(ipl) = orgz
│  pl_mass(j)%leaf(ipl) = orgz
│  pl_mass(j)%stem(ipl) = orgz
│  pl_mass(j)%seed(ipl) = orgz
│  pl_mass(j)%root(ipl) = orgz
│  pcom(j)%plg(ipl)%plet = 0.
│
│  !! compare maximum depth in soil to maximum rooting depth of plant
│
▼
</pre>


---

## salt_fert_wet

this subroutine adds salt fertilizer to a wetland

**Called from:** [`mgt_sched`](#mgt_sched)

Source: `salt_fert_wet.f90`

<pre>
salt_fert_wet
│
│  !! jj          |none          |HRU number
│
├── [if cs_db%num_salts > 0]
│   
│   ├── [if ifrt > 0]
│      │  wet_water(jj)%salt(1) = wet_water(jj)%salt(1) + (frt_kg * fert_salt(ifrt
│      │  wet_water(jj)%salt(2) = wet_water(jj)%salt(2) + (frt_kg * fert_salt(ifrt
│      │  wet_water(jj)%salt(3) = wet_water(jj)%salt(3) + (frt_kg * fert_salt(ifrt
│      │  wet_water(jj)%salt(4) = wet_water(jj)%salt(4) + (frt_kg * fert_salt(ifrt
│      │  wet_water(jj)%salt(5) = wet_water(jj)%salt(5) + (frt_kg * fert_salt(ifrt
│      │  wet_water(jj)%salt(6) = wet_water(jj)%salt(6) + (frt_kg * fert_salt(ifrt
│      │  wet_water(jj)%salt(7) = wet_water(jj)%salt(7) + (frt_kg * fert_salt(ifrt
│      │  wet_water(jj)%salt(8) = wet_water(jj)%salt(8) + (frt_kg * fert_salt(ifrt
│      │  wetsalt_d(jj)%salt(1)%fert = (frt_kg * fert_salt(ifrt)%so4 * hru(jj)%are
│      │  wetsalt_d(jj)%salt(2)%fert = (frt_kg * fert_salt(ifrt)%ca * hru(jj)%area
│
▼
</pre>


---

## cs_fert_wet

this subroutine adds constituent fertilizer to a wetland

**Called from:** [`mgt_sched`](#mgt_sched)

Source: `cs_fert_wet.f90`

<pre>
cs_fert_wet
│
│  !! jj          |none          |HRU number
│
├── [if cs_db%num_cs > 0]
│   
│   ├── [if ifrt > 0]
│      │  wet_water(jj)%cs(1) = wet_water(jj)%cs(1) + (frt_kg * fert_cs(ifrt)%seo4
│      │  wet_water(jj)%cs(2) = wet_water(jj)%cs(2) + (frt_kg * fert_cs(ifrt)%seo3
│      │  wet_water(jj)%cs(3) = wet_water(jj)%cs(3) + (frt_kg * fert_cs(ifrt)%boro
│      │  wetcs_d(jj)%cs(1)%fert = frt_kg * fert_cs(ifrt)%seo4 * hru(jj)%area_ha
│      │  wetcs_d(jj)%cs(2)%fert = frt_kg * fert_cs(ifrt)%seo3 * hru(jj)%area_ha
│      │  wetcs_d(jj)%cs(3)%fert = frt_kg * fert_cs(ifrt)%boron * hru(jj)%area_ha
│
▼
</pre>


---

## sq_daycn

Predicts daily runoff given daily precipitation and snow melt

**Called from:** [`sq_volq`](#sq_volq)

Source: `sq_daycn.f90`

<pre>
sq_daycn
│
│  !! |as directly connected impervious
│  r2 = 25400. / cnday(j) - 254.
│  bb = .2 * r2
│  pb = precip_eff - bb
│  surfq(j) = 0.
│
├── [if pb > 0.]
│   │  surfq(j) = pb * pb / (precip_eff + .8 * r2)
│
├── [if hru(j)%luse%urb_lu > 0]
│   │  r2 = 25400. / cnimp - 254.
│   │  bb = .2 * r2
│   │  pb = precip_eff - bb
│   
│   ├── [if pb > 0.]
│      │  surfqimp = pb * pb / (precip_eff + .8 * r2)
│   │  ulu = hru(j)%luse%urb_lu
│
▼
</pre>


---

## sq_greenampt

Predicts daily runoff given breakpoint precipitation and snow melt

**Called from:** [`sq_volq`](#sq_volq)

Source: `sq_greenampt.f90`

<pre>
sq_greenampt
│
│  !! Intrinsic: Sum, Exp, Real, Mod
│
│  !! array location #1 is for last time step of prev day
│  ulu = hru(j)%luse%urb_lu
│
│  !! reset values for day
│
├── [if swtrg(j) == 1]
│   │  swtrg(j) = 0
│   │  dthet = 0.001 * soil(j)%phys(1)%por * 0.95
│   │  rateinf(1) = rateinf_prev(j)
│   │  rateinf_prev(j) = 0.
│   
│   ├── [if soil(j)%sw >= soil(j)%sumfc]
│      │  soilw = 0.999 * soil(j)%sumfc
│      │  soilw = soil(j)%sw
│   │  dthet = (1. - soilw / soil(j)%sumfc) * soil(j)%phys(1)%por * 0.95
│   │  rateinf(1) = 2000.
│  psidt = dthet * wfsh(j)
│
├── [loop k = 1,]
│   
│   │  !! Update effective hydraulic conductivity
│   
│   │  !! calculate total amount of rainfall during day for time step
│   
│   │  !! and rainfall intensity for time step
│   
│   │  !! if rainfall intensity is less than infiltration rate everything will infiltrate
│   
│   ├── [if rateinf(k) >= rintns(k)]
│      
│      ├── [if excum(k-1) > 0.]
│
▼
</pre>


---

## swr_depstor

this subroutine computes maximum surface depressional storage depth based on

**Called from:** [`swr_drains`](#swr_drains)

Source: `swr_depstor.f90`

<pre>
swr_depstor
│
│  !! SWAT:eiusle
│  real:: df = 0.
│  real:: hru_slpp = 0.
│  real:: sol_orgm = 0.
│  real:: sol_rrr = 0.
│  real:: ei = 0.
│
│  !! Calculate current cumulative erosivity and rainfall
│  ei = usle_ei*18.7633
│
├── [if itill(j) ==1]
│   │  cumeira(j) = cumeira(j) + ei
│   │  cumei(j) = cumeira(j) - ei
│   │  cumrai(j) = cumrai(j) + precip_eff
│   │  cumrt(j) = cumrai(j) - precip_eff
│
│  !! Calculate the decay factor df based on %clay and %organic matter or %organic carbon
│
├── [if xx > 1.]
│
│  !! random and oriented roughness values
│
│  !! and current random and oriented roughness values determined above
│
▼
</pre>


---

## pl_nupd

This subroutine calculates plant nitrogen demand

**Called from:** [`pl_nut_demand`](#pl_nut_demand)

Source: `pl_nupd.f90`

<pre>
pl_nupd
│
│  !! name        |units         |definition
│  idp = pcom(j)%plcur(ipl)%idplt
│
│  !! set fraction to maturity for annuals and perennials
│
├── [if pldb(idp)%typ == "perennial"]
│   │  matur_frac = float(pcom(j)%plcur(ipl)%curyr_mat) / float(pldb(idp)%mat_y
│   │  matur_frac = pcom(j)%plcur(ipl)%phuacc
│  pcom(j)%plm(ipl)%n_fr = (pldb(idp)%pltnfr1 - pldb(idp)%pltnfr3) *       
│  un2(ipl) = pcom(j)%plm(ipl)%n_fr * pl_mass(j)%tot(ipl)%m
│  uno3d(ipl) = un2(ipl) - pl_mass(j)%tot(ipl)%n
│
▼
</pre>


---

## pl_pupd

this subroutine calculates plant phosphorus demand

**Called from:** [`pl_nut_demand`](#pl_nut_demand)

Source: `pl_pupd.f90`

<pre>
pl_pupd
│  idp = pcom(j)%plcur(ipl)%idplt
│
│  !! set fraction to maturity for annuals and perennials
│
├── [if pldb(idp)%typ == "perennial"]
│   │  matur_frac = float(pcom(j)%plcur(ipl)%curyr_mat) / float(pldb(idp)%mat_y
│   │  matur_frac = pcom(j)%plcur(ipl)%phuacc
│  pcom(j)%plm(ipl)%p_fr = (pldb(idp)%pltpfr1 - pldb(idp)%pltpfr3) *       
│  up2(ipl) = pcom(j)%plm(ipl)%p_fr * pl_mass(j)%ab_gr(ipl)%m
│  uapd(ipl) = up2(ipl) - pl_mass(j)%ab_gr(ipl)%p
│  uapd(ipl) = 1.5 * uapd(ipl)
│
▼
</pre>


---

## pl_tstr

computes temperature stress for crop growth - strstmp

**Called from:** [`pl_biomass_gro`](#pl_biomass_gro)

Source: `pl_tstr.f90`

<pre>
pl_tstr
│
│  !! Intrinsic: Exp
│  idp = pcom(j)%plcur(ipl)%idplt
│  tgx = w%tave - pldb(idp)%t_base
│
├── [if tgx <= 0.]
│   │  pcom(j)%plstr(ipl)%strst = 0.
│   
│   ├── [if w%tave > pldb(idp)%t_opt]
│      │  tgx = 2. * pldb(idp)%t_opt - pldb(idp)%t_base - w%tave
│   │  rto = ((pldb(idp)%t_opt - w%tave) / (tgx + 1.e-6)) ** 2
│   
│   ├── [if rto <= 200. .and. tgx > 0.]
│      │  pcom(j)%plstr(ipl)%strst = Exp(-0.1054 * rto)
│      │  pcom(j)%plstr(ipl)%strst = 0.
│   │  if(w%tmin <= wgn_pms(iwgen)%tmp_an - 15.) pcom(j)%plstr(ipl)%strst = 0.
│
│  !! APEX temperature stress equation
│  rto = (w%tave - pldb(idp)%t_base) / (pldb(idp)%t_opt - pldb(idp)%t_base)
│
├── [if rto > 0. .or. rto < 2.]
│   │  pcom(j)%plstr(ipl)%strst = Sin(1.5707 * rto)
│
▼
</pre>


---

## salt_uptake

this subroutine simulates salt ion uptake in the root zone

**Called from:** [`pl_biomass_gro`](#pl_biomass_gro)

Source: `salt_uptake.f90`

<pre>
salt_uptake
│
│  !! j           |none          |HRU number
│  idp = pcom(j)%plcur(1)%idplt
│
├── [if pcom(j)%plg(1)%root_dep > 0. .and. pl_mass(j)%]
│   │  rd = pcom(j)%plg(1)%root_dep
│   │  rm = pl_mass(j)%root(1)%m * ob(j)%area_ha
│   
│   ├── [loop jj=1,soil(j)%nly]
│      │  depth = depth + soil(j)%phys(jj)%thick
│      
│      ├── [if rd >= depth]
│         │  rm_layer = rm * (soil(j)%phys(jj)%thick / rd)
│         │  rm_layer = rm * ((soil(j)%phys(jj)%thick - (depth-rd)) / rd)
│      
│      ├── [if rm_layer > 0]
│         │  rm_fract(jj) = rm_layer / rm
│   
│   ├── [loop isalt=1,cs_db%num_salts]
│      
│      ├── [loop jj=1,soil(j)%nly]
│         │  uptake_mass = salt_uptake_kg(idp,isalt) * rm_fract(jj)
│         
│         ├── [if uptake_mass > cs_soil(j)%ly(jj)%salt(isalt)]
│         │  uptake_mass = cs_soil(j)%ly(jj)%salt(isalt)
│      │  hsaltb_d(j)%salt(isalt)%uptk = hsaltb_d(j)%salt(isalt)%uptk + uptake_mas
│
▼
</pre>


---

## cs_uptake

this subroutine simulates constituent uptake in the root zone

**Called from:** [`pl_biomass_gro`](#pl_biomass_gro)

Source: `cs_uptake.f90`

<pre>
cs_uptake
│
│  !! j           |none          |HRU number
│  idp = pcom(j)%plcur(1)%idplt
│
├── [if pcom(j)%plg(1)%root_dep > 0. .and. pl_mass(j)%]
│   │  rd = pcom(j)%plg(1)%root_dep
│   │  rm = pl_mass(j)%root(1)%m * ob(j)%area_ha
│   
│   ├── [loop jj=1,soil(j)%nly]
│      │  depth = depth + soil(j)%phys(jj)%thick
│      
│      ├── [if rd >= depth]
│         │  rm_layer = rm * (soil(j)%phys(jj)%thick / rd)
│         │  rm_layer = rm * ((soil(j)%phys(jj)%thick - (depth-rd)) / rd)
│      
│      ├── [if rm_layer > 0]
│         │  rm_fract(jj) = rm_layer / rm
│   
│   ├── [loop ics=1,cs_db%num_cs]
│      
│      ├── [loop jj=1,soil(j)%nly]
│         │  uptake_mass = cs_uptake_kg(idp,ics) * rm_fract(jj)
│         
│         ├── [if uptake_mass > cs_soil(j)%ly(jj)%cs(ics)]
│         │  uptake_mass = cs_soil(j)%ly(jj)%cs(ics)
│      │  hcsb_d(j)%cs(ics)%uptk = hcsb_d(j)%cs(ics)%uptk + uptake_mass
│
▼
</pre>


---

## pl_nfix

this subroutine estimates nitrogen fixation by legumes

**Called from:** [`pl_nup`](#pl_nup)

Source: `pl_nfix.f90`

<pre>
pl_nfix
│
│  !! Intrinsic: Max, Min
│  idp = pcom(j)%plcur(ipl)%idplt
│
│  !! compute the difference between supply and demand
│
├── [if uno3d(ipl) > nplnt(j)]
│   │  uno3l = uno3d(ipl) - nplnt(j)
│   
│   │  !! if supply is being met, fixation=0 and return
│
│  !! compute soil water factor
│  fxw = soil(j)%sw / (.85 * soil(j)%sumfc)
│
│  !! compute no3 factor
│
├── [loop l = 1,]
│   │  sumn = sumn + soil1(j)%mn(l)%no3
│
│  !! compute growth stage factor
│
├── [if pcom(j)%plcur(ipl)%phuacc > .15 .and. pcom(j)%]
│   │  fxg = 6.67 * pcom(j)%plcur(ipl)%phuacc - 1.
│  if(pcom(j)%plcur(ipl)%phuacc > .30 .and. pcom(j)%plcur(ipl)%phuacc <= .5
│
├── [if pcom(j)%plcur(ipl)%phuacc > .55 .and. pcom(j)%]
│   │  fxg = 3.75 - 5. * pcom(j)%plcur(ipl)%phuacc
│  fxr = Min(1., fxw, fxn) * fxg
│  fxr = Max(0., fxr)
│  fixn = Min(6., fxr * uno3l)
│
▼
</pre>


---

## nuts

this function calculates the plant stress factor caused by limited

**Called from:** [`pl_nup`](#pl_nup), [`pl_pup`](#pl_pup)

Source: `nuts.f90`

<pre>
nuts
│
│  !! Intrinsic: Exp
│  uu = 200. * (u1 / (u2 + .0001) - .5)
│
├── [if uu <= 0.]
│   
│   ├── [if uu < 99.]
│      │  uu = uu / (uu + Exp(3.535 - .02597 * uu))
│
▼
</pre>


---

## gwflow_minl

this subroutine calculates chemical reactions in gwflow cells.

**Called from:** [`gwflow_chem`](#gwflow_chem)

Source: `gwflow_chem.f90`

<pre>
gwflow_minl
│
│  !! salt chemistry modules are reconciled.
│
▼
</pre>


---

