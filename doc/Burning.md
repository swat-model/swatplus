# Simulating Fire / Burning in SWAT+

SWAT+ provides two distinct approaches to simulating a burn event. Understanding the
differences helps you choose the right method for your scenario.

---

## Method 1 — `scen.dtl` with `lu_change` to a landuse that contains a burn in its management schedule

### How it works

1. A `lu_change` action is placed in the **scenario decision table** (`scen.dtl`).
2. When the conditions in that table are met, the HRU is switched to a new **Land Use
   and Management (LUM)** entry defined in `landuse.lum`.
3. Switching the land use triggers a **complete reinitialization** of the HRU
   (`actions.f90`, `lu_change` case):
   - `hru_lum_init` is called → replaces plant cover, CN table pointer, conservation
     practice, tile drain, septic, filter strip, grass waterway, and BMP-user settings
     with those of the new LUM.
   - `plant_init(1, j)` is called → **deallocates and fully resets the plant
     community** (species, growth stage, root depth, heat unit accumulation).
   - `cn2_init` is called → **resets curve number (CN2)** from the new LUM's CN table.
   - USLE composite value (K × P × LS) is recalculated from the new LUM's parameters.
   - The **management schedule resets to operation 1** of the new LUM's schedule.
4. If that new LUM's management schedule (`management.sch`) contains a `burn` operation,
   the burn fires at its scheduled date/heat unit threshold in the new schedule.

### What a burn operation does (`pl_burnop`)

When the `burn` operation eventually executes it:

- Increases CN2 by the `chg_cn2` value defined in `fire.ops` (capped at 98).
- Reduces **aboveground biomass** (leaves, stems, seeds) by fraction `frac_burn`.
- Reduces **surface residue** by fraction `frac_burn`.
- Reduces **surface layer (layer 1) humus** (active + stable) by fraction `frac_burn`.
- Tracks emitted carbon as CO₂ in the residue and plant carbon output streams.

### When to use this method

Use `lu_change` + burn-schedule when the fire event **fundamentally changes the
land cover** of the HRU — for example, converting a mature forest to a post-fire
shrubland or grassland. Because the HRU is reassigned to a different LUM, all
downstream model behavior (hydrology, USLE erosion, nutrient cycling, and management
rotations) reflects the new land cover type after the burn.

---

## Method 2 — Direct `burn` action in a management schedule or decision table

### How it works

A `burn` operation is placed directly in one of:

| File | Mechanism |
|---|---|
| `management.sch` | Scheduled by calendar date or heat-unit threshold for specific HRUs |
| `lum.dtl` | Fired by conditions evaluated each time step against the HRU's current land use |
| `scen.dtl` | Fired by basin-wide scenario conditions (year, area, etc.) |

When the condition/schedule triggers, `pl_burnop` is called directly without any
land-use reinitialization.

### What changes — and what does not

| State variable | Changed? |
|---|---|
| Aboveground biomass (leaves, stems, seeds) | **Yes** — reduced by `frac_burn` |
| Surface residue | **Yes** — reduced by `frac_burn` |
| Surface humus (layer 1) | **Yes** — reduced by `frac_burn` |
| Carbon emissions (CO₂) | **Yes** — tracked in output |
| CN2 | **Yes** — increased by `chg_cn2` (capped at 98) |
| Land use classification | No |
| Plant species / community | No — plants continue growing with reduced biomass |
| Plant growth stage / heat unit accumulation | No |
| Management schedule / current operation | No |
| CN table, USLE P-factor, LS-factor | No |

### When to use this method

Use a direct `burn` when fire removes fuel but the **land cover type does not
fundamentally change** — for example, periodic prescribed burns in a grassland or
savanna, or wildfire in a shrubland that will regenerate the same plant community.
The HRU keeps its land use identity and management rotation; only biomass, residue,
and surface organic matter are consumed.

---

## Summary comparison

| Feature | `lu_change` → burn schedule | Direct `burn` action |
|---|---|---|
| Changes land use type | **Yes** | No |
| Resets plant community | **Yes** | No |
| Resets CN2 from LUM CN table | **Yes** | No — only adds `chg_cn2` delta |
| Resets management schedule | **Yes** — restarts at op 1 | No |
| Resets USLE parameters | **Yes** | No |
| Removes biomass / residue | When burn op in new schedule runs | **Yes — immediately** |
| Output written | `lu_change` output file + `mgt` output | `mgt` output |
| Best for | Land-cover transition events | Periodic / prescribed burns |

---

## Files involved

| File | Purpose |
|---|---|
| `scen.dtl` | Scenario decision table — conditions and actions applied basin-wide |
| `lum.dtl` | Land-use decision table — conditions evaluated per HRU each time step |
| `management.sch` | Management schedule — date/HU-triggered operations per LUM |
| `landuse.lum` | Land use and management entries; points to plant cover and schedule |
| `fire.ops` | Fire operation database — defines `chg_cn2` and `frac_burn` for each fire type |
