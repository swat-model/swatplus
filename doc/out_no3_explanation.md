# Understanding `out_no3` in SWAT+ Model

## What is `out_no3`?

`out_no3` is a variable in the SWAT+ (Soil and Water Assessment Tool Plus) model that represents **outgoing nitrate nitrogen (NO3-N) from channel reaches**. It is a key component of the channel nitrogen budget and tracks the mass of nitrate leaving a channel segment with the water flow.

### Units and Measurement
- **Primary units**: tons (t) or kilograms (kg) depending on context
- **Chemical form**: NO3-N (nitrate nitrogen)
- **Temporal scale**: Daily mass flux

## Where does `out_no3` come from?

### Source Code Location
The variable is defined and used in several key files:
- **Definition**: `src/sd_channel_module.f90` (line 72)
- **Assignment**: `src/sd_channel_control3.f90` (line 456)
- **Processing**: `src/sd_channel_nutrients.f90`

### Assignment in the Code
```fortran
ch_sed_bud(ich)%out_no3 = ht2%no3
```

Where `ht2%no3` represents the nitrate mass in the hydrograph component after all channel processes have been applied.

### Sources of Nitrate in Channels

1. **Upstream Inflow** (`in_no3`)
   - Nitrate from upstream channel reaches
   - Tributary contributions

2. **Surface Runoff**
   - Nitrate transported from HRUs (Hydrologic Response Units)
   - Agricultural runoff containing fertilizer nitrogen
   - Urban runoff

3. **Groundwater Discharge**
   - Baseflow containing dissolved nitrate
   - Subsurface flow contributions

4. **Point Sources**
   - Wastewater treatment plants
   - Industrial discharges
   - Direct applications

## Channel Nitrogen Processes

### 1. Denitrification (Primary Removal Process)
Denitrification is the main process that can reduce nitrate in channels:

```fortran
! From sd_channel_nutrients.f90
denit = 10. ** (rte_nut(inut)%no3_slp * alog(no3_conc) + rte_nut(inut)%no3_int)
ht2%no3 = ht2%no3 - denit
ht2%no3 = max(0., ht2%no3)
```

**Factors affecting denitrification:**
- NO3 concentration in water
- Water temperature
- Channel substrate characteristics
- Flow conditions (overbank vs. under-bank)
- Dissolved oxygen levels
- Organic matter availability

### 2. Channel Types
The model handles different channel configurations:
- **Two-stage ditches** (overbank flow conditions)
- **Single-stage channels** (under-bank flow)
- Different denitrification coefficients for each type

### 3. Mass Balance Calculation
The model converts between concentration and mass:
```fortran
! Convert concentration to mass
ht2%no3 = ht3%no3 * ht1%flo / 1000.
! Convert mass to concentration
no3_conc = 1000. * ht2%no3 / ht2%flo
```

## Why `out_no3` Might Be Negative

While the model includes safeguards (`max(0., ht2%no3)`), negative values can still occur due to:

### 1. Numerical Precision Issues
- Floating-point arithmetic errors
- Accumulation of small rounding errors
- Precision loss in mass-concentration conversions

### 2. Mass Balance Errors
- Inconsistencies in routing algorithms
- Temporal discretization errors
- Spatial interpolation errors between channel segments

### 3. Extreme Process Rates
- Very high denitrification rates
- Rapid changes in flow conditions
- Extreme temperature conditions affecting reaction rates

### 4. Model Limitations
- Simplified representation of complex biogeochemical processes
- Linear interpolation in non-linear systems
- Time step limitations in capturing rapid changes

### 5. Input Data Issues
- Inconsistent boundary conditions
- Unrealistic parameter values
- Poor calibration of denitrification parameters

## Implications of Negative Values

### Physical Interpretation
Negative `out_no3` values are **physically unrealistic** because:
- Nitrate concentration cannot be below zero
- Mass cannot be negative in real systems
- Indicates model calculation errors

### Model Diagnostics
When negative values occur, consider:
1. **Parameter Review**: Check denitrification coefficients
2. **Time Step**: Reduce simulation time step
3. **Flow Conditions**: Examine extreme flow events
4. **Calibration**: Re-calibrate nitrogen parameters
5. **Input Data**: Verify boundary conditions and point sources

## Recommendations

### For Model Users
1. **Monitor** `out_no3` values during simulation
2. **Check** for negative values in output files
3. **Calibrate** denitrification parameters carefully
4. **Validate** against observed data when available

### For Model Debugging
1. **Examine** mass balance components
2. **Review** nitrogen parameter values
3. **Check** for extreme flow or concentration conditions
4. **Consider** reducing time step for stability

## Related Variables

- `in_no3`: Incoming nitrate to channel
- `no3_in`/`no3_out`: General hydrograph nitrate components  
- Channel nitrogen budget components:
  - `fp_no3`: Floodplain nitrate losses
  - `bank_no3`: Bank nitrate contributions
  - `bed_no3`: Bed nitrate exchanges

## References

- SWAT+ model source code: `src/sd_channel_nutrients.f90`
- SWAT+ documentation: https://swatplus.gitbook.io/docs
- Channel module: `src/channel_module.f90`
- Hydrograph module: `src/hydrograph_module.f90`