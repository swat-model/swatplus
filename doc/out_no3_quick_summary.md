# Quick Answer: out_no3 in SWAT+

## What is it?
`out_no3` represents **outgoing nitrate nitrogen (NO3-N) from stream channels** in the SWAT+ watershed model.

## Units
- **Mass**: tons (t) or kilograms (kg) 
- **Time**: daily flux

## Where it comes from
1. **Upstream channels** - nitrate flowing from upstream reaches
2. **Surface runoff** - agricultural and urban runoff containing nitrate
3. **Groundwater** - baseflow with dissolved nitrate 
4. **Point sources** - wastewater, industrial discharges

## What processes affect it
- **Denitrification**: Main removal process - converts NO3 to N2 gas
- **Routing**: Transport through channel network
- **Mixing**: Confluence of tributaries

## Why it might be negative
Negative values are **physically unrealistic** but can occur due to:

1. **Numerical errors** - floating point precision issues
2. **Extreme denitrification** - very high removal rates
3. **Mass balance errors** - model calculation inconsistencies  
4. **Poor calibration** - unrealistic parameter values

## What to do about negative values
- Check denitrification parameters
- Examine extreme flow conditions
- Reduce simulation time step
- Validate input data
- Re-calibrate nitrogen parameters

## Key insight
`out_no3` is essentially the nitrate that "exits" a channel reach after accounting for all inputs, removals (especially denitrification), and transport processes. Negative values indicate model problems that need investigation.