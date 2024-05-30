      subroutine ch_temp1
      
      use basin_module
      use input_file_module
      use maximum_data_module
      use channel_data_module
      use sd_channel_module
      use hydrograph_module
      use climate_module

      implicit none
      
      integer :: iob                !none       |channel object number
      !integer :: iwst               !none       |weather station number
      real :: tdx                   !           |
      real :: t_md
      real :: ke_beta
      real ::f_wind
      real :: k_e
      real :: ssff
      real :: h_sr
      real :: e_s
      real :: e_a
      real :: cloud
      real :: e_atm
      real :: h_atm
      real :: numerator
      real :: t_equil
      real :: k_factor
      real :: t_heat_exch
      
      !! Stream Temperature from heat exchange
      
      !! set weather station
      iob = sp_ob1%chandeg + ich - 1
      iwst = ob(iob)%wst
      w = wst(iwst)%weat

      tdx = w_temp%hex_coef1 * w%tave + w_temp%hex_coef2
      tdx = amin1 (tdx, w%tave)                         !! It should be contratined to tdx < t_air
      t_md = (ht1%temp + tdx) / 2.
      ke_beta = 0.35 + 0.015 * t_md + 0.0012 * (t_md**2)
      f_wind = 9.2 + 0.46 * (w%windsp**2)              !! These coefficients might vary. See Figure 2.4.1 in Edinger et al. 1974
      k_e = 4.48 + 0.05 * ht1%temp + (ke_beta + 0.47) * f_wind

      !! Solar radiation (Short wave radiation). The shade-factor is included in the Solar radiation calculation
      ssff = 0.5                                        !! Add code for shade factor ***jga***
      h_sr = 0.97 * w%solrad * (1. - ssff)              !! Get the Solar radiation (Short wave radiation)
    
      !! Atmospheric radiation (Long wave radiation)
      e_s = 6.1275 * Exp(17.62 * w%tave / (237.3 + w%tave))                 !! Get the Saturated vapor pressure 
      e_a = e_s * w%rhum                                                    !! Get the vapor pressure
      cloud = 1.                                                            !! need to set ***jga***
      e_atm = 0.74 + 0.0065 * e_a * (1. + 0.17 * cloud**2)                  !! Get the emissivity of the atmosphere 
      h_atm = 0.96 * e_atm * 5.67e-8 * (w%tave + 273.15)**4                 !! Atmospheric radiation (Long wave radiation)

      !! Equilibrium temperature 
      numerator = h_atm - 305.5 - 4.48 * tdx
      t_equil = tdx + h_sr / k_e + numerator / k_e                          !!E: Reduced T-equilibrium approach

      !! The temperature gain/loss to/from the stream due to heat transfer
      !! "rttime" is the travel time given by the SWAT model
      !! "ch_l2" is the stream length given by SWAT that can be used as a surrogate for travel time
      k_factor = 1000. * 4186. * rcurv%dep        !!E: Density*Spec_heat_water*water_depth / time_convers
      t_heat_exch = k_e * (t_equil - ht1%temp) / k_factor * rcurv%ttime

      !! Final stream temperature
      ob(iob)%hd(1)%temp = ht1%temp + t_heat_exch
    
      return    
      end subroutine ch_temp1