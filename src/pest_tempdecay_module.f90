      module pest_tempdecay_module

      implicit none

      contains

      real function decay_temp_adjust(hl_25, temp, q10)
      !! Calculates temperature-adjusted decay based on Q10 model 
      !! Need to calculate the temperature-adjusted half-life first 
      !! then re-calculate the decay-rate
      !! hl_25: pesticide specific half life (soil, foliar or aquatic) at 25 degC
      !! q10 FOCUS default: 2.2
      !! q10 EFSA update 2007: 2.58
      !! q10 for plants: 1.22
      !! https://efsa.onlinelibrary.wiley.com/doi/epdf/10.2903/j.efsa.2008.622
      !! https://archive.epa.gov/epa/sites/production/files/2015-11/documents/attachment_3_-_przm-gw_input_parameter_guidance.pdf

      real, intent(in) :: hl_25
      real, intent(in) :: temp
      real, optional, intent(in) :: q10
      real :: q10_local
      real :: hl_t

      q10_local = 2.58
      if (present(q10)) q10_local = q10

      if (hl_25 <= 0.) then
        decay_temp_adjust = 0.
        return
      end if

      hl_t = hl_25 * q10_local ** ((25.0 - temp) / 10.0)
      if (hl_t <= 0.) then
        decay_temp_adjust = 0.
      else
        decay_temp_adjust = exp(-0.693 / hl_t)
      end if

      end function decay_temp_adjust

      end module pest_tempdecay_module
