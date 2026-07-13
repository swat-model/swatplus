    function fcgd(xx)

      use carbon_module, only : org_con
    
      implicit none
      
      real :: fcgd          !             |
      real :: tn            !             |
      real :: top           !             |
      real :: tx            !             |
      real :: qq = 0.       !             |
      real :: xx            !             |
      
      tn  = org_con%tn   ! orig hard coded value was -5. and default value if not supplied in carb_coefs.cbn
      top = org_con%top  ! orig hard coded value was 35. and default value if not supplied in carb_coefs.cbn
      tx  = org_con%tx   ! orig hard coded value was 50. and default value if not supplied in carb_coefs.cbn

      !! guard: when soil temp xx is below tn or above tx,
      !! the (xx-tn)**qq term has a negative base with fractional exponent and produces NaN.
      !! Biological activity is zero outside [tn, tx] anyway, so return 0 early.
      if (xx <= tn .or. xx >= tx) then
        fcgd = 0.
        return
      end if
      qq = (tn - top)/(top - tx)
      fcgd = ((xx-tn)**qq)*(tx-xx)/(((top-tn)**qq)*(tx-top))
      if (fcgd < 0.) fcgd = 0.
      
    end function fcgd