    function fcgd(xx)

      use carbon_module, only : org_con
    
      implicit none
      
      real :: fcgd          !             |
      real :: tn            !             |
      real :: top           !             |
      real :: tx            !             |
      real :: qq = 0.       !             |
      real :: xx            !             |
      
      tn  = org_con%tn
      top = org_con%top
      tx  = org_con%tx
      qq = (tn - top)/(top - tx)
      fcgd = ((xx-tn)**qq)*(tx-xx)/(((top-tn)**qq)*(tx-top))
      if (fcgd < 0.) fcgd = 0.
      
    end function fcgd