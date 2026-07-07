    function fcgd(xx)
    
      implicit none
      
      real :: fcgd          !             |
      real :: tn            !             |
      real :: top           !             |
      real :: tx            !             |
      real :: qq            !             |
      real :: xx            !             |
      
      tn = -5.
	  top = 35.
      tx = 50.
      qq = (tn - top)/(top - tx)
	  fcgd = ((xx-tn)**qq)*(tx-xx)/(((top-tn)**qq)*(tx-top))
      if (fcgd < 0.) fcgd = 0.
    end function