      subroutine nut_denit(k,j,cdg,wdn,void)
!!    this subroutine computes denitrification 

      use basin_module
      use organic_mineral_mass_module
      use soil_module
      
      implicit none 

	  integer :: k          !none          |counter
      integer :: j          !none          |HRU number
 	  real :: cdg           !none          |soil temperature factor
      real ::wdn            !kg N/ha       |amount of nitrogen lost from nitrate pool in
                            !              |layer due to denitrification 
      real :: void          !              |
      real :: vof           !              |

      wdn = 0.
	  vof = 1. / (1. + (void/0.04)**5)
	  wdn =  soil1(j)%mn(k)%no3 * (1. - Exp(-bsn_prm%cdn * cdg * vof *          &
              soil1(j)%tot(k)%c))
	  soil1(j)%mn(k)%no3 = max(0.0001,soil1(j)%mn(k)%no3 - wdn)

	  return
	  end subroutine nut_denit