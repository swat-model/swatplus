       subroutine mgt_tillfactor(jj,bmix,emix,dtil)
	!!!!!!!!!!!!!!!!!!!!!!!
	! Armen 16 January 2008
	! This procedure increases tillage factor (tillagef(l,jj) per layer for each operation
	! The tillage factor settling will depend of soil moisture (tentatively) and must be called every day
	! For simplicity the settling is calculated now at the soil carbon subroutine because soil water content is available

	! The tillage factor depends on the cumulative soil disturbance rating = csdr
	! For simplicity, csdr is a function of emix
	! First step is to calculate "current" csdr by inverting tillage factor function
	! The effect of texture on tillage factor (ZZ) is removed first (and recovered at the end of the procedure)
	! YY = tillagef(l,jj) / ZZ
	! Since the tillage factor function is non linear, iterations are needed 
	! XX = 0.5 is the initial value that works OK for the range of values observed
	! If a layer is only partially tilled then emix is corrected accordingly

	use soil_module
    
    implicit none
    
	integer, intent (in) :: jj        !none           |HRU number
    real, intent (in) :: bmix         !none           |biological mixing efficiency: this 
                                      !               |number is zero for tillage operations
    integer :: l                      !none           |counter 
    integer ::m1                      !none           |array location (see definition of ndays)
    integer :: m2                     !               |
    real :: emix                      !none           |mixing efficiency
    real :: dtil                      !mm             |depth of mixing
    real :: XX                        !varies         |variable to hold calculation results
    integer :: j                      !none           |counter
    real :: zz                        !               |
    real :: yy                        !               |
    real :: xx1                       !               | 
    real :: xx2                       !               | 
    real :: csdr                      !               | 
      
	emix = emix - bmix ! this is to avoid affecting tillage factor with biological mixing
	
	if (emix > 0.) then

	  do l = 1, soil(j)%nly
			
	    if (soil(jj)%phys(l)%d <= dtil) then
		  emix = emix
          else if (soil(jj)%phys(l)%d > dtil .and. soil(jj)%phys(l-1)%d < dtil) then 
		   emix = emix * (dtil - soil(jj)%phys(l-1)%d) / soil(jj)%phys(l)%thick
	    else
		  emix = 0.
	    end if
			
	    ! to save computation time if emix = 0 here then the other layers can be avoided
	    ! tillage always proceeds from top to bottom
	    if (emix == 0.) exit

	    xx = 0.
	    zz = 3. + (8. - 3.)*exp(-5.5*soil(jj)%phys(1)%clay/100.)
	    yy = soil(jj)%ly(l)%tillagef / zz
	    m1 = 1
	    m2 = 2

	    ! empirical solution for x when y is known and y=x/(x+exp(m1-m2*x)) 
	    if (yy > 0.01) then
		 xx1 = yy ** exp(-0.13 + 1.06 * yy)
		 xx2 = exp(0.64 + 0.64 * yy ** 100.)
		 xx = xx1 * xx2
	    end if

	    csdr = xx + emix
	    soil(jj)%ly(l)%tillagef = zz * (csdr / (csdr + exp(m1 - m2*csdr)))

	  end do		
		
	end if
		
	return
	end subroutine mgt_tillfactor