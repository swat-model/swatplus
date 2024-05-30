      subroutine nut_pminrl2
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes p flux between the labile, active mineral
!!    and stable mineral p pools.  
!!    this is the alternate phosphorus model described in Vadas and White (2010)
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module
      use organic_mineral_mass_module
      use hru_module, only : ihru
      use output_landscape_module, only : hnb_d
      use soil_module
      use time_module
      
      implicit none      

      integer :: j                      !none          |HRU number
      integer :: l                      !none          |counter 
      real :: rto                       !              |
      real :: rmp1                      !kg P/ha       |amount of phosphorus moving from the solution
                                        !              |mineral to the active mineral pool in the soil layer
      real :: roc                       !kg P/ha       |amount of phosphorus moving from the active
                                        !              |mineral to the stable mineral pool in the soil layer
      real :: wetness                   !              |
      real :: base                      !              |
      real :: vara                      !		       |Intermediate Variable
      real :: varb                      !    	       |Intermediate Variable
      real :: varc                      !    	       |Intermediate Variable
      real :: as_p_coeff                !              | 
      real :: solp                      !mg/kg	       |Solution pool phosphorous content
      real :: actpp                     !mg/kg	       |Active pool phosphorous content
      real :: stap                      !mg/kg	       |Stable pool phosphorous content
      real :: arate                     !			   |Intermediate Variable      
      real :: ssp                       !              |     
      real :: psp                       !              | 

      j = ihru
        
      hnb_d(j)%lab_min_p = 0.
      hnb_d(j)%act_sta_p = 0.
      do l = 1, soil(j)%nly !! loop through soil layers in this HRU
	!! make sure that no zero or negative pool values come in
	if (soil1(j)%mp(l)%lab <= 1.e-6) soil1(j)%mp(l)%lab = 1.e-6
	if (soil1(j)%mp(l)%act <= 1.e-6) soil1(j)%mp(l)%act = 1.e-6
      if (soil1(j)%mp(l)%sta <= 1.e-6) soil1(j)%mp(l)%sta = 1.e-6
      
!! Convert kg/ha to ppm so that it is more meaningful to compare between soil layers
	  solp = soil1(j)%mp(l)%lab / soil(j)%phys(l)%conv_wt 
	  actpp = soil1(j)%mp(l)%act / soil(j)%phys(l)%conv_wt 
	  stap = soil1(j)%mp(l)%sta / soil(j)%phys(l)%conv_wt 

!! ***************Soluble - Active Transformations***************	

	  !! Dynamic PSP Ratio
	    !!PSP = -0.045*log (% clay) + 0.001*(Solution P, mg kg-1) - 0.035*(% Organic C) + 0.43
	    if (soil(j)%phys(l)%clay > 0.) then
	      psp = -0.045 * log(soil(j)%phys(l)%clay)+ (0.001 * solp) 
	      psp = psp - (0.035  * soil1(j)%cbn(l)) + 0.43
	    else
	      psp = 0.4
	    end if    		
		!! Limit PSP range
		if (psp < .1) psp = 0.1 ! limits on PSP
	    if (psp > 0.7) psp = 0.7  

        !! Calculate smoothed PSP average 
	  if (soil(j)%ly(l)%psp_store > 0.) then
	    psp = (soil(j)%ly(l)%psp_store * 29. + psp * 1.) / 30.
	  end if
        !! Store PSP for tomrrows smoothing calculation
	  soil(j)%ly(l)%psp_store = psp

!!***************Dynamic Active/Soluble Transformation Coeff******************

	  !! on day 1 just set to a value of zero
      if ((time%day == 1) .and. (time%yrs == 1)) then 
        soil(j)%ly(l)%a_days = 0 !! days since P Application 
        soil(j)%ly(l)%b_days = 0 !! days since P deficit
      end if	   

      !! Calculate P balance
      rto = psp / (1. - psp)
      rmp1 = soil1(j)%mp(l)%lab - soil1(j)%mp(l)%act * rto !! P imbalance

	  !! Move P between the soluble and active pools based on Vadas et al., 2006
		if (rmp1 >= 0.) then !! Net movement from soluble to active	
		  rmp1 = Max(rmp1, (-1 * soil1(j)%mp(l)%lab))
		  !! Calculate Dynamic Coefficant		
          vara = 0.918 * (exp(-4.603 * psp))          
		  varb = (-0.238 * ALOG(vara)) - 1.126
		  if (soil(j)%ly(l)%a_days >0) then 
		    arate = vara * (soil(j)%ly(l)%a_days ** varb)
		  else
		    arate = vara * (1) ** varb
		  end if
		  !! limit rate coeff from 0.05 to .5 helps on day 1 when a_days is zero
		  if (arate > 0.5) arate  = 0.5
		  if (arate < 0.1) arate  = 0.1
		  rmp1 = arate * rmp1		
	      soil(j)%ly(l)%a_days = soil(j)%ly(l)%a_days  + 1 !! add a day to the imbalance counter
	      soil(j)%ly(l)%b_days = 0
        end if

		if (rmp1 < 0.) then !! Net movement from Active to Soluble 		
		  rmp1 = Min(rmp1, soil1(j)%mp(l)%act)	
		  !! Calculate Dynamic Coefficant
		  base = (-1.08 * psp) + 0.79
		  varc = base * (exp (-0.29))
	       !! limit varc from 0.1 to 1
		  if (varc > 1.0) varc  = 1.0
		  if (varc < 0.1) varc  = 0.1
          rmp1 = rmp1 * varc
		  soil(j)%ly(l)%a_days = 0
		  soil(j)%ly(l)%b_days = soil(j)%ly(l)%b_days  + 1 !! add a day to the imbalance counter
        End if

!!*************** Active - Stable Transformations ******************
        !! Estimate active stable transformation rate coeff
	  !! original value was .0006
		!! based on linear regression rate coeff = 0.005 @ 0% CaCo3 0.05 @ 20% CaCo3
		  as_p_coeff = 0.0023 * soil(j)%ly(l)%cal + 0.005 
          if (as_p_coeff > 0.05) as_p_coeff = 0.05
         if (as_p_coeff < 0.002) as_p_coeff = 0.002
        !! Estimate active/stable pool ratio
        !! Generated from sharpley 2003
      	ssp = 25.044 * (actpp + (actpp * rto))** (-0.3833)
	  ! limit ssp to range in measured data
	  if (ssp > 10.) ssp = 10.
	  if (ssp < 0.7) ssp = 0.7

	  ! Smooth ssp, no rapid changes
		 if (soil(j)%ly(l)%ssp_store > 0.) then
		    ssp = (ssp + soil(j)%ly(l)%ssp_store * 99.)/100.
		 end if

         roc = ssp * (soil1(j)%mp(l)%act + soil1(j)%mp(l)%act * rto) 
		 roc = roc - soil1(j)%mp(l)%sta
		 roc = as_p_coeff * roc 
		 !! Store todays ssp for tomarrows calculation
		 soil(j)%ly(l)%ssp_store = ssp

!! **************** Account for Soil Water content, do not allow movement in dry soil************
         wetness = (soil(j)%phys(l)%st/soil(j)%phys(l)%fc) !! range from 0-1 1 = field cap
		 if (wetness >1.)  wetness = 1.
		 if (wetness <0.25)  wetness = 0.25 
		 rmp1 = rmp1 * wetness
		 roc  = roc  * wetness
	  
!! If total P is greater than 10,000 mg/kg do not allow transformations at all
	   If ((solp + actpp + stap) < 10000.) then 
	      !! Allow P Transformations
		  soil1(j)%mp(l)%sta = soil1(j)%mp(l)%sta + roc
		  if (soil1(j)%mp(l)%sta < 0.) soil1(j)%mp(l)%sta = 0.
		  soil1(j)%mp(l)%act = soil1(j)%mp(l)%act - roc + rmp1
		  if (soil1(j)%mp(l)%act < 0.) soil1(j)%mp(l)%act = 0.
		  soil1(j)%mp(l)%lab = soil1(j)%mp(l)%lab - rmp1
		  if (soil1(j)%mp(l)%lab < 0.) soil1(j)%mp(l)%lab = 0.
	   end if

!! Add water soluble P pool assume 1:5 ratio based on sharpley 2005 et al
	soil(j)%ly(l)%watp = soil1(j)%mp(l)%lab / 5.
    
    hnb_d(j)%lab_min_p = hnb_d(j)%lab_min_p + rmp1
    hnb_d(j)%act_sta_p = hnb_d(j)%act_sta_p + roc

      end do
      return
      end subroutine nut_pminrl2