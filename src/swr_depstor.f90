	subroutine swr_depstor

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes maximum surface depressional storage depth based on   
!!    random and oriented roughness and slope steepness

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    iop(:,:,:)  |julian date   |date of tillage operation
!!    mgt_op      |none          |operation code number
!!    ranrns_hru(:)|mm           |random roughness for a given HRU
!!	sol_ori(:)	|mm			   |oriented roughness (ridges) at time of a given tillage operation
!!    usle_ei     |100(ft-tn in)/(acre-hr)|USLE rainfall erosion index
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!	stmaxd(:)	|mm			   |maximum surface depressional storage for day in a given HRU 							   
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!	cumei(:)	|Mj*mm/ha*hr   |cumulative USLE rainfall erosion index since last 
!!							   |tillage operation
!!	cumrt(:)	|mm H2O		   |cumulative rainfall since last tillage operation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic:exp 
!!    SWAT:eiusle

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use hru_module, only : hru, cumeira, cumei, cumrt, cumrai, ranrns_hru, stmaxd, itill, ihru,  &
         itill, precip_eff, usle_ei
      use soil_module
      use organic_mineral_mass_module
      
      implicit none

      integer ::j               !none          |HRU number
	  real:: df                 !none		   |oriented and random roughness decay factor - based
                                !              |on cumulative EI and cumulative precip_eff
      real:: hru_slpp           !%	           |average percent slope steepness
      real:: sol_orgm           !%      	   |percent organic matter content in soil material
      !real:: sol_orr            !cm			   |oriented roughness (ridges) after a rain event 
      real:: sol_rrr            !cm			   |random roughness after a rain event
      real:: ei                 !Mj*mm/ha*hr   |USLE rainfall erosion index
      real :: xx                !              |
      
      j = ihru

!! Calculate current cummulative erosivity and rainfall
	ei = usle_ei*18.7633
	if (itill(j) ==1)then
	  cumeira(j) = cumeira(j) + ei
	  cumei(j) = cumeira(j) - ei
	  cumrai(j) = cumrai(j) + precip_eff
	  cumrt(j) = cumrai(j) - precip_eff
      end if
!! Calculate the decay factor df based on %clay and %organic matter or %organic carbon
	sol_orgm = soil1(j)%tot(1)%c / 0.58
	xx = (0.943 - 0.07 * soil(j)%phys(1)%clay + 0.0011 *                 &
         soil(j)%phys(1)%clay**2 - 0.67 * sol_orgm + 0.12 * sol_orgm**2)
      if (xx > 1.) then
        df = 1.
      else
        df = exp (xx)
      end if
      

!! Determine the current random and oriented roughness using cumei and cumrt and initial
!! random and oriented roughness values
      
	sol_rrr = 0.1 * ranrns_hru(j)                                      &                                     
       	* exp(df*(-0.0009*cumei(j)-0.0007 * cumrt(j)))
	
!	sol_orr = 0.1*sol_ori(j)*                                                
!     &	exp(df*(-0.025*(cumei(j)**0.31)-0.0085*(cumrt(j)**0.567)))

!! Compute the current maximum depressional storage using percent slope steepness 
!! and current random and oriented roughness values determined above
	hru_slpp = hru(j)%topo%slope*100
!	if(irk=0) then !irk=0 for random rough, and irk=1, for oriented roughness
	stmaxd(j)= 0.112*sol_rrr+0.031*sol_rrr**2-0.012*sol_rrr*hru_slpp
!	else 
!	stmaxd(j)= 0.112*sol_orr+0.031*sol_orr**2-0.012*sol_orr*hru_slpp 
!	endif
   
   	return
	end subroutine swr_depstor