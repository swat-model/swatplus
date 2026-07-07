      subroutine nut_solp
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the amount of phosphorus lost from the soil
!!    profile in runoff and the movement of soluble phosphorus from the first
!!    to the second layer via percolation

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru          |none         |HRU number
!!    surfq(:)      |mm H2O       |surface runoff generated on day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min, Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module
      use organic_mineral_mass_module
      use gwflow_module, only : gwflow_flag, gw_transfer_flag, gw_transport_flag, hru_ptran, gwflow_percp
      use hru_module, only : hru, surqsolp, surfq, i_sep, ihru, qtile, gwtranp 
      use soil_module
      use output_landscape_module
      
      implicit none 

      integer :: j           !none          |HRU number
      integer :: jj          !none          |counter
      real :: xx             !none          |variable to hold intermediate calculation
                             !              |result
      real :: vap            !kg P/ha       |exponential coefficient for P leached and tile flow
      real :: plch           !kg P/ha       |amount of P leached from soil layer
      
      integer :: ly          !none          |counter 

      j = ihru
      
      !rtb gwflow: add P mass transferred to soil profile from the aquifer
      if(gw_transfer_flag.eq.1 .and. gw_transport_flag.eq.1) then
        do jj = 1,soil(j)%nly
          soil1(j)%mp(jj)%lab = soil1(j)%mp(jj)%lab + hru_ptran(j,jj) !kg/ha
          gwtranp(j) = gwtranp(j) + hru_ptran(j,jj) !HRU total
        enddo
      endif
      
      hls_d(j)%surqsolp = 0.
      hls_d(j)%lchlabp = 0.
      hls_d(j)%tilelabp = 0.
      
      !! compute soluble P lost in surface runoff
      xx = soil(j)%phys(1)%bd * soil(j)%phys(1)%d * bsn_prm%phoskd
      surqsolp(j) = soil1(j)%mp(1)%lab  * surfq(j) / (xx + 1.)   !dont merge
      !!units ==> surqsolp = [kg/ha * mm] / [t/m^3 * mm * m^3/t] = kg/ha
      surqsolp(j) = Min(surqsolp(j), soil1(j)%mp(1)%lab)
      surqsolp(j) = Max(surqsolp(j), 0.)
      hls_d(j)%surqsolp = surqsolp(j)
      soil1(j)%mp(1)%lab = soil1(j)%mp(1)%lab - surqsolp(j)

      !! compute soluble P leaching
      do ly = 1, soil(j)%nly
        vap = 0.
	   if (ly /= i_sep(j)) then
         vap = -soil(j)%ly(ly)%prk / (.01 * soil(j)%phys(ly)%st + .1 * bsn_prm%pperco *  soil(j)%phys(ly)%bd)
         plch = .001 * soil1(j)%mp(ly)%lab * (1. - Exp(vap))
         plch = Min(plch, soil1(j)%mp(ly)%lab)
	     soil1(j)%mp(ly)%lab = soil1(j)%mp(ly)%lab - plch
         if (ly == soil(j)%nly) then
           !! leach p from bottom layer
           hls_d(j)%lchlabp = plch
         else
           !! perc p to next layer
           soil1(j)%mp(ly+1)%lab = soil1(j)%mp(ly+1)%lab + plch
         endif
         !! tile p
         if (ly == hru(j)%lumv%ldrain) then
           vap = -qtile / (.01 * soil(j)%phys(ly)%st + .1 * bsn_prm%pperco *  soil(j)%phys(ly)%bd)
           plch = .001 * soil1(j)%mp(ly)%lab * (1. - Exp(vap))
           plch = Min(plch, soil1(j)%mp(ly)%lab)
           soil1(j)%mp(ly)%lab = soil1(j)%mp(ly)%lab - plch
           hls_d(j)%tilelabp = plch
         endif
	   endif
   !rtb gwflow: store phosphorus leaching concentration for gwflow module
   if(gwflow_flag .and. gw_transport_flag) then
     gwflow_percp(j) = hls_d(j)%lchlabp  
   endif
      end do
      
      return
      end subroutine nut_solp