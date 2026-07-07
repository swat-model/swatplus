      subroutine sim_initday

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine initialized arrays at the beginning of the day

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    frad(:,:)   |none          |fraction of solar radiation occuring during 
!!                               |hour in day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
     
      use hru_module, only : cbodu,chl_a,clayld,cnday,doxq,etday,grayld,hhsedy,         &
         hhsurfq,hru,lagyld,latno3,latq,nplnt,                                          &
         par,percn,pplnt,qdr,rcn,sagyld,sanyld,sedminpa,                                &
         sedminps,sedorgn,sedorgp,sedyld,sepbtm,silyld,sol_sumno3,sol_sumsolp, surfq,   &
         surqno3,surqsolp,tileno3,ubnrunoff,ubntss,                                     &
         gwtranq, satexq, gwtrann, gwtranp, satexn !rtb gwflow
      use soil_module
            
      use organic_mineral_mass_module
      use carbon_module
      use hydrograph_module
      use reservoir_module
      use maximum_data_module

      !!initialize variables at beginning of day
      ! initialising wetland by Ann 

      
      implicit none
      
      real :: drift             !kg               |amount of pesticide drifting onto main 
                                !                 |channel in subbasin
      real :: hrupstd           !varies           |HRU daily pesticide output array
      integer :: j              !none             |HRU number 
      integer :: ly             !none             |counter 
      integer :: ires           !none             |counter

      !!initialize variables at beginning of day
      cbodu = 0.
      chl_a = 0.
      cnday = 0.
      doxq = 0.
      drift = 0.
      hrupstd = 0.
      latno3 = 0.
      latq = 0.
      nplnt = 0.
      percn = 0.
      pplnt = 0.
      qdr = 0.
      sedminpa = 0.
      sedminps = 0.
      sedorgn = 0.
      sedorgp = 0.

      sedyld = 0.
      sanyld = 0.
      silyld = 0.
      clayld = 0.
      sagyld = 0.
      lagyld = 0.
      grayld = 0.

      sepbtm = 0.
      surfq = 0.
      surqno3 = 0.
      surqsolp = 0.
      tileno3 = 0.    !CB 8/24/09
      
      gwtranq = 0. !rtb gwflow
      gwtrann = 0. !rtb gwflow
      gwtranp = 0. !rtb gwflow
      satexq = 0.

      satexn = 0.

!----------------------------------------------------        
! added by J.Jeong for urban modeling 4/29/2008
      ubnrunoff = 0.
      ubntss = 0.
      latq = 0.
      hhsurfq = 0.
!-----------------------------------------------------        

      ! zero carbon losses for the day
      do j = 1, sp_ob%hru
        cbn_loss(j) = cbn_lossz
      end do
	
        !! added for Srini in output.mgt nitrogen and phosphorus nutrients per JGA by gsm 9/8/2011
                  
          sol_sumno3 = 0.
          sol_sumsolp = 0.
          do j = 1, sp_ob%hru
            do ly = 1, soil(j)%nly
              sol_sumno3(j) = sol_sumno3(j) + soil1(j)%mn(ly)%no3 +          &
                soil1(j)%mn(ly)%nh4
              sol_sumsolp(j) = sol_sumsolp(j) +  soil1(j)%mp(ly)%lab
            enddo
          enddo

      return
      end