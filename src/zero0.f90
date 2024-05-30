      subroutine zero0

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine initializes the values for some of the arrays 

      use hru_module, only : brt,bss,canstor,cn2,cumei, urb_abstinit, rateinf_prev,   &
       cumeira,cumrai,cumrt,dormhr,filterw,grz_days,                                  &
       igrz, isep_ly,iseptic,itb,                                                     &
       latno3,orgn_con,orgp_con,phubase,ranrns_hru,                                   &
       sed_con,sepcrk,sol_sumsolp,soln_con,solp_con,sstmaxd,stmaxd,wt_shall,yr_skip

      implicit none

      integer :: iop                      !none               !counter
      real :: pltnfr                      !kg N/kg biomass    |nitrogen uptake parameter normal fraction
                                          !                   |of N in crop biomass at emergence 
      real :: pltpfr                      !kg P/kg biomass    |phosphorus uptake parameter normal
                                          !                   |fraction of P in crop biomass at emergence
      real :: ranrns                      !mm                 |random roughness of a given tillage operation
      
      !! Green and Ampt storages for urban runoff
      urb_abstinit = 0.
      rateinf_prev = 0.
      
!    Drainmod tile equations  01/2006 
      cumeira = 0.
      cumei = 0.
      cumrai = 0.
      cumrt = 0.
      ranrns_hru = 20.
!    Drainmod tile equations  01/2006
      brt = 0.
      bss = 0.
      canstor = 0.

!!    Initialization by balaji
      cn2 = 0.
      dormhr = 0.
      filterw = 0.

!    Drainmod tile equations  01/2006
      igrz = 0
      iop = 0

      iseptic = 0
      isep_ly = 0
      itb = 0
      grz_days = 0

  !! change per JGA irrigation 4/2/2009
      latno3 = 0.
      orgn_con = 0.
      orgp_con = 0.
	  phubase = 0.
      pltnfr = 0.
      pltpfr = 0.
!! drainmod tile equations   06/2006
      ranrns = 0.
!! drainmod tile equations   06/2006
      sstmaxd = 0.
!    Drainmod tile equations  01/2006
      sed_con = 0.
      sepcrk = 0.
!    Drainmod tile equations  01/2006 
      stmaxd = 0.
!    Drainmod tile equations  01/2006 

!!   added for Srini in output.mgt nitrogen and phosphorus nutrients per JGA by gsm 9/8/2011
      sol_sumsolp = 0.
      soln_con = 0.
      solp_con = 0.
      wt_shall = 0.
      yr_skip = 0
    
      return
      end