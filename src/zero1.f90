      subroutine zero1

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine initializes the values for some of the arrays 

      use hru_module, only : bio_bod,biom,bz_perc,fcoli,i_sep,  &
       plqm,qstemm,rbiom,sep_tsincefail,sweepeff,swtrg,t_ov,    &
       tconc,usle_cfac,usle_eifac,wfsh
      
      implicit none

      real :: sep_opt                   !                |
      real :: filt_w                    !                |
      real :: grwat_veg                 !                |
      real :: plq_rt                    !                |
      real :: pr_w                      !none            |probability of wet day after dry day in month 
      real :: rchrg                     !mm              |recharge
      real :: sedst                     !metric tons     |amount of sediment stored in reach
                                        !                |reentrained in channel sediment routing
      real :: sol_wp                    !                |
      real :: thalf                     !days            |time for the amount of solids on
                                        !                |impervious areas to build up to 1/2
                                        !                |the maximum level
      real :: tnconc                    !mg N/kg sed     |concentration of total nitrogen in
                                        !                |suspended solid load from impervious
                                        !                |areas
      real :: tno3conc                  !mg NO3-N/kg sed |concentration of NO3-N in suspended
                                        !                |solid load from impervious areas
      real :: tpconc                    !mg P/kg sed     |concentration of total phosphorus in
                                        !                |suspended solid load from impervious
                                        !                |areas 
      real :: urbcoef                   !1/mm            |wash-off coefficient for removal of
                                        !                |constituents from an impervious surface
      real :: urbcn2                    ! none           |moisture condiction II curve number for imp areas
      real :: vp                        !                |  

!!  septic changes 6/07/10  jaehak
      bio_bod = 0.
      fcoli = 0.  
      biom = 0.
      rbiom = 0.
      bz_perc = 0.
      plqm = 0.
      qstemm = 0
      i_sep = 0
      sep_opt= 1
      sep_tsincefail = 0

      filt_w = 0.
      grwat_veg = 0.
!!  septic changes 1/29/09 
      plqm = 0.
      plq_rt = 0.
!!  septic changes 1/29/09
      pr_w = 0.
      rchrg = 0.
      sedst = 0.
      sol_wp = 0.
      sweepeff = 0.
      swtrg = 0
      t_ov = 0.
      tconc = 0.
      thalf = 0.
      tnconc = 0.
      tno3conc = 0.
      tpconc = 0.
      urbcoef = 0.
      urbcn2 = 0.
      usle_cfac = 0.
      usle_eifac = 0.
!!  septic changes 1/29/09
      vp = 0. 
      wfsh = 0.

      return
      end