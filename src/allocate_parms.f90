      subroutine allocate_parms
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine allocates array sizes

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    mhyd        |none          |max number of hydrographs
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

      use hru_module      
      use time_module
      use hydrograph_module
      use constituent_mass_module

      implicit none
      
      integer :: mhru = 0
      integer :: mch = 0
      integer :: mpc = 0
      
!! initialize variables    
      mhyd = 1  !!added for jaehak vars
      mhru = sp_ob%hru
      mch = sp_ob%chan

!!    drains
      allocate (wnan(10), source = 0.)
      allocate (ranrns_hru(mhru), source = 0.)
      
      !dimension plant arrays used each day and not saved
       mpc = 20
       allocate (uno3d(mpc), source = 0.)
       allocate (uapd(mpc), source = 0.)
       allocate (un2(mpc), source = 0.)
       allocate (up2(mpc), source = 0.)
       allocate (translt(mpc), source = 0.)
       allocate (par(mpc), source = 0.)
       allocate (htfac(mpc), source = 0.)
       allocate (epmax(mpc), source = 0.)
       epmax = 0.

!!    arrays for plant communities
      allocate (cvm_com(mhru), source = 0.)
      allocate (rsdco_plcom(mhru), source = 0.)
      allocate (percn(mhru), source = 0.)

!! septic changes added 1/28/09 gsm
      allocate (i_sep(mhru), source = 0)
      allocate (sep_tsincefail(mhru), source = 0)
      allocate (qstemm(mhru), source = 0.)
      allocate (bio_bod(mhru), source = 0.)
      allocate (biom(mhru), source = 0.)
      allocate (rbiom(mhru), source = 0.)
      allocate (fcoli(mhru), source = 0.)
      allocate (bz_perc(mhru), source = 0.)
      allocate (plqm(mhru), source = 0.)
      allocate (itb(mhru), source = 0)
      
      allocate (hhqday(mhru,time%step), source = 0.)
      
 !!  added per JGA for Srini by gsm 9/8/2011
 !! arrays for management output (output.mgt)  
      allocate (sol_sumno3(mhru), source = 0.)
      allocate (sol_sumsolp(mhru), source = 0.)

      allocate (iseptic(mhru), source = 0)

!!    arrays which contain data related to years of rotation,
!!    grazings per year, and HRUs
      allocate (grz_days(mhru), source = 0)

!!    arrays which contain data related to HRUs
      allocate (brt(mhru), source = 0.)
      allocate (canstor(mhru), source = 0.)
      allocate (cbodu(mhru), source = 0.)
      allocate (chl_a(mhru), source = 0.)
      allocate (cklsp(mhru), source = 0.)
      allocate (cn2(mhru), source = 0.)
      allocate (cnday(mhru), source = 0.)
!    Drainmod tile equations  01/2006 
   allocate (cumei(mhru), source = 0.)
   allocate (cumeira(mhru), source = 0.)
   allocate (cumrt(mhru), source = 0.)
   allocate (cumrai(mhru), source = 0.)
!    Drainmod tile equations  01/2006
      allocate (dormhr(mhru), source = 0.)
      allocate (doxq(mhru), source = 0.)
      allocate (filterw(mhru), source = 0.)
      allocate (igrz(mhru), source = 0)
      allocate (yr_skip(mhru), source = 0)
      allocate (isweep(mhru), source = 0)
      allocate (phusw(mhru), source = 0.)
      allocate (latno3(mhru), source = 0.)
      allocate (latq(mhru), source = 0.)
      allocate (ndeat(mhru), source = 0)
      allocate (nplnt(mhru), source = 0.)
      allocate (orgn_con(mhru), source = 0.)
      allocate (orgp_con(mhru), source = 0.)
      allocate (ovrlnd(mhru), source = 0.)
      allocate (phubase(mhru), source = 0.)

      allocate (pplnt(mhru), source = 0.)
      allocate (qdr(mhru), source = 0.)

      allocate (gwsoilq(mhru), source = 0.)  !rtb gwflow


      allocate (satexq(mhru), source = 0.)  !rtb gwflow


      allocate (gwsoiln(mhru), source = 0.)  !rtb gwflow


      allocate (gwsoilp(mhru), source = 0.)  !rtb gwflow


      allocate (satexn(mhru), source = 0.)  !rtb gwflow


      
!    Drainmod tile equations  01/2006 
   allocate (sstmaxd(mhru), source = 0.)
!    Drainmod tile equations  01/2006 
      allocate (sedminpa(mhru), source = 0.)
      allocate (sedminps(mhru), source = 0.)
      allocate (sedorgn(mhru), source = 0.)
      allocate (sedorgp(mhru), source = 0.)
      allocate (sedyld(mhru), source = 0.)

      allocate (sanyld(mhru), source = 0.)
      allocate (silyld(mhru), source = 0.)
      allocate (clayld(mhru), source = 0.)
      allocate (sagyld(mhru), source = 0.)
      allocate (lagyld(mhru), source = 0.)
      allocate (grayld(mhru), source = 0.)
      allocate (sed_con(mhru), source = 0.)
      allocate (sepbtm(mhru), source = 0.)
      allocate (smx(mhru), source = 0.)
      allocate (soln_con(mhru), source = 0.)
      allocate (solp_con(mhru), source = 0.)
!!    Drainmod tile equations  01/2006 
   allocate (stmaxd(mhru), source = 0.)
      allocate (itill(mhru), source = 0)
      allocate (surfq(mhru), source = 0.)
      allocate (surqno3(mhru), source = 0.)
      allocate (surqsolp(mhru), source = 0.)
      allocate (swtrg(mhru), source = 0)
      allocate (rateinf_prev(mhru), source = 0.)
      allocate (urb_abstinit(mhru), source = 0.)
      allocate (t_ov(mhru), source = 0.)
      allocate (tconc(mhru), source = 0.)
      allocate (tc_gwat(mhru), source = 0.)
      allocate (tileno3(mhru), source = 0.)
      allocate (twash(mhru), source = 0.)
      allocate (usle_cfac(mhru), source = 0.)
      allocate (usle_eifac(mhru), source = 0.)
      allocate (wfsh(mhru), source = 0.)
      !rtb salt
      allocate (surqsalt(mhru,8), source = 0.)
      allocate (latqsalt(mhru,8), source = 0.)
      allocate (tilesalt(mhru,8), source = 0.)
      allocate (percsalt(mhru,8), source = 0.)
      allocate (gwupsalt(mhru,8), source = 0.)
      allocate (urbqsalt(mhru,8), source = 0.)
      allocate (wetqsalt(mhru,8), source = 0.)
      allocate (wtspsalt(mhru,8), source = 0.)
      !rtb cs
      allocate (surqcs(mhru,10), source = 0.)
      allocate (latqcs(mhru,10), source = 0.)
      allocate (tilecs(mhru,10), source = 0.)
      allocate (perccs(mhru,10), source = 0.)
      allocate (gwupcs(mhru,10), source = 0.)
      allocate (sedmcs(mhru,10), source = 0.)
      allocate (urbqcs(mhru,10), source = 0.)
      allocate (wetqcs(mhru,10), source = 0.)
      allocate (wtspcs(mhru,10), source = 0.)
      allocate (irswcs(mhru,10), source = 0.)
      allocate (irgwcs(mhru,10), source = 0.)
      
      allocate (bss(40,mhru), source = 0.)  !rtb salt/cs (changed to 40)


      allocate (bss_ex(10,mhru), source = 0.)  !rtb gwflow


      allocate (wrt(2,mhru), source = 0.)
      allocate (surf_bs(55,mhru), source = 0.)  !rtb salt/cs (changed to 55)



!! sj aug 09 end
   allocate (hhsurf_bs(2,mhru,time%step), source = 0.)
      allocate (ubnrunoff(time%step), source = 0.)
      allocate (ubntss(time%step), source = 0.)

!! Arrays for subdaily erosion modeling by Jaehak Jeong
   allocate (hhsedy(mhru,time%step), source = 0.)
   allocate (ovrlnd_dt(mhru,time%step), source = 0.)
   allocate (init_abstrc(mhru), source = 0.)
   allocate (hhsurfq(mhru,time%step), source = 0.)

       !Tillage factor on SOM decomposition
       allocate (tillage_switch(mhru), source = 0)
       allocate (tillage_depth(mhru), source = 0.)
       allocate (tillage_days(mhru), source = 0)
       allocate (tillage_factor(mhru), source = 0.)
       
       tillage_switch = 0
       tillage_depth = 0.
       tillage_days = 0
       tillage_factor = 0.
       
      !! By Zhang for C/N cycling
      !! ============================
          
      call zero0
      call zero1
      call zero2
      call zeroini

!!    zero reservoir module
      return
      end