      subroutine mgt_transplant (itrans)

      use hru_module, only : hru, ihru, ipl
      use plant_module
      use plant_data_module
      use organic_mineral_mass_module
      
      implicit none
      
      integer, intent (in) :: itrans
      integer :: j                   !none       |counter
      integer :: icom                !none       |plant community counter 
      integer :: idp                 !none       |plant database number - plants.plt
      real :: xx
      real :: laimx_pop

      j = ihru
      icom = hru(j)%plant_cov
      idp = pcom(j)%plcur(ipl)%idplt

      ! set initial heat units and other data
      pcom(j)%plcur(ipl)%phuacc = transpl(itrans)%phuacc
      pcom(j)%plcur(ipl)%phuacc_p = transpl(itrans)%fr_yrmat + (pcom(j)%plcur(ipl)%phumat *         &
                                          transpl(itrans)%phuacc) / pcom(j)%plcur(ipl)%phumat_p
      pcom(j)%plg(ipl)%laimxfr = pcom(j)%plcur(ipl)%phuacc / (pcom(j)%plcur(ipl)%phuacc +           &
              Exp(plcp(idp)%leaf1 - plcp(idp)%leaf2 * pcom(j)%plcur(ipl)%phuacc))
      pcom(j)%plg(ipl)%laimxfr_p = pcom(j)%plcur(ipl)%phuacc_p / (pcom(j)%plcur(ipl)%phuacc_p +     &
              Exp(plcp(idp)%leaf1 - plcp(idp)%leaf2 * pcom(j)%plcur(ipl)%phuacc_p))
      !pcom(j)%plg(ipl)%lai = transpl(itrans)%lai
      pl_mass(j)%tot(ipl)%m = transpl(itrans)%bioms
      pcom(j)%plcur(ipl)%curyr_mat = int (transpl(itrans)%fr_yrmat * float(pldb(idp)%mat_yrs))
      pcom(j)%plcur(ipl)%curyr_mat = max (1, pcom(j)%plcur(ipl)%curyr_mat)
      pcom(j)%plcur(ipl)%idplt = pcomdb(icom)%pl(ipl)%db_num
      pcom(j)%plm(ipl)%p_fr = (pldb(idp)%pltpfr1-pldb(idp)%pltpfr3)*            &
          (1. - pcom(j)%plcur(ipl)%phuacc/(pcom(j)%plcur(ipl)%phuacc +          &
          Exp(plcp(idp)%pup1 - plcp(idp)%pup2 *                                 &
          pcom(j)%plcur(ipl)%phuacc))) + pldb(idp)%pltpfr3
      pl_mass(j)%tot(ipl)%n = pcom(j)%plm(ipl)%n_fr * pl_mass(j)%tot(ipl)%m                  
      pcom(j)%plm(ipl)%n_fr = (pldb(idp)%pltnfr1- pldb(idp)%pltnfr3) *          &
          (1.- pcom(j)%plcur(ipl)%phuacc/(pcom(j)%plcur(ipl)%phuacc +           &
          Exp(plcp(idp)%nup1 - plcp(idp)%nup2 *                                 &
          pcom(j)%plcur(ipl)%phuacc))) + pldb(idp)%pltnfr3
          
      !! check plant population to set max lai
      if (transpl(itrans)%pop < 1.e-6) then
        laimx_pop = pldb(idp)%blai
      else
        xx = transpl(itrans)%pop 
        laimx_pop = pldb(idp)%blai * xx / (xx + exp(plcp(idp)%popsc1 - plcp(idp)%popsc2 * xx))
      end if
      pcom(j)%plcur(ipl)%lai_pot = laimx_pop
          
      !! initialize plant mass
      call pl_root_gro(j)
      call pl_seed_gro(j)
      call pl_partition(j)

      return
      end subroutine mgt_transplant