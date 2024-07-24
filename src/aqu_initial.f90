      subroutine aqu_initial 
    
      use aquifer_module  
      use hydrograph_module
      use constituent_mass_module
      use aqu_pesticide_module
      use salt_module !rtb salt
      use salt_aquifer !rtb salt
      use cs_module !rtb cs
      use cs_aquifer !rtb cs
       
      implicit none
      
      integer :: iaq                   !none      |counter
      integer :: iob                   !          | 
      integer :: iaqdb                 !          | 
      integer :: isalt                 !          |salt ion counter
      integer :: ics                   !          |constituent counter 

      !allocate objects for each aquifer
      allocate (aqu_om_init(sp_ob%aqu))
      allocate (aqu_d(sp_ob%aqu))
      allocate (aqu_dat(sp_ob%aqu))
      allocate (aqu_prm(sp_ob%aqu))
      allocate (aqu_m(sp_ob%aqu))
      allocate (aqu_y(sp_ob%aqu))
      allocate (aqu_a(sp_ob%aqu))
      allocate (cs_aqu(sp_ob%aqu))
      allocate (aqupst_d(sp_ob%aqu))
      allocate (aqupst_m(sp_ob%aqu))
      allocate (aqupst_y(sp_ob%aqu))
      allocate (aqupst_a(sp_ob%aqu))

      if (cs_db%num_pests > 0) then
        allocate (baqupst_d%pest(cs_db%num_pests))
        allocate (baqupst_m%pest(cs_db%num_pests))
        allocate (baqupst_y%pest(cs_db%num_pests))
        allocate (baqupst_a%pest(cs_db%num_pests))
      end if
      
      !salts !rtb salt
      if (cs_db%num_salts > 0) then
        allocate (asaltb_d(sp_ob%aqu))
        allocate (asaltb_m(sp_ob%aqu))
        allocate (asaltb_y(sp_ob%aqu))
        allocate (asaltb_a(sp_ob%aqu))
        do iaq = 1,sp_ob%aqu
          allocate (asaltb_d(iaq)%salt(cs_db%num_salts))
          allocate (asaltb_m(iaq)%salt(cs_db%num_salts))
          allocate (asaltb_y(iaq)%salt(cs_db%num_salts))
          allocate (asaltb_a(iaq)%salt(cs_db%num_salts))
          do isalt=1,cs_db%num_salts
            asaltb_m(iaq)%salt(isalt)%rchrg = 0.
            asaltb_m(iaq)%salt(isalt)%seep = 0.
            asaltb_m(iaq)%salt(isalt)%saltgw = 0.
            asaltb_m(iaq)%salt(isalt)%conc = 0.
            asaltb_m(iaq)%salt(isalt)%irr = 0.
            asaltb_y(iaq)%salt(isalt)%rchrg = 0.
            asaltb_y(iaq)%salt(isalt)%seep = 0.
            asaltb_y(iaq)%salt(isalt)%saltgw = 0.
            asaltb_y(iaq)%salt(isalt)%conc = 0.
            asaltb_y(iaq)%salt(isalt)%irr = 0.
            asaltb_a(iaq)%salt(isalt)%rchrg = 0.
            asaltb_a(iaq)%salt(isalt)%seep = 0.
            asaltb_a(iaq)%salt(isalt)%saltgw = 0.
            asaltb_a(iaq)%salt(isalt)%conc = 0.
            asaltb_a(iaq)%salt(isalt)%irr = 0.
          enddo
          asaltb_m(iaq)%salt(1)%diss = 0.
          asaltb_y(iaq)%salt(1)%diss = 0.
          asaltb_a(iaq)%salt(1)%diss = 0.
        enddo
      endif
      
      !constituents !rtb cs
      if (cs_db%num_cs > 0) then
        allocate (acsb_d(sp_ob%aqu))
        allocate (acsb_m(sp_ob%aqu))
        allocate (acsb_y(sp_ob%aqu))
        allocate (acsb_a(sp_ob%aqu))
        do iaq = 1,sp_ob%aqu
          allocate (acsb_d(iaq)%cs(cs_db%num_cs))
          allocate (acsb_m(iaq)%cs(cs_db%num_cs))
          allocate (acsb_y(iaq)%cs(cs_db%num_cs))
          allocate (acsb_a(iaq)%cs(cs_db%num_cs))
          do ics=1,cs_db%num_cs
            acsb_m(iaq)%cs(ics)%csgw = 0. !monthly
            acsb_m(iaq)%cs(ics)%rchrg = 0.
            acsb_m(iaq)%cs(ics)%seep = 0.
            acsb_m(iaq)%cs(ics)%irr = 0.
            acsb_m(iaq)%cs(ics)%sorb = 0.
            acsb_m(iaq)%cs(ics)%rctn = 0.
            acsb_m(iaq)%cs(ics)%conc = 0.
            acsb_m(iaq)%cs(ics)%srbd = 0.
            acsb_y(iaq)%cs(ics)%csgw = 0. !yearly
            acsb_y(iaq)%cs(ics)%rchrg = 0.
            acsb_y(iaq)%cs(ics)%seep = 0.
            acsb_y(iaq)%cs(ics)%irr = 0.
            acsb_y(iaq)%cs(ics)%sorb = 0.
            acsb_y(iaq)%cs(ics)%rctn = 0.
            acsb_y(iaq)%cs(ics)%conc = 0.
            acsb_y(iaq)%cs(ics)%srbd = 0.
            acsb_a(iaq)%cs(ics)%csgw = 0. !average annual
            acsb_a(iaq)%cs(ics)%rchrg = 0.
            acsb_a(iaq)%cs(ics)%seep = 0.
            acsb_a(iaq)%cs(ics)%irr = 0.
            acsb_a(iaq)%cs(ics)%sorb = 0.
            acsb_a(iaq)%cs(ics)%rctn = 0.
            acsb_a(iaq)%cs(ics)%conc = 0.
            acsb_a(iaq)%cs(ics)%srbd = 0.
          enddo
        enddo
      endif
      
      do iaq = 1, sp_ob%aqu
        if (cs_db%num_pests > 0) then
          !! allocate constituents
          allocate (cs_aqu(iaq)%pest(cs_db%num_pests))
          allocate (aqupst_d(iaq)%pest(cs_db%num_pests))
          allocate (aqupst_m(iaq)%pest(cs_db%num_pests))
          allocate (aqupst_y(iaq)%pest(cs_db%num_pests))
          allocate (aqupst_a(iaq)%pest(cs_db%num_pests))
          allocate (cs_aqu(iaq)%path(cs_db%num_paths))
          allocate (cs_aqu(iaq)%hmet(cs_db%num_metals))
          allocate (cs_aqu(iaq)%salt(cs_db%num_salts))
        end if
        !salts !rtb salt
        if (cs_db%num_salts > 0) then
          allocate (cs_aqu(iaq)%salt(cs_db%num_salts)) !salt ion mass (kg)
          allocate (cs_aqu(iaq)%salt_min(5)) !salt mineral fractions
          allocate (cs_aqu(iaq)%saltc(cs_db%num_salts)) !salt ion concentration (mg/L)
          cs_aqu(iaq)%salt = 0. !rtb salt
          cs_aqu(iaq)%salt_min = 0.
          cs_aqu(iaq)%saltc = 0.
        end if
        !constituents !rtb cs
        if (cs_db%num_cs > 0) then
          allocate (cs_aqu(iaq)%cs(cs_db%num_cs)) !constituent mass (kg)
          allocate (cs_aqu(iaq)%csc(cs_db%num_cs)) !constituent concentration (mg/L)
          allocate (cs_aqu(iaq)%cs_sorb(cs_db%num_cs)) !sorbed constituent mass (kg/ha)
          allocate (cs_aqu(iaq)%csc_sorb(cs_db%num_cs)) !sorbed constituent mass concentration (mg/kg)
          cs_aqu(iaq)%cs = 0. !rtb cs
          cs_aqu(iaq)%csc = 0.
          cs_aqu(iaq)%cs_sorb = 0.
          cs_aqu(iaq)%csc_sorb = 0.
        end if
        
        iob = sp_ob1%aqu + iaq - 1
        iaqdb = ob(iob)%props

        !! initialize parameters
        aqu_dat(iaq) = aqudb(iaqdb)
        
        aqu_prm(iaq)%area_ha = ob(iob)%area_ha
        aqu_prm(iaq)%alpha_e = Exp(-aqu_dat(iaq)%alpha)
        aqu_prm(iaq)%nloss = Exp(-.693 / (aqu_dat(iaq)%hlife_n + .1))
        
        aqu_d(iaq)%flo = aqu_dat(iaq)%flo
        aqu_d(iaq)%dep_wt = aqu_dat(iaq)%dep_wt
        aqu_d(iaq)%stor = 1000. * (aqu_dat(iaq)%dep_bot - aqu_d(iaqdb)%dep_wt) * aqu_dat(iaq)%spyld
        !! convert ppm -> kg    (m3=10*mm*ha)     kg=m3*ppm/1000
        aqu_d(iaq)%no3_st = (10. * aqu_d(iaq)%flo * aqu_prm(iaq)%area_ha) * aqu_dat(iaq)%no3 / 1000.
        aqu_d(iaq)%minp = 0.
        aqu_d(iaq)%cbn = aqu_dat(iaq)%cbn
        aqu_d(iaq)%rchrg = 0.
        aqu_d(iaq)%seep = 0.
        aqu_d(iaq)%revap = 0.
        aqu_d(iaq)%no3_rchg = 0.
        aqu_d(iaq)%no3_loss = 0.
        aqu_d(iaq)%no3_lat = 0.
        aqu_d(iaq)%no3_seep = 0.
        aqu_d(iaq)%flo_cha = 0.
        aqu_d(iaq)%flo_res = 0.
        aqu_d(iaq)%flo_ls = 0.
      end do
            
      ! pesticides and constituents are initialized in aqu_read_init

      return
      end subroutine aqu_initial         