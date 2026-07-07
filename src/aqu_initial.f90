      subroutine aqu_initial 
    
      use aquifer_module  
      use hydrograph_module
      use constituent_mass_module
      use aqu_pesticide_module
       
      implicit none
      
      character (len=500) :: header    !header for output file
      character (len=80) :: titldum    !title 
      integer :: iaq                   !none      |counter
      integer :: iob                   !          | 
      integer :: iaqdb                 !          | 
      integer :: ipest                 !none      |counter
      integer :: ipath                 !          | 
      integer :: isalt                 !          | 
      integer :: i                     !none      |counter
      integer :: init                  !          | 
      integer :: idat

      !allocate objects for each aquifer
      allocate (aqu_om_init(sp_ob%aqu))
      allocate (aqu_d(sp_ob%aqu))
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
              
        iob = sp_ob1%aqu + iaq - 1
        iaqdb = ob(iob)%props

        !! initialize parameters
        aqu_prm(iaq)%area_ha = ob(iob)%area_ha
        aqu_prm(iaq)%alpha = aqudb(iaqdb)%alpha
        aqu_prm(iaq)%flo_min = aqudb(iaqdb)%flo_min
        aqu_prm(iaq)%revap_co = aqudb(iaqdb)%revap_co
        aqu_prm(iaq)%revap_min = aqudb(iaqdb)%revap_min
        aqu_prm(iaq)%spyld = aqudb(iaqdb)%spyld
        aqu_prm(iaq)%seep = aqudb(iaqdb)%seep
        aqu_prm(iaq)%alpha_e = Exp(-aqudb(iaqdb)%alpha)
        aqu_prm(iaq)%bf_max = aqudb(iaq)%bf_max
        aqu_prm(iaq)%nloss = Exp(-.693 / (aqudb(iaqdb)%hlife_n + .1))
        aqu_d(iaq)%flo = aqudb(iaqdb)%flo
        aqu_d(iaq)%dep_wt = aqudb(iaqdb)%dep_wt
        aqu_d(iaq)%stor = 1000. * (aqudb(iaqdb)%dep_bot - aqu_d(iaqdb)%dep_wt) * aqudb(iaqdb)%spyld
        aqu_d(iaq)%no3 = aqudb(iaqdb)%no3
        aqu_d(iaq)%minp = aqudb(iaqdb)%minp
        aqu_d(iaq)%cbn = aqudb(iaqdb)%cbn
        aqu_d(iaq)%rchrg = 0.
        aqu_d(iaq)%seep = 0.
        aqu_d(iaq)%revap = 0.
        aqu_d(iaq)%rchrg_n = 0.
        aqu_d(iaq)%nloss = 0.
        aqu_d(iaq)%no3gw = 0.
        aqu_d(iaq)%seepno3 = 0.
        aqu_d(iaq)%flo_cha = 0.
        aqu_d(iaq)%flo_res = 0.
        aqu_d(iaq)%flo_ls = 0
      end do
            
      ! pesticides and constituents are initialized in aqu_read_init

      return
      end subroutine aqu_initial         