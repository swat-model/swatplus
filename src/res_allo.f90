      subroutine res_allo
      
      use reservoir_module
      use reservoir_data_module
      use res_pesticide_module
      use res_salt_module !rtb salt
      use res_cs_module !rtb cs
      use hydrograph_module
      use constituent_mass_module
      use water_body_module
      
      implicit none     

      integer :: ires           !             |
      integer :: mres           !             |
      
      mres = sp_ob%res
      allocate (res(0:mres))
      allocate (res_prm(0:mres))
      allocate (res_hyd(0:mres))
      allocate (res_om_init(0:mres))
      allocate (res_ob(0:mres))
      allocate (res_in_d(mres))
      allocate (res_in_m(mres))
      allocate (res_in_y(mres))
      allocate (res_in_a(mres))
      allocate (res_out_d(mres))
      allocate (res_out_m(mres))
      allocate (res_out_y(mres))
      allocate (res_out_a(mres))
      allocate (res_wat_d(mres))
      allocate (res_wat_m(mres))
      allocate (res_wat_y(mres))
      allocate (res_wat_a(mres))
      allocate (res_water(mres))
      allocate (res_benthic(mres))
      allocate (respst_d(mres))
      allocate (respst_m(mres))
      allocate (respst_y(mres))
      allocate (respst_a(mres))
      allocate (ressalt_d(mres)) !rtb salt
      allocate (ressalt_m(mres))
      allocate (ressalt_y(mres))
      allocate (ressalt_a(mres))
      allocate (rescs_d(mres)) !rtb cs
      allocate (rescs_m(mres))
      allocate (rescs_y(mres))
      allocate (rescs_a(mres))
      
      if (cs_db%num_tot > 0) then
        do ires = 1, sp_ob%res
          if (cs_db%num_pests > 0) then 
            allocate (res_water(ires)%pest(cs_db%num_pests))
            allocate (res_benthic(ires)%pest(cs_db%num_pests))
            allocate (res_ob(ires)%aq_mix(cs_db%num_pests))
            allocate (respst_d(ires)%pest(cs_db%num_pests))
            allocate (respst_m(ires)%pest(cs_db%num_pests))
            allocate (respst_y(ires)%pest(cs_db%num_pests))
            allocate (respst_a(ires)%pest(cs_db%num_pests))
            allocate (res_water(ires)%path(cs_db%num_paths))
          end if 
          allocate (res_benthic(ires)%path(cs_db%num_paths))
          allocate (res_water(ires)%hmet(cs_db%num_metals))
          allocate (res_benthic(ires)%hmet(cs_db%num_metals))
          !rtb salt
          if (cs_db%num_salts > 0) then
            allocate (ressalt_d(ires)%salt(cs_db%num_salts))
            allocate (ressalt_m(ires)%salt(cs_db%num_salts))
            allocate (ressalt_y(ires)%salt(cs_db%num_salts))
            allocate (ressalt_a(ires)%salt(cs_db%num_salts))  
          endif
          allocate (res_water(ires)%salt(cs_db%num_salts))
          allocate (res_water(ires)%saltc(cs_db%num_salts))
          allocate (res_benthic(ires)%salt(cs_db%num_salts))
          !rtb cs
          if (cs_db%num_cs > 0) then
            allocate (rescs_d(ires)%cs(cs_db%num_cs))
            allocate (rescs_m(ires)%cs(cs_db%num_cs))
            allocate (rescs_y(ires)%cs(cs_db%num_cs))
            allocate (rescs_a(ires)%cs(cs_db%num_cs))  
          endif
          allocate (res_water(ires)%cs(cs_db%num_cs))
          allocate (res_water(ires)%csc(cs_db%num_cs))
          allocate (res_benthic(ires)%cs(cs_db%num_cs))
        end do
        if (cs_db%num_pests > 0) then
          allocate (brespst_d%pest(cs_db%num_pests))
          allocate (brespst_m%pest(cs_db%num_pests))
          allocate (brespst_y%pest(cs_db%num_pests))
          allocate (brespst_a%pest(cs_db%num_pests))
        end if
      end if

      return
      end subroutine res_allo