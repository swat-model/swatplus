      subroutine hru_output_allo

      use output_landscape_module
      use hydrograph_module
      use constituent_mass_module
      use output_ls_pesticide_module
      use output_ls_pathogen_module
      use output_ls_salt_module
      
      implicit none
      
      integer :: ihru           !               |
      integer :: mhru           !               |
      
      mhru = sp_ob%hru
     
      !!Section 3
      !!this section sets parameters related to soil and other processes

      !! dimension hru output variables
      allocate (hwb_d(mhru))
      allocate (hwb_m(mhru))
      allocate (hwb_y(mhru))
      allocate (hwb_a(mhru))
      allocate (hnb_d(mhru))
      allocate (hnb_m(mhru))
      allocate (hnb_y(mhru))
      allocate (hnb_a(mhru))
      allocate (hls_d(mhru))
!!!  new nut carb files
      allocate (hgl_d(mhru))
      allocate (hgl_m(mhru))
      allocate (hgl_y(mhru))
      allocate (hgl_a(mhru))
      allocate (hcyl_d(mhru))
      allocate (hcyl_m(mhru))
      allocate (hcyl_y(mhru))
      allocate (hcyl_a(mhru))
!!!  new nut carb files
      allocate (hls_m(mhru))
      allocate (hls_y(mhru))
      allocate (hls_a(mhru))
      allocate (hpw_d(mhru))
      allocate (hpw_m(mhru))
      allocate (hpw_y(mhru))
      allocate (hpw_a(mhru))
      if (cs_db%num_pests > 0) then
        allocate (hpestb_d(mhru))
        allocate (hpestb_m(mhru))
        allocate (hpestb_y(mhru))
        allocate (hpestb_a(mhru))
        allocate (bpestb_d%pest(cs_db%num_pests))
        allocate (bpestb_m%pest(cs_db%num_pests))
        allocate (bpestb_y%pest(cs_db%num_pests))
        allocate (bpestb_a%pest(cs_db%num_pests))
        do ihru = 1, sp_ob%hru
          allocate (hpestb_d(ihru)%pest(cs_db%num_pests))
          allocate (hpestb_m(ihru)%pest(cs_db%num_pests))
          allocate (hpestb_y(ihru)%pest(cs_db%num_pests))
          allocate (hpestb_a(ihru)%pest(cs_db%num_pests))
        end do
      end if
      if (cs_db%num_paths > 0) then
        allocate (hpath_bal(mhru))
        allocate (hpathb_m(mhru))
        allocate (hpathb_y(mhru))
        allocate (hpathb_a(mhru))
        do ihru = 1, sp_ob%hru
          allocate (hpath_bal(ihru)%path(cs_db%num_paths))
          allocate (hpathb_m(ihru)%path(cs_db%num_paths))
          allocate (hpathb_y(ihru)%path(cs_db%num_paths))
          allocate (hpathb_a(ihru)%path(cs_db%num_paths))
        end do
      end if
      if (cs_db%num_salts > 0) then
        allocate (hsaltb_d(mhru))
        allocate (hsaltb_m(mhru))
        allocate (hsaltb_y(mhru))
        allocate (hsaltb_a(mhru))
        do ihru = 1, sp_ob%hru
          allocate (hsaltb_d(ihru)%salt(cs_db%num_salts))
          allocate (hsaltb_m(ihru)%salt(cs_db%num_salts))
          allocate (hsaltb_y(ihru)%salt(cs_db%num_salts))
          allocate (hsaltb_a(ihru)%salt(cs_db%num_salts))
        end do
      end if
      !if (cs_db%num_metals > 0) then
      !  allocate (hhmet_bal(mhru))
      !end if
      !if (cs_db%num_salts > 0) then
      !  allocate (hsalt_bal(mhru))
      !end if

      return
      end subroutine hru_output_allo