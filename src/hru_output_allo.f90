      subroutine hru_output_allo

      use output_landscape_module
      use hydrograph_module
      use constituent_mass_module
      use output_ls_pesticide_module
      use output_ls_pathogen_module
      use salt_module
      use cs_module
      use carbon_module
      
      implicit none
      
      integer :: ihru           !               |
      integer :: mhru           !               |
      integer :: isalt          !               |salt ion counter
      integer :: ics            !               |constituent counter
      
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
!!! new carbon files
      allocate (hsc_d(mhru))
      allocate (hsc_m(mhru))
      allocate (hsc_y(mhru))
      allocate (hsc_a(mhru))
      allocate (hrc_d(mhru))
      allocate (hrc_m(mhru))
      allocate (hrc_y(mhru))
      allocate (hrc_a(mhru))
      allocate (hpc_d(mhru))
      allocate (hpc_m(mhru))
      allocate (hpc_y(mhru))
      allocate (hpc_a(mhru))
      allocate (hscf_d(mhru))
      allocate (hscf_m(mhru))
      allocate (hscf_y(mhru))
      allocate (hscf_a(mhru))  
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
      !rtb salt
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
          do isalt=1,cs_db%num_salts
            !monthly mass arrays
            hsaltb_m(ihru)%salt(isalt)%soil = 0.
            hsaltb_m(ihru)%salt(isalt)%surq = 0.
            hsaltb_m(ihru)%salt(isalt)%latq = 0.
            hsaltb_m(ihru)%salt(isalt)%urbq = 0.
            hsaltb_m(ihru)%salt(isalt)%wetq = 0.
            hsaltb_m(ihru)%salt(isalt)%tile = 0.
            hsaltb_m(ihru)%salt(isalt)%perc = 0.
            hsaltb_m(ihru)%salt(isalt)%wtsp = 0.
            hsaltb_m(ihru)%salt(isalt)%irsw = 0.
            hsaltb_m(ihru)%salt(isalt)%irgw = 0.
            hsaltb_m(ihru)%salt(isalt)%irwo = 0.
            hsaltb_m(ihru)%salt(isalt)%rain = 0.
            hsaltb_m(ihru)%salt(isalt)%dryd = 0.
            hsaltb_m(ihru)%salt(isalt)%road = 0.
            hsaltb_m(ihru)%salt(isalt)%fert = 0.
            hsaltb_m(ihru)%salt(isalt)%amnd = 0.
            hsaltb_m(ihru)%salt(isalt)%uptk = 0.
            !yearly mass arrays
            hsaltb_y(ihru)%salt(isalt)%soil = 0.
            hsaltb_y(ihru)%salt(isalt)%surq = 0.
            hsaltb_y(ihru)%salt(isalt)%latq = 0.
            hsaltb_y(ihru)%salt(isalt)%urbq = 0.
            hsaltb_y(ihru)%salt(isalt)%wetq = 0.
            hsaltb_y(ihru)%salt(isalt)%tile = 0.
            hsaltb_y(ihru)%salt(isalt)%perc = 0.
            hsaltb_y(ihru)%salt(isalt)%wtsp = 0.
            hsaltb_y(ihru)%salt(isalt)%irsw = 0.
            hsaltb_y(ihru)%salt(isalt)%irgw = 0.
            hsaltb_y(ihru)%salt(isalt)%irwo = 0.
            hsaltb_y(ihru)%salt(isalt)%rain = 0.
            hsaltb_y(ihru)%salt(isalt)%dryd = 0.
            hsaltb_y(ihru)%salt(isalt)%road = 0.
            hsaltb_y(ihru)%salt(isalt)%fert = 0.
            hsaltb_y(ihru)%salt(isalt)%amnd = 0.
            hsaltb_y(ihru)%salt(isalt)%uptk = 0.
            !average annual mass arrays
            hsaltb_a(ihru)%salt(isalt)%soil = 0.
            hsaltb_a(ihru)%salt(isalt)%surq = 0.
            hsaltb_a(ihru)%salt(isalt)%latq = 0.
            hsaltb_a(ihru)%salt(isalt)%urbq = 0.
            hsaltb_a(ihru)%salt(isalt)%wetq = 0.
            hsaltb_a(ihru)%salt(isalt)%tile = 0.
            hsaltb_a(ihru)%salt(isalt)%perc = 0.
            hsaltb_a(ihru)%salt(isalt)%wtsp = 0.
            hsaltb_a(ihru)%salt(isalt)%irsw = 0.
            hsaltb_a(ihru)%salt(isalt)%irgw = 0.
            hsaltb_a(ihru)%salt(isalt)%irwo = 0.
            hsaltb_a(ihru)%salt(isalt)%rain = 0.
            hsaltb_a(ihru)%salt(isalt)%dryd = 0.
            hsaltb_a(ihru)%salt(isalt)%road = 0.
            hsaltb_a(ihru)%salt(isalt)%fert = 0.
            hsaltb_a(ihru)%salt(isalt)%amnd = 0.
            hsaltb_a(ihru)%salt(isalt)%uptk = 0.
          enddo
          hsaltb_m(ihru)%salt(1)%diss = 0.
          hsaltb_y(ihru)%salt(1)%diss = 0.
          hsaltb_a(ihru)%salt(1)%diss = 0.
        end do
      end if
      !rtb cs
      if (cs_db%num_cs > 0) then
        allocate (hcsb_d(mhru))
        allocate (hcsb_m(mhru))
        allocate (hcsb_y(mhru))
        allocate (hcsb_a(mhru))
        do ihru = 1, sp_ob%hru
          allocate (hcsb_d(ihru)%cs(cs_db%num_cs))
          allocate (hcsb_m(ihru)%cs(cs_db%num_cs))
          allocate (hcsb_y(ihru)%cs(cs_db%num_cs))
          allocate (hcsb_a(ihru)%cs(cs_db%num_cs))
          do ics=1,cs_db%num_cs
            !monthly mass arrays
            hcsb_m(ihru)%cs(ics)%soil = 0.
            hcsb_m(ihru)%cs(ics)%surq = 0.
            hcsb_m(ihru)%cs(ics)%sedm = 0.
            hcsb_m(ihru)%cs(ics)%latq = 0.
            hcsb_m(ihru)%cs(ics)%urbq = 0.
            hcsb_m(ihru)%cs(ics)%wetq = 0.
            hcsb_m(ihru)%cs(ics)%tile = 0.
            hcsb_m(ihru)%cs(ics)%perc = 0.
            hcsb_m(ihru)%cs(ics)%wtsp = 0.
            hcsb_m(ihru)%cs(ics)%irsw = 0.
            hcsb_m(ihru)%cs(ics)%irgw = 0.
            hcsb_m(ihru)%cs(ics)%irwo = 0.
            hcsb_m(ihru)%cs(ics)%rain = 0.
            hcsb_m(ihru)%cs(ics)%dryd = 0.
            hcsb_m(ihru)%cs(ics)%fert = 0.
            hcsb_m(ihru)%cs(ics)%uptk = 0.
            hcsb_m(ihru)%cs(ics)%rctn = 0.
            hcsb_m(ihru)%cs(ics)%sorb = 0.
            hcsb_m(ihru)%cs(ics)%conc = 0.
            hcsb_m(ihru)%cs(ics)%srbd = 0.
            !yearly mass arrays
            hcsb_y(ihru)%cs(ics)%soil = 0.
            hcsb_y(ihru)%cs(ics)%surq = 0.
            hcsb_y(ihru)%cs(ics)%sedm = 0.
            hcsb_y(ihru)%cs(ics)%latq = 0.
            hcsb_y(ihru)%cs(ics)%urbq = 0.
            hcsb_y(ihru)%cs(ics)%wetq = 0.
            hcsb_y(ihru)%cs(ics)%tile = 0.
            hcsb_y(ihru)%cs(ics)%perc = 0.
            hcsb_y(ihru)%cs(ics)%wtsp = 0.
            hcsb_y(ihru)%cs(ics)%irsw = 0.
            hcsb_y(ihru)%cs(ics)%irgw = 0.
            hcsb_y(ihru)%cs(ics)%irwo = 0.
            hcsb_y(ihru)%cs(ics)%rain = 0.
            hcsb_y(ihru)%cs(ics)%dryd = 0.
            hcsb_y(ihru)%cs(ics)%fert = 0.
            hcsb_y(ihru)%cs(ics)%uptk = 0.
            hcsb_y(ihru)%cs(ics)%rctn = 0.
            hcsb_y(ihru)%cs(ics)%sorb = 0.
            hcsb_y(ihru)%cs(ics)%conc = 0.
            hcsb_y(ihru)%cs(ics)%srbd = 0.
            !average annual mass arrays
            hcsb_a(ihru)%cs(ics)%soil = 0.
            hcsb_a(ihru)%cs(ics)%surq = 0.
            hcsb_a(ihru)%cs(ics)%sedm = 0.
            hcsb_a(ihru)%cs(ics)%latq = 0.
            hcsb_a(ihru)%cs(ics)%urbq = 0.
            hcsb_a(ihru)%cs(ics)%wetq = 0.
            hcsb_a(ihru)%cs(ics)%tile = 0.
            hcsb_a(ihru)%cs(ics)%perc = 0.
            hcsb_a(ihru)%cs(ics)%wtsp = 0.
            hcsb_a(ihru)%cs(ics)%irsw = 0.
            hcsb_a(ihru)%cs(ics)%irgw = 0.
            hcsb_a(ihru)%cs(ics)%irwo = 0.
            hcsb_a(ihru)%cs(ics)%rain = 0.
            hcsb_a(ihru)%cs(ics)%dryd = 0.
            hcsb_a(ihru)%cs(ics)%fert = 0.
            hcsb_a(ihru)%cs(ics)%uptk = 0.
            hcsb_a(ihru)%cs(ics)%rctn = 0.
            hcsb_a(ihru)%cs(ics)%sorb = 0.
            hcsb_a(ihru)%cs(ics)%conc = 0.
            hcsb_a(ihru)%cs(ics)%srbd = 0.
          enddo
        end do
      end if

      return
      end subroutine hru_output_allo