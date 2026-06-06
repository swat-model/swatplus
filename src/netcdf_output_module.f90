      module netcdf_output_module
      use netcdf, only: nf90_global
      use netcdf_writer_module
      use basin_module
      use time_module
      use hydrograph_module
      use maximum_data_module
      use output_landscape_module
      use aquifer_module
      use carbon_module
      use sd_channel_module
      use water_body_module
      use channel_module, only : ch_output
      implicit none
      private
      public :: nc_use_output, nc_set_deflate_from_env
      public :: netcdf_output_init, netcdf_output_finish
      public :: nc_flush_daily_hru, nc_flush_monthly_hru, nc_flush_yearly_hru, nc_flush_aa_hru
      public :: nc_stage_hru_wb, nc_stage_hru_nb, nc_stage_hru_ls, nc_stage_hru_pw
      public :: nc_stage_hru_wb_mon, nc_stage_hru_nb_mon, nc_stage_hru_ls_mon, nc_stage_hru_pw_mon
      public :: nc_stage_hru_wb_yr, nc_stage_hru_nb_yr, nc_stage_hru_ls_yr, nc_stage_hru_pw_yr
      public :: nc_stage_hru_wb_aa, nc_stage_hru_nb_aa, nc_stage_hru_ls_aa, nc_stage_hru_pw_aa
      public :: nc_stage_hru_carbon, nc_stage_hru_carbon_mon, nc_stage_hru_carbon_yr, nc_stage_hru_carbon_aa
      public :: nc_stage_basin_wb, nc_stage_basin_nb, nc_stage_basin_ls, nc_stage_basin_pw
      public :: nc_stage_basin_wb_mon, nc_stage_basin_nb_mon, nc_stage_basin_ls_mon, nc_stage_basin_pw_mon
      public :: nc_stage_basin_wb_yr, nc_stage_basin_nb_yr, nc_stage_basin_ls_yr, nc_stage_basin_pw_yr
      public :: nc_stage_basin_wb_aa, nc_stage_basin_nb_aa, nc_stage_basin_ls_aa, nc_stage_basin_pw_aa
      public :: nc_stage_lsu_wb, nc_stage_lsu_nb, nc_stage_lsu_ls, nc_stage_lsu_pw
      public :: nc_stage_lsu_wb_mon, nc_stage_lsu_nb_mon, nc_stage_lsu_ls_mon, nc_stage_lsu_pw_mon
      public :: nc_stage_lsu_wb_yr, nc_stage_lsu_nb_yr, nc_stage_lsu_ls_yr, nc_stage_lsu_pw_yr
      public :: nc_stage_lsu_wb_aa, nc_stage_lsu_nb_aa, nc_stage_lsu_ls_aa, nc_stage_lsu_pw_aa
      public :: nc_stage_aquifer, nc_stage_aquifer_mon, nc_stage_aquifer_yr, nc_stage_aquifer_aa
      public :: nc_stage_sd_channel, nc_stage_sd_channel_mon, nc_stage_sd_channel_yr, nc_stage_sd_channel_aa
      public :: nc_stage_basin_aquifer, nc_stage_basin_aquifer_mon, nc_stage_basin_aquifer_yr, nc_stage_basin_aquifer_aa
      public :: nc_stage_basin_channel, nc_stage_basin_channel_mon, nc_stage_basin_channel_yr, nc_stage_basin_channel_aa
      public :: nc_stage_channel, nc_stage_channel_mon, nc_stage_channel_yr, nc_stage_channel_aa
      public :: nc_flush_daily_channel, nc_flush_monthly_channel, nc_flush_yearly_channel, nc_flush_aa_channel
      public :: nc_flush_daily_basin, nc_flush_daily_lsu, nc_flush_daily_aqu, nc_flush_daily_sd
      public :: nc_flush_monthly_basin, nc_flush_monthly_lsu, nc_flush_monthly_aqu, nc_flush_monthly_sd
      public :: nc_flush_yearly_basin, nc_flush_yearly_lsu, nc_flush_yearly_aqu, nc_flush_yearly_sd
      public :: nc_flush_aa_basin, nc_flush_aa_lsu, nc_flush_aa_aqu, nc_flush_aa_sd

      integer, parameter :: nwb = 42, nnb = 19, nls = 13, npw = 25
      integer, parameter :: nhsc = 15, nhrc = 6, nhpc = 6, nhscf = 13
      integer, parameter :: naqu = 18
      integer, parameter :: nchnsd = 73
      integer, parameter :: nch = 59
      integer, parameter :: name_len = 64

      type nc_stream
        logical :: on = .false.
        integer :: ncid = -1
        integer :: time_dim = 0, obj_dim = 0, name_dim = 0
        integer :: nobj = 0, nvar = 0, time_idx = 0, tchunk = 30
        integer, allocatable :: var_id(:)
        character(len=32), allocatable :: vname(:)
        real, allocatable :: buf(:,:)
        integer, allocatable :: obj_id(:), gis_id(:)
        character(len=name_len), allocatable :: oname(:)
        character(len=256) :: path = ""
        integer :: vid_time, vid_mo, vid_day_mo, vid_yrc, vid_jday
        integer :: vid_oid, vid_gid, vid_oname
      end type nc_stream

      type(nc_stream) :: s_hru_wb_d, s_hru_wb_m, s_hru_wb_y, s_hru_wb_a
      type(nc_stream) :: s_hru_nb_d, s_hru_nb_m, s_hru_nb_y, s_hru_nb_a
      type(nc_stream) :: s_hru_ls_d, s_hru_ls_m, s_hru_ls_y, s_hru_ls_a
      type(nc_stream) :: s_hru_pw_d, s_hru_pw_m, s_hru_pw_y, s_hru_pw_a
      type(nc_stream) :: s_hru_sc_d, s_hru_rc_d, s_hru_pc_d, s_hru_cf_d
      type(nc_stream) :: s_hru_sc_m, s_hru_rc_m, s_hru_pc_m, s_hru_cf_m
      type(nc_stream) :: s_hru_sc_y, s_hru_rc_y, s_hru_pc_y, s_hru_cf_y
      type(nc_stream) :: s_hru_sc_a, s_hru_rc_a, s_hru_pc_a, s_hru_cf_a
      type(nc_stream) :: s_bsn_wb_d, s_bsn_nb_d, s_bsn_ls_d, s_bsn_pw_d
      type(nc_stream) :: s_bsn_wb_m, s_bsn_nb_m, s_bsn_ls_m, s_bsn_pw_m
      type(nc_stream) :: s_bsn_wb_y, s_bsn_nb_y, s_bsn_ls_y, s_bsn_pw_y
      type(nc_stream) :: s_bsn_wb_a, s_bsn_nb_a, s_bsn_ls_a, s_bsn_pw_a
      type(nc_stream) :: s_lsu_wb_d, s_lsu_nb_d, s_lsu_ls_d, s_lsu_pw_d
      type(nc_stream) :: s_lsu_wb_m, s_lsu_nb_m, s_lsu_ls_m, s_lsu_pw_m
      type(nc_stream) :: s_lsu_wb_y, s_lsu_nb_y, s_lsu_ls_y, s_lsu_pw_y
      type(nc_stream) :: s_lsu_wb_a, s_lsu_nb_a, s_lsu_ls_a, s_lsu_pw_a
      type(nc_stream) :: s_aqu_d, s_aqu_m, s_aqu_y, s_aqu_a
      type(nc_stream) :: s_cha_sd_d, s_cha_sd_m, s_cha_sd_y, s_cha_sd_a
      type(nc_stream) :: s_bsn_aqu_d, s_bsn_aqu_m, s_bsn_aqu_y, s_bsn_aqu_a
      type(nc_stream) :: s_bsn_cha_d, s_bsn_cha_m, s_bsn_cha_y, s_bsn_cha_a
      type(nc_stream) :: s_cha_d, s_cha_m, s_cha_y, s_cha_a
      integer :: nc_epoch_jday = 0
      character(len=64) :: nc_time_units = ""

      contains

      logical function nc_use_output()
        nc_use_output = (pco%cdfout == "y")
      end function nc_use_output

      subroutine nc_set_deflate_from_env()
        character(len=32) :: envv
        call get_environment_variable("SWATPLUS_NC_DEFLATE", envv)
        if (len_trim(envv) > 0) call nc_set_deflate_level(ichar(envv(1:1)) - ichar('0'))
      end subroutine nc_set_deflate_from_env

      subroutine wb_pack(wb, arr)
        type(output_waterbal), intent(in) :: wb
        real, intent(out) :: arr(nwb)
        arr(1)=wb%precip; arr(2)=wb%snofall; arr(3)=wb%snomlt; arr(4)=wb%surq_gen
        arr(5)=wb%latq; arr(6)=wb%wateryld; arr(7)=wb%perc; arr(8)=wb%et
        arr(9)=wb%ecanopy; arr(10)=wb%eplant; arr(11)=wb%esoil; arr(12)=wb%surq_cont
        arr(13)=wb%cn; arr(14)=wb%sw_init; arr(15)=wb%sw_final; arr(16)=wb%sw
        arr(17)=wb%sw_300; arr(18)=wb%sno_init; arr(19)=wb%sno_final; arr(20)=wb%snopack
        arr(21)=wb%pet; arr(22)=wb%qtile; arr(23)=wb%irr; arr(24)=wb%surq_runon
        arr(25)=wb%latq_runon; arr(26)=wb%overbank; arr(27)=wb%surq_cha
        arr(28)=wb%surq_res; arr(29)=wb%surq_ls; arr(30)=wb%latq_cha
        arr(31)=wb%latq_res; arr(32)=wb%latq_ls; arr(33)=wb%gwsoil
        arr(34)=wb%satex; arr(35)=wb%satex_chan; arr(36)=wb%delsw
        arr(37)=wb%lagsurf; arr(38)=wb%laglatq; arr(39)=wb%lagsatex
        arr(40)=wb%wet_evap; arr(41)=wb%wet_out; arr(42)=wb%wet_stor
      end subroutine wb_pack

      subroutine nb_pack(nb, arr)
        type(output_nutbal), intent(in) :: nb
        real, intent(out) :: arr(nnb)
        arr(1)=nb%grazn; arr(2)=nb%grazp; arr(3)=nb%lab_min_p; arr(4)=nb%act_sta_p
        arr(5)=nb%fertn; arr(6)=nb%fertp; arr(7)=nb%fixn; arr(8)=nb%denit
        arr(9)=nb%act_nit_n; arr(10)=nb%act_sta_n; arr(11)=nb%org_lab_p
        arr(12)=nb%rsd_nitorg_n; arr(13)=nb%rsd_laborg_p; arr(14)=nb%no3atmo
        arr(15)=nb%nh4atmo; arr(16)=nb%nuptake; arr(17)=nb%puptake
        arr(18)=nb%gwsoiln; arr(19)=nb%gwsoilp
      end subroutine nb_pack

      subroutine ls_pack(ls, arr, percn)
        type(output_losses), intent(in) :: ls
        real, intent(in) :: percn
        real, intent(out) :: arr(nls)
        arr(1)=ls%sedyld; arr(2)=ls%sedorgn; arr(3)=ls%sedorgp; arr(4)=ls%surqno3
        arr(5)=ls%latno3; arr(6)=ls%surqsolp; arr(7)=ls%usle; arr(8)=ls%sedminp
        arr(9)=ls%tileno3; arr(10)=ls%lchlabp; arr(11)=ls%tilelabp; arr(12)=ls%satexn
        arr(13)=percn
      end subroutine ls_pack

      subroutine pw_pack(pw, arr)
        type(output_plantweather), intent(in) :: pw
        real, intent(out) :: arr(npw)
        arr(1)=pw%lai; arr(2)=pw%bioms; arr(3)=pw%yield; arr(4)=pw%residue
        arr(5)=pw%sol_tmp; arr(6)=pw%strsw; arr(7)=pw%strsa; arr(8)=pw%strstmp
        arr(9)=pw%strsn; arr(10)=pw%strsp; arr(11)=pw%strss; arr(12)=pw%nplnt
        arr(13)=pw%percn; arr(14)=pw%pplnt; arr(15)=pw%tmx; arr(16)=pw%tmn
        arr(17)=pw%tmpav; arr(18)=pw%solrad; arr(19)=pw%wndspd; arr(20)=pw%rhum
        arr(21)=pw%phubase0; arr(22)=pw%lai_max; arr(23)=pw%bm_max
        arr(24)=pw%bm_grow; arr(25)=pw%c_gro
      end subroutine pw_pack

      subroutine hsc_pack(x, arr)
        type(carbon_soil_gain_losses), intent(in) :: x
        real, intent(out) :: arr(nhsc)
        arr(1)=x%sed_c; arr(2)=x%surq_c; arr(3)=x%surq_doc; arr(4)=x%surq_dic
        arr(5)=x%latq_c; arr(6)=x%latq_doc; arr(7)=x%latq_dic; arr(8)=x%perc_c
        arr(9)=x%perc_doc; arr(10)=x%perc_dic; arr(11)=x%rsd_decay_c
        arr(12)=x%man_app_c; arr(13)=x%man_graz_c; arr(14)=x%rsp_c; arr(15)=x%emit_c
      end subroutine hsc_pack

      subroutine hrc_pack(x, arr)
        type(carbon_residue_gain_losses), intent(in) :: x
        real, intent(out) :: arr(nhrc)
        arr(1)=x%plant_surf_c; arr(2)=x%plant_root_c; arr(3)=x%rsd_surfdecay_c
        arr(4)=x%rsd_rootdecay_c; arr(5)=x%harv_stov_c; arr(6)=x%emit_c
      end subroutine hrc_pack

      subroutine aqu_pack(x, arr)
        type(aquifer_dynamic), intent(in) :: x
        real, intent(out) :: arr(naqu)
        arr(1)=x%flo; arr(2)=x%dep_wt; arr(3)=x%stor; arr(4)=x%rchrg
        arr(5)=x%seep; arr(6)=x%revap; arr(7)=x%no3_st; arr(8)=x%minp
        arr(9)=x%cbn; arr(10)=x%orgn; arr(11)=x%no3_rchg; arr(12)=x%no3_loss
        arr(13)=x%no3_lat; arr(14)=x%no3_seep; arr(15)=x%flo_cha
        arr(16)=x%flo_res; arr(17)=x%flo_ls; arr(18)=0.
      end subroutine aqu_pack

      subroutine hyd_pack(h, arr, off)
        type(hyd_output), intent(in) :: h
        integer, intent(in) :: off
        real, intent(inout) :: arr(:)
        arr(off+1)=h%flo; arr(off+2)=h%sed; arr(off+3)=h%orgn; arr(off+4)=h%sedp
        arr(off+5)=h%no3; arr(off+6)=h%solp; arr(off+7)=h%chla; arr(off+8)=h%nh3
        arr(off+9)=h%no2; arr(off+10)=h%cbod; arr(off+11)=h%dox; arr(off+12)=h%san
        arr(off+13)=h%sil; arr(off+14)=h%cla; arr(off+15)=h%sag; arr(off+16)=h%lag
        arr(off+17)=h%grv; arr(off+18)=h%temp
      end subroutine hyd_pack

      subroutine chn_sd_pack(wb, stor, hin, hout, wtemp, arr)
        type(water_body), intent(in) :: wb
        type(hyd_output), intent(in) :: stor, hin, hout
        real, intent(in) :: wtemp
        real, intent(out) :: arr(nchnsd)
        arr(1)=wb%area_ha; arr(2)=wb%precip; arr(3)=wb%evap; arr(4)=wb%seep
        call hyd_pack(stor, arr, 4)
        call hyd_pack(hin, arr, 22)
        call hyd_pack(hout, arr, 40)
        arr(59)=wtemp
        if (nchnsd > 59) arr(60:nchnsd) = 0.
      end subroutine chn_sd_pack

      subroutine bch_pack(c, arr)
        type(ch_output), intent(in) :: c
        real, intent(out) :: arr(nch)
        arr(1)=c%flo_in;  arr(2)=c%flo_out; arr(3)=c%evap;     arr(4)=c%tloss
        arr(5)=c%sed_in;  arr(6)=c%sed_out; arr(7)=c%sed_conc
        arr(8)=c%orgn_in; arr(9)=c%orgn_out
        arr(10)=c%orgp_in; arr(11)=c%orgp_out
        arr(12)=c%no3_in;  arr(13)=c%no3_out
        arr(14)=c%nh4_in;  arr(15)=c%nh4_out
        arr(16)=c%no2_in;  arr(17)=c%no2_out
        arr(18)=c%solp_in; arr(19)=c%solp_out
        arr(20)=c%chla_in; arr(21)=c%chla_out
        arr(22)=c%cbod_in; arr(23)=c%cbod_out
        arr(24)=c%dis_in;  arr(25)=c%dis_out
        arr(26)=c%solpst_in;  arr(27)=c%solpst_out
        arr(28)=c%sorbpst_in; arr(29)=c%sorbpst_out
        arr(30)=c%react;   arr(31)=c%volat;   arr(32)=c%setlpst
        arr(33)=c%resuspst; arr(34)=c%difus; arr(35)=c%reactb
        arr(36)=c%bury;    arr(37)=c%sedpest
        arr(38)=c%bacp;    arr(39)=c%baclp
        arr(40)=c%met1;    arr(41)=c%met2;    arr(42)=c%met3
        arr(43)=c%sand_in; arr(44)=c%sand_out
        arr(45)=c%silt_in; arr(46)=c%silt_out
        arr(47)=c%clay_in; arr(48)=c%clay_out
        arr(49)=c%smag_in; arr(50)=c%smag_out
        arr(51)=c%lag_in;  arr(52)=c%lag_out
        arr(53)=c%grvl_in; arr(54)=c%grvl_out
        arr(55)=c%bnk_ero; arr(56)=c%ch_deg
        arr(57)=c%ch_dep;  arr(58)=c%fp_dep
        arr(59)=c%tot_ssed
      end subroutine bch_pack

      subroutine nc_epoch_init()
        write(nc_time_units,'(a,i0.4,a)') "days since ", pco%yrc_start, "-01-01"
        nc_epoch_jday = pco%day_start
      end subroutine nc_epoch_init

      subroutine nc_stream_open(s, path, nobj, vnames, tchunk)
        type(nc_stream), intent(inout) :: s
        character(len=*), intent(in) :: path
        integer, intent(in) :: nobj, tchunk
        character(len=*), intent(in) :: vnames(:)
        integer :: i, dimids(2), chunk(2), n
        n = size(vnames)
        s%on = .true.
        s%path = path
        s%nobj = nobj
        s%nvar = n
        s%tchunk = tchunk
        s%time_idx = 0
        allocate(s%vname(n), s%var_id(n), s%buf(nobj,n))
        s%vname = vnames
        s%buf = 0.
        allocate(s%obj_id(nobj), s%gis_id(nobj), s%oname(nobj))
        call nc_create_file(path, s%ncid)
        call nc_put_att_text(s%ncid, nf90_global, "title", "SWAT+ "//trim(path))
        call nc_put_att_text(s%ncid, nf90_global, "model", "SWAT+")
        call nc_put_att_text(s%ncid, nf90_global, "Conventions", "CF-1.10")
        call nc_def_dim_unlim(s%ncid, "time", s%time_dim)
        call nc_def_dim_len(s%ncid, "obj", nobj, s%obj_dim)
        call nc_def_dim_len(s%ncid, "name_strlen", name_len, s%name_dim)
        dimids(1) = s%obj_dim
        dimids(2) = s%time_dim
        chunk(1) = max(1, min(nobj, 512))
        chunk(2) = max(1, tchunk)
        call nc_def_var_r32(s%ncid, "time", (/s%time_dim/), s%vid_time, (/1/), .false.)
        call nc_put_att_text(s%ncid, s%vid_time, "units", trim(nc_time_units))
        call nc_def_var_r32(s%ncid, "mo", (/s%time_dim/), s%vid_mo, (/1/), .false.)
        call nc_def_var_r32(s%ncid, "day_mo", (/s%time_dim/), s%vid_day_mo, (/1/), .false.)
        call nc_def_var_r32(s%ncid, "yrc", (/s%time_dim/), s%vid_yrc, (/1/), .false.)
        call nc_def_var_r32(s%ncid, "jday", (/s%time_dim/), s%vid_jday, (/1/), .false.)
        call nc_def_var_r32(s%ncid, "obj_id", (/s%obj_dim/), s%vid_oid, (/chunk(1)/), .false.)
        call nc_def_var_r32(s%ncid, "gis_id", (/s%obj_dim/), s%vid_gid, (/chunk(1)/), .false.)
        do i = 1, n
          call nc_def_var_r32(s%ncid, trim(s%vname(i)), dimids, s%var_id(i), chunk, .true.)
        end do
        call nc_enddef_file(s%ncid)
        write(9000,*) "HRU                       ", trim(path)
      end subroutine nc_stream_open

      subroutine nc_stream_set_hru_meta(s)
        type(nc_stream), intent(inout) :: s
        integer :: j, iob
        real :: rid(s%nobj), gid(s%nobj)
        if (.not. s%on) return
        do j = 1, s%nobj
          iob = sp_ob1%hru + j - 1
          s%obj_id(j) = j
          s%gis_id(j) = ob(iob)%gis_id
          s%oname(j) = ob(iob)%name
        end do
        rid = real(s%obj_id)
        gid = real(s%gis_id)
        call nc_put_var_1d_r32_slice(s%ncid, s%vid_oid, rid, 1)
        call nc_put_var_1d_r32_slice(s%ncid, s%vid_gid, gid, 1)
      end subroutine nc_stream_set_hru_meta

      subroutine nc_stream_stage(s, iobj, arr)
        type(nc_stream), intent(inout) :: s
        integer, intent(in) :: iobj
        real, intent(in) :: arr(:)
        if (.not. s%on) return
        s%buf(iobj,1:s%nvar) = arr(1:s%nvar)
      end subroutine nc_stream_stage

      subroutine nc_stream_flush(s)
        type(nc_stream), intent(inout) :: s
        integer :: t, i
        real :: tarr(1), mo(1), dm(1), yr(1), jd(1)
        real, allocatable :: slab(:,:)
        if (.not. s%on) return
        s%time_idx = s%time_idx + 1
        t = s%time_idx
        tarr(1) = real(time%day - nc_epoch_jday)
        mo(1) = real(time%mo); dm(1) = real(time%day_mo)
        yr(1) = real(time%yrc); jd(1) = real(time%day)
        call nc_put_var_1d_r32_slice(s%ncid, s%vid_time, tarr, t)
        call nc_put_var_1d_r32_slice(s%ncid, s%vid_mo, mo, t)
        call nc_put_var_1d_r32_slice(s%ncid, s%vid_day_mo, dm, t)
        call nc_put_var_1d_r32_slice(s%ncid, s%vid_yrc, yr, t)
        call nc_put_var_1d_r32_slice(s%ncid, s%vid_jday, jd, t)
        allocate(slab(s%nobj, 1))
        do i = 1, s%nvar
          slab(:, 1) = s%buf(:, i)
          call nc_put_var_2d_r32_slice(s%ncid, s%var_id(i), slab, t)
        end do
        deallocate(slab)
        s%buf = 0.
      end subroutine nc_stream_flush

      subroutine names_wb(vn)
        character(len=32), intent(out) :: vn(:)
        vn(1)='precip'; vn(2)='snofall'; vn(3)='snomlt'; vn(4)='surq_gen'
        vn(5)='latq'; vn(6)='wateryld'; vn(7)='perc'; vn(8)='et'
        vn(9)='ecanopy'; vn(10)='eplant'; vn(11)='esoil'; vn(12)='surq_cont'
        vn(13)='cn'; vn(14)='sw_init'; vn(15)='sw_final'; vn(16)='sw'
        vn(17)='sw_300'; vn(18)='sno_init'; vn(19)='sno_final'; vn(20)='snopack'
        vn(21)='pet'; vn(22)='qtile'; vn(23)='irr'; vn(24)='surq_runon'
        vn(25)='latq_runon'; vn(26)='overbank'; vn(27)='surq_cha'; vn(28)='surq_res'
        vn(29)='surq_ls'; vn(30)='latq_cha'; vn(31)='latq_res'; vn(32)='latq_ls'
        vn(33)='gwsoil'; vn(34)='satex'; vn(35)='satex_chan'; vn(36)='delsw'
        vn(37)='lagsurf'; vn(38)='laglatq'; vn(39)='lagsatex'
        vn(40)='wet_evap'; vn(41)='wet_out'; vn(42)='wet_stor'
      end subroutine names_wb

      subroutine names_nb(vn)
        character(len=32), intent(out) :: vn(:)
        vn(1)='grazn'; vn(2)='grazp'; vn(3)='lab_min_p'; vn(4)='act_sta_p'
        vn(5)='fertn'; vn(6)='fertp'; vn(7)='fixn'; vn(8)='denit'
        vn(9)='act_nit_n'; vn(10)='act_sta_n'; vn(11)='org_lab_p'
        vn(12)='rsd_nitorg_n'; vn(13)='rsd_laborg_p'; vn(14)='no3atmo'
        vn(15)='nh4atmo'; vn(16)='nuptake'; vn(17)='puptake'
        vn(18)='gwsoiln'; vn(19)='gwsoilp'
      end subroutine names_nb

      subroutine names_ls(vn)
        character(len=32), intent(out) :: vn(:)
        vn(1)='sedyld'; vn(2)='sedorgn'; vn(3)='sedorgp'; vn(4)='surqno3'
        vn(5)='latno3'; vn(6)='surqsolp'; vn(7)='usle'; vn(8)='sedminp'
        vn(9)='tileno3'; vn(10)='lchlabp'; vn(11)='tilelabp'; vn(12)='satexn'
        vn(13)='percn'
      end subroutine names_ls

      subroutine names_pw(vn)
        character(len=32), intent(out) :: vn(:)
        vn(1)='lai'; vn(2)='bioms'; vn(3)='yield'; vn(4)='residue'
        vn(5)='sol_tmp'; vn(6)='strsw'; vn(7)='strsa'; vn(8)='strstmp'
        vn(9)='strsn'; vn(10)='strsp'; vn(11)='strss'; vn(12)='nplnt'
        vn(13)='percn'; vn(14)='pplnt'; vn(15)='tmx'; vn(16)='tmn'
        vn(17)='tmpav'; vn(18)='solrad'; vn(19)='wndspd'; vn(20)='rhum'
        vn(21)='phubase0'; vn(22)='lai_max'; vn(23)='bm_max'
        vn(24)='bm_grow'; vn(25)='c_gro'
      end subroutine names_pw

      integer function nc_tchunk_for_suffix(suffix)
        character(len=*), intent(in) :: suffix
        nc_tchunk_for_suffix = 30
        if (trim(suffix) == "mon") nc_tchunk_for_suffix = 12
        if (trim(suffix) == "yr" .or. trim(suffix) == "aa") nc_tchunk_for_suffix = 1
      end function nc_tchunk_for_suffix

      subroutine nc_open_hru_wb(s, suffix, flag)
        type(nc_stream), intent(inout) :: s
        character(len=*), intent(in) :: suffix
        character(len=1), intent(in) :: flag
        character(len=32) :: vn(nwb)
        if (.not. nc_use_output()) return
        if (flag /= "y" .or. sp_ob%hru <= 0) return
        call names_wb(vn)
        call nc_stream_open(s, "hru_wb_"//trim(suffix)//".nc", sp_ob%hru, vn, nc_tchunk_for_suffix(suffix))
        call nc_stream_set_hru_meta(s)
      end subroutine nc_open_hru_wb

      subroutine nc_open_hru_nb(s, suffix, flag)
        type(nc_stream), intent(inout) :: s
        character(len=*), intent(in) :: suffix
        character(len=1), intent(in) :: flag
        character(len=32) :: vn(nnb)
        if (.not. nc_use_output()) return
        if (flag /= "y" .or. sp_ob%hru <= 0) return
        call names_nb(vn)
        call nc_stream_open(s, "hru_nb_"//trim(suffix)//".nc", sp_ob%hru, vn, nc_tchunk_for_suffix(suffix))
        call nc_stream_set_hru_meta(s)
      end subroutine nc_open_hru_nb

      subroutine nc_open_hru_ls(s, suffix, flag)
        type(nc_stream), intent(inout) :: s
        character(len=*), intent(in) :: suffix
        character(len=1), intent(in) :: flag
        character(len=32) :: vn(nls)
        if (.not. nc_use_output()) return
        if (flag /= "y" .or. sp_ob%hru <= 0) return
        call names_ls(vn)
        call nc_stream_open(s, "hru_ls_"//trim(suffix)//".nc", sp_ob%hru, vn, nc_tchunk_for_suffix(suffix))
        call nc_stream_set_hru_meta(s)
      end subroutine nc_open_hru_ls

      subroutine nc_open_hru_carbon(s, fname, nvar, flag, tchunk)
        type(nc_stream), intent(inout) :: s
        character(len=*), intent(in) :: fname
        integer, intent(in) :: nvar, tchunk
        character(len=1), intent(in) :: flag
        character(len=32) :: vn(32)
        integer :: i
        if (.not. nc_use_output()) return
        if (flag /= "y" .or. sp_ob%hru <= 0) return
        do i = 1, nvar
          write(vn(i), '(a,i0)') "v", i
        end do
        call nc_stream_open(s, trim(fname)//".nc", sp_ob%hru, vn(1:nvar), tchunk)
        call nc_stream_set_hru_meta(s)
      end subroutine nc_open_hru_carbon

      subroutine nc_open_basin_singleton(s, fname, nvar, flag, tchunk)
        type(nc_stream), intent(inout) :: s
        character(len=*), intent(in) :: fname
        integer, intent(in) :: nvar, tchunk
        character(len=1), intent(in) :: flag
        character(len=32) :: vn(64)
        integer :: i
        if (.not. nc_use_output()) return
        if (flag /= "y") return
        do i = 1, nvar
          write(vn(i), '(a,i0)') "v", i
        end do
        call nc_stream_open(s, trim(fname)//".nc", 1, vn(1:nvar), tchunk)
        s%obj_id(1) = 1; s%gis_id(1) = 1; s%oname(1) = bsn%name
      end subroutine nc_open_basin_singleton

      subroutine nc_open_lsu_stream(s, fname, n, vn, flag, tchunk)
        type(nc_stream), intent(inout) :: s
        character(len=*), intent(in) :: fname
        integer, intent(in) :: n, tchunk
        character(len=32), intent(in) :: vn(:)
        character(len=1), intent(in) :: flag
        if (flag == "y" .and. n > 0) call nc_stream_open(s, trim(fname), n, vn, tchunk)
      end subroutine nc_open_lsu_stream

      subroutine nc_open_lsu_streams()
        character(len=32) :: vnwb(nwb), vnnb(nnb), vnls(nls), vnpw(npw)
        integer :: n
        if (.not. nc_use_output()) return
        n = db_mx%lsu_out
        if (n <= 0) return
        call names_wb(vnwb); call names_nb(vnnb); call names_ls(vnls); call names_pw(vnpw)
        call nc_open_lsu_stream(s_lsu_wb_d, "lsunit_wb_day.nc", n, vnwb, pco%wb_lsu%d, 30)
        call nc_open_lsu_stream(s_lsu_wb_m, "lsunit_wb_mon.nc", n, vnwb, pco%wb_lsu%m, 12)
        call nc_open_lsu_stream(s_lsu_wb_y, "lsunit_wb_yr.nc", n, vnwb, pco%wb_lsu%y, 1)
        call nc_open_lsu_stream(s_lsu_wb_a, "lsunit_wb_aa.nc", n, vnwb, pco%wb_lsu%a, 1)
        call nc_open_lsu_stream(s_lsu_nb_d, "lsunit_nb_day.nc", n, vnnb, pco%nb_lsu%d, 30)
        call nc_open_lsu_stream(s_lsu_nb_m, "lsunit_nb_mon.nc", n, vnnb, pco%nb_lsu%m, 12)
        call nc_open_lsu_stream(s_lsu_nb_y, "lsunit_nb_yr.nc", n, vnnb, pco%nb_lsu%y, 1)
        call nc_open_lsu_stream(s_lsu_nb_a, "lsunit_nb_aa.nc", n, vnnb, pco%nb_lsu%a, 1)
        call nc_open_lsu_stream(s_lsu_ls_d, "lsunit_ls_day.nc", n, vnls, pco%ls_lsu%d, 30)
        call nc_open_lsu_stream(s_lsu_ls_m, "lsunit_ls_mon.nc", n, vnls, pco%ls_lsu%m, 12)
        call nc_open_lsu_stream(s_lsu_ls_y, "lsunit_ls_yr.nc", n, vnls, pco%ls_lsu%y, 1)
        call nc_open_lsu_stream(s_lsu_ls_a, "lsunit_ls_aa.nc", n, vnls, pco%ls_lsu%a, 1)
        call nc_open_lsu_stream(s_lsu_pw_d, "lsunit_pw_day.nc", n, vnpw, pco%pw_lsu%d, 30)
        call nc_open_lsu_stream(s_lsu_pw_m, "lsunit_pw_mon.nc", n, vnpw, pco%pw_lsu%m, 12)
        call nc_open_lsu_stream(s_lsu_pw_y, "lsunit_pw_yr.nc", n, vnpw, pco%pw_lsu%y, 1)
        call nc_open_lsu_stream(s_lsu_pw_a, "lsunit_pw_aa.nc", n, vnpw, pco%pw_lsu%a, 1)
      end subroutine nc_open_lsu_streams

      subroutine nc_open_aquifer(s, suffix, flag)
        type(nc_stream), intent(inout) :: s
        character(len=*), intent(in) :: suffix
        character(len=1), intent(in) :: flag
        character(len=32) :: vn(naqu)
        integer :: i, iaq, iob
        if (.not. nc_use_output()) return
        if (flag /= "y" .or. sp_ob%aqu <= 0) return
        vn(1)='flo'; vn(2)='dep_wt'; vn(3)='stor'; vn(4)='rchrg'
        vn(5)='seep'; vn(6)='revap'; vn(7)='no3_st'; vn(8)='minp'
        vn(9)='cbn'; vn(10)='orgn'; vn(11)='no3_rchg'; vn(12)='no3_loss'
        vn(13)='no3_lat'; vn(14)='no3_seep'; vn(15)='flo_cha'
        vn(16)='flo_res'; vn(17)='flo_ls'; vn(18)='pad'
        call nc_stream_open(s, "aquifer_"//trim(suffix)//".nc", sp_ob%aqu, vn, nc_tchunk_for_suffix(suffix))
        do iaq = 1, sp_ob%aqu
          iob = sp_ob1%aqu + iaq - 1
          s%obj_id(iaq) = iaq
          s%gis_id(iaq) = ob(iob)%gis_id
          s%oname(iaq) = ob(iob)%name
        end do
      end subroutine nc_open_aquifer

      subroutine nc_open_channel(s, suffix, flag)
        type(nc_stream), intent(inout) :: s
        character(len=*), intent(in) :: suffix
        character(len=1), intent(in) :: flag
        character(len=32) :: vn(nch)
        integer :: i, ich, iob
        if (.not. nc_use_output()) return
        if (flag /= "y" .or. sp_ob%chan <= 0) return
        do i = 1, nch
          write(vn(i), '(a,i0)') "v", i
        end do
        call nc_stream_open(s, "channel_"//trim(suffix)//".nc", sp_ob%chan, vn, nc_tchunk_for_suffix(suffix))
        do ich = 1, sp_ob%chan
          iob = sp_ob1%chan + ich - 1
          s%obj_id(ich) = ich
          s%gis_id(ich) = ob(iob)%gis_id
          s%oname(ich) = ob(iob)%name
        end do
      end subroutine nc_open_channel

      subroutine nc_open_sd_channel(s, suffix, flag)
        type(nc_stream), intent(inout) :: s
        character(len=*), intent(in) :: suffix
        character(len=1), intent(in) :: flag
        character(len=32) :: vn(nchnsd)
        integer :: i, ich, iob
        if (.not. nc_use_output()) return
        if (flag /= "y" .or. sp_ob%chandeg <= 0) return
        do i = 1, nchnsd
          write(vn(i), '(a,i0)') "v", i
        end do
        call nc_stream_open(s, "channel_sd_"//trim(suffix)//".nc", sp_ob%chandeg, vn, nc_tchunk_for_suffix(suffix))
        do ich = 1, sp_ob%chandeg
          iob = sp_ob1%chandeg + ich - 1
          s%obj_id(ich) = ich
          s%gis_id(ich) = ob(iob)%gis_id
          s%oname(ich) = ob(iob)%name
        end do
      end subroutine nc_open_sd_channel

      subroutine nc_open_hru_pw(s, suffix, flag)
        type(nc_stream), intent(inout) :: s
        character(len=*), intent(in) :: suffix
        character(len=1), intent(in) :: flag
        character(len=32) :: vn(npw)
        if (.not. nc_use_output()) return
        if (flag /= "y" .or. sp_ob%hru <= 0) return
        call names_pw(vn)
        call nc_stream_open(s, "hru_pw_"//trim(suffix)//".nc", sp_ob%hru, vn, nc_tchunk_for_suffix(suffix))
        call nc_stream_set_hru_meta(s)
      end subroutine nc_open_hru_pw

      subroutine netcdf_output_init()
        if (.not. nc_use_output()) return
        call nc_set_deflate_from_env()
        call nc_epoch_init()
        call nc_open_hru_wb(s_hru_wb_d, "day", pco%wb_hru%d)
        call nc_open_hru_wb(s_hru_wb_m, "mon", pco%wb_hru%m)
        call nc_open_hru_wb(s_hru_wb_y, "yr", pco%wb_hru%y)
        call nc_open_hru_wb(s_hru_wb_a, "aa", pco%wb_hru%a)
        call nc_open_hru_nb(s_hru_nb_d, "day", pco%nb_hru%d)
        call nc_open_hru_nb(s_hru_nb_m, "mon", pco%nb_hru%m)
        call nc_open_hru_nb(s_hru_nb_y, "yr", pco%nb_hru%y)
        call nc_open_hru_nb(s_hru_nb_a, "aa", pco%nb_hru%a)
        call nc_open_hru_ls(s_hru_ls_d, "day", pco%ls_hru%d)
        call nc_open_hru_ls(s_hru_ls_m, "mon", pco%ls_hru%m)
        call nc_open_hru_ls(s_hru_ls_y, "yr", pco%ls_hru%y)
        call nc_open_hru_ls(s_hru_ls_a, "aa", pco%ls_hru%a)
        call nc_open_hru_pw(s_hru_pw_d, "day", pco%pw_hru%d)
        call nc_open_hru_pw(s_hru_pw_m, "mon", pco%pw_hru%m)
        call nc_open_hru_pw(s_hru_pw_y, "yr", pco%pw_hru%y)
        call nc_open_hru_pw(s_hru_pw_a, "aa", pco%pw_hru%a)
        call nc_open_hru_carbon(s_hru_sc_d, "hru_soilcarb_day", nhsc, pco%nb_hru%d, 30)
        call nc_open_hru_carbon(s_hru_rc_d, "hru_rescarb_day", nhrc, pco%nb_hru%d, 30)
        call nc_open_hru_carbon(s_hru_pc_d, "hru_plcarb_day", nhpc, pco%nb_hru%d, 30)
        call nc_open_hru_carbon(s_hru_cf_d, "hru_scf_day", nhscf, pco%nb_hru%d, 30)
        call nc_open_hru_carbon(s_hru_sc_m, "hru_soilcarb_mon", nhsc, pco%nb_hru%m, 12)
        call nc_open_hru_carbon(s_hru_rc_m, "hru_rescarb_mon", nhrc, pco%nb_hru%m, 12)
        call nc_open_hru_carbon(s_hru_pc_m, "hru_plcarb_mon", nhpc, pco%nb_hru%m, 12)
        call nc_open_hru_carbon(s_hru_cf_m, "hru_scf_mon", nhscf, pco%nb_hru%m, 12)
        call nc_open_hru_carbon(s_hru_sc_y, "hru_soilcarb_yr", nhsc, pco%nb_hru%y, 1)
        call nc_open_hru_carbon(s_hru_rc_y, "hru_rescarb_yr", nhrc, pco%nb_hru%y, 1)
        call nc_open_hru_carbon(s_hru_pc_y, "hru_plcarb_yr", nhpc, pco%nb_hru%y, 1)
        call nc_open_hru_carbon(s_hru_cf_y, "hru_scf_yr", nhscf, pco%nb_hru%y, 1)
        call nc_open_hru_carbon(s_hru_sc_a, "hru_soilcarb_aa", nhsc, pco%nb_hru%a, 1)
        call nc_open_hru_carbon(s_hru_rc_a, "hru_rescarb_aa", nhrc, pco%nb_hru%a, 1)
        call nc_open_hru_carbon(s_hru_pc_a, "hru_plcarb_aa", nhpc, pco%nb_hru%a, 1)
        call nc_open_hru_carbon(s_hru_cf_a, "hru_scf_aa", nhscf, pco%nb_hru%a, 1)
        call nc_open_basin_singleton(s_bsn_wb_d, "basin_wb_day", nwb, pco%wb_bsn%d, 30)
        call nc_open_basin_singleton(s_bsn_nb_d, "basin_nb_day", nnb, pco%nb_bsn%d, 30)
        call nc_open_basin_singleton(s_bsn_ls_d, "basin_ls_day", nls, pco%ls_bsn%d, 30)
        call nc_open_basin_singleton(s_bsn_pw_d, "basin_pw_day", npw, pco%pw_bsn%d, 30)
        call nc_open_basin_singleton(s_bsn_wb_m, "basin_wb_mon", nwb, pco%wb_bsn%m, 12)
        call nc_open_basin_singleton(s_bsn_nb_m, "basin_nb_mon", nnb, pco%nb_bsn%m, 12)
        call nc_open_basin_singleton(s_bsn_ls_m, "basin_ls_mon", nls, pco%ls_bsn%m, 12)
        call nc_open_basin_singleton(s_bsn_pw_m, "basin_pw_mon", npw, pco%pw_bsn%m, 12)
        call nc_open_basin_singleton(s_bsn_wb_y, "basin_wb_yr", nwb, pco%wb_bsn%y, 1)
        call nc_open_basin_singleton(s_bsn_nb_y, "basin_nb_yr", nnb, pco%nb_bsn%y, 1)
        call nc_open_basin_singleton(s_bsn_ls_y, "basin_ls_yr", nls, pco%ls_bsn%y, 1)
        call nc_open_basin_singleton(s_bsn_pw_y, "basin_pw_yr", npw, pco%pw_bsn%y, 1)
        call nc_open_basin_singleton(s_bsn_wb_a, "basin_wb_aa", nwb, pco%wb_bsn%a, 1)
        call nc_open_basin_singleton(s_bsn_nb_a, "basin_nb_aa", nnb, pco%nb_bsn%a, 1)
        call nc_open_basin_singleton(s_bsn_ls_a, "basin_ls_aa", nls, pco%ls_bsn%a, 1)
        call nc_open_basin_singleton(s_bsn_pw_a, "basin_pw_aa", npw, pco%pw_bsn%a, 1)
        call nc_open_basin_singleton(s_bsn_aqu_d, "basin_aqu_day", naqu, pco%aqu_bsn%d, 30)
        call nc_open_basin_singleton(s_bsn_aqu_m, "basin_aqu_mon", naqu, pco%aqu_bsn%m, 12)
        call nc_open_basin_singleton(s_bsn_aqu_y, "basin_aqu_yr", naqu, pco%aqu_bsn%y, 1)
        call nc_open_basin_singleton(s_bsn_aqu_a, "basin_aqu_aa", naqu, pco%aqu_bsn%a, 1)
        call nc_open_basin_singleton(s_bsn_cha_d, "basin_cha_day", nch, pco%chan_bsn%d, 30)
        call nc_open_basin_singleton(s_bsn_cha_m, "basin_cha_mon", nch, pco%chan_bsn%m, 12)
        call nc_open_basin_singleton(s_bsn_cha_y, "basin_cha_yr", nch, pco%chan_bsn%y, 1)
        call nc_open_basin_singleton(s_bsn_cha_a, "basin_cha_aa", nch, pco%chan_bsn%a, 1)
        call nc_open_channel(s_cha_d, "day", pco%chan%d)
        call nc_open_channel(s_cha_m, "mon", pco%chan%m)
        call nc_open_channel(s_cha_y, "yr", pco%chan%y)
        call nc_open_channel(s_cha_a, "aa", pco%chan%a)
        call nc_open_lsu_streams()
        call nc_open_aquifer(s_aqu_d, "day", pco%aqu%d)
        call nc_open_aquifer(s_aqu_m, "mon", pco%aqu%m)
        call nc_open_aquifer(s_aqu_y, "yr", pco%aqu%y)
        call nc_open_aquifer(s_aqu_a, "aa", pco%aqu%a)
        call nc_open_sd_channel(s_cha_sd_d, "day", pco%sd_chan%d)
        call nc_open_sd_channel(s_cha_sd_m, "mon", pco%sd_chan%m)
        call nc_open_sd_channel(s_cha_sd_y, "yr", pco%sd_chan%y)
        call nc_open_sd_channel(s_cha_sd_a, "aa", pco%sd_chan%a)
      end subroutine netcdf_output_init

      subroutine nc_close_stream(s)
        type(nc_stream), intent(inout) :: s
        if (s%on) call nc_close_file(s%ncid)
        s%on = .false.
      end subroutine nc_close_stream

      subroutine netcdf_output_finish()
        if (.not. nc_use_output()) return
        call nc_close_stream(s_hru_wb_d); call nc_close_stream(s_hru_wb_m)
        call nc_close_stream(s_hru_wb_y); call nc_close_stream(s_hru_wb_a)
        call nc_close_stream(s_hru_nb_d); call nc_close_stream(s_hru_nb_m)
        call nc_close_stream(s_hru_nb_y); call nc_close_stream(s_hru_nb_a)
        call nc_close_stream(s_hru_ls_d); call nc_close_stream(s_hru_ls_m)
        call nc_close_stream(s_hru_ls_y); call nc_close_stream(s_hru_ls_a)
        call nc_close_stream(s_hru_pw_d); call nc_close_stream(s_hru_pw_m)
        call nc_close_stream(s_hru_pw_y); call nc_close_stream(s_hru_pw_a)
        call nc_close_stream(s_hru_sc_d); call nc_close_stream(s_hru_rc_d)
        call nc_close_stream(s_hru_pc_d); call nc_close_stream(s_hru_cf_d)
        call nc_close_stream(s_hru_sc_m); call nc_close_stream(s_hru_rc_m)
        call nc_close_stream(s_hru_pc_m); call nc_close_stream(s_hru_cf_m)
        call nc_close_stream(s_hru_sc_y); call nc_close_stream(s_hru_rc_y)
        call nc_close_stream(s_hru_pc_y); call nc_close_stream(s_hru_cf_y)
        call nc_close_stream(s_hru_sc_a); call nc_close_stream(s_hru_rc_a)
        call nc_close_stream(s_hru_pc_a); call nc_close_stream(s_hru_cf_a)
        call nc_close_stream(s_bsn_wb_d); call nc_close_stream(s_bsn_nb_d)
        call nc_close_stream(s_bsn_ls_d); call nc_close_stream(s_bsn_pw_d)
        call nc_close_stream(s_bsn_wb_m); call nc_close_stream(s_bsn_nb_m)
        call nc_close_stream(s_bsn_ls_m); call nc_close_stream(s_bsn_pw_m)
        call nc_close_stream(s_bsn_wb_y); call nc_close_stream(s_bsn_nb_y)
        call nc_close_stream(s_bsn_ls_y); call nc_close_stream(s_bsn_pw_y)
        call nc_close_stream(s_bsn_wb_a); call nc_close_stream(s_bsn_nb_a)
        call nc_close_stream(s_bsn_ls_a); call nc_close_stream(s_bsn_pw_a)
        call nc_close_stream(s_lsu_wb_d); call nc_close_stream(s_lsu_nb_d)
        call nc_close_stream(s_lsu_ls_d); call nc_close_stream(s_lsu_pw_d)
        call nc_close_stream(s_lsu_wb_m); call nc_close_stream(s_lsu_nb_m)
        call nc_close_stream(s_lsu_ls_m); call nc_close_stream(s_lsu_pw_m)
        call nc_close_stream(s_lsu_wb_y); call nc_close_stream(s_lsu_nb_y)
        call nc_close_stream(s_lsu_ls_y); call nc_close_stream(s_lsu_pw_y)
        call nc_close_stream(s_lsu_wb_a); call nc_close_stream(s_lsu_nb_a)
        call nc_close_stream(s_lsu_ls_a); call nc_close_stream(s_lsu_pw_a)
        call nc_close_stream(s_aqu_d); call nc_close_stream(s_aqu_m)
        call nc_close_stream(s_aqu_y); call nc_close_stream(s_aqu_a)
        call nc_close_stream(s_cha_sd_d); call nc_close_stream(s_cha_sd_m)
        call nc_close_stream(s_cha_sd_y); call nc_close_stream(s_cha_sd_a)
        call nc_close_stream(s_bsn_aqu_d); call nc_close_stream(s_bsn_aqu_m)
        call nc_close_stream(s_bsn_aqu_y); call nc_close_stream(s_bsn_aqu_a)
        call nc_close_stream(s_bsn_cha_d); call nc_close_stream(s_bsn_cha_m)
        call nc_close_stream(s_bsn_cha_y); call nc_close_stream(s_bsn_cha_a)
        call nc_close_stream(s_cha_d); call nc_close_stream(s_cha_m)
        call nc_close_stream(s_cha_y); call nc_close_stream(s_cha_a)
      end subroutine netcdf_output_finish

      subroutine nc_stage_hru_wb(ihru, wb)
        integer, intent(in) :: ihru
        type(output_waterbal), intent(in) :: wb
        real :: a(nwb)
        if (.not. nc_use_output()) return
        call wb_pack(wb, a)
        if (s_hru_wb_d%on) call nc_stream_stage(s_hru_wb_d, ihru, a)
      end subroutine nc_stage_hru_wb

      subroutine nc_stage_hru_nb(ihru, nb)
        integer, intent(in) :: ihru
        type(output_nutbal), intent(in) :: nb
        real :: a(nnb)
        if (.not. nc_use_output()) return
        call nb_pack(nb, a)
        if (s_hru_nb_d%on) call nc_stream_stage(s_hru_nb_d, ihru, a)
      end subroutine nc_stage_hru_nb

      subroutine nc_stage_hru_ls(ihru, ls, percn)
        integer, intent(in) :: ihru
        type(output_losses), intent(in) :: ls
        real, intent(in) :: percn
        real :: a(nls)
        if (.not. nc_use_output()) return
        call ls_pack(ls, a, percn)
        if (s_hru_ls_d%on) call nc_stream_stage(s_hru_ls_d, ihru, a)
      end subroutine nc_stage_hru_ls

      subroutine nc_stage_hru_ls_extra(ihru, percn)
        integer, intent(in) :: ihru
        real, intent(in) :: percn
        if (.not. nc_use_output()) return
        if (s_hru_ls_d%on) s_hru_ls_d%buf(ihru, nls) = percn
      end subroutine nc_stage_hru_ls_extra

      subroutine nc_stage_hru_pw(ihru, pw)
        integer, intent(in) :: ihru
        type(output_plantweather), intent(in) :: pw
        real :: a(npw)
        if (.not. nc_use_output()) return
        call pw_pack(pw, a)
        if (s_hru_pw_d%on) call nc_stream_stage(s_hru_pw_d, ihru, a)
      end subroutine nc_stage_hru_pw

      subroutine nc_stage_hru_wb_mon(ihru, wb)
        integer, intent(in) :: ihru
        type(output_waterbal), intent(in) :: wb
        real :: a(nwb)
        if (s_hru_wb_m%on) then
          call wb_pack(wb, a)
          call nc_stream_stage(s_hru_wb_m, ihru, a)
        end if
      end subroutine nc_stage_hru_wb_mon

      subroutine nc_stage_hru_nb_mon(ihru, nb)
        integer, intent(in) :: ihru
        type(output_nutbal), intent(in) :: nb
        real :: a(nnb)
        if (s_hru_nb_m%on) then
          call nb_pack(nb, a)
          call nc_stream_stage(s_hru_nb_m, ihru, a)
        end if
      end subroutine nc_stage_hru_nb_mon

      subroutine nc_stage_hru_ls_mon(ihru, ls, percn)
        integer, intent(in) :: ihru
        type(output_losses), intent(in) :: ls
        real, intent(in) :: percn
        real :: a(nls)
        if (s_hru_ls_m%on) then
          call ls_pack(ls, a, percn)
          call nc_stream_stage(s_hru_ls_m, ihru, a)
        end if
      end subroutine nc_stage_hru_ls_mon

      subroutine nc_stage_hru_pw_mon(ihru, pw)
        integer, intent(in) :: ihru
        type(output_plantweather), intent(in) :: pw
        real :: a(npw)
        if (s_hru_pw_m%on) then
          call pw_pack(pw, a)
          call nc_stream_stage(s_hru_pw_m, ihru, a)
        end if
      end subroutine nc_stage_hru_pw_mon

      subroutine nc_stage_hru_wb_yr(ihru, wb)
        integer, intent(in) :: ihru
        type(output_waterbal), intent(in) :: wb
        real :: a(nwb)
        if (s_hru_wb_y%on) then; call wb_pack(wb, a); call nc_stream_stage(s_hru_wb_y, ihru, a); end if
      end subroutine nc_stage_hru_wb_yr

      subroutine nc_stage_hru_nb_yr(ihru, nb)
        integer, intent(in) :: ihru
        type(output_nutbal), intent(in) :: nb
        real :: a(nnb)
        if (s_hru_nb_y%on) then; call nb_pack(nb, a); call nc_stream_stage(s_hru_nb_y, ihru, a); end if
      end subroutine nc_stage_hru_nb_yr

      subroutine nc_stage_hru_ls_yr(ihru, ls, percn)
        integer, intent(in) :: ihru
        type(output_losses), intent(in) :: ls
        real, intent(in) :: percn
        real :: a(nls)
        if (s_hru_ls_y%on) then; call ls_pack(ls, a, percn); call nc_stream_stage(s_hru_ls_y, ihru, a); end if
      end subroutine nc_stage_hru_ls_yr

      subroutine nc_stage_hru_pw_yr(ihru, pw)
        integer, intent(in) :: ihru
        type(output_plantweather), intent(in) :: pw
        real :: a(npw)
        if (s_hru_pw_y%on) then; call pw_pack(pw, a); call nc_stream_stage(s_hru_pw_y, ihru, a); end if
      end subroutine nc_stage_hru_pw_yr

      subroutine nc_stage_hru_wb_aa(ihru, wb)
        integer, intent(in) :: ihru
        type(output_waterbal), intent(in) :: wb
        real :: a(nwb)
        if (s_hru_wb_a%on) then; call wb_pack(wb, a); call nc_stream_stage(s_hru_wb_a, ihru, a); end if
      end subroutine nc_stage_hru_wb_aa

      subroutine nc_stage_hru_nb_aa(ihru, nb)
        integer, intent(in) :: ihru
        type(output_nutbal), intent(in) :: nb
        real :: a(nnb)
        if (s_hru_nb_a%on) then; call nb_pack(nb, a); call nc_stream_stage(s_hru_nb_a, ihru, a); end if
      end subroutine nc_stage_hru_nb_aa

      subroutine nc_stage_hru_ls_aa(ihru, ls, percn)
        integer, intent(in) :: ihru
        type(output_losses), intent(in) :: ls
        real, intent(in) :: percn
        real :: a(nls)
        if (s_hru_ls_a%on) then; call ls_pack(ls, a, percn); call nc_stream_stage(s_hru_ls_a, ihru, a); end if
      end subroutine nc_stage_hru_ls_aa

      subroutine nc_stage_hru_pw_aa(ihru, pw)
        integer, intent(in) :: ihru
        type(output_plantweather), intent(in) :: pw
        real :: a(npw)
        if (s_hru_pw_a%on) then; call pw_pack(pw, a); call nc_stream_stage(s_hru_pw_a, ihru, a); end if
      end subroutine nc_stage_hru_pw_aa

      subroutine nc_stage_hru_carbon_mon(ihru, sc, rc, pc, cf)
        integer, intent(in) :: ihru
        type(carbon_soil_gain_losses), intent(in) :: sc
        type(carbon_residue_gain_losses), intent(in) :: rc
        type(carbon_plant_gain_losses), intent(in) :: pc
        type(carbon_soil_transformations), intent(in) :: cf
        real :: a(nhsc), b(nhrc), c(nhpc), d(nhscf)
        call hsc_pack(sc, a); call hrc_pack(rc, b)
        call hpc_pack(pc, c); call hscf_pack(cf, d)
        if (s_hru_sc_m%on) call nc_stream_stage(s_hru_sc_m, ihru, a)
        if (s_hru_rc_m%on) call nc_stream_stage(s_hru_rc_m, ihru, b)
        if (s_hru_pc_m%on) call nc_stream_stage(s_hru_pc_m, ihru, c)
        if (s_hru_cf_m%on) call nc_stream_stage(s_hru_cf_m, ihru, d)
      end subroutine nc_stage_hru_carbon_mon

      subroutine nc_stage_hru_carbon_yr(ihru, sc, rc, pc, cf)
        integer, intent(in) :: ihru
        type(carbon_soil_gain_losses), intent(in) :: sc
        type(carbon_residue_gain_losses), intent(in) :: rc
        type(carbon_plant_gain_losses), intent(in) :: pc
        type(carbon_soil_transformations), intent(in) :: cf
        real :: a(nhsc), b(nhrc), c(nhpc), d(nhscf)
        call hsc_pack(sc, a); call hrc_pack(rc, b)
        call hpc_pack(pc, c); call hscf_pack(cf, d)
        if (s_hru_sc_y%on) call nc_stream_stage(s_hru_sc_y, ihru, a)
        if (s_hru_rc_y%on) call nc_stream_stage(s_hru_rc_y, ihru, b)
        if (s_hru_pc_y%on) call nc_stream_stage(s_hru_pc_y, ihru, c)
        if (s_hru_cf_y%on) call nc_stream_stage(s_hru_cf_y, ihru, d)
      end subroutine nc_stage_hru_carbon_yr

      subroutine nc_stage_hru_carbon_aa(ihru, sc, rc, pc, cf)
        integer, intent(in) :: ihru
        type(carbon_soil_gain_losses), intent(in) :: sc
        type(carbon_residue_gain_losses), intent(in) :: rc
        type(carbon_plant_gain_losses), intent(in) :: pc
        type(carbon_soil_transformations), intent(in) :: cf
        real :: a(nhsc), b(nhrc), c(nhpc), d(nhscf)
        call hsc_pack(sc, a); call hrc_pack(rc, b)
        call hpc_pack(pc, c); call hscf_pack(cf, d)
        if (s_hru_sc_a%on) call nc_stream_stage(s_hru_sc_a, ihru, a)
        if (s_hru_rc_a%on) call nc_stream_stage(s_hru_rc_a, ihru, b)
        if (s_hru_pc_a%on) call nc_stream_stage(s_hru_pc_a, ihru, c)
        if (s_hru_cf_a%on) call nc_stream_stage(s_hru_cf_a, ihru, d)
      end subroutine nc_stage_hru_carbon_aa

      subroutine nc_flush_daily_hru()
        if (s_hru_wb_d%on) call nc_stream_flush(s_hru_wb_d)
        if (s_hru_nb_d%on) call nc_stream_flush(s_hru_nb_d)
        if (s_hru_ls_d%on) call nc_stream_flush(s_hru_ls_d)
        if (s_hru_pw_d%on) call nc_stream_flush(s_hru_pw_d)
        if (s_hru_sc_d%on) call nc_stream_flush(s_hru_sc_d)
        if (s_hru_rc_d%on) call nc_stream_flush(s_hru_rc_d)
        if (s_hru_pc_d%on) call nc_stream_flush(s_hru_pc_d)
        if (s_hru_cf_d%on) call nc_stream_flush(s_hru_cf_d)
      end subroutine nc_flush_daily_hru

      subroutine nc_flush_monthly_hru()
        if (s_hru_wb_m%on) call nc_stream_flush(s_hru_wb_m)
        if (s_hru_nb_m%on) call nc_stream_flush(s_hru_nb_m)
        if (s_hru_ls_m%on) call nc_stream_flush(s_hru_ls_m)
        if (s_hru_pw_m%on) call nc_stream_flush(s_hru_pw_m)
        if (s_hru_sc_m%on) call nc_stream_flush(s_hru_sc_m)
        if (s_hru_rc_m%on) call nc_stream_flush(s_hru_rc_m)
        if (s_hru_pc_m%on) call nc_stream_flush(s_hru_pc_m)
        if (s_hru_cf_m%on) call nc_stream_flush(s_hru_cf_m)
      end subroutine nc_flush_monthly_hru

      subroutine nc_flush_yearly_hru()
        if (s_hru_wb_y%on) call nc_stream_flush(s_hru_wb_y)
        if (s_hru_nb_y%on) call nc_stream_flush(s_hru_nb_y)
        if (s_hru_ls_y%on) call nc_stream_flush(s_hru_ls_y)
        if (s_hru_pw_y%on) call nc_stream_flush(s_hru_pw_y)
        if (s_hru_sc_y%on) call nc_stream_flush(s_hru_sc_y)
        if (s_hru_rc_y%on) call nc_stream_flush(s_hru_rc_y)
        if (s_hru_pc_y%on) call nc_stream_flush(s_hru_pc_y)
        if (s_hru_cf_y%on) call nc_stream_flush(s_hru_cf_y)
      end subroutine nc_flush_yearly_hru

      subroutine nc_flush_aa_hru()
        if (s_hru_wb_a%on) call nc_stream_flush(s_hru_wb_a)
        if (s_hru_nb_a%on) call nc_stream_flush(s_hru_nb_a)
        if (s_hru_ls_a%on) call nc_stream_flush(s_hru_ls_a)
        if (s_hru_pw_a%on) call nc_stream_flush(s_hru_pw_a)
        if (s_hru_sc_a%on) call nc_stream_flush(s_hru_sc_a)
        if (s_hru_rc_a%on) call nc_stream_flush(s_hru_rc_a)
        if (s_hru_pc_a%on) call nc_stream_flush(s_hru_pc_a)
        if (s_hru_cf_a%on) call nc_stream_flush(s_hru_cf_a)
      end subroutine nc_flush_aa_hru

      ! stubs for basin/lsu/aqu/sd — expanded below in same file continuation
      subroutine hscf_pack(x, arr)
        type(carbon_soil_transformations), intent(in) :: x
        real, intent(out) :: arr(nhscf)
        arr(1)=x%meta_micr; arr(2)=x%str_micr; arr(3)=x%str_hs; arr(4)=x%co2_meta
        arr(5)=x%co2_str; arr(6)=x%micr_hs; arr(7)=x%micr_hp; arr(8)=x%hs_micr
        arr(9)=x%hs_hp; arr(10)=x%hp_micr; arr(11)=x%co2_micr; arr(12)=x%co2_hs; arr(13)=x%co2_hp
      end subroutine hscf_pack

      subroutine hpc_pack(x, arr)
        type(carbon_plant_gain_losses), intent(in) :: x
        real, intent(out) :: arr(nhpc)
        arr(1)=x%npp_c; arr(2)=x%harv_abgr_c; arr(3)=x%harv_root_c
        arr(4)=x%drop_c; arr(5)=x%grazeat_c; arr(6)=x%emit_c
      end subroutine hpc_pack

      subroutine nc_stage_hru_carbon(ihru, sc, rc, pc, cf)
        integer, intent(in) :: ihru
        type(carbon_soil_gain_losses), intent(in) :: sc
        type(carbon_residue_gain_losses), intent(in) :: rc
        type(carbon_plant_gain_losses), intent(in) :: pc
        type(carbon_soil_transformations), intent(in) :: cf
        real :: a(nhsc), b(nhrc), c(nhpc), d(nhscf)
        if (.not. nc_use_output()) return
        call hsc_pack(sc, a); call hrc_pack(rc, b)
        call hpc_pack(pc, c); call hscf_pack(cf, d)
        if (s_hru_sc_d%on) call nc_stream_stage(s_hru_sc_d, ihru, a)
        if (s_hru_rc_d%on) call nc_stream_stage(s_hru_rc_d, ihru, b)
        if (s_hru_pc_d%on) call nc_stream_stage(s_hru_pc_d, ihru, c)
        if (s_hru_cf_d%on) call nc_stream_stage(s_hru_cf_d, ihru, d)
      end subroutine nc_stage_hru_carbon

      subroutine nc_stage_basin_wb(wb)
        type(output_waterbal), intent(in) :: wb
        real :: a(nwb)
        if (s_bsn_wb_d%on) then; call wb_pack(wb, a); call nc_stream_stage(s_bsn_wb_d, 1, a); end if
      end subroutine nc_stage_basin_wb

      subroutine nc_stage_basin_nb(nb)
        type(output_nutbal), intent(in) :: nb
        real :: a(nnb)
        if (s_bsn_nb_d%on) then; call nb_pack(nb, a); call nc_stream_stage(s_bsn_nb_d, 1, a); end if
      end subroutine nc_stage_basin_nb

      subroutine nc_stage_basin_ls(ls)
        type(output_losses), intent(in) :: ls
        real :: a(nls)
        if (s_bsn_ls_d%on) then; call ls_pack(ls, a, 0.); call nc_stream_stage(s_bsn_ls_d, 1, a); end if
      end subroutine nc_stage_basin_ls

      subroutine nc_stage_basin_pw(pw)
        type(output_plantweather), intent(in) :: pw
        real :: a(npw)
        if (s_bsn_pw_d%on) then; call pw_pack(pw, a); call nc_stream_stage(s_bsn_pw_d, 1, a); end if
      end subroutine nc_stage_basin_pw

      subroutine nc_stage_lsu_wb(i, wb)
        integer, intent(in) :: i
        type(output_waterbal), intent(in) :: wb
        real :: a(nwb)
        if (s_lsu_wb_d%on) then; call wb_pack(wb, a); call nc_stream_stage(s_lsu_wb_d, i, a); end if
      end subroutine nc_stage_lsu_wb

      subroutine nc_stage_lsu_nb(i, nb)
        integer, intent(in) :: i
        type(output_nutbal), intent(in) :: nb
        real :: a(nnb)
        if (s_lsu_nb_d%on) then; call nb_pack(nb, a); call nc_stream_stage(s_lsu_nb_d, i, a); end if
      end subroutine nc_stage_lsu_nb

      subroutine nc_stage_lsu_ls(i, ls)
        integer, intent(in) :: i
        type(output_losses), intent(in) :: ls
        real :: a(nls)
        if (s_lsu_ls_d%on) then; call ls_pack(ls, a, 0.); call nc_stream_stage(s_lsu_ls_d, i, a); end if
      end subroutine nc_stage_lsu_ls

      subroutine nc_stage_lsu_pw(i, pw)
        integer, intent(in) :: i
        type(output_plantweather), intent(in) :: pw
        real :: a(npw)
        if (s_lsu_pw_d%on) then; call pw_pack(pw, a); call nc_stream_stage(s_lsu_pw_d, i, a); end if
      end subroutine nc_stage_lsu_pw

      subroutine nc_stage_aquifer(i, a)
        integer, intent(in) :: i
        type(aquifer_dynamic), intent(in) :: a
        real :: arr(naqu)
        if (s_aqu_d%on) then; call aqu_pack(a, arr); call nc_stream_stage(s_aqu_d, i, arr); end if
      end subroutine nc_stage_aquifer

      subroutine nc_stage_sd_channel(i, wb, stor, hin, hout, t)
        integer, intent(in) :: i
        type(water_body), intent(in) :: wb
        type(hyd_output), intent(in) :: stor, hin, hout
        real, intent(in) :: t
        real :: arr(nchnsd)
        if (s_cha_sd_d%on) then
          call chn_sd_pack(wb, stor, hin, hout, t, arr)
          call nc_stream_stage(s_cha_sd_d, i, arr)
        end if
      end subroutine nc_stage_sd_channel

      subroutine nc_flush_daily_basin()
        if (s_bsn_wb_d%on) call nc_stream_flush(s_bsn_wb_d)
        if (s_bsn_nb_d%on) call nc_stream_flush(s_bsn_nb_d)
        if (s_bsn_ls_d%on) call nc_stream_flush(s_bsn_ls_d)
        if (s_bsn_pw_d%on) call nc_stream_flush(s_bsn_pw_d)
        if (s_bsn_aqu_d%on) call nc_stream_flush(s_bsn_aqu_d)
        if (s_bsn_cha_d%on) call nc_stream_flush(s_bsn_cha_d)
      end subroutine nc_flush_daily_basin

      subroutine nc_flush_daily_lsu()
        if (s_lsu_wb_d%on) call nc_stream_flush(s_lsu_wb_d)
        if (s_lsu_nb_d%on) call nc_stream_flush(s_lsu_nb_d)
        if (s_lsu_ls_d%on) call nc_stream_flush(s_lsu_ls_d)
        if (s_lsu_pw_d%on) call nc_stream_flush(s_lsu_pw_d)
      end subroutine nc_flush_daily_lsu

      subroutine nc_flush_daily_aqu()
        if (s_aqu_d%on) call nc_stream_flush(s_aqu_d)
      end subroutine nc_flush_daily_aqu

      subroutine nc_flush_daily_sd()
        if (s_cha_sd_d%on) call nc_stream_flush(s_cha_sd_d)
      end subroutine nc_flush_daily_sd

      subroutine nc_stage_basin_wb_to(s, wb)
        type(nc_stream), intent(inout) :: s
        type(output_waterbal), intent(in) :: wb
        real :: a(nwb)
        if (s%on) then; call wb_pack(wb, a); call nc_stream_stage(s, 1, a); end if
      end subroutine nc_stage_basin_wb_to

      subroutine nc_stage_basin_nb_to(s, nb)
        type(nc_stream), intent(inout) :: s
        type(output_nutbal), intent(in) :: nb
        real :: a(nnb)
        if (s%on) then; call nb_pack(nb, a); call nc_stream_stage(s, 1, a); end if
      end subroutine nc_stage_basin_nb_to

      subroutine nc_stage_basin_ls_to(s, ls)
        type(nc_stream), intent(inout) :: s
        type(output_losses), intent(in) :: ls
        real :: a(nls)
        if (s%on) then; call ls_pack(ls, a, 0.); call nc_stream_stage(s, 1, a); end if
      end subroutine nc_stage_basin_ls_to

      subroutine nc_stage_basin_pw_to(s, pw)
        type(nc_stream), intent(inout) :: s
        type(output_plantweather), intent(in) :: pw
        real :: a(npw)
        if (s%on) then; call pw_pack(pw, a); call nc_stream_stage(s, 1, a); end if
      end subroutine nc_stage_basin_pw_to

      subroutine nc_stage_lsu_wb_to(s, i, wb)
        type(nc_stream), intent(inout) :: s
        integer, intent(in) :: i
        type(output_waterbal), intent(in) :: wb
        real :: a(nwb)
        if (s%on) then; call wb_pack(wb, a); call nc_stream_stage(s, i, a); end if
      end subroutine nc_stage_lsu_wb_to

      subroutine nc_stage_lsu_nb_to(s, i, nb)
        type(nc_stream), intent(inout) :: s
        integer, intent(in) :: i
        type(output_nutbal), intent(in) :: nb
        real :: a(nnb)
        if (s%on) then; call nb_pack(nb, a); call nc_stream_stage(s, i, a); end if
      end subroutine nc_stage_lsu_nb_to

      subroutine nc_stage_lsu_ls_to(s, i, ls)
        type(nc_stream), intent(inout) :: s
        integer, intent(in) :: i
        type(output_losses), intent(in) :: ls
        real :: a(nls)
        if (s%on) then; call ls_pack(ls, a, 0.); call nc_stream_stage(s, i, a); end if
      end subroutine nc_stage_lsu_ls_to

      subroutine nc_stage_lsu_pw_to(s, i, pw)
        type(nc_stream), intent(inout) :: s
        integer, intent(in) :: i
        type(output_plantweather), intent(in) :: pw
        real :: a(npw)
        if (s%on) then; call pw_pack(pw, a); call nc_stream_stage(s, i, a); end if
      end subroutine nc_stage_lsu_pw_to

      subroutine nc_stage_aquifer_to(s, i, a)
        type(nc_stream), intent(inout) :: s
        integer, intent(in) :: i
        type(aquifer_dynamic), intent(in) :: a
        real :: arr(naqu)
        if (s%on) then; call aqu_pack(a, arr); call nc_stream_stage(s, i, arr); end if
      end subroutine nc_stage_aquifer_to

      subroutine nc_stage_sd_to(s, i, wb, stor, hin, hout, t)
        type(nc_stream), intent(inout) :: s
        integer, intent(in) :: i
        type(water_body), intent(in) :: wb
        type(hyd_output), intent(in) :: stor, hin, hout
        real, intent(in) :: t
        real :: arr(nchnsd)
        if (s%on) then; call chn_sd_pack(wb, stor, hin, hout, t, arr); call nc_stream_stage(s, i, arr); end if
      end subroutine nc_stage_sd_to

      subroutine nc_flush_monthly_basin()
        if (s_bsn_wb_m%on) call nc_stream_flush(s_bsn_wb_m)
        if (s_bsn_nb_m%on) call nc_stream_flush(s_bsn_nb_m)
        if (s_bsn_ls_m%on) call nc_stream_flush(s_bsn_ls_m)
        if (s_bsn_pw_m%on) call nc_stream_flush(s_bsn_pw_m)
        if (s_bsn_aqu_m%on) call nc_stream_flush(s_bsn_aqu_m)
        if (s_bsn_cha_m%on) call nc_stream_flush(s_bsn_cha_m)
      end subroutine nc_flush_monthly_basin

      subroutine nc_flush_monthly_lsu()
        if (s_lsu_wb_m%on) call nc_stream_flush(s_lsu_wb_m)
        if (s_lsu_nb_m%on) call nc_stream_flush(s_lsu_nb_m)
        if (s_lsu_ls_m%on) call nc_stream_flush(s_lsu_ls_m)
        if (s_lsu_pw_m%on) call nc_stream_flush(s_lsu_pw_m)
      end subroutine nc_flush_monthly_lsu

      subroutine nc_flush_monthly_aqu()
        if (s_aqu_m%on) call nc_stream_flush(s_aqu_m)
      end subroutine nc_flush_monthly_aqu

      subroutine nc_flush_monthly_sd()
        if (s_cha_sd_m%on) call nc_stream_flush(s_cha_sd_m)
      end subroutine nc_flush_monthly_sd

      subroutine nc_flush_yearly_basin()
        if (s_bsn_wb_y%on) call nc_stream_flush(s_bsn_wb_y)
        if (s_bsn_nb_y%on) call nc_stream_flush(s_bsn_nb_y)
        if (s_bsn_ls_y%on) call nc_stream_flush(s_bsn_ls_y)
        if (s_bsn_pw_y%on) call nc_stream_flush(s_bsn_pw_y)
        if (s_bsn_aqu_y%on) call nc_stream_flush(s_bsn_aqu_y)
        if (s_bsn_cha_y%on) call nc_stream_flush(s_bsn_cha_y)
      end subroutine nc_flush_yearly_basin

      subroutine nc_flush_yearly_lsu()
        if (s_lsu_wb_y%on) call nc_stream_flush(s_lsu_wb_y)
        if (s_lsu_nb_y%on) call nc_stream_flush(s_lsu_nb_y)
        if (s_lsu_ls_y%on) call nc_stream_flush(s_lsu_ls_y)
        if (s_lsu_pw_y%on) call nc_stream_flush(s_lsu_pw_y)
      end subroutine nc_flush_yearly_lsu

      subroutine nc_flush_yearly_aqu()
        if (s_aqu_y%on) call nc_stream_flush(s_aqu_y)
      end subroutine nc_flush_yearly_aqu

      subroutine nc_flush_yearly_sd()
        if (s_cha_sd_y%on) call nc_stream_flush(s_cha_sd_y)
      end subroutine nc_flush_yearly_sd

      subroutine nc_flush_aa_basin()
        if (s_bsn_wb_a%on) call nc_stream_flush(s_bsn_wb_a)
        if (s_bsn_nb_a%on) call nc_stream_flush(s_bsn_nb_a)
        if (s_bsn_ls_a%on) call nc_stream_flush(s_bsn_ls_a)
        if (s_bsn_pw_a%on) call nc_stream_flush(s_bsn_pw_a)
        if (s_bsn_aqu_a%on) call nc_stream_flush(s_bsn_aqu_a)
        if (s_bsn_cha_a%on) call nc_stream_flush(s_bsn_cha_a)
      end subroutine nc_flush_aa_basin

      subroutine nc_flush_aa_lsu()
        if (s_lsu_wb_a%on) call nc_stream_flush(s_lsu_wb_a)
        if (s_lsu_nb_a%on) call nc_stream_flush(s_lsu_nb_a)
        if (s_lsu_ls_a%on) call nc_stream_flush(s_lsu_ls_a)
        if (s_lsu_pw_a%on) call nc_stream_flush(s_lsu_pw_a)
      end subroutine nc_flush_aa_lsu

      subroutine nc_flush_aa_aqu()
        if (s_aqu_a%on) call nc_stream_flush(s_aqu_a)
      end subroutine nc_flush_aa_aqu

      subroutine nc_flush_aa_sd()
        if (s_cha_sd_a%on) call nc_stream_flush(s_cha_sd_a)
      end subroutine nc_flush_aa_sd

      subroutine nc_stage_basin_wb_mon(wb)
        type(output_waterbal), intent(in) :: wb
        call nc_stage_basin_wb_to(s_bsn_wb_m, wb)
      end subroutine nc_stage_basin_wb_mon

      subroutine nc_stage_basin_nb_mon(nb)
        type(output_nutbal), intent(in) :: nb
        call nc_stage_basin_nb_to(s_bsn_nb_m, nb)
      end subroutine nc_stage_basin_nb_mon

      subroutine nc_stage_basin_ls_mon(ls)
        type(output_losses), intent(in) :: ls
        call nc_stage_basin_ls_to(s_bsn_ls_m, ls)
      end subroutine nc_stage_basin_ls_mon

      subroutine nc_stage_basin_pw_mon(pw)
        type(output_plantweather), intent(in) :: pw
        call nc_stage_basin_pw_to(s_bsn_pw_m, pw)
      end subroutine nc_stage_basin_pw_mon

      subroutine nc_stage_basin_wb_yr(wb)
        type(output_waterbal), intent(in) :: wb
        call nc_stage_basin_wb_to(s_bsn_wb_y, wb)
      end subroutine nc_stage_basin_wb_yr

      subroutine nc_stage_basin_nb_yr(nb)
        type(output_nutbal), intent(in) :: nb
        call nc_stage_basin_nb_to(s_bsn_nb_y, nb)
      end subroutine nc_stage_basin_nb_yr

      subroutine nc_stage_basin_ls_yr(ls)
        type(output_losses), intent(in) :: ls
        call nc_stage_basin_ls_to(s_bsn_ls_y, ls)
      end subroutine nc_stage_basin_ls_yr

      subroutine nc_stage_basin_pw_yr(pw)
        type(output_plantweather), intent(in) :: pw
        call nc_stage_basin_pw_to(s_bsn_pw_y, pw)
      end subroutine nc_stage_basin_pw_yr

      subroutine nc_stage_basin_wb_aa(wb)
        type(output_waterbal), intent(in) :: wb
        call nc_stage_basin_wb_to(s_bsn_wb_a, wb)
      end subroutine nc_stage_basin_wb_aa

      subroutine nc_stage_basin_nb_aa(nb)
        type(output_nutbal), intent(in) :: nb
        call nc_stage_basin_nb_to(s_bsn_nb_a, nb)
      end subroutine nc_stage_basin_nb_aa

      subroutine nc_stage_basin_ls_aa(ls)
        type(output_losses), intent(in) :: ls
        call nc_stage_basin_ls_to(s_bsn_ls_a, ls)
      end subroutine nc_stage_basin_ls_aa

      subroutine nc_stage_basin_pw_aa(pw)
        type(output_plantweather), intent(in) :: pw
        call nc_stage_basin_pw_to(s_bsn_pw_a, pw)
      end subroutine nc_stage_basin_pw_aa

      subroutine nc_stage_lsu_wb_mon(i, wb)
        integer, intent(in) :: i
        type(output_waterbal), intent(in) :: wb
        call nc_stage_lsu_wb_to(s_lsu_wb_m, i, wb)
      end subroutine nc_stage_lsu_wb_mon

      subroutine nc_stage_lsu_nb_mon(i, nb)
        integer, intent(in) :: i
        type(output_nutbal), intent(in) :: nb
        call nc_stage_lsu_nb_to(s_lsu_nb_m, i, nb)
      end subroutine nc_stage_lsu_nb_mon

      subroutine nc_stage_lsu_ls_mon(i, ls)
        integer, intent(in) :: i
        type(output_losses), intent(in) :: ls
        call nc_stage_lsu_ls_to(s_lsu_ls_m, i, ls)
      end subroutine nc_stage_lsu_ls_mon

      subroutine nc_stage_lsu_pw_mon(i, pw)
        integer, intent(in) :: i
        type(output_plantweather), intent(in) :: pw
        call nc_stage_lsu_pw_to(s_lsu_pw_m, i, pw)
      end subroutine nc_stage_lsu_pw_mon

      subroutine nc_stage_lsu_wb_yr(i, wb)
        integer, intent(in) :: i
        type(output_waterbal), intent(in) :: wb
        call nc_stage_lsu_wb_to(s_lsu_wb_y, i, wb)
      end subroutine nc_stage_lsu_wb_yr

      subroutine nc_stage_lsu_nb_yr(i, nb)
        integer, intent(in) :: i
        type(output_nutbal), intent(in) :: nb
        call nc_stage_lsu_nb_to(s_lsu_nb_y, i, nb)
      end subroutine nc_stage_lsu_nb_yr

      subroutine nc_stage_lsu_ls_yr(i, ls)
        integer, intent(in) :: i
        type(output_losses), intent(in) :: ls
        call nc_stage_lsu_ls_to(s_lsu_ls_y, i, ls)
      end subroutine nc_stage_lsu_ls_yr

      subroutine nc_stage_lsu_pw_yr(i, pw)
        integer, intent(in) :: i
        type(output_plantweather), intent(in) :: pw
        call nc_stage_lsu_pw_to(s_lsu_pw_y, i, pw)
      end subroutine nc_stage_lsu_pw_yr

      subroutine nc_stage_lsu_wb_aa(i, wb)
        integer, intent(in) :: i
        type(output_waterbal), intent(in) :: wb
        call nc_stage_lsu_wb_to(s_lsu_wb_a, i, wb)
      end subroutine nc_stage_lsu_wb_aa

      subroutine nc_stage_lsu_nb_aa(i, nb)
        integer, intent(in) :: i
        type(output_nutbal), intent(in) :: nb
        call nc_stage_lsu_nb_to(s_lsu_nb_a, i, nb)
      end subroutine nc_stage_lsu_nb_aa

      subroutine nc_stage_lsu_ls_aa(i, ls)
        integer, intent(in) :: i
        type(output_losses), intent(in) :: ls
        call nc_stage_lsu_ls_to(s_lsu_ls_a, i, ls)
      end subroutine nc_stage_lsu_ls_aa

      subroutine nc_stage_lsu_pw_aa(i, pw)
        integer, intent(in) :: i
        type(output_plantweather), intent(in) :: pw
        call nc_stage_lsu_pw_to(s_lsu_pw_a, i, pw)
      end subroutine nc_stage_lsu_pw_aa

      subroutine nc_stage_aquifer_mon(i, a)
        integer, intent(in) :: i
        type(aquifer_dynamic), intent(in) :: a
        call nc_stage_aquifer_to(s_aqu_m, i, a)
      end subroutine nc_stage_aquifer_mon

      subroutine nc_stage_aquifer_yr(i, a)
        integer, intent(in) :: i
        type(aquifer_dynamic), intent(in) :: a
        call nc_stage_aquifer_to(s_aqu_y, i, a)
      end subroutine nc_stage_aquifer_yr

      subroutine nc_stage_aquifer_aa(i, a)
        integer, intent(in) :: i
        type(aquifer_dynamic), intent(in) :: a
        call nc_stage_aquifer_to(s_aqu_a, i, a)
      end subroutine nc_stage_aquifer_aa

      subroutine nc_stage_sd_channel_mon(i, wb, stor, hin, hout, t)
        integer, intent(in) :: i
        type(water_body), intent(in) :: wb
        type(hyd_output), intent(in) :: stor, hin, hout
        real, intent(in) :: t
        call nc_stage_sd_to(s_cha_sd_m, i, wb, stor, hin, hout, t)
      end subroutine nc_stage_sd_channel_mon

      subroutine nc_stage_sd_channel_yr(i, wb, stor, hin, hout, t)
        integer, intent(in) :: i
        type(water_body), intent(in) :: wb
        type(hyd_output), intent(in) :: stor, hin, hout
        real, intent(in) :: t
        call nc_stage_sd_to(s_cha_sd_y, i, wb, stor, hin, hout, t)
      end subroutine nc_stage_sd_channel_yr

      subroutine nc_stage_sd_channel_aa(i, wb, stor, hin, hout, t)
        integer, intent(in) :: i
        type(water_body), intent(in) :: wb
        type(hyd_output), intent(in) :: stor, hin, hout
        real, intent(in) :: t
        call nc_stage_sd_to(s_cha_sd_a, i, wb, stor, hin, hout, t)
      end subroutine nc_stage_sd_channel_aa

      subroutine nc_stage_basin_aquifer_to(s, a)
        type(nc_stream), intent(inout) :: s
        type(aquifer_dynamic), intent(in) :: a
        real :: arr(naqu)
        if (s%on) then; call aqu_pack(a, arr); call nc_stream_stage(s, 1, arr); end if
      end subroutine nc_stage_basin_aquifer_to

      subroutine nc_stage_basin_aquifer(a)
        type(aquifer_dynamic), intent(in) :: a
        call nc_stage_basin_aquifer_to(s_bsn_aqu_d, a)
      end subroutine nc_stage_basin_aquifer

      subroutine nc_stage_basin_aquifer_mon(a)
        type(aquifer_dynamic), intent(in) :: a
        call nc_stage_basin_aquifer_to(s_bsn_aqu_m, a)
      end subroutine nc_stage_basin_aquifer_mon

      subroutine nc_stage_basin_aquifer_yr(a)
        type(aquifer_dynamic), intent(in) :: a
        call nc_stage_basin_aquifer_to(s_bsn_aqu_y, a)
      end subroutine nc_stage_basin_aquifer_yr

      subroutine nc_stage_basin_aquifer_aa(a)
        type(aquifer_dynamic), intent(in) :: a
        call nc_stage_basin_aquifer_to(s_bsn_aqu_a, a)
      end subroutine nc_stage_basin_aquifer_aa

      subroutine nc_stage_basin_channel_to(s, c)
        type(nc_stream), intent(inout) :: s
        type(ch_output), intent(in) :: c
        real :: arr(nch)
        if (s%on) then; call bch_pack(c, arr); call nc_stream_stage(s, 1, arr); end if
      end subroutine nc_stage_basin_channel_to

      subroutine nc_stage_basin_channel(c)
        type(ch_output), intent(in) :: c
        call nc_stage_basin_channel_to(s_bsn_cha_d, c)
      end subroutine nc_stage_basin_channel

      subroutine nc_stage_basin_channel_mon(c)
        type(ch_output), intent(in) :: c
        call nc_stage_basin_channel_to(s_bsn_cha_m, c)
      end subroutine nc_stage_basin_channel_mon

      subroutine nc_stage_basin_channel_yr(c)
        type(ch_output), intent(in) :: c
        call nc_stage_basin_channel_to(s_bsn_cha_y, c)
      end subroutine nc_stage_basin_channel_yr

      subroutine nc_stage_basin_channel_aa(c)
        type(ch_output), intent(in) :: c
        call nc_stage_basin_channel_to(s_bsn_cha_a, c)
      end subroutine nc_stage_basin_channel_aa

      subroutine nc_stage_channel_to(s, i, c)
        type(nc_stream), intent(inout) :: s
        integer, intent(in) :: i
        type(ch_output), intent(in) :: c
        real :: arr(nch)
        if (s%on) then; call bch_pack(c, arr); call nc_stream_stage(s, i, arr); end if
      end subroutine nc_stage_channel_to

      subroutine nc_stage_channel(i, c)
        integer, intent(in) :: i
        type(ch_output), intent(in) :: c
        call nc_stage_channel_to(s_cha_d, i, c)
      end subroutine nc_stage_channel

      subroutine nc_stage_channel_mon(i, c)
        integer, intent(in) :: i
        type(ch_output), intent(in) :: c
        call nc_stage_channel_to(s_cha_m, i, c)
      end subroutine nc_stage_channel_mon

      subroutine nc_stage_channel_yr(i, c)
        integer, intent(in) :: i
        type(ch_output), intent(in) :: c
        call nc_stage_channel_to(s_cha_y, i, c)
      end subroutine nc_stage_channel_yr

      subroutine nc_stage_channel_aa(i, c)
        integer, intent(in) :: i
        type(ch_output), intent(in) :: c
        call nc_stage_channel_to(s_cha_a, i, c)
      end subroutine nc_stage_channel_aa

      subroutine nc_flush_daily_channel()
        if (s_cha_d%on) call nc_stream_flush(s_cha_d)
      end subroutine nc_flush_daily_channel

      subroutine nc_flush_monthly_channel()
        if (s_cha_m%on) call nc_stream_flush(s_cha_m)
      end subroutine nc_flush_monthly_channel

      subroutine nc_flush_yearly_channel()
        if (s_cha_y%on) call nc_stream_flush(s_cha_y)
      end subroutine nc_flush_yearly_channel

      subroutine nc_flush_aa_channel()
        if (s_cha_a%on) call nc_stream_flush(s_cha_a)
      end subroutine nc_flush_aa_channel

      end module netcdf_output_module
