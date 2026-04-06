      subroutine gwflow_output_init

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine opens all gwflow output files and writes headers
!!    (extracted from gwflow_read)

      use gwflow_module
      use hydrograph_module
      use sd_channel_module
      use time_module
      use constituent_mass_module, only : cs_db
      use basin_module, only : pco, bsn

      implicit none

      logical :: i_exist
      integer :: in_gw = 1230
      integer :: s = 0
      character*4 aString
      character(len=16) :: hydsep_hdr(10) = ""
      !general
      character(len=13) :: header = ""
      character*100 file_name(50)
      character*100 file_name_scalar
      integer :: i = 0
      integer :: j = 0
      integer :: n = 0
      integer :: max_num = 0
      integer :: wb_cell = 0
      real :: group_area = 0.

      !initialize groundwater balance ---------------------------------------------------------------------------------------------------------------
      write(out_gw,*)
      write(out_gw,*) '     initialize groundwater balance files and arrays'

      !write cell definition file (maps cell index to spatial location for all output)
      call gwflow_write_celldef
      write(out_gw,*) '          cell definition written to gwflow_cell_definition.txt'

      !--- print.prt overrides: when gwflow entries are listed in print.prt, override gwflow.input flags ---
      if(pco%gwflow_wb%already_read_in) then
        gwflag_day = 0; gwflag_mon = 0; gwflag_yr = 0; gwflag_aa = 0
        if(pco%gwflow_wb%d == 'y') gwflag_day = 1
        if(pco%gwflow_wb%m == 'y') gwflag_mon = 1
        if(pco%gwflow_wb%y == 'y') gwflag_yr  = 1
        if(pco%gwflow_wb%a == 'y') gwflag_aa  = 1
        write(out_gw,*) '          print.prt gwflow_wb overrides gwflow.input flags'
      endif
      if(pco%gwflow_obs%already_read_in) then
        gwflag_obs = 0
        if(pco%gwflow_obs%d == 'y') gwflag_obs = 1
        write(out_gw,*) '          print.prt gwflow_obs controls observation well output'
      endif
      if(pco%gwflow_pump%already_read_in) then
        gwflag_pump = 0
        if(pco%gwflow_pump%d == 'y' .or. pco%gwflow_pump%m == 'y' .or. &
           pco%gwflow_pump%y == 'y' .or. pco%gwflow_pump%a == 'y') gwflag_pump = 1
        write(out_gw,*) '          print.prt gwflow_pump controls HRU pumping output'
      endif
      if(pco%gwflow_heat%already_read_in) then
        gwflag_heat = 0
        if(pco%gwflow_heat%d == 'y' .or. pco%gwflow_heat%y == 'y' .or. &
           pco%gwflow_heat%a == 'y') gwflag_heat = 1
        write(out_gw,*) '          print.prt gwflow_heat controls heat balance output'
      endif
      if(pco%gwflow_solute%already_read_in) then
        gwflag_solute = 0
        if(pco%gwflow_solute%d == 'y' .or. pco%gwflow_solute%m == 'y' .or. &
           pco%gwflow_solute%y == 'y' .or. pco%gwflow_solute%a == 'y') gwflag_solute = 1
        write(out_gw,*) '          print.prt gwflow_solute controls solute balance output'
      endif
      if(pco%gwflow_flux%already_read_in) then
        gwflag_flux = 0
        if(pco%gwflow_flux%d == 'y' .or. pco%gwflow_flux%y == 'y') gwflag_flux = 1
        write(out_gw,*) '          print.prt gwflow_flux controls canal/pond/tile/chan diagnostic output'
      endif

      !open file to track daily groundwater water balance
      if(gwflag_day.eq.1) then
        open(out_gwbal,file='gwflow_basin_wb_day.txt')
        write(out_gwbal,*) 'Groundwater watershed-wide fluxes for each day (mm)'
        write(out_gwbal,8000) 'jday','mon','day','yr','unit','gis_id', &
          'name','ts','vbef','vaft','recharge','gw_et','gw_sw','sw_gw', &
          'sat_excess','soil','lateral','boundary','pump_allo','pump_ext', &
          'tile','reservoir','wetland','canal','floodplain','pond', &
          'phytorem','error','sat_frac','wt_depth','pump_def'
        write(out_gwbal,8000) '','','','','','','','---','mm','mm', &
          'mm','mm','mm','mm','mm','mm','mm','mm','mm','mm', &
          'mm','mm','mm','mm','mm','mm','mm','%','frac','m','mm'
      endif

      !open file to track monthly groundwater water balance
      if(gwflag_mon.eq.1) then
        open(out_gwbal_mon,file='gwflow_basin_wb_mon.txt')
        write(out_gwbal_mon,*) 'Groundwater watershed-wide fluxes for each month (mm)'
        write(out_gwbal_mon,8000) 'jday','mon','day','yr','unit','gis_id', &
          'name','dvol','recharge','gw_et','gw_sw','sw_gw', &
          'sat_excess','soil','lateral','boundary','pump_allo','pump_ext', &
          'tile','reservoir','wetland','canal','floodplain','pond', &
          'phytorem','pump_def'
        write(out_gwbal_mon,8000) '','','','','','','','mm','mm','mm', &
          'mm','mm','mm','mm','mm','mm','mm','mm','mm','mm', &
          'mm','mm','mm','mm','mm','mm'
      endif

      !open file to track yearly groundwater water balance
      if(gwflag_yr.eq.1) then
        open(out_gwbal_yr,file='gwflow_basin_wb_yr.txt')
        write(out_gwbal_yr,*) 'Groundwater watershed-wide fluxes for each year (mm)'
        write(out_gwbal_yr,8000) 'jday','mon','day','yr','unit','gis_id', &
          'name','dvol','recharge','gw_et','gw_sw','sw_gw', &
          'sat_excess','soil','lateral','boundary','pump_allo','pump_ext', &
          'tile','reservoir','wetland','canal','floodplain','pond', &
          'phytorem','pump_def'
        write(out_gwbal_yr,8000) '','','','','','','','mm','mm','mm', &
          'mm','mm','mm','mm','mm','mm','mm','mm','mm','mm', &
          'mm','mm','mm','mm','mm','mm'
      endif

      !open file to write out average annual groundwater water balance
        if(gwflag_aa.eq.1) then
        open(out_gwbal_aa,file='gwflow_basin_wb_aa.txt')
        write(out_gwbal_aa,*) 'Average annual groundwater watershed-wide fluxes (mm)'
        write(out_gwbal_aa,8000) 'jday','mon','day','yr','unit','gis_id', &
          'name','dvol','recharge','gw_et','gw_sw','sw_gw', &
          'sat_excess','soil','lateral','boundary','pump_allo','pump_ext', &
          'tile','reservoir','wetland','canal','floodplain','pond', &
          'phytorem','pump_def'
        write(out_gwbal_aa,8000) '','','','','','','','mm','mm','mm', &
          'mm','mm','mm','mm','mm','mm','mm','mm','mm','mm', &
          'mm','mm','mm','mm','mm','mm'
        endif

      !open files to track daily groundwater water balance for selected groups of cells
      inquire(file='gwflow.wbgroups',exist=i_exist)
      if(i_exist) then
        gw_group_flag = 1
        open(in_gw,file='gwflow.wbgroups')
        read(in_gw,*) header
        read(in_gw,*) gw_wb_grp_num !number of cell groups
        read(in_gw,*) max_num !maximum number of cells in a group
        allocate(gw_wb_grp_ncell(gw_wb_grp_num))
        allocate(gw_wb_grp_cells(gw_wb_grp_num,max_num))
        gw_wb_grp_ncell = 0
        !loop through the cell groups
        do i=1,gw_wb_grp_num
          !read in the cells in each group
          read(in_gw,*) header
          read(in_gw,*) gw_wb_grp_ncell(i)
          group_area = 0.
          do j=1,gw_wb_grp_ncell(i)
            read(in_gw,*) wb_cell
            if(wb_cell > 0) then
              if(grid_type == "structured") then
                wb_cell = cell_id_list(wb_cell)
                gw_wb_grp_cells(i,j) = wb_cell
              elseif(grid_type == "unstructured") then
                gw_wb_grp_cells(i,j) = wb_cell
              endif
              group_area = group_area + gw_state(wb_cell)%area !m2
            endif
          enddo
          !open the water balance output file
          write(aString,1091) i
          file_name_scalar = 'gwflow_group_wb_day_'//aString
          open(out_gwbal_grp+i,file=file_name_scalar)
          write(out_gwbal_grp+i,*) 'Groundwater fluxes for cell group (m3)',i
          write(out_gwbal_grp+i,8000) 'jday','mon','day','yr','unit', &
            'gis_id','name','ts','vbef','vaft','recharge','gw_et', &
            'gw_sw','sw_gw','sat_excess','soil','lateral','boundary', &
            'pump_allo','pump_ext','tile','reservoir','wetland','canal', &
            'floodplain','pond','phytorem','error','wt_depth','pump_def'
          write(out_gwbal_grp+i,8000) '','','','','','','','---', &
            'm3','m3','m3','m3','m3','m3','m3','m3','m3','m3', &
            'm3','m3','m3','m3','m3','m3','m3','m3','m3','%', &
            'm','m3'
        enddo !go to next cell group
        close(in_gw)
  1091  format(i2)
      endif



      !initialize groundwater heat balance ----------------------------------------------------------------------------------------------------------
      if(gw_heat_flag == 1) then
      write(out_gw,*)
      write(out_gw,*) '     initialize groundwater heat balance files and arrays'

      !open file to track daily groundwater heat balance
      if(gwflag_day.eq.1) then
        open(out_heatbal_dy,file='gwflow_basin_heat_day.txt')
        write(out_heatbal_dy,*) 'Groundwater watershed-wide heat fluxes for each day (MJ)'
        write(out_heatbal_dy,8000) 'jday','mon','day','yr','unit','gis_id', &
          'name','ts','hbef','haft','recharge','gw_et','gw_sw','sw_gw', &
          'sat_excess','soil','lateral','dispersion','boundary', &
          'pump_allo','pump_ext','tile','reservoir','wetland','canal', &
          'floodplain','pond','error','tavg'
        write(out_heatbal_dy,8000) '','','','','','','','---', &
          'MJ','MJ','MJ','MJ','MJ','MJ','MJ','MJ','MJ','MJ','MJ', &
          'MJ','MJ','MJ','MJ','MJ','MJ','MJ','%','degC'
      endif

      !open file to track yearly groundwater heat balance
      if(gwflag_yr.eq.1) then
        open(out_heatbal_yr,file='gwflow_basin_heat_yr.txt')
        write(out_heatbal_yr,*) 'Groundwater watershed-wide heat fluxes for each year (MJ)'
        write(out_heatbal_yr,8000) 'jday','mon','day','yr','unit','gis_id', &
          'name','hdel','recharge','gw_et','gw_sw','sw_gw', &
          'sat_excess','soil','lateral','dispersion','boundary', &
          'pump_allo','pump_ext','tile','reservoir','wetland','canal', &
          'floodplain','pond'
        write(out_heatbal_yr,8000) '','','','','','','','MJ','MJ','MJ', &
          'MJ','MJ','MJ','MJ','MJ','MJ','MJ','MJ','MJ','MJ','MJ', &
          'MJ','MJ','MJ'
      endif

      !open file to write out average annual groundwater heat balance
      if(gwflag_aa.eq.1) then
        open(out_heatbal_aa,file='gwflow_basin_heat_aa.txt')
        write(out_heatbal_aa,*) 'Groundwater watershed-wide heat fluxes average annual (MJ)'
        write(out_heatbal_aa,8000) 'jday','mon','day','yr','unit','gis_id', &
          'name','hdel','recharge','gw_et','gw_sw','sw_gw', &
          'sat_excess','soil','lateral','dispersion','boundary', &
          'pump_allo','pump_ext','tile','reservoir','wetland','canal', &
          'floodplain','pond'
        write(out_heatbal_aa,8000) '','','','','','','','MJ','MJ','MJ', &
          'MJ','MJ','MJ','MJ','MJ','MJ','MJ','MJ','MJ','MJ','MJ', &
          'MJ','MJ','MJ'
      endif

      endif



      !initialize solute mass balance ---------------------------------------------------------------------------------------------------------------
      if(gw_solute_flag == 1) then

        !allocate monthly, yearly, and total arrays
        !monthly
        allocate(sol_grid_chng_mo(gw_nsolute))
        allocate(sol_grid_rech_mo(gw_nsolute))
        allocate(sol_grid_gwsw_mo(gw_nsolute))
        allocate(sol_grid_swgw_mo(gw_nsolute))
        allocate(sol_grid_satx_mo(gw_nsolute))
        allocate(sol_grid_advn_mo(gw_nsolute))
        allocate(sol_grid_disp_mo(gw_nsolute))
        allocate(sol_grid_rcti_mo(gw_nsolute))
        allocate(sol_grid_rcto_mo(gw_nsolute))
        allocate(sol_grid_minl_mo(gw_nsolute))
        allocate(sol_grid_sorb_mo(gw_nsolute))
        allocate(sol_grid_ppag_mo(gw_nsolute))
        allocate(sol_grid_ppex_mo(gw_nsolute))
        allocate(sol_grid_tile_mo(gw_nsolute))
        allocate(sol_grid_soil_mo(gw_nsolute))
        allocate(sol_grid_resv_mo(gw_nsolute))
        allocate(sol_grid_wetl_mo(gw_nsolute))
        allocate(sol_grid_canl_mo(gw_nsolute))
        allocate(sol_grid_fpln_mo(gw_nsolute))
        allocate(sol_grid_pond_mo(gw_nsolute))
        !yearly
        allocate(sol_grid_chng_yr(gw_nsolute))
        allocate(sol_grid_rech_yr(gw_nsolute))
        allocate(sol_grid_gwsw_yr(gw_nsolute))
        allocate(sol_grid_swgw_yr(gw_nsolute))
        allocate(sol_grid_satx_yr(gw_nsolute))
        allocate(sol_grid_advn_yr(gw_nsolute))
        allocate(sol_grid_disp_yr(gw_nsolute))
        allocate(sol_grid_rcti_yr(gw_nsolute))
        allocate(sol_grid_rcto_yr(gw_nsolute))
        allocate(sol_grid_minl_yr(gw_nsolute))
        allocate(sol_grid_sorb_yr(gw_nsolute))
        allocate(sol_grid_ppag_yr(gw_nsolute))
        allocate(sol_grid_ppex_yr(gw_nsolute))
        allocate(sol_grid_tile_yr(gw_nsolute))
        allocate(sol_grid_soil_yr(gw_nsolute))
        allocate(sol_grid_resv_yr(gw_nsolute))
        allocate(sol_grid_wetl_yr(gw_nsolute))
        allocate(sol_grid_canl_yr(gw_nsolute))
        allocate(sol_grid_fpln_yr(gw_nsolute))
        allocate(sol_grid_pond_yr(gw_nsolute))
        !total
        allocate(sol_grid_chng_tt(gw_nsolute))
        allocate(sol_grid_rech_tt(gw_nsolute))
        allocate(sol_grid_gwsw_tt(gw_nsolute))
        allocate(sol_grid_swgw_tt(gw_nsolute))
        allocate(sol_grid_satx_tt(gw_nsolute))
        allocate(sol_grid_advn_tt(gw_nsolute))
        allocate(sol_grid_disp_tt(gw_nsolute))
        allocate(sol_grid_rcti_tt(gw_nsolute))
        allocate(sol_grid_rcto_tt(gw_nsolute))
        allocate(sol_grid_minl_tt(gw_nsolute))
        allocate(sol_grid_sorb_tt(gw_nsolute))
        allocate(sol_grid_ppag_tt(gw_nsolute))
        allocate(sol_grid_ppex_tt(gw_nsolute))
        allocate(sol_grid_tile_tt(gw_nsolute))
        allocate(sol_grid_soil_tt(gw_nsolute))
        allocate(sol_grid_resv_tt(gw_nsolute))
        allocate(sol_grid_wetl_tt(gw_nsolute))
        allocate(sol_grid_canl_tt(gw_nsolute))
        allocate(sol_grid_fpln_tt(gw_nsolute))
        allocate(sol_grid_pond_tt(gw_nsolute))

        !loop through the solutes
        do n=1,gw_nsolute

          !daily solute mass balance
          if(gwflag_day.eq.1) then
            !prepare solute mass balance output files (daily output)
            file_name(1) = 'gwflow_basin_sol_no3_day.txt'
            file_name(2) = 'gwflow_basin_sol_p_day.txt'
            if(cs_db%num_salts > 0) then
              file_name(3) = 'gwflow_basin_sol_so4_day.txt'
              file_name(4) = 'gwflow_basin_sol_ca_day.txt'
              file_name(5) = 'gwflow_basin_sol_mg_day.txt'
              file_name(6) = 'gwflow_basin_sol_na_day.txt'
              file_name(7) = 'gwflow_basin_sol_k_day.txt'
              file_name(8) = 'gwflow_basin_sol_cl_day.txt'
              file_name(9) = 'gwflow_basin_sol_co3_day.txt'
              file_name(10) = 'gwflow_basin_sol_hco3_day.txt'
            endif
            if(cs_db%num_cs > 0) then
              file_name(11) = 'gwflow_basin_sol_seo4_day.txt'
              file_name(12) = 'gwflow_basin_sol_seo3_day.txt'
            endif
            open(out_solbal_dy+n,file=file_name(n))
            write(out_solbal_dy+n,*) 'Solute basin daily loads (kg):',gwsol_nm(n)
            write(out_solbal_dy+n,8000) 'jday','mon','day','yr','unit', &
              'gis_id','name','ts','mbef','maft','recharge','gw_sw', &
              'sw_gw','sat_excess','soil','advection','dispersion', &
              'react_in','react_out','mineral','sorption','pump_allo', &
              'pump_ext','tile','reservoir','wetland','canal', &
              'floodplain','pond','error'
            write(out_solbal_dy+n,8000) '','','','','','','','---', &
              'kg','kg','kg','kg','kg','kg','kg','kg','kg','kg','kg', &
              'kg','kg','kg','kg','kg','kg','kg','kg','kg','kg','%'
          endif

          !monthly solute mass balance
          if(gwflag_mon.eq.1) then
            !prepare solute mass balance output files (monthly output)
            file_name(1) = 'gwflow_basin_sol_no3_mon.txt'
            file_name(2) = 'gwflow_basin_sol_p_mon.txt'
            if(cs_db%num_salts > 0) then
              file_name(3) = 'gwflow_basin_sol_so4_mon.txt'
              file_name(4) = 'gwflow_basin_sol_ca_mon.txt'
              file_name(5) = 'gwflow_basin_sol_mg_mon.txt'
              file_name(6) = 'gwflow_basin_sol_na_mon.txt'
              file_name(7) = 'gwflow_basin_sol_k_mon.txt'
              file_name(8) = 'gwflow_basin_sol_cl_mon.txt'
              file_name(9) = 'gwflow_basin_sol_co3_mon.txt'
              file_name(10) = 'gwflow_basin_sol_hco3_mon.txt'
            endif
            if(cs_db%num_cs > 0) then
              file_name(11) = 'gwflow_basin_sol_seo4_mon.txt'
              file_name(12) = 'gwflow_basin_sol_seo3_mon.txt'
            endif
            open(out_solbal_mo+n,file=file_name(n))
            write(out_solbal_mo+n,*) 'Solute basin monthly loads (kg):',gwsol_nm(n)
            write(out_solbal_mo+n,8000) 'jday','mon','day','yr','unit', &
              'gis_id','name','delm','recharge','gw_sw','sw_gw', &
              'sat_excess','soil','advection','dispersion','react_in', &
              'react_out','mineral','sorption','pump_allo','pump_ext', &
              'tile','reservoir','wetland','canal','floodplain','pond'
            write(out_solbal_mo+n,8000) '','','','','','','','kg','kg', &
              'kg','kg','kg','kg','kg','kg','kg','kg','kg','kg','kg', &
              'kg','kg','kg','kg','kg','kg','kg'
            !zero out monthly arrays
            sol_grid_chng_mo(n) = 0.
            sol_grid_rech_mo(n) = 0.
            sol_grid_gwsw_mo(n) = 0.
            sol_grid_swgw_mo(n) = 0.
            sol_grid_satx_mo(n) = 0.
            sol_grid_advn_mo(n) = 0.
            sol_grid_disp_mo(n) = 0.
            sol_grid_rcti_mo(n) = 0.
            sol_grid_rcto_mo(n) = 0.
            sol_grid_minl_mo(n) = 0.
            sol_grid_sorb_mo(n) = 0.
            sol_grid_ppag_mo(n) = 0.
            sol_grid_ppex_mo(n) = 0.
            sol_grid_tile_mo(n) = 0.
            sol_grid_soil_mo(n) = 0.
            sol_grid_resv_mo(n) = 0.
            sol_grid_wetl_mo(n) = 0.
            sol_grid_canl_mo(n) = 0.
            sol_grid_fpln_mo(n) = 0.
            sol_grid_pond_mo(n) = 0.
          endif

          !yearly solute mass balance
          if(gwflag_yr.eq.1) then
            !prepare solute mass balance output files (yearly output)
            file_name(1) = 'gwflow_basin_sol_no3_yr.txt'
            file_name(2) = 'gwflow_basin_sol_p_yr.txt'
            if(cs_db%num_salts > 0) then
              file_name(3) = 'gwflow_basin_sol_so4_yr.txt'
              file_name(4) = 'gwflow_basin_sol_ca_yr.txt'
              file_name(5) = 'gwflow_basin_sol_mg_yr.txt'
              file_name(6) = 'gwflow_basin_sol_na_yr.txt'
              file_name(7) = 'gwflow_basin_sol_k_yr.txt'
              file_name(8) = 'gwflow_basin_sol_cl_yr.txt'
              file_name(9) = 'gwflow_basin_sol_co3_yr.txt'
              file_name(10) = 'gwflow_basin_sol_hco3_yr.txt'
            endif
            if(cs_db%num_cs > 0) then
              file_name(11) = 'gwflow_basin_sol_seo4_yr.txt'
              file_name(12) = 'gwflow_basin_sol_seo3_yr.txt'
            endif
            open(out_solbal_yr+n,file=file_name(n))
            write(out_solbal_yr+n,*) 'Solute basin annual loads (kg):',gwsol_nm(n)
            write(out_solbal_yr+n,8000) 'jday','mon','day','yr','unit', &
              'gis_id','name','delm','recharge','gw_sw','sw_gw', &
              'sat_excess','soil','advection','dispersion','react_in', &
              'react_out','mineral','sorption','pump_allo','pump_ext', &
              'tile','reservoir','wetland','canal','floodplain','pond'
            write(out_solbal_yr+n,8000) '','','','','','','','kg','kg', &
              'kg','kg','kg','kg','kg','kg','kg','kg','kg','kg','kg', &
              'kg','kg','kg','kg','kg','kg','kg'
            !zero out yearly arrays
            sol_grid_chng_yr(n) = 0.
            sol_grid_rech_yr(n) = 0.
            sol_grid_gwsw_yr(n) = 0.
            sol_grid_swgw_yr(n) = 0.
            sol_grid_satx_yr(n) = 0.
            sol_grid_advn_yr(n) = 0.
            sol_grid_disp_yr(n) = 0.
            sol_grid_rcti_yr(n) = 0.
            sol_grid_rcto_yr(n) = 0.
            sol_grid_minl_yr(n) = 0.
            sol_grid_sorb_yr(n) = 0.
            sol_grid_ppag_yr(n) = 0.
            sol_grid_ppex_yr(n) = 0.
            sol_grid_tile_yr(n) = 0.
            sol_grid_soil_yr(n) = 0.
            sol_grid_resv_yr(n) = 0.
            sol_grid_wetl_yr(n) = 0.
            sol_grid_canl_yr(n) = 0.
            sol_grid_fpln_yr(n) = 0.
            sol_grid_pond_yr(n) = 0.
          endif

          !average annual solute mass balance
          if(gwflag_aa.eq.1) then
            !prepare solute mass balance output files (average annual output)
            file_name(1) = 'gwflow_basin_sol_no3_aa.txt'
            file_name(2) = 'gwflow_basin_sol_p_aa.txt'
            if(cs_db%num_salts > 0) then
              file_name(3) = 'gwflow_basin_sol_so4_aa.txt'
              file_name(4) = 'gwflow_basin_sol_ca_aa.txt'
              file_name(5) = 'gwflow_basin_sol_mg_aa.txt'
              file_name(6) = 'gwflow_basin_sol_na_aa.txt'
              file_name(7) = 'gwflow_basin_sol_k_aa.txt'
              file_name(8) = 'gwflow_basin_sol_cl_aa.txt'
              file_name(9) = 'gwflow_basin_sol_co3_aa.txt'
              file_name(10) = 'gwflow_basin_sol_hco3_aa.txt'
            endif
            if(cs_db%num_cs > 0) then
              file_name(11) = 'gwflow_basin_sol_seo4_aa.txt'
              file_name(12) = 'gwflow_basin_sol_seo3_aa.txt'
            endif
            open(out_solbal_aa+n,file=file_name(n))
            write(out_solbal_aa+n,*) 'Solute basin average annual loads (kg):',gwsol_nm(n)
            write(out_solbal_aa+n,8000) 'jday','mon','day','yr','unit', &
              'gis_id','name','delm','recharge','gw_sw','sw_gw', &
              'sat_excess','soil','advection','dispersion','react_in', &
              'react_out','mineral','sorption','pump_allo','pump_ext', &
              'tile','reservoir','wetland','canal','floodplain','pond'
            write(out_solbal_aa+n,8000) '','','','','','','','kg','kg', &
              'kg','kg','kg','kg','kg','kg','kg','kg','kg','kg','kg', &
              'kg','kg','kg','kg','kg','kg','kg'
            !zero out yearly arrays
            sol_grid_chng_tt(n) = 0.
            sol_grid_rech_tt(n) = 0.
            sol_grid_gwsw_tt(n) = 0.
            sol_grid_swgw_tt(n) = 0.
            sol_grid_satx_tt(n) = 0.
            sol_grid_advn_tt(n) = 0.
            sol_grid_disp_tt(n) = 0.
            sol_grid_rcti_tt(n) = 0.
            sol_grid_rcto_tt(n) = 0.
            sol_grid_minl_tt(n) = 0.
            sol_grid_sorb_tt(n) = 0.
            sol_grid_ppag_tt(n) = 0.
            sol_grid_ppex_tt(n) = 0.
            sol_grid_tile_tt(n) = 0.
            sol_grid_soil_tt(n) = 0.
            sol_grid_resv_tt(n) = 0.
            sol_grid_wetl_tt(n) = 0.
            sol_grid_canl_tt(n) = 0.
            sol_grid_fpln_tt(n) = 0.
            sol_grid_pond_tt(n) = 0.
          endif

        enddo !go to next solute

      endif !check for solutes


      !open cell-level long-format output files -----------------------------------------
      if(gwflag_day.eq.1) then
        open(out_gwcell_day,file='gwflow_cell_wb_day.txt')
        write(out_gwcell_day,*) 'gwflow cell-level daily output'
        write(out_gwcell_day,*) 'fluxes in m3/day; head and wtdepth in m'
        write(out_gwcell_day,'(a8,a6,a6,a8,a8,a10,a12,20a13)') &
          'jday','mon','day','yr','unit','gis_id','name', &
          'head','wt_depth','recharge','gw_et','gw_sw','sw_gw', &
          'sat_excess','soil','lateral', &
          'pump_allo','pump_ext','tile','reservoir','wetland', &
          'floodplain','canal','pond','phytorem'
      endif
      if(gwflag_mon.eq.1) then
        open(out_gwcell_mon,file='gwflow_cell_wb_mon.txt')
        write(out_gwcell_mon,*) 'gwflow cell-level monthly output'
        write(out_gwcell_mon,*) 'values are monthly average daily rates' &
          //' (m3/day for fluxes, m for head/wtdepth)'
        write(out_gwcell_mon,'(a8,a6,a6,a8,a8,a10,a12,20a13)') &
          'jday','mon','day','yr','unit','gis_id','name', &
          'head','wt_depth','recharge','gw_et','gw_sw','sw_gw', &
          'sat_excess','soil','lateral', &
          'pump_allo','pump_ext','tile','reservoir','wetland', &
          'floodplain','canal','pond','phytorem'
      endif
      if(gwflag_yr.eq.1) then
        open(out_gwcell_yr,file='gwflow_cell_wb_yr.txt')
        write(out_gwcell_yr,*) 'gwflow cell-level annual output'
        write(out_gwcell_yr,*) 'values are annual average daily rates' &
          //' (m3/day for fluxes, m for head/wtdepth)'
        write(out_gwcell_yr,'(a8,a6,a6,a8,a8,a10,a12,20a13)') &
          'jday','mon','day','yr','unit','gis_id','name', &
          'head','wt_depth','recharge','gw_et','gw_sw','sw_gw', &
          'sat_excess','soil','lateral', &
          'pump_allo','pump_ext','tile','reservoir','wetland', &
          'floodplain','canal','pond','phytorem'
      endif
      if(gwflag_aa.eq.1) then
        open(out_gwcell_aa,file='gwflow_cell_wb_aa.txt')
        write(out_gwcell_aa,*) 'gwflow cell-level average annual output'
        write(out_gwcell_aa,*) 'values are avg annual daily rates' &
          //' (m3/day for fluxes, m for head/wtdepth)'
        write(out_gwcell_aa,'(a8,a6,a6,a8,a8,a10,a12,20a13)') &
          'jday','mon','day','yr','unit','gis_id','name', &
          'head','wt_depth','recharge','gw_et','gw_sw','sw_gw', &
          'sat_excess','soil','lateral', &
          'pump_allo','pump_ext','tile','reservoir','wetland', &
          'floodplain','canal','pond','phytorem'
      endif


      !format statement for standard SWAT+ basin-level headers
8000  format(4a6,2a8,a18,50a13)

      return
      end subroutine gwflow_output_init


      subroutine gwflow_output_day

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes and writes daily gwflow output:
!!    observation wells, cell groups, basin water balance, heat balance,
!!    solute mass balance; accumulates to monthly/yearly/aa totals

      use gwflow_module
      use hydrograph_module
      use sd_channel_module
      use time_module
      use basin_module, only : bsn

      implicit none

      integer :: i, j, k, s, iob
      integer :: cell_id
      real :: sum
      real :: obs_temp, obs_no3, obs_p
      character(len=16) :: obs_name
      real :: frac_sat
      real :: depth_wt_avg
      real :: depth_wt_avg_grp
      real :: mass_error
      real :: temp_avg
      !grid totals of fluxes for day (m3, mm)
      !note: vbef_grid, vaft_grid are module variables (shared with output_aa)
      real*8 :: rech_grid
      real*8 :: gwet_grid
      real*8 :: gwsw_grid
      real*8 :: swgw_grid
      real*8 :: satx_grid
      real*8 :: soil_grid
      real*8 :: latl_grid
      real*8 :: bndr_grid
      real*8 :: ppag_grid
      real*8 :: ppdf_grid
      real*8 :: ppex_grid
      real*8 :: tile_grid
      real*8 :: resv_grid
      real*8 :: wetl_grid
      real*8 :: canl_grid
      real*8 :: fpln_grid
      real*8 :: pond_grid
      real*8 :: phyt_grid
      !cell group totals of fluxes for day (m3)
      real*8 :: vbef_grp
      real*8 :: vaft_grp
      real*8 :: rech_grp
      real*8 :: gwet_grp
      real*8 :: gwsw_grp
      real*8 :: swgw_grp
      real*8 :: satx_grp
      real*8 :: soil_grp
      real*8 :: latl_grp
      real*8 :: bndr_grp
      real*8 :: ppag_grp
      real*8 :: ppdf_grp
      real*8 :: ppex_grp
      real*8 :: tile_grp
      real*8 :: resv_grp
      real*8 :: wetl_grp
      real*8 :: canl_grp
      real*8 :: fpln_grp
      real*8 :: pond_grp
      real*8 :: phyt_grp
      !heat grid totals
      !note: heat_hbef_grid, heat_haft_grid are module variables (shared with output_aa)
      real*8 :: heat_rech_grid
      real*8 :: heat_gwet_grid
      real*8 :: heat_gwsw_grid
      real*8 :: heat_swgw_grid
      real*8 :: heat_satx_grid
      real*8 :: heat_soil_grid
      real*8 :: heat_latl_grid
      real*8 :: heat_disp_grid
      real*8 :: heat_bndr_grid
      real*8 :: heat_ppag_grid
      real*8 :: heat_ppex_grid
      real*8 :: heat_tile_grid
      real*8 :: heat_resv_grid
      real*8 :: heat_wetl_grid
      real*8 :: heat_canl_grid
      real*8 :: heat_fpln_grid
      real*8 :: heat_pond_grid
      !solute grid totals (kg)
      !note: sol_grid_mbef, sol_grid_maft are module variables (shared with output_aa)
      real :: sol_grid_rech
      real :: sol_grid_gwsw
      real :: sol_grid_swgw
      real :: sol_grid_satx
      real :: sol_grid_advn
      real :: sol_grid_disp
      real :: sol_grid_rcti
      real :: sol_grid_rcto
      real :: sol_grid_minl
      real :: sol_grid_sorb
      real :: sol_grid_ppag
      real :: sol_grid_ppex
      real :: sol_grid_tile
      real :: sol_grid_soil
      real :: sol_grid_resv
      real :: sol_grid_wetl
      real :: sol_grid_canl
      real :: sol_grid_fpln
      real :: sol_grid_pond
      integer :: count


      !compute average depth to water table across watershed
      sum = 0.
      count = 0
      do i=1,ncell
        if(gw_state(i)%stat == 1) then
          sum = sum + (gw_state(i)%elev - gw_state(i)%head)
          count = count + 1
        endif
      enddo
      depth_wt_avg = sum / count

      !compute average groundwater temperature
      temp_avg = 0.
      if(gw_heat_flag == 1) then
        sum = 0.
        count = 0
        do i=1,ncell
          if(gw_state(i)%stat == 1) then
            sum = sum + gwheat_state(i)%temp
            count = count + 1
          endif
        enddo
        temp_avg = sum / count
      endif

      !print out head values and solute concentration values for observation cells
      do k=1,gw_num_obs_wells
        gw_obs_head(k) = gw_state(gw_obs_cells(k))%head
        if(gw_heat_flag == 1) then
          gw_obs_temp(k) = gwheat_state(gw_obs_cells(k))%temp
        endif
        if(gw_solute_flag == 1) then
          do s=1,gw_nsolute
            gw_obs_solute(k,s) = gwsol_state(gw_obs_cells(k))%solute(s)%conc
          enddo
        endif
      enddo
      if(gwflag_obs == 1) then
        do k=1,gw_num_obs_wells
          obs_temp = -99.
          obs_no3 = -99.
          obs_p = -99.
          if(gw_heat_flag == 1) obs_temp = gw_obs_temp(k)
          if(gw_solute_flag == 1) then
            obs_no3 = gw_obs_solute(k,1)
            obs_p = gw_obs_solute(k,2)
          endif
          write(obs_name,'(a4,i4.4)') 'obs_',k
          write(out_gwobs,8102) time%day,time%mo,time%day_mo,time%yrc, &
            k,gw_obs_cells(k),obs_name, &
            gw_obs_head(k), &
            gw_state(gw_obs_cells(k))%elev - gw_obs_head(k), &
            obs_temp, obs_no3, obs_p
        enddo
      endif

      !compute groundwater volumes at the end of the day
      do i=1,ncell
        if(gw_state(i)%stat == 1) then
          gw_state(i)%vaft = ((gw_state(i)%head - gw_state(i)%botm) * gw_state(i)%area) * gw_state(i)%spyd
        endif
      enddo
      !compute groundwater heat at the end of the day
      if(gw_heat_flag == 1) then
        do i=1,ncell
          if(gw_state(i)%stat == 1) then
            gwheat_state(i)%haft = gwheat_state(i)%stor
          endif
        enddo
      endif
      !compute solute mass at the end of the day
      if(gw_solute_flag == 1) then
        do i=1,ncell
          if(gw_state(i)%stat == 1) then
            do s=1,gw_nsolute
              gwsol_state(i)%solute(s)%maft = gwsol_state(i)%solute(s)%mass !g
            enddo
          endif
        enddo
      endif


      !--- groundwater water balance ---

      !calculate values for cell groups (if specified)
      if(gw_group_flag == 1) then
        do i=1,gw_wb_grp_num
          vbef_grp = 0.
          vaft_grp = 0.
          rech_grp = 0.
          gwet_grp = 0.
          gwsw_grp = 0.
          swgw_grp = 0.
          satx_grp = 0.
          soil_grp = 0.
          latl_grp = 0.
          bndr_grp = 0.
          ppag_grp = 0.
          ppdf_grp = 0.
          ppex_grp = 0.
          tile_grp = 0.
          resv_grp = 0.
          wetl_grp = 0.
          canl_grp = 0.
          fpln_grp = 0.
          pond_grp = 0.
          phyt_grp = 0.
          do j=1,gw_wb_grp_ncell(i)
            cell_id = gw_wb_grp_cells(i,j)
            if(cell_id > 0) then
            sum = 0.
            if(gw_state(cell_id)%stat == 1) then
              vbef_grp = vbef_grp + gw_state(cell_id)%vbef
              vaft_grp = vaft_grp + gw_state(cell_id)%vaft
              rech_grp = rech_grp + gw_hyd_ss(cell_id)%rech
              gwet_grp = gwet_grp + gw_hyd_ss(cell_id)%gwet
              gwsw_grp = gwsw_grp + gw_hyd_ss(cell_id)%gwsw
              swgw_grp = swgw_grp + gw_hyd_ss(cell_id)%swgw
              satx_grp = satx_grp + gw_hyd_ss(cell_id)%satx
              soil_grp = soil_grp + gw_hyd_ss(cell_id)%soil
              latl_grp = latl_grp + gw_hyd_ss(cell_id)%latl
              bndr_grp = bndr_grp + gw_hyd_ss(cell_id)%bndr
              ppag_grp = ppag_grp + gw_hyd_ss(cell_id)%ppag
              ppdf_grp = ppdf_grp + gw_hyd_ss(cell_id)%ppdf
              ppex_grp = ppex_grp + gw_hyd_ss(cell_id)%ppex
              tile_grp = tile_grp + gw_hyd_ss(cell_id)%tile
              resv_grp = resv_grp + gw_hyd_ss(cell_id)%resv
              wetl_grp = wetl_grp + gw_hyd_ss(cell_id)%wetl
              canl_grp = canl_grp + gw_hyd_ss(cell_id)%canl
              fpln_grp = fpln_grp + gw_hyd_ss(cell_id)%fpln
              pond_grp = pond_grp + gw_hyd_ss(cell_id)%pond
              phyt_grp = phyt_grp + gw_hyd_ss(cell_id)%phyt
              sum = sum + (gw_state(cell_id)%elev - gw_state(cell_id)%head)
            endif
            endif
          enddo
          depth_wt_avg_grp = sum / gw_wb_grp_ncell(i)
          !calculate mass error
          mass_error = 0.
          mass_error = (1-((vbef_grp + rech_grp + gwet_grp + gwsw_grp + swgw_grp + satx_grp + soil_grp + &
                            latl_grp + bndr_grp + ppag_grp + ppex_grp + tile_grp + resv_grp + wetl_grp + &
                            canl_grp + fpln_grp + pond_grp + phyt_grp) &
                           /vaft_grp)) * 100
          !print out daily information
          write(out_gwbal_grp+i,8100) time%day,time%mo,time%day_mo, &
            time%yrc,"       1","       1",bsn%name,gw_time_step, &
            vbef_grp,vaft_grp,rech_grp,gwet_grp,gwsw_grp,swgw_grp, &
            satx_grp,soil_grp,latl_grp,bndr_grp,ppag_grp,ppex_grp, &
            tile_grp,resv_grp,wetl_grp,canl_grp,fpln_grp,pond_grp,phyt_grp, &
            mass_error,depth_wt_avg_grp,ppdf_grp
        enddo !go to next cell group
      endif

      !calculate values for entire grid (all cells)
      vbef_grid = 0.
      vaft_grid = 0.
      rech_grid = 0.
      gwet_grid = 0.
      gwsw_grid = 0.
      swgw_grid = 0.
      satx_grid = 0.
      soil_grid = 0.
      latl_grid = 0.
      bndr_grid = 0.
      ppag_grid = 0.
      ppdf_grid = 0.
      ppex_grid = 0.
      tile_grid = 0.
      resv_grid = 0.
      wetl_grid = 0.
      canl_grid = 0.
      fpln_grid = 0.
      pond_grid = 0.
      phyt_grid = 0.
      do i=1,ncell
        if(gw_state(i)%stat == 1) then
          vbef_grid = vbef_grid + gw_state(i)%vbef
          vaft_grid = vaft_grid + gw_state(i)%vaft
          rech_grid = rech_grid + gw_hyd_ss(i)%rech
          gwet_grid = gwet_grid + gw_hyd_ss(i)%gwet
          gwsw_grid = gwsw_grid + gw_hyd_ss(i)%gwsw
          swgw_grid = swgw_grid + gw_hyd_ss(i)%swgw
          satx_grid = satx_grid + gw_hyd_ss(i)%satx
          soil_grid = soil_grid + gw_hyd_ss(i)%soil
          latl_grid = latl_grid + gw_hyd_ss(i)%latl
          bndr_grid = bndr_grid + gw_hyd_ss(i)%bndr
          ppag_grid = ppag_grid + gw_hyd_ss(i)%ppag
          ppdf_grid = ppdf_grid + gw_hyd_ss(i)%ppdf
          ppex_grid = ppex_grid + gw_hyd_ss(i)%ppex
          tile_grid = tile_grid + gw_hyd_ss(i)%tile
          resv_grid = resv_grid + gw_hyd_ss(i)%resv
          wetl_grid = wetl_grid + gw_hyd_ss(i)%wetl
          canl_grid = canl_grid + gw_hyd_ss(i)%canl
          fpln_grid = fpln_grid + gw_hyd_ss(i)%fpln
          pond_grid = pond_grid + gw_hyd_ss(i)%pond
          phyt_grid = phyt_grid + gw_hyd_ss(i)%phyt
        endif
      enddo
      mass_error = (1-((vbef_grid + rech_grid + gwet_grid + gwsw_grid + swgw_grid + satx_grid + soil_grid + &
                        latl_grid + bndr_grid + ppag_grid + ppex_grid + tile_grid + resv_grid + wetl_grid + &
                        canl_grid + fpln_grid + pond_grid + phyt_grid) &
                       /vaft_grid)) * 100

      !print out daily information
      !first, normalize volumes to the watershed area (m3 --> mm)
      vbef_grid = (vbef_grid / (bsn%area_tot_ha*10000.)) * 1000.
      vaft_grid = (vaft_grid / (bsn%area_tot_ha*10000.)) * 1000.
      rech_grid = (rech_grid / (bsn%area_tot_ha*10000.)) * 1000.
      gwet_grid = (gwet_grid / (bsn%area_tot_ha*10000.)) * 1000.
      gwsw_grid = (gwsw_grid / (bsn%area_tot_ha*10000.)) * 1000.
      swgw_grid = (swgw_grid / (bsn%area_tot_ha*10000.)) * 1000.
      satx_grid = (satx_grid / (bsn%area_tot_ha*10000.)) * 1000.
      soil_grid = (soil_grid / (bsn%area_tot_ha*10000.)) * 1000.
      latl_grid = (latl_grid / (bsn%area_tot_ha*10000.)) * 1000.
      bndr_grid = (bndr_grid / (bsn%area_tot_ha*10000.)) * 1000.
      ppag_grid = (ppag_grid / (bsn%area_tot_ha*10000.)) * 1000.
      ppdf_grid = (ppdf_grid / (bsn%area_tot_ha*10000.)) * 1000.
      ppex_grid = (ppex_grid / (bsn%area_tot_ha*10000.)) * 1000.
      tile_grid = (tile_grid / (bsn%area_tot_ha*10000.)) * 1000.
      resv_grid = (resv_grid / (bsn%area_tot_ha*10000.)) * 1000.
      wetl_grid = (wetl_grid / (bsn%area_tot_ha*10000.)) * 1000.
      canl_grid = (canl_grid / (bsn%area_tot_ha*10000.)) * 1000.
      fpln_grid = (fpln_grid / (bsn%area_tot_ha*10000.)) * 1000.
      pond_grid = (pond_grid / (bsn%area_tot_ha*10000.)) * 1000.
      phyt_grid = (phyt_grid / (bsn%area_tot_ha*10000.)) * 1000.
      frac_sat = real(satx_count) / real(num_active)
      if(gwflag_day == 1) then
        write(out_gwbal,8100) time%day,time%mo,time%day_mo,time%yrc, &
          "       1","       1",bsn%name,gw_time_step, &
          vbef_grid,vaft_grid,rech_grid,gwet_grid,gwsw_grid,swgw_grid, &
          satx_grid,soil_grid,latl_grid,bndr_grid,ppag_grid,ppex_grid, &
          tile_grid,resv_grid,wetl_grid,canl_grid,fpln_grid,pond_grid, &
          phyt_grid,mass_error,frac_sat,depth_wt_avg,ppdf_grid
      endif

      !add daily water balance volumes to monthly values
      gw_hyd_grid_mo%chng = gw_hyd_grid_mo%chng + (vaft_grid-vbef_grid)
      gw_hyd_grid_mo%rech = gw_hyd_grid_mo%rech + rech_grid
      gw_hyd_grid_mo%gwet = gw_hyd_grid_mo%gwet + gwet_grid
      gw_hyd_grid_mo%gwsw = gw_hyd_grid_mo%gwsw + gwsw_grid
      gw_hyd_grid_mo%swgw = gw_hyd_grid_mo%swgw + swgw_grid
      gw_hyd_grid_mo%satx = gw_hyd_grid_mo%satx + satx_grid
      gw_hyd_grid_mo%soil = gw_hyd_grid_mo%soil + soil_grid
      gw_hyd_grid_mo%latl = gw_hyd_grid_mo%latl + latl_grid
      gw_hyd_grid_mo%bndr = gw_hyd_grid_mo%bndr + bndr_grid
      gw_hyd_grid_mo%ppag = gw_hyd_grid_mo%ppag + ppag_grid
      gw_hyd_grid_mo%ppdf = gw_hyd_grid_mo%ppdf + ppdf_grid
      gw_hyd_grid_mo%ppex = gw_hyd_grid_mo%ppex + ppex_grid
      gw_hyd_grid_mo%tile = gw_hyd_grid_mo%tile + tile_grid
      gw_hyd_grid_mo%resv = gw_hyd_grid_mo%resv + resv_grid
      gw_hyd_grid_mo%wetl = gw_hyd_grid_mo%wetl + wetl_grid
      gw_hyd_grid_mo%canl = gw_hyd_grid_mo%canl + canl_grid
      gw_hyd_grid_mo%fpln = gw_hyd_grid_mo%fpln + fpln_grid
      gw_hyd_grid_mo%pond = gw_hyd_grid_mo%pond + pond_grid
      gw_hyd_grid_mo%phyt = gw_hyd_grid_mo%phyt + phyt_grid
      !add daily water balance volumes to yearly values
      gw_hyd_grid_yr%chng = gw_hyd_grid_yr%chng + (vaft_grid-vbef_grid)
      gw_hyd_grid_yr%rech = gw_hyd_grid_yr%rech + rech_grid
      gw_hyd_grid_yr%gwet = gw_hyd_grid_yr%gwet + gwet_grid
      gw_hyd_grid_yr%gwsw = gw_hyd_grid_yr%gwsw + gwsw_grid
      gw_hyd_grid_yr%swgw = gw_hyd_grid_yr%swgw + swgw_grid
      gw_hyd_grid_yr%satx = gw_hyd_grid_yr%satx + satx_grid
      gw_hyd_grid_yr%soil = gw_hyd_grid_yr%soil + soil_grid
      gw_hyd_grid_yr%latl = gw_hyd_grid_yr%latl + latl_grid
      gw_hyd_grid_yr%bndr = gw_hyd_grid_yr%bndr + bndr_grid
      gw_hyd_grid_yr%ppag = gw_hyd_grid_yr%ppag + ppag_grid
      gw_hyd_grid_yr%ppdf = gw_hyd_grid_yr%ppdf + ppdf_grid
      gw_hyd_grid_yr%ppex = gw_hyd_grid_yr%ppex + ppex_grid
      gw_hyd_grid_yr%tile = gw_hyd_grid_yr%tile + tile_grid
      gw_hyd_grid_yr%resv = gw_hyd_grid_yr%resv + resv_grid
      gw_hyd_grid_yr%wetl = gw_hyd_grid_yr%wetl + wetl_grid
      gw_hyd_grid_yr%canl = gw_hyd_grid_yr%canl + canl_grid
      gw_hyd_grid_yr%fpln = gw_hyd_grid_yr%fpln + fpln_grid
      gw_hyd_grid_yr%pond = gw_hyd_grid_yr%pond + pond_grid
      gw_hyd_grid_yr%phyt = gw_hyd_grid_yr%phyt + phyt_grid
      !add daily water balance volumes to total values
      gw_hyd_grid_aa%chng = gw_hyd_grid_aa%chng + (vaft_grid-vbef_grid)
      gw_hyd_grid_aa%rech = gw_hyd_grid_aa%rech + rech_grid
      gw_hyd_grid_aa%gwet = gw_hyd_grid_aa%gwet + gwet_grid
      gw_hyd_grid_aa%gwsw = gw_hyd_grid_aa%gwsw + gwsw_grid
      gw_hyd_grid_aa%swgw = gw_hyd_grid_aa%swgw + swgw_grid
      gw_hyd_grid_aa%satx = gw_hyd_grid_aa%satx + satx_grid
      gw_hyd_grid_aa%soil = gw_hyd_grid_aa%soil + soil_grid
      gw_hyd_grid_aa%latl = gw_hyd_grid_aa%latl + latl_grid
      gw_hyd_grid_aa%bndr = gw_hyd_grid_aa%bndr + bndr_grid
      gw_hyd_grid_aa%ppag = gw_hyd_grid_aa%ppag + ppag_grid
      gw_hyd_grid_aa%ppdf = gw_hyd_grid_aa%ppdf + ppdf_grid
      gw_hyd_grid_aa%ppex = gw_hyd_grid_aa%ppex + ppex_grid
      gw_hyd_grid_aa%tile = gw_hyd_grid_aa%tile + tile_grid
      gw_hyd_grid_aa%resv = gw_hyd_grid_aa%resv + resv_grid
      gw_hyd_grid_aa%wetl = gw_hyd_grid_aa%wetl + wetl_grid
      gw_hyd_grid_aa%canl = gw_hyd_grid_aa%canl + canl_grid
      gw_hyd_grid_aa%fpln = gw_hyd_grid_aa%fpln + fpln_grid
      gw_hyd_grid_aa%pond = gw_hyd_grid_aa%pond + pond_grid
      gw_hyd_grid_aa%phyt = gw_hyd_grid_aa%phyt + phyt_grid

      !groundwater heat balance output
      if(gw_heat_flag == 1) then
        !calculate values for entire grid (all cells)
        heat_hbef_grid = 0.
        heat_haft_grid = 0.
        heat_rech_grid = 0.
        heat_gwet_grid = 0.
        heat_gwsw_grid = 0.
        heat_swgw_grid = 0.
        heat_satx_grid = 0.
        heat_soil_grid = 0.
        heat_latl_grid = 0.
        heat_disp_grid = 0.
        heat_bndr_grid = 0.
        heat_ppag_grid = 0.
        heat_ppex_grid = 0.
        heat_tile_grid = 0.
        heat_resv_grid = 0.
        heat_wetl_grid = 0.
        heat_canl_grid = 0.
        heat_fpln_grid = 0.
        heat_pond_grid = 0.
        do i=1,ncell
          if(gw_state(i)%stat == 1) then
            heat_hbef_grid = heat_hbef_grid + gwheat_state(i)%hbef
            heat_haft_grid = heat_haft_grid + gwheat_state(i)%haft
            heat_rech_grid = heat_rech_grid + gw_heat_ss(i)%rech
            heat_gwet_grid = heat_gwet_grid + gw_heat_ss(i)%gwet
            heat_gwsw_grid = heat_gwsw_grid + gw_heat_ss(i)%gwsw
            heat_swgw_grid = heat_swgw_grid + gw_heat_ss(i)%swgw
            heat_satx_grid = heat_satx_grid + gw_heat_ss(i)%satx
            heat_soil_grid = heat_soil_grid + gw_heat_ss(i)%soil
            heat_latl_grid = heat_latl_grid + gw_heat_ss(i)%latl
            heat_disp_grid = heat_disp_grid + gw_heat_ss(i)%disp
            heat_bndr_grid = heat_bndr_grid + gw_heat_ss(i)%bndr
            heat_ppag_grid = heat_ppag_grid + gw_heat_ss(i)%ppag
            heat_ppex_grid = heat_ppex_grid + gw_heat_ss(i)%ppex
            heat_tile_grid = heat_tile_grid + gw_heat_ss(i)%tile
            heat_resv_grid = heat_resv_grid + gw_heat_ss(i)%resv
            heat_wetl_grid = heat_wetl_grid + gw_heat_ss(i)%wetl
            heat_canl_grid = heat_canl_grid + gw_heat_ss(i)%canl
            heat_fpln_grid = heat_fpln_grid + gw_heat_ss(i)%fpln
            heat_pond_grid = heat_pond_grid + gw_heat_ss(i)%pond
          endif
        enddo
        mass_error = (1-((heat_hbef_grid + heat_rech_grid + heat_gwet_grid + heat_gwsw_grid + &
                          heat_swgw_grid + heat_satx_grid + heat_soil_grid + heat_latl_grid + &
                          heat_disp_grid + heat_bndr_grid + heat_ppag_grid + heat_ppex_grid + &
                          heat_tile_grid + heat_resv_grid + heat_wetl_grid + heat_canl_grid + &
                          heat_fpln_grid + heat_pond_grid) / heat_haft_grid)) * 100
        !print out daily information
        !first, divide by 1,000,000 to get MJ
        heat_hbef_grid = heat_hbef_grid / 1000000.
        heat_haft_grid = heat_haft_grid / 1000000.
        heat_rech_grid = heat_rech_grid / 1000000.
        heat_gwet_grid = heat_gwet_grid / 1000000.
        heat_gwsw_grid = heat_gwsw_grid / 1000000.
        heat_swgw_grid = heat_swgw_grid / 1000000.
        heat_satx_grid = heat_satx_grid / 1000000.
        heat_soil_grid = heat_soil_grid / 1000000.
        heat_latl_grid = heat_latl_grid / 1000000.
        heat_disp_grid = heat_disp_grid / 1000000.
        heat_bndr_grid = heat_bndr_grid / 1000000.
        heat_ppag_grid = heat_ppag_grid / 1000000.
        heat_ppex_grid = heat_ppex_grid / 1000000.
        heat_tile_grid = heat_tile_grid / 1000000.
        heat_resv_grid = heat_resv_grid / 1000000.
        heat_wetl_grid = heat_wetl_grid / 1000000.
        heat_canl_grid = heat_canl_grid / 1000000.
        heat_fpln_grid = heat_fpln_grid / 1000000.
        heat_pond_grid = heat_pond_grid / 1000000.
        if(gwflag_day == 1) then
          write(out_heatbal_dy,8100) time%day,time%mo,time%day_mo, &
            time%yrc,"       1","       1",bsn%name,gw_time_step, &
            heat_hbef_grid,heat_haft_grid,heat_rech_grid,heat_gwet_grid, &
            heat_gwsw_grid,heat_swgw_grid,heat_satx_grid,heat_soil_grid, &
            heat_latl_grid,heat_disp_grid,heat_bndr_grid,heat_ppag_grid, &
            heat_ppex_grid,heat_tile_grid,heat_resv_grid,heat_wetl_grid, &
            heat_canl_grid,heat_fpln_grid,heat_pond_grid, &
            mass_error,temp_avg
        endif
        !add daily heat fluxes to yearly values
        gw_heat_grid_yr%chng = gw_heat_grid_yr%chng + (heat_haft_grid-heat_hbef_grid)
        gw_heat_grid_yr%rech = gw_heat_grid_yr%rech + heat_rech_grid
        gw_heat_grid_yr%gwet = gw_heat_grid_yr%gwet + heat_gwet_grid
        gw_heat_grid_yr%gwsw = gw_heat_grid_yr%gwsw + heat_gwsw_grid
        gw_heat_grid_yr%swgw = gw_heat_grid_yr%swgw + heat_swgw_grid
        gw_heat_grid_yr%satx = gw_heat_grid_yr%satx + heat_satx_grid
        gw_heat_grid_yr%soil = gw_heat_grid_yr%soil + heat_soil_grid
        gw_heat_grid_yr%latl = gw_heat_grid_yr%latl + heat_latl_grid
        gw_heat_grid_yr%disp = gw_heat_grid_yr%disp + heat_disp_grid
        gw_heat_grid_yr%bndr = gw_heat_grid_yr%bndr + heat_bndr_grid
        gw_heat_grid_yr%ppag = gw_heat_grid_yr%ppag + heat_ppag_grid
        gw_heat_grid_yr%ppex = gw_heat_grid_yr%ppex + heat_ppex_grid
        gw_heat_grid_yr%tile = gw_heat_grid_yr%tile + heat_tile_grid
        gw_heat_grid_yr%resv = gw_heat_grid_yr%resv + heat_resv_grid
        gw_heat_grid_yr%wetl = gw_heat_grid_yr%wetl + heat_wetl_grid
        gw_heat_grid_yr%canl = gw_heat_grid_yr%canl + heat_canl_grid
        gw_heat_grid_yr%fpln = gw_heat_grid_yr%fpln + heat_fpln_grid
        gw_heat_grid_yr%pond = gw_heat_grid_yr%pond + heat_pond_grid
        !add daily heat fluxes to total values
        gw_heat_grid_aa%chng = gw_heat_grid_aa%chng + (heat_haft_grid-heat_hbef_grid)
        gw_heat_grid_aa%rech = gw_heat_grid_aa%rech + heat_rech_grid
        gw_heat_grid_aa%gwet = gw_heat_grid_aa%gwet + heat_gwet_grid
        gw_heat_grid_aa%gwsw = gw_heat_grid_aa%gwsw + heat_gwsw_grid
        gw_heat_grid_aa%swgw = gw_heat_grid_aa%swgw + heat_swgw_grid
        gw_heat_grid_aa%satx = gw_heat_grid_aa%satx + heat_satx_grid
        gw_heat_grid_aa%soil = gw_heat_grid_aa%soil + heat_soil_grid
        gw_heat_grid_aa%latl = gw_heat_grid_aa%latl + heat_latl_grid
        gw_heat_grid_aa%disp = gw_heat_grid_aa%disp + heat_disp_grid
        gw_heat_grid_aa%bndr = gw_heat_grid_aa%bndr + heat_bndr_grid
        gw_heat_grid_aa%ppag = gw_heat_grid_aa%ppag + heat_ppag_grid
        gw_heat_grid_aa%ppex = gw_heat_grid_aa%ppex + heat_ppex_grid
        gw_heat_grid_aa%tile = gw_heat_grid_aa%tile + heat_tile_grid
        gw_heat_grid_aa%resv = gw_heat_grid_aa%resv + heat_resv_grid
        gw_heat_grid_aa%wetl = gw_heat_grid_aa%wetl + heat_wetl_grid
        gw_heat_grid_aa%canl = gw_heat_grid_aa%canl + heat_canl_grid
        gw_heat_grid_aa%fpln = gw_heat_grid_aa%fpln + heat_fpln_grid
        gw_heat_grid_aa%pond = gw_heat_grid_aa%pond + heat_pond_grid
      endif

      !solute mass balance output
      if(gw_solute_flag == 1) then

        !loop through the solutes
        do s=1,gw_nsolute
          sol_grid_mbef = 0.
          sol_grid_maft = 0.
          sol_grid_rech = 0.
          sol_grid_gwsw = 0.
          sol_grid_swgw = 0.
          sol_grid_satx = 0.
          sol_grid_soil = 0.
          sol_grid_advn = 0.
          sol_grid_disp = 0.
          sol_grid_rcti = 0.
          sol_grid_rcto = 0.
          sol_grid_minl = 0.
          sol_grid_sorb = 0.
          sol_grid_ppag = 0.
          sol_grid_ppex = 0.
          sol_grid_tile = 0.
          sol_grid_resv = 0.
          sol_grid_wetl = 0.
          sol_grid_canl = 0.
          sol_grid_fpln = 0.
          sol_grid_pond = 0.
          !add up mass for the grid (convert g-->kg)
          do i=1,ncell
            if(gw_state(i)%stat == 1) then
              sol_grid_mbef = sol_grid_mbef + (gwsol_state(i)%solute(s)%mbef / 1000.)
              sol_grid_maft = sol_grid_maft + (gwsol_state(i)%solute(s)%maft / 1000.)
              sol_grid_rech = sol_grid_rech + (gwsol_ss(i)%solute(s)%rech / 1000.)
              sol_grid_gwsw = sol_grid_gwsw + (gwsol_ss(i)%solute(s)%gwsw / 1000.)
              sol_grid_swgw = sol_grid_swgw + (gwsol_ss(i)%solute(s)%swgw / 1000.)
              sol_grid_satx = sol_grid_satx + (gwsol_ss(i)%solute(s)%satx / 1000.)
              sol_grid_advn = sol_grid_advn + (gwsol_ss(i)%solute(s)%advn / 1000.)
              sol_grid_disp = sol_grid_disp + (gwsol_ss(i)%solute(s)%disp / 1000.)
              sol_grid_rcti = sol_grid_rcti + (gwsol_ss(i)%solute(s)%rcti / 1000.)
              sol_grid_rcto = sol_grid_rcto + (gwsol_ss(i)%solute(s)%rcto / 1000.)
              sol_grid_minl = sol_grid_minl + (gwsol_ss(i)%solute(s)%minl / 1000.)
              sol_grid_sorb = sol_grid_sorb + (gwsol_ss(i)%solute(s)%sorb / 1000.)
              sol_grid_ppag = sol_grid_ppag + (gwsol_ss(i)%solute(s)%ppag / 1000.)
              sol_grid_ppex = sol_grid_ppex + (gwsol_ss(i)%solute(s)%ppex / 1000.)
              sol_grid_tile = sol_grid_tile + (gwsol_ss(i)%solute(s)%tile / 1000.)
              sol_grid_soil = sol_grid_soil + (gwsol_ss(i)%solute(s)%soil / 1000.)
              sol_grid_resv = sol_grid_resv + (gwsol_ss(i)%solute(s)%resv / 1000.)
              sol_grid_wetl = sol_grid_wetl + (gwsol_ss(i)%solute(s)%wetl / 1000.)
              sol_grid_canl = sol_grid_canl + (gwsol_ss(i)%solute(s)%canl / 1000.)
              sol_grid_fpln = sol_grid_fpln + (gwsol_ss(i)%solute(s)%fpln / 1000.)
              sol_grid_pond = sol_grid_pond + (gwsol_ss(i)%solute(s)%pond / 1000.)
            endif
          enddo
          sol_grid_sorb = sol_grid_sorb * (-1) !leaving groundwater (sorbing to aquifer material)
          !calculate mass error
          mass_error = 0.
          if(sol_grid_maft > 0) then
            mass_error = (1- ((sol_grid_mbef + sol_grid_rech + sol_grid_gwsw + sol_grid_swgw + &
                               sol_grid_satx + sol_grid_advn + sol_grid_disp + &
                               sol_grid_rcti + sol_grid_rcto + sol_grid_minl + &
                               sol_grid_ppag + sol_grid_ppex + sol_grid_tile + sol_grid_soil + &
                               sol_grid_resv + sol_grid_wetl + sol_grid_canl + sol_grid_fpln + sol_grid_pond) / sol_grid_maft)) * 100
          endif
          !print out daily values for the solute
          if(gwflag_day == 1) then
            write(out_solbal_dy+s,8100) time%day,time%mo,time%day_mo, &
              time%yrc,"       1","       1",bsn%name,gw_time_step, &
              sol_grid_mbef,sol_grid_maft,sol_grid_rech,sol_grid_gwsw,sol_grid_swgw, &
              sol_grid_satx,sol_grid_soil,sol_grid_advn,sol_grid_disp, &
              sol_grid_rcti,sol_grid_rcto,sol_grid_minl, &
              sol_grid_sorb,sol_grid_ppag,sol_grid_ppex,sol_grid_tile,sol_grid_resv, &
              sol_grid_wetl,sol_grid_canl,sol_grid_fpln,sol_grid_pond, &
              mass_error
          endif
          !add grid values to monthly, yearly, and total mass values
          !monthly (kg)
          sol_grid_chng_mo(s) = sol_grid_chng_mo(s) + (sol_grid_maft-sol_grid_mbef)
          sol_grid_rech_mo(s) = sol_grid_rech_mo(s) + sol_grid_rech
          sol_grid_gwsw_mo(s) = sol_grid_gwsw_mo(s) + sol_grid_gwsw
          sol_grid_swgw_mo(s) = sol_grid_swgw_mo(s) + sol_grid_swgw
          sol_grid_satx_mo(s) = sol_grid_satx_mo(s) + sol_grid_satx
          sol_grid_advn_mo(s) = sol_grid_advn_mo(s) + sol_grid_advn
          sol_grid_disp_mo(s) = sol_grid_disp_mo(s) + sol_grid_disp
          sol_grid_rcti_mo(s) = sol_grid_rcti_mo(s) + sol_grid_rcti
          sol_grid_rcto_mo(s) = sol_grid_rcto_mo(s) + sol_grid_rcto
          sol_grid_minl_mo(s) = sol_grid_minl_mo(s) + sol_grid_minl
          sol_grid_sorb_mo(s) = sol_grid_sorb_mo(s) + sol_grid_sorb
          sol_grid_ppag_mo(s) = sol_grid_ppag_mo(s) + sol_grid_ppag
          sol_grid_ppex_mo(s) = sol_grid_ppex_mo(s) + sol_grid_ppex
          sol_grid_tile_mo(s) = sol_grid_tile_mo(s) + sol_grid_tile
          sol_grid_soil_mo(s) = sol_grid_soil_mo(s) + sol_grid_soil
          sol_grid_resv_mo(s) = sol_grid_resv_mo(s) + sol_grid_resv
          sol_grid_wetl_mo(s) = sol_grid_wetl_mo(s) + sol_grid_wetl
          sol_grid_canl_mo(s) = sol_grid_canl_mo(s) + sol_grid_canl
          sol_grid_fpln_mo(s) = sol_grid_fpln_mo(s) + sol_grid_fpln
          sol_grid_pond_mo(s) = sol_grid_pond_mo(s) + sol_grid_pond
          !yearly (kg)
          sol_grid_chng_yr(s) = sol_grid_chng_yr(s) + (sol_grid_maft-sol_grid_mbef)
          sol_grid_rech_yr(s) = sol_grid_rech_yr(s) + sol_grid_rech
          sol_grid_gwsw_yr(s) = sol_grid_gwsw_yr(s) + sol_grid_gwsw
          sol_grid_swgw_yr(s) = sol_grid_swgw_yr(s) + sol_grid_swgw
          sol_grid_satx_yr(s) = sol_grid_satx_yr(s) + sol_grid_satx
          sol_grid_advn_yr(s) = sol_grid_advn_yr(s) + sol_grid_advn
          sol_grid_disp_yr(s) = sol_grid_disp_yr(s) + sol_grid_disp
          sol_grid_rcti_yr(s) = sol_grid_rcti_yr(s) + sol_grid_rcti
          sol_grid_rcto_yr(s) = sol_grid_rcto_yr(s) + sol_grid_rcto
          sol_grid_minl_yr(s) = sol_grid_minl_yr(s) + sol_grid_minl
          sol_grid_sorb_yr(s) = sol_grid_sorb_yr(s) + sol_grid_sorb
          sol_grid_ppag_yr(s) = sol_grid_ppag_yr(s) + sol_grid_ppag
          sol_grid_ppex_yr(s) = sol_grid_ppex_yr(s) + sol_grid_ppex
          sol_grid_tile_yr(s) = sol_grid_tile_yr(s) + sol_grid_tile
          sol_grid_soil_yr(s) = sol_grid_soil_yr(s) + sol_grid_soil
          sol_grid_resv_yr(s) = sol_grid_resv_yr(s) + sol_grid_resv
          sol_grid_wetl_yr(s) = sol_grid_wetl_yr(s) + sol_grid_wetl
          sol_grid_canl_yr(s) = sol_grid_canl_yr(s) + sol_grid_canl
          sol_grid_fpln_yr(s) = sol_grid_fpln_yr(s) + sol_grid_fpln
          sol_grid_pond_yr(s) = sol_grid_pond_yr(s) + sol_grid_pond
          !total (kg)
          sol_grid_chng_tt(s) = sol_grid_chng_tt(s) + (sol_grid_maft-sol_grid_mbef)
          sol_grid_rech_tt(s) = sol_grid_rech_tt(s) + sol_grid_rech
          sol_grid_gwsw_tt(s) = sol_grid_gwsw_tt(s) + sol_grid_gwsw
          sol_grid_swgw_tt(s) = sol_grid_swgw_tt(s) + sol_grid_swgw
          sol_grid_satx_tt(s) = sol_grid_satx_tt(s) + sol_grid_satx
          sol_grid_advn_tt(s) = sol_grid_advn_tt(s) + sol_grid_advn
          sol_grid_disp_tt(s) = sol_grid_disp_tt(s) + sol_grid_disp
          sol_grid_rcti_tt(s) = sol_grid_rcti_tt(s) + sol_grid_rcti
          sol_grid_rcto_tt(s) = sol_grid_rcto_tt(s) + sol_grid_rcto
          sol_grid_minl_tt(s) = sol_grid_minl_tt(s) + sol_grid_minl
          sol_grid_sorb_tt(s) = sol_grid_sorb_tt(s) + sol_grid_sorb
          sol_grid_ppag_tt(s) = sol_grid_ppag_tt(s) + sol_grid_ppag
          sol_grid_ppex_tt(s) = sol_grid_ppex_tt(s) + sol_grid_ppex
          sol_grid_tile_tt(s) = sol_grid_tile_tt(s) + sol_grid_tile
          sol_grid_soil_tt(s) = sol_grid_soil_tt(s) + sol_grid_soil
          sol_grid_resv_tt(s) = sol_grid_resv_tt(s) + sol_grid_resv
          sol_grid_wetl_tt(s) = sol_grid_wetl_tt(s) + sol_grid_wetl
          sol_grid_canl_tt(s) = sol_grid_canl_tt(s) + sol_grid_canl
          sol_grid_fpln_tt(s) = sol_grid_fpln_tt(s) + sol_grid_fpln
          sol_grid_pond_tt(s) = sol_grid_pond_tt(s) + sol_grid_pond
        enddo !go to next solute
      endif !check for solute transport


      !--- cell-level daily long-format output ---
      if(gwflag_day == 1) then
        do i=1,ncell
          if(gw_state(i)%stat == 1) then
            write(out_gwcell_day,140) time%day, time%mo, time%day_mo, &
              time%yrc, i, cell_row(i)*grid_ncol+cell_col(i), &
              'gw_', i, &
              gw_state(i)%head, &
              gw_state(i)%elev - gw_state(i)%head, &
              gw_hyd_ss(i)%rech, gw_hyd_ss(i)%gwet, &
              gw_hyd_ss(i)%gwsw, gw_hyd_ss(i)%swgw, &
              gw_hyd_ss(i)%satx, gw_hyd_ss(i)%soil, &
              gw_hyd_ss(i)%latl, &
              gw_hyd_ss(i)%ppag, gw_hyd_ss(i)%ppex, &
              gw_hyd_ss(i)%tile, gw_hyd_ss(i)%resv, &
              gw_hyd_ss(i)%wetl, gw_hyd_ss(i)%fpln, &
              gw_hyd_ss(i)%canl, gw_hyd_ss(i)%pond, &
              gw_hyd_ss(i)%phyt
          endif
        enddo
      endif


      !--- HRU pumping daily long-format output ---
      if(gwflag_pump == 1) then
        do i=1,sp_ob%hru
          if(hru_pump(i) > 0.) then
            iob = sp_ob1%hru + i - 1
            write(out_hru_pump_day,8101) time%day,time%mo,time%day_mo, &
              time%yrc,i,ob(iob)%gis_id,ob(iob)%name,hru_pump(i)
          endif
        enddo
      endif

      !format statements (subroutine-local)
119   format(i8,i8,i8,1000(f12.3))
130   format(i8,i8,1000(e13.4))
140   format(i8,i6,i6,i8,i8,i10,4x,a4,i4.4,2f13.3,16e13.4)
8100  format(4i6,2a,2x,a16,f10.3,50e13.4)
8101  format(4i6,2i8,a18,e13.4)
8102  format(4i6,2i8,a18,5e13.4)

      return
      end subroutine gwflow_output_day


      subroutine gwflow_output_mon

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes monthly gwflow output in SWAT+ long format:
!!    one row per active cell with average head, wtdepth, and average
!!    daily flow rates; basin-level water/heat/solute balance; HRU pumping

      use gwflow_module
      use hydrograph_module
      use sd_channel_module
      use time_module
      use basin_module, only : bsn

      implicit none

      integer :: i, j, k, s, iob
      integer :: gis_id
      real :: wtdepth
      real :: day_mo_r
      real :: obs_temp, obs_no3, obs_p
      character(len=16) :: obs_name

      if (time%end_mo == 1) then

      day_mo_r = real(time%day_mo)

        !monthly average groundwater head -----------------------------------------------
        do i=1,ncell
          gw_state(i)%hdmo = gw_state(i)%hdmo / day_mo_r
        enddo
        !monthly average groundwater temperature ----------------------------------------
        if(gw_heat_flag == 1) then
          do i=1,ncell
            gwheat_state(i)%tpmo = gwheat_state(i)%tpmo / day_mo_r
          enddo
          !zero out for next month
          do i=1,ncell
            gwheat_state(i)%tpmo = 0.
          enddo
        endif

        !monthly average solute concentration -------------------------------------------
        if(gw_solute_flag == 1) then
          do s=1,gw_nsolute
            do i=1,ncell
              gwsol_state(i)%solute(s)%cnmo = gwsol_state(i)%solute(s)%cnmo / day_mo_r
            enddo
            !zero out for next month
            do i=1,ncell
              gwsol_state(i)%solute(s)%cnmo = 0.
            enddo
          enddo !next solute
        endif

        !pumping (irrigation) (for HRUs) -- monthly long-format output ----------------
        if(gwflag_pump == 1) then
          do i=1,sp_ob%hru
            if(hru_pump_mo(i) > 0.) then
              iob = sp_ob1%hru + i - 1
              write(out_hru_pump_mo,8101) time%day,time%mo,time%day_mo, &
                time%yrc,i,ob(iob)%gis_id,ob(iob)%name,hru_pump_mo(i)
            endif
          enddo
        endif
        hru_pump_mo = 0.

        !compute average daily groundwater fluxes (m3/day) for the month ----------------
        do i=1,ncell
          gw_hyd_ss_mo(i)%rech = gw_hyd_ss_mo(i)%rech / day_mo_r
          gw_hyd_ss_mo(i)%gwet = gw_hyd_ss_mo(i)%gwet / day_mo_r
          gw_hyd_ss_mo(i)%gwsw = gw_hyd_ss_mo(i)%gwsw / day_mo_r
          gw_hyd_ss_mo(i)%swgw = gw_hyd_ss_mo(i)%swgw / day_mo_r
          gw_hyd_ss_mo(i)%satx = gw_hyd_ss_mo(i)%satx / day_mo_r
          gw_hyd_ss_mo(i)%soil = gw_hyd_ss_mo(i)%soil / day_mo_r
          gw_hyd_ss_mo(i)%latl = gw_hyd_ss_mo(i)%latl / day_mo_r
          gw_hyd_ss_mo(i)%bndr = gw_hyd_ss_mo(i)%bndr / day_mo_r
          gw_hyd_ss_mo(i)%ppag = gw_hyd_ss_mo(i)%ppag / day_mo_r
          gw_hyd_ss_mo(i)%ppdf = gw_hyd_ss_mo(i)%ppdf / day_mo_r
          gw_hyd_ss_mo(i)%ppex = gw_hyd_ss_mo(i)%ppex / day_mo_r
          gw_hyd_ss_mo(i)%tile = gw_hyd_ss_mo(i)%tile / day_mo_r
          gw_hyd_ss_mo(i)%resv = gw_hyd_ss_mo(i)%resv / day_mo_r
          gw_hyd_ss_mo(i)%wetl = gw_hyd_ss_mo(i)%wetl / day_mo_r
          gw_hyd_ss_mo(i)%canl = gw_hyd_ss_mo(i)%canl / day_mo_r
          gw_hyd_ss_mo(i)%fpln = gw_hyd_ss_mo(i)%fpln / day_mo_r
          gw_hyd_ss_mo(i)%pond = gw_hyd_ss_mo(i)%pond / day_mo_r
          gw_hyd_ss_mo(i)%phyt = gw_hyd_ss_mo(i)%phyt / day_mo_r
        enddo

        !--- cell-level monthly long-format output (one row per active cell) ---
        if(gwflag_mon == 1) then
          do i=1,ncell
            if(gw_state(i)%stat == 1) then
              gis_id = (cell_row(i)-1)*grid_ncol + cell_col(i)
              wtdepth = gw_state(i)%elev - gw_state(i)%hdmo
              write(out_gwcell_mon,140) time%day, time%mo, &
                time%day_mo, time%yrc, i, gis_id, &
                'gw_', i, &
                gw_state(i)%hdmo, wtdepth, &
                gw_hyd_ss_mo(i)%rech, gw_hyd_ss_mo(i)%gwet, &
                gw_hyd_ss_mo(i)%gwsw, gw_hyd_ss_mo(i)%swgw, &
                gw_hyd_ss_mo(i)%satx, gw_hyd_ss_mo(i)%soil, &
                gw_hyd_ss_mo(i)%latl, &
                gw_hyd_ss_mo(i)%ppag, gw_hyd_ss_mo(i)%ppex, &
                gw_hyd_ss_mo(i)%tile, gw_hyd_ss_mo(i)%resv, &
                gw_hyd_ss_mo(i)%wetl, gw_hyd_ss_mo(i)%fpln, &
                gw_hyd_ss_mo(i)%canl, gw_hyd_ss_mo(i)%pond, &
                gw_hyd_ss_mo(i)%phyt
            endif
          enddo
        endif

        !--- obs well monthly output ---
        if(gwflag_obs == 1 .and. gw_num_obs_wells > 0) then
          do k=1,gw_num_obs_wells
            i = gw_obs_cells(k)
            obs_temp = -99.; obs_no3 = -99.; obs_p = -99.
            if(gw_heat_flag == 1) obs_temp = gwheat_state(i)%tpmo
            if(gw_solute_flag == 1) then
              obs_no3 = gwsol_state(i)%solute(1)%cnmo
              obs_p = gwsol_state(i)%solute(2)%cnmo
            endif
            write(obs_name,'(a4,i4.4)') 'obs_',k
            write(out_gwobs_mon,8102) time%day,time%mo,time%day_mo, &
              time%yrc,k,gw_obs_cells(k),obs_name, &
              gw_state(i)%hdmo, gw_state(i)%elev - gw_state(i)%hdmo, &
              obs_temp, obs_no3, obs_p
          enddo
        endif

        !zero out head average and flux sums for next month
        do i=1,ncell
          gw_state(i)%hdmo = 0.
          gw_hyd_ss_mo(i)%rech = 0.
          gw_hyd_ss_mo(i)%gwet = 0.
          gw_hyd_ss_mo(i)%gwsw = 0.
          gw_hyd_ss_mo(i)%swgw = 0.
          gw_hyd_ss_mo(i)%satx = 0.
          gw_hyd_ss_mo(i)%soil = 0.
          gw_hyd_ss_mo(i)%latl = 0.
          gw_hyd_ss_mo(i)%bndr = 0.
          gw_hyd_ss_mo(i)%ppag = 0.
          gw_hyd_ss_mo(i)%ppdf = 0.
          gw_hyd_ss_mo(i)%ppex = 0.
          gw_hyd_ss_mo(i)%tile = 0.
          gw_hyd_ss_mo(i)%resv = 0.
          gw_hyd_ss_mo(i)%wetl = 0.
          gw_hyd_ss_mo(i)%canl = 0.
          gw_hyd_ss_mo(i)%fpln = 0.
          gw_hyd_ss_mo(i)%pond = 0.
          gw_hyd_ss_mo(i)%phyt = 0.
        enddo

        !solute monthly zeroing
        if(gw_solute_flag == 1) then
          do i=1,ncell
            do s=1,gw_nsolute
              gwsol_ss_sum_mo(i)%solute(s)%rech = 0.
              gwsol_ss_sum_mo(i)%solute(s)%gwsw = 0.
              gwsol_ss_sum_mo(i)%solute(s)%swgw = 0.
              gwsol_ss_sum_mo(i)%solute(s)%satx = 0.
              gwsol_ss_sum_mo(i)%solute(s)%soil = 0.
              gwsol_ss_sum_mo(i)%solute(s)%ppag = 0.
              gwsol_ss_sum_mo(i)%solute(s)%ppex = 0.
              gwsol_ss_sum_mo(i)%solute(s)%tile = 0.
              gwsol_ss_sum_mo(i)%solute(s)%resv = 0.
              gwsol_ss_sum_mo(i)%solute(s)%wetl = 0.
              gwsol_ss_sum_mo(i)%solute(s)%canl = 0.
              gwsol_ss_sum_mo(i)%solute(s)%fpln = 0.
              gwsol_ss_sum_mo(i)%solute(s)%pond = 0.
              gwsol_ss_sum_mo(i)%solute(s)%advn = 0.
              gwsol_ss_sum_mo(i)%solute(s)%disp = 0.
              gwsol_ss_sum_mo(i)%solute(s)%rcti = 0.
              gwsol_ss_sum_mo(i)%solute(s)%rcto = 0.
              gwsol_ss_sum_mo(i)%solute(s)%minl = 0.
              gwsol_ss_sum_mo(i)%solute(s)%sorb = 0.
            enddo
          enddo
        endif

        !monthly groundwater balance (basin) --------------------------------------------
        if(gwflag_mon == 1) then
          write(out_gwbal_mon,8100) time%day,time%mo,time%day_mo, &
            time%yrc,"       1","       1",bsn%name, &
            gw_hyd_grid_mo%chng,gw_hyd_grid_mo%rech,gw_hyd_grid_mo%gwet, &
            gw_hyd_grid_mo%gwsw,gw_hyd_grid_mo%swgw, &
            gw_hyd_grid_mo%satx,gw_hyd_grid_mo%soil,gw_hyd_grid_mo%latl, &
            gw_hyd_grid_mo%bndr,gw_hyd_grid_mo%ppag, &
            gw_hyd_grid_mo%ppex,gw_hyd_grid_mo%tile,gw_hyd_grid_mo%resv, &
            gw_hyd_grid_mo%wetl,gw_hyd_grid_mo%canl, &
            gw_hyd_grid_mo%fpln,gw_hyd_grid_mo%pond,gw_hyd_grid_mo%phyt, &
            gw_hyd_grid_mo%ppdf
        endif
        !zero out basin monthly accumulators
        gw_hyd_grid_mo%chng = 0.
        gw_hyd_grid_mo%rech = 0.
        gw_hyd_grid_mo%gwet = 0.
        gw_hyd_grid_mo%gwsw = 0.
        gw_hyd_grid_mo%swgw = 0.
        gw_hyd_grid_mo%satx = 0.
        gw_hyd_grid_mo%soil = 0.
        gw_hyd_grid_mo%latl = 0.
        gw_hyd_grid_mo%bndr = 0.
        gw_hyd_grid_mo%ppag = 0.
        gw_hyd_grid_mo%ppdf = 0.
        gw_hyd_grid_mo%ppex = 0.
        gw_hyd_grid_mo%tile = 0.
        gw_hyd_grid_mo%resv = 0.
        gw_hyd_grid_mo%wetl = 0.
        gw_hyd_grid_mo%canl = 0.
        gw_hyd_grid_mo%fpln = 0.
        gw_hyd_grid_mo%pond = 0.
        gw_hyd_grid_mo%phyt = 0.

        !solute mass values (basin) -----------------------------------------------------
        if(gw_solute_flag == 1) then
          do s=1,gw_nsolute
            if(gwflag_mon == 1) then
              write(out_solbal_mo+s,8100) time%day,time%mo,time%day_mo, &
                time%yrc,"       1","       1",bsn%name, &
                sol_grid_chng_mo(s),sol_grid_rech_mo(s), &
                sol_grid_gwsw_mo(s),sol_grid_swgw_mo(s), &
                sol_grid_satx_mo(s), &
                sol_grid_soil_mo(s),sol_grid_advn_mo(s), &
                sol_grid_disp_mo(s), &
                sol_grid_rcti_mo(s),sol_grid_rcto_mo(s), &
                sol_grid_minl_mo(s),sol_grid_sorb_mo(s), &
                sol_grid_ppag_mo(s),sol_grid_ppex_mo(s), &
                sol_grid_tile_mo(s),sol_grid_resv_mo(s), &
                sol_grid_wetl_mo(s), &
                sol_grid_canl_mo(s),sol_grid_fpln_mo(s), &
                sol_grid_pond_mo(s)
            endif
            sol_grid_chng_mo(s) = 0.
            sol_grid_rech_mo(s) = 0.
            sol_grid_gwsw_mo(s) = 0.
            sol_grid_swgw_mo(s) = 0.
            sol_grid_satx_mo(s) = 0.
            sol_grid_soil_mo(s) = 0.
            sol_grid_advn_mo(s) = 0.
            sol_grid_disp_mo(s) = 0.
            sol_grid_rcti_mo(s) = 0.
            sol_grid_rcto_mo(s) = 0.
            sol_grid_minl_mo(s) = 0.
            sol_grid_sorb_mo(s) = 0.
            sol_grid_ppag_mo(s) = 0.
            sol_grid_ppex_mo(s) = 0.
            sol_grid_tile_mo(s) = 0.
            sol_grid_resv_mo(s) = 0.
            sol_grid_wetl_mo(s) = 0.
            sol_grid_canl_mo(s) = 0.
            sol_grid_fpln_mo(s) = 0.
            sol_grid_pond_mo(s) = 0.
          enddo !go to next solute
        endif

      endif !end_mo

      !format statements (subroutine-local)
140   format(i8,i6,i6,i8,i8,i10,4x,a4,i4.4,2f13.3,16e13.4)
8100  format(4i6,2a,2x,a16,50e13.4)
8101  format(4i6,2i8,a18,e13.4)
8102  format(4i6,2i8,a18,5e13.4)

      return
      end subroutine gwflow_output_mon


      subroutine gwflow_output_yr

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes annual gwflow output in SWAT+ long format:
!!    one row per active cell with average head, wtdepth, and average
!!    daily flow rates; basin-level water/heat/solute balance; HRU pumping

      use gwflow_module
      use hydrograph_module
      use sd_channel_module
      use time_module
      use basin_module, only : bsn

      implicit none

      integer :: i, j, k, s, iob
      integer :: gis_id
      real :: wtdepth
      real :: day_yr_r
      real :: obs_temp, obs_no3, obs_p
      character(len=16) :: obs_name

      !--- only execute at end of year ---
      if(time%end_yr /= 1) return

      day_yr_r = real(time%day_end_yr)

      !annual average groundwater head ------------------------------------------------
      do i=1,ncell
        gw_state(i)%hdyr = gw_state(i)%hdyr / day_yr_r
      enddo

      !annual average groundwater temperature -----------------------------------------
      if(gw_heat_flag == 1) then
        do i=1,ncell
          gwheat_state(i)%tpyr = gwheat_state(i)%tpyr / day_yr_r
        enddo
        !zero out for next year
        do i=1,ncell
          gwheat_state(i)%tpyr = 0.
        enddo
      endif

      !annual average solute concentration --------------------------------------------
      if(gw_solute_flag == 1) then
        do s=1,gw_nsolute
          do i=1,ncell
            gwsol_state(i)%solute(s)%cnyr = gwsol_state(i)%solute(s)%cnyr / day_yr_r
          enddo
          !zero out for next year
          do i=1,ncell
            gwsol_state(i)%solute(s)%cnyr = 0.
          enddo
        enddo !next solute
      endif !check for solutes

      !compute average daily groundwater fluxes (m3/day) for the year -----------------
      do i=1,ncell
        gw_hyd_ss_yr(i)%rech = gw_hyd_ss_yr(i)%rech / day_yr_r
        gw_hyd_ss_yr(i)%gwet = gw_hyd_ss_yr(i)%gwet / day_yr_r
        gw_hyd_ss_yr(i)%gwsw = gw_hyd_ss_yr(i)%gwsw / day_yr_r
        gw_hyd_ss_yr(i)%swgw = gw_hyd_ss_yr(i)%swgw / day_yr_r
        gw_hyd_ss_yr(i)%satx = gw_hyd_ss_yr(i)%satx / day_yr_r
        gw_hyd_ss_yr(i)%soil = gw_hyd_ss_yr(i)%soil / day_yr_r
        gw_hyd_ss_yr(i)%latl = gw_hyd_ss_yr(i)%latl / day_yr_r
        gw_hyd_ss_yr(i)%bndr = gw_hyd_ss_yr(i)%bndr / day_yr_r
        gw_hyd_ss_yr(i)%ppag = gw_hyd_ss_yr(i)%ppag / day_yr_r
        gw_hyd_ss_yr(i)%ppdf = gw_hyd_ss_yr(i)%ppdf / day_yr_r
        gw_hyd_ss_yr(i)%ppex = gw_hyd_ss_yr(i)%ppex / day_yr_r
        gw_hyd_ss_yr(i)%tile = gw_hyd_ss_yr(i)%tile / day_yr_r
        gw_hyd_ss_yr(i)%resv = gw_hyd_ss_yr(i)%resv / day_yr_r
        gw_hyd_ss_yr(i)%wetl = gw_hyd_ss_yr(i)%wetl / day_yr_r
        gw_hyd_ss_yr(i)%canl = gw_hyd_ss_yr(i)%canl / day_yr_r
        gw_hyd_ss_yr(i)%fpln = gw_hyd_ss_yr(i)%fpln / day_yr_r
        gw_hyd_ss_yr(i)%pond = gw_hyd_ss_yr(i)%pond / day_yr_r
        gw_hyd_ss_yr(i)%phyt = gw_hyd_ss_yr(i)%phyt / day_yr_r
      enddo

      !--- cell-level annual long-format output (one row per active cell) ---
      if(gwflag_yr == 1) then
        do i=1,ncell
          if(gw_state(i)%stat == 1) then
            gis_id = (cell_row(i)-1)*grid_ncol + cell_col(i)
            wtdepth = gw_state(i)%elev - gw_state(i)%hdyr
            write(out_gwcell_yr,140) time%day, time%mo, &
              time%day_mo, time%yrc, i, gis_id, &
              'gw_', i, &
              gw_state(i)%hdyr, wtdepth, &
              gw_hyd_ss_yr(i)%rech, gw_hyd_ss_yr(i)%gwet, &
              gw_hyd_ss_yr(i)%gwsw, gw_hyd_ss_yr(i)%swgw, &
              gw_hyd_ss_yr(i)%satx, gw_hyd_ss_yr(i)%soil, &
              gw_hyd_ss_yr(i)%latl, &
              gw_hyd_ss_yr(i)%ppag, gw_hyd_ss_yr(i)%ppex, &
              gw_hyd_ss_yr(i)%tile, gw_hyd_ss_yr(i)%resv, &
              gw_hyd_ss_yr(i)%wetl, gw_hyd_ss_yr(i)%fpln, &
              gw_hyd_ss_yr(i)%canl, gw_hyd_ss_yr(i)%pond, &
              gw_hyd_ss_yr(i)%phyt
          endif
        enddo
      endif

      !accumulate yearly values into per-cell AA arrays (before zeroing)
      do i=1,ncell
        gw_head_sum_aa(i) = gw_head_sum_aa(i) + gw_state(i)%hdyr
        gw_hyd_ss_aa(i)%rech = gw_hyd_ss_aa(i)%rech + gw_hyd_ss_yr(i)%rech
        gw_hyd_ss_aa(i)%gwet = gw_hyd_ss_aa(i)%gwet + gw_hyd_ss_yr(i)%gwet
        gw_hyd_ss_aa(i)%gwsw = gw_hyd_ss_aa(i)%gwsw + gw_hyd_ss_yr(i)%gwsw
        gw_hyd_ss_aa(i)%swgw = gw_hyd_ss_aa(i)%swgw + gw_hyd_ss_yr(i)%swgw
        gw_hyd_ss_aa(i)%satx = gw_hyd_ss_aa(i)%satx + gw_hyd_ss_yr(i)%satx
        gw_hyd_ss_aa(i)%soil = gw_hyd_ss_aa(i)%soil + gw_hyd_ss_yr(i)%soil
        gw_hyd_ss_aa(i)%latl = gw_hyd_ss_aa(i)%latl + gw_hyd_ss_yr(i)%latl
        gw_hyd_ss_aa(i)%ppag = gw_hyd_ss_aa(i)%ppag + gw_hyd_ss_yr(i)%ppag
        gw_hyd_ss_aa(i)%ppex = gw_hyd_ss_aa(i)%ppex + gw_hyd_ss_yr(i)%ppex
        gw_hyd_ss_aa(i)%tile = gw_hyd_ss_aa(i)%tile + gw_hyd_ss_yr(i)%tile
        gw_hyd_ss_aa(i)%resv = gw_hyd_ss_aa(i)%resv + gw_hyd_ss_yr(i)%resv
        gw_hyd_ss_aa(i)%wetl = gw_hyd_ss_aa(i)%wetl + gw_hyd_ss_yr(i)%wetl
        gw_hyd_ss_aa(i)%fpln = gw_hyd_ss_aa(i)%fpln + gw_hyd_ss_yr(i)%fpln
        gw_hyd_ss_aa(i)%canl = gw_hyd_ss_aa(i)%canl + gw_hyd_ss_yr(i)%canl
        gw_hyd_ss_aa(i)%pond = gw_hyd_ss_aa(i)%pond + gw_hyd_ss_yr(i)%pond
        gw_hyd_ss_aa(i)%phyt = gw_hyd_ss_aa(i)%phyt + gw_hyd_ss_yr(i)%phyt
      enddo

      !--- obs well yearly output + AA accumulation ---
      if(gwflag_obs == 1 .and. gw_num_obs_wells > 0) then
        do k=1,gw_num_obs_wells
          i = gw_obs_cells(k)
          obs_temp = -99.; obs_no3 = -99.; obs_p = -99.
          if(gw_heat_flag == 1) then
            obs_temp = gwheat_state(i)%tpyr
            gw_obs_temp_aa(k) = gw_obs_temp_aa(k) + obs_temp
          endif
          if(gw_solute_flag == 1) then
            obs_no3 = gwsol_state(i)%solute(1)%cnyr
            obs_p = gwsol_state(i)%solute(2)%cnyr
            gw_obs_sol_aa(k,1) = gw_obs_sol_aa(k,1) + obs_no3
            gw_obs_sol_aa(k,2) = gw_obs_sol_aa(k,2) + obs_p
          endif
          write(obs_name,'(a4,i4.4)') 'obs_',k
          write(out_gwobs_yr,8102) time%day,time%mo,time%day_mo, &
            time%yrc,k,gw_obs_cells(k),obs_name, &
            gw_state(i)%hdyr, gw_state(i)%elev - gw_state(i)%hdyr, &
            obs_temp, obs_no3, obs_p
        enddo
      endif

      !zero out head average for next year
      do i=1,ncell
        gw_state(i)%hdyr = 0.
      enddo

      !compute average daily heat fluxes (MJ/day) for the year
      if(gw_heat_flag == 1) then
        do i=1,ncell
          gw_heat_ss_yr(i)%rech = (gw_heat_ss_yr(i)%rech/1000000.) / day_yr_r !J --> MJ
          gw_heat_ss_yr(i)%gwet = (gw_heat_ss_yr(i)%gwet/1000000.) / day_yr_r !J --> MJ
          gw_heat_ss_yr(i)%gwsw = (gw_heat_ss_yr(i)%gwsw/1000000.) / day_yr_r !J --> MJ
          gw_heat_ss_yr(i)%swgw = (gw_heat_ss_yr(i)%swgw/1000000.) / day_yr_r !J --> MJ
          gw_heat_ss_yr(i)%satx = (gw_heat_ss_yr(i)%satx/1000000.) / day_yr_r !J --> MJ
          gw_heat_ss_yr(i)%soil = (gw_heat_ss_yr(i)%soil/1000000.) / day_yr_r !J --> MJ
          gw_heat_ss_yr(i)%latl = (gw_heat_ss_yr(i)%latl/1000000.) / day_yr_r !J --> MJ
          gw_heat_ss_yr(i)%disp = (gw_heat_ss_yr(i)%disp/1000000.) / day_yr_r !J --> MJ
          gw_heat_ss_yr(i)%bndr = (gw_heat_ss_yr(i)%bndr/1000000.) / day_yr_r !J --> MJ
          gw_heat_ss_yr(i)%ppag = (gw_heat_ss_yr(i)%ppag/1000000.) / day_yr_r !J --> MJ
          gw_heat_ss_yr(i)%ppex = (gw_heat_ss_yr(i)%ppex/1000000.) / day_yr_r !J --> MJ
          gw_heat_ss_yr(i)%tile = (gw_heat_ss_yr(i)%tile/1000000.) / day_yr_r !J --> MJ
          gw_heat_ss_yr(i)%resv = (gw_heat_ss_yr(i)%resv/1000000.) / day_yr_r !J --> MJ
          gw_heat_ss_yr(i)%wetl = (gw_heat_ss_yr(i)%wetl/1000000.) / day_yr_r !J --> MJ
          gw_heat_ss_yr(i)%canl = (gw_heat_ss_yr(i)%canl/1000000.) / day_yr_r !J --> MJ
          gw_heat_ss_yr(i)%fpln = (gw_heat_ss_yr(i)%fpln/1000000.) / day_yr_r !J --> MJ
          gw_heat_ss_yr(i)%pond = (gw_heat_ss_yr(i)%pond/1000000.) / day_yr_r !J --> MJ
        enddo
      endif

      !compute average daily solute fluxes (kg/day) for the year
      if(gw_solute_flag == 1) then
        do i=1,ncell
          do s=1,gw_nsolute
            gwsol_ss_sum(i)%solute(s)%rech = (gwsol_ss_sum(i)%solute(s)%rech/1000.) / day_yr_r !g --> kg
            gwsol_ss_sum(i)%solute(s)%gwsw = (gwsol_ss_sum(i)%solute(s)%gwsw/1000.) / day_yr_r !g --> kg
            gwsol_ss_sum(i)%solute(s)%swgw = (gwsol_ss_sum(i)%solute(s)%swgw/1000.) / day_yr_r !g --> kg
            gwsol_ss_sum(i)%solute(s)%soil = (gwsol_ss_sum(i)%solute(s)%soil/1000.) / day_yr_r !g --> kg
            gwsol_ss_sum(i)%solute(s)%satx = (gwsol_ss_sum(i)%solute(s)%satx/1000.) / day_yr_r !g --> kg
            gwsol_ss_sum(i)%solute(s)%ppex = (gwsol_ss_sum(i)%solute(s)%ppex/1000.) / day_yr_r !g --> kg
            gwsol_ss_sum(i)%solute(s)%tile = (gwsol_ss_sum(i)%solute(s)%tile/1000.) / day_yr_r !g --> kg
            gwsol_ss_sum(i)%solute(s)%resv = (gwsol_ss_sum(i)%solute(s)%resv/1000.) / day_yr_r !g --> kg
            gwsol_ss_sum(i)%solute(s)%wetl = (gwsol_ss_sum(i)%solute(s)%wetl/1000.) / day_yr_r !g --> kg
            gwsol_ss_sum(i)%solute(s)%canl = (gwsol_ss_sum(i)%solute(s)%canl/1000.) / day_yr_r !g --> kg
            gwsol_ss_sum(i)%solute(s)%fpln = (gwsol_ss_sum(i)%solute(s)%fpln/1000.) / day_yr_r !g --> kg
            gwsol_ss_sum(i)%solute(s)%pond = (gwsol_ss_sum(i)%solute(s)%pond/1000.) / day_yr_r !g --> kg
            gwsol_ss_sum(i)%solute(s)%advn = (gwsol_ss_sum(i)%solute(s)%advn/1000.) / day_yr_r !g --> kg
            gwsol_ss_sum(i)%solute(s)%disp = (gwsol_ss_sum(i)%solute(s)%disp/1000.) / day_yr_r !g --> kg
            gwsol_ss_sum(i)%solute(s)%rcti = (gwsol_ss_sum(i)%solute(s)%rcti/1000.) / day_yr_r !g --> kg
            gwsol_ss_sum(i)%solute(s)%rcto = (gwsol_ss_sum(i)%solute(s)%rcto/1000.) / day_yr_r !g --> kg
            gwsol_ss_sum(i)%solute(s)%minl = (gwsol_ss_sum(i)%solute(s)%minl/1000.) / day_yr_r !g --> kg
            gwsol_ss_sum(i)%solute(s)%sorb = (gwsol_ss_sum(i)%solute(s)%sorb/1000.) / day_yr_r !g --> kg
          enddo
        enddo
      endif

      !pumping (irrigation) (for HRUs) -- yearly long-format output ----------------------
      if(gwflag_pump == 1) then
        do i=1,sp_ob%hru
          if(hru_pump_yr(i) > 0.) then
            iob = sp_ob1%hru + i - 1
            write(out_hru_pump_yr,8101) time%day,time%mo,time%day_mo, &
              time%yrc,i,ob(iob)%gis_id,ob(iob)%name,hru_pump_yr(i)
          endif
        enddo
      endif
      !accumulate into AA before zeroing
      do i=1,sp_ob%hru
        hru_pump_aa(i) = hru_pump_aa(i) + hru_pump_yr(i)
      enddo
      hru_pump_yr = 0.

      !(individual grid-file writes per flux component removed -- replaced by long-format above)

      !zero out flux sums to prepare for the next year ------------------------------------------
      !flow
      do i=1,ncell
        gw_hyd_ss_yr(i)%rech = 0.
        gw_hyd_ss_yr(i)%gwet = 0.
        gw_hyd_ss_yr(i)%gwsw = 0.
        gw_hyd_ss_yr(i)%swgw = 0.
        gw_hyd_ss_yr(i)%satx = 0.
        gw_hyd_ss_yr(i)%soil = 0.
        gw_hyd_ss_yr(i)%latl = 0.
        gw_hyd_ss_yr(i)%bndr = 0.
        gw_hyd_ss_yr(i)%ppag = 0.
        gw_hyd_ss_yr(i)%ppdf = 0.
        gw_hyd_ss_yr(i)%ppex = 0.
        gw_hyd_ss_yr(i)%tile = 0.
        gw_hyd_ss_yr(i)%resv = 0.
        gw_hyd_ss_yr(i)%wetl = 0.
        gw_hyd_ss_yr(i)%canl = 0.
        gw_hyd_ss_yr(i)%fpln = 0.
        gw_hyd_ss_yr(i)%pond = 0.
        gw_hyd_ss_yr(i)%phyt = 0.
      enddo
      !heat flux
      if(gw_heat_flag == 1) then
        do i=1,ncell
          gw_heat_ss_yr(i)%rech = 0.
          gw_heat_ss_yr(i)%gwet = 0.
          gw_heat_ss_yr(i)%gwsw = 0.
          gw_heat_ss_yr(i)%swgw = 0.
          gw_heat_ss_yr(i)%satx = 0.
          gw_heat_ss_yr(i)%soil = 0.
          gw_heat_ss_yr(i)%latl = 0.
          gw_heat_ss_yr(i)%disp = 0.
          gw_heat_ss_yr(i)%bndr = 0.
          gw_heat_ss_yr(i)%ppag = 0.
          gw_heat_ss_yr(i)%ppex = 0.
          gw_heat_ss_yr(i)%tile = 0.
          gw_heat_ss_yr(i)%resv = 0.
          gw_heat_ss_yr(i)%wetl = 0.
          gw_heat_ss_yr(i)%canl = 0.
          gw_heat_ss_yr(i)%fpln = 0.
          gw_heat_ss_yr(i)%pond = 0.
        enddo
      endif
      !solute
      if(gw_solute_flag == 1) then
        do i=1,ncell
          do s=1,gw_nsolute
            gwsol_ss_sum(i)%solute(s)%rech = 0.
            gwsol_ss_sum(i)%solute(s)%gwsw = 0.
            gwsol_ss_sum(i)%solute(s)%swgw = 0.
            gwsol_ss_sum(i)%solute(s)%satx = 0.
            gwsol_ss_sum(i)%solute(s)%soil = 0.
            gwsol_ss_sum(i)%solute(s)%ppag = 0.
            gwsol_ss_sum(i)%solute(s)%ppex = 0.
            gwsol_ss_sum(i)%solute(s)%tile = 0.
            gwsol_ss_sum(i)%solute(s)%resv = 0.
            gwsol_ss_sum(i)%solute(s)%wetl = 0.
            gwsol_ss_sum(i)%solute(s)%canl = 0.
            gwsol_ss_sum(i)%solute(s)%fpln = 0.
            gwsol_ss_sum(i)%solute(s)%pond = 0.
            gwsol_ss_sum(i)%solute(s)%advn = 0.
            gwsol_ss_sum(i)%solute(s)%disp = 0.
            gwsol_ss_sum(i)%solute(s)%rcti = 0.
            gwsol_ss_sum(i)%solute(s)%rcto = 0.
            gwsol_ss_sum(i)%solute(s)%minl = 0.
            gwsol_ss_sum(i)%solute(s)%sorb = 0.
          enddo
        enddo
      endif

      !yearly water balance (basin) -------------------------------------------------------------
      if(gwflag_yr == 1) then
        write(out_gwbal_yr,8100) time%day,time%mo,time%day_mo, &
          time%yrc,"       1","       1",bsn%name, &
          gw_hyd_grid_yr%chng,gw_hyd_grid_yr%rech,gw_hyd_grid_yr%gwet, &
          gw_hyd_grid_yr%gwsw,gw_hyd_grid_yr%swgw, &
          gw_hyd_grid_yr%satx,gw_hyd_grid_yr%soil,gw_hyd_grid_yr%latl, &
          gw_hyd_grid_yr%bndr,gw_hyd_grid_yr%ppag, &
          gw_hyd_grid_yr%ppex,gw_hyd_grid_yr%tile,gw_hyd_grid_yr%resv, &
          gw_hyd_grid_yr%wetl,gw_hyd_grid_yr%canl, &
          gw_hyd_grid_yr%fpln,gw_hyd_grid_yr%pond,gw_hyd_grid_yr%phyt, &
          gw_hyd_grid_yr%ppdf
      endif

      !zero out annual basin arrays
      gw_hyd_grid_yr%chng = 0.
      gw_hyd_grid_yr%rech = 0.
      gw_hyd_grid_yr%gwet = 0.
      gw_hyd_grid_yr%gwsw = 0.
      gw_hyd_grid_yr%swgw = 0.
      gw_hyd_grid_yr%satx = 0.
      gw_hyd_grid_yr%soil = 0.
      gw_hyd_grid_yr%latl = 0.
      gw_hyd_grid_yr%bndr = 0.
      gw_hyd_grid_yr%ppag = 0.
      gw_hyd_grid_yr%ppdf = 0.
      gw_hyd_grid_yr%ppex = 0.
      gw_hyd_grid_yr%tile = 0.
      gw_hyd_grid_yr%resv = 0.
      gw_hyd_grid_yr%wetl = 0.
      gw_hyd_grid_yr%canl = 0.
      gw_hyd_grid_yr%fpln = 0.
      gw_hyd_grid_yr%pond = 0.
      gw_hyd_grid_yr%phyt = 0.

      !heat flux values (basin) -----------------------------------------------------------------
      if(gw_heat_flag == 1) then
        if(gwflag_yr == 1) then
          write(out_heatbal_yr,8100) time%day,time%mo,time%day_mo, &
            time%yrc,"       1","       1",bsn%name, &
            gw_heat_grid_yr%chng, &
            gw_heat_grid_yr%rech,gw_heat_grid_yr%gwet, &
            gw_heat_grid_yr%gwsw, &
            gw_heat_grid_yr%swgw,gw_heat_grid_yr%satx, &
            gw_heat_grid_yr%soil, &
            gw_heat_grid_yr%latl,gw_heat_grid_yr%disp, &
            gw_heat_grid_yr%bndr, &
            gw_heat_grid_yr%ppag,gw_heat_grid_yr%ppex, &
            gw_heat_grid_yr%tile, &
            gw_heat_grid_yr%resv,gw_heat_grid_yr%wetl, &
            gw_heat_grid_yr%canl, &
            gw_heat_grid_yr%fpln,gw_heat_grid_yr%pond
        endif
        gw_heat_grid_yr%chng = 0.
        gw_heat_grid_yr%rech = 0.
        gw_heat_grid_yr%gwet = 0.
        gw_heat_grid_yr%gwsw = 0.
        gw_heat_grid_yr%swgw = 0.
        gw_heat_grid_yr%satx = 0.
        gw_heat_grid_yr%soil = 0.
        gw_heat_grid_yr%latl = 0.
        gw_heat_grid_yr%disp = 0.
        gw_heat_grid_yr%bndr = 0.
        gw_heat_grid_yr%ppag = 0.
        gw_heat_grid_yr%ppex = 0.
        gw_heat_grid_yr%tile = 0.
        gw_heat_grid_yr%resv = 0.
        gw_heat_grid_yr%wetl = 0.
        gw_heat_grid_yr%canl = 0.
        gw_heat_grid_yr%fpln = 0.
        gw_heat_grid_yr%pond = 0.
      endif

      !solute mass values (basin) ---------------------------------------------------------------
      if(gw_solute_flag == 1) then
        do s=1,gw_nsolute
          if(gwflag_yr == 1) then
            write(out_solbal_yr+s,8100) time%day,time%mo,time%day_mo, &
              time%yrc,"       1","       1",bsn%name, &
              sol_grid_chng_yr(s),sol_grid_rech_yr(s), &
              sol_grid_gwsw_yr(s),sol_grid_swgw_yr(s), &
              sol_grid_satx_yr(s), &
              sol_grid_soil_yr(s),sol_grid_advn_yr(s), &
              sol_grid_disp_yr(s), &
              sol_grid_rcti_yr(s),sol_grid_rcto_yr(s), &
              sol_grid_minl_yr(s),sol_grid_sorb_yr(s), &
              sol_grid_ppag_yr(s),sol_grid_ppex_yr(s), &
              sol_grid_tile_yr(s),sol_grid_resv_yr(s), &
              sol_grid_wetl_yr(s), &
              sol_grid_canl_yr(s),sol_grid_fpln_yr(s), &
              sol_grid_pond_yr(s)
          endif
          sol_grid_chng_yr(s) = 0.
          sol_grid_rech_yr(s) = 0.
          sol_grid_gwsw_yr(s) = 0.
          sol_grid_swgw_yr(s) = 0.
          sol_grid_satx_yr(s) = 0.
          sol_grid_soil_yr(s) = 0.
          sol_grid_advn_yr(s) = 0.
          sol_grid_disp_yr(s) = 0.
          sol_grid_rcti_yr(s) = 0.
          sol_grid_rcto_yr(s) = 0.
          sol_grid_minl_yr(s) = 0.
          sol_grid_sorb_yr(s) = 0.
          sol_grid_ppag_yr(s) = 0.
          sol_grid_ppex_yr(s) = 0.
          sol_grid_tile_yr(s) = 0.
          sol_grid_resv_yr(s) = 0.
          sol_grid_wetl_yr(s) = 0.
          sol_grid_canl_yr(s) = 0.
          sol_grid_fpln_yr(s) = 0.
          sol_grid_pond_yr(s) = 0.
        enddo !go to next solute
      endif

      !format statements (subroutine-local)
140   format(i8,i6,i6,i8,i8,i10,4x,a4,i4.4,2f13.3,16e13.4)
8100  format(4i6,2a,2x,a16,50e13.4)
8101  format(4i6,2i8,a18,e13.4)
8102  format(4i6,2i8,a18,5e13.4)

      return
      end subroutine gwflow_output_yr

      !NOTE: old individual grid-file writes per flux component removed.
      !      (formerly ~400 lines of per-variable grid write blocks)
      !      All cell-level data now written in long-format above.
      !

      subroutine gwflow_output_aa

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes average annual gwflow output in SWAT+ long format:
!!    one row per active cell with average annual head, wtdepth, and average
!!    daily flow rates; basin-level water/heat/solute balance; HRU pumping;
!!    transit time grids.

      use gwflow_module
      use hydrograph_module
      use sd_channel_module
      use time_module
      use basin_module, only : bsn

      implicit none

      integer :: i, j, k, s, iob
      integer :: num_months
      integer :: gis_id
      real :: wtdepth
      real :: nbyr_r
      real :: obs_temp, obs_no3, obs_p
      character(len=16) :: obs_name
      real, allocatable :: temp_array(:)
      external :: gwflow_write_cell_array

      !--- only execute on the last day of the simulation ---
      if(time%yrc /= time%yrc_end .or. time%day /= time%day_end) return

      nbyr_r = real(time%nbyr)
      allocate(temp_array(ncell))

      !pumping for HRUs -- average annual long-format output
      if(gwflag_pump == 1) then
        do i=1,sp_ob%hru
          if(hru_pump_aa(i) > 0.) then
            iob = sp_ob1%hru + i - 1
            write(out_hru_pump_aa,8101) time%day,time%mo,time%day_mo, &
              time%yrc,i,ob(iob)%gis_id,ob(iob)%name,hru_pump_aa(i)/nbyr_r
          endif
        enddo
      endif

      !--- obs well average annual output ---
      if(gwflag_obs == 1 .and. gw_num_obs_wells > 0) then
        do k=1,gw_num_obs_wells
          i = gw_obs_cells(k)
          obs_temp = -99.; obs_no3 = -99.; obs_p = -99.
          if(gw_heat_flag == 1) obs_temp = gw_obs_temp_aa(k) / nbyr_r
          if(gw_solute_flag == 1) then
            obs_no3 = gw_obs_sol_aa(k,1) / nbyr_r
            obs_p = gw_obs_sol_aa(k,2) / nbyr_r
          endif
          write(obs_name,'(a4,i4.4)') 'obs_',k
          write(out_gwobs_aa,8102) time%day,time%mo,time%day_mo, &
            time%yrc,k,gw_obs_cells(k),obs_name, &
            gw_head_sum_aa(i)/nbyr_r, &
            gw_state(i)%elev - gw_head_sum_aa(i)/nbyr_r, &
            obs_temp, obs_no3, obs_p
        enddo
      endif

      !--- cell-level average annual long-format output ---
      !      Per-cell yearly averages were accumulated into gw_hyd_ss_aa in _yr;
      !      divide by nbyr to get average annual values.
      if(gwflag_aa == 1) then
        do i=1,ncell
          if(gw_state(i)%stat == 1) then
            gis_id = (cell_row(i)-1)*grid_ncol + cell_col(i)
            wtdepth = gw_state(i)%elev - (gw_head_sum_aa(i) / nbyr_r)
            write(out_gwcell_aa,140) time%day, time%mo, &
              time%day_mo, time%yrc, i, gis_id, &
              'gw_', i, &
              gw_head_sum_aa(i) / nbyr_r, wtdepth, &
              gw_hyd_ss_aa(i)%rech / nbyr_r, &
              gw_hyd_ss_aa(i)%gwet / nbyr_r, &
              gw_hyd_ss_aa(i)%gwsw / nbyr_r, &
              gw_hyd_ss_aa(i)%swgw / nbyr_r, &
              gw_hyd_ss_aa(i)%satx / nbyr_r, &
              gw_hyd_ss_aa(i)%soil / nbyr_r, &
              gw_hyd_ss_aa(i)%latl / nbyr_r, &
              gw_hyd_ss_aa(i)%ppag / nbyr_r, &
              gw_hyd_ss_aa(i)%ppex / nbyr_r, &
              gw_hyd_ss_aa(i)%tile / nbyr_r, &
              gw_hyd_ss_aa(i)%resv / nbyr_r, &
              gw_hyd_ss_aa(i)%wetl / nbyr_r, &
              gw_hyd_ss_aa(i)%fpln / nbyr_r, &
              gw_hyd_ss_aa(i)%canl / nbyr_r, &
              gw_hyd_ss_aa(i)%pond / nbyr_r, &
              gw_hyd_ss_aa(i)%phyt / nbyr_r
          endif
        enddo
      endif

      !average annual water balance (basin) -----------------------------------------------------
      gw_hyd_grid_aa%chng = gw_hyd_grid_aa%chng + (vaft_grid-vbef_grid)
      gw_hyd_grid_aa%rech = gw_hyd_grid_aa%rech / nbyr_r
      gw_hyd_grid_aa%gwet = gw_hyd_grid_aa%gwet / nbyr_r
      gw_hyd_grid_aa%gwsw = gw_hyd_grid_aa%gwsw / nbyr_r
      gw_hyd_grid_aa%swgw = gw_hyd_grid_aa%swgw / nbyr_r
      gw_hyd_grid_aa%satx = gw_hyd_grid_aa%satx / nbyr_r
      gw_hyd_grid_aa%soil = gw_hyd_grid_aa%soil / nbyr_r
      gw_hyd_grid_aa%latl = gw_hyd_grid_aa%latl / nbyr_r
      gw_hyd_grid_aa%bndr = gw_hyd_grid_aa%bndr / nbyr_r
      gw_hyd_grid_aa%ppag = gw_hyd_grid_aa%ppag / nbyr_r
      gw_hyd_grid_aa%ppdf = gw_hyd_grid_aa%ppdf / nbyr_r
      gw_hyd_grid_aa%ppex = gw_hyd_grid_aa%ppex / nbyr_r
      gw_hyd_grid_aa%tile = gw_hyd_grid_aa%tile / nbyr_r
      gw_hyd_grid_aa%resv = gw_hyd_grid_aa%resv / nbyr_r
      gw_hyd_grid_aa%wetl = gw_hyd_grid_aa%wetl / nbyr_r
      gw_hyd_grid_aa%canl = gw_hyd_grid_aa%canl / nbyr_r
      gw_hyd_grid_aa%fpln = gw_hyd_grid_aa%fpln / nbyr_r
      gw_hyd_grid_aa%pond = gw_hyd_grid_aa%pond / nbyr_r
      gw_hyd_grid_aa%phyt = gw_hyd_grid_aa%phyt / nbyr_r
      if(gwflag_aa == 1) then
        write(out_gwbal_aa,8100) time%day,time%mo,time%day_mo, &
          time%yrc,"       1","       1",bsn%name, &
          gw_hyd_grid_aa%chng,gw_hyd_grid_aa%rech,gw_hyd_grid_aa%gwet, &
          gw_hyd_grid_aa%gwsw,gw_hyd_grid_aa%swgw, &
          gw_hyd_grid_aa%satx,gw_hyd_grid_aa%soil,gw_hyd_grid_aa%latl, &
          gw_hyd_grid_aa%bndr,gw_hyd_grid_aa%ppag, &
          gw_hyd_grid_aa%ppex,gw_hyd_grid_aa%tile,gw_hyd_grid_aa%resv, &
          gw_hyd_grid_aa%wetl,gw_hyd_grid_aa%canl, &
          gw_hyd_grid_aa%fpln,gw_hyd_grid_aa%pond,gw_hyd_grid_aa%phyt, &
          gw_hyd_grid_aa%ppdf
      endif

      !average annual heat fluxes (basin) -------------------------------------------------------
      gw_heat_grid_aa%chng = gw_heat_grid_aa%chng + (heat_haft_grid-heat_hbef_grid)
      gw_heat_grid_aa%rech = gw_heat_grid_aa%rech / nbyr_r
      gw_heat_grid_aa%gwet = gw_heat_grid_aa%gwet / nbyr_r
      gw_heat_grid_aa%gwsw = gw_heat_grid_aa%gwsw / nbyr_r
      gw_heat_grid_aa%swgw = gw_heat_grid_aa%swgw / nbyr_r
      gw_heat_grid_aa%satx = gw_heat_grid_aa%satx / nbyr_r
      gw_heat_grid_aa%soil = gw_heat_grid_aa%soil / nbyr_r
      gw_heat_grid_aa%latl = gw_heat_grid_aa%latl / nbyr_r
      gw_heat_grid_aa%disp = gw_heat_grid_aa%disp / nbyr_r
      gw_heat_grid_aa%bndr = gw_heat_grid_aa%bndr / nbyr_r
      gw_heat_grid_aa%ppag = gw_heat_grid_aa%ppag / nbyr_r
      gw_heat_grid_aa%ppex = gw_heat_grid_aa%ppex / nbyr_r
      gw_heat_grid_aa%tile = gw_heat_grid_aa%tile / nbyr_r
      gw_heat_grid_aa%resv = gw_heat_grid_aa%resv / nbyr_r
      gw_heat_grid_aa%wetl = gw_heat_grid_aa%wetl / nbyr_r
      gw_heat_grid_aa%canl = gw_heat_grid_aa%canl / nbyr_r
      gw_heat_grid_aa%fpln = gw_heat_grid_aa%fpln / nbyr_r
      gw_heat_grid_aa%pond = gw_heat_grid_aa%pond / nbyr_r
      if(gwflag_aa == 1) then
        write(out_heatbal_aa,8100) time%day,time%mo,time%day_mo, &
          time%yrc,"       1","       1",bsn%name, &
          gw_heat_grid_aa%chng, &
          gw_heat_grid_aa%rech,gw_heat_grid_aa%gwet,gw_heat_grid_aa%gwsw, &
          gw_heat_grid_aa%swgw,gw_heat_grid_aa%satx,gw_heat_grid_aa%soil, &
          gw_heat_grid_aa%latl,gw_heat_grid_aa%disp,gw_heat_grid_aa%bndr, &
          gw_heat_grid_aa%ppag,gw_heat_grid_aa%ppex,gw_heat_grid_aa%tile, &
          gw_heat_grid_aa%resv,gw_heat_grid_aa%wetl,gw_heat_grid_aa%canl, &
          gw_heat_grid_aa%fpln,gw_heat_grid_aa%pond
      endif

      !average annual solute values (basin) -----------------------------------------------------
      if(gw_solute_flag == 1) then
        do s=1,gw_nsolute
          sol_grid_chng_tt(s) = sol_grid_chng_tt(s) + (sol_grid_maft-sol_grid_mbef)
          sol_grid_rech_tt(s) = sol_grid_rech_tt(s) / nbyr_r
          sol_grid_gwsw_tt(s) = sol_grid_gwsw_tt(s) / nbyr_r
          sol_grid_swgw_tt(s) = sol_grid_swgw_tt(s) / nbyr_r
          sol_grid_satx_tt(s) = sol_grid_satx_tt(s) / nbyr_r
          sol_grid_advn_tt(s) = sol_grid_advn_tt(s) / nbyr_r
          sol_grid_disp_tt(s) = sol_grid_disp_tt(s) / nbyr_r
          sol_grid_rcti_tt(s) = sol_grid_rcti_tt(s) / nbyr_r
          sol_grid_rcto_tt(s) = sol_grid_rcto_tt(s) / nbyr_r
          sol_grid_minl_tt(s) = sol_grid_minl_tt(s) / nbyr_r
          sol_grid_sorb_tt(s) = sol_grid_sorb_tt(s) / nbyr_r
          sol_grid_ppag_tt(s) = sol_grid_ppag_tt(s) / nbyr_r
          sol_grid_ppex_tt(s) = sol_grid_ppex_tt(s) / nbyr_r
          sol_grid_tile_tt(s) = sol_grid_tile_tt(s) / nbyr_r
          sol_grid_soil_tt(s) = sol_grid_soil_tt(s) / nbyr_r
          sol_grid_resv_tt(s) = sol_grid_resv_tt(s) / nbyr_r
          sol_grid_wetl_tt(s) = sol_grid_wetl_tt(s) / nbyr_r
          sol_grid_canl_tt(s) = sol_grid_canl_tt(s) / nbyr_r
          sol_grid_fpln_tt(s) = sol_grid_fpln_tt(s) / nbyr_r
          sol_grid_pond_tt(s) = sol_grid_pond_tt(s) / nbyr_r
          if(gwflag_aa == 1) then
            write(out_solbal_aa+s,8100) time%day,time%mo,time%day_mo, &
              time%yrc,"       1","       1",bsn%name, &
              sol_grid_chng_tt(s),sol_grid_rech_tt(s), &
              sol_grid_gwsw_tt(s),sol_grid_swgw_tt(s), &
              sol_grid_satx_tt(s), &
              sol_grid_soil_tt(s),sol_grid_advn_tt(s), &
              sol_grid_disp_tt(s), &
              sol_grid_rcti_tt(s),sol_grid_rcto_tt(s), &
              sol_grid_minl_tt(s),sol_grid_sorb_tt(s), &
              sol_grid_ppag_tt(s),sol_grid_ppex_tt(s), &
              sol_grid_tile_tt(s),sol_grid_resv_tt(s), &
              sol_grid_wetl_tt(s), &
              sol_grid_canl_tt(s),sol_grid_fpln_tt(s), &
              sol_grid_pond_tt(s)
          endif
        enddo !next solute
      endif

      !if soft calibration, prepare for next simulation
      sim_month = 1

      !write out groundwater transit time to channels and tiles
      if(gw_ttime == 1) then
        do k=1,ncell
          temp_array(k) = gw_cell_chan_time(k)
        enddo
        call gwflow_write_cell_array(out_gw_transit_chan, temp_array, ncell, 2)
        if(gw_tile_flag == 1) then
          do k=1,ncell
            temp_array(k) = gw_cell_tile_time(k)
          enddo
          call gwflow_write_cell_array(out_gw_transit_tile, temp_array, ncell, 2)
        endif
      endif

      if(allocated(temp_array)) deallocate(temp_array)

      !format statements (subroutine-local)
105   format(i8,1000(e13.4))
140   format(i8,i6,i6,i8,i8,i10,4x,a4,i4.4,2f13.3,16e13.4)
8100  format(4i6,2a,2x,a16,50e13.4)
8101  format(4i6,2i8,a18,e13.4)
8102  format(4i6,2i8,a18,5e13.4)

      return
      end subroutine gwflow_output_aa


!     ==========================================================================
!     Helper subroutines for output redesign
!     ==========================================================================


      subroutine gwflow_write_celldef
!!    Writes gwflow_cell_definition.txt once during initialization.
!!    Maps cell index to spatial location for all output files.
      use gwflow_module
      implicit none
      integer :: i

      open(out_gw_celldef, file='gwflow_cell_definition.txt')
      write(out_gw_celldef,'(a)') 'cell_id  row  col        x_coord        y_coord  zone  status          area'
      do i=1,ncell
        if(gw_state(i)%stat > 0) then
          write(out_gw_celldef,'(i8,2i6,2f15.1,2i6,e15.4)') &
            i, cell_row(i), cell_col(i), gw_state(i)%xcrd, gw_state(i)%ycrd, &
            gw_state(i)%zone, gw_state(i)%stat, gw_state(i)%area
        endif
      enddo
      close(out_gw_celldef)

      return
      end subroutine gwflow_write_celldef


      subroutine gwflow_write_cell_array(iunit, values, ncell_in, fmt_code)
!!    Writes active cell values as a single row.
!!    fmt_code: 1=f12.3 (heads), 2=e12.3 (fluxes), 3=e12.6 (high precision)
      use gwflow_module, only : gw_state
      implicit none
      integer, intent(in) :: iunit
      real, dimension(ncell_in), intent(in) :: values
      integer, intent(in) :: ncell_in
      integer, intent(in) :: fmt_code
      integer :: i

      select case(fmt_code)
      case(1) !heads
        write(iunit,101) (values(i), i=1,ncell_in)
      case(2) !fluxes
        write(iunit,102) (values(i), i=1,ncell_in)
      case(3) !high precision
        write(iunit,103) (values(i), i=1,ncell_in)
      end select

101   format(99999(f12.3))
102   format(99999(e12.3))
103   format(99999(e12.6))

      return
      end subroutine gwflow_write_cell_array


