      subroutine gwflow_output_init

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine opens all gwflow output files and writes headers
!!    (extracted from gwflow_read)

      use gwflow_module
      use hydrograph_module
      use sd_channel_module
      use time_module
      use constituent_mass_module, only : cs_db

      implicit none

      logical :: i_exist
      integer :: in_gw = 1230
      integer :: s = 0
      character*4 aString
      character(len=16) :: hydsep_hdr(10) = ""
      !header arrays
      character(len=13) :: gwflow_hdr_day(26) = ""
      character(len=13) :: gwflow_hdr_mon(21) = ""
      character(len=13) :: gwflow_hdr_yr(20) = ""
      character(len=13) :: gwflow_hdr_aa(20) = ""
      character(len=13) :: gwflow_hdr_day_grp(25) = ""
      character(len=13) :: sol_hdr_day(25) = ""
      character(len=13) :: sol_hdr_mo(22) = ""
      character(len=13) :: sol_hdr_yr(21) = ""
      character(len=13) :: sol_hdr_aa(21) = ""
      character(len=13) :: heat_hdr_day(24) = ""
      character(len=13) :: heat_hdr_yr(22) = ""
      character(len=13) :: heat_hdr_aa(22) = ""
      !general
      character(len=13) :: header = ""
      character*100 file_name(50)
      integer :: i = 0
      integer :: j = 0
      integer :: n = 0
      integer :: max_num = 0
      integer :: wb_cell = 0
      real :: group_area = 0.

      !initialize groundwater balance ---------------------------------------------------------------------------------------------------------------
      write(out_gw,*)
      write(out_gw,*) '     initialize groundwater balance files and arrays'

      !open file to track daily groundwater water balance
      if(gwflag_day.eq.1) then
        open(out_gwbal,file='gwflow_basin_wb_day.txt')
        write(out_gwbal,*) 'Groundwater watershed-wide fluxes for each day'
        write(out_gwbal,*)
        write(out_gwbal,*) 'watershed area (m2):',(bsn%area_tot_ha*10000.)
        write(out_gwbal,*)
        write(out_gwbal,*) 'Positive value: groundwater added to aquifer'
        write(out_gwbal,*) 'Negative value: groundwater removed from aquifer'
        write(out_gwbal,*)
        write(out_gwbal,*) 'ts:           days time step used for groundwater storage calculations'
        write(out_gwbal,*) 'vbef:         mm   total groundwater volume at the beginning of the day'
        write(out_gwbal,*) 'vaft:         mm   total groundwater volume at the end of the day'
        write(out_gwbal,*) 'rech:         mm   soil water added to groundwater'
        write(out_gwbal,*) 'gwet:         mm   groundwater removed by evapotranspiration'
        write(out_gwbal,*) 'gwsw:         mm   groundwater discharge to streams'
        write(out_gwbal,*) 'swgw:         mm   stream water seepage to groundwater'
        write(out_gwbal,*) 'satx:         mm   saturation excess flow (water table above ground)'
        write(out_gwbal,*) 'soil:         mm   groundwater transferred to HRU soil profile'
        write(out_gwbal,*) 'latl:         mm   groundwater transferred between cells'
        write(out_gwbal,*) 'bndr:         mm   groundwater added/removed at watershed boundary'
        write(out_gwbal,*) 'ppag:         mm   groundwater pumped for irrigation'
        write(out_gwbal,*) 'ppex:         mm   groundwater pumping specified by user'
        write(out_gwbal,*) 'tile:         mm   groundwater removed via tile drains'
        write(out_gwbal,*) 'resv:         mm   groundwater exchanged with reservoirs'
        write(out_gwbal,*) 'wetl:         mm   groundwater outflow to wetlands'
        write(out_gwbal,*) 'canl:         mm   canal seepage to groundwater'
        write(out_gwbal,*) 'fpln:         mm   floodplain exchange'
        write(out_gwbal,*) 'pond:         mm   recharge pond seepage'
        write(out_gwbal,*) 'phyt:         mm   phreatophyte transpiration'
        write(out_gwbal,*) 'error:        --   water balance error for aquifer'
        write(out_gwbal,*) 'satfr:        --   fraction of cells that have water table at ground'
        write(out_gwbal,*) 'wtdep:        m    average depth to water table for watershed'
        write(out_gwbal,*) 'ppdf:         mm   groundwater demand not satisfied for irrigation'
        write(out_gwbal,*)
        gwflow_hdr_day = (/"year","day","ts","vbef","vaft","rech","gwet","gwsw","swgw","satx","soil", &
                                             "latl","bndr","ppag","ppex","tile","resv","wetl","canl", &
                                             "fpln","pond","phyt","error","satfr","wtdepth","ppdf"/)
        write(out_gwbal,119) (gwflow_hdr_day(j),j=1,26)
      endif

      !open file to track monthly groundwater water balance
      if(gwflag_mon.eq.1) then
        open(out_gwbal_mon,file='gwflow_basin_wb_mon.txt')
        write(out_gwbal_mon,*) 'Groundwater watershed-wide fluxes for each month'
        write(out_gwbal_mon,*)
        write(out_gwbal_mon,*) 'watershed area (m2):',(bsn%area_tot_ha*10000.)
        write(out_gwbal_mon,*)
        write(out_gwbal_mon,*) 'Positive value: groundwater added to aquifer'
        write(out_gwbal_mon,*) 'Negative value: groundwater removed from aquifer'
        write(out_gwbal_mon,*)
        write(out_gwbal_mon,*) 'dvol:         mm   change in groundwater volume during the month'
        write(out_gwbal_mon,*) 'rech:         mm   soil water added to groundwater'
        write(out_gwbal_mon,*) 'gwet:         mm   groundwater removed by evapotranspiration'
        write(out_gwbal_mon,*) 'gwsw:         mm   groundwater discharge to streams'
        write(out_gwbal_mon,*) 'swgw:         mm   stream water seepage to groundwater'
        write(out_gwbal_mon,*) 'satx:         mm   saturation excess flow (water table above ground)'
        write(out_gwbal_mon,*) 'soil:         mm   groundwater transferred to HRU soil profile'
        write(out_gwbal_mon,*) 'latl:         mm   groundwater transferred between cells'
        write(out_gwbal_mon,*) 'bndr:         mm   groundwater added/removed at watershed boundary'
        write(out_gwbal_mon,*) 'ppag:         mm   groundwater pumped for irrigation'
        write(out_gwbal_mon,*) 'ppex:         mm   groundwater pumping specified by user'
        write(out_gwbal_mon,*) 'tile:         mm   groundwater removed via tile drains'
        write(out_gwbal_mon,*) 'resv:         mm   groundwater exchanged with reservoirs'
        write(out_gwbal_mon,*) 'wetl:         mm   groundwater outflow to wetlands'
        write(out_gwbal_mon,*) 'canl:         mm   canal seepage to groundwater'
        write(out_gwbal_mon,*) 'fpln:         mm   floodplain exchange'
        write(out_gwbal_mon,*) 'pond:         mm   recharge pond seepage'
        write(out_gwbal_mon,*) 'phyt:         mm   phreatophyte transpiration'
        write(out_gwbal_mon,*) 'ppdf:         mm   groundwater demand not satisfied for irrigation'
        write(out_gwbal_mon,*)
        gwflow_hdr_mon = (/"year","month","dvol","rech","gwet","gwsw","swgw","satx","soil", &
                                          "latl","bndr","ppag","ppex","tile","resv","wetl","canl", &
                                          "fpln","pond","phyt","ppdf"/)
        write(out_gwbal_mon,132) (gwflow_hdr_mon(j),j=1,21)
      endif

      !open file to track yearly groundwater water balance
      if(gwflag_yr.eq.1) then
        open(out_gwbal_yr,file='gwflow_basin_wb_yr.txt')
        write(out_gwbal_yr,*) 'Groundwater watershed-wide fluxes for each year'
        write(out_gwbal_yr,*)
        write(out_gwbal_yr,*) 'watershed area (m2):',(bsn%area_tot_ha*10000.)
        write(out_gwbal_yr,*)
        write(out_gwbal_yr,*) 'Positive value: groundwater added to aquifer'
        write(out_gwbal_yr,*) 'Negative value: groundwater removed from aquifer'
        write(out_gwbal_yr,*)
        write(out_gwbal_yr,*) 'dvol:      mm   change in groundwater volume during the year'
        write(out_gwbal_yr,*) 'rech:      mm   soil water added to groundwater'
        write(out_gwbal_yr,*) 'gwet:      mm   groundwater removed by evapotranspiration'
        write(out_gwbal_yr,*) 'gwsw:      mm   groundwater discharge to streams'
        write(out_gwbal_yr,*) 'swgw:      mm   stream water seepage to groundwater'
        write(out_gwbal_yr,*) 'satx:      mm   saturation excess flow (water table above ground)'
        write(out_gwbal_yr,*) 'soil:      mm   groundwater transferred to HRU soil profile'
        write(out_gwbal_yr,*) 'latl:      mm   groundwater transferred between cells'
        write(out_gwbal_yr,*) 'bndr:      mm   groundwater added/removed at watershed boundary'
        write(out_gwbal_yr,*) 'ppag:      mm   groundwater pumped for irrigation'
        write(out_gwbal_yr,*) 'ppex:      mm   groundwater pumping specified by user'
        write(out_gwbal_yr,*) 'tile:      mm   groundwater removed via tile drains'
        write(out_gwbal_yr,*) 'resv:      mm   groundwater exchanged with reservoirs'
        write(out_gwbal_yr,*) 'wetl:      mm   groundwater outflow to wetlands'
        write(out_gwbal_yr,*) 'canl:      mm   canal seepage to groundwater'
        write(out_gwbal_yr,*) 'fpln:      mm   floodplain exchange'
        write(out_gwbal_yr,*) 'pond:      mm   recharge pond seepage'
        write(out_gwbal_yr,*) 'phyt:      mm   phreatophyte transpiration'
        write(out_gwbal_yr,*) 'ppdf:      mm   groundwater demand not satisfied for irrigation'
        write(out_gwbal_yr,*)
        gwflow_hdr_yr = (/"  year","dvol","rech","gwet","gwsw","swgw","satx","soil","latl","bndr","ppag","ppex", &
                                   "tile","resv","wetl","canl","fpln","pond","phyt","ppdf"/)
        write(out_gwbal_yr,120) (gwflow_hdr_yr(j),j=1,20)
      endif

      !open file to write out average annual groundwater water balance
        if(gwflag_aa.eq.1) then
        open(out_gwbal_aa,file='gwflow_basin_wb_aa.txt')
        write(out_gwbal_aa,*) 'Average annual groundwater watershed-wide fluxes'
        write(out_gwbal_aa,*)
        write(out_gwbal_aa,*) 'watershed area (m2):',(bsn%area_tot_ha*10000.)
        write(out_gwbal_aa,*)
        write(out_gwbal_aa,*) 'Positive value: groundwater added to aquifer'
        write(out_gwbal_aa,*) 'Negative value: groundwater removed from aquifer'
        write(out_gwbal_aa,*)
        write(out_gwbal_aa,*) 'dvol:      mm   change in groundwater volume during the year'
        write(out_gwbal_aa,*) 'rech:      mm   soil water added to groundwater'
        write(out_gwbal_aa,*) 'gwet:      mm   groundwater removed by evapotranspiration'
        write(out_gwbal_aa,*) 'gwsw:      mm   groundwater discharge to streams'
        write(out_gwbal_aa,*) 'swgw:      mm   stream water seepage to groundwater'
        write(out_gwbal_aa,*) 'satx:      mm   saturation excess flow (water table above ground)'
        write(out_gwbal_aa,*) 'soil:      mm   groundwater transferred to HRU soil profile'
        write(out_gwbal_aa,*) 'latl:      mm   groundwater transferred between cells'
        write(out_gwbal_aa,*) 'bndr:      mm   groundwater added/removed at watershed boundary'
        write(out_gwbal_aa,*) 'ppag:      mm   groundwater pumped for irrigation'
        write(out_gwbal_aa,*) 'ppex:      mm   groundwater pumping specified by user'
        write(out_gwbal_aa,*) 'tile:      mm   groundwater removed via tile drains'
        write(out_gwbal_aa,*) 'resv:      mm   groundwater exchanged with reservoirs'
        write(out_gwbal_aa,*) 'wetl:      mm   groundwater outflow to wetlands'
        write(out_gwbal_aa,*) 'canl:      mm   canal seepage to groundwater'
        write(out_gwbal_aa,*) 'fpln:      mm   floodplain exchange'
        write(out_gwbal_aa,*) 'pond:      mm   recharge pond seepage'
        write(out_gwbal_aa,*) 'phyt:      mm   phreatophyte transpiration'
        write(out_gwbal_aa,*) 'ppdf:      mm   groundwater demand not satisfied for irrigation'
        write(out_gwbal_aa,*)
        gwflow_hdr_aa = (/"  year","dvol","rech","gwet","gwsw","swgw","satx","soil","latl","bndr","ppag","ppex", &
                                   "tile","resv","wetl","canl","fpln","pond","phyt","ppdf"/)
        write(out_gwbal_aa,120) (gwflow_hdr_aa(j),j=1,20)
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
          file_name = 'gwflow_group_wb_day_'//aString
          open(out_gwbal_grp+i,file=file_name)
          write(out_gwbal_grp+i,*) 'Groundwater fluxes for each day'
          write(out_gwbal_grp+i,*) 'Cell Group:',i
          write(out_gwbal_grp+i,*)
          write(out_gwbal_grp+i,*) 'Cell Group area (m2):',group_area
          write(out_gwbal_grp+i,*)
          write(out_gwbal_grp+i,*) 'Positive value: groundwater added to aquifer'
          write(out_gwbal_grp+i,*) 'Negative value: groundwater removed from aquifer'
          write(out_gwbal_grp+i,*)
          write(out_gwbal_grp+i,*) 'ts:           days time step used for groundwater storage calculations'
          write(out_gwbal_grp+i,*) 'vbef:         m3   total groundwater volume at the beginning of the day'
          write(out_gwbal_grp+i,*) 'vaft:         m3   total groundwater volume at the end of the day'
          write(out_gwbal_grp+i,*) 'rech:         m3   soil water added to groundwater'
          write(out_gwbal_grp+i,*) 'gwet:         m3   groundwater removed by evapotranspiration'
          write(out_gwbal_grp+i,*) 'gwsw:         m3   groundwater discharge to streams'
          write(out_gwbal_grp+i,*) 'swgw:         m3   stream water seepage to groundwater'
          write(out_gwbal_grp+i,*) 'satx:         m3   saturation excess flow (water table above ground)'
          write(out_gwbal_grp+i,*) 'soil:         m3   groundwater transferred to HRU soil profile'
          write(out_gwbal_grp+i,*) 'latl:         m3   groundwater transferred between cells'
          write(out_gwbal_grp+i,*) 'bndr:         m3   groundwater added/removed at watershed boundary'
          write(out_gwbal_grp+i,*) 'ppag:         m3   groundwater pumped for irrigation'
          write(out_gwbal_grp+i,*) 'ppex:         m3   groundwater pumping specified by user'
          write(out_gwbal_grp+i,*) 'tile:         m3   groundwater removed via tile drains'
          write(out_gwbal_grp+i,*) 'resv:         m3   groundwater exchanged with reservoirs'
          write(out_gwbal_grp+i,*) 'wetl:         m3   groundwater outflow to wetlands'
          write(out_gwbal_grp+i,*) 'canl:         m3   canal seepage to groundwater'
          write(out_gwbal_grp+i,*) 'fpln:         m3   floodplain exchange'
          write(out_gwbal_grp+i,*) 'pond:         m3   recharge pond seepage'
          write(out_gwbal_grp+i,*) 'phyt:         m3   phreatophyte transpiration'
          write(out_gwbal_grp+i,*) 'error:        --   water balance error for aquifer'
          write(out_gwbal_grp+i,*) 'wtdep:        m    average depth to water table for watershed'
          write(out_gwbal_grp+i,*) 'ppdf:         m3   groundwater demand not satisfied for irrigation'
          write(out_gwbal_grp+i,*)
          gwflow_hdr_day_grp = (/"year","day","ts","vbef","vaft","rech","gwet","gwsw","swgw","satx","soil", &
                                              "latl","bndr","ppag","ppex","tile","resv","wetl","canl", &
                                              "fpln","pond","phyt","error","wtdepth","ppdf"/)
          write(out_gwbal_grp+i,119) (gwflow_hdr_day_grp(j),j=1,25)
        enddo !go to next cell group
        close(in_gw)
  1091  format(i2)
      endif



      !initialize groundwater heat balance ----------------------------------------------------------------------------------------------------------
      if(gw_heat_flag == 1) then
      write(out_gw,*)
      write(out_gw,*) '     initialize groundwater heat balance files and arrays'

      !open file to track daily groundwater water balance
      if(gwflag_day.eq.1) then
        open(out_heatbal_dy,file='gwflow_basin_heat_day.txt')
        write(out_heatbal_dy,*) 'Groundwater watershed-wide heat fluxes for each day'
        write(out_heatbal_dy,*)
        write(out_heatbal_dy,*) 'watershed area (m2):',(bsn%area_tot_ha*10000.)
        write(out_heatbal_dy,*)
        write(out_heatbal_dy,*) 'Positive value: heat (million joules) added to aquifer'
        write(out_heatbal_dy,*) 'Negative value: heat (million joules) removed from aquifer'
        write(out_heatbal_dy,*)
        write(out_heatbal_dy,*) 'ts:           days time step used for groundwater heat calculations'
        write(out_heatbal_dy,*) 'hbef:         MJ   total groundwater heat at the beginning of the day'
        write(out_heatbal_dy,*) 'haft:         MJ   total groundwater heat at the end of the day'
        write(out_heatbal_dy,*) 'rech:         MJ   soil water heat added to groundwater'
        write(out_heatbal_dy,*) 'gwet:         MJ   groundwater heat removed by evapotranspiration'
        write(out_heatbal_dy,*) 'gwsw:         MJ   groundwater heat discharge to streams'
        write(out_heatbal_dy,*) 'swgw:         MJ   stream water heat to groundwater'
        write(out_heatbal_dy,*) 'satx:         MJ   saturation excess flow heat to streams'
        write(out_heatbal_dy,*) 'soil:         MJ   groundwater heat transferred to HRU soil profile'
        write(out_heatbal_dy,*) 'latl:         MJ   groundwater heat transferred between cells'
        write(out_heatbal_dy,*) 'disp:         MJ   groundwater heat transported by dispersion'
        write(out_heatbal_dy,*) 'bndr:         MJ   groundwater heat added/removed at watershed boundary'
        write(out_heatbal_dy,*) 'ppag:         MJ   groundwater heat pumped for irrigation'
        write(out_heatbal_dy,*) 'ppex:         MJ   groundwater heat pumping specified by user'
        write(out_heatbal_dy,*) 'tile:         MJ   groundwater heat removed via tile drains'
        write(out_heatbal_dy,*) 'resv:         MJ   groundwater heat exchanged with reservoirs'
        write(out_heatbal_dy,*) 'wetl:         MJ   groundwater heat outflow to wetlands'
        write(out_heatbal_dy,*) 'canl:         MJ   groundwater heat exchanged with canals'
        write(out_heatbal_dy,*) 'fpln:         MJ   groundwater heat exchanged with floodplains'
        write(out_heatbal_dy,*) 'pond:         MJ   groundwater heat in recharge pond seepage'
        write(out_heatbal_dy,*) 'error:        --   heat balance error for aquifer'
        write(out_heatbal_dy,*) 'tavg:         C   average groundwater temperature'
        write(out_heatbal_dy,*)
        heat_hdr_day = (/"year","day","ts","hbef","haft","rech","gwet","gwsw","swgw","satx","soil", &
                                           "latl","disp","bndr","ppag","ppex","tile","resv","wetl","canl", &
                                           "fpln","pond","error","tavg"/)
        write(out_heatbal_dy,133) (heat_hdr_day(j),j=1,24)
      endif

      !open file to track yearly groundwater water balance
      if(gwflag_yr.eq.1) then
        open(out_heatbal_yr,file='gwflow_basin_heat_yr.txt')
        write(out_heatbal_yr,*) 'Groundwater watershed-wide heat fluxes for each year'
        write(out_heatbal_yr,*)
        write(out_heatbal_yr,*) 'watershed area (m2):',(bsn%area_tot_ha*10000.)
        write(out_heatbal_yr,*)
        write(out_heatbal_yr,*) 'Positive value: heat (million joules) added to aquifer'
        write(out_heatbal_yr,*) 'Negative value: heat (million joules) removed from aquifer'
        write(out_heatbal_yr,*)
        write(out_heatbal_yr,*) 'ts:           days time step used for groundwater heat calculations'
        write(out_heatbal_yr,*) 'hdel:         MJ   total groundwater heat change during the year'
        write(out_heatbal_yr,*) 'rech:         MJ   soil water heat added to groundwater'
        write(out_heatbal_yr,*) 'gwet:         MJ   groundwater heat removed by evapotranspiration'
        write(out_heatbal_yr,*) 'gwsw:         MJ   groundwater heat discharge to streams'
        write(out_heatbal_yr,*) 'swgw:         MJ   stream water heat to groundwater'
        write(out_heatbal_yr,*) 'satx:         MJ   saturation excess flow heat to streams'
        write(out_heatbal_yr,*) 'soil:         MJ   groundwater heat transferred to HRU soil profile'
        write(out_heatbal_yr,*) 'latl:         MJ   groundwater heat transferred between cells'
        write(out_heatbal_yr,*) 'disp:         MJ   groundwater heat transported by dispersion'
        write(out_heatbal_yr,*) 'bndr:         MJ   groundwater heat added/removed at watershed boundary'
        write(out_heatbal_yr,*) 'ppag:         MJ   groundwater heat pumped for irrigation'
        write(out_heatbal_yr,*) 'ppex:         MJ   groundwater heat pumping specified by user'
        write(out_heatbal_yr,*) 'tile:         MJ   groundwater heat removed via tile drains'
        write(out_heatbal_yr,*) 'resv:         MJ   groundwater heat exchanged with reservoirs'
        write(out_heatbal_yr,*) 'wetl:         MJ   groundwater heat outflow to wetlands'
        write(out_heatbal_yr,*) 'canl:         MJ   groundwater heat exchanged with canals'
        write(out_heatbal_yr,*) 'fpln:         MJ   groundwater heat exchanged with floodplains'
        write(out_heatbal_yr,*) 'pond:         MJ   groundwater heat in recharge pond seepage'
        write(out_heatbal_yr,*) 'error:        --   heat balance error for aquifer'
        write(out_heatbal_yr,*)
        heat_hdr_yr = (/"year","day","ts","hdel","rech","gwet","gwsw","swgw","satx","soil", &
                                          "latl","disp","bndr","ppag","ppex","tile","resv","wetl","canl", &
                                          "fpln","pond","error"/)
        write(out_heatbal_yr,120) (heat_hdr_yr(j),j=1,22)
      endif

      !open file to write out average annual groundwater water balance
      if(gwflag_aa.eq.1) then
        open(out_heatbal_aa,file='gwflow_basin_heat_aa.txt')
        write(out_heatbal_aa,*) 'Groundwater watershed-wide heat fluxes across all years'
        write(out_heatbal_aa,*)
        write(out_heatbal_aa,*) 'watershed area (m2):',(bsn%area_tot_ha*10000.)
        write(out_heatbal_aa,*)
        write(out_heatbal_aa,*) 'Positive value: heat (million joules) added to aquifer'
        write(out_heatbal_aa,*) 'Negative value: heat (million joules) removed from aquifer'
        write(out_heatbal_aa,*)
        write(out_heatbal_aa,*) 'ts:           days time step used for groundwater heat calculations'
        write(out_heatbal_aa,*) 'hdel:         MJ   total groundwater heat change across all years'
        write(out_heatbal_aa,*) 'rech:         MJ   soil water heat added to groundwater'
        write(out_heatbal_aa,*) 'gwet:         MJ   groundwater heat removed by evapotranspiration'
        write(out_heatbal_aa,*) 'gwsw:         MJ   groundwater heat discharge to streams'
        write(out_heatbal_aa,*) 'swgw:         MJ   stream water heat to groundwater'
        write(out_heatbal_aa,*) 'satx:         MJ   saturation excess flow heat to streams'
        write(out_heatbal_aa,*) 'soil:         MJ   groundwater heat transferred to HRU soil profile'
        write(out_heatbal_aa,*) 'latl:         MJ   groundwater heat transferred between cells'
        write(out_heatbal_aa,*) 'disp:         MJ   groundwater heat transported by dispersion'
        write(out_heatbal_aa,*) 'bndr:         MJ   groundwater heat added/removed at watershed boundary'
        write(out_heatbal_aa,*) 'ppag:         MJ   groundwater heat pumped for irrigation'
        write(out_heatbal_aa,*) 'ppex:         MJ   groundwater heat pumping specified by user'
        write(out_heatbal_aa,*) 'tile:         MJ   groundwater heat removed via tile drains'
        write(out_heatbal_aa,*) 'resv:         MJ   groundwater heat exchanged with reservoirs'
        write(out_heatbal_aa,*) 'wetl:         MJ   groundwater heat outflow to wetlands'
        write(out_heatbal_aa,*) 'canl:         MJ   groundwater heat exchanged with canals'
        write(out_heatbal_aa,*) 'fpln:         MJ   groundwater heat exchanged with floodplains'
        write(out_heatbal_aa,*) 'pond:         MJ   groundwater heat in recharge pond seepage'
        write(out_heatbal_aa,*) 'error:        --   heat balance error for aquifer'
        write(out_heatbal_aa,*)
        heat_hdr_aa = (/"year","day","ts","hdel","rech","gwet","gwsw","swgw","satx","soil", &
                                          "latl","disp","bndr","ppag","ppex","tile","resv","wetl","canl", &
                                          "fpln","pond","error"/)
        write(out_heatbal_aa,120) (heat_hdr_aa(j),j=1,22)
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
            write(out_solbal_dy+n,*) 'Solute:',gwsol_nm(n)
            write(out_solbal_dy+n,*) 'Groundwater watershed-wide solute loads for each day'
            write(out_solbal_dy+n,*)
            write(out_solbal_dy+n,*) 'Positive value: solute mass added to aquifer'
            write(out_solbal_dy+n,*) 'Negative value: solute mass removed from aquifer'
            write(out_solbal_dy+n,*)
            write(out_solbal_dy+n,*) 'ts:        days time step used for groundwater solute calculations'
            write(out_solbal_dy+n,*) 'mbef:      kg   total groundwater solute mass at the beginning of the day'
            write(out_solbal_dy+n,*) 'maft:      kg   total groundwater solute mass at the end of the day'
            write(out_solbal_dy+n,*) 'rech:      kg   solute mass in recharge water'
            write(out_solbal_dy+n,*) 'gwsw:      kg   solute mass loaded to streams'
            write(out_solbal_dy+n,*) 'swgw:      kg   solute mass loaded from streams'
            write(out_solbal_dy+n,*) 'satx:      kg   solute mass loaded to streams by saturation excess flow'
            write(out_solbal_dy+n,*) 'soil:      kg   solute mass loaded to HRU soil profiles'
            write(out_solbal_dy+n,*) 'advn:      kg   solute mass transported by advection'
            write(out_solbal_dy+n,*) 'disp:      kg   solute mass transported by dispersion'
            write(out_solbal_dy+n,*) 'rcti:      kg   solute mass produced by kinetic reaction'
            write(out_solbal_dy+n,*) 'rcto:      kg   solute mass consumed by kinetic reaction'
            write(out_solbal_dy+n,*) 'minl:      kg   solute mass added by mineral dissolution'
            write(out_solbal_dy+n,*) 'sorb:      kg   solute mass removed by sorption'
            write(out_solbal_dy+n,*) 'ppag:      kg   solute mass removed by groundwater pumping for irrigation'
            write(out_solbal_dy+n,*) 'ppex:      kg   solute mass removed by groundwater pumping specified by user'
            write(out_solbal_dy+n,*) 'tile:      kg   solute mass removed by tile drains'
            write(out_solbal_dy+n,*) 'resv:      kg   solute mass loaded to/from reservoirs'
            write(out_solbal_dy+n,*) 'wetl:      kg   solute mass loaded to/from wetlands'
            write(out_solbal_dy+n,*) 'canl:      kg   solute mass loaded to groundwater from canal seepage'
            write(out_solbal_dy+n,*) 'fpln:      kg   solute mass in floodplain exchange'
            write(out_solbal_dy+n,*) 'pond:      kg   solute mass in recharge pond seepage'
            write(out_solbal_dy+n,*) 'error:     --   mass balance error for aquifer'
            write(out_solbal_dy+n,*)
            sol_hdr_day = (/"  year","   day","ts","mbef","maft","rech","gwsw","swgw","satx","soil","advn", &
                            "disp","rcti","rcto","minl","sorb","ppag","ppex","tile","resv","wetl","canl","fpln","pond","error"/)
            write(out_solbal_dy+n,119) (sol_hdr_day(j),j=1,25)
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
            write(out_solbal_mo+n,*) 'Solute:',gwsol_nm(n)
            write(out_solbal_mo+n,*) 'Groundwater watershed-wide solute loads for each month'
            write(out_solbal_mo+n,*)
            write(out_solbal_mo+n,*) 'Positive value: solute mass added to aquifer'
            write(out_solbal_mo+n,*) 'Negative value: solute mass removed from aquifer'
            write(out_solbal_mo+n,*)
            write(out_solbal_mo+n,*) 'delm:     kg   change in groundwater solute mass during the month'
            write(out_solbal_mo+n,*) 'rech:     kg   solute mass in recharge water'
            write(out_solbal_mo+n,*) 'gwsw:     kg   solute mass loaded to streams'
            write(out_solbal_mo+n,*) 'swgw:     kg   solute mass loaded from streams'
            write(out_solbal_mo+n,*) 'satx:     kg   solute mass loaded to streams by saturation excess flow'
            write(out_solbal_mo+n,*) 'soil:     kg   solute mass loaded to HRU soil profiles'
            write(out_solbal_mo+n,*) 'advn:     kg   solute mass transported by advection'
            write(out_solbal_mo+n,*) 'disp:     kg   solute mass transported by dispersion'
            write(out_solbal_mo+n,*) 'rcti:     kg   solute mass produced by kinetic reaction'
            write(out_solbal_mo+n,*) 'rcto:     kg   solute mass consumed by kinetic reaction'
            write(out_solbal_mo+n,*) 'minl:     kg   solute mass added by mineral dissolution'
            write(out_solbal_mo+n,*) 'sorb:     kg   solute mass removed by denitrification'
            write(out_solbal_mo+n,*) 'ppag:     kg   solute mass removed by groundwater pumping for irrigation'
            write(out_solbal_mo+n,*) 'ppex:     kg   solute mass removed by groundwater pumping specified by user'
            write(out_solbal_mo+n,*) 'tile:     kg   solute mass removed by tile drains'
            write(out_solbal_mo+n,*) 'resv:     kg   solute mass loaded to/from reservoirs'
            write(out_solbal_mo+n,*) 'wetl:     kg   solute mass loaded to/from wetlands'
            write(out_solbal_mo+n,*) 'canl:     kg   solute mass loaded to groundwater from canal seepage'
            write(out_solbal_mo+n,*) 'fpln:     kg   solute mass in floodplain exchange'
            write(out_solbal_mo+n,*) 'pond:     kg   solute mass in recharge pond seepage'
            write(out_solbal_mo+n,*)
            sol_hdr_mo = (/"  year","month","delm","rech","gwsw","swgw","satx","soil","advn","disp","rcti","rcto","minl", &
                           "sorb","ppag","ppex","tile","resv","wetl","canl","fpln","pond"/)
            write(out_solbal_mo+n,132) (sol_hdr_mo(j),j=1,22)
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
            write(out_solbal_yr+n,*) 'Solute:',gwsol_nm(n)
            write(out_solbal_yr+n,*) 'Groundwater watershed-wide solute loads for each year'
            write(out_solbal_yr+n,*)
            write(out_solbal_yr+n,*) 'Positive value: solute mass added to aquifer'
            write(out_solbal_yr+n,*) 'Negative value: solute mass removed from aquifer'
            write(out_solbal_yr+n,*)
            write(out_solbal_yr+n,*) 'delm:     kg   change in groundwater solute mass during the year'
            write(out_solbal_yr+n,*) 'rech:     kg   solute mass in recharge water'
            write(out_solbal_yr+n,*) 'gwsw:     kg   solute mass loaded to streams'
            write(out_solbal_yr+n,*) 'swgw:     kg   solute mass loaded from streams'
            write(out_solbal_yr+n,*) 'satx:     kg   solute mass loaded to streams by saturation excess flow'
            write(out_solbal_yr+n,*) 'soil:     kg   solute mass loaded to HRU soil profiles'
            write(out_solbal_yr+n,*) 'advn:     kg   solute mass transported by advection'
            write(out_solbal_yr+n,*) 'disp:     kg   solute mass transported by dispersion'
            write(out_solbal_yr+n,*) 'rcti:     kg   solute mass produced by kinetic reaction'
            write(out_solbal_yr+n,*) 'rcto:     kg   solute mass consumed by kinetic reaction'
            write(out_solbal_yr+n,*) 'minl:     kg   solute mass added by mineral dissolution'
            write(out_solbal_yr+n,*) 'sorb:     kg   solute mass removed by denitrification'
            write(out_solbal_yr+n,*) 'ppag:     kg   solute mass removed by groundwater pumping for irrigation'
            write(out_solbal_yr+n,*) 'ppex:     kg   solute mass removed by groundwater pumping specified by user'
            write(out_solbal_yr+n,*) 'tile:     kg   solute mass removed by tile drains'
            write(out_solbal_yr+n,*) 'resv:     kg   solute mass loaded to/from reservoirs'
            write(out_solbal_yr+n,*) 'wetl:     kg   solute mass loaded to/from wetlands'
            write(out_solbal_yr+n,*) 'canl:     kg   solute mass loaded to groundwater from canal seepage'
            write(out_solbal_yr+n,*) 'fpln:     kg   solute mass in floodplain exchange'
            write(out_solbal_yr+n,*) 'pond:     kg   solute mass in recharge pond seepage'
            write(out_solbal_yr+n,*)
            sol_hdr_yr = (/"  year","delm","rech","gwsw","swgw","satx","soil","advn","disp","rcti","rcto","minl", &
                           "sorb","ppag","ppex","tile","resv","wetl","canl","fpln","pond"/)
            write(out_solbal_yr+n,120) (sol_hdr_yr(j),j=1,21)
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
            write(out_solbal_aa+n,*) 'Solute:',gwsol_nm(n)
            write(out_solbal_aa+n,*) 'Average annual groundwater watershed-wide solute loads'
            write(out_solbal_aa+n,*)
            write(out_solbal_aa+n,*) 'Positive value: solute mass added to aquifer'
            write(out_solbal_aa+n,*) 'Negative value: solute mass removed from aquifer'
            write(out_solbal_aa+n,*)
            write(out_solbal_aa+n,*) 'delm:      kg   total change in groundwater solute mass across all years'
            write(out_solbal_aa+n,*) 'rech:      kg   solute mass in recharge water'
            write(out_solbal_aa+n,*) 'gwsw:      kg   solute mass loaded to streams'
            write(out_solbal_aa+n,*) 'swgw:      kg   solute mass loaded from streams'
            write(out_solbal_aa+n,*) 'satx:      kg   solute mass loaded to streams by saturation excess flow'
            write(out_solbal_aa+n,*) 'soil:      kg   solute mass loaded to HRU soil profiles'
            write(out_solbal_aa+n,*) 'advn:      kg   solute mass transported by advection'
            write(out_solbal_aa+n,*) 'disp:      kg   solute mass transported by dispersion'
            write(out_solbal_aa+n,*) 'rcti:      kg   solute mass produced by kinetic reaction'
            write(out_solbal_aa+n,*) 'rcto:      kg   solute mass consumed by kinetic reaction'
            write(out_solbal_aa+n,*) 'minl:      kg   solute mass added by mineral dissolution'
            write(out_solbal_aa+n,*) 'sorb:      kg   solute mass removed via sorption'
            write(out_solbal_aa+n,*) 'ppag:      kg   solute mass removed by groundwater pumping for irrigation'
            write(out_solbal_aa+n,*) 'ppex:      kg   solute mass removed by groundwater pumping specified by user'
            write(out_solbal_aa+n,*) 'tile:      kg   solute mass removed by tile drains'
            write(out_solbal_aa+n,*) 'resv:      kg   solute mass loaded to/from reservoirs'
            write(out_solbal_aa+n,*) 'wetl:      kg   solute mass loaded to/from wetlands'
            write(out_solbal_aa+n,*) 'canl:      kg   solute mass loaded to groundwater from canal seepage'
            write(out_solbal_aa+n,*) 'fpln:      kg   solute mass in floodplain exchange'
            write(out_solbal_aa+n,*) 'pond:      kg   solute mass in recharge pond seepage'
            write(out_solbal_aa+n,*)
            sol_hdr_aa = (/"  year","delm","rech","gwsw","swgw","satx","soil","advn","disp","rcti","rcto","minl", &
                               "sorb","ppag","ppex","tile","resv","wetl","canl","fpln","pond"/)
            write(out_solbal_aa+n,120) (sol_hdr_aa(j),j=1,21)
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




      !format statements (duplicated from gwflow_read - format labels are subroutine-local)
119   format(4x,a8,a8,a10,a16,a19,50(a13))
120   format(a8,7x,50(a13))
132   format(4x,a8,a8,50(a13))
133   format(4x,a8,a8,a10,a16,a19,50(a18))

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

      implicit none

      integer :: i, j, k, s
      integer :: cell_id
      real :: sum
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
      write(out_gwobs,130) time%yrc,time%day,(gw_obs_head(k),k=1,gw_num_obs_wells)
      if(gw_heat_flag == 1) then
        write(out_gwobs_temp,130) time%yrc,time%day,(gw_obs_temp(k),k=1,gw_num_obs_wells)
      endif
      if(gw_solute_flag == 1) then
        do k=1,gw_num_obs_wells
          write(out_gwobs_sol,119) time%yrc,time%day,k,(gw_obs_solute(k,s),s=1,gw_nsolute)
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
          write(out_gwbal_grp+i,102) time%yrc,time%day,gw_time_step, &
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
        write(out_gwbal,102) time%yrc,time%day,gw_time_step,vbef_grid,vaft_grid,rech_grid,gwet_grid,gwsw_grid,swgw_grid, &
                                                            satx_grid,soil_grid,latl_grid,bndr_grid,ppag_grid,ppex_grid, &
                                                            tile_grid,resv_grid,wetl_grid,canl_grid,fpln_grid,pond_grid, &
                                                            phyt_grid, &
                                                            mass_error,frac_sat,depth_wt_avg,ppdf_grid
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
          write(out_heatbal_dy,103) time%yrc,time%day,gw_time_step, &
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
            write(out_solbal_dy+s,102) time%yrc,time%day,gw_time_step, &
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


      !format statements (subroutine-local)
100   format(10000(f12.3))
101   format(10000(e12.3))
102   format(i8,i8,f10.3,e16.7,e16.7,1000(e13.4))
103   format(i8,i8,f10.3,e18.9,e18.9,1000(e18.9))
105   format(i8,1000(e13.4))
119   format(i8,i8,i8,1000(f12.3))
120   format(<out_cols>(f12.3))
121   format(<out_cols>(e12.6))
130   format(i8,i8,1000(e13.4))

      return
      end subroutine gwflow_output_day


      subroutine gwflow_output_mon

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes monthly gwflow output: average head grids,
!!    average temperature grids, average solute concentration grids,
!!    monthly cell-level flow rate grids, basin water balance, HRU pumping

      use gwflow_module
      use hydrograph_module
      use sd_channel_module
      use time_module

      implicit none

      integer :: i, j, s


      if (time%end_mo == 1) then

        !monthly average groundwater head -----------------------------------------------
        do i=1,ncell
          gw_state(i)%hdmo = gw_state(i)%hdmo / time%day_mo
        enddo
        write(out_head_mo,*) time%yrc,time%mo
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_state(cell_id_usg(i,j))%hdmo
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_head_mo,100) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_head_mo,121) (gw_state(i)%hdmo,i=1,ncell)
        endif
        write(out_head_mo,*)
        !zero out for next month
        do i=1,ncell
          gw_state(i)%hdmo = 0.
        enddo

        !monthly average groundwater temperature ----------------------------------------
        if(gw_heat_flag == 1) then
          do i=1,ncell
            gwheat_state(i)%tpmo = gwheat_state(i)%tpmo / time%day_mo
          enddo
          write(out_temp_mo,*) time%yrc,time%mo
          if(grid_type == "structured") then
            grid_val = 0.
            do i=1,grid_nrow
              do j=1,grid_ncol
                if(cell_id_usg(i,j) > 0) then
                  grid_val(i,j) = gwheat_state(cell_id_usg(i,j))%tpmo
                endif
              enddo
            enddo
            do i=1,grid_nrow
              write(out_temp_mo,100) (grid_val(i,j),j=1,grid_ncol)
            enddo
          else
            write(out_temp_mo,121) (gwheat_state(i)%tpmo,i=1,ncell)
          endif
          write(out_temp_mo,*)
          !zero out for next month
          do i=1,ncell
            gwheat_state(i)%tpmo = 0.
          enddo
        endif

        !monthly average solute concentration -------------------------------------------
        if(gw_solute_flag == 1) then
          write(out_conc_mo,*) time%yrc,time%mo
          do s=1,gw_nsolute
            !calculate average concentration
            do i=1,ncell
              gwsol_state(i)%solute(s)%cnmo = gwsol_state(i)%solute(s)%cnmo / time%day_mo
            enddo
            !write out
            write(out_conc_mo,*) gwsol_nm(s) !solute name
            if(grid_type == "structured") then
              grid_val = 0.
              do i=1,grid_nrow
                do j=1,grid_ncol
                  if(cell_id_usg(i,j) > 0) then
                    grid_val(i,j) = gwsol_state(cell_id_usg(i,j))%solute(s)%cnmo
                  endif
                enddo
              enddo
              do i=1,grid_nrow
                write(out_conc_mo,101) (grid_val(i,j),j=1,grid_ncol)
              enddo
            else
              write(out_conc_mo,121) (gwsol_state(i)%solute(s)%cnmo,i=1,ncell)
            endif
            !zero out for next month
            do i=1,ncell
              gwsol_state(i)%solute(s)%cnmo = 0.
            enddo
          enddo !next solute
          write(out_conc_mo,*)
        endif

        !pumping (irrigation) (for HRUs)
        if(hru_pump_flag == 1) then
          do i=1,sp_ob%hru
            hru_pump_mo_all(i,((time%yrs-1)*12)+time%mo) = hru_pump_mo(i)
          enddo
          hru_pump_mo = 0.
        endif

        !compute average daily groundwater fluxes (m3/day) for the month
        do i=1,ncell
          gw_hyd_ss_mo(i)%rech = gw_hyd_ss_mo(i)%rech / time%day_mo
          gw_hyd_ss_mo(i)%gwet = gw_hyd_ss_mo(i)%gwet / time%day_mo
          gw_hyd_ss_mo(i)%gwsw = gw_hyd_ss_mo(i)%gwsw / time%day_mo
          gw_hyd_ss_mo(i)%swgw = gw_hyd_ss_mo(i)%swgw / time%day_mo
          gw_hyd_ss_mo(i)%satx = gw_hyd_ss_mo(i)%satx / time%day_mo
          gw_hyd_ss_mo(i)%soil = gw_hyd_ss_mo(i)%soil / time%day_mo
          gw_hyd_ss_mo(i)%latl = gw_hyd_ss_mo(i)%latl / time%day_mo
          gw_hyd_ss_mo(i)%bndr = gw_hyd_ss_mo(i)%bndr / time%day_mo
          gw_hyd_ss_mo(i)%ppag = gw_hyd_ss_mo(i)%ppag / time%day_mo
          gw_hyd_ss_mo(i)%ppdf = gw_hyd_ss_mo(i)%ppdf / time%day_mo
          gw_hyd_ss_mo(i)%ppex = gw_hyd_ss_mo(i)%ppex / time%day_mo
          gw_hyd_ss_mo(i)%tile = gw_hyd_ss_mo(i)%tile / time%day_mo
          gw_hyd_ss_mo(i)%resv = gw_hyd_ss_mo(i)%resv / time%day_mo
          gw_hyd_ss_mo(i)%wetl = gw_hyd_ss_mo(i)%wetl / time%day_mo
          gw_hyd_ss_mo(i)%canl = gw_hyd_ss_mo(i)%canl / time%day_mo
          gw_hyd_ss_mo(i)%fpln = gw_hyd_ss_mo(i)%fpln / time%day_mo
          gw_hyd_ss_mo(i)%pond = gw_hyd_ss_mo(i)%pond / time%day_mo
          gw_hyd_ss_mo(i)%phyt = gw_hyd_ss_mo(i)%phyt / time%day_mo
        enddo

        !compute average daily solute fluxes (kg/day) for the month
        if(gw_solute_flag == 1) then
          do i=1,ncell
            do s=1,gw_nsolute
              gwsol_ss_sum_mo(i)%solute(s)%rech = (gwsol_ss_sum_mo(i)%solute(s)%rech/1000.) / time%day_mo !g --> kg
              gwsol_ss_sum_mo(i)%solute(s)%gwsw = (gwsol_ss_sum_mo(i)%solute(s)%gwsw/1000.) / time%day_mo !g --> kg
              gwsol_ss_sum_mo(i)%solute(s)%swgw = (gwsol_ss_sum_mo(i)%solute(s)%swgw/1000.) / time%day_mo !g --> kg
              gwsol_ss_sum_mo(i)%solute(s)%soil = (gwsol_ss_sum_mo(i)%solute(s)%soil/1000.) / time%day_mo !g --> kg
              gwsol_ss_sum_mo(i)%solute(s)%satx = (gwsol_ss_sum_mo(i)%solute(s)%satx/1000.) / time%day_mo !g --> kg
              gwsol_ss_sum_mo(i)%solute(s)%ppex = (gwsol_ss_sum_mo(i)%solute(s)%ppex/1000.) / time%day_mo !g --> kg
              gwsol_ss_sum_mo(i)%solute(s)%tile = (gwsol_ss_sum_mo(i)%solute(s)%tile/1000.) / time%day_mo !g --> kg
              gwsol_ss_sum_mo(i)%solute(s)%resv = (gwsol_ss_sum_mo(i)%solute(s)%resv/1000.) / time%day_mo !g --> kg
              gwsol_ss_sum_mo(i)%solute(s)%wetl = (gwsol_ss_sum_mo(i)%solute(s)%wetl/1000.) / time%day_mo !g --> kg
              gwsol_ss_sum_mo(i)%solute(s)%canl = (gwsol_ss_sum_mo(i)%solute(s)%canl/1000.) / time%day_mo !g --> kg
              gwsol_ss_sum_mo(i)%solute(s)%fpln = (gwsol_ss_sum_mo(i)%solute(s)%fpln/1000.) / time%day_mo !g --> kg
              gwsol_ss_sum_mo(i)%solute(s)%pond = (gwsol_ss_sum_mo(i)%solute(s)%pond/1000.) / time%day_mo !g --> kg
              gwsol_ss_sum_mo(i)%solute(s)%advn = (gwsol_ss_sum_mo(i)%solute(s)%advn/1000.) / time%day_mo !g --> kg
              gwsol_ss_sum_mo(i)%solute(s)%disp = (gwsol_ss_sum_mo(i)%solute(s)%disp/1000.) / time%day_mo !g --> kg
              gwsol_ss_sum_mo(i)%solute(s)%rcti = (gwsol_ss_sum_mo(i)%solute(s)%rcti/1000.) / time%day_mo !g --> kg
              gwsol_ss_sum_mo(i)%solute(s)%rcto = (gwsol_ss_sum_mo(i)%solute(s)%rcto/1000.) / time%day_mo !g --> kg
              gwsol_ss_sum_mo(i)%solute(s)%minl = (gwsol_ss_sum_mo(i)%solute(s)%minl/1000.) / time%day_mo !g --> kg
              gwsol_ss_sum_mo(i)%solute(s)%sorb = (gwsol_ss_sum_mo(i)%solute(s)%sorb/1000.) / time%day_mo !g --> kg
            enddo
          enddo
        endif

        !write out monthly flow rates for each groundwater source/sink ------------------

        !recharge
        write(out_gw_rech_mo,*) 'Recharge rates (m3/day) for month:',time%yrc,time%mo
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_hyd_ss_mo(cell_id_usg(i,j))%rech
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_gw_rech_mo,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_gw_rech_mo,121) (gw_hyd_ss_mo(i)%rech,i=1,ncell)
        endif
        write(out_gw_rech_mo,*)
        if(gw_solute_flag == 1) then !solute mass flux
          do s=1,gw_nsolute
            write(out_sol_rech_mo,*) gwsol_nm(s),'Recharge flux for month (kg/day):',time%yrc,time%mo
            if(grid_type == "structured") then
              grid_val = 0.
              do i=1,grid_nrow
                do j=1,grid_ncol
                  if(cell_id_usg(i,j) > 0) then
                    grid_val(i,j) = gwsol_ss_sum_mo(cell_id_usg(i,j))%solute(s)%rech
                  endif
                enddo
              enddo
              do i=1,grid_nrow
                write(out_sol_rech_mo,101) (grid_val(i,j),j=1,grid_ncol)
              enddo
            else
              write(out_sol_rech_mo,121) (gwsol_ss_sum_mo(i)%solute(s)%rech,i=1,ncell)
            endif
            write(out_sol_rech_mo,*)
          enddo
        endif

        !groundwater ET
        write(out_gw_gwet_mo,*) 'Groundwater ET rates (m3/day) for month:',time%yrc,time%mo
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_hyd_ss_mo(cell_id_usg(i,j))%gwet
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_gw_gwet_mo,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_gw_gwet_mo,121) (gw_hyd_ss_mo(i)%gwet,i=1,ncell)
        endif
        write(out_gw_gwet_mo,*)

        !groundwater-channel exchange rates
        write(out_gw_gwsw_mo,*) 'Groundwater-channel Exchange rates (m3/day) for month:',time%yrc,time%mo
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_hyd_ss_mo(cell_id_usg(i,j))%gwsw
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_gw_gwsw_mo,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_gw_gwsw_mo,121) (gw_hyd_ss_mo(i)%gwsw,i=1,ncell)
        endif
        write(out_gw_gwsw_mo,*)
        if(gw_solute_flag == 1) then !solute mass flux
          do s=1,gw_nsolute
            write(out_sol_gwsw_mo,*) gwsol_nm(s),'GW-channel flux for month (kg/day):',time%yrc,time%mo
            if(grid_type == "structured") then
              grid_val = 0.
              do i=1,grid_nrow
                do j=1,grid_ncol
                  if(cell_id_usg(i,j) > 0) then
                    grid_val(i,j) = gwsol_ss_sum_mo(cell_id_usg(i,j))%solute(s)%gwsw
                  endif
                enddo
              enddo
              do i=1,grid_nrow
                write(out_sol_gwsw_mo,101) (grid_val(i,j),j=1,grid_ncol)
              enddo
            else
              write(out_sol_gwsw_mo,121) (gwsol_ss_sum_mo(i)%solute(s)%gwsw,i=1,ncell)
            endif
            write(out_sol_gwsw_mo,*)
          enddo
        endif

        !saturation excess flow
        if(gw_satx_flag.eq.1) then
        write(out_gw_satx_mo,*) 'Saturation excess flow rates (m3/day) for month:',time%yrc,time%mo
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_hyd_ss_mo(cell_id_usg(i,j))%satx
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_gw_satx_mo,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_gw_satx_mo,121) (gw_hyd_ss_mo(i)%satx,i=1,ncell)
        endif
        write(out_gw_satx_mo,*)
        if(gw_solute_flag == 1) then !solute mass flux
          do s=1,gw_nsolute
            write(out_sol_satx_mo,*) gwsol_nm(s),'Saturation excee flux for month (kg/day):',time%yrc,time%mo
            if(grid_type == "structured") then
              grid_val = 0.
              do i=1,grid_nrow
                do j=1,grid_ncol
                  if(cell_id_usg(i,j) > 0) then
                    grid_val(i,j) = gwsol_ss_sum_mo(cell_id_usg(i,j))%solute(s)%satx
                  endif
                enddo
              enddo
              do i=1,grid_nrow
                write(out_sol_satx_mo,101) (grid_val(i,j),j=1,grid_ncol)
              enddo
            else
              write(out_sol_satx_mo,121) (gwsol_ss_sum_mo(i)%solute(s)%satx,i=1,ncell)
            endif
            write(out_sol_satx_mo,*)
          enddo
        endif
        endif

        !groundwater --> soil transfer
        if(gw_soil_flag.eq.1) then
        write(out_gw_soil_mo,*) 'Groundwater-->Soil Transfer rates (m3/day) for month:',time%yrc,time%mo
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_hyd_ss_mo(cell_id_usg(i,j))%soil
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_gw_soil_mo,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_gw_soil_mo,121) (gw_hyd_ss_mo(i)%soil,i=1,ncell)
        endif
        write(out_gw_soil_mo,*)
        if(gw_solute_flag == 1) then !solute mass flux
          do s=1,gw_nsolute
            write(out_sol_soil_mo,*) gwsol_nm(s),'GW-soil transfer flux for month (kg/day):',time%yrc,time%mo
            if(grid_type == "structured") then
              grid_val = 0.
              do i=1,grid_nrow
                do j=1,grid_ncol
                  if(cell_id_usg(i,j) > 0) then
                    grid_val(i,j) = gwsol_ss_sum_mo(cell_id_usg(i,j))%solute(s)%soil
                  endif
                enddo
              enddo
              do i=1,grid_nrow
                write(out_sol_soil_mo,101) (grid_val(i,j),j=1,grid_ncol)
              enddo
            else
              write(out_sol_soil_mo,121) (gwsol_ss_sum_mo(i)%solute(s)%soil,i=1,ncell)
            endif
            write(out_sol_soil_mo,*)
          enddo
        endif
        endif

        !tile drain flow
        if(gw_tile_flag == 1) then
        write(out_gw_tile_mo,*) 'Tile Drain Outflow rates (m3/day) for month:',time%yrc,time%mo
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_hyd_ss_mo(cell_id_usg(i,j))%tile
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_gw_tile_mo,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_gw_tile_mo,121) (gw_hyd_ss_mo(i)%tile,i=1,ncell)
        endif
        write(out_gw_tile_mo,*)
        if(gw_solute_flag == 1) then !solute mass flux
          do s=1,gw_nsolute
            write(out_sol_tile_mo,*) gwsol_nm(s),'Tile drain flux for month (kg/day):',time%yrc,time%mo
            if(grid_type == "structured") then
              grid_val = 0.
              do i=1,grid_nrow
                do j=1,grid_ncol
                  if(cell_id_usg(i,j) > 0) then
                    grid_val(i,j) = gwsol_ss_sum_mo(cell_id_usg(i,j))%solute(s)%tile
                  endif
                enddo
              enddo
              do i=1,grid_nrow
                write(out_sol_tile_mo,101) (grid_val(i,j),j=1,grid_ncol)
              enddo
            else
              write(out_sol_tile_mo,121) (gwsol_ss_sum_mo(i)%solute(s)%tile,i=1,ncell)
            endif
            write(out_sol_tile_mo,*)
          enddo
        endif
        endif

        !pumping (irrigation)
        write(out_gw_ppag_mo,*) 'Pumping rates (m3/day) for month:',time%yrc,time%mo
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_hyd_ss_mo(cell_id_usg(i,j))%ppag
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_gw_ppag_mo,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_gw_ppag_mo,121) (gw_hyd_ss_mo(i)%ppag,i=1,ncell)
        endif
        write(out_gw_ppag_mo,*)
        if(gw_solute_flag == 1) then !solute mass flux
          do s=1,gw_nsolute
            write(out_sol_ppag_mo,*) gwsol_nm(s),'Pumping flux for month (kg/day):',time%yrc,time%mo
            if(grid_type == "structured") then
              grid_val = 0.
              do i=1,grid_nrow
                do j=1,grid_ncol
                  if(cell_id_usg(i,j) > 0) then
                    grid_val(i,j) = gwsol_ss_sum_mo(cell_id_usg(i,j))%solute(s)%ppag
                  endif
                enddo
              enddo
              do i=1,grid_nrow
                write(out_sol_ppag_mo,101) (grid_val(i,j),j=1,grid_ncol)
              enddo
            else
              write(out_sol_ppag_mo,121) (gwsol_ss_sum_mo(i)%solute(s)%ppag,i=1,ncell)
            endif
            write(out_sol_ppag_mo,*)
          enddo
        endif

        !pumping (user specified)
        if(gw_pumpex_flag == 1) then
        write(out_gw_ppex_mo,*) 'Pumping rates (m3/day) for month:',time%yrc,time%mo
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_hyd_ss_mo(cell_id_usg(i,j))%ppex
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_gw_ppex_mo,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_gw_ppex_mo,121) (gw_hyd_ss_mo(i)%ppex,i=1,ncell)
        endif
        write(out_gw_ppex_mo,*)
        if(gw_solute_flag == 1) then !solute mass flux
          do s=1,gw_nsolute
            write(out_sol_ppex_mo,*) gwsol_nm(s),'Pumpin flux for month (kg/day):',time%yrc,time%mo
            if(grid_type == "structured") then
              grid_val = 0.
              do i=1,grid_nrow
                do j=1,grid_ncol
                  if(cell_id_usg(i,j) > 0) then
                    grid_val(i,j) = gwsol_ss_sum_mo(cell_id_usg(i,j))%solute(s)%ppex
                  endif
                enddo
              enddo
              do i=1,grid_nrow
                write(out_sol_ppex_mo,101) (grid_val(i,j),j=1,grid_ncol)
              enddo
            else
              write(out_sol_ppex_mo,121) (gwsol_ss_sum_mo(i)%solute(s)%ppex,i=1,ncell)
            endif
            write(out_sol_ppex_mo,*)
          enddo
        endif
        endif

        !groundwater-reservoir exchange
        if(gw_res_flag == 1) then
        write(out_gw_resv_mo,*) 'Groundwater-Reservoir Exchange rates (m3/day) for month:',time%yrc,time%mo
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_hyd_ss_mo(cell_id_usg(i,j))%resv
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_gw_resv_mo,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_gw_resv_mo,121) (gw_hyd_ss_mo(i)%resv,i=1,ncell)
        endif
        write(out_gw_resv_mo,*)
        if(gw_solute_flag == 1) then !solute mass flux
          do s=1,gw_nsolute
            write(out_sol_resv_mo,*) gwsol_nm(s),'GW-Reservoir flux for month (kg/day):',time%yrc,time%mo
            if(grid_type == "structured") then
              grid_val = 0.
              do i=1,grid_nrow
                do j=1,grid_ncol
                  if(cell_id_usg(i,j) > 0) then
                    grid_val(i,j) = gwsol_ss_sum_mo(cell_id_usg(i,j))%solute(s)%resv
                  endif
                enddo
              enddo
              do i=1,grid_nrow
                write(out_sol_resv_mo,101) (grid_val(i,j),j=1,grid_ncol)
              enddo
            else
              write(out_sol_resv_mo,121) (gwsol_ss_sum_mo(i)%solute(s)%resv,i=1,ncell)
            endif
            write(out_sol_resv_mo,*)
          enddo
        endif
        endif

        !groundwater-wetland exchange
        if(gw_wet_flag == 1) then
        write(out_gw_wetl_mo,*) 'Groundwater outflow rates (m3/day) to wetlands for month:',time%yrc,time%mo
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_hyd_ss_mo(cell_id_usg(i,j))%wetl
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_gw_wetl_mo,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_gw_wetl_mo,121) (gw_hyd_ss_mo(i)%wetl,i=1,ncell)
        endif
        write(out_gw_wetl_mo,*)
        if(gw_solute_flag == 1) then !solute mass flux
          do s=1,gw_nsolute
            write(out_sol_wetl_mo,*) gwsol_nm(s),'GW flux to wetlands for month (kg/day):',time%yrc,time%mo
            if(grid_type == "structured") then
              grid_val = 0.
              do i=1,grid_nrow
                do j=1,grid_ncol
                  if(cell_id_usg(i,j) > 0) then
                    grid_val(i,j) = gwsol_ss_sum_mo(cell_id_usg(i,j))%solute(s)%wetl
                  endif
                enddo
              enddo
              do i=1,grid_nrow
                write(out_sol_wetl_mo,101) (grid_val(i,j),j=1,grid_ncol)
              enddo
            else
              write(out_sol_wetl_mo,121) (gwsol_ss_sum_mo(i)%solute(s)%wetl,i=1,ncell)
            endif
            write(out_sol_wetl_mo,*)
          enddo
        endif
        endif

        !floodplain exchange
        if(gw_fp_flag == 1) then
        write(out_gw_fpln_mo,*) 'Groundwater-floodplain exchange rates (m3/day) for month:',time%yrc,time%mo
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_hyd_ss_mo(cell_id_usg(i,j))%fpln
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_gw_fpln_mo,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_gw_fpln_mo,121) (gw_hyd_ss_mo(i)%fpln,i=1,ncell)
        endif
        write(out_gw_fpln_mo,*)
        if(gw_solute_flag == 1) then !solute mass flux
          do s=1,gw_nsolute
            write(out_sol_fpln_mo,*) gwsol_nm(s),'GW-floodplain flux for month (kg/day):',time%yrc,time%mo
            if(grid_type == "structured") then
              grid_val = 0.
              do i=1,grid_nrow
                do j=1,grid_ncol
                  if(cell_id_usg(i,j) > 0) then
                    grid_val(i,j) = gwsol_ss_sum_mo(cell_id_usg(i,j))%solute(s)%fpln
                  endif
                enddo
              enddo
              do i=1,grid_nrow
                write(out_sol_fpln_mo,101) (grid_val(i,j),j=1,grid_ncol)
              enddo
            else
              write(out_sol_fpln_mo,121) (gwsol_ss_sum_mo(i)%solute(s)%fpln,i=1,ncell)
            endif
            write(out_sol_fpln_mo,*)
          enddo
        endif
        endif

        !groundwater-canal exchange
        if(gw_canal_flag == 1) then
        write(out_gw_canl_mo,*) 'Groundwater-Canal Exchange rates (m3/day) for month:',time%yrc,time%mo
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_hyd_ss_mo(cell_id_usg(i,j))%canl
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_gw_canl_mo,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_gw_canl_mo,121) (gw_hyd_ss_mo(i)%canl,i=1,ncell)
        endif
        write(out_gw_canl_mo,*)
        if(gw_solute_flag == 1) then !solute mass flux
          do s=1,gw_nsolute
            write(out_sol_canl_mo,*) gwsol_nm(s),'GW-Canal flux for month (kg/day):',time%yrc,time%mo
            if(grid_type == "structured") then
              grid_val = 0.
              do i=1,grid_nrow
                do j=1,grid_ncol
                  if(cell_id_usg(i,j) > 0) then
                    grid_val(i,j) = gwsol_ss_sum_mo(cell_id_usg(i,j))%solute(s)%canl
                  endif
                enddo
              enddo
              do i=1,grid_nrow
                write(out_sol_canl_mo,101) (grid_val(i,j),j=1,grid_ncol)
              enddo
            else
              write(out_sol_canl_mo,121) (gwsol_ss_sum_mo(i)%solute(s)%canl,i=1,ncell)
            endif
            write(out_sol_canl_mo,*)
          enddo
        endif
        endif

        !recharge pond seepage
        if(gw_pond_flag == 1) then
        write(out_gw_pond_mo,*) 'Recharge pond seepage rates (m3/day) for month:',time%yrc,time%mo
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_hyd_ss_mo(cell_id_usg(i,j))%pond
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_gw_pond_mo,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_gw_pond_mo,121) (gw_hyd_ss_mo(i)%pond,i=1,ncell)
        endif
        write(out_gw_pond_mo,*)
        if(gw_solute_flag == 1) then !solute mass flux
          do s=1,gw_nsolute
            write(out_sol_pond_mo,*) gwsol_nm(s),'Recharge pond flux for month (kg/day):',time%yrc,time%mo
            if(grid_type == "structured") then
              grid_val = 0.
              do i=1,grid_nrow
                do j=1,grid_ncol
                  if(cell_id_usg(i,j) > 0) then
                    grid_val(i,j) = gwsol_ss_sum_mo(cell_id_usg(i,j))%solute(s)%pond
                  endif
                enddo
              enddo
              do i=1,grid_nrow
                write(out_sol_pond_mo,101) (grid_val(i,j),j=1,grid_ncol)
              enddo
            else
              write(out_sol_pond_mo,121) (gwsol_ss_sum_mo(i)%solute(s)%pond,i=1,ncell)
            endif
            write(out_sol_pond_mo,*)
          enddo
        endif
        endif

        !phreatophyte transpiration
        if(gw_phyt_flag == 1) then
        write(out_gw_phyt_mo,*) 'Phreatophyte transpiration rates (m3/day) for month:',time%yrc,time%mo
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_hyd_ss_mo(cell_id_usg(i,j))%phyt
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_gw_phyt_mo,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_gw_phyt_mo,121) (gw_hyd_ss_mo(i)%phyt,i=1,ncell)
        endif
        write(out_gw_phyt_mo,*)
        endif

        !zero out flux sums to prepare for the next month
        !flow
        do i=1,ncell
          gw_hyd_ss_mo(i)%rech = 0.
          gw_hyd_ss_mo(i)%gwet = 0.
          gw_hyd_ss_mo(i)%gwsw = 0.
          gw_hyd_ss_mo(i)%satx = 0.
          gw_hyd_ss_mo(i)%soil = 0.
          gw_hyd_ss_mo(i)%ppag = 0.
          gw_hyd_ss_mo(i)%ppex = 0.
          gw_hyd_ss_mo(i)%tile = 0.
          gw_hyd_ss_mo(i)%resv = 0.
          gw_hyd_ss_mo(i)%wetl = 0.
          gw_hyd_ss_mo(i)%canl = 0.
          gw_hyd_ss_mo(i)%fpln = 0.
          gw_hyd_ss_mo(i)%pond = 0.
          gw_hyd_ss_mo(i)%phyt = 0.
        enddo

        !solute
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

        !monthly groundwater balance (basin)
        if(gwflag_mon == 1) then
          write(out_gwbal_mon,105) time%yrc,time%mo, &
                                   gw_hyd_grid_mo%chng,gw_hyd_grid_mo%rech,gw_hyd_grid_mo%gwet,gw_hyd_grid_mo%gwsw,gw_hyd_grid_mo%swgw, &
                                   gw_hyd_grid_mo%satx,gw_hyd_grid_mo%soil,gw_hyd_grid_mo%latl,gw_hyd_grid_mo%bndr,gw_hyd_grid_mo%ppag, &
                                   gw_hyd_grid_mo%ppex,gw_hyd_grid_mo%tile,gw_hyd_grid_mo%resv,gw_hyd_grid_mo%wetl,gw_hyd_grid_mo%canl, &
                                   gw_hyd_grid_mo%fpln,gw_hyd_grid_mo%pond,gw_hyd_grid_mo%phyt,gw_hyd_grid_mo%ppdf
        endif
        !zero out for next month
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

        !solute mass values
        if(gw_solute_flag == 1) then
          do s=1,gw_nsolute !loop through the solutes
            !write out monthly values
            if(gwflag_mon == 1) then
              write(out_solbal_mo+s,105) time%yrc,time%mo, &
                                         sol_grid_chng_mo(s),sol_grid_rech_mo(s),sol_grid_gwsw_mo(s),sol_grid_swgw_mo(s),sol_grid_satx_mo(s), &
                                         sol_grid_soil_mo(s),sol_grid_advn_mo(s),sol_grid_disp_mo(s), &
                                         sol_grid_rcti_mo(s),sol_grid_rcto_mo(s),sol_grid_minl_mo(s),sol_grid_sorb_mo(s), &
                                         sol_grid_ppag_mo(s),sol_grid_ppex_mo(s),sol_grid_tile_mo(s),sol_grid_resv_mo(s),sol_grid_wetl_mo(s), &
                                         sol_grid_canl_mo(s),sol_grid_fpln_mo(s),sol_grid_pond_mo(s)
            endif
            !zero out values for next month
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

      endif


      !format statements (subroutine-local)
100   format(10000(f12.3))
101   format(10000(e12.3))
105   format(i8,i8,1000(e13.4))
120   format(<out_cols>(f12.3))
121   format(<out_cols>(e12.6))

      return
      end subroutine gwflow_output_mon


      subroutine gwflow_output_yr

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes annual (end of year) gwflow output grids and
!!    water/heat/solute balance values, then zeroes annual accumulators.
!!    (extracted from gwflow_simulate section 8)

      use gwflow_module
      use hydrograph_module
      use sd_channel_module
      use time_module

      implicit none

      integer :: i, j, k, s

      !--- only execute at end of year ---
      if(time%end_yr /= 1) return

      !annual average groundwater head ------------------------------------------------
      do i=1,ncell
        gw_state(i)%hdyr = gw_state(i)%hdyr / time%day
      enddo
      write(out_head_yr,*) time%yrc
      if(grid_type == "structured") then
        grid_val = 0.
        do i=1,grid_nrow
          do j=1,grid_ncol
            if(cell_id_usg(i,j) > 0) then
              grid_val(i,j) = gw_state(cell_id_usg(i,j))%hdyr
            endif
          enddo
        enddo
        do i=1,grid_nrow
          write(out_head_yr,100) (grid_val(i,j),j=1,grid_ncol)
        enddo
      else
        write(out_head_yr,122) (gw_state(i)%hdyr,i=1,ncell)
      endif
      write(out_head_yr,*)
      !zero out for next year
      do i=1,ncell
        gw_state(i)%hdyr = 0.
      enddo

      !annual average groundwater temperature -----------------------------------------
      if(gw_heat_flag == 1) then
        do i=1,ncell
          gwheat_state(i)%tpyr = gwheat_state(i)%tpyr / time%day
        enddo
        write(out_temp_yr,*) time%yrc
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gwheat_state(cell_id_usg(i,j))%tpyr
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_temp_yr,100) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_temp_yr,122) (gwheat_state(i)%tpyr,i=1,ncell)
        endif
        write(out_temp_yr,*)
        !zero out for next year
        do i=1,ncell
          gwheat_state(i)%tpyr = 0.
        enddo
      endif

      !annual average solute concentration --------------------------------------------
      if(gw_solute_flag == 1) then
        write(out_conc_yr,*) time%yrc
        do s=1,gw_nsolute
          !calculate average concentration
          do i=1,ncell
            gwsol_state(i)%solute(s)%cnyr = gwsol_state(i)%solute(s)%cnyr / time%day
          enddo
          !write out
          write(out_conc_yr,*) gwsol_nm(s) !solute name
          if(grid_type == "structured") then
            grid_val = 0.
            do i=1,grid_nrow
              do j=1,grid_ncol
                if(cell_id_usg(i,j) > 0) then
                  grid_val(i,j) = gwsol_state(cell_id_usg(i,j))%solute(s)%cnyr
                endif
              enddo
            enddo
            do i=1,grid_nrow
              write(out_conc_yr,101) (grid_val(i,j),j=1,grid_ncol)
            enddo
          else
            write(out_conc_yr,122) (gwsol_state(i)%solute(s)%cnyr,i=1,ncell)
          endif
          !zero out for next year
          do i=1,ncell
            gwsol_state(i)%solute(s)%cnyr = 0.
          enddo
        enddo !next solute
        write(out_conc_yr,*)
      endif !check for solutes

      !compute average daily groundwater fluxes (m3/day) for the year -----------------
      do i=1,ncell
        gw_hyd_ss_yr(i)%rech = gw_hyd_ss_yr(i)%rech / time%day_end_yr
        gw_hyd_ss_yr(i)%gwet = gw_hyd_ss_yr(i)%gwet / time%day_end_yr
        gw_hyd_ss_yr(i)%gwsw = gw_hyd_ss_yr(i)%gwsw / time%day_end_yr
        gw_hyd_ss_yr(i)%swgw = gw_hyd_ss_yr(i)%swgw / time%day_end_yr
        gw_hyd_ss_yr(i)%satx = gw_hyd_ss_yr(i)%satx / time%day_end_yr
        gw_hyd_ss_yr(i)%soil = gw_hyd_ss_yr(i)%soil / time%day_end_yr
        gw_hyd_ss_yr(i)%latl = gw_hyd_ss_yr(i)%latl / time%day_end_yr
        gw_hyd_ss_yr(i)%bndr = gw_hyd_ss_yr(i)%bndr / time%day_end_yr
        gw_hyd_ss_yr(i)%ppag = gw_hyd_ss_yr(i)%ppag / time%day_end_yr
        gw_hyd_ss_yr(i)%ppdf = gw_hyd_ss_yr(i)%ppdf / time%day_end_yr
        gw_hyd_ss_yr(i)%ppex = gw_hyd_ss_yr(i)%ppex / time%day_end_yr
        gw_hyd_ss_yr(i)%tile = gw_hyd_ss_yr(i)%tile / time%day_end_yr
        gw_hyd_ss_yr(i)%resv = gw_hyd_ss_yr(i)%resv / time%day_end_yr
        gw_hyd_ss_yr(i)%wetl = gw_hyd_ss_yr(i)%wetl / time%day_end_yr
        gw_hyd_ss_yr(i)%canl = gw_hyd_ss_yr(i)%canl / time%day_end_yr
        gw_hyd_ss_yr(i)%fpln = gw_hyd_ss_yr(i)%fpln / time%day_end_yr
        gw_hyd_ss_yr(i)%pond = gw_hyd_ss_yr(i)%pond / time%day_end_yr
        gw_hyd_ss_yr(i)%phyt = gw_hyd_ss_yr(i)%phyt / time%day_end_yr
      enddo

      !compute average daily heat fluxes (MJ/day) for the year
      if(gw_heat_flag == 1) then
        do i=1,ncell
          gw_heat_ss_yr(i)%rech = (gw_heat_ss_yr(i)%rech/1000000.) / time%day_end_yr !J --> MJ
          gw_heat_ss_yr(i)%gwet = (gw_heat_ss_yr(i)%gwet/1000000.) / time%day_end_yr !J --> MJ
          gw_heat_ss_yr(i)%gwsw = (gw_heat_ss_yr(i)%gwsw/1000000.) / time%day_end_yr !J --> MJ
          gw_heat_ss_yr(i)%swgw = (gw_heat_ss_yr(i)%swgw/1000000.) / time%day_end_yr !J --> MJ
          gw_heat_ss_yr(i)%satx = (gw_heat_ss_yr(i)%satx/1000000.) / time%day_end_yr !J --> MJ
          gw_heat_ss_yr(i)%soil = (gw_heat_ss_yr(i)%soil/1000000.) / time%day_end_yr !J --> MJ
          gw_heat_ss_yr(i)%latl = (gw_heat_ss_yr(i)%latl/1000000.) / time%day_end_yr !J --> MJ
          gw_heat_ss_yr(i)%disp = (gw_heat_ss_yr(i)%disp/1000000.) / time%day_end_yr !J --> MJ
          gw_heat_ss_yr(i)%bndr = (gw_heat_ss_yr(i)%bndr/1000000.) / time%day_end_yr !J --> MJ
          gw_heat_ss_yr(i)%ppag = (gw_heat_ss_yr(i)%ppag/1000000.) / time%day_end_yr !J --> MJ
          gw_heat_ss_yr(i)%ppex = (gw_heat_ss_yr(i)%ppex/1000000.) / time%day_end_yr !J --> MJ
          gw_heat_ss_yr(i)%tile = (gw_heat_ss_yr(i)%tile/1000000.) / time%day_end_yr !J --> MJ
          gw_heat_ss_yr(i)%resv = (gw_heat_ss_yr(i)%resv/1000000.) / time%day_end_yr !J --> MJ
          gw_heat_ss_yr(i)%wetl = (gw_heat_ss_yr(i)%wetl/1000000.) / time%day_end_yr !J --> MJ
          gw_heat_ss_yr(i)%canl = (gw_heat_ss_yr(i)%canl/1000000.) / time%day_end_yr !J --> MJ
          gw_heat_ss_yr(i)%fpln = (gw_heat_ss_yr(i)%fpln/1000000.) / time%day_end_yr !J --> MJ
          gw_heat_ss_yr(i)%pond = (gw_heat_ss_yr(i)%pond/1000000.) / time%day_end_yr !J --> MJ
        enddo
      endif

      !compute average daily solute fluxes (kg/day) for the year
      if(gw_solute_flag == 1) then
        do i=1,ncell
          do s=1,gw_nsolute
            gwsol_ss_sum(i)%solute(s)%rech = (gwsol_ss_sum(i)%solute(s)%rech/1000.) / time%day_end_yr !g --> kg
            gwsol_ss_sum(i)%solute(s)%gwsw = (gwsol_ss_sum(i)%solute(s)%gwsw/1000.) / time%day_end_yr !g --> kg
            gwsol_ss_sum(i)%solute(s)%swgw = (gwsol_ss_sum(i)%solute(s)%swgw/1000.) / time%day_end_yr !g --> kg
            gwsol_ss_sum(i)%solute(s)%soil = (gwsol_ss_sum(i)%solute(s)%soil/1000.) / time%day_end_yr !g --> kg
            gwsol_ss_sum(i)%solute(s)%satx = (gwsol_ss_sum(i)%solute(s)%satx/1000.) / time%day_end_yr !g --> kg
            gwsol_ss_sum(i)%solute(s)%ppex = (gwsol_ss_sum(i)%solute(s)%ppex/1000.) / time%day_end_yr !g --> kg
            gwsol_ss_sum(i)%solute(s)%tile = (gwsol_ss_sum(i)%solute(s)%tile/1000.) / time%day_end_yr !g --> kg
            gwsol_ss_sum(i)%solute(s)%resv = (gwsol_ss_sum(i)%solute(s)%resv/1000.) / time%day_end_yr !g --> kg
            gwsol_ss_sum(i)%solute(s)%wetl = (gwsol_ss_sum(i)%solute(s)%wetl/1000.) / time%day_end_yr !g --> kg
            gwsol_ss_sum(i)%solute(s)%canl = (gwsol_ss_sum(i)%solute(s)%canl/1000.) / time%day_end_yr !g --> kg
            gwsol_ss_sum(i)%solute(s)%fpln = (gwsol_ss_sum(i)%solute(s)%fpln/1000.) / time%day_end_yr !g --> kg
            gwsol_ss_sum(i)%solute(s)%pond = (gwsol_ss_sum(i)%solute(s)%pond/1000.) / time%day_end_yr !g --> kg
            gwsol_ss_sum(i)%solute(s)%advn = (gwsol_ss_sum(i)%solute(s)%advn/1000.) / time%day_end_yr !g --> kg
            gwsol_ss_sum(i)%solute(s)%disp = (gwsol_ss_sum(i)%solute(s)%disp/1000.) / time%day_end_yr !g --> kg
            gwsol_ss_sum(i)%solute(s)%rcti = (gwsol_ss_sum(i)%solute(s)%rcti/1000.) / time%day_end_yr !g --> kg
            gwsol_ss_sum(i)%solute(s)%rcto = (gwsol_ss_sum(i)%solute(s)%rcto/1000.) / time%day_end_yr !g --> kg
            gwsol_ss_sum(i)%solute(s)%minl = (gwsol_ss_sum(i)%solute(s)%minl/1000.) / time%day_end_yr !g --> kg
            gwsol_ss_sum(i)%solute(s)%sorb = (gwsol_ss_sum(i)%solute(s)%sorb/1000.) / time%day_end_yr !g --> kg
          enddo
        enddo
      endif

      !recharge ---------------------------------------------------------------------------------
      write(out_gw_rech,*) 'Recharge (m3/day) for year:',time%yrc
      if(grid_type == "structured") then
        grid_val = 0.
        do i=1,grid_nrow
          do j=1,grid_ncol
            if(cell_id_usg(i,j) > 0) then
              grid_val(i,j) = gw_hyd_ss_yr(cell_id_usg(i,j))%rech
            endif
          enddo
        enddo
        do i=1,grid_nrow
          write(out_gw_rech,101) (grid_val(i,j),j=1,grid_ncol)
        enddo
      else
        write(out_gw_rech,122) (gw_hyd_ss_yr(i)%rech,i=1,ncell)
      endif
      write(out_gw_rech,*)
      if(gw_heat_flag == 1) then !heat flux
        write(out_heat_rech,*) 'Recharge heat (MJ/day) for year:',time%yrc
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_heat_ss_yr(cell_id_usg(i,j))%rech
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_heat_rech,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_heat_rech,122) (gw_heat_ss_yr(i)%rech,i=1,ncell)
        endif
        write(out_heat_rech,*)
      endif
      if(gw_solute_flag == 1) then !solute mass flux
        do s=1,gw_nsolute
          write(out_sol_rech,*) gwsol_nm(s),'recharge flux for year (kg/day):',time%yrc
          if(grid_type == "structured") then
            grid_val = 0.
            do i=1,grid_nrow
              do j=1,grid_ncol
                if(cell_id_usg(i,j) > 0) then
                  grid_val(i,j) = gwsol_ss_sum(cell_id_usg(i,j))%solute(s)%rech
                endif
              enddo
            enddo
            do i=1,grid_nrow
              write(out_sol_rech,101) (grid_val(i,j),j=1,grid_ncol)
            enddo
          else
            write(out_sol_rech,122) (gwsol_ss_sum(i)%solute(s)%rech,i=1,ncell)
          endif
          write(out_sol_rech,*)
        enddo
      endif

      !groundwater ET ---------------------------------------------------------------------------
      write(out_gw_gwet,*) 'Groundwater ET (m3/day) for year:',time%yrc
      if(grid_type == "structured") then
        grid_val = 0.
        do i=1,grid_nrow
          do j=1,grid_ncol
            if(cell_id_usg(i,j) > 0) then
              grid_val(i,j) = gw_hyd_ss_yr(cell_id_usg(i,j))%gwet
            endif
          enddo
        enddo
        do i=1,grid_nrow
          write(out_gw_gwet,101) (grid_val(i,j),j=1,grid_ncol)
        enddo
      else
        write(out_gw_gwet,122) (gw_hyd_ss_yr(i)%gwet,i=1,ncell)
      endif
      write(out_gw_gwet,*)
      if(gw_heat_flag == 1) then !heat flux
        write(out_heat_gwet,*) 'Groundwater ET heat flux (MJ/day) for year:',time%yrc
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_heat_ss_yr(cell_id_usg(i,j))%gwet
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_heat_gwet,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_heat_gwet,122) (gw_heat_ss_yr(i)%gwet,i=1,ncell)
        endif
        write(out_heat_gwet,*)
      endif

      !gw-sw exchange rates ---------------------------------------------------------------------
      write(out_gw_gwsw,*) 'Groundwater-channel Exchange Rates (m3/day) for year:',time%yrc
      if(grid_type == "structured") then
        grid_val = 0.
        do i=1,grid_nrow
          do j=1,grid_ncol
            if(cell_id_usg(i,j) > 0) then
              grid_val(i,j) = gw_hyd_ss_yr(cell_id_usg(i,j))%gwsw
            endif
          enddo
        enddo
        do i=1,grid_nrow
          write(out_gw_gwsw,101) (grid_val(i,j),j=1,grid_ncol)
        enddo
      else
        write(out_gw_gwsw,122) (gw_hyd_ss_yr(i)%gwsw,i=1,ncell)
      endif
      write(out_gw_gwsw,*)
      if(gw_heat_flag == 1) then !heat flux
        write(out_heat_gwsw,*) 'Groundwater-channel heat exchange (MJ/day) for year:',time%yrc
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_heat_ss_yr(cell_id_usg(i,j))%gwsw
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_heat_gwsw,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_heat_gwsw,122) (gw_heat_ss_yr(i)%gwsw,i=1,ncell)
        endif
        write(out_heat_gwsw,*)
      endif
      if(gw_solute_flag == 1) then !solute mass flux
        do s=1,gw_nsolute
          write(out_sol_gwsw,*) gwsol_nm(s),'gw-channel flux for year (kg/day):',time%yrc
          if(grid_type == "structured") then
            grid_val = 0.
            do i=1,grid_nrow
              do j=1,grid_ncol
                if(cell_id_usg(i,j) > 0) then
                  grid_val(i,j) = gwsol_ss_sum(cell_id_usg(i,j))%solute(s)%gwsw
                endif
              enddo
            enddo
            do i=1,grid_nrow
              write(out_sol_gwsw,101) (grid_val(i,j),j=1,grid_ncol)
            enddo
          else
            write(out_sol_gwsw,122) (gwsol_ss_sum(i)%solute(s)%gwsw,i=1,ncell)
          endif
          write(out_sol_gwsw,*)
        enddo
      endif

      !saturation excess flow -------------------------------------------------------------------
      if(gw_satx_flag.eq.1) then
      write(out_gw_satx,*) 'Saturation Excess flow rates (m3/day) for:',time%yrc
      if(grid_type == "structured") then
        grid_val = 0.
        do i=1,grid_nrow
          do j=1,grid_ncol
            if(cell_id_usg(i,j) > 0) then
              grid_val(i,j) = gw_hyd_ss_yr(cell_id_usg(i,j))%satx
            endif
          enddo
        enddo
        do i=1,grid_nrow
          write(out_gw_satx,101) (grid_val(i,j),j=1,grid_ncol)
        enddo
      else
        write(out_gw_satx,122) (gw_hyd_ss_yr(i)%satx,i=1,ncell)
      endif
      write(out_gw_satx,*)
      if(gw_heat_flag == 1) then !heat flux
        write(out_heat_satx,*) 'Saturation Excess flow heat flux (MJ/day) for:',time%yrc
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_heat_ss_yr(cell_id_usg(i,j))%satx
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_heat_satx,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_heat_satx,122) (gw_heat_ss_yr(i)%satx,i=1,ncell)
        endif
        write(out_heat_satx,*)
      endif
      if(gw_solute_flag == 1) then !solute mass flux
        do s=1,gw_nsolute
          write(out_sol_satx,*) gwsol_nm(s),'sat. excess flux for year (kg/day):',time%yrc
          if(grid_type == "structured") then
            grid_val = 0.
            do i=1,grid_nrow
              do j=1,grid_ncol
                if(cell_id_usg(i,j) > 0) then
                  grid_val(i,j) = gwsol_ss_sum(cell_id_usg(i,j))%solute(s)%satx
                endif
              enddo
            enddo
            do i=1,grid_nrow
              write(out_sol_satx,101) (grid_val(i,j),j=1,grid_ncol)
            enddo
          else
            write(out_sol_satx,122) (gwsol_ss_sum(i)%solute(s)%satx,i=1,ncell)
          endif
          write(out_sol_satx,*)
        enddo
      endif
      endif

      !groundwater --> soil transfer ------------------------------------------------------------
      if(gw_soil_flag.eq.1) then
      write(out_gw_soil,*) 'Groundwater --> Soil Transfer rates (m3/day) for:',time%yrc
      if(grid_type == "structured") then
        grid_val = 0.
        do i=1,grid_nrow
          do j=1,grid_ncol
            if(cell_id_usg(i,j) > 0) then
              grid_val(i,j) = gw_hyd_ss_yr(cell_id_usg(i,j))%soil
            endif
          enddo
        enddo
        do i=1,grid_nrow
          write(out_gw_soil,101) (grid_val(i,j),j=1,grid_ncol)
        enddo
      else
        write(out_gw_soil,122) (gw_hyd_ss_yr(i)%soil,i=1,ncell)
      endif
      write(out_gw_soil,*)
      if(gw_heat_flag == 1) then !heat flux
        write(out_heat_soil,*) 'Groundwater --> Soil Transfer heat flux (MJ/day) for:',time%yrc
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_heat_ss_yr(cell_id_usg(i,j))%soil
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_heat_soil,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_heat_soil,122) (gw_heat_ss_yr(i)%soil,i=1,ncell)
        endif
        write(out_heat_soil,*)
      endif
      if(gw_solute_flag == 1) then !solute mass flux
        do s=1,gw_nsolute
          write(out_sol_soil,*) gwsol_nm(s),'gw-->soil flux for year (kg/day):',time%yrc
          if(grid_type == "structured") then
            grid_val = 0.
            do i=1,grid_nrow
              do j=1,grid_ncol
                if(cell_id_usg(i,j) > 0) then
                  grid_val(i,j) = gwsol_ss_sum(cell_id_usg(i,j))%solute(s)%soil
                endif
              enddo
            enddo
            do i=1,grid_nrow
              write(out_sol_soil,101) (grid_val(i,j),j=1,grid_ncol)
            enddo
          else
            write(out_sol_soil,122) (gwsol_ss_sum(i)%solute(s)%soil,i=1,ncell)
          endif
          write(out_sol_soil,*)
        enddo
      endif
      endif

      !lateral flow -----------------------------------------------------------------------------
      write(out_lateral,*) 'Lateral flow for year:',time%yrc
      if(grid_type == "structured") then
        grid_val = 0.
        do i=1,grid_nrow
          do j=1,grid_ncol
            if(cell_id_usg(i,j) > 0) then
              grid_val(i,j) = gw_hyd_ss_yr(cell_id_usg(i,j))%latl
            endif
          enddo
        enddo
        do i=1,grid_nrow
          write(out_lateral,101) (grid_val(i,j),j=1,grid_ncol)
        enddo
      else
        write(out_lateral,122) (gw_hyd_ss_yr(i)%latl,i=1,ncell)
      endif
      write(out_lateral,*)

      !tile drain flow --------------------------------------------------------------------------
      if(gw_tile_flag.eq.1) then
      write(out_gw_tile,*) 'Tile Drain Outflow rates (m3/day) for:',time%yrc
      if(grid_type == "structured") then
        grid_val = 0.
        do i=1,grid_nrow
          do j=1,grid_ncol
            if(cell_id_usg(i,j) > 0) then
              grid_val(i,j) = gw_hyd_ss_yr(cell_id_usg(i,j))%tile
            endif
          enddo
        enddo
        do i=1,grid_nrow
          write(out_gw_tile,101) (grid_val(i,j),j=1,grid_ncol)
        enddo
      else
        write(out_gw_tile,122) (gw_hyd_ss_yr(i)%tile,i=1,ncell)
      endif
      write(out_gw_tile,*)
      if(gw_heat_flag == 1) then !heat flux
        write(out_heat_tile,*) 'Tile Drain Outflow heat flux (MJ/day) for:',time%yrc
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_heat_ss_yr(cell_id_usg(i,j))%tile
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_heat_tile,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_heat_tile,122) (gw_heat_ss_yr(i)%tile,i=1,ncell)
        endif
        write(out_heat_tile,*)
      endif
      if(gw_solute_flag == 1) then !solute mass flux
        do s=1,gw_nsolute
          write(out_sol_tile,*) gwsol_nm(s),'tile drain flux for year (kg/day):',time%yrc
          if(grid_type == "structured") then
            grid_val = 0.
            do i=1,grid_nrow
              do j=1,grid_ncol
                if(cell_id_usg(i,j) > 0) then
                  grid_val(i,j) = gwsol_ss_sum(cell_id_usg(i,j))%solute(s)%tile
                endif
              enddo
            enddo
            do i=1,grid_nrow
              write(out_sol_tile,101) (grid_val(i,j),j=1,grid_ncol)
            enddo
          else
            write(out_sol_tile,122) (gwsol_ss_sum(i)%solute(s)%tile,i=1,ncell)
          endif
          write(out_sol_tile,*)
        enddo
      endif
      endif

      !pumping (irrigation)
      write(out_gw_ppag,*) 'Pumping rates (m3/day) for year:',time%yrc
      if(grid_type == "structured") then
        grid_val = 0.
        do i=1,grid_nrow
          do j=1,grid_ncol
            if(cell_id_usg(i,j) > 0) then
              grid_val(i,j) = gw_hyd_ss_yr(cell_id_usg(i,j))%ppag
            endif
          enddo
        enddo
        do i=1,grid_nrow
          write(out_gw_ppag,101) (grid_val(i,j),j=1,grid_ncol)
        enddo
      else
        write(out_gw_ppag,122) (gw_hyd_ss_yr(i)%ppag,i=1,ncell)
      endif
      write(out_gw_ppag,*)
      if(gw_heat_flag == 1) then !heat flux
        write(out_heat_ppag,*) 'Irrigation pumping heat flux (MJ/day) for year:',time%yrc
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_heat_ss_yr(cell_id_usg(i,j))%ppag
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_heat_ppag,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_heat_ppag,122) (gw_heat_ss_yr(i)%ppag,i=1,ncell)
        endif
        write(out_heat_ppag,*)
      endif
      if(gw_solute_flag == 1) then !solute mass flux
        do s=1,gw_nsolute
          write(out_sol_ppag,*) gwsol_nm(s),'ag pumping flux for year (kg/day):',time%yrc
          if(grid_type == "structured") then
            grid_val = 0.
            do i=1,grid_nrow
              do j=1,grid_ncol
                if(cell_id_usg(i,j) > 0) then
                  grid_val(i,j) = gwsol_ss_sum(cell_id_usg(i,j))%solute(s)%ppag
                endif
              enddo
            enddo
            do i=1,grid_nrow
              write(out_sol_ppag,101) (grid_val(i,j),j=1,grid_ncol)
            enddo
          else
            write(out_sol_ppag,122) (gwsol_ss_sum(i)%solute(s)%ppag,i=1,ncell)
          endif
          write(out_sol_ppag,*)
        enddo
      endif

      !pumping (irrigation) (for HRUs)
      do i=1,sp_ob%hru
        hru_pump_yr_all(i,time%yrs) = hru_pump_yr(i)
      enddo
      hru_pump_yr = 0.

      !pumping deficit (not satisfied) (irrigation)
      write(out_gw_pumpdef,*) 'Pumping rates (m3/day) not satisfied for year:',time%yrc
      if(grid_type == "structured") then
        grid_val = 0.
        do i=1,grid_nrow
          do j=1,grid_ncol
            if(cell_id_usg(i,j) > 0) then
              grid_val(i,j) = gw_hyd_ss_yr(cell_id_usg(i,j))%ppdf
            endif
          enddo
        enddo
        do i=1,grid_nrow
          write(out_gw_pumpdef,101) (grid_val(i,j),j=1,grid_ncol)
        enddo
      else
        write(out_gw_pumpdef,122) (gw_hyd_ss_yr(i)%ppdf,i=1,ncell)
      endif
      write(out_gw_pumpdef,*)

      !pumping (user specified) -----------------------------------------------------------------
      if(gw_pumpex_flag) then
      write(out_gw_ppex,*) 'Pumping rates (m3/day) for year:',time%yrc
      if(grid_type == "structured") then
        grid_val = 0.
        do i=1,grid_nrow
          do j=1,grid_ncol
            if(cell_id_usg(i,j) > 0) then
              grid_val(i,j) = gw_hyd_ss_yr(cell_id_usg(i,j))%ppex
            endif
          enddo
        enddo
        do i=1,grid_nrow
          write(out_gw_ppex,101) (grid_val(i,j),j=1,grid_ncol)
        enddo
      else
        write(out_gw_ppex,122) (gw_hyd_ss_yr(i)%ppex,i=1,ncell)
      endif
      write(out_gw_ppex,*)
      if(gw_heat_flag == 1) then !heat flux
        write(out_heat_ppex,*) 'Pumping heat fluxes (MJ/day) for year:',time%yrc
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_heat_ss_yr(cell_id_usg(i,j))%ppex
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_heat_ppex,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_heat_ppex,122) (gw_heat_ss_yr(i)%ppex,i=1,ncell)
        endif
        write(out_heat_ppex,*)
      endif
      if(gw_solute_flag == 1) then !solute mass flux
        do s=1,gw_nsolute
          write(out_sol_ppex,*) gwsol_nm(s),'ex pumping flux for year (kg/day):',time%yrc
          if(grid_type == "structured") then
            grid_val = 0.
            do i=1,grid_nrow
              do j=1,grid_ncol
                if(cell_id_usg(i,j) > 0) then
                  grid_val(i,j) = gwsol_ss_sum(cell_id_usg(i,j))%solute(s)%ppex
                endif
              enddo
            enddo
            do i=1,grid_nrow
              write(out_sol_ppex,101) (grid_val(i,j),j=1,grid_ncol)
            enddo
          else
            write(out_sol_ppex,122) (gwsol_ss_sum(i)%solute(s)%ppex,i=1,ncell)
          endif
          write(out_sol_ppex,*)
        enddo
      endif
      endif

      !groundwater-reservoir exchange -----------------------------------------------------------
      if(gw_res_flag) then
      write(out_gw_resv,*) 'Groundwater-Reservoir Exchange rates (m3/day) for:',time%yrc
      if(grid_type == "structured") then
        grid_val = 0.
        do i=1,grid_nrow
          do j=1,grid_ncol
            if(cell_id_usg(i,j) > 0) then
              grid_val(i,j) = gw_hyd_ss_yr(cell_id_usg(i,j))%resv
            endif
          enddo
        enddo
        do i=1,grid_nrow
          write(out_gw_resv,101) (grid_val(i,j),j=1,grid_ncol)
        enddo
      else
        write(out_gw_resv,122) (gw_hyd_ss_yr(i)%resv,i=1,ncell)
      endif
      write(out_gw_resv,*)
      if(gw_heat_flag == 1) then !heat flux
        write(out_heat_resv,*) 'Groundwater-Reservoir heat flux (MJ/day) for:',time%yrc
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_heat_ss_yr(cell_id_usg(i,j))%resv
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_heat_resv,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_heat_resv,122) (gw_heat_ss_yr(i)%resv,i=1,ncell)
        endif
        write(out_heat_resv,*)
      endif
      if(gw_solute_flag == 1) then !solute mass flux
        do s=1,gw_nsolute
          write(out_sol_resv,*) gwsol_nm(s),'gw-reservoir flux for year (kg/day):',time%yrc
          if(grid_type == "structured") then
            grid_val = 0.
            do i=1,grid_nrow
              do j=1,grid_ncol
                if(cell_id_usg(i,j) > 0) then
                  grid_val(i,j) = gwsol_ss_sum(cell_id_usg(i,j))%solute(s)%resv
                endif
              enddo
            enddo
            do i=1,grid_nrow
              write(out_sol_resv,101) (grid_val(i,j),j=1,grid_ncol)
            enddo
          else
            write(out_sol_resv,122) (gwsol_ss_sum(i)%solute(s)%resv,i=1,ncell)
          endif
          write(out_sol_resv,*)
        enddo
      endif
      endif

      !groundwater-wetland exchange -------------------------------------------------------------
      if(gw_wet_flag) then
      write(out_gw_wetl,*) 'Groundwater outflow rates (m3/day) to wetlands for:',time%yrc
      if(grid_type == "structured") then
        grid_val = 0.
        do i=1,grid_nrow
          do j=1,grid_ncol
            if(cell_id_usg(i,j) > 0) then
              grid_val(i,j) = gw_hyd_ss_yr(cell_id_usg(i,j))%wetl
            endif
          enddo
        enddo
        do i=1,grid_nrow
          write(out_gw_wetl,101) (grid_val(i,j),j=1,grid_ncol)
        enddo
      else
        write(out_gw_wetl,122) (gw_hyd_ss_yr(i)%wetl,i=1,ncell)
      endif
      write(out_gw_wetl,*)
      if(gw_heat_flag == 1) then !heat flux
        write(out_heat_wetl,*) 'Groundwater heat flux (MJ/day) to wetlands for:',time%yrc
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_heat_ss_yr(cell_id_usg(i,j))%wetl
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_heat_wetl,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_heat_wetl,122) (gw_heat_ss_yr(i)%wetl,i=1,ncell)
        endif
        write(out_heat_wetl,*)
      endif
      if(gw_solute_flag == 1) then !solute mass flux
        do s=1,gw_nsolute
          write(out_sol_wetl,*) gwsol_nm(s),'gw-wetland flux for year (kg/day):',time%yrc
          if(grid_type == "structured") then
            grid_val = 0.
            do i=1,grid_nrow
              do j=1,grid_ncol
                if(cell_id_usg(i,j) > 0) then
                  grid_val(i,j) = gwsol_ss_sum(cell_id_usg(i,j))%solute(s)%wetl
                endif
              enddo
            enddo
            do i=1,grid_nrow
              write(out_sol_wetl,101) (grid_val(i,j),j=1,grid_ncol)
            enddo
          else
            write(out_sol_wetl,122) (gwsol_ss_sum(i)%solute(s)%wetl,i=1,ncell)
          endif
          write(out_sol_wetl,*)
        enddo
      endif
      endif

      !groundwater-canal exchange ---------------------------------------------------------------
      if(gw_canal_flag) then
      write(out_gw_canl,*) 'Groundwater-Canal Exchange rates (m3/day) for:',time%yrc
      if(grid_type == "structured") then
        grid_val = 0.
        do i=1,grid_nrow
          do j=1,grid_ncol
            if(cell_id_usg(i,j) > 0) then
              grid_val(i,j) = gw_hyd_ss_yr(cell_id_usg(i,j))%canl
            endif
          enddo
        enddo
        do i=1,grid_nrow
          write(out_gw_canl,101) (grid_val(i,j),j=1,grid_ncol)
        enddo
      else
        write(out_gw_canl,122) (gw_hyd_ss_yr(i)%canl,i=1,ncell)
      endif
      write(out_gw_canl,*)
      if(gw_heat_flag == 1) then !heat flux
        write(out_heat_canl,*) 'Groundwater-Canal heat flux (MJ/day) for:',time%yrc
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_heat_ss_yr(cell_id_usg(i,j))%canl
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_heat_canl,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_heat_canl,122) (gw_heat_ss_yr(i)%canl,i=1,ncell)
        endif
        write(out_heat_canl,*)
      endif
      if(gw_solute_flag == 1) then !solute mass flux
        do s=1,gw_nsolute
          write(out_sol_canl,*) gwsol_nm(s),'gw-canal flux for year (kg/day):',time%yrc
          if(grid_type == "structured") then
            grid_val = 0.
            do i=1,grid_nrow
              do j=1,grid_ncol
                if(cell_id_usg(i,j) > 0) then
                  grid_val(i,j) = gwsol_ss_sum(cell_id_usg(i,j))%solute(s)%canl
                endif
              enddo
            enddo
            do i=1,grid_nrow
              write(out_sol_canl,101) (grid_val(i,j),j=1,grid_ncol)
            enddo
          else
            write(out_sol_canl,122) (gwsol_ss_sum(i)%solute(s)%canl,i=1,ncell)
          endif
          write(out_sol_canl,*)
        enddo
      endif
      endif

      !floodplain exchange ----------------------------------------------------------------------
      if(gw_fp_flag) then
      write(out_gw_fpln,*) 'Groundwater-Floodplain Exchange rates (m3/day) for:',time%yrc
      if(grid_type == "structured") then
        grid_val = 0.
        do i=1,grid_nrow
          do j=1,grid_ncol
            if(cell_id_usg(i,j) > 0) then
              grid_val(i,j) = gw_hyd_ss_yr(cell_id_usg(i,j))%fpln
            endif
          enddo
        enddo
        do i=1,grid_nrow
          write(out_gw_fpln,101) (grid_val(i,j),j=1,grid_ncol)
        enddo
      else
        write(out_gw_fpln,122) (gw_hyd_ss_yr(i)%fpln,i=1,ncell)
      endif
      write(out_gw_fpln,*)
      if(gw_heat_flag == 1) then !heat flux
        write(out_heat_fpln,*) 'Groundwater-Floodplain heat flux (MJ/day) for:',time%yrc
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_heat_ss_yr(cell_id_usg(i,j))%fpln
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_heat_fpln,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_heat_fpln,122) (gw_heat_ss_yr(i)%fpln,i=1,ncell)
        endif
        write(out_heat_fpln,*)
      endif
      if(gw_solute_flag == 1) then !solute mass flux
        do s=1,gw_nsolute
          write(out_sol_fpln,*) gwsol_nm(s),'gw-floodplain flux for year (kg/day):',time%yrc
          if(grid_type == "structured") then
            grid_val = 0.
            do i=1,grid_nrow
              do j=1,grid_ncol
                if(cell_id_usg(i,j) > 0) then
                  grid_val(i,j) = gwsol_ss_sum(cell_id_usg(i,j))%solute(s)%fpln
                endif
              enddo
            enddo
            do i=1,grid_nrow
              write(out_sol_fpln,101) (grid_val(i,j),j=1,grid_ncol)
            enddo
          else
            write(out_sol_fpln,122) (gwsol_ss_sum(i)%solute(s)%fpln,i=1,ncell)
          endif
          write(out_sol_fpln,*)
        enddo
      endif
      endif

      !recharge pond seepage --------------------------------------------------------------------
      if(gw_pond_flag) then
      write(out_gw_pond,*) 'Recharge seepage rates (m3/day) for:',time%yrc
      if(grid_type == "structured") then
        grid_val = 0.
        do i=1,grid_nrow
          do j=1,grid_ncol
            if(cell_id_usg(i,j) > 0) then
              grid_val(i,j) = gw_hyd_ss_yr(cell_id_usg(i,j))%pond
            endif
          enddo
        enddo
        do i=1,grid_nrow
          write(out_gw_pond,101) (grid_val(i,j),j=1,grid_ncol)
        enddo
      else
        write(out_gw_pond,122) (gw_hyd_ss_yr(i)%pond,i=1,ncell)
      endif
      write(out_gw_pond,*)
      if(gw_heat_flag == 1) then !heat flux
        write(out_heat_pond,*) 'Recharge pond seepage heat flux (MJ/day) for:',time%yrc
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_heat_ss_yr(cell_id_usg(i,j))%pond
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_heat_pond,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_heat_pond,122) (gw_heat_ss_yr(i)%pond,i=1,ncell)
        endif
        write(out_heat_pond,*)
      endif
      if(gw_solute_flag == 1) then !solute mass flux
        do s=1,gw_nsolute
          write(out_sol_pond,*) gwsol_nm(s),'Recharge pond seepage flux for year (kg/day):',time%yrc
          if(grid_type == "structured") then
            grid_val = 0.
            do i=1,grid_nrow
              do j=1,grid_ncol
                if(cell_id_usg(i,j) > 0) then
                  grid_val(i,j) = gwsol_ss_sum(cell_id_usg(i,j))%solute(s)%pond
                endif
              enddo
            enddo
            do i=1,grid_nrow
              write(out_sol_pond,101) (grid_val(i,j),j=1,grid_ncol)
            enddo
          else
            write(out_sol_pond,122) (gwsol_ss_sum(i)%solute(s)%pond,i=1,ncell)
          endif
          write(out_sol_pond,*)
        enddo
      endif
      endif

      !phreatophyte transpiration ---------------------------------------------------------------
      if(gw_phyt_flag) then
      write(out_gw_phyt,*) 'Phreatophyte transpiration rates (m3/day) for:',time%yrc
      if(grid_type == "structured") then
        grid_val = 0.
        do i=1,grid_nrow
          do j=1,grid_ncol
            if(cell_id_usg(i,j) > 0) then
              grid_val(i,j) = gw_hyd_ss_yr(cell_id_usg(i,j))%phyt
            endif
          enddo
        enddo
        do i=1,grid_nrow
          write(out_gw_phyt,101) (grid_val(i,j),j=1,grid_ncol)
        enddo
      else
        write(out_gw_phyt,122) (gw_hyd_ss_yr(i)%phyt,i=1,ncell)
      endif
      write(out_gw_phyt,*)
      endif

      !chemical reaction (produced = positive values) -------------------------------------------
      if(gw_solute_flag == 1) then !solute mass flux
        do s=1,gw_nsolute
          write(out_sol_rcti,*) gwsol_nm(s),'chem. reaction flux for year (kg/day):',time%yrc
          if(grid_type == "structured") then
            grid_val = 0.
            do i=1,grid_nrow
              do j=1,grid_ncol
                if(cell_id_usg(i,j) > 0) then
                  grid_val(i,j) = gwsol_ss_sum(cell_id_usg(i,j))%solute(s)%rcti
                endif
              enddo
            enddo
            do i=1,grid_nrow
              write(out_sol_rcti,101) (grid_val(i,j),j=1,grid_ncol)
            enddo
          else
            write(out_sol_rcti,122) (gwsol_ss_sum(i)%solute(s)%rcti,i=1,ncell)
          endif
          write(out_sol_rcti,*)
        enddo
      endif

      !chemical reaction (consumed = negative values) -------------------------------------------
      if(gw_solute_flag == 1) then !solute mass flux
        do s=1,gw_nsolute
          write(out_sol_rcto,*) gwsol_nm(s),'chem. reaction flux for year (kg/day):',time%yrc
          if(grid_type == "structured") then
            grid_val = 0.
            do i=1,grid_nrow
              do j=1,grid_ncol
                if(cell_id_usg(i,j) > 0) then
                  grid_val(i,j) = gwsol_ss_sum(cell_id_usg(i,j))%solute(s)%rcto
                endif
              enddo
            enddo
            do i=1,grid_nrow
              write(out_sol_rcto,101) (grid_val(i,j),j=1,grid_ncol)
            enddo
          else
            write(out_sol_rcto,122) (gwsol_ss_sum(i)%solute(s)%rcto,i=1,ncell)
          endif
          write(out_sol_rcto,*)
        enddo
      endif

      !precipitation-dissolution ----------------------------------------------------------------
      if(gw_solute_flag == 1) then !solute mass flux
        do s=1,gw_nsolute
          write(out_sol_minl,*) gwsol_nm(s),'mineral dissolved mass for year (kg/day):',time%yrc
          if(grid_type == "structured") then
            grid_val = 0.
            do i=1,grid_nrow
              do j=1,grid_ncol
                if(cell_id_usg(i,j) > 0) then
                  grid_val(i,j) = gwsol_ss_sum(cell_id_usg(i,j))%solute(s)%minl
                endif
              enddo
            enddo
            do i=1,grid_nrow
              write(out_sol_minl,101) (grid_val(i,j),j=1,grid_ncol)
            enddo
          else
            write(out_sol_minl,122) (gwsol_ss_sum(i)%solute(s)%minl,i=1,ncell)
          endif
          write(out_sol_minl,*)
        enddo
      endif

      !sorption ---------------------------------------------------------------------------------
      if(gw_solute_flag == 1) then !solute mass flux
        do s=1,gw_nsolute
          write(out_sol_sorb,*) gwsol_nm(s),'sorption flux for year (kg/day):',time%yrc
          if(grid_type == "structured") then
            grid_val = 0.
            do i=1,grid_nrow
              do j=1,grid_ncol
                if(cell_id_usg(i,j) > 0) then
                  grid_val(i,j) = gwsol_ss_sum(cell_id_usg(i,j))%solute(s)%sorb
                endif
              enddo
            enddo
            do i=1,grid_nrow
              write(out_sol_sorb,101) (grid_val(i,j),j=1,grid_ncol)
            enddo
          else
            write(out_sol_sorb,122) (gwsol_ss_sum(i)%solute(s)%sorb,i=1,ncell)
          endif
          write(out_sol_sorb,*)
        enddo
      endif

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

      !yearly water balance ---------------------------------------------------------------------
      if(gwflag_yr == 1) then
        write(out_gwbal_yr,105) time%yrc, &
                                gw_hyd_grid_yr%chng,gw_hyd_grid_yr%rech,gw_hyd_grid_yr%gwet,gw_hyd_grid_yr%gwsw,gw_hyd_grid_yr%swgw, &
                                gw_hyd_grid_yr%satx,gw_hyd_grid_yr%soil,gw_hyd_grid_yr%latl,gw_hyd_grid_yr%bndr,gw_hyd_grid_yr%ppag, &
                                gw_hyd_grid_yr%ppex,gw_hyd_grid_yr%tile,gw_hyd_grid_yr%resv,gw_hyd_grid_yr%wetl,gw_hyd_grid_yr%canl, &
                                gw_hyd_grid_yr%fpln,gw_hyd_grid_yr%pond,gw_hyd_grid_yr%phyt,gw_hyd_grid_yr%ppdf
      endif

      !zero out annual arrays
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

      !heat flux values -------------------------------------------------------------------------
      if(gw_heat_flag == 1) then
        if(gwflag_yr == 1) then
          write(out_heatbal_yr,105) time%yrc,gw_heat_grid_yr%chng, &
                                    gw_heat_grid_yr%rech,gw_heat_grid_yr%gwet,gw_heat_grid_yr%gwsw, &
                                    gw_heat_grid_yr%swgw,gw_heat_grid_yr%satx,gw_heat_grid_yr%soil, &
                                    gw_heat_grid_yr%latl,gw_heat_grid_yr%disp,gw_heat_grid_yr%bndr, &
                                    gw_heat_grid_yr%ppag,gw_heat_grid_yr%ppex,gw_heat_grid_yr%tile, &
                                    gw_heat_grid_yr%resv,gw_heat_grid_yr%wetl,gw_heat_grid_yr%canl, &
                                    gw_heat_grid_yr%fpln,gw_heat_grid_yr%pond
        endif
        !zero out annual arrays
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

      !solute mass values -----------------------------------------------------------------------
      if(gw_solute_flag == 1) then
        do s=1,gw_nsolute !loop through the solutes
          !write out annual values
          if(gwflag_yr == 1) then
            write(out_solbal_yr+s,105) time%yrc, &
                                       sol_grid_chng_yr(s),sol_grid_rech_yr(s),sol_grid_gwsw_yr(s),sol_grid_swgw_yr(s),sol_grid_satx_yr(s), &
                                       sol_grid_soil_yr(s),sol_grid_advn_yr(s),sol_grid_disp_yr(s), &
                                       sol_grid_rcti_yr(s),sol_grid_rcto_yr(s),sol_grid_minl_yr(s),sol_grid_sorb_yr(s), &
                                       sol_grid_ppag_yr(s),sol_grid_ppex_yr(s),sol_grid_tile_yr(s),sol_grid_resv_yr(s),sol_grid_wetl_yr(s), &
                                       sol_grid_canl_yr(s),sol_grid_fpln_yr(s),sol_grid_pond_yr(s)
          endif
          !zero out values for next year
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

      !format statements
100   format(10000(f12.3))
101   format(10000(e12.3))
105   format(i8,1000(e13.4))
122   format(<out_cols>(e12.6))

      return
      end subroutine gwflow_output_yr

      subroutine gwflow_output_aa

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes average annual gwflow water/heat/solute balance
!!    values and HRU pumping summaries at the end of the simulation.
!!    (extracted from gwflow_simulate section 9)

      use gwflow_module
      use hydrograph_module
      use sd_channel_module
      use time_module

      implicit none

      integer :: i, j, s
      integer :: num_months

      !--- only execute on the last day of the simulation ---
      if(time%yrc /= time%yrc_end .or. time%day /= time%day_end) return

      !pumping for HRUs
      num_months = time%nbyr * 12
      do i=1,sp_ob%hru
        write(out_hru_pump_mo,105) i,(hru_pump_mo_all(i,j),j=1,num_months)
        write(out_hru_pump_yr,105) i,(hru_pump_yr_all(i,j),j=1,time%nbyr)
      enddo

      !average annual water balance -------------------------------------------------------------
      gw_hyd_grid_aa%chng = gw_hyd_grid_aa%chng + (vaft_grid-vbef_grid)
      gw_hyd_grid_aa%rech = gw_hyd_grid_aa%rech / time%nbyr
      gw_hyd_grid_aa%gwet = gw_hyd_grid_aa%gwet / time%nbyr
      gw_hyd_grid_aa%gwsw = gw_hyd_grid_aa%gwsw / time%nbyr
      gw_hyd_grid_aa%swgw = gw_hyd_grid_aa%swgw / time%nbyr
      gw_hyd_grid_aa%satx = gw_hyd_grid_aa%satx / time%nbyr
      gw_hyd_grid_aa%soil = gw_hyd_grid_aa%soil / time%nbyr
      gw_hyd_grid_aa%latl = gw_hyd_grid_aa%latl / time%nbyr
      gw_hyd_grid_aa%bndr = gw_hyd_grid_aa%bndr / time%nbyr
      gw_hyd_grid_aa%ppag = gw_hyd_grid_aa%ppag / time%nbyr
      gw_hyd_grid_aa%ppdf = gw_hyd_grid_aa%ppdf / time%nbyr
      gw_hyd_grid_aa%ppex = gw_hyd_grid_aa%ppex / time%nbyr
      gw_hyd_grid_aa%tile = gw_hyd_grid_aa%tile / time%nbyr
      gw_hyd_grid_aa%resv = gw_hyd_grid_aa%resv / time%nbyr
      gw_hyd_grid_aa%wetl = gw_hyd_grid_aa%wetl / time%nbyr
      gw_hyd_grid_aa%canl = gw_hyd_grid_aa%canl / time%nbyr
      gw_hyd_grid_aa%fpln = gw_hyd_grid_aa%fpln / time%nbyr
      gw_hyd_grid_aa%pond = gw_hyd_grid_aa%pond / time%nbyr
      gw_hyd_grid_aa%phyt = gw_hyd_grid_aa%phyt / time%nbyr
      if(gwflag_aa == 1) then
        write(out_gwbal_aa,105) time%yrc, &
                                gw_hyd_grid_aa%chng,gw_hyd_grid_aa%rech,gw_hyd_grid_aa%gwet,gw_hyd_grid_aa%gwsw,gw_hyd_grid_aa%swgw, &
                                gw_hyd_grid_aa%satx,gw_hyd_grid_aa%soil,gw_hyd_grid_aa%latl,gw_hyd_grid_aa%bndr,gw_hyd_grid_aa%ppag, &
                                gw_hyd_grid_aa%ppex,gw_hyd_grid_aa%tile,gw_hyd_grid_aa%resv,gw_hyd_grid_aa%wetl,gw_hyd_grid_aa%canl, &
                                gw_hyd_grid_aa%fpln,gw_hyd_grid_aa%pond,gw_hyd_grid_aa%phyt,gw_hyd_grid_aa%ppdf
      endif

      !average annual heat fluxes ---------------------------------------------------------------
      gw_heat_grid_aa%chng = gw_heat_grid_aa%chng + (heat_haft_grid-heat_hbef_grid)
      gw_heat_grid_aa%rech = gw_heat_grid_aa%rech / time%nbyr
      gw_heat_grid_aa%gwet = gw_heat_grid_aa%gwet / time%nbyr
      gw_heat_grid_aa%gwsw = gw_heat_grid_aa%gwsw / time%nbyr
      gw_heat_grid_aa%swgw = gw_heat_grid_aa%swgw / time%nbyr
      gw_heat_grid_aa%satx = gw_heat_grid_aa%satx / time%nbyr
      gw_heat_grid_aa%soil = gw_heat_grid_aa%soil / time%nbyr
      gw_heat_grid_aa%latl = gw_heat_grid_aa%latl / time%nbyr
      gw_heat_grid_aa%disp = gw_heat_grid_aa%disp / time%nbyr
      gw_heat_grid_aa%bndr = gw_heat_grid_aa%bndr / time%nbyr
      gw_heat_grid_aa%ppag = gw_heat_grid_aa%ppag / time%nbyr
      gw_heat_grid_aa%ppex = gw_heat_grid_aa%ppex / time%nbyr
      gw_heat_grid_aa%tile = gw_heat_grid_aa%tile / time%nbyr
      gw_heat_grid_aa%resv = gw_heat_grid_aa%resv / time%nbyr
      gw_heat_grid_aa%wetl = gw_heat_grid_aa%wetl / time%nbyr
      gw_heat_grid_aa%canl = gw_heat_grid_aa%canl / time%nbyr
      gw_heat_grid_aa%fpln = gw_heat_grid_aa%fpln / time%nbyr
      gw_heat_grid_aa%pond = gw_heat_grid_aa%pond / time%nbyr
      if(gwflag_aa == 1) then
        write(out_heatbal_aa,105) time%yrc,gw_heat_grid_aa%chng, &
                                  gw_heat_grid_aa%rech,gw_heat_grid_aa%gwet,gw_heat_grid_aa%gwsw, &
                                  gw_heat_grid_aa%swgw,gw_heat_grid_aa%satx,gw_heat_grid_aa%soil, &
                                  gw_heat_grid_aa%latl,gw_heat_grid_aa%disp,gw_heat_grid_aa%bndr, &
                                  gw_heat_grid_aa%ppag,gw_heat_grid_aa%ppex,gw_heat_grid_aa%tile, &
                                  gw_heat_grid_aa%resv,gw_heat_grid_aa%wetl,gw_heat_grid_aa%canl, &
                                  gw_heat_grid_aa%fpln,gw_heat_grid_aa%pond
      endif

      !average annual solute values -------------------------------------------------------------
      if(gw_solute_flag == 1) then
        do s=1,gw_nsolute
          sol_grid_chng_tt(s) = sol_grid_chng_tt(s) + (sol_grid_maft-sol_grid_mbef)
          sol_grid_rech_tt(s) = sol_grid_rech_tt(s) / time%nbyr
          sol_grid_gwsw_tt(s) = sol_grid_gwsw_tt(s) / time%nbyr
          sol_grid_swgw_tt(s) = sol_grid_swgw_tt(s) / time%nbyr
          sol_grid_satx_tt(s) = sol_grid_satx_tt(s) / time%nbyr
          sol_grid_advn_tt(s) = sol_grid_advn_tt(s) / time%nbyr
          sol_grid_disp_tt(s) = sol_grid_disp_tt(s) / time%nbyr
          sol_grid_rcti_tt(s) = sol_grid_rcti_tt(s) / time%nbyr
          sol_grid_rcto_tt(s) = sol_grid_rcto_tt(s) / time%nbyr
          sol_grid_minl_tt(s) = sol_grid_minl_tt(s) / time%nbyr
          sol_grid_sorb_tt(s) = sol_grid_sorb_tt(s) / time%nbyr
          sol_grid_ppag_tt(s) = sol_grid_ppag_tt(s) / time%nbyr
          sol_grid_ppex_tt(s) = sol_grid_ppex_tt(s) / time%nbyr
          sol_grid_tile_tt(s) = sol_grid_tile_tt(s) / time%nbyr
          sol_grid_soil_tt(s) = sol_grid_soil_tt(s) / time%nbyr
          sol_grid_resv_tt(s) = sol_grid_resv_tt(s) / time%nbyr
          sol_grid_wetl_tt(s) = sol_grid_wetl_tt(s) / time%nbyr
          sol_grid_canl_tt(s) = sol_grid_canl_tt(s) / time%nbyr
          sol_grid_fpln_tt(s) = sol_grid_fpln_tt(s) / time%nbyr
          sol_grid_pond_tt(s) = sol_grid_pond_tt(s) / time%nbyr
          if(gwflag_aa == 1) then
            write(out_solbal_aa+s,105) time%yrc, &
                                       sol_grid_chng_tt(s),sol_grid_rech_tt(s),sol_grid_gwsw_tt(s),sol_grid_swgw_tt(s),sol_grid_satx_tt(s), &
                                       sol_grid_soil_tt(s),sol_grid_advn_tt(s),sol_grid_disp_tt(s), &
                                       sol_grid_rcti_tt(s),sol_grid_rcto_tt(s),sol_grid_minl_tt(s),sol_grid_sorb_tt(s), &
                                       sol_grid_ppag_tt(s),sol_grid_ppex_tt(s),sol_grid_tile_tt(s),sol_grid_resv_tt(s),sol_grid_wetl_tt(s), &
                                       sol_grid_canl_tt(s),sol_grid_fpln_tt(s),sol_grid_pond_tt(s)
          endif
        enddo !next solute
      endif

      !if soft calibration, prepare for next simulation
      sim_month = 1

      !write out groundwater transit time to channels and tiles
      !channels
      if(gw_ttime) then
        if(grid_type == "structured") then
          grid_val = 0.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(cell_id_usg(i,j) > 0) then
                grid_val(i,j) = gw_cell_chan_time(cell_id_usg(i,j))
              endif
            enddo
          enddo
          do i=1,grid_nrow
            write(out_gw_transit_chan,101) (grid_val(i,j),j=1,grid_ncol)
          enddo
        else
          write(out_gw_transit_chan,121) (gw_cell_chan_time(i),i=1,ncell)
        endif
        !tiles
        if(gw_tile_flag) then
          if(grid_type == "structured") then
            grid_val = 0.
            do i=1,grid_nrow
              do j=1,grid_ncol
                if(cell_id_usg(i,j) > 0) then
                  grid_val(i,j) = gw_cell_tile_time(cell_id_usg(i,j))
                endif
              enddo
            enddo
            do i=1,grid_nrow
              write(out_gw_transit_tile,101) (grid_val(i,j),j=1,grid_ncol)
            enddo
          else
            write(out_gw_transit_tile,122) (gw_cell_tile_time(i),i=1,ncell)
          endif
        endif
      endif

      !format statements
101   format(10000(e12.3))
105   format(i8,1000(e13.4))
121   format(<out_cols>(e12.3))
122   format(<out_cols>(e12.6))

      return
      end subroutine gwflow_output_aa
