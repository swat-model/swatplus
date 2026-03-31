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
!!    this subroutine writes daily gwflow output (basin water balance,
!!    observation wells, record file)

      implicit none

      return
      end subroutine gwflow_output_day


      subroutine gwflow_output_mon

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes monthly gwflow output (basin water balance,
!!    cell-level grids, pumping per HRU, cell groups)

      implicit none

      return
      end subroutine gwflow_output_mon


      subroutine gwflow_output_yr

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes yearly gwflow output (basin water balance,
!!    cell-level grids, pumping per HRU)

      implicit none

      return
      end subroutine gwflow_output_yr


      subroutine gwflow_output_aa

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes average annual gwflow output and
!!    end-of-simulation summaries

      implicit none

      return
      end subroutine gwflow_output_aa
