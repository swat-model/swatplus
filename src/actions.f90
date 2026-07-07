      subroutine actions (ob_cur, ob_num, idtbl)
      use conditional_module
      use climate_module
      use time_module
      use aquifer_module
      use hru_module, only : hru, cn2, fertno3, fertnh3, fertorgn, fertorgp, fertsolp,   &
        ihru, ipl, isol, ndeat, phubase, sol_sumno3, sol_sumsolp
      use soil_module
      use plant_module
      use plant_data_module
      use mgt_operations_module  
      use landuse_data_module
      use tillage_data_module
      use reservoir_module
      use sd_channel_module
      use septic_data_module
      use hru_lte_module
      use basin_module
      use organic_mineral_mass_module
      use hydrograph_module
      use output_landscape_module
      use conditional_module
      use constituent_mass_module
      use calibration_data_module
      use fertilizer_data_module
      use maximum_data_module
      use tiles_data_module
      use gwflow_module, only : gwflow_flag, hru_num_cells, hru_cells, cell_size, gw_cell_head,  &
            gw_cell_bot, gw_cell_sy, gw_cell_ss_pumpag

      implicit none

      integer, intent (in)  :: ob_cur      !none     |sequential number of individual objects
      integer, intent (in)  :: ob_num      !none     |sequential number for all objects
      integer, intent (in)  :: idtbl       !none     |
      integer :: icom                      !none     |
      integer :: iac                       !none     |counter
      integer :: ial                       !none     |counter
      integer :: jj                        !none     |counter
      integer :: i                         !none     |counter
      integer :: iburn                     !none     |burn type from fire data base
      integer :: idtill                    !none     |tillage type
      integer :: ifertop                   !         |surface application fraction from chem app data base
      integer :: ifrt                      !         |fertilizer type from fert data base
      integer :: ipestop                   !         |surface application fraction from chem app data base
      integer :: ipst                      !         |pesticide type from pest data base
      integer :: iharvop                   !         |harvest operation type
      integer :: iihru                     !         |
      integer :: ilu                       !         |landuse type 
      integer :: j                         !none     |counter
      integer :: iob
      integer :: idp                       !         |
      integer :: istr                      !         |
      integer :: istr1                     !         |
      integer :: iob_out
      integer :: inhyd                     !         |
      integer :: ihyd_in                   !         |
      integer :: icon                      !         |
      integer :: iplt_bsn
      integer :: irrop                     !         |
      integer :: igr
      integer :: ireg                      !         |
      integer :: ilum
      integer :: iwro                      !         |
      integer :: isrc
      integer :: irr_ob
      integer :: iaqdb
      integer :: isched
      real :: hiad1                        !         |
      real :: irrig_m3                     !         |
      real :: amt_mm                       !         |
      real :: biomass                      !         |
      real :: frt_kg
      real :: wur                          !         |
      real :: frac                         !         |
      real :: rto                          !         |
      real :: rto1                         !         |
      real :: pest_kg                      !kg/ha    |amount of pesticide applied 
      real :: irr_mm
      real :: vol_avail
      real :: chg_par                      !variable |new parameter value
      real :: yield 
      real :: sumpst = 0.
      real :: rock
      real :: p_factor
      real :: cn_prev
      real :: irrig_total,gwvol_demand,gwvol_avail,gwvol_diff,gwmm_diff,gwvol_removed !rtb gwflow
      integer :: cell_row,cell_col !rtb gwflow
      character(len=1) :: action           !         |
      character(len=25) :: lu_prev         !         |

      do iac = 1, d_tbl%acts
        action = "n"
        do ial = 1, d_tbl%alts
          if (d_tbl%act_hit(ial) == "y" .and. d_tbl%act_outcomes(iac,ial) == "y") then
            action = "y"
            exit
          end if
        end do
      
        if (action == "y") then
          select case (d_tbl%act(iac)%typ)
          
          !irrigation demand - hru action
          case ("irr_demand")
            ipl = 1
            j = d_tbl%act(iac)%ob_num
            if (j == 0) j = ob_cur

            irrop = d_tbl%act_typ(iac)      ! irrigation application type in irr.ops
            irrig(j)%demand = d_tbl%act(iac)%const * hru(j)%area_ha * 10.       ! m3 = mm * ha * 10.
            
            !! if unlimited source, set irrigation applied directly to hru
            if (d_tbl%act(iac)%file_pointer == "unlim") then
              irrig(j)%applied = irrop_db(irrop)%amt_mm * irrop_db(irrop)%eff * (1. - irrop_db(irrop)%surq)
              irrig(j)%runoff = irrop_db(irrop)%amt_mm * irrop_db(irrop)%eff * irrop_db(irrop)%surq
              
              
              !rtb gwflow - connect irrigation to groundwater pumping from aquifer
              if(gwflow_flag ==1) then
                irrig_total = (irrop_db(irrop)%amt_mm/1000.) * hru(j)%area_ha * 10000. !m3 of irrigation water
                if(hru_num_cells(j).gt.0) then
                  gwvol_demand = irrig_total / hru_num_cells(j) !groundwater to remove from each cell connected to the HRU
                  !loop through the cells that are connected to the HRU
                  gwvol_diff = 0.
                  do i=1,hru_num_cells(j)
                    cell_row = hru_cells(j,i,1)
                    cell_col = hru_cells(j,i,2)
                    !check for available groundwater
                    gwvol_avail = ((gw_cell_head(cell_row,cell_col)-gw_cell_bot(cell_row,cell_col)) * (cell_size * cell_size)) * gw_cell_Sy(cell_row,cell_col) !m3 of groundwater available for removal
                    if(gwvol_avail.lt.gwvol_demand) then
                      gwvol_removed = gwvol_avail
                      gwvol_diff = gwvol_diff + (gwvol_demand - gwvol_avail) !track the amount that is not available for irrigation
                    else
                      gwvol_removed = gwvol_demand
                    endif
                    gw_cell_ss_pumpag(cell_row,cell_col) = gwvol_removed * (-1) !m3 --> store for groundwater balance calculations in gwflow_simulate (negative = leaving the aquifer)
                  enddo
                  !if available < demand, re-calculate irrigation applied
                  gwmm_diff = gwvol_diff  / (hru(j)%area_ha * 10000.) * 1000. !m3 --> mm
                  irrig(j)%applied = (irrop_db(irrop)%amt_mm - gwmm_diff) * irrop_db(irrop)%eff * (1. - irrop_db(irrop)%surq) !decrease ammount by the difference
                  irrig(j)%runoff = (irrop_db(irrop)%amt_mm - gwmm_diff) * irrop_db(irrop)%eff * irrop_db(irrop)%surq
                  if(irrig(j)%applied.lt.0) irrig(j)%applied = 0.
                  if(irrig(j)%runoff.lt.0) irrig(j)%runoff = 0.
                endif
              endif
                
              
              !set organics and constituents from irr.ops ! irrig(j)%water =  cs_irr(j) = 
              if (pco%mgtout == "y") then
                write (2612, *) j, time%yrc, time%mo, time%day_mo, "        ", "IRRIGATE", phubase(j),  &
                    pcom(j)%plcur(ipl)%phuacc, soil(j)%sw,pl_mass(j)%tot(ipl)%m, rsd1(j)%tot(ipl)%m, &
                    sol_sumno3(j), sol_sumsolp(j), irrig(j)%applied
              end if
            else
              !! set demand for irrigation from channel, reservoir or aquifer
              if (pco%mgtout == "y") then
                write (2612, *) j, time%yrc, time%mo, time%day_mo, "        ", "IRRIG_DMD", phubase(j), &
                    pcom(j)%plcur(ipl)%phuacc, soil(j)%sw,pl_mass(j)%tot(ipl)%m, rsd1(j)%tot(ipl)%m, &
                    sol_sumno3(j), sol_sumsolp(j), irrop_db(irrop)%amt_mm
              end if
            end if

          !irrigate - hru action
          case ("irrigate")
            ipl = 1
            j = ob_cur                      ! hru number 
            isrc = d_tbl%act(iac)%ob_num    ! source object type number
            irrop = d_tbl%act_typ(iac)      ! irrigation application type in irr.ops

            irrig(j)%applied = d_tbl%act(iac)%const * irrop_db(irrop)%eff * (1. - irrop_db(irrop)%surq)
            irrig(j)%runoff = d_tbl%act(iac)%const * irrop_db(irrop)%surq

            irrig(j)%demand = d_tbl%act(iac)%const * hru(j)%area_ha * 10.       ! m3 = mm * ha * 10.

            !select object type
            iob = d_tbl%act(iac)%ob_num
            select case (d_tbl%act(iac)%ob)
            case ("aqu")
              if (aqu_d(iob)%stor > 0.001) then
                rto = d_tbl%act(iac)%const / aqu_d(iob)%stor            ! ratio of water removed from aquifer volume
              else
                rto = 1.
              end if
              rto1 = (1. - rto)
              irrig(j)%water%flo = rto * aqu_d(iob)%flo                 ! organics in irrigation water
              !! need to conver irrig(j)%water%flo from mm  to m3 
              aqu_d(iob)%stor = rto1 * aqu_d(iob)%stor                  ! remainder stays in aquifer
              cs_irr(iob) = rto * cs_aqu(iob)                           ! constituents in irrigation water
              cs_aqu(iob) = rto1 * cs_aqu(iob)                          ! remainder stays in aquifer
              
            case ("cha")
              if (aqu_d(iob)%stor > 0.001) then
                rto = irrig(j)%demand / ch_stor(iob)%flo                ! ratio of water removed from channel volume
              else
                rto = 1.
              end if
              rto1 = (1. - rto)
              irrig(j)%water = rto * ch_stor(iob)                       ! organics in irrigation water
              ch_stor(iob) = rto1 * ch_stor(iob)                        ! remainder stays in channel
              cs_irr(iob) = rto * ch_water(iob)                         ! constituents in irrigation water
              ch_water(iob) = rto1 * ch_water(iob)                      ! remainder stays in channel
              
            case ("res")
              if (res(iob)%flo > 0.001) then
                rto = irrig(j)%demand / res(iob)%flo                    ! ratio of water removed from res volume
              else
                rto = 1.
              end if
              rto1 = (1. - rto)
              irrig(j)%water = rto * res(iob)                           ! organics in irrigation water
              res(iob) = rto1 * res(iob)                                ! remainder stays in reservoir
              cs_irr(iob) = rto * res_water(iob)                        ! constituents in irrigation water
              res_water(iob) = rto1 * res_water(iob)                    ! remainder stays in reservoir
              
            end select
                  
            if (pco%mgtout == "y") then
              write (2612, *) j, time%yrc, time%mo, time%day_mo, "        ", "IRRIGATE", phubase(j),  &
                  pcom(j)%plcur(ipl)%phuacc, soil(j)%sw,pl_mass(j)%tot(ipl)%m, rsd1(j)%tot(ipl)%m, &
                  sol_sumno3(j), sol_sumsolp(j), irrig(j)%demand
            end if

          !irrigate - wro action
          case ("allocate_wro")
            iwro = d_tbl%act(iac)%ob_num
            isrc = Int(d_tbl%act(iac)%const)
            
            !! loop through each field in the water rights object
            do irr_ob = 1, wro(iwro)%num_objs
              
              irrop = wro(iwro)%field(irr_ob)%irr_no
              j = wro(iwro)%field(irr_ob)%ob_num
              if (irrig(j)%demand > 0.) then
              
            !! determine water available for allocating by source object
            select case (d_tbl%act(iac)%option)
            case ("aqu")
                iob = sp_ob1%aqu + isrc - 1
                iaqdb = ob(iob)%props
                vol_avail = (wro(iwro)%min_mon(time%mo) - aqu_d(isrc)%dep_wt) * aqu_prm(iaqdb)%spyld *      &
                                ob(iob)%area_ha * 10000.                             !m * m/m * ha * 10000. = m3
            case ("cha")
                iob = sp_ob1%chandeg + isrc - 1
                vol_avail = (ob(iob)%hd(1)%flo / 86400. - wro(iwro)%min_mon(time%mo)) * 86400. !min_mon flow = cms -> m3

            case ("res")
                vol_avail = res(isrc)%flo - wro(iwro)%min_mon(time%mo) * res_ob(isrc)%pvol * 10000.  !min_mon vol = ha-m -> m3

            end select
                  
              !! check if enough water is available - assume irrigate if > 25% of demand
              if (vol_avail > .25 * irrig(j)%demand) then
                  
              irrig_m3 = amin1 (vol_avail, irrig(j)%demand)
              irr_mm = irrig_m3 / (hru(j)%area_ha * 10.)  ! convert to mm for hru application
              irrig(j)%applied = irr_mm * irrop_db(irrop)%eff * (1. - irrop_db(irrop)%surq)
              irrig(j)%runoff = irr_mm * irrop_db(irrop)%surq

            !! allocate irrigation water for each field in the water rights object
            select case (d_tbl%act(iac)%file_pointer)
          
            !! first come first serve if there is a demand
            case ("fcfs_if_demand")
              
              !! remove water from source
              select case (d_tbl%act(iac)%option)
              case ("aqu")
                rto = irrig_m3 / aqu_d(isrc)%stor                            ! ratio of water removed from aquifer volume
                rto1 = (1. - rto)
                aqu_d(isrc)%stor = rto * aqu_d(isrc)%stor
                !! compute groundwater depth from surface
                aqu_d(isrc)%dep_wt = aqudb(iaqdb)%dep_bot - (aqu_d(isrc)%stor / (1000. * aqu_prm(isrc)%spyld))
                aqu_d(isrc)%dep_wt = amax1 (0., aqu_d(isrc)%dep_wt)
                irrig(j)%water%flo = rto * aqu_d(isrc)%stor                      ! irrigation water vol - not full organics
                !cs_irr(j) = rto * cs_aqu(j)                                 ! constituents in irrigation water
                !cs_aqu(j) = rto1 * cs_aqu(j)                                ! remainder stays in aquifer
              
              case ("cha")
                rto = irrig_m3 / ob(iob)%hd(1)%flo                               ! ratio of water removed from channel volume
                rto1 = (1. - rto)
                irrig(j)%water = rto * ob(iob)%hd(1)                             ! organics in irrigation water
                ob(iob)%hd(1) = rto1 * ob(iob)%hd(1)                              ! remainder stays in channel
                !cs_irr(j) = rto * ch_water(j)                                 ! constituents in irrigation water
                !ch_water(j) = rto1 * ch_water(j)                              ! remainder stays in channel
              
              case ("res")
                rto = irrig_m3 / res(j)%flo                                   ! ratio of water removed from res volume
                rto1 = (1. - rto)
                irrig(j)%water = rto * res(j)                                 ! organics in irrigation water
                res(j) = rto1 * res(j)                                        ! remainder stays in reservoir
                !cs_irr(j) = rto * res_water(j)                                ! constituents in irrigation water
                !res_water(j) = rto1 * res_water(j)                            ! remainder stays in reservoir
              end select
            end select
                                
              if (pco%mgtout == "y") then
                ipl = 1
                write (2612, *) j, time%yrc, time%mo, time%day_mo, "        ", "IRRIGATE", phubase(j), &
                  pcom(j)%plcur(ipl)%phuacc, soil(j)%sw,pl_mass(j)%tot(ipl)%m, rsd1(j)%tot(ipl)%m,  &
                  sol_sumno3(j), sol_sumsolp(j), irr_mm
              end if
              
              end if    ! irrig(j)%demand > 0.
            end if      ! vol_avail
          end do        ! irr_ob

          !fertilize
          case ("fertilize")
            j = d_tbl%act(iac)%ob_num
            if (j == 0) j = ob_cur
            
            if (pcom(j)%dtbl(idtbl)%num_actions(iac) <= Int(d_tbl%act(iac)%const2)) then
              ipl = 1
              ifrt = d_tbl%act_typ(iac)               !fertilizer type from fert data base
              frt_kg = d_tbl%act(iac)%const           !amount applied in kg/ha
              ifertop = d_tbl%act_app(iac)            !surface application fraction from chem app data base
              call pl_fert (j, ifrt, frt_kg, ifertop)

              if (pco%mgtout == "y") then
                !write (2612, *) j, time%yrc, time%mo, time%day_mo, chemapp_db(mgt%op4)%name, "    FERT", &
                write (2612, *) j, time%yrc, time%mo, time%day_mo, fertdb(ifrt)%fertnm, "    FERT",       &
                  phubase(j),pcom(j)%plcur(ipl)%phuacc, soil(j)%sw, pl_mass(j)%tot(ipl)%m,            &
                  rsd1(j)%tot(ipl)%m, sol_sumno3(j), sol_sumsolp(j), frt_kg, fertno3, fertnh3,        &
                  fertorgn, fertsolp, fertorgp
              endif
              pcom(j)%dtbl(idtbl)%num_actions(iac) = pcom(j)%dtbl(idtbl)%num_actions(iac) + 1
              pcom(j)%dtbl(idtbl)%days_act(iac) = 1     !reset days since last action
              if (iac > 1) pcom(j)%dtbl(idtbl)%days_act(iac-1) =  0     !reset previous action day counter
            end if

          !tillage
          case ("till")
            j = d_tbl%act(iac)%ob_num
            if (j == 0) j = ob_cur
            
            if (pcom(j)%dtbl(idtbl)%num_actions(iac) <= Int(d_tbl%act(iac)%const2)) then
              idtill = d_tbl%act_typ(iac)
              ipl = 1
              call mgt_newtillmix(j, 0., idtill)
            
              if (pco%mgtout == "y") then
                write (2612, *) j, time%yrc, time%mo, time%day_mo, tilldb(idtill)%tillnm, "    TILLAGE",    &
                    phubase(j), pcom(j)%plcur(ipl)%phuacc, soil(j)%sw, pl_mass(j)%tot(ipl)%m,        &
                    rsd1(j)%tot(ipl)%m, sol_sumno3(j), sol_sumsolp(j), tilldb(idtill)%effmix
              end if
              pcom(j)%dtbl(idtbl)%num_actions(iac) = pcom(j)%dtbl(idtbl)%num_actions(iac) + 1
              pcom(j)%dtbl(idtbl)%days_act(iac) = 1     !reset days since this action
              if (iac > 1) pcom(j)%dtbl(idtbl)%days_act(iac-1) =  0     !reset previous action day counter
            end if

          !plant
          case ("plant")
            j = d_tbl%act(iac)%ob_num
            if (j == 0) j = ob_cur
            icom = pcom(j)%pcomdb
            pcom(j)%days_plant = 1       !reset days since last planting
            !! check for generic plant-harv and set crops
            isched = hru(j)%mgt_ops
            if (sched(isched)%auto_name(idtbl) == "pl_hv_summer1" .or.      &
                sched(isched)%auto_name(idtbl) == "pl_hv_winter1") then
              d_tbl%act(iac)%option = sched(isched)%auto_crop(1)
            end if
            if (sched(isched)%auto_name(idtbl) == "pl_hv_summer2") then
              d_tbl%act(iac)%option = sched(isched)%auto_crop(pcom(j)%rot_yr)
            end if
            
            if (pcom(j)%dtbl(idtbl)%num_actions(iac) <= Int(d_tbl%act(iac)%const2)) then
              do ipl = 1, pcom(j)%npl
                idp = pcomdb(icom)%pl(ipl)%db_num
                if (d_tbl%act(iac)%option == pcomdb(icom)%pl(ipl)%cpnm) then
                  pcom(j)%plcur(ipl)%gro = "y"
                  pcom(j)%plcur(ipl)%idorm = "n"
                  if (d_tbl%act_app(iac) > 0) then
                    call mgt_transplant (d_tbl%act_app(iac))
                  end if
                if (pco%mgtout == "y") then
                  write (2612, *) j, time%yrc, time%mo, time%day_mo, pldb(idp)%plantnm, "    PLANT",   &
                      phubase(j), pcom(j)%plcur(ipl)%phuacc, soil(ihru)%sw,                     &
                      pl_mass(j)%tot(ipl)%m, rsd1(j)%tot_com%m, sol_sumno3(j),                  &
                      sol_sumsolp(j), pcom(j)%plg(ipl)%lai, pcom(j)%plcur(ipl)%lai_pot
                  end if
                end if
              end do
              pcom(j)%dtbl(idtbl)%num_actions(iac) = pcom(j)%dtbl(idtbl)%num_actions(iac) + 1
              pcom(j)%dtbl(idtbl)%days_act(iac) = 1     !reset days since last action
              if (iac > 1) pcom(j)%dtbl(idtbl)%days_act(iac-1) =  0     !reset previous action day counter
            end if
            
          !harvest only
          case ("harvest")
            j = d_tbl%act(iac)%ob_num
            if (j == 0) j = ob_cur
            
            if (pcom(j)%dtbl(idtbl)%num_actions(iac) <= Int(d_tbl%act(iac)%const2)) then
              iharvop = d_tbl%act_typ(iac)
              icom = pcom(j)%pcomdb
              pcom(j)%days_harv = 1       !reset days since last harvest
              !! check for generic plant-harv and set crops
              isched = hru(j)%mgt_ops
              if (sched(isched)%auto_name(idtbl) == "pl_hv_summer1" .or.      &
                  sched(isched)%auto_name(idtbl) == "pl_hv_winter1") then
                d_tbl%act(iac)%option = sched(isched)%auto_crop(1)
              end if
              if (sched(isched)%auto_name(idtbl) == "pl_hv_summer2") then
                d_tbl%act(iac)%option = sched(isched)%auto_crop(pcom(j)%rot_yr)
              end if
            
              do ipl = 1, pcom(j)%npl
                biomass = pl_mass(j)%tot(ipl)%m
                if (d_tbl%act(iac)%option == pcomdb(icom)%pl(ipl)%cpnm .or. d_tbl%act(iac)%option == "all") then
                          
                  !harvest specific type
                  select case (harvop_db(iharvop)%typ)
                  case ("biomass")    
                    call mgt_harvbiomass (j, ipl, iharvop)
                  case ("grain")
                    call mgt_harvgrain (j, ipl, iharvop)
                  case ("residue")
                  case ("tree")
                  case ("tuber")
                    call mgt_harvtuber (j, ipl, iharvop)
                  end select

                  !! sum yield and num. of harvest to calc ave yields
                  pl_mass(j)%yield_tot(ipl) = pl_mass(j)%yield_tot(ipl) + pl_yield
                  pcom(j)%plcur(ipl)%harv_num = pcom(j)%plcur(ipl)%harv_num + 1
                            
                  !! sum basin crop yields and area harvested
                  iplt_bsn = pcom(j)%plcur(ipl)%bsn_num
                  bsn_crop_yld(iplt_bsn)%area_ha = bsn_crop_yld(iplt_bsn)%area_ha + hru(j)%area_ha
                  bsn_crop_yld(iplt_bsn)%yield = bsn_crop_yld(iplt_bsn)%yield + yield * hru(j)%area_ha / 1000.
                  !! sum regional crop yields for soft calibration
                  ireg = hru(j)%crop_reg
                  do ilum = 1, plcal(ireg)%lum_num
                    if (plcal(ireg)%lum(ilum)%meas%name == mgt%op_char) then
                      plcal(ireg)%lum(ilum)%ha = plcal(ireg)%lum(ilum)%ha + hru(j)%area_ha
                      plcal(ireg)%lum(ilum)%sim%yield = plcal(ireg)%lum(ilum)%sim%yield + pl_yield%m * hru(j)%area_ha / 1000.
                    end if
                  end do
            
                  idp = pcom(j)%plcur(ipl)%idplt
                  if (pco%mgtout == "y") then
                    write (2612, *) j, time%yrc, time%mo, time%day_mo,  pldb(idp)%plantnm, "    HARVEST",      &
                        phubase(j), pcom(j)%plcur(ipl)%phuacc, soil(j)%sw, biomass, rsd1(j)%tot(ipl)%m, &
                        sol_sumno3(j), sol_sumsolp(j), pl_yield%m, pcom(j)%plstr(ipl)%sum_n, &
                        pcom(j)%plstr(ipl)%sum_p, pcom(j)%plstr(ipl)%sum_tmp, pcom(j)%plstr(ipl)%sum_w, &
                        pcom(j)%plstr(ipl)%sum_a
                  end if 
                end if
                pcom(j)%plcur(ipl)%phuacc = 0.
              end do
              pcom(j)%dtbl(idtbl)%num_actions(iac) = pcom(j)%dtbl(idtbl)%num_actions(iac) + 1
              pcom(j)%dtbl(idtbl)%days_act(iac) = 1     !reset days since last action
              if (iac > 1) pcom(j)%dtbl(idtbl)%days_act(iac-1) =  0     !reset previous action day counter
            end if

          !kill plant
          case ("kill")
            j = d_tbl%act(iac)%ob_num
            if (j == 0) j = ob_cur
            
            if (pcom(j)%dtbl(idtbl)%num_actions(iac) <= Int(d_tbl%act(iac)%const2)) then
              icom = pcom(j)%pcomdb
              do ipl = 1, pcom(j)%npl
                biomass = pl_mass(j)%tot(ipl)%m
                if (d_tbl%act(iac)%option == pcomdb(icom)%pl(ipl)%cpnm .or. d_tbl%act(iac)%option == "all") then

                  call mgt_killop (j, ipl)

                  idp = pcom(j)%plcur(ipl)%idplt
                  if (pco%mgtout == "y") then
                    write (2612, *) j, time%yrc, time%mo, time%day_mo,  pldb(idp)%plantnm, "    HARV/KILL",     &
                        phubase(j), pcom(j)%plcur(ipl)%phuacc, soil(j)%sw, biomass, rsd1(j)%tot(ipl)%m,  &
                        sol_sumno3(j), sol_sumsolp(j), yield, pcom(j)%plstr(ipl)%sum_n,                  &
                        pcom(j)%plstr(ipl)%sum_p, pcom(j)%plstr(ipl)%sum_tmp, pcom(j)%plstr(ipl)%sum_w,  &
                        pcom(j)%plstr(ipl)%sum_a
                  end if 
                end if
                pcom(j)%plcur(ipl)%phuacc = 0.
                phubase(j) = 0.
              end do
              pcom(j)%dtbl(idtbl)%num_actions(iac) = pcom(j)%dtbl(idtbl)%num_actions(iac) + 1
              pcom(j)%dtbl(idtbl)%days_act(iac) = 1     !reset days since last action
              if (iac > 1) pcom(j)%dtbl(idtbl)%days_act(iac-1) =  0     !reset previous action day counter
            end if
  
          !harvest and kill
          case ("harvest_kill")
            j = d_tbl%act(iac)%ob_num
            if (j == 0) j = ob_cur
            
            if (pcom(j)%dtbl(idtbl)%num_actions(iac) <= Int(d_tbl%act(iac)%const2)) then
              iharvop = d_tbl%act_typ(iac)
              icom = pcom(j)%pcomdb
              pcom(j)%days_harv = 1       !reset days since last harvest
              !! check for generic plant-harv and set crops
              isched = hru(j)%mgt_ops
              if (sched(isched)%auto_name(idtbl) == "pl_hv_summer1" .or.      &
                  sched(isched)%auto_name(idtbl) == "pl_hv_winter1") then
                d_tbl%act(iac)%option = sched(isched)%auto_crop(1)
              end if
              if (sched(isched)%auto_name(idtbl) == "pl_hv_summer2") then
                d_tbl%act(iac)%option = sched(isched)%auto_crop(pcom(j)%rot_yr)
              end if
              
              do ipl = 1, pcom(j)%npl
                biomass = pl_mass(j)%tot(ipl)%m
                if (d_tbl%act(iac)%option == pcomdb(icom)%pl(ipl)%cpnm .or. d_tbl%act(iac)%option == "all") then
                          
                  !harvest specific type
                  select case (harvop_db(iharvop)%typ)
                  case ("biomass")    
                    call mgt_harvbiomass (j, ipl, iharvop)
                  case ("grain")
                    call mgt_harvgrain (j, ipl, iharvop)
                  case ("residue")
                  case ("tree")
                  case ("tuber")
                    call mgt_harvtuber (j, ipl, iharvop)
                  end select
            
                  call mgt_killop (j, ipl)

                  !! sum yield and number of harvests to calc ave yields
                  pl_mass(j)%yield_tot(ipl) = pl_mass(j)%yield_tot(ipl) + pl_yield
                  pcom(j)%plcur(ipl)%harv_num = pcom(j)%plcur(ipl)%harv_num + 1
                            
                  !! sum basin crop yields and area harvested
                  iplt_bsn = pcom(j)%plcur(ipl)%bsn_num
                  bsn_crop_yld(iplt_bsn)%area_ha = bsn_crop_yld(iplt_bsn)%area_ha + hru(j)%area_ha
                  bsn_crop_yld(iplt_bsn)%yield = bsn_crop_yld(iplt_bsn)%yield + pl_yield%m * hru(j)%area_ha / 1000.
                  !! sum regional crop yields for soft calibration
                  ireg = hru(j)%crop_reg
                  do ilum = 1, plcal(ireg)%lum_num
                    if (plcal(ireg)%lum(ilum)%meas%name == d_tbl%act(iac)%option) then
                      plcal(ireg)%lum(ilum)%ha = plcal(ireg)%lum(ilum)%ha + hru(j)%area_ha
                      plcal(ireg)%lum(ilum)%sim%yield = plcal(ireg)%lum(ilum)%sim%yield + pl_yield%m * hru(j)%area_ha / 1000.
                    end if
                  end do
            
                  idp = pcom(j)%plcur(ipl)%idplt
                  if (pco%mgtout == "y") then
                    write (2612, *) j, time%yrc, time%mo, time%day_mo,  pldb(idp)%plantnm, "    HARV/KILL",        &
                        phubase(j), pcom(j)%plcur(ipl)%phuacc, soil(j)%sw, biomass, rsd1(j)%tot(ipl)%m,     &
                        sol_sumno3(j), sol_sumsolp(j), pl_yield%m, pcom(j)%plstr(ipl)%sum_n,   &
                        pcom(j)%plstr(ipl)%sum_p, pcom(j)%plstr(ipl)%sum_tmp, pcom(j)%plstr(ipl)%sum_w,     &
                        pcom(j)%plstr(ipl)%sum_a
                  end if 
                end if
                pcom(j)%plcur(ipl)%phuacc = 0.
                phubase(j) = 0.
              end do
              pcom(j)%dtbl(idtbl)%num_actions(iac) = pcom(j)%dtbl(idtbl)%num_actions(iac) + 1
              pcom(j)%dtbl(idtbl)%days_act(iac) = 1     !reset days since last action
              if (iac > 1) pcom(j)%dtbl(idtbl)%days_act(iac-1) =  0     !reset previous action day counter
            end if
  
          !reset rotation year
          case ("rot_reset")
            j = d_tbl%act(iac)%ob_num
            if (j == 0) j = ob_cur
            if (d_tbl%act(iac)%const < 1) d_tbl%act(iac)%const = 1
            pcom(j)%rot_yr = d_tbl%act(iac)%const
              
          !apply pesticide
          case ("pest_apply")
            j = d_tbl%act(iac)%ob_num
            if (j == 0) j = ob_cur
            
            if (pcom(j)%dtbl(idtbl)%num_actions(iac) <= Int(d_tbl%act(iac)%const2)) then
              ipl = 1
              sumpst = sumpst + 1
              ipst = d_tbl%act_typ(iac)                                     !pesticide type from fert data base
              ipestop = d_tbl%act_app(iac)                                  !surface application fraction from chem app data base
              pest_kg = d_tbl%act(iac)%const * chemapp_db(ipestop)%app_eff  !amount applied in kg/ha
              call pest_apply (j, ipst, pest_kg, ipestop)

              if (pco%mgtout == "y") then
                write (2612, *) j, time%yrc, time%mo, time%day_mo, d_tbl%act(iac)%option, "    PEST ",        &
                 phubase(j), pcom(j)%plcur(ipl)%phuacc, soil(j)%sw,pl_mass(j)%tot(ipl)%m,           &
                 rsd1(j)%tot(ipl)%m, sol_sumno3(j), sol_sumsolp(j), pest_kg
              endif
              pcom(j)%dtbl(idtbl)%num_actions(iac) = pcom(j)%dtbl(idtbl)%num_actions(iac) + 1
              !dtbl_lum(idtbl)%hru_lu_cur = dtbl_lum(idtbl)%hru_lu_cur + 1
              !dtbl_lum(idtbl)%hru_ha_cur = dtbl_lum(idtbl)%hru_ha_cur + hru(j)%area_ha
            end if
          
          case ("graze")    !! grazing operation
            j = d_tbl%act(iac)%ob_num
            if (j == 0) j = ob_cur

            igr = d_tbl%act_typ(iac)
            graze = grazeop_db(igr)
            call pl_graze
            
              !if (pco%mgtout == "y") then
              !  write (2612, *) j, time%yrc, time%mo, time%day_mo, "         ", "    GRAZE",         &
              !    phubase(j), pcom(j)%plcur(ipl)%phuacc, soil(j)%sw,pl_mass(j)%tot(ipl)%m,        &
              !    rsd1(j)%tot(ipl)%m, sol_sumno3(j), sol_sumsolp(j), grazeop_db(igr)%eat, grazeop_db(igr)%manure
              !end if
              pcom(j)%dtbl(idtbl)%num_actions(iac) = pcom(j)%dtbl(idtbl)%num_actions(iac) + 1

          !initiate growing season for hru_lte
          case ("grow_init")
            j = d_tbl%act(iac)%ob_num
            if (j == 0) j = ob_cur
            
            hlt(j)%gro = "y"
            hlt(j)%g = 0.
            hlt(j)%alai = 0.
            hlt(j)%dm = 0.
            hlt(j)%hufh = 0.

          !end growing season for hru_lte
          case ("grow_end")
            !calculate yield - print lai, biomass and yield
            j = d_tbl%act(iac)%ob_num
            if (j == 0) j = ob_cur
            
              idp = hlt(j)%iplant
              if (hlt(j)%pet < 10.) then
                wur = 100.
              else
                wur = 100. * hlt(j)%aet / hlt(j)%pet
              endif
              hiad1 = (pldb(idp)%hvsti - pldb(idp)%wsyf) *                            &   
                        (wur / (wur + Exp(6.13 - .0883 * wur))) + pldb(idp)%wsyf
              hiad1 = amin1 (hiad1, pldb(idp)%hvsti)
              yield = 0.8 * hlt(j)%dm * hiad1  ! * hlt(isd)%stress
              hlt(j)%yield = yield / 1000.
              hlt(j)%npp = hlt(j)%dm / 1000.
              hlt(j)%lai_mx = hlt(j)%alai
              !compute annual net primary productivity (npp) for perennial non-harvested?
              !use output.mgt print code
              !write() isd, time%day, time%yrc, pldb(iplt)%plantnm, hlt(isd)%alai, hlt(isd)%dm, yield
              hlt(j)%gro = "n"
              hlt(j)%g = 0.
              hlt(j)%alai = 0.
              hlt(j)%dm = 0.     !adjust for non-harvested perennials?
              hlt(j)%hufh = 0.
              hlt(j)%aet = 0.
              hlt(j)%pet = 0.

          !drainage water management
          case ("drain_control") !! set drain depth for drainage water management
            j = d_tbl%act(iac)%ob_num
            if (j == 0) j = ob_cur
            
            if (pcom(j)%dtbl(idtbl)%num_actions(iac) <= Int(d_tbl%act(iac)%const2)) then
             istr = hru(j)%tiledrain
              hru(j)%lumv%sdr_dep = d_tbl%act(iac)%const
              !if (hru(j)%lumv%sdr_dep > 0) then
              !  do jj = 1, soil(j)%nly
              !    if (hru(j)%lumv%sdr_dep < soil(j)%phys(jj)%d) hru(j)%lumv%ldrain = jj
              !    if (hru(j)%lumv%sdr_dep < soil(j)%phys(jj)%d) exit
              !  end do
              !else
              !  hru(j)%lumv%ldrain = 0
              !end if
              pcom(j)%dtbl(idtbl)%num_actions(iac) = pcom(j)%dtbl(idtbl)%num_actions(iac) + 1
            end if
                           
          !flow control for water allocation - needs to be modified***
          case ("flow_control") !! set flow fractions to buffer tile and direct to channel
            ! ob_num is the object number of the current channel
            select case (d_tbl%act(iac)%option)
            case ("min_cms")    
              if (ob(ob_num)%hd(1)%flo / 86400. < d_tbl%act(iac)%const + .0001) then
                frac = 1.
              else
                frac = d_tbl%act(iac)%const / (ob(ob_num)%hd(1)%flo / 86400.)
              end if
              
            case ("all_flo")
              frac = 1.

            case ("min_frac")
              frac = d_tbl%act(iac)%const
              
            case ("demand")
                
            end select
            
            ! set inflow hydrograph fraction of recieving objects - used for dtbl flow fractions
            ! set first object hyd fractin as defined in decision table
            inhyd = dtbl_flo(idtbl)%act(iac)%ob_num
            ihyd_in = ob(ob_num)%rcvob_inhyd(inhyd)
            iob_out = ob(ob_num)%obj_out(inhyd)
            ob(iob_out)%frac_in(ihyd_in) = frac
              
            ! set second hydrograph fraction
            if (inhyd < ob(ob_num)%src_tot .and. dtbl_flo(idtbl)%act(iac)%typ /= "irrigate_direct") then
              inhyd = inhyd + 1
              ihyd_in = ob(ob_num)%rcvob_inhyd(inhyd)
              iob_out = ob(ob_num)%obj_out(inhyd)
              ob(iob_out)%frac_in(ihyd_in) = 1. - frac
            end if
                                       
          !tile flow control for saturated buffers
          case ("tile_control") !! set flow fractions to buffer tile and direct to channel
            icon = d_tbl%act(iac)%ob_num
            if (j == 0) j = ob_cur
            select case (d_tbl%act(iac)%option)
            case ("min_flo")    
              if (hwb_d(j)%qtile < d_tbl%act(iac)%const) then
                frac = 1.
              else
                frac = d_tbl%act(iac)%const / hwb_d(j)%qtile
              end if
              ! set inflow hydrograph fraction of recieving objects - used for dtbl flow fractions
              ! set first object hyd fractin as defined in decision table
              inhyd = dtbl_flo(idtbl)%act(iac)%ob_num
              ihyd_in = ob(ob_num)%rcvob_inhyd(inhyd)
              iob_out = ob(ob_num)%obj_out(inhyd)
              ob(iob_out)%frac_in(ihyd_in) = frac
              
              ! set second hydrograph fraction
              if (inhyd < ob(ob_num)%src_tot .and. dtbl_flo(idtbl)%act(iac)%typ /= "irrigate_direct") then
                inhyd = inhyd + 1
                ihyd_in = ob(ob_num)%rcvob_inhyd(inhyd)
                iob_out = ob(ob_num)%obj_out(inhyd)
                ob(iob_out)%frac_in(ihyd_in) = 1. - frac
              end if

            case ("linear")

            case ("power")
                
            end select
                        
          !water rights decision to move water
          case ("water_rights")
                        
          !hru area fraction change - update lsu_unit.ele and rout_unit.ele
          case ("hru_fr_update")
            !"option" is the updated lsu_unit.ele and "file_pointer" is rout_unit.ele
            call hru_fr_change (d_tbl%act(iac)%option, d_tbl%act(iac)%file_pointer)
            !! write to new landuse change file
            write (3612,*) j, time%yrc, time%mo, time%day_mo,  "   HRU_FRACTION_CHANGE ",        &
                    d_tbl%act(iac)%option, d_tbl%act(iac)%file_pointer, "   0   0"
                            
          !land use change - total land use and management change
          case ("lu_change")
            j = d_tbl%act(iac)%ob_num
            if (j == 0) j = ob_cur
            ilu = d_tbl%act_typ(iac)
            hru(j)%dbs%land_use_mgt = ilu
            lu_prev = hru(j)%land_use_mgt_c
            hru(j)%land_use_mgt_c = d_tbl%act(iac)%file_pointer
            isol = hru(j)%dbs%soil
            call hru_lum_init (j)
            call plant_init (1,j)     ! (1) is to deallocate and reset
            call cn2_init (j)
            !! reset composite usle value - in hydro_init
            rock = Exp(-.053 * soil(j)%phys(1)%rock)
            hru(j)%lumv%usle_mult = rock * soil(j)%ly(1)%usle_k *       &
                                 hru(j)%lumv%usle_p * hru(j)%lumv%usle_ls * 11.8
            !! write to new landuse change file
            write (3612,*) j, time%yrc, time%mo, time%day_mo,  "    LU_CHANGE ",        &
                    lu_prev, hru(j)%land_use_mgt_c, "   0   0"
                            
          !land use change - contouring
          case ("p_factor")
            j = d_tbl%act(iac)%ob_num
            if (j == 0) j = ob_cur
            
            p_factor = hru(j)%lumv%usle_p
            !! set new p factor
            hru(j)%lumv%usle_p = d_tbl%act(iac)%const
            !! reset composite usle value - in hydro_init
            rock = Exp(-.053 * soil(j)%phys(1)%rock)
            hru(j)%lumv%usle_mult = rock * soil(j)%ly(1)%usle_k *       &
                                 hru(j)%lumv%usle_p * hru(j)%lumv%usle_ls * 11.8
            !! write to new landuse change file
            write (3612,*) j, time%yrc, time%mo, time%day_mo,  "     P_FACTOR",        &
                    "  null           null",  p_factor, hru(j)%lumv%usle_p
                                
          !land use change - contouring
          case ("contour")
            j = d_tbl%act(iac)%ob_num
            if (j == 0) j = ob_cur
            
            p_factor = hru(j)%lumv%usle_p
            !! set new p factor as function of slope - from cons_practice.lum relationship
            hru(j)%lumv%usle_p = 2.7 * hru(j)%topo%slope + .26
            !! reset composite usle value - in hydro_init
            rock = Exp(-.053 * soil(j)%phys(1)%rock)
            hru(j)%lumv%usle_mult = rock * soil(j)%ly(1)%usle_k *       &
                                 hru(j)%lumv%usle_p * hru(j)%lumv%usle_ls * 11.8
            write (3612,*) j, time%yrc, time%mo, time%day_mo,  "      CONTOUR ",        &
                    "  null           null",  p_factor, hru(j)%lumv%usle_p
                                  
          !land use change - strip cropping
          case ("stripcrop")
            j = d_tbl%act(iac)%ob_num
            if (j == 0) j = ob_cur
            
            p_factor = hru(j)%lumv%usle_p
            !! set new p factor as function of slope - from cons_practice.lum relationship
            hru(j)%lumv%usle_p = 2.32 * hru(j)%topo%slope + .36
            !! adjust for pasture - above equation is for row crops
            if (d_tbl%act(iac)%file_pointer == "pasture") hru(j)%lumv%usle_p = .5 * hru(j)%lumv%usle_p
            !! reset composite usle value - in hydro_init
            rock = Exp(-.053 * soil(j)%phys(1)%rock)
            hru(j)%lumv%usle_mult = rock * soil(j)%ly(1)%usle_k *       &
                                 hru(j)%lumv%usle_p * hru(j)%lumv%usle_ls * 11.8
            write (3612,*) j, time%yrc, time%mo, time%day_mo,  "    STRIPCROP ",        &
                    "  null           null",  p_factor, hru(j)%lumv%usle_p  
                    
          !land use change
          case ("terrace")
            j = d_tbl%act(iac)%ob_num
            if (j == 0) j = ob_cur
              
            p_factor = hru(j)%lumv%usle_p
            !! set new p factor as function of slope - from cons_practice.lum relationship
            hru(j)%lumv%usle_p = 1.4 * hru(j)%topo%slope + .13
            !! adjust for sod outlet - above equation is for underflow outlet
            if (d_tbl%act(iac)%file_pointer == "sod_outlet") hru(j)%lumv%usle_p = 2. * hru(j)%lumv%usle_p
            !! reset composite usle value - in hydro_init
            rock = Exp(-.053 * soil(j)%phys(1)%rock)
            hru(j)%lumv%usle_mult = rock * soil(j)%ly(1)%usle_k *       &
                                 hru(j)%lumv%usle_p * hru(j)%lumv%usle_ls * 11.8
            write (3612,*) j, time%yrc, time%mo, time%day_mo,  "      TERRACE ",        &
                    "  null           null",  p_factor, hru(j)%lumv%usle_p  
                    
          !install tile drains
          case ("tile_install")
            j = d_tbl%act(iac)%ob_num
            if (j == 0) j = ob_cur
              
            do istr = 1, db_mx%sdr
              if (d_tbl%act(iac)%file_pointer == sdr(istr)%name) then
                istr1 = istr
                exit
              end if
            end do
            
            !! set parameters for structural land use/managment
            if (d_tbl%act(iac)%file_pointer /= "null") then
              call structure_set_parms("tiledrain       ", istr1, j)
            end if
            !! write to new landuse change file
            istr = hru(j)%tiledrain
            write (3612,*) j, time%yrc, time%mo, time%day_mo,  " TILE_INSTALL ",        &
              sdr(istr)%name, sdr(istr1)%name, "   0   0"
                      
          !install septic tanks
          case ("septic_install")
            j = d_tbl%act(iac)%ob_num
            if (j == 0) j = ob_cur
              
            do istr = 1, db_mx%septic
              if (d_tbl%act(iac)%file_pointer == sep(istr)%name) then
                istr1 = istr
                exit
              end if
            end do
                  
            !! set parameters for structural land use/managment
            if (d_tbl%act(iac)%file_pointer /= "null") then
              call structure_set_parms("septic          ", istr1, j)
            end if
            write (3612,*) j, time%yrc, time%mo, time%day_mo,  " SEPTIC_INSTALL ",       &
              sdr(istr)%name, sdr(istr1)%name, "   0   0"
                                    
          !install filter strips
          case ("fstrip_install")
            j = d_tbl%act(iac)%ob_num
            if (j == 0) j = ob_cur
              
            do istr = 1, db_mx%filtop_db
              if (d_tbl%act(iac)%file_pointer == filtstrip_db(istr)%name) then
                istr1 = istr
                exit
              end if
            end do
            
            !! set parameters for structural land use/managment
            if (d_tbl%act(iac)%file_pointer /= "null") then
              call structure_set_parms("fstrip         ", istr1, j)
            end if
            write (3612,*) j, time%yrc, time%mo, time%day_mo,  " FSTRIP_INSTALL ",       &
              sdr(istr)%name, sdr(istr1)%name, "   0   0"
                                              
          !install grass waterways
          case ("grassww_install")
            j = d_tbl%act(iac)%ob_num
            if (j == 0) j = ob_cur
              
            do istr = 1, db_mx%grassop_db
              if (d_tbl%act(iac)%file_pointer == grwaterway_db(istr)%name) then
                istr1 = istr
                exit
              end if
            end do
            
            !! set parameters for structural land use/managment
            if (d_tbl%act(iac)%file_pointer /= "null") then
              call structure_set_parms("grassww         ", istr1, j)
            end if
            write (3612,*) j, time%yrc, time%mo, time%day_mo,  " GRASSWW_INSTALL ",       &
              sdr(istr)%name, sdr(istr1)%name, "   0   0"
                                                         
          !user defined bmp reductions
          case ("user_def_bmp")
            j = d_tbl%act(iac)%ob_num
            if (j == 0) j = ob_cur
              
            do istr = 1, db_mx%bmpuserop_db
              if (d_tbl%act(iac)%file_pointer == bmpuser_db(istr)%name) then
                istr1 = istr
                exit
              end if
            end do
            
            !! set parameters for structural land use/managment
            if (d_tbl%act(iac)%file_pointer /= "null") then
              call structure_set_parms("user_def        ", istr1, j)
            end if
            write (3612,*) j, time%yrc, time%mo, time%day_mo,  " USER_DEF_BMP ",       &
              sdr(istr)%name, sdr(istr1)%name, "   0   0"
                                        
          !channel change
          case ("chan_change")
            ich = ob_cur
            !set new cover and name for calibration
            sd_ch(ich)%cov = d_tbl%act(iac)%const
            sd_ch(ich)%order = d_tbl%act(iac)%file_pointer
        
          ! burning
          case ("burn")
            j = d_tbl%act(iac)%ob_num
            if (j == 0) j = ob_cur
            
            if (pcom(j)%dtbl(idtbl)%num_actions(iac) <= Int(d_tbl%act(iac)%const2)) then
              iburn = d_tbl%act_typ(iac)           !burn type from fire data base
              do ipl = 1, pcom(j)%npl
                call pl_burnop (j, ipl, iburn)
              end do
                        
              if (pco%mgtout == "y") then
                write (2612, *) j, time%yrc, time%mo, time%day_mo, "        ", "    BURN", phubase(j),    &
                    pcom(j)%plcur(ipl)%phuacc, soil(j)%sw,pl_mass(j)%tot(ipl)%m, rsd1(j)%tot(ipl)%m,   &
                    sol_sumno3(j), sol_sumsolp(j)
              end if
              pcom(j)%dtbl(idtbl)%num_actions(iac) = pcom(j)%dtbl(idtbl)%num_actions(iac) + 1
            end if
          
          !update curve number
          case ("cn_update")
            j = d_tbl%act(iac)%ob_num
            if (j == 0) j = ob_cur
            
            cn_prev = cn2(j)
            if (pcom(j)%dtbl(idtbl)%num_actions(iac) <= Int(d_tbl%act(iac)%const2)) then
              cn2(j) = chg_par (cn2(j), j, d_tbl%act(iac)%option, d_tbl%act(iac)%const, 35., 95., 0)
              call curno (cn2(j), j)
            end if
            
            if (pco%mgtout == "y") then
              ipl = 1
              write (2612, *) j, time%yrc, time%mo, time%day_mo, "        ", "    CNUP", phubase(j),    &
                pcom(j)%plcur(ipl)%phuacc, soil(j)%sw, pl_mass(j)%tot(ipl)%m, rsd1(j)%tot(ipl)%m,       &
                sol_sumno3(j), sol_sumsolp(j), cn_prev, cn2(j)
            end if
            
            pcom(j)%dtbl(idtbl)%num_actions(iac) = pcom(j)%dtbl(idtbl)%num_actions(iac) + 1
                          
          case ("pheno_reset")  !! begin and end monsoon initiation period
            j = d_tbl%act(iac)%ob_num
            if (j == 0) j = ob_cur
            
            if (pcom(j)%dtbl(idtbl)%num_actions(iac) <= Int(d_tbl%act(iac)%const2)) then
              do ipl = 1, pcom(j)%npl
                idp = pcom(j)%plcur(ipl)%idplt
                if (pldb(idp)%trig == "moisture_gro") then
                  pcom(j)%plcur(ipl)%phuacc = 0.
                  pcom(j)%plcur(ipl)%gro = "y" 
                  pcom(j)%plcur(ipl)%idorm = "n"
                endif
              end do
            end if
            
            pcom(j)%dtbl(idtbl)%num_actions(iac) = pcom(j)%dtbl(idtbl)%num_actions(iac) + 1
            pcom(j)%dtbl(idtbl)%days_act(iac) = 1     !reset days since last action
            if (iac > 1) pcom(j)%dtbl(idtbl)%days_act(iac-1) =  0     !reset previous action day counter

          !herd management - move the herd
          case ("herd")

          end select
        end if
      end do

      return
      end subroutine actions