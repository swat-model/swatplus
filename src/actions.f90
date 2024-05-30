      subroutine actions (ob_cur, ob_num, idtbl)
      use conditional_module
      use climate_module
      use time_module
      use aquifer_module
      use hru_module, only : hru, cn2, fertno3, fertnh3, fertorgn, fertorgp, fertsolp,   &
        ihru, ipl, isol,  phubase, sol_sumno3, sol_sumsolp
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
      use water_body_module
      use reservoir_data_module
      use manure_allocation_module
      use water_allocation_module

      implicit none

      integer, intent (in)  :: ob_cur      !none     |sequential number of individual objects
      integer, intent (in)  :: ob_num      !none     |sequential number for all objects
      integer, intent (in)  :: idtbl       !none     |
      integer :: icom                      !none     |
      integer :: iac                       !none     |counter
      integer :: ial                       !none     |counter
      !integer :: jj                        !none     |counter
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
      integer :: isrc
      integer :: isched
      integer :: ipud, ipdl
      integer :: ires,idb
      integer :: imallo, idmd,irec
      real :: hiad1                        !         |
      real :: biomass                      !         |
      real :: frt_kg
      real :: harveff
      real :: wur                          !         |
      real :: frac                         !         |
      real :: rto                          !         |
      real :: rto1                         !         |
      real :: pest_kg                      !kg/ha    |amount of pesticide applied 
      real :: chg_par                      !variable |new parameter value
      real :: yield 
      real :: sumpst = 0.
      real :: rock
      real :: p_factor
      real :: cn_prev
      real :: stor_m3
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
          
          !manure demand - for manure allocation
          case ("manure_demand")
            j = ob_cur
            idmd = ob_num
            imallo = 1      !if mallo objects > 1, need to input
            
            mallo(imallo)%dmd(idmd)%manure_amt = manure_amtz
            if (pcom(j)%dtbl(idtbl)%num_actions(iac) <= Int(d_tbl%act(iac)%const2)) then
              mallo(imallo)%dmd(idmd)%manure_amt%mallo_obj = 1                      !assume 1 manure allocation object - could use file_pointer to xwalk
              mallo(imallo)%dmd(idmd)%manure_amt%src_obj = d_tbl%act(iac)%ob_num    !amount applied - t/ha
              mallo(imallo)%dmd(idmd)%manure_amt%app_t_ha = d_tbl%act(iac)%const    !manure source object number
              mallo(imallo)%dmd(idmd)%manure_amt%app_method = d_tbl%act_app(iac)    !manure application method
            end if

          !irrigation demand - hru action
          case ("irr_demand")
            ipl = 1
            j = d_tbl%act(iac)%ob_num
            if (j == 0) j = ob_cur
            
            if (d_tbl%act(iac)%name=='ponding') then !irrigation demand calculated by paddy/wetland ponding depth requirements Jaehak 2023
              hru(j)%irr_hmax = d_tbl%act(iac)%const !mm target ponding depth
              hru(j)%irr_hmin = d_tbl%act(iac)%const2 !mm threshold ponding depth for irrigation
              
              wet_ob(j)%depth = wet_ob(j)%depth + irrig(j)%applied / 1000. !mm irrigation by wro already happend for today Jaehak 2023

              if (wet_ob(j)%depth*1000.<hru(j)%irr_hmin) then
                irrig(j)%demand = max(0.,d_tbl%act(iac)%const-wet_ob(j)%depth*1000.) * hru(j)%area_ha * 10.       ! m3 = mm * ha * 10.
              else
                irrig(j)%demand = 0.
              endif
            else
              if (pcom(j)%dtbl(idtbl)%num_actions(iac) <= Int(d_tbl%act(iac)%const2)) then
                irrop = d_tbl%act_typ(iac)      ! irrigation application type in irr.ops
                irrig(j)%demand = d_tbl%act(iac)%const * hru(j)%area_ha * 10.       ! m3 = mm * ha * 10.
            
            !! if unlimited source, set irrigation applied directly to hru
            if (d_tbl%act(iac)%file_pointer == "unlim") then
              irrig(j)%applied = irrop_db(irrop)%amt_mm * irrop_db(irrop)%eff * (1. - irrop_db(irrop)%surq)
              irrig(j)%runoff = irrop_db(irrop)%amt_mm * irrop_db(irrop)%eff * irrop_db(irrop)%surq
            end if  
              
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
            endif

          !irrigate - hru action
          case ("irrigate")
            ipl = 1
            j = ob_cur                      ! hru number 
            
            !! check number of applications per year
            if (pcom(j)%dtbl(idtbl)%num_actions(iac) <= Int(d_tbl%act(iac)%const2)) then
            isrc = d_tbl%act(iac)%ob_num    ! source object type number
            irrop = d_tbl%act_typ(iac)      ! irrigation application type in irr.ops

            if (d_tbl%act(iac)%name=='ponding') then !paddy irrigation
              hru(j)%irr_hmax = d_tbl%act(iac)%const !mm
              hru(j)%irr_hmin = d_tbl%act(iac)%const2 !mm
              irrig(j)%applied = max(0.,d_tbl%act(iac)%const-wet_ob(j)%depth*1000.) * irrop_db(irrop)%eff * &
                        (1. - irrop_db(irrop)%surq) !mm
              irrig(j)%runoff = max(0.,d_tbl%act(iac)%const-wet_ob(j)%depth*1000.) * irrop_db(irrop)%surq   !mm
              irrig(j)%demand = max(0.,d_tbl%act(iac)%const-wet_ob(j)%depth*1000.) * hru(j)%area_ha * 10.       ! m3 = mm * ha * 10.
            else
              irrig(j)%applied = d_tbl%act(iac)%const * irrop_db(irrop)%eff * (1. - irrop_db(irrop)%surq)
              irrig(j)%runoff = d_tbl%act(iac)%const * irrop_db(irrop)%surq
              irrig(j)%demand = d_tbl%act(iac)%const * hru(j)%area_ha * 10.       ! m3 = mm * ha * 10.
            end if

            !select object type
            iob = d_tbl%act(iac)%ob_num
            select case (d_tbl%act(iac)%ob)
            case ("aqu")
              stor_m3 = aqu_d(iob)%stor * aqu_prm(iob)%area_ha * 10.
              if (stor_m3 > irrig(j)%demand) then
                rto = irrig(j)%demand / stor_m3                 ! ratio of water removed from aquifer volume (m3)
              else
                rto = 0.
                irrig(j)%applied = 0.
                irrig(j)%runoff = 0.
                irrig(j)%demand = 0.
              end if
              rto = amax1 (0., rto)
              rto = amin1 (1., rto)
              rto1 = (1. - rto)
              irrig(j)%water%flo = rto * aqu_d(iob)%stor                ! organics in irrigation water
              aqu_d(iob)%stor = rto1 * aqu_d(iob)%stor                  ! remainder stays in aquifer
              cs_irr(iob) = rto * cs_aqu(iob)                           ! constituents in irrigation water
              cs_aqu(iob) = rto1 * cs_aqu(iob)                          ! remainder stays in aquifer
              
            case ("cha", "sdc")
              if (ch_stor(iob)%flo > irrig(j)%demand) then
                rto = irrig(j)%demand / ch_stor(iob)%flo                ! ratio of water removed from channel volume
              else
                rto = 0.
                irrig(j)%applied = 0.
                irrig(j)%runoff = 0.
                irrig(j)%demand = 0.
              end if
              rto = amax1 (0., rto)
              rto = amin1 (1., rto)
              rto1 = (1. - rto)
              irrig(j)%water = rto * ch_stor(iob)                       ! organics in irrigation water
              ch_stor(iob) = rto1 * ch_stor(iob)                        ! remainder stays in channel
              cs_irr(iob) = rto * ch_water(iob)                         ! constituents in irrigation water
              ch_water(iob) = rto1 * ch_water(iob)                      ! remainder stays in channel
              
            case ("res")
              if (res(iob)%flo > irrig(j)%demand) then
                rto = irrig(j)%demand / res(iob)%flo                    ! ratio of water removed from res volume
              else
                rto = 0.
                irrig(j)%applied = 0.
                irrig(j)%runoff = 0.
                irrig(j)%demand = 0.
              end if
              rto = amax1 (0., rto)
              rto = amin1 (1., rto)
              rto1 = (1. - rto)
              irrig(j)%water = rto * res(iob)                           ! organics in irrigation water
              res(iob) = rto1 * res(iob)                                ! remainder stays in reservoir
              !cs_irr(iob) = rto * res_water(iob)                        ! constituents in irrigation water
              !res_water(iob) = rto1 * res_water(iob)                    ! remainder stays in reservoir
              
            end select
                  
            if (pco%mgtout == "y") then
              write (2612, *) j, time%yrc, time%mo, time%day_mo, "        ", "IRRIGATE", phubase(j),  &
                  pcom(j)%plcur(ipl)%phuacc, soil(j)%sw,pl_mass(j)%tot(ipl)%m, rsd1(j)%tot(ipl)%m, &
                  sol_sumno3(j), sol_sumsolp(j), irrig(j)%demand
            end if
            
              pcom(j)%dtbl(idtbl)%num_actions(iac) = pcom(j)%dtbl(idtbl)%num_actions(iac) + 1
              pcom(j)%dtbl(idtbl)%days_act(iac) = 1                     !reset days since last action
              pcom(j)%days_irr = 1                                      ! reset days since last irrigation
              if (iac > 1) pcom(j)%dtbl(idtbl)%days_act(iac-1) =  0     !reset previous action day counter
            end if      ! const2 - number per year check

          !fertilize
          case ("fertilize")
            j = d_tbl%act(iac)%ob_num
            if (j == 0) j = ob_cur
            
            if (pcom(j)%dtbl(idtbl)%num_actions(iac) <= Int(d_tbl%act(iac)%const2)) then
              ipl = 1
              ifrt = d_tbl%act_typ(iac)               !fertilizer type from fert data base
              frt_kg = d_tbl%act(iac)%const           !amount applied in kg/ha
              ifertop = d_tbl%act_app(iac)            !surface application fraction from chem app data base

              if (wet(j)%flo > 0. .and. chemapp_db(ifertop)%surf_frac == 1) then
                call pl_fert_wet (ifrt, frt_kg)
                if (pco%mgtout == "y") then
                  write (2612,*) j, time%yrc, time%mo, time%day_mo, mgt%op_char, " FERT-WET", &
                    phubase(j), pcom(j)%plcur(ipl)%phuacc, soil(j)%sw, pl_mass(j)%tot(ipl)%m,           &
                    rsd1(j)%tot_com%m, sol_sumno3(j), sol_sumsolp(j), frt_kg, fertno3, fertnh3,         &
                    fertorgn, fertsolp, fertorgp
                endif
              else
                call pl_fert (ifrt, frt_kg, ifertop)
                if (pco%mgtout == "y") then
                  write (2612,*) j, time%yrc, time%mo, time%day_mo, mgt%op_char, "    FERT ", &
                    phubase(j), pcom(j)%plcur(ipl)%phuacc, soil(j)%sw, pl_mass(j)%tot(ipl)%m,           &
                    rsd1(j)%tot_com%m, sol_sumno3(j), sol_sumsolp(j), frt_kg, fertno3, fertnh3,         &
                    fertorgn, fertsolp, fertorgp
                endif
              endif
              pcom(j)%dtbl(idtbl)%num_actions(iac) = pcom(j)%dtbl(idtbl)%num_actions(iac) + 1
              pcom(j)%dtbl(idtbl)%days_act(iac) = 1     !reset days since last action
              if (iac > 1) pcom(j)%dtbl(idtbl)%days_act(iac-1) =  0     !reset previous action day counter
            end if

          !future fertilizer
          case ("fert_future")
            j = ob_cur
            ifrt = d_tbl%act(iac)%ob_num
            pcom(j)%fert_fut(ifrt)%day_fert = Int (d_tbl%act(iac)%const2)
              
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
                  !! check to see if the crop is already growing
                  if (pcom(j)%plcur(ipl)%gro == "n") then
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
                  else
                    !! don't plant if the crop is already growing
                    if (pco%mgtout ==  "y") then
                      write (2612, *) j, time%yrc, time%mo, time%day_mo, pldb(idp)%plantnm,         &
                        "    PLANT_ALREADY_GROWING", phubase(j), pcom(j)%plcur(ipl)%phuacc,       &
                        soil(j)%sw, pl_mass(j)%tot(ipl)%m, rsd1(j)%tot_com%m, sol_sumno3(j),      &
                        sol_sumsolp(j),pcom(j)%plg(ipl)%lai, pcom(j)%plcur(ipl)%lai_pot
                    end if
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
                  
                !check minimum biomass for harvest
                if (biomass > harvop_db(iharvop)%bm_min) then
                !harvest specific type
                select case (harvop_db(iharvop)%typ)
                case ("biomass")    
                  call mgt_harvbiomass (j, ipl, iharvop)
                case ("grain")
                  call mgt_harvgrain (j, ipl, iharvop)
                case ("residue")
                  harveff = d_tbl%act(iac)%const
                  call mgt_harvresidue (j, harveff)
                case ("tree")
                case ("tuber")
                  call mgt_harvtuber (j, ipl, iharvop)
                case ("peanuts")
                  call mgt_harvtuber (j, ipl, iharvop)
                case ("stripper")
                  call mgt_harvgrain (j, ipl, iharvop)
                case ("picker")
                  call mgt_harvgrain (j, ipl, iharvop)
                end select
                end if

                  !! sum yield and num. of harvest to calc ave yields
                  pl_mass(j)%yield_tot(ipl) = pl_mass(j)%yield_tot(ipl) + pl_yield
                  pl_mass(j)%yield_yr(ipl) = pl_mass(j)%yield_yr(ipl) + pl_yield
                  pcom(j)%plcur(ipl)%harv_num_yr = pcom(j)%plcur(ipl)%harv_num_yr + 1
                  pcom(j)%plcur(ipl)%harv_num_yr = pcom(j)%plcur(ipl)%harv_num_yr + 1
                            
                  !! sum basin crop yields and area harvested
                  iplt_bsn = pcom(j)%plcur(ipl)%bsn_num
                  bsn_crop_yld(iplt_bsn)%area_ha = bsn_crop_yld(iplt_bsn)%area_ha + hru(j)%area_ha
                  bsn_crop_yld(iplt_bsn)%yield = bsn_crop_yld(iplt_bsn)%yield + yield * hru(j)%area_ha / 1000.
                  
                  if (cal_codes%plt == "y") then
                    !! sum regional crop yields for soft calibration
                    ireg = hru(j)%crop_reg
                    do ilum = 1, plcal(ireg)%lum_num
                      if (plcal(ireg)%lum(ilum)%meas%name == mgt%op_char) then
                        plcal(ireg)%lum(ilum)%ha = plcal(ireg)%lum(ilum)%ha + hru(j)%area_ha
                        plcal(ireg)%lum(ilum)%sim%yield = plcal(ireg)%lum(ilum)%sim%yield + pl_yield%m * hru(j)%area_ha / 1000.
                      end if
                    end do
                  end if
            
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
                  pcom(j)%last_kill = pcomdb(icom)%pl(ipl)%cpnm
                  call mgt_killop (j, ipl)

                  idp = pcom(j)%plcur(ipl)%idplt
                  if (pco%mgtout == "y") then
                    write (2612, *) j, time%yrc, time%mo, time%day_mo,  pldb(idp)%plantnm, "         KILL",     &
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
                  
                !check minimum biomass for harvest
                if (biomass > harvop_db(iharvop)%bm_min) then
                !harvest specific type
                select case (harvop_db(iharvop)%typ)
                case ("biomass")    
                  call mgt_harvbiomass (j, ipl, iharvop)
                case ("grain")
                  call mgt_harvgrain (j, ipl, iharvop)
                case ("residue")
                  harveff = d_tbl%act(iac)%const
                  call mgt_harvresidue (j, harveff)
                case ("tree")
                case ("tuber")
                  call mgt_harvtuber (j, ipl, iharvop)
                case ("peanuts")
                  call mgt_harvtuber (j, ipl, iharvop)
                case ("stripper")
                  call mgt_harvgrain (j, ipl, iharvop)
                case ("picker")
                  call mgt_harvgrain (j, ipl, iharvop)
                end select
                end if

                  pcom(j)%last_kill = pcomdb(icom)%pl(ipl)%cpnm
                  call mgt_killop (j, ipl)

                  !! sum yield and number of harvests to calc ave yields
                  pl_mass(j)%yield_tot(ipl) = pl_mass(j)%yield_tot(ipl) + pl_yield
                  pl_mass(j)%yield_yr(ipl) = pl_mass(j)%yield_yr(ipl) + pl_yield
                  pcom(j)%plcur(ipl)%harv_num = pcom(j)%plcur(ipl)%harv_num + 1
                  pcom(j)%plcur(ipl)%harv_num_yr = pcom(j)%plcur(ipl)%harv_num_yr + 1
                            
                  !! sum basin crop yields and area harvested
                  iplt_bsn = pcom(j)%plcur(ipl)%bsn_num
                  bsn_crop_yld(iplt_bsn)%area_ha = bsn_crop_yld(iplt_bsn)%area_ha + hru(j)%area_ha
                  bsn_crop_yld(iplt_bsn)%yield = bsn_crop_yld(iplt_bsn)%yield + pl_yield%m * hru(j)%area_ha / 1000.
                  
                  !! sum regional crop yields for soft calibration
                  if (cal_codes%plt == "y") then
                    ireg = hru(j)%crop_reg
                    do ilum = 1, plcal(ireg)%lum_num
                      if (plcal(ireg)%lum(ilum)%meas%name == d_tbl%act(iac)%option) then
                        plcal(ireg)%lum(ilum)%ha = plcal(ireg)%lum(ilum)%ha + hru(j)%area_ha
                        plcal(ireg)%lum(ilum)%sim%yield = plcal(ireg)%lum(ilum)%sim%yield + pl_yield%m * hru(j)%area_ha / 1000.
                      end if
                    end do
                  end if
            
                  idp = pcom(j)%plcur(ipl)%idplt
                  if (pco%mgtout == "y") then
                    write (2612, *) j, time%yrc, time%mo, time%day_mo,  pldb(idp)%plantnm, "    HARV/KILL",        &
                        phubase(j), pcom(j)%plcur(ipl)%phuacc, soil(j)%sw, biomass, rsd1(j)%tot(ipl)%m,     &
                        sol_sumno3(j), sol_sumsolp(j), pl_yield%m, pcom(j)%plstr(ipl)%sum_n,   &
                        pcom(j)%plstr(ipl)%sum_p, pcom(j)%plstr(ipl)%sum_tmp, pcom(j)%plstr(ipl)%sum_w,     &
                        pcom(j)%plstr(ipl)%sum_a
                  end if 
                end if
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
              
              if (pco%mgtout == "y") then
                write (2612, *) j, time%yrc, time%mo, time%day_mo, tilldb(idtill)%tillnm, "  DRAIN_CONTROL",    &
                    phubase(j), pcom(j)%plcur(1)%phuacc, soil(j)%sw, pl_mass(j)%tot(1)%m,        &
                    rsd1(j)%tot(1)%m, sol_sumno3(j), sol_sumsolp(j), hru(j)%lumv%sdr_dep
              end if
            end if
                                   
          ! set the amount of water to be diverted
          case ("divert") 
            ! ob_num is set in wallo_demand
            select case (d_tbl%act(iac)%option)
                
            case ("flo_cms")    !! flow diversion demand to m3
              trans_m3 = d_tbl%act(iac)%const * 86400.

            case ("min_cms")    !! minimum flow rate (m3/s)
              if (ob(ob_num)%hd(1)%flo / 86400. < d_tbl%act(iac)%const + .0001) then
                !! below min - all flow to downstream channel (first outflow hydrograph in connect file)
                trans_m3 = 0.
              else
                !! above min flow 
                trans_m3 = ob(ob_num)%hd(1)%flo - d_tbl%act(iac)%const * 86400.
              end if
              
            case ("all_flo")    !! all flow diverted
              trans_m3 = ob(ob_num)%hd(1)%flo

            case ("min_frac")   !! minimum - constant fraction 
              trans_m3 = d_tbl%act(iac)%const * ob(ob_num)%hd(1)%flo
              
            case ("recall")
              irec = d_tbl%act_typ(iac)
              select case (recall(irec)%typ)
              case (1)    !daily
                trans_m3 = recall(irec)%hd(time%day,time%yrs)%flo
              case (2)    !monthly
                trans_m3 = recall(irec)%hd(time%mo,time%yrs)%flo
              case (3)    !annual
                trans_m3 = recall(irec)%hd(1,time%yrs)%flo
              end select
                
            end select
                            
          ! set the demand from a reservoir
          case ("res_demand") 
            j = d_tbl%act(iac)%ob_num
            if (j == 0) j = ob_cur
            
            select case (d_tbl%act(iac)%option)
            !! demand is to fill to principal spillway
            case ("storage")
              if (d_tbl%act(iac)%file_pointer == "pvol") then
                dmd_m3 = d_tbl%act(iac)%const * res_ob(j)%pvol - res(j)%flo
                dmd_m3 = Max (0., dmd_m3)
              end if
              if (d_tbl%act(iac)%file_pointer == "evol") then
                dmd_m3 = d_tbl%act(iac)%const * res_ob(j)%evol - res(j)%flo
                dmd_m3 = Max (0., dmd_m3)
              end if
            end select
                                                                        
          !flow control for water allocation - needs to be modified***
          case ("flow_control") !! set flow fractions in con file
            ! ob_num is the object number of the current channel
            select case (d_tbl%act(iac)%option)
                
            case ("min_cms")    !! minimum flow rate (m3/s) left in first outflow channel in connect file
              if (ob(ob_num)%hd(1)%flo / 86400. < d_tbl%act(iac)%const + .0001) then
                !! below min - all flow to downstream channel (first outflow hydrograph in connect file)
                frac = 1.
              else
                !! above min flow - set first channel fraction to min and divert the rest to the second channel
                frac = d_tbl%act(iac)%const / (ob(ob_num)%hd(1)%flo / 86400.)
              end if
              
            case ("all_flo")    !! all flow to first outflow channel in connect file
              frac = 1.

            case ("min_frac")   !! minimum or constant fraction 
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
            
          !turn off hru impounded water - rice paddy or wetland
          case ("impound_off")
            j = d_tbl%act(iac)%ob_num
            if (j == 0) j = ob_cur
            
            hru(j)%dbs%surf_stor = 0
            wet(j) = hz
            wet_wat_d(j) = wbodz
           
          !turn on hru impounded water - rice paddy or wetland
          case ("impound_on")
            j = d_tbl%act(iac)%ob_num
            if (j == 0) j = ob_cur
            iihru = j
            
            !! xwalk with wetland.wet name
            do ires = 1, db_mx%wet_dat
              if (wet_dat(ires)%name == d_tbl%act(iac)%file_pointer) then
                hru(j)%dbs%surf_stor = ires
              end if
            end do
              
            call wet_initial (iihru)
         
          !adjust weir height - rice paddy
          case ("weir_height")
            j = d_tbl%act(iac)%ob_num
            if (j == 0) j = ob_cur
            
            if (d_tbl%act(iac)%option == "wet") then
              wet_ob(j)%weir_hgt = d_tbl%act(iac)%const / 1000. !m
              !update pvol/evol according to weir height for paddy weir discharge. Jaehak 2023
              wet_ob(j)%pvol = hru(j)%area_ha * wet_ob(j)%weir_hgt * 10.**4  ! m3
              if (wet_ob(j)%evol < wet_ob(j)%pvol*1.2) then
                wet_ob(j)%evol = wet_ob(j)%pvol * 1.2   
              endif
              
            else 
              res_ob(j)%weir_hgt = d_tbl%act(iac)%const / 1000.
            end if
           
          !puddling operation for rice paddies
          case ("puddle")
            j = d_tbl%act(iac)%ob_num
            if (j == 0) j = ob_cur
            if (pcom(j)%dtbl(idtbl)%num_actions(iac) <= Int(d_tbl%act(iac)%const2)) then

              !! xwalk with puddle.ops
              do ipdl = 1, db_mx%pudl_db
                if (pudl_db(ipdl)%name == d_tbl%act(iac)%option) then
                  ipud = ipdl
                end if
              end do
              
              !decrease hydraulic conductivity of upper layer
              if (pudl_db(ipud)%wet_hc>0) hru(j)%wet_hc = pudl_db(ipud)%wet_hc
              !increase sediment concentration in water
              wet(j)%sed = pudl_db(ipud)%sed * wet(j)%flo / 1000000.      ! t = ppm (1 t/1000000 m3) * m3
              !we can add nutrients when needed - may need to subtract nutrients from soil to maintain balance

              do idb = 1, db_mx%tillparm
                if (tilldb(idb)%tillnm == d_tbl%act(iac)%name) then
                  idtill = idb
                  exit
                endif
              end do
            
              if (wet_ob(j)%depth > 0.001) then
                call mgt_newtillmix_wet(j,idtill) 
              else
                call mgt_newtillmix(j,0.,idtill) 
              endif
              pcom(j)%dtbl(idtbl)%num_actions(iac) = pcom(j)%dtbl(idtbl)%num_actions(iac) + 1
            endif
            
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
                call pl_burnop (j, iburn)
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
              cn2(j) = chg_par (cn2(j), d_tbl%act(iac)%option, d_tbl%act(iac)%const, 35., 95.)
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