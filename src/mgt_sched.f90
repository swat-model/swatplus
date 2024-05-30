      subroutine mgt_sched (isched)

      use plant_data_module
      use mgt_operations_module
      use tillage_data_module
      use basin_module
      use hydrograph_module
      use hru_module, only : hru, ihru, cn2, phubase, ndeat, igrz, grz_days,    &
        yr_skip, sol_sumno3, sol_sumsolp, fertnh3, fertno3, fertorgn,  &
        fertorgp, fertsolp, ipl, sweepeff, yr_skip
      use soil_module
      use plant_module
      use time_module
      use constituent_mass_module
      use organic_mineral_mass_module
      use calibration_data_module
      use reservoir_data_module
      use reservoir_module
      use maximum_data_module
      use aquifer_module
      
      implicit none
      
      integer :: icom              !         |  
      integer :: idp               !         |
      integer :: j                 !none     |counter
      integer :: iharvop           !         |harvest operation type 
      integer :: idtill            !none     |tillage type
      integer :: ifrt              !         |fertilizer type from fert data base
      integer :: iob               !         | 
      integer :: ipestcom          !none     |counter
      integer :: ipest             !none     |sequential pesticide type from pest community
      integer :: ipestop           !none     |surface application fraction from chem app data base 
      integer :: irrop             !none     |irrigation ops data base pointer
      integer :: jj                !none     |counter
      integer :: isched            !         | 
      integer :: iburn             !none     |burn type from fire data base
      integer :: ifertop           !frac     |surface application fraction from chem app data base
      integer :: iplt_bsn
      integer :: ireg
      integer :: ilum
      real :: fr_curb              !none     |availability factor, the fraction of the 
      integer :: ires = 0
      integer :: ipud, ipdl        !none     |counter Jaehak 2022
                                   !         |curb length that is sweepable
      real :: biomass              !         |
      real :: frt_kg               !kg/ha    |amount of fertilizer applied
      real :: pest_kg              !kg/ha    |amount of pesticide applied 
      real :: chg_par              !variable |new parameter value
      real :: wsa1
      real :: harveff
      integer :: idb               !none     |counter
      integer :: itr

      j = ihru
      ires= hru(j)%dbs%surf_stor ! for paddy management Jaehak 2022
      iob = hru(j)%obj_no
      wsa1 = hru(ihru)%area_ha * 10. 
      
      ! determine which plant in community (%op2)
      if (mgt%op /= "fert      ") then
        mgt%op2 = 0
        icom = pcom(j)%pcomdb
        if (icom > 0) then
          if (pcom(j)%npl > 1) then
            do ipl = 1, pcom(j)%npl
              if (mgt%op_char == pcomdb(icom)%pl(ipl)%cpnm) then
                mgt%op2 = ipl
                exit
              end if
            end do
          end if
        end if
      end if
         
      select case (mgt%op)

          case ("plnt")    !! plant one plant or entire community
            icom = pcom(j)%pcomdb
            pcom(j)%days_plant = 1       !reset days since last plant
            
            do ipl = 1, pcom(j)%npl
              
              idp = pcomdb(icom)%pl(ipl)%db_num
              if (mgt%op_char == pcomdb(icom)%pl(ipl)%cpnm) then
                !! check to see if the crop is already growing
                if (pcom(j)%plcur(ipl)%gro == "n") then
                !! plant if the crop is not growing
                  pcom(j)%plcur(ipl)%gro = "y"
                  pcom(j)%plcur(ipl)%idorm = "n"
                  call mgt_plantop
                  if (mgt%op3 > 0) then
                    do idb = 1, db_mx%transplant
                      if (mgt%op_plant == transpl(idb)%name) then
                        itr = idb
                        exit
                      endif
                    end do
                    if (itr > 0) call mgt_transplant (itr)
                  end if
                  if (pco%mgtout ==  "y") then
                    write (2612, *) j, time%yrc, time%mo, time%day_mo, pldb(idp)%plantnm,  "    PLANT ",    &
                      phubase(j), pcom(j)%plcur(ipl)%phuacc,  soil(j)%sw,                                   &
                      pl_mass(j)%tot(ipl)%m, rsd1(j)%tot_com%m, sol_sumno3(j),                              &
                      sol_sumsolp(j),pcom(j)%plg(ipl)%lai, pcom(j)%plcur(ipl)%lai_pot
                  end if
                else
                  !! don't plant if the crop is already growing
                  if (pco%mgtout ==  "y") then
                    write (2612, *) j, time%yrc, time%mo, time%day_mo, pldb(idp)%plantnm,       &
                      "    PLANT_ALREADY_GROWING", phubase(j), pcom(j)%plcur(ipl)%phuacc,       &
                      soil(j)%sw, pl_mass(j)%tot(ipl)%m, rsd1(j)%tot_com%m, sol_sumno3(j),      &
                      sol_sumsolp(j),pcom(j)%plg(ipl)%lai, pcom(j)%plcur(ipl)%lai_pot
                  end if
                end if
              end if
            end do
            
          case ("mons")  !! begin and end monsoon initiation period
            do ipl = 1, pcom(j)%npl
              idp = pcom(j)%plcur(ipl)%idplt
              if (pldb(idp)%trig == "moisture_gro") then
                if (mgt%op3 == 0) then
                  !! if precip/pet ratio was not triggered during monsoon season - reset phenology
                  if (pcom(j)%plcur(ipl)%gro == "n") then
                    pcom(j)%plcur(ipl)%phuacc = 0.
                    pcom(j)%plcur(ipl)%gro = "y" 
                    pcom(j)%plcur(ipl)%idorm = "n"
                    pcom(j)%plcur(ipl)%mseas = "n"
                  end if
                end if 
                if (mgt%op3 == 1) pcom(j)%plcur(ipl)%mseas = "y"
              endif
            end do

          case ("harv")  !! harvest only operation
            iharvop = mgt%op1

            do ipl = 1, pcom(j)%npl
              if (pcom(j)%plcur(ipl)%gro == "y") then
              biomass = pl_mass(j)%tot(ipl)%m
              if (mgt%op_char == pcomdb(icom)%pl(ipl)%cpnm .or. mgt%op_char == "all") then
                pcom(j)%days_harv = 1       !reset days since last harvest    
                  
                !check minimum biomass for harvest
                if (biomass > harvop_db(iharvop)%bm_min) then
                !harvest specific type
                select case (harvop_db(iharvop)%typ)
                case ("biomass")
                  call mgt_harvbiomass (j, ipl, iharvop)
                case ("grain")
                  call mgt_harvgrain (j, ipl, iharvop)
                case ("residue")
                  harveff = mgt%op3
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

                !! sum yield and number of harvest to calc ave yields
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
                  if (hru(j)%crop_reg > 0) then
                    ireg = hru(j)%crop_reg
                    do ilum = 1, plcal(ireg)%lum_num
                      if (plcal(ireg)%lum(ilum)%meas%name == mgt%op_char) then
                        plcal(ireg)%lum(ilum)%ha = plcal(ireg)%lum(ilum)%ha + hru(j)%area_ha
                        plcal(ireg)%lum(ilum)%sim%yield = plcal(ireg)%lum(ilum)%sim%yield + pl_yield%m * hru(j)%area_ha / 1000.
                      end if
                    end do
                  end if
                end if
            
                idp = pcom(j)%plcur(ipl)%idplt
                if (pco%mgtout == "y") then
                  write (2612, *) j, time%yrc, time%mo, time%day_mo,  pldb(idp)%plantnm, "    HARVEST ",    &
                      phubase(j), pcom(j)%plcur(ipl)%phuacc, soil(j)%sw, biomass, rsd1(j)%tot(ipl)%m,   &
                      sol_sumno3(j), sol_sumsolp(j), pl_yield%m, pcom(j)%plstr(ipl)%sum_n,              &
                      pcom(j)%plstr(ipl)%sum_p, pcom(j)%plstr(ipl)%sum_tmp, pcom(j)%plstr(ipl)%sum_w,   &
                      pcom(j)%plstr(ipl)%sum_a
                end if 
              end if
              pcom(j)%plcur(ipl)%phuacc = 0.
              end if
            end do
          
            case ("kill")   !! kill operation
              do ipl = 1, pcom(j)%npl
                biomass = pl_mass(j)%tot(ipl)%m
                if (mgt%op_char == pcomdb(icom)%pl(ipl)%cpnm .or. mgt%op_char == "all") then
                  call mgt_killop (j, ipl)
  
                  idp = pcom(j)%plcur(ipl)%idplt
                  if (pco%mgtout == "y") then
                    write (2612, *) j, time%yrc, time%mo, time%day_mo,  pldb(idp)%plantnm, "    KILL ",  &
                      phubase(j), pcom(j)%plcur(ipl)%phuacc, soil(j)%sw, biomass, rsd1(j)%tot(ipl)%m,   &
                      sol_sumno3(j), sol_sumsolp(j), pl_yield%m, pcom(j)%plstr(ipl)%sum_n,              &
                      pcom(j)%plstr(ipl)%sum_p, pcom(j)%plstr(ipl)%sum_tmp, pcom(j)%plstr(ipl)%sum_w,   &
                      pcom(j)%plstr(ipl)%sum_a
                  end if 
                end if
                pcom(j)%plcur(ipl)%phuacc = 0.
              end do
       
          case ("hvkl")   !! harvest and kill operation
            iharvop = mgt%op1

            do ipl = 1, pcom(j)%npl
              biomass = pl_mass(j)%tot(ipl)%m
              if (mgt%op_char == pcomdb(icom)%pl(ipl)%cpnm .or. mgt%op_char == "all") then
                pcom(j)%days_harv = 1       !reset days since last harvest   
                  
                !check minimum biomass for harvest
                if (biomass > harvop_db(iharvop)%bm_min) then
                !harvest specific type
                select case (harvop_db(iharvop)%typ)
                case ("biomass")    
                  call mgt_harvbiomass (j, ipl, iharvop)
                case ("grain")
                  call mgt_harvgrain (j, ipl, iharvop)
                case ("residue")
                  harveff = mgt%op3
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

                call mgt_killop (j, ipl)

                !! sum yield and num. of harvest to calc ave yields
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
                    if (plcal(ireg)%lum(ilum)%meas%name == mgt%op_char) then
                      plcal(ireg)%lum(ilum)%ha = plcal(ireg)%lum(ilum)%ha + hru(j)%area_ha
                      plcal(ireg)%lum(ilum)%sim%yield = plcal(ireg)%lum(ilum)%sim%yield + pl_yield%m * hru(j)%area_ha / 1000.
                    end if
                  end do
                end if
            
                idp = pcom(j)%plcur(ipl)%idplt
                if (pco%mgtout == "y") then
                  write (2612, *) j, time%yrc, time%mo, time%day_mo,  pldb(idp)%plantnm, "    HARV/KILL ",      &
                      phubase(j), pcom(j)%plcur(ipl)%phuacc, soil(j)%sw, biomass, rsd1(j)%tot(ipl)%m,           &
                      sol_sumno3(j), sol_sumsolp(j), pl_yield%m, pcom(j)%plstr(ipl)%sum_n,                      &
                      pcom(j)%plstr(ipl)%sum_p, pcom(j)%plstr(ipl)%sum_tmp, pcom(j)%plstr(ipl)%sum_w,           &
                      pcom(j)%plstr(ipl)%sum_a
                end if 
              end if
              pcom(j)%plstr(ipl) = plstrz
              pcom(j)%plcur(ipl)%phuacc = 0.
              phubase(j) = 0.
            end do
          case ("till")   !! tillage operation
            idtill = mgt%op1
            ipl = Max(1, mgt%op2)
            call mgt_newtillmix(j, 0., idtill)
            
            if (pco%mgtout == "y") then
              write (2612, *) j, time%yrc, time%mo, time%day_mo, tilldb(idtill)%tillnm, "    TILLAGE ", &
                  phubase(j), pcom(j)%plcur(ipl)%phuacc, soil(j)%sw, pl_mass(j)%tot(ipl)%m,         &
                  rsd1(j)%tot(ipl)%m, sol_sumno3(j), sol_sumsolp(j), tilldb(idtill)%effmix
            end if

          case ("irrm")  !! date scheduled irrigation operation
            ipl = 1
            irrop = mgt%op1                        !irrigation amount (mm) from irr.ops data base
            irrig(j)%applied = irrop_db(irrop)%amt_mm * irrop_db(irrop)%eff * (1. - irrop_db(irrop)%surq)
            irrig(j)%runoff = irrop_db(irrop)%amt_mm * irrop_db(irrop)%surq
            pcom(j)%days_irr = 1            ! reset days since last irrigation

            !print irrigation applied
            if (pco%mgtout == "y") then
              write (2612, *) j, time%yrc, time%mo, time%day_mo, "        ", "IRRIGATE ", phubase(j),   &
                  pcom(j)%plcur(ipl)%phuacc, soil(j)%sw,pl_mass(j)%tot(ipl)%m, rsd1(j)%tot(ipl)%m,      &
                  sol_sumno3(j), sol_sumsolp(j), irrig(j)%applied, irrig(j)%runoff
            end if
          
          case ("fert")   !! fertilizer operation
            ipl = 1
            ifrt = mgt%op1                          !fertilizer type from fert data base
            frt_kg = mgt%op3                        !amount applied in kg/ha
            ifertop = mgt%op4                       !surface application fraction from chem app data base
            if (wet(j)%flo>0.) then !case for surface application with standing water
              call pl_fert_wet (ifrt, frt_kg) 
              call salt_fert_wet(j,ifrt,frt_kg)
              call cs_fert_wet(j,ifrt,frt_kg)
              if (pco%mgtout == "y") then
                write (2612,*) j, time%yrc, time%mo, time%day_mo, mgt%op_char, " FERT-WET", &
                  phubase(j), pcom(j)%plcur(ipl)%phuacc, soil(j)%sw, pl_mass(j)%tot(ipl)%m,           &
                  rsd1(j)%tot_com%m, sol_sumno3(j), sol_sumsolp(j), frt_kg, fertno3, fertnh3,         &
                  fertorgn, fertsolp, fertorgp
              endif
            else
              call pl_fert (ifrt, frt_kg, ifertop)
              call salt_fert(j,ifrt,frt_kg,ifertop) !rtb salt 
              call cs_fert(j,ifrt,frt_kg,ifertop) !rtb cs
              if (pco%mgtout == "y") then
                write (2612,*) j, time%yrc, time%mo, time%day_mo, mgt%op_char, "    FERT ", &
                  phubase(j), pcom(j)%plcur(ipl)%phuacc, soil(j)%sw, pl_mass(j)%tot(ipl)%m,           &
                  rsd1(j)%tot_com%m, sol_sumno3(j), sol_sumsolp(j), frt_kg, fertno3, fertnh3,         &
                  fertorgn, fertsolp, fertorgp
              endif
            endif
            

 
          case ("pest")   !! pesticide operation
            !xwalk application in the mgt file with the pest community
            iob = sp_ob%hru + ihru - 1
            do ipestcom = 1, cs_db%num_pests
              if (cs_db%pests(ipestcom) == mgt%op_char) then
                mgt%op1 = ipestcom
                exit
              end if
            end do

            ipl = 1
            ipest = mgt%op1                                     !sequential pesticide type from pest community
            ipestop = mgt%op4                                   !surface application option from chem app data base
            pest_kg = mgt%op3 * chemapp_db(ipestop)%app_eff    !amount applied in kg/ha
            call pest_apply (j, ipest, pest_kg, ipestop)
            
            if (pco%mgtout == "y") then
              write (2612, *) j, time%yrc, time%mo, time%day_mo, mgt%op_char, "    PEST ", &
                phubase(j), pcom(j)%plcur(ipl)%phuacc, soil(j)%sw,pl_mass(j)%tot(ipl)%m,   &
                rsd1(j)%tot(ipl)%m, sol_sumno3(j), sol_sumsolp(j), pest_kg
            endif

          case ("graz")    !! grazing operation
            ndeat(j) = 0
            igrz(j) = 1
            ipl = Max(1, mgt%op2)
            grz_days(j) = Int(mgt%op3)
            graze = grazeop_db(mgt%op1)

            if (pco%mgtout == "y") then
              write (2612, *) j, time%yrc, time%mo, time%day_mo, "         ", "    GRAZE ",         &
                phubase(j), pcom(j)%plcur(ipl)%phuacc, soil(j)%sw,pl_mass(j)%tot(ipl)%m,        &
                rsd1(j)%tot(ipl)%m, sol_sumno3(j), sol_sumsolp(j), grazeop_db(mgt%op1)%eat, grazeop_db(mgt%op1)%manure
            endif
 
          case ("cnup")   !! fertilizer operation
            ipl = 1
            ifertop = mgt%op4                       !surface application fraction from chem app data base
            
            cn2(j) = chg_par (cn2(j), mgt%op_char, mgt%op3, 35., 95.)
            call curno (cn2(j),j)

            if (pco%mgtout == "y") then
              write (2612,*) j, time%yrc, time%mo, time%day_mo, mgt%op_char, "    CNUP ", &
                phubase(j), pcom(j)%plcur(ipl)%phuacc, soil(j)%sw, pl_mass(j)%tot(ipl)%m,           &
                rsd1(j)%tot_com%m, sol_sumno3(j), sol_sumsolp(j), mgt%op3, cn2(j)
            endif
 
          case ("burn")   !! burning
            iburn = mgt%op1                 !burn type from fire data base
            do ipl = 1, pcom(j)%npl
              call pl_burnop (j, iburn)
            end do
                        
            if (pco%mgtout == "y") then
              write (2612, *) j, time%yrc, time%mo, time%day_mo, "        ", "    BURN ", phubase(j),   &
                  pcom(j)%plcur(ipl)%phuacc, soil(j)%sw,pl_mass(j)%tot(ipl)%m, rsd1(j)%tot(ipl)%m,      &
                  sol_sumno3(j), sol_sumsolp(j)
            end if

          case ("swep")   !! street sweeping (only if iurban=2)
            ipl = Max(1, mgt%op2)
            sweepop = sweepop_db(mgt%op1)
            sweepeff = sweepop%eff
            fr_curb = sweepop%fr_curb
                  
            if (pco%mgtout == "y") then
              write (2612, *) j, time%yrc, time%mo, time%day_mo, "        ", "STREET SWEEP ", phubase(j),    &
                  pcom(j)%plcur(ipl)%phuacc, soil(j)%sw, pl_mass(j)%tot(ipl)%m, rsd1(j)%tot(ipl)%m,          &
                  sol_sumno3(j), sol_sumsolp(j)
            end if
            
          case ("dwm")    !! set drain depth for drainage water management
            hru(j)%lumv%sdr_dep = mgt%op3
            if (hru(j)%lumv%sdr_dep > 0) then
              do jj = 1, soil(j)%nly
                if (hru(j)%lumv%sdr_dep < soil(j)%phys(jj)%d) hru(j)%lumv%ldrain = jj
                if (hru(j)%lumv%sdr_dep < soil(j)%phys(jj)%d) exit
              end do
            else
                hru(j)%lumv%ldrain = 0
            endif 
            !! added below changed plcur(ipl) to plcur(j) and plm(ipl) to plm(j) gsm 1/30/2018
            if (pco%mgtout ==  "y") then
              write (2612, *) j, time%yrc, time%mo, time%day_mo, pldb(idp)%plantnm,  "  DRAIN_CONTROL",         &
                   phubase(j), pcom(j)%plcur(j)%phuacc,  soil(j)%sw, pl_mass(j)%tot(j)%m, rsd1(j)%tot_com%m,    &
                   sol_sumno3(j), sol_sumsolp(j),hru(j)%lumv%sdr_dep
            endif

          case ("weir")    !! set/adjust weir height

            !! set weir height and adjust principal spillway storage and depth
            wet_ob(j)%weir_hgt = mgt%op3 / 1000. !weir height, m
            wet_ob(j)%pvol = hru(j)%area_ha * wet_ob(j)%weir_hgt * 10.
            if (wet_ob(j)%evol < wet_ob(j)%pvol*1.1) then
              wet_ob(j)%evol = wet_ob(j)%pvol * 1.1   
            endif
              
          case ("irrp")  !! continuous irrigation to maintain surface ponding in rice fields Jaehak 2022
            hru(j)%irr_src = mgt%op_plant                   !irrigation source: cha; res; aqu; or unlim													
            hru(j)%irr_hmin = irrop_db(mgt%op1)%dep_mm     !threshold ponding depth, mm
            irrig(j)%eff = irrop_db(mgt%op1)%eff
            irrig(j)%frac_surq = irrop_db(mgt%op1)%surq
            pcom(j)%days_irr = 1            ! reset days since last irrigation
            if (mgt%op3 < 0) then
              hru(j)%irr_hmax = irrop_db(mgt%op1)%amt_mm     !irrigation amount in irr.org, mm
            else
              hru(j)%irr_hmax = mgt%op3       !target ponding depth, mm
              if (mgt%op3 > 0) then
                hru(j)%paddy_irr = 1 !paddy irrigation is on with manual scheduling
              else
                hru(j)%paddy_irr = 0!paddy irrigation is off
                hru(j)%irr_hmin = 0
              endif
            endif
            
          !print irrigation COMMAND
            if (pco%mgtout == "y") then
              write (2612, *) j, time%yrc, time%mo, time%day_mo, "        ", "BEGIN PADDY IRRIGATION ", phubase(j),   &
                  pcom(j)%plcur(ipl)%phuacc, soil(j)%sw,pl_mass(j)%tot(ipl)%m, rsd1(j)%tot(ipl)%m,      &
                  sol_sumno3(j), sol_sumsolp(j), mgt%op3, mgt%op4
            end if
 
          case ("irpm")  !! date scheduled irrigation operation for rice fields
            ipl = 1
            irrop = mgt%op1                        !irrigation amount (mm) from irr.ops data base
            irrig(j)%applied = irrop_db(irrop)%amt_mm * irrop_db(irrop)%eff * (1. - irrop_db(irrop)%surq)
            irrig(j)%runoff = irrop_db(irrop)%amt_mm * irrop_db(irrop)%surq
            pcom(j)%days_irr = 1            ! reset days since last irrigation

            !print irrigation applied
            if (pco%mgtout == "y") then
              write (2612, *) j, time%yrc, time%mo, time%day_mo, "        ", "IRRIGATE ", phubase(j),   &
                  pcom(j)%plcur(ipl)%phuacc, soil(j)%sw,pl_mass(j)%tot(ipl)%m, rsd1(j)%tot(ipl)%m,      &
                  sol_sumno3(j), sol_sumsolp(j), irrig(j)%applied, irrig(j)%runoff
            end if

            
          case ("pudl")    !! Puddling operation Jaehak 2022
            !! xwalk with puddling ops names
            do ipdl = 1, db_mx%pudl_db
              if (pudl_db(ipdl)%name == mgt%op_plant) then
                ipud = ipdl
              end if
            end do
              
            !decrease hydraulic conductivity of upper layer
            if (pudl_db(ipud)%wet_hc>0) hru(j)%wet_hc = pudl_db(ipud)%wet_hc
            !increase sediment concentration in water
            wet(j)%sed = pudl_db(ipud)%sed * wet(j)%flo / 1000000.      ! t = ppm (1 t/1000000 m3) * m3
            
            do idb = 1, db_mx%tillparm
              if (tilldb(idb)%tillnm == mgt%op_char) then
                idtill = idb
                exit
              endif
            end do
            
            if (wet_ob(j)%depth > 0.001) then
              call mgt_newtillmix_wet(j,idtill) 
            else
              call mgt_newtillmix(j,0.,idtill) 
            endif

          case ("skip")    !! skip a year
            yr_skip(j) = 1

      end select

      if (mgt%op /= "skip") hru(j)%cur_op = hru(j)%cur_op + 1  !don't icrement if skip year
      if (hru(j)%cur_op > sched(isched)%num_ops) then
        hru(j)%cur_op = 1
      end if
      
      mgt = sched(isched)%mgt_ops(hru(j)%cur_op)
   
      return

      end subroutine mgt_sched