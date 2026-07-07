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
                                   !         |curb length that is sweepable
      real :: biomass              !         |
      real :: amt_mm               !         |
      real :: frt_kg               !kg/ha    |amount of fertilizer applied
      real :: pest_kg              !kg/ha    |amount of pesticide applied 
      real :: chg_par              !variable |new parameter value

      j = ihru
      
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
                pcom(j)%plcur(ipl)%gro = "y"
                pcom(j)%plcur(ipl)%idorm = "n"
                call mgt_plantop
                if (mgt%op4 > 0) then
                    call mgt_transplant (mgt%op4)
                  end if
                if (pco%mgtout ==  "y") then
                  write (2612, *) j, time%yrc, time%mo, time%day_mo, pldb(idp)%plantnm,  "    PLANT ", &
                      phubase(j), pcom(j)%plcur(ipl)%phuacc,  soil(j)%sw,                          &
                      pl_mass(j)%tot(ipl)%m, rsd1(j)%tot_com%m, sol_sumno3(j),                     &
                      sol_sumsolp(j),pcom(j)%plg(ipl)%lai, pcom(j)%plcur(ipl)%lai_pot
                end if
              end if
            end do

          case ("harv")  !! harvest only operation
            iharvop = mgt%op1

            do ipl = 1, pcom(j)%npl
              if (pcom(j)%plcur(ipl)%gro == "y") then
              biomass = pl_mass(j)%tot(ipl)%m
              if (mgt%op_char == pcomdb(icom)%pl(ipl)%cpnm .or. mgt%op_char == "all") then
                pcom(j)%days_harv = 1       !reset days since last harvest    
                  
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
                case ("peanuts")
                  call mgt_harvtuber (j, ipl, iharvop)
                case ("stripper")
                  call mgt_harvgrain (j, ipl, iharvop)
                case ("picker")
                  call mgt_harvgrain (j, ipl, iharvop)
                end select

                !! sum yield and number of harvest to calc ave yields
                pl_mass(j)%yield_tot(ipl) = pl_mass(j)%yield_tot(ipl) + pl_yield
                pcom(j)%plcur(ipl)%harv_num = pcom(j)%plcur(ipl)%harv_num + 1
                
                !! sum basin crop yields and area harvested
                iplt_bsn = pcom(j)%plcur(ipl)%bsn_num
                bsn_crop_yld(iplt_bsn)%area_ha = bsn_crop_yld(iplt_bsn)%area_ha + hru(j)%area_ha
                bsn_crop_yld(iplt_bsn)%yield = bsn_crop_yld(iplt_bsn)%yield + pl_yield%m * hru(j)%area_ha / 1000.
                !! sum regional crop yields for soft calibration
                if (hru(j)%crop_reg > 0) then
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
                          
                !harvest specific type
                select case (harvop_db(iharvop)%typ)
                case ("biomass")    
                  call mgt_harvbiomass (j, ipl, iharvop)
                case ("grain")
                  call mgt_harvgrain (j, ipl, iharvop)
                case ("residue")
                case ("tree")
                case ("tuber")
                end select
            
                call mgt_killop (j, ipl)

                !! sum yield and num. of harvest to calc ave yields
                pl_mass(j)%yield_tot(ipl) = pl_mass(j)%yield_tot(ipl) + pl_yield
                pcom(j)%plcur(ipl)%harv_num = pcom(j)%plcur(ipl)%harv_num + 1
                
                !! sum basin crop yields and area harvested
                iplt_bsn = pcom(j)%plcur(ipl)%bsn_num
                bsn_crop_yld(iplt_bsn)%area_ha = bsn_crop_yld(iplt_bsn)%area_ha + hru(j)%area_ha
                bsn_crop_yld(iplt_bsn)%yield = bsn_crop_yld(iplt_bsn)%yield + pl_yield%m * hru(j)%area_ha / 1000.
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
                  write (2612, *) j, time%yrc, time%mo, time%day_mo,  pldb(idp)%plantnm, "    HARV/KILL ",          &
                      phubase(j), pcom(j)%plcur(ipl)%phuacc, soil(j)%sw, biomass, rsd1(j)%tot(ipl)%m,           &
                      sol_sumno3(j), sol_sumsolp(j), pl_mass(j)%yield_tot(ipl)%m, pcom(j)%plstr(ipl)%sum_n,     &
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
            call pl_fert (j, ifrt, frt_kg, ifertop)

            if (pco%mgtout == "y") then
              write (2612,*) j, time%yrc, time%mo, time%day_mo, mgt%op_char, "    FERT ", &
                phubase(j), pcom(j)%plcur(ipl)%phuacc, soil(j)%sw, pl_mass(j)%tot(ipl)%m,           &
                rsd1(j)%tot_com%m, sol_sumno3(j), sol_sumsolp(j), frt_kg, fertno3, fertnh3,         &
                fertorgn, fertsolp, fertorgp
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
            
            cn2(j) = chg_par (cn2(j), j, mgt%op_char, mgt%op3, 35., 95., 0)
            call curno (cn2(j), j)

            if (pco%mgtout == "y") then
              write (2612,*) j, time%yrc, time%mo, time%day_mo, mgt%op_char, "    CNUP ", &
                phubase(j), pcom(j)%plcur(ipl)%phuacc, soil(j)%sw, pl_mass(j)%tot(ipl)%m,           &
                rsd1(j)%tot_com%m, sol_sumno3(j), sol_sumsolp(j), mgt%op3, cn2(j)
            endif
 
          case ("burn")   !! burning
            iburn = mgt%op1                 !burn type from fire data base
            do ipl = 1, pcom(j)%npl
              call pl_burnop (j, ipl, iburn)
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
              do jj = 1, soil(ihru)%nly
                if (hru(j)%lumv%sdr_dep < soil(ihru)%phys(jj)%d) hru(ihru)%lumv%ldrain = jj
                if (hru(j)%lumv%sdr_dep < soil(ihru)%phys(jj)%d) exit
              end do
            else
                hru(ihru)%lumv%ldrain = 0
            endif 
            !! added below changed plcur(ipl) to plcur(j) and plm(ipl) to plm(j) gsm 1/30/2018
            if (pco%mgtout ==  "y") then
              write (2612, *) j, time%yrc, time%mo, time%day_mo, pldb(idp)%plantnm,  "    DRAINAGE_MGT ",       &
                   phubase(j), pcom(ihru)%plcur(j)%phuacc,  soil(ihru)%sw,                                  &
                   pl_mass(j)%tot(j)%m, rsd1(j)%tot_com%m, sol_sumno3(j),                                   &
                   sol_sumsolp(ihru),hru(j)%lumv%sdr_dep
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