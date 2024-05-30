      subroutine conditions (ob_cur, idtbl)
      !current conditions include: w_stress, n_stress, phu_plant, phu_base0, soil_water, jday, month, vol
      ! year_rot, year_cal, year_seq, prob, land_use   
      !target variables include: w_stress -> wp, fc, ul; vol -> pvol, evol
    
      use conditional_module
      use climate_module
      use time_module
      use hru_module, only : hru
      use soil_module
      use plant_module
      use reservoir_module
      use reservoir_data_module
      use sd_channel_module
      use hydrograph_module
      use output_landscape_module
      use aquifer_module
      use organic_mineral_mass_module
      use mgt_operations_module
      use water_allocation_module

      implicit none

      integer, intent (in)  :: ob_cur         !          |
      integer, intent (in)  :: idtbl          !none      |
      integer :: ob_num                       !          |object number   
      integer :: ic                           !none      |counter
      integer :: ialt                         !none      |counter
      integer :: iac                          !none      |counter
      integer :: iob                          !          |
      real :: targ_val                        !          |
      real :: ran_num                         !          |
      real :: aunif                           !          |
      integer :: ires                         !          |
      integer :: ipl                          !          |
      integer :: iipl                         !          |
      integer :: id                           !          |
      integer :: isched                       !          |
      integer :: iauto                        !          |
      integer :: ivar_cur 
      integer :: ivar_tbl
      real :: targ                            !          |
      integer :: pl_sum                       !none      |number of plants growing
      integer :: days_tot                     !none      |
      integer :: iwgn                         !units     |
      integer :: ly                           !units     |soil layer
      real :: strs_sum                        !none      |sum of stress (water or n) of all growing plants
      real :: prob_cum                        !          |
      real :: prob_apply                      !          |
      real :: hru_exp_left                    !          |number of hru's expected to still be applied (uniform or normal distr)
      real :: hru_act_left                    !          |number of hru's actually still to be applied
      real :: flo_m3
      real :: wt_tot
      real :: p_lab_tot
      real :: p_lab_ppm
      real :: rto
      real :: var_cur
      character(len=1) :: pl_chk
      
      d_tbl%act_hit = "y"
      do ic = 1, d_tbl%conds
        select case (d_tbl%cond(ic)%var)
            
        !water stress
        case ("w_stress")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          !! find average water stress of all growing plants
          pl_sum = 0
          strs_sum = 0.
          do ipl = 1, pcom(ob_num)%npl
            if (pcom(ob_num)%plcur(ipl)%gro == "y") then
              pl_sum = pl_sum + 1
              strs_sum = strs_sum + pcom(ob_num)%plstr(ipl)%strsw
            end if
          end do
          if (pl_sum > 0) then
            strs_sum = strs_sum / pl_sum
          else
            strs_sum = 1.
          end if

          call cond_real (ic, strs_sum, d_tbl%cond(ic)%lim_const, idtbl)
         
        !nitrogen stress
        case ("n_stress")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          !! find average water stress of all growing plants
          pl_sum = 0
          strs_sum = 0.
          do ipl = 1, pcom(ob_num)%npl
            if (pcom(ob_num)%plcur(ipl)%gro == "y") then
              pl_sum = pl_sum + 1
              strs_sum = strs_sum + pcom(ob_num)%plstr(ipl)%strsn
            end if
          end do
          if (pl_sum > 0) strs_sum = strs_sum / pl_sum

          call cond_real (ic, strs_sum, d_tbl%cond(ic)%lim_const, idtbl)
          
        !phosphorus stress
        case ("p_stress")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          !! find average water stress of all growing plants
          pl_sum = 0
          strs_sum = 0.
          do ipl = 1, pcom(ob_num)%npl
            if (pcom(ob_num)%plcur(ipl)%gro == "y") then
              pl_sum = pl_sum + 1
              strs_sum = strs_sum + pcom(ob_num)%plstr(ipl)%strsp
            end if
          end do
          if (pl_sum > 0) strs_sum = strs_sum / pl_sum

          call cond_real (ic, strs_sum, d_tbl%cond(ic)%lim_const, idtbl)
          
        !potential heat units - plant based
        case ("phu_plant")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          do ialt = 1, d_tbl%alts
            !if no plants in the community - set action to no
            if (pcom(ob_num)%npl == 0) then
              d_tbl%act_hit(ialt) = "n"
            else
              !if more than one plant in community - use first one that is growing
              ipl = 1
              do iipl = 1, pcom(ob_num)%npl
                if (pcom(ob_num)%plcur(iipl)%phuacc > 1.e-6) then
                  ipl = iipl
                  exit
                end if
              end do
            end if
          end do
            
            call cond_real (ic, pcom(ob_num)%plcur(ipl)%phuacc, d_tbl%cond(ic)%lim_const, idtbl)
            
        !potential heat units - base zero
        case ("phu_base0")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          if (d_tbl%cond(ic)%ob == "hru") then
            iob = sp_ob1%hru + ob_num - 1
          end if
          if (d_tbl%cond(ic)%ob == "hlt") then
            iob = sp_ob1%hru_lte + ob_num - 1
          end if
          iwst = ob(iob)%wst
          
          call cond_real (ic, wst(iwst)%weat%phubase0, d_tbl%cond(ic)%lim_const, idtbl)
                         
        !precip on current day
        case ("precip_cur")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          if (d_tbl%cond(ic)%ob == "hru") then
            iob = sp_ob1%hru + ob_num - 1
          end if
          if (d_tbl%cond(ic)%ob == "hlt") then
            iob = sp_ob1%hru_lte + ob_num - 1
          end if
          iwst = ob(iob)%wst
          
          call cond_real (ic, wst(iwst)%weat%precip, d_tbl%cond(ic)%lim_const, idtbl)
                                      
        !precip on next day day
        case ("precip_next")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          if (d_tbl%cond(ic)%ob == "hru") then
            iob = sp_ob1%hru + ob_num - 1
          end if
          if (d_tbl%cond(ic)%ob == "hlt") then
            iob = sp_ob1%hru_lte + ob_num - 1
          end if
          iwst = ob(iob)%wst
          
          call cond_real (ic, wst(iwst)%weat%precip_next, d_tbl%cond(ic)%lim_const, idtbl)
                      
        !plant growing
        case ("plant_gro")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          ipl = Max (Int(d_tbl%cond(ic)%lim_const), 1)
          do ialt = 1, d_tbl%alts
            if (d_tbl%alt(ic,ialt) == "=") then    !determine if growing (y) or not (n)
              if (pcom(ob_num)%plcur(ipl)%gro /= d_tbl%cond(ic)%lim_var) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
          end do
                                    
        !specific plant growing
        case ("plant_name_gro")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          ipl = 0
          do iipl = 1, pcom(ob_num)%npl
            if (d_tbl%cond(ic)%lim_var == pcom(ob_num)%pl(iipl)) then
              ipl = iipl
            end if
          end do
          !plant not in community of ipl = 0
          if (ipl == 0) then
            d_tbl%act_hit(ialt) = "n"
          else 
            !if plant is in the community - check to see if it is growing
            do ialt = 1, d_tbl%alts
              if (d_tbl%alt(ic,ialt) == "=") then    !determine if growing (y) or not (n)
                if (pcom(ob_num)%plcur(ipl)%gro == "n") then
                  d_tbl%act_hit(ialt) = "n"
                end if
              end if
            end do
          end if
                         
        !days since last plant
        case ("days_plant")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur 
          
          ivar_cur = pcom(ob_num)%days_plant
          ivar_tbl = int(d_tbl%cond(ic)%lim_const)
          call cond_integer (ic, ivar_cur, ivar_tbl)
                                   
        !days since last harvest
        case ("days_harv")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          ivar_cur = pcom(ob_num)%days_harv
          ivar_tbl = int(d_tbl%cond(ic)%lim_const)
          call cond_integer (ic, ivar_cur, ivar_tbl)
                                         
        !days since last irrigation
        case ("days_irr")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          ivar_cur = pcom(ob_num)%days_irr
          ivar_tbl = int(d_tbl%cond(ic)%lim_const)
          call cond_integer (ic, ivar_cur, ivar_tbl)
                                         
        !days since last action
        case ("days_act")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          iac = d_tbl%con_act(ic)
          ivar_cur = pcom(ob_num)%dtbl(idtbl)%days_act(iac)
          ivar_tbl = int(d_tbl%cond(ic)%lim_const) + 2
          call cond_integer (ic, ivar_cur, ivar_tbl)
          
        !days since first simulation day of year
        case ("day_start")
          ivar_cur = time%day_start 
          ivar_tbl = int(d_tbl%cond(ic)%lim_const)
          call cond_integer (ic, ivar_cur, ivar_tbl)
                                           
        !slope
        case ("slope")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          call cond_real (ic, hru(ob_num)%topo%slope, d_tbl%cond(ic)%lim_const, idtbl)
          
        !soil water
        case ("soil_water")
          !determine target variable
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          targ_val = 0.
          select case (d_tbl%cond(ic)%lim_var)
          case ("wp")   !wilting point
            targ_val = 0.
          case ("fc")   !field capacity
            targ_val = soil(ob_num)%sumfc
          case ("ul")   !upper limit (porosity)
            targ_val = soil(ob_num)%sumul
          end select
          
          !perform operation on target variable to get target
          select case ((d_tbl%cond(ic)%lim_op))
          case ("*")
            targ = targ_val * d_tbl%cond(ic)%lim_const
          case ("+")
            targ = targ_val + d_tbl%cond(ic)%lim_const
          case ("-")
            targ = targ_val - d_tbl%cond(ic)%lim_const
          case ("/")
            targ = targ_val / d_tbl%cond(ic)%lim_const
          end select
          
          call cond_real (ic, soil(ob_num)%sw, targ, idtbl)
            
        !julian day
        case ("jday")
          ivar_cur = time%day 
          ivar_tbl = int(d_tbl%cond(ic)%lim_const)
          call cond_integer (ic, ivar_cur, ivar_tbl)

        !month
        case ("month")
          ivar_cur = time%mo 
          ivar_tbl = int(d_tbl%cond(ic)%lim_const)
          call cond_integer (ic, ivar_cur, ivar_tbl)
          
        !rotation year
        case ("year_rot")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          ivar_cur = pcom(ob_num)%rot_yr 
          ivar_tbl = int(d_tbl%cond(ic)%lim_const)
          call cond_integer (ic, ivar_cur, ivar_tbl)

        !growth year of perennials
        case ("year_gro")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          ivar_cur = pcom(ob_num)%plcur(1)%curyr_mat
          ivar_tbl = int(d_tbl%cond(ic)%lim_const)
          call cond_integer (ic, ivar_cur, ivar_tbl)
                       
        !calendar year
        case ("year_cal")
          ivar_cur = time%yrc
          ivar_tbl = int(d_tbl%cond(ic)%lim_const)
          call cond_integer (ic, ivar_cur, ivar_tbl)
          
        !sequential year of simulation
        case ("year_seq")
          ivar_cur = time%yrs
          ivar_tbl = int(d_tbl%cond(ic)%lim_const)
          call cond_integer (ic, ivar_cur, ivar_tbl)
                  
        !current years of maturity for perennial plants
        case ("cur_yrs_mat")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          ivar_cur = pcom(ob_num)%plcur(1)%curyr_mat
          ivar_tbl = int(d_tbl%cond(ic)%lim_const)
          call cond_integer (ic, ivar_cur, ivar_tbl)
                       
        !above ground biomass
        case ("biomass")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          call cond_real (ic, pl_mass(ob_num)%ab_gr_com%m, d_tbl%cond(ic)%lim_const, idtbl)
                                                               
        !leaf area index
        case ("leaf_area")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          call cond_real (ic, pcom(ob_num)%lai_sum, d_tbl%cond(ic)%lim_const, idtbl)
                                                                         
        !total ground cover - above ground biomass + surface residue
        case ("ground_cov")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          call cond_real (ic, pl_mass(ob_num)%ab_gr_com%m, d_tbl%cond(ic)%lim_const, idtbl)
                                                                                        
        !usle conservation practice P factor
        case ("p_factor")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          call cond_real (ic, hru(ob_num)%lumv%usle_p, d_tbl%cond(ic)%lim_const, idtbl)
                                                                                                   
        !usle soil erodibility K factor
        case ("k_factor")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          call cond_real (ic, soil(ob_num)%ly(1)%usle_k, d_tbl%cond(ic)%lim_const, idtbl)
                        
        !hydrologic soil group
        case ("hyd_soil_group")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          do ialt = 1, d_tbl%alts
            if (d_tbl%alt(ic,ialt) == "=") then
              if (soil(ob_num)%hydgrp /= d_tbl%cond(ic)%lim_var) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
          end do
                                                          
        !precip/pet ratio
        case ("p_pet")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          iwst = ob(ob_num)%wst
          iwgn = wst(iwst)%wco%wgn
          call cond_real (ic, wgn_pms(iwgn)%p_pet_rto, d_tbl%cond(ic)%lim_const, idtbl)
                                                                                                                            
        !soil labile phosphorus of first layer
        case ("p_lab_150")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          !! use labile P concentration of upper (10 mm) layer
          wt_tot = soil(ob_num)%phys(1)%bd * soil(ob_num)%phys(1)%thick / 100.    ! mg/kg => kg/ha
          p_lab_tot =  soil1(ob_num)%mp(1)%lab / wt_tot
          do ly = 2, soil(ob_num)%nly
            if (soil(ob_num)%phys(ly)%d > 150.) then
              rto = (150. - soil(ob_num)%phys(ly-1)%d) / (soil(ob_num)%phys(ly)%d - soil(ob_num)%phys(ly-1)%d)
            else
              rto = 1.
            end if
            wt_tot = wt_tot + soil(ob_num)%phys(ly)%bd * soil(ob_num)%phys(ly)%thick / 100.    ! mg/kg => kg/ha
            p_lab_tot = p_lab_tot + soil1(ob_num)%mp(ly)%lab
            exit
          end do
          p_lab_ppm = p_lab_tot / wt_tot
            
          call cond_real (ic, p_lab_ppm, d_tbl%cond(ic)%lim_const, idtbl)

        !soil temperature of second layer
        case ("soil_temp2")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          call cond_real (ic, soil(ob_num)%phys(2)%tmp, d_tbl%cond(ic)%lim_const, idtbl)
                 
        !soil organic carbon of first layer
        case ("soil_carbon")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          call cond_real (ic, soil1(ob_num)%cbn(1), d_tbl%cond(ic)%lim_const, idtbl)
                                                                                                         
        !is the hru tiled - 0 = no; >0 = yes
        case ("tile_drained")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          do ialt = 1, d_tbl%alts
            if (d_tbl%alt(ic,ialt) == "=") then
              if (hru(ob_num)%tiledrain /= Int(d_tbl%cond(ic)%lim_const)) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl%alt(ic,ialt) == "/") then
              if (hru(ob_num)%tiledrain == Int(d_tbl%cond(ic)%lim_const)) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
          end do
                    
        !probability
        case ("prob")
          !generate random number
          ran_num = Aunif(rndseed_cond)
          
          call cond_real (ic, ran_num, d_tbl%cond(ic)%lim_const, idtbl)
                    
        !probability of event within a defined period assuming uniform distribution
        case ("prob_unif")
          !! set application day if first day of application window
          if (time%day == d_tbl%cond(ic)%ob_num) then
          !period falls within a calendar year - ob_num is first and lim_const is last day
          if (d_tbl%cond(ic)%lim_const > d_tbl%cond(ic)%ob_num) then
            !set the application day
            ran_num = Aunif(rndseed_cond)
            if (ran_num < d_tbl%frac_app) then
              days_tot = d_tbl%cond(ic)%lim_const - d_tbl%cond(ic)%ob_num + 1
              ran_num = Aunif(rndseed_cond)
              pcom(ob_cur)%dtbl(idtbl)%apply_day = int(ran_num * days_tot) + d_tbl%cond(ic)%ob_num + 1
            else
              pcom(ob_cur)%dtbl(idtbl)%apply_day = 0.
            end if
          else
            !period falls over a calendar year - ob_num is first and lim_const is last day
            ran_num = Aunif(rndseed_cond)
            if (ran_num < d_tbl%frac_app) then
              if (time%day >= d_tbl%cond(ic)%lim_const) then
                days_tot = time%day_end_yr - d_tbl%cond(ic)%ob_num + 1
                ran_num = Aunif(rndseed_cond)
                pcom(ob_cur)%dtbl(idtbl)%apply_day = int(ran_num * days_tot) + d_tbl%cond(ic)%ob_num + 1
              else
                days_tot = d_tbl%cond(ic)%lim_const
                ran_num = Aunif(rndseed_cond)
                pcom(ob_cur)%dtbl(idtbl)%apply_day = int(ran_num * days_tot) + 1
              end if
            else
              pcom(ob_cur)%dtbl(idtbl)%apply_day = 0.
            end if
          end if
          end if

          if (time%day /= pcom(ob_cur)%dtbl(idtbl)%apply_day) then
            do ialt = 1, d_tbl%alts
              if (d_tbl%alt(ic,ialt) == "=") then
                d_tbl%act_hit(ialt) = "n"
              end if
            end do
          end if
                         
        !probability of event within a defined period assuming uniform distribution
        case ("prob_unif1")
          !! update cumulative probability if new day
          if (time%day /= d_tbl%day_prev .and. d_tbl%days_prob > 0) then
            !! reset if first day of window
            if (time%day == d_tbl%cond(ic)%ob_num) then
              d_tbl%days_prob = d_tbl%cond(ic)%lim_const
              d_tbl%prob_cum = 0.
            end if
            d_tbl%day_prev = time%day
            !! check if winow is over - days_prob are the number of days left in window
            if (d_tbl%days_prob >= 1) then
              !! cumulative prob of uniform distribution on current day of the window
              d_tbl%prob_cum = 1. / float(d_tbl%days_prob)
              d_tbl%days_prob = d_tbl%days_prob - 1
            else
              d_tbl%days_prob = 0.
              d_tbl%prob_cum = 0.
            end if
          end if

          ran_num = Aunif(rndseed_cond)
          if (ran_num > d_tbl%prob_cum) then
            do ialt = 1, d_tbl%alts
              if (d_tbl%alt(ic,ialt) == "=") then
                d_tbl%act_hit(ialt) = "n"
              end if
            end do
          end if
 
        !probability of event within a defined period assuming uniform distribution across a land use
        case ("prob_unif_lu")
          prob_cum = 0.
          !check if period falls over a calendar year - ob_num is first and lim_const is last day
          if (d_tbl%cond(ic)%lim_const > d_tbl%cond(ic)%ob_num) then
            if (time%day <= d_tbl%cond(ic)%lim_const .and. time%day >= d_tbl%cond(ic)%ob_num) then
              !cumulative prob of uniform distribution on current day of the window
              prob_cum = (time%day - d_tbl%cond(ic)%ob_num + 1) / (d_tbl%cond(ic)%lim_const - d_tbl%cond(ic)%ob_num)
            end if
          else
            if (time%day >= d_tbl%cond(ic)%lim_const .or. time%day <= d_tbl%cond(ic)%ob_num) then
              days_tot = time%day_end_yr - d_tbl%cond(ic)%lim_const + d_tbl%cond(ic)%ob_num
              if (time%day >= d_tbl%cond(ic)%lim_const) then
                !cumulative prob of uniform distribution on current day of the window
                prob_cum = (time%day - d_tbl%cond(ic)%lim_const) / days_tot
              else
                !cumulative prob of uniform distribution on current day of the window
                prob_cum = (time%day_end_yr - d_tbl%cond(ic)%lim_const + time%day + 1) / days_tot
              end if
            end if
          end if
          if (prob_cum > 0.) then
            ran_num = Aunif(rndseed_cond)
            hru_exp_left = d_tbl%hru_lu - (prob_cum * d_tbl%hru_lu)
            hru_act_left = d_tbl%hru_lu - d_tbl%hru_lu_cur
            prob_apply = (hru_act_left - hru_exp_left) / (hru_act_left + 1)
            !prob_apply = (prob_cum * d_tbl%hru_lu - d_tbl%hru_lu_cur + 1) / d_tbl%hru_lu
            if (ran_num > prob_apply) then
              do ialt = 1, d_tbl%alts
                if (d_tbl%alt(ic,ialt) == "=") then
                  d_tbl%act_hit(ialt) = "n"
                end if
              end do
            else
              !if (pcom(ob_cur)%dtbl(idtbl)%num_actions(iac) <= Int(d_tbl%act(iac)%const2)) then
              !  d_tbl%hru_lu_cur = d_tbl%hru_lu_cur + 1
              !  d_tbl%hru_ha_cur = d_tbl%hru_ha_cur + hru(ob_cur)%area_ha
              !end if
              ipl = 1
            end if
          else
            do ialt = 1, d_tbl%alts
              if (d_tbl%alt(ic,ialt) == "=") then
                d_tbl%act_hit(ialt) = "n"
              end if
            end do
          end if
          
          if (time%day > d_tbl%cond(ic)%lim_const) then
            d_tbl%hru_lu_cur = 0
            d_tbl%hru_ha_cur = 0.
          end if

        !channel flow
        case ("channel_flo")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          !ob_num is channel number - need object number
          iob = sp_ob1%chandeg + ob_num - 1
          flo_m3 = ob(iob)%hd(1)%flo / 86400. 
          call cond_real (ic, flo_m3, d_tbl%cond(ic)%lim_const, idtbl)
                
        !tile flow
        case ("tile_flo")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          call cond_real (ic, hwb_d(ob_num)%qtile, d_tbl%cond(ic)%lim_const, idtbl)
                            
        !irrigation demand
        case ("irr_demand")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          call cond_real (ic, irrig(ob_num)%demand, d_tbl%cond(ic)%lim_const, idtbl)
                                        
        !irrigation demand
        case ("irr_demand_wro")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          call cond_real (ic, wallo(ob_num)%tot%demand, d_tbl%cond(ic)%lim_const, idtbl)
            
        !aquifer depth below surface
        case ("aqu_dep")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          call cond_real (ic, aqu_d(ob_num)%dep_wt, d_tbl%cond(ic)%lim_const, idtbl)
          
        !land use and management
        case ("land_use")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          do ialt = 1, d_tbl%alts
            if (d_tbl%alt(ic,ialt) == "=") then
              if (hru(ob_num)%dbsc%land_use_mgt /= d_tbl%cond(ic)%lim_var) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
          end do
                   
        !tillage system - name of tillage decision table in lum.dtl
        case ("tillage")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          do ialt = 1, d_tbl%alts
            pl_chk = "n"
            if (d_tbl%alt(ic,ialt) == "=") then
              isched = hru(ob_num)%mgt_ops
              do iauto = 1, sched(isched)%num_autos
                id = sched(isched)%num_db(iauto)
                if (dtbl_lum(id)%name == d_tbl%cond(ic)%lim_var) then
                  pl_chk = "y"
                end if
              end do
            end if
            if (pl_chk == "n") d_tbl%act_hit(ialt) = "n"
          end do
               
        !plants - if plant is in the cummunity
        case ("plant")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          do ialt = 1, d_tbl%alts
            !don't check if "-"
            if (d_tbl%alt(ic,ialt) /= "-") then
            !for equal condition - set "n" if not in community
            if (d_tbl%alt(ic,ialt) == "=") then
              pl_chk = "n"
              do ipl = 1, pcom(ob_num)%npl
                if (pcom(ob_num)%pl(ipl) == d_tbl%cond(ic)%lim_var) then
                  pl_chk = "y"
                end if
              end do
              if (pl_chk == "n") d_tbl%act_hit(ialt) = "n"
            end if
            !for not equal condition - set to "n" if in the plant community
            if (d_tbl%alt(ic,ialt) == "/") then
              do ipl = 1, pcom(ob_num)%npl
                if (pcom(ob_num)%pl(ipl) == d_tbl%cond(ic)%lim_var) then
                  d_tbl%act_hit(ialt) = "n"
                end if
              end do
            end if
            end if
          end do
          
        !channel management
        case ("ch_use")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          do ialt = 1, d_tbl%alts
            if (d_tbl%alt(ic,ialt) == "=") then
              if (sd_ch(ob_num)%order /= d_tbl%cond(ic)%lim_var) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
          end do
             
        !reservoir volume
        case ("vol")
          !determine target variable
          ires = d_tbl%cond(ic)%ob_num
          if (ires == 0) ires = ob_cur
          
          select case (d_tbl%cond(ic)%lim_var)
          case ("e-pv")   !emergency minus prinicpal storage volume
            targ_val = res_ob(ires)%pvol
            targ = targ_val + (res_ob(ires)%evol - res_ob(ires)%pvol) * d_tbl%cond(ic)%lim_const
          case ("pvol")   !prinicpal storage volume
            targ_val = res_ob(ires)%pvol
            !perform operation on target variable to get target
            select case ((d_tbl%cond(ic)%lim_op))
            case ("*")
              targ = targ_val * d_tbl%cond(ic)%lim_const
            case ("+")
              targ = targ_val + 10000. * d_tbl%cond(ic)%lim_const
            case ("-")
              targ = targ_val - 10000. * d_tbl%cond(ic)%lim_const   !convert ha-m to m3
            end select
          case ("evol")   !emergency storage volume
            targ_val = res_ob(ires)%evol
            !perform operation on target variable to get target
            select case ((d_tbl%cond(ic)%lim_op))
            case ("*")
              targ = targ_val * d_tbl%cond(ic)%lim_const
            case ("+")
              targ = targ_val + 10000. * d_tbl%cond(ic)%lim_const
            case ("-")
              targ = targ_val - 10000. * d_tbl%cond(ic)%lim_const   !convert ha-m to m3
            end select
          end select

          !check alternatives
          call cond_real (ic, res(ires)%flo, targ, idtbl)
               
        !reservoir inflow: JK added 28/02/2023
        case ("res_inflo")
          !determine target variable
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          iob = sp_ob1%res + ob_num - 1
          flo_m3 = ob(iob)%hin%flo / 86400. 
          call cond_real (ic, flo_m3, d_tbl%cond(ic)%lim_const, idtbl)         
          
            
        !impounded water depth -paddy average water depth of water
        case ("wet_depth")
          !determine target variable
          ires = d_tbl%cond(ic)%ob_num
          if (ires == 0) ires = ob_cur
          
          !set limit constant if comparing to weir height
          if (d_tbl%cond(ic)%lim_var == "hwater") then
            targ = d_tbl%cond(ic)%lim_const/1000. !m
          else
            targ = wet_ob(ires)%weir_hgt
          end if
          
          !check alternatives
          call cond_real (ic, wet_ob(ires)%depth, targ, idtbl)

        !paddy weir height
        case ("weirh")
          !determine target variable
          ires = d_tbl%cond(ic)%ob_num
          if (ires == 0) ires = ob_cur
          
          !set limit constant if comparing to weir height
          targ = d_tbl%cond(ic)%lim_const/1000. !m
          
          !check alternatives
          call cond_real (ic, wet_ob(ires)%weir_hgt, targ, idtbl)

       !paddy storage volume - stored on an hru
            
        !water volume - stored on an hru
        case ("vol_wet")
          !determine target variable
          ires = d_tbl%cond(ic)%ob_num
          if (ires == 0) ires = ob_cur
          
          select case (d_tbl%cond(ic)%lim_var)
          case ("pvol")   !prinicpal storage volume
            targ_val = wet_ob(ires)%pvol
          case ("evol")   !emergency storage volume
            targ_val = wet_ob(ires)%evol
          end select
                      
          !perform operation on target variable to get target
          select case ((d_tbl%cond(ic)%lim_op))
          case ("*")
            targ = targ_val * d_tbl%cond(ic)%lim_const
          case ("+")
            targ = targ_val + d_tbl%cond(ic)%lim_const
          case ("-")
            targ = targ_val - d_tbl%cond(ic)%lim_const
          case ("/")
            targ = targ_val / d_tbl%cond(ic)%lim_const
          end select

          !check alternatives
          call cond_real (ic, wet(ires)%flo, targ, idtbl)
            
        end select
      end do
 
      return
      end subroutine conditions