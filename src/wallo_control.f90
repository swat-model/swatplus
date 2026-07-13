      subroutine wallo_control (iwallo)
      
      use water_allocation_module
      use hydrograph_module   !, only : irrig, hz, recall, icmd
      use hru_module
      use basin_module
      use time_module
      use plant_module
      use soil_module
      use reservoir_module
      use sd_channel_module
      use organic_mineral_mass_module
      use constituent_mass_module !rtb
      
      implicit none 

      integer, intent (inout) :: iwallo     !water allocation object number
      integer :: itrn = 0                   !water demand object number
      integer :: iosrc = 0                  !source object number
      integer :: isrc = 0                   !source object number
      integer :: j = 0                      !hru number
      integer :: jj = 0                     !variable for passing
      integer :: irec = 0                   !recall id
      integer :: iob = 0                    !object number for passing transfers to a stream
      integer :: dum = 0
      real :: irr_mm = 0.                   !mm     |irrigation applied
      real :: div_total = 0.                !m3     |cumulative available diversion water
      real :: div_daily = 0.                !m3     |daily water diverted for irrigation
      
      !! zero total transfer and treatment and use outflows
      wal_omd(iwallo)%trn(:)%h_tot = hz
      
      !!transfer water from sources to receiving objects for transfer object
      itrn = wallo(iwallo)%trn_cur
               
        !! zero demand, withdrawal, and unmet for each source
        do isrc = 1, wallo(iwallo)%trn(itrn)%src_num
          wallod_out(iwallo)%trn(itrn)%src(isrc) = walloz
          wal_omd(iwallo)%trn(itrn)%src(isrc)%hd = hz
        end do
          
        !! set demand for each transfer object - wallod_out(iwallo)%trn(itrn)%trn_flo
        call wallo_demand (iwallo, itrn)
        
        !! initialize unmet to total demand and subtract as water is withdrawn
        wallo(iwallo)%trn(itrn)%unmet_m3 = wallod_out(iwallo)%trn(itrn)%trn_flo
      
        !! zero demand, withdrawal, and unmet for each source
        wallod_out(iwallo)%trn(itrn)%src(:) = walloz

        !! compute demand for each source object
        wdraw_om_tot = hz
        do isrc = 1, wallo(iwallo)%trn(itrn)%src_num
          wallod_out(iwallo)%trn(itrn)%src(isrc)%demand = wallo(iwallo)%trn(itrn)%src(isrc)%frac *      &
                                                                wallod_out(iwallo)%trn(itrn)%trn_flo
        end do
 
        !! if demand - check source availability and withdraw water
        if (wallod_out(iwallo)%trn(itrn)%trn_flo > 0.) then
            
          !! check if water is available from each source - set withdrawal and unmet - wallo(iwallo)%trn(itrn)%src(isrc)%hd
          wdraw_om_tot = hz
          do isrc = 1, wallo(iwallo)%trn(itrn)%src_num
            trn_m3 = wallod_out(iwallo)%trn(itrn)%src(isrc)%demand
            if (trn_m3 > 1.e-6) then
              call wallo_withdraw (iwallo, itrn, isrc)
            end if
          end do
        
          !! loop through sources again to check if compensation is allowed
          do isrc = 1, wallo(iwallo)%trn(itrn)%src_num
            if (wallo(iwallo)%trn(itrn)%src(isrc)%comp == "y") then
              trn_m3 = wallo(iwallo)%trn(itrn)%unmet_m3
              if (trn_m3 > 1.e-6) then
                call wallo_withdraw (iwallo, itrn, isrc)
              end if
            end if
          end do
        
          !! compute total withdrawal for receiving object from all sources
          wallo(iwallo)%trn(itrn)%withdr_tot = 0.
          do isrc = 1, wallo(iwallo)%trn(itrn)%src_num
            wallo(iwallo)%trn(itrn)%withdr_tot = wallo(iwallo)%trn(itrn)%withdr_tot +           &
                                                  wallod_out(iwallo)%trn(itrn)%src(isrc)%withdr
          end do
        
          !! transfer water (pipes) to receiving object from all sources
          call wallo_transfer (iwallo, itrn)
        
          !! add water withdrawn from source to the receiving object  - wal_omd(iwallo)%trn(itrn)%h_tot
          j = wallo(iwallo)%trn(itrn)%rcv%num
          select case (wallo(iwallo)%trn(itrn)%rcv%typ)
          !! irrigation transfer - set amount applied and runoff
          case ("hru")
            !! irrigate if amount withdrawn is > 0 --> or > irrig(j)%demand
            if (wallo(iwallo)%trn(itrn)%withdr_tot > 0.) then
              irr_mm = wallo(iwallo)%trn(itrn)%withdr_tot / (hru(j)%area_ha * 10.)      !mm = m3 / (ha * 10.)
              irrig(j)%applied = irr_mm * wallo(iwallo)%trn(itrn)%irr_eff * (1. - wallo(iwallo)%trn(itrn)%surq)
              irrig(j)%runoff = wallo(iwallo)%trn(itrn)%amount * wallo(iwallo)%trn(itrn)%surq
              !! add irrigation water n and p to the soil when routing the soluble n and p
              irrig(j)%water = wal_omd(iwallo)%trn(itrn)%h_tot
              pcom(j)%days_irr = 1            ! reset days since last irrigation
              
              !rtb salt: irrigation salt mass accounting
              if(cs_db%num_salts > 0) then
                jj = itrn !to avoid a compiler warning
                call salt_irrig(iwallo,jj,j)
              endif
              !rtb cs: irrigation constituent mass accounting
              if(cs_db%num_cs > 0) then
                jj = itrn !to avoid a compiler warning
                call cs_irrig(iwallo,jj,j)
              endif
              
              ! add irrigation to yearly sum for dtbl conditioning jga6-25
              hru(j)%irr_yr = hru(j)%irr_yr + irrig(j)%applied
            
              if (pco%mgtout == "y") then
                write (2612, *) j, time%yrc, time%mo, time%day_mo, wallo(iwallo)%name, "IRRIGATE", phubase(j),  &
                  pcom(j)%plcur(1)%phuacc, soil(j)%sw, pl_mass(j)%tot(1)%m, pl_mass(j)%rsd_tot%m,      &
                  sol_sumno3(j), sol_sumsolp(j), irrig(j)%applied
              end if
            end if
            
            !! divert flowing water from channel source
            case ("cha")
              iob = sd_ch(j)%obj_no
              ob(iob)%trans = wal_omd(iwallo)%trn(itrn)%h_tot
            
            case ("res")
              !! reservoir transfer - maintain reservoir levels at a specified level or required transfer
              res(j) = res(j) + wal_omd(iwallo)%trn(itrn)%h_tot
              if (ob(res_ob(j)%ob)%rcv_tot == 0) then
                call res_control (j)
              end if 
            
            case ("aqu")
              !! aquifer transfer - maintain aquifer levels at a specified level or required transfer
              aqu(j) = aqu(j) + wal_omd(iwallo)%trn(itrn)%h_tot
              !! calculate water table depth
              
            case ("wtp")
              !! wastewater treatment 
              wtp_om_stor(j) = wtp_om_stor(j) + wal_omd(iwallo)%trn(itrn)%h_tot
              !! compute outflow and concentrations
              call wallo_treatment (iwallo, itrn, j)
              
            case ("use")
              !! water use (domestic, industrial, commercial) 
              wuse_om_stor(j) = wuse_om_stor(j) + wal_omd(iwallo)%trn(itrn)%h_tot
              !! compute outflow and concentrations
              call wallo_use (iwallo, itrn, j)
              
            case ("stor")
              !! water tower storage - don't change concentrations or compute outflow
              wtow_om_stor(j) = wtow_om_stor(j) + wal_omd(iwallo)%trn(itrn)%h_tot
           
            case ("can")
              !! canal storage - compute outflow - change concentrations?
              canal_om_stor(j) = canal_om_stor(j) + wal_omd(iwallo)%trn(itrn)%h_tot
              !! compute losses - evap and seepage, and outflow
              call wallo_canal (iwallo, itrn, j)
              
            case ("orcv")
              !! outside receiving object
              orcv_om(j) = orcv_om(j) + wal_omd(iwallo)%trn(itrn)%h_tot
           
          end select
        
        end if      !if there is demand 
        
        !! sum organics 
        wdraw_om_tot = wdraw_om_tot + wdraw_om
        
        !! sum constituents
        
        !! sum demand, withdrawal, and unmet for entire allocation object
        wallo(iwallo)%tot%demand = wallo(iwallo)%tot%demand + wallod_out(iwallo)%trn(itrn)%trn_flo
        wallo(iwallo)%tot%withdr = wallo(iwallo)%tot%withdr + wallo(iwallo)%trn(itrn)%withdr_tot
        wallo(iwallo)%tot%unmet = wallo(iwallo)%tot%unmet + wallo(iwallo)%trn(itrn)%unmet_m3
        
        wallo(iwallo)%trn_cur = wallo(iwallo)%trn_cur + 1
        if (wallo(iwallo)%trn_cur > wallo(iwallo)%trn_obs) wallo(iwallo)%trn_cur = 0
        
      return
      end subroutine wallo_control