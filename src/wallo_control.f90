      subroutine wallo_control (iwallo)
      
      use water_allocation_module
      use hydrograph_module, only : irrig, hz, recall
      use hru_module
      use basin_module
      use time_module
      use plant_module
      use soil_module
      use organic_mineral_mass_module
      use constituent_mass_module !rtb
      
      implicit none 

      integer, intent (inout) :: iwallo     !water allocation object number
      integer :: idmd                       !water demand object number
      integer :: isrc                       !source object number
      integer :: j                          !hru number
      integer :: jj                         !variable for passing
      integer :: irec                       !recall id
      integer :: dum
      real :: irr_mm                        !mm     |irrigation applied
      real :: div_total                     !m3     |cumulative available diversion water
      real :: div_daily                     !m3     |daily water diverted for irrigation
      

      !! zero demand, withdrawal, and unmet for entire allocation object
      wallo(iwallo)%tot = walloz
      
      !! check if canal or inflow diversions and set available water from recall object
      do isrc = 1,wallo(iwallo)%src_obs
        !! set total volumes of canal diversions (source = "div") - rtb
        if (wallo(iwallo)%src(isrc)%ob_typ == "div") then  
          irec = wallo(iwallo)%src(isrc)%ob_num !number in recall.rec
          !calculate newly available canal water
          div_daily = recall(irec)%hd(time%day,time%yrs)%flo * (-1) !m3; make positive
          if(div_daily > 0) then
            dum = 10
          endif
          div_total = div_volume_daily(irec) !m3
          div_volume_daily(irec) = ((1.-div_delay)*div_daily) + (div_delay*div_total) !m3
          if (div_volume_daily(irec) < 1.e-6) then
            div_volume_daily(irec) = 0.
          endif
          !total available canal water for irrigation
          div_volume_total(irec) = div_volume_total(irec) + div_volume_daily(irec) !m3
        endif
        
        !! set total volumes of inflow diversions from recall file
        if (wallo(iwallo)%src(isrc)%ob_typ == "div_rec") then  
          irec = wallo(iwallo)%src(isrc)%rec_num !number in recall.rec
          !total available source water
          wallo(iwallo)%src(isrc)%div_vol = recall(irec)%hd(time%day,time%yrs)%flo !m3
        end if
        
        !! set total volumes of inflow diversions from monthly flow rate (m3/s)
        if (wallo(iwallo)%src(isrc)%ob_typ == "div_in") then  
          !total available source water - m3
          wallo(iwallo)%src(isrc)%div_vol = wallo(iwallo)%src(isrc)%limit_mon(time%mo) * 86400.
        end if
      end do
      
      !!loop through each demand object
      do idmd = 1, wallo(iwallo)%dmd_obs
               
        !! zero demand, withdrawal, and unmet for each source
        do isrc = 1, wallo(iwallo)%dmd(idmd)%dmd_src_obs
          wallod_out(iwallo)%dmd(idmd)%src(isrc) = walloz
        end do
  
        !! set demand for each object
        call wallo_demand (iwallo, idmd)
 
        !! if demand - check source availability
        if (wallod_out(iwallo)%dmd(idmd)%dmd_tot > 0.) then
            
          wallo(iwallo)%dmd(idmd)%hd = hz
          !! check if water is available from each source - set withdrawal and unmet
          do isrc = 1, wallo(iwallo)%dmd(idmd)%dmd_src_obs
            dmd_m3 = wallod_out(iwallo)%dmd(idmd)%src(isrc)%demand
            if (dmd_m3 > 1.e-6) then
              call wallo_withdraw (iwallo, idmd, isrc)
            end if
          end do
        
          !! loop through sources again to check if compensation is allowed
          do isrc = 1, wallo(iwallo)%dmd(idmd)%dmd_src_obs
            if (wallo(iwallo)%dmd(idmd)%src(isrc)%comp == "y") then
              dmd_m3 = wallo(iwallo)%dmd(idmd)%unmet_m3
              if (dmd_m3 > 1.e-6) then
                call wallo_withdraw (iwallo, idmd, isrc)
              end if
            end if
          end do
        
          !! compute total withdrawal for demand object from all sources
          wallo(iwallo)%dmd(idmd)%withdr_tot = 0.
          do isrc = 1, wallo(iwallo)%dmd(idmd)%dmd_src_obs
            wallo(iwallo)%dmd(idmd)%withdr_tot = wallo(iwallo)%dmd(idmd)%withdr_tot +           &
                                                  wallod_out(iwallo)%dmd(idmd)%src(isrc)%withdr
          end do
        
          !! if irrigating set amount applied and runoff
          select case (wallo(iwallo)%dmd(idmd)%ob_typ)
          !! irrigation demand
          case ("hru")
            if (wallo(iwallo)%dmd(idmd)%withdr_tot > 0.) then
              j = wallo(iwallo)%dmd(idmd)%ob_num
              irr_mm = wallo(iwallo)%dmd(idmd)%withdr_tot / (hru(j)%area_ha * 10.)      !mm = m3 / (ha * 10.)
              irrig(j)%applied = irr_mm * wallo(iwallo)%dmd(idmd)%irr_eff * (1. - wallo(iwallo)%dmd(idmd)%surq)
              irrig(j)%runoff = wallo(iwallo)%dmd(idmd)%amount * wallo(iwallo)%dmd(idmd)%surq
              pcom(j)%days_irr = 1            ! reset days since last irrigation
              
              !rtb salt: irrigation salt mass accounting
              if(cs_db%num_salts > 0) then
                jj = idmd !to avoid a compiler warning
                call salt_irrig(iwallo,jj,j)
			  endif
              !rtb cs: irrigation constituent mass accounting
              if(cs_db%num_cs > 0) then
                jj = idmd !to avoid a compiler warning
                call cs_irrig(iwallo,jj,j)
              endif
              
              if (pco%mgtout == "y") then
                write (2612, *) j, time%yrc, time%mo, time%day_mo, "        ", "IRRIGATE", phubase(j),  &
                  pcom(j)%plcur(1)%phuacc, soil(j)%sw, pl_mass(j)%tot(1)%m, rsd1(j)%tot(1)%m,           &
                  sol_sumno3(j), sol_sumsolp(j), irrig(j)%applied
              end if
            end if
          end select
        
        end if      !if there is demand 
        
        !! treatment of withdrawn water
        if (wallo(iwallo)%dmd(idmd)%treat_typ == "out") then
          !! no treatment - treated = withdrawal
          wallo(iwallo)%dmd(idmd)%trt = wallo(iwallo)%dmd(idmd)%hd
        else
          !! compute treatment by inputting the mass or concentrations
          call wallo_treatment (iwallo, idmd)
        end if
        
        !! transfer (diversion) of withdrawn and possibly treated water
        if (wallo(iwallo)%dmd(idmd)%rcv_ob  /= "null") then
          call wallo_transfer (iwallo, idmd)
        end if
        
        !! sum demand, withdrawal, and unmet for entire allocation object
        wallo(iwallo)%tot%demand = wallo(iwallo)%tot%demand + wallod_out(iwallo)%dmd(idmd)%dmd_tot
        wallo(iwallo)%tot%withdr = wallo(iwallo)%tot%withdr + wallo(iwallo)%dmd(idmd)%withdr_tot
        wallo(iwallo)%tot%unmet = wallo(iwallo)%tot%unmet + wallo(iwallo)%dmd(idmd)%unmet_m3
        
      end do        !demand object loop
        
      return
    end subroutine wallo_control
