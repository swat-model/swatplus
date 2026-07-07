      subroutine wallo_control (iwallo)
      
      use water_allocation_module
      use hydrograph_module, only : irrig
      use hru_module
      use basin_module
      use time_module
      use plant_module
      use soil_module
      use organic_mineral_mass_module
      
      implicit none 

      integer, intent (inout) :: iwallo     !water allocation object number
      integer :: idmd                       !water demand object number
      integer :: isrc                       !source object number
      integer :: j                          !hru number
      real :: dmd_m3                        !m3     |demand
      real :: irr_mm                        !mm     |irrigation applied

      do idmd = 1, wallo(iwallo)%dmd_obs
               
        !! zero demand, withdrawal, and unmet for each source
        do isrc = 1, wallo(iwallo)%src_obs
          wallod_out(iwallo)%dmd(idmd)%src(isrc) = walloz
        end do
  
        !! set demand for each object
        call wallo_demand (iwallo, idmd)
 
        !! if demand - check source availability
        if (wallod_out(iwallo)%dmd(idmd)%dmd_tot > 0.) then
            
          !! check if water is available from each source - set withdrawal and unmet
          do isrc = 1, wallo(iwallo)%dmd(idmd)%dmd_src_obs
            dmd_m3 = wallod_out(iwallo)%dmd(idmd)%src(isrc)%demand
            if (dmd_m3 > 1.e-6) then
              call wallo_withdraw (iwallo, idmd, isrc, dmd_m3)
            end if
          end do
        
          !! loop through sources again to check if compensation is allowed
          do isrc = 1, wallo(iwallo)%dmd(idmd)%dmd_src_obs
            if (wallo(iwallo)%dmd(idmd)%src(isrc)%comp == "y") then
              dmd_m3 = wallo(iwallo)%dmd(idmd)%unmet_m3
              if (dmd_m3 > 1.e-6) then
                call wallo_withdraw (iwallo, idmd, isrc, dmd_m3)
              end if
            end  if
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
                                
              if (pco%mgtout == "y") then
                write (2612, *) j, time%yrc, time%mo, time%day_mo, "        ", "IRRIGATE", phubase(j),  &
                  pcom(j)%plcur(1)%phuacc, soil(j)%sw, pl_mass(j)%tot(1)%m, rsd1(j)%tot(1)%m,           &
                  sol_sumno3(j), sol_sumsolp(j), irrig(j)%applied
              end if
            end if
          end select
        
        end if      !if there is demand loop 
      end do        !demand object loop
        
      return
      end subroutine wallo_control