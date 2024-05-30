      subroutine mallo_control (imallo)
      
      use manure_allocation_module
      use hru_module
      use basin_module
      use time_module
      use plant_module
      use soil_module
      use organic_mineral_mass_module
      use conditional_module
      
      implicit none 

      integer, intent (in) :: imallo     !water allocation object number
      integer :: idmd                       !water demand object number
      integer :: isrc                       !source object number
      integer :: j                          !hru number
      integer :: id                         !decision table number
      integer :: ifrt                       !number in fertilizer.frt
      integer :: ifertop                    !number in chem_app file
      real :: frt_kg                        !m3     |demand

      !! zero demand, withdrawal, and unmet for entire allocation object
      mallo(imallo)%tot = malloz
      
      !!add manure produced on the first day of the month
      if (time%day_mo == 1) then
        mallo(imallo)%src(isrc)%bal_d%stor = mallo(imallo)%src(isrc)%bal_d%stor +   &
                                      mallo(imallo)%src(isrc)%prod_mon(time%day_mo)
        mallo(imallo)%src(isrc)%bal_d%prod = mallo(imallo)%src(isrc)%prod_mon(time%day_mo)
      end if
      
      !!loop through each demand object for manure demand
      do idmd = 1, mallo(imallo)%dmd_obs
        !! check decision table for manure application
        if (mallo(imallo)%dmd(idmd)%dtbl /= "null" .and. mallo(imallo)%dmd(idmd)%dtbl_num /= 0) then
          j = mallo(imallo)%dmd(idmd)%ob_num
          id = mallo(imallo)%dmd(idmd)%dtbl_num
          d_tbl => dtbl_lum(id)
          call conditions (j, id)
          call actions (j, idmd, id)
        end if
      end do
 
      !!loop through each demand object again and subtract from source if available
      do idmd = 1, mallo(imallo)%dmd_obs
        if (mallo(imallo)%dmd(idmd)%manure_amt%app_t_ha > 0. .and.      &
                    frt_kg > mallo(imallo)%src(isrc)%bal_d%stor) then
          isrc = mallo(imallo)%dmd(idmd)%manure_amt%src_obj         !source object
          ifrt = mallo(imallo)%src(isrc)%fertdb                     !fertilizer type from fert data base
          frt_kg = mallo(imallo)%dmd(idmd)%manure_amt%app_t_ha      !amount applied in kg/ha
          ifertop = mallo(imallo)%dmd(idmd)%manure_amt%app_method   !surface application fraction from chem app data base
          ihru = mallo(imallo)%dmd(idmd)%ob_num                        !hru number
          call pl_fert (ifrt, frt_kg, ifertop)
          mallo(imallo)%dmd(idmd)%manure_amt = manure_amtz
          
          !! subtract manure from source
          mallo(imallo)%src(isrc)%bal_d%stor = mallo(imallo)%src(isrc)%bal_d%stor - frt_kg
          mallo(imallo)%src(isrc)%bal_d%withdr = frt_kg
          
          !! set daily withdrawal and source
          mallo(imallo)%dmd(idmd)%withdr(isrc) = frt_kg

          if (pco%mgtout == "y") then
            write (2612, *) j, time%yrc, time%mo, time%day_mo, fertdb(ifrt)%fertnm, "    MANU",       &
                  phubase(j),pcom(j)%plcur(ipl)%phuacc, soil(j)%sw, pl_mass(j)%tot(ipl)%m,            &
                  rsd1(j)%tot(ipl)%m, sol_sumno3(j), sol_sumsolp(j), frt_kg, fertno3, fertnh3,        &
                  fertorgn, fertsolp, fertorgp
          endif
        end if

      end do
        
      return
      end subroutine mallo_control