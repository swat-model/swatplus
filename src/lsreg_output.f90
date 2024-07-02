      subroutine lsreg_output
      
      use time_module
      use basin_module
      use maximum_data_module
      use calibration_data_module
      use plant_data_module
      use landuse_data_module
      use hru_module, only : hru, ihru
      use plant_module
      use output_landscape_module
      use organic_mineral_mass_module
      
      implicit none
      
      integer, dimension(:), allocatable :: iarea
      integer :: ireg
      integer :: ielem
      real :: area_ha
      integer :: i
      integer :: ilum
      integer :: nlum
      real :: const
      integer :: ilum_db
      real :: constnb
      integer :: icu
      real :: constwb
      real :: constpw
              
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!!    PRINT CODES: "avann" = average annual (always print)
!!                  "year"  = yearly
!!                  "mon"   = monthly
!!                  "day"   = daily

      allocate (iarea(db_mx%landuse))
      ! determine the number of land uses within each region
      if (time%day == 1) then   !reset on first day of year (lum can change on day 1)
      iarea = 0
      do ireg = 1, db_mx%lsu_out
        area_ha = 0.
        do ielem = i, region(ireg)%num_tot
          ihru = region(ireg)%num(ielem)
          ilum = hru(ihru)%land_use_mgt
          iarea(ilum) = 1       !track all lums in the lcu- don"t need to sum
        end do

        nlum = sum(iarea)     !number of different lums in the lcu
        region(ireg)%nlum = nlum

        ! allocate output variables
        allocate (rwb_d(ireg)%lum(nlum)); allocate (rwb_m(ireg)%lum(nlum)); allocate (rwb_y(ireg)%lum(nlum))
        allocate (rnb_d(ireg)%lum(nlum)); allocate (rnb_m(ireg)%lum(nlum)); allocate (rnb_y(ireg)%lum(nlum))
        allocate (rls_d(ireg)%lum(nlum)); allocate (rls_m(ireg)%lum(nlum)); allocate (rls_y(ireg)%lum(nlum))
        allocate (rpw_d(ireg)%lum(nlum)); allocate (rpw_m(ireg)%lum(nlum)); allocate (rpw_y(ireg)%lum(nlum))
        
        !set the lum number from the lum database -sequential for the region
        nlum = 1
        iarea = 0
        do ilum = 1, db_mx%landuse
          if (iarea(ilum) == 1) then
            region(ireg)%lum_num(nlum) = ilum
            nlum = nlum + 1
          end if
        end do      !ilum
      end do        !icu

      !set area for each lum within the region
      do ireg = 1, db_mx%lsu_out
        region(ireg)%lum_ha = 0.
        do ielem = 1, region(ireg)%num_tot
          ihru = region(ireg)%num(ielem)
          do ilum = 1, region(ireg)%nlum
            if (hru(ihru)%land_use_mgt == region(ireg)%lum_num(ilum)) then
              region(ireg)%lum_ha(ilum) = region(ireg)%lum_ha(ilum) + hru(ihru)%area_ha
            end if
          end do
        end do
      end do 
      end if    ! time%day == 1
      
      do ireg = 1, db_mx%lsu_out
        do ielem = 1, region(ireg)%num_tot
          ihru = region(ireg)%num(ielem)
          do ilum = 1, region(ireg)%nlum
            if (hru(ihru)%land_use_mgt_c == "                ") then    !need to change "  " to a variable
              !! const should be fraction of the element that was read in element.lcu
              !! for entire basin - should be the basin fraction
              const = region(ireg)%lum_ha(ilum) / hru(ihru)%area_ha
              rwb_d(ireg)%lum(ilum) = hwb_d(ihru) / const
              rwb_m(ireg)%lum(ilum) = hwb_m(ihru) / const
              rwb_y(ireg)%lum(ilum) = hwb_y(ihru) / const
              rwb_a(ireg)%lum(ilum) = hwb_a(ihru) / const
            end if
          end do
        end do 
      end do 
    
      do ireg = 1, db_mx%lsu_out
        do ilum = 1, region(ireg)%nlum
!!!!! daily print
        ilum_db = region(ireg)%lum_num(ilum)
        
         if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
          if (pco%wb_hru%d == "y") then
            write (4412,100) time%day, time%mo, time%day_mo, time%yrc, region(ireg)%name, lum(ilum_db)%plant_cov,  &
                 region(ireg)%lum_ha(ilum), rwb_d(ireg)%lum(ilum)  !! waterbal
             if (pco%csvout == "y") then
               write (4413,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, region(ireg)%name, lum(ilum_db)%plant_cov,  &
                 region(ireg)%lum_ha(ilum), rwb_d(ireg)%lum(ilum)  !! waterbal
             end if
          end if
          if (pco%nb_hru%d == "y") then
            write (4414,100) time%day, time%mo, time%day_mo, time%yrc, region(ireg)%name, lum(ilum_db)%plant_cov,  &
                 region(ireg)%lum_ha(ilum), rnb_d(ireg)%lum(ilum)  !! nutrient bal
              if (pco%csvout == "y") then
                write (4415,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, region(ireg)%name, lum(ilum_db)%plant_cov, &
                    region(ireg)%lum_ha(ilum), rnb_d(ireg)%lum(ilum)  !! nutrient bal
              end if
          end if
          if (pco%ls_hru%d == "y") then
            write (4416,102) time%day, time%mo, time%day_mo, time%yrc, region(ireg)%name, lum(ilum_db)%plant_cov,  &
                 region(ireg)%lum_ha(ilum), rls_d(ireg)%lum(ilum)  !! losses
              if (pco%csvout == "y") then
                write (4417,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, region(ireg)%name, lum(ilum_db)%plant_cov, &
                    region(ireg)%lum_ha(ilum), rls_d(ireg)%lum(ilum)  !! losses
              end if
          end if
          if (pco%pw_hru%d == "y") then
            write (4418,101) time%day, time%mo, time%day_mo, time%yrc, region(ireg)%name, lum(ilum_db)%plant_cov,  &
                 region(ireg)%lum_ha(ilum), rpw_d(ireg)%lum(ilum)  !! plant weather 
              if (pco%csvout == "y") then 
                write (4419,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, region(ireg)%name, lum(ilum_db)%plant_cov, &
                    region(ireg)%lum_ha(ilum), rpw_d(ireg)%lum(ilum)  !! plant weather
              end if 
          end if
        end if

        !! check end of month
        if (time%end_mo == 1) then
          const = float (ndays(time%mo + 1) - ndays(time%mo))
          rpw_m(ireg)%lum(ilum) = rpw_m(ireg)%lum(ilum) // const
          !rwb_m(ireg)%lum(ilum) = rwb_m(ireg)%lum(ilum) // const
          rwb_m(ireg)%lum(ilum)%cn = rwb_m(ireg)%lum(ilum)%cn / const 
          rwb_m(ireg)%lum(ilum)%sw = rwb_m(ireg)%lum(ilum)%sw / const
          rwb_m(ireg)%lum(ilum)%sw_300 = rwb_m(ireg)%lum(ilum)%sw_300 / const
          rwb_y(ireg)%lum(ilum) = rwb_y(ireg)%lum(ilum) + rwb_m(ireg)%lum(ilum)
          rnb_y(ireg)%lum(ilum) = rnb_y(ireg)%lum(ilum) + rnb_m(ireg)%lum(ilum)
          rls_y(ireg)%lum(ilum) = rls_y(ireg)%lum(ilum) + rls_m(ireg)%lum(ilum)
          rpw_y(ireg)%lum(ilum) = rpw_y(ireg)%lum(ilum) + rpw_m(ireg)%lum(ilum)
          
          !! monthly print
           if (pco%wb_hru%m == "y") then
             write (4412,100) time%day, time%mo, time%day_mo, time%yrc, region(ireg)%name, lum(ilum_db)%plant_cov,  &
                 region(ireg)%lum_ha(ilum), rwb_m(ireg)%lum(ilum)
               if (pco%csvout == "y") then
                 write (4413,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, region(ireg)%name, lum(ilum_db)%plant_cov,&
                    region(ireg)%lum_ha(ilum), rwb_m(ireg)%lum(ilum)
               end if
           end if
           if (pco%nb_hru%m == "y") then
             write (4414,100) time%day, time%mo, time%day_mo, time%yrc, region(ireg)%name, lum(ilum_db)%plant_cov,  &
                 region(ireg)%lum_ha(ilum), rnb_m(ireg)%lum(ilum)
               if (pco%csvout == "y") then
                 write (4415,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, region(ireg)%name, lum(ilum_db)%plant_cov,&
                    region(ireg)%lum_ha(ilum), rnb_m(ireg)%lum(ilum)
               end if
           end if
           if (pco%ls_hru%m == "y") then
             write (4416,102) time%day, time%mo, time%day_mo, time%yrc, region(ireg)%name, lum(ilum_db)%plant_cov,  &
                 region(ireg)%lum_ha(ilum), rls_m(ireg)%lum(ilum)
               if (pco%csvout == "y") then 
                 write (4417,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, region(ireg)%name, lum(ilum_db)%plant_cov,&
                    region(ireg)%lum_ha(ilum), rls_m(ireg)%lum(ilum)
               end if
           end if
           if (pco%pw_hru%m == "y") then
             write (4418,101) time%day, time%mo, time%day_mo, time%yrc, region(ireg)%name, lum(ilum_db)%plant_cov,  &
                 region(ireg)%lum_ha(ilum), rpw_m(ireg)%lum(ilum)
               if (pco%csvout == "y") then 
                 write (4419,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, region(ireg)%name, lum(ilum_db)%plant_cov,&
                    region(ireg)%lum_ha(ilum), rpw_m(ireg)%lum(ilum)
               end if 
           end if
          
          rwb_m(ireg)%lum(ilum) = hwbz
          rnb_m(ireg)%lum(ilum) = hnbz
          rpw_m(ireg)%lum(ilum) = hpwz
          rls_m(ireg)%lum(ilum) = hlsz
        end if
        
        !! check end of year
        if (time%end_yr == 1) then
          rpw_y(ireg)%lum(ilum) = rpw_y(ireg)%lum(ilum) // 12.
          !rwb_y(ireg)%lum(ilum) = rwb_y(ireg)%lum(ilum) // 12.
          rwb_y(ireg)%lum(ilum)%cn = rwb_y(ireg)%lum(ilum)%cn / 12. 
          rwb_y(ireg)%lum(ilum)%sw = rwb_y(ireg)%lum(ilum)%sw / 12.
          rwb_y(ireg)%lum(ilum)%sw_300 = rwb_y(ireg)%lum(ilum)%sw_300 / 12.
          constwb = 1. / (10. * region(ireg)%lum_ha(ilum))              !10.*mm*ha=m3
          constnb = 1. / (region(ireg)%lum_ha(ilum))                    !kg/ha*ha=kg
          constpw = region(ireg)%area_ha / region(ireg)%lum_ha(ilum)    !weighted ave fro non-dimensional and weather
          ilum_db = region(ireg)%lum_num(ilum)                          !lum database number
          region(ireg)%lum_ha_tot(ilum_db) = region(ireg)%lum_ha_tot(ilum_db) + region(ireg)%lum_ha(ilum)
          rwb_a(ireg)%lum(ilum_db) = rwb_a(ireg)%lum(ilum_db) + rwb_y(ireg)%lum(ilum) / constwb
          rnb_a(ireg)%lum(ilum_db) = rnb_a(ireg)%lum(ilum_db) + rnb_y(ireg)%lum(ilum) / constnb
          rls_a(ireg)%lum(ilum_db) = rls_a(ireg)%lum(ilum_db) + rls_y(ireg)%lum(ilum) / constnb
          rpw_a(ireg)%lum(ilum_db) = rpw_a(ireg)%lum(ilum_db) + rpw_y(ireg)%lum(ilum) / constpw
          
          !! yearly print
           if (time%end_yr == 1 .and. pco%wb_hru%y == "y") then
             write (4412,100) time%day, time%mo, time%day_mo, time%yrc, region(ireg)%name, lum(ilum_db)%plant_cov,  &
                 region(ireg)%lum_ha(ilum), rwb_y(ireg)%lum(ilum)
               if (pco%csvout == "y") then
                 write (4413,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, region(ireg)%name, lum(ilum_db)%plant_cov,&
                    region(ireg)%lum_ha(ilum), rwb_y(ireg)%lum(ilum)
               end if
           end if
           if (time%end_yr == 1 .and. pco%nb_hru%y == "y") then
             write (4414,100) time%day, time%mo, time%day_mo, time%yrc, region(ireg)%name, lum(ilum_db)%plant_cov,  &
                 region(ireg)%lum_ha(ilum), rnb_y(ireg)%lum(ilum)
               if (pco%csvout == "y") then
                 write (4415,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, region(ireg)%name, lum(ilum_db)%plant_cov,&
                    region(ireg)%lum_ha(ilum), rnb_y(ireg)%lum(ilum)
               end if
           end if
           if (time%end_yr == 1 .and. pco%ls_hru%y == "y") then
             write (4416,102) time%day, time%mo, time%day_mo, time%yrc, region(ireg)%name, lum(ilum_db)%plant_cov,  &
                 region(ireg)%lum_ha(ilum), rls_y(ireg)%lum(ilum)
               if (pco%csvout == "y") then
                 write (4417,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, region(ireg)%name, lum(ilum_db)%plant_cov,&
                    region(ireg)%lum_ha(ilum), rls_y(ireg)%lum(ilum)
               end if
           end if
           if (time%end_yr == 1 .and. pco%pw_hru%y == "y") then
             write (4418,101) time%day, time%mo, time%day_mo, time%yrc, region(ireg)%name, lum(ilum_db)%plant_cov,  &
                 region(ireg)%lum_ha(ilum), rpw_y(ireg)%lum(ilum)
               if (pco%csvout == "y") then 
                 write (4419,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, region(ireg)%name, lum(ilum_db)%plant_cov,&
                    region(ireg)%lum_ha(ilum), rpw_y(ireg)%lum(ilum)
               end if 
           end if
                 
          ! deallocate output variables except average annual
          deallocate (rwb_d(ireg)%lum); deallocate (rwb_m(ireg)%lum); deallocate (rwb_y(ireg)%lum)
          deallocate (rnb_d(ireg)%lum); deallocate (rnb_m(ireg)%lum); deallocate (rnb_y(ireg)%lum)
          deallocate (rls_d(ireg)%lum); deallocate (rls_m(ireg)%lum); deallocate (rls_y(ireg)%lum)
          deallocate (rpw_d(ireg)%lum); deallocate (rpw_m(ireg)%lum); deallocate (rpw_y(ireg)%lum)
           
        end if
        end do      ! region(ireg)%nlum
        
!!!!! average annual print
        if (time%end_aa_prt == 1) then
          ! determine the number of land uses within each region
          iarea = 0.
          do ilum = i, db_mx%landuse
            if (region(ireg)%lum_ha_tot(ilum) > 1.e-6) then
              iarea(ilum) = 1       !track all lums in the lcu for all years
            end if
          end do

          nlum = sum(iarea)     !number of different lums in the lcu
          region(icu)%nlum = nlum

          !set the lum number from the lum database -sequential for the region
          nlum = 1
          do ilum = 1, db_mx%landuse
            if (iarea(ilum) == 1) then
              region(icu)%lum_num_tot(nlum) = ilum
              nlum = nlum + 1
            end if
          end do      !ilum
        end if     !time%end_aa_prt == 1

        do ilum = 1, region(icu)%nlum
         ilum_db = region(ireg)%lum_num_tot(ilum)
         region(ireg)%lum_ha_tot(ilum)  = region(ireg)%lum_ha_tot(ilum) / time%yrs_prt_int
         if (time%end_aa_prt == 1 .and. pco%wb_hru%a == "y") then
           rwb_a(ireg)%lum(ilum) = rwb_a(ireg)%lum(ilum) / time%yrs_prt_int
           write (4420,100) time%day, time%mo, time%day_mo, time%yrc, region(ireg)%name, lum(ilum_db)%plant_cov,     &
              region(ireg)%lum_ha_tot(ilum), rwb_a(ireg)%lum(ilum)
           if (pco%csvout == "y") then
             write (4421,100) time%day, time%mo, time%day_mo, time%yrc, region(ireg)%name, lum(ilum_db)%plant_cov,    &
              region(ireg)%lum_ha_tot(ilum), rwb_a(ireg)%lum(ilum)
           end if
           rwb_a(ireg)%lum(ilum) = hwbz
         end if
         
         if (time%end_aa_prt == 1 .and. pco%nb_hru%a == "y") then 
           rnb_a(ireg)%lum(ilum) = rnb_a(ireg)%lum(ilum) / time%yrs_prt_int
           write (4422,100) time%day, time%mo, time%day_mo, time%yrc, region(ireg)%name, lum(ilum_db)%plant_cov,    &
              region(ireg)%lum_ha_tot(ilum), rnb_a(ireg)%lum(ilum)
             if (pco%csvout == "y") then 
               write (4423,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, region(ireg)%name, lum(ilum_db)%plant_cov, &
                region(ireg)%lum_ha_tot(ilum), rnb_a(ireg)%lum(ilum)
             end if
             rnb_a(ireg)%lum(ilum) = hnbz
         end if
        
         if (time%end_aa_prt == 1 .and. pco%ls_hru%a == "y") then
           rls_a(ireg)%lum(ilum) = rls_a(ireg)%lum(ilum) / time%yrs_prt_int 
           write (4424,101) time%day, time%mo, time%day_mo, time%yrc, region(ireg)%name, lum(ilum_db)%plant_cov,    &
              region(ireg)%lum_ha_tot(ilum), rls_a(ireg)%lum(ilum)
             if (pco%csvout == "y") then 
               write (4425,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, region(ireg)%name, lum(ilum_db)%plant_cov, &
                region(ireg)%lum_ha_tot(ilum), rls_a(ireg)%lum(ilum)
             end if
             rls_a(ireg)%lum(ilum) = hlsz
         end if
        
         if (time%end_aa_prt == 1 .and. pco%pw_hru%a == "y") then     
           rpw_a(ireg)%lum(ilum) = rpw_a(ireg)%lum(ilum) / time%yrs_prt_int      
           write (4426,102) time%day, time%mo, time%day_mo, time%yrc, region(ireg)%name, lum(ilum_db)%plant_cov,   &
              region(ireg)%lum_ha_tot(ilum), rpw_a(ireg)%lum(ilum)
             if (pco%csvout == "y") then 
               write (4427,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, region(ireg)%name, lum(ilum_db)%plant_cov, &
                region(ireg)%lum_ha_tot(ilum), rpw_a(ireg)%lum(ilum)
             end if
             rpw_a(ireg)%lum(ilum) = hpwz
         end if

        end do      ! region(icu)%nlum
      end do        ! db_mx%lsu_out
 
      deallocate (iarea)
      return
      
100   format (4i6,2a16,22f12.3)
101   format (4i6,2a16,24f12.3)
102   format (4i6,2a16,24f12.3)
103   format (4i6,i8,4x,a,5x,f12.3)
       
      end subroutine lsreg_output