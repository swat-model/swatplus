      subroutine hcsin_output
    
      use hydrograph_module
      use time_module
      use constituent_mass_module

      implicit none
      
      integer :: iin              !none          |counter
      integer :: ipest            !none          |pesticide counter
      integer :: ipath            !none          |pathogen counter
      integer :: imetal           !none          |heavy metal counter
      integer :: isalt            !none          |salt counter
      integer :: iob              !none          |object counter
           
      do iob = 1, sp_ob%objs
        do iin = 1, ob(iob)%rcv_tot
        !! daily print
         if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
          if (pco%hyd%d == "y") then
            if (cs_db%num_pests > 0) then        !! pests
            write (2708,*) time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,    &
              ob(iob)%num, ob(iob)%obtyp_in(iin), ob(iob)%obtypno_in(iin), ob(iob)%htyp_in(iin),          &
             ob(iob)%frac_in(iin), (obcs(iob)%hcsin_d(iin)%pest(ipest), ipest = 1, cs_db%num_pests)          
              if (pco%csvout == "y") then
                write (2724,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,   &
                  ob(iob)%num, ob(iob)%obtyp_in(iin), ob(iob)%obtypno_in(iin), ob(iob)%htyp_in(iin),                        &
                  ob(iob)%frac_in(iin), (obcs(iob)%hcsin_d(iin)%pest(ipest), ipest = 1, cs_db%num_pests)
              end if                             !! cvs pests
            end if                               !! pests
              
              if (cs_db%num_paths > 0) then      !! paths
                write (2712,*) time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,    &
                  ob(iob)%num, ob(iob)%obtyp_in(iin), ob(iob)%obtypno_in(iin), ob(iob)%htyp_in(iin),          &
                  ob(iob)%frac_in(iin), (obcs(iob)%hcsin_d(iin)%path(ipath), ipath = 1, cs_db%num_paths)          
              if (pco%csvout == "y") then
                write (2728,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,   &
                  ob(iob)%num, ob(iob)%obtyp_in(iin), ob(iob)%obtypno_in(iin), ob(iob)%htyp_in(iin),                        &
                  ob(iob)%frac_in(iin), (obcs(iob)%hcsin_d(iin)%path(ipath), ipath = 1, cs_db%num_paths)
              end if                            !! cvs paths
              end if                            !! paths
              
              if (cs_db%num_metals > 0) then    !! metals
                write (2716,*) time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,    &
                  ob(iob)%num, ob(iob)%obtyp_in(iin), ob(iob)%obtypno_in(iin), ob(iob)%htyp_in(iin),          &
                  ob(iob)%frac_in(iin), (obcs(iob)%hcsin_d(iin)%hmet(imetal), imetal = 1, cs_db%num_metals)          
              if (pco%csvout == "y") then
                write (2732,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,   &
                  ob(iob)%num, ob(iob)%obtyp_in(iin), ob(iob)%obtypno_in(iin), ob(iob)%htyp_in(iin),                        &
                  ob(iob)%frac_in(iin), (obcs(iob)%hcsin_d(iin)%hmet(imetal), imetal = 1, cs_db%num_metals)
              end if                            !! cvs metals
              end if                            !! metals
              
              if (cs_db%num_salts > 0) then     !! salts
                write (2720,*) time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,   &
                  ob(iob)%num, ob(iob)%obtyp_in(iin), ob(iob)%obtypno_in(iin), ob(iob)%htyp_in(iin),         &
                  ob(iob)%frac_in(iin), (obcs(iob)%hcsin_d(iin)%salt(isalt), isalt = 1, cs_db%num_salts)          
              if (pco%csvout == "y") then
                write (2736,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,   &
                  ob(iob)%num, ob(iob)%obtyp_in(iin), ob(iob)%obtypno_in(iin), ob(iob)%htyp_in(iin),                        &
                  ob(iob)%frac_in(iin), (obcs(iob)%hcsin_d(iin)%salt(isalt), isalt = 1, cs_db%num_salts)
              end if                            !! cvs salts
              end if                            !! salts
            end if       
          end if
                                                    
        obcs(iob)%hcsin_m(iin) = obcs(iob)%hcsin_m(iin) + obcs(iob)%hcsin_d(iin)

        !! monthly print
        if (time%end_mo == 1) then
          if (pco%hyd%m == "y") then
              if (cs_db%num_pests > 0) then        !! pests
                write (2709,*) time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,   &
                  ob(iob)%num, ob(iob)%obtyp_in(iin), ob(iob)%obtypno_in(iin), ob(iob)%htyp_in(iin),         &
                  ob(iob)%frac_in(iin), (obcs(iob)%hcsin_m(iin)%pest(ipest), ipest = 1, cs_db%num_pests)          
              if (pco%csvout == "y") then
                write (2725,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,   &
                  ob(iob)%num, ob(iob)%obtyp_in(iin), ob(iob)%obtypno_in(iin), ob(iob)%htyp_in(iin),                        &
                  ob(iob)%frac_in(iin), (obcs(iob)%hcsin_m(iin)%pest(ipest), ipest = 1, cs_db%num_pests)
              end if                             !! cvs pests
            end if                               !! pests
              
              if (cs_db%num_paths > 0) then      !! paths
                write (2713,*) time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,   &
                  ob(iob)%num, ob(iob)%obtyp_in(iin), ob(iob)%obtypno_in(iin), ob(iob)%htyp_in(iin),         &
                 ob(iob)%frac_in(iin), (obcs(iob)%hcsin_m(iin)%path(ipath), ipath = 1, cs_db%num_paths)          
              if (pco%csvout == "y") then
                write (2729,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,   &
                  ob(iob)%num, ob(iob)%obtyp_in(iin), ob(iob)%obtypno_in(iin), ob(iob)%htyp_in(iin),                        &
                  ob(iob)%frac_in(iin), (obcs(iob)%hcsin_m(iin)%path(ipath), ipath = 1, cs_db%num_paths)
              end if                            !! cvs paths
              end if                            !! paths
              
              if (cs_db%num_metals > 0) then    !! metals
                write (2717,*) time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,   &
                  ob(iob)%num, ob(iob)%obtyp_in(iin), ob(iob)%obtypno_in(iin), ob(iob)%htyp_in(iin),         &
                  ob(iob)%frac_in(iin), (obcs(iob)%hcsin_m(iin)%hmet(imetal), imetal = 1, cs_db%num_metals)          
              if (pco%csvout == "y") then
                write (2733,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,   &
                  ob(iob)%num, ob(iob)%obtyp_in(iin), ob(iob)%obtypno_in(iin), ob(iob)%htyp_in(iin),                        &
                  ob(iob)%frac_in(iin), (obcs(iob)%hcsin_m(iin)%hmet(imetal), imetal = 1, cs_db%num_metals)
              end if                            !! cvs metals
              end if                            !! metals
              
              if (cs_db%num_salts > 0) then     !! salts
                write (2721,*) time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,   &
                  ob(iob)%num, ob(iob)%obtyp_in(iin), ob(iob)%obtypno_in(iin), ob(iob)%htyp_in(iin),         &
                  ob(iob)%frac_in(iin), (obcs(iob)%hcsin_m(iin)%salt(isalt), isalt = 1, cs_db%num_salts)          
              if (pco%csvout == "y") then
                write (2737,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,   &
                  ob(iob)%num, ob(iob)%obtyp_in(iin), ob(iob)%obtypno_in(iin), ob(iob)%htyp_in(iin),                        &
                  ob(iob)%frac_in(iin), (obcs(iob)%hcsin_m(iin)%salt(isalt), isalt = 1, cs_db%num_salts)
              end if                            !! cvs salts
              end if                            !! salts
            end if
          end if
        
        obcs(iob)%hcsin_y(iin) = obcs(iob)%hcsin_y(iin) + obcs(iob)%hcsin_m(iin)

        !! yearly print
        if (time%end_yr == 1) then
          if (pco%hyd%y == "y") then
            if (cs_db%num_pests > 0) then        !! pests
                write (2710,*) time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,   &
                  ob(iob)%num, ob(iob)%obtyp_in(iin), ob(iob)%obtypno_in(iin), ob(iob)%htyp_in(iin),         &
                  ob(iob)%frac_in(iin), (obcs(iob)%hcsin_y(iin)%pest(ipest), ipest = 1, cs_db%num_pests)          
              if (pco%csvout == "y") then
                write (2726,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,   &
                   ob(iob)%num, ob(iob)%obtyp_in(iin), ob(iob)%obtypno_in(iin), ob(iob)%htyp_in(iin),                       &
                   ob(iob)%frac_in(iin), (obcs(iob)%hcsin_y(iin)%pest(ipest), ipest = 1, cs_db%num_pests)
              end if                             !! cvs pests
            end if                               !! pests
              
              if (cs_db%num_paths > 0) then      !! paths
                write (2714,*) time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,   &
                  ob(iob)%num, ob(iob)%obtyp_in(iin), ob(iob)%obtypno_in(iin), ob(iob)%htyp_in(iin),         &
                  ob(iob)%frac_in(iin), (obcs(iob)%hcsin_y(iin)%path(ipath), ipath = 1, cs_db%num_paths)          
              if (pco%csvout == "y") then
                write (2730,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,  &
                  ob(iob)%num, ob(iob)%obtyp_in(iin), ob(iob)%obtypno_in(iin), ob(iob)%htyp_in(iin),                       &
                  ob(iob)%frac_in(iin), (obcs(iob)%hcsin_y(iin)%path(ipath), ipath = 1, cs_db%num_paths)
              end if                            !! cvs paths
              end if                            !! paths
              
              if (cs_db%num_metals > 0) then    !! metals
                write (2718,*) time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,   &
                  ob(iob)%num, ob(iob)%obtyp_in(iin), ob(iob)%obtypno_in(iin), ob(iob)%htyp_in(iin),         &
                  ob(iob)%frac_in(iin), (obcs(iob)%hcsin_y(iin)%hmet(imetal), imetal = 1, cs_db%num_metals)          
              if (pco%csvout == "y") then
                write (2734,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,   &
                  ob(iob)%num, ob(iob)%obtyp_in(iin), ob(iob)%obtypno_in(iin), ob(iob)%htyp_in(iin),                        &
                  ob(iob)%frac_in(iin), (obcs(iob)%hcsin_y(iin)%hmet(imetal), imetal = 1, cs_db%num_metals)
              end if                            !! cvs metals
              end if                            !! metals
              
              if (cs_db%num_salts > 0) then     !! salts
                write (2722,*) time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,   &
                  ob(iob)%num, ob(iob)%obtyp_in(iin), ob(iob)%obtypno_in(iin), ob(iob)%htyp_in(iin),         &
                  ob(iob)%frac_in(iin), (obcs(iob)%hcsin_y(iin)%salt(isalt), isalt = 1, cs_db%num_salts)          
              if (pco%csvout == "y") then
                write (2738,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,   &
                  ob(iob)%num, ob(iob)%obtyp_in(iin), ob(iob)%obtypno_in(iin), ob(iob)%htyp_in(iin),                        &
                  ob(iob)%frac_in(iin), (obcs(iob)%hcsin_y(iin)%salt(isalt), isalt = 1, cs_db%num_salts)
              end if                            !! cvs salts
              end if                            !! salts              
           end if
          end if
          
        obcs(iob)%hcsin_a(iin) = obcs(iob)%hcsin_a(iin) + obcs(iob)%hcsin_y(iin)

        !! average annual print
        if (time%end_sim == 1 .and. pco%hyd%a == "y") then
          ob(iob)%hin_a(iin) = ob(iob)%hin_a(iin) / time%yrs_prt
            if (cs_db%num_pests > 0) then        !! pests
                write (2711,*) time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,   &
                  ob(iob)%num, ob(iob)%obtyp_in(iin), ob(iob)%obtypno_in(iin), ob(iob)%htyp_in(iin),         &
                  ob(iob)%frac_in(iin), (obcs(iob)%hcsin_a(iin)%pest(ipest), ipest = 1, cs_db%num_pests)          
              if (pco%csvout == "y") then
                write (2727,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,   &
                  ob(iob)%num, ob(iob)%obtyp_in(iin), ob(iob)%obtypno_in(iin), ob(iob)%htyp_in(iin),                        &
                  ob(iob)%frac_in(iin), (obcs(iob)%hcsin_a(iin)%pest(ipest), ipest = 1, cs_db%num_pests)
              end if                             !! cvs pests
            end if                               !! pests
              
              if (cs_db%num_paths > 0) then      !! paths
                write (2715,*) time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,   &
                  ob(iob)%num, ob(iob)%obtyp_in(iin), ob(iob)%obtypno_in(iin), ob(iob)%htyp_in(iin),         &
                  ob(iob)%frac_in(iin), (obcs(iob)%hcsin_a(iin)%path(ipath), ipath = 1, cs_db%num_paths)          
              if (pco%csvout == "y") then
                write (2731,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,   &
                  ob(iob)%num,ob(iob)%obtyp_in(iin), ob(iob)%obtypno_in(iin), ob(iob)%htyp_in(iin),                         &
                  ob(iob)%frac_in(iin), (obcs(iob)%hcsin_a(iin)%path(ipath), ipath = 1, cs_db%num_paths)
              end if                            !! cvs paths
              end if                            !! paths
              
              if (cs_db%num_metals > 0) then    !! metals
                write (2719,*) time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,   &
                  ob(iob)%num, ob(iob)%obtyp_in(iin), ob(iob)%obtypno_in(iin), ob(iob)%htyp_in(iin),         &
                 ob(iob)%frac_in(iin), (obcs(iob)%hcsin_a(iin)%hmet(imetal), imetal = 1, cs_db%num_metals)          
              if (pco%csvout == "y") then
                write (2735,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id,ob(iob)%typ,   &
                  ob(iob)%num, ob(iob)%obtyp_in(iin), ob(iob)%obtypno_in(iin), ob(iob)%htyp_in(iin),                       &
                  ob(iob)%frac_in(iin), (obcs(iob)%hcsin_a(iin)%hmet(imetal), imetal = 1, cs_db%num_metals)
              end if                            !! cvs metals
              end if                            !! metals
              
              if (cs_db%num_salts > 0) then     !! salts
                write (2723,*) time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,   &
                  ob(iob)%num, ob(iob)%obtyp_in(iin), ob(iob)%obtypno_in(iin), ob(iob)%htyp_in(iin),         &
                  ob(iob)%frac_in(iin), (obcs(iob)%hcsin_a(iin)%salt(isalt), isalt = 1, cs_db%num_salts)          
              if (pco%csvout == "y") then
                write (2739,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,  &
                  ob(iob)%num, ob(iob)%obtyp_in(iin), ob(iob)%obtypno_in(iin), ob(iob)%htyp_in(iin),                       &
                  ob(iob)%frac_in(iin), (obcs(iob)%hcsin_a(iin)%salt(isalt), isalt = 1, cs_db%num_salts)
              end if                            !! cvs salts
              end if                            !! salts              
        end if
        
        end do   !! sp_ob%objs
      end do     !! ob(iob)%rcv_tot 
      
      end subroutine hcsin_output