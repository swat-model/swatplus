      subroutine hcsout_output
    
      use hydrograph_module
      use time_module
      use constituent_mass_module

      implicit none
      
      integer :: iiout            !none          |counter
      integer :: ipest            !none          |pesticide counter
      integer :: ipath            !none          |pathogen counter
      integer :: imetal           !none          |heavy metal counter
      integer :: isalt            !none          |salt counter
      integer :: iob              !none          |object counter
           
      do iob = 1, sp_ob%objs
        do iiout = 1, ob(iob)%src_tot
        !! daily print
         if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
          if (pco%hyd%d == "y") then
            if (cs_db%num_pests > 0) then        !! pests
              write (2740,*) time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,    &
               ob(iob)%num, ob(iob)%obtyp_out(iiout), ob(iob)%obtypno_out(iiout), ob(iob)%htyp_out(iiout),  &
               ob(iob)%frac_out(iiout), (hcs1%pest(ipest), ipest = 1, cs_db%num_pests)          
              if (pco%csvout == "y") then
                write (2756,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,  & 
                  ob(iob)%num, ob(iob)%obtyp_out(iiout), ob(iob)%obtypno_out(iiout), ob(iob)%htyp_out(iiout),              &
                  ob(iob)%frac_out(iiout), (hcs1%pest(ipest), ipest = 1, cs_db%num_pests)
              end if                             !! cvs pests
            end if                               !! pests
              
              if (cs_db%num_paths > 0) then      !! paths
                write (2744,*) time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,   &
                 ob(iob)%num, ob(iob)%obtyp_out(iiout), ob(iob)%obtypno_out(iiout), ob(iob)%htyp_out(iiout), &
                 ob(iob)%frac_out(iiout), (hcs1%path(ipath), ipath = 1, cs_db%num_paths)          
              if (pco%csvout == "y") then
                write (2760,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,  & 
				 ob(iob)%num, ob(iob)%obtyp_out(iiout), ob(iob)%obtypno_out(iiout), ob(iob)%htyp_out(iiout),               &
                 ob(iob)%frac_out(iiout), (hcs1%path(ipath), ipath = 1, cs_db%num_paths)
              end if                            !! cvs paths
              end if                            !! paths
              
              if (cs_db%num_metals > 0) then    !! metals
                write (2748,*) time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,   &
				 ob(iob)%num, ob(iob)%obtyp_out(iiout), ob(iob)%obtypno_out(iiout), ob(iob)%htyp_out(iiout), &
                 ob(iob)%frac_out(iiout), (hcs1%hmet(imetal), imetal = 1, cs_db%num_metals)          
              if (pco%csvout == "y") then
                write (2764,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,  &
				 ob(iob)%num, ob(iob)%obtyp_out(iiout), ob(iob)%obtypno_out(iiout), ob(iob)%htyp_out(iiout),               &
                 ob(iob)%frac_out(iiout), (hcs1%hmet(imetal), imetal = 1, cs_db%num_metals)
              end if                            !! cvs metals
              end if                            !! metals
              
              if (cs_db%num_salts > 0) then     !! salts
                write (2752,*) time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,    &
				 ob(iob)%num, ob(iob)%obtyp_out(iiout), ob(iob)%obtypno_out(iiout), ob(iob)%htyp_out(iiout),  &
                 ob(iob)%frac_out(iiout), (hcs1%salt(isalt), isalt = 1, cs_db%num_salts)          
              if (pco%csvout == "y") then
                write (2768,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,  &
				 ob(iob)%num, ob(iob)%obtyp_out(iiout), ob(iob)%obtypno_out(iiout), ob(iob)%htyp_out(iiout),               &
                 ob(iob)%frac_out(iiout), (hcs1%salt(isalt), isalt = 1, cs_db%num_salts)
              end if                            !! cvs salts
              end if                            !! salts
            end if       
          end if
                                                    
        obcs(iob)%hcsout_m(iiout) = obcs(iob)%hcsout_m(iiout) + hcs1

        !! monthly print
        if (time%end_mo == 1) then
          if (pco%hyd%m == "y") then
              if (cs_db%num_pests > 0) then        !! pests
                write (2741,*) time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,     &
				 ob(iob)%num, ob(iob)%obtyp_out(iiout), ob(iob)%obtypno_out(iiout), ob(iob)%htyp_out(iiout),   &
                 ob(iob)%frac_out(iiout), (obcs(iob)%hcsout_m(iiout)%pest(ipest), ipest = 1, cs_db%num_pests)          
              if (pco%csvout == "y") then
                write (2757,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,  &
				 ob(iob)%num, ob(iob)%obtyp_out(iiout), ob(iob)%obtypno_out(iiout), ob(iob)%htyp_out(iiout),               &
                 ob(iob)%frac_out(iiout), (obcs(iob)%hcsout_m(iiout)%pest(ipest), ipest = 1, cs_db%num_pests)
              end if                             !! cvs pests
            end if                               !! pests
              
              if (cs_db%num_paths > 0) then      !! paths
                write (2745,*) time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,     &
				 ob(iob)%num, ob(iob)%obtyp_out(iiout), ob(iob)%obtypno_out(iiout), ob(iob)%htyp_out(iiout),   &
                 ob(iob)%frac_out(iiout), (obcs(iob)%hcsout_m(iiout)%path(ipath), ipath = 1, cs_db%num_paths)          
              if (pco%csvout == "y") then
                write (2761,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,   &
				 ob(iob)%num, ob(iob)%obtyp_out(iiout), ob(iob)%obtypno_out(iiout), ob(iob)%htyp_out(iiout),                &
                 ob(iob)%frac_out(iiout), (obcs(iob)%hcsout_m(iiout)%path(ipath), ipath = 1, cs_db%num_paths)
              end if                            !! cvs paths
              end if                            !! paths
              
              if (cs_db%num_metals > 0) then    !! metals
                write (2749,*) time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,       &
				 ob(iob)%num, ob(iob)%obtyp_out(iiout), ob(iob)%obtypno_out(iiout), ob(iob)%htyp_out(iiout),     &
                 ob(iob)%frac_out(iiout), (obcs(iob)%hcsout_m(iiout)%hmet(imetal), imetal = 1, cs_db%num_metals)          
              if (pco%csvout == "y") then
                write (2765,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,   &
				 ob(iob)%num, ob(iob)%obtyp_out(iiout), ob(iob)%obtypno_out(iiout), ob(iob)%htyp_out(iiout),                &
                 ob(iob)%frac_out(iiout), (obcs(iob)%hcsout_m(iiout)%hmet(imetal), imetal = 1, cs_db%num_metals)
              end if                            !! cvs metals
              end if                            !! metals
              
              if (cs_db%num_salts > 0) then     !! salts
                write (2753,*) time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,    &
				ob(iob)%num, ob(iob)%obtyp_out(iiout), ob(iob)%obtypno_out(iiout), ob(iob)%htyp_out(iiout),   &
                ob(iob)%frac_out(iiout), (obcs(iob)%hcsout_m(iiout)%salt(isalt), isalt = 1, cs_db%num_salts)          
              if (pco%csvout == "y") then
                write (2769,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,   &
				 ob(iob)%num, ob(iob)%obtyp_out(iiout), ob(iob)%obtypno_out(iiout), ob(iob)%htyp_out(iiout),                &
                 ob(iob)%frac_out(iiout), (obcs(iob)%hcsout_m(iiout)%salt(isalt), isalt = 1, cs_db%num_salts)
              end if                            !! cvs salts
              end if                            !! salts
            end if
          end if
        
        obcs(iob)%hcsout_y(iiout) = obcs(iob)%hcsout_y(iiout) + obcs(iob)%hcsout_m(iiout)

        !! yearly print
        if (time%end_yr == 1) then
          if (pco%hyd%y == "y") then
            if (cs_db%num_pests > 0) then        !! pests
                write (2742,*) time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,     &
				 ob(iob)%num, ob(iob)%obtyp_out(iiout), ob(iob)%obtypno_out(iiout), ob(iob)%htyp_out(iiout),   &
                 ob(iob)%frac_out(iiout), (obcs(iob)%hcsout_y(iiout)%pest(ipest), ipest = 1, cs_db%num_pests)          
              if (pco%csvout == "y") then
                write (2752,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,   &
				 ob(iob)%num, ob(iob)%obtyp_out(iiout), ob(iob)%obtypno_out(iiout), ob(iob)%htyp_out(iiout),                &
                 ob(iob)%frac_out(iiout), (obcs(iob)%hcsout_y(iiout)%pest(ipest), ipest = 1, cs_db%num_pests)
              end if                             !! cvs pests
            end if                               !! pests
              
              if (cs_db%num_paths > 0) then      !! paths
                write (2746,*) time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,      &
				 ob(iob)%num, ob(iob)%obtyp_out(iiout), ob(iob)%obtypno_out(iiout), ob(iob)%htyp_out(iiout),    &
                 ob(iob)%frac_out(iiout), (obcs(iob)%hcsout_y(iiout)%path(ipath), ipath = 1, cs_db%num_paths)          
              if (pco%csvout == "y") then
                write (2762,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,   &
				 ob(iob)%num, ob(iob)%obtyp_out(iiout), ob(iob)%obtypno_out(iiout), ob(iob)%htyp_out(iiout),                &
                 ob(iob)%frac_out(iiout), (obcs(iob)%hcsout_y(iiout)%path(ipath), ipath = 1, cs_db%num_paths)
              end if                            !! cvs paths
              end if                            !! paths
              
              if (cs_db%num_metals > 0) then    !! metals
                write (2750,*) time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,      &
				 ob(iob)%num, ob(iob)%obtyp_out(iiout), ob(iob)%obtypno_out(iiout), ob(iob)%htyp_out(iiout),    &
                 ob(iob)%frac_out(iiout), (obcs(iob)%hcsout_y(iiout)%hmet(imetal), imetal = 1, cs_db%num_metals)          
              if (pco%csvout == "y") then
                write (2766,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,   &
				 ob(iob)%num, ob(iob)%obtyp_out(iiout), ob(iob)%obtypno_out(iiout), ob(iob)%htyp_out(iiout),                &
                 ob(iob)%frac_out(iiout), (obcs(iob)%hcsout_y(iiout)%hmet(imetal), imetal = 1, cs_db%num_metals)
              end if                            !! cvs metals
              end if                            !! metals
              
              if (cs_db%num_salts > 0) then     !! salts
                write (2754,*) time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,    &
				ob(iob)%num, ob(iob)%obtyp_out(iiout), ob(iob)%obtypno_out(iiout), ob(iob)%htyp_out(iiout),   &
                ob(iob)%frac_out(iiout), (obcs(iob)%hcsout_y(iiout)%salt(isalt), isalt = 1, cs_db%num_salts)          
              if (pco%csvout == "y") then
                write (2770,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,   &
				 ob(iob)%num, ob(iob)%obtyp_out(iiout), ob(iob)%obtypno_out(iiout), ob(iob)%htyp_out(iiout),                &
                 ob(iob)%frac_out(iiout), (obcs(iob)%hcsout_y(iiout)%salt(isalt), isalt = 1, cs_db%num_salts)
              end if                            !! cvs salts
              end if                            !! salts              
           end if
          end if
          
        obcs(iob)%hcsout_a(iiout) = obcs(iob)%hcsout_a(iiout) + obcs(iob)%hcsout_y(iiout)

        !! average annual print
        if (time%end_sim == 1 .and. pco%hyd%a == "y") then
          ob(iob)%hin_a(iiout) = ob(iob)%hin_a(iiout) / time%yrs_prt
            if (cs_db%num_pests > 0) then        !! pests
                write (2743,*) time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,     &
				 ob(iob)%num, ob(iob)%obtyp_out(iiout), ob(iob)%obtypno_out(iiout), ob(iob)%htyp_out(iiout),   &
                 ob(iob)%frac_out(iiout), (obcs(iob)%hcsout_a(iiout)%pest(ipest), ipest = 1, cs_db%num_pests)          
              if (pco%csvout == "y") then
                write (2759,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,    &
				 ob(iob)%num, ob(iob)%obtyp_out(iiout), ob(iob)%obtypno_out(iiout), ob(iob)%htyp_out(iiout),                 &
                 ob(iob)%frac_out(iiout), (obcs(iob)%hcsout_a(iiout)%pest(ipest), ipest = 1, cs_db%num_pests)
              end if                             !! cvs pests
            end if                               !! pests
              
              if (cs_db%num_paths > 0) then      !! paths
                write (2747,*) time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,    &
				 ob(iob)%num, ob(iob)%obtyp_out(iiout), ob(iob)%obtypno_out(iiout), ob(iob)%htyp_out(iiout),  &
                 ob(iob)%frac_out(iiout), (obcs(iob)%hcsout_a(iiout)%path(ipath), ipath = 1, cs_db%num_paths)          
              if (pco%csvout == "y") then
                write (2763,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,   &
				 ob(iob)%num, ob(iob)%obtyp_out(iiout), ob(iob)%obtypno_out(iiout), ob(iob)%htyp_out(iiout),                &
                 ob(iob)%frac_out(iiout), (obcs(iob)%hcsout_a(iiout)%path(ipath), ipath = 1, cs_db%num_paths)
              end if                            !! cvs paths
              end if                            !! paths
              
              if (cs_db%num_metals > 0) then    !! metals
                write (2751,*) time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,      &
				 ob(iob)%num, ob(iob)%obtyp_out(iiout), ob(iob)%obtypno_out(iiout), ob(iob)%htyp_out(iiout),    &
                 ob(iob)%frac_out(iiout), (obcs(iob)%hcsout_a(iiout)%hmet(imetal), imetal = 1, cs_db%num_metals)          
              if (pco%csvout == "y") then
                write (2767,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,   &
				 ob(iob)%num, ob(iob)%obtyp_out(iiout), ob(iob)%obtypno_out(iiout), ob(iob)%htyp_out(iiout),                &
                 ob(iob)%frac_out(iiout), (obcs(iob)%hcsout_a(iiout)%hmet(imetal), imetal = 1, cs_db%num_metals)
              end if                            !! cvs metals
              end if                            !! metals
              
              if (cs_db%num_salts > 0) then     !! salts
                write (2755,*) time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,    &
				 ob(iob)%num, ob(iob)%obtyp_out(iiout), ob(iob)%obtypno_out(iiout), ob(iob)%htyp_out(iiout),  &
                 ob(iob)%frac_out(iiout), (obcs(iob)%hcsout_a(iiout)%salt(isalt), isalt = 1, cs_db%num_salts)          
              if (pco%csvout == "y") then
                write (2771,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, iob, ob(iob)%gis_id, ob(iob)%typ,   &
				 ob(iob)%num, ob(iob)%obtyp_out(iiout), ob(iob)%obtypno_out(iiout), ob(iob)%htyp_out(iiout),                &
                 ob(iob)%frac_out(iiout), (obcs(iob)%hcsout_a(iiout)%salt(isalt), isalt = 1, cs_db%num_salts)
              end if                            !! cvs salts
              end if                            !! salts              
        end if
        
        end do   !! sp_ob%objs
      end do     !! ob(iob)%rcv_tot 
      
      end subroutine hcsout_output