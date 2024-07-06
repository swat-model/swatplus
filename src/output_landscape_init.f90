      subroutine output_landscape_init

      use hydrograph_module
      use channel_module
      use sd_channel_module
      use basin_module
      use maximum_data_module
      use calibration_data_module
      use aquifer_module
      use output_landscape_module
      use time_module
      implicit none 

      if (sp_ob%hru > 0) then
!!!  HRU - Water balance
        if (pco%wb_hru%d == "y") then
          open (2000,file="hru_wb_day.txt",recl = 1500)
          write (2000,*)  bsn%name, prog
          write (2000,*) wb_hdr  !! hru
          write (2000,*) wb_hdr_units
          write (9000,*) "HRU                       hru_wb_day.txt"
          !write (9000,*) "HRU                 waterbal_day_hru.txt"
            if (pco%csvout == "y") then
              open (2004,file="hru_wb_day.csv",recl = 1500)
              write (2004,*)  bsn%name, prog
              write (2004,'(*(G0.3,:,","))') wb_hdr  !! hru
              write (2004,'(*(G0.3,:,","))') wb_hdr_units
              write (9000,*) "HRU                       hru_wb_day.csv"
              !write (9000,*) "HRU                 waterbal_day_hru.csv"              
            end if 
        endif
 
        if (pco%wb_hru%m == "y") then 
          open (2001,file="hru_wb_mon.txt",recl = 1500)
          write (2001,*)  bsn%name, prog
          write (2001,*) wb_hdr   !! hru
          write (2001,*) wb_hdr_units
           write (9000,*) "HRU                       hru_wb_mon.txt"
          !write (9000,*) "HRU                 waterbal_mon_hru.txt"
          if (pco%csvout == "y") then
            open (2005,file="hru_wb_mon.csv",recl = 1500)
            write (2005,*)  bsn%name, prog
            write (2005,'(*(G0.3,:,","))') wb_hdr   !! hru
            write (2005,'(*(G0.3,:,","))') wb_hdr_units
            write (9000,*) "HRU                       hru_wb_mon.csv"
            !write (9000,*) "HRU                 waterbal_mon_hru.csv"
          end if
        end if 

        if (pco%wb_hru%y == "y") then
          open (2002,file="hru_wb_yr.txt",recl = 1500)
          write (2002,*)  bsn%name, prog
          write (2002,*) wb_hdr  !! hru
          write (2002,*) wb_hdr_units
          write (9000,*) "HRU                       hru_wb_yr.txt"
            if (pco%csvout == "y") then
              open (2006,file="hru_wb_yr.csv",recl = 1500)
              write (2006,*)  bsn%name, prog
              write (2006,'(*(G0.3,:,","))') wb_hdr  !! hru
              write (2006,'(*(G0.3,:,","))') wb_hdr_units
              write (9000,*) "HRU                       hru_wb_yr.csv"
              !write (9000,*) "HRU                 waterbal_yr_hru.csv"
            end if 
        endif
        
        if (pco%wb_hru%a == "y") then
          open (2003,file="hru_wb_aa.txt",recl = 1500)
          write (2003,*)  bsn%name, prog
          write (2003,*) wb_hdr   !! hru
          write (2003,*) wb_hdr_units
          write (9000,*) "HRU                       hru_wb_aa.txt"
          if (pco%csvout == "y") then
            open (2007,file="hru_wb_aa.csv",recl = 1500)
            write (2007,*)  bsn%name, prog
            write (2007,'(*(G0.3,:,","))') wb_hdr   !! hru
            write (2007,'(*(G0.3,:,","))') wb_hdr_units
            write (9000,*) "HRU                       hru_wb_aa.csv"
          end if
        end if 

!!!  HRU - Nutrient balance
        if (pco%nb_hru%d == "y") then
          open (2020,file="hru_nb_day.txt", recl = 1500)
          write (2020,*)  bsn%name, prog
          write (2020,*) nb_hdr
          write (2020,*) nb_hdr_units
          write (9000,*) "HRU                       hru_nb_day.txt"
            if (pco%csvout == "y") then
              open (2024,file="hru_nb_day.csv", recl = 1500)
              write (2024,*)  bsn%name, prog
              write (2024,'(*(G0.3,:,","))') nb_hdr
              write (2024,'(*(G0.3,:,","))') nb_hdr_units
              write (9000,*) "HRU                       hru_nb_day.csv"
            end if
        endif
        
!!!  HRU - NEW Nutrient cycling output
        if (pco%nb_hru%d == "y") then
          open (3333,file="hru_ncycle_day.txt", recl = 1500)
          write (3333,*)  bsn%name, prog
          write (3333,*) nb_hdr1
          write (3333,*) nb_hdr_units1
          write (9000,*) "HRU                       hru_ncycle_day.txt"
            if (pco%csvout == "y") then
              open (3334,file="hru_ncycle_day.csv", recl = 1500)
              write (3334,*)  bsn%name, prog
              write (3334,'(*(G0.3,:,","))') nb_hdr1
              write (3334,'(*(G0.3,:,","))') nb_hdr_units1
              write (9000,*) "HRU                       hru_ncycle_day.csv"
            end if
        endif
        
        if (pco%nb_hru%m == "y") then
          open (3335,file="hru_ncycle_mon.txt", recl = 1500)
          write (3335,*)  bsn%name, prog
          write (3335,*) nb_hdr1
          write (3335,*) nb_hdr_units1
          write (9000,*) "HRU                       hru_ncycle_mon.txt"
            if (pco%csvout == "y") then
              open (3336,file="hru_ncycle_mon.csv", recl = 1500)
              write (3336,*)  bsn%name, prog
              write (3336,'(*(G0.3,:,","))') nb_hdr1
              write (3336,'(*(G0.3,:,","))') nb_hdr_units1
              write (9000,*) "HRU                       hru_ncycle_mon.csv"
            end if
        endif
        
     if (pco%nb_hru%y == "y") then
          open (3337,file="hru_ncycle_yr.txt", recl = 1500)
          write (3337,*)  bsn%name, prog
          write (3337,*) nb_hdr1
          write (3337,*) nb_hdr_units1
          write (9000,*) "HRU                       hru_ncycle_yr.txt"
            if (pco%csvout == "y") then
              open (3338,file="hru_ncycle_yr.csv", recl = 1500)
              write (3338,*)  bsn%name, prog
              write (3338,'(*(G0.3,:,","))') nb_hdr1
              write (3338,'(*(G0.3,:,","))') nb_hdr_units1
              write (9000,*) "HRU                       hru_ncycle_yr.csv"
            end if
        endif
        
        if (pco%nb_hru%a == "y") then
          open (3339,file="hru_ncycle_aa.txt", recl = 1500)
          write (3339,*)  bsn%name, prog
          write (3339,*) nb_hdr1
          write (3339,*) nb_hdr_units1
          write (9000,*) "HRU                       hru_ncycle_aa.txt"
            if (pco%csvout == "y") then
              open (3340,file="hru_ncycle_aa.csv", recl = 1500)
              write (3340,*)  bsn%name, prog
              write (3340,'(*(G0.3,:,","))') nb_hdr1
              write (3340,'(*(G0.3,:,","))') nb_hdr_units1
              write (9000,*) "HRU                       hru_ncycle_aa.csv"
            end if
        endif
!!!  HRU - NEW Nutrient cycling output
        
       if (pco%nb_hru%m == "y") then
        open (2021,file="hru_nb_mon.txt", recl = 1500)
          write (2021,*) bsn%name, prog
          write (2021,*) nb_hdr
          write (2021,*) nb_hdr_units
          write (9000,*) "HRU                       hru_nb_mon.txt"
        if (pco%csvout == "y") then
          open (2025,file="hru_nb_mon.csv", recl = 1500)
          write (2025,*) bsn%name, prog
          write (2025,'(*(G0.3,:,","))') nb_hdr
          write (2025,'(*(G0.3,:,","))') nb_hdr_units
          write (9000,*) "HRU                       hru_nb_mon.csv"
        end if
       end if

        if (pco%nb_hru%y == "y") then
          open (2022,file="hru_nb_yr.txt", recl = 1500)
          write (2022,*) bsn%name, prog
          write (2022,*) nb_hdr
          write (2022,*) nb_hdr_units
          write (9000,*) "HRU                       hru_nb_yr.txt"
            if (pco%csvout == "y") then
              open (2026,file="hru_nb_yr.csv", recl = 1500)
              write (2026,*) bsn%name, prog
              write (2026,'(*(G0.3,:,","))') nb_hdr
              write (2026,'(*(G0.3,:,","))') nb_hdr_units
              write (9000,*) "HRU                       hru_nb_yr.csv" 
            end if
        endif
        
       if (pco%nb_hru%a == "y") then 
        open (2023,file="hru_nb_aa.txt", recl = 1500)
          write (2023,*) bsn%name, prog
          write (2023,*) nb_hdr
          write (2023,*) nb_hdr_units
          write (9000,*) "HRU                       hru_nb_aa.txt"
        if (pco%csvout == "y") then
          open (2027,file="hru_nb_aa.csv", recl = 1500)
          write (2027,*) bsn%name, prog
          write (2027,'(*(G0.3,:,","))') nb_hdr
          write (2027,'(*(G0.3,:,","))') nb_hdr_units
          write (9000,*) "HRU                       hru_nb_aa.csv"
        end if
       end if
               
 !!!NEW SOIL CARBON OUTPUT
        if (pco%nb_hru%d == "y") then
          open (4520,file="hru_soilcarb_day.txt", recl = 1500)
          write (4520,*)  bsn%name, prog
          write (4520,*) soilcarb_hdr
          write (4520,*) soilcarb_hdr_units
              write (9000,*) "HRU                       hru_soilcarb_day.txt"          
             if (pco%csvout == "y") then
              open (4524,file="hru_soilcarb_day.csv", recl = 1500)
              write (4524,*)  bsn%name, prog
              write (4524,'(*(G0.3,:,","))') soilcarb_hdr
              write (4524,'(*(G0.3,:,","))') soilcarb_hdr_units
              write (9000,*) "HRU                       hru_soilcarb_day.csv"
            end if
        endif
        
        if (pco%nb_hru%m == "y") then
          open (4521,file="hru_soilcarb_mon.txt", recl = 1500)
          write (4521,*)  bsn%name, prog
          write (4521,*) soilcarb_hdr
          write (4521,*) soilcarb_hdr_units
          write (9000,*) "HRU                       hru_soilcarb_mon.txt"
            if (pco%csvout == "y") then
              open (4525,file="hru_soilcarb_mon.csv", recl = 1500)
              write (4525,*)  bsn%name, prog
              write (4525,'(*(G0.3,:,","))') soilcarb_hdr
              write (4525,'(*(G0.3,:,","))') soilcarb_hdr_units
              write (9000,*) "HRU                       hru_soilcarb_mon.csv"
            end if
        endif
        
     if (pco%nb_hru%y == "y") then
          open (4522,file="hru_soilcarb_yr.txt", recl = 1500)
          write (4522,*)  bsn%name, prog
          write (4522,*) soilcarb_hdr
          write (4522,*) soilcarb_hdr_units
          write (9000,*) "HRU                       hru_soilcarb_yr.txt"
            if (pco%csvout == "y") then
              open (4526,file="hru_soilcarb_yr.csv", recl = 1500)
              write (4526,*)  bsn%name, prog
              write (4526,'(*(G0.3,:,","))') soilcarb_hdr
              write (4526,'(*(G0.3,:,","))') soilcarb_hdr_units
              write (9000,*) "HRU                       hru_soilcarb_yr.csv"
            end if
        endif
        
        if (pco%nb_hru%a == "y") then
          open (4523,file="hru_soilcarb_aa.txt", recl = 1500)
          write (4523,*)  bsn%name, prog
          write (4523,*) soilcarb_hdr
          write (4523,*) soilcarb_hdr_units
          write (9000,*) "HRU                       hru_soilcarb_aa.txt"
            if (pco%csvout == "y") then
              open (4527,file="hru_soilcarb_aa.csv", recl = 1500)
              write (4527,*)  bsn%name, prog
              write (4527,'(*(G0.3,:,","))') soilcarb_hdr
              write (4527,'(*(G0.3,:,","))') soilcarb_hdr_units
              write (9000,*) "HRU                       hru_soilcarb_aa.csv"
            end if
        endif
        
 !!!NEW SOIL CARBON OUTPUT     
              
!!!NEW RESIDUE CARBON OUTPUT
        if (pco%nb_hru%d == "y") then
          open (4530,file="hru_rescarb_day.txt", recl = 1500)
          write (4530,*)  bsn%name, prog
          write (4530,*) rescarb_hdr
          write (4530,*) rescarb_hdr_units
              write (9000,*) "HRU                       hru_rescarb_day.txt"          
             if (pco%csvout == "y") then
              open (4534,file="hru_rescarb_day.csv", recl = 1500)
              write (4534,*)  bsn%name, prog
              write (4534,'(*(G0.3,:,","))') rescarb_hdr
              write (4534,'(*(G0.3,:,","))') rescarb_hdr_units
              write (9000,*) "HRU                       hru_rescarb_day.csv"
            end if
        endif
        
        if (pco%nb_hru%m == "y") then
          open (4531,file="hru_rescarb_mon.txt", recl = 1500)
          write (4531,*)  bsn%name, prog
          write (4531,*) rescarb_hdr
          write (4531,*) rescarb_hdr_units
          write (9000,*) "HRU                       hru_rescarb_mon.txt"
            if (pco%csvout == "y") then
              open (4535,file="hru_rescarb_mon.csv", recl = 1500)
              write (4535,*)  bsn%name, prog
              write (4535,'(*(G0.3,:,","))') rescarb_hdr
              write (4535,'(*(G0.3,:,","))') rescarb_hdr_units
              write (9000,*) "HRU                       hru_rescarb_mon.csv"
            end if
        endif
        
     if (pco%nb_hru%y == "y") then
          open (4532,file="hru_rescarb_yr.txt", recl = 1500)
          write (4532,*)  bsn%name, prog
          write (4532,*) rescarb_hdr
          write (4532,*) rescarb_hdr_units
          write (9000,*) "HRU                       hru_rescarb_yr.txt"
            if (pco%csvout == "y") then
              open (4536,file="hru_rescarb_yr.csv", recl = 1500)
              write (4536,*)  bsn%name, prog
              write (4536,'(*(G0.3,:,","))') rescarb_hdr
              write (4536,'(*(G0.3,:,","))') rescarb_hdr_units
              write (9000,*) "HRU                       hru_rescarb_yr.csv"
            end if
        endif
        
        if (pco%nb_hru%a == "y") then
          open (4533,file="hru_rescarb_aa.txt", recl = 1500)
          write (4533,*)  bsn%name, prog
          write (4533,*) rescarb_hdr
          write (4533,*) rescarb_hdr_units
          write (9000,*) "HRU                       hru_rescarb_aa.txt"
            if (pco%csvout == "y") then
              open (4537,file="hru_rescarb_aa.csv", recl = 1500)
              write (4537,*)  bsn%name, prog
              write (4537,'(*(G0.3,:,","))') rescarb_hdr
              write (4537,'(*(G0.3,:,","))') rescarb_hdr_units
              write (9000,*) "HRU                       hru_rescarb_aa.csv"
            end if
        endif
        
 !!!NEW RESIDUE CARBON OUTPUT 
        
 !!!NEW PLANT CARBON OUTPUT
        if (pco%nb_hru%d == "y") then
          open (4540,file="hru_plcarb_day.txt", recl = 1500)
          write (4540,*)  bsn%name, prog
          write (4540,*) plcarb_hdr
          write (4540,*) plcarb_hdr_units
              write (9000,*) "HRU                       hru_plcarb_day.txt"          
             if (pco%csvout == "y") then
              open (4544,file="hru_plcarb_day.csv", recl = 1500)
              write (4544,*)  bsn%name, prog
              write (4544,'(*(G0.3,:,","))') plcarb_hdr
              write (4544,'(*(G0.3,:,","))') plcarb_hdr_units
              write (9000,*) "HRU                       hru_plcarb_day.csv"
            end if
        endif
        
        if (pco%nb_hru%m == "y") then
          open (4541,file="hru_plcarb_mon.txt", recl = 1500)
          write (4541,*)  bsn%name, prog
          write (4541,*) plcarb_hdr
          write (4541,*) plcarb_hdr_units
          write (9000,*) "HRU                       hru_plcarb_mon.txt"
            if (pco%csvout == "y") then
              open (4545,file="hru_plcarb_mon.csv", recl = 1500)
              write (4545,*)  bsn%name, prog
              write (4545,'(*(G0.3,:,","))') plcarb_hdr
              write (4545,'(*(G0.3,:,","))') plcarb_hdr_units
              write (9000,*) "HRU                       hru_plcarb_mon.csv"
            end if
        endif
        
     if (pco%nb_hru%y == "y") then
          open (4542,file="hru_plcarb_yr.txt", recl = 1500)
          write (4542,*)  bsn%name, prog
          write (4542,*) plcarb_hdr
          write (4542,*) plcarb_hdr_units
          write (9000,*) "HRU                       hru_plcarb_yr.txt"
            if (pco%csvout == "y") then
              open (4546,file="hru_plcarb_yr.csv", recl = 1500)
              write (4546,*)  bsn%name, prog
              write (4546,'(*(G0.3,:,","))') plcarb_hdr
              write (4546,'(*(G0.3,:,","))') plcarb_hdr_units
              write (9000,*) "HRU                       hru_plcarb_yr.csv"
            end if
        endif
        
        if (pco%nb_hru%a == "y") then
          open (4543,file="hru_plcarb_aa.txt", recl = 1500)
          write (4543,*)  bsn%name, prog
          write (4543,*) plcarb_hdr
          write (4543,*) plcarb_hdr_units
          write (9000,*) "HRU                       hru_plcarb_aa.txt"
            if (pco%csvout == "y") then
              open (4547,file="hru_plcarb_aa.csv", recl = 1500)
              write (4547,*)  bsn%name, prog
              write (4547,'(*(G0.3,:,","))') plcarb_hdr
              write (4547,'(*(G0.3,:,","))') plcarb_hdr_units
              write (9000,*) "HRU                       hru_plcarb_aa.csv"
            end if
        endif
        
 !!!NEW RESIDUE CARBON OUTPUT     
       
!!!NEW SOIL TRANSFORMATIONS CARBON OUTPUT
        if (pco%nb_hru%d == "y") then
          open (4550,file="hru_scf_day.txt", recl = 1500)
          write (4550,*)  bsn%name, prog
          write (4550,*) hscf_hdr
          write (4550,*) hscf_hdr_units
              write (9000,*) "HRU                       hru_scf_day.txt"          
             if (pco%csvout == "y") then
              open (4554,file="hru_scf_day.csv", recl = 1500)
              write (4554,*)  bsn%name, prog
              write (4554,'(*(G0.3,:,","))') hscf_hdr
              write (4554,'(*(G0.3,:,","))') hscf_hdr_units
              write (9000,*) "HRU                       hru_plcarb_day.csv"
            end if
        endif
        
        if (pco%nb_hru%m == "y") then
          open (4551,file="hru_scf_mon.txt", recl = 1500)
          write (4551,*)  bsn%name, prog
          write (4551,*) hscf_hdr
          write (4551,*) hscf_hdr_units
          write (9000,*) "HRU                       hru_scf_mon.txt"
            if (pco%csvout == "y") then
              open (4555,file="hru_scf_mon.csv", recl = 1500)
              write (4555,*)  bsn%name, prog
              write (4555,'(*(G0.3,:,","))') hscf_hdr
              write (4555,'(*(G0.3,:,","))') hscf_hdr_units
              write (9000,*) "HRU                       hru_scf_mon.csv"
            end if
        endif
        
     if (pco%nb_hru%y == "y") then
          open (4552,file="hru_scf_yr.txt", recl = 1500)
          write (4552,*)  bsn%name, prog
          write (4552,*) hscf_hdr
          write (4552,*) hscf_hdr_units
          write (9000,*) "HRU                       hru_scf_yr.txt"
            if (pco%csvout == "y") then
              open (4556,file="hru_scf_yr.csv", recl = 1500)
              write (4556,*)  bsn%name, prog
              write (4556,'(*(G0.3,:,","))') hscf_hdr
              write (4556,'(*(G0.3,:,","))') hscf_hdr_units
              write (9000,*) "HRU                       hru_scf_yr.csv"
            end if
        endif
        
        if (pco%nb_hru%a == "y") then
          open (4553,file="hru_scf_aa.txt", recl = 1500)
          write (4553,*)  bsn%name, prog
          write (4553,*) hscf_hdr
          write (4553,*) hscf_hdr_units
          write (9000,*) "HRU                       hru_scf_aa.txt"
            if (pco%csvout == "y") then
              open (4557,file="hru_scf_aa.csv", recl = 1500)
              write (4557,*)  bsn%name, prog
              write (4557,'(*(G0.3,:,","))') hscf_hdr
              write (4557,'(*(G0.3,:,","))') hscf_hdr_units
              write (9000,*) "HRU                       hru_scf_aa.csv"
            end if
        endif
        
 !!!NEW SOIL TRANSFORMATIONS CARBON OUTPUT     
       
!!! NEW SOILC_STAT/RESC_STAT/PLC_STAT CARBON OUTPUT FILES

        !! write carbon in soil by layer
        open (9999,file = "hru_cbn_lyr.txt", recl = 1500)
          write (9999,*)  bsn%name, prog
          write (9999,*)                                     &
    "        jday        mon          day       year hru     name               total soil carbon (kg/ha) by layer "
    
        !! write carbon in soil, plant, and residue
        open (4560,file = "hru_plc_stat.txt", recl = 1500)
        if (pco%nb_hru%a == "y") then
          write (4560,*)  bsn%name, prog
          write (4560,*) plc_hdr
          write (4560,*) plc_hdr_units
          write (9000,*) "HRU                       hru_plc_stat.txt"
            if (pco%csvout == "y") then
              open (4563,file="hru_plc_stat.csv", recl = 1500)
              write (4563,*)  bsn%name, prog
              write (4563,'(*(G0.3,:,","))') plc_hdr
              write (4563,'(*(G0.3,:,","))') plc_hdr_units
              write (9000,*) "HRU                       hru_plc_stat.csv"
            end if
        endif
    
        open (4561,file = "hru_resc_stat.txt", recl = 1500)
        if (pco%nb_hru%a == "y") then
          write (4561,*)  bsn%name, prog
          write (4561,*) resc_hdr
          write (4561,*) resc_hdr_units
          write (9000,*) "HRU                       hru_resc_stat.txt"
            if (pco%csvout == "y") then
              open (4564,file="hru_resc_stat.csv", recl = 1500)
              write (4564,*)  bsn%name, prog
              write (4564,'(*(G0.3,:,","))') resc_hdr
              write (4564,'(*(G0.3,:,","))') resc_hdr_units
              write (9000,*) "HRU                       hru_resc_stat.csv"
            end if
        endif
        
        open (4562,file = "hru_soilc_stat.txt", recl = 1500)
        if (pco%nb_hru%a == "y") then
          write (4562,*)  bsn%name, prog
          write (4562,*) soilc_hdr
          write (4562,*) soilc_hdr_units
          write (9000,*) "HRU                       hru_soilc_stat.txt"
            if (pco%csvout == "y") then
              open (4565,file="hru_soilc_stat.csv", recl = 1500)
              write (4565,*)  bsn%name, prog
              write (4565,'(*(G0.3,:,","))') soilc_hdr
              write (4565,'(*(G0.3,:,","))') soilc_hdr_units
              write (9000,*) "HRU                       hru_soilc_stat.csv"
            end if
        endif
        
 !!! NEW SOILC_STAT/RESC_STAT/PLC_STAT CARBON OUTPUT FILES
        
 !! NEW BASIN CARBON ALL OUTPUT FILE
        
        open (4566,file = "basin_carbon_all.txt", recl = 1500)
        if (pco%nb_hru%a == "y") then
          write (4566,*)  bsn%name, prog
          write (4566,*) bsn_carb_hdr
          write (4566,*) bsn_carb_hdr_units
          write (9000,*) "BASIN                     basin_carbon_all.txt"
        end if
          
 !! NEW BASIN CARBON ALL OUTPUT FILE
        
!!!  HRU - Losses
        if (pco%ls_hru%d == "y") then
          open (2030,file="hru_ls_day.txt", recl = 1500)
          write (2030,*) bsn%name, prog
          write (2030,*) ls_hdr    !! hru
          write (2030,*) ls_hdr_units
          write (9000,*) "HRU                       hru_ls_day.txt"
            if (pco%csvout == "y") then
              open (2034,file="hru_ls_day.csv", recl = 1500)
              write (2034,*) bsn%name, prog
              write (2034,'(*(G0.3,:,","))') ls_hdr    !! hru
              write (2034,'(*(G0.3,:,","))') ls_hdr_units
              write (9000,*) "HRU                       hru_ls_day.csv"
            end if 
        endif
        
                
!!!  HRU - New nutcarb gain loss file
        if (pco%ls_hru%d == "y") then
          open (3341,file="hru_nut_carb_gl_day.txt", recl = 1500)
          write (3341,*) bsn%name, prog
          write (3341,*) ls_hdr1    !! hru
          write (3341,*) ls_hdr_units1
          write (9000,*) "HRU                       hru_nut_carb_gl_day.txt"
            if (pco%csvout == "y") then
              open (3342,file="hru_nut_carb_gl_day.csv", recl = 1500)
              write (3342,*) bsn%name, prog
              write (3342,'(*(G0.3,:,","))') ls_hdr1    !! hru
              write (3342,'(*(G0.3,:,","))') ls_hdr_units1
              write (9000,*) "HRU                       hru_nut_carb_gl_day.csv"
            end if 
        endif

        if (pco%ls_hru%m == "y") then
          open (3343,file="hru_nut_carb_gl_mon.txt", recl = 1500)
          write (3343,*) bsn%name, prog
          write (3343,*) ls_hdr1    !! hru
          write (3343,*) ls_hdr_units1
          write (9000,*) "HRU                       hru_nut_carb_gl_mon.txt"
            if (pco%csvout == "y") then
              open (3344,file="hru_nut_carb_gl_mon.csv", recl = 1500)
              write (3344,*) bsn%name, prog
              write (3344,'(*(G0.3,:,","))') ls_hdr1    !! hru
              write (3344,'(*(G0.3,:,","))') ls_hdr_units1
              write (9000,*) "HRU                       hru_nut_carb_gl_mon.csv"
            end if 
        endif
        
        if (pco%ls_hru%y == "y") then
          open (3345,file="hru_nut_carb_gl_yr.txt", recl = 1500)
          write (3345,*) bsn%name, prog
          write (3345,*) ls_hdr1    !! hru
          write (3345,*) ls_hdr_units1
          write (9000,*) "HRU                       hru_nut_carb_gl_yr.txt"
            if (pco%csvout == "y") then
              open (3346,file="hru_nut_carb_gl_yr.csv", recl = 1500)
              write (3346,*) bsn%name, prog
              write (3346,'(*(G0.3,:,","))') ls_hdr1    !! hru
              write (3346,'(*(G0.3,:,","))') ls_hdr_units1
              write (9000,*) "HRU                       hru_nut_carb_gl_yr.csv"
            end if 
        endif
        
        if (pco%ls_hru%a == "y") then
          open (3347,file="hru_nut_carb_gl_aa.txt", recl = 1500)
          write (3347,*) bsn%name, prog
          write (3347,*) ls_hdr1    !! hru
          write (3347,*) ls_hdr_units1
          write (9000,*) "HRU                       hru_nut_carb_gl_aa.txt"
            if (pco%csvout == "y") then
              open (3348,file="hru_nut_carb_gl_aa.csv", recl = 1500)
              write (3348,*) bsn%name, prog
              write (3348,'(*(G0.3,:,","))') ls_hdr1    !! hru
              write (3348,'(*(G0.3,:,","))') ls_hdr_units1
              write (9000,*) "HRU                       hru_nut_carb_gl_aa.csv"
            end if 
        endif
!!!  HRU - New nutcarb gain loss file       
           
       if (pco%ls_hru%m == "y") then
        open (2031,file="hru_ls_mon.txt",recl = 1500)
        write (2031,*) bsn%name, prog
        write (2031,*) ls_hdr  !! hru 
        write (2031,*) ls_hdr_units
        write (9000,*) "HRU                       hru_ls_mon.txt"
          if (pco%csvout == "y") then 
            open (2035,file="hru_ls_mon.csv",recl = 1500)
            write (2035,*) bsn%name, prog
            write (2035,'(*(G0.3,:,","))') ls_hdr  !! hru
            write (2035,'(*(G0.3,:,","))') ls_hdr_units
            write (9000,*) "HRU                       hru_ls_mon.csv"
          end if
       endif
          
        if (pco%ls_hru%y == "y") then
          open (2032,file="hru_ls_yr.txt", recl = 1500)
          write (2032,*) bsn%name, prog
          write (2032,*) ls_hdr    !! hru
          write (2032,*) ls_hdr_units
          write (9000,*) "HRU                       hru_ls_yr.txt"
            if (pco%csvout == "y") then
              open (2036,file="hru_ls_yr.csv", recl = 1500)
              write (2036,*) bsn%name, prog
              write (2036,'(*(G0.3,:,","))') ls_hdr    !! hru
              write (2036,'(*(G0.3,:,","))') ls_hdr_units
              write (9000,*) "HRU                       hru_ls_yr.csv"
            end if 
        endif
        
       if (pco%ls_hru%a == "y") then
        open (2033,file="hru_ls_aa.txt",recl = 1500)
        write (2033,*) bsn%name, prog
        write (2033,*) ls_hdr  !! hru
        write (2033,*) ls_hdr_units
        write (9000,*) "HRU                       hru_ls_aa.txt"
          if (pco%csvout == "y") then 
            open (2037,file="hru_ls_aa.csv",recl = 1500)
            write (2037,*) bsn%name, prog
            write (2037,'(*(G0.3,:,","))') ls_hdr  !! hru
            write (2037,'(*(G0.3,:,","))') ls_hdr_units
            write (9000,*) "HRU                       hru_ls_aa.csv"
          end if 
       end if

!!!  HRU - Plant/Weather
        if (pco%pw_hru%d == "y") then
          open (2040,file="hru_pw_day.txt", recl = 1500)
          write (2040,*) bsn%name, prog
          write (2040,*) pw_hdr  !! hru 
          write (2040,*) pw_hdr_units
          write (9000,*) "HRU                       hru_pw_day.txt"
            if (pco%csvout == "y") then 
              open (2044,file="hru_pw_day.csv", recl = 1500)
              write (2044,*) bsn%name, prog
              write (2044,'(*(G0.3,:,","))') pw_hdr  !! hru
              write (2044,'(*(G0.3,:,","))') pw_hdr_units
              write (9000,*) "HRU                       hru_pw_day.csv"
            end if 
        endif
        
      if (pco%pw_hru%m == "y") then
        open (2041,file="hru_pw_mon.txt",recl = 1500)
        write (2041,*) bsn%name, prog
        write (2041,*) pw_hdr  !! hru
        write (2041,*) pw_hdr_units
        write (9000,*) "HRU                       hru_pw_mon.txt"
          if (pco%csvout == "y") then 
            open (2045,file="hru_pw_mon.csv",recl = 1500)
            write (2045,*) bsn%name, prog
            write (2045,'(*(G0.3,:,","))') pw_hdr  !! hru
            write (2045,'(*(G0.3,:,","))') pw_hdr_units
            write (9000,*) "HRU                       hru_pw_mon.csv"
          end if 
      endif
      
        if (pco%pw_hru%y == "y") then
          open (2042,file="hru_pw_yr.txt", recl = 1500)
          write (2042,*) bsn%name, prog                  
          write (2042,*) pw_hdr  !! hru
          write (2042,*) pw_hdr_units
          write (9000,*) "HRU                       hru_pw_yr.txt"
            if (pco%csvout == "y") then 
              open (2046,file="hru_pw_yr.csv", recl = 1500)
              write (2046,*) bsn%name, prog
              write (2046,'(*(G0.3,:,","))') pw_hdr  !! hru
              write (2046,'(*(G0.3,:,","))') pw_hdr_units
              write (9000,*) "HRU                       hru_pw_yr.csv"
            end if 
        endif
        
       if (pco%pw_hru%a == "y") then
        open (2043,file="hru_pw_aa.txt",recl = 1500)
        write (2043,*) bsn%name, prog
        write (2043,*) pw_hdr  !! hru
        write (2043,*) pw_hdr_units
        write (9000,*) "HRU                       hru_pw_aa.txt"
          if (pco%csvout == "y") then 
            open (2047,file="hru_pw_aa.csv",recl = 1500)
            write (2047,*) bsn%name, prog
            write (2047,'(*(G0.3,:,","))') pw_hdr  !! hru
            write (2047,'(*(G0.3,:,","))') pw_hdr_units
            write (9000,*) "HRU                       hru_pw_aa.csv"
          end if 
       endif
      end if 

 !!! SWAT-DEG - Water Balance 
  
     if (sp_ob%hru_lte > 0) then
        if (pco%wb_sd%d == "y") then
          open (2300,file="hru-lte_wb_day.txt",recl = 1500)
          write (2300,*) bsn%name, prog
          write (2300,*) wb_hdr  !! swat-deg
          write (2300,*) wb_hdr_units
          write (9000,*) "SWAT-DEG                  hru-lte_wb_day.txt"
            if (pco%csvout == "y") then 
              open (2304,file="hru-lte_wb_day.csv",recl = 1500)
              write (2304,*) bsn%name, prog
              write (2304,'(*(G0.3,:,","))') wb_hdr  !! swat-deg
              write (2304,'(*(G0.3,:,","))') wb_hdr_units
              write (9000,*) "SWAT-DEG                  hru-lte_wb_day.csv"
            end if 
        endif

                
      if (pco%wb_sd%m == "y") then
      open (2301,file="hru-lte_wb_mon.txt",recl = 1500)
        write (2301,*) bsn%name, prog
        write (2301,*) wb_hdr   !! swat deg 
        write (2301,*) wb_hdr_units
        write (9000,*) "SWAT-DEG                  hru-lte_wb_mon.txt"
          if (pco%csvout == "y") then 
            open (2305,file="hru-lte_wb_mon.csv",recl = 1500)
            write (2305,*) bsn%name, prog
            write (2305,'(*(G0.3,:,","))') wb_hdr   !! swat deg
            write (2305,'(*(G0.3,:,","))') wb_hdr_units
            write (9000,*) "SWAT-DEG                  hru-lte_wb_mon.csv"
          end if
      end if
          
       
     if (sp_ob%hru_lte > 0) then   
        if (pco%wb_sd%y == "y") then
          open (2302,file="hru-lte_wb_yr.txt",recl = 1500)
          write (2302,*) bsn%name, prog
          write (2302,*) wb_hdr  !! swat-deg
          write (2302,*) wb_hdr_units
          write (9000,*) "SWAT-DEG                  hru-lte_wb_yr.txt"
            if (pco%csvout == "y") then 
              open (2306,file="hru-lte_wb_yr.csv",recl = 1500)
              write (2306,*) bsn%name, prog
              write (2306,'(*(G0.3,:,","))') wb_hdr  !! swat-deg
              write (2306,'(*(G0.3,:,","))') wb_hdr_units
              write (9000,*) "SWAT-DEG                  hru-lte_wb_yr.csv"
            end if 
        endif
      end if 
        
        
      if (pco%wb_sd%a == "y") then
        open (2303,file="hru-lte_wb_aa.txt",recl = 1500)
        write (2303,*) bsn%name, prog
        write (2303,*) wb_hdr   !! swat deg
        write (2303,*) wb_hdr_units
        write (9000,*) "SWAT-DEG                  hru-lte_wb_aa.txt"
          if (pco%csvout == "y") then 
            open (2307,file="hru-lte_wb_aa.csv",recl = 1500)
            write (2307,*) bsn%name, prog
            write (2307,'(*(G0.3,:,","))') wb_hdr   !! swat deg
            write (2307,'(*(G0.3,:,","))') wb_hdr_units
            write (9000,*) "SWAT-DEG                  hru-lte_wb_aa.csv"
          end if
      end if

!!!  SWAT-DEG - Nutrient Balance
!       open (4101,file="nutbal.sd", recl = 1500)  !! no nuts in SWAT-DEG
!       write (4101,*) nb_hdr
!       open (4105,file="nutbal_aa.sd", recl = 1500)
!       write (4105,*) nb_hdr
!       if (pco%csvout == "y") then 
!         open (4025,file="nutbal_sd.csv", recl = 1500)  !! no nuts in SWAT-DEG
!         write (4025,*) nb_hdr
!         open (4026,file="nutbal_aa_sd.csv", recl = 1500)
!         write (4026,*) nb_hdr
!       end if 

!!!  SWAT-DEG - Losses
        if (pco%ls_sd%d == "y") then
          open (2440,file="hru-lte_ls_day.txt",recl = 1500)
          write (2440,*) bsn%name, prog
          write (2440,*) ls_hdr    !! swat-deg
          write (2440,*) ls_hdr_units
          write (9000,*) "SWAT-DEG                  hru-lte_ls_day.txt"
            if (pco%csvout == "y") then 
              open (2444,file="hru-lte_ls_day.csv",recl = 1500)
              write (2444,*) bsn%name, prog
              write (2444,'(*(G0.3,:,","))') ls_hdr    !! swat-deg 
              write (2444,'(*(G0.3,:,","))') ls_hdr_units
              write (9000,*) "SWAT-DEG                  hru-lte_ls_day.csv"
            end if 
        endif
        
      if (pco%ls_sd%m == "y") then
        open (2441,file="hru-lte_ls_mon.txt",recl = 1500)
        write (2441,*) bsn%name, prog
        write (2441,*) ls_hdr  !! swat-deg
        write (2441,*) ls_hdr_units
        write (9000,*) "SWAT-DEG                  hru-lte_ls_mon.txt"
        if (pco%csvout == "y") then 
          open (2445,file="hru-lte_ls_mon.csv",recl = 1500)
          write (2445,*) bsn%name, prog
          write (2445,'(*(G0.3,:,","))') ls_hdr  !! swat-deg
          write (2445,'(*(G0.3,:,","))') ls_hdr_units
          write (9000,*) "SWAT-DEG                  hru-lte_ls_mon.csv"
        end if
      end if
        
        if (pco%ls_sd%y == "y") then
          open (2442,file="hru-lte_ls_yr.txt",recl = 1500)
          write (2442,*) bsn%name, prog
          write (2442,*) ls_hdr    !! swat-deg
          write (2442,*) ls_hdr_units
          write (9000,*) "SWAT-DEG                  hru-lte_ls_yr.txt"
            if (pco%csvout == "y") then 
              open (2446,file="hru-lte_ls_yr.csv",recl = 1500)
              write (2446,*) bsn%name, prog
              write (2446,'(*(G0.3,:,","))') ls_hdr    !! swat-deg 
              write (2446,'(*(G0.3,:,","))') ls_hdr_units
              write (9000,*) "SWAT-DEG                  hru-lte_ls_yr.csv"
            end if 
        endif
        
      if (pco%ls_sd%a == "y") then
         open (2443,file="hru-lte_ls_aa.txt",recl = 1500)
         write (2443,*) bsn%name, prog
         write (2443,*) ls_hdr  !! swat-deg
         write (2443,*) ls_hdr_units
         write (9000,*) "SWAT-DEG                  hru-lte_ls_aa.txt"
        if (pco%csvout == "y") then 
          open (2447,file="hru-lte_ls_aa.csv",recl = 1500)
          write (2447,*) bsn%name, prog
          write (2447,'(*(G0.3,:,","))') ls_hdr  !! swat-deg
          write (2447,'(*(G0.3,:,","))') ls_hdr_units
          write (9000,*) "SWAT-DEG                  hru-lte_ls_aa.csv"
        end if
      end if 
        
        
!!!  SWAT-DEG - Plant/Weather
        if (pco%pw_sd%d == "y") then
          open (2460,file="hru-lte_pw_day.txt",recl = 1500)
          write (2460,*) bsn%name, prog
          write (2460,*) pw_hdr  !! swat-deg
          write (2460,*) pw_hdr_units
          write (9000,*) "SWAT-DEG                  hru-lte_pw_day.txt"
           if (pco%csvout == "y") then 
             open (2464,file="hru-lte_pw_day.csv",recl = 1500)
             write (2464,*) bsn%name, prog
             write (2464,'(*(G0.3,:,","))') pw_hdr  !! swat-deg
             write (2464,'(*(G0.3,:,","))') pw_hdr_units
             write (9000,*) "SWAT-DEG                  hru-lte_pw_day.csv"
           end if
        endif
        
        if (pco%pw_sd%m == "y") then
          open (2461,file="hru-lte_pw_mon.txt",recl = 1500)
          write (2461,*) bsn%name, prog
          write (2461,*) pw_hdr !! swat-deg
          write (2461,*) pw_hdr_units
          write (9000,*) "SWAT-DEG                  hru-lte_pw_mon.txt"
          if (pco%csvout == "y") then 
            open (2465,file="hru-lte_pw_mon.csv",recl = 1500)
            write (2465,*) bsn%name, prog
            write (2465,'(*(G0.3,:,","))') pw_hdr !! swat-deg
            write (2465,'(*(G0.3,:,","))') pw_hdr_units
            write (9000,*) "SWAT-DEG                  hru-lte_pw_mon.csv"
          end if
        end if

       if (pco%pw_sd%y == "y") then
          open (2462,file="hru-lte_pw_yr.txt",recl = 1500)
          write (2462,*) bsn%name, prog
          write (2462,*) pw_hdr  !! swat-deg
          write (2462,*) pw_hdr_units
          write (9000,*) "SWAT-DEG                  hru-lte_pw_yr.txt"
           if (pco%csvout == "y") then 
             open (2466,file="hru-lte_pw_yr.csv",recl = 1500)
             write (2466,*) bsn%name, prog
             write (2466,'(*(G0.3,:,","))') pw_hdr  !! swat-deg
             write (2466,'(*(G0.3,:,","))') pw_hdr_units
             write (9000,*) "SWAT-DEG                  hru-lte_pw_yr.csv"
           end if
       endif
        
      if (pco%pw_sd%a == "y") then    !!!
        open (2463,file="hru-lte_pw_aa.txt",recl = 1500)
        write (2463,*) bsn%name, prog
        write (2463,*) pw_hdr !! swat-deg
        write (2463,*) pw_hdr_units
        write (9000,*) "SWAT-DEG                  hru-lte_pw_aa.txt"
         if (pco%csvout == "y") then 
          open (2467,file="hru-lte_pw_aa.csv",recl = 1500)
          write (2467,*) bsn%name, prog
          write (2467,'(*(G0.3,:,","))') pw_hdr !! swat-deg
          write (2467,'(*(G0.3,:,","))') pw_hdr_units
          write (9000,*) "SWAT-DEG                  hru-lte_pw_aa.csv"
        end if 
      endif
      end if  
      
!!!  Water Balance
      if (db_mx%lsu_out > 0) then   !! Water Balance  
        if (pco%wb_lsu%d == "y") then
          open (2140,file="lsunit_wb_day.txt",recl = 1500)
          write (2140,*) bsn%name, prog
          write (2140,*) wb_hdr  !! subbasin
          write (2140,*) wb_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_wb_day.txt"
          if (pco%csvout == "y") then 
            open (2144,file="lsunit_wb_day.csv",recl = 1500)
            write (2144,*) bsn%name, prog
            write (2144,'(*(G0.3,:,","))') wb_hdr  !! subbasin
            write (2144,'(*(G0.3,:,","))') wb_hdr_units
            write (9000,*) "ROUTING_UNIT              lsunit_wb_day.csv"
          end if 
        endif
    
        
       if (pco%wb_lsu%m == "y") then
        open (2141,file="lsunit_wb_mon.txt",recl = 1500)
        write (2141,*) bsn%name, prog
        write (2141,*) wb_hdr  !! subbasin
        write (2141,*) wb_hdr_units
        write (9000,*) "ROUTING_UNIT              lsunit_wb_mon.txt"
          if (pco%csvout == "y") then
            open (2145,file="lsunit_wb_mon.csv",recl = 1500)
            write (2145,*) bsn%name, prog
            write (2145,'(*(G0.3,:,","))') wb_hdr   !! subbasin
            write (2145,'(*(G0.3,:,","))') wb_hdr_units
            write (9000,*) "ROUTING_UNIT              lsunit_wb_mon.csv"
          end if
        end if 

     if (sp_ob%ru > 0) then   
        if (pco%wb_lsu%y == "y") then
          open (2142,file="lsunit_wb_yr.txt",recl = 1500)
          write (2142,*) bsn%name, prog
          write (2142,*) wb_hdr  !! subbasin
          write (2142,*) wb_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_wb_yr.txt"
          if (pco%csvout == "y") then 
            open (2146,file="lsunit_wb_yr.csv",recl = 1500)
            write (2146,*) bsn%name, prog
            write (2146,'(*(G0.3,:,","))') wb_hdr  !! subbasin
            write (2146,'(*(G0.3,:,","))') wb_hdr_units
            write (9000,*) "ROUTING_UNIT              lsunit_wb_yr.csv"
          end if 
        endif
     end if
        
       if (pco%wb_lsu%a == "y") then
         open (2143,file="lsunit_wb_aa.txt",recl = 1500)
         write (2143,*) bsn%name, prog
         write (2143,*) wb_hdr   !! subbasin
         write (2143,*) wb_hdr_units
         write (9000,*) "ROUTING_UNIT              lsunit_wb_aa.txt"
          if (pco%csvout == "y") then
           open (2147,file="lsunit_wb_aa.csv",recl = 1500)
           write (2147,*) bsn%name, prog
           write (2147,'(*(G0.3,:,","))') wb_hdr   !! subbasin
           write (2147,'(*(G0.3,:,","))') wb_hdr_units
           write (9000,*) "ROUTING_UNIT              lsunit_wb_aa.csv"
          end if
       end if
        
!!!  Nutrient Balance
        if (pco%nb_lsu%d == "y") then
          open (2150,file="lsunit_nb_day.txt",recl = 1500)
          write (2150,*) bsn%name, prog
          write (2150,*) nb_hdr
          write (2150,*) nb_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_nb_day.txt"
          if (pco%csvout == "y") then 
            open (2154,file="lsunit_nb_day.csv",recl = 1500)
            write (2154,*) bsn%name, prog
            write (2154,'(*(G0.3,:,","))') nb_hdr
            write (2154,'(*(G0.3,:,","))') nb_hdr_units
            write (9000,*) "ROUTING_UNIT              lsunit_nb_day.csv"
          end if 
        endif
        
        if (pco%nb_lsu%m == "y") then
        open (2151,file="lsunit_nb_mon.txt", recl = 1500)
        write (2151,*) bsn%name, prog
        write (2151,*) nb_hdr
        write (2151,*) nb_hdr_units
        write (9000,*) "ROUTING_UNIT              lsunit_nb_mon.txt"
          if (pco%csvout == "y") then
            open (2155,file="lsunit_nb_mon.csv", recl = 1500)
            write (2155,*) bsn%name, prog
            write (2155,'(*(G0.3,:,","))') nb_hdr
            write (2155,'(*(G0.3,:,","))') nb_hdr_units
            write (9000,*) "ROUTING_UNIT              lsunit_nb_mon.csv"
          end if
        end if
        
        if (pco%nb_lsu%y == "y") then
          open (2152,file="lsunit_nb_yr.txt",recl = 1500)
          write (2152,*) bsn%name, prog
          write (2152,*) nb_hdr
          write (2152,*) nb_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_nb_yr.txt"
          if (pco%csvout == "y") then 
            open (2156,file="lsunit_nb_yr.csv",recl = 1500)
            write (2156,*) bsn%name, prog
            write (2156,'(*(G0.3,:,","))') nb_hdr
            write (2156,'(*(G0.3,:,","))') nb_hdr_units
            write (9000,*) "ROUTING_UNIT              lsunit_nb_yr.csv"
          end if 
        endif
        
        if (pco%nb_lsu%a == "y") then
        open (2153,file="lsunit_nb_aa.txt", recl = 1500)
        write (2153,*) bsn%name, prog
        write (2153,*) nb_hdr
        write (2153,*) nb_hdr_units
        write (9000,*) "ROUTING_UNIT              lsunit_nb_aa.txt"
          if (pco%csvout == "y") then
            open (2157,file="lsunit_nb_aa.csv", recl = 1500)
            write (2157,*) bsn%name, prog
            write (2157,'(*(G0.3,:,","))') nb_hdr
            write (2157,'(*(G0.3,:,","))') nb_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_nb_aa.csv"
          end if 
        end if 

!!!  Losses
        if (pco%ls_lsu%d == "y") then
          open (2160,file="lsunit_ls_day.txt",recl = 1500)
          write (2160,*) bsn%name, prog
          write (2160,*) ls_hdr 
          write (2160,*) ls_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_ls_day.txt"
          if (pco%csvout == "y") then 
            open (2164,file="lsunit_ls_day.csv",recl = 1500)
            write (2164,*) bsn%name, prog
            write (2164,'(*(G0.3,:,","))') ls_hdr    !! subbasin
            write (2164,'(*(G0.3,:,","))') ls_hdr_units
            write (9000,*) "ROUTING_UNIT              lsunit_ls_day.csv"
          end if 
        endif
        
      if (pco%ls_lsu%m == "y") then
        open (2161,file="lsunit_ls_mon.txt",recl = 1500)
        write (2161,*) bsn%name, prog
        write (2161,*) ls_hdr 
        write (2161,*) ls_hdr_units
        write (9000,*) "ROUTING_UNIT              lsunit_ls_mon.txt"
        if (pco%csvout == "y") then 
          open (2165,file="lsunit_ls_mon.csv",recl = 1500)
          write (2165,*) bsn%name, prog
          write (2165,'(*(G0.3,:,","))') ls_hdr  !! subbasin 
          write (2165,'(*(G0.3,:,","))') ls_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_ls_mon.csv"
        end if 
      end if 
        
       if (pco%ls_lsu%y == "y") then
          open (2162,file="lsunit_ls_yr.txt",recl = 1500)
          write (2162,*) bsn%name, prog
          write (2162,*) ls_hdr 
          write (2162,*) ls_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_ls_yr.txt"
          if (pco%csvout == "y") then 
            open (2166,file="lsunit_ls_yr.csv",recl = 1500)
            write (2166,*) bsn%name, prog
            write (2166,'(*(G0.3,:,","))') ls_hdr
            write (2166,'(*(G0.3,:,","))') ls_hdr_units
            write (9000,*) "ROUTING_UNIT              lsunit_ls_yr.csv"
          end if 
       endif
        
       if (pco%ls_lsu%a == "y") then
       open (2163,file="lsunit_ls_aa.txt",recl = 1500)
        write (2163,*) bsn%name, prog
        write (2163,*) ls_hdr 
        write (2163,*) ls_hdr_units
        write (9000,*) "ROUTING_UNIT              lsunit_ls_aa.txt"
        if (pco%csvout == "y") then 
          open (2167,file="lsunit_ls_aa.csv",recl = 1500)
          write (2167,*) bsn%name, prog
          write (2167,'(*(G0.3,:,","))') ls_hdr
          write (2167,'(*(G0.3,:,","))') ls_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_ls_aa.csv"
        end if 
       end if

!!!  Plant/Weather
        if (pco%pw_lsu%d == "y") then
          open (2170,file="lsunit_pw_day.txt",recl = 1500)
          write (2170,*) bsn%name, prog
          write (2170,*) pw_hdr
          write (2170,*) pw_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_pw_day.txt"
          if (pco%csvout == "y") then 
            open (2174,file="lsunit_pw_day.csv",recl = 1500)
            write (2174,*) bsn%name, prog
            write (2174,'(*(G0.3,:,","))') pw_hdr 
            write (2174,'(*(G0.3,:,","))') pw_hdr_units
            write (9000,*) "ROUTING_UNIT              lsunit_pw_day.csv"
          end if 
        end if 
    
      
      if (pco%pw_lsu%m == "y") then
       open (2171,file="lsunit_pw_mon.txt",recl = 1500)
        write (2171,*) bsn%name, prog
        write (2171,*) pw_hdr
        write (2171,*) pw_hdr_units
        write (9000,*) "ROUTING_UNIT              lsunit_pw_mon.txt"
        if (pco%csvout == "y") then 
          open (2175,file="lsunit_pw_mon.csv",recl = 1500)
          write (2175,*) bsn%name, prog
          write (2175,'(*(G0.3,:,","))') pw_hdr
          write (2175,'(*(G0.3,:,","))') pw_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_pw_mon.csv"
        end if
       end if
        
        if (pco%pw_lsu%y == "y") then
          open (2172,file="lsunit_pw_yr.txt",recl = 1500)
          write (2172,*) bsn%name, prog
          write (2172,*) pw_hdr
          write (2172,*) pw_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_pw_yr.txt"
          if (pco%csvout == "y") then 
            open (2176,file="lsunit_pw_yr.csv",recl = 1500)
            write (2176,*) bsn%name, prog
            write (2176, '(*(G0.3,:,","))')pw_hdr
            write (2176,'(*(G0.3,:,","))') pw_hdr_units
            write (9000,*) "ROUTING_UNIT              lsunit_pw_yr.csv"
          end if 
        end if 
      
     if (pco%pw_lsu%a == "y") then
       open (2173,file="lsunit_pw_aa.txt",recl = 1500)
        write (2173,*) bsn%name, prog
        write (2173,*) pw_hdr
        write (2173,*) pw_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_pw_aa.txt"
        if (pco%csvout == "y") then 
          open (2177,file="lsunit_pw_aa.csv",recl = 1500)
          write (2177,*) bsn%name, prog
          write (2177,'(*(G0.3,:,","))') pw_hdr
          write (2177,'(*(G0.3,:,","))') pw_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_pw_aa.csv"
        end if
     end if
      end if
      
!!!  BASIN - Water balance 
        if (pco%wb_bsn%d == "y") then
          open (2050,file="basin_wb_day.txt",recl = 1500)
          write (2050,*) bsn%name, prog
          write (2050,*) wb_hdr  !! bsn
          write (2050,*) wb_hdr_units
          write (9000,*) "BASIN                     basin_wb_day.txt"
          if (pco%csvout == "y") then 
            open (2054,file="basin_wb_day.csv",recl = 1500)
            write (2054,*) bsn%name, prog
            write (2054,'(*(G0.3,:,","))') wb_hdr !! bsn
            write (2054,'(*(G0.3,:,","))') wb_hdr_units
            write (9000,*) "BASIN                     basin_wb_day.csv"
          end if 
        endif
            
       if (pco%wb_bsn%m == "y") then 
        open (2051,file="basin_wb_mon.txt",recl = 1500)
        write (2051,*) bsn%name, prog
        write (2051,*) wb_hdr  !! bsn
        write (2051,*) wb_hdr_units
        write (9000,*) "BASIN                     basin_wb_mon.txt"
        if (pco%csvout == "y") then 
          open (2055,file="basin_wb_mon.csv",recl = 1500)
          write (2055,*) bsn%name, prog
          write (2055,'(*(G0.3,:,","))') wb_hdr !! bsn
          write (2055,'(*(G0.3,:,","))') wb_hdr_units
          write (9000,*) "BASIN                     basin_wb_mon.csv"
        end if
       end if 

        if (pco%wb_bsn%y == "y") then
          open (2052,file="basin_wb_yr.txt",recl = 1500)
          write (2052,*) bsn%name, prog
          write (2052,*) wb_hdr  !! bsn
          write (2052,*) wb_hdr_units
          write (9000,*) "BASIN                     basin_wb_yr.txt"
          if (pco%csvout == "y") then 
            open (2056,file="basin_wb_yr.csv",recl = 1500)
            write (2056,*) bsn%name, prog
            write (2056,'(*(G0.3,:,","))') wb_hdr !! bsn
            write (2056,'(*(G0.3,:,","))') wb_hdr_units
            write (9000,*) "BASIN                     basin_wb_yr.csv"
          end if 
        endif
        
       if (pco%wb_bsn%a == "y") then 
        open (2053,file="basin_wb_aa.txt",recl = 1500)
        write (2053,*) bsn%name, prog
        write (2053,*) wb_hdr  !! bsn
        write (2053,*) wb_hdr_units
        write (9000,*) "BASIN                     basin_wb_aa.txt"
        if (pco%csvout == "y") then 
          open (2057,file="basin_wb_aa.csv",recl = 1500)
          write (2057,*) bsn%name, prog
          write (2057,'(*(G0.3,:,","))') wb_hdr !! bsn
          write (2057,'(*(G0.3,:,","))') wb_hdr_units
          write (9000,*) "BASIN                     basin_wb_aa.csv"
        end if
       end if 

!!!  BASIN - Nutrient balance    
        if (pco%nb_bsn%d == "y") then
          open (2060,file="basin_nb_day.txt", recl = 1500)
          write (2060,*) bsn%name, prog
          write (2060,*) nb_hdr
          write (2060,*) nb_hdr_units
          write (9000,*) "BASIN                     basin_nb_day.txt"
          if (pco%csvout == "y") then 
            open (2064,file="basin_nb_day.csv", recl = 1500)
            write (2064,*) bsn%name, prog
            write (2064,'(*(G0.3,:,","))') nb_hdr
            write (2064,'(*(G0.3,:,","))') nb_hdr_units
            write (9000,*) "BASIN                     basin_nb_day.csv"
          end if 
        endif
        
       if (pco%nb_bsn%m == "y") then 
        open (2061,file="basin_nb_mon.txt", recl = 1500)
        write (2061,*) bsn%name, prog
        write (2061,*) nb_hdr
        write (2061,*) nb_hdr_units
        write (9000,*) "BASIN                     basin_nb_mon.txt"
        if (pco%csvout == "y") then 
          open (2065,file="basin_nb_mon.csv", recl = 1500)
          write (2065,*) bsn%name, prog
          write (2065,'(*(G0.3,:,","))') nb_hdr
          write (2065,'(*(G0.3,:,","))') nb_hdr_units
          write (9000,*) "BASIN                     basin_nb_mon.csv"
        end if
       end if 

        if (pco%nb_bsn%y == "y") then
          open (2062,file="basin_nb_yr.txt", recl = 1500)
          write (2062,*) bsn%name, prog
          write (2062,*) nb_hdr
          write (2062,*) nb_hdr_units
          write (9000,*) "BASIN                     basin_nb_yr.txt"
          if (pco%csvout == "y") then 
            open (2066,file="basin_nb_yr.csv", recl = 1500)
            write (2066,*) bsn%name, prog
            write (2066,'(*(G0.3,:,","))') nb_hdr
            write (2066,'(*(G0.3,:,","))') nb_hdr_units
            write (9000,*) "BASIN                     basin_nb_yr.csv"
          end if 
        endif
        
       if (pco%nb_bsn%a == "y") then 
        open (2063,file="basin_nb_aa.txt", recl = 1500)
        write (2063,*) bsn%name, prog
        write (2063,*) nb_hdr
        write (2063,*) nb_hdr_units
        write (9000,*) "BASIN                     basin_nb_aa.txt"
        if (pco%csvout == "y") then 
          open (2067,file="basin_nb_aa.csv", recl = 1500)
          write (2067,*) bsn%name, prog
          write (2067,'(*(G0.3,:,","))') nb_hdr
          write (2067,'(*(G0.3,:,","))') nb_hdr_units
          write (9000,*) "BASIN                     basin_nb_aa.csv"
        end if
       end if 
                
!!!  BASIN - Losses
        if (pco%ls_bsn%d == "y") then
          open (2070,file="basin_ls_day.txt", recl = 1500)
          write (2070,*) bsn%name, prog
          write (2070,*) ls_hdr    !! bsn
          write (2070,*) ls_hdr_units
          write (9000,*) "BASIN                     basin_ls_day.txt"
          if (pco%csvout == "y") then 
            open (2074,file="basin_ls_day.csv", recl = 1500)
            write (2074,*) bsn%name, prog
            write (2074,'(*(G0.3,:,","))') ls_hdr    !! bsn
            write (2074,'(*(G0.3,:,","))') ls_hdr_units
            write (9000,*) "BASIN                     basin_ls_day.csv"
          end if 
        endif
        
       if (pco%ls_bsn%m == "y") then
        open (2071,file="basin_ls_mon.txt",recl = 1500)
        write (2071,*) bsn%name, prog
        write (2071,*) ls_hdr     !! bsn
        write (2071,*) ls_hdr_units
        write (9000,*) "BASIN                     basin_ls_mon.txt"
        if (pco%csvout == "y") then 
          open (2075,file="basin_ls_mon.csv",recl = 1500)
          write (2075,*) bsn%name, prog
          write (2075,'(*(G0.3,:,","))') ls_hdr     !! bsn
          write (2075,'(*(G0.3,:,","))') ls_hdr_units
          write (9000,*) "BASIN                     basin_ls_mon.csv"
        end if
       end if

        if (pco%ls_bsn%y == "y") then
          open (2072,file="basin_ls_yr.txt", recl = 1500)
          write (2072,*) bsn%name, prog
          write (2072,*) ls_hdr    !! bsn
          write (2072,*) ls_hdr_units
          write (9000,*) "BASIN                     basin_ls_yr.txt"
          if (pco%csvout == "y") then 
            open (2076,file="basin_ls_yr.csv", recl = 1500)
            write (2076,*) bsn%name, prog
            write (2076,'(*(G0.3,:,","))') ls_hdr    !! bsn
            write (2076,'(*(G0.3,:,","))') ls_hdr_units
            write (9000,*) "BASIN                     basin_ls_yr.csv"
          end if 
        endif
        
       if (pco%ls_bsn%a == "y") then
        open (2073,file="basin_ls_aa.txt",recl = 1500)
        write (2073,*) bsn%name, prog
        write (2073,*) ls_hdr     !! bsn
        write (2073,*) ls_hdr_units
        write (9000,*) "BASIN                     basin_ls_aa.txt"
        if (pco%csvout == "y") then 
          open (2077,file="basin_ls_aa.csv",recl = 1500)
          write (2077,*) bsn%name, prog
          write (2077,'(*(G0.3,:,","))') ls_hdr     !! bsn
          write (2077,'(*(G0.3,:,","))') ls_hdr_units
          write (9000,*) "BASIN                     basin_ls_aa.csv"
        end if
       end if
        
!!!  BASIN - Plant/Weather
        if (pco%pw_bsn%d == "y") then
          open (2080,file="basin_pw_day.txt", recl = 1500)
          write (2080,*) bsn%name, prog
          write (2080,*) pw_hdr  !! bsn
          write (2080,*) pw_hdr_units
          write (9000,*) "BASIN                     basin_pw_day.txt"
          if (pco%csvout == "y") then 
            open (2084,file="basin_pw_day.csv", recl = 1500)
            write (2084,*) bsn%name, prog
            write (2084,'(*(G0.3,:,","))') pw_hdr  !! bsn
            write (2084,'(*(G0.3,:,","))') pw_hdr_units
            write (9000,*) "BASIN                     basin_pw_day.csv"
          end if
        endif
        
       if (pco%pw_bsn%m == "y") then
        open (2081,file="basin_pw_mon.txt",recl = 1500) 
        write (2081,*) bsn%name, prog
        write (2081,*) pw_hdr  !! bsn
        write (2081,*) pw_hdr_units
        write (9000,*) "BASIN                     basin_pw_mon.txt"
       if (pco%csvout == "y") then 
          open (2085,file="basin_pw_mon.csv",recl = 1500)
          write (2085,*) bsn%name, prog
          write (2085,'(*(G0.3,:,","))') pw_hdr     !! bsn
          write (2085,'(*(G0.3,:,","))') pw_hdr_units
          write (9000,*) "BASIN                     basin_pw_mon.csv"
       end if
      end if

        if (pco%pw_bsn%y == "y") then
          open (2082,file="basin_pw_yr.txt", recl = 1500)
          write (2082,*) bsn%name, prog
          write (2082,*) pw_hdr  !! bsn
          write (2082,*) pw_hdr_units
          write (9000,*) "BASIN                     basin_pw_yr.txt"
          if (pco%csvout == "y") then 
            open (2086,file="basin_pw_yr.csv", recl = 1500)
            write (2086,*) bsn%name, prog
            write (2086,'(*(G0.3,:,","))') pw_hdr  !! bsn
            write (2086,'(*(G0.3,:,","))') pw_hdr_units
            write (9000,*) "BASIN                     basin_pw_yr.csv"
          end if
        endif
        
       if (pco%pw_bsn%a == "y") then
        open (2083,file="basin_pw_aa.txt",recl = 1500) 
        write (2083,*) bsn%name, prog
        write (2083,*) pw_hdr  !! bsn
        write (2083,*) pw_hdr_units
        write (9000,*) "BASIN                     basin_pw_aa.txt"
       if (pco%csvout == "y") then 
          open (2087,file="basin_pw_aa.csv",recl = 1500)
          write (2087,*) bsn%name, prog
          write (2087,'(*(G0.3,:,","))') pw_hdr     !! bsn
          write (2087,'(*(G0.3,:,","))') pw_hdr_units
          write (9000,*) "BASIN                     basin_pw_aa.csv"
       end if
       end if
       
!!! CROP YIELDS - output file only written for yearly or annual timesteps; "b" = both files written;
      if (pco%crop_yld == "y" .or. pco%crop_yld == "b") then
        !! headers for yearly crop yields
        open (4010,file="crop_yld_yr.txt")
        write (4010,*) bsn%name, prog
          write (4010,1000)
        write (9000,*) "CROP                      crop_yld_yr.txt"
        if (pco%csvout == "y") then
            open (4011,file="crop_yld_yr.csv")
            write (4011,*) bsn%name, prog
            write (4011,'(*(G0.3,:,","))') "jday","mon","day","year","unit","plantnm","yield"
            write (9000,*) "CROP                      crop_yld_yr.csv"
        end if
      end if
              
      !! headers for annual crop yields
      if (pco%crop_yld == "a" .or. pco%crop_yld == "b") then
        open (4008,file="crop_yld_aa.txt")
        write (4008,*) bsn%name, prog
          write (4008,1000)
        write (9000,*) "CROP                      crop_yld_aa.txt"
        if (pco%csvout == "y") then
            open (4009,file="crop_yld_aa.csv")
            write (4009,*) bsn%name, prog
            write (4009,'(*(G0.3,:,","))') "jday","mon","day","year","unit","plantnm","yield"
            write (9000,*) "CROP                      crop_yld_aa.csv"
        end if
      end if
      
1000    format (76x,"--YIELD (kg/ha)--",/,1x," jday",1x,"  mon",1x,"  day",1x,"   yr",1x,"   unit", 1x,"PLANTNM",   &
                 18x,"       MASS","          C", "           N","           P")
        
      return
      end subroutine output_landscape_init