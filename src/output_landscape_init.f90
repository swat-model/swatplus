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
      use output_path_module
      
      implicit none 
      
      external :: soil_nutcarb_write

      if (sp_ob%hru > 0) then
!!!  HRU - Water balance
        if (pco%wb_hru%d == "y") then
          call open_output_file(2000, "hru_wb_day.txt", 1500)
          write (2000,*)  bsn%name, prog
          write (2000,*) wb_hdr  !! hru
          write (2000,*) wb_hdr_units
          write (9000,*) "HRU                       hru_wb_day.txt"
          !write (9000,*) "HRU                 waterbal_day_hru.txt"
            if (pco%csvout == "y") then
              call open_output_file(2004, "hru_wb_day.csv", 1500)
              write (2004,*)  bsn%name, prog
              write (2004,'(*(G0.3,:,","))') wb_hdr  !! hru
              write (2004,'(*(G0.3,:,","))') wb_hdr_units
              write (9000,*) "HRU                       hru_wb_day.csv"
              !write (9000,*) "HRU                 waterbal_day_hru.csv"              
            end if 
        endif
 
        if (pco%wb_hru%m == "y") then 
          call open_output_file(2001, "hru_wb_mon.txt", 1500)
          write (2001,*)  bsn%name, prog
          write (2001,*) wb_hdr   !! hru
          write (2001,*) wb_hdr_units
           write (9000,*) "HRU                       hru_wb_mon.txt"
          !write (9000,*) "HRU                 waterbal_mon_hru.txt"
          if (pco%csvout == "y") then
            call open_output_file(2005, "hru_wb_mon.csv", 1500)
            write (2005,*)  bsn%name, prog
            write (2005,'(*(G0.3,:,","))') wb_hdr   !! hru
            write (2005,'(*(G0.3,:,","))') wb_hdr_units
            write (9000,*) "HRU                       hru_wb_mon.csv"
            !write (9000,*) "HRU                 waterbal_mon_hru.csv"
          end if
        end if 

        if (pco%wb_hru%y == "y") then
          call open_output_file(2002, "hru_wb_yr.txt", 1500)
          write (2002,*)  bsn%name, prog
          write (2002,*) wb_hdr  !! hru
          write (2002,*) wb_hdr_units
          write (9000,*) "HRU                       hru_wb_yr.txt"
            if (pco%csvout == "y") then
              call open_output_file(2006, "hru_wb_yr.csv", 1500)
              write (2006,*)  bsn%name, prog
              write (2006,'(*(G0.3,:,","))') wb_hdr  !! hru
              write (2006,'(*(G0.3,:,","))') wb_hdr_units
              write (9000,*) "HRU                       hru_wb_yr.csv"
              !write (9000,*) "HRU                 waterbal_yr_hru.csv"
            end if 
        endif
        
        if (pco%wb_hru%a == "y") then
          call open_output_file(2003, "hru_wb_aa.txt", 1500)
          write (2003,*)  bsn%name, prog
          write (2003,*) wb_hdr   !! hru
          write (2003,*) wb_hdr_units
          write (9000,*) "HRU                       hru_wb_aa.txt"
          if (pco%csvout == "y") then
            call open_output_file(2007, "hru_wb_aa.csv", 1500)
            write (2007,*)  bsn%name, prog
            write (2007,'(*(G0.3,:,","))') wb_hdr   !! hru
            write (2007,'(*(G0.3,:,","))') wb_hdr_units
            write (9000,*) "HRU                       hru_wb_aa.csv"
          end if
        end if 

!!!  HRU - Nutrient balance
        if (pco%nb_hru%d == "y") then
          call open_output_file(2020, "hru_nb_day.txt", 1500)
          write (2020,*)  bsn%name, prog
          write (2020,*) nb_hdr
          write (2020,*) nb_hdr_units
          write (9000,*) "HRU                       hru_nb_day.txt"
            if (pco%csvout == "y") then
              call open_output_file(2024, "hru_nb_day.csv", 1500)
              write (2024,*)  bsn%name, prog
              write (2024,'(*(G0.3,:,","))') nb_hdr
              write (2024,'(*(G0.3,:,","))') nb_hdr_units
              write (9000,*) "HRU                       hru_nb_day.csv"
            end if
        endif
        
!!!  HRU - NEW Nutrient cycling output
        if (pco%nb_hru%d == "y") then
          call open_output_file(3333, "hru_ncycle_day.txt", 1500)
          write (3333,*)  bsn%name, prog
          write (3333,*) nb_hdr1
          write (3333,*) nb_hdr_units1
          write (9000,*) "HRU                       hru_ncycle_day.txt"
            if (pco%csvout == "y") then
              call open_output_file(3334, "hru_ncycle_day.csv", 1500)
              write (3334,*)  bsn%name, prog
              write (3334,'(*(G0.3,:,","))') nb_hdr1
              write (3334,'(*(G0.3,:,","))') nb_hdr_units1
              write (9000,*) "HRU                       hru_ncycle_day.csv"
            end if
        endif
        
        if (pco%nb_hru%m == "y") then
          call open_output_file(3335, "hru_ncycle_mon.txt", 1500)
          write (3335,*)  bsn%name, prog
          write (3335,*) nb_hdr1
          write (3335,*) nb_hdr_units1
          write (9000,*) "HRU                       hru_ncycle_mon.txt"
            if (pco%csvout == "y") then
              call open_output_file(3336, "hru_ncycle_mon.csv", 1500)
              write (3336,*)  bsn%name, prog
              write (3336,'(*(G0.3,:,","))') nb_hdr1
              write (3336,'(*(G0.3,:,","))') nb_hdr_units1
              write (9000,*) "HRU                       hru_ncycle_mon.csv"
            end if
        endif
        
     if (pco%nb_hru%y == "y") then
          call open_output_file(3337, "hru_ncycle_yr.txt", 1500)
          write (3337,*)  bsn%name, prog
          write (3337,*) nb_hdr1
          write (3337,*) nb_hdr_units1
          write (9000,*) "HRU                       hru_ncycle_yr.txt"
            if (pco%csvout == "y") then
              call open_output_file(3338, "hru_ncycle_yr.csv", 1500)
              write (3338,*)  bsn%name, prog
              write (3338,'(*(G0.3,:,","))') nb_hdr1
              write (3338,'(*(G0.3,:,","))') nb_hdr_units1
              write (9000,*) "HRU                       hru_ncycle_yr.csv"
            end if
        endif
        
        if (pco%nb_hru%a == "y") then
          call open_output_file(3339, "hru_ncycle_aa.txt", 1500)
          write (3339,*)  bsn%name, prog
          write (3339,*) nb_hdr1
          write (3339,*) nb_hdr_units1
          write (9000,*) "HRU                       hru_ncycle_aa.txt"
            if (pco%csvout == "y") then
              call open_output_file(3340, "hru_ncycle_aa.csv", 1500)
              write (3340,*)  bsn%name, prog
              write (3340,'(*(G0.3,:,","))') nb_hdr1
              write (3340,'(*(G0.3,:,","))') nb_hdr_units1
              write (9000,*) "HRU                       hru_ncycle_aa.csv"
            end if
        endif
!!!  HRU - NEW Nutrient cycling output
        
       if (pco%nb_hru%m == "y") then
        call open_output_file(2021, "hru_nb_mon.txt", 1500)
          write (2021,*) bsn%name, prog
          write (2021,*) nb_hdr
          write (2021,*) nb_hdr_units
          write (9000,*) "HRU                       hru_nb_mon.txt"
        if (pco%csvout == "y") then
          call open_output_file(2025, "hru_nb_mon.csv", 1500)
          write (2025,*) bsn%name, prog
          write (2025,'(*(G0.3,:,","))') nb_hdr
          write (2025,'(*(G0.3,:,","))') nb_hdr_units
          write (9000,*) "HRU                       hru_nb_mon.csv"
        end if
       end if

        if (pco%nb_hru%y == "y") then
          call open_output_file(2022, "hru_nb_yr.txt", 1500)
          write (2022,*) bsn%name, prog
          write (2022,*) nb_hdr
          write (2022,*) nb_hdr_units
          write (9000,*) "HRU                       hru_nb_yr.txt"
            if (pco%csvout == "y") then
              call open_output_file(2026, "hru_nb_yr.csv", 1500)
              write (2026,*) bsn%name, prog
              write (2026,'(*(G0.3,:,","))') nb_hdr
              write (2026,'(*(G0.3,:,","))') nb_hdr_units
              write (9000,*) "HRU                       hru_nb_yr.csv" 
            end if
        endif
        
       if (pco%nb_hru%a == "y") then 
        call open_output_file(2023, "hru_nb_aa.txt", 1500)
          write (2023,*) bsn%name, prog
          write (2023,*) nb_hdr
          write (2023,*) nb_hdr_units
          write (9000,*) "HRU                       hru_nb_aa.txt"
        if (pco%csvout == "y") then
          call open_output_file(2027, "hru_nb_aa.csv", 1500)
          write (2027,*) bsn%name, prog
          write (2027,'(*(G0.3,:,","))') nb_hdr
          write (2027,'(*(G0.3,:,","))') nb_hdr_units
          write (9000,*) "HRU                       hru_nb_aa.csv"
        end if
       end if
               
 !!!NEW SOIL CARBON OUTPUT
        if (pco%nb_hru%d == "y") then
          call open_output_file(4520, "hru_soilcarb_day.txt", 1500)
          write (4520,*)  bsn%name, prog
          write (4520,*) soilcarb_hdr
          write (4520,*) soilcarb_hdr_units
              write (9000,*) "HRU                       hru_soilcarb_day.txt"          
             if (pco%csvout == "y") then
              call open_output_file(4524, "hru_soilcarb_day.csv", 1500)
              write (4524,*)  bsn%name, prog
              write (4524,'(*(G0.3,:,","))') soilcarb_hdr
              write (4524,'(*(G0.3,:,","))') soilcarb_hdr_units
              write (9000,*) "HRU                       hru_soilcarb_day.csv"
            end if
        endif
        
        if (pco%nb_hru%m == "y") then
          call open_output_file(4521, "hru_soilcarb_mon.txt", 1500)
          write (4521,*)  bsn%name, prog
          write (4521,*) soilcarb_hdr
          write (4521,*) soilcarb_hdr_units
          write (9000,*) "HRU                       hru_soilcarb_mon.txt"
            if (pco%csvout == "y") then
              call open_output_file(4525, "hru_soilcarb_mon.csv", 1500)
              write (4525,*)  bsn%name, prog
              write (4525,'(*(G0.3,:,","))') soilcarb_hdr
              write (4525,'(*(G0.3,:,","))') soilcarb_hdr_units
              write (9000,*) "HRU                       hru_soilcarb_mon.csv"
            end if
        endif
        
     if (pco%nb_hru%y == "y") then
          call open_output_file(4522, "hru_soilcarb_yr.txt", 1500)
          write (4522,*)  bsn%name, prog
          write (4522,*) soilcarb_hdr
          write (4522,*) soilcarb_hdr_units
          write (9000,*) "HRU                       hru_soilcarb_yr.txt"
            if (pco%csvout == "y") then
              call open_output_file(4526, "hru_soilcarb_yr.csv", 1500)
              write (4526,*)  bsn%name, prog
              write (4526,'(*(G0.3,:,","))') soilcarb_hdr
              write (4526,'(*(G0.3,:,","))') soilcarb_hdr_units
              write (9000,*) "HRU                       hru_soilcarb_yr.csv"
            end if
        endif
        
        if (pco%nb_hru%a == "y") then
          call open_output_file(4523, "hru_soilcarb_aa.txt", 1500)
          write (4523,*)  bsn%name, prog
          write (4523,*) soilcarb_hdr
          write (4523,*) soilcarb_hdr_units
          write (9000,*) "HRU                       hru_soilcarb_aa.txt"
            if (pco%csvout == "y") then
              call open_output_file(4527, "hru_soilcarb_aa.csv", 1500)
              write (4527,*)  bsn%name, prog
              write (4527,'(*(G0.3,:,","))') soilcarb_hdr
              write (4527,'(*(G0.3,:,","))') soilcarb_hdr_units
              write (9000,*) "HRU                       hru_soilcarb_aa.csv"
            end if
        endif
        
 !!!NEW SOIL CARBON OUTPUT     
              
!!!NEW RESIDUE CARBON OUTPUT
        if (pco%nb_hru%d == "y") then
          call open_output_file(4530, "hru_rescarb_day.txt", 1500)
          write (4530,*)  bsn%name, prog
          write (4530,*) rescarb_hdr
          write (4530,*) rescarb_hdr_units
              write (9000,*) "HRU                       hru_rescarb_day.txt"          
             if (pco%csvout == "y") then
              call open_output_file(4534, "hru_rescarb_day.csv", 1500)
              write (4534,*)  bsn%name, prog
              write (4534,'(*(G0.3,:,","))') rescarb_hdr
              write (4534,'(*(G0.3,:,","))') rescarb_hdr_units
              write (9000,*) "HRU                       hru_rescarb_day.csv"
            end if
        endif
        
        if (pco%nb_hru%m == "y") then
          call open_output_file(4531, "hru_rescarb_mon.txt", 1500)
          write (4531,*)  bsn%name, prog
          write (4531,*) rescarb_hdr
          write (4531,*) rescarb_hdr_units
          write (9000,*) "HRU                       hru_rescarb_mon.txt"
            if (pco%csvout == "y") then
              call open_output_file(4535, "hru_rescarb_mon.csv", 1500)
              write (4535,*)  bsn%name, prog
              write (4535,'(*(G0.3,:,","))') rescarb_hdr
              write (4535,'(*(G0.3,:,","))') rescarb_hdr_units
              write (9000,*) "HRU                       hru_rescarb_mon.csv"
            end if
        endif
        
     if (pco%nb_hru%y == "y") then
          call open_output_file(4532, "hru_rescarb_yr.txt", 1500)
          write (4532,*)  bsn%name, prog
          write (4532,*) rescarb_hdr
          write (4532,*) rescarb_hdr_units
          write (9000,*) "HRU                       hru_rescarb_yr.txt"
            if (pco%csvout == "y") then
              call open_output_file(4536, "hru_rescarb_yr.csv", 1500)
              write (4536,*)  bsn%name, prog
              write (4536,'(*(G0.3,:,","))') rescarb_hdr
              write (4536,'(*(G0.3,:,","))') rescarb_hdr_units
              write (9000,*) "HRU                       hru_rescarb_yr.csv"
            end if
        endif
        
        if (pco%nb_hru%a == "y") then
          call open_output_file(4533, "hru_rescarb_aa.txt", 1500)
          write (4533,*)  bsn%name, prog
          write (4533,*) rescarb_hdr
          write (4533,*) rescarb_hdr_units
          write (9000,*) "HRU                       hru_rescarb_aa.txt"
            if (pco%csvout == "y") then
              call open_output_file(4537, "hru_rescarb_aa.csv", 1500)
              write (4537,*)  bsn%name, prog
              write (4537,'(*(G0.3,:,","))') rescarb_hdr
              write (4537,'(*(G0.3,:,","))') rescarb_hdr_units
              write (9000,*) "HRU                       hru_rescarb_aa.csv"
            end if
        endif
        
 !!!NEW RESIDUE CARBON OUTPUT 
        
 !!!NEW PLANT CARBON OUTPUT
        if (pco%nb_hru%d == "y") then
          call open_output_file(4540, "hru_plcarb_day.txt", 1500)
          write (4540,*)  bsn%name, prog
          write (4540,*) plcarb_hdr
          write (4540,*) plcarb_hdr_units
              write (9000,*) "HRU                       hru_plcarb_day.txt"          
             if (pco%csvout == "y") then
              call open_output_file(4544, "hru_plcarb_day.csv", 1500)
              write (4544,*)  bsn%name, prog
              write (4544,'(*(G0.3,:,","))') plcarb_hdr
              write (4544,'(*(G0.3,:,","))') plcarb_hdr_units
              write (9000,*) "HRU                       hru_plcarb_day.csv"
            end if
        endif
        
        if (pco%nb_hru%m == "y") then
          call open_output_file(4541, "hru_plcarb_mon.txt", 1500)
          write (4541,*)  bsn%name, prog
          write (4541,*) plcarb_hdr
          write (4541,*) plcarb_hdr_units
          write (9000,*) "HRU                       hru_plcarb_mon.txt"
            if (pco%csvout == "y") then
              call open_output_file(4545, "hru_plcarb_mon.csv", 1500)
              write (4545,*)  bsn%name, prog
              write (4545,'(*(G0.3,:,","))') plcarb_hdr
              write (4545,'(*(G0.3,:,","))') plcarb_hdr_units
              write (9000,*) "HRU                       hru_plcarb_mon.csv"
            end if
        endif
        
     if (pco%nb_hru%y == "y") then
          call open_output_file(4542, "hru_plcarb_yr.txt", 1500)
          write (4542,*)  bsn%name, prog
          write (4542,*) plcarb_hdr
          write (4542,*) plcarb_hdr_units
          write (9000,*) "HRU                       hru_plcarb_yr.txt"
            if (pco%csvout == "y") then
              call open_output_file(4546, "hru_plcarb_yr.csv", 1500)
              write (4546,*)  bsn%name, prog
              write (4546,'(*(G0.3,:,","))') plcarb_hdr
              write (4546,'(*(G0.3,:,","))') plcarb_hdr_units
              write (9000,*) "HRU                       hru_plcarb_yr.csv"
            end if
        endif
        
        if (pco%nb_hru%a == "y") then
          call open_output_file(4543, "hru_plcarb_aa.txt", 1500)
          write (4543,*)  bsn%name, prog
          write (4543,*) plcarb_hdr
          write (4543,*) plcarb_hdr_units
          write (9000,*) "HRU                       hru_plcarb_aa.txt"
            if (pco%csvout == "y") then
              call open_output_file(4547, "hru_plcarb_aa.csv", 1500)
              write (4547,*)  bsn%name, prog
              write (4547,'(*(G0.3,:,","))') plcarb_hdr
              write (4547,'(*(G0.3,:,","))') plcarb_hdr_units
              write (9000,*) "HRU                       hru_plcarb_aa.csv"
            end if
        endif
        
 !!!NEW RESIDUE CARBON OUTPUT     
       
!!!NEW SOIL TRANSFORMATIONS CARBON OUTPUT
        if (pco%nb_hru%d == "y") then
          call open_output_file(4550, "hru_scf_day.txt", 1500)
          write (4550,*)  bsn%name, prog
          write (4550,*) hscf_hdr
          write (4550,*) hscf_hdr_units
              write (9000,*) "HRU                       hru_scf_day.txt"          
             if (pco%csvout == "y") then
              call open_output_file(4554, "hru_scf_day.csv", 1500)
              write (4554,*)  bsn%name, prog
              write (4554,'(*(G0.3,:,","))') hscf_hdr
              write (4554,'(*(G0.3,:,","))') hscf_hdr_units
              write (9000,*) "HRU                       hru_plcarb_day.csv"
            end if
        endif
        
        if (pco%nb_hru%m == "y") then
          call open_output_file(4551, "hru_scf_mon.txt", 1500)
          write (4551,*)  bsn%name, prog
          write (4551,*) hscf_hdr
          write (4551,*) hscf_hdr_units
          write (9000,*) "HRU                       hru_scf_mon.txt"
            if (pco%csvout == "y") then
              call open_output_file(4555, "hru_scf_mon.csv", 1500)
              write (4555,*)  bsn%name, prog
              write (4555,'(*(G0.3,:,","))') hscf_hdr
              write (4555,'(*(G0.3,:,","))') hscf_hdr_units
              write (9000,*) "HRU                       hru_scf_mon.csv"
            end if
        endif
        
     if (pco%nb_hru%y == "y") then
          call open_output_file(4552, "hru_scf_yr.txt", 1500)
          write (4552,*)  bsn%name, prog
          write (4552,*) hscf_hdr
          write (4552,*) hscf_hdr_units
          write (9000,*) "HRU                       hru_scf_yr.txt"
            if (pco%csvout == "y") then
              call open_output_file(4556, "hru_scf_yr.csv", 1500)
              write (4556,*)  bsn%name, prog
              write (4556,'(*(G0.3,:,","))') hscf_hdr
              write (4556,'(*(G0.3,:,","))') hscf_hdr_units
              write (9000,*) "HRU                       hru_scf_yr.csv"
            end if
        endif
        
        if (pco%nb_hru%a == "y") then
          call open_output_file(4553, "hru_scf_aa.txt", 1500)
          write (4553,*)  bsn%name, prog
          write (4553,*) hscf_hdr
          write (4553,*) hscf_hdr_units
          write (9000,*) "HRU                       hru_scf_aa.txt"
            if (pco%csvout == "y") then
              call open_output_file(4557, "hru_scf_aa.csv", 1500)
              write (4557,*)  bsn%name, prog
              write (4557,'(*(G0.3,:,","))') hscf_hdr
              write (4557,'(*(G0.3,:,","))') hscf_hdr_units
              write (9000,*) "HRU                       hru_scf_aa.csv"
            end if
        endif
        
 !!!NEW SOIL TRANSFORMATIONS CARBON OUTPUT     
       
!!! NEW SOILC_STAT/RESC_STAT/PLC_STAT CARBON OUTPUT FILES

        !! write carbon in soil by layer
        if (pco%cb_hru%d == "y" .or. pco%cb_hru%m == "y"  .or. pco%cb_hru%y == "y" .or. & 
            pco%cb_hru%d == "l" .or. pco%cb_hru%m == "l"  .or. pco%cb_hru%y == "l") then
          call open_output_file(4548, "hru_cbn_lyr.txt", 1500)
          write (4548,*)  bsn%name, prog, "total soil carbon (Mg/ha) by layer depth in mm"
          write (9000,*) "HRU                       hru_cbn_lyr.txt"
          if (pco%csvout == "y") then
            call open_output_file(4549, "hru_cbn_lyr.csv", 1500)
            write (4549,*)  bsn%name, prog, "total soil carbon (Mg/ha) by layer depth in mm"
            write (9000,*) "HRU                       hru_cbn_lyr.csv"
          end if
    
          call open_output_file(4558, "hru_seq_lyr.txt", 1500)
          write (4558,*)  bsn%name, prog, "total sequestered soil carbon (Mg/ha) by layer depth in mm"
          write (9000,*) "HRU                       hru_seq_lyr.txt"
          if (pco%csvout == "y") then
            call open_output_file(4559, "hru_seq_lyr.csv", 1500)
            write (4559,*)  bsn%name, prog, "total sequestered soil carbon (Mg/ha) by layer depth in mm" 
            write (9000,*) "HRU                       hru_seq_lyr.csv"
          end if
    
          !! write carbon in soil, plant, and residue
          call open_output_file(4560, "hru_plc_stat.txt", 1500)
          write (4560,*)  bsn%name, prog
          write (4560,*) plc_hdr
          write (4560,*) plc_hdr_units
          write (9000,*) "HRU                       hru_plc_stat.txt"
          if (pco%csvout == "y") then
            call open_output_file(4563, "hru_plc_stat.csv", 1500)
            write (4563,*)  bsn%name, prog
            write (4563,'(*(G0.3,:,","))') plc_hdr
            write (4563,'(*(G0.3,:,","))') plc_hdr_units
            write (9000,*) "HRU                       hru_plc_stat.csv"
          end if
    
          if (bsn_cc%cswat == 2) then
            call open_output_file(4561, "hru_rsdc_stat.txt", 1500)
            write (4561,*)  bsn%name, prog
            write (4561,*) rsdc_hdr
            write (4561,*) rsdc_hdr_units
            write (9000,*) "HRU                       hru_rsdc_stat.txt"
            if (pco%csvout == "y") then
              call open_output_file(4564, "hru_rsdc_stat.csv", 1500)
              write (4564,*)  bsn%name, prog
              write (4564,'(*(G0.3,:,","))') rsdc_hdr
              write (4564,'(*(G0.3,:,","))') rsdc_hdr_units
              write (9000,*) "HRU                       hru_rsdc_stat.csv"
            end if
          
            call open_output_file(4562, "hru_soilc_stat.txt", 1500)
            write (4562,*)  bsn%name, prog
            write (4562,*) soilc_hdr
            write (4562,*) soilc_hdr_units
            write (9000,*) "HRU                       hru_soilc_stat.txt"
            if (pco%csvout == "y") then
              call open_output_file(4565, "hru_soilc_stat.csv", 1500)
              write (4565,*)  bsn%name, prog
              write (4565,'(*(G0.3,:,","))') soilc_hdr
              write (4565,'(*(G0.3,:,","))') soilc_hdr_units
              write (9000,*) "HRU                       hru_soilc_stat.csv"
            end if

            call open_output_file(4567, "hru_cflux_stat.txt", 1500)
            write (4567,*)  bsn%name, prog
            write (4567,*) soil_org_flux_hdr
            write (4567,*) soil_org_flux_hdr_units
            write (9000,*) "HRU                       hru_cflux_stat.txt"
            if (pco%csvout == "y") then
              call open_output_file(4568, "hru_cflux_stat.csv", 1500)
              write (4568,*)  bsn%name, prog
              write (4568,'(*(G0.3,:,","))') soil_org_flux_hdr
              write (4568,'(*(G0.3,:,","))') soil_org_flux_hdr_units
              write (9000,*) "HRU                       hru_cflux_stat.csv"
            end if
          
            call open_output_file(4570, "hru_soilcarb_mb_stat.txt", 1500)
            write (4570,*)  bsn%name, prog
            write (4570,*) soil_mb_hdr
            write (4570,*) soil_mb_units
            write (9000,*) "HRU                       hru_soilcarb_mb_stat.txt"
            if (pco%csvout == "y") then
              call open_output_file(4571, "hru_soilcarb_mb_stat.csv", 1500)
              write (4571,*)  bsn%name, prog
              write (4571,'(*(G0.3,:,","))') soil_mb_hdr
              write (4571,'(*(G0.3,:,","))') soil_mb_units
              write (9000,*) "HRU                       hru_soilcarb_mb_stat.csv"
            end if

            call open_output_file(4572, "hru_cpool_stat.txt", 1500)
            write (4572,*)  bsn%name, prog
            write (4572,*) cpool_hdr
            write (4572,*) cpool_units
            write (9000,*) "HRU                       hru_cpool_stat.txt"
            if (pco%csvout == "y") then
              call open_output_file(4573, "hru_cpool_stat.csv", 1500)
              write (4573,*)  bsn%name, prog
              write (4573,'(*(G0.3,:,","))') cpool_hdr
              write (4573,'(*(G0.3,:,","))') cpool_units
              write (9000,*) "HRU                       hru_cpool_stat.csv"
            end if

            call open_output_file(4582, "hru_n_p_pool_stat.txt", 1500)
            write (4582,*)  bsn%name, prog
            write (4582,*) n_p_pool_hdr
            write (4582,*) n_p_pool_units
            write (9000,*) "HRU                       hru_n_p_pool_stat.txt"
            if (pco%csvout == "y") then
              call open_output_file(4583, "hru_n_p_pool_stat.csv", 1500)
              write (4583,*)  bsn%name, prog
              write (4583,'(*(G0.3,:,","))') n_p_pool_hdr
              write (4583,'(*(G0.3,:,","))') n_p_pool_units
              write (9000,*) "HRU                       hru_n_p_pool_stat.csv"
            end if

            call open_output_file(4580, "hru_org_trans_vars.txt", 1500)
            write (4580,*)  bsn%name, prog
            write (4580,*) org_trans_hdr
            write (4580,*) org_trans_units
            write (9000,*) "HRU                       hru_org_trans_vars.txt"
            if (pco%csvout == "y") then
              call open_output_file(4581, "hru_org_trans_vars.csv", 1500)
              write (4581,*)  bsn%name, prog
              write (4581,'(*(G0.3,:,","))') org_trans_hdr
              write (4581,'(*(G0.3,:,","))') org_trans_units
              write (9000,*) "HRU                       hru_org_trans_vars.csv"
            end if

          endif
        endif

        !! write carbon variables headers to hru_carbvars
        if (bsn_cc%cswat == 2) then
          if (pco%cb_vars_hru%d == "y" .or. pco%cb_vars_hru%m == "y"  .or. pco%cb_vars_hru%y == "y" ) then
            call open_output_file(4574, "hru_carbvars.txt", 1500)
            write (4574,*)  bsn%name, prog
            write (4574,*) carbvars_hdr
            write (9000,*) "HRU                       hru_carbvars.txt"
            if (pco%csvout == "y") then
              call open_output_file(4575, "hru_carbvars.csv", 1500)
              write (4575,*)  bsn%name, prog
              write (4575,'(*(G0.3,:,","))') carbvars_hdr
              write (9000,*) "HRU                       hru_carbvars.csv"
            end if
          endif
        endif

        !! write org_allo variable headers to hru_org_allo_vars
        if (bsn_cc%cswat == 2) then
          if (pco%cb_vars_hru%d == "y" .or. pco%cb_vars_hru%m == "y"  .or. pco%cb_vars_hru%y == "y" ) then
            call open_output_file(4576, "hru_org_allo_vars.txt", 1500)
            write (4576,*)  bsn%name, prog
            write (4576,*)  org_allow_hdr
            write (9000,*) "HRU                       hru_org_allo_vars.txt"
            if (pco%csvout == "y") then
              call open_output_file(4577, "hru_org_allo_vars.csv", 1500)
              write (4577,*)  bsn%name, prog
              write (4577,'(*(G0.3,:,","))') org_allow_hdr
              write (9000,*) "HRU                       hru_org_allo_vars.csv"
            end if
          endif
        endif

        !! write org_ratio variable headers to hru_org_ratio_vars
        if (bsn_cc%cswat == 2) then
          if (pco%cb_vars_hru%d == "y" .or. pco%cb_vars_hru%m == "y"  .or. pco%cb_vars_hru%y == "y" ) then
            call open_output_file(4578, "hru_org_ratio_vars.txt", 1500)
            write (4578,*)  bsn%name, prog
            write (4578,*)  org_ratio_hdr
            write (9000,*) "HRU                       hru_org_ratio_vars.txt"
            if (pco%csvout == "y") then
              call open_output_file(4579, "hru_org_ratio_vars.csv", 1500)
              write (4579,*)  bsn%name, prog
              write (4579,'(*(G0.3,:,","))') org_ratio_hdr
              write (9000,*) "HRU                       hru_org_ratio_vars.csv"
            end if
          endif
        endif

        !! write end of simulation soil properties headers to hru_endsim_soil_prop
        if (bsn_cc%cswat == 2) then
          if (pco%cb_hru%d /= "n" .or. pco%cb_hru%m /= "n" .or. pco%cb_hru%y /= "n" .or. pco%cb_hru%a /= "n") then
            call open_output_file(4584, "hru_endsim_soil_prop.txt", 1500)
            write (4584,*)  bsn%name, prog
            write (4584,*)  endsim_soil_prop_hdr
            write (9000,*) "HRU                       hru_endsim_soil_prop.txt"
            if (pco%csvout == "y") then
              call open_output_file(4585, "hru_endsim_soil_prop.csv", 1500)
              write (4585,*)  bsn%name, prog
              write (4585,'(*(G0.3,:,","))') endsim_soil_prop_hdr
              write (9000,*) "HRU                       hru_endsim_soil_prop.csv"
            end if
          endif
        endif

        !! write beginning of simulation soil properties headers to hru_begsim_soil_prop
        if (bsn_cc%cswat == 2) then
          if (pco%cb_hru%d /= "n" .or. pco%cb_hru%m /= "n" .or. pco%cb_hru%y /= "n" .or. pco%cb_hru%a /= "n") then
            call open_output_file(4586, "hru_begsim_soil_prop.txt", 1500)
            write (4586,*)  bsn%name, prog
            write (4586,*)  endsim_soil_prop_hdr  ! begsim can use the same header as endsim 
            write (9000,*) "HRU                       hru_begsim_soil_prop.txt"
            if (pco%csvout == "y") then
              call open_output_file(4587, "hru_begsim_soil_prop.csv", 1500)
              write (4587,*)  bsn%name, prog
              write (4587,'(*(G0.3,:,","))') endsim_soil_prop_hdr ! begsim can use the same header as endsim 
              write (9000,*) "HRU                       hru_begsim_soil_prop.csv"
            end if
            ! Write out begining adjusted soil properties if any value of cb_hru is not "n"
              if (pco%cb_hru%d /= "n" .or. pco%cb_hru%m /= "n" .or. pco%cb_hru%y /= "n" .or. pco%cb_hru%a /= "n") then
                call soil_nutcarb_write(" b")    ! Outputs beginning soil values to hru_begsim_soil_prop.txt/csv
              endif


          endif
        endif
        
 !! NEW BASIN CARBON ALL OUTPUT FILE
        
        call open_output_file(4566, "basin_carbon_all.txt", 1500)
        if (pco%nb_hru%a == "y") then
          write (4566,*)  bsn%name, prog
          write (4566,*) bsn_carb_hdr
          write (4566,*) bsn_carb_hdr_units
          write (9000,*) "BASIN                     basin_carbon_all.txt"
        end if
          
 !! NEW BASIN CARBON ALL OUTPUT FILE
        
!!!  HRU - Losses
        if (pco%ls_hru%d == "y") then
          call open_output_file(2030, "hru_ls_day.txt", 1500)
          write (2030,*) bsn%name, prog
          write (2030,*) ls_hdr    !! hru
          write (2030,*) ls_hdr_units
          write (9000,*) "HRU                       hru_ls_day.txt"
            if (pco%csvout == "y") then
              call open_output_file(2034, "hru_ls_day.csv", 1500)
              write (2034,*) bsn%name, prog
              write (2034,'(*(G0.3,:,","))') ls_hdr    !! hru
              write (2034,'(*(G0.3,:,","))') ls_hdr_units
              write (9000,*) "HRU                       hru_ls_day.csv"
            end if 
        endif
        
                
!!!  HRU - New nutcarb gain loss file
        if (pco%ls_hru%d == "y") then
          call open_output_file(3341, "hru_nut_carb_gl_day.txt", 1500)
          write (3341,*) bsn%name, prog
          write (3341,*) ls_hdr1    !! hru
          write (3341,*) ls_hdr_units1
          write (9000,*) "HRU                       hru_nut_carb_gl_day.txt"
            if (pco%csvout == "y") then
              call open_output_file(3342, "hru_nut_carb_gl_day.csv", 1500)
              write (3342,*) bsn%name, prog
              write (3342,'(*(G0.3,:,","))') ls_hdr1    !! hru
              write (3342,'(*(G0.3,:,","))') ls_hdr_units1
              write (9000,*) "HRU                       hru_nut_carb_gl_day.csv"
            end if 
        endif

        if (pco%ls_hru%m == "y") then
          call open_output_file(3343, "hru_nut_carb_gl_mon.txt", 1500)
          write (3343,*) bsn%name, prog
          write (3343,*) ls_hdr1    !! hru
          write (3343,*) ls_hdr_units1
          write (9000,*) "HRU                       hru_nut_carb_gl_mon.txt"
            if (pco%csvout == "y") then
              call open_output_file(3344, "hru_nut_carb_gl_mon.csv", 1500)
              write (3344,*) bsn%name, prog
              write (3344,'(*(G0.3,:,","))') ls_hdr1    !! hru
              write (3344,'(*(G0.3,:,","))') ls_hdr_units1
              write (9000,*) "HRU                       hru_nut_carb_gl_mon.csv"
            end if 
        endif
        
        if (pco%ls_hru%y == "y") then
          call open_output_file(3345, "hru_nut_carb_gl_yr.txt", 1500)
          write (3345,*) bsn%name, prog
          write (3345,*) ls_hdr1    !! hru
          write (3345,*) ls_hdr_units1
          write (9000,*) "HRU                       hru_nut_carb_gl_yr.txt"
            if (pco%csvout == "y") then
              call open_output_file(3346, "hru_nut_carb_gl_yr.csv", 1500)
              write (3346,*) bsn%name, prog
              write (3346,'(*(G0.3,:,","))') ls_hdr1    !! hru
              write (3346,'(*(G0.3,:,","))') ls_hdr_units1
              write (9000,*) "HRU                       hru_nut_carb_gl_yr.csv"
            end if 
        endif
        
        if (pco%ls_hru%a == "y") then
          call open_output_file(3347, "hru_nut_carb_gl_aa.txt", 1500)
          write (3347,*) bsn%name, prog
          write (3347,*) ls_hdr1    !! hru
          write (3347,*) ls_hdr_units1
          write (9000,*) "HRU                       hru_nut_carb_gl_aa.txt"
            if (pco%csvout == "y") then
              call open_output_file(3348, "hru_nut_carb_gl_aa.csv", 1500)
              write (3348,*) bsn%name, prog
              write (3348,'(*(G0.3,:,","))') ls_hdr1    !! hru
              write (3348,'(*(G0.3,:,","))') ls_hdr_units1
              write (9000,*) "HRU                       hru_nut_carb_gl_aa.csv"
            end if 
        endif
!!!  HRU - New nutcarb gain loss file       
           
       if (pco%ls_hru%m == "y") then
        call open_output_file(2031, "hru_ls_mon.txt", 1500)
        write (2031,*) bsn%name, prog
        write (2031,*) ls_hdr  !! hru 
        write (2031,*) ls_hdr_units
        write (9000,*) "HRU                       hru_ls_mon.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2035, "hru_ls_mon.csv", 1500)
            write (2035,*) bsn%name, prog
            write (2035,'(*(G0.3,:,","))') ls_hdr  !! hru
            write (2035,'(*(G0.3,:,","))') ls_hdr_units
            write (9000,*) "HRU                       hru_ls_mon.csv"
          end if
       endif
          
        if (pco%ls_hru%y == "y") then
          call open_output_file(2032, "hru_ls_yr.txt", 1500)
          write (2032,*) bsn%name, prog
          write (2032,*) ls_hdr    !! hru
          write (2032,*) ls_hdr_units
          write (9000,*) "HRU                       hru_ls_yr.txt"
            if (pco%csvout == "y") then
              call open_output_file(2036, "hru_ls_yr.csv", 1500)
              write (2036,*) bsn%name, prog
              write (2036,'(*(G0.3,:,","))') ls_hdr    !! hru
              write (2036,'(*(G0.3,:,","))') ls_hdr_units
              write (9000,*) "HRU                       hru_ls_yr.csv"
            end if 
        endif
        
       if (pco%ls_hru%a == "y") then
        call open_output_file(2033, "hru_ls_aa.txt", 1500)
        write (2033,*) bsn%name, prog
        write (2033,*) ls_hdr  !! hru
        write (2033,*) ls_hdr_units
        write (9000,*) "HRU                       hru_ls_aa.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2037, "hru_ls_aa.csv", 1500)
            write (2037,*) bsn%name, prog
            write (2037,'(*(G0.3,:,","))') ls_hdr  !! hru
            write (2037,'(*(G0.3,:,","))') ls_hdr_units
            write (9000,*) "HRU                       hru_ls_aa.csv"
          end if 
       end if

!!!  HRU - Plant/Weather
        if (pco%pw_hru%d == "y") then
          call open_output_file(2040, "hru_pw_day.txt", 1500)
          write (2040,*) bsn%name, prog
          write (2040,*) pw_hdr  !! hru 
          write (2040,*) pw_hdr_units
          write (9000,*) "HRU                       hru_pw_day.txt"
            if (pco%csvout == "y") then 
              call open_output_file(2044, "hru_pw_day.csv", 1500)
              write (2044,*) bsn%name, prog
              write (2044,'(*(G0.3,:,","))') pw_hdr  !! hru
              write (2044,'(*(G0.3,:,","))') pw_hdr_units
              write (9000,*) "HRU                       hru_pw_day.csv"
            end if 
        endif
        
      if (pco%pw_hru%m == "y") then
        call open_output_file(2041, "hru_pw_mon.txt", 1500)
        write (2041,*) bsn%name, prog
        write (2041,*) pw_hdr  !! hru
        write (2041,*) pw_hdr_units
        write (9000,*) "HRU                       hru_pw_mon.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2045, "hru_pw_mon.csv", 1500)
            write (2045,*) bsn%name, prog
            write (2045,'(*(G0.3,:,","))') pw_hdr  !! hru
            write (2045,'(*(G0.3,:,","))') pw_hdr_units
            write (9000,*) "HRU                       hru_pw_mon.csv"
          end if 
      endif
      
        if (pco%pw_hru%y == "y") then
          call open_output_file(2042, "hru_pw_yr.txt", 1500)
          write (2042,*) bsn%name, prog                  
          write (2042,*) pw_hdr  !! hru
          write (2042,*) pw_hdr_units
          write (9000,*) "HRU                       hru_pw_yr.txt"
            if (pco%csvout == "y") then 
              call open_output_file(2046, "hru_pw_yr.csv", 1500)
              write (2046,*) bsn%name, prog
              write (2046,'(*(G0.3,:,","))') pw_hdr  !! hru
              write (2046,'(*(G0.3,:,","))') pw_hdr_units
              write (9000,*) "HRU                       hru_pw_yr.csv"
            end if 
        endif
        
       if (pco%pw_hru%a == "y") then
        call open_output_file(2043, "hru_pw_aa.txt", 1500)
        write (2043,*) bsn%name, prog
        write (2043,*) pw_hdr  !! hru
        write (2043,*) pw_hdr_units
        write (9000,*) "HRU                       hru_pw_aa.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2047, "hru_pw_aa.csv", 1500)
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
          call open_output_file(2300, "hru-lte_wb_day.txt", 1500)
          write (2300,*) bsn%name, prog
          write (2300,*) wb_hdr  !! swat-deg
          write (2300,*) wb_hdr_units
          write (9000,*) "SWAT-DEG                  hru-lte_wb_day.txt"
            if (pco%csvout == "y") then 
              call open_output_file(2304, "hru-lte_wb_day.csv", 1500)
              write (2304,*) bsn%name, prog
              write (2304,'(*(G0.3,:,","))') wb_hdr  !! swat-deg
              write (2304,'(*(G0.3,:,","))') wb_hdr_units
              write (9000,*) "SWAT-DEG                  hru-lte_wb_day.csv"
            end if 
        endif

                
      if (pco%wb_sd%m == "y") then
      call open_output_file(2301, "hru-lte_wb_mon.txt", 1500)
        write (2301,*) bsn%name, prog
        write (2301,*) wb_hdr   !! swat deg 
        write (2301,*) wb_hdr_units
        write (9000,*) "SWAT-DEG                  hru-lte_wb_mon.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2305, "hru-lte_wb_mon.csv", 1500)
            write (2305,*) bsn%name, prog
            write (2305,'(*(G0.3,:,","))') wb_hdr   !! swat deg
            write (2305,'(*(G0.3,:,","))') wb_hdr_units
            write (9000,*) "SWAT-DEG                  hru-lte_wb_mon.csv"
          end if
      end if
          
       
     if (sp_ob%hru_lte > 0) then   
        if (pco%wb_sd%y == "y") then
          call open_output_file(2302, "hru-lte_wb_yr.txt", 1500)
          write (2302,*) bsn%name, prog
          write (2302,*) wb_hdr  !! swat-deg
          write (2302,*) wb_hdr_units
          write (9000,*) "SWAT-DEG                  hru-lte_wb_yr.txt"
            if (pco%csvout == "y") then 
              call open_output_file(2306, "hru-lte_wb_yr.csv", 1500)
              write (2306,*) bsn%name, prog
              write (2306,'(*(G0.3,:,","))') wb_hdr  !! swat-deg
              write (2306,'(*(G0.3,:,","))') wb_hdr_units
              write (9000,*) "SWAT-DEG                  hru-lte_wb_yr.csv"
            end if 
        endif
      end if 
        
        
      if (pco%wb_sd%a == "y") then
        call open_output_file(2303, "hru-lte_wb_aa.txt", 1500)
        write (2303,*) bsn%name, prog
        write (2303,*) wb_hdr   !! swat deg
        write (2303,*) wb_hdr_units
        write (9000,*) "SWAT-DEG                  hru-lte_wb_aa.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2307, "hru-lte_wb_aa.csv", 1500)
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
!         call open_output_file(4025, "nutbal_sd.csv", 1500)  !! no nuts in SWAT-DEG
!         write (4025,*) nb_hdr
!         call open_output_file(4026, "nutbal_aa_sd.csv", 1500)
!         write (4026,*) nb_hdr
!       end if 

!!!  SWAT-DEG - Losses
        if (pco%ls_sd%d == "y") then
          call open_output_file(2440, "hru-lte_ls_day.txt", 1500)
          write (2440,*) bsn%name, prog
          write (2440,*) ls_hdr    !! swat-deg
          write (2440,*) ls_hdr_units
          write (9000,*) "SWAT-DEG                  hru-lte_ls_day.txt"
            if (pco%csvout == "y") then 
              call open_output_file(2444, "hru-lte_ls_day.csv", 1500)
              write (2444,*) bsn%name, prog
              write (2444,'(*(G0.3,:,","))') ls_hdr    !! swat-deg 
              write (2444,'(*(G0.3,:,","))') ls_hdr_units
              write (9000,*) "SWAT-DEG                  hru-lte_ls_day.csv"
            end if 
        endif
        
      if (pco%ls_sd%m == "y") then
        call open_output_file(2441, "hru-lte_ls_mon.txt", 1500)
        write (2441,*) bsn%name, prog
        write (2441,*) ls_hdr  !! swat-deg
        write (2441,*) ls_hdr_units
        write (9000,*) "SWAT-DEG                  hru-lte_ls_mon.txt"
        if (pco%csvout == "y") then 
          call open_output_file(2445, "hru-lte_ls_mon.csv", 1500)
          write (2445,*) bsn%name, prog
          write (2445,'(*(G0.3,:,","))') ls_hdr  !! swat-deg
          write (2445,'(*(G0.3,:,","))') ls_hdr_units
          write (9000,*) "SWAT-DEG                  hru-lte_ls_mon.csv"
        end if
      end if
        
        if (pco%ls_sd%y == "y") then
          call open_output_file(2442, "hru-lte_ls_yr.txt", 1500)
          write (2442,*) bsn%name, prog
          write (2442,*) ls_hdr    !! swat-deg
          write (2442,*) ls_hdr_units
          write (9000,*) "SWAT-DEG                  hru-lte_ls_yr.txt"
            if (pco%csvout == "y") then 
              call open_output_file(2446, "hru-lte_ls_yr.csv", 1500)
              write (2446,*) bsn%name, prog
              write (2446,'(*(G0.3,:,","))') ls_hdr    !! swat-deg 
              write (2446,'(*(G0.3,:,","))') ls_hdr_units
              write (9000,*) "SWAT-DEG                  hru-lte_ls_yr.csv"
            end if 
        endif
        
      if (pco%ls_sd%a == "y") then
         call open_output_file(2443, "hru-lte_ls_aa.txt", 1500)
         write (2443,*) bsn%name, prog
         write (2443,*) ls_hdr  !! swat-deg
         write (2443,*) ls_hdr_units
         write (9000,*) "SWAT-DEG                  hru-lte_ls_aa.txt"
        if (pco%csvout == "y") then 
          call open_output_file(2447, "hru-lte_ls_aa.csv", 1500)
          write (2447,*) bsn%name, prog
          write (2447,'(*(G0.3,:,","))') ls_hdr  !! swat-deg
          write (2447,'(*(G0.3,:,","))') ls_hdr_units
          write (9000,*) "SWAT-DEG                  hru-lte_ls_aa.csv"
        end if
      end if 
        
        
!!!  SWAT-DEG - Plant/Weather
        if (pco%pw_sd%d == "y") then
          call open_output_file(2460, "hru-lte_pw_day.txt", 1500)
          write (2460,*) bsn%name, prog
          write (2460,*) pw_hdr  !! swat-deg
          write (2460,*) pw_hdr_units
          write (9000,*) "SWAT-DEG                  hru-lte_pw_day.txt"
           if (pco%csvout == "y") then 
             call open_output_file(2464, "hru-lte_pw_day.csv", 1500)
             write (2464,*) bsn%name, prog
             write (2464,'(*(G0.3,:,","))') pw_hdr  !! swat-deg
             write (2464,'(*(G0.3,:,","))') pw_hdr_units
             write (9000,*) "SWAT-DEG                  hru-lte_pw_day.csv"
           end if
        endif
        
        if (pco%pw_sd%m == "y") then
          call open_output_file(2461, "hru-lte_pw_mon.txt", 1500)
          write (2461,*) bsn%name, prog
          write (2461,*) pw_hdr !! swat-deg
          write (2461,*) pw_hdr_units
          write (9000,*) "SWAT-DEG                  hru-lte_pw_mon.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2465, "hru-lte_pw_mon.csv", 1500)
            write (2465,*) bsn%name, prog
            write (2465,'(*(G0.3,:,","))') pw_hdr !! swat-deg
            write (2465,'(*(G0.3,:,","))') pw_hdr_units
            write (9000,*) "SWAT-DEG                  hru-lte_pw_mon.csv"
          end if
        end if

       if (pco%pw_sd%y == "y") then
          call open_output_file(2462, "hru-lte_pw_yr.txt", 1500)
          write (2462,*) bsn%name, prog
          write (2462,*) pw_hdr  !! swat-deg
          write (2462,*) pw_hdr_units
          write (9000,*) "SWAT-DEG                  hru-lte_pw_yr.txt"
           if (pco%csvout == "y") then 
             call open_output_file(2466, "hru-lte_pw_yr.csv", 1500)
             write (2466,*) bsn%name, prog
             write (2466,'(*(G0.3,:,","))') pw_hdr  !! swat-deg
             write (2466,'(*(G0.3,:,","))') pw_hdr_units
             write (9000,*) "SWAT-DEG                  hru-lte_pw_yr.csv"
           end if
       endif
        
      if (pco%pw_sd%a == "y") then    !!!
        call open_output_file(2463, "hru-lte_pw_aa.txt", 1500)
        write (2463,*) bsn%name, prog
        write (2463,*) pw_hdr !! swat-deg
        write (2463,*) pw_hdr_units
        write (9000,*) "SWAT-DEG                  hru-lte_pw_aa.txt"
         if (pco%csvout == "y") then 
          call open_output_file(2467, "hru-lte_pw_aa.csv", 1500)
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
          call open_output_file(2140, "lsunit_wb_day.txt", 1500)
          write (2140,*) bsn%name, prog
          write (2140,*) wb_hdr  !! subbasin
          write (2140,*) wb_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_wb_day.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2144, "lsunit_wb_day.csv", 1500)
            write (2144,*) bsn%name, prog
            write (2144,'(*(G0.3,:,","))') wb_hdr  !! subbasin
            write (2144,'(*(G0.3,:,","))') wb_hdr_units
            write (9000,*) "ROUTING_UNIT              lsunit_wb_day.csv"
          end if 
        endif
    
        
       if (pco%wb_lsu%m == "y") then
        call open_output_file(2141, "lsunit_wb_mon.txt", 1500)
        write (2141,*) bsn%name, prog
        write (2141,*) wb_hdr  !! subbasin
        write (2141,*) wb_hdr_units
        write (9000,*) "ROUTING_UNIT              lsunit_wb_mon.txt"
          if (pco%csvout == "y") then
            call open_output_file(2145, "lsunit_wb_mon.csv", 1500)
            write (2145,*) bsn%name, prog
            write (2145,'(*(G0.3,:,","))') wb_hdr   !! subbasin
            write (2145,'(*(G0.3,:,","))') wb_hdr_units
            write (9000,*) "ROUTING_UNIT              lsunit_wb_mon.csv"
          end if
        end if 

     if (sp_ob%ru > 0) then   
        if (pco%wb_lsu%y == "y") then
          call open_output_file(2142, "lsunit_wb_yr.txt", 1500)
          write (2142,*) bsn%name, prog
          write (2142,*) wb_hdr  !! subbasin
          write (2142,*) wb_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_wb_yr.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2146, "lsunit_wb_yr.csv", 1500)
            write (2146,*) bsn%name, prog
            write (2146,'(*(G0.3,:,","))') wb_hdr  !! subbasin
            write (2146,'(*(G0.3,:,","))') wb_hdr_units
            write (9000,*) "ROUTING_UNIT              lsunit_wb_yr.csv"
          end if 
        endif
     end if
        
       if (pco%wb_lsu%a == "y") then
         call open_output_file(2143, "lsunit_wb_aa.txt", 1500)
         write (2143,*) bsn%name, prog
         write (2143,*) wb_hdr   !! subbasin
         write (2143,*) wb_hdr_units
         write (9000,*) "ROUTING_UNIT              lsunit_wb_aa.txt"
          if (pco%csvout == "y") then
           call open_output_file(2147, "lsunit_wb_aa.csv", 1500)
           write (2147,*) bsn%name, prog
           write (2147,'(*(G0.3,:,","))') wb_hdr   !! subbasin
           write (2147,'(*(G0.3,:,","))') wb_hdr_units
           write (9000,*) "ROUTING_UNIT              lsunit_wb_aa.csv"
          end if
       end if
        
!!!  Nutrient Balance
        if (pco%nb_lsu%d == "y") then
          call open_output_file(2150, "lsunit_nb_day.txt", 1500)
          write (2150,*) bsn%name, prog
          write (2150,*) nb_hdr
          write (2150,*) nb_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_nb_day.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2154, "lsunit_nb_day.csv", 1500)
            write (2154,*) bsn%name, prog
            write (2154,'(*(G0.3,:,","))') nb_hdr
            write (2154,'(*(G0.3,:,","))') nb_hdr_units
            write (9000,*) "ROUTING_UNIT              lsunit_nb_day.csv"
          end if 
        endif
        
        if (pco%nb_lsu%m == "y") then
        call open_output_file(2151, "lsunit_nb_mon.txt", 1500)
        write (2151,*) bsn%name, prog
        write (2151,*) nb_hdr
        write (2151,*) nb_hdr_units
        write (9000,*) "ROUTING_UNIT              lsunit_nb_mon.txt"
          if (pco%csvout == "y") then
            call open_output_file(2155, "lsunit_nb_mon.csv", 1500)
            write (2155,*) bsn%name, prog
            write (2155,'(*(G0.3,:,","))') nb_hdr
            write (2155,'(*(G0.3,:,","))') nb_hdr_units
            write (9000,*) "ROUTING_UNIT              lsunit_nb_mon.csv"
          end if
        end if
        
        if (pco%nb_lsu%y == "y") then
          call open_output_file(2152, "lsunit_nb_yr.txt", 1500)
          write (2152,*) bsn%name, prog
          write (2152,*) nb_hdr
          write (2152,*) nb_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_nb_yr.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2156, "lsunit_nb_yr.csv", 1500)
            write (2156,*) bsn%name, prog
            write (2156,'(*(G0.3,:,","))') nb_hdr
            write (2156,'(*(G0.3,:,","))') nb_hdr_units
            write (9000,*) "ROUTING_UNIT              lsunit_nb_yr.csv"
          end if 
        endif
        
        if (pco%nb_lsu%a == "y") then
        call open_output_file(2153, "lsunit_nb_aa.txt", 1500)
        write (2153,*) bsn%name, prog
        write (2153,*) nb_hdr
        write (2153,*) nb_hdr_units
        write (9000,*) "ROUTING_UNIT              lsunit_nb_aa.txt"
          if (pco%csvout == "y") then
            call open_output_file(2157, "lsunit_nb_aa.csv", 1500)
            write (2157,*) bsn%name, prog
            write (2157,'(*(G0.3,:,","))') nb_hdr
            write (2157,'(*(G0.3,:,","))') nb_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_nb_aa.csv"
          end if 
        end if 

!!!  Losses
        if (pco%ls_lsu%d == "y") then
          call open_output_file(2160, "lsunit_ls_day.txt", 1500)
          write (2160,*) bsn%name, prog
          write (2160,*) ls_hdr 
          write (2160,*) ls_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_ls_day.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2164, "lsunit_ls_day.csv", 1500)
            write (2164,*) bsn%name, prog
            write (2164,'(*(G0.3,:,","))') ls_hdr    !! subbasin
            write (2164,'(*(G0.3,:,","))') ls_hdr_units
            write (9000,*) "ROUTING_UNIT              lsunit_ls_day.csv"
          end if 
        endif
        
      if (pco%ls_lsu%m == "y") then
        call open_output_file(2161, "lsunit_ls_mon.txt", 1500)
        write (2161,*) bsn%name, prog
        write (2161,*) ls_hdr 
        write (2161,*) ls_hdr_units
        write (9000,*) "ROUTING_UNIT              lsunit_ls_mon.txt"
        if (pco%csvout == "y") then 
          call open_output_file(2165, "lsunit_ls_mon.csv", 1500)
          write (2165,*) bsn%name, prog
          write (2165,'(*(G0.3,:,","))') ls_hdr  !! subbasin 
          write (2165,'(*(G0.3,:,","))') ls_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_ls_mon.csv"
        end if 
      end if 
        
       if (pco%ls_lsu%y == "y") then
          call open_output_file(2162, "lsunit_ls_yr.txt", 1500)
          write (2162,*) bsn%name, prog
          write (2162,*) ls_hdr 
          write (2162,*) ls_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_ls_yr.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2166, "lsunit_ls_yr.csv", 1500)
            write (2166,*) bsn%name, prog
            write (2166,'(*(G0.3,:,","))') ls_hdr
            write (2166,'(*(G0.3,:,","))') ls_hdr_units
            write (9000,*) "ROUTING_UNIT              lsunit_ls_yr.csv"
          end if 
       endif
        
       if (pco%ls_lsu%a == "y") then
       call open_output_file(2163, "lsunit_ls_aa.txt", 1500)
        write (2163,*) bsn%name, prog
        write (2163,*) ls_hdr 
        write (2163,*) ls_hdr_units
        write (9000,*) "ROUTING_UNIT              lsunit_ls_aa.txt"
        if (pco%csvout == "y") then 
          call open_output_file(2167, "lsunit_ls_aa.csv", 1500)
          write (2167,*) bsn%name, prog
          write (2167,'(*(G0.3,:,","))') ls_hdr
          write (2167,'(*(G0.3,:,","))') ls_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_ls_aa.csv"
        end if 
       end if

!!!  Plant/Weather
        if (pco%pw_lsu%d == "y") then
          call open_output_file(2170, "lsunit_pw_day.txt", 1500)
          write (2170,*) bsn%name, prog
          write (2170,*) pw_hdr
          write (2170,*) pw_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_pw_day.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2174, "lsunit_pw_day.csv", 1500)
            write (2174,*) bsn%name, prog
            write (2174,'(*(G0.3,:,","))') pw_hdr 
            write (2174,'(*(G0.3,:,","))') pw_hdr_units
            write (9000,*) "ROUTING_UNIT              lsunit_pw_day.csv"
          end if 
        end if 
    
      
      if (pco%pw_lsu%m == "y") then
       call open_output_file(2171, "lsunit_pw_mon.txt", 1500)
        write (2171,*) bsn%name, prog
        write (2171,*) pw_hdr
        write (2171,*) pw_hdr_units
        write (9000,*) "ROUTING_UNIT              lsunit_pw_mon.txt"
        if (pco%csvout == "y") then 
          call open_output_file(2175, "lsunit_pw_mon.csv", 1500)
          write (2175,*) bsn%name, prog
          write (2175,'(*(G0.3,:,","))') pw_hdr
          write (2175,'(*(G0.3,:,","))') pw_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_pw_mon.csv"
        end if
       end if
        
        if (pco%pw_lsu%y == "y") then
          call open_output_file(2172, "lsunit_pw_yr.txt", 1500)
          write (2172,*) bsn%name, prog
          write (2172,*) pw_hdr
          write (2172,*) pw_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_pw_yr.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2176, "lsunit_pw_yr.csv", 1500)
            write (2176,*) bsn%name, prog
            write (2176, '(*(G0.3,:,","))')pw_hdr
            write (2176,'(*(G0.3,:,","))') pw_hdr_units
            write (9000,*) "ROUTING_UNIT              lsunit_pw_yr.csv"
          end if 
        end if 
      
     if (pco%pw_lsu%a == "y") then
       call open_output_file(2173, "lsunit_pw_aa.txt", 1500)
        write (2173,*) bsn%name, prog
        write (2173,*) pw_hdr
        write (2173,*) pw_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_pw_aa.txt"
        if (pco%csvout == "y") then 
          call open_output_file(2177, "lsunit_pw_aa.csv", 1500)
          write (2177,*) bsn%name, prog
          write (2177,'(*(G0.3,:,","))') pw_hdr
          write (2177,'(*(G0.3,:,","))') pw_hdr_units
          write (9000,*) "ROUTING_UNIT              lsunit_pw_aa.csv"
        end if
     end if
      end if
      
!!!  BASIN - Water balance 
        if (pco%wb_bsn%d == "y") then
          call open_output_file(2050, "basin_wb_day.txt", 1500)
          write (2050,*) bsn%name, prog
          write (2050,*) wb_hdr  !! bsn
          write (2050,*) wb_hdr_units
          write (9000,*) "BASIN                     basin_wb_day.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2054, "basin_wb_day.csv", 1500)
            write (2054,*) bsn%name, prog
            write (2054,'(*(G0.3,:,","))') wb_hdr !! bsn
            write (2054,'(*(G0.3,:,","))') wb_hdr_units
            write (9000,*) "BASIN                     basin_wb_day.csv"
          end if 
        endif
            
       if (pco%wb_bsn%m == "y") then 
        call open_output_file(2051, "basin_wb_mon.txt", 1500)
        write (2051,*) bsn%name, prog
        write (2051,*) wb_hdr  !! bsn
        write (2051,*) wb_hdr_units
        write (9000,*) "BASIN                     basin_wb_mon.txt"
        if (pco%csvout == "y") then 
          call open_output_file(2055, "basin_wb_mon.csv", 1500)
          write (2055,*) bsn%name, prog
          write (2055,'(*(G0.3,:,","))') wb_hdr !! bsn
          write (2055,'(*(G0.3,:,","))') wb_hdr_units
          write (9000,*) "BASIN                     basin_wb_mon.csv"
        end if
       end if 

        if (pco%wb_bsn%y == "y") then
          call open_output_file(2052, "basin_wb_yr.txt", 1500)
          write (2052,*) bsn%name, prog
          write (2052,*) wb_hdr  !! bsn
          write (2052,*) wb_hdr_units
          write (9000,*) "BASIN                     basin_wb_yr.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2056, "basin_wb_yr.csv", 1500)
            write (2056,*) bsn%name, prog
            write (2056,'(*(G0.3,:,","))') wb_hdr !! bsn
            write (2056,'(*(G0.3,:,","))') wb_hdr_units
            write (9000,*) "BASIN                     basin_wb_yr.csv"
          end if 
        endif
        
       if (pco%wb_bsn%a == "y") then 
        call open_output_file(2053, "basin_wb_aa.txt", 1500)
        write (2053,*) bsn%name, prog
        write (2053,*) wb_hdr  !! bsn
        write (2053,*) wb_hdr_units
        write (9000,*) "BASIN                     basin_wb_aa.txt"
        if (pco%csvout == "y") then 
          call open_output_file(2057, "basin_wb_aa.csv", 1500)
          write (2057,*) bsn%name, prog
          write (2057,'(*(G0.3,:,","))') wb_hdr !! bsn
          write (2057,'(*(G0.3,:,","))') wb_hdr_units
          write (9000,*) "BASIN                     basin_wb_aa.csv"
        end if
       end if 

!!!  BASIN - Nutrient balance    
        if (pco%nb_bsn%d == "y") then
          call open_output_file(2060, "basin_nb_day.txt", 1500)
          write (2060,*) bsn%name, prog
          write (2060,*) nb_hdr
          write (2060,*) nb_hdr_units
          write (9000,*) "BASIN                     basin_nb_day.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2064, "basin_nb_day.csv", 1500)
            write (2064,*) bsn%name, prog
            write (2064,'(*(G0.3,:,","))') nb_hdr
            write (2064,'(*(G0.3,:,","))') nb_hdr_units
            write (9000,*) "BASIN                     basin_nb_day.csv"
          end if 
        endif
        
       if (pco%nb_bsn%m == "y") then 
        call open_output_file(2061, "basin_nb_mon.txt", 1500)
        write (2061,*) bsn%name, prog
        write (2061,*) nb_hdr
        write (2061,*) nb_hdr_units
        write (9000,*) "BASIN                     basin_nb_mon.txt"
        if (pco%csvout == "y") then 
          call open_output_file(2065, "basin_nb_mon.csv", 1500)
          write (2065,*) bsn%name, prog
          write (2065,'(*(G0.3,:,","))') nb_hdr
          write (2065,'(*(G0.3,:,","))') nb_hdr_units
          write (9000,*) "BASIN                     basin_nb_mon.csv"
        end if
       end if 

        if (pco%nb_bsn%y == "y") then
          call open_output_file(2062, "basin_nb_yr.txt", 1500)
          write (2062,*) bsn%name, prog
          write (2062,*) nb_hdr
          write (2062,*) nb_hdr_units
          write (9000,*) "BASIN                     basin_nb_yr.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2066, "basin_nb_yr.csv", 1500)
            write (2066,*) bsn%name, prog
            write (2066,'(*(G0.3,:,","))') nb_hdr
            write (2066,'(*(G0.3,:,","))') nb_hdr_units
            write (9000,*) "BASIN                     basin_nb_yr.csv"
          end if 
        endif
        
       if (pco%nb_bsn%a == "y") then 
        call open_output_file(2063, "basin_nb_aa.txt", 1500)
        write (2063,*) bsn%name, prog
        write (2063,*) nb_hdr
        write (2063,*) nb_hdr_units
        write (9000,*) "BASIN                     basin_nb_aa.txt"
        if (pco%csvout == "y") then 
          call open_output_file(2067, "basin_nb_aa.csv", 1500)
          write (2067,*) bsn%name, prog
          write (2067,'(*(G0.3,:,","))') nb_hdr
          write (2067,'(*(G0.3,:,","))') nb_hdr_units
          write (9000,*) "BASIN                     basin_nb_aa.csv"
        end if
       end if 
                
!!!  BASIN - Losses
        if (pco%ls_bsn%d == "y") then
          call open_output_file(2070, "basin_ls_day.txt", 1500)
          write (2070,*) bsn%name, prog
          write (2070,*) ls_hdr    !! bsn
          write (2070,*) ls_hdr_units
          write (9000,*) "BASIN                     basin_ls_day.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2074, "basin_ls_day.csv", 1500)
            write (2074,*) bsn%name, prog
            write (2074,'(*(G0.3,:,","))') ls_hdr    !! bsn
            write (2074,'(*(G0.3,:,","))') ls_hdr_units
            write (9000,*) "BASIN                     basin_ls_day.csv"
          end if 
        endif
        
       if (pco%ls_bsn%m == "y") then
        call open_output_file(2071, "basin_ls_mon.txt", 1500)
        write (2071,*) bsn%name, prog
        write (2071,*) ls_hdr     !! bsn
        write (2071,*) ls_hdr_units
        write (9000,*) "BASIN                     basin_ls_mon.txt"
        if (pco%csvout == "y") then 
          call open_output_file(2075, "basin_ls_mon.csv", 1500)
          write (2075,*) bsn%name, prog
          write (2075,'(*(G0.3,:,","))') ls_hdr     !! bsn
          write (2075,'(*(G0.3,:,","))') ls_hdr_units
          write (9000,*) "BASIN                     basin_ls_mon.csv"
        end if
       end if

        if (pco%ls_bsn%y == "y") then
          call open_output_file(2072, "basin_ls_yr.txt", 1500)
          write (2072,*) bsn%name, prog
          write (2072,*) ls_hdr    !! bsn
          write (2072,*) ls_hdr_units
          write (9000,*) "BASIN                     basin_ls_yr.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2076, "basin_ls_yr.csv", 1500)
            write (2076,*) bsn%name, prog
            write (2076,'(*(G0.3,:,","))') ls_hdr    !! bsn
            write (2076,'(*(G0.3,:,","))') ls_hdr_units
            write (9000,*) "BASIN                     basin_ls_yr.csv"
          end if 
        endif
        
       if (pco%ls_bsn%a == "y") then
        call open_output_file(2073, "basin_ls_aa.txt", 1500)
        write (2073,*) bsn%name, prog
        write (2073,*) ls_hdr     !! bsn
        write (2073,*) ls_hdr_units
        write (9000,*) "BASIN                     basin_ls_aa.txt"
        if (pco%csvout == "y") then 
          call open_output_file(2077, "basin_ls_aa.csv", 1500)
          write (2077,*) bsn%name, prog
          write (2077,'(*(G0.3,:,","))') ls_hdr     !! bsn
          write (2077,'(*(G0.3,:,","))') ls_hdr_units
          write (9000,*) "BASIN                     basin_ls_aa.csv"
        end if
       end if
        
!!!  BASIN - Plant/Weather
        if (pco%pw_bsn%d == "y") then
          call open_output_file(2080, "basin_pw_day.txt", 1500)
          write (2080,*) bsn%name, prog
          write (2080,*) pw_hdr  !! bsn
          write (2080,*) pw_hdr_units
          write (9000,*) "BASIN                     basin_pw_day.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2084, "basin_pw_day.csv", 1500)
            write (2084,*) bsn%name, prog
            write (2084,'(*(G0.3,:,","))') pw_hdr  !! bsn
            write (2084,'(*(G0.3,:,","))') pw_hdr_units
            write (9000,*) "BASIN                     basin_pw_day.csv"
          end if
        endif
        
       if (pco%pw_bsn%m == "y") then
        call open_output_file(2081, "basin_pw_mon.txt", 1500) 
        write (2081,*) bsn%name, prog
        write (2081,*) pw_hdr  !! bsn
        write (2081,*) pw_hdr_units
        write (9000,*) "BASIN                     basin_pw_mon.txt"
       if (pco%csvout == "y") then 
          call open_output_file(2085, "basin_pw_mon.csv", 1500)
          write (2085,*) bsn%name, prog
          write (2085,'(*(G0.3,:,","))') pw_hdr     !! bsn
          write (2085,'(*(G0.3,:,","))') pw_hdr_units
          write (9000,*) "BASIN                     basin_pw_mon.csv"
       end if
      end if

        if (pco%pw_bsn%y == "y") then
          call open_output_file(2082, "basin_pw_yr.txt", 1500)
          write (2082,*) bsn%name, prog
          write (2082,*) pw_hdr  !! bsn
          write (2082,*) pw_hdr_units
          write (9000,*) "BASIN                     basin_pw_yr.txt"
          if (pco%csvout == "y") then 
            call open_output_file(2086, "basin_pw_yr.csv", 1500)
            write (2086,*) bsn%name, prog
            write (2086,'(*(G0.3,:,","))') pw_hdr  !! bsn
            write (2086,'(*(G0.3,:,","))') pw_hdr_units
            write (9000,*) "BASIN                     basin_pw_yr.csv"
          end if
        endif
        
       if (pco%pw_bsn%a == "y") then
        call open_output_file(2083, "basin_pw_aa.txt", 1500) 
        write (2083,*) bsn%name, prog
        write (2083,*) pw_hdr  !! bsn
        write (2083,*) pw_hdr_units
        write (9000,*) "BASIN                     basin_pw_aa.txt"
       if (pco%csvout == "y") then 
          call open_output_file(2087, "basin_pw_aa.csv", 1500)
          write (2087,*) bsn%name, prog
          write (2087,'(*(G0.3,:,","))') pw_hdr     !! bsn
          write (2087,'(*(G0.3,:,","))') pw_hdr_units
          write (9000,*) "BASIN                     basin_pw_aa.csv"
       end if
       end if
       
!!! CROP YIELDS - output file only written for yearly or annual timesteps; "b" = both files written;
      if (pco%crop_yld == "y" .or. pco%crop_yld == "b") then
        !! headers for yearly crop yields
        call open_output_file(4010, "crop_yld_yr.txt", 1500)
        write (4010,*) bsn%name, prog
        write (4010,1000)
        write (9000,*) "CROP                      crop_yld_yr.txt"
        if (pco%csvout == "y") then
            call open_output_file(4011, "crop_yld_yr.csv")
            write (4011,*) bsn%name, prog
            write (4011,'(*(G0.3,:,","))') "jday","mon","day","year","unit","plantnm","yield"
            write (9000,*) "CROP                      crop_yld_yr.csv"
        end if
      end if
              
      !! headers for annual crop yields
      if (pco%crop_yld == "a" .or. pco%crop_yld == "b") then
        call open_output_file(4008, "crop_yld_aa.txt", 1500)
        write (4008,*) bsn%name, prog
        write (4008,1000)
        write (9000,*) "CROP                      crop_yld_aa.txt"
        if (pco%csvout == "y") then
            call open_output_file(4009, "crop_yld_aa.csv")
            write (4009,*) bsn%name, prog
            write (4009,'(*(G0.3,:,","))') "jday","mon","day","year","unit","plantnm","yield"
            write (9000,*) "CROP                      crop_yld_aa.csv"
        end if
      end if
      
1000    format (76x,"--YIELD (kg/ha)--",/,1x," jday",1x,"  mon",1x,"  day",1x,"   yr",1x,"   unit", 1x,"PLANTNM",   &
                 18x,"       MASS","          C", "           N","           P")
        
      return
      end subroutine output_landscape_init