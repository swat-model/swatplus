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
            write (9000,*) "BASIN                    basin_wb_yr.csv"
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
       
!!! CROP YIELDS
      if (sp_ob%hru > 0) then
        open (4008,file="crop_yld_aa.txt")
        write (4008,*) bsn%name, prog
          write (4008,1000)
1000    format (76x,"--YIELD (kg/ha)--",/,1x," jday",1x,"  mon",1x,"  day",1x,"   yr",1x,"   unit", 1x,"PLANTNM",   &
                 18x,"       MASS","          C", "           N","           P")
        write (9000,*) "CROP                      crop_yld_aa.txt"
        if (pco%csvout == "y") then
            open (4009,file="crop_yld_aa.csv")
            write (4009,*) bsn%name, prog
            write (4009,'(*(G0.3,:,","))') "jday","mon","day","year","unit","plantnm","yield"
            write (9000,*) "CROP                      crop_yld_aa.csv"
        end if
      end if
                 
      return
      end subroutine output_landscape_init