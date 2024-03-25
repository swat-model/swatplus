     subroutine header_salt
    
     use basin_module
     use reservoir_module
     use hydrograph_module, only : res, sp_ob
     use constituent_mass_module
     use ch_salt_module
     use res_salt_module
     use salt_module
     use salt_aquifer
     
     implicit none 


     !basin - daily -----------------------------------------------------------------------------------------------------------------------
     if (pco%salt_basin%d == "y" .and. cs_db%num_salts > 0) then
       open (5080,file="basin_salt_day.txt", recl = 2000)
       write(5080,*) bsn%name, prog
       write(5080,*)
       write(5080,*) 'Basin salt fluxes - daily'
       write(5080,*)
       write(5080,*) 'Column header units and definitions'
       write(5080,*) 'salt = sum of salt ion mass (so4,ca,mg,na,k,cl,co3,hco3)'
       write(5080,*) 
       write(5080,*) 'lat_kg:       kg      salt mass in soil lateral flow            (soil --> stream)'
       write(5080,*) 'gw_kg:        kg      salt mass in groundwater flow             (aquifer --> stream)'
       write(5080,*) 'sur_kg:       kg      salt mass in surface runoff               (soil --> stream)'
       write(5080,*) 'urb_kg:       kg      salt mass in urban runoff                 (soil --> stream)'
       write(5080,*) 'wet_kg:       kg      salt mass in wetland runoff               (wetland --> stream)'
       write(5080,*) 'tile_kg:      kg      salt mass in tile drain flow              (soil --> stream)'
       write(5080,*) 'perc_kg:      kg      salt mass in deep percolation             (soil --> aquifer)'
       write(5080,*) 'gwup_kg:      kg      salt mass from groundwater transfer       (aquifer --> soil)'
       write(5080,*) 'wtsp_kg:      kg      salt mass in wetland seepage              (wetland --> soil)'
       write(5080,*) 'irsw_kg:      kg      salt mass in surface water irrigation     (channel --> soil)'
       write(5080,*) 'irgw_kg:      kg      salt mass in groundwater irrigation       (aquifer --> soil)'
       write(5080,*) 'irwo_kg:      kg      salt mass in outside source irrigation    (outside --> soil)'
       write(5080,*) 'rain_kg:      kg      salt mass in rainfall                     (outside --> soil)'
       write(5080,*) 'dryd_kg:      kg      salt mass in dry deposition               (outside --> soil)'
       write(5080,*) 'road_kg:      kg      salt mass in applied road salt            (outside --> soil)'
       write(5080,*) 'fert_kg:      kg      salt mass in applied fertilizer           (outside --> soil)'
       write(5080,*) 'amnd_kg:      kg      salt mass in soil amendments              (outside --> soil)'
       write(5080,*) 'uptk_kg:      kg      salt mass in plant uptake                 (soil --> outside)'
       write(5080,*) 'ptso_kg:      kg      salt mass in watershed point sources      (outside --> channel)'
       write(5080,*) 'ptout_kg:     kg      salt mass in outside point sources        (outside --> channel)'
       write(5080,*) 'rchg_kg:      kg      salt mass in groundwater recharge         (soil --> aquifer)'
       write(5080,*) 'seep_kg:      kg      salt mass in groundwater seepage          (aquifer --> deep)'
       write(5080,*) 'dssl_kg:      kg      salt mass dissolved from soil minerals    (mineral --> soil)'
       write(5080,*) 'dsaq_kg:      kg      salt mass dissolved from aquifer minerals (mineral --> aquifer)'
       write(5080,*) 'soilds_kg:    kg      total dissolved salt mass in soil water'
       write(5080,*) 'soilmn_kg:    kg      total salt mineral mass in soil profile'
       write(5080,*) 'aquds_kg:     kg      total dissolved salt mass in groundwater'
       write(5080,*) 'aqumn_kg:     kg      total salt mineral mass in aquifer'
       write(5080,*)
       write(5080,*) saltb_hdr
     endif
     
     !basin - monthly
     if (pco%salt_basin%m == "y" .and. cs_db%num_salts > 0) then
       open (5082,file="basin_salt_mon.txt", recl = 2000)
       write(5082,*) bsn%name, prog
       write(5082,*)
       write(5082,*) 'Basin salt fluxes - monthly'
       write(5082,*)
       write(5082,*) 'Column header units and definitions'
       write(5082,*) 'salt = sum of salt ion mass (so4,ca,mg,na,k,cl,co3,hco3)'
       write(5082,*) 
       write(5082,*) 'lat_kg:       kg      salt mass in soil lateral flow            (soil --> stream)'
       write(5082,*) 'gw_kg:        kg      salt mass in groundwater flow             (aquifer --> stream)'
       write(5082,*) 'sur_kg:       kg      salt mass in surface runoff               (soil --> stream)'
       write(5082,*) 'urb_kg:       kg      salt mass in urban runoff                 (soil --> stream)'
       write(5082,*) 'wet_kg:       kg      salt mass in wetland runoff               (wetland --> stream)'
       write(5082,*) 'tile_kg:      kg      salt mass in tile drain flow              (soil --> stream)'
       write(5082,*) 'perc_kg:      kg      salt mass in deep percolation             (soil --> aquifer)'
       write(5082,*) 'gwup_kg:      kg      salt mass from groundwater transfer       (aquifer --> soil)'
       write(5082,*) 'wtsp_kg:      kg      salt mass in wetland seepage              (wetland --> soil)'
       write(5082,*) 'irsw_kg:      kg      salt mass in surface water irrigation     (channel --> soil)'
       write(5082,*) 'irgw_kg:      kg      salt mass in groundwater irrigation       (aquifer --> soil)'
       write(5082,*) 'irwo_kg:      kg      salt mass in outside source irrigation    (outside --> soil)'
       write(5082,*) 'rain_kg:      kg      salt mass in rainfall                     (outside --> soil)'
       write(5082,*) 'dryd_kg:      kg      salt mass in dry deposition               (outside --> soil)'
       write(5082,*) 'road_kg:      kg      salt mass in applied road salt            (outside --> soil)'
       write(5082,*) 'fert_kg:      kg      salt mass in applied fertilizer           (outside --> soil)'
       write(5082,*) 'amnd_kg:      kg      salt mass in soil amendments              (outside --> soil)'
       write(5082,*) 'uptk_kg:      kg      salt mass in plant uptake                 (soil --> outside)'
       write(5082,*) 'ptso_kg:      kg      salt mass in watershed point sources      (outside --> channel)'
       write(5082,*) 'ptout_kg:     kg      salt mass in outside point sources        (outside --> channel)'
       write(5082,*) 'rchg_kg:      kg      salt mass in groundwater recharge         (soil --> aquifer)'
       write(5082,*) 'seep_kg:      kg      salt mass in groundwater seepage          (aquifer --> deep)'
       write(5082,*) 'dssl_kg:      kg      salt mass dissolved from soil minerals    (mineral --> soil)'
       write(5082,*) 'dsaq_kg:      kg      salt mass dissolved from aquifer minerals (mineral --> aquifer)'
       write(5082,*) 'soilds_kg:    kg      total dissolved salt mass in soil water   (averaged over month)'
       write(5082,*) 'soilmn_kg:    kg      total salt mineral mass in soil profile   (averaged over month)'
       write(5082,*) 'aquds_kg:     kg      total dissolved salt mass in groundwater  (averaged over month)'
       write(5082,*) 'aqumn_kg:     kg      total salt mineral mass in aquifer        (averaged over month)'
       write(5082,*)
       write(5082,*) saltb_hdr
     endif
     
     !basin - yearly
     if (pco%salt_basin%y == "y" .and. cs_db%num_salts > 0) then
       open (5084,file="basin_salt_yr.txt", recl = 2000)
       write(5084,*) bsn%name, prog
       write(5084,*)
       write(5084,*) 'Basin salt fluxes - yearly'
       write(5084,*)
       write(5084,*) 'Column header units and definitions'
       write(5084,*) 'salt = sum of salt ion mass (so4,ca,mg,na,k,cl,co3,hco3)'
       write(5084,*) 
       write(5084,*) 'lat_kg:       kg      salt mass in soil lateral flow            (soil --> stream)'
       write(5084,*) 'gw_kg:        kg      salt mass in groundwater flow             (aquifer --> stream)'
       write(5084,*) 'sur_kg:       kg      salt mass in surface runoff               (soil --> stream)'
       write(5084,*) 'urb_kg:       kg      salt mass in urban runoff                 (soil --> stream)'
       write(5084,*) 'wet_kg:       kg      salt mass in wetland runoff               (wetland --> stream)'
       write(5084,*) 'tile_kg:      kg      salt mass in tile drain flow              (soil --> stream)'
       write(5084,*) 'perc_kg:      kg      salt mass in deep percolation             (soil --> aquifer)'
       write(5084,*) 'gwup_kg:      kg      salt mass from groundwater transfer       (aquifer --> soil)'
       write(5084,*) 'wtsp_kg:      kg      salt mass in wetland seepage              (wetland --> soil)'
       write(5084,*) 'irsw_kg:      kg      salt mass in surface water irrigation     (channel --> soil)'
       write(5084,*) 'irgw_kg:      kg      salt mass in groundwater irrigation       (aquifer --> soil)'
       write(5084,*) 'irwo_kg:      kg      salt mass in outside source irrigation    (outside --> soil)'
       write(5084,*) 'rain_kg:      kg      salt mass in rainfall                     (outside --> soil)'
       write(5084,*) 'dryd_kg:      kg      salt mass in dry deposition               (outside --> soil)'
       write(5084,*) 'road_kg:      kg      salt mass in applied road salt            (outside --> soil)'
       write(5084,*) 'fert_kg:      kg      salt mass in applied fertilizer           (outside --> soil)'
       write(5084,*) 'amnd_kg:      kg      salt mass in soil amendments              (outside --> soil)'
       write(5084,*) 'uptk_kg:      kg      salt mass in plant uptake                 (soil --> outside)'
       write(5084,*) 'ptso_kg:      kg      salt mass in watershed point sources      (outside --> channel)'
       write(5084,*) 'ptout_kg:     kg      salt mass in outside point sources        (outside --> channel)'
       write(5084,*) 'rchg_kg:      kg      salt mass in groundwater recharge         (soil --> aquifer)'
       write(5084,*) 'seep_kg:      kg      salt mass in groundwater seepage          (aquifer --> deep)'
       write(5084,*) 'dssl_kg:      kg      salt mass dissolved from soil minerals    (mineral --> soil)'
       write(5084,*) 'dsaq_kg:      kg      salt mass dissolved from aquifer minerals (mineral --> aquifer)'
       write(5084,*) 'soilds_kg:    kg      total dissolved salt mass in soil water   (averaged over year)'
       write(5084,*) 'soilmn_kg:    kg      total salt mineral mass in soil profile   (averaged over year)'
       write(5084,*) 'aquds_kg:     kg      total dissolved salt mass in groundwater  (averaged over year)'
       write(5084,*) 'aqumn_kg:     kg      total salt mineral mass in aquifer        (averaged over year)'
       write(5084,*)
       write(5084,*) saltb_hdr
     endif
     
     !basin - ave annual
     if (pco%salt_basin%a == "y" .and. cs_db%num_salts > 0) then
       open (5086,file="basin_salt_aa.txt", recl = 2000)
       write(5086,*) bsn%name, prog
       write(5086,*)
       write(5086,*) 'Basin salt fluxes - average annual'
       write(5086,*)
       write(5086,*) 'Column header units and definitions'
       write(5086,*) 'salt = sum of salt ion mass (so4,ca,mg,na,k,cl,co3,hco3)'
       write(5086,*) 
       write(5086,*) 'lat_kg:       kg/yr   salt mass in soil lateral flow            (soil --> stream)'
       write(5086,*) 'gw_kg:        kg/yr   salt mass in groundwater flow             (aquifer --> stream)'
       write(5086,*) 'sur_kg:       kg/yr   salt mass in surface runoff               (soil --> stream)'
       write(5086,*) 'urb_kg:       kg/yr   salt mass in urban runoff                 (soil --> stream)'
       write(5086,*) 'wet_kg:       kg/yr   salt mass in wetland runoff               (wetland --> stream)'
       write(5086,*) 'tile_kg:      kg/yr   salt mass in tile drain flow              (soil --> stream)'
       write(5086,*) 'perc_kg:      kg/yr   salt mass in deep percolation             (soil --> aquifer)'
       write(5086,*) 'gwup_kg:      kg/yr   salt mass from groundwater transfer       (aquifer --> soil)'
       write(5086,*) 'wtsp_kg:      kg/yr   salt mass in wetland seepage              (wetland --> soil)'
       write(5086,*) 'irsw_kg:      kg/yr   salt mass in surface water irrigation     (channel --> soil)'
       write(5086,*) 'irgw_kg:      kg/yr   salt mass in groundwater irrigation       (aquifer --> soil)'
       write(5086,*) 'irwo_kg:      kg/yr   salt mass in outside source irrigation    (outside --> soil)'
       write(5086,*) 'rain_kg:      kg/yr   salt mass in rainfall                     (outside --> soil)'
       write(5086,*) 'dryd_kg:      kg/yr   salt mass in dry deposition               (outside --> soil)'
       write(5086,*) 'road_kg:      kg/yr   salt mass in applied road salt            (outside --> soil)'
       write(5086,*) 'fert_kg:      kg/yr   salt mass in applied fertilizer           (outside --> soil)'
       write(5086,*) 'amnd_kg:      kg/yr   salt mass in soil amendments              (outside --> soil)'
       write(5086,*) 'uptk_kg:      kg/yr   salt mass in plant uptake                 (soil --> outside)'
       write(5086,*) 'ptso_kg:      kg/yr   salt mass in watershed point sources      (outside --> channel)'
       write(5086,*) 'ptout_kg:     kg/yr   salt mass in outside point sources        (outside --> channel)'
       write(5086,*) 'rchg_kg:      kg/yr   salt mass in groundwater recharge         (soil --> aquifer)'
       write(5086,*) 'seep_kg:      kg/yr   salt mass in groundwater seepage          (aquifer --> deep)'
       write(5086,*) 'dssl_kg:      kg/yr   salt mass dissolved from soil minerals    (mineral --> soil)'
       write(5086,*) 'dsaq_kg:      kg/yr   salt mass dissolved from aquifer minerals (mineral --> aquifer)'
       write(5086,*) 'soilds_kg:    kg/yr   total dissolved salt mass in soil water   (averaged over years)'
       write(5086,*) 'soilmn_kg:    kg/yr   total salt mineral mass in soil profile   (averaged over years)'
       write(5086,*) 'aquds_kg:     kg/yr   total dissolved salt mass in groundwater  (averaged over years)'
       write(5086,*) 'aqumn_kg:     kg/yr   total salt mineral mass in aquifer        (averaged over years)'
       write(5086,*)
       write(5086,*) saltb_hdr
     endif
     
     !hru - daily -------------------------------------------------------------------------------------------------------------------------
     if (pco%salt_hru%d == "y" .and. cs_db%num_salts > 0) then
       open (5021,file="hru_salt_day.txt", recl = 3000)
       write(5021,*) bsn%name, prog
       write(5021,*) 
       write(5021,*) 'HRU salt fluxes and state variables - daily'
       write(5021,*)
       write(5021,*) 'Column header units and definitions'
       write(5021,*) 'salt ion values provided for each category'
       write(5021,*) 
       write(5021,*) 'soil:         kg/ha   total salt in soil profile'
       write(5021,*) 'surq:         kg/ha   salt mass in surface runoff            (soil --> stream)'
       write(5021,*) 'latq:         kg/ha   salt mass in soil lateral flow         (soil --> stream)'
       write(5021,*) 'urbq:         kg/ha   salt mass in urban runoff              (soil --> stream)'
       write(5021,*) 'wetq:         kg/ha   salt mass in wetland runoff            (wetland --> stream)'
       write(5021,*) 'tile:         kg/ha   salt mass in tile drainage             (soil --> stream)'
       write(5021,*) 'perc:         kg/ha   salt mass in deep percolation          (soil --> aquifer)'
       write(5021,*) 'gwup:         kg/ha   salt mass from groundwater transfer    (aquifer --> soil)'
       write(5021,*) 'wtsp:         kg/ha   salt mass in wetland seepage           (wetland --> soil)'
       write(5021,*) 'irsw:         kg/ha   salt mass in surfac water irrigation   (channel --> soil)'
       write(5021,*) 'irgw:         kg/ha   salt mass in groundwater irrigation    (aquifer --> soil)'
       write(5021,*) 'irwo:         kg/ha   salt mass in outside source irrigation (outside --> soil)'
       write(5021,*) 'rain:         kg/ha   salt mass in rainfall                  (outside --> soil)'
       write(5021,*) 'dryd:         kg/ha   salt mass in dry deposition            (outside --> soil)'
       write(5021,*) 'road:         kg/ha   salt mass in applied road salt         (outside --> soil)'
       write(5021,*) 'fert:         kg/ha   salt mass in applied fertilizer        (outside --> soil)'
       write(5021,*) 'amnd:         kg/ha   salt mass in soil amendments           (outside --> soil)'
       write(5021,*) 'uptk:         kg/ha   salt mass in plant uptake              (soil --> outside)'
       write(5021,*) 'conc:         g/m3    salt concentration in soil water (averaged over soil layers)'
       write(5021,*) 'dssl:         kg/ha   salt mineral dissolution (summed over soil layers)'
       write(5021,*) 
       write(5021,*) salt_hdr_hru
       if (pco%csvout == "y") then
         open (5022,file="hru_salt_day.csv", recl = 3000)
         write (5022,*) bsn%name, prog
         write (5022,*) 'conc = mg/L; other = kg/ha'
         write (5022,'(*(G0.3,:","))') salt_hdr_hru
       endif
     endif
     
     !hru - monthly
     if (pco%salt_hru%m == "y" .and. cs_db%num_salts > 0) then
       open (5023,file="hru_salt_mon.txt", recl = 3000)
       write(5023,*) bsn%name, prog
       write(5023,*) 
       write(5023,*) 'HRU salt fluxes and state variables - monthly'
       write(5023,*)
       write(5023,*) 'Column header units and definitions'
       write(5023,*) 'salt ion values provided for each category'
       write(5023,*) 
       write(5023,*) 'soil:         kg/ha   total salt in soil profile'
       write(5023,*) 'surq:         kg/ha   salt mass in surface runoff            (soil --> stream)'
       write(5023,*) 'latq:         kg/ha   salt mass in soil lateral flow         (soil --> stream)'
       write(5023,*) 'urbq:         kg/ha   salt mass in urban runoff              (soil --> stream)'
       write(5023,*) 'wetq:         kg/ha   salt mass in wetland runoff            (wetland --> stream)'
       write(5023,*) 'tile:         kg/ha   salt mass in tile drainage             (soil --> stream)'
       write(5023,*) 'perc:         kg/ha   salt mass in deep percolation          (soil --> aquifer)'
       write(5023,*) 'gwup:         kg/ha   salt mass from groundwater transfer    (aquifer --> soil)'
       write(5023,*) 'wtsp:         kg/ha   salt mass in wetland seepage           (wetland --> soil)'
       write(5023,*) 'irsw:         kg/ha   salt mass in surfac water irrigation   (channel --> soil)'
       write(5023,*) 'irgw:         kg/ha   salt mass in groundwater irrigation    (aquifer --> soil)'
       write(5023,*) 'irwo:         kg/ha   salt mass in outside source irrigation (outside --> soil)'
       write(5023,*) 'rain:         kg/ha   salt mass in rainfall                  (outside --> soil)'
       write(5023,*) 'dryd:         kg/ha   salt mass in dry deposition            (outside --> soil)'
       write(5023,*) 'road:         kg/ha   salt mass in applied road salt         (outside --> soil)'
       write(5023,*) 'fert:         kg/ha   salt mass in applied fertilizer        (outside --> soil)'
       write(5023,*) 'amnd:         kg/ha   salt mass in soil amendments           (outside --> soil)'
       write(5023,*) 'uptk:         kg/ha   salt mass in plant uptake              (soil --> outside)'
       write(5023,*) 'conc:         g/m3    salt concentration in soil water (averaged over soil layers)'
       write(5023,*) 'dssl:         kg/ha   salt mineral dissolution (summed over soil layers)'
       write(5023,*) 
       write(5023,*) salt_hdr_hru
       if (pco%csvout == "y") then
         open (5024,file="hru_salt_mon.csv", recl = 3000)
         write (5024,*) bsn%name, prog
         write (5024,*) 'conc = mg/L; other = kg/ha'
         write (5024,'(*(G0.3,:","))') salt_hdr_hru
       endif
     endif
     
     !hru - yearly
     if (pco%salt_hru%y == "y" .and. cs_db%num_salts > 0) then
       open (5025,file="hru_salt_yr.txt", recl = 3000)
       write(5025,*) bsn%name, prog
       write(5025,*) 
       write(5025,*) 'HRU salt fluxes and state variables - yearly'
       write(5025,*)
       write(5025,*) 'Column header units and definitions'
       write(5025,*) 'salt ion values provided for each category'
       write(5025,*) 
       write(5025,*) 'soil:         kg/ha   total salt in soil profile'
       write(5025,*) 'surq:         kg/ha   salt mass in surface runoff            (soil --> stream)'
       write(5025,*) 'latq:         kg/ha   salt mass in soil lateral flow         (soil --> stream)'
       write(5025,*) 'urbq:         kg/ha   salt mass in urban runoff              (soil --> stream)'
       write(5025,*) 'wetq:         kg/ha   salt mass in wetland runoff            (wetland --> stream)'
       write(5025,*) 'tile:         kg/ha   salt mass in tile drainage             (soil --> stream)'
       write(5025,*) 'perc:         kg/ha   salt mass in deep percolation          (soil --> aquifer)'
       write(5025,*) 'gwup:         kg/ha   salt mass from groundwater transfer    (aquifer --> soil)'
       write(5025,*) 'wtsp:         kg/ha   salt mass in wetland seepage           (wetland --> soil)'
       write(5025,*) 'irsw:         kg/ha   salt mass in surfac water irrigation   (channel --> soil)'
       write(5025,*) 'irgw:         kg/ha   salt mass in groundwater irrigation    (aquifer --> soil)'
       write(5025,*) 'irwo:         kg/ha   salt mass in outside source irrigation (outside --> soil)'
       write(5025,*) 'rain:         kg/ha   salt mass in rainfall                  (outside --> soil)'
       write(5025,*) 'dryd:         kg/ha   salt mass in dry deposition            (outside --> soil)'
       write(5025,*) 'road:         kg/ha   salt mass in applied road salt         (outside --> soil)'
       write(5025,*) 'fert:         kg/ha   salt mass in applied fertilizer        (outside --> soil)'
       write(5025,*) 'amnd:         kg/ha   salt mass in soil amendments           (outside --> soil)'
       write(5025,*) 'uptk:         kg/ha   salt mass in plant uptake              (soil --> outside)'
       write(5025,*) 'conc:         g/m3    salt concentration in soil water (averaged over soil layers)'
       write(5025,*) 'dssl:         kg/ha   salt mineral dissolution (summed over soil layers)'
       write(5025,*) 
       write(5025,*) salt_hdr_hru
       if (pco%csvout == "y") then
         open (5026,file="hru_salt_yr.csv", recl = 3000)
         write (5026,*) bsn%name, prog
         write (5026,*) 'conc = mg/L; other = kg/ha'
         write (5026,'(*(G0.3,:","))') salt_hdr_hru
       endif
     endif
     
     !hru - ave annual
     if (pco%salt_hru%a == "y" .and. cs_db%num_salts > 0) then
       open (5027,file="hru_salt_aa.txt", recl = 3000)
       write(5027,*) bsn%name, prog
       write(5027,*) 
       write(5027,*) 'HRU salt fluxes and state variables - average annual'
       write(5027,*)
       write(5027,*) 'Column header units and definitions'
       write(5027,*) 'salt ion values provided for each category'
       write(5027,*) 
       write(5027,*) 'soil:         kg/ha/yr   total salt in soil profile'
       write(5027,*) 'surq:         kg/ha/yr   salt mass in surface runoff            (soil --> stream)'
       write(5027,*) 'latq:         kg/ha/yr   salt mass in soil lateral flow         (soil --> stream)'
       write(5027,*) 'urbq:         kg/ha/yr   salt mass in urban runoff              (soil --> stream)'
       write(5027,*) 'wetq:         kg/ha/yr   salt mass in wetland runoff            (wetland --> stream)'
       write(5027,*) 'tile:         kg/ha/yr   salt mass in tile drainage             (soil --> stream)'
       write(5027,*) 'perc:         kg/ha/yr   salt mass in deep percolation          (soil --> aquifer)'
       write(5027,*) 'gwup:         kg/ha/yr   salt mass from groundwater transfer    (aquifer --> soil)'
       write(5027,*) 'wtsp:         kg/ha/yr   salt mass in wetland seepage           (wetland --> soil)'
       write(5027,*) 'irsw:         kg/ha/yr   salt mass in surfac water irrigation   (channel --> soil)'
       write(5027,*) 'irgw:         kg/ha/yr   salt mass in groundwater irrigation    (aquifer --> soil)'
       write(5027,*) 'irwo:         kg/ha/yr   salt mass in outside source irrigation (outside --> soil)'
       write(5027,*) 'rain:         kg/ha/yr   salt mass in rainfall                  (outside --> soil)'
       write(5027,*) 'dryd:         kg/ha/yr   salt mass in dry deposition            (outside --> soil)'
       write(5027,*) 'road:         kg/ha/yr   salt mass in applied road salt         (outside --> soil)'
       write(5027,*) 'fert:         kg/ha/yr   salt mass in applied fertilizer        (outside --> soil)'
       write(5027,*) 'amnd:         kg/ha/yr   salt mass in soil amendments           (outside --> soil)'
       write(5027,*) 'uptk:         kg/ha/yr   salt mass in plant uptake              (soil --> outside)'
       write(5027,*) 'conc:         g/m3       salt concentration in soil water (averaged over soil layers)'
       write(5027,*) 'dssl:         kg/ha      salt mineral dissolution (summed over soil layers)'
       write(5027,*) 
       write(5027,*) salt_hdr_hru
       if (pco%csvout == "y") then
         open (5028,file="hru_salt_aa.csv", recl = 3000)
         write (5028,*) bsn%name, prog
         write (5028,*) 'conc = mg/L; other = kg/ha'
         write (5028,'(*(G0.3,:","))') salt_hdr_hru
       endif
     endif

     !aquifer - daily ---------------------------------------------------------------------------------------------------------------------
     if (sp_ob%aqu > 0) then
       if (pco%salt_aqu%d == "y" .and. cs_db%num_salts > 0) then
         open (5060,file="aquifer_salt_day.txt",recl=2000)
         write(5060,*) bsn%name, prog
         write(5060,*) 
         write(5060,*) 'Aquifer salt fluxes and state variables - daily'
         write(5060,*)
         write(5060,*) 'Column header units and definitions'
         write(5060,*) 'salt ion values provided for each category'
         write(5060,*) 
         write(5060,*) 'gw:           kg      salt mass in groundwater discharge        (aquifer --> stream)' 
         write(5060,*) 'rchg:         kg      salt mass in groundwater recharge         (soil --> aquifer)'
         write(5060,*) 'seep:         kg      salt mass in groundwater seepage          (aquifer --> deep)'
         write(5060,*) 'irr:          kg      salt mass in groundwater irrigation       (aquifer --> soil)'
         write(5060,*) 'div:          kg      salt mass removed (-) or added (+) via diversion'
         write(5060,*) 'mass:         kg      salt mass stored in aquifer'
         write(5060,*) 'conc:         g/m3    salt concentration in groundwater'
         write(5060,*) 'dssl:         kg      salt mass dissolved from aquifer minerals (mineral --> aquifer)'
         write(5060,*)
         write(5060,*) salt_hdr_aqu
         if (pco%csvout == "y") then
           open (5061,file="aquifer_salt_day.csv",recl=2000)
           write (5061,*) bsn%name, prog
           write (5061,*) 'conc = mg/L; other = kg'
           write (5061,'(*(G0.3,:","))') salt_hdr_aqu
         endif
       endif
     endif
     
     !aquifer - monthly     
     if (sp_ob%aqu > 0) then
       if (pco%salt_aqu%m == "y" .and. cs_db%num_salts > 0) then
         open (5062,file="aquifer_salt_mon.txt",recl=2000)
         write(5062,*) bsn%name, prog
         write(5062,*) 
         write(5062,*) 'Aquifer salt fluxes and state variables - monthly'
         write(5062,*)
         write(5062,*) 'Column header units and definitions'
         write(5062,*) 'salt ion values provided for each category'
         write(5062,*) 
         write(5062,*) 'gw:           kg      salt mass in groundwater discharge        (aquifer --> stream)' 
         write(5062,*) 'rchg:         kg      salt mass in groundwater recharge         (soil --> aquifer)'
         write(5062,*) 'seep:         kg      salt mass in groundwater seepage          (aquifer --> deep)'
         write(5062,*) 'irr:          kg      salt mass in groundwater irrigation       (aquifer --> soil)'
         write(5062,*) 'div:          kg      salt mass removed (-) or added (+) via diversion'
         write(5062,*) 'mass:         kg      salt mass stored in aquifer               (averaged over month)'
         write(5062,*) 'conc:         g/m3    salt concentration in groundwater         (averaged over month)'
         write(5062,*) 'dssl:         kg      salt mass dissolved from aquifer minerals (mineral --> aquifer)'
         write(5062,*)
         write(5062,*) salt_hdr_aqu
         if (pco%csvout == "y") then
           open (5063,file="aquifer_salt_mon.csv",recl=2000)
           write (5063,*) bsn%name, prog
           write (5063,*) 'conc = mg/L; other = kg'
           write (5063,'(*(G0.3,:","))') salt_hdr_aqu
         endif
       endif
     endif
     
     !aquifer - yearly     
     if (sp_ob%aqu > 0) then
       if (pco%salt_aqu%y == "y" .and. cs_db%num_salts > 0) then
         open (5064,file="aquifer_salt_yr.txt",recl=2000)
         write(5064,*) bsn%name, prog
         write(5064,*) 
         write(5064,*) 'Aquifer salt fluxes and state variables - yearly'
         write(5064,*)
         write(5064,*) 'Column header units and definitions'
         write(5064,*) 'salt ion values provided for each category'
         write(5064,*) 
         write(5064,*) 'gw:           kg      salt mass in groundwater discharge        (aquifer --> stream)' 
         write(5064,*) 'rchg:         kg      salt mass in groundwater recharge         (soil --> aquifer)'
         write(5064,*) 'seep:         kg      salt mass in groundwater seepage          (aquifer --> deep)'
         write(5064,*) 'irr:          kg      salt mass in groundwater irrigation       (aquifer --> soil)'
         write(5064,*) 'div:          kg      salt mass removed (-) or added (+) via diversion'
         write(5064,*) 'mass:         kg      salt mass stored in aquifer               (averaged over year)'
         write(5064,*) 'conc:         g/m3    salt concentration in groundwater         (averaged over year)'
         write(5064,*) 'dssl:         kg      salt mass dissolved from aquifer minerals (mineral --> aquifer)'
         write(5064,*)
         write(5064,*) salt_hdr_aqu
         if (pco%csvout == "y") then
           open (5065,file="aquifer_salt_yr.csv",recl=2000)
           write (5065,*) bsn%name, prog
           write (5065,*) 'conc = mg/L; other = kg'
           write (5065,'(*(G0.3,:","))') salt_hdr_aqu
         endif
       endif
     endif
     
     !aquifer - ave annual     
     if (sp_ob%aqu > 0) then
       if (pco%salt_aqu%a == "y" .and. cs_db%num_salts > 0) then
         open (5066,file="aquifer_salt_aa.txt",recl=2000)
         write(5066,*) bsn%name, prog
         write(5066,*) 
         write(5066,*) 'Aquifer salt fluxes and state variables - average annual'
         write(5066,*)
         write(5066,*) 'Column header units and definitions'
         write(5066,*) 'salt ion values provided for each category'
         write(5066,*) 
         write(5066,*) 'gw:           kg/yr   salt mass in groundwater discharge        (aquifer --> stream)' 
         write(5066,*) 'rchg:         kg/yr   salt mass in groundwater recharge         (soil --> aquifer)'
         write(5066,*) 'seep:         kg/yr   salt mass in groundwater seepage          (aquifer --> deep)'
         write(5066,*) 'irr:          kg/yr   salt mass in groundwater irrigation       (aquifer --> soil)'
         write(5066,*) 'div:          kg/yr   salt mass removed (-) or added (+) via diversion'
         write(5066,*) 'mass:         kg      salt mass stored in aquifer               (averaged over years)'
         write(5066,*) 'conc:         g/m3    salt concentration in groundwater         (averaged over years)'
         write(5066,*) 'dssl:         kg/yr   salt mass dissolved from aquifer minerals (mineral --> aquifer)'
         write(5066,*)
         write(5066,*) salt_hdr_aqu
         if (pco%csvout == "y") then
           open (5067,file="aquifer_salt_aa.csv",recl=2000)
           write (5067,*) bsn%name, prog
           write (5067,*) 'conc = mg/L; other = kg'
           write (5067,'(*(G0.3,:","))') salt_hdr_aqu
         endif
       endif
     endif
     
     !channel - daily ---------------------------------------------------------------------------------------------------------------------
     if (sp_ob%chandeg > 0) then
       if (pco%salt_chn%d == "y" .and. cs_db%num_salts > 0) then
         open (5030,file="channel_salt_day.txt",recl=4000)
         write(5030,*) bsn%name, prog
         write(5030,*) 
         write(5030,*) 'Channel salt fluxes and state variables - daily'
         write(5030,*)
         write(5030,*) 'Column header units and definitions'
         write(5030,*) 'salt ion values provided for each category'
         write(5030,*) 
         write(5030,*) 'in:          kg       salt mass entering channel (upstream + groundwater)' 
         write(5030,*) 'gw:          kg       salt mass entering from groundwater'
         write(5030,*) 'out:         kg       salt mass leaving downstream end of channel'
         write(5030,*) 'seep:        kg       salt mass leaving channel via seepage'
         write(5030,*) 'irr:         kg       salt mass removed from channel for irrigation'
         write(5030,*) 'div:         kg       salt mass removed (-) or added (+) via diversion'
         write(5030,*) 'mass:        kg       salt mass stored in channel'
         write(5030,*) 'conc:        g/m3     salt concentration in channel water'
         write(5030,*) 
         write(5030,*) chsalt_hdr
         if (pco%csvout == "y") then
           open (5031,file="channel_salt_day.csv",recl=2000)
           write (5031,*) bsn%name, prog
           write (5031,'(*(G0.3,:","))') chsalt_hdr
         endif
       endif
     endif
      
     !channel - monthly     
     if (sp_ob%chandeg > 0) then
       if (pco%salt_chn%m == "y" .and. cs_db%num_salts > 0) then
         open (5032,file="channel_salt_mon.txt",recl=4000)
         write(5032,*) bsn%name, prog
         write(5032,*) 
         write(5032,*) 'Channel salt fluxes and state variables - monthly'
         write(5032,*)
         write(5032,*) 'Column header units and definitions'
         write(5032,*) 'salt ion values provided for each category'
         write(5032,*) 
         write(5032,*) 'in:          kg       salt mass entering channel (upstream + groundwater)' 
         write(5032,*) 'gw:          kg       salt mass entering from groundwater'
         write(5032,*) 'out:         kg       salt mass leaving downstream end of channel'
         write(5032,*) 'seep:        kg       salt mass leaving channel via seepage'
         write(5032,*) 'irr:         kg       salt mass removed from channel for irrigation'
         write(5032,*) 'div:         kg       salt mass removed (-) or added (+) via diversion'
         write(5032,*) 'wat:         kg       salt mass stored in channel (averaged over month)'
         write(5032,*) 'conc:        g/m3     salt concentration in channel water (averaged over month)'
         write(5032,*) 
         write(5032,*) chsalt_hdr
         if (pco%csvout == "y") then
           open (5033,file="channel_salt_mon.csv",recl=2000)
           write (5033,*) bsn%name, prog
           write (5033,'(*(G0.3,:","))') chsalt_hdr
         endif
       endif
     endif
     
     !channel - yearly     
     if (sp_ob%chandeg > 0) then
       if (pco%salt_chn%y == "y" .and. cs_db%num_salts > 0) then
         open (5034,file="channel_salt_yr.txt",recl=4000)
         write(5034,*) bsn%name, prog
         write(5034,*) 
         write(5034,*) 'Channel salt fluxes and state variables - yearly'
         write(5034,*)
         write(5034,*) 'Column header units and definitions'
         write(5034,*) 'salt ion values provided for each category'
         write(5034,*) 
         write(5034,*) 'in:          kg       salt mass entering channel (upstream + groundwater)' 
         write(5034,*) 'gw:          kg       salt mass entering from groundwater'
         write(5034,*) 'out:         kg       salt mass leaving downstream end of channel'
         write(5034,*) 'seep:        kg       salt mass leaving channel via seepage'
         write(5034,*) 'irr:         kg       salt mass removed from channel for irrigation'
         write(5034,*) 'div:         kg       salt mass removed (-) or added (+) via diversion'
         write(5034,*) 'wat:         kg       salt mass stored in channel (averaged over year)'
         write(5034,*) 'conc:        g/m3     salt concentration in channel water (averaged over year)'
         write(5034,*) 
         write(5034,*) chsalt_hdr
         if (pco%csvout == "y") then
           open (5035,file="channel_salt_yr.csv",recl=2000)
           write (5035,*) bsn%name, prog
           write (5035,'(*(G0.3,:","))') chsalt_hdr
         endif
       endif
     endif
     
     !channel - ave annual     
     if (sp_ob%chandeg > 0) then
       if (pco%salt_chn%a == "y" .and. cs_db%num_salts > 0) then
         open (5036,file="channel_salt_aa.txt",recl=4000)
         write(5036,*) bsn%name, prog
         write(5036,*) 
         write(5036,*) 'Channel salt fluxes and state variables - average annual'
         write(5036,*)
         write(5036,*) 'Column header units and definitions'
         write(5036,*) 'salt ion values provided for each category'
         write(5036,*) 
         write(5036,*) 'in:          kg/yr    salt mass entering channel (upstream + groundwater)' 
         write(5036,*) 'gw:          kg/yr    salt mass entering from groundwater'
         write(5036,*) 'out:         kg/yr    salt mass leaving downstream end of channel'
         write(5036,*) 'seep:        kg/yr    salt mass leaving channel via seepage'
         write(5036,*) 'irr:         kg/yr    salt mass removed from channel for irrigation'
         write(5036,*) 'div:         kg/yr    salt mass removed (-) or added (+) via diversion'
         write(5036,*) 'wat:         kg       salt mass stored in channel (averaged over years)'
         write(5036,*) 'conc:        g/m3     salt concentration in channel water (averaged over years)'
         write(5036,*) 
         write(5036,*) chsalt_hdr
         if (pco%csvout == "y") then
           open (5037,file="channel_salt_aa.csv",recl=2000)
           write (5037,*) bsn%name, prog
           write (5037,'(*(G0.3,:","))') chsalt_hdr
         endif
       endif
     endif
     
     !reservoir - daily -------------------------------------------------------------------------------------------------------------------
     if (sp_ob%res > 0) then
       if (pco%salt_res%d == "y" .and. cs_db%num_salts > 0) then
         open (5040,file="reservoir_salt_day.txt",recl=2000)
         write(5040,*) bsn%name, prog
         write(5040,*) 
         write(5040,*) 'Reservoir salt fluxes and state variables - daily'
         write(5040,*)
         write(5040,*) 'Column header units and definitions'
         write(5040,*) 'salt ion values provided for each category'
         write(5040,*) 
         write(5040,*) 'in:          kg       salt mass entering reservoir with inflow' 
         write(5040,*) 'out:         kg       salt mass leaving reservoir with outflow'
         write(5040,*) 'seep:        kg       salt mass leaving reservoir via seepage'
         write(5040,*) 'fert:        kg       salt mass added to reservoir via fertilizer'
         write(5040,*) 'irr:         kg       salt mass removed from reservoir for irrigation'
         write(5040,*) 'div:         kg       salt mass removed (-) or added (+) via diversion'
         write(5040,*) 'mass:        kg       salt mass stored in reservoir'
         write(5040,*) 'conc:        g/m3     salt concentration in reservoir water'
         write(5040,*) 'vol_m3:      m3       volume of water stored in reservoir'
         write(5040,*) 
         write(5040,*) ressalt_hdr
         if (pco%csvout == "y") then
           open (5041,file="reservoir_salt_day.csv",recl=2000)
           write (5041,*) bsn%name, prog
           write (5041,'(*(G0.3,:","))') ressalt_hdr
         endif
       endif
     endif
     
     !reservoir - monthly
     if (sp_ob%res > 0) then
       if (pco%salt_res%m == "y" .and. cs_db%num_salts > 0) then
         open (5042,file="reservoir_salt_mon.txt",recl=2000)
         write(5042,*) bsn%name, prog
         write(5042,*) 
         write(5042,*) 'Reservoir salt fluxes and state variables - monthly'
         write(5042,*)
         write(5042,*) 'Column header units and definitions'
         write(5042,*) 'salt ion values provided for each category'
         write(5042,*) 
         write(5042,*) 'in:          kg       salt mass entering reservoir with inflow' 
         write(5042,*) 'out:         kg       salt mass leaving reservoir with outflow'
         write(5042,*) 'seep:        kg       salt mass leaving reservoir via seepage'
         write(5042,*) 'fert:        kg       salt mass added to reservoir via fertilizer'
         write(5042,*) 'irr:         kg       salt mass removed from reservoir for irrigation'
         write(5042,*) 'div:         kg       salt mass removed (-) or added (+) via diversion'
         write(5042,*) 'mass:        kg       salt mass stored in reservoir (averaged over month)'
         write(5042,*) 'conc:        g/m3     salt concentration in reservoir water (averaged over month)'
         write(5042,*) 'vol_m3:      m3       volume of water stored in reservoir (averaged over month)'
         write(5042,*) 
         write(5042,*) ressalt_hdr
         if (pco%csvout == "y") then
           open (5043,file="reservoir_salt_mon.csv",recl=2000)
           write (5043,*) bsn%name, prog
           write (5043,'(*(G0.3,:","))') ressalt_hdr
         endif
       endif
     endif

     !reservoir - yearly
     if (sp_ob%res > 0) then
       if (pco%salt_res%y == "y" .and. cs_db%num_salts > 0) then
         open (5044,file="reservoir_salt_yr.txt",recl=2000)
         write(5044,*) bsn%name, prog
         write(5044,*) 
         write(5044,*) 'Reservoir salt fluxes and state variables - yearly'
         write(5044,*)
         write(5044,*) 'Column header units and definitions'
         write(5044,*) 'salt ion values provided for each category'
         write(5044,*) 
         write(5044,*) 'in:          kg       salt mass entering reservoir with inflow' 
         write(5044,*) 'out:         kg       salt mass leaving reservoir with outflow'
         write(5044,*) 'seep:        kg       salt mass leaving reservoir via seepage'
         write(5044,*) 'fert:        kg       salt mass added to reservoir via fertilizer'
         write(5044,*) 'irr:         kg       salt mass removed from reservoir for irrigation'
         write(5044,*) 'div:         kg       salt mass removed (-) or added (+) via diversion'
         write(5044,*) 'mass:        kg       salt mass stored in reservoir (averaged over year)'
         write(5044,*) 'conc:        g/m3     salt concentration in reservoir water (averaged over year)'
         write(5044,*) 'vol_m3:      m3       volume of water stored in reservoir (averaged over year)'
         write(5044,*) 
         write(5044,*) ressalt_hdr
         if (pco%csvout == "y") then
           open (5045,file="reservoir_salt_yr.csv",recl=2000)
           write (5045,*) bsn%name, prog
           write (5045,'(*(G0.3,:","))') ressalt_hdr
         endif
       endif
     endif
         
     !reservoir - ave annual
     if (sp_ob%res > 0) then
       if (pco%salt_res%a == "y" .and. cs_db%num_salts > 0) then
         open (5046,file="reservoir_salt_aa.txt",recl=2000)
         write(5046,*) bsn%name, prog
         write(5046,*) 
         write(5046,*) 'Reservoir salt fluxes and state variables - average annual'
         write(5046,*)
         write(5046,*) 'Column header units and definitions'
         write(5046,*) 'salt ion values provided for each category'
         write(5046,*) 
         write(5046,*) 'in:          kg/yr    salt mass entering reservoir with inflow' 
         write(5046,*) 'out:         kg/yr    salt mass leaving reservoir with outflow'
         write(5046,*) 'seep:        kg/yr    salt mass leaving reservoir via seepage'
         write(5046,*) 'fert:        kg/yr    salt mass added to reservoir via fertilizer'
         write(5046,*) 'irr:         kg/yr    salt mass removed from reservoir for irrigation'
         write(5046,*) 'div:         kg/yr    salt mass removed (-) or added (+) via diversion'
         write(5046,*) 'mass:        kg/yr    salt mass stored in reservoir (averaged over years)'
         write(5046,*) 'conc:        g/m3     salt concentration in reservoir water (averaged over years)'
         write(5046,*) 'vol_m3:      m3       volume of water stored in reservoir (averaged over years)'
         write(5046,*) 
         write(5046,*) ressalt_hdr
         if (pco%csvout == "y") then
           open (5047,file="reservoir_salt_aa.csv",recl=2000)
           write (5047,*) bsn%name, prog
           write (5047,'(*(G0.3,:","))') ressalt_hdr
         endif
       endif
     endif 
      
     !routing unit - daily ----------------------------------------------------------------------------------------------------------------
     if (sp_ob%ru > 0) then
       if (pco%salt_ru%d == "y" .and. cs_db%num_salts > 0) then
         open (5070,file="rout_unit_salt_day.txt",recl=2000)
         write(5070,*) bsn%name, prog
         write(5070,*) 
         write(5070,*) 'Routing unit salt fluxes - daily'
         write(5070,*)
         write(5070,*) 'Column header units and definitions'
         write(5070,*) 'salt ion values provided for each category'
         write(5070,*) 
         write(5070,*) 'total:        kg      salt mass = perc + surq + latq + tile'
         write(5070,*) 'perc:         kg      salt mass in deep percolation          (soil --> aquifer)'
         write(5070,*) 'surq:         kg      salt mass in surface runoff            (soil --> stream)'
         write(5070,*) 'latq:         kg      salt mass in soil lateral flow         (soil --> stream)'
         write(5070,*) 'tile:         kg      salt mass in tile drainage             (soil --> stream)'
         write(5070,*) 'wtsp:         kg      salt mass in wetland seepage           (wetland --> soil)'
         write(5070,*) 'irsw:         kg      salt mass in surfac water irrigation   (channel --> soil)'
         write(5070,*) 'irgw:         kg      salt mass in groundwater irrigation    (aquifer --> soil)'
         write(5070,*) 'irwo:         kg      salt mass in outside source irrigation (outside --> soil)'
         write(5070,*) 'rain:         kg      salt mass in rainfall                  (outside --> soil)'
         write(5070,*) 'dryd:         kg      salt mass in dry deposition            (outside --> soil)'
         write(5070,*) 'road:         kg      salt mass in applied road salt         (outside --> soil)'
         write(5070,*) 'fert:         kg      salt mass in applied fertilizer        (outside --> soil)'
         write(5070,*) 'amnd:         kg      salt mass in soil amendments           (outside --> soil)'
         write(5070,*) 'uptk:         kg      salt mass in plant uptake              (soil --> outside)'
         write(5070,*) 'dssl:         kg      salt mineral dissolution (summed over soil layers)'
         write(5070,*) 
         write(5070,*) rusaltb_hdr
         if (pco%csvout == "y") then
           open (5071,file="rout_unit_salt_day.csv",recl=2000)
           write (5071,*) bsn%name, prog
           write (5071,*) 'all values in kg'
           write (5071,'(*(G0.3,:","))') rusaltb_hdr
         endif
       endif
     endif
     
     !routing unit - monthly
     if (sp_ob%ru > 0) then
       if (pco%salt_ru%m == "y" .and. cs_db%num_salts > 0) then
         open (5072,file="rout_unit_salt_mon.txt",recl=2000)
         write(5072,*) bsn%name, prog
         write(5072,*) 
         write(5072,*) 'Routing unit salt fluxes - monthly'
         write(5072,*)
         write(5072,*) 'Column header units and definitions'
         write(5072,*) 'salt ion values provided for each category'
         write(5072,*) 
         write(5072,*) 'total:        kg      salt mass = perc + surq + latq + tile'
         write(5072,*) 'perc:         kg      salt mass in deep percolation          (soil --> aquifer)'
         write(5072,*) 'surq:         kg      salt mass in surface runoff            (soil --> stream)'
         write(5072,*) 'latq:         kg      salt mass in soil lateral flow         (soil --> stream)'
         write(5072,*) 'tile:         kg      salt mass in tile drainage             (soil --> stream)'
         write(5072,*) 'wtsp:         kg      salt mass in wetland seepage           (wetland --> soil)'
         write(5072,*) 'irsw:         kg      salt mass in surfac water irrigation   (channel --> soil)'
         write(5072,*) 'irgw:         kg      salt mass in groundwater irrigation    (aquifer --> soil)'
         write(5072,*) 'irwo:         kg      salt mass in outside source irrigation (outside --> soil)'
         write(5072,*) 'rain:         kg      salt mass in rainfall                  (outside --> soil)'
         write(5072,*) 'dryd:         kg      salt mass in dry deposition            (outside --> soil)'
         write(5072,*) 'road:         kg      salt mass in applied road salt         (outside --> soil)'
         write(5072,*) 'fert:         kg      salt mass in applied fertilizer        (outside --> soil)'
         write(5072,*) 'amnd:         kg      salt mass in soil amendments           (outside --> soil)'
         write(5072,*) 'uptk:         kg      salt mass in plant uptake              (soil --> outside)'
         write(5072,*) 'dssl:         kg      salt mineral dissolution (summed over soil layers)'
         write(5072,*) 
         write(5072,*) rusaltb_hdr
         if (pco%csvout == "y") then
           open (5073,file="rout_unit_salt_mon.csv",recl=2000)
           write (5073,*) bsn%name, prog
           write (5073,*) 'all values in kg'
           write (5073,'(*(G0.3,:","))') rusaltb_hdr
         endif
       endif
     endif

     !routing unit - yearly
     if (sp_ob%ru > 0) then
       if (pco%salt_ru%y == "y" .and. cs_db%num_salts > 0) then
         open (5074,file="rout_unit_salt_yr.txt",recl=2000)
         write(5074,*) bsn%name, prog
         write(5074,*) 
         write(5074,*) 'Routing unit salt fluxes - yearly'
         write(5074,*)
         write(5074,*) 'Column header units and definitions'
         write(5074,*) 'salt ion values provided for each category'
         write(5074,*) 
         write(5074,*) 'total:        kg      salt mass = perc + surq + latq + tile'
         write(5074,*) 'perc:         kg      salt mass in deep percolation          (soil --> aquifer)'
         write(5074,*) 'surq:         kg      salt mass in surface runoff            (soil --> stream)'
         write(5074,*) 'latq:         kg      salt mass in soil lateral flow         (soil --> stream)'
         write(5074,*) 'tile:         kg      salt mass in tile drainage             (soil --> stream)'
         write(5074,*) 'wtsp:         kg      salt mass in wetland seepage           (wetland --> soil)'
         write(5074,*) 'irsw:         kg      salt mass in surfac water irrigation   (channel --> soil)'
         write(5074,*) 'irgw:         kg      salt mass in groundwater irrigation    (aquifer --> soil)'
         write(5074,*) 'irwo:         kg      salt mass in outside source irrigation (outside --> soil)'
         write(5074,*) 'rain:         kg      salt mass in rainfall                  (outside --> soil)'
         write(5074,*) 'dryd:         kg      salt mass in dry deposition            (outside --> soil)'
         write(5074,*) 'road:         kg      salt mass in applied road salt         (outside --> soil)'
         write(5074,*) 'fert:         kg      salt mass in applied fertilizer        (outside --> soil)'
         write(5074,*) 'amnd:         kg      salt mass in soil amendments           (outside --> soil)'
         write(5074,*) 'uptk:         kg      salt mass in plant uptake              (soil --> outside)'
         write(5074,*) 'dssl:         kg      salt mineral dissolution (summed over soil layers)'
         write(5074,*) 
         write(5074,*) rusaltb_hdr
         if (pco%csvout == "y") then
           open (5075,file="rout_unit_salt_yr.csv",recl=2000)
           write (5075,*) bsn%name, prog
           write (5075,*) 'all values in kg'
           write (5075,'(*(G0.3,:","))') rusaltb_hdr
         endif
       endif
     endif
         
     !routing unit - ave annual
     if (sp_ob%ru > 0) then
       if (pco%salt_ru%a == "y" .and. cs_db%num_salts > 0) then
         open (5076,file="rout_unit_salt_aa.txt",recl=2000)
         write(5076,*) bsn%name, prog
         write(5076,*) 
         write(5076,*) 'Routing unit salt fluxes - average annual'
         write(5076,*)
         write(5076,*) 'Column header units and definitions'
         write(5076,*) 'salt ion values provided for each category'
         write(5076,*) 
         write(5076,*) 'total:        kg/yr   salt mass = perc + surq + latq + tile'
         write(5076,*) 'perc:         kg/yr   salt mass in deep percolation          (soil --> aquifer)'
         write(5076,*) 'surq:         kg/yr   salt mass in surface runoff            (soil --> stream)'
         write(5076,*) 'latq:         kg/yr   salt mass in soil lateral flow         (soil --> stream)'
         write(5076,*) 'tile:         kg/yr   salt mass in tile drainage             (soil --> stream)'
         write(5076,*) 'wtsp:         kg/yr   salt mass in wetland seepage           (wetland --> soil)'
         write(5076,*) 'irsw:         kg/yr   salt mass in surfac water irrigation   (channel --> soil)'
         write(5076,*) 'irgw:         kg/yr   salt mass in groundwater irrigation    (aquifer --> soil)'
         write(5076,*) 'irwo:         kg/yr   salt mass in outside source irrigation (outside --> soil)'
         write(5076,*) 'rain:         kg/yr   salt mass in rainfall                  (outside --> soil)'
         write(5076,*) 'dryd:         kg/yr   salt mass in dry deposition            (outside --> soil)'
         write(5076,*) 'road:         kg/yr   salt mass in applied road salt         (outside --> soil)'
         write(5076,*) 'fert:         kg/yr   salt mass in applied fertilizer        (outside --> soil)'
         write(5076,*) 'amnd:         kg/yr   salt mass in soil amendments           (outside --> soil)'
         write(5076,*) 'uptk:         kg/yr   salt mass in plant uptake              (soil --> outside)'
         write(5076,*) 'dssl:         kg/yr   salt mineral dissolution (summed over soil layers)'
         write(5076,*) 
         write(5076,*) rusaltb_hdr
         if (pco%csvout == "y") then
           open (5077,file="rout_unit_salt_aa.csv",recl=2000)
           write (5077,*) bsn%name, prog
           write (5077,*) 'all values in kg'
           write (5077,'(*(G0.3,:","))') rusaltb_hdr
         endif
       endif
     endif 
     
     !wetland - daily -------------------------------------------------------------------------------------------------------------------
     if (sp_ob%res > 0) then
       if (pco%salt_wet%d == "y" .and. cs_db%num_salts > 0) then
         open (5090,file="wetland_salt_day.txt",recl=2000)
         write(5090,*) bsn%name, prog
         write(5090,*) 
         write(5090,*) 'Wetland salt fluxes and state variables - daily'
         write(5090,*)
         write(5090,*) 'Column header units and definitions'
         write(5090,*) 'salt ion values provided for each category'
         write(5090,*) 
         write(5090,*) 'in:          kg       salt mass entering wetland with inflow' 
         write(5090,*) 'out:         kg       salt mass leaving wetland with outflow'
         write(5090,*) 'seep:        kg       salt mass leaving wetland via seepage'
         write(5090,*) 'fert:        kg       salt mass added to wetland via fertilizer'
         write(5090,*) 'irr:         kg       salt mass added via irrigation'
         write(5090,*) 'div:         kg       salt mass removed (-) or added (+) via diversion'
         write(5090,*) 'mass:        kg       salt mass stored in wetland at beginning of day'
         write(5090,*) 'conc:        g/m3     salt concentration in wetland water at beginning of day'
         write(5090,*) 'vol_m3:      m3       volume of water stored in wetland at beginning of day'
         write(5090,*) 
         write(5090,*) ressalt_hdr
         if (pco%csvout == "y") then
           open (5091,file="wetland_salt_day.csv",recl=2000)
           write (5091,*) bsn%name, prog
           write (5091,'(*(G0.3,:","))') ressalt_hdr
         endif
       endif
     endif
     
     !wetland - monthly
     if (sp_ob%res > 0) then
       if (pco%salt_wet%m == "y" .and. cs_db%num_salts > 0) then
         open (5092,file="wetland_salt_mon.txt",recl=2000)
         write(5092,*) bsn%name, prog
         write(5092,*) 
         write(5092,*) 'Wetland salt fluxes and state variables - monthly'
         write(5092,*)
         write(5092,*) 'Column header units and definitions'
         write(5092,*) 'salt ion values provided for each category'
         write(5092,*) 
         write(5092,*) 'in:          kg       salt mass entering wetland with inflow' 
         write(5092,*) 'out:         kg       salt mass leaving wetland with outflow'
         write(5092,*) 'seep:        kg       salt mass leaving wetland via seepage'
         write(5092,*) 'fert:        kg       salt mass added to wetland via fertilizer'
         write(5092,*) 'irr:         kg       salt mass added via irrigation'
         write(5092,*) 'div:         kg       salt mass removed (-) or added (+) via diversion'
         write(5092,*) 'mass:        kg       salt mass stored in wetland'
         write(5092,*) 'conc:        g/m3     salt concentration in wetland water (averaged over month)'
         write(5092,*) 'vol_m3:      m3       volume of water stored in wetland (averaged over month)'
         write(5092,*) 
         write(5092,*) ressalt_hdr
         if (pco%csvout == "y") then
           open (5093,file="wetland_salt_mon.csv",recl=2000)
           write (5093,*) bsn%name, prog
           write (5093,'(*(G0.3,:","))') ressalt_hdr
         endif
       endif
     endif

     !wetland - yearly
     if (sp_ob%res > 0) then
       if (pco%salt_wet%y == "y" .and. cs_db%num_salts > 0) then
         open (5094,file="wetland_salt_yr.txt",recl=2000)
         write(5094,*) bsn%name, prog
         write(5094,*) 
         write(5094,*) 'Wetland salt fluxes and state variables - yearly'
         write(5094,*)
         write(5094,*) 'Column header units and definitions'
         write(5094,*) 'salt ion values provided for each category'
         write(5094,*) 
         write(5094,*) 'in:          kg       salt mass entering wetland with inflow' 
         write(5094,*) 'out:         kg       salt mass leaving wetland with outflow'
         write(5094,*) 'seep:        kg       salt mass leaving wetland via seepage'
         write(5094,*) 'fert:        kg       salt mass added to wetland via fertilizer'
         write(5094,*) 'irr:         kg       salt mass added via irrigation'
         write(5094,*) 'div:         kg       salt mass removed (-) or added (+) via diversion'
         write(5094,*) 'mass:        kg       salt mass stored in wetland'
         write(5094,*) 'conc:        g/m3     salt concentration in wetland water (averaged over year)'
         write(5094,*) 'vol_m3:      m3       volume of water stored in wetland (averaged over year)'
         write(5094,*) 
         write(5094,*) ressalt_hdr
         if (pco%csvout == "y") then
           open (5095,file="wetland_salt_yr.csv",recl=2000)
           write (5095,*) bsn%name, prog
           write (5095,'(*(G0.3,:","))') ressalt_hdr
         endif
       endif
     endif
         
     !wetland - ave annual
     if (sp_ob%res > 0) then
       if (pco%salt_wet%a == "y" .and. cs_db%num_salts > 0) then
         open (5096,file="wetland_salt_aa.txt",recl=2000)
         write(5096,*) bsn%name, prog
         write(5096,*) 
         write(5096,*) 'Wetland salt fluxes and state variables - average annual'
         write(5096,*)
         write(5096,*) 'Column header units and definitions'
         write(5096,*) 'salt ion values provided for each category'
         write(5096,*) 
         write(5096,*) 'in:          kg/yr    salt mass entering wetland with inflow' 
         write(5096,*) 'out:         kg/yr    salt mass leaving wetland with outflow'
         write(5096,*) 'seep:        kg/yr    salt mass leaving wetland via seepage'
         write(5096,*) 'fert:        kg/yr    salt mass added to wetland via fertilizer'
         write(5096,*) 'irr:         kg/yr    salt mass added via irrigation'
         write(5096,*) 'div:         kg/yr    salt mass removed (-) or added (+) via diversion'
         write(5096,*) 'mass:        kg/yr    salt mass stored in wetland'
         write(5096,*) 'conc:        g/m3     salt concentration in wetland water (averaged over years)'
         write(5096,*) 'vol_m3:      m3       volume of water stored in wetland (averaged over years)'
         write(5096,*) 
         write(5096,*) ressalt_hdr
         if (pco%csvout == "y") then
           open (5097,file="wetland_salt_aa.csv",recl=2000)
           write (5097,*) bsn%name, prog
           write (5097,'(*(G0.3,:","))') ressalt_hdr
         endif
       endif
     endif
        
      return
      end subroutine header_salt