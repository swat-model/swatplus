     subroutine header_const !rtb cs
    
     use basin_module
     use reservoir_module
     use hydrograph_module, only : res, sp_ob
     use constituent_mass_module
     use ch_cs_module
     use res_cs_module
     use cs_module
     use cs_aquifer
     
     implicit none 


     !basin - daily -----------------------------------------------------------------------------------------------------------------------
     if (pco%cs_basin%d == "y" .and. cs_db%num_cs > 0) then
       open (6080,file="basin_cs_day.txt", recl = 2000)
       write(6080,*) bsn%name, prog
       write(6080,*)
       write(6080,*) 'Basin constituent fluxes - daily'
       write(6080,*)
       write(6080,*) 'Column header units and definitions'
       write(6080,*) 'values for seo4, seo3, and boron'
       write(6080,*) 
       write(6080,*) 'latq:         kg      mass in soil lateral flow            (soil --> stream)'
       write(6080,*) 'surq:         kg      mass in surface runoff               (soil --> stream)'
       write(6080,*) 'sedm:         kg      mass in sediment runoff              (soil --> stream)'
       write(6080,*) 'urbq:         kg      mass in urban runoff                 (soil --> stream)'
       write(6080,*) 'wetq:         kg      mass in wetland runoff               (wetland --> stream)'
       write(6080,*) 'tile:         kg      mass in tile drain flow              (soil --> stream)'
       write(6080,*) 'perc:         kg      mass in deep percolation             (soil --> aquifer)'
       write(6080,*) 'gwup:         kg      mass from groundwater transfer       (aquifer --> soil)'
       write(6080,*) 'wtsp:         kg      mass in wetland seepage              (wetland --> soil)'
       write(6080,*) 'irsw:         kg      mass in surface water irrigation     (channel --> soil)'
       write(6080,*) 'irgw:         kg      mass in groundwater irrigation       (aquifer --> soil)'
       write(6080,*) 'irwo:         kg      mass in outside source irrigation    (outside --> soil)'
       write(6080,*) 'rain:         kg      mass in rainfall                     (outside --> soil)'
       write(6080,*) 'dryd:         kg      mass in dry deposition               (outside --> soil)'
       write(6080,*) 'fert:         kg      mass in applied fertilizer           (outside --> soil)'
       write(6080,*) 'uptk:         kg      mass in plant uptake                 (soil --> outside)'
       write(6080,*) 'rct_sl:       kg      mass transferred via reaction        (soil --> transferred)'
       write(6080,*) 'srb_sl:       kg      mass transferred via sorption        (soil --> sorbed mass)'
       write(6080,*) 'ptso:         kg      mass in watershed point sources      (outside --> channel)'
       write(6080,*) 'ptout:        kg      mass in outside point sources        (outside --> channel)'
       write(6080,*) 'dis_sl:       kg      total mass in soil water'
       write(6080,*) 'sbd_sl:       kg      total sorbed mass in soil profile'
       write(6080,*) 'gw:           kg      mass in groundwater discharge        (aquifer --> channel)'
       write(6080,*) 'rchg:         kg      mass in groundwater recharge         (soil --> aquifer)'
       write(6080,*) 'seep:         kg      mass in groundwater seepage          (aquifer --> deep)'
       write(6080,*) 'rct_aq:       kg      mass transferred via reaction        (aquifer --> transferred)'
       write(6080,*) 'srb_aq:       kg      mass transferred via sorption        (aquifer --> sorbed mass)'
       write(6080,*) 'dis_aq:       kg      total mass in groundwater'
       write(6080,*) 'sbd_aq:       kg      total sorbed mass in aquifer'
       write(6080,*)
       write(6080,*) csb_hdr
     endif
     
     !basin - monthly
     if (pco%cs_basin%m == "y" .and. cs_db%num_cs > 0) then
       open (6082,file="basin_cs_mon.txt", recl = 2000)
       write(6082,*) bsn%name, prog
       write(6082,*)
       write(6082,*) 'Basin constituent fluxes - monthly'
       write(6082,*)
       write(6082,*) 'Column header units and definitions'
       write(6082,*) 'values for seo4, seo3, and boron'
       write(6082,*) 
       write(6082,*) 'latq:         kg      mass in soil lateral flow            (soil --> stream)'
       write(6082,*) 'surq:         kg      mass in surface runoff               (soil --> stream)'
       write(6082,*) 'sedm:         kg      mass in sediment runoff              (soil --> stream)'
       write(6082,*) 'urbq:         kg      mass in urban runoff                 (soil --> stream)'
       write(6082,*) 'wetq:         kg      mass in wetland runoff               (wetland --> stream)'
       write(6082,*) 'tile:         kg      mass in tile drain flow              (soil --> stream)'
       write(6082,*) 'perc:         kg      mass in deep percolation             (soil --> aquifer)'
       write(6082,*) 'gwup:         kg      mass from groundwater transfer       (aquifer --> soil)'
       write(6082,*) 'wtsp:         kg      mass in wetland seepage              (wetland --> soil)'
       write(6082,*) 'irsw:         kg      mass in surface water irrigation     (channel --> soil)'
       write(6082,*) 'irgw:         kg      mass in groundwater irrigation       (aquifer --> soil)'
       write(6082,*) 'irwo:         kg      mass in outside source irrigation    (outside --> soil)'
       write(6082,*) 'rain:         kg      mass in rainfall                     (outside --> soil)'
       write(6082,*) 'dryd:         kg      mass in dry deposition               (outside --> soil)'
       write(6082,*) 'fert:         kg      mass in applied fertilizer           (outside --> soil)'
       write(6082,*) 'uptk:         kg      mass in plant uptake                 (soil --> outside)'
       write(6082,*) 'rct_sl:       kg      mass transferred via reaction        (soil --> transferred)'
       write(6082,*) 'srb_sl:       kg      mass transferred via sorption        (soil --> sorbed mass)'
       write(6082,*) 'ptso:         kg      mass in watershed point sources      (outside --> channel)'
       write(6082,*) 'ptout:        kg      mass in outside point sources        (outside --> channel)'
       write(6082,*) 'dis_sl:       kg      total mass in soil water             (averaged over month)'
       write(6082,*) 'sbd_sl:       kg      total sorbed mass in soil profile    (averaged over month)'
       write(6082,*) 'gw:           kg      mass in groundwater discharge        (aquifer --> channel)'
       write(6082,*) 'rchg:         kg      mass in groundwater recharge         (soil --> aquifer)'
       write(6082,*) 'seep:         kg      mass in groundwater seepage          (aquifer --> deep)'
       write(6082,*) 'rct_aq:       kg      mass transferred via reaction        (aquifer --> transferred)'
       write(6082,*) 'srb_aq:       kg      mass transferred via sorption        (aquifer --> sorbed mass)'
       write(6082,*) 'dis_aq:       kg      total mass in groundwater            (averaged over month)'
       write(6082,*) 'sbd_aq:       kg      total sorbed mass in aquifer         (averaged over month)'
       write(6082,*)
       write(6082,*) csb_hdr
     endif
     
     !basin - yearly
     if (pco%cs_basin%y == "y" .and. cs_db%num_cs > 0) then
       open (6084,file="basin_cs_yr.txt", recl = 2000)
       write(6084,*) bsn%name, prog
       write(6084,*)
       write(6084,*) 'Basin constituent fluxes - yearly'
       write(6084,*)
       write(6084,*) 'Column header units and definitions'
       write(6084,*) 'values for seo4, seo3, and boron'
       write(6084,*) 
       write(6084,*) 'latq:         kg      mass in soil lateral flow            (soil --> stream)'
       write(6084,*) 'surq:         kg      mass in surface runoff               (soil --> stream)'
       write(6084,*) 'sedm:         kg      mass in sediment runoff              (soil --> stream)'
       write(6084,*) 'urbq:         kg      mass in urban runoff                 (soil --> stream)'
       write(6084,*) 'wetq:         kg      mass in wetland runoff               (wetland --> stream)'
       write(6084,*) 'tile:         kg      mass in tile drain flow              (soil --> stream)'
       write(6084,*) 'perc:         kg      mass in deep percolation             (soil --> aquifer)'
       write(6084,*) 'gwup:         kg      mass from groundwater transfer       (aquifer --> soil)'
       write(6084,*) 'wtsp:         kg      mass in wetland seepage              (wetland --> soil)'
       write(6084,*) 'irsw:         kg      mass in surface water irrigation     (channel --> soil)'
       write(6084,*) 'irgw:         kg      mass in groundwater irrigation       (aquifer --> soil)'
       write(6084,*) 'irwo:         kg      mass in outside source irrigation    (outside --> soil)'
       write(6084,*) 'rain:         kg      mass in rainfall                     (outside --> soil)'
       write(6084,*) 'dryd:         kg      mass in dry deposition               (outside --> soil)'
       write(6084,*) 'fert:         kg      mass in applied fertilizer           (outside --> soil)'
       write(6084,*) 'uptk:         kg      mass in plant uptake                 (soil --> outside)'
       write(6084,*) 'rct_sl:       kg      mass transferred via reaction        (soil --> transferred)'
       write(6084,*) 'srb_sl:       kg      mass transferred via sorption        (soil --> sorbed mass)'
       write(6084,*) 'ptso:         kg      mass in watershed point sources      (outside --> channel)'
       write(6084,*) 'ptout:        kg      mass in outside point sources        (outside --> channel)'
       write(6084,*) 'dis_sl:       kg      total mass in soil water             (averaged over year)'
       write(6084,*) 'sbd_sl:       kg      total sorbed mass in soil profile    (averaged over year)'
       write(6084,*) 'gw:           kg      mass in groundwater discharge        (aquifer --> channel)'
       write(6084,*) 'rchg:         kg      mass in groundwater recharge         (soil --> aquifer)'
       write(6084,*) 'seep:         kg      mass in groundwater seepage          (aquifer --> deep)'
       write(6084,*) 'rct_aq:       kg      mass transferred via reaction        (aquifer --> transferred)'
       write(6084,*) 'srb_aq:       kg      mass transferred via sorption        (aquifer --> sorbed mass)'
       write(6084,*) 'dis_aq:       kg      total mass in groundwater            (averaged over year)'
       write(6084,*) 'sbd_aq:       kg      total sorbed mass in aquifer         (averaged over year)'
       write(6084,*)
       write(6084,*) csb_hdr
     endif
     
     !basin - ave annual
     if (pco%cs_basin%a == "y" .and. cs_db%num_cs > 0) then
       open (6086,file="basin_cs_aa.txt", recl = 2000)
       write(6086,*) bsn%name, prog
       write(6086,*)
       write(6086,*) 'Basin constituent fluxes - average annual'
       write(6086,*)
       write(6086,*) 'Column header units and definitions'
       write(6086,*) 'values for seo4, seo3, and boron'
       write(6086,*) 
       write(6086,*) 'latq:         kg/yr   mass in soil lateral flow            (soil --> stream)'
       write(6086,*) 'surq:         kg/yr   mass in surface runoff               (soil --> stream)'
       write(6086,*) 'sedm:         kg/yr   mass in sediment runoff              (soil --> stream)'
       write(6086,*) 'urbq:         kg/yr   mass in urban runoff                 (soil --> stream)'
       write(6086,*) 'wetq:         kg/yr   mass in wetland runoff               (wetland --> stream)'
       write(6086,*) 'tile:         kg/yr   mass in tile drain flow              (soil --> stream)'
       write(6086,*) 'perc:         kg/yr   mass in deep percolation             (soil --> aquifer)'
       write(6086,*) 'gwup:         kg/yr   mass from groundwater transfer       (aquifer --> soil)'
       write(6086,*) 'wtsp:         kg/yr   mass in wetland seepage              (wetland --> soil)'
       write(6086,*) 'irsw:         kg/yr   mass in surface water irrigation     (channel --> soil)'
       write(6086,*) 'irgw:         kg/yr   mass in groundwater irrigation       (aquifer --> soil)'
       write(6086,*) 'irwo:         kg/yr   mass in outside source irrigation    (outside --> soil)'
       write(6086,*) 'rain:         kg/yr   mass in rainfall                     (outside --> soil)'
       write(6086,*) 'dryd:         kg/yr   mass in dry deposition               (outside --> soil)'
       write(6086,*) 'fert:         kg/yr   mass in applied fertilizer           (outside --> soil)'
       write(6086,*) 'uptk:         kg/yr   mass in plant uptake                 (soil --> outside)'
       write(6086,*) 'rct_sl:       kg/yr   mass transferred via reaction        (soil --> transferred)'
       write(6086,*) 'srb_sl:       kg/yr   mass transferred via sorption        (soil --> sorbed mass)'
       write(6086,*) 'ptso:         kg/yr   mass in watershed point sources      (outside --> channel)'
       write(6086,*) 'ptout:        kg/yr   mass in outside point sources        (outside --> channel)'
       write(6086,*) 'dis_sl:       kg/yr   total mass in soil water             (averaged over years)'
       write(6086,*) 'sbd_sl:       kg/yr   total sorbed mass in soil profile    (averaged over years)'
       write(6086,*) 'gw:           kg/yr   mass in groundwater discharge        (aquifer --> channel)'
       write(6086,*) 'rchg:         kg/yr   mass in groundwater recharge         (soil --> aquifer)'
       write(6086,*) 'seep:         kg/yr   mass in groundwater seepage          (aquifer --> deep)'
       write(6086,*) 'rct_aq:       kg/yr   mass transferred via reaction        (aquifer --> transferred)'
       write(6086,*) 'srb_aq:       kg/yr   mass transferred via sorption        (aquifer --> sorbed mass)'
       write(6086,*) 'dis_aq:       kg/yr   total mass in groundwater            (averaged over years)'
       write(6086,*) 'sbd_aq:       kg/yr   total sorbed mass in aquifer         (averaged over years)'
       write(6086,*)
       write(6086,*) csb_hdr
     endif
     
     !hru - daily -------------------------------------------------------------------------------------------------------------------------
     if (pco%cs_hru%d == "y" .and. cs_db%num_cs > 0) then
       open (6021,file="hru_cs_day.txt", recl = 3000)
       write(6021,*) bsn%name, prog
       write(6021,*) 
       write(6021,*) 'HRU constituent fluxes and state variables - daily'
       write(6021,*)
       write(6021,*) 'Column header units and definitions'
       write(6021,*) 'values provided for seo4, seo3, and boron'
       write(6021,*) 
       write(6021,*) 'sl:           kg/ha   total mass in soil profile'
       write(6021,*) 'surq:         kg/ha   mass in surface runoff               (soil --> stream)'
       write(6021,*) 'sedm:         kg/ha   mass in sediment runoff              (soil --> stream)'
       write(6021,*) 'latq:         kg/ha   mass in soil lateral flow            (soil --> stream)'
       write(6021,*) 'urbq:         kg/ha   mass in urban runoff                 (soil --> stream)'
       write(6021,*) 'wetq:         kg/ha   mass in wetland runoff               (wetland --> stream)'
       write(6021,*) 'tile:         kg/ha   mass in tile drainage                (soil --> stream)'
       write(6021,*) 'perc:         kg/ha   mass in deep percolation             (soil --> aquifer)'
       write(6021,*) 'gwup:         kg/ha   mass from groundwater transfer       (aquifer --> soil)'
       write(6021,*) 'wtsp:         kg/ha   mass in wetland seepage              (wetland --> soil)'
       write(6021,*) 'irsw:         kg/ha   mass in surfac water irrigation      (channel --> soil)'
       write(6021,*) 'irgw:         kg/ha   mass in groundwater irrigation       (aquifer --> soil)'
       write(6021,*) 'irwo:         kg/ha   mass in outside source irrigation    (outside --> soil)'
       write(6021,*) 'rain:         kg/ha   mass in rainfall                     (outside --> soil)'
       write(6021,*) 'dryd:         kg/ha   mass in dry deposition               (outside --> soil)'
       write(6021,*) 'fert:         kg/ha   mass in applied fertilizer           (outside --> soil)'
       write(6021,*) 'uptk:         kg/ha   mass in plant uptake                 (soil --> outside)'
       write(6021,*) 'rctn:         kg/ha   mass transferred via reaction        (soil --> transferred)'
       write(6021,*) 'sorb:         kg/ha   mass transferred via sorption        (soil --> sorbed mass)'
       write(6021,*) 'conc:         g/m3    concentration in soil water'
       write(6021,*) 'srbd:         kg/ha   mass sorbed in soil profile'
       write(6021,*) 
       write(6021,*) cs_hdr_hru
       if (pco%csvout == "y") then
         open (6022,file="hru_cs_day.csv", recl = 3000)
         write (6022,*) bsn%name, prog
         write (6022,*) 'conc = mg/L; other = kg/ha'
         write (6022,'(*(G0.3,:","))') cs_hdr_hru
       endif
     endif
     
     !hru - monthly
     if (pco%cs_hru%m == "y" .and. cs_db%num_cs > 0) then
       open (6023,file="hru_cs_mon.txt", recl = 3000)
       write(6023,*) bsn%name, prog
       write(6023,*) 
       write(6023,*) 'HRU constituent fluxes and state variables - monthly'
       write(6023,*)
       write(6023,*) 'Column header units and definitions'
       write(6023,*) 'values provided for seo4, seo3, and boron'
       write(6023,*) 
       write(6023,*) 'sl:           kg/ha   total mass in soil profile'
       write(6023,*) 'surq:         kg/ha   mass in surface runoff               (soil --> stream)'
       write(6023,*) 'sedm:         kg/ha   mass in sediment runoff              (soil --> stream)'
       write(6023,*) 'latq:         kg/ha   mass in soil lateral flow            (soil --> stream)'
       write(6023,*) 'urbq:         kg/ha   mass in urban runoff                 (soil --> stream)'
       write(6023,*) 'wetq:         kg/ha   mass in wetland runoff               (wetland --> stream)'
       write(6023,*) 'tile:         kg/ha   mass in tile drainage                (soil --> stream)'
       write(6023,*) 'perc:         kg/ha   mass in deep percolation             (soil --> aquifer)'
       write(6023,*) 'gwup:         kg/ha   mass from groundwater transfer       (aquifer --> soil)'
       write(6023,*) 'wtsp:         kg/ha   mass in wetland seepage              (wetland --> soil)'
       write(6023,*) 'irsw:         kg/ha   mass in surfac water irrigation      (channel --> soil)'
       write(6023,*) 'irgw:         kg/ha   mass in groundwater irrigation       (aquifer --> soil)'
       write(6023,*) 'irwo:         kg/ha   mass in outside source irrigation    (outside --> soil)'
       write(6023,*) 'rain:         kg/ha   mass in rainfall                     (outside --> soil)'
       write(6023,*) 'dryd:         kg/ha   mass in dry deposition               (outside --> soil)'
       write(6023,*) 'fert:         kg/ha   mass in applied fertilizer           (outside --> soil)'
       write(6023,*) 'uptk:         kg/ha   mass in plant uptake                 (soil --> outside)'
       write(6023,*) 'rctn:         kg/ha   mass transferred via reaction        (soil --> transferred)'
       write(6023,*) 'sorb:         kg/ha   mass transferred via sorption        (soil --> sorbed mass)'
       write(6023,*) 'conc:         g/m3    concentration in soil water          (averaged over month)'
       write(6023,*) 'srbd:         kg/ha   mass sorbed in soil profile          (averaged over month)'
       write(6023,*) 
       write(6023,*) cs_hdr_hru
       if (pco%csvout == "y") then
         open (6024,file="hru_cs_mon.csv", recl = 3000)
         write (6024,*) bsn%name, prog
         write (6024,*) 'conc = mg/L; other = kg/ha'
         write (6024,'(*(G0.3,:","))') cs_hdr_hru
       endif
     endif
     
     !hru - yearly
     if (pco%cs_hru%y == "y" .and. cs_db%num_cs > 0) then
       open (6025,file="hru_cs_yr.txt", recl = 3000)
       write(6025,*) bsn%name, prog
       write(6025,*) 
       write(6025,*) 'HRU constituent fluxes and state variables - yearly'
       write(6025,*)
       write(6025,*) 'Column header units and definitions'
       write(6025,*) 'values provided for seo4, seo3, and boron'
       write(6025,*) 
       write(6025,*) 'sl:           kg/ha   total mass in soil profile'
       write(6025,*) 'surq:         kg/ha   mass in surface runoff               (soil --> stream)'
       write(6025,*) 'sedm:         kg/ha   mass in sediment runoff              (soil --> stream)'
       write(6025,*) 'latq:         kg/ha   mass in soil lateral flow            (soil --> stream)'
       write(6025,*) 'urbq:         kg/ha   mass in urban runoff                 (soil --> stream)'
       write(6025,*) 'wetq:         kg/ha   mass in wetland runoff               (wetland --> stream)'
       write(6025,*) 'tile:         kg/ha   mass in tile drainage                (soil --> stream)'
       write(6025,*) 'perc:         kg/ha   mass in deep percolation             (soil --> aquifer)'
       write(6025,*) 'gwup:         kg/ha   mass from groundwater transfer       (aquifer --> soil)'
       write(6025,*) 'wtsp:         kg/ha   mass in wetland seepage              (wetland --> soil)'
       write(6025,*) 'irsw:         kg/ha   mass in surfac water irrigation      (channel --> soil)'
       write(6025,*) 'irgw:         kg/ha   mass in groundwater irrigation       (aquifer --> soil)'
       write(6025,*) 'irwo:         kg/ha   mass in outside source irrigation    (outside --> soil)'
       write(6025,*) 'rain:         kg/ha   mass in rainfall                     (outside --> soil)'
       write(6025,*) 'dryd:         kg/ha   mass in dry deposition               (outside --> soil)'
       write(6025,*) 'fert:         kg/ha   mass in applied fertilizer           (outside --> soil)'
       write(6025,*) 'uptk:         kg/ha   mass in plant uptake                 (soil --> outside)'
       write(6025,*) 'rctn:         kg/ha   mass transferred via reaction        (soil --> transferred)'
       write(6025,*) 'sorb:         kg/ha   mass transferred via sorption        (soil --> sorbed mass)'
       write(6025,*) 'conc:         g/m3    concentration in soil water          (averaged over year)'
       write(6025,*) 'srbd:         kg/ha   mass sorbed in soil profile          (averaged over year)'
       write(6025,*) 
       write(6025,*) cs_hdr_hru
       if (pco%csvout == "y") then
         open (6026,file="hru_cs_yr.csv", recl = 3000)
         write (6026,*) bsn%name, prog
         write (6026,*) 'conc = mg/L; other = kg/ha'
         write (6026,'(*(G0.3,:","))') cs_hdr_hru
       endif
     endif
     
     !hru - ave annual
     if (pco%cs_hru%a == "y" .and. cs_db%num_cs > 0) then
       open (6027,file="hru_cs_aa.txt", recl = 3000)
       write(6027,*) bsn%name, prog
       write(6027,*) 
       write(6027,*) 'HRU constituent fluxes and state variables - average annual'
       write(6027,*)
       write(6027,*) 'Column header units and definitions'
       write(6027,*) 'values provided for seo4, seo3, and boron'
       write(6027,*) 
       write(6027,*) 'sl:           kg/ha/yr   total mass in soil profile'
       write(6027,*) 'surq:         kg/ha/yr   mass in surface runoff               (soil --> stream)'
       write(6027,*) 'sedm:         kg/ha/yr   mass in sediment runoff              (soil --> stream)'
       write(6027,*) 'latq:         kg/ha/yr   mass in soil lateral flow            (soil --> stream)'
       write(6027,*) 'urbq:         kg/ha/yr   mass in urban runoff                 (soil --> stream)'
       write(6027,*) 'wetq:         kg/ha/yr   mass in wetland runoff               (wetland --> stream)'
       write(6027,*) 'tile:         kg/ha/yr   mass in tile drainage                (soil --> stream)'
       write(6027,*) 'perc:         kg/ha/yr   mass in deep percolation             (soil --> aquifer)'
       write(6027,*) 'gwup:         kg/ha/yr   mass from groundwater transfer       (aquifer --> soil)'
       write(6027,*) 'wtsp:         kg/ha/yr   mass in wetland seepage              (wetland --> soil)'
       write(6027,*) 'irsw:         kg/ha/yr   mass in surfac water irrigation      (channel --> soil)'
       write(6027,*) 'irgw:         kg/ha/yr   mass in groundwater irrigation       (aquifer --> soil)'
       write(6027,*) 'irwo:         kg/ha/yr   mass in outside source irrigation    (outside --> soil)'
       write(6027,*) 'rain:         kg/ha/yr   mass in rainfall                     (outside --> soil)'
       write(6027,*) 'dryd:         kg/ha/yr   mass in dry deposition               (outside --> soil)'
       write(6027,*) 'fert:         kg/ha/yr   mass in applied fertilizer           (outside --> soil)'
       write(6027,*) 'uptk:         kg/ha/yr   mass in plant uptake                 (soil --> outside)'
       write(6027,*) 'rctn:         kg/ha/yr   mass transferred via reaction        (soil --> transferred)'
       write(6027,*) 'sorb:         kg/ha/yr   mass transferred via sorption        (soil --> sorbed mass)'
       write(6027,*) 'conc:         g/m3       concentration in soil water          (averaged over years)'
       write(6027,*) 'srbd:         kg/ha      mass sorbed in soil profile          (averaged over years)'
       write(6027,*) 
       write(6027,*) cs_hdr_hru
       if (pco%csvout == "y") then
         open (6028,file="hru_cs_aa.csv", recl = 3000)
         write (6028,*) bsn%name, prog
         write (6028,*) 'conc = mg/L; other = kg/ha'
         write (6028,'(*(G0.3,:","))') cs_hdr_hru
       endif
     endif

     !aquifer - daily ---------------------------------------------------------------------------------------------------------------------
     if (sp_ob%aqu > 0) then
       if (pco%cs_aqu%d == "y" .and. cs_db%num_cs > 0) then
         open (6060,file="aquifer_cs_day.txt",recl=2000)
         write(6060,*) bsn%name, prog
         write(6060,*) 
         write(6060,*) 'Aquifer constituent fluxes and state variables - daily'
         write(6060,*)
         write(6060,*) 'Column header units and definitions'
         write(6060,*) 'values provided for seo4, seo3, and boron'
         write(6060,*) 
         write(6060,*) 'gw:           kg      mass in groundwater discharge        (aquifer --> stream)' 
         write(6060,*) 'rchg:         kg      mass in groundwater recharge         (soil --> aquifer)'
         write(6060,*) 'seep:         kg      mass in groundwater seepage          (aquifer --> deep)'
         write(6060,*) 'irr:          kg      mass in groundwater irrigation       (aquifer --> soil)'
         write(6060,*) 'div:          kg      mass removed (-) or added (+) via diversion'
         write(6060,*) 'sorb:         kg      mass transferred via sorption        (aquifer --> sorbed mass)'
         write(6060,*) 'rctn:         kg      mass transferred via reaction        (aquifer --> transferred)'
         write(6060,*) 'mass:         kg      mass stored in groundwater'
         write(6060,*) 'conc:         g/m3    concentration in groundwater'
         write(6060,*) 'srbd:         kg      mass sorbed in aquifer'
         write(6060,*)
         write(6060,*) cs_hdr_aqu
         if (pco%csvout == "y") then
           open (6061,file="aquifer_cs_day.csv",recl=2000)
           write (6061,*) bsn%name, prog
           write (6061,*) 'conc = mg/L; other = kg'
           write (6061,'(*(G0.3,:","))') cs_hdr_aqu
         endif
       endif
     endif
     
     !aquifer - monthly     
     if (sp_ob%aqu > 0) then
       if (pco%cs_aqu%m == "y" .and. cs_db%num_cs > 0) then
         open (6062,file="aquifer_cs_mon.txt",recl=2000)
         write(6062,*) bsn%name, prog
         write(6062,*) 
         write(6062,*) 'Aquifer constituent fluxes and state variables - monthly'
         write(6062,*)
         write(6062,*) 'Column header units and definitions'
         write(6062,*) 'values provided for seo4, seo3, and boron'
         write(6062,*) 
         write(6062,*) 'gw:           kg      mass in groundwater discharge        (aquifer --> stream)' 
         write(6062,*) 'rchg:         kg      mass in groundwater recharge         (soil --> aquifer)'
         write(6062,*) 'seep:         kg      mass in groundwater seepage          (aquifer --> deep)'
         write(6062,*) 'irr:          kg      mass in groundwater irrigation       (aquifer --> soil)'
         write(6062,*) 'div:          kg      mass removed (-) or added (+) via diversion'
         write(6062,*) 'sorb:         kg      mass transferred via sorption        (aquifer --> sorbed mass)'
         write(6062,*) 'rctn:         kg      mass transferred via reaction        (aquifer --> transferred)'
         write(6062,*) 'mass:         kg      mass stored in groundwater           (averaged over month)'
         write(6062,*) 'conc:         g/m3    concentration in groundwater         (averaged over month)'
         write(6062,*) 'srbd:         kg      mass sorbed in aquifer               (averaged over month)'
         write(6062,*)
         write(6062,*) cs_hdr_aqu
         if (pco%csvout == "y") then
           open (6063,file="aquifer_cs_mon.csv",recl=2000)
           write (6063,*) bsn%name, prog
           write (6063,*) 'conc = mg/L; other = kg'
           write (6063,'(*(G0.3,:","))') cs_hdr_aqu
         endif
       endif
     endif
     
     !aquifer - yearly     
     if (sp_ob%aqu > 0) then
       if (pco%cs_aqu%y == "y" .and. cs_db%num_cs > 0) then
         open (6064,file="aquifer_cs_yr.txt",recl=2000)
         write(6064,*) bsn%name, prog
         write(6064,*) 
         write(6064,*) 'Aquifer constituent fluxes and state variables - yearly'
         write(6064,*)
         write(6064,*) 'Column header units and definitions'
         write(6064,*) 'values provided for seo4, seo3, and boron'
         write(6064,*) 
         write(6064,*) 'gw:           kg      mass in groundwater discharge        (aquifer --> stream)' 
         write(6064,*) 'rchg:         kg      mass in groundwater recharge         (soil --> aquifer)'
         write(6064,*) 'seep:         kg      mass in groundwater seepage          (aquifer --> deep)'
         write(6064,*) 'irr:          kg      mass in groundwater irrigation       (aquifer --> soil)'
         write(6064,*) 'div:          kg      mass removed (-) or added (+) via diversion'
         write(6064,*) 'sorb:         kg      mass transferred via sorption        (aquifer --> sorbed mass)'
         write(6064,*) 'rctn:         kg      mass transferred via reaction        (aquifer --> transferred)'
         write(6064,*) 'mass:         kg      mass stored in groundwater           (averaged over year)'
         write(6064,*) 'conc:         g/m3    concentration in groundwater         (averaged over year)'
         write(6064,*) 'srbd:         kg      mass sorbed in aquifer               (averaged over year)'
         write(6064,*)
         write(6064,*) cs_hdr_aqu
         if (pco%csvout == "y") then
           open (6065,file="aquifer_cs_yr.csv",recl=2000)
           write (6065,*) bsn%name, prog
           write (6065,*) 'conc = mg/L; other = kg'
           write (6065,'(*(G0.3,:","))') cs_hdr_aqu
         endif
       endif
     endif
     
     !aquifer - ave annual     
     if (sp_ob%aqu > 0) then
       if (pco%cs_aqu%a == "y" .and. cs_db%num_cs > 0) then
         open (6066,file="aquifer_cs_aa.txt",recl=2000)
         write(6066,*) bsn%name, prog
         write(6066,*) 
         write(6066,*) 'Aquifer constituent fluxes and state variables - yearly'
         write(6066,*)
         write(6066,*) 'Column header units and definitions'
         write(6066,*) 'values provided for seo4, seo3, and boron'
         write(6066,*) 
         write(6066,*) 'gw:           kg/yr   mass in groundwater discharge        (aquifer --> stream)' 
         write(6066,*) 'rchg:         kg/yr   mass in groundwater recharge         (soil --> aquifer)'
         write(6066,*) 'seep:         kg/yr   mass in groundwater seepage          (aquifer --> deep)'
         write(6066,*) 'irr:          kg/yr   mass in groundwater irrigation       (aquifer --> soil)'
         write(6066,*) 'div:          kg/yr   mass removed (-) or added (+) via diversion'
         write(6066,*) 'sorb:         kg/yr   mass transferred via sorption        (aquifer --> sorbed mass)'
         write(6066,*) 'rctn:         kg/yr   mass transferred via reaction        (aquifer --> transferred)'
         write(6066,*) 'mass:         kg      mass stored in groundwater           (averaged over years)'
         write(6066,*) 'conc:         g/m3    concentration in groundwater         (averaged over years)'
         write(6066,*) 'srbd:         kg      mass sorbed in aquifer               (averaged over years)'
         write(6066,*)
         write(6066,*) cs_hdr_aqu
         if (pco%csvout == "y") then
           open (6067,file="aquifer_cs_aa.csv",recl=2000)
           write (6067,*) bsn%name, prog
           write (6067,*) 'conc = mg/L; other = kg'
           write (6067,'(*(G0.3,:","))') cs_hdr_aqu
         endif
       endif
     endif
     
     !channel - daily ---------------------------------------------------------------------------------------------------------------------
     if (sp_ob%chandeg > 0) then
       if (pco%cs_chn%d == "y" .and. cs_db%num_cs > 0) then
         open (6030,file="channel_cs_day.txt",recl=2000)
         write(6030,*) bsn%name, prog
         write(6030,*) 
         write(6030,*) 'Channel constituent fluxes and state variables - daily'
         write(6030,*)
         write(6030,*) 'Column header units and definitions'
         write(6030,*) 'values provided for seo4, seo3, and boron'
         write(6030,*) 
         write(6030,*) 'in:          kg       mass entering channel (upstream + groundwater)' 
         write(6030,*) 'gw:          kg       mass entering from groundwater'
         write(6030,*) 'out:         kg       mass leaving downstream end of channel'
         write(6030,*) 'seep:        kg       mass leaving channel via seepage'
         write(6030,*) 'irr:         kg       mass removed from channel for irrigation'
         write(6030,*) 'div:         kg       mass removed (-) or added (+) via diversion'
         write(6030,*) 'mass:        kg       mass stored in channel'
         write(6030,*) 'conc:        g/m3     concentration in channel water'
         write(6030,*) 
         write(6030,*) chcs_hdr
         if (pco%csvout == "y") then
           open (6031,file="channel_cs_day.csv",recl=2000)
           write (6031,*) bsn%name, prog
           write (6031,'(*(G0.3,:","))') chcs_hdr
         endif
       endif
     endif
  
     !channel - monthly     
     if (sp_ob%chandeg > 0) then
       if (pco%cs_chn%m == "y" .and. cs_db%num_cs > 0) then
         open (6032,file="channel_cs_mon.txt",recl=2000)
         write(6032,*) bsn%name, prog
         write(6032,*) 
         write(6032,*) 'Channel constituent fluxes and state variables - monthly'
         write(6032,*)
         write(6032,*) 'Column header units and definitions'
         write(6032,*) 'values provided for seo4, seo3, and boron'
         write(6032,*) 
         write(6032,*) 'in:          kg       mass entering channel (upstream + groundwater)' 
         write(6032,*) 'gw:          kg       mass entering from groundwater'
         write(6032,*) 'out:         kg       mass leaving downstream end of channel'
         write(6032,*) 'seep:        kg       mass leaving channel via seepage'
         write(6032,*) 'irr:         kg       mass removed from channel for irrigation'
         write(6032,*) 'div:         kg       mass removed (-) or added (+) via diversion'
         write(6032,*) 'mass:        kg       mass stored in channel (averaged over month)'
         write(6032,*) 'conc:        g/m3     concentration in channel water (averaged over month)'
         write(6032,*) 
         write(6032,*) chcs_hdr
         if (pco%csvout == "y") then
           open (6033,file="channel_cs_mon.csv",recl=2000)
           write (6033,*) bsn%name, prog
           write (6033,'(*(G0.3,:","))') chcs_hdr
         endif
       endif
     endif
     
     !channel - yearly     
     if (sp_ob%chandeg > 0) then
       if (pco%cs_chn%y == "y" .and. cs_db%num_cs > 0) then
         open (6034,file="channel_cs_yr.txt",recl=2000)
         write(6034,*) bsn%name, prog
         write(6034,*) 
         write(6034,*) 'Channel constituent fluxes and state variables - yearly'
         write(6034,*)
         write(6034,*) 'Column header units and definitions'
         write(6034,*) 'values provided for seo4, seo3, and boron'
         write(6034,*) 
         write(6034,*) 'in:          kg       mass entering channel (upstream + groundwater)' 
         write(6034,*) 'gw:          kg       mass entering from groundwater' 
         write(6034,*) 'out:         kg       mass leaving downstream end of channel'
         write(6034,*) 'seep:        kg       mass leaving channel via seepage'
         write(6034,*) 'irr:         kg       mass removed from channel for irrigation'
         write(6034,*) 'div:         kg       mass removed (-) or added (+) via diversion'
         write(6034,*) 'mass:        kg       mass stored in channel (averaged over year)'
         write(6034,*) 'conc:        g/m3     concentration in channel water (averaged over year)'
         write(6034,*) 
         write(6034,*) chcs_hdr
         if (pco%csvout == "y") then
           open (6035,file="channel_cs_yr.csv",recl=2000)
           write (6035,*) bsn%name, prog
           write (6035,'(*(G0.3,:","))') chcs_hdr
         endif
       endif
     endif
     
     !channel - ave annual     
     if (sp_ob%chandeg > 0) then
       if (pco%cs_chn%a == "y" .and. cs_db%num_cs > 0) then
         open (6036,file="channel_cs_aa.txt",recl=2000)
         write(6036,*) bsn%name, prog
         write(6036,*) 
         write(6036,*) 'Channel constituent fluxes and state variables - average annual'
         write(6036,*)
         write(6036,*) 'Column header units and definitions'
         write(6036,*) 'values provided for seo4, seo3, and boron'
         write(6036,*) 
         write(6036,*) 'in:          kg/yr    mass eentering channel (upstream + groundwater)' 
         write(6036,*) 'gw:          kg/yr    mass entering from groundwater'
         write(6036,*) 'out:         kg/yr    mass leaving downstream end of channel'
         write(6036,*) 'seep:        kg/yr    mass leaving channel via seepage'
         write(6036,*) 'irr:         kg/yr    mass removed from channel for irrigation'
         write(6036,*) 'div:         kg/yr    mass removed (-) or added (+) via diversion'
         write(6036,*) 'mass:        kg       mass stored in channel (averaged over years)'
         write(6036,*) 'conc:        g/m3     concentration in channel water (averaged over years)'
         write(6036,*) 
         write(6036,*) chcs_hdr
         if (pco%csvout == "y") then
           open (6037,file="channel_cs_aa.csv",recl=2000)
           write (6037,*) bsn%name, prog
           write (6037,'(*(G0.3,:","))') chcs_hdr
         endif
       endif
     endif
     
     !reservoir - daily -------------------------------------------------------------------------------------------------------------------
     if (sp_ob%res > 0) then
       if (pco%cs_res%d == "y" .and. cs_db%num_cs > 0) then
         open (6040,file="reservoir_cs_day.txt",recl=2000)
         write(6040,*) bsn%name, prog
         write(6040,*) 
         write(6040,*) 'Reservoir constituent fluxes and state variables - daily'
         write(6040,*)
         write(6040,*) 'Column header units and definitions'
         write(6040,*) 'values provided for seo4, seo3, and boron'
         write(6040,*) 
         write(6040,*) 'in:          kg       mass entering reservoir with inflow' 
         write(6040,*) 'out:         kg       mass leaving reservoir with outflow'
         write(6040,*) 'seep:        kg       mass leaving reservoir via seepage'
         write(6040,*) 'setl:        kg       mass settling to bottom of reservoir'
         write(6040,*) 'rctn:        kg       mass consumed due to chemical reaction'
         write(6040,*) 'prod:        kg       mass produced due to chemical reaction'
         write(6040,*) 'fert:        kg       mass added to reservoir via fertilizer'
         write(6040,*) 'irr:         kg       mass removed from reservoir for irrigation'
         write(6040,*) 'div:         kg       mass removed (-) or added (+) via diversion'
         write(6040,*) 'mass:        kg       mass stored in reservoir'
         write(6040,*) 'conc:        g/m3     concentration in reservoir water'
         write(6040,*) 'vol_m3:      m3       volume of water stored in reservoir'
         write(6040,*) 
         write(6040,*) rescs_hdr
         if (pco%csvout == "y") then
           open (6041,file="reservoir_cs_day.csv",recl=2000)
           write (6041,*) bsn%name, prog
           write (6041,'(*(G0.3,:","))') rescs_hdr
         endif
       endif
     endif
     
     !reservoir - monthly
     if (sp_ob%res > 0) then
       if (pco%cs_res%m == "y" .and. cs_db%num_cs > 0) then
         open (6042,file="reservoir_cs_mon.txt",recl=2000)
         write(6042,*) bsn%name, prog
         write(6042,*) 
         write(6042,*) 'Reservoir constituent fluxes and state variables - monthly'
         write(6042,*)
         write(6042,*) 'Column header units and definitions'
         write(6042,*) 'values provided for seo4, seo3, and boron'
         write(6042,*) 
         write(6042,*) 'in:          kg       mass entering reservoir with inflow' 
         write(6042,*) 'out:         kg       mass leaving reservoir with outflow'
         write(6042,*) 'seep:        kg       mass leaving reservoir via seepage'
         write(6042,*) 'setl:        kg       mass settling to bottom of reservoir'
         write(6042,*) 'rctn:        kg       mass consumed due to chemical reaction'
         write(6042,*) 'prod:        kg       mass produced due to chemical reaction'
         write(6042,*) 'fert:        kg       mass added to reservoir via fertilizer'
         write(6042,*) 'irr:         kg       mass removed from reservoir for irrigation'
         write(6042,*) 'div:         kg       mass removed (-) or added (+) via diversion'
         write(6042,*) 'mass:        kg       mass stored in reservoir (averaged over month)'
         write(6042,*) 'conc:        g/m3     concentration in reservoir water (averaged over month)'
         write(6042,*) 'vol_m3:      m3       volume of water stored in reservoir (averaged over month)'
         write(6042,*) 
         write(6042,*) rescs_hdr
         if (pco%csvout == "y") then
           open (6043,file="reservoir_cs_mon.csv",recl=2000)
           write (6043,*) bsn%name, prog
           write (6043,'(*(G0.3,:","))') rescs_hdr
         endif
       endif
     endif

     !reservoir - yearly
     if (sp_ob%res > 0) then
       if (pco%cs_res%y == "y" .and. cs_db%num_cs > 0) then
         open (6044,file="reservoir_cs_yr.txt",recl=2000)
         write(6044,*) bsn%name, prog
         write(6044,*) 
         write(6044,*) 'Reservoir constituent fluxes and state variables - yearly'
         write(6044,*)
         write(6044,*) 'Column header units and definitions'
         write(6044,*) 'values provided for seo4, seo3, and boron'
         write(6044,*) 
         write(6044,*) 'in:          kg       mass entering reservoir with inflow' 
         write(6044,*) 'out:         kg       mass leaving reservoir with outflow'
         write(6044,*) 'seep:        kg       mass leaving reservoir via seepage'
         write(6044,*) 'setl:        kg       mass settling to bottom of reservoir'
         write(6044,*) 'rctn:        kg       mass consumed due to chemical reaction'
         write(6044,*) 'prod:        kg       mass produced due to chemical reaction'
         write(6044,*) 'fert:        kg       mass added to reservoir via fertilizer'
         write(6044,*) 'irr:         kg       mass removed from reservoir for irrigation'
         write(6044,*) 'div:         kg       mass removed (-) or added (+) via diversion'
         write(6044,*) 'mass:        kg       mass stored in reservoir (averaged over year)'
         write(6044,*) 'conc:        g/m3     concentration in reservoir water (averaged over year)'
         write(6044,*) 'vol_m3:      m3       volume of water stored in reservoir (averaged over year)'
         write(6044,*) 
         write(6044,*) rescs_hdr
         if (pco%csvout == "y") then
           open (6045,file="reservoir_cs_yr.csv",recl=2000)
           write (6045,*) bsn%name, prog
           write (6045,'(*(G0.3,:","))') rescs_hdr
         endif
       endif
     endif
         
     !reservoir - ave annual
     if (sp_ob%res > 0) then
       if (pco%cs_res%a == "y" .and. cs_db%num_cs > 0) then
         open (6046,file="reservoir_cs_aa.txt",recl=2000)
         write(6046,*) bsn%name, prog
         write(6046,*) 
         write(6046,*) 'Reservoir constituent fluxes and state variables - average annual'
         write(6046,*)
         write(6046,*) 'Column header units and definitions'
         write(6046,*) 'values provided for seo4, seo3, and boron'
         write(6046,*) 
         write(6046,*) 'in:          kg/yr    mass entering reservoir with inflow' 
         write(6046,*) 'out:         kg/yr    mass leaving reservoir with outflow'
         write(6046,*) 'seep:        kg/yr    mass leaving reservoir via seepage'
         write(6046,*) 'setl:        kg/yr    mass settling to bottom of reservoir'
         write(6046,*) 'rctn:        kg/yr    mass consumed due to chemical reaction'
         write(6046,*) 'prod:        kg/yr    mass produced due to chemical reaction'
         write(6046,*) 'fert:        kg/yr    mass added to reservoir via fertilizer'
         write(6046,*) 'irr:         kg/yr    mass removed from reservoir for irrigation'
         write(6046,*) 'div:         kg/yr    mass removed (-) or added (+) via diversion'
         write(6046,*) 'mass:        kg       mass stored in reservoir (averaged over years)'
         write(6046,*) 'conc:        g/m3     concentration in reservoir water (averaged over years)'
         write(6046,*) 'vol_m3:      m3       volume of water stored in reservoir (averaged over years)'
         write(6046,*) 
         write(6046,*) rescs_hdr
         if (pco%csvout == "y") then
           open (6047,file="reservoir_cs_aa.csv",recl=2000)
           write (6047,*) bsn%name, prog
           write (6047,'(*(G0.3,:","))') rescs_hdr
         endif
       endif
     endif 
      
     !routing unit - daily ----------------------------------------------------------------------------------------------------------------
     if (sp_ob%ru > 0) then
       if (pco%cs_ru%d == "y" .and. cs_db%num_cs > 0) then
         open (6070,file="rout_unit_cs_day.txt",recl=2000)
         write(6070,*) bsn%name, prog
         write(6070,*) 
         write(6070,*) 'Routing unit constituent fluxes - daily'
         write(6070,*)
         write(6070,*) 'Column header units and definitions'
         write(6070,*) 'values provided for seo4, seo3, and boron'
         write(6070,*) 
         write(6070,*) 'total:        kg      mass = perc + surq + latq + tile'
         write(6070,*) 'perc:         kg      mass in deep percolation          (soil --> aquifer)'
         write(6070,*) 'surq:         kg      mass in surface runoff            (soil --> stream)'
         write(6070,*) 'latq:         kg      mass in soil lateral flow         (soil --> stream)'
         write(6070,*) 'tile:         kg      mass in tile drainage             (soil --> stream)'
         write(6070,*) 'sedm:         kg      mass in sediment runoff           (soil --> stream)'
         write(6070,*) 'wtsp:         kg      mass in wetland seepage           (wetland --> soil)'
         write(6070,*) 'irsw:         kg      mass in surfac water irrigation   (channel --> soil)'
         write(6070,*) 'irgw:         kg      mass in groundwater irrigation    (aquifer --> soil)'
         write(6070,*) 'irwo:         kg      mass in outside source irrigation (outside --> soil)'
         write(6070,*) 'rain:         kg      mass in rainfall                  (outside --> soil)'
         write(6070,*) 'dryd:         kg      mass in dry deposition            (outside --> soil)'
         write(6070,*) 'fert:         kg      mass in applied fertilizer        (outside --> soil)'
         write(6070,*) 'uptk:         kg      mass in plant uptake              (soil --> outside)'
         write(6070,*) 'rctn:         kg      mass transferred via reaction     (soil --> outside)'
         write(6070,*) 'sorb:         kg      mass transferred via sorption     (soil --> sorbed mass)'
         write(6070,*) 
         write(6070,*) rucsb_hdr
         if (pco%csvout == "y") then
           open (6071,file="rout_unit_cs_day.csv",recl=2000)
           write (6071,*) bsn%name, prog
           write (6071,*) 'all values in kg'
           write (6071,'(*(G0.3,:","))') rucsb_hdr
         endif
       endif
     endif
     
     !routing unit - monthly
     if (sp_ob%ru > 0) then
       if (pco%cs_ru%m == "y" .and. cs_db%num_cs > 0) then
         open (6072,file="rout_unit_cs_mon.txt",recl=2000)
         write(6072,*) bsn%name, prog
         write(6072,*) 
         write(6072,*) 'Routing unit constituent fluxes - monthly'
         write(6072,*)
         write(6072,*) 'Column header units and definitions'
         write(6072,*) 'values provided for seo4, seo3, and boron'
         write(6072,*) 
         write(6072,*) 'total:        kg      mass = perc + surq + latq + tile'
         write(6072,*) 'perc:         kg      mass in deep percolation          (soil --> aquifer)'
         write(6072,*) 'surq:         kg      mass in surface runoff            (soil --> stream)'
         write(6072,*) 'latq:         kg      mass in soil lateral flow         (soil --> stream)'
         write(6072,*) 'tile:         kg      mass in tile drainage             (soil --> stream)'
         write(6072,*) 'sedm:         kg      mass in sediment runoff           (soil --> stream)'
         write(6072,*) 'wtsp:         kg      mass in wetland seepage           (wetland --> soil)'
         write(6072,*) 'irsw:         kg      mass in surfac water irrigation   (channel --> soil)'
         write(6072,*) 'irgw:         kg      mass in groundwater irrigation    (aquifer --> soil)'
         write(6072,*) 'irwo:         kg      mass in outside source irrigation (outside --> soil)'
         write(6072,*) 'rain:         kg      mass in rainfall                  (outside --> soil)'
         write(6072,*) 'dryd:         kg      mass in dry deposition            (outside --> soil)'
         write(6072,*) 'fert:         kg      mass in applied fertilizer        (outside --> soil)'
         write(6072,*) 'uptk:         kg      mass in plant uptake              (soil --> outside)'
         write(6072,*) 'rctn:         kg      mass transferred via reaction     (soil --> outside)'
         write(6072,*) 'sorb:         kg      mass transferred via sorption     (soil --> sorbed mass)'
         write(6072,*) 
         write(6072,*) rucsb_hdr
         if (pco%csvout == "y") then
           open (6073,file="rout_unit_cs_mon.csv",recl=2000)
           write (6073,*) bsn%name, prog
           write (6073,*) 'all values in kg'
           write (6073,'(*(G0.3,:","))') rucsb_hdr
         endif
       endif
     endif

     !routing unit - yearly
     if (sp_ob%ru > 0) then
       if (pco%cs_ru%y == "y" .and. cs_db%num_cs > 0) then
         open (6074,file="rout_unit_cs_yr.txt",recl=2000)
         write(6074,*) bsn%name, prog
         write(6074,*) 
         write(6074,*) 'Routing unit constituent fluxes - yearly'
         write(6074,*)
         write(6074,*) 'Column header units and definitions'
         write(6074,*) 'values provided for seo4, seo3, and boron'
         write(6074,*) 
         write(6074,*) 'total:        kg      mass = perc + surq + latq + tile'
         write(6074,*) 'perc:         kg      mass in deep percolation          (soil --> aquifer)'
         write(6074,*) 'surq:         kg      mass in surface runoff            (soil --> stream)'
         write(6074,*) 'latq:         kg      mass in soil lateral flow         (soil --> stream)'
         write(6074,*) 'tile:         kg      mass in tile drainage             (soil --> stream)'
         write(6074,*) 'sedm:         kg      mass in sediment runoff           (soil --> stream)'
         write(6074,*) 'wtsp:         kg      mass in wetland seepage           (wetland --> soil)'
         write(6074,*) 'irsw:         kg      mass in surfac water irrigation   (channel --> soil)'
         write(6074,*) 'irgw:         kg      mass in groundwater irrigation    (aquifer --> soil)'
         write(6074,*) 'irwo:         kg      mass in outside source irrigation (outside --> soil)'
         write(6074,*) 'rain:         kg      mass in rainfall                  (outside --> soil)'
         write(6074,*) 'dryd:         kg      mass in dry deposition            (outside --> soil)'
         write(6074,*) 'fert:         kg      mass in applied fertilizer        (outside --> soil)'
         write(6074,*) 'uptk:         kg      mass in plant uptake              (soil --> outside)'
         write(6074,*) 'rctn:         kg      mass transferred via reaction     (soil --> outside)'
         write(6074,*) 'sorb:         kg      mass transferred via sorption     (soil --> sorbed mass)'
         write(6074,*) 
         write(6074,*) rucsb_hdr
         if (pco%csvout == "y") then
           open (6075,file="rout_unit_cs_yr.csv",recl=2000)
           write (6075,*) bsn%name, prog
           write (6075,*) 'all values in kg'
           write (6075,'(*(G0.3,:","))') rucsb_hdr
         endif
       endif
     endif
         
     !routing unit - ave annual
     if (sp_ob%ru > 0) then
       if (pco%cs_ru%a == "y" .and. cs_db%num_cs > 0) then
         open (6076,file="rout_unit_cs_aa.txt",recl=2000)
         write(6076,*) bsn%name, prog
         write(6076,*) 
         write(6076,*) 'Routing unit constituent fluxes - average annaul'
         write(6076,*)
         write(6076,*) 'Column header units and definitions'
         write(6076,*) 'values provided for seo4, seo3, and boron'
         write(6076,*) 
         write(6076,*) 'total:        kg/yr   mass = perc + surq + latq + tile'
         write(6076,*) 'perc:         kg/yr   mass in deep percolation          (soil --> aquifer)'
         write(6076,*) 'surq:         kg/yr   mass in surface runoff            (soil --> stream)'
         write(6076,*) 'latq:         kg/yr   mass in soil lateral flow         (soil --> stream)'
         write(6076,*) 'tile:         kg/yr   mass in tile drainage             (soil --> stream)'
         write(6076,*) 'sedm:         kg/yr   mass in sediment runoff           (soil --> stream)'
         write(6076,*) 'wtsp:         kg/yr   mass in wetland seepage           (wetland --> soil)'
         write(6076,*) 'irsw:         kg/yr   mass in surfac water irrigation   (channel --> soil)'
         write(6076,*) 'irgw:         kg/yr   mass in groundwater irrigation    (aquifer --> soil)'
         write(6076,*) 'irwo:         kg/yr   mass in outside source irrigation (outside --> soil)'
         write(6076,*) 'rain:         kg/yr   mass in rainfall                  (outside --> soil)'
         write(6076,*) 'dryd:         kg/yr   mass in dry deposition            (outside --> soil)'
         write(6076,*) 'fert:         kg/yr   mass in applied fertilizer        (outside --> soil)'
         write(6076,*) 'uptk:         kg/yr   mass in plant uptake              (soil --> outside)'
         write(6076,*) 'rctn:         kg/yr   mass transferred via reaction     (soil --> outside)'
         write(6076,*) 'sorb:         kg/yr   mass transferred via sorption     (soil --> sorbed mass)'
         write(6076,*) 
         write(6076,*) rucsb_hdr
         if (pco%csvout == "y") then
           open (6077,file="rout_unit_cs_aa.csv",recl=2000)
           write (6077,*) bsn%name, prog
           write (6077,*) 'all values in kg'
           write (6077,'(*(G0.3,:","))') rucsb_hdr
         endif
       endif
     endif 
     
     !wetland - daily ---------------------------------------------------------------------------------------------------------------------
     if (sp_ob%res > 0) then
       if (pco%cs_res%d == "y" .and. cs_db%num_cs > 0) then
         open (6090,file="wetland_cs_day.txt",recl=2000)
         write(6090,*) bsn%name, prog
         write(6090,*) 
         write(6090,*) 'Wetland constituent fluxes and state variables - daily'
         write(6090,*)
         write(6090,*) 'Column header units and definitions'
         write(6090,*) 'values provided for seo4, seo3, and boron'
         write(6090,*) 
         write(6090,*) 'in:          kg       mass entering wetland with inflow' 
         write(6090,*) 'out:         kg       mass leaving wetland with outflow'
         write(6090,*) 'seep:        kg       mass leaving wetland via seepage'
         write(6090,*) 'setl:        kg       mass settling to bottom of wetland'
         write(6090,*) 'rctn:        kg       mass consumed due to chemical reaction'
         write(6090,*) 'prod:        kg       mass produced due to chemical reaction'
         write(6090,*) 'fert:        kg       mass added to wetland via fertilizer'
         write(6090,*) 'irrg:        kg       mass added via irrigation'
         write(6090,*) 'mass:        kg       mass stored in wetland at beginning of day'
         write(6090,*) 'conc:        g/m3     concentration in wetland water at beginning of day'
         write(6090,*) 'vol_m3:      m3       volume of water stored in wetland at beginning of day'
         write(6090,*) 
         write(6090,*) rescs_hdr
         if (pco%csvout == "y") then
           open (6091,file="wetland_cs_day.csv",recl=2000)
           write (6091,*) bsn%name, prog
           write (6091,'(*(G0.3,:","))') rescs_hdr
         endif
       endif
     endif
     
     !wetland - monthly
     if (sp_ob%res > 0) then
       if (pco%cs_res%m == "y" .and. cs_db%num_cs > 0) then
         open (6092,file="wetland_cs_mon.txt",recl=2000)
         write(6092,*) bsn%name, prog
         write(6092,*) 
         write(6092,*) 'Wetland constituent fluxes and state variables - monthly'
         write(6092,*)
         write(6092,*) 'Column header units and definitions'
         write(6092,*) 'values provided for seo4, seo3, and boron'
         write(6092,*) 
         write(6092,*) 'in:          kg       mass entering wetland with inflow' 
         write(6092,*) 'out:         kg       mass leaving wetland with outflow'
         write(6092,*) 'seep:        kg       mass leaving wetland via seepage'
         write(6092,*) 'setl:        kg       mass settling to bottom of wetland'
         write(6092,*) 'rctn:        kg       mass consumed due to chemical reaction'
         write(6092,*) 'prod:        kg       mass produced due to chemical reaction'
         write(6092,*) 'fert:        kg       mass added to wetland via fertilizer'
         write(6092,*) 'irrg:        kg       mass added via irrigation'
         write(6092,*) 'mass:        kg       mass stored in wetland (averaged over month)'
         write(6092,*) 'conc:        g/m3     concentration in wetland water (averaged over month)'
         write(6092,*) 'vol_m3:      m3       volume of water stored in wetland (averaged over month)'
         write(6092,*) 
         write(6092,*) rescs_hdr
         if (pco%csvout == "y") then
           open (6093,file="wetland_cs_mon.csv",recl=2000)
           write (6093,*) bsn%name, prog
           write (6093,'(*(G0.3,:","))') rescs_hdr
         endif
       endif
     endif

     !wetland - yearly
     if (sp_ob%res > 0) then
       if (pco%cs_res%y == "y" .and. cs_db%num_cs > 0) then
         open (6094,file="wetland_cs_yr.txt",recl=2000)
         write(6094,*) bsn%name, prog
         write(6094,*) 
         write(6094,*) 'Wetland constituent fluxes and state variables - yearly'
         write(6094,*)
         write(6094,*) 'Column header units and definitions'
         write(6094,*) 'values provided for seo4, seo3, and boron'
         write(6094,*) 
         write(6094,*) 'in:          kg       mass entering wetland with inflow' 
         write(6094,*) 'out:         kg       mass leaving wetland with outflow'
         write(6094,*) 'seep:        kg       mass leaving wetland via seepage'
         write(6094,*) 'setl:        kg       mass settling to bottom of wetland'
         write(6094,*) 'rctn:        kg       mass consumed due to chemical reaction'
         write(6094,*) 'prod:        kg       mass produced due to chemical reaction'
         write(6094,*) 'fert:        kg       mass added to wetland via fertilizer'
         write(6094,*) 'irrg:        kg       mass added via irrigation'
         write(6094,*) 'mass:        kg       mass stored in wetland (averaged over year)'
         write(6094,*) 'conc:        g/m3     concentration in wetland water (averaged over year)'
         write(6094,*) 'vol_m3:      m3       volume of water stored in wetland (averaged over year)'
         write(6094,*) 
         write(6094,*) rescs_hdr
         if (pco%csvout == "y") then
           open (6095,file="wetland_cs_yr.csv",recl=2000)
           write (6095,*) bsn%name, prog
           write (6095,'(*(G0.3,:","))') rescs_hdr
         endif
       endif
     endif
         
     !wetland - ave annual
     if (sp_ob%res > 0) then
       if (pco%cs_res%a == "y" .and. cs_db%num_cs > 0) then
         open (6096,file="wetland_cs_aa.txt",recl=2000)
         write(6096,*) bsn%name, prog
         write(6096,*) 
         write(6096,*) 'Wetland constituent fluxes and state variables - average annual'
         write(6096,*)
         write(6096,*) 'Column header units and definitions'
         write(6096,*) 'values provided for seo4, seo3, and boron'
         write(6096,*) 
         write(6096,*) 'in:          kg/yr    mass entering wetland with inflow' 
         write(6096,*) 'out:         kg/yr    mass leaving wetland with outflow'
         write(6096,*) 'seep:        kg/yr    mass leaving wetland via seepage'
         write(6096,*) 'setl:        kg/yr    mass settling to bottom of wetland'
         write(6096,*) 'rctn:        kg/yr    mass consumed due to chemical reaction'
         write(6096,*) 'prod:        kg/yr    mass produced due to chemical reaction'
         write(6096,*) 'fert:        kg/yr    mass added to wetland via fertilizer'
         write(6096,*) 'irrg:        kg/yr    mass added via irrigation'
         write(6096,*) 'mass:        kg       mass stored in wetland (averaged over years)'
         write(6096,*) 'conc:        g/m3     concentration in wetland water (averaged over years)'
         write(6096,*) 'vol_m3:      m3       volume of water stored in wetland (averaged over years)'
         write(6096,*) 
         write(6096,*) rescs_hdr
         if (pco%csvout == "y") then
           open (6097,file="wetland_cs_aa.csv",recl=2000)
           write (6097,*) bsn%name, prog
           write (6097,'(*(G0.3,:","))') rescs_hdr
         endif
       endif
     endif 
     
     
      return
      end subroutine header_const