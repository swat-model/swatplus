      module carbon_legacy_module

!!    will be removed in revision 63.
!!
!!    legacy carbon output support for the CSU workflows.
!!
!!    the refactor on carbonDev standardised the carbon output into per-family
!!    print.prt flags (hru_cb_*). the CSU team still relies on the older,
!!    non-standard files, so this module keeps that path alive without touching
!!    the new one. it holds the old fixed-column header types and the open
!!    routine for those files. the writers live in soil_nutcarb_write_legacy and
!!    soil_carbvar_write_legacy. everything here is gated by the hru_cb and
!!    hru_cb_vars rows in print.prt, which swat+ editor does not emit, so a normal
!!    run produces none of these files. file units are the old ones shifted by
!!    +3800 into the free 8300-8399 band to avoid clashing with the new families.

      implicit none

     type output_plc_header     
         character (len=7) ::  freq          =    "freq   "
         character (len=11) :: day           =    "       jday"
         character (len=11) :: day_mo        =    "        day"
         character (len=11) :: mo            =    "        mon"
         character (len=11) :: yrc           =    "         yr"
         character (len=16) :: isd           =    "            unit"
         character (len=21) :: id            =    "              gis_id "
         character (len=16) :: name          =    "    name        "
         character(len=15)  :: tot_c     =    "        total_c"
         character(len=15)  :: ab_gr_c   =    "        ab_gr_c"
         character(len=15)  :: leaf_c    =    "         leaf_c"
         character(len=15)  :: stem_c    =    "         stem_c"
         character(len=15)  :: seed_c    =    "         seed_c"
         character(len=15)  :: root_c    =    "         root_c"
         character(len=15)  :: rsd_c     =    "     surf_rsd_c"
         end type output_plc_header       
      type (output_plc_header) :: plc_hdr
      
      type output_plc_header_units      
         character (len=7) ::  freq         =    "       "
         character (len=11) :: day          =    "           "
         character (len=11) :: day_mo       =    "           "
         character (len=11) :: mo           =    "           "
         character (len=11) :: yrc          =    "           "
         character (len=16)  :: isd         =  "           "
         character (len=21) :: id           =  "                "
         character (len=16) :: name         =  "           "
         character(len=15)  :: tot_c     =    "          kg/ha"
         character(len=15)  :: ab_gr_c   =    "          kg/ha"
         character(len=15)  :: leaf_c    =    "          kg/ha"
         character(len=15)  :: stem_c    =    "          kg/ha"
         character(len=15)  :: seed_c    =    "          kg/ha"
         character(len=15)  :: root_c    =    "          kg/ha"
         character(len=15)  :: rsd_c     =    "          kg/ha"
        end type output_plc_header_units         
      type (output_plc_header_units) :: plc_hdr_units

!!! NEW PLANT CARBON STAT OUTPUT
      
!! NEW RESIDUE CARBON STAT OUTPUT
      
     type output_soil_org_flux_header     
         character (len=7) ::  freq          =    "freq   "
         character (len=12) :: soil_lyr      =    "    soil_lyr"
         character (len=12) :: soil_depth    =    "  soil_depth"
         character (len=12) :: day           =    "        jday"
         character (len=12) :: mo            =    "         mon"
         character (len=12) :: day_mo        =    "         day"
         character (len=12) :: yrc           =    "          yr"
         character (len=12) :: isd           =    "        unit"
         character (len=23) :: id            =    "                gis_id "
         character (len=15) :: name          =    "name           "         
         character(len=15)  :: cfmets1   =    "        cfmets1"
         character(len=15)  :: cfstrs1   =    "        cfstrs1"
         character(len=15)  :: cfstrs2   =    "        cfstrs2"
         character(len=15)  :: efmets1   =    "        efmets1"
         character(len=15)  :: efstrs1   =    "        efstrs1"
         character(len=15)  :: efstrs2   =    "        efstrs2"
         character(len=15)  :: immmets1  =    "       immmets1"
         character(len=15)  :: immstrs1  =    "       immstrs1" 
         character(len=15)  :: immstrs2  =    "       immstrs2" 
         character(len=15)  :: mnrmets1  =    "       mnrmets1" 
         character(len=15)  :: mnrstrs1  =    "       mnrstrs1" 
         character(len=15)  :: mnrstrs2  =    "       mnrstrs2"  
         character(len=15)  :: co2fmet   =    "        co2fmet" 
         character(len=15)  :: co2fstr   =    "        co2fstr" 
         character(len=15)  :: cfs1s2    =    "         cfs1s2"  
         character(len=15)  :: cfs1s3    =    "         cfs1s3"  
         character(len=15)  :: cfs2s1    =    "         cfs2s1"  
         character(len=15)  :: cfs2s3    =    "         cfs2s3"  
         character(len=15)  :: cfs3s1    =    "         cfs3s1"  
         character(len=15)  :: efs1s2    =    "         efs1s2"  
         character(len=15)  :: efs1s3    =    "         efs1s3"  
         character(len=15)  :: efs2s1    =    "         efs2s1"  
         character(len=15)  :: efs2s3    =    "         efs2s3"  
         character(len=15)  :: efs3s1    =    "         efs3s1"  
         character(len=15)  :: imms1s2   =    "        imms1s2" 
         character(len=15)  :: imms1s3   =    "        imms1s3" 
         character(len=15)  :: imms2s1   =    "        imms2s1" 
         character(len=15)  :: imms2s3   =    "        imms2s3" 
         character(len=15)  :: imms3s1   =    "        imms3s1" 
         character(len=15)  :: mnrs1s2   =    "        mnrs1s2" 
         character(len=15)  :: mnrs1s3   =    "        mnrs1s3" 
         character(len=15)  :: mnrs2s1   =    "        mnrs2s1" 
         character(len=15)  :: mnrs2s3   =    "        mnrs2s3" 
         character(len=15)  :: mnrs3s1   =    "        mnrs3s1" 
         character(len=15)  :: co2fs1    =    "         co2fs1" 
         character(len=15)  :: co2fs2    =    "         co2fs2"  
         character(len=15)  :: co2fs3    =    "         co2fs3" 
      end type output_soil_org_flux_header
      type (output_soil_org_flux_header) :: soil_org_flux_hdr

     type output_soil_org_flux_header_units     
         character (len=7)  :: freq         = "       "
         character (len=12) :: soil_lyr     = "            "
         character (len=12) :: soil_depth   = "          mm"
         character (len=12) :: day          = "            "
         character (len=12) :: mo           = "            "
         character (len=12) :: day_mo       = "            "
         character (len=12) :: yrc          = "            "
         character (len=12) :: isd          = "            "
         character (len=23) :: id           = "                       "
         character (len=15) :: name         = "                "         
         character(len=15)  :: cfmets1   =    "        kg_C/ha"
         character(len=15)  :: cfstrs1   =    "        kg_C/ha"
         character(len=15)  :: cfstrs2   =    "        kg_C/ha"
         character(len=15)  :: efmets1   =    "        kg_N/ha"
         character(len=15)  :: efstrs1   =    "        kg_N/ha"
         character(len=15)  :: efstrs2   =    "        kg_N/ha"
         character(len=15)  :: immmets1  =    "        kg_N/ha"
         character(len=15)  :: immstrs1  =    "        kg_N/ha" 
         character(len=15)  :: immstrs2  =    "        kg_N/ha" 
         character(len=15)  :: mnrmets1  =    "        kg_N/ha" 
         character(len=15)  :: mnrstrs1  =    "        kg_N/ha" 
         character(len=15)  :: mnrstrs2  =    "        kg_N/ha"  
         character(len=15)  :: co2fmet   =    "        kg_C/ha" 
         character(len=15)  :: co2fstr   =    "        kg_C/ha" 
         character(len=15)  :: cfs1s2    =    "        kg_C/ha"  
         character(len=15)  :: cfs1s3    =    "        kg_C/ha"  
         character(len=15)  :: cfs2s1    =    "        kg_C/ha"  
         character(len=15)  :: cfs2s3    =    "        kg_C/ha"  
         character(len=15)  :: cfs3s1    =    "        kg_C/ha"  
         character(len=15)  :: efs1s2    =    "        kg_N/ha"  
         character(len=15)  :: efs1s3    =    "        kg_N/ha"  
         character(len=15)  :: efs2s1    =    "        kg_N/ha"  
         character(len=15)  :: efs2s3    =    "        kg_N/ha"  
         character(len=15)  :: efs3s1    =    "        kg_N/ha"  
         character(len=15)  :: imms1s2   =    "        kg_N/ha" 
         character(len=15)  :: imms1s3   =    "        kg_N/ha" 
         character(len=15)  :: imms2s1   =    "        kg_N/ha" 
         character(len=15)  :: imms2s3   =    "        kg_N/ha" 
         character(len=15)  :: imms3s1   =    "        kg_N/ha" 
         character(len=15)  :: mnrs1s2   =    "        kg_N/ha" 
         character(len=15)  :: mnrs1s3   =    "        kg_N/ha" 
         character(len=15)  :: mnrs2s1   =    "        kg_N/ha" 
         character(len=15)  :: mnrs2s3   =    "        kg_N/ha" 
         character(len=15)  :: co2fs2    =    "        kg_C/ha"  
         character(len=15)  :: co2fs3    =    "        kg_C/ha" 
         end type output_soil_org_flux_header_units
      type (output_soil_org_flux_header_units) :: soil_org_flux_hdr_units
      
     type output_cpool_header     
         character (len=7) ::  freq          =    "freq   "
         character (len=12) :: soil_lyr      =    "    soil_lyr"
         character (len=12) :: soil_depth    =    "  soil_depth"
         character (len=12) :: day           =    "        jday"
         character (len=12) :: mo            =    "         mon"
         character (len=12) :: day_mo        =    "         day"
         character (len=12) :: yrc           =    "          yr"
         character (len=12) :: isd           =    "        unit"
         character (len=22) :: id            =    "                gis_id"
         character (len=13) :: name          =    "    name     "
         character(len=15)  :: residue_c     =    "      residue_c"
         character(len=15)  :: str_c         =    "   structural_c"  
         character(len=15)  :: meta_c        =    "    metabolic_c"         
         character(len=15)  :: hs_c          =    "           hs_c"         
         character(len=15)  :: hp_c          =    "           hp_c"         
         character(len=15)  :: microb_c      =    "   microbrial_c"         
         character(len=15)  :: lig_c         =    "       lignin_c"      
         character(len=15)  :: nonlig_c      =    "    nonlignin_c"      
         character(len=15)  :: water_c       =    "        water_c"
         character(len=15)  :: manure_c      =    "       manure_c"  
         character(len=15)  :: root_mass     =    "      root_mass"  
         character(len=15)  :: soil_water    =    "     soil_water"  
         end type output_cpool_header       
      type (output_cpool_header) :: cpool_hdr
      
      type output_cpool_header_units      
         character (len=7) ::  freq         =  "       "
         character (len=12) :: soil_lyr     =  "            "
         character (len=12) :: soil_depth   =  "          mm"
         character (len=12) :: day          =  "            "
         character (len=12) :: mo           =  "            "
         character (len=12) :: day_mo       =  "            "
         character (len=12) :: yrc          =  "            "
         character (len=12)  :: isd         =  "            "
         character (len=22) :: id           =  "                      "
         character (len=13) :: name         =  "             "
         character(len=15)  :: residue_c    =  "          kg/ha"
         character(len=15)  :: str_c        =  "          kg/ha"  
         character(len=15)  :: meta_c       =  "          kg/ha"         
         character(len=15)  :: hs_c         =  "          kg/ha"         
         character(len=15)  :: hp_c         =  "          kg/ha"         
         character(len=15)  :: microb_c     =  "          kg/ha"         
         character(len=15)  :: lig_c        =  "          kg/ha"      
         character(len=15)  :: nonlig_c     =  "          kg/ha"      
         character(len=15)  :: water_c      =  "          kg/ha"
         character(len=15)  :: manure_c     =  "          kg/ha"  
         character(len=15)  :: root_mass    =  "          kg/ha"  
         character(len=15)  :: soil_water   =  "          mm/mm"  
        end type output_cpool_header_units         
      type (output_cpool_header_units) :: cpool_units

     type output_n_p_pool_header     
         character (len=7) ::  freq          =    "freq   "
         character (len=12) :: soil_lyr      =    "    soil_lyr"
         character (len=12) :: soil_depth    =    "  soil_depth"
         character (len=12) :: day           =    "        jday"
         character (len=12) :: mo            =    "         mon"
         character (len=12) :: day_mo        =    "         day"
         character (len=12) :: yrc           =    "          yr"
         character (len=12) :: isd           =    "        unit"
         character (len=22) :: id            =    "                gis_id"
         character (len=13) :: name          =    "    name     "
         character(len=15)  :: total_pool_n  =    "     tot_pool_n"
         character(len=15)  :: residue_n     =    "      residue_n"
         character(len=15)  :: str_n         =    "   structural_n"  
         character(len=15)  :: meta_n        =    "    metabolic_n"         
         character(len=15)  :: hs_n          =    "           hs_n"         
         character(len=15)  :: hp_n          =    "           hp_n"         
         character(len=15)  :: microb_n      =    "    microbial_n"         
         character(len=15)  :: lig_n         =    "       lignin_n"      
         character(len=15)  :: nonlig_n      =    "    nonlignin_n"      
         character(len=15)  :: water_n       =    "        water_n"
         character(len=15)  :: manure_n      =    "       manure_n"  
         character(len=15)  :: total_pool_p  =    "     tot_pool_p"
         character(len=15)  :: residue_p     =    "      residue_p"
         character(len=15)  :: str_p         =    "   structural_p"  
         character(len=15)  :: meta_p        =    "    metabolic_p"         
         character(len=15)  :: hs_p          =    "           hs_p"         
         character(len=15)  :: hp_p          =    "           hp_p"         
         character(len=15)  :: microb_p      =    "    microbial_p"         
         character(len=15)  :: lig_p         =    "       lignin_p"      
         character(len=15)  :: nonlig_p      =    "    nonlignin_p"      
         character(len=15)  :: water_p       =    "        water_p"
         character(len=15)  :: manure_p      =    "       manure_p"  
         end type output_n_p_pool_header       
      type (output_n_p_pool_header) :: n_p_pool_hdr
      
      type output_n_p_pool_header_units      
         character (len=7) ::  freq         =  "       "
         character (len=12) :: soil_lyr     =  "            "
         character (len=12) :: soil_depth   =  "          mm"
         character (len=12) :: day          =  "            "
         character (len=12) :: mo           =  "            "
         character (len=12) :: day_mo       =  "            "
         character (len=12) :: yrc          =  "            "
         character (len=12)  :: isd         =  "            "
         character (len=22) :: id           =  "                      "
         character (len=13) :: name         =  "             "
         character(len=15)  :: total_pool_n =  "          kg/ha"
         character(len=15)  :: residue_n    =  "          kg/ha"
         character(len=15)  :: str_n        =  "          kg/ha"  
         character(len=15)  :: meta_n       =  "          kg/ha"         
         character(len=15)  :: hs_n         =  "          kg/ha"         
         character(len=15)  :: hp_n         =  "          kg/ha"         
         character(len=15)  :: microb_n     =  "          kg/ha"         
         character(len=15)  :: lig_n        =  "          kg/ha"      
         character(len=15)  :: nonlig_n     =  "          kg/ha"      
         character(len=15)  :: water_n      =  "          kg/ha"
         character(len=15)  :: manure_n     =  "          kg/ha"  
         character(len=15)  :: total_pool_p =  "          kg/ha"
         character(len=15)  :: residue_p    =  "          kg/ha"
         character(len=15)  :: str_p        =  "          kg/ha"  
         character(len=15)  :: meta_p       =  "          kg/ha"         
         character(len=15)  :: hs_p         =  "          kg/ha"         
         character(len=15)  :: hp_p         =  "          kg/ha"         
         character(len=15)  :: microb_p     =  "          kg/ha"         
         character(len=15)  :: lig_p        =  "          kg/ha"      
         character(len=15)  :: nonlig_p     =  "          kg/ha"      
         character(len=15)  :: water_p      =  "          kg/ha"
         character(len=15)  :: manure_p     =  "          kg/ha"  
        end type output_n_p_pool_header_units         
      type (output_n_p_pool_header_units) :: n_p_pool_units

     type output_carb_vars_header     
         character (len=7) ::  freq          =    "freq   "
         character (len=12) :: soil_lyr      =    "    soil_lyr"
         character (len=12) :: soil_depth    =    "  soil_depth"
         character (len=12) :: day           =    "        jday"
         character (len=12) :: mo            =    "         mon"
         character (len=12) :: day_mo        =    "         day"
         character (len=12) :: yrc           =    "          yr"
         character (len=12) :: isd           =    "        unit"
         character (len=22) :: id            =    "                gis_id"
         character (len=13) :: name          =    "    name     "
         character(len=15)  :: sut           =    "            sut"
         character(len=16)  :: tillagef      =    "        tillagef" 
         character(len=16)  :: bmix          =    "       cons_bmix" 
         character(len=16)  :: tillagef_biomix     =    " tillagef_biomix" 
         character(len=17)  :: tillagef_tillmix    =    " tillagef_tillmix" 
         character(len=15)  :: till_eff      =    "       till_eff"  
         character(len=15)  :: cdg           =    "            cdg"         
         character(len=15)  :: ox            =    "             ox"         
         character(len=15)  :: cs            =    "             cs"         
         character(len=15)  :: no3           =    "            no3"         
         character(len=15)  :: nh4           =    "            nh4"         
         character(len=15)  :: resp          =    "       co2_resp"         
         character(len=15)  :: soil_tmp      =    "      soil_temp"         
         character(len=15)  :: emix          =    "           emix"         
         end type output_carb_vars_header
      type (output_carb_vars_header) :: carbvars_hdr
      
     type output_org_allo_header     
         character (len=7) ::  freq          =    "freq   "
         character (len=12) :: soil_lyr      =    "    soil_lyr"
         character (len=12) :: soil_depth    =    "  soil_depth"
         character (len=12) :: day           =    "        jday"
         character (len=12) :: mo            =    "         mon"
         character (len=12) :: day_mo        =    "         day"
         character (len=12) :: yrc           =    "          yr"
         character (len=12) :: isd           =    "        unit"
         character (len=22) :: id            =    "                gis_id"
         character (len=13) :: name          =    "    name     "
         character(len=15)  :: asp           =    "            asp"         
         character(len=15)  :: abpt          =    "            abp"  
         character(len=15)  :: abco2         =    "          abco2"
         character(len=15)  :: a1co2         =    "          a1co2"         
         character(len=15)  :: asco2         =    "          asco2"         
         character(len=15)  :: apco2         =    "          apco2"         
      end type output_org_allo_header     
      type (output_org_allo_header) :: org_allow_hdr
      
     type output_org_ratio_header     
         character (len=7) ::  freq          =    "freq   "
         character (len=12) :: soil_lyr      =    "    soil_lyr"
         character (len=12) :: soil_depth    =    "  soil_depth"
         character (len=12) :: day           =    "        jday"
         character (len=12) :: mo            =    "         mon"
         character (len=12) :: day_mo        =    "         day"
         character (len=12) :: yrc           =    "          yr"
         character (len=12) :: isd           =    "        unit"
         character (len=22) :: id            =    "                gis_id"
         character (len=13) :: name          =    "    name     "
         character(len=15)  :: ncbm          =    "           ncbm"
         character(len=15)  :: nchp          =    "           nchp"  
         character(len=15)  :: nchs          =    "           nchs"  
      end type output_org_ratio_header
      type (output_org_ratio_header) :: org_ratio_hdr
      
     type output_org_trans_header     
         character (len=7) ::  freq          =    "freq   "
         character (len=12) :: soil_lyr      =    "    soil_lyr"
         character (len=12) :: soil_depth    =    "  soil_depth"
         character (len=12) :: day           =    "        jday"
         character (len=12) :: mo            =    "         mon"
         character (len=12) :: day_mo        =    "         day"
         character (len=12) :: yrc           =    "          yr"
         character (len=12) :: isd           =    "        unit"
         character (len=22) :: id            =    "                gis_id"
         character (len=13) :: name          =    "    name     "
         character(len=15)  :: bmctp         =    "          bmctp"
         character(len=15)  :: bmntp         =    "          bmntp"  
         character(len=15)  :: hsctp         =    "          hsctp"         
         character(len=15)  :: hsntp         =    "          hsntp"         
         character(len=15)  :: hpctp         =    "          hpctp"         
         character(len=15)  :: hpntp         =    "          hpntp"         
         character(len=15)  :: lmctp         =    "          lmctp"      
         character(len=15)  :: lmntp         =    "          lmntp"
         character(len=15)  :: lsctp         =    "          lsctp"  
         character(len=15)  :: lslctp        =    "         lslctp"  
         character(len=15)  :: lslnctp       =    "        lslnctp"  
         character(len=15)  :: lsntp         =    "          lsntp"  
      end type output_org_trans_header
      type (output_org_trans_header) :: org_trans_hdr
      
      type output_org_trans_header_units      
         character (len=7) ::  freq         =  "       "
         character (len=12) :: soil_lyr     =  "            "
         character (len=12) :: soil_depth   =  "          mm"
         character (len=12) :: day          =  "            "
         character (len=12) :: mo           =  "            "
         character (len=12) :: day_mo       =  "            "
         character (len=12) :: yrc          =  "            "
         character (len=12)  :: isd         =  "            "
         character (len=22) :: id           =  "                      "
         character (len=13) :: name         =  "             "
         character(len=15)  :: bmctp        =  "          kg/ha"
         character(len=15)  :: bmntp        =  "          kg/ha"  
         character(len=15)  :: hsctp        =  "          kg/ha"         
         character(len=15)  :: hsntp        =  "          kg/ha"         
         character(len=15)  :: hpctp        =  "          kg/ha"         
         character(len=15)  :: hpntp        =  "          kg/ha"         
         character(len=15)  :: lmctp        =  "          kg/ha"      
         character(len=15)  :: lmntp        =  "          kg/ha"
         character(len=15)  :: lsctp        =  "          kg/ha"  
         character(len=15)  :: lslctp       =  "          kg/ha"  
         character(len=15)  :: lslnctp      =  "          kg/ha"  
         character(len=15)  :: lsntp        =  "          kg/ha"  
        end type output_org_trans_header_units
      type (output_org_trans_header_units) :: org_trans_units

     type output_endsim_soil_prop_header     
         character (len=7)  :: freq          =    "freq   "
         character (len=20) :: soil_name     =    "soil_name           "
         character (len=8)  :: soil_lyr      =    "soil_lyr"
         character (len=12) :: soil_depth    =    "  soil_depth"
         character (len=12) :: day           =    "        jday"
         character (len=12) :: mo            =    "         mon"
         character (len=12) :: day_mo        =    "         day"
         character (len=12) :: yrc           =    "          yr"
         character (len=12) :: isd           =    "        unit"
         character (len=22) :: id            =    "                gis_id"
         character (len=13) :: name          =    "    name     "  
         character(len=15)  :: bd            =    "             bd"
         character(len=15)  :: awc           =    "            awc"
         character(len=15)  :: soil_k        =    "         soil_k"  
         character(len=15)  :: carbon        =    "         carbon"         
         character(len=15)  :: clay          =    "           clay"         
         character(len=15)  :: silt          =    "           silt"         
         character(len=15)  :: sand          =    "           sand"         
         character(len=15)  :: rock          =    "           rock"         
         character(len=15)  :: alb           =    "            alb"         
         character(len=15)  :: usle_k        =    "         usle_k"         
         character(len=15)  :: ec            =    "             ec"      
         character(len=15)  :: caco3         =    "          caco3"
         character(len=15)  :: ph            =    "             ph"  
         end type output_endsim_soil_prop_header
      type (output_endsim_soil_prop_header) ::endsim_soil_prop_hdr
      


!!! NEW SOIL CARBON STAT OUTPUT
      
!!! NEW BASIN CARBON OUTPUT
      
     type output_bsn_carb_header     
         character (len=11) :: day           =    "       jday"
         character (len=11) :: yrc           =    "         yr"
         character (len=6)  :: blnk          =    "      "
         character (len=15)  :: org_soilc    =    "      org_soilc"
         character (len=15)  :: org_plc      =    "        org_plc"
         character (len=15)  :: org_resc     =    "       org_resc"
         end type output_bsn_carb_header       
      type (output_bsn_carb_header) :: bsn_carb_hdr
      
      type output_bsn_carb_header_units      
         character (len=11) :: day          =    "           "
         character (len=11) :: yrc          =    "           "
         character (len=6)  :: blnk         =    "      "
         character(len=15)  :: org_soilc    =    "          kg/ha"
         character(len=15)  :: org_plc      =    "          kg/ha"
         character(len=15)  :: org_resc     =    "          kg/ha"
        end type output_bsn_carb_header_units         
      type (output_bsn_carb_header_units) :: bsn_carb_hdr_units

      contains

      subroutine carbon_legacy_open

      use basin_module
      use carbon_module
      use output_path_module
      use hydrograph_module

      implicit none

      external :: soil_nutcarb_write_legacy

      !! cbn_diagnostics drives the extra legacy plc/cflux/cpool and soil-prop
      !! files. it used to be read from carb_coefs.cbn (now removed). it now maps
      !! to the hru_cb flag letter: "l" (layer) turns diagnostics on; "y" gives the
      !! light files only (hru_cbn_lyr, hru_seq_lyr, hru_n_p_pool_stat).
      cbn_diagnostics = (pco%cb_hru%d == "l" .or. pco%cb_hru%m == "l" .or. &
                         pco%cb_hru%y == "l" .or. pco%cb_hru%a == "l")

        !! write carbon in soil by layer
        if (pco%cb_hru%d /= "n" .or. pco%cb_hru%m /= "n"  .or. pco%cb_hru%y /= "n") then
          call open_output_file(8348, "hru_cbn_lyr.txt", 1500)
          write (8348,*)  bsn%name, prog, "total soil carbon (Mg/ha) by layer depth in mm"
          write (9000,*) "HRU                       hru_cbn_lyr.txt"
          if (pco%csvout == "y") then
            call open_output_file(8349, "hru_cbn_lyr.csv", 1500)
            write (8349,*)  bsn%name, prog, "total soil carbon (Mg/ha) by layer depth in mm"
            write (9000,*) "HRU                       hru_cbn_lyr.csv"
          end if
    
          call open_output_file(8358, "hru_seq_lyr.txt", 1500)
          write (8358,*)  bsn%name, prog, "total sequestered soil carbon (Mg/ha) by layer depth in mm"
          write (9000,*) "HRU                       hru_seq_lyr.txt"
          if (pco%csvout == "y") then
            call open_output_file(8359, "hru_seq_lyr.csv", 1500)
            write (8359,*)  bsn%name, prog, "total sequestered soil carbon (Mg/ha) by layer depth in mm" 
            write (9000,*) "HRU                       hru_seq_lyr.csv"
          end if

          call open_output_file(8382, "hru_n_p_pool_stat.txt", 1500)
          write (8382,*)  bsn%name, prog
          write (8382,*) n_p_pool_hdr
          write (8382,*) n_p_pool_units
          write (9000,*) "HRU                       hru_n_p_pool_stat.txt"
          if (pco%csvout == "y") then
            call open_output_file(8383, "hru_n_p_pool_stat.csv", 1500)
            write (8383,*)  bsn%name, prog
            write (8383,'(*(G0.6,:,","))') n_p_pool_hdr
            write (8383,'(*(G0.6,:,","))') n_p_pool_units
            write (9000,*) "HRU                       hru_n_p_pool_stat.csv"
          end if

          if (cbn_diagnostics .eqv. .true.) then
            !! write beginning of simulation soil properties headers to hru_begsim_soil_prop
            call open_output_file(8386, "hru_begsim_soil_prop.txt", 1500)
            write (8386,*)  bsn%name, prog
            write (8386,*)  endsim_soil_prop_hdr  ! begsim can use the same header as endsim 
            write (9000,*) "HRU                       hru_begsim_soil_prop.txt"
            if (pco%csvout == "y") then
              call open_output_file(8387, "hru_begsim_soil_prop.csv", 1500)
              write (8387,*)  bsn%name, prog
              write (8387,'(*(G0.6,:,","))') endsim_soil_prop_hdr ! begsim can use the same header as endsim 
              write (9000,*) "HRU                       hru_begsim_soil_prop.csv"
            endif

            !! write end of simulation soil properties headers to hru_endsim_soil_prop
            call open_output_file(8384, "hru_endsim_soil_prop.txt", 1500)
            write (8384,*)  bsn%name, prog
            write (8384,*)  endsim_soil_prop_hdr
            write (9000,*) "HRU                       hru_endsim_soil_prop.txt"
            if (pco%csvout == "y") then
              call open_output_file(8385, "hru_endsim_soil_prop.csv", 1500)
              write (8385,*)  bsn%name, prog
              write (8385,'(*(G0.6,:,","))') endsim_soil_prop_hdr
              write (9000,*) "HRU                       hru_endsim_soil_prop.csv"
            end if


            if (bsn_cc%cswat == 2 ) then
              ! Write out begining adjusted soil properties if any value of cb_hru is not "n"
              call soil_nutcarb_write_legacy(" b")    ! Outputs beginning soil values to hru_begsim_soil_prop.txt/csv

              if (pco%cb_hru%d /= "n" .or. pco%cb_hru%m /= "n" .or. pco%cb_hru%y /= "n" .or. pco%cb_hru%a /= "n") then
                !! write carbon in soil, plant, and residue
                call open_output_file(8360, "hru_plc_stat.txt", 1500)
                write (8360,*)  bsn%name, prog
                write (8360,*) plc_hdr
                write (8360,*) plc_hdr_units
                write (9000,*) "HRU                       hru_plc_stat.txt"
                if (pco%csvout == "y") then
                  call open_output_file(8363, "hru_plc_stat.csv", 1500)
                  write (8363,*)  bsn%name, prog
                  write (8363,'(*(G0.6,:,","))') plc_hdr
                  write (8363,'(*(G0.6,:,","))') plc_hdr_units
                  write (9000,*) "HRU                       hru_plc_stat.csv"
                end if
      
                call open_output_file(8367, "hru_cflux_stat.txt", 1500)
                write (8367,*)  bsn%name, prog
                write (8367,*) soil_org_flux_hdr
                write (8367,*) soil_org_flux_hdr_units
                write (9000,*) "HRU                       hru_cflux_stat.txt"
                if (pco%csvout == "y") then
                  call open_output_file(8368, "hru_cflux_stat.csv", 1500)
                  write (8368,*)  bsn%name, prog
                  write (8368,'(*(G0.6,:,","))') soil_org_flux_hdr
                  write (8368,'(*(G0.6,:,","))') soil_org_flux_hdr_units
                  write (9000,*) "HRU                       hru_cflux_stat.csv"
                end if
              
                call open_output_file(8372, "hru_cpool_stat.txt", 1500)
                write (8372,*)  bsn%name, prog
                write (8372,*) cpool_hdr
                write (8372,*) cpool_units
                write (9000,*) "HRU                       hru_cpool_stat.txt"
                if (pco%csvout == "y") then
                  call open_output_file(8373, "hru_cpool_stat.csv", 1500)
                  write (8373,*)  bsn%name, prog
                  write (8373,'(*(G0.6,:,","))') cpool_hdr
                  write (8373,'(*(G0.6,:,","))') cpool_units
                  write (9000,*) "HRU                       hru_cpool_stat.csv"
                end if

              endif
            endif
          endif
        endif

        !! write carbon variables headers to hru_carbvars
        if (bsn_cc%cswat == 2 ) then
          if (pco%cb_vars_hru%d /= "n" .or. pco%cb_vars_hru%m /= "n"  .or. pco%cb_vars_hru%y /= "n" ) then
            call open_output_file(8374, "hru_carbvars.txt", 1500)
            write (8374,*)  bsn%name, prog
            write (8374,*) carbvars_hdr
            write (9000,*) "HRU                       hru_carbvars.txt"
            if (pco%csvout == "y") then
              call open_output_file(8375, "hru_carbvars.csv", 1500)
              write (8375,*)  bsn%name, prog
              write (8375,'(*(G0.6,:,","))') carbvars_hdr
              write (9000,*) "HRU                       hru_carbvars.csv"
            end if

            !! write org_allo variable headers to hru_org_allo_vars
            call open_output_file(8376, "hru_org_allo_vars.txt", 1500)
            write (8376,*)  bsn%name, prog
            write (8376,*)  org_allow_hdr
            write (9000,*) "HRU                       hru_org_allo_vars.txt"
            if (pco%csvout == "y") then
              call open_output_file(8377, "hru_org_allo_vars.csv", 1500)
              write (8377,*)  bsn%name, prog
              write (8377,'(*(G0.6,:,","))') org_allow_hdr
              write (9000,*) "HRU                       hru_org_allo_vars.csv"
            end if

            !! write org_ratio variable headers to hru_org_ratio_vars
            call open_output_file(8378, "hru_org_ratio_vars.txt", 1500)
            write (8378,*)  bsn%name, prog
            write (8378,*)  org_ratio_hdr
            write (9000,*) "HRU                       hru_org_ratio_vars.txt"
            if (pco%csvout == "y") then
              call open_output_file(8379, "hru_org_ratio_vars.csv", 1500)
              write (8379,*)  bsn%name, prog
              write (8379,'(*(G0.6,:,","))') org_ratio_hdr
              write (9000,*) "HRU                       hru_org_ratio_vars.csv"
            end if

           !! write org_trans variable headers to hru_org_trans_vars
            call open_output_file(8380, "hru_org_trans_vars.txt", 1500)
            write (8380,*)  bsn%name, prog
            write (8380,*) org_trans_hdr
            write (8380,*) org_trans_units
            write (9000,*) "HRU                       hru_org_trans_vars.txt"
            if (pco%csvout == "y") then
              call open_output_file(8381, "hru_org_trans_vars.csv", 1500)
              write (8381,*)  bsn%name, prog
              write (8381,'(*(G0.6,:,","))') org_trans_hdr
              write (8381,'(*(G0.6,:,","))') org_trans_units
              write (9000,*) "HRU                       hru_org_trans_vars.csv"
            end if
          endif
        endif


        !! basin carbon summary - only when the legacy soil carbon path is on
        if (pco%cb_hru%d /= "n" .or. pco%cb_hru%m /= "n" .or. &
            pco%cb_hru%y /= "n" .or. pco%cb_hru%a /= "n") then
          call open_output_file(8366, "basin_carbon_all.txt", 1500)
          if (pco%nb_hru%a == "y") then
            write (8366,*)  bsn%name, prog
            write (8366,*) bsn_carb_hdr
            write (8366,*) bsn_carb_hdr_units
            write (9000,*) "BASIN                     basin_carbon_all.txt"
          end if
        end if

      return
      end subroutine carbon_legacy_open

      end module carbon_legacy_module
