      subroutine proc_db
      
      implicit none

      !! databases used by all spatial modules
      call plant_parm_read                          !! read the plant paramter database
      call plantparm_init                           !! initialize plant parameters
      call plant_transplant_read                    !! read plant transplant data
      call till_parm_read                           !! read the tillage database
      call pest_parm_read                           !! read the pesticide database
      call fert_parm_read                           !! read the fertilizer/nutrient database
      call manure_parm_read                         !! read the manure database - includes pathogens/antibiotics
      call urban_parm_read                          !! read the urban land types database
      call path_parm_read                           !! read the pathogen data parameters
      call septic_parm_read 
      
      !! read management scheduling and data files      
      call mgt_read_irrops
      call mgt_read_chemapp
      call mgt_read_harvops
      call mgt_read_grazeops
      call mgt_read_sweepops
      call mgt_read_fireops
      call mgt_read_mgtops
      call mgt_read_puddle
      
      !! read structural operations files
      call sdr_read
      call sep_read
      call scen_read_grwway
      call scen_read_filtstrip
      call scen_read_bmpuser

      !! read the plant community database
      call readpcom
      
      call cntbl_read
      call cons_prac_read
      call overland_n_read
      call landuse_read
     
	  return
      end subroutine proc_db