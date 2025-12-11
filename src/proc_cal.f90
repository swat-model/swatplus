      subroutine proc_cal
    
      use hydrograph_module
      use calibration_data_module
   
      implicit none
      
      external :: aqu_read_elements, cal_conditions, cal_parm_read, cal_parmchg_read, calsoft_read_codes, &
                  ch_read_elements, ch_read_orders_cal, ch_read_parms_cal, lcu_read_softcal, &
                  ls_read_lsparms_cal, pl_read_parms_cal, pl_read_regions_cal, rec_read_elements, &
                  res_read_elements, cal_cond_read, lsu_read_elements, cal_allo_init
   
      !read calibration data (if included)
      call cal_parm_read
      call cal_parmchg_read

      !! need to read plant parms before calibrating
      call pl_read_regions_cal      !soft data for plant parms (lai_pot and harv_idx) calibration
      call pl_read_parms_cal
      call cal_conditions

      !! read soft calibration parameters
      call calsoft_read_codes
      
      !call reg_read_elements       !defining regions by lsu and/or hru
      call lcu_read_softcal         !soft data for landscape calibration (needs to be renamed)***
      call ls_read_lsparms_cal
      call aqu_read_elements        !defining regions by aquifer
      call ch_read_elements         !defining regions by channel
      call res_read_elements        !defining regions by reservoir
      call rec_read_elements        !defining regions by recall object (point source, gage data, model output, etc)
      call ch_read_orders_cal
      call ch_read_parms_cal
      
      if (cal_soft == "y" .or. cal_hard == "y") call cal_allo_init

      return
      
      end subroutine proc_cal