      subroutine calsoft_control
    
      use sd_channel_module
      use hru_lte_module
      use maximum_data_module 
      use calibration_data_module
      use time_module
      use basin_module
      use hru_module, only : hru
      use hydrograph_module
      
      implicit none
      
      integer :: ireg        !none      |counter
      integer :: ilum        !none      |counter
      integer :: icvmax      !          |
      integer :: nyskip      !          |
      integer :: ihru        !none      |counter
      integer :: isdh        !none      |counter
      integer :: idb         !          |   
      integer :: iord        !none      |counter 
      
      nyskip = pco%nyskip
      pco = pco_init
      pco%wb_bsn%a = "y"
      pco%nyskip = nyskip
      !pco%wb_bsn%y = "y"
      !pco%wb_bsn%m = "y"
      !pco%wb_bsn%d = "y"
      !pco%wb_hru%a = "y"
      !pco%sd_chan%a = "y"

      !calibrate hydrology for hru
      if (cal_codes%hyd_hru /= "n") then
        if (cal_codes%hyd_hru == "a") then
          call calsoft_hyd        !calibrate all components
        else
          call calsoft_hyd_bfr    !calibrate total and baseflow
        end if
 
        !print calibrated hydrology for hru_lte
		do ireg = 1, db_mx%lsu_reg
           do ilum = 1, region(ireg)%nlum
            lscal(ireg)%lum(ilum)%meas%srr = lscal(ireg)%lum(ilum)%precip_aa_sav * lscal(ireg)%lum(ilum)%meas%srr
            lscal(ireg)%lum(ilum)%meas%lfr = lscal(ireg)%lum(ilum)%precip_aa_sav * lscal(ireg)%lum(ilum)%meas%lfr
            lscal(ireg)%lum(ilum)%meas%pcr = lscal(ireg)%lum(ilum)%precip_aa_sav * lscal(ireg)%lum(ilum)%meas%pcr
            lscal(ireg)%lum(ilum)%meas%etr = lscal(ireg)%lum(ilum)%precip_aa_sav * lscal(ireg)%lum(ilum)%meas%etr
            lscal(ireg)%lum(ilum)%meas%tfr = lscal(ireg)%lum(ilum)%precip_aa_sav * lscal(ireg)%lum(ilum)%meas%tfr
            
            !write (5000,500) lscal(ireg)%lum(ilum)%name, lscal(ireg)%lum(ilum)%ha, lscal(ireg)%lum(ilum)%nbyr,  &
            !        lscal(ireg)%lum(ilum)%precip_aa_sav, lscal(ireg)%lum(ilum)%meas, lscal(ireg)%lum(ilum)%aa,  &
            !        lscal(ireg)%lum(ilum)%prm
		  end do
        end do  

      end if

      !calibrate hydrology for hru_lte
      if (cal_codes%hyd_hrul == "y") then
        call caltsoft_hyd
        !print calibrated hydrology for hru_lte
		do ireg = 1, db_mx%lsu_reg
		  do ilum = 1, lscalt(ireg)%lum_num
            lscalt(ireg)%lum(ilum)%meas%srr = lscalt(ireg)%lum(ilum)%precip_aa_sav * lscalt(ireg)%lum(ilum)%meas%srr
            lscalt(ireg)%lum(ilum)%meas%lfr = lscalt(ireg)%lum(ilum)%precip_aa_sav * lscalt(ireg)%lum(ilum)%meas%lfr
            lscalt(ireg)%lum(ilum)%meas%pcr = lscalt(ireg)%lum(ilum)%precip_aa_sav * lscalt(ireg)%lum(ilum)%meas%pcr
            lscalt(ireg)%lum(ilum)%meas%etr = lscalt(ireg)%lum(ilum)%precip_aa_sav * lscalt(ireg)%lum(ilum)%meas%etr
            lscalt(ireg)%lum(ilum)%meas%tfr = lscalt(ireg)%lum(ilum)%precip_aa_sav * lscalt(ireg)%lum(ilum)%meas%tfr
            
            !write (5000,500) lscalt(ireg)%name, lscalt(ireg)%lum(ilum)%ha, lscalt(ireg)%lum(ilum)%nbyr,           &
            !        lscalt(ireg)%lum(ilum)%precip_aa_sav, lscalt(ireg)%lum(ilum)%meas, lscalt(ireg)%lum(ilum)%aa, &
            !        lscalt(ireg)%lum(ilum)%prm	
		  end do
        end do  

	    do isdh = 1, sp_ob%hru_lte
	      idb = hlt(isdh)%props
		  write (4999,*) hlt(isdh)%name, hlt_db(idb)%dakm2, hlt(isdh)%cn2, hlt(isdh)%cn3_swf, hlt_db(idb)%tc,       &
		    hlt_db(idb)%soildep, hlt(isdh)%perco, hlt_db(isdh)%slope, hlt_db(idb)%slopelen,                         &
		    hlt(isdh)%etco, hlt_db(idb)%sy, hlt_db(idb)%abf, hlt(idb)%revapc,                                       &
		    hlt_db(idb)%percc, hlt_db(idb)%sw, hlt_db(idb)%gw, hlt_db(idb)%gwflow,                                  &
		    hlt_db(idb)%gwdeep, hlt_db(idb)%snow, hlt_db(idb)%xlat, hlt_db(idb)%text,                               &
		    hlt_db(idb)%tropical, hlt_db(idb)%igrow1, hlt_db(idb)%igrow2, hlt_db(idb)%plant, hlt(isdh)%stress,      &
		    hlt_db(idb)%ipet, hlt_db(idb)%irr, hlt_db(idb)%irrsrc, hlt_db(idb)%tdrain,                              &
            hlt_db(idb)%uslek, hlt_db(idb)%uslec, hlt_db(idb)%uslep, hlt_db(idb)%uslels
	    end do
      end if
        
      !calibrate plant growth
      if (cal_codes%plt == "y") then
        call calsoft_plant
      end if
      
      !calibrate sediment yield from uplands (hru"s)
      if (cal_codes%sed == "y") then
        call calsoft_sed
      end if 

      !calibrate channel sediment 
      if (cal_codes%chsed == "y") then
        call calsoft_chsed
      end if

      if (cal_codes%chsed == "y") then
        do ireg = 1, db_mx%ch_reg
          do iord = 1, chcal(ireg)%ord_num
            !! channel sediment parms
            if (chcal(ireg)%ord(iord)%aa%chw > 1.e-6) icvmax = icvmax + 1
          end do
        end do
      end if
           
      !! write output to hydrology-cal.hyd   
      if (cal_codes%hyd_hru /= "n") then
        write (5001,*) " hydrology-cal.hyd developed from soft data calibration"
        !write (5001,*) " NAME LAT_TTIME LAT_SED CAN_MAX  ESCO  EPCO ORGN_ENRICH ORGP_ENRICH CN3_SWF &
        !                                BIO_MIX PERCO LAT_ORGN LAT_ORGP PET_CO LATQ_CO NOT_USED"
        write (5001,5010)
        do ihru = 1, sp_ob%hru
          write (5001,*) hru(ihru)%hyd
        end do
      end if

      !! plant parms
      if (cal_codes%plt == "y") then
        call pl_write_parms_cal
                   
        !! write perco to hydrology-cal.hyd
        write (5001,*) " hydrology-cal.hyd developed from soft data calibration"
        !write (5001,*) " NAME LAT_TTIME LAT_SED CAN_MAX  ESCO  EPCO ORGN_ENRICH ORGP_ENRICH CN3_SWF &
        !                                BIO_MIX PERCO LAT_ORGN LAT_ORGP PET_CO LATQ_CO NOT_USED"
        write (5001,5010)
5010    format (1x,'NAME',38x,'LAT_TTIME',12x,'LAT_SED',8x,'CAN_MAX',11x,'ESCO',7x,'EPCO',8x,        &
        'ORGN_ENRICH',4x,'ORGP_ENRICH',4x,'CN3_SWF',8x,'BIO_MIX',10x,'PERCO',11x,'LAT_ORGN',7x,      &
        'LAT_ORGP',5x,'PET_CO',8x,'LATQ_CO',7x,'NOT_USED')
        do ihru = 1, sp_ob%hru
          write (5001,*) hru(ihru)%hyd
        end do
      end if      ! plant parms
      
      if (cal_codes%chsed == "y") then
        do ireg = 1, db_mx%ch_reg
          do iord = 1, chcal(ireg)%ord_num
            !! channel sediment parms
            if (chcal(ireg)%ord(iord)%aa%chw > 1.e-6) icvmax = icvmax + 1
          end do
        end do
      end if
      
      !! channel sediment parms
      if (cal_codes%chsed == "y") then
        do ireg = 1, db_mx%ch_reg
          do iord = 1, chcal(ireg)%ord_num
            if (abs(chcal(ireg)%ord(iord)%aa%chw) > 1.e-6) then
              write (5000,503) ch_prms(1)%name, ch_prms(1)%chg_typ, chcal(ireg)%ord(iord)%prm%erod,          & 
              "     0      0      0      0      0      0      0      0      0"
            end if
          end do
        end do
      end if      ! channel sediment parms

  500 format (a16,f12.3,i12,f12.3,2(1x,a16,10f12.3),10f12.3)
  503 format (2a16,f12.5,a)
      
      return
      end subroutine calsoft_control