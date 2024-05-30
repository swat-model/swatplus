      subroutine cal_allo_init

      use sd_channel_module
      use hru_lte_module
      use organic_mineral_mass_module
      use hru_module, only : hru, hru_init, bss
      use soil_module
      use plant_module
      use plant_data_module
      use hydrograph_module !, only : sp_ob, res, res_om_init, ch_stor, ch_om_water_init, wet, wet_om_init
      use calibration_data_module
      use reservoir_data_module
      use aquifer_module
      use mgt_operations_module
      use conditional_module
      
      implicit none
      
      integer :: nplt
      integer :: icom
      integer :: iauto
      integer :: isched
      integer :: id
      integer :: nly1
      integer :: iihru

      allocate (hru_init(0:sp_ob%hru))
      allocate (soil_init(0:sp_ob%hru))
      allocate (rsd1_init(0:sp_ob%hru))
      allocate (pl_mass_init(0:sp_ob%hru))
      allocate (pcom_init(0:sp_ob%hru))

      do iihru = 1, sp_ob%hru
        icom = hru(iihru)%plant_cov
        nplt = pcomdb(icom)%plants_com
        allocate (pcom_init(iihru)%plg(nplt)) 
        allocate (pcom_init(iihru)%plm(nplt)) 
        allocate (pcom_init(iihru)%plstr(nplt)) 
        allocate (pcom_init(iihru)%plcur(nplt))
        allocate (pl_mass_init(iihru)%tot(nplt)) 
        allocate (pl_mass_init(iihru)%ab_gr(nplt))
        allocate (pl_mass_init(iihru)%leaf(nplt))
        allocate (pl_mass_init(iihru)%stem(nplt))
        allocate (pl_mass_init(iihru)%seed(nplt))
        allocate (pl_mass_init(iihru)%root(nplt))
        allocate (pl_mass_init(iihru)%yield_tot(nplt))
        allocate (pl_mass_init(iihru)%yield_yr(nplt))

        isched = hru(iihru)%mgt_ops
        allocate (pcom_init(iihru)%dtbl(sched(isched)%num_autos))
        do iauto = 1, sched(isched)%num_autos
          id = sched(isched)%num_db(iauto)
          allocate (pcom_init(iihru)%dtbl(iauto)%num_actions(dtbl_lum(id)%acts))
          pcom_init(iihru)%dtbl(iauto)%num_actions = 1
          allocate (pcom_init(iihru)%dtbl(iauto)%days_act(dtbl_lum(id)%acts))
          pcom_init(iihru)%dtbl(iauto)%days_act = 0
        end do
             
        allocate (rsd1_init(iihru)%tot(nplt))
             
        nly1 = soil(iihru)%nly + 1                                                                                                         
        allocate (soil_init(iihru)%ly(nly1))
        !allocate (soil_init(iihru)%ly(nly1)%rs(nplt))    !bac and pest not allocated
        allocate (soil_init(iihru)%phys(nly1))
      end do
      
      allocate (hlt_init(0:sp_ob%hru_lte))
      allocate (sdch_init(0:sp_ob%chandeg))

      !! initialize all hru parameters
      if (sp_ob%hru > 0) then
        hru_init = hru
        soil_init = soil
        soil1_init = soil1
        rsd1_init = rsd1
        pcom_init = pcom
        pl_mass_init = pl_mass
        wet = wet_om_init
        bss = 0.
      end if
      
      !! initialize hru_lte parameters
      if (sp_ob%hru_lte > 0) then
        hlt_init = hlt
      end if
      
      !! initialize channel lte storage and dimensions
      if (sp_ob%chandeg > 0) then
        sdch_init = sd_ch
        ch_stor = ch_om_water_init
        fp_stor = fp_om_water_init
      end if
      
      !! initialize reservoir storage
      if (sp_ob%res > 0) then
        res = res_om_init
      end if
      
      !! initialize aquifer storage
      if (sp_ob%aqu > 0) then
        aqu_d = aqu_om_init
      end if

	  return
      end subroutine cal_allo_init