      subroutine lcu_read_softcal
   
      use input_file_module
      use maximum_data_module
      use calibration_data_module
      use hydrograph_module
      use hru_module, only : ihru  
      use hru_lte_module
      use output_landscape_module
      use basin_module
      
      implicit none
      
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: mcal                 !           |
      integer :: mreg                 !none       |end of loop
      integer :: ireg                 !none       |counter 
      integer :: mlug                 !none       |end of loop
      integer :: ilum                 !none       |counter
       
      imax = 0
      mcal = 0
      mreg = 0
	  
      inquire (file=in_chg%water_balance_sft, exist=i_exist)
      if (.not. i_exist .or. in_chg%water_balance_sft == "null") then
        allocate (lscal(0:0))
        allocate (region(0:0))
      else  
        do
          open (107,file=in_chg%water_balance_sft)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) mreg
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          
          allocate (lscal(0:mreg))
          allocate (region(0:mreg))
          !! allocate regional output files
          allocate (rwb_d(mreg)); allocate (rwb_m(mreg)); allocate (rwb_y(mreg)); allocate (rwb_a(mreg))
          allocate (rnb_d(mreg)); allocate (rnb_m(mreg)); allocate (rnb_y(mreg)); allocate (rnb_a(mreg))
          allocate (rls_d(mreg)); allocate (rls_m(mreg)); allocate (rls_y(mreg)); allocate (rls_a(mreg))
          allocate (rpw_d(mreg)); allocate (rpw_m(mreg)); allocate (rpw_y(mreg)); allocate (rpw_a(mreg))

          db_mx%lsu_reg = mreg

          do ireg = 1, mreg

            read (107,*,iostat=eof) region(ireg)%name, region(ireg)%nlum
            if (eof < 0) exit
            
            db_mx%landuse = region(ireg)%nlum
            mlug = region(ireg)%nlum
            allocate (region(ireg)%lum_ha_tot(mlug))
            allocate (region(ireg)%lum_num_tot(mlug))
            allocate (lscal(ireg)%lum(mlug))
            !! allocate land use for each regional output
            allocate (rwb_a(ireg)%lum(mlug))
            allocate (rnb_a(ireg)%lum(mlug))
            allocate (rls_a(ireg)%lum(mlug))
            allocate (rpw_a(ireg)%lum(mlug))
            region(ireg)%lum_ha_tot = 0.
            region(ireg)%lum_num_tot = 0
            region(ireg)%lum_ha_tot = 0.
            
            if (mlug > 0) then
              read (107,*,iostat=eof) header
              if (eof < 0) exit
              !! read soft calibration data for each land use within the region
              do ilum = 1, mlug
                read (107,*,iostat=eof) lscal(ireg)%lum(ilum)%meas
                if (eof < 0) exit
                !! when using wyr and bfr to calibrate
                if (cal_codes%hyd_hru /= "a") then
                  !! convert baseflow ratio from frac of water yield to frac of precip
                  lscal(ireg)%lum(ilum)%meas%srr = lscal(ireg)%lum(ilum)%meas%wyr * (1. - lscal(ireg)%lum(ilum)%meas%bfr)
                  lscal(ireg)%lum(ilum)%meas%bfr = lscal(ireg)%lum(ilum)%meas%wyr * lscal(ireg)%lum(ilum)%meas%bfr
                  lscal(ireg)%lum(ilum)%meas%lfr = 0.1 * lscal(ireg)%lum(ilum)%meas%bfr
                end if
              end do
            end if 
               
            !! if calibrating the entire region - later we can set up for lsu/regional calibrations
            if (region(ireg)%name == "basin" .or. db_mx%lsu_reg == 1) then
              region(ireg)%num_tot = sp_ob%hru
              allocate (region(ireg)%num(sp_ob%hru))
              allocate (region(ireg)%hru_ha(sp_ob%hru))
              do ihru = 1, sp_ob%hru
                region(ireg)%num(ihru) = ihru
                region(ireg)%hru_ha(ihru) = bsn%area_ls_ha * lsu_elem(ihru)%bsn_frac
              end do
            end if
            
          end do    !mreg

          exit
        end do 
      end if	  
	  
      return
      end subroutine lcu_read_softcal