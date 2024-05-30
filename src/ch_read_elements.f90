      subroutine ch_read_elements
   
      use input_file_module
      use maximum_data_module
      use calibration_data_module
      use hydrograph_module
      use sd_channel_module
      
      implicit none

      character (len=80) :: titldum    !          |title of file
      character (len=80) :: header     !          |header of file
      integer :: eof                   !          |end of file
      integer :: imax                  !          |determine max number for array (imax) and total number in file
      integer :: mcal                  !units     |description
      logical :: i_exist               !          |check to determine if file exists
      integer :: mreg                  !units     |description 
      integer :: i                     !none      |counter
      integer :: k                     !units     |description
      integer :: nspu                  !units     |description
      integer :: isp                   !none      |counter
      integer :: ielem1                !units     |description
      integer :: icha                  !none      |counter 
      integer :: ireg                  !none      |counter
      integer :: ires                  !none      |counter
     
      imax = 0
      mcal = 0
      mreg = 0
            
    inquire (file=in_regs%def_cha, exist=i_exist)
    if (i_exist .or. in_regs%def_cha /= "null") then
      do
        open (107,file=in_regs%def_cha)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) mreg
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        
        !! allocate channel outputs for regions
        allocate (schsd_d(0:mreg)); allocate (schsd_m(0:mreg)); allocate (schsd_y(0:mreg)); allocate (schsd_a(0:mreg))

      do i = 1, mreg

        read (107,*,iostat=eof) k, ccu_out(i)%name, ccu_out(i)%area_ha, nspu       
        if (eof < 0) exit
        if (nspu > 0) then
          allocate (elem_cnt(nspu))
          backspace (107)
          read (107,*,iostat=eof) k, ccu_out(i)%name, ccu_out(i)%area_ha, nspu, (elem_cnt(isp), isp = 1, nspu)
          if (eof < 0) exit

          call define_unit_elements (nspu, ielem1)
          
          allocate (ccu_out(i)%num(ielem1))
          ccu_out(i)%num = defunit_num
          ccu_out(i)%num_tot = ielem1
          deallocate (defunit_num)
        else
          !!all hrus are in region 
          allocate (ccu_out(i)%num(sp_ob%hru))
          ccu_out(i)%num_tot = sp_ob%res
          do icha = 1, sp_ob%chan
            ccu_out(i)%num(ires) = icha
          end do      
        end if

      end do    ! i = 1, mreg
      exit
         
      db_mx%cha_reg = mreg
      end do 
      end if	  
        
    !! setting up regions for channel soft cal and/or output by order
    inquire (file=in_regs%def_cha_reg, exist=i_exist)
    if (i_exist .or. in_regs%def_cha_reg /= "null") then
      do
        open (107,file=in_regs%def_cha_reg)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) mreg
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit

      do i = 1, mreg

        read (107,*,iostat=eof) k, ccu_reg(i)%name, ccu_reg(i)%area_ha, nspu      
        if (eof < 0) exit
        if (nspu > 0) then
          allocate (elem_cnt(nspu))
          backspace (107)
          read (107,*,iostat=eof) k, ccu_reg(i)%name, ccu_reg(i)%area_ha, nspu, (elem_cnt(isp), isp = 1, nspu)
          if (eof < 0) exit

          call define_unit_elements (nspu, ielem1)
          
          allocate (ccu_reg(i)%num(ielem1))
          ccu_reg(i)%num = defunit_num
          ccu_reg(i)%num_tot = ielem1

        else
          !!all channels are in region 
          allocate (ccu_reg(i)%num(sp_ob%hru))
          ccu_reg(i)%num_tot = sp_ob%chan
          do icha = 1, sp_ob%chan
            ccu_reg(i)%num(icha) = icha
          end do      
        end if

      end do    ! i = 1, mreg
      exit
         
      db_mx%cha_reg = mreg
      end do 
      end if	  

      !! if no regions are input, don"t need elements
      if (mreg > 0) then
      
      do ireg = 1, mreg
        ccu_cal(ireg)%lum_ha_tot = 0.
        ccu_cal(ireg)%lum_num_tot = 0
        ccu_cal(ireg)%lum_ha_tot = 0.
        !allocate (region(ireg)%lum_ha_tot(db_mx%landuse))
        !allocate (region(ireg)%lum_num_tot(db_mx%landuse))
        !allocate (rwb_a(ireg)%lum(db_mx%landuse))
        !allocate (rnb_a(ireg)%lum(db_mx%landuse))
        !allocate (rls_a(ireg)%lum(db_mx%landuse))
        !allocate (rpw_a(ireg)%lum(db_mx%landuse))
      end do
      end if    ! mreg > 0
      
      !!read data for each element in all landscape cataloging units
      inquire (file="element.ccu", exist=i_exist)
      if (i_exist ) then
      do
        open (107,file="element.ccu")
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        imax = 0
          do while (eof == 0)
              read (107,*,iostat=eof) i
              if (eof < 0) exit
              imax = Max(i,imax)
          end do

        allocate (ccu_elem(imax))

        rewind (107)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit

        do isp = 1, imax
          read (107,*,iostat=eof) i
          if (eof < 0) exit
          backspace (107)
          read (107,*,iostat=eof) k, ccu_elem(i)%name, ccu_elem(i)%obtyp, ccu_elem(i)%obtypno,      &
                                    ccu_elem(i)%bsn_frac, ccu_elem(i)%ru_frac, ccu_elem(i)%reg_frac
          if (eof < 0) exit
        end do
        exit
      end do
      end if
      
      ! set hru number from element number and set hru areas in the region
      do ireg = 1, mreg
        do icha = 1, ccu_reg(ireg)%num_tot      !elements have to be hru or hru_lte
          ielem1 = ccu_reg(ireg)%num(icha)
          !switch %num from element number to hru number
          ccu_cal(ireg)%num(icha) = ccu_elem(ielem1)%obtypno
          ccu_cal(ireg)%hru_ha(icha) = ccu_elem(ielem1)%ru_frac * ccu_cal(ireg)%area_ha
        end do
      end do
      
      close (107)

      return
      end subroutine ch_read_elements