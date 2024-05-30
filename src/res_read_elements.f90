      subroutine res_read_elements
   
      use input_file_module
      use maximum_data_module
      use calibration_data_module
      use hydrograph_module
      use reservoir_module
      
      implicit none

      character (len=500) :: header    !              |header of file
      character (len=80) :: titldum    !              |title of file
      integer :: eof                   !              |end of file
      logical :: i_exist               !              |check to determine if file exists
      integer :: imax                  !none          |determine max number for array (imax) and total number in file
      integer :: mcal                  !              |
      integer :: mreg                   !             |
      integer :: i                      !none         |counter
      integer :: k                      !             |
      integer :: nspu                   !             | 
      integer :: isp                    !             |
      integer :: ielem1                 !none         |counter
      integer :: ireg                   !none         |counter
      integer :: ires                   !none         |counter
      
      imax = 0
      mcal = 0
      mreg = 0
            
    inquire (file=in_regs%def_res, exist=i_exist)
    if (i_exist .or. in_regs%def_res /= "null") then
      do
        open (107,file=in_regs%def_res)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) mreg
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        
        !allocate subbasin (landscape unit) outputs
        !allocate (reg_aqu_d(0:mreg)); allocate (reg_aqu_m(0:mreg)); allocate (reg_aqu_y(0:mreg)); allocate (reg_aqu_a(0:mreg))

      do i = 1, mreg

        read (107,*,iostat=eof) k, rcu_out(i)%name, rcu_out(i)%area_ha, nspu       
        if (eof < 0) exit
        if (nspu > 0) then
          allocate (elem_cnt(nspu))
          backspace (107)
          read (107,*,iostat=eof) k, rcu_out(i)%name, rcu_out(i)%area_ha, nspu, (elem_cnt(isp), isp = 1, nspu)
          if (eof < 0) exit

          call define_unit_elements (nspu, ielem1)
          
          allocate (rcu_out(i)%num(ielem1))
          rcu_out(i)%num = defunit_num
          rcu_out(i)%num_tot = ielem1
          deallocate (defunit_num)
        else
          !!all hrus are in region 
          allocate (rcu_out(i)%num(sp_ob%hru))
          rcu_out(i)%num_tot = sp_ob%res
          do ires = 1, sp_ob%res
            rcu_out(i)%num(ires) = ires
          end do      
        end if

      end do    ! i = 1, mreg
      exit
         
      db_mx%res_out = mreg
      end do 
      end if	  
        
    !! setting up regions for reservoir soft cal and/or output by type
    inquire (file=in_regs%def_res_reg, exist=i_exist)
    if (i_exist .or. in_regs%def_res_reg /= "null") then
      do
        open (107,file=in_regs%def_res_reg)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) mreg
        if (eof < 0) exit
        read (107,*,iostat=eof) header    
        if (eof < 0) exit
        
      do i = 1, mreg

        read (107,*,iostat=eof) k, rcu_cal(i)%name, rcu_cal(i)%area_ha, nspu       
        if (eof < 0) exit
        if (nspu > 0) then
          allocate (elem_cnt(nspu))
          backspace (107)
          read (107,*,iostat=eof) k, rcu_cal(i)%name, rcu_cal(i)%area_ha, nspu, (elem_cnt(isp), isp = 1, nspu)
          if (eof < 0) exit

          call define_unit_elements (nspu, ielem1)
          
          allocate (rcu_cal(i)%num(ielem1))
          rcu_cal(i)%num = defunit_num
          rcu_cal(i)%num_tot = ielem1
          deallocate (defunit_num)
        else
          !!all hrus are in region 
          allocate (rcu_reg(i)%num(sp_ob%hru))
          rcu_reg(i)%num_tot = sp_ob%res
          do ires = 1, sp_ob%res
            rcu_reg(i)%num(ires) = ires
          end do      
        end if

      end do    ! i = 1, mreg
      exit
         
      db_mx%res_reg = mreg
      end do 
      end if	  
      
      !! if no regions are input, don"t need elements
      if (mreg > 0) then
        do ireg = 1, mreg
          rcu_cal(ireg)%lum_ha_tot = 0.
          rcu_cal(ireg)%lum_num_tot = 0
          rcu_cal(ireg)%lum_ha_tot = 0.
          !allocate (region(ireg)%lum_ha_tot(db_mx%landuse))
          !allocate (region(ireg)%lum_num_tot(db_mx%landuse))
          !allocate (rwb_a(ireg)%lum(db_mx%landuse))
          !allocate (rnb_a(ireg)%lum(db_mx%landuse))
          !allocate (rls_a(ireg)%lum(db_mx%landuse))
          !allocate (rpw_a(ireg)%lum(db_mx%landuse))
        end do
      end if
      
      !!read data for each element in all landscape cataloging units
      inquire (file=in_regs%ele_res, exist=i_exist)
      if (i_exist .or. in_regs%ele_res /= "null") then
      do
        open (107,file=in_regs%ele_res)
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

        allocate (rcu_elem(imax))

        rewind (107)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit

        do isp = 1, imax
          read (107,*,iostat=eof) i
          if (eof < 0) exit
          backspace (107)
          read (107,*,iostat=eof) k, rcu_elem(i)%name, rcu_elem(i)%obtyp, rcu_elem(i)%obtypno,      &
                                    rcu_elem(i)%bsn_frac, rcu_elem(i)%ru_frac, rcu_elem(i)%reg_frac
          if (eof < 0) exit
        end do
        exit
      end do
      end if
      
      ! set hru number from element number and set hru areas in the region
      do ireg = 1, mreg
        do ires = 1, rcu_reg(ireg)%num_tot
          ielem1 = rcu_reg(ireg)%num(ires)
          !switch %num from element number to res number
          rcu_cal(ireg)%num(ires) = rcu_elem(ielem1)%obtypno
        end do
      end do
      
      close (107)

      return
      end subroutine res_read_elements