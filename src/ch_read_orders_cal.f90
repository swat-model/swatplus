      subroutine ch_read_orders_cal
   
      use input_file_module
      use maximum_data_module
      use calibration_data_module
      use hydrograph_module
      use sd_channel_module
      
      implicit none

      character (len=80) :: titldum    !          |title of file
      character (len=80) :: header     !          |header of file
      integer :: eof                   !          |end of file
      integer :: ihru                  !          |number of hrus
      logical :: i_exist               !          |check to determine if file exists
      integer :: imax                  !          |determine max number for array (imax) and total number in file
      integer :: mcal                  !units     |description
      integer :: mreg                  !units     |description
      integer :: i                     !none      |counter
      integer :: nspu                  !units     |description
      integer :: isp                   !none      |counter
      integer :: ielem                 !none      |counter
      integer :: ii                    !none      |counter
      integer :: ie                    !none      |counter
      integer :: ie1                   !beginning of loop
      integer :: ie2                   !ending of loop
      integer :: iord_mx               !ending of loop
      integer :: iord                  !none      |counter
      integer :: ich_s                 !none      |counter
      
      imax = 0
      mcal = 0
      mreg = 0
 
      inquire (file=in_chg%ch_sed_budget_sft, exist=i_exist)
      if (.not. i_exist .or. in_chg%ch_sed_budget_sft == "null") then
           allocate (chcal(0:0))	      
      else 
      do
        open (107,file=in_chg%ch_sed_budget_sft)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) mreg
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        allocate (chcal(mreg))

      do i = 1, mreg

        read (107,*,iostat=eof) chcal(i)%name, chcal(i)%ord_num, nspu       
        if (eof < 0) exit
        if (nspu > 0) then
          allocate (elem_cnt(nspu))
          backspace (107)
          read (107,*,iostat=eof) chcal(i)%name, chcal(i)%ord_num,  nspu, (elem_cnt(isp), isp = 1, nspu)
          if (eof < 0) exit         
          !!save the object number of each defining unit
          ielem = 0
          do ii = 1, nspu
            ie1 = elem_cnt(ii)
            if (ii == nspu) then
              ielem = ielem + 1
            else
              if (elem_cnt(ii+1) < 0) then
                ie2 = abs(elem_cnt(ii+1))
                do ie = ie1, ie2
                  ielem = ielem + 1
                end do
                if (ii+1 == nspu) exit
              else
                ielem = ielem + 1
              end if
            end if
            if (ii == nspu .and. elem_cnt(ii) < 0) exit
          end do
          allocate (chcal(i)%num(ielem))
          chcal(i)%num_tot = ielem

          ielem = 0
          ii = 1
          do while (ii <= nspu)
            ie1 = elem_cnt(ii)
            if (ii == nspu) then
              ielem = ielem + 1
              ii = ii + 1
              chcal(i)%num(ielem) = ie1
            else
              ie2 = elem_cnt(ii+1)
              if (ie2 > 0) then
                ielem = ielem + 1
                chcal(i)%num(ielem) = ie1
                ielem = ielem + 1
                chcal(i)%num(ielem) = ie2
              else
                ie2 = abs(ie2)
                do ie = ie1, ie2
                  ielem = ielem + 1
                  chcal(i)%num(ielem) = ie
                end do
              end if
              ii = ii + 2
            end if
          end do
          deallocate (elem_cnt)
        else
          !!all channels are in region
          allocate (chcal(i)%num(sp_ob%chandeg))
          chcal(i)%num_tot = sp_ob%chandeg
          do ich = 1, sp_ob%chandeg
            chcal(i)%num(ich) = ich
          end do
        end if
        
        !! read channel soft calibration data for each land use
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        if (chcal(i)%ord_num > 0) then
          iord_mx = chcal(i)%ord_num
          allocate (chcal(i)%ord(iord_mx))
          do iord = 1, iord_mx
            read (107,*,iostat=eof) chcal(i)%ord(iord)%meas
            if (eof < 0) exit
            
            ! set hru number from element number and set hru areas in the region
            if (db_mx%cha_reg > 0) then
              do ihru = 1, ccu_reg(i)%num_tot      !elements have to be hru or hru_lte
                ielem = ccu_reg(i)%num(ihru)
                !switch %num from element number to hru number
                ccu_cal(i)%num(ihru) = ccu_elem(ielem)%obtypno
                ccu_cal(i)%hru_ha(ihru) = ccu_elem(ielem)%ru_frac * ccu_cal(i)%area_ha
              end do
            end if
        
            !! sum total channel length for
            do ich_s = 1, chcal(i)%num_tot
              ich = chcal(i)%num(ich_s)
              if (chcal(i)%ord(iord)%meas%name == sd_ch(ich)%order) then
                chcal(i)%ord(iord)%length = chcal(i)%ord(iord)%length + sd_ch(ich)%chl
              end if
            end do
          end do
        end if   
      end do
      exit
         
      end do    
      end if
        
      db_mx%ch_reg = mreg
      
      return
      end subroutine ch_read_orders_cal