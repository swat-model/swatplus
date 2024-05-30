      subroutine ru_read_elements
    
      use hydrograph_module
      use input_file_module
      use maximum_data_module
      use dr_module
      
      implicit none
  
      character (len=3) :: iobtyp     !none       |object type   
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      character (len=16) :: namedum   !           |
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      integer :: nspu                 !           |
      logical :: i_exist              !none       |check to determine if file exists
      integer :: i                    !none       |counter
      integer :: max                  !           | 
      integer :: isp                  !none       |counter
      integer :: k                    !           |
      integer :: iob                  !           |
      integer :: iob1                 !none       |beginning of loop
      integer :: iob2                 !none       |ending of loop
      integer :: iru                  !none       |counter
      integer :: idr                  !none       |counter
      integer :: numb                 !           |
      integer :: ielem1               !none       |counter
      integer :: ii                   !none       |counter
      integer :: ie                   !none       |counter
      integer :: iru_tot              !           |
      
      eof = 0
      imax = 0
      
      !!read data for each element in all subbasins
      inquire (file=in_ru%ru_ele, exist=i_exist)
      if (i_exist .or. in_ru%ru_ele /= "null") then
      do
        open (107,file=in_ru%ru_ele)
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

        db_mx%ru_elem = imax
        allocate (ru_def(imax))
        allocate (ru_elem(imax))
        allocate (ielem_ru(imax))
        
        rewind (107)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        
        ielem_ru = 0
   
        do isp = 1, imax
          read (107,*,iostat=eof) i
          if (eof < 0) exit
          backspace (107)
          read (107,*,iostat=eof) k, ru_elem(i)%name, ru_elem(i)%obtyp, ru_elem(i)%obtypno,     &
                                ru_elem(i)%frac, ru_elem(i)%dr_name
          if (eof < 0) exit
          
          ! xwalk ru_elem(i)%dr_name with dr_db()%name from delratio.del file
          do idr = 1, db_mx%dr_om
            if (ru_elem(i)%dr_name == dr_db(idr)%name) then
              !! dr_om_num was previously xwalked with dr_db()%om_file
              ru_elem(i)%dr = dr(dr_om_num(idr))
              exit
            end if
          end do
      
        end do
        exit
      end do
      close (107)

      end if
      
      !!read subbasin definition data -ie. hru"s in the subbasin
      inquire (file=in_ru%ru_def, exist=i_exist)
      if (i_exist .or. in_ru%ru_def /= "null") then
      do
        open (107,file=in_ru%ru_def)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
          imax = 0
          do while (eof == 0)
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do

        rewind (107)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit

      iob1 = sp_ob1%ru
      iob2 = sp_ob1%ru + sp_ob%ru - 1
      do iru = 1, sp_ob%ru
        iob = sp_ob1%ru + iru - 1
        ob(iob)%ru_tot = 0
        read (107,*,iostat=eof) numb, namedum, nspu
        if (eof < 0) exit
        
        ru_def(iru)%num_tot = 0                     !! Linux initialization issue (Srin)
        
        if (nspu > 0) then
          backspace (107)
          allocate (elem_cnt(nspu))
          read (107,*,iostat=eof) numb, ru_def(iru)%name, nspu, (elem_cnt(isp), isp = 1, nspu)
          if (eof < 0) exit

          call define_unit_elements (nspu, ielem1)
          
          allocate (ru_def(iru)%num(ielem1))
          ru_def(iru)%num = defunit_num
          ru_def(iru)%num_tot = ielem1
          deallocate (defunit_num)
          
          iob = sp_ob1%ru + iru - 1
          ob(iob)%dfn_tot = ru_def(iru)%num_tot

          ! determine how many subbasins the object is in
          do ielem1 = 1, ru_def(iru)%num_tot
            ii = ru_def(iru)%num(ielem1)
            iobtyp = ru_elem(ii)%obtyp       !object type in sub
            select case (iobtyp)
            case ("hru")   !hru
              ru_elem(ii)%obj = sp_ob1%hru + ru_elem(ii)%obtypno - 1
            case ("hlt")   !hru_lte
              ru_elem(ii)%obj = sp_ob1%hru_lte + ru_elem(ii)%obtypno - 1
            case ("ru")   !ru
              ru_elem(ii)%obj = sp_ob1%ru + ru_elem(ii)%obtypno - 1
            case ("cha")   !channel
              ru_elem(ii)%obj = sp_ob1%chan + ru_elem(ii)%obtypno - 1
            case ("res")   !reservoir
              ru_elem(ii)%obj = sp_ob1%res + ru_elem(ii)%obtypno - 1
            case ("exc")   !export coefficient
              ru_elem(ii)%obj = sp_ob1%exco + ru_elem(ii)%obtypno - 1
            case ("dr")   !delivery ratio
              ru_elem(ii)%obj = sp_ob1%dr + ru_elem(ii)%obtypno - 1
            case ("out")   !outlet
              ru_elem(ii)%obj = sp_ob1%outlet + ru_elem(ii)%obtypno - 1
            case ("sdc")   !swat-deg channel
              ru_elem(ii)%obj = sp_ob1%chandeg + ru_elem(ii)%obtypno - 1
            end select
            k = ru_elem(ii)%obj
            ob(k)%ru_tot = ob(k)%ru_tot + 1
          end do

        end if
      end do    ! i = subbasin object numbers
        exit
      enddo
      endif
      
        ! set all subbasins that each element is in
        do iru = 1, sp_ob%ru
          do ielem1 = 1, ru_def(iru)%num_tot
            ie = ru_def(iru)%num(ielem1)
            iob = ru_elem(ie)%obj
            iru_tot = ob(iob)%ru_tot
            allocate (ob(iob)%ru(1))
          end do

          do ielem1 = 1, ru_def(iru)%num_tot
            ie = ru_def(iru)%num(ielem1)
            ielem_ru(ie) = ielem_ru(ie) + 1
            iob = ru_elem(ie)%obj
            ob(iob)%ru(ielem_ru(ie)) = iru
            ob(iob)%elem = ielem1
          end do
        end do

      close (107)
      return

      end subroutine ru_read_elements