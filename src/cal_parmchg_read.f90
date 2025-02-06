      subroutine cal_parmchg_read
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this function computes new parameter value based on 
!!    user defined change

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    val_cur     |variable      |current parameter value
!!                               |the standard temperature (20 degrees C)
!!    chg         |data type     |contains information on variable change
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      
      use input_file_module
      use maximum_data_module
      use calibration_data_module
      use hydrograph_module
      use gwflow_module, only : ncell
      
      implicit none

      !integer, dimension (:), allocatable :: elem_cnt1   !           |  
      character (len=80) :: titldum = ""                  !           |title of file
      character (len=80) :: header = ""                   !           |header of file
      character (len=10) :: range = ""                    !           |real conditional variable range
      integer :: eof = 0                                  !           |end of file  
      integer :: imax = 0                                 !none       |determine max number for array (imax) and total number in file
      integer :: nspu = 0                                 !           |
      logical :: i_exist                                  !none       |check to determine if file exists
      integer :: i = 0                                    !none       |counter
      integer :: ie = 0                                   !none       |counter
      integer :: mcal = 0                                 !           |
      integer :: isp = 0                                  !none       |counter
      integer :: ical = 0                                 !none       |counter
      integer :: ipar = 0                                 !           |
      integer :: ielem1 = 0                               !none       |counter
      integer :: nconds = 0                               !none       |counter
      integer :: icond = 0                                !none       |counter
      
      imax = 0
      mcal = 0
        
      !!read parameter change values for calibration
      inquire (file=in_chg%cal_upd, exist=i_exist)
      if (.not. i_exist .or. in_chg%cal_upd == "null") then
        allocate (cal_upd(0:0))
      else
      do
        open (107,file=in_chg%cal_upd)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) mcal
        allocate (cal_upd(mcal))
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit

      do i = 1, mcal

        read (107,*,iostat=eof) cal_upd(i)%name, cal_upd(i)%chg_typ, cal_upd(i)%val, cal_upd(i)%conds,      &
              cal_upd(i)%lyr1, cal_upd(i)%lyr2, cal_upd(i)%year1, cal_upd(i)%year2, cal_upd(i)%day1,        &
              cal_upd(i)%day2, nspu       
        if (eof < 0) exit
        if (nspu > 0) then
          backspace (107)
          allocate (elem_cnt(nspu), source = 0)
          read (107,*,iostat=eof) cal_upd(i)%name, cal_upd(i)%chg_typ, cal_upd(i)%val, cal_upd(i)%conds,    &
              cal_upd(i)%lyr1, cal_upd(i)%lyr2, cal_upd(i)%year1, cal_upd(i)%year2, cal_upd(i)%day1,        &
              cal_upd(i)%day2, cal_upd(i)%num_tot, (elem_cnt(isp), isp = 1, nspu)
          if (eof < 0) exit
        end if
          
          !! crosswalk name with calibration parameter db
          do ical = 1, db_mx%cal_parms
            if (cal_upd(i)%name == cal_parms(ical)%name) then
              cal_upd(i)%num_db = ical
              exit
            end if
          end do
          
          !! allocate and read the conditions
          nconds = cal_upd(i)%conds
          if (nconds > 0) then
            allocate (cal_upd(i)%cond(nconds))
            do icond = 1, nconds
              read (107,*,iostat=eof) range
              backspace (107)
              if (range == "range") then
                read (107,*,iostat=eof) range, cal_upd(i)%cond(icond)%var,   &
                    cal_upd(i)%val1, cal_upd(i)%val2
              else
                read (107,*,iostat=eof) cal_upd(i)%cond(icond)
              end if 
            end do
          end if
        
          !!if no objects are specified - check all of them
          if (cal_upd(i)%num_tot == 0) then
            ipar = cal_upd(i)%num_db
            select case (cal_parms(ipar)%ob_typ)
            case ("hru")
              cal_upd(i)%num_elem = sp_ob%hru
            case ("rdt")
              cal_upd(i)%num_elem = db_mx%dtbl_res
            case ("plt")
              cal_upd(i)%num_elem = db_mx%plantparm
            case ("lyr")
              cal_upd(i)%num_elem = sp_ob%hru
            case ("sol")
              cal_upd(i)%num_elem = sp_ob%hru
            case ("hlt")
              cal_upd(i)%num_elem = sp_ob%hru_lte
            case ("ru")
              cal_upd(i)%num_elem = sp_ob%ru
            case ("aqu")
              cal_upd(i)%num_elem = sp_ob%aqu
            case ("cha")
              cal_upd(i)%num_elem = sp_ob%chan
            case ("swq")
              cal_upd(i)%num_elem = db_mx%ch_nut
            case ("res")
              cal_upd(i)%num_elem = sp_ob%res
            case ("sdc")
              cal_upd(i)%num_elem = sp_ob%chandeg
            case ("rte")
              cal_upd(i)%num_elem = sp_ob%chandeg
            case ("bsn")
              cal_upd(i)%num_elem = 1
            case ("pcp")
              cal_upd(i)%num_elem = db_mx%pcpfiles
            case ("tmp")
              cal_upd(i)%num_elem = db_mx%tmpfiles
            case ("gwf") !rtb - all gwflow cells
               cal_upd(i)%num_elem = ncell
            case ("gwf_riv") !rtb - all channels
               cal_upd(i)%num_elem = sp_ob%chandeg
            case ("gwf_sgl") !rtb - single value
               cal_upd(i)%num_elem = 1
            end select
            
            allocate (cal_upd(i)%num(cal_upd(i)%num_elem), source = 0)
            do ie = 1, cal_upd(i)%num_elem
                cal_upd(i)%num(ie) = ie
            end do
          else

            call define_unit_elements (nspu, ielem1)
            allocate (cal_upd(i)%num(ielem1), source = 0)
            cal_upd(i)%num = defunit_num
            cal_upd(i)%num_elem = ielem1
            deallocate (defunit_num)
            
          end if
        end do
        exit
         
      end do     
      end if
        
      db_mx%cal_upd = mcal
      
      return
      end subroutine cal_parmchg_read      