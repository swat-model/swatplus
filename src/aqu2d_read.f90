      subroutine aqu2d_read
    
      use hydrograph_module
      use input_file_module
      use maximum_data_module
      
      implicit none
      
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      character (len=16) :: namedum   !           |
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      integer :: nspu                 !           |
      logical :: i_exist              !none       |check to determine if file exists
      integer :: i                    !none       |counter
      integer :: isp                  !none       |counter
      integer :: numb                 !           |
      integer :: iaq                  !none       |counter
      integer :: iaq_db               !none       |counter
      integer :: ielem1               !none       |counter

      eof = 0
      imax = 0
      
    !!read data for aquifer elements for 2-D groundwater model
      inquire (file=in_link%aqu_cha, exist=i_exist)
      if (.not. i_exist .or. in_link%aqu_cha == "null" ) then
        allocate (aq_ch(0:0))
      else 
      do
        if (eof < 0) exit
        open (107,file=in_link%aqu_cha)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        imax = 0
        do while (eof == 0)
          read (107,*,iostat=eof) i
          if (eof < 0) exit
          imax = Max(imax,i)
        end do
      end do

      db_mx%aqu2d = imax
      allocate (aq_ch(sp_ob%aqu))
      rewind (107)
      read (107,*) titldum
      read (107,*) header

      do iaq_db = 1, imax

        read (107,*,iostat=eof) iaq, namedum, nspu
        if (eof < 0) exit
        
        if (nspu > 0) then
          backspace (107)
          allocate (elem_cnt(nspu))
          read (107,*,iostat=eof) numb, aq_ch(iaq)%name, nspu, (elem_cnt(isp), isp = 1, nspu)
          if (eof < 0) exit
          
          call define_unit_elements (nspu, ielem1)
          
          allocate (aq_ch(iaq)%num(ielem1))
          aq_ch(iaq)%num = defunit_num
          aq_ch(iaq)%num_tot = ielem1
          deallocate (defunit_num)

        end if
      end do
      end if

      close (107)
      
      return
      end subroutine aqu2d_read