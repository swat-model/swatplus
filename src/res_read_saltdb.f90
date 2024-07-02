      subroutine res_read_saltdb !rtb salt
      
      use input_file_module
      use maximum_data_module
      use reservoir_data_module
      use res_salt_module
      use constituent_mass_module

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads reservoir water quality parameters for salt ions
      
      implicit none
      
      character (len=80) :: titldum     !             |title of file
      character (len=80) :: header      !             |header of file
      integer :: i                      !             |counter
      integer :: eof                    !             |end of file
      integer :: imax                   !             |determine max number for array (imax) and total number in file
      logical :: i_exist                !none         |check to determine if file exists
      integer :: ires                   !none         |counter
      integer :: isalti                 !none         |counter
      
      eof = 0
      imax = 0

      inquire (file="salt_res",exist=i_exist)
      if (.not. i_exist .or. in_res%nut_res == "null") then
        allocate (res_salt_data(0:0))
      else
      do
        open (105,file="salt_res")
        read (105,*,iostat=eof) titldum
        read (105,*,iostat=eof) titldum
        do i=1,8
          read(105,*)
        enddo
        
        if (eof < 0) exit
        read (105,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof == 0)
            read (105,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do   
          
        db_mx%res_salt = imax
        
        allocate (res_salt_data(0:imax))
        do isalti=1,imax
          allocate(res_salt_data(isalti)%c_init(cs_db%num_salts))
        end do
        rewind (105)
        read (105,*,iostat=eof) titldum
        if (eof < 0) exit
        read (105,*,iostat=eof) titldum
        do i=1,8
          read(105,*)
        enddo
        read (105,*,iostat=eof) header
        if (eof < 0) exit
          
        do ires = 1, imax
          read (105,*,iostat=eof) titldum
          if (eof < 0) exit
          backspace (105)
          read (105,*,iostat=eof) res_salt_data(ires)%name,res_salt_data(ires)%c_init
          if (eof < 0) exit
        end do
        exit
      enddo
      endif
      close(105)
      
      return
      end subroutine res_read_saltdb  