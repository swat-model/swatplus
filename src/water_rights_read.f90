      subroutine water_rights_read
      
      use input_file_module
      use maximum_data_module
      use hydrograph_module
      use mgt_operations_module
      
      implicit none 
      
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: i                    !none       |counter
      integer :: iwro                 !none       |counter
      integer :: num_objs
      integer :: ifield
      integer :: idb
      
      eof = 0
      imax = 0
      
      !! read water rights file
      inquire (file=in_watrts%water_rights, exist=i_exist)
      if (.not. i_exist .or. in_watrts%water_rights == "null") then
        allocate (wro(0:0))
      else
      do 
        open (107,file=in_watrts%water_rights)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) imax
        if (eof < 0) exit

        allocate (wro(0:imax))

        do iwro = 1, imax
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          read (107,*,iostat=eof) wro(iwro)%name, wro(iwro)%rule_typ, wro(iwro)%right, wro(iwro)%num_objs
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          read (107,*,iostat=eof) wro(iwro)%min_mon
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          num_objs = wro(iwro)%num_objs
          allocate (wro(iwro)%field(num_objs))
          do ifield = 1, num_objs
            read (107,*,iostat=eof) wro(iwro)%field(ifield)%num, wro(iwro)%field(ifield)%ob_typ,                &
              wro(iwro)%field(ifield)%ob_num, wro(iwro)%field(ifield)%irr_typ, wro(iwro)%field(ifield)%amount,  &
              wro(iwro)%field(ifield)%rights
            
            !! xwalk irr_typ with irr.ops
            do idb = 1, db_mx%irrop_db
              if (wro(iwro)%field(ifield)%irr_typ == irrop_db(idb)%name) then
                wro(iwro)%field(ifield)%irr_no = idb
                exit
              end if
            end do
                  
          end do
        end do

        exit
      enddo
      endif
      close(107)
      
      db_mx%wro_db = imax
      
      return
      end subroutine water_rights_read