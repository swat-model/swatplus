      subroutine ru_read
      
      use basin_module
      use input_file_module
      use time_module
      use ru_module
      use hydrograph_module, only : ru_d, ru_m, ru_y, ru_a, sp_ob
      use maximum_data_module
      use topography_data_module
      
      implicit none
      
      ! read subbasin parameters (ie drainage area and topographic inputs)
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: i                    !           |
      integer :: max                  !           |
      integer :: k                    !           |
      integer :: ith                  !none       |counter
      
      mru_db = 0
      eof = 0
      imax = 0
      
      inquire (file=in_ru%ru, exist=i_exist)
      if (.not. i_exist .or. in_ru%ru == "null") then
          allocate (ru(0:0))
      else
      do
        open (107,file=in_ru%ru)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof == 0)
            read (107,*,iostat=eof) i
            if (eof < 0 .or. i == 0) exit
            imax = Max(imax,i)
            mru_db = mru_db + 1
          end do
          
        allocate (ru(0:sp_ob%ru))
        allocate (ru_d(sp_ob%ru))
        allocate (ru_m(sp_ob%ru))
        allocate (ru_y(sp_ob%ru))
        allocate (ru_a(sp_ob%ru))
        allocate (ru_tc(0:sp_ob%ru))
        allocate (ru_n(0:sp_ob%ru))
        allocate (itsb(sp_ob%ru))

        rewind (107)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit

        !! read subbasin parameters
        do iru = 1, mru_db
          read (107,*,iostat=eof) i
          if (eof < 0) exit
          backspace (107)
          read (107,*,iostat=eof) k, ru(i)%name, ru(i)%dbsc
          if (eof < 0) exit

          do ith = 1, db_mx%topo
            if (ru(i)%dbsc%toposub_db == topo_db(ith)%name) then
              ru(i)%dbs%toposub_db = ith
              exit
            end if
            ! if (ru(i)%dbs%toposub_db == 0) write (9001,*) ru(i)%dbsc%toposub_db, " not found (ru-toposub)" 
          end do
      
          do ith = 1, db_mx%field
            if (ru(i)%dbsc%field_db == field_db(ith)%name) then
              ru(i)%dbs%field_db = ith
              ! set field data
              ru(i)%field%length = field_db(ith)%length
              ru(i)%field%wid = field_db(ith)%wid
              ru(i)%field%ang = field_db(ith)%ang
              exit
            end if
            ! if (ru(i)%dbs%field_db == 0) write (9001,*) ru(i)%dbsc%field_db, " not found (ru-field_db)"
          end do
        end do      ! iru = 1, mru_db

      
      close(107)
      exit
      end do
      end if      

      return
      end subroutine ru_read