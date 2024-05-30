      subroutine ru_read
      
      use basin_module
      use input_file_module
      use time_module
      use ru_module
      use hydrograph_module, only : ru_d, ru_m, ru_y, ru_a, sp_ob
      use maximum_data_module
      use topography_data_module
      use constituent_mass_module
      use salt_module !rtb salt
      use cs_module !rtb cs
      
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
      integer :: isalt                !none       |salt ion counter (rtb salt)
      integer :: ics                  !none       |constituent counter (rtb cs)
      integer :: ihyd                 !none       |hydrograph counter
      
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

        !rtb salt
        if (cs_db%num_salts > 0) then
          allocate (rusaltb_d(sp_ob%ru))
          allocate (rusaltb_m(sp_ob%ru))
          allocate (rusaltb_y(sp_ob%ru))
          allocate (rusaltb_a(sp_ob%ru))
          allocate (ru_hru_saltb_d(sp_ob%ru))
          allocate (ru_hru_saltb_m(sp_ob%ru))
          allocate (ru_hru_saltb_y(sp_ob%ru))
          allocate (ru_hru_saltb_a(sp_ob%ru))
          do iru=1,sp_ob%ru
            !routing unit loadings
            allocate (rusaltb_d(iru)%hd(5))
            allocate (rusaltb_m(iru)%hd(5))
            allocate (rusaltb_y(iru)%hd(5))
            allocate (rusaltb_a(iru)%hd(5))
            do ihyd=1,5
              allocate (rusaltb_d(iru)%hd(ihyd)%salt(cs_db%num_salts))      
              allocate (rusaltb_m(iru)%hd(ihyd)%salt(cs_db%num_salts))
              allocate (rusaltb_y(iru)%hd(ihyd)%salt(cs_db%num_salts))
              allocate (rusaltb_a(iru)%hd(ihyd)%salt(cs_db%num_salts))
              rusaltb_d(iru)%hd(ihyd)%salt = 0.
              rusaltb_m(iru)%hd(ihyd)%salt = 0.
              rusaltb_y(iru)%hd(ihyd)%salt = 0.
              rusaltb_a(iru)%hd(ihyd)%salt = 0.
            enddo
            !routing unit - other salt sources and sinks
            allocate (ru_hru_saltb_d(iru)%salt(cs_db%num_salts))
            allocate (ru_hru_saltb_m(iru)%salt(cs_db%num_salts))
            allocate (ru_hru_saltb_y(iru)%salt(cs_db%num_salts))
            allocate (ru_hru_saltb_a(iru)%salt(cs_db%num_salts))
            do isalt=1,cs_db%num_salts
              ru_hru_saltb_d(iru)%salt(isalt)%wtsp = 0.
              ru_hru_saltb_d(iru)%salt(isalt)%irsw = 0.
              ru_hru_saltb_d(iru)%salt(isalt)%irgw = 0.
              ru_hru_saltb_d(iru)%salt(isalt)%irwo = 0.
              ru_hru_saltb_d(iru)%salt(isalt)%rain = 0.
              ru_hru_saltb_d(iru)%salt(isalt)%dryd = 0.
              ru_hru_saltb_d(iru)%salt(isalt)%road = 0.
              ru_hru_saltb_d(iru)%salt(isalt)%fert = 0.
              ru_hru_saltb_d(iru)%salt(isalt)%amnd = 0.
              ru_hru_saltb_d(iru)%salt(isalt)%uptk = 0.
              ru_hru_saltb_m(iru)%salt(isalt)%wtsp = 0.
              ru_hru_saltb_m(iru)%salt(isalt)%irsw = 0.
              ru_hru_saltb_m(iru)%salt(isalt)%irgw = 0.
              ru_hru_saltb_m(iru)%salt(isalt)%irwo = 0.
              ru_hru_saltb_m(iru)%salt(isalt)%rain = 0.
              ru_hru_saltb_m(iru)%salt(isalt)%dryd = 0.
              ru_hru_saltb_m(iru)%salt(isalt)%road = 0.
              ru_hru_saltb_m(iru)%salt(isalt)%fert = 0.
              ru_hru_saltb_m(iru)%salt(isalt)%amnd = 0.
              ru_hru_saltb_m(iru)%salt(isalt)%uptk = 0.
              ru_hru_saltb_y(iru)%salt(isalt)%wtsp = 0.
              ru_hru_saltb_y(iru)%salt(isalt)%irsw = 0.
              ru_hru_saltb_y(iru)%salt(isalt)%irgw = 0.
              ru_hru_saltb_y(iru)%salt(isalt)%irwo = 0.
              ru_hru_saltb_y(iru)%salt(isalt)%rain = 0.
              ru_hru_saltb_y(iru)%salt(isalt)%dryd = 0.
              ru_hru_saltb_y(iru)%salt(isalt)%road = 0.
              ru_hru_saltb_y(iru)%salt(isalt)%fert = 0.
              ru_hru_saltb_y(iru)%salt(isalt)%amnd = 0.
              ru_hru_saltb_y(iru)%salt(isalt)%uptk = 0.
              ru_hru_saltb_a(iru)%salt(isalt)%wtsp = 0.
              ru_hru_saltb_a(iru)%salt(isalt)%irsw = 0.
              ru_hru_saltb_a(iru)%salt(isalt)%irgw = 0.
              ru_hru_saltb_a(iru)%salt(isalt)%irwo = 0.
              ru_hru_saltb_a(iru)%salt(isalt)%rain = 0.
              ru_hru_saltb_a(iru)%salt(isalt)%dryd = 0.
              ru_hru_saltb_a(iru)%salt(isalt)%road = 0.
              ru_hru_saltb_a(iru)%salt(isalt)%fert = 0.
              ru_hru_saltb_a(iru)%salt(isalt)%amnd = 0.
              ru_hru_saltb_a(iru)%salt(isalt)%uptk = 0.
            enddo
            ru_hru_saltb_m(iru)%salt(1)%diss = 0.
            ru_hru_saltb_y(iru)%salt(1)%diss = 0.
            ru_hru_saltb_a(iru)%salt(1)%diss = 0.
          enddo  
        endif !rtb salt
        
        !rtb cs
        if (cs_db%num_cs > 0) then
          allocate (rucsb_d(sp_ob%ru))
          allocate (rucsb_m(sp_ob%ru))
          allocate (rucsb_y(sp_ob%ru))
          allocate (rucsb_a(sp_ob%ru))
          allocate (ru_hru_csb_d(sp_ob%ru))
          allocate (ru_hru_csb_m(sp_ob%ru))
          allocate (ru_hru_csb_y(sp_ob%ru))
          allocate (ru_hru_csb_a(sp_ob%ru))
          do iru=1,sp_ob%ru
            !routing unit loadings
            allocate (rucsb_d(iru)%hd(5))
            allocate (rucsb_m(iru)%hd(5))
            allocate (rucsb_y(iru)%hd(5))
            allocate (rucsb_a(iru)%hd(5))
            do ihyd=1,5
              allocate (rucsb_d(iru)%hd(ihyd)%cs(cs_db%num_cs))      
              allocate (rucsb_m(iru)%hd(ihyd)%cs(cs_db%num_cs))
              allocate (rucsb_y(iru)%hd(ihyd)%cs(cs_db%num_cs))
              allocate (rucsb_a(iru)%hd(ihyd)%cs(cs_db%num_cs))
              rucsb_d(iru)%hd(ihyd)%cs = 0.
              rucsb_m(iru)%hd(ihyd)%cs = 0.
              rucsb_y(iru)%hd(ihyd)%cs = 0.
              rucsb_a(iru)%hd(ihyd)%cs = 0.
            enddo
            !routing unit - other constituent sources and sinks
            allocate (ru_hru_csb_d(iru)%cs(cs_db%num_cs))
            allocate (ru_hru_csb_m(iru)%cs(cs_db%num_cs))
            allocate (ru_hru_csb_y(iru)%cs(cs_db%num_cs))
            allocate (ru_hru_csb_a(iru)%cs(cs_db%num_cs))
            do ics=1,cs_db%num_cs
              ru_hru_csb_d(iru)%cs(ics)%sedm = 0.
              ru_hru_csb_d(iru)%cs(ics)%wtsp = 0.
              ru_hru_csb_d(iru)%cs(ics)%irsw = 0.
              ru_hru_csb_d(iru)%cs(ics)%irgw = 0.
              ru_hru_csb_d(iru)%cs(ics)%irwo = 0.
              ru_hru_csb_d(iru)%cs(ics)%rain = 0.
              ru_hru_csb_d(iru)%cs(ics)%dryd = 0.
              ru_hru_csb_d(iru)%cs(ics)%fert = 0.
              ru_hru_csb_d(iru)%cs(ics)%uptk = 0.
              ru_hru_csb_d(iru)%cs(ics)%rctn = 0.
              ru_hru_csb_d(iru)%cs(ics)%sorb = 0.
              ru_hru_csb_m(iru)%cs(ics)%sedm = 0.
              ru_hru_csb_m(iru)%cs(ics)%wtsp = 0.
              ru_hru_csb_m(iru)%cs(ics)%irsw = 0.
              ru_hru_csb_m(iru)%cs(ics)%irgw = 0.
              ru_hru_csb_m(iru)%cs(ics)%irwo = 0.
              ru_hru_csb_m(iru)%cs(ics)%rain = 0.
              ru_hru_csb_m(iru)%cs(ics)%dryd = 0.
              ru_hru_csb_m(iru)%cs(ics)%fert = 0.
              ru_hru_csb_m(iru)%cs(ics)%uptk = 0.
              ru_hru_csb_m(iru)%cs(ics)%rctn = 0.
              ru_hru_csb_m(iru)%cs(ics)%sorb = 0.
              ru_hru_csb_y(iru)%cs(ics)%sedm = 0.
              ru_hru_csb_y(iru)%cs(ics)%wtsp = 0.
              ru_hru_csb_y(iru)%cs(ics)%irsw = 0.
              ru_hru_csb_y(iru)%cs(ics)%irgw = 0.
              ru_hru_csb_y(iru)%cs(ics)%irwo = 0.
              ru_hru_csb_y(iru)%cs(ics)%rain = 0.
              ru_hru_csb_y(iru)%cs(ics)%dryd = 0.
              ru_hru_csb_y(iru)%cs(ics)%fert = 0.
              ru_hru_csb_y(iru)%cs(ics)%uptk = 0.
              ru_hru_csb_y(iru)%cs(ics)%rctn = 0.
              ru_hru_csb_y(iru)%cs(ics)%sorb = 0.
              ru_hru_csb_a(iru)%cs(ics)%sedm = 0.
              ru_hru_csb_a(iru)%cs(ics)%wtsp = 0.
              ru_hru_csb_a(iru)%cs(ics)%irsw = 0.
              ru_hru_csb_a(iru)%cs(ics)%irgw = 0.
              ru_hru_csb_a(iru)%cs(ics)%irwo = 0.
              ru_hru_csb_a(iru)%cs(ics)%rain = 0.
              ru_hru_csb_a(iru)%cs(ics)%dryd = 0.
              ru_hru_csb_a(iru)%cs(ics)%fert = 0.
              ru_hru_csb_a(iru)%cs(ics)%uptk = 0.
              ru_hru_csb_a(iru)%cs(ics)%rctn = 0.
              ru_hru_csb_a(iru)%cs(ics)%sorb = 0.
            enddo
          enddo  
        endif !rtb cs
        
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