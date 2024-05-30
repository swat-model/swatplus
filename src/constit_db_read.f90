      subroutine constit_db_read
      
      use basin_module
      use input_file_module
      use constituent_mass_module
      use maximum_data_module
      use pesticide_data_module
      use pathogen_data_module

      implicit none
         
      character (len=80) :: titldum   !           |title of file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: eof                  !           |end of file
      integer :: i                    !           |
      integer :: imax                 !           |
      integer :: ipest                !none       |counter
      integer :: ipestdb              !none       |counter
      integer :: ipath                !none       |counter
      integer :: ipathdb              !none       |counter
       
      eof = 0
      imax = 0
      
      inquire (file=in_sim%cs_db, exist=i_exist)
      if (.not. i_exist .or. in_sim%cs_db == "null") then
        allocate (cs_db%pests(0:0))
        allocate (cs_db%paths(0:0))
        allocate (cs_db%metals(0:0))
        allocate (cs_db%salts(0:0))
      else
      do
        open (106,file=in_sim%cs_db)
        read (106,*,iostat=eof) titldum
        if (eof < 0) exit
        read (106,*,iostat=eof) cs_db%num_pests
        if (eof < 0) exit
        allocate (cs_db%pests(0:cs_db%num_pests))
        allocate (cs_db%pest_num(0:cs_db%num_pests))
        read (106,*,iostat=eof) (cs_db%pests(i), i = 1, cs_db%num_pests)
        if (eof < 0) exit
        read (106,*,iostat=eof) cs_db%num_paths
        if (eof < 0) exit
        allocate (cs_db%paths(cs_db%num_paths))
        allocate (cs_db%path_num(0:cs_db%num_paths))
        read (106,*,iostat=eof) (cs_db%paths(i), i = 1, cs_db%num_paths)
        if (eof < 0) exit
        read (106,*,iostat=eof) cs_db%num_metals
        if (eof < 0) exit
        allocate (cs_db%metals(cs_db%num_metals))
        allocate (cs_db%metals_num(0:cs_db%num_metals))
        read (106,*,iostat=eof) (cs_db%metals(i), i = 1, cs_db%num_metals)
        if (eof < 0) exit
        !salt ions
        read (106,*,iostat=eof) cs_db%num_salts
        if (eof < 0) exit
        allocate (cs_db%salts(cs_db%num_salts))
        allocate (cs_db%salts_num(0:cs_db%num_salts))
        read (106,*,iostat=eof) (cs_db%salts(i), i = 1, cs_db%num_salts)
        !other constituents
        read (106,*,iostat=eof) cs_db%num_cs
        if (eof < 0) exit
        allocate (cs_db%cs(cs_db%num_cs))
        allocate (cs_db%cs_num (0:cs_db%num_cs))
        read (106,*,iostat=eof) (cs_db%cs(i), i = 1, cs_db%num_cs)
        exit
      end do
      end if

      do ipest = 1, cs_db%num_pests
        do ipestdb = 1, db_mx%pestparm
          if (pestdb(ipestdb)%name == cs_db%pests(ipest)) then
            cs_db%pest_num(ipest) = ipestdb
            exit
          end if
        end do
      end do  
      
      do ipath = 1, cs_db%num_paths
        do ipathdb = 1, db_mx%path
          if (path_db(ipathdb)%pathnm == cs_db%paths(ipath)) then
            cs_db%path_num(ipath) = ipathdb
            exit
          end if
        end do
      end do
          
!      do ihmet = 1, cs_db%num_hmets
!        do ihmetdb = 1, db_mx%pestparm
!          if (hmetdb(ihmetdb)%pestnm == cs_db%hmets(ihmet)) then
!            cs_db%hmet_num(ihmet) = ihmetdb
!            exit
!          end if
!        end do
!      end do  
      
!      do isalt = 1, cs_db%num_salts
!        do isaltdb = 1, db_mx%salt
!          if (path_db(isaltdb)%saltnm == cs_db%salts(isalt)) then
!            cs_db%salt_num(isalt) = isaltdb
!            exit
!          end if
!        end do
!      end do
          
      !sum up the number of pesticides, pathogens, metals, salt ions, and other constituents
      cs_db%num_tot = cs_db%num_pests + cs_db%num_paths + cs_db%num_metals + cs_db%num_salts + cs_db%num_cs !rtb salt, cs
      
      
      close (106)
      return
      end subroutine constit_db_read