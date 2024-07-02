      !the purpose of this subroutine is to read initial groundwater salt ion data
      subroutine salt_aqu_read !rtb salt
    
      use constituent_mass_module
      use input_file_module
      use maximum_data_module
      
      implicit none
 
      character (len=80) :: titldum
      character (len=80) :: header
      integer :: isalt
      integer :: eof, imax
      logical :: i_exist              !none       |check to determine if file exists

      eof = 0
      
      !read salt ion data for aquifers
      inquire (file="salt_aqu.ini", exist=i_exist)
      if (i_exist .or. "salt_aqu.ini" /= "null") then
        do
          open (107,file="salt_aqu.ini")
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          imax = 0
          do while (eof == 0)
            if (eof < 0) exit
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do
          
          !record the number of inputs
          db_mx%salt_gw_ini = imax
          
          !allocate array
          allocate(salt_aqu_ini(imax))
          
          !loop through, reading in groundwater data (concentrations, sorbed)
          do isalt=1,imax
            allocate(salt_aqu_ini(isalt)%conc(cs_db%num_salts))
            allocate(salt_aqu_ini(isalt)%frac(5))
          end do
           
          rewind (107)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit

          do isalt = 1, imax
            read (107,*,iostat=eof) salt_aqu_ini(isalt)%name,salt_aqu_ini(isalt)%conc,salt_aqu_ini(isalt)%frac
            if (eof < 0) exit
          end do
          close (107)
          exit
        end do
      end if

      return
      end subroutine salt_aqu_read