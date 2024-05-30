      !the purpose of this subroutine is to read initial groundwater constituent data
      subroutine cs_aqu_read !rtb cs
    
      use constituent_mass_module
      use input_file_module
      use maximum_data_module
      
      implicit none
 
      character (len=80) :: titldum
      character (len=80) :: header
      integer :: ics
      integer :: eof, imax
      logical :: i_exist              !none       |check to determine if file exists

      eof = 0
      
      !read cs data for aquifers
      inquire (file="cs_aqu.ini", exist=i_exist)
      if (i_exist .or. "cs_aqu.ini" /= "null") then
        do
          open (107,file="cs_aqu.ini")
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          imax = 0
          do while (eof == 0)
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do
          
          db_mx%cs_ini = imax
          
          allocate (cs_aqu_ini(imax))
          
          !loop through, reading in groundwater data (concentrations, sorbed)
          do ics = 1, imax
            allocate (cs_aqu_ini(ics)%aqu(cs_db%num_cs + cs_db%num_cs))
          end do
           
          rewind (107)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit

          do ics = 1, imax
            read (107,*,iostat=eof) cs_aqu_ini(ics)%name,cs_aqu_ini(ics)%aqu
            if (eof < 0) exit
          end do
          close (107)
          exit
        end do
      end if
      

      return
      end subroutine cs_aqu_read