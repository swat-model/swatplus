      subroutine res_read_salt_cs
      
      use maximum_data_module
      use reservoir_data_module
      use constituent_mass_module
      use reservoir_module
      use res_salt_module
      use res_cs_module
      
      implicit none

      integer :: i = 0
      
      character (len=80) :: header = "" !           |header of file
      integer :: eof = 0              !           |end of file
      integer :: imax = 0             !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: ires = 0             !none       |counter 
      integer :: k = 0                !           |
      integer :: isalt = 0            !none      |counter
      integer :: ics = 0              !none      |counter
      
      eof = 0
      imax = 0
            
      !read reservoir.res_cs
      inquire (file="reservoir.res_cs",exist=i_exist)
      if(i_exist) then
      
        !open file
        open(105,file="reservoir.res_cs")
        read(105,*) header
        read(105,*) header
        
        !allocate array
        allocate (res_dat_c_cs(db_mx%res_dat))

        !loop through the reservoir entries
        do i = 1, db_mx%res_dat
          read (105,*,iostat=eof) ires
          if (eof < 0) exit
          backspace (105)
          read (105,*,iostat=eof) k, res_dat_c_cs(ires)
          if (eof < 0) exit
          
          !rtb salt
          do isalt = 1, db_mx%res_salt
            if (res_salt_data(isalt)%name == res_dat_c_cs(ires)%salt) then
              res_dat(ires)%salt = isalt
              exit
            endif
          enddo 
         
          !rtb cs
          do ics = 1, db_mx%res_cs
            if (res_cs_data(ics)%name == res_dat_c_cs(ires)%cs) then
              res_dat(ires)%cs = ics
              exit
            endif
          enddo 
          
        enddo !go to next reservoir entry
         
       close(105)
      endif
       
      return
      end subroutine res_read_salt_cs     