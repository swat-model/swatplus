      subroutine salt_cha_read !rtb salt
    
      use constituent_mass_module
      use input_file_module
      use maximum_data_module
      use channel_data_module
      use hydrograph_module
      use sd_channel_module
      use organic_mineral_mass_module
 
      implicit none
      
      character (len=80) :: titldum
      character (len=80) :: header
      integer :: eof, imax
      logical :: i_exist
      integer :: isalt
      integer :: isalti

      eof = 0
      
      !read all export coefficient data
      inquire (file="salt_channel.ini", exist=i_exist)
      if (i_exist .or. "salt_channel.ini" /= "null") then
        do
          open (107,file="salt_channel.ini")
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          imax = 0
          do while (eof == 0)
            read (107,*,iostat=eof) titldum   !name and concentrations
            if (eof < 0) exit
            imax = imax + 1
          end do
          
          db_mx%salt_cha_ini = imax
          
          allocate (salt_cha_ini(imax))

          do isalt=1,imax
            allocate (salt_cha_ini(isalt)%conc(cs_db%num_salts))
          enddo
          
          rewind (107)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          do isalti = 1, imax
            read (107,*,iostat=eof) salt_cha_ini(isalti)%name,salt_cha_ini(isalti)%conc
            if (eof < 0) exit
          end do
          close (107)
          exit
        end do
      end if

      return
      end subroutine salt_cha_read