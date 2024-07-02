      subroutine cs_cha_read !rtb cs
    
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
      integer :: ics,i
      integer :: icsi

      eof = 0
      
      !open and read file contents
      inquire (file="cs_channel.ini", exist=i_exist)
      if (i_exist .or. "cs_channel.ini" /= "null") then
        do
          open (107,file="cs_channel.ini")
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          imax = 0
          do while (eof == 0)
            read (107,*,iostat=eof) titldum   !name
            if (eof < 0) exit
            imax = imax + 1
          end do
          
          db_mx%cs_cha_ini = imax
          
          allocate (cs_cha_ini(imax))

          do ics=1,imax
            allocate (cs_cha_ini(ics)%conc(cs_db%num_cs))
          enddo
          
          rewind (107)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          do icsi = 1, imax
            read (107,*,iostat=eof) cs_cha_ini(icsi)%name,cs_cha_ini(icsi)%conc
            if (eof < 0) exit
          end do
          close (107)
          exit
        end do
      end if

      !determine if daily channel concentrations and loads should be output
      inquire (file="cs_streamobs", exist=i_exist)
      if (cs_obs_file == 1) then
        open(107,file='cs_streamobs')
        read(107,*)
        read(107,*) cs_str_nobs
        allocate(cs_str_obs(cs_str_nobs))
        do i=1,cs_str_nobs
          read(107,*) cs_str_obs(i)
        enddo
        close(107)
        open(8200,file='cs_streamobs_output')
        write(8200,*) 'Daily concentrations and loads for specified channels'
        write(8200,*)
        write(8200,*) 'first set of columns:   flow rate (m3/sec)'
        write(8200,*) 'second set of columns:  seo4 conc (g/m3)'
        write(8200,*) 'third set of columns:   seo3 conc (g/m3)'
        write(8200,*) 'fourth set of columns:  seo4 load (kg/day)'
        write(8200,*) 'fifth set of columns:   seo3 load (kg/day)'
        write(8200,*) 'sixth set of columns:   no3n load (kg/day)'
        write(8200,*)
        write(8200,100) 'channels:',(cs_str_obs(i),i=1,cs_str_nobs)
        write(8200,*)
      endif

      
100   format(a10,100i8)   

      return
      end subroutine cs_cha_read