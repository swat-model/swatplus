      subroutine cli_read_atmodep
      
      use basin_module
      use input_file_module
      use climate_module
      use time_module
      use maximum_data_module
      
      implicit none
      
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      integer :: eof                  !           |end of file
      integer :: iadep                !           |counter
      integer :: imo                  !           |counter
      integer :: iyr                  !           |counter
      integer :: imo_atmo             !           |
      logical :: i_exist              !none       |check to determine if file exists
      integer :: iyrc_atmo            !           |
      
      eof = 0

      inquire (file=in_cli%atmo_cli,exist=i_exist)
      if (.not. i_exist .or. in_cli%atmo_cli == "null") then
        !!no filename 
        allocate (atmodep(0:0))
        allocate (atmo_n(0:0))
        db_mx%atmodep = 0
      else
        do
          open (127,file = in_cli%atmo_cli)
          read (127,*,iostat=eof) titldum
          if (eof < 0) exit
          read (127,*,iostat=eof) header
          if (eof < 0) exit
          read (127,*,iostat=eof) atmodep_cont%num_sta, atmodep_cont%timestep, atmodep_cont%mo_init, atmodep_cont%yr_init, &
             atmodep_cont%num
          if (eof < 0) exit
          
          ! set array pointer to first year and month
          iyrc_atmo = atmodep_cont%yr_init
          imo_atmo = atmodep_cont%mo_init
          if (atmodep_cont%timestep == "yr") then
            do iyr = 1, atmodep_cont%num
              if (iyrc_atmo == time%yrc_start) then
                atmodep_cont%ts = iyr
                atmodep_cont%first = 0
                exit
              end if
              iyrc_atmo = iyrc_atmo + 1
            end do
          end if
          
          if (atmodep_cont%timestep == "mo") then
            do imo = 1, atmodep_cont%num
              if (iyrc_atmo == time%yrc_start .and. imo_atmo == time%mo_start) then
                atmodep_cont%ts = 12 * (time%yrc_start - atmodep_cont%yr_init) + imo_atmo
                atmodep_cont%first = 0
                exit
              end if
              imo_atmo = imo_atmo + 1
              if (imo_atmo > 12) then
                imo_atmo = 1
                iyrc_atmo = iyrc_atmo + 1
              end if
            end do
          end if

          allocate (atmodep(0:atmodep_cont%num_sta))
          allocate (atmo_n(atmodep_cont%num_sta))

          do iadep = 1, atmodep_cont%num_sta
            if (atmodep_cont%timestep == "aa") then
              read (127,*,iostat=eof) atmodep(iadep)%name
              if (eof < 0) exit
              atmo_n(iadep) = atmodep(iadep)%name
              read (127,*,iostat=eof)   atmodep(iadep)%nh4_rf
              if (eof < 0) exit
              read (127,*,iostat=eof)   atmodep(iadep)%no3_rf 
              if (eof < 0) exit
              read (127,*,iostat=eof)   atmodep(iadep)%nh4_dry
              if (eof < 0) exit
              read (127,*,iostat=eof)   atmodep(iadep)%no3_dry
              if (eof < 0) exit
            end if 
          
            if (atmodep_cont%timestep == "mo") then
              allocate (atmodep(iadep)%nh4_rfmo(atmodep_cont%num))
              allocate (atmodep(iadep)%no3_rfmo(atmodep_cont%num))
              allocate (atmodep(iadep)%nh4_drymo(atmodep_cont%num))
              allocate (atmodep(iadep)%no3_drymo(atmodep_cont%num))
              read (127,*,iostat=eof) atmodep(iadep)%name
              if (eof < 0) exit
              atmo_n(iadep) = atmodep(iadep)%name
              read (127,*,iostat=eof) (atmodep(iadep)%nh4_rfmo(imo), imo = 1,atmodep_cont%num)
              if (eof < 0) exit
              read (127,*,iostat=eof) (atmodep(iadep)%no3_rfmo(imo), imo = 1,atmodep_cont%num)
              if (eof < 0) exit
              read (127,*,iostat=eof) (atmodep(iadep)%nh4_drymo(imo),imo = 1,atmodep_cont%num)
              if (eof < 0) exit
              read (127,*,iostat=eof) (atmodep(iadep)%no3_drymo(imo),imo = 1,atmodep_cont%num)
              if (eof < 0) exit
            end if
              
            if (atmodep_cont%timestep == "yr") then
              allocate (atmodep(iadep)%nh4_rfyr(atmodep_cont%num))
              allocate (atmodep(iadep)%no3_rfyr(atmodep_cont%num))
              allocate (atmodep(iadep)%nh4_dryyr(atmodep_cont%num))
              allocate (atmodep(iadep)%no3_dryyr(atmodep_cont%num))
              read (127,*,iostat=eof) atmodep(iadep)%name
              if (eof < 0) exit
              atmo_n(iadep) = atmodep(iadep)%name
              read (127,*,iostat=eof) (atmodep(iadep)%nh4_rfyr(iyr), iyr = 1,atmodep_cont%num)
              if (eof < 0) exit
              read (127,*,iostat=eof) (atmodep(iadep)%no3_rfyr(iyr), iyr = 1,atmodep_cont%num)
              if (eof < 0) exit
              read (127,*,iostat=eof) (atmodep(iadep)%nh4_dryyr(iyr),iyr = 1,atmodep_cont%num)
              if (eof < 0) exit
              read (127,*,iostat=eof) (atmodep(iadep)%no3_dryyr(iyr),iyr = 1,atmodep_cont%num)
              if (eof < 0) exit
            end if
          end do    ! iadep
          exit
        end do
      end if        ! if file exists

      db_mx%atmodep = atmodep_cont%num_sta
      
      return
      end subroutine cli_read_atmodep