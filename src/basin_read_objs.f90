      subroutine basin_read_objs

!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     reads in the routing information from the watershed configuration
!!     input file (.fig) and calculates the number of subbasins, reaches, 
!!     and reservoirs

      use hydrograph_module
      use input_file_module
      use organic_mineral_mass_module
      use constituent_mass_module
      use basin_module
      
      implicit none
      
      character (len=80) :: titldum   !            |title of file
      character (len=80) :: header    !            |header
      integer :: npsu                 !            |
      integer :: isp                  !            |counter
      integer :: eof                  !            |end of file
      logical :: i_exist              !            |check to determine if file exists
      
      eof = 0
      
      !! read number of spatial objects from obj_connect.dat
      inquire (file=in_sim%object_cnt, exist=i_exist)
      if (.not. i_exist .or. in_sim%object_cnt == "null") then
          write (*,*) 'Cannot find object.cnt input file'
          stop
      else
      do
        open (107,file=in_sim%object_cnt)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        read (107,*,iostat=eof) bsn, sp_ob
        if (eof < 0) exit
      enddo
      endif

      close (107)
      allocate (ob(sp_ob%objs))
      allocate (obcs(sp_ob%objs))
      allocate (obom(sp_ob%objs))
      return    
      end subroutine basin_read_objs