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
      use gwflow_module, only : out_gw
      
      implicit none
      
      character (len=80) :: titldum   !            |title of file
      character (len=80) :: header    !            |header
      integer :: eof                  !            |end of file
      integer :: nriv                 !            |number of gwflow river cells
      integer :: riv_id               !            |id of gwflow river cell
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
      
      !rtb: if gwflow active: adjust the number of objects; open record file
      if(bsn_cc%gwflow == 1 ) then
        !search for gwflow.rivcells file; if present, then proceed with gwflow being
        !active, and read in the number of river cells (= number of objects)
        inquire(file='gwflow.chancells',exist=i_exist)
        if(i_exist) then
          if(sp_ob%gwflow == 0) then
            open(107,file='gwflow.chancells')
            read(107,*,iostat=eof) header
            if(eof == 0) then
            read(107,*,iostat=eof)
            read(107,*,iostat=eof) header
            !determine the number of river cells in gwflow
            nriv = 0
            eof = 0
            do while (eof == 0)
              if (eof < 0) exit
              read (107,*,iostat=eof) riv_id
              if (eof < 0) exit
              nriv = nriv + 1
            end do
            !set object number for gwflow and calculate new total object number; set gwflow.con
            sp_ob%gwflow = nriv
            sp_ob%objs = sp_ob%objs + nriv - sp_ob%aqu
            in_con%gwflow_con = 'gwflow.con'
            !set aquifer to 0 and null
            sp_ob%aqu = 0
            in_con%aqu_con = 'null'
            else
              bsn_cc%gwflow = 0 !set to inactive     
            endif
          else !gwflow objects already present in object.cnt
            !just to be sure, set aquifer to 0 and null
            sp_ob%aqu = 0
            in_con%aqu_con = 'null'
          endif
        else
          bsn_cc%gwflow = 0 !set to inactive    
        endif
        !open record file
        open(out_gw,file='gwflow_record')
        write(out_gw,*) 'Record file for gwflow subroutine'
        write(out_gw,*) 
      endif
      
      
      allocate (ob(sp_ob%objs))
      allocate (obcs(sp_ob%objs))
      allocate (obcs_alloc(sp_ob%objs))
      obcs_alloc = 0
      allocate (obom(sp_ob%objs))
      return    
    end subroutine basin_read_objs
