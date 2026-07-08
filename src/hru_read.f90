      subroutine hru_read

      use maximum_data_module
      use reservoir_data_module
      use landuse_data_module
      use hydrology_data_module
      use topography_data_module
      use soil_data_module
      use input_file_module
      use hru_module, only : hru_db, ihru, sol_plt_ini, snodb
      use constituent_mass_module
      use name_index_module

      implicit none

      external :: allocate_parms

      ! name->index hash maps so per-HRU database lookups are O(1) instead of an
      ! O(N_hru x N_db) linear scan (the per-HRU topography/hydrology matching on
      ! large models was a top for_cpstr_eq startup hotspot).
      type (name_index) :: ix_topo, ix_hyd, ix_soil, ix_field
      character (len=40), allocatable :: nm(:)
      integer :: jj = 0               !none       |index-build counter

      character (len=80) :: titldum = ""!           |title of file
      character (len=80) :: header = "" !           |header of file
      integer :: eof = 0              !           |end of file
      integer :: imax = 0             !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: i = 0                !           |
      integer :: max                  !           |
      integer :: k = 0                !           |
      integer :: ilum = 0             !none       |counter
      integer :: ith = 0              !none       |counter 
      integer :: ithyd = 0            !none       |counter
      integer :: isol = 0             !none       |counter
      integer :: isno = 0             !none       |counter
      integer :: ifld = 0             !none       |counter
      integer :: isp_ini = 0          !none       |counter
      integer :: ics = 0              !none       |counter
      
      eof = 0
      imax = 0
      
      call allocate_parms

      inquire (file=in_hru%hru_data, exist=i_exist)
      if (.not. i_exist .or. in_hru%hru_data == "null") then
        allocate (hru_db(0:0))
      else 
      do
        open (113,file=in_hru%hru_data)
        read (113,*,iostat=eof) titldum
        if (eof < 0) exit
        read (113,*,iostat=eof) header
        if (eof < 0) exit
         do while (eof == 0)
            read (113,*,iostat=eof) i
            if (eof < 0) exit
            imax = Max(imax,i)
          end do
          
        allocate (hru_db(0:imax))

        ! build name->index maps once for the databases with many entries
        ! (topography and hydrology are per-HRU and dominate the O(N^2) scan)
        if (db_mx%topo > 0) then
          allocate (nm(db_mx%topo))
          do jj = 1, db_mx%topo; nm(jj) = topo_db(jj)%name; end do
          call nix_build (ix_topo, nm, db_mx%topo); deallocate (nm)
        end if
        if (db_mx%hyd > 0) then
          allocate (nm(db_mx%hyd))
          do jj = 1, db_mx%hyd; nm(jj) = hyd_db(jj)%name; end do
          call nix_build (ix_hyd, nm, db_mx%hyd); deallocate (nm)
        end if
        if (db_mx%soil > 0) then
          allocate (nm(db_mx%soil))
          do jj = 1, db_mx%soil; nm(jj) = soildb(jj)%s%snam; end do
          call nix_build (ix_soil, nm, db_mx%soil); deallocate (nm)
        end if
        if (db_mx%field > 0) then
          allocate (nm(db_mx%field))
          do jj = 1, db_mx%field; nm(jj) = field_db(jj)%name; end do
          call nix_build (ix_field, nm, db_mx%field); deallocate (nm)
        end if

        rewind (113)
        read (113,*,iostat=eof) titldum
        if (eof < 0) exit
        read (113,*,iostat=eof) header
        if (eof < 0) exit

      do ihru = 1, imax
        read (113,*,iostat=eof) i
        if (eof < 0) exit
        backspace (113)
        read (113,*,iostat=eof) k, hru_db(i)%dbsc
        if (eof < 0) exit

         do ilum = 1, db_mx%landuse
            if (hru_db(i)%dbsc%land_use_mgt == lum(ilum)%name) then
               hru_db(i)%dbs%land_use_mgt = ilum
            exit
            end if
          end do
          
         if (hru_db(i)%dbs%land_use_mgt == 0) write (9001,*) hru_db(i)%dbsc%land_use_mgt, "not found (landuse.lum)"
          
          ! initialize nutrients and constituents in soil and plants
          do isp_ini = 1, db_mx%sol_plt_ini
            if (hru_db(i)%dbsc%soil_plant_init == sol_plt_ini(isp_ini)%name) then
              hru_db(i)%dbs%soil_plant_init = isp_ini
              
            if (hru_db(i)%dbs%soil_plant_init == 0) write (9001,*) hru_db(i)%dbsc%soil_plant_init, "not found (plant.ini)" 
              
              ! initial soil nutrients (soil test)
              do ics = 1, db_mx%soiltest
                if (sol_plt_ini(isp_ini)%nutc == solt_db(ics)%name) then
                  sol_plt_ini(isp_ini)%nut = ics
                  exit
                end if
              end do
              ! initial pesticides
              do ics = 1, db_mx%pest_ini
                if (sol_plt_ini(isp_ini)%pestc == pest_soil_ini(ics)%name) then
                  sol_plt_ini(isp_ini)%pest = ics
                  exit
                end if
              end do
              ! initial pathogens
              do ics = 1, db_mx%path_ini
                if (sol_plt_ini(isp_ini)%pathc == path_soil_ini(ics)%name) then
                  sol_plt_ini(isp_ini)%path = ics
                  exit
                end if
              end do
              ! initial heavy metals
              do ics = 1, db_mx%hmet_ini
                if (sol_plt_ini(isp_ini)%hmetc == hmet_soil_ini(ics)%name) then
                  sol_plt_ini(isp_ini)%hmet = ics
                  exit
                end if
              end do
              ! initial salts
              do ics = 1, db_mx%salt_ini
                if (sol_plt_ini(isp_ini)%saltc == salt_soil_ini(ics)%name) then
                  sol_plt_ini(isp_ini)%salt = ics
                  exit
                end if
              end do
              ! initial constituents
              do ics = 1, db_mx%cs_ini
                if (sol_plt_ini(isp_ini)%csc == cs_soil_ini(ics)%name) then
                  sol_plt_ini(isp_ini)%cs = ics
                  exit
                end if
              end do
              
            exit
            end if
          end do
          ith = nix_get (ix_topo, hru_db(i)%dbsc%topo)
          if (ith > 0) hru_db(i)%dbs%topo = ith

         if (hru_db(i)%dbs%topo == 0) write (9001,*) hru_db(i)%dbsc%topo, "not found (topography.hyd)"

         ithyd = nix_get (ix_hyd, hru_db(i)%dbsc%hyd)
         if (ithyd > 0) hru_db(i)%dbs%hyd = ithyd

         if (hru_db(i)%dbs%hyd == 0) write (9001,*) hru_db(i)%dbsc%hyd, "not found (hydrograph.hyd)"

         isol = nix_get (ix_soil, hru_db(i)%dbsc%soil)
         if (isol > 0) hru_db(i)%dbs%soil = isol

         if (hru_db(i)%dbs%soil == 0) write (9001,*) hru_db(i)%dbsc%soil, "not found (soils.sol)"

         do isno = 1, db_mx%sno
            if (hru_db(i)%dbsc%snow == snodb(isno)%name) then
               hru_db(i)%dbs%snow = isno
            exit
            end if
         end do
         
         if (hru_db(i)%dbs%snow == 0 .and. hru_db(i)%dbsc%snow /= 'null') write (9001,*) hru_db(i)%dbsc%snow, "not found (snow.sno)"
         
         ifld = nix_get (ix_field, hru_db(i)%dbsc%field)
         if (ifld > 0) hru_db(i)%dbs%field = ifld

        if (hru_db(i)%dbs%field == 0 .and. hru_db(i)%dbsc%field /= 'null') write (9001,*) &
          hru_db(i)%dbsc%field, "not found (field.fld)"

      end do
      exit
      enddo
      endif
      
      close (113)
     
      return
      end subroutine hru_read     