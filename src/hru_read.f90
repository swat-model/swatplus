      subroutine hru_read

      use maximum_data_module
      use reservoir_data_module
      use landuse_data_module
      use hydrology_data_module
      use hydrograph_module, only : sp_ob
      use topography_data_module
      use soil_data_module
      use input_file_module
      use hru_module, only : hru_db, ihru, sol_plt_ini, sol_plt_ini_cs, snodb
      use constituent_mass_module
      
      implicit none
      
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: i                    !           |
      integer :: max                  !           |
      integer :: k                    !           |
      integer :: ilum                 !none       |counter
      integer :: ith                  !none       |counter 
      integer :: ithyd                !none       |counter
      integer :: isol                 !none       |counter
      integer :: isno                 !none       |counter
      integer :: ifld                 !none       |counter
      integer :: isp_ini              !none       |counter
      integer :: ics                  !none       |counter
      
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

        rewind (113)
        read (113,*,iostat=eof) titldum
        if (eof < 0) exit
        read (113,*,iostat=eof) header
        if (eof < 0) exit

      do ihru = 1, sp_ob%hru
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
                if (sol_plt_ini_cs(isp_ini)%pestc == pest_soil_ini(ics)%name) then
                  sol_plt_ini_cs(isp_ini)%pest = ics
                  exit
                end if
              end do
              ! initial pathogens
              do ics = 1, db_mx%path_ini
                if (sol_plt_ini_cs(isp_ini)%pathc == path_soil_ini(ics)%name) then
                  sol_plt_ini_cs(isp_ini)%path = ics
                  exit
                end if
              end do
              ! initial heavy metals
              do ics = 1, db_mx%hmet_ini
                if (sol_plt_ini_cs(isp_ini)%hmetc == hmet_water_ini(ics)%name) then
                  sol_plt_ini_cs(isp_ini)%hmet = ics
                  exit
                end if
              end do
              ! initial salts
              do ics = 1, db_mx%salt_ini
                if (sol_plt_ini_cs(isp_ini)%saltc == salt_soil_ini(ics)%name) then
                  sol_plt_ini_cs(isp_ini)%salt = ics
                  exit
                end if
              end do
              ! initial constituents
              do ics = 1, db_mx%cs_ini
                if (sol_plt_ini_cs(isp_ini)%csc == cs_soil_ini(ics)%name) then
                  sol_plt_ini_cs(isp_ini)%cs = ics
                  exit
                end if
              end do
              
            exit
            end if
          end do
          do ith = 1, db_mx%topo
            if (hru_db(i)%dbsc%topo == topo_db(ith)%name) then
               hru_db(i)%dbs%topo = ith
            exit
            end if
          end do
          
         if (hru_db(i)%dbs%topo == 0) write (9001,*) hru_db(i)%dbsc%topo, "not found (topography.hyd)"
        
         do ithyd = 1, db_mx%hyd
            if (hru_db(i)%dbsc%hyd == hyd_db(ithyd)%name) then
               hru_db(i)%dbs%hyd = ithyd
            exit
            end if
         end do
         
         if (hru_db(i)%dbs%hyd == 0) write (9001,*) hru_db(i)%dbsc%hyd, "not found (hydrograph.hyd)"
         
         do isol = 1, db_mx%soil
            if (hru_db(i)%dbsc%soil == soildb(isol)%s%snam) then
               hru_db(i)%dbs%soil = isol
            exit
            end if
         end do
         
         if (hru_db(i)%dbs%soil == 0) write (9001,*) hru_db(i)%dbsc%soil, "not found (soils.sol)"

         do isno = 1, db_mx%sno
            if (hru_db(i)%dbsc%snow == snodb(isno)%name) then
               hru_db(i)%dbs%snow = isno
            exit
            end if
         end do
         
         if (hru_db(i)%dbs%snow == 0 .and. hru_db(i)%dbsc%snow /= 'null') write (9001,*) hru_db(i)%dbsc%snow, "not found (snow.sno)"
         
         do ifld = 1, db_mx%field
             if (hru_db(i)%dbsc%field == field_db(ifld)%name) then
               hru_db(i)%dbs%field = ifld
            exit
            end if
         end do
         
        if (hru_db(i)%dbs%field == 0 .and. hru_db(i)%dbsc%field /= 'null') write (9001,*) &
          hru_db(i)%dbsc%field, "not found (field.fld)"

      end do
      exit
      enddo
      endif
      
      close (113)
     
      return
      end subroutine hru_read     