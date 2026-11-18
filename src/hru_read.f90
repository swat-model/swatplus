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
      
      implicit none
      
      external :: allocate_parms
      
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
      integer, parameter :: i64 = selected_int_kind(15) !none |64-bit integer kind for hash
      integer, parameter :: ht_min_slots = 16           !none |minimum hash table slot count
      integer(i64), parameter :: ht_mult = 131_i64      !none |polynomial hash multiplier
      integer(i64), parameter :: ht_mod = 2147483647_i64 !none |Mersenne prime modulus (2^31-1)
      integer(i64), parameter :: ht_mix_a = 1103515245_i64 !none |finalizer multiplier
      integer(i64), parameter :: ht_mix_b = 12345_i64   !none |finalizer increment
      integer :: h_slot               !none       |hash probe slot index
      integer, dimension(:), allocatable :: lum_ht  !none |hash table for landuse.lum
      integer, dimension(:), allocatable :: sp_ht   !none |hash table for plant.ini
      integer, dimension(:), allocatable :: topo_ht !none |hash table for topography.hyd
      integer, dimension(:), allocatable :: hyd_ht  !none |hash table for hydrology.hyd
      integer, dimension(:), allocatable :: soil_ht !none |hash table for soils.sol
      
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

      !! Build open-addressing hash tables for the 5 large cross-referenced databases (cf. PR #219)
      call ht_init (db_mx%landuse, lum_ht)
      do ilum = 1, db_mx%landuse
        call ht_insert (lum(ilum)%name, ilum, lum_ht)
      end do
      call ht_init (db_mx%sol_plt_ini, sp_ht)
      do isp_ini = 1, db_mx%sol_plt_ini
        call ht_insert (sol_plt_ini(isp_ini)%name, isp_ini, sp_ht)
      end do
      call ht_init (db_mx%topo, topo_ht)
      do ith = 1, db_mx%topo
        call ht_insert (topo_db(ith)%name, ith, topo_ht)
      end do
      call ht_init (db_mx%hyd, hyd_ht)
      do ithyd = 1, db_mx%hyd
        call ht_insert (hyd_db(ithyd)%name, ithyd, hyd_ht)
      end do
      call ht_init (db_mx%soil, soil_ht)
      do isol = 1, db_mx%soil
        call ht_insert (soildb(isol)%s%snam, isol, soil_ht)
      end do

      do ihru = 1, imax
        read (113,*,iostat=eof) i
        if (eof < 0) exit
        backspace (113)
        read (113,*,iostat=eof) k, hru_db(i)%dbsc
        if (eof < 0) exit

         h_slot = ht_slot(hru_db(i)%dbsc%land_use_mgt, size(lum_ht))
         do while (lum_ht(h_slot) /= 0)
           ilum = lum_ht(h_slot)
           if (hru_db(i)%dbsc%land_use_mgt == lum(ilum)%name) then
             hru_db(i)%dbs%land_use_mgt = ilum
             exit
           end if
           h_slot = h_slot + 1
           if (h_slot > size(lum_ht)) h_slot = 1
         end do
          
         if (hru_db(i)%dbs%land_use_mgt == 0) write (9001,*) hru_db(i)%dbsc%land_use_mgt, "not found (landuse.lum)"
          
          ! initialize nutrients and constituents in soil and plants
          h_slot = ht_slot(hru_db(i)%dbsc%soil_plant_init, size(sp_ht))
          do while (sp_ht(h_slot) /= 0)
            isp_ini = sp_ht(h_slot)
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
            h_slot = h_slot + 1
            if (h_slot > size(sp_ht)) h_slot = 1
          end do
          h_slot = ht_slot(hru_db(i)%dbsc%topo, size(topo_ht))
          do while (topo_ht(h_slot) /= 0)
            ith = topo_ht(h_slot)
            if (hru_db(i)%dbsc%topo == topo_db(ith)%name) then
              hru_db(i)%dbs%topo = ith
              exit
            end if
            h_slot = h_slot + 1
            if (h_slot > size(topo_ht)) h_slot = 1
          end do
          
         if (hru_db(i)%dbs%topo == 0) write (9001,*) hru_db(i)%dbsc%topo, "not found (topography.hyd)"
        
         h_slot = ht_slot(hru_db(i)%dbsc%hyd, size(hyd_ht))
         do while (hyd_ht(h_slot) /= 0)
           ithyd = hyd_ht(h_slot)
           if (hru_db(i)%dbsc%hyd == hyd_db(ithyd)%name) then
             hru_db(i)%dbs%hyd = ithyd
             exit
           end if
           h_slot = h_slot + 1
           if (h_slot > size(hyd_ht)) h_slot = 1
         end do
         
         if (hru_db(i)%dbs%hyd == 0) write (9001,*) hru_db(i)%dbsc%hyd, "not found (hydrograph.hyd)"
         
         h_slot = ht_slot(hru_db(i)%dbsc%soil, size(soil_ht))
         do while (soil_ht(h_slot) /= 0)
           isol = soil_ht(h_slot)
           if (hru_db(i)%dbsc%soil == soildb(isol)%s%snam) then
             hru_db(i)%dbs%soil = isol
             exit
           end if
           h_slot = h_slot + 1
           if (h_slot > size(soil_ht)) h_slot = 1
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
      if (allocated(lum_ht)) deallocate (lum_ht, sp_ht, topo_ht, hyd_ht, soil_ht)

      return

      contains

      !! Allocate power-of-two open-addressing hash table with load factor < 0.5
      subroutine ht_init (n, ht)
        integer, intent(in) :: n                        ! number of rows to index
        integer, dimension(:), allocatable, intent(out) :: ht ! zero-initialized slot table
        integer :: nslots                               ! chosen power-of-two slot count
        nslots = ht_min_slots
        do while (nslots < 2 * max(n, 0) + 1)
          nslots = nslots * 2
        end do
        allocate (ht(nslots), source = 0)
      end subroutine ht_init

      !! Compute 1-based home slot for key (trailing blanks ignored to match Fortran ==)
      pure integer function ht_slot (key, nslots)
        character(len=*), intent(in) :: key             ! database name key
        integer, intent(in) :: nslots                   ! slot table size
        integer(i64) :: h                               ! 64-bit hash accumulator
        integer :: ic                                   ! character index
        h = 0_i64
        do ic = 1, len_trim(key)
          h = modulo(h * ht_mult + int(modulo(iachar(key(ic:ic)), 256), i64), ht_mod)
        end do
        h = modulo(h * ht_mix_a + ht_mix_b, ht_mod)
        ht_slot = int(modulo(h, int(nslots, i64))) + 1
      end function ht_slot

      !! Insert 1-based row index in ascending order so linear probing preserves first-match
      subroutine ht_insert (key, idx, ht)
        character(len=*), intent(in) :: key             ! database name key
        integer, intent(in) :: idx                      ! 1-based database row index
        integer, dimension(:), intent(inout) :: ht      ! slot table
        integer :: s                                    ! probe slot
        s = ht_slot(key, size(ht))
        do while (ht(s) /= 0)
          s = s + 1
          if (s > size(ht)) s = 1
        end do
        ht(s) = idx
      end subroutine ht_insert

      end subroutine hru_read