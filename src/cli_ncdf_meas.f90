subroutine cli_ncdf_meas
    
    ! This subroutine reads NetCDF climate data for SWAT+ simulation
    ! Using NetCDF C interface instead of Fortran interface

    use climate_module
    use maximum_data_module
    use time_module
    use input_file_module
    use iso_c_binding
    
    implicit none
    
    ! NetCDF C constants
    integer(c_int), parameter :: NC_NOERR = 0
    integer(c_int), parameter :: NC_NOWRITE = 0
    integer(c_int), parameter :: NC_FLOAT = 5
    integer(c_int), parameter :: NC_CHAR = 2
    integer(c_int), parameter :: NC_MAX_NAME = 256
    integer(c_int), parameter :: NC_MAX_VAR_DIMS = 32
    
    ! NetCDF C function interfaces
    interface
        function nc_open_c(path, mode, ncidp) bind(c, name='nc_open')
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), value, intent(in) :: mode
            integer(c_int), intent(out) :: ncidp
            integer(c_int) :: nc_open_c
        end function
        
        function nc_close_c(ncid) bind(c, name='nc_close')
            import :: c_int
            integer(c_int), value, intent(in) :: ncid
            integer(c_int) :: nc_close_c
        end function
        
        function nc_inq_dimid_c(ncid, name, dimidp) bind(c, name='nc_inq_dimid')
            import :: c_int, c_char
            integer(c_int), value, intent(in) :: ncid
            character(kind=c_char), intent(in) :: name(*)
            integer(c_int), intent(out) :: dimidp
            integer(c_int) :: nc_inq_dimid_c
        end function
        
        function nc_inq_dimlen_c(ncid, dimid, lenp) bind(c, name='nc_inq_dimlen')
            import :: c_int, c_size_t
            integer(c_int), value, intent(in) :: ncid
            integer(c_int), value, intent(in) :: dimid
            integer(c_size_t), intent(out) :: lenp
            integer(c_int) :: nc_inq_dimlen_c
        end function
        
        function nc_inq_varid_c(ncid, name, varidp) bind(c, name='nc_inq_varid')
            import :: c_int, c_char
            integer(c_int), value, intent(in) :: ncid
            character(kind=c_char), intent(in) :: name(*)
            integer(c_int), intent(out) :: varidp
            integer(c_int) :: nc_inq_varid_c
        end function
        
        function nc_get_var_float_c(ncid, varid, ip) bind(c, name='nc_get_var_float')
            import :: c_int, c_float
            integer(c_int), value, intent(in) :: ncid
            integer(c_int), value, intent(in) :: varid
            real(c_float), intent(out) :: ip(*)
            integer(c_int) :: nc_get_var_float_c
        end function
        
        function nc_get_att_text_c(ncid, varid, name, ip) bind(c, name='nc_get_att_text')
            import :: c_int, c_char
            integer(c_int), value, intent(in) :: ncid
            integer(c_int), value, intent(in) :: varid
            character(kind=c_char), intent(in) :: name(*)
            character(kind=c_char), intent(out) :: ip(*)
            integer(c_int) :: nc_get_att_text_c
        end function
        
        function nc_inq_attlen_c(ncid, varid, name, lenp) bind(c, name='nc_inq_attlen')
            import :: c_int, c_char, c_size_t
            integer(c_int), value, intent(in) :: ncid
            integer(c_int), value, intent(in) :: varid
            character(kind=c_char), intent(in) :: name(*)
            integer(c_size_t), intent(out) :: lenp
            integer(c_int) :: nc_inq_attlen_c
        end function
        
        function nc_strerror_c(ncerr) bind(c, name='nc_strerror')
            import :: c_int, c_ptr
            integer(c_int), value, intent(in) :: ncerr
            type(c_ptr) :: nc_strerror_c
        end function
    end interface
    
    ! NetCDF variables
    integer(c_int) :: ncid, varid, dimid
    integer(c_int) :: status
    character(len=256) :: ncdf_file
    character(len=257, kind=c_char) :: ncdf_file_c
    
    ! NetCDF dimensions
    integer(c_int) :: time_dimid, lat_dimid, lon_dimid
    integer(c_size_t) :: ntime_c, nlat_c, nlon_c
    integer :: ntime, nlat, nlon
    
    ! NetCDF variable IDs
    integer(c_int) :: time_varid, lat_varid, lon_varid
    integer(c_int) :: pcp_varid, tmin_varid, tmax_varid, slr_varid, hmd_varid, wnd_varid
    
    ! Arrays for reading ALL data
    real(c_float), dimension(:), allocatable :: time_vals, lat_vals, lon_vals
    real(c_float), dimension(:,:,:), allocatable :: pcp_data, tmin_data, tmax_data
    real(c_float), dimension(:,:,:), allocatable :: slr_data, hmd_data, wnd_data
    
    ! Variables for finding closest grid point
    integer :: target_lat_idx, target_lon_idx
    real :: min_dist, dist
    integer :: ilat, ilon
    
    ! Variables for date calculation
    integer :: days_since_ref, year, month, day
    integer :: ref_year, ref_month, ref_day
    character(len=256) :: time_units
    character(len=257, kind=c_char) :: time_units_c
    integer(c_size_t) :: att_len
    
    ! Loop counters and diagnostics
    integer :: itime, iyear, iday, i, iwst
    integer :: actual_year, days_in_year_loop
    logical :: exists
    
    ! Helper function to convert C string pointer to Fortran string
    interface
        function c_strlen(str) bind(c, name='strlen')
            import :: c_ptr, c_size_t
            type(c_ptr), value, intent(in) :: str
            integer(c_size_t) :: c_strlen
        end function
    end interface
    
    write (*,*) "reading data using netcdf C interface"
    write (9003,*) "reading data using netcdf C interface"

    ! Get NetCDF filename based on precipitation path from file.cio
    if (in_path_pcp%pcp == "null" .or. trim(in_path_pcp%pcp) == " ") then
        write(*,*) "! error: No NetCDF file specified in pcp path in 'file.cio'"
        write (9003,*) "! error: No NetCDF file specified in pcp path in 'file.cio'"
        stop
    else
        ncdf_file = TRIM(ADJUSTL(in_path_pcp%pcp))
        inquire(file=trim(ncdf_file), exist=exists)
        if (.not. exists) then
            write(*,*) "! error: NetCDF file does not exist at", trim(ncdf_file)
            write (9003,*) "! error: NetCDF file does not exist at", trim(ncdf_file)
            stop
        end if
    endif
    
    ! Convert filename to C string
    call f_to_c_string(ncdf_file, ncdf_file_c)
    
    ! Open NetCDF file
    status = nc_open_c(ncdf_file_c, NC_NOWRITE, ncid)
    if (status /= NC_NOERR) then
        write (*,*) "! error: Cannot open NetCDF file: ", trim(ncdf_file)
        write (*,*) "! NetCDF Error code: ", status
        write (9003,*) "! error: Cannot open NetCDF file: ", trim(ncdf_file)
        stop
    endif
    
    ! Get dimensions for gridded data
    status = nc_inq_dimid_c(ncid, "time" // c_null_char, time_dimid)
    if (status /= NC_NOERR) then
        write (*,*) "! error: Cannot find 'time' dimension, code:", status
        write (9003,*) "! error: Cannot find 'time' dimension, code:", status
        stop
    endif
    
    status = nc_inq_dimid_c(ncid, "lat" // c_null_char, lat_dimid)
    if (status /= NC_NOERR) then
        write (*,*) "! error: Cannot find 'lat' dimension, code:", status
        write (9003,*) "! error: Cannot find 'lat' dimension, code:", status
        stop
    endif
    
    status = nc_inq_dimid_c(ncid, "lon" // c_null_char, lon_dimid)
    if (status /= NC_NOERR) then
        write (*,*) "! error: Cannot find 'lon' dimension, code:", status
        write (9003,*) "! error: Cannot find 'lon' dimension, code:", status
        stop
    endif
    
    ! Get dimension sizes
    status = nc_inq_dimlen_c(ncid, time_dimid, ntime_c)
    if (status /= NC_NOERR) then
        write (*,*) "! error: Cannot get time dimension length, code:", status
        stop
    endif
    ntime = int(ntime_c)
    
    status = nc_inq_dimlen_c(ncid, lat_dimid, nlat_c)
    if (status /= NC_NOERR) then
        write (*,*) "! error: Cannot get lat dimension length, code:", status
        stop
    endif
    nlat = int(nlat_c)
    
    status = nc_inq_dimlen_c(ncid, lon_dimid, nlon_c)
    if (status /= NC_NOERR) then
        write (*,*) "! error: Cannot get lon dimension length, code:", status
        stop
    endif
    nlon = int(nlon_c)
    
    ! Allocate arrays - storage order is (lon, lat, time)
    allocate(time_vals(ntime), lat_vals(nlat), lon_vals(nlon))
    allocate(pcp_data(nlon, nlat, ntime), tmin_data(nlon, nlat, ntime), tmax_data(nlon, nlat, ntime))
    allocate(slr_data(nlon, nlat, ntime), hmd_data(nlon, nlat, ntime), wnd_data(nlon, nlat, ntime))
    
    ! Read coordinate data
    status = nc_inq_varid_c(ncid, "lat" // c_null_char, lat_varid)
    if (status == NC_NOERR) then
        status = nc_get_var_float_c(ncid, lat_varid, lat_vals)
        if (status /= NC_NOERR) then
            write (*,*) "! error reading lat data, code: ", status
            write (9003,*) "! error reading lat data, code: ", status
            stop
        endif
    else
        write (*,*) "WARNING: Cannot find lat variable, code:", status
    endif
    
    status = nc_inq_varid_c(ncid, "lon" // c_null_char, lon_varid)
    if (status == NC_NOERR) then
        status = nc_get_var_float_c(ncid, lon_varid, lon_vals)
        if (status /= NC_NOERR) then
            write (*,*) "! error reading lon data, code: ", status
            write (9003,*) "! error reading lon data, code: ", status
            stop
        endif
    else
        write (*,*) "! WARNING: Cannot find lon variable, code:", status
    endif
    
    ! Read time data to get the date for first time step
    status = nc_inq_varid_c(ncid, "time" // c_null_char, time_varid)
    if (status == NC_NOERR) then
        ! Try to read time units attribute
        status = nc_inq_attlen_c(ncid, time_varid, "units" // c_null_char, att_len)
        if (status == NC_NOERR .and. att_len > 0) then
            ! Allocate space for the attribute (including null terminator)
            if (att_len < 256) then
                time_units_c = repeat(c_null_char, 257)
                status = nc_get_att_text_c(ncid, time_varid, "units" // c_null_char, time_units_c)
                if (status == NC_NOERR) then
                    call c_to_f_string(time_units_c, time_units)
                else
                    time_units = "days since 1970-01-01 00:00:00"
                    write (*,*) "! warning: Cannot read time units attribute, using default"
                    write (9003,*) "! warning: Cannot read time units attribute, using default"
                endif
            else
                time_units = "days since 1970-01-01 00:00:00"
                write (*,*) "! warning: Time units attribute too long, using default"
            endif
        else
            time_units = "days since 1970-01-01 00:00:00"
            write (*,*) "! warning: Cannot find time units attribute, using default"
            write (9003,*) "! warning: Cannot find time units attribute, using default"
        endif
        
        ! Parse reference date from time units string
        call parse_time_units(time_units, ref_year, ref_month, ref_day)
        
        status = nc_get_var_float_c(ncid, time_varid, time_vals)
        if (status == NC_NOERR) then
            if (ntime >= 1) then
                days_since_ref = int(time_vals(1))
                call add_days_to_date(ref_year, ref_month, ref_day, days_since_ref, year, month, day)
            endif
        else
            write (*,*) "! error reading time data, code: ", status
            write (9003,*) "! error reading time data, code: ", status
            stop
        endif
    else
        write (*,*) "WARNING: No time variable found in NetCDF, code:", status
        write (9003,*) "WARNING: No time variable found in NetCDF, code:", status
        stop
    endif
    
    ! Read all climate variables
    call read_climate_variable("pcp", pcp_varid, pcp_data, "precipitation")
    call read_climate_variable("tmin", tmin_varid, tmin_data, "minimum temperature")
    call read_climate_variable("tmax", tmax_varid, tmax_data, "maximum temperature")
    call read_climate_variable("slr", slr_varid, slr_data, "solar radiation")
    call read_climate_variable("hmd", hmd_varid, hmd_data, "humidity")
    call read_climate_variable("wnd", wnd_varid, wnd_data, "wind speed")

    ! allocate and populate climate arrays
    allocate (pcp(0:db_mx%wst))
    allocate (pcp_n(db_mx%wst))
    allocate (tmp(0:db_mx%wst))
    allocate (tmp_n(db_mx%wst))  
    allocate (slr(0:db_mx%wst))
    allocate (slr_n(db_mx%wst))
    allocate (hmd(0:db_mx%wst))
    allocate (hmd_n(db_mx%wst))
    allocate (wnd(0:db_mx%wst))
    allocate (wnd_n(db_mx%wst))
    db_mx%pcpfiles = db_mx%wst
    db_mx%tmpfiles = db_mx%wst  
    db_mx%slrfiles = db_mx%wst
    db_mx%rhfiles = db_mx%wst
    db_mx%wndfiles = db_mx%wst
    
    ! Set the climate file indices for each station
    do i = 1, db_mx%wst
        wst(i)%wco%pgage = i
        wst(i)%wco%tgage = i
        wst(i)%wco%sgage = i
        wst(i)%wco%hgage = i
        wst(i)%wco%wgage = i
    end do
    
    ! Populate station metadata and time series data
    do iwst = 1, db_mx%wst
        
        ! Find closest grid point to this station
        min_dist = 999999.0
        target_lat_idx = 1
        target_lon_idx = 1
        
        do ilat = 1, nlat
            do ilon = 1, nlon
                dist = sqrt((lat_vals(ilat) - wst(iwst)%lat)**2 + (lon_vals(ilon) - wst(iwst)%lon)**2)
                if (dist < min_dist) then
                    min_dist = dist
                    target_lat_idx = ilat
                    target_lon_idx = ilon
                endif
            end do
        end do
        
        ! Set station names and metadata
        call setup_station_metadata(iwst)
        
        ! Setup time series arrays
        call setup_timeseries_arrays(iwst, ntime)
        
        ! Populate time series data
        call populate_timeseries_data(iwst, target_lat_idx, target_lon_idx, ntime)
        
    end do
    
    ! Close NetCDF file
    status = nc_close_c(ncid)
    if (status /= NC_NOERR) then
        write (*,*) "Warning: Error closing NetCDF file, code:", status
    endif
    
    ! Clean up allocated arrays
    deallocate(time_vals, lat_vals, lon_vals)
    deallocate(pcp_data, tmin_data, tmax_data, slr_data, hmd_data, wnd_data)
    
    write(*,'(A,I0,A)') " successfully populated time series for ", db_mx%wst, " stations"
    write(9003,'(A,I0,A)') " successfully populated time series for ", db_mx%wst, " stations"

    return

contains

    ! Helper subroutine to convert Fortran string to C string
    subroutine f_to_c_string(f_str, c_str)
        character(len=*), intent(in) :: f_str
        character(len=*, kind=c_char), intent(out) :: c_str
        integer :: i, f_len
        
        f_len = len_trim(f_str)
        do i = 1, f_len
            c_str(i:i) = f_str(i:i)
        end do
        c_str(f_len+1:f_len+1) = c_null_char
        
    end subroutine f_to_c_string
    
    ! Helper subroutine to convert C string to Fortran string
    subroutine c_to_f_string(c_str, f_str)
        character(len=*, kind=c_char), intent(in) :: c_str
        character(len=*), intent(out) :: f_str
        integer :: i, c_len
        
        ! Find the null terminator
        c_len = 0
        do i = 1, len(c_str)
            if (c_str(i:i) == c_null_char) then
                c_len = i - 1
                exit
            endif
        end do
        
        if (c_len == 0) c_len = len(c_str)
        
        ! Copy characters
        do i = 1, min(c_len, len(f_str))
            f_str(i:i) = c_str(i:i)
        end do
        
        ! Pad with spaces if necessary
        if (c_len < len(f_str)) then
            f_str(c_len+1:) = ' '
        endif
        
    end subroutine c_to_f_string

    ! Helper subroutine to read a climate variable
    subroutine read_climate_variable(var_name, var_id, var_data, description)
        character(len=*), intent(in) :: var_name, description
        integer(c_int), intent(out) :: var_id
        real(c_float), dimension(:,:,:), intent(out) :: var_data
        
        status = nc_inq_varid_c(ncid, trim(var_name) // c_null_char, var_id)
        if (status == NC_NOERR) then
            status = nc_get_var_float_c(ncid, var_id, var_data)
            if (status /= NC_NOERR) then
                write (*,*) "! error reading ", description, ", code: ", status
                write (9003,*) "! error reading ", description, ", code: ", status
                stop
            endif
        else
            write (*,*) "WARNING: No ", trim(var_name), " variable found in NetCDF, code:", status
            write (9003,*) "WARNING: No ", trim(var_name), " variable found in NetCDF, code:", status
            var_data = 0.0  ! Fill with zeros if variable not found
        endif
        
    end subroutine read_climate_variable
    
    ! Helper subroutine to setup station metadata
    subroutine setup_station_metadata(iwst)
        integer, intent(in) :: iwst
        
        ! Set station names as "filenames"
        pcp_n(iwst) = wst(iwst)%name
        tmp_n(iwst) = wst(iwst)%name
        slr_n(iwst) = wst(iwst)%name 
        hmd_n(iwst) = wst(iwst)%name
        wnd_n(iwst) = wst(iwst)%name
        
        ! Populate station metadata - use station coordinates (not NetCDF grid)
        pcp(iwst)%filename = wst(iwst)%name
        pcp(iwst)%lat = wst(iwst)%lat
        pcp(iwst)%long = wst(iwst)%lon
        pcp(iwst)%elev = wst(iwst)%elev
        
        ! Copy metadata to other climate arrays
        tmp(iwst)%filename = pcp(iwst)%filename
        tmp(iwst)%lat = pcp(iwst)%lat
        tmp(iwst)%long = pcp(iwst)%long
        tmp(iwst)%elev = pcp(iwst)%elev
        
        slr(iwst)%filename = pcp(iwst)%filename
        slr(iwst)%lat = pcp(iwst)%lat
        slr(iwst)%long = pcp(iwst)%long
        slr(iwst)%elev = pcp(iwst)%elev
        
        hmd(iwst)%filename = pcp(iwst)%filename
        hmd(iwst)%lat = pcp(iwst)%lat
        hmd(iwst)%long = pcp(iwst)%long
        hmd(iwst)%elev = pcp(iwst)%elev
        
        wnd(iwst)%filename = pcp(iwst)%filename
        wnd(iwst)%lat = pcp(iwst)%lat
        wnd(iwst)%long = pcp(iwst)%long
        wnd(iwst)%elev = pcp(iwst)%elev
        
    end subroutine setup_station_metadata
    
    ! Helper subroutine to setup time series arrays
    subroutine setup_timeseries_arrays(iwst, ntime_total)
        integer, intent(in) :: iwst, ntime_total
        
        ! Calculate number of years (approximate for daily data)
        pcp(iwst)%nbyr = max(1, ntime_total / 365)
        tmp(iwst)%nbyr = pcp(iwst)%nbyr
        slr(iwst)%nbyr = pcp(iwst)%nbyr
        hmd(iwst)%nbyr = pcp(iwst)%nbyr
        wnd(iwst)%nbyr = pcp(iwst)%nbyr
        
        ! Set timestep (0 = daily)
        pcp(iwst)%tstep = 0
        tmp(iwst)%tstep = 0
        slr(iwst)%tstep = 0
        hmd(iwst)%tstep = 0
        wnd(iwst)%tstep = 0
        
        ! Initialize counters
        pcp(iwst)%days_gen = 0
        tmp(iwst)%days_gen = 0
        slr(iwst)%days_gen = 0
        hmd(iwst)%days_gen = 0
        wnd(iwst)%days_gen = 0
        
        ! Set start and end years
        pcp(iwst)%start_yr = time%yrc
        pcp(iwst)%end_yr = time%yrc + pcp(iwst)%nbyr - 1
        pcp(iwst)%start_day = 1
        
        ! Calculate end_day based on leap year for last year
        actual_year = pcp(iwst)%end_yr
        if (is_leap_year(actual_year)) then
            pcp(iwst)%end_day = 366
        else
            pcp(iwst)%end_day = 365
        endif
        
        ! Calculate yrs_start
        if (pcp(iwst)%start_yr > time%yrc) then
            pcp(iwst)%yrs_start = pcp(iwst)%start_yr - time%yrc
        else
            pcp(iwst)%yrs_start = 0
        end if
        
        ! Copy to other climate variables
        tmp(iwst)%start_yr = pcp(iwst)%start_yr
        tmp(iwst)%end_yr = pcp(iwst)%end_yr
        tmp(iwst)%start_day = pcp(iwst)%start_day
        tmp(iwst)%end_day = pcp(iwst)%end_day
        tmp(iwst)%yrs_start = pcp(iwst)%yrs_start
        
        slr(iwst)%start_yr = pcp(iwst)%start_yr
        slr(iwst)%end_yr = pcp(iwst)%end_yr
        slr(iwst)%start_day = pcp(iwst)%start_day
        slr(iwst)%end_day = pcp(iwst)%end_day
        slr(iwst)%yrs_start = pcp(iwst)%yrs_start
        
        hmd(iwst)%start_yr = pcp(iwst)%start_yr
        hmd(iwst)%end_yr = pcp(iwst)%end_yr
        hmd(iwst)%start_day = pcp(iwst)%start_day
        hmd(iwst)%end_day = pcp(iwst)%end_day
        hmd(iwst)%yrs_start = pcp(iwst)%yrs_start
        
        wnd(iwst)%start_yr = pcp(iwst)%start_yr
        wnd(iwst)%end_yr = pcp(iwst)%end_yr
        wnd(iwst)%start_day = pcp(iwst)%start_day
        wnd(iwst)%end_day = pcp(iwst)%end_day
        wnd(iwst)%yrs_start = pcp(iwst)%yrs_start
        
        ! Allocate time series arrays
        allocate (pcp(iwst)%ts(366, pcp(iwst)%nbyr), source = 0.)
        allocate (tmp(iwst)%ts(366, tmp(iwst)%nbyr), source = 0.)   ! ts for TMAX
        allocate (tmp(iwst)%ts2(366, tmp(iwst)%nbyr), source = 0.) ! ts2 for TMIN
        allocate (slr(iwst)%ts(366, slr(iwst)%nbyr), source = 0.)
        allocate (hmd(iwst)%ts(366, hmd(iwst)%nbyr), source = 0.)
        allocate (wnd(iwst)%ts(366, wnd(iwst)%nbyr), source = 0.)
        
    end subroutine setup_timeseries_arrays
    
    ! Helper subroutine to populate time series data
    subroutine populate_timeseries_data(iwst, target_lat_idx, target_lon_idx, ntime_total)
        integer, intent(in) :: iwst, target_lat_idx, target_lon_idx, ntime_total
        
        itime = 1
        do iyear = 1, pcp(iwst)%nbyr
            ! Calculate actual year for leap year check
            actual_year = pcp(iwst)%start_yr + iyear - 1
            if (is_leap_year(actual_year)) then
                days_in_year_loop = 366
            else
                days_in_year_loop = 365
            endif
            
            do iday = 1, days_in_year_loop
                if (itime <= ntime_total) then
                    ! Precipitation - apply station scaling factor
                    if (allocated(pcp_data)) then
                        pcp(iwst)%ts(iday, iyear) = pcp_data(target_lon_idx, target_lat_idx, itime) * wst(iwst)%pcp_factor
                        if (pcp(iwst)%ts(iday, iyear) < 0.0) pcp(iwst)%ts(iday, iyear) = 0.0
                    endif
                    
                    ! Temperature - populate both ts (TMAX) and ts2 (TMIN)
                    if (allocated(tmax_data)) then
                        tmp(iwst)%ts(iday, iyear) = tmax_data(target_lon_idx, target_lat_idx, itime) * wst(iwst)%tmax_factor
                    endif
                    
                    if (allocated(tmin_data)) then
                        tmp(iwst)%ts2(iday, iyear) = tmin_data(target_lon_idx, target_lat_idx, itime) * wst(iwst)%tmin_factor
                    endif
                    
                    ! Solar radiation
                    if (allocated(slr_data)) then
                        slr(iwst)%ts(iday, iyear) = slr_data(target_lon_idx, target_lat_idx, itime) * wst(iwst)%slr_factor
                        if (slr(iwst)%ts(iday, iyear) < 0.0) slr(iwst)%ts(iday, iyear) = 0.0
                    endif
                    
                    ! Humidity
                    if (allocated(hmd_data)) then
                        hmd(iwst)%ts(iday, iyear) = hmd_data(target_lon_idx, target_lat_idx, itime) * wst(iwst)%hmd_factor
                        if (hmd(iwst)%ts(iday, iyear) < 0.0) hmd(iwst)%ts(iday, iyear) = 0.0
                        if (hmd(iwst)%ts(iday, iyear) > 1.0) hmd(iwst)%ts(iday, iyear) = 1.0
                    endif
                    
                    ! Wind speed
                    if (allocated(wnd_data)) then
                        wnd(iwst)%ts(iday, iyear) = wnd_data(target_lon_idx, target_lat_idx, itime) * wst(iwst)%wnd_factor
                        if (wnd(iwst)%ts(iday, iyear) < 0.0) wnd(iwst)%ts(iday, iyear) = 0.0
                    endif
                    
                    itime = itime + 1
                else
                    ! No more NetCDF data available
                    exit
                endif
            end do
        end do
        
    end subroutine populate_timeseries_data

    ! Helper subroutine to parse time units string
    subroutine parse_time_units(units_str, ref_yr, ref_mo, ref_dy)
        character(len=*), intent(in) :: units_str
        integer, intent(out) :: ref_yr, ref_mo, ref_dy
        
        integer :: pos1, pos2, ios
        character(len=20) :: date_part
        
        ! Initialize defaults
        ref_yr = 1970
        ref_mo = 1
        ref_dy = 1
        
        ! Find "since" keyword
        pos1 = index(units_str, "since")
        if (pos1 > 0) then
            pos1 = pos1 + 5  ! Move past "since"
            
            ! Skip whitespace
            do while (pos1 <= len(units_str) .and. units_str(pos1:pos1) == ' ')
                pos1 = pos1 + 1
            end do
            
            ! Find end of date part (before time if present)
            pos2 = index(units_str(pos1:), ' ')
            if (pos2 == 0) then
                pos2 = len(units_str) + 1
            else
                pos2 = pos1 + pos2 - 1
            endif
            
            date_part = units_str(pos1:pos2-1)
            
            ! Parse YYYY-MM-DD format
            read(date_part(1:4), *, iostat=ios) ref_yr
            if (ios == 0 .and. len_trim(date_part) >= 7) then
                read(date_part(6:7), *, iostat=ios) ref_mo
            endif
            if (ios == 0 .and. len_trim(date_part) >= 10) then
                read(date_part(9:10), *, iostat=ios) ref_dy
            endif
        endif
        
        ! Validate parsed values
        if (ref_yr < 1000 .or. ref_yr > 5000) ref_yr = 1970
        if (ref_mo < 1 .or. ref_mo > 12) ref_mo = 1
        if (ref_dy < 1 .or. ref_dy > 31) ref_dy = 1
        
    end subroutine parse_time_units
    
    ! Helper subroutine to add days to a date
    subroutine add_days_to_date(start_yr, start_mo, start_dy, days_to_add, end_yr, end_mo, end_dy)
        integer, intent(in) :: start_yr, start_mo, start_dy, days_to_add
        integer, intent(out) :: end_yr, end_mo, end_dy
        
        integer :: days_left, days_in_month
        integer :: month_days(12) = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        
        end_yr = start_yr
        end_mo = start_mo
        end_dy = start_dy
        days_left = days_to_add
        
        ! Add days
        do while (days_left > 0)
            ! Check for leap year and adjust February
            if (end_mo == 2) then
                if (is_leap_year(end_yr)) then
                    days_in_month = 29
                else
                    days_in_month = 28
                endif
            else
                days_in_month = month_days(end_mo)
            endif
            
            if (end_dy + days_left <= days_in_month) then
                ! Remaining days fit in current month
                end_dy = end_dy + days_left
                days_left = 0
            else
                ! Move to next month
                days_left = days_left - (days_in_month - end_dy + 1)
                end_dy = 1
                end_mo = end_mo + 1
                if (end_mo > 12) then
                    end_mo = 1
                    end_yr = end_yr + 1
                endif
            endif
        end do
        
    end subroutine add_days_to_date
    
    ! Helper function to check if year is leap year
    logical function is_leap_year(check_year)
        integer, intent(in) :: check_year
        
        is_leap_year = .false.
        if (mod(check_year, 4) == 0) then
            if (mod(check_year, 100) == 0) then
                if (mod(check_year, 400) == 0) then
                    is_leap_year = .true.
                endif
            else
                is_leap_year = .true.
            endif
        endif
        
    end function is_leap_year

end subroutine cli_ncdf_meas