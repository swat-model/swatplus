      module netcdf_writer_module
      use netcdf
      implicit none
      private
      public :: nc_check, nc_deflate_level, nc_set_deflate_level
      public :: nc_create_file, nc_enddef_file, nc_close_file
      public :: nc_def_dim_unlim, nc_def_dim_len
      public :: nc_def_var_r32, nc_put_var_1d_i32, nc_put_var_1d_i64
      public :: nc_put_var_2d_r32_slice, nc_put_var_1d_r32_slice
      public :: nc_put_att_text, nc_put_att_r32

      integer :: nc_deflate_level = 4
      integer, parameter :: nc_fill_r32 = -9999

      contains

      subroutine nc_set_deflate_level(level)
        integer, intent(in) :: level
        nc_deflate_level = max(0, min(9, level))
      end subroutine nc_set_deflate_level

      subroutine nc_check(istat, msg)
        integer, intent(in) :: istat
        character(len=*), intent(in) :: msg
        if (istat == nf90_noerr) return
        write (9003,*) "NetCDF error: ", trim(msg), " : ", trim(nf90_strerror(istat))
        stop 1
      end subroutine nc_check

      subroutine nc_create_file(path, ncid)
        character(len=*), intent(in) :: path
        integer, intent(out) :: ncid
        integer :: istat
        istat = nf90_create(trim(path), ior(nf90_netcdf4, nf90_classic_model), ncid)
        call nc_check(istat, "nf90_create "//trim(path))
      end subroutine nc_create_file

      subroutine nc_enddef_file(ncid)
        integer, intent(in) :: ncid
        integer :: istat
        istat = nf90_enddef(ncid)
        call nc_check(istat, "nf90_enddef")
      end subroutine nc_enddef_file

      subroutine nc_close_file(ncid)
        integer, intent(in) :: ncid
        integer :: istat
        if (ncid < 0) return
        istat = nf90_close(ncid)
        call nc_check(istat, "nf90_close")
      end subroutine nc_close_file

      subroutine nc_def_dim_unlim(ncid, name, dimid)
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: name
        integer, intent(out) :: dimid
        integer :: istat
        istat = nf90_def_dim(ncid, trim(name), nf90_unlimited, dimid)
        call nc_check(istat, "nf90_def_dim "//trim(name))
      end subroutine nc_def_dim_unlim

      subroutine nc_def_dim_len(ncid, name, len, dimid)
        integer, intent(in) :: ncid, len
        character(len=*), intent(in) :: name
        integer, intent(out) :: dimid
        integer :: istat
        istat = nf90_def_dim(ncid, trim(name), len, dimid)
        call nc_check(istat, "nf90_def_dim "//trim(name))
      end subroutine nc_def_dim_len

      subroutine nc_def_var_r32(ncid, name, dimids, varid, chunk, deflate)
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: name
        integer, intent(in) :: dimids(:)
        integer, intent(out) :: varid
        integer, intent(in), optional :: chunk(:)
        logical, intent(in), optional :: deflate
        integer :: istat
        real :: fillv
        fillv = nc_fill_r32
        istat = nf90_def_var(ncid, trim(name), nf90_float, dimids, varid)
        call nc_check(istat, "nf90_def_var "//trim(name))
        istat = nf90_put_att(ncid, varid, "_FillValue", fillv)
        call nc_check(istat, "_FillValue")
        if (present(chunk)) then
          istat = nf90_def_var_chunking(ncid, varid, 0, chunk)
          call nc_check(istat, "chunking")
        end if
        if (present(deflate)) then
          if (deflate .and. nc_deflate_level > 0) then
            istat = nf90_def_var_deflate(ncid, varid, 1, 1, nc_deflate_level)
            call nc_check(istat, "deflate")
          end if
        end if
      end subroutine nc_def_var_r32

      subroutine nc_put_att_text(ncid, varid, att, val)
        integer, intent(in) :: ncid, varid
        character(len=*), intent(in) :: att, val
        integer :: istat
        istat = nf90_put_att(ncid, varid, trim(att), trim(val))
        call nc_check(istat, "put_att "//trim(att))
      end subroutine nc_put_att_text

      subroutine nc_put_att_r32(ncid, varid, att, val)
        integer, intent(in) :: ncid, varid
        character(len=*), intent(in) :: att
        real, intent(in) :: val
        integer :: istat
        istat = nf90_put_att(ncid, varid, trim(att), val)
        call nc_check(istat, "put_att "//trim(att))
      end subroutine nc_put_att_r32

      subroutine nc_put_var_1d_i32(ncid, varid, vals, start)
        integer, intent(in) :: ncid, varid
        integer, intent(in) :: vals(:)
        integer, intent(in), optional :: start(:)
        integer :: istat
        if (present(start)) then
          istat = nf90_put_var(ncid, varid, vals, start=start, count=(/size(vals)/))
        else
          istat = nf90_put_var(ncid, varid, vals)
        end if
        call nc_check(istat, "put_var 1d i32")
      end subroutine nc_put_var_1d_i32

      subroutine nc_put_var_1d_i64(ncid, varid, vals, start)
        integer, intent(in) :: ncid, varid
        integer(kind=8), intent(in) :: vals(:)
        integer, intent(in), optional :: start(:)
        integer :: istat
        if (present(start)) then
          istat = nf90_put_var(ncid, varid, vals, start=start, count=(/size(vals)/))
        else
          istat = nf90_put_var(ncid, varid, vals)
        end if
        call nc_check(istat, "put_var 1d i64")
      end subroutine nc_put_var_1d_i64

      subroutine nc_put_var_1d_r32_slice(ncid, varid, vals, tidx)
        integer, intent(in) :: ncid, varid, tidx
        real, intent(in) :: vals(:)
        integer :: istat, start(1)
        start(1) = tidx
        istat = nf90_put_var(ncid, varid, vals, start=start, count=(/size(vals)/))
        call nc_check(istat, "put_var 1d r32 slice")
      end subroutine nc_put_var_1d_r32_slice

      subroutine nc_put_var_2d_r32_slice(ncid, varid, slab, tidx)
        integer, intent(in) :: ncid, varid, tidx
        real, intent(in) :: slab(:,:)
        integer :: istat, start(2)
        start(1) = 1
        start(2) = tidx
        istat = nf90_put_var(ncid, varid, slab, start=start, count=(/size(slab,1), 1/))
        call nc_check(istat, "put_var 2d r32 slice")
      end subroutine nc_put_var_2d_r32_slice

      end module netcdf_writer_module
