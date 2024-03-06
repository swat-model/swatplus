      subroutine cli_bounds_check (st_day, st_yr, end_day, end_yr, out_bounds)
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine checks to see if climate data is in current simulation day
!!

      use time_module
      use climate_module

      implicit none
        
      character(len=1) :: out_bounds    ! none          |y==out of bounds; n==not out
      integer :: st_day                 ! none          |start day of measured climate data
      integer :: end_day                ! none          |end day of measured climate data
      integer :: st_yr                  ! none          |start year of measured climate data
      integer :: end_yr                 ! none          |end year of measured climate data

      !! check id climate data starts before simulation
      if (st_yr > time%yrc) then
        out_bounds = "y"
      else
        if (st_yr == time%yrc) then
          if (st_day > time%day) then
            out_bounds = "y"
          end if
        end if
      end if
      
      !! check if climate data starts after simulation
      if (end_yr < time%yrc) then
        out_bounds = "y"
      else
        if (end_yr == time%yrc) then
          if (end_day < time%day) then
            out_bounds = "y"
          end if
        end if
      end if

      return
      end subroutine cli_bounds_check