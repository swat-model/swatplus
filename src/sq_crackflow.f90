      subroutine sq_crackflow
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this surboutine modifies surface runoff to account for crack flow

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hhqday(:)   |mm H2O        |surface runoff for the hour in HRUS
 
!!    surfq(:)    |mm H2O        |surface runoff in the HRU for the day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hhqday(:)   |mm H2O        |surface runoff for the hour in HRU
!!    surfq(:)    |mm H2O        |surface runoff in the HRU for the day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module
      use hru_module, only : surfq, hhqday, ihru, voltot 
      use time_module
      
      implicit none

      integer :: j      !none          |HRU number
      real :: voli      !none          |volume available for crack flow
      integer :: ii     !none          |counter

      j = ihru

      !! subtract crack flow from surface runoff
      if (surfq(j) > voltot) then
        surfq(j) = surfq(j) - voltot
      else
        surfq(j) = 0.
      endif

      if (time%step > 1) then
        voli = 0.
        voli = voltot
        do ii = 1, time%step  !j.jeong 4/24/2009
          if (hhqday(j,ii) > voli) then
            hhqday(j,ii) = hhqday(j,ii) - voli
            voli = 0.
          else
            voli = voli - hhqday(j,ii)
            hhqday(j,ii) = 0.
          endif
        end do
      end if

      return
      end subroutine sq_crackflow