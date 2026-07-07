      function jdt(numdays,i,m)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes the julian date given the month and
!!    the day of the month

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use time_module
      
      implicit none

      integer, intent (in), dimension (13) :: numdays     !julian date   |julian date for last day of preceding
                                                          !              |month (where the array location is the
                                                          !              |number of the month). The dates are for
                                                          !              |leap years (numdays=ndays)
      integer, intent (in) :: m                           !none          |month
      integer, intent (in) ::i                            !none          |day
      integer :: jdt                                      !julian date   |julian date
     
      jdt = 0

      if (m /= 0) then
        if (m <= 2) then
          jdt = numdays(m) + i
        else
          jdt = numdays(m) - 1 + i
        end if
      end if

      return
      end function jdt