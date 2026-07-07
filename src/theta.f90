      function theta(r20,thk,tmp)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this function corrects rate constants for temperature
!!    Equation is III-52 from QUAL2E

      implicit none

      real, intent (in) :: r20   !1/day         |value of the reaction rate coefficient at
                                 !              |the standard temperature (20 degrees C)
      real, intent (in) :: thk   !none          |temperature adjustment factor (empirical
                                 !              |constant for each reaction coefficient)
      real, intent (in) :: tmp   !deg C         |temperature on current day
      real :: theta              !1/day         |value of the reaction rate coefficient at
                                 !              |the local temperature 

      theta = r20 * thk ** (tmp - 20.)

      return
      end