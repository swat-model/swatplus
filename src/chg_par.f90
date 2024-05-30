      function chg_par (val_cur, chg_typ, chg_val, absmin, absmax)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this function computes new paramter value based on 
!!    user defined change

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    val_cur     |variable      |current parameter value
!!                               |the standard temperature (20 degrees C)
!!    chg_typ     |variable      |type of change (absval, abschg, pctchg)
!!    chg_val     |variable      |amount of change
!!    parm_num    |              |calibration parameter number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    chg_par     |variable      |new parameter value
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      implicit none

      real, intent (in) :: val_cur                    !variable      |current parameter value
                                                      !              |the standard temperature (20 degrees C)
      real, intent (in) :: chg_val                    !variable      |amount of change
      character(len=16), intent (in) :: chg_typ       !variable      |type of change (absval, abschg, pctchg) 
      real :: chg_par                                 !variable      |new parameter value
      real :: absmin                                  !minimum range for variable
      real :: absmax                                  !maximum change for variable
      real :: amin1                                   !units         |description 

      select case (chg_typ)

      case ("absval")
        chg_par = chg_val
     
      case ("abschg")
        chg_par = val_cur + chg_val

      case ("pctchg")
        chg_par = (1. + chg_val / 100.) * val_cur
        
      case ("relchg")
        chg_par = (1. + chg_val) * val_cur
      
      end select
      
      chg_par = Max(chg_par, absmin)
      chg_par = amin1(chg_par, absmax)
      
      return
      end function chg_par