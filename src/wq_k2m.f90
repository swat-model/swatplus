     function wq_k2m (t1, t2, tk, c1, c2)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This function solves a semi-analytic solution for the QUAL2E equations (cfr Befekadu Woldegiorgis).

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    xx          |none          |Exponential argument
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    tres        |days          |residence time in reach
!!    tdel        |days          |calculation time step
!!    term_m      |              |constant term in equation
!!    cprev       |mg/l          |concentration previous timestep
!!    cint        |mg/l          |incoming concentration      
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp
 
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      use utils

      external :: wq_semianalyt

      real, intent (in) :: t1
      real, intent (in) :: t2
      real, intent (in) :: tk
      real, intent (in) :: c1
      real, intent (in) :: c2
      real :: h1 = 0.
      real :: h2 = 0.
      real :: help = 0.
      real :: tm = 0.
      real :: h3 = 0.
      real :: wq_k2m
      real :: wq_semianalyt
      
      h1 = wq_semianalyt (t1, t2, 0., 0., c1, c2)
      h2 = wq_semianalyt (t1, t2, 0., tk, c1, c2)
      help = exp_w(-t2 / t1)
         
      tm = (h2 - c1 * help) / (t1 * (1. - help)) - c2 / t1
      h3 = wq_semianalyt (t1, t2, tm, 0., c1, c2)
      wq_k2m = tm

      return
      end function wq_k2m