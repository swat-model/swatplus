      function wq_semianalyt(tres, tdel, term_m, prock, cprev, cint)
      
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

      implicit none
 
      real, intent (in) :: tres
      real, intent (in) :: tdel
      real, intent (in) :: prock
      real, intent (in) :: term_m
      real, intent (in) :: cprev
      real, intent (in) :: cint
      real :: help1, help2, help3, help4, term1, term2, yy
      real :: wq_semianalyt

      help1 = 1. / tres - prock
      help2 = exp(-tdel * help1)
      help3 = cint / tres + term_m
      help4 = help3 / help1
      term1 = cprev * help2
      term2 = help4 * (1. - help2)
      yy = term1 + term2
      wq_semianalyt = term1 + term2
      
    !! if time of residence in reach is less than or eq to timestep don't do this. MJW 2023
      if (tres <= tdel) then
          wq_semianalyt =cint  
      end if

      return
      end function
      
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

      real, intent (in) :: t1
      real, intent (in) :: t2
      real, intent (in) :: tk
      real, intent (in) :: c1
      real, intent (in) :: c2
      real :: h1, h2, help, tm, h3, wq_k2m, wq_semianalyt
      
      h1 = wq_semianalyt (t1, t2, 0., 0., c1, c2)
      h2 = wq_semianalyt (t1, t2, 0., tk, c1, c2)
      help = exp(-t2 / t1)
         
      tm = (h2 - c1 * help) / (t1 * (1. - help)) - c2 / t1
      h3 = wq_semianalyt (t1, t2, tm, 0., c1, c2)
      wq_k2m = tm

      return
      end function