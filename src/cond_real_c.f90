      subroutine cond_real_c (op, var_cur, var_tbl)
    
      use reservoir_conditions_module
      
      implicit none

      character(2), intent (in) :: op       !       |operator - <, <=, >, >=, =, /=
      real, intent (in)  :: var_cur         !       |current variable to check alternative
      real, intent (in)  :: var_tbl         !       |decision table variable to cpmpare with

      if (op == "<") then
        if (var_cur >= var_tbl) then
          hit = "n"
        end if
      end if
      if (op == ">") then
        if (var_cur <= var_tbl) then
          hit = "n"
        end if
      end if
      if (op == "<=") then
        if (var_cur > var_tbl) then
          hit = "n"
        end if
      end if
      if (op == ">=") then
        if (var_cur < var_tbl) then
          hit = "n"
        end if
      end if
      if (op == "=") then
        if (var_cur /= var_tbl) then
          hit = "n"
        end if
      end if
      if (op == "/=") then
        if (var_cur == var_tbl) then
          hit = "n"
        end if
      end if

      return
      end subroutine cond_real_c