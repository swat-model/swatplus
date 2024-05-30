      subroutine cond_integer (ic, var_cur, var_tbl)
      !current conditions include: w_stress, n_stress, phu_plant, phu_base0, soil_water, jday, month, vol
      ! year_rot, year_cal, year_seq, prob, land_use   
      !target variables include: w_stress -> wp, fc, ul; vol -> pvol, evol
    
      use conditional_module
      
      implicit none

      integer, intent (in)  :: ic           !           |current condition
      integer, intent (in)  :: var_cur      !           |current variable to check alternative
      integer, intent (in)  :: var_tbl      !           |decision table variable to compare with
      integer :: ialt

      do ialt = 1, d_tbl%alts
        if (d_tbl%alt(ic,ialt) /= "-" .and. d_tbl%act_hit(ialt) == "y") then
          if (d_tbl%alt(ic,ialt) == "<") then
            if (var_cur >= var_tbl) then
              d_tbl%act_hit(ialt) = "n"
            end if
          end if
          if (d_tbl%alt(ic,ialt) == ">") then
            if (var_cur <= var_tbl) then
              d_tbl%act_hit(ialt) = "n"
            end if
          end if
          if (d_tbl%alt(ic,ialt) == "<=") then
            if (var_cur > var_tbl) then
              d_tbl%act_hit(ialt) = "n"
            end if
          end if
          if (d_tbl%alt(ic,ialt) == ">=") then
            if (var_cur < var_tbl) then
              d_tbl%act_hit(ialt) = "n"
            end if
          end if
          if (d_tbl%alt(ic,ialt) == "=") then
            if (var_cur /= var_tbl) then
              d_tbl%act_hit(ialt) = "n"
            end if
          end if
          if (d_tbl%alt(ic,ialt) == "/=") then
            if (var_cur == var_tbl) then
              d_tbl%act_hit(ialt) = "n"
            end if
          end if
        end if
      end do

      return
      end subroutine cond_integer