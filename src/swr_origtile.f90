	subroutine swr_origtile(wt_above_tile)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes tile drainage using basic tile equations 

      use hru_module, only : hru, ihru, qtile, sw_excess, wt_shall
      use soil_module
      
      implicit none

      integer :: j                          !none       |HRU number
      real, intent (in) :: wt_above_tile    !mm         |height of water table above tiles

      j = ihru

      !! compute tile flow using the original tile equations

      if (soil(j)%sw > soil(j)%sumfc) then
        sw_excess = (wt_above_tile / wt_shall) * (soil(j)%sw - soil(j)%sumfc)
        !! (wt_above_btm - tile_above_btm) / wt_above_btm * (sw - fc)
        sw_excess = (wt_shall - wt_above_tile) / wt_shall * (soil(j)%sw - soil(j)%sumfc)
        qtile = sw_excess * (1. - Exp(-24. / hru(j)%sdr%time))
        qtile = amin1(qtile, hru(j)%sdr%drain_co)
      else
        qtile = 0.
      end if
     
	return
	end subroutine swr_origtile