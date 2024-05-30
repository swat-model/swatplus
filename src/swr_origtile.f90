	subroutine swr_origtile(tile_above_btm)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes tile drainage using basic tile equations 

      use hru_module, only : hru, ihru, qtile, sw_excess, wt_shall
      use soil_module
      
      implicit none

      integer :: j                          !none       |HRU number
      real, intent (in) :: tile_above_btm   !mm         |!height of tiles above bottom of soil profile

      j = ihru

      !! compute tile flow using the original tile equations

      !if (soil(j)%sw > soil(j)%sumfc .and. wt_shall > 1.e-6) then
      if (soil(j)%sw > soil(j)%sumfc ) then
        sw_excess = (tile_above_btm / wt_shall) * (soil(j)%sw - soil(j)%sumfc)
        !! (wt_above_btm - tile_above_btm) / wt_above_btm * (sw - fc)
        sw_excess = (wt_shall - tile_above_btm) / wt_shall * (soil(j)%sw - soil(j)%sumfc)
        qtile = sw_excess * (1. - Exp(-24. / hru(j)%sdr%time))
        qtile = Min(qtile, hru(j)%sdr%drain_co)
      else
        qtile = 0.
      end if
     
	return
	end subroutine swr_origtile