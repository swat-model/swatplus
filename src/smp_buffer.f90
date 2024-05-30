      subroutine smp_buffer
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the reduction of nitrates through a riparian 
!!    buffer system - developed for Sushama at NC State

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sedminpa    |kg P/ha       |amount of active mineral phosphorus sorbed to
!!                               |sediment in surface runoff in HRU for day
!!    sedminps    |kg P/ha       |amount of stable mineral phosphorus sorbed to
!!                               |sediment in surface runoff in HRU for day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sedminpa    |kg P/ha       |amount of active mineral phosphorus sorbed to
!!                               |sediment in surface runoff in HRU for day
!!    sedminps    |kg P/ha       |amount of stable mineral phosphorus sorbed to
!!                               |sediment in surface runoff in HRU for day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use hru_module, only : latno3, filterw, ihru
      
      implicit none
      
      integer :: j              !none             |hru number
      real :: reduc             !none             |fraction of water uptake by plants achieved 

      j = 0
      j = ihru

!! compute nitrate reduction as a function of distance to stream
      reduc = 2.1661 * filterw(j) - 5.1302
      if (reduc < 0.) reduc = 0.
      latno3(j) = latno3(j) * (1. - reduc / 100.)

      return
      end subroutine smp_buffer