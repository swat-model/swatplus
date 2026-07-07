      subroutine smp_bmpfixed
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine applies fixed removal eff. from the .ops to upland loads 
                  
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru          |none          |HRU number  
!!    bmp_pn(:)     |%             | Particulate N removal by BMP  
!!    bmp_sn(:)     |%             | Soluble N removal by BMP  
!!    bmp_bac(:)    |%             | Bacteria removal by BMP  
!!    sedminpa(:)   |kg P/ha       |amount of active mineral phosphorus sorbed
!!                                 |to sediment in surface runoff in HRU for day
!!    sedminps(:)   |kg P/ha       |amount of stable mineral phosphorus sorbed
!!                                 |to sediment in surface runoff in HRU for day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!                                 |to sediment in surface runoff in HRU for day
!!    sedminps(:)   |kg P/ha       |amount of stable mineral phosphorus sorbed
!!                                 |to sediment in surface runoff in HRU for day\
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
  
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use hru_module, only : hru, sedyld, ihru, sedminpa, sedminps, sedorgp, sedorgn, surqsolp, surqno3,   &
         latno3
      
      implicit none
      
      integer :: j           !none           |hru number
        
!!	set variables
      j = ihru

!! Subtract reductions from sediment, nutrients, bacteria, NOT SURFACE RUNOFF to protect water balance
!! Sediment
	sedyld(j) = sedyld(j) * (1 - hru(j)%lumv%bmp_sed/100)

!! Phosphorus
      !! Particulate
	sedminpa(j) = sedminpa(j) * (1 - hru(j)%lumv%bmp_pp/100)
	sedminps(j) = sedminps(j) * (1 - hru(j)%lumv%bmp_pp/100)
	sedorgp(j) = sedorgp(j) * (1 - hru(j)%lumv%bmp_pp/100)
      !! Soluble
	surqsolp(j) = surqsolp(j) * (1 - hru(j)%lumv%bmp_sp/100)

!! Nitrogen
	!! Particulate
	sedorgn(j) = sedorgn(j) * (1 - hru(j)%lumv%bmp_pn/100)
      !! Soluble
      surqno3(j) = surqno3(j) * (1 - hru(j)%lumv%bmp_sn/100)
	latno3(j) = latno3(j) * (1 - hru(j)%lumv%bmp_sn/100)

      return
      end subroutine smp_bmpfixed