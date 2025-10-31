      subroutine sq_greenampt

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    Predicts daily runoff given breakpoint precipitation and snow melt
!!    using the Green & Ampt technique

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    wst(:)%weat%ts(:)  |mm H2O        |precipitation for the time step during day
!!    swtrg(:)    |none          |rainfall event flag:
!!                               |  0: no rainfall event over midnight
!!                               |  1: rainfall event over midnight
!!    wfsh(:)     |mm            |average capillary suction at wetting front
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hhqday(:)   |mm H2O        |surface runoff generated each hour of day
!!                               |in HRU
!!    surfq(:)    |mm H2O        |surface runoff for the day in HRU
!!    swtrg(:)    |none          |rainfall event flag:
!!                               |  0: no rainfall event over midnight
!!                               |  1: rainfall event over midnight
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Sum, Exp, Real, Mod

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use urban_data_module
      use climate_module
      use basin_module
      use hydrograph_module
      use hru_module, only : hru, swtrg, hhqday, ubnrunoff, hhsurfq, surfq, cnday, wfsh, &
          ihru, urb_abstinit, rateinf_prev, pet_day,cnday, wrt, smx
      use soil_module
      use time_module

      
      implicit none
      
      integer :: j = 0                           !none          |HRU number
      integer :: k = 0                           !none          |counter
      real :: adj_hc = 0.                        !mm/hr         |adjusted hydraulic conductivity
      real :: dthet = 0.                         !mm/mm         |initial moisture deficit
      real :: soilw = 0.                         !mm H2O        |amount of water in soil profile
      real :: psidt = 0.                         !mm            |suction at wetting front*initial moisture 
                                                 !              |deficit
      real :: tst = 0.                           !mm H2O        |test value for cumulative infiltration
      real :: f1 = 0.                            !mm H2O        |test value for cumulative infiltration
      integer :: ulu = 0
      real, dimension (0:time%step+1) :: cuminf    !mm H2O        |cumulative infiltration for day
      real, dimension (0:time%step+1) :: cumr      !mm H2O        |cumulative rainfall for day
      real, dimension (0:time%step+1) :: excum     !mm H2O        |cumulative runoff for day
      real, dimension (0:time%step+1) :: exinc     !mm H2O        |runoff for time step
      real, dimension (0:time%step+1) :: rateinf   !mm/hr         |infiltration rate for time step
      real, dimension (0:time%step+1) :: rintns    !mm/hr         |rainfall intensity
		real:: swdt, sw_fac, r2
      !! array location #1 is for last time step of prev day
       j = ihru
       ulu = hru(j)%luse%urb_lu
       
       !! reset values for day
       cumr = 0.
       cuminf = 0.
       excum = 0.
       exinc = 0.
       rateinf = 0.
       rintns = 0.

       dthet = 0.
       if (swtrg(j) == 1) then
         swtrg(j) = 0
         dthet = 0.001 * soil(j)%phys(1)%por * 0.95
         rateinf(1) = rateinf_prev(j)
         rateinf_prev(j) = 0.
       else
         if (soil(j)%sw >= soil(j)%sumfc) then
           soilw = 0.999 * soil(j)%sumfc
         else
           soilw = soil(j)%sw
         end if
         dthet = (1. - soilw / soil(j)%sumfc) * soil(j)%phys(1)%por * 0.95
         rateinf(1) = 2000.
       end if
       psidt = dthet * wfsh(j)
   !    rintns(1) = 60. * w%ts(1) / Real(time%dtm) 
		 
       do k = 1, time%step
			 
          !! Update effective hydraulic conductivity
          adj_hc = (56.82 * soil(j)%phys(1)%k ** 0.286) / (1. + 0.051 * Exp(0.062 * cnday(j))) - 2.
          if (adj_hc <= 0.) adj_hc = 0.001
			 
         !! calculate total amount of rainfall during day for time step
           cumr(k) = cumr(k-1) + w%ts(k)
         
         !! and rainfall intensity for time step
         rintns(k) = 60. * w%ts(k) / Real(time%dtm) 
         !! if rainfall intensity is less than infiltration rate everything will infiltrate
         if (rateinf(k) >= rintns(k)) then
             cuminf(k) = cuminf(k-1) + w%ts(k)
             if (excum(k-1) > 0.) then
               excum(k) = excum(k-1)
               !exinc(k) = 0.
             else
               excum(k) = 0.
               !exinc(k) = 0.
             end if
         else
          !! if rainfall intensity is greater than infiltration rate
          !! find cumulative infiltration for time step by successive substitution
           tst = adj_hc * Real(time%dtm) / 60.  
           do
               f1 = cuminf(k-1) + adj_hc * Real(time%dtm) / 60. +        &
                     psidt * Log((tst + psidt)/(cuminf(k-1) + psidt))
           
             if (Abs(f1 - tst) <= 0.001) then
               cuminf(k) = f1
               excum(k) = max(0.,cumr(k) - cuminf(k)) !=cumulative rainfall - cumulative infiltration for the day
               exinc(k) = max(0.,excum(k) - excum(k-1)) !runoff during this time step
               hhqday(j,k) = exinc(k)
               exit
             else
               tst = f1
             end if
           end do
         end if  
         !! Urban Impervious cover 
         if (ulu > 0) then
           if(rintns(k) > 0.017) then 
             !! effective pcp is > 0.017 mm/min (or 4/100 inches/hr)
             !! the potential for initial dabstraction from paved surface is less than the user input initial dabstraction
             urb_abstinit(j) = max(0., urb_abstinit(j) - w%ts(k))
           else
             !! the potential for initial dabstraction from paved surface increases based on evaporation
             urb_abstinit(j) = min(bsn_prm%urb_init_abst, urb_abstinit(j) + pet_day / time%step)
           end if
           !runoff from previous area
           hhqday(j,k) = hhqday(j,k) * (1. - urbdb(ulu)%fcimp) 
           
           !runoff from impervious area with initial abstraction
           !ubnrunoff(k-1) = (wst(iwst)%weat%ts(k) - urb_abstinit(j)) * urbdb(ulu)%fcimp   orig
            ubnrunoff(k) = (wst(iwst)%weat%ts(k) - urb_abstinit(j)) * urbdb(ulu)%fcimp   !! nbs
           !if (ubnrunoff(k)<0) ubnrunoff(k) = 0.   orig
            if (ubnrunoff(k)<0) ubnrunoff(k) = 0.   !! nbs
         end if

         !! daily total runoff
         hhsurfq(j,k) = hhqday(j,k) + ubnrunoff(k)
         surfq(j) = surfq(j) + hhsurfq(j,k) 

         !! calculate new rate of infiltration
         rateinf(k+1) = adj_hc * (psidt / (cuminf(k) + 1.e-6) + 1.)
			
			!update daily curve number value for the time step -Jaehak 2025
			swdt = soil(j)%sw + cuminf(k) 
			sw_fac = wrt(1,j) - wrt(2,j) * swdt
         if (sw_fac < -20.) sw_fac = -20.
         if (sw_fac > 20.) sw_fac = 20.
         
         if ((swdt + Exp(sw_fac)) > 0.001) then
           r2 = smx(j) * (1. - swdt / (swdt + Exp(sw_fac)))
         else
           r2 = smx(j)
         end if
         
         if (soil(j)%phys(2)%tmp <= 0.) r2 = smx(j) * (1. - Exp(- bsn_prm%cn_froz * r2))
         r2 = Max(3.,r2)
         
         cnday(j) = 25400. / (r2 + 254.)
         
       end do    !end of time%step loop
          
       if (Sum(w%ts) > 12.) then
         swtrg(j) = 1
         rateinf_prev(j) = rateinf(time%step+1)
       end if

       return
!*** tu Wunused-label:  5000 format(//,"Excess rainfall calculation for day ",i3," of year ",   &  
              !i4," for sub-basin",i4,".",/)
!*** tu Wunused-label:  5001 format(t2,"Time",t9,"Incremental",t22,"Cumulative",t35,"Rainfall",   &
             !t45,"Infiltration",t59,"Cumulative",t71,"Cumulative",t82,     &  
             !"Incremental",/,t2,"Step",t10,"Rainfall",t23,"Rainfall",      &   
             !t35,"Intensity",t49,"Rate",t58,"Infiltration",t73,"Runoff",   & 
             !t84,"Runoff",/,t12,"(mm)",t25,"(mm)",t36,"(mm/h)",t48,        &    
             !"(mm/h)",t62,"(mm)",t74,"(mm)",t85,"(mm)",/)
!*** tu Wunused-label:  5002 format(i5,t12,f5.2,t24,f6.2,t36,f6.2,t47,f7.2,t61,f6.2,t73,f6.2,     &  
             !t84,f6.2)
      end subroutine sq_greenampt