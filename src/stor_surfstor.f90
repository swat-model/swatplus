      subroutine stor_surfstor

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine stores and lags sediment and nutrients in surface runoff

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    brt(:)        |none         |fraction of surface runoff that takes
!!                                |one day or less to reach the subbasin
!!                                |outlet

!!    ihru          |none         |HRU number
!!    sedminpa(:)   |kg P/ha      |amount of active mineral phosphorus sorbed to
!!                                |sediment in surface runoff in HRU for day
!!    sedminps(:)   |kg P/ha      |amount of stable mineral phosphorus sorbed to
!!                                |sediment in surface runoff in HRU for day
!!    surf_bs(2,:)  |metric tons  |amount of sediment yield lagged over one
!!                                |day
!!    surf_bs(3,:)  |kg N/ha      |amount of organic nitrogen loading lagged
!!                                |over one day
!!    surf_bs(4,:)  |kg P/ha      |amount of organic phosphorus loading lagged
!!                                |over one day
!!    surf_bs(5,:)  |kg N/ha      |amount of nitrate loading in surface runoff
!!                                |lagged over one day
!!    surf_bs(6,:)  |kg P/ha      |amount of soluble phosphorus loading lagged
!!                                |over one day
!!    surf_bs(7,:)  |kg P/ha      |amount of active mineral phosphorus loading 
!!                                |lagged over one day
!!    surf_bs(8,:)  |kg P/ha      |amount of stable mineral phosphorus loading 
!!                                |lagged over one day
!!    surf_bs(9,:)  |# colonies/ha|amount of less persistent bacteria in 
!!                                |solution lagged over one day
!!    surf_bs(10,:) |# colonies/ha|amount of persistent bacteria in 
!!                                |solution lagged over one day
!!    surf_bs(11,:) |# colonies/ha|amount of less persistent bacteria
!!                                |sorbed lagged over one day
!!    surf_bs(12,:) |# colonies/ha|amount of persistent bacteria
!!                                |sorbed lagged over one day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sedminpa(:)   |kg P/ha      |amount of active mineral phosphorus sorbed to
!!                                |sediment in surface runoff in HRU for day
!!    sedminps(:)   |kg P/ha      |amount of stable mineral phosphorus sorbed to
!!                                |sediment in surface runoff in HRU for day
!!    surf_bs(2,:)  |metric tons  |amount of sediment yield lagged over one
!!                                |day
!!    surf_bs(3,:)  |kg N/ha      |amount of organic nitrogen loading lagged
!!                                |over one day
!!    surf_bs(4,:)  |kg P/ha      |amount of organic phosphorus loading lagged
!!                                |over one day
!!    surf_bs(5,:)  |kg N/ha      |amount of nitrate loading in surface runoff
!!                                |lagged over one day
!!    surf_bs(6,:)  |kg P/ha      |amount of soluble phosphorus loading lagged
!!                                |over one day
!!    surf_bs(7,:)  |kg P/ha      |amount of active mineral phosphorus loading 
!!                                |lagged over one day
!!    surf_bs(8,:)  |kg P/ha      |amount of stable mineral phosphorus loading 
!!                                |lagged over one day
!!    surf_bs(9,:)  |# colonies/ha|amount of less persistent bacteria in 
!!                                |solution lagged over one day
!!    surf_bs(10,:) |# colonies/ha|amount of persistent bacteria in 
!!                                |solution lagged over one day
!!    surf_bs(11,:) |# colonies/ha|amount of less persistent bacteria
!!                                |sorbed lagged over one day
!!    surf_bs(12,:) |# colonies/ha|amount of persistent bacteria
!!                                |sorbed lagged over one day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use septic_data_module
      use basin_module
      use time_module
      use constituent_mass_module
      use output_ls_pesticide_module
      use hru_module, only : hru, sedyld, surf_bs, ihru, hhsurf_bs, hhsedy, sanyld, silyld,     &
         clayld, sagyld, lagyld, sedorgn, sedorgp, surqno3, surqsolp, sedminpa, sedminps, brt 
      
      implicit none
 
      integer :: j         !none          |HRU number
      integer :: k         !none          |counter
      integer :: icmd      !              |
      real :: sedprev      !              | 

      j = ihru

      if (bsn_cc%event<3) then
         surf_bs(2,j) = Max(1.e-9, surf_bs(2,j) + sedyld(j))
         sedyld(j) = surf_bs(2,j) * brt(j)
         surf_bs(2,j) = surf_bs(2,j) - sedyld(j)

      else !subdaily time steps, Jaehak Jeong 2011
      	sedprev = hhsurf_bs(2,j,time%step)

	   do k=1,time%step

	!! Left-over (previous timestep) + inflow (current  timestep)
          hhsurf_bs(2,j,k) = Max(0., sedprev + hhsedy(j,k))
	
	!! new estimation of sediment reaching the main channel
          hhsedy(j,k) = hhsurf_bs(2,j,k) * brt(j)! tons
 	    hhsurf_bs(2,j,k) = hhsurf_bs(2,j,k) - hhsedy(j,k)
	  
	!! lagged at the end of time step  
	    sedprev = hhsurf_bs(2,j,k)
      surf_bs(2,j) = Max(1.e-9, surf_bs(2,j) + sedyld(j))

	   end do

	!! daily total sediment yield from the HRU
	   sedyld(j) = sum(hhsedy(j,:))
      endif
      
      surf_bs(13,j) = Max(1.e-6, surf_bs(13,j) + sanyld(j))
      surf_bs(14,j) = Max(1.e-6, surf_bs(14,j) + silyld(j))
      surf_bs(15,j) = Max(1.e-6, surf_bs(15,j) + clayld(j))
      surf_bs(16,j) = Max(1.e-6, surf_bs(16,j) + sagyld(j))
      surf_bs(17,j) = Max(1.e-6, surf_bs(17,j) + lagyld(j))

      surf_bs(3,j) = Max(1.e-9, surf_bs(3,j) + sedorgn(j))
      surf_bs(4,j) = Max(1.e-9, surf_bs(4,j) + sedorgp(j))
      surf_bs(5,j) = Max(1.e-9, surf_bs(5,j) + surqno3(j))
      surf_bs(6,j) = Max(1.e-9, surf_bs(6,j) + surqsolp(j))
      surf_bs(7,j) = Max(1.e-9, surf_bs(7,j) + sedminpa(j))
      surf_bs(8,j) = Max(1.e-9, surf_bs(8,j) + sedminps(j))

 !!     sedyld(j) = surf_bs(2,j) * brt(j)  <--line of code in x 2. fixes sedyld low prob

      sanyld(j) = surf_bs(13,j) * brt(j)
      silyld(j) = surf_bs(14,j) * brt(j)
      clayld(j) = surf_bs(15,j) * brt(j)
      sagyld(j) = surf_bs(16,j) * brt(j)
      lagyld(j) = surf_bs(17,j) * brt(j)

      sedorgn(j) = surf_bs(3,j) * brt(j)
      sedorgp(j) = surf_bs(4,j) * brt(j)
      surqno3(j) = surf_bs(5,j) * brt(j)
      surqsolp(j) = surf_bs(6,j) * brt(j)
      sedminpa(j) = surf_bs(7,j) * brt(j)
      sedminps(j) = surf_bs(8,j) * brt(j)

      surf_bs(2,j) = surf_bs(2,j) - sedyld(j)
      surf_bs(13,j) = surf_bs(13,j) - sanyld(j)
      surf_bs(14,j) = surf_bs(14,j) - silyld(j)
      surf_bs(15,j) = surf_bs(15,j) - clayld(j)
      surf_bs(16,j) = surf_bs(16,j) - sagyld(j)
      surf_bs(17,j) = surf_bs(17,j) - lagyld(j)

      surf_bs(3,j) = surf_bs(3,j) - sedorgn(j)
      surf_bs(4,j) = surf_bs(4,j) - sedorgp(j)
      surf_bs(5,j) = surf_bs(5,j) - surqno3(j)
      surf_bs(6,j) = surf_bs(6,j) - surqsolp(j)
      surf_bs(7,j) = surf_bs(7,j) - sedminpa(j)
      surf_bs(8,j) = surf_bs(8,j) - sedminps(j)

      return
      end subroutine stor_surfstor