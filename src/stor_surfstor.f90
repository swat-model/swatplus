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
      use hru_module, only : sedyld, surf_bs, ihru, hhsurf_bs, hhsedy, sanyld, silyld,          &
         clayld, sagyld, lagyld, sedorgn, sedorgp, surqno3, surqsolp, sedminpa, sedminps, brt,  &
         surqsalt,urbqsalt,wetqsalt,                                                            & !rtb salt
         surqcs,sedmcs,urbqcs,wetqcs                                                              !rtb cs
      
      implicit none
 
      integer :: j         !none          |HRU number
      integer :: k         !none          |counter
      real :: sedprev      !              | 

      j = ihru

      if (time%step == 1) then
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

      !rtb salt
      if(cs_db%num_salts > 0) then
        !surface runoff
        surf_bs(20,j) = surf_bs(20,j) + surqsalt(j,1) !so4
        surf_bs(21,j) = surf_bs(21,j) + surqsalt(j,2) !ca
        surf_bs(22,j) = surf_bs(22,j) + surqsalt(j,3) !mg
        surf_bs(23,j) = surf_bs(23,j) + surqsalt(j,4) !na
        surf_bs(24,j) = surf_bs(24,j) + surqsalt(j,5) !k
        surf_bs(25,j) = surf_bs(25,j) + surqsalt(j,6) !cl
        surf_bs(26,j) = surf_bs(26,j) + surqsalt(j,7) !co3
        surf_bs(27,j) = surf_bs(27,j) + surqsalt(j,8) !hco3
        !urban runoff
        surf_bs(28,j) = surf_bs(28,j) + urbqsalt(j,1) !so4
        surf_bs(29,j) = surf_bs(29,j) + urbqsalt(j,2) !ca
        surf_bs(30,j) = surf_bs(30,j) + urbqsalt(j,3) !mg
        surf_bs(31,j) = surf_bs(31,j) + urbqsalt(j,4) !na
        surf_bs(32,j) = surf_bs(32,j) + urbqsalt(j,5) !k
        surf_bs(33,j) = surf_bs(33,j) + urbqsalt(j,6) !cl
        surf_bs(34,j) = surf_bs(34,j) + urbqsalt(j,7) !co3
        surf_bs(35,j) = surf_bs(35,j) + urbqsalt(j,8) !hco3
        !wetland runoff
        surf_bs(36,j) = surf_bs(36,j) + wetqsalt(j,1) !so4
        surf_bs(37,j) = surf_bs(37,j) + wetqsalt(j,2) !ca
        surf_bs(38,j) = surf_bs(38,j) + wetqsalt(j,3) !mg
        surf_bs(39,j) = surf_bs(39,j) + wetqsalt(j,4) !na
        surf_bs(40,j) = surf_bs(40,j) + wetqsalt(j,5) !k
        surf_bs(41,j) = surf_bs(41,j) + wetqsalt(j,6) !cl
        surf_bs(42,j) = surf_bs(42,j) + wetqsalt(j,7) !co3
        surf_bs(43,j) = surf_bs(43,j) + wetqsalt(j,8) !hco3
			endif
      
      !rtb cs
      if(cs_db%num_cs > 0) then
        !surface runoff
        surf_bs(44,j) = surf_bs(44,j) + surqcs(j,1) !seo4
        surf_bs(45,j) = surf_bs(45,j) + surqcs(j,2) !seo3
        surf_bs(46,j) = surf_bs(46,j) + surqcs(j,3) !boron
        !sediment runoff
        surf_bs(47,j) = surf_bs(47,j) + sedmcs(j,1) !seo4
        surf_bs(48,j) = surf_bs(48,j) + sedmcs(j,2) !seo3
        surf_bs(49,j) = surf_bs(49,j) + sedmcs(j,3) !boron
        !urban runoff
        surf_bs(50,j) = surf_bs(50,j) + urbqcs(j,1) !seo4
        surf_bs(51,j) = surf_bs(51,j) + urbqcs(j,2) !seo3
        surf_bs(52,j) = surf_bs(52,j) + urbqcs(j,3) !born
        !wetland runoff
        surf_bs(53,j) = surf_bs(53,j) + wetqcs(j,1) !seo4
        surf_bs(54,j) = surf_bs(54,j) + wetqcs(j,2) !seo3
        surf_bs(55,j) = surf_bs(55,j) + wetqcs(j,3) !born
			endif
      
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

      !rtb salt
      if(cs_db%num_salts > 0) then
        !surface runoff
        surqsalt(j,1) = surf_bs(20,j) * brt(j) !so4
        surqsalt(j,2) = surf_bs(21,j) * brt(j) !ca
        surqsalt(j,3) = surf_bs(22,j) * brt(j) !mg
        surqsalt(j,4) = surf_bs(23,j) * brt(j) !na
        surqsalt(j,5) = surf_bs(24,j) * brt(j) !k
        surqsalt(j,6) = surf_bs(25,j) * brt(j) !cl
        surqsalt(j,7) = surf_bs(26,j) * brt(j) !co3
        surqsalt(j,8) = surf_bs(27,j) * brt(j) !hco3
        !urban runoff
        urbqsalt(j,1) = surf_bs(28,j) * brt(j) !so4
        urbqsalt(j,2) = surf_bs(29,j) * brt(j) !ca
        urbqsalt(j,3) = surf_bs(30,j) * brt(j) !mg
        urbqsalt(j,4) = surf_bs(31,j) * brt(j) !na
        urbqsalt(j,5) = surf_bs(32,j) * brt(j) !k
        urbqsalt(j,6) = surf_bs(33,j) * brt(j) !cl
        urbqsalt(j,7) = surf_bs(34,j) * brt(j) !co3
        urbqsalt(j,8) = surf_bs(35,j) * brt(j) !hco3
        !wetland runoff
        wetqsalt(j,1) = surf_bs(36,j) * brt(j) !so4
        wetqsalt(j,2) = surf_bs(37,j) * brt(j) !ca
        wetqsalt(j,3) = surf_bs(38,j) * brt(j) !mg
        wetqsalt(j,4) = surf_bs(39,j) * brt(j) !na
        wetqsalt(j,5) = surf_bs(40,j) * brt(j) !k
        wetqsalt(j,6) = surf_bs(41,j) * brt(j) !cl
        wetqsalt(j,7) = surf_bs(42,j) * brt(j) !co3
        wetqsalt(j,8) = surf_bs(43,j) * brt(j) !hco3
			endif
      
      !rtb cs
      if(cs_db%num_cs > 0) then
        !surface runoff
        surqcs(j,1) = surf_bs(44,j) * brt(j) !seo4
        surqcs(j,2) = surf_bs(45,j) * brt(j) !seo3
        surqcs(j,3) = surf_bs(46,j) * brt(j) !boron
        !sediment runoff
        sedmcs(j,1) = surf_bs(47,j) * brt(j) !seo4
        sedmcs(j,2) = surf_bs(48,j) * brt(j) !seo3
        sedmcs(j,3) = surf_bs(49,j) * brt(j) !boron
        !urban runoff
        urbqcs(j,1) = surf_bs(50,j) * brt(j) !seo4
        urbqcs(j,2) = surf_bs(51,j) * brt(j) !seo3
        urbqcs(j,3) = surf_bs(52,j) * brt(j) !boron
        !wetland runoff
        wetqcs(j,1) = surf_bs(53,j) * brt(j) !seo4
        wetqcs(j,2) = surf_bs(54,j) * brt(j) !seo3
        wetqcs(j,3) = surf_bs(55,j) * brt(j) !boron
			endif
      
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

      !rtb salt
      if(cs_db%num_salts > 0) then
        !surface runoff
        surf_bs(20,j) = surf_bs(20,j) - surqsalt(j,1) !so4
        surf_bs(21,j) = surf_bs(21,j) - surqsalt(j,2) !ca
        surf_bs(22,j) = surf_bs(22,j) - surqsalt(j,3) !mg
        surf_bs(23,j) = surf_bs(23,j) - surqsalt(j,4) !na
        surf_bs(24,j) = surf_bs(24,j) - surqsalt(j,5) !k
        surf_bs(25,j) = surf_bs(25,j) - surqsalt(j,6) !cl
        surf_bs(26,j) = surf_bs(26,j) - surqsalt(j,7) !co3
        surf_bs(27,j) = surf_bs(27,j) - surqsalt(j,8) !hco3
        !urban runoff
        surf_bs(28,j) = surf_bs(28,j) - urbqsalt(j,1) !so4
        surf_bs(29,j) = surf_bs(29,j) - urbqsalt(j,2) !ca
        surf_bs(30,j) = surf_bs(30,j) - urbqsalt(j,3) !mg
        surf_bs(31,j) = surf_bs(31,j) - urbqsalt(j,4) !na
        surf_bs(32,j) = surf_bs(32,j) - urbqsalt(j,5) !k
        surf_bs(33,j) = surf_bs(33,j) - urbqsalt(j,6) !cl
        surf_bs(34,j) = surf_bs(34,j) - urbqsalt(j,7) !co3
        surf_bs(35,j) = surf_bs(35,j) - urbqsalt(j,8) !hco3
        !wetland runoff
        surf_bs(36,j) = surf_bs(36,j) - wetqsalt(j,1) !so4
        surf_bs(37,j) = surf_bs(37,j) - wetqsalt(j,2) !ca
        surf_bs(38,j) = surf_bs(38,j) - wetqsalt(j,3) !mg
        surf_bs(39,j) = surf_bs(39,j) - wetqsalt(j,4) !na
        surf_bs(40,j) = surf_bs(40,j) - wetqsalt(j,5) !k
        surf_bs(41,j) = surf_bs(41,j) - wetqsalt(j,6) !cl
        surf_bs(42,j) = surf_bs(42,j) - wetqsalt(j,7) !co3
        surf_bs(43,j) = surf_bs(43,j) - wetqsalt(j,8) !hco3
			endif
      
      !rtb cs
      if(cs_db%num_cs > 0) then
        !surface runoff
        surf_bs(44,j) = surf_bs(44,j) - surqcs(j,1) !seo4
        surf_bs(45,j) = surf_bs(45,j) - surqcs(j,2) !seo3
        surf_bs(46,j) = surf_bs(46,j) - surqcs(j,3) !boron
        !sediment runoff
        surf_bs(47,j) = surf_bs(47,j) - sedmcs(j,1) !seo4
        surf_bs(48,j) = surf_bs(48,j) - sedmcs(j,2) !seo3
        surf_bs(49,j) = surf_bs(49,j) - sedmcs(j,3) !boron
        !urban runoff
        surf_bs(50,j) = surf_bs(50,j) - urbqcs(j,1) !seo4
        surf_bs(51,j) = surf_bs(51,j) - urbqcs(j,2) !seo3
        surf_bs(52,j) = surf_bs(52,j) - urbqcs(j,3) !boron
        !wetland runoff
        surf_bs(53,j) = surf_bs(53,j) - wetqcs(j,1) !seo4
        surf_bs(54,j) = surf_bs(54,j) - wetqcs(j,2) !seo3
        surf_bs(55,j) = surf_bs(55,j) - wetqcs(j,3) !boron
			endif
      
      return
      end subroutine stor_surfstor