      subroutine mgt_killop (jj, iplant)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs the kill operation

      use basin_module
      use organic_mineral_mass_module
      use hru_module, only : ipl
      use soil_module
      use plant_module
      use constituent_mass_module
      use carbon_module
      
      implicit none
   
      integer :: j                     !none           |HRU number
      integer :: k                     !none           |counter
      integer, intent (in) :: jj       !none           |counter
      integer, intent (in) :: iplant   !               |plant number xwalked from hlt_db()%plant and plants.plt
      integer :: ly                    !none           |soil layer

      j = jj
      ipl = iplant

      !! update root fractions in each layer
      call pl_rootfr
      
      !! allocate dead roots, N, P to soil layers
	  do ly = 1, soil(j)%nly
	      soil1(j)%rsd(ly) = soil(j)%ly(ly)%rtfr * pl_mass(j)%root(ipl) + soil1(j)%rsd(ly)
      end do
      
      !! add above ground mass to residue pool
      rsd1(j)%tot(1) = pl_mass(j)%ab_gr(ipl) + rsd1(j)%tot(1)
      !! add plant carbon for printing
      hrc_d(j)%plant_c = hrc_d(j)%plant_c + pl_mass(j)%ab_gr(ipl)%c
      hpc_d(j)%drop_c = hpc_d(j)%drop_c + pl_mass(j)%ab_gr(ipl)%c

      !! zero all plant mass
      pl_mass(j)%tot(ipl) = plt_mass_z
      pl_mass(j)%ab_gr(ipl) = plt_mass_z
      pl_mass(j)%leaf(ipl) = plt_mass_z
      pl_mass(j)%stem(ipl) = plt_mass_z
      pl_mass(j)%seed(ipl) = plt_mass_z
      pl_mass(j)%root(ipl) = plt_mass_z
      
      do k = 1, cs_db%num_pests
        cs_soil(j)%ly(1)%pest(k) = cs_soil(j)%ly(1)%pest(k) + cs_pl(j)%pl_in(ipl)%pest(k)   &
                                                             + cs_pl(j)%pl_on(ipl)%pest(k)
        cs_pl(j)%pl_in(ipl)%pest(k) = 0.
        cs_pl(j)%pl_on(ipl)%pest(k) = 0.
      end do

	  !! reset plant variables
      pcom(j)%plg(ipl) = plgz
      pcom(j)%plm(ipl) = plmz
      pcom(j)%plstr(ipl) = plstrz
      !! can't reset entire plcur - harv_num can't be zero'd
      pcom(j)%plcur(ipl)%gro = "n"
      pcom(j)%plcur(ipl)%idorm = "n"
      pcom(j)%plcur(ipl)%phuacc = 0.
      pcom(j)%plcur(ipl)%curyr_mat = 1

      return
      end subroutine mgt_killop