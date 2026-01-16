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
      
      external :: pl_rootfr
   
      integer :: j = 0                 !none           |HRU number
      integer :: k = 0                 !none           |counter
      integer :: npl                   !none           |counter
      integer, intent (in) :: jj       !none           |counter
      integer, intent (in) :: iplant   !               |plant number xwalked from hlt_db()%plant and plants.plt
      integer :: ly = 0                !none           |soil layer

      j = jj
      ipl = iplant

      !! update root fractions in each layer
      call pl_rootfr
      
      !! add above ground biomass to surface residue pools
      pl_mass(j)%rsd(ipl) = pl_mass(j)%rsd(ipl) + pl_mass(j)%ab_gr(ipl)
          
      !! update total surface residue pool
      pl_mass(j)%rsd_tot = orgz
      do npl = 1, pcom(j)%npl
        pl_mass(j)%rsd_tot = pl_mass(j)%rsd_tot + pl_mass(j)%rsd(npl)
      end do
      
      !! add dead roots to soil residue pools
      do ly = 1, soil(j)%nly
        soil1(j)%pl(ipl)%rsd(ly) = soil1(j)%pl(ipl)%rsd(ly) + soil(j)%ly(ly)%rtfr * pl_mass(j)%root(ipl)
      end do
      
      !! sum total community masses
      pl_mass(j)%tot_com = pl_mass(j)%tot_com - pl_mass(j)%tot(ipl)
      pl_mass(j)%ab_gr_com = pl_mass(j)%ab_gr_com - pl_mass(j)%ab_gr(ipl)
      pl_mass(j)%leaf_com = pl_mass(j)%leaf_com - pl_mass(j)%leaf(ipl)
      pl_mass(j)%stem_com = pl_mass(j)%stem_com - pl_mass(j)%stem(ipl)
      pl_mass(j)%seed_com = pl_mass(j)%seed_com - pl_mass(j)%seed(ipl)
      pl_mass(j)%root_com = pl_mass(j)%root_com - pl_mass(j)%root(ipl)
       
      !! add plant carbon for printing
      hrc_d(j)%plant_surf_c = hrc_d(j)%plant_surf_c + pl_mass(j)%ab_gr(ipl)%c
      hrc_d(j)%plant_root_c = hrc_d(j)%plant_root_c + pl_mass(j)%root(ipl)%c
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