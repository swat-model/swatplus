      !rate laws for Se chemical reduction (seo4 --> seo3) --------------------------------------------------------------------------------
      subroutine se_reactions_soil(j,jj,conc_rg,k_rg,k_slope)

      use cs_data_module

      implicit none

      integer :: j
      integer :: jj ! unused
      integer :: k_slope
      integer :: kk = 0
      real :: conc_rg
      real :: k_rg
      real :: cseo4 = 0.
      real :: cseo3 = 0.
      real :: no3inhib = 0.
      real :: seo4red = 0.
      real :: seo3red = 0.
      real :: dseo4 = 0.
      real :: dseo3 = 0.
      real :: dno3 = 0.
      real :: cno3 = 0.
      real :: o2 = 0.
      real :: o2red = 0.
      real :: no3red = 0.
      real :: yseo4_o2 = 0.
      real :: yseo4_no3 = 0.
      real :: se_prod_o2 = 0.
      real :: se_prod_no3 = 0.
      real :: ko2a = 0.
      real :: kno3 = 0.
      real :: sseratio = 0.
      dimension conc_rg(3), k_rg(4,3)

      !! suppress unused variable warning
      if (jj < 0) continue

      !get concentration of SeO4 and SeO3
      cseo4 = conc_rg(1)
      cseo3 = conc_rg(2)
      cno3 = conc_rg(3)

      !concentration of dissolved oxygen (O2) (specified in selenium input file)
      o2 = cs_rct_soil(j)%oxy_soil
       
      !rate law for selenate reduction
      no3inhib = cs_rct_soil(j)%se_ino3 / (cs_rct_soil(j)%se_ino3 + cno3)
      seo4red = cs_rct_soil(j)%kseo4 * cseo4 * no3inhib
      
      !rate law for selenite reduction
      seo3red = cs_rct_soil(j)%kseo3 * cseo3 * no3inhib
      
      !rate law for oxygen reduction and nitrate reduction, in the presence of shale
      yseo4_o2 = 315.84 / 224.0
      yseo4_no3 = 789.6 / 196.0
      no3red = 0.
      se_prod_o2 = 0.
      se_prod_no3 = 0.
      do kk=1,num_geol_shale
        !reduction of o2
        ko2a = cs_rct_soil(j)%ko2a(kk)
        o2red = ko2a * o2 * cs_rct_soil(j)%shale(kk)
        !reduction of no3
        kno3 = cs_rct_soil(j)%kno3a(kk)
        no3red = no3red + (kno3 * cno3 * cs_rct_soil(j)%shale(kk))
        !total selenium released from the shale (via oxidation)
        sseratio = cs_rct_soil(j)%sseratio(kk)
        se_prod_o2 = se_prod_o2 + (o2red * yseo4_o2 * (1/sseratio))
        se_prod_no3 = se_prod_no3 + (no3red * yseo4_no3 * (1/sseratio))
      enddo
       
      !change in seo4 and seo3
      dseo4 = (se_prod_o2 + se_prod_no3) - seo4red
      dseo3 = seo4red - seo3red !all of reduced seo4 becomes seo3
      dno3 = no3red * (-1)
       
      !store change in concentrations
      k_rg(k_slope,1) = dseo4
      k_rg(k_slope,2) = dseo3
      k_rg(k_slope,3) = dno3

      return
      
      end subroutine se_reactions_soil