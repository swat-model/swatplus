      subroutine res_pest (jres)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes the lake hydrologic pesticide balance.

      use reservoir_data_module
      use reservoir_module
      use res_pesticide_module
      use hydrograph_module, only : res, ob, ht2
      use constituent_mass_module
      use pesticide_data_module
      use water_body_module
      
      implicit none      
      
      real :: tpest1                !mg pst        |amount of pesticide in water
      real :: tpest2                !mg pst        |amount of pesticide in benthic sediment
      real :: kd                    !(mg/kg)/(mg/L) |koc * carbon
      real :: fd1                   !              |frac of soluble pesticide in water column
      real :: fd2                   !              |frac of sorbed pesticide in water column
      real :: fp1                   !              |frac of soluble pesticide in benthic column
      real :: fp2                   !              |frac of sorbed pesticide in benthic column
      real :: depth                 !              |average depth of reservoir
      real :: bedvol                !m^3           |volume of river bed sediment
      real :: solpesto              !mg pst        |soluble pesticide transported out of reservoir
      real :: sorpesto              !mg pst        |sorbed pesticide transported out of reservoir
      real :: sedmass_watervol      !kg/L or t/m3  |sediment mass divided by water volume in water and benthic
      real :: pest_init             !mg            |amount of pesticide before decay
      real :: pest_end              !mg            |amount of pesticide after decay
      real :: mol_wt_rto        !ratio      |molecular weight ratio of duaghter to parent pesticide
      integer :: ipest_db       !none       |pesticide number from pesticide data base
      integer :: ipseq          !none       |sequential basin pesticide number
      integer :: ipdb           !none       |seqential pesticide number of daughter pesticide
      integer :: imeta          !none       |pesticide metabolite counter
      integer :: jres               !none          |reservoir number  
      integer :: ipst               !none          |counter
      integer :: icmd               !none          |
      integer :: jsed               !none          |counter
      integer :: idb                !none          |

      if (res(jres)%flo > 1.) then
          
      do ipst = 1, cs_db%num_pests
        icmd = res_ob(jres)%ob
        idb = ob(icmd)%props
        ipest_db = cs_db%pest_num(ipst)
        jsed = res_dat(idb)%sed
        respst_d(jres)%pest(ipst)%tot_in = obcs(icmd)%hin(1)%pest(ipst)
        tpest1 = obcs(icmd)%hin(1)%pest(ipst) + res_water(jres)%pest(ipst)
        bedvol = 1000. * res_wat_d(jres)%area_ha * pestdb(ipest_db)%ben_act_dep + .01
        tpest2 = res_benthic(jres)%pest(ipst) * bedvol

        !! calculate average depth of reservoir
        depth = res(jres)%flo / (res_wat_d(jres)%area_ha * 10000.)
        !! sor conc/sol conc = Koc * frac_oc = Kd -> (sor mass/mass sed) / (sol mass/mass water) = Kd
        !! -> sor mass/sol mass = Kd * (kg sed)/(L water) --> sol mass/tot mass = 1 / (1 + Kd * (kg sed)/(L water))
        !! water column --> kg sed/L water = t/m3 = t / (m3 - (t * m3/t)) --> sedvol = sed/particle density(2.65)
        sedmass_watervol = (res(jres)%sed) / (res(jres)%flo - (res(jres)%sed / 2.65))
        kd = pestdb(ipest_db)%koc * res_sed(jsed)%carbon / 100.
        fd1 = 1. / (1. + kd * sedmass_watervol)
        fd1 = amin1 (1., fd1)
        fp1 = 1. - fd1
        !! assume; fraction organic = 1%;\; por=0.8; density=2.6 t/m^3
        !! benthic layer --> kg sed/L water = t/m3 = bd (t sed/m3 total) / por --> por*total gives volume of water
        sedmass_watervol = res_sed(jsed)%bd / (1. - res_sed(jsed)%bd / 2.65)
        fd2 = 1. / (1. + kd * sedmass_watervol)
        fd2 = amin1 (1., fd2)
        fp2 = 1. - fd2
        
        fd2 = 1. / (.8 + .026 * kd)
        fd2 = amin1 (1., fd2)
        fp2 = 1. - fd2

        !! determine pesticide lost through reactions in water layer
        pest_init = tpest1
        if (pest_init > 1.e-12) then
          pest_end = tpest1 * pestcp(ipest_db)%decay_a
          tpest1 = pest_end
          respst_d(jres)%pest(ipst)%react = pest_init - pest_end
          !! add decay to daughter pesticides
          do imeta = 1, pestcp(ipest_db)%num_metab
            ipseq = pestcp(ipest_db)%daughter(imeta)%num
            ipdb = cs_db%pest_num(ipseq)
            mol_wt_rto = pestdb(ipdb)%mol_wt / pestdb(ipest_db)%mol_wt
            respst_d(jres)%pest(ipseq)%metab = respst_d(jres)%pest(ipseq)%metab + respst_d(jres)%pest(ipst)%react *     &
                                           pestcp(ipest_db)%daughter(imeta)%soil_fr * mol_wt_rto
            res_water(jres)%pest(ipseq) = res_water(jres)%pest(ipseq) + respst_d(jres)%pest(ipseq)%metab
          end do
        end if
        
        !! determine pesticide lost through volatilization
        volatpst = pestdb(ipest_db)%aq_volat * fd1 * tpest1 / depth
        if (volatpst > tpest1) then
          volatpst = tpest1
          tpest1 = 0.
        else
          tpest1 = tpest1 - volatpst
        end if
        respst_d(jres)%pest(ipst)%volat = volatpst

        !! determine amount of pesticide settling to sediment layer
        setlpst = pestdb(ipest_db)%aq_settle * fp1 * tpest1 / depth
        if (setlpst > tpest1) then
          setlpst = tpest1
          tpest1 = 0.
          tpest2 = tpest2 + setlpst
        else
          tpest1 = tpest1 - setlpst
          tpest2 = tpest2 + setlpst
        end if
        respst_d(jres)%pest(ipst)%settle = setlpst

        !! determine pesticide resuspended into lake water
        resuspst = pestdb(ipest_db)%aq_resus * tpest2 / pestdb(ipest_db)%ben_act_dep
        if (resuspst > tpest2) then
          resuspst = tpest2
          tpest2 = 0.
          tpest1 = tpest1 + resuspst
        else
          tpest2 = tpest2 - resuspst
          tpest1 = tpest1 + resuspst
        end if
        respst_d(jres)%pest(ipst)%resus = resuspst

        !! determine pesticide diffusing from sediment to water
        difus = res_ob(jres)%aq_mix(ipst) *                                 &                                
              (fd2 * tpest2 / pestdb(ipest_db)%ben_act_dep - fd1 * tpest1 / depth)
        if (difus > 0.) then
          if (difus > tpest2) then
            difus = tpest2
            tpest2 = 0.
          else
            tpest2 = tpest2 - Abs(difus)
          end if
          tpest1 = tpest1 + Abs(difus)
        else
          if (Abs(difus) > tpest1) then
            difus = -tpest1
            tpest1 = 0.
          else
            tpest1 = tpest1 - Abs(difus)
          end if
          tpest2 = tpest2 + Abs(difus)
        end if
        respst_d(jres)%pest(ipst)%difus = difus

        !! determine pesticide lost from sediment by reactions
        pest_init = tpest2
        if (pest_init > 1.e-12) then
          pest_end = tpest2 * pestcp(ipest_db)%decay_b
          tpest2 = pest_end
          respst_d(jres)%pest(ipst)%react_bot = pest_init - pest_end
          !! add decay to daughter pesticides
          do imeta = 1, pestcp(ipest_db)%num_metab
            ipseq = pestcp(ipest_db)%daughter(imeta)%num
            ipdb = cs_db%pest_num(ipseq)
            mol_wt_rto = pestdb(ipdb)%mol_wt / pestdb(ipest_db)%mol_wt
            respst_d(jres)%pest(ipseq)%metab = respst_d(jres)%pest(ipseq)%metab + respst_d(jres)%pest(ipst)%react *     &
                                           pestcp(ipest_db)%daughter(imeta)%soil_fr * mol_wt_rto
            res_benthic(jres)%pest(ipseq) = res_benthic(jres)%pest(ipseq) + respst_d(jres)%pest(ipseq)%metab
          end do
        end if

        !! determine pesticide lost from sediment by burial
        bury = pestdb(ipest_db)%ben_bury * tpest2 / pestdb(ipest_db)%ben_act_dep
        if (bury > tpest2) then
          bury = tpest2
          tpest2 = 0.
        else
          tpest2 = tpest2 - bury
        end if
        respst_d(jres)%pest(ipst)%bury = bury

        !! calculate soluble pesticide transported out of reservoir
        solpesto = ht2%flo * fd1 * tpest1 / res(jres)%flo
        if (solpesto > tpest1) then
          solpesto = tpest1
          tpest1 = 0.
        else
          tpest1 = tpest1 - solpesto
        end if

        !! calculate sorbed pesticide transported out of reservoir
        sorpesto = ht2%flo * fp1 * tpest1 / res(jres)%flo
        if (sorpesto > tpest1) then
          sorpesto = tpest1
          tpest1 = 0.
        else
          tpest1 = tpest1 - sorpesto
        end if
        respst_d(jres)%pest(ipst)%sol_out = solpesto
        respst_d(jres)%pest(ipst)%sor_out = sorpesto

        !! update concentration of pesticide in lake water and sediment
        if (tpest1 < 1.e-10) tpest1 = 0.0
        if (tpest2 < 1.e-10) tpest2 = 0.0
        res_water(jres)%pest(ipst) = tpest1
        res_benthic(jres)%pest(ipst) = tpest2
        
        !! set outgoing pesticide for routing
        hcs2%pest(ipst) = solpesto + sorpesto

      end do    ! ipst = 1, cs_db%num_pests
      end if    ! res(jres)%flo > 1.

      return
      end subroutine res_pest