      subroutine pest_decay
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates degradation of pesticide in the soil and on 
!!    the plants

      use pesticide_data_module
      use hru_module, only : ihru
      use basin_module, only : bsn_prm
      use pest_tempdecay_module, only: decay_temp_adjust
      use constituent_mass_module
      use soil_module
      use plant_module
      use output_ls_pesticide_module
      
      implicit none 
      
      integer :: j = 0           !none     |hru number
      integer :: k = 0           !none     |seqential pesticide number being simulated
      integer :: ipl = 0         !none     |plant number
      integer :: ipest_db = 0    !none     |pesticide number from pesticide data base
      integer :: l = 0           !none     |layer number
      integer :: ipseq = 0       !none     |sequential basin pesticide number
      integer :: ipdb = 0        !none     |sequential pesticide number of daughter pesticide
      integer :: imeta = 0       !none     |pesticide metabolite counter
      real :: mol_wt_rto = 0.    !ratio    |molecular weight ratio of duaghter to parent pesticide
      real :: pest_init = 0.     !kg/ha    |amount of pesticide present at beginning of day
      real :: pest_end = 0.      !kg/ha    |amount of pesticide present at end of day
      real :: pst_decay = 0.     !kg/ha    |amount of pesticide decay in soil layer during day
      real :: pst_decay_s = 0.   !kg/ha    |amount of pesticide decay in soil profile during day
      real :: metab_decay = 0.   !kg/ha    |amount of metabolite decay in soil layer during day
      real :: metab_fol = 0.     !kg/ha    |amount of foliar metabolite formed per plant (v19)
      real :: decay_t = 0.       !none     |temperature-adjusted daily survival factor

      j = ihru

      if (cs_db%num_pests == 0) return
     
      do k = 1, cs_db%num_pests
        hpestb_d(j)%pest(k)%decay_s = 0.
        hpestb_d(j)%pest(k)%decay_f = 0.
        ipest_db = cs_db%pest_num(k)
        if (ipest_db > 0) then
          pst_decay_s = 0.
          !! calculate degradation in soil
          do l = 1, soil(j)%nly
            pest_init = cs_soil(j)%ly(l)%pest(k)
            if (pest_init > 1.e-12) then
              if (bsn_prm%temp_decay == 1) then
                decay_t = decay_temp_adjust(pestdb(ipest_db)%soil_hlife, soil(j)%phys(l)%tmp)
              else
                decay_t = pestcp(ipest_db)%decay_s
              end if
              pest_end = pest_init * decay_t
              cs_soil(j)%ly(l)%pest(k) = pest_end
              pst_decay = (pest_init - pest_end)
              pst_decay_s = pst_decay_s + pst_decay
              !! add decay to daughter pesticides
              do imeta = 1, pestcp(ipest_db)%num_metab
                ipseq = pestcp(ipest_db)%daughter(imeta)%num
                ipdb = cs_db%pest_num(ipseq)
                mol_wt_rto = pestdb(ipdb)%mol_wt / pestdb(ipest_db)%mol_wt
                metab_decay = pst_decay * pestcp(ipest_db)%daughter(imeta)%soil_fr * mol_wt_rto
                hpestb_d(j)%pest(ipseq)%metab_s = hpestb_d(j)%pest(ipseq)%metab_s + metab_decay
                cs_soil(j)%ly(l)%pest(ipseq) = cs_soil(j)%ly(l)%pest(ipseq) + metab_decay
              end do
            end if
          end do
          hpestb_d(j)%pest(k)%decay_s = pst_decay_s

          !! calculate degradation on plant foliage
          !! adjust foliar pesticide for wash off
          do ipl = 1, pcom(j)%npl
            pest_init = cs_pl(j)%pl_on(ipl)%pest(k)
            if (pest_init > 1.e-12) then
              if (bsn_prm%temp_decay == 1) then
                ! Use top-layer soil temperature as surface/foliar proxy in this branch.
                decay_t = decay_temp_adjust(pestdb(ipest_db)%foliar_hlife, soil(j)%phys(1)%tmp)
              else
                decay_t = pestcp(ipest_db)%decay_f
              end if
              pest_end = pest_init * decay_t
              cs_pl(j)%pl_on(ipl)%pest(k) = pest_end
              !! BUG FIX: use += to accumulate foliar decay across plants
              hpestb_d(j)%pest(k)%decay_f = hpestb_d(j)%pest(k)%decay_f + (pest_init - pest_end)
              !! add decay to daughter pesticides
              do imeta = 1, pestcp(ipest_db)%num_metab
                ipseq = pestcp(ipest_db)%daughter(imeta)%num
                ipdb = cs_db%pest_num(ipseq)
                mol_wt_rto = pestdb(ipdb)%mol_wt / pestdb(ipest_db)%mol_wt
                !! BUG FIX: use local metab_fol instead of accumulated hpestb_d%metab_f
                !! Previously, cs_pl was updated with the TOTAL accumulated metab_f,
                !! causing mass duplication for multi-plant HRUs
                metab_fol = (pest_init - pest_end) * pestcp(ipest_db)%daughter(imeta)%foliar_fr * mol_wt_rto
                hpestb_d(j)%pest(ipseq)%metab_f = hpestb_d(j)%pest(ipseq)%metab_f + metab_fol
                cs_pl(j)%pl_on(ipl)%pest(ipseq) = cs_pl(j)%pl_on(ipl)%pest(ipseq) + metab_fol
              end do
            end if 
          end do

        end if
      end do
      
      return
      end subroutine pest_decay