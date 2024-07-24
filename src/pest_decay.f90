      subroutine pest_decay
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates degradation of pesticide in the soil and on 
!!    the plants

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    decay_f(:)    |none          |exponential of the rate constant for
!!                                 |degradation of the pesticide on foliage
!!    decay_s(:)    |none          |exponential of the rate constant for
!!                                 |degradation of the pesticide in soil
!!    ihru          |none          |HRU number

      use pesticide_data_module
      use hru_module, only : ihru
      use constituent_mass_module
      use soil_module
      use plant_module
      use output_ls_pesticide_module
      
      implicit none 
      
      integer :: j               !none     |hru number
      integer :: k               !none     |seqential pesticide number being simulated
      integer :: ipl             !none     |plant number
      integer :: ipest_db        !none     |pesticide number from pesticide data base
      integer :: l               !none     |layer number
      integer :: ipseq           !none     |sequential basin pesticide number
      integer :: ipdb            !none     |seqential pesticide number of daughter pesticide
      integer :: imeta           !none     |pesticide metabolite counter
      real :: mol_wt_rto         !ratio    |molecular weight ratio of duaghter to parent pesticide
      real :: pest_init          !kg/ha    |amount of pesticide present at beginning of day
      real :: pest_end           !kg/ha    |amount of pesticide present at end of day
      real :: pst_decay          !kg/ha    |amount of pesticide decay in soil layer during day
      real :: pst_decay_s        !kg/ha    |amount of pesticide decay in soil profile during day
      real :: metab_decay        !kg/ha    |amount of metabolite decay in soil layer during day

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
              pest_end = pest_init * pestcp(ipest_db)%decay_s
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
              pest_end = pest_init * pestcp(ipest_db)%decay_f
              cs_pl(j)%pl_on(ipl)%pest(k) = pest_end
              hpestb_d(j)%pest(k)%decay_f = pest_init - pest_end
              !! add decay to daughter pesticides
              do imeta = 1, pestcp(ipest_db)%num_metab
                ipseq = pestcp(ipest_db)%daughter(imeta)%num
                ipdb = cs_db%pest_num(ipseq)
                mol_wt_rto = pestdb(ipdb)%mol_wt / pestdb(ipest_db)%mol_wt
                hpestb_d(j)%pest(ipseq)%metab_f = hpestb_d(j)%pest(ipseq)%metab_f + (pest_init - pest_end) *     &
                                           pestcp(ipest_db)%daughter(imeta)%soil_fr * mol_wt_rto
                cs_pl(j)%pl_on(ipl)%pest(ipseq) = cs_pl(j)%pl_on(ipl)%pest(ipseq) + hpestb_d(j)%pest(ipseq)%metab_f
              end do
            end if 
          end do
        end if
      end do
      
      return
      end subroutine pest_decay