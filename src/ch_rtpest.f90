      subroutine ch_rtpest
      
!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     this subroutine computes the daily stream pesticide balance
!!     (soluble and sorbed)     

      use channel_data_module
      use channel_module
      use sd_channel_module
      use basin_module, only : bsn_prm
      use pest_tempdecay_module, only: decay_temp_adjust
      use ch_pesticide_module
      use hydrograph_module, only : jrch, ht1, ht2, ch_stor
      use constituent_mass_module
      use pesticide_data_module

      implicit none
      
      external :: theta

      integer :: ipest = 0      !none                   |pesticide counter - sequential
      integer :: jpst = 0       !none                   |pesticide counter from data base
      integer :: ipseq = 0      !none                   |sequential basin pesticide number
      integer :: ipdb = 0       !none                   |seqential pesticide number of daughter pesticide
      integer :: imeta = 0      !none                   |pesticide metabolite counter
      real :: mol_wt_rto = 0.   !ratio                  |molecular weight ratio of duaghter to parent pesticide
      real :: pstin = 0.        !mg pst                 |total pesticide transported into reach during time step
      real :: kd = 0.           !(mg/kg)/(mg/L)         |koc * carbon
      real :: depth = 0.        !m             |depth of water in reach
      real :: chpstmass = 0.    !mg pst        |mass of pesticide in reach
      real :: sedpstmass = 0.   !mg pst        |mass of pesticide in bed sediment
      real :: fd2 = 0.          !units         |description
      real :: solmax = 0.       !units         |description
      real :: sedcon = 0.       !g/m^3         |sediment concentration 
      real :: tday = 0.         !none          |flow duration (fraction of 24 hr)
      real :: por = 0.          !none          |porosity of bottom sediments
      real :: pest_init = 0.    !mg            |amount of pesticide before decay
      real :: pest_end = 0.     !mg            |amount of pesticide after decay
      real :: decay_t = 0.      !none          |temperature-adjusted daily survival factor
      real :: temp_wat = 0.     !deg C         |channel water temperature proxy for decay
      real :: rto_out = 0.      !none          |ratio of outflow to sum of outflow and storage
      real :: metab_aq = 0.     !mg            |aqueous metabolite formed per parent
      real :: metab_ben = 0.    !mg            |benthic metabolite formed per parent

      !! zero outputs
      chpst_d(jrch) = chpstz
      
      !! initialize depth of water for pesticide calculations

      depth = rcurv%dep
      if (depth < 0.01) then
        depth = .01
      endif

      do ipest = 1, cs_db%num_pests
        jpst = cs_db%pest_num(ipest)

        !! volume of water entering reach and stored in reach
        wtrin = ht1%flo + ch_stor(jrch)%flo

        !! use channel water temperature (inflow + storage weighted) for aquatic/benthic decay
        if (wtrin > 1.e-6) then
          temp_wat = (ht1%flo * ht1%temp + ch_stor(jrch)%flo * ch_stor(jrch)%temp) / wtrin
        else if (ch_stor(jrch)%flo > 1.e-6) then
          temp_wat = ch_stor(jrch)%temp
        else
          temp_wat = ht1%temp
        end if
        if (temp_wat <= 0.) temp_wat = 0.1
         
        !! pesticide transported into reach during day
        pstin = hcs1%pest(ipest) 

        !! calculate mass of pesticide in reach
        chpstmass = pstin + ch_water(jrch)%pest(ipest)
      
        !! calculate mass of pesticide in bed sediment
        sedpstmass = ch_benthic(jrch)%pest(ipest)

        if (chpstmass + sedpstmass < 1.e-12) then
          ch_water(jrch)%pest(ipest) = 0.
          ch_benthic(jrch)%pest(ipest) = 0.
          !! BUG FIX
          !! zero outflow/accumulators so values from a previous channel
          !! are not forwarded downstream or into basin totals
          hcs2%pest(ipest) = 0.
          chpst%pest(ipest) = ch_pestbz
          cycle
        end if

        !!in-stream processes
        if (wtrin / 86400. > 1.e-9) then
          !! calculate sediment concentration
          sedcon = ht1%sed / wtrin * 1.e6
          
          !! set kd
          kd = pestdb(jpst)%koc * sd_ch(jrch)%carbon / 100.

          !! calculate fraction of soluble and sorbed pesticide
          !! BUG FIX
          !! kd is in mL/g (=L/kg), sedcon in mg/L (=g/m3)
          !! Product kd*sedcon has units (L/kg)*(mg/L) = mg/kg, NOT dimensionless.
          !! Need 1.e-6 to convert to (m3/g)*(g/m3) = dimensionless.
          !frsol = 1. / (1. + kd * sedcon)                  !! ORIGINAL (wrong units)
          frsol = 1. / (1. + kd * sedcon * 1.e-6)           !! CORRECTED
          frsrb = 1. - frsol

          !! Benthic partition: fd2 = pore water fraction in benthic sediment
          !! per unit bulk volume: dissolved = por*cw; sorbed = ch_bd*kd*cw
          !! fd2 = por / (por + kd*ch_bd); kd [cm3/g] * ch_bd [g/cm3] = dimensionless
          por = 1. - sd_ch(jrch)%ch_bd / 2.65
          !fd2 = 1. / (por + kd)                              !! ORIGINAL: numerator=1 and bd=1 assumed (wrong)
          fd2 = por / (por + kd * sd_ch(jrch)%ch_bd)         !! CORRECTED

          !! calculate flow duration
          tday = rttime / 24.0
          if (tday > 1.0) tday = 1.0
          !tday = 1.0

          !! calculate amount of pesticide that undergoes chemical or biological degradation on day in reach
          pest_init = chpstmass
          if (pest_init > 1.e-12) then
            if (bsn_prm%temp_decay == 1) then
              decay_t = decay_temp_adjust(pestdb(jpst)%aq_hlife, temp_wat)
              pest_end = chpstmass * (decay_t ** tday)
            else
              pest_end = chpstmass * (pestcp(jpst)%decay_a ** tday)
            end if
            chpstmass = pest_end
            chpst%pest(ipest)%react = pest_init - pest_end
            !! add decay to daughter pesticides
            do imeta = 1, pestcp(jpst)%num_metab
              ipseq = pestcp(jpst)%daughter(imeta)%num
              ipdb = cs_db%pest_num(ipseq)
              mol_wt_rto = pestdb(ipdb)%mol_wt / pestdb(jpst)%mol_wt
              !! BUG FIX: local variable avoids double-counting with multiple parents
              metab_aq = chpst%pest(ipest)%react * pestcp(jpst)%daughter(imeta)%aq_fr * mol_wt_rto
              chpst_d(jrch)%pest(ipseq)%metab = chpst_d(jrch)%pest(ipseq)%metab + metab_aq
              hcs1%pest(ipseq) = hcs1%pest(ipseq) + metab_aq
            end do
          end if

          !! calculate amount of pesticide that volatilizes from reach
          chpst%pest(ipest)%volat = pestdb(jpst)%aq_volat * frsol * chpstmass * tday / depth
          if (chpst%pest(ipest)%volat > frsol * chpstmass) then
            chpst%pest(ipest)%volat = frsol * chpstmass 
            chpstmass = chpstmass - chpst%pest(ipest)%volat
          else
            chpstmass = chpstmass - chpst%pest(ipest)%volat
          end if

          !! calculate amount of pesticide removed from reach by settling
          chpst%pest(ipest)%settle = pestdb(jpst)%aq_settle * frsrb * chpstmass * tday / depth
          if (chpst%pest(ipest)%settle >  frsrb * chpstmass) then
            chpst%pest(ipest)%settle = frsrb * chpstmass
            chpstmass = chpstmass - chpst%pest(ipest)%settle
          else
            chpstmass = chpstmass - chpst%pest(ipest)%settle
          end if
          sedpstmass = sedpstmass + chpst%pest(ipest)%settle

          !! calculate resuspension of pesticide in reach
          chpst%pest(ipest)%resus = pestdb(jpst)%aq_resus * sedpstmass * tday / depth
          if (chpst%pest(ipest)%resus > sedpstmass) then
            chpst%pest(ipest)%resus = sedpstmass
            sedpstmass = 0.
          else
            sedpstmass = sedpstmass - chpst%pest(ipest)%resus
          end if
          chpstmass = chpstmass + chpst%pest(ipest)%resus

          !! calculate diffusion of pesticide between reach and sediment
          chpst%pest(ipest)%difus = sd_ch(jrch)%aq_mix(ipest) * (fd2 * sedpstmass - frsol * chpstmass) * tday / depth
          if (chpst%pest(ipest)%difus > 0.) then
            if (chpst%pest(ipest)%difus > sedpstmass) then
              chpst%pest(ipest)%difus = sedpstmass
              sedpstmass = 0.
            else
              sedpstmass = sedpstmass - Abs(chpst%pest(ipest)%difus)
            end if
            chpstmass = chpstmass + Abs(chpst%pest(ipest)%difus)
          else
            if (Abs(chpst%pest(ipest)%difus) > chpstmass) then
              chpst%pest(ipest)%difus = -chpstmass
              chpstmass = 0.
            else
              chpstmass = chpstmass - Abs(chpst%pest(ipest)%difus)
            end if
            sedpstmass = sedpstmass + Abs(chpst%pest(ipest)%difus)
          end if

          !! calculate removal of pesticide from active sediment layer by burial
          chpst%pest(ipest)%bury = pestdb(jpst)%ben_bury * sedpstmass / pestdb(jpst)%ben_act_dep
          if (chpst%pest(ipest)%bury > sedpstmass) then
            chpst%pest(ipest)%bury = sedpstmass
            sedpstmass = 0.
          else
            sedpstmass = sedpstmass - chpst%pest(ipest)%bury
          end if

          !! verify that water concentration is at or below solubility
          solmax = pestdb(jpst)%solub * wtrin
          if (solmax < chpstmass * frsol) then
            sedpstmass = sedpstmass + (chpstmass * frsol - solmax)
            chpstmass = chpstmass - (chpstmass * frsol - solmax)
          end if
        
        else   
          !!insignificant flow
          sedpstmass = sedpstmass + chpstmass
          chpstmass = 0.
        end if

        !! benthic processes
        !! calculate loss of pesticide from bed sediments by reaction
        pest_init = sedpstmass
        if (pest_init > 1.e-12) then
          if (bsn_prm%temp_decay == 1) then
            decay_t = decay_temp_adjust(pestdb(jpst)%ben_hlife, temp_wat)
            pest_end = sedpstmass * decay_t
          else
            pest_end = sedpstmass * pestcp(jpst)%decay_b
          end if
          sedpstmass = pest_end
          chpst%pest(ipest)%react_bot = pest_init - pest_end
          !! add decay to daughter pesticides
          do imeta = 1, pestcp(jpst)%num_metab
            ipseq = pestcp(jpst)%daughter(imeta)%num
            ipdb = cs_db%pest_num(ipseq)
            mol_wt_rto = pestdb(ipdb)%mol_wt / pestdb(jpst)%mol_wt
            !! BUG FIX: local variable avoids double-counting with multiple parents
            metab_ben = chpst%pest(ipest)%react_bot * pestcp(jpst)%daughter(imeta)%ben_fr * mol_wt_rto
            chpst_d(jrch)%pest(ipseq)%metab_bot = chpst_d(jrch)%pest(ipseq)%metab_bot + metab_ben
            ch_benthic(jrch)%pest(ipseq) = ch_benthic(jrch)%pest(ipseq) + metab_ben
          end do
        end if

        !! set new pesticide mass of (in + store) after processes
        if (wtrin > 1.e-6) then
          hcs1%pest(ipest) = chpstmass
        else
          sedpstmass = sedpstmass + chpstmass
        end if
        ch_benthic(jrch)%pest(ipest) = sedpstmass

        !! calculate outflow and storage in water column
        rto_out = ht2%flo / (1.e-6 + ht2%flo + ch_stor(jrch)%flo)
        rto_out = Min (1., rto_out)
        hcs2%pest(ipest) = rto_out * chpstmass
        ch_water(jrch)%pest(ipest) = (1. - rto_out) * chpstmass
        
      end do

      return
      end subroutine ch_rtpest