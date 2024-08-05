      subroutine ch_rtpest
      
!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     this subroutine computes the daily stream pesticide balance
!!     (soluble and sorbed)     

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    chpst_conc(:) |mg/(m**3)     |initial pesticide concentration in reach
!!    chpst_koc(:)  |m**3/g        |pesticide partition coefficient between
!!                                 |water and sediment in reach
!!    chpst_mix(:)  |m/day         |mixing velocity (diffusion/dispersion) for
!!                                 |pesticide in reach
!!    chpst_rea(:)  |1/day         |pesticide reaction coefficient in reach
!!    chpst_rsp(:)  |m/day         |resuspension velocity in reach for pesticide
!!                                 |sorbed to sediment
!!    chpst_stl(:)  |m/day         |settling velocity in reach for pesticide
!!                                 |sorbed to sediment
!!    chpst_vol(:)  |m/day         |pesticide volatilization coefficient in 
!!                                 |reach
!!    drift(:)      |kg            |amount of pesticide drifting onto main
!!                                 |channel in subbasin
!!    rchdep        |m             |depth of flow on day
!!    rchwtr        |m^3 H2O       |water stored in reach at beginning of day
!!    sedpst_rea(:) |1/day         |pesticide reaction coefficient in river bed
!!                                 |sediment
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bury        |mg pst        |loss of pesticide from active sediment layer
!!                               |by burial
!!    difus       |mg pst        |diffusion of pesticide from sediment to reach
!!    reactb      |mg pst        |amount of pesticide in sediment that is lost
!!                               |through reactions
!!    reactw      |mg pst        |amount of pesticide in reach that is lost
!!                               |through reactions
!!    resuspst    |mg pst        |amount of pesticide moving from sediment to
!!                               |reach due to resuspension
!!    setlpst     |mg pst        |amount of pesticide moving from water to
!!                               |sediment due to settling
!!    solpesto    |mg pst/m^3    |soluble pesticide concentration in outflow
!!                               |on day
!!    sorpesto    |mg pst/m^3    |sorbed pesticide concentration in outflow
!!                               |on day
!!    volatpst    |mg pst        |amount of pesticide in reach lost by
!!                               |volatilization
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    chpstmass   |mg pst        |mass of pesticide in reach
!!    depth       |m             |depth of water in reach
!!    fd2         |
!!    frsol       |none          |fraction of pesticide in reach that is soluble
!!    frsrb       |none          |fraction of pesticide in reach that is sorbed
!!    jrch        |none          |reach number
!!    pstin       |mg pst        |total pesticide transported into reach
!!                               |during time step
!!    sedcon      |g/m^3         |sediment concentration
!!    sedpstmass  |mg pst        |mass of pesticide in bed sediment
!!    solpstin    |mg pst        |soluble pesticide entering reach during 
!!                               |time step
!!    sorpstin    |mg pst        |sorbed pesticide entering reach during
!!                               |time step
!!    tday        |days          |flow duration
!!    wtrin       |m^3 H2O       |volume of water entering reach during time
!!                               |step
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Abs

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      
      use channel_data_module
      use channel_module
      use sd_channel_module
      use ch_pesticide_module
      use hydrograph_module, only : jrch, ht1, ht2, ch_stor
      use constituent_mass_module
      use pesticide_data_module

      implicit none
      
      integer :: ipest          !none                   |pesticide counter - sequential
      integer :: jpst           !none                   |pesticide counter from data base
      integer :: ipseq          !none                   |sequential basin pesticide number
      integer :: ipdb           !none                   |seqential pesticide number of daughter pesticide
      integer :: imeta          !none                   |pesticide metabolite counter
      real :: mol_wt_rto        !ratio                  |molecular weight ratio of duaghter to parent pesticide
      real :: pstin             !mg pst                 |total pesticide transported into reach during time step
      real :: kd                !(mg/kg)/(mg/L)         |koc * carbon
      real :: depth             !m             |depth of water in reach
      real :: chpstmass         !mg pst        |mass of pesticide in reach
      real :: sedpstmass        !mg pst        |mass of pesticide in bed sediment
      real :: fd2               !units         |description
      real :: solmax            !units         |description
      real :: sedcon            !g/m^3         |sediment concentration 
      real :: tday              !none          |flow duration (fraction of 24 hr)
      real :: por               !none          |porosity of bottom sediments
      real :: pest_init         !mg            |amount of pesticide before decay
      real :: pest_end          !mg            |amount of pesticide after decay
      real :: rto_out           !none          |ratio of outflow to sum of outflow and storage

      !! zero outputs
      chpst_d(jrch) = chpstz
      
      !! initialize depth of water for pesticide calculations
      if (rchdep < 0.01) then
        depth = .01
      else
        depth = rchdep
      endif

      do ipest = 1, cs_db%num_pests
        jpst = cs_db%pest_num(ipest)

        !! volume of water entering reach and stored in reach
        wtrin = ht1%flo + ch_stor(jrch)%flo
         
        !! pesticide transported into reach during day
        pstin = hcs1%pest(ipest) 

        !! calculate mass of pesticide in reach
        chpstmass = pstin + ch_water(jrch)%pest(ipest)
      
        !! calculate mass of pesticide in bed sediment
        sedpstmass = ch_benthic(jrch)%pest(ipest)

        if (chpstmass + sedpstmass < 1.e-12) then
          ch_water(jrch)%pest(ipest) = 0.
          ch_benthic(jrch)%pest(ipest) = 0.
        end if
        if (chpstmass + sedpstmass < 1.e-12) cycle

        !!in-stream processes
        if (wtrin / 86400. > 1.e-9) then
          !! calculate sediment concentration
          sedcon = ht1%sed / wtrin * 1.e6
          
          !! set kd
          kd = pestdb(jpst)%koc * sd_ch(jrch)%carbon / 100.

          !! calculate fraction of soluble and sorbed pesticide
          frsol = 1. / (1. + kd * sedcon)
          frsrb = 1. - frsol

          !! ASSUME DENSITY=2.6E6; KD2=KD1
          por = 1. - sd_ch(jrch)%ch_bd / 2.65
          fd2 = 1. / (por + kd)

          !! calculate flow duration
          tday = rttime / 24.0
          if (tday > 1.0) tday = 1.0
          !tday = 1.0

          !! calculate amount of pesticide that undergoes chemical or biological degradation on day in reach
          pest_init = chpstmass
          if (pest_init > 1.e-12) then
            pest_end = chpstmass * (pestcp(jpst)%decay_a * tday)
            chpstmass = pest_end
            chpst%pest(ipest)%react = pest_init - pest_end
            !! add decay to daughter pesticides
            do imeta = 1, pestcp(jpst)%num_metab
              ipseq = pestcp(jpst)%daughter(imeta)%num
              ipdb = cs_db%pest_num(ipseq)
              mol_wt_rto = pestdb(ipdb)%mol_wt / pestdb(jpst)%mol_wt
              chpst_d(jrch)%pest(ipseq)%metab = chpst_d(jrch)%pest(ipseq)%metab + chpst%pest(ipest)%react *     &
                                           pestcp(jpst)%daughter(imeta)%aq_fr * mol_wt_rto
              hcs1%pest(ipseq) = hcs1%pest(ipseq) + chpst_d(jrch)%pest(ipseq)%metab
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
          pest_end = sedpstmass * pestcp(jpst)%decay_b
          sedpstmass = pest_end
          chpst%pest(ipest)%react_bot = pest_init - pest_end
          !! add decay to daughter pesticides
          do imeta = 1, pestcp(jpst)%num_metab
            ipseq = pestcp(jpst)%daughter(imeta)%num
            ipdb = cs_db%pest_num(ipseq)
            mol_wt_rto = pestdb(ipdb)%mol_wt / pestdb(jpst)%mol_wt
            chpst_d(jrch)%pest(ipseq)%metab_bot = chpst_d(jrch)%pest(ipseq)%metab + chpst%pest(ipest)%react_bot *     &
                                           pestcp(jpst)%daughter(imeta)%ben_fr * mol_wt_rto
            ch_benthic(jrch)%pest(ipseq) = ch_benthic(jrch)%pest(ipseq) + chpst_d(jrch)%pest(ipseq)%metab
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