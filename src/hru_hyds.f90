      subroutine hru_hyds
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine summarizes data for subbasins with multiple HRUs and
!!    prints the daily output.hru file

      use hru_module, only : cbodu, chl_a, clayld, doxq, hhsurfq, hru, ihru, lagyld, latq, qp_cms, percn, qday,  &
         sagyld, sanyld, silyld, sedminpa, sedminps, sedorgn, sedorgp, sepbtm, surqno3, surqsolp, tileno3,     &
         sedyld, latno3, qtile, tconc, satexq_chan, surqsalt, latqsalt, tilesalt, percsalt, urbqsalt,          & !rtb salt
         snomlt, wetqsalt,                                                                                     &
         surqcs, latqcs, tilecs, perccs, sedmcs, urbqcs, wetqcs !rtb cs
      use hydrograph_module
      use basin_module
      use time_module
      use constituent_mass_module
      use output_landscape_module
      use output_ls_pesticide_module
      use climate_module
      
      implicit none

      integer :: j                   !none          |same as ihru (hru number)
      real :: cnv_m3                 !              |
      real :: cnv_kg                 !              |
      real :: cnv_ppm                !              |
      integer :: iob                 !              |
      integer :: ihyd                !none          |counter
      integer :: ipest               !none          |counter
      integer :: ipath               !none          |counter 
      integer :: isalt               !none          |counter for salt ions (rtb salt)
      integer :: ics                 !none          |counter for constituents (rtb cs)
      integer :: istep               !none          |counter
      integer :: istep_bak           !none          |counter
      integer :: day_cur             !none          |counter
      integer :: day_next            !none          |counter
      integer :: tinc                !none          |
      integer :: inext_step
      
      j = ihru
      cnv_m3 = hru(j)%area_ha * 10.
      cnv_kg = hru(j)%area_ha
      
      !! assign reach loadings for subbasin
      !! zero out hydrograph storage locations
      iob = icmd 
      ob(icmd)%hd(1) = hz
      ob(icmd)%hd(2) = hz
      ob(icmd)%hd(3) = hz
      ob(icmd)%hd(4) = hz
      ob(icmd)%hd(5) = hz
      if (cs_db%num_tot > 0) then
        obcs(icmd)%hd(1) = hin_csz
        obcs(icmd)%hd(2) = hin_csz
        obcs(icmd)%hd(3) = hin_csz
        obcs(icmd)%hd(4) = hin_csz
        obcs(icmd)%hd(5) = hin_csz
      end if

      !! surface runoff hydrograph (3)
      ob(icmd)%hdsep%flo_surq = qday * cnv_m3            !!rtb gwflow - hydrograph separation (surface runoff)
      ob(icmd)%hdsep%flo_satexsw = satexq_chan * cnv_m3  !!rtb gwflow - hydrograph separation (saturation excess runoff)
      ob(icmd)%peakrate = qp_cms
      ob(icmd)%hd(3)%temp = 5. + .75 * w%tave         !!wtmp
      ob(icmd)%hd(3)%flo = qday * cnv_m3              !!qdr m3/d
      ob(icmd)%hd(3)%sed = sedyld(j)                  !!sedyld
      ob(icmd)%hd(3)%orgn = sedorgn(j) * cnv_kg       !!sedorgn
      ob(icmd)%hd(3)%sedp = (sedorgp(j) + sedminpa(j) +                 &
                        sedminps(j)) * cnv_kg         !!sedorgp & sedminps
      ob(icmd)%hd(3)%no3 = surqno3(j) * cnv_kg        !!surqno3 & latno3 & no3gw
      ob(icmd)%hd(3)%solp = (surqsolp(j) + hls_d(j)%tilelabp) * cnv_kg      !!surqsolp & sedminpa
      ob(icmd)%hd(3)%chla = chl_a(j) *cnv_kg          !!chl_a
      ob(icmd)%hd(3)%nh3 = 0.                         !! NH3
      ob(icmd)%hd(3)%no2 = 0.                         !! NO2
      if (ob(icmd)%hd(3)%flo > 0.01) then
        cnv_ppm = 1. / (1000. * ob(icmd)%hd(3)%flo)
      else
        cnv_ppm = 0.
      end if
      ob(icmd)%hd(3)%cbod = cbodu(j) * cnv_ppm        !!cbodu
      ob(icmd)%hd(3)%dox = doxq(j) * cnv_ppm          !!doxq & soxy

      ob(icmd)%hd(3)%san = sanyld(j)                  !! detached sand
      ob(icmd)%hd(3)%sil = silyld(j)                  !! detached silt
      ob(icmd)%hd(3)%cla = clayld(j)                  !! detached clay
      ob(icmd)%hd(3)%sag = sagyld(j)                  !! detached small aggregates
      ob(icmd)%hd(3)%lag = lagyld(j)                  !! detached large aggregates
      
      !set constituents
      do ipest = 1, cs_db%num_pests
        obcs(icmd)%hd(3)%pest(ipest) = (hpestb_d(j)%pest(ipest)%surq + hpestb_d(j)%pest(ipest)%sed) * cnv_kg
      end do
      do ipath = 1, cs_db%num_paths
        obcs(icmd)%hd(3)%path(ipath) = 0
      end do
      do isalt = 1, cs_db%num_salts !rtb salt
			  obcs(icmd)%hd(3)%salt(isalt) = (surqsalt(j,isalt)+urbqsalt(j,isalt)+wetqsalt(j,isalt)) * cnv_kg !kg of each salt ion
      enddo
      do ics = 1, cs_db%num_cs !rtb cs
			  obcs(icmd)%hd(3)%cs(ics) = (surqcs(j,ics)+sedmcs(j,ics)+urbqcs(j,ics)+wetqcs(j,ics)) * cnv_kg !kg of each constituent (surface runoff + attached)
      enddo
      
      !recharge hydrograph (2)
      ob(icmd)%hd(2)%flo = sepbtm(j) * cnv_m3           !! recharge flow
      ob(icmd)%hd(2)%no3 = percn(j) * cnv_kg            !! recharge nitrate
      !set constituents
      do ipest = 1, cs_db%num_pests
        obcs(icmd)%hd(2)%pest(ipest) = hpestb_d(j)%pest(ipest)%perc * cnv_kg
      end do
      do ipath = 1, cs_db%num_paths
        obcs(icmd)%hd(2)%path(ipath) = 0
      end do
      do isalt = 1, cs_db%num_salts !rtb salt
        obcs(icmd)%hd(2)%salt(isalt) = percsalt(j,isalt) * cnv_kg !kg of each salt ion
      enddo
      do ics = 1, cs_db%num_cs !rtb cs
        obcs(icmd)%hd(2)%cs(ics) = perccs(j,ics) * cnv_kg !kg of each constituent
      enddo
      
      !lateral soil flow hydrograph (4)
      ob(icmd)%hd(4)%flo = latq(j) * cnv_m3                 !! lateral flow
      ob(icmd)%hdsep%flo_latq = latq(j) * cnv_m3            !!rtb gwflow - hydrograph separation
      ob(icmd)%hd(4)%no3 = latno3(j) * cnv_kg
      !set constituents
      do ipest = 1, cs_db%num_pests
        obcs(icmd)%hd(4)%pest(ipest) = hpestb_d(j)%pest(ipest)%latq * cnv_kg
      end do
      do ipath = 1, cs_db%num_paths
        obcs(icmd)%hd(4)%path(ipath) = 0
      end do
      do isalt = 1, cs_db%num_salts !rtb salt
        obcs(icmd)%hd(4)%salt(isalt) = latqsalt(j,isalt) * cnv_kg !kg of each salt ion
      enddo
      do ics = 1, cs_db%num_cs !rtb cs
        obcs(icmd)%hd(4)%cs(ics) = latqcs(j,ics) * cnv_kg !kg of each constituent
      enddo
      
      !tile flow hydrograph (5)
      ob(icmd)%hd(5)%flo = qtile * cnv_m3               !! tile flow
      ob(icmd)%hd(5)%no3 = tileno3(j) * cnv_kg          !! tile flow nitrate 
      !set constituents
      do ipest = 1, cs_db%num_pests
        obcs(icmd)%hd(5)%pest(ipest) = hpestb_d(j)%pest(ipest)%tileq * cnv_kg
      end do
      do ipath = 1, cs_db%num_paths
        obcs(icmd)%hd(5)%path(ipath) = 0.
      end do
      
      !water temperature calculations
      !percolate temperature
      ob(icmd)%hd(2)%temp = w_temp%sur_lat
      !surface runoff temperature
      if (snomlt > 0.1) then
        ob(icmd)%hd(3)%temp = w_temp%sno_mlt
      else
        ob(icmd)%hd(3)%temp = w_temp%sur_lat
      end if
      !lateral soil flow temperature
      ob(icmd)%hd(4)%temp = w_temp%sur_lat
      !tile flow temperature
      ob(icmd)%hd(5)%temp = w_temp%sur_lat
      do isalt = 1, cs_db%num_salts !rtb salt
        obcs(icmd)%hd(5)%salt(isalt) = tilesalt(j,isalt) * cnv_kg !kg of each salt ion
      enddo
      do ics = 1, cs_db%num_cs !rtb cs
        obcs(icmd)%hd(5)%cs(ics) = tilecs(j,ics) * cnv_kg !kg of each constituent
      enddo
      
      !sum to obtain the total outflow hydrograph (1)
      do ihyd = 3, 5
        ob(icmd)%hd(1) = ob(icmd)%hd(1) + ob(icmd)%hd(ihyd)
      end do
      
      !set constituents
      do ipest = 1, cs_db%num_pests
        obcs(icmd)%hd(1)%pest(ipest) = obcs(icmd)%hd(3)%pest(ipest) + obcs(icmd)%hd(4)%pest(ipest) +    &
                                                                      obcs(icmd)%hd(5)%pest(ipest)
      end do
      do ipath = 1, cs_db%num_paths
        obcs(icmd)%hd(1)%path(ipath) = 0
      end do
      do isalt = 1, cs_db%num_salts !rtb salt
        obcs(icmd)%hd(1)%salt(isalt) = obcs(icmd)%hd(3)%salt(isalt) + obcs(icmd)%hd(4)%salt(isalt) +    &  !total = surface runoff + lateral flow + tile flow
                                                                      obcs(icmd)%hd(5)%salt(isalt)
      enddo
      do ics = 1, cs_db%num_cs !rtb cs
        obcs(icmd)%hd(1)%cs(ics) = obcs(icmd)%hd(3)%cs(ics) + obcs(icmd)%hd(4)%cs(ics) +    &  !total = surface runoff + lateral flow + tile flow
                                                              obcs(icmd)%hd(5)%cs(ics)
      enddo
      
      !! set subdaily hydrographs
      !! set previous and next days for adding previous and translating to next
      day_cur = ob(icmd)%day_cur
      day_next = day_cur + 1
      if (day_next > ob(icmd)%day_max) day_next = 1
          
      if (time%step > 1) then
        if (bsn_cc%gampt == 1) then
          !! hhsurfq from sq_greenampt - mm
          ob(icmd)%hyd_flo(day_cur,:) = ob(icmd)%hyd_flo(day_cur,:) + hhsurfq(j,:) * cnv_m3
          
          !! translate the hydrogrpah by time of concentration - no attenuation
          ob(icmd)%hyd_flo(day_next,:) = 0.
          if (tconc(j) * 60. > time%dtm) then
            tinc = int (tconc(j) * 60. / time%dtm)
            tinc = min1 (float(time%step), float(tinc))
            !! move to next days hydrograph
            do istep = 1, tinc
              inext_step = time%step - tinc + istep
              ob(icmd)%hyd_flo(day_next,istep) = ob(icmd)%hyd_flo(day_cur,inext_step)
            end do
            !! shift current day hydrograph
            do istep = 1, time%step
              istep_bak = time%step - istep + 1
              if (istep_bak <= tinc) then
                ob(icmd)%hyd_flo(day_cur,istep_bak) = 0.
              else
                ob(icmd)%hyd_flo(day_cur,istep_bak) = ob(icmd)%hyd_flo(day_cur,istep_bak-tinc)
              end if
            end do
          end if
        else
          !! use unit hydrograph and daily runoff
          call flow_hyd_ru_hru (ob(icmd)%day_cur, ob(icmd)%hd(3)%flo, ob(icmd)%hd(4)%flo,     &
                                        ob(icmd)%hd(5)%flo, ob(icmd)%uh, ob(icmd)%hyd_flo)
        end if
      else
        !! set to total runoff needed when summing incoming in command
        ob(icmd)%hyd_flo(day_cur,1) = ob(icmd)%hd(1)%flo
      end if

      return   
      end subroutine hru_hyds