      subroutine wallo_canal (iwallo, itrn, ican)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    Routes water through a wallo canal: computes outflow, applies loss,
!!    and distributes canal seepage to aquifer (gwflow grid cells or 1-D aquifer).

      use water_allocation_module
      use hydrograph_module
      use constituent_mass_module
      use basin_module, only : bsn_cc
      use aquifer_module
      use gwflow_module, only : gw_canal_flag, gw_canl_div_cell, &
          gw_canal_ncells_div, gw_state, gw_hyd_ss, gw_hyd_ss_mo, &
          gw_hyd_ss_yr

      implicit none

      integer, intent (in):: iwallo     !water allocation object number
      integer, intent (in) :: ican      !canal object number
      integer, intent (in) :: itrn      !water transfer object number
      integer :: iaq, iaqu_ob, ic, cell_id
      real :: canal_loss_vol            !m3   |total canal loss volume
      real :: aqu_loss_vol              !m3   |loss volume to a specific aquifer
      real :: aqu_loss_mm               !mm   |loss converted to mm for aquifer
      real :: total_length              !m    |total canal length across all connected cells
      real :: cell_frac                 !-    |fraction of loss to this cell (by length)
      real :: cell_seep                 !m3   |seepage volume to this cell

      !! compute outflow from canal using decision table or simple lag
      if (canal(ican)%dtbl == "null") then
        !! simple drawdown days
        wallod_out(iwallo)%trn(itrn)%trn_flo = canal_om_stor(ican)%flo / canal(ican)%ddown_days
      else
        !! decision table to condition outflow from canal
      end if

      !! outflow is the fraction of the withdrawal from the canal
      canal_om_out(ican) = (wallod_out(iwallo)%trn(itrn)%trn_flo / canal_om_stor(ican)%flo) *    &
                                                                            canal_om_stor(ican)

      !! subtract amount that is removed
      canal_om_stor(ican) = canal_om_stor(ican) - canal_om_out(ican)

      !! compute canal loss volume
      canal_loss_vol = canal(ican)%loss_fr * canal_om_out(ican)%flo  !m3

      !! outflow to receiving object (after loss)
      outflo_om = (1. - canal(ican)%loss_fr) * canal_om_out(ican)

      !! route canal loss to aquifer
      if (canal_loss_vol > 0.) then
        if (bsn_cc%gwflow == 1 .and. gw_canal_flag == 1) then
          !! gwflow active: distribute loss to grid cells proportional to canal length
          !! cell connections read from gwflow.canals, indexed by canal_id matching ican
          total_length = 0.
          do ic = 1, gw_canal_ncells_div
            if (gw_canl_div_cell(ic)%canal_id == ican) then
              total_length = total_length + gw_canl_div_cell(ic)%leng
            endif
          enddo
          if (total_length > 0.) then
            do ic = 1, gw_canal_ncells_div
              if (gw_canl_div_cell(ic)%canal_id == ican) then
                cell_id = gw_canl_div_cell(ic)%cell_id
                if (gw_state(cell_id)%stat == 1) then
                  cell_frac = gw_canl_div_cell(ic)%leng / total_length
                  cell_seep = canal_loss_vol * cell_frac
                  !! add seepage to cell storage and flux tracking
                  gw_state(cell_id)%stor = gw_state(cell_id)%stor + cell_seep
                  gw_hyd_ss(cell_id)%canl = gw_hyd_ss(cell_id)%canl + cell_seep
                  gw_hyd_ss_mo(cell_id)%canl = gw_hyd_ss_mo(cell_id)%canl + cell_seep
                  gw_hyd_ss_yr(cell_id)%canl = gw_hyd_ss_yr(cell_id)%canl + cell_seep
                endif
              endif
            enddo
          else
            !! no cell connections for this canal -- fall back to 1-D aquifer
            do iaq = 1, canal(ican)%num_aqu
              iaqu_ob = canal(ican)%aqu_loss(iaq)%aqu_num
              if (iaqu_ob > 0 .and. iaqu_ob <= sp_ob%aqu) then
                aqu_loss_vol = canal_loss_vol * canal(ican)%aqu_loss(iaq)%frac
                aqu_loss_mm = aqu_loss_vol / (10. * ob(sp_ob1%aqu + iaqu_ob - 1)%area_ha)
                aqu_d(iaqu_ob)%stor = aqu_d(iaqu_ob)%stor + aqu_loss_mm
                aqu_d(iaqu_ob)%rchrg = aqu_d(iaqu_ob)%rchrg + aqu_loss_mm
              endif
            enddo
          endif
        else
          !! no gwflow: add loss to 1-D aquifer storage
          do iaq = 1, canal(ican)%num_aqu
            iaqu_ob = canal(ican)%aqu_loss(iaq)%aqu_num
            if (iaqu_ob > 0 .and. iaqu_ob <= sp_ob%aqu) then
              aqu_loss_vol = canal_loss_vol * canal(ican)%aqu_loss(iaq)%frac
              aqu_loss_mm = aqu_loss_vol / (10. * ob(sp_ob1%aqu + iaqu_ob - 1)%area_ha)
              aqu_d(iaqu_ob)%stor = aqu_d(iaqu_ob)%stor + aqu_loss_mm
              aqu_d(iaqu_ob)%rchrg = aqu_d(iaqu_ob)%rchrg + aqu_loss_mm
            endif
          enddo
        endif
      endif

    return
    end subroutine wallo_canal
