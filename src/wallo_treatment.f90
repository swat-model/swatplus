      subroutine wallo_treatment (iwallo, itrn, itrt)
      
      use water_allocation_module
      use hydrograph_module
      use constituent_mass_module
      
      implicit none 

      integer, intent (in):: iwallo     !water allocation object number
      integer, intent (in) :: itrn      !water transfer object number
      integer, intent (in) :: itrt      !water treatment plant object number
      integer :: iom                    !unused number of organic-mineral data concentrations of treated water   !!!!NBS 
      
      !! suppress unused variable warning
      if (iom < 0) continue
      
      !! treating water to wtp concentrations
      outflo_om = wtp_om_treat(itrt)
      
      !! treated outflow is a fraction of withdrawal
      outflo_om%flo = outflo_om%flo * wal_omd(iwallo)%trn(itrn)%h_tot%flo
      
      !! convert concentration to mass
      call hyd_convert_conc_to_mass (outflo_om)
      wtp_om_out(itrt) = wtp_om_out(itrt) + outflo_om
      
      !! amount that is removed
      wal_tr_omd(itrt) = wal_omd(iwallo)%trn(itrn)%h_tot - wtp_om_out(itrt)
      
      !! treat constituents - convert concentration to mass
      if (cs_db%num_tot > 0) then
        call hydcsout_conc_mass (outflo_om%flo, wtp_cs_treat(itrt), outflo_cs)
      end if
      
      outflo_om = hz
      
      return
    end subroutine wallo_treatment