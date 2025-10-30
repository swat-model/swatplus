      subroutine swift_output
    
      use hydrograph_module
      use hru_module
      use soil_module
      use output_landscape_module
      use reservoir_data_module
      use maximum_data_module
      use climate_module
      use aquifer_module
      use input_file_module
      use sd_channel_module
      use time_module

      implicit none
      
      integer :: iaqu = 0
      integer :: icha = 0
      integer :: ires = 0
      integer :: ihyd = 0
      integer :: idat = 0
      integer :: idb = 0
      integer :: iobj_out = 0
      integer :: irec = 0
      integer :: iob = 0
      integer :: i = 0                              ! loop counter
      integer, parameter :: ifile = 14              ! number of SWAT input files to copy to SWIFT folder
      character (len=8) :: wet_y_n = ""
      character(len=100) :: folderPath = ""
      character(len=100) :: command = ""
      character(len=25)  :: file_list(ifile) = "" ! list of SWAT input files to copy to SWIFT folder
      logical :: i_exist
      
      ! SWIFT file formats
      ! Format statements where changed below because they cause runtime errors in gfortran
      ! a format likes 12xA48 need to be 12x,A48
      ! 201 format (A8,12xA8,46X,*(A16,F5.1,A4,1xA16,F5.1,A4))        ! format of precip.swf headers
      201 format (A8,12x,A8,46X,*(A16,F5.1,A4,1x,A16,F5.1,A4))        ! format of precip.swf headers
      ! 301 format (I8,1xA64,F16.4,8xF16.4)                           ! format of precip.swf
      301 format (I8,1x,A64,F16.4,8x,F16.4)                           ! format of precip.swf
      ! 202 format (A8,30xA8,18X,A8,36xA8,4xA8,218x1A8,6x1A8)         ! format of hru_dat.swf headers
      202 format (A8,30x,A8,18X,A8,36x,A8,4x,A8,218x,1A8,6x,1A8)      ! format of hru_dat.swf headers
      ! 302 format (1I8,1x2A48, G16.4 ,1x*(G16.4))                      ! format of hru_dat.swf
      302 format (1I8,1x,2A48, G16.4 ,1x, *(G16.4))                      ! format of hru_dat.swf
      ! 203 format (A8,*(2x1A16,10x1A16, 6x1A16, 2x6A16))               ! format of hru_exco.swf headers      
      203 format (A8,*(2x,1A16,10x,1A16, 6x,1A16, 2x,6A16))               ! format of hru_exco.swf headers      
      ! 303 format (I8,*(2x1A16,8xF16.4, 7F16.4,10x))                   ! format of hru_exco.swf 
      303 format (I8,*(2x,1A16,8x,F16.4, 7F16.4,10x))                   ! format of hru_exco.swf 
      ! 204 format (4xA8,*(A8,8x))                                      ! format of hru_wet.swf headers
      204 format (4x,A8,*(A8,8x))                                      ! format of hru_wet.swf headers
      ! 205 format (7xA16, A16, 10x,*(A16))                           ! format of chan_dat.swf headers
      205 format (7x,A16, A16, 10x,*(A16))                           ! format of chan_dat.swf headers
      305 format (I8, 1x, A16, A16,*(F16.4))                             ! format of chan_dat.swf
      ! 206 format (4xA8, 1xA8, 20x,*(A16))                           ! format of chan_dr.swf headers
      206 format (4x,A8, 1x,A8, 20x,*(A16))                           ! format of chan_dr.swf headers
!*** tu Wunused-label:       306 format (I8,4xA16, 10xA16,*(F16.4))                        ! format of chan_dr.swf
      ! 207 format (A16,1x*(A16))                                     ! format of aqu_dr.swf headers
      207 format (A16,1x,*(A16))                                     ! format of aqu_dr.swf headers
      ! 208 format (6xA8, 1xA8, 16x,*(A8,6x))                      ! format of res_dat.swf headers
      208 format (6x,A8,1x,A8,16x,*(A8,6x))                      ! format of res_dat.swf headers
      
      !! check for file_cio.swf to determine if SWIFT folder exist
      inquire (file="SWIFT/file_cio.swf", exist=i_exist)
      if (.not. i_exist) then   ! if not use system-specific command to create SWIFT folder
        folderPath = "SWIFT"
        command = 'mkdir ' // trim(folderPath)
        call SYSTEM(command)
      end if
    
      !! write new file.cio
      open (107,file="SWIFT/file_cio.swf",recl = 1500)
      write (107, *) "SWIFT file.cio"
      write (107, *) "BASIN         ", in_sim%object_cnt, in_sim%object_prt, in_sim%cs_db
      write (107, *) "CLIMATE       ", "  precip.swf"
      write (107, *) "CONNECT       ", in_con%hru_con, in_con%ru_con, in_con%aqu_con, in_con%chandeg_con,  &
                                          in_con%res_con, in_con%rec_con, in_con%out_con 
      write (107, *) "CHANNEL       ", "  chan_dat.swf", "  chan_dr.swf"
      write (107, *) "RESERVOIR     ", "  res_dat.swf", "  res_dr.swf"
      write (107, *) "ROUT_UNIT     ", in_ru%ru_def, in_ru%ru_ele
      write (107, *) "HRU           ", "  hru_dat.swf", "  hru_exco.swf", "  hru_wet.swf",    &
                                       "  hru_bmp.swf", "  hru_dr.swf"
      write (107, *) "RECALL        ", "  recall.swf"
      write (107, *) "AQUIFER       ", "  aqu_dr.swf"
      write (107, *) "LS_UNIT       ", in_regs%def_lsu, in_regs%ele_lsu
      close (107)
      
      ! Create a list of SWAT input files to copy to the SWIFT folder
      file_list = [in_sim%object_cnt, in_sim%object_prt,          & 
          in_sim%cs_db, in_con%hru_con, in_con%ru_con, in_con%aqu_con, in_con%chandeg_con,  &
          in_con%res_con, in_con%rec_con, in_con%out_con, in_ru%ru_def, in_ru%ru_ele,       &
          in_regs%def_lsu, in_regs%ele_lsu]
      
      ! Loop through the file list and copy each file to the SWIFT folder
      do i = 1, ifile
         call copy_file(file_list(i), "SWIFT/" // trim(adjustl(file_list(i))))
      end do

      
      !! write ave annual precip to SWIFT model
      open (107,file="SWIFT/precip.swf",recl = 1500)
      write (107, *) bsn%name
      write (107, *) db_mx%wst
      write (107, 201) "iwst ", "name ", "precip_aa/", yrs_print,'yrs', "pet_aa/", yrs_print, 'yrs'
      ! write (107, '(A8,12xA8,46X,A16,6xA16)') "--- ", "---- ", "mm", "mm"
      write (107, '(A8,12x,A8,46X,A16,6x,A16)') "--- ", "---- ", "mm", "mm"
      do iwst = 1, db_mx%wst
        wst(iwst)%precip_aa = wst(iwst)%precip_aa / yrs_print
        wst(iwst)%pet_aa = wst(iwst)%pet_aa / yrs_print
        write (107, *) iwst, wst(iwst)%name, wst(iwst)%precip_aa, wst(iwst)%pet_aa
      end do
      close (107)
      
      !! write hru data to SWIFT model
      open (107,file="SWIFT/hru_dat.swf",recl = 1500)
      write (107, *) bsn%name
      write (107, *) sp_ob%hru
      write (107, *) "iwst ", "name ", "land_use_mgt_c", "slope", "hydgrp", "null", "null"
      write (107, *) "--- ", "---- ", "--------------", "m/m", "------", "null", "null"
      do ihru = 1, sp_ob%hru
        write (107, *) ihru, ob(ihru)%name, hru(ihru)%land_use_mgt_c, hru(ihru)%topo%slope,    &
                                                    soil(ihru)%hydgrp, "  null", "   null"
      end do
      close (107)
      
      !! write hru export coefficients to SWIFT model
      open (107,file="SWIFT/hru_exco.swf",recl = 1500)
      write (107, *) bsn%name
      write (107, *) sp_ob%hru
      write (107, *) "HRU ", (hru_swift_hdr%hd_type(ihyd), 'wyld_rto', &
          hru_swift_hdr%exco, ihyd = 1, hd_tot%hru)

      write (107, *) "--- ", (hru_swift_hdr%hd_type(ihyd), 'wyld_rto', &
          hru_swift_hdr%exco_unit, ihyd = 1, hd_tot%hru)
      
      do ihru = 1, sp_ob%hru
        icmd = hru(ihru)%obj_no
        !! Allocate wyld_rto array based on the number of hydrological components
        allocate(wyld_rto(hd_tot%hru))
        
        do ihyd = 1, hd_tot%hru
            !! convert mass to concentrations
            if (ob(icmd)%hd_aa(ihyd)%flo > 1.e-6) then
                call hyd_convert_mass_to_conc(ob(icmd)%hd_aa(ihyd))
            else
                ob(icmd)%hd_aa(ihyd) = hz
            end if
            !! output runoff/precip ratio - mm=m3/(10*ha)
            wyld_rto(ihyd) = hru(ihru)%flow(ihyd) / (hru(ihru)%precip_aa + 1.e-6)
        end do
        
        !! write to SWIFT hru export coefficient file
        write(107, *) ihru, (hru_swift_hdr%hd_type(ihyd), &
            wyld_rto(ihyd), ob(icmd)%hd_aa(ihyd)%sed, ob(icmd)%hd_aa(ihyd)%orgn, &
            ob(icmd)%hd_aa(ihyd)%sedp, ob(icmd)%hd_aa(ihyd)%no3, ob(icmd)%hd_aa(ihyd)%solp, &
            ob(icmd)%hd_aa(ihyd)%nh3, ob(icmd)%hd_aa(ihyd)%no2, ihyd = 1, hd_tot%hru)
        
        !! Deallocate the wyld_rto array
        deallocate(wyld_rto)
      end do

      close(107)
      
      !! write hru wetland inputs to SWIFT model
      open (107,file="SWIFT/hru_wet.swf",recl = 1500)
      write (107, *) bsn%name
      write (107, *) sp_ob%hru
      write (107, *) "ires", "psa ", "pdep", "esa ", "edep"
      write (107, *) "----", "frac", "mm  ", "frac", "mm  "
      do ihru = 1, sp_ob%hru
        icmd = hru(ihru)%obj_no
        
        !! write to SWIFT wetland input file
        if (hru(ihru)%dbs%surf_stor > 0) then
          !! wetland hru
          ires= hru(ihru)%dbs%surf_stor
          ihyd = wet_dat(ires)%hyd
          write (107, *) ihru, wet_hyd(ihyd)%psa, wet_hyd(ihyd)%pdep, wet_hyd(ihyd)%esa, &
              wet_hyd(ihyd)%edep
        end if
      end do
      close (107)
      
      !! write channel data for SWIFT
      open (107,file="SWIFT/chan_dat.swf",recl = 1500)
      write (107, *) bsn%name
      write (107, *) sd_chd_hdr
      do icha = 1, sp_ob%chandeg
        icmd = sp_ob1%chandeg + icha - 1
        idat = ob(icmd)%props
        idb = sd_dat(idat)%hyd
        write (107, *) icha, sd_chd(idb)
      end do
      close (107)
      
      !! write channel delivery ratios for SWIFT
      open (107,file="SWIFT/chan_dr.swf",recl = 1500)
      write (107, *) bsn%name
      write (107, *) sp_ob%chandeg
      write (107, *) "icha ", "name ", hru_swift_hdr%dr
      write (107, *) "--- ", "---- ", hru_swift_hdr%dr_unit
      do icha = 1, sp_ob%chandeg
        icmd = sp_ob1%chandeg + icha - 1
        ht5 = ob(icmd)%hout_tot // ob(icmd)%hin_tot
        ht5%flo = 1.    !sediment and organic transport are simulated in SWIFT
        ht5%sed = 1.
        ht5%orgn = 1.
        ht5%sedp = 1.
        ht5%nh3 = 1. 
        ht5%no2 = 1.
        write (107, *) icha, sd_chd(idb)%name, ht5%flo, ht5%sed, ht5%orgn, ht5%sedp, &
            ht5%no3, ht5%solp, ht5%nh3, ht5%no2
      end do
      close (107)
           
      !! write aquifer delivery ratios for SWIFT
      open (107,file="SWIFT/aqu_dr.swf",recl = 1500)
      write (107, *) bsn%name
      write (107, *) sp_ob%aqu
      write (107, *) "iaqu ", hru_swift_hdr%dr
      write (107, *) "--- ",  hru_swift_hdr%dr_unit
      do iaqu = 1, sp_ob%aqu
        icmd = sp_ob1%aqu + iaqu - 1
        ht5 = ob(icmd)%hout_tot // ob(icmd)%hin_tot
        write (107, *) iaqu, ht5%flo, ht5%sed, ht5%orgn, ht5%sedp, ht5%no3, ht5%solp, &
            ht5%nh3, ht5%no2
      end do
      close (107)
            
      !! write reservoir delivery ratios for SWIFT
      open (107,file="SWIFT/res_dat.swf",recl = 1500)
      write (107, *) bsn%name
      write (107, *) sp_ob%res
      write (107, *) "icha ", "name ", "psa  ", "pvol ", "esa  ", "evol "
      write (107, *) "---- ", "---- ", "frac ", "m3   ", "frac ", "m3   " 
      do ires = 1, sp_ob%res
        write (107, *) ires, res_hyd(ires)%name, res_hyd(ires)%psa, res_hyd(ires)%pvol, &
            res_hyd(ires)%esa, res_hyd(ires)%evol
      end do
      close (107)
      
      !! write reservoir delivery ratios for SWIFT
      open (107,file="SWIFT/res_dr.swf",recl = 1500)
      write (107, *) bsn%name
      write (107, *) sp_ob%res
      write (107, *) "ires ", "name ", hru_swift_hdr%dr
      write (107, *) "---- ", "---- ", hru_swift_hdr%dr_unit
      do ires = 1, sp_ob%res
        icmd = sp_ob1%res + ires - 1
        ht5 = ob(icmd)%hout_tot // ob(icmd)%hin_tot
        write (107, *) ires, ob(icmd)%name, ht5%flo, ht5%sed, ht5%orgn, ht5%sedp, ht5%no3, &
            ht5%solp, ht5%nh3, ht5%no2
      end do
      close (107)
      
      !! write recal_swift.rec --> change files to average annual and use the object name for the file name
      open (107,file="SWIFT/recall.swf",recl = 1500)
      write (107,*)           "         ID            NAME              REC_TYP         FILENAME"
      do irec = 1, db_mx%recall_max
        write (107,*) irec, recall(irec)%name, recall(irec)%typ, recall(irec)%name
        
        !! write to each recall file
        open (108,file="SWIFT/" // trim(adjustl(recall(irec)%name)),recl = 1500)
        write (108,*) " AVE ANNUAL RECALL FILE  ", recall(irec)%filename
        write (108,*) "     1    1    1     1    type    ", recall(irec)%filename, rec_a(irec)%flo,     &
                rec_a(irec)%sed, rec_a(irec)%orgn, rec_a(irec)%sedp, rec_a(irec)%no3, rec_a(irec)%solp, &
                rec_a(irec)%nh3, rec_a(irec)%no2
        close (108)
      end do
      close (107)
      
      !! write object.prt file - using the same file for now
      !open (107,file="object_prt.swf",recl = 1500)
      do iobj_out = 1, mobj_out
        !write (107,*) irec, recall(irec)%name, "   4   ", recall(irec)%name
        
        !! write to each object print file
        open (108,file="SWIFT/object_prt.swf",recl = 1500)
        write (108,*) " AVE ANNUAL OBJECT OUTPUT FILE  ", ob_out(iobj_out)%filename
        iob = ob_out(iobj_out)%objno
        ihyd = ob_out(iobj_out)%hydno
        ob(iob)%hd_aa(ihyd) = ob(iob)%hd_aa(ihyd) / yrs_print
        write (108,*) "     1    1    1     1    ", ob_out(iobj_out)%name, ob_out(iobj_out)%name,       &
                        ob(iob)%hd_aa(ihyd)%flo, ob(iob)%hd_aa(ihyd)%sed, ob(iob)%hd_aa(ihyd)%orgn,     &
                        ob(iob)%hd_aa(ihyd)%sedp, ob(iob)%hd_aa(ihyd)%no3, ob(iob)%hd_aa(ihyd)%solp,    &
                        ob(iob)%hd_aa(ihyd)%nh3, ob(iob)%hd_aa(ihyd)%no2
        close (108)
      end do
      close (107)
      
            
      return
      end subroutine swift_output