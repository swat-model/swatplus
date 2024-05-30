      subroutine basin_print_codes_read
      
      use input_file_module
      use basin_module
      use time_module
      
      implicit none
       
      character (len=500) :: header    !              |header of file
      character (len=80) :: titldum    !              |title of file
      character (len=16) :: name       !              |name
      integer :: eof                   !              |end of file
      logical :: i_exist               !              |check to determine if file exists
      integer :: ii                    !none          |counter
       
      eof = 0

      !! read time codes
      inquire (file=in_sim%prt, exist=i_exist)
      if (i_exist .or. in_sim%prt /= "null") then
      do
        open (107,file=in_sim%prt)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        read (107,*,iostat=eof) pco%nyskip, pco%day_start, pco%yrc_start, pco%day_end, pco%yrc_end, pco%int_day        
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        read (107,*,iostat=eof) pco%aa_numint
        if (pco%aa_numint > 0) then
          allocate (pco%aa_yrs(pco%aa_numint))
          backspace (107)
          read (107,*,iostat=eof) pco%aa_numint, (pco%aa_yrs(ii), ii = 1, pco%aa_numint)
          if (eof < 0) exit
        else
          allocate (pco%aa_yrs(1))
        end if
     !! read database output
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        read (107,*,iostat=eof) pco%csvout, pco%carbout, pco%cdfout
        if (eof < 0) exit
        
     !! read other output
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        read (107,*,iostat=eof) pco%crop_yld, pco%mgtout, pco%hydcon, pco%fdcout
        if (eof < 0) exit
             
     !! read objects output
     !! basin
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        read (107,*,iostat=eof) name, pco%wb_bsn
        if (eof < 0) exit
        read (107,*,iostat=eof) name, pco%nb_bsn
        if (eof < 0) exit       
        read (107,*,iostat=eof) name, pco%ls_bsn
        if (eof < 0) exit
        read (107,*,iostat=eof) name, pco%pw_bsn
        if (eof < 0) exit        
        read (107,*,iostat=eof) name, pco%aqu_bsn
        if (eof < 0) exit            
        read (107,*,iostat=eof) name, pco%res_bsn
        if (eof < 0) exit        
        read (107,*,iostat=eof) name, pco%chan_bsn
        if (eof < 0) exit            
        read (107,*,iostat=eof) name, pco%sd_chan_bsn
        if (eof < 0) exit 
        read (107,*,iostat=eof) name, pco%recall_bsn
        if (eof < 0) exit            
     !! region
        read (107,*,iostat=eof) name, pco%wb_reg
        if (eof < 0) exit     
        read (107,*,iostat=eof) name, pco%nb_reg
        if (eof < 0) exit       
        read (107,*,iostat=eof) name, pco%ls_reg
        if (eof < 0) exit
        read (107,*,iostat=eof) name, pco%pw_reg
        if (eof < 0) exit        
        read (107,*,iostat=eof) name, pco%aqu_reg
        if (eof < 0) exit            
        read (107,*,iostat=eof) name, pco%res_reg
        if (eof < 0) exit                           
        read (107,*,iostat=eof) name, pco%sd_chan_reg
        if (eof < 0) exit 
        read (107,*,iostat=eof) name, pco%recall_reg
        if (eof < 0) exit 
        read (107,*,iostat=eof) name, pco%water_allo
        if (eof < 0) exit   
    !! lsu
        read (107,*,iostat=eof) name, pco%wb_lsu
        if (eof < 0) exit     
        read (107,*,iostat=eof) name, pco%nb_lsu
        if (eof < 0) exit       
        read (107,*,iostat=eof) name, pco%ls_lsu
        if (eof < 0) exit
        read (107,*,iostat=eof) name, pco%pw_lsu
        if (eof < 0) exit                
     !! hru
        read (107,*,iostat=eof) name, pco%wb_hru
        if (eof < 0) exit     
        read (107,*,iostat=eof) name, pco%nb_hru
        if (eof < 0) exit       
        read (107,*,iostat=eof) name, pco%ls_hru
        if (eof < 0) exit
        read (107,*,iostat=eof) name, pco%pw_hru
        if (eof < 0) exit 
        
     !! hru-lte
        read (107,*,iostat=eof) name, pco%wb_sd
        if (eof < 0) exit     
        read (107,*,iostat=eof) name, pco%nb_sd
        if (eof < 0) exit       
        read (107,*,iostat=eof) name, pco%ls_sd
        if (eof < 0) exit
        read (107,*,iostat=eof) name, pco%pw_sd
        if (eof < 0) exit                   
     !! channel
        read (107,*,iostat=eof) name, pco%chan
        if (eof < 0) exit             
     !! channel-lte
        read (107,*,iostat=eof) name, pco%sd_chan
        if (eof < 0) exit          
     !! aquifer
        read (107,*,iostat=eof) name, pco%aqu
        if (eof < 0) exit
     !! reservoir
        read (107,*,iostat=eof) name, pco%res
        if (eof < 0) exit
     !! recall
        read (107,*,iostat=eof) name, pco%recall
        if (eof < 0) exit        
     !! hydin and hydout
        read (107,*,iostat=eof) name, pco%hyd
        if (eof < 0) exit
     !! routing units
        read (107,*,iostat=eof) name, pco%ru
        if (eof < 0) exit 
     !! all pesticide outputs
        read (107,*,iostat=eof) name, pco%pest
        if (eof < 0) exit  
		 !! salt outputs !rtb salt
        read (107,*,iostat=eof) name, pco%salt_basin
        if (eof < 0) exit
        read (107,*,iostat=eof) name, pco%salt_hru
        if (eof < 0) exit
        read (107,*,iostat=eof) name, pco%salt_ru
        if (eof < 0) exit
        read (107,*,iostat=eof) name, pco%salt_aqu
        if (eof < 0) exit
        read (107,*,iostat=eof) name, pco%salt_chn
        if (eof < 0) exit
        read (107,*,iostat=eof) name, pco%salt_res
        if (eof < 0) exit
        read (107,*,iostat=eof) name, pco%salt_wet
        if (eof < 0) exit
      !! constituent outputs !rtb cs
        read (107,*,iostat=eof) name, pco%cs_basin
        if (eof < 0) exit
        read (107,*,iostat=eof) name, pco%cs_hru
        if (eof < 0) exit
        read (107,*,iostat=eof) name, pco%cs_ru
        if (eof < 0) exit
        read (107,*,iostat=eof) name, pco%cs_aqu
        if (eof < 0) exit
        read (107,*,iostat=eof) name, pco%cs_chn
        if (eof < 0) exit
        read (107,*,iostat=eof) name, pco%cs_res
        if (eof < 0) exit
        read (107,*,iostat=eof) name, pco%cs_wet
        if (eof < 0) exit
        exit
      end do
      end if
      close (107)
      
      if (pco%day_start == 0) pco%day_start = 1
      if (pco%day_end == 0) pco%day_end = 366
      if (pco%yrc_start == 0) pco%yrc_start = time%yrc
      if (pco%yrc_end == 0) pco%yrc_end = time%yrc + time%nbyr
      if (pco%int_day <= 0) pco%int_day = 1
      pco%int_day_cur = pco%int_day
 
      return
      end subroutine basin_print_codes_read           