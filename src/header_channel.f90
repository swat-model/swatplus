      subroutine header_channel

      use channel_module
      use basin_module
      use hydrograph_module, only : sp_ob

      implicit none 
            
!!!  CHANNEL
      !if (sp_ob%chan > 0) then !subdaily main channel output, Jaehak 2017
      !   if (pco%chan%t == "y") then
          !open (,file="channel_subday.txt",recl = 1500)
          !write (,"(4a6,10(1x,a15))") "REACH", "year","day","step","pcpmm","flow_m^3/s","sed_tons"
          !write (9000,*) "CHANNEL             channel_subday.txt"
          !if (pco%csvout == "y")  then
          !  open (,file="channel_subday.csv",recl = 1500)
          !  write (,'(*(G0.3,:,","))') ch_hdr !! channel header csv format
          ! write (9000,*) "CHANNEL             channel_subday.csv"
          !end if
        !endif
      !endif

      if (sp_ob%chan > 0) then
        if (pco%chan%d == "y") then
          open (2480,file="channel_day.txt",recl = 1500)
          write (2480,*) bsn%name, prog
          write (2480,*) ch_hdr !! channel
          write (2480,*) ch_hdr_units
          write (9000,*) "CHANNEL                   channel_day.txt"
          if (pco%csvout == "y")  then
            open (2484,file="channel_day.csv",recl = 1500)
            write (2484,*) bsn%name, prog
            write (2484,'(*(G0.3,:,","))') ch_hdr !! channel header csv format
            write (2484,'(*(G0.3,:,","))') ch_hdr_units
           write (9000,*) "CHANNEL                   channel_day.csv"
          end if
        endif
      endif
        
        if (sp_ob%chan > 0) then
          if (pco%chan%m == "y") then
          open (2481,file="channel_mon.txt",recl = 1500)
          write (2481,*) bsn%name, prog
          write (2481,*) ch_hdr   !! channel
          write (2481,*) ch_hdr_units
          write (9000,*) "CHANNEL                   channel_mon.txt"
          if (pco%csvout == "y") then
            open (2485,file="channel_mon.csv",recl = 1500)
            write (2485,*) bsn%name, prog
            write (2485,'(*(G0.3,:,","))') ch_hdr   !! channel aa header csv format
            write (2485,'(*(G0.3,:,","))') ch_hdr_units
            write (9000,*) "CHANNEL                   channel_mon.csv"
          end if
          end if
         end if

      if (sp_ob%chan > 0) then
        if (pco%chan%y == "y") then
          open (2482,file="channel_yr.txt",recl = 1500)
          write (2482,*) bsn%name, prog
          write (2482,*) ch_hdr !! channel
          write (2482,*) ch_hdr_units
          write (9000,*) "CHANNEL                   channel_yr.txt"
          if (pco%csvout == "y")  then
            open (2486,file="channel_yr.csv",recl = 1500)
            write (2486,*) bsn%name, prog
            write (2486,'(*(G0.3,:,","))') ch_hdr !! channel header csv format
            write (2486,'(*(G0.3,:,","))') ch_hdr_units
           write (9000,*) "CHANNEL                   channel_yr.csv"
          end if
        endif
      endif
        
        if (sp_ob%chan > 0) then
          if (pco%chan%a == "y") then
          open (2483,file="channel_aa.txt",recl = 1500)
          write (2483,*) bsn%name, prog
          write (2483,*) ch_hdr   !! channel
          write (2483,*) ch_hdr_units
          write (9000,*) "CHANNEL                   channel_aa.txt"
          if (pco%csvout == "y") then
            open (2487,file="channel_aa.csv",recl = 1500)
            write (2487,*) bsn%name, prog
            write (2487,'(*(G0.3,:,","))') ch_hdr   !! channel aa header csv format
            write (2487,'(*(G0.3,:,","))') ch_hdr_units
            write (9000,*) "CHANNEL                   channel_aa.csv"
          end if
          end if
        end if
                         
      return
      end subroutine header_channel