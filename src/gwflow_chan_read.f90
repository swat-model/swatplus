
      !This subroutine reads in channel-cell connection information and writes the gwflow.con input file
  
      !  Prepared by: Ryan Bailey, Colorado State University
  
      subroutine gwflow_chan_read
      
      use gwflow_module
      use hydrograph_module
      
      implicit none

      character(len=8) :: col_head_con(17)
      integer  i,j,k,in_chan,in_con,cell_ID,channel,chan_zone,dum1,dum2,chan_cell
      real     bed_elev,chan_length,dum3

      !message to record file
      write(out_gw,*) 'reading cell-channel connections in gwflow.chancells...'
      
      !integers for input and output files
      !in_chan = 1280
      !in_con = 1281
      open(1280,file='gwflow.chancells')
      open(1281,file='gwflow.con')
      
      !number of cells that intersect with channels
      num_chancells = sp_ob%gwflow
      
      !allocate global arrays for channel cells
      allocate(gw_chan_id(num_chancells))
      allocate(gw_chan_chan(num_chancells))
      allocate(gw_chan_len(num_chancells))
      allocate(gw_chan_elev(num_chancells))
      allocate(gw_chan_zone(num_chancells))
      gw_chan_len = 0.
      
      !read in channel-cell connection information
      read(1280,*)
      read(1280,*)
      read(1280,*)
      do k=1,num_chancells
        read(1280,*) cell_ID,bed_elev,channel,chan_length,chan_zone
        gw_chan_id(k) = cell_ID
        gw_chan_elev(k) = bed_elev
        gw_chan_chan(k) = channel
        gw_chan_len(k) = chan_length
        gw_chan_zone(k) = chan_zone
      enddo
      
      !write out gwflow.con input file (connection file)
      write(out_gw,*) 'writing gwflow.con file...'
      write(1281,*) 'gwflow.con: channel-cell spatial connections'
      col_head_con = [character(len=17) :: "NUMB","NAME","GISID","AREA","LAT","LONG","ELEV","CELL","WST","CONST","OVER",  &
            "RULE","SRC_TOT ","OBTYPE_OUT1 ","OBTYPNO_OUT1 ","HTYPE_OUT1 ","FRAC_OUT1 "]
      write(1281,103) (col_head_con(j),j=1,17)
      dum1 = 1
      dum2 = 0
      dum3 = 1.00
      do k=1,num_chancells
        write(1281,107) dum1,dum1,dum1,dum2,dum2,dum2,dum2,gw_chan_id(k),dum1,dum2,dum2,dum2,dum1,'sdc',gw_chan_chan(k),'tot',dum3
      enddo     
      close(1281)
      
 103  format(30(a10))   
 107  format(i5,i5,i6,i5,i4,i5,i5,i10,i4,i6,i5,i5,i8,a12,i13,a11,f6.2)
 
      end subroutine gwflow_chan_read      