
      !This subroutine reads in River Cell information and writes the gwflow.con input file
  
      !  Prepared by: Ryan Bailey, Colorado State University
      !  May 2021
  
      subroutine gwflow_riv
      
      use gwflow_module
      use hydrograph_module
      
      implicit none

      character(len=8) :: col_head_con(17)
      integer  i,j,k,in_riv,in_con,cell_ID,channel,riv_zone,dum1,dum2,river_cell
      real     bed_elev,riv_length,dum3

      !integers for input and output files
      in_riv = 1280
      in_con = 1281
      open(in_riv,file='gwflow.rivcells')
      open(in_con,file='gwflow.con')
      
      !number of cells that intersect with channels
      num_rivcells = sp_ob%gwflow
      
      !allocate global arrays for river cells
      allocate(gw_riv_id(num_rivcells))
      allocate(gw_riv_chan(num_rivcells))
      allocate(gw_riv_len(num_rivcells))
      allocate(gw_riv_elev(num_rivcells))
      allocate(gw_riv_zone(num_rivcells))
      gw_riv_len = 0.
      
      !read in River Cell information
      read(in_riv,*)
      read(in_riv,*)
      read(in_riv,*)
      do k=1,num_rivcells
        read(in_riv,*) cell_ID,bed_elev,channel,riv_length,riv_zone
        gw_riv_id(k) = cell_ID
        gw_riv_elev(k) = bed_elev
        gw_riv_chan(k) = channel
        gw_riv_len(k) = riv_length
        gw_riv_zone(k) = riv_zone
      enddo
      
      !write out gwflow.con input file (connection file)
      write(in_con,*) 'gwflow.con: river cell spatial connections'
      col_head_con = (/"NUMB","NAME","GISID","AREA","LAT","LONG","ELEV","CELL","WST","CONST","OVER","RULE","SRC_TOT ","OBTYPE_OUT1 ","OBTYPNO_OUT1 ","HTYPE_OUT1 ","FRAC_OUT1 "/)
      write(in_con,103) (col_head_con(j),j=1,17)
      dum1 = 1
      dum2 = 0
      dum3 = 1.00
      do k=1,num_rivcells
        write(in_con,107) dum1,dum1,dum1,dum2,dum2,dum2,dum2,gw_riv_id(k),dum1,dum2,dum2,dum2,dum1,'sdc',gw_riv_chan(k),'tot',dum3
      enddo     
 
      
 103  format(30(a10))   
 107  format(i5,i5,i6,i5,i4,i5,i5,i10,i4,i6,i5,i5,i8,a12,i13,a11,f6.2)
 
      end subroutine gwflow_riv
      
           
      