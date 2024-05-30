      !the purpose of this subroutine is to read chemical reaction data for constituents
      subroutine cs_reactions_read !rtb cs
      
      use hydrograph_module, only: sp_ob
      use constituent_mass_module
      use cs_data_module
      
      implicit none
      
      character (len=80) :: titldum
      character (len=80) :: header
      integer :: eof,icount,igroup,irct,ishale,group
      integer :: hru_dum,aqu_dum
      logical :: i_exist              !none          |check to determine if file exists

      integer :: num_rct							!              |number of reaction parameters
      integer :: num_groups           !						   |number of reaction groups
      real    :: shale_fractions(500) !              |fraction of shale that covers an area's object
      
      
      eof = 0
      
      !read cs reaction data
      inquire (file="cs_reactions", exist=i_exist)
      if(i_exist) then
        
        !open the file
        open(107,file="cs_reactions")
        read(107,*,iostat=eof) titldum
        
        !allocate arrays
        allocate(cs_rct_soil(sp_ob%hru))
        allocate(cs_rct_aqu(sp_ob%aqu))
        
        !read the chemical reaction values for soils and aquifers, for each reaction group
        read(107,*) header
        read(107,*) num_rct,num_groups
        allocate(rct(num_rct,num_groups))
        do icount=1,num_rct
          read(107,*) (rct(icount,igroup),igroup=1,num_groups)
        enddo
        
        !read the parameters for each geologic unit
        read(107,*) header
        read(107,*) num_geol_shale
        allocate(rct_shale(num_geol_shale,3))
        do icount=1,num_geol_shale
          read(107,*) (rct_shale(icount,irct),irct=1,3)
        enddo
        
        !values for each HRU object
        read(107,*) header
        do icount=1,sp_ob%hru
          allocate(cs_rct_soil(icount)%shale(num_geol_shale))
          allocate(cs_rct_soil(icount)%sseratio(num_geol_shale))
          allocate(cs_rct_soil(icount)%ko2a(num_geol_shale))
          allocate(cs_rct_soil(icount)%kno3a(num_geol_shale))
          read(107,*) hru_dum,group,(shale_fractions(ishale),ishale=1,num_geol_shale)
          !store parameter values for the current HRU
          cs_rct_soil(icount)%se_ino3 = rct(1,group)
          cs_rct_soil(icount)%oxy_soil = rct(2,group)
          cs_rct_soil(icount)%kd_seo4 = rct(4,group)
          cs_rct_soil(icount)%kd_seo3 = rct(5,group)
          cs_rct_soil(icount)%kd_born = rct(6,group)
          cs_rct_soil(icount)%kseo4 = rct(7,group)
          do ishale=1,num_geol_shale
            cs_rct_soil(icount)%shale(ishale) = shale_fractions(ishale)
            cs_rct_soil(icount)%sseratio(ishale) = rct_shale(ishale,1)
            cs_rct_soil(icount)%ko2a(ishale) = rct_shale(ishale,2)
            cs_rct_soil(icount)%kno3a(ishale) = rct_shale(ishale,3)
          enddo
        enddo
        
        !values for each Aquifer object
        if(sp_ob%aqu > 0) then
        read(107,*) header
        do icount=1,sp_ob%aqu
          allocate(cs_rct_aqu(icount)%shale(num_geol_shale))
          allocate(cs_rct_aqu(icount)%sseratio(num_geol_shale))
          allocate(cs_rct_aqu(icount)%ko2a(num_geol_shale))
          allocate(cs_rct_aqu(icount)%kno3a(num_geol_shale))  
          read(107,*) aqu_dum,group,(shale_fractions(ishale),ishale=1,num_geol_shale)
          !store parameter values for the current HRU
          cs_rct_aqu(icount)%se_ino3 = rct(1,group)
          cs_rct_aqu(icount)%oxy_aqu = rct(3,group)
          cs_rct_aqu(icount)%kd_seo4 = rct(4,group)
          cs_rct_aqu(icount)%kd_seo3 = rct(5,group)
          cs_rct_aqu(icount)%kd_born = rct(6,group)
          cs_rct_aqu(icount)%kseo4 = rct(8,group)
          do ishale=1,num_geol_shale
            cs_rct_aqu(icount)%shale(ishale) = shale_fractions(ishale)
            cs_rct_aqu(icount)%sseratio(ishale) = rct_shale(ishale,1)
            cs_rct_aqu(icount)%ko2a(ishale) = rct_shale(ishale,2)
            cs_rct_aqu(icount)%kno3a(ishale) = rct_shale(ishale,3)
          enddo
        enddo
        endif

      endif
      
      !close the file
      close(107)
      
      return
      end !cs_read_reactions