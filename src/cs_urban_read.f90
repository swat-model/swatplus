      subroutine cs_urban_read !rtb cs

      use maximum_data_module
      use urban_data_module
      use constituent_mass_module
      use cs_module
      
      implicit none
      
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      character (len=16) :: urb_type   !           |urban land use type
      logical :: i_exist              !none       |check to determine if file exists
      integer :: eof                  !           |end of file
      integer :: imax = 0.            !none       |determine max number for array (imax) and total number in file
      integer :: itype                !           |counter
      integer :: iu                   !none       |counter 
      integer :: ics                  !none       |constituent ion counter
      
      
      !only proceed if there are constituents in the simulation
      if(cs_db%num_cs > 0) then
      
      inquire (file='cs_urban',exist=i_exist)
      if(i_exist) then
      
        !open file and read first two header lines
        open(5054,file='cs_urban')
        read(5054,*) header
        read(5054,*) header
      
        !read in constituent concentrations for each urban land use type  
        !determine number of land use types listed
        eof = 0
        imax = 0
        do while (eof == 0)
          read (5054,*,iostat=eof) titldum
          if (eof < 0) exit
          imax = imax + 1
        enddo

        !allocate urban constituent arrays
        allocate(cs_urban_conc(imax,cs_db%num_cs))
        cs_urban_conc = 0.
        
        !loop through each land use type; verify match with types listed in urban.urb        
        rewind(5054)
        read(5054,*) header
        read(5054,*) header
        do itype=1,imax
          read(5054,*) urb_type
          !see if there is a match (loop through urban land use types)
          do iu=1,db_mx%urban
            if(urb_type == urbdb(iu)%urbnm) then
              backspace(5054)
              read(5054,*) urb_type,(cs_urban_conc(iu,ics),ics=1,cs_db%num_cs)
            endif
          enddo
        enddo
        
      endif
      
      endif
      
      close (5054)

      
      return
      end subroutine cs_urban_read
      