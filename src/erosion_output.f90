      subroutine erosion_output
      
      use erosion_module
      use hydrograph_module, only : sp_ob
      
      implicit none
      
      integer :: j   !none      |counter
      real :: nevents

      do j = 1, sp_ob%hru
        !! divide by number of events to get event average
        nevents = float(ero_output(j)%n_events)
        ero_output(j)%ero_ave = ero_output(j)%ero_ave / nevents

        !! print average per event
        write (4444,*) j, ero_output(j)%n_events, ero_output(j)%ero_ave
      end do
      
      return
       
      end subroutine erosion_output