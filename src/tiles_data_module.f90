      module tiles_data_module
    
      implicit none
     
        type subsurface_drainage
        character(len=13) :: name = "null"
        real :: depth = 0.    !! |mm            |depth of drain tube from the soil surface
        real :: time = 0.     !! |hrs           |time to drain soil to field capacity
        real :: lag = 0.      !! |hours         |drain tile lag time
        real :: radius =0.    !! |mm		       effective radius of drains
        real :: dist = 0.     !! |mm            |distance between two drain tubes or tiles
        real :: drain_co      !! |mm/day        |drainage coefficient 
        real :: pumpcap = 0.  !! |mm/hr         |pump capacity (default pump capacity = 1.042mm/hr or 25mm/day)
        real :: latksat = 0.  !! |none          |multiplication factor to determine conk(j1,j) from sol_k(j1,j) for HRU 
      end type subsurface_drainage
      type (subsurface_drainage), dimension (:), allocatable :: sdr

      end module tiles_data_module 