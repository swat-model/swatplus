      module topography_data_module
    
      implicit none
     
      type topography_db
        character(len=16) :: name = "default"
        real :: slope = .02       !!	hru_slp(:) |m/m           |average slope steepness in HRU
        real :: slope_len = 50.   !! slsubbsn(:)   |m             |average slope length for erosion
        real :: lat_len = 50.     !! slsoil(:)     |m             |slope length for lateral subsurface flow
        real :: dis_stream = 100. !! dis_stream(:) |m             |average distance to stream
        real :: dep_co = 1.       !!               |              |deposition coefficient
      end type topography_db
      type (topography_db), dimension (:), allocatable :: topo_db
      
     type fields_db
           character(len=16) :: name = "default"
           real :: length = 500. !!               |m             |field length for wind erosion
           real :: wid = 100.    !!               |m             |field width for wind erosion
           real :: ang = 30.     !!               |deg           |field angle for wind erosion
      end type fields_db
      type (fields_db), dimension (:), allocatable :: field_db
      
      end module topography_data_module 