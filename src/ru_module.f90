      module ru_module
    
      implicit none 

      integer :: iru                               !none            |counter
      integer :: mru_db                            !                |
      real, dimension (:), allocatable :: ru_tc    !                |    
      real, dimension (:), allocatable :: ru_n     !                |
      integer, dimension (:), allocatable :: itsb  !none            |end of loop
   
      type ru_databases_char
        character(len=16) :: elem_def = ""
        character(len=16) :: elem_dr = ""
        character(len=16) :: toposub_db = ""
        character(len=16) :: field_db = ""
      end type ru_databases_char
      
      type ru_databases
        integer :: elem_def = 1
        integer :: elem_dr = 1
        integer :: toposub_db = 1
        integer :: field_db = 1
      end type ru_databases
          
      type field
        character(len=13) :: name = "default"
        real :: length = 500. !!               |m             |field length for wind erosion
        real :: wid = 100.    !!               |m             |field width for wind erosion
        real :: ang = 30.     !!               |deg           |field angle for wind erosion
      end type field
      
      type ru_parameters
        character(len=16) :: name = ""
        real :: da_km2 = 0.                         !! km2      |drainage area
        type (ru_databases_char) :: dbsc
        type (ru_databases) :: dbs
        type (field) :: field
      end type ru_parameters
      type (ru_parameters), dimension(:), allocatable :: ru

      end module ru_module