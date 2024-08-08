      module reservoir_conditions_module
    
      implicit none
      
      
      !! real :: stor, inflo, pdsi   nbs
      real :: release = 0.
      integer :: day = 0
      character(1) :: hit = ""
      
      type cond
        character(10) :: var = ""
        character(2) :: op = ""
        real :: val = 0.
      end type cond
      
      type conditions
        integer :: num_conds = 0
        real :: action = 0.
        type (cond), dimension(:), allocatable :: scon
      end type conditions
      
      type modules
        integer :: num_conds = 0
        type (conditions), dimension(:), allocatable :: con
      end type modules
     
      type reservoir_condition_tables
        character(25) :: name = ""
        integer :: num_tbl = 0
        integer :: num_conds = 0
        integer :: num_modules = 0
        type (conditions), dimension(:), allocatable :: conds
        type (modules), dimension(:), allocatable :: mods
      end type 
      type (reservoir_condition_tables), dimension(:), allocatable :: ctbl
      
      end module reservoir_conditions_module