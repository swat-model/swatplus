      module reservoir_conditions_module
    
      implicit none
      
      
      !! real :: stor, inflo, pdsi   nbs
      real :: release
      integer :: day
      character(1) :: hit
      
      type cond
        character(10) :: var
        character(2) :: op
        real :: val
      end type cond
      
      type conditions
        integer :: num_conds
        real :: action
        type (cond), dimension(:), allocatable :: scon
      end type conditions
      
      type modules
        integer :: num_conds
        type (conditions), dimension(:), allocatable :: con
      end type modules
     
      type reservoir_condition_tables
        character(25) :: name
        integer :: num_tbl
        integer :: num_conds
        integer :: num_modules
        type (conditions), dimension(:), allocatable :: conds
        type (modules), dimension(:), allocatable :: mods
      end type 
      type (reservoir_condition_tables), dimension(:), allocatable :: ctbl
      
      end module reservoir_conditions_module