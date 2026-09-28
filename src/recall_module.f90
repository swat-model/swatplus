      module recall_module
    
      implicit none
    
      type recall_databases
        character(len=13) :: name = ""
        character(len=25) :: org_min
        character(len=25) :: pest
        character(len=25) :: path
        character(len=25) :: hmet
        character(len=25) :: salt
        character(len=25) :: constit
        integer :: iorg_min
        integer :: ipest
        integer :: ipath
        integer :: ihmet
        integer :: isalt
        integer :: iconstit
        character(len=50) :: descrip
      end type recall_databases
      
      !! use this type for all recall objects including exco and dr
      !! exco and dr are average annual recalls - all data in one file
      !! recall are for daily, monthly, and annual time series - each recall is individual file
      type (recall_databases), dimension(:), allocatable :: recall_db
      !type (recall_databases), dimension(:), allocatable :: exco_db
      !type (recall_databases), dimension(:), allocatable :: dr_db
      
      end module recall_module