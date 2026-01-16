      module recall_module
    
      implicit none
    
      type constituent_file_data
        character(len=25) :: name = ""
        character(len=13) :: units = ""          !mass, conc
        character(len=13) :: tstep = ""          !day, mo, yr, aa
      end type constituent_file_data
      
      type recall_databases
        character(len=13) :: name = ""
        type (constituent_file_data) :: org_min
        type (constituent_file_data) :: pest
        type (constituent_file_data) :: path
        type (constituent_file_data) :: hmet
        type (constituent_file_data) :: salt
        type (constituent_file_data) :: constit
      end type recall_databases
      
      !! use this type for all recall objects including exco and dr
      !! exco and dr are average annual recalls - all data in one file
      !! recall are for daily, monthly, and annual time series - each recall is individual file
      type (recall_databases), dimension(:), allocatable :: recall_db
      !type (recall_databases), dimension(:), allocatable :: exco_db
      !type (recall_databases), dimension(:), allocatable :: dr_db
      
      end module recall_module