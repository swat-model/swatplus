      module recall_module
    
      implicit none
    
      type recall_databases
        character(len=13) :: name = ""
        integer :: units = 1                !1=mass, 2=mass/area, 3=frac for dr, 4=cms and concentration
        character (len=16) :: org_min       !name of organic-mineral data (filename for recall, and object in file for exco/dr)
        character (len=16) :: pest_com      !name of pesticide community (pest_init)
        character (len=16) :: pest_dat      !name of pesticide data (filename for recall, and object in file for exco/dr)
        character (len=16) :: path_com      !name of pathogen community (path_init)
        character (len=16) :: path_dat      !name of pathogen data (filename for recall, and object in file for exco/dr)
        character (len=16) :: hmet_com      !name of heavy metal community (hmet_init)
        character (len=16) :: hmet_dat      !name of heavy metal data (filename for recall, and object in file for exco/dr)
        character (len=16) :: salt_com      !name of salt ion community (salt_init)
        character (len=16) :: salt_dat      !name of salt ion data (filename for recall, and object in file for exco/dr)
      end type recall_databases
      
      !! use this type for all recall objects including exco and dr
      !! exco and dr are average annual recalls - all data in one file
      !! recall are for daily, monthly, and annual time series - each recall is individual file
      type (recall_databases), dimension(:), allocatable :: recall_db
      !type (recall_databases), dimension(:), allocatable :: exco_db
      !type (recall_databases), dimension(:), allocatable :: dr_db
      
      end module recall_module