      module hydrology_data_module
    
      implicit none
     
      !! hydrology.hyd
      type hydrology_db
        character(len=16) :: name  !!none          |0          |0      |name
        real :: lat_ttime = 0.     !!days          |0-120      |0      |Exponential of the lateral flow travel time
        real :: lat_sed = 0.       !!g/L           |sediment concentration in lateral flow
        real :: canmx = 0.         !!mm H2O        |maximum canopy storage
        real :: esco = 0.          !!none          |soil evaporation compensation factor (0-1)
        real :: epco = 0.          !!none          |plant water uptake compensation factor (0-1)
        real :: erorgn = 0.        !!none          |organic N enrichment ratio, if left blank
                                   !!%             |the model will calculate for every event
        real :: erorgp = 0.        !!none          |organic P enrichment ratio, if left blank
                                   !!%             |the model will calculate for every event
        real :: cn3_swf = 0.       !!none          |soil water at cn3 - 0=fc; .99=near saturation
        real :: biomix = 0.        !!none          |biological mixing efficiency.
                                   !!%             |Mixing of soil due to activity of earthworms and other soil biota.
                                   !!%             |Mixing is performed at the end of every calendar year.
        real :: perco = 0.         !!none          |0-1           |percolation coefficient - linear adjustment to daily perc
        real :: lat_orgn = 0.      !!ppm           |organic N concentration in lateral flow
        real :: lat_orgp = 0.      !!ppm           |organic P concentration in lateral flow
        real :: pet_co  = 1.0      !!none          |coefficient related to radiation used in Hargreaves equation
        real :: latq_co = 0.3      !!none          |0-1           |lateral soil flow coefficient - linear adjustment to daily lat flow
      end type hydrology_db
      type (hydrology_db), dimension (:), allocatable :: hyd_db
            
      end module hydrology_data_module 