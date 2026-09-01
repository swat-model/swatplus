      module hydrology_data_module
    
      implicit none
     
      !! hydrology.hyd
      type hydrology_db
        character(len=16) :: name = ""!!none          |0          |0      |name
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
        real :: perco = 0.         !!none          |0-1           |percolation coefficient for the bottom soil layer.
                                   !!              |              |NOT a linear adjustment, despite this comment's long-
                                   !!              |              |standing claim. topohyd_init turns perco into
                                   !!              |              |hru%hyd%perco_lim, the multiplier swr_percmicro applies
                                   !!              |              |to seepage out of the last layer only. That nest of
                                   !!              |              |exp/log reduces exactly to a reciprocal-log law:
                                   !!              |              |  perco_lim = amin1(1., exp(-5.6862)*(-log(perco))**(-1.0052))
                                   !!              |              |            ~= 0.00339 / (-log(perco))
                                   !!              |              |so the multiplier is minute over most of the 0-1 range
                                   !!              |              |and does nearly all its work in the last 0.05:
                                   !!              |              |  perco     0.50   0.90   0.95   0.99   0.9965
                                   !!              |              |  perco_lim .0049  .0326  .0672  .3457  1.0
                                   !!              |              |The amin1 saturates at perco = 0.9965134 in single
                                   !!              |              |precision: every value at or above that is identical
                                   !!              |              |to perco = 1.0 (verified byte-for-byte), so the whole
                                   !!              |              |usable range lies below it. Values are NOT evenly
                                   !!              |              |spaced in effect - to sample perco_lim evenly use
                                   !!              |              |perco = exp(-(0.00339/perco_lim)**(1/1.0052)).
                                   !!              |              |Realized percolation is damped further still, because
                                   !!              |              |throttling seepage lets (st - fc) build in the last
                                   !!              |              |layer and partly compensate. On IA-Clayton_Test_Case a
                                   !!              |              |204x span in perco_lim (.0049 to 1.0) moved annual perc
                                   !!              |              |only 2.1x (14.6 to 30.8 mm/yr), and above perco_lim
                                   !!              |              |0.05 a 20x span moved it 4%. Back-pressure shows up in
                                   !!              |              |stored water and lateral flow instead.
                                   !!              |              |NOTE: topohyd_init overrides perco to 0.1 on any
                                   !!              |              |tile-drained HRU, ignoring whatever is in the file.
        real :: lat_orgn = 0.      !!ppm           |organic N concentration in lateral flow
        real :: lat_orgp = 0.      !!ppm           |organic P concentration in lateral flow
        real :: pet_co  = 1.0      !!none          |coefficient related to radiation used in Hargreaves equation
        real :: latq_co = 0.3      !!none          |0-1           |lateral soil flow coefficient - linear adjustment to daily lat flow
      end type hydrology_db
      type (hydrology_db), dimension (:), allocatable :: hyd_db
            
      end module hydrology_data_module 