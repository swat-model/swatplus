    !Purpose: Calcualte N or P flow from Pool A to Pool B, as well as immobilization or mineralization
    SUBROUTINE nut_np_flow (                                           &                                 
        C_A, E_A, CEtoB, C_AtoB, CO2fromA,                                  &   !Input             
        E_AtoB, IMM_AtoB, MNR_AtoB)                          !Output

    IMPLICIT NONE

    real, intent(in) :: C_A            !         |Amount of carbon in pool A at the beginning of the time step (kg C ha-1)
    real, intent(in) :: E_A            !         | Amount of N or P in pool A at the beginning of the time step (kg C ha-1)
    real, intent(in) :: CEtoB          !         | C:E ratio (i.e. C:N or C:P) of the material enters pool B (fraction)
    real, intent(in) :: C_AtoB         !         | C flow from pool A to pool B (kg N or P ha-1 day-1)
    real, intent(in) ::  CO2fromA      !         | CO2 emission during the decomposition of pool A (kg C ha-1 day-1)
    real, intent(out) :: E_AtoB        !         | E (i.e. N or P) flow from pool A to B (kg N or P ha-1 day-1)
    real, intent(out) :: IMM_AtoB      !         | Immobilization of E in order to satisfy the CEtoB ratio (kg N or P ha-1 day-1)
    real, intent(out) :: MNR_AtoB      !         | Mineralization of E as the result of pool A to Pool B transformation (kg N or P ha-1 day-1)

    !!Local
    real :: EfromCO2                   !         |
    real :: efco2                      !         |E mineralization resulting from CO2 respiration as result of pool A decomposition (kg N or P ha-1 day-1)  

    !Initialize to zero. 
    E_AtoB = 0.
    EfromCO2 = 0.
    MNR_AtoB = 0.
    IMM_AtoB = 0.

    ! IF N or P in pool A is zero or no C flows from pool A to B, then skip the rest of the calculationg and return
    IF (E_A .LT. 1.E-6 .OR. C_AtoB .LT. 1.E-6 .OR. C_A .LT. 1.E-6) RETURN

    !Calcualting E (N or P) transformed from pool A to pool B by assuming E flow is proportional to C flow
    E_AtoB = E_A * (C_AtoB / C_A)

    !E supply due to CO2 emission during the transformation of C from Pool A to B.
    IF (CO2fromA .LT. 0.) THEN
        EfromCO2 = 0.
    ELSE
        EfromCO2 = E_A * (CO2fromA / C_A)
    ENDIF

    !If C:E ratio of the material transformed from pool A to B is larger than
    !C:E ratio that is permitted by the receiving pool B, then immobilization of
    !N or P is required to lower C:E ratio of the transformed material to meet
    ! the requirement by pool B.
    IF (C_AtoB / E_AtoB .GT. CEtoB) THEN
        !Potential immobilization is calcualted as:
        IMM_AtoB = C_AtoB / CEtoB - E_AtoB

    ELSE
        !The amount of E that flows from pool A to pool B is enough to
        !satisfy the condition of the C/E ratio that is allowed to enter
        !pool B. The rest of the E coming from pool A is mineralized.
        MNR_AtoB = E_AtoB - C_AtoB / CEtoB

        !Correct the E flow from pool A to pool B for the E
        !mineralization.
        E_AtoB = E_AtoB - MNR_AtoB
    ENDIF

    !Sum the E released due to CO2 respiration with the E release
    !related to the flow from pool A to B.
    MNR_AtoB = MNR_AtoB + EFCO2


    RETURN
    END SUBROUTINE nut_np_flow