module instant_discharge_math
    use stdlib_constants, only: pi_dp
    use stdlib_math, only: 
    use stdlib_kinds, only: dp
    
    implicit none(type, external)

    
contains
    
    subroutine derivd(E,U,W,DEPTH)
        real(dp), intent(in)  :: E       ! Entrainment rate in m/s
        real(dp), intent(in)  :: U(3)    ! Velocity of the cloud in m/s
        real(dp), intent(in)  :: W       ! Vertical velocity of the ambient fluid in m/s
        real(dp), intent(out) :: DEPTH   ! Rate of change of depth in m/s
        
        DEPTH = E * (norm2(U) - W) / norm2(U)
    end subroutine derivd
end module instant_discharge_math