module mod_instant_descent_deriv
    ! Module to calculate the derivatives of the instant descent equations
    use stdlib_kinds, only: dp
    use constants, only: g, pi
    use mod_search, only: binary_search

    implicit none
    
contains
    pure function calc_dM_dt(F_b, drag, E, rho_a, U_a, S_i, rho_p, U_c) result(dM_dt)
        !! Eqn. 3.2 - Calculate the time derivative of the momentum of the cloud
        ! dM/dt = [0, F_b, 0] - D + E * rho_a * U_a - sum(S_i * rho_p) * U_c
        ! Note: S_i and rho_p are arrays for each particle type in the cloud
        real(dp), intent(in) :: F_b          ! Buoyancy force acting on the cloud
        real(dp), intent(in) :: drag(3)      ! Drag force on the cloud
        real(dp), intent(in) :: E            ! Entrainment rate in m^3/s
        real(dp), intent(in) :: rho_a        ! Density of the ambient fluid (kg/m^3)
        real(dp), intent(in) :: U_a(3)       ! Ambient velocity at the location of the cloud
        real(dp), intent(in) :: S_i(:)       ! Volume rate of solids settling out of the cloud (m^3/s)
        real(dp), intent(in) :: rho_p(:)     ! Density of particles in cloud
        real(dp), intent(in) :: U_c(3)       ! Velocity of the cloud in m/s
        real(dp)             :: dM_dt(3)     ! Result: Time derivative of the momentum of the cloud in kg·m/s^2

        ! Not entirely sure why S_i * rho_p is multiplied by U_c, instead of the relative particle velocities 
        ! but this is how it is in the paper
        dM_dt = [0.0_dp, F_b, 0.0_dp] - drag + E * rho_a * U_a - sum(S_i * rho_p) * U_c
    end function calc_dM_dt

    pure function calc_dB_dt(E, surf_rho_a, rho_a, S_i, rho_p) result(dB_dt)
        !! Eqn. 3.3 - Calculate the time derivative of the buoyancy of the cloud
        ! dB/dt = E * surf_rho_a - rho_a - sum(S_i * (surf_rho_a - rho_p))
        real(dp), intent(in) :: E            ! Entrainment rate in m^3/s
        real(dp), intent(in) :: surf_rho_a   ! Ambient density at the surface of the cloud
        real(dp), intent(in) :: rho_a        ! Density of the ambient fluid (kg/m^3)
        real(dp), intent(in) :: S_i(:)       ! Volume rate of solids settling out of the cloud (m^3/s)
        real(dp), intent(in) :: rho_p(:)     ! Density of particles in cloud (kg/m^3)
        real(dp)             :: dB_dt        ! Result: Time derivative of the buoyancy of the cloud in kg/s^2

        dB_dt = E * (surf_rho_a - rho_a) - sum(S_i * (surf_rho_a - rho_p))
    end function calc_dB_dt

    pure function dP_dt(S_i)
        !! Eqn. 3.4 - Calculate the time derivative of the solid volume of the ith component in the cloud
        ! dP_i/dt = - S_i
        real(dp), intent(in) :: S_i(:)       ! Volume rate of solids settling out of the cloud (m^3/s)
        real(dp)             :: dP_dt(size(S_i)) ! Result: Time derivative of the solid volume of each component in the cloud (m^3/s)

        dP_dt = -S_i
    end function dP_dt

    pure function calc_dK_dt(A, density_gradient) result(dK_dt)
        !! Eqn. 3.14 - Calculate the time derivative of the kinetic energy of the ambient flow field
        ! dK/dt = A · density_gradient
        real(dp), intent(in) :: A                     ! Dissipation parameter (kg)
        real(dp), intent(in) :: density_gradient      ! Vertical density gradient of the ambient density field (kg/m^4)
        real(dp)             :: dK_dt                 ! Result: Time derivative of the kinetic energy of the ambient flow field (kg·m^2/s^3)

        dK_dt = -A * density_gradient
    end function calc_dK_dt

    pure function calc_dEpsilon_dy(cloud_pos, rho_a, y) result(dEpsilon_dy)
        !! Eqn. 3.16 - Calculate the vertical gradient of the turbulent dissipation rate
        ! dε/dy = - (g / rho_a) * (d rho_a / dy)
        !NOTE: Pretty sure this is going to be the derivative around the clouds centroid
        !TODO: Look back at this.
        real(dp), intent(in) :: cloud_pos(3)   ! Position of the cloud (x, y, z)
        real(dp), intent(in) :: rho_a(:)        ! Density of the ambient fluid (kg/m^3)
        real(dp), intent(in) :: y(:)         ! 1D array of y-coordinates (must be sorted)
        real(dp)             :: dEpsilon_dy ! Result: Vertical gradient of the turbulent dissipation rate (m^2/s^3 per meter)

        integer :: j, n

        ! Check that rho_a and y have the same size
        if (size(rho_a) /= size(y)) then
            error stop "Error: rho_a and y must have the same size"
        end if

        n = size(y)
        if (cloud_pos(2) < y(1) .or. cloud_pos(2) > y(n)) then
            ! print *, "Point: ", cloud_pos(2)
            ! print *, "Coordinates: ", y(1), y(n)
            error stop "Error: cloud_pos(2) is out of bounds"
        end if
        ! Find the interval [y(i), y(i+1)] that contains cloud_pos(2)
        j = binary_search(cloud_pos(2), y(1:n))
        if (j == n) j = n - 1  ! Handle edge case where cloud_pos(2) is exactly y(n)

        ! Calculate the finite difference approximation of the gradient
        ! Setting the first element to zero as we cannot calculate the gradient there
        dEpsilon_dy = (rho_a(n+1) - rho_a(n)) / (y(n+1) - y(n))

        ! Could implement more accurate complicated methods, but this should be sufficient for now
    end function calc_dEpsilon_dy

end module mod_instant_descent_deriv
