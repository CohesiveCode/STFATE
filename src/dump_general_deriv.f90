module mod_dump_gen_deriv
   !! Derivatives that are general to the entire dump process
   use stdlib_kinds, only: dp
   use mod_search, only: binary_search
   implicit none(type, external)
   private
   public :: calc_dump_drho_dy, calc_dump_dP_dt, calc_dump_dB_dt, calc_dump_dMass_dt

contains

   pure function calc_dump_drho_dy(yi, y, rho) result(drho_dy)
      !! Calc the spatial derivative of density in 1d
      real(dp), intent(in) :: yi           ! Location of interest
      real(dp), intent(in) :: y(:)         ! Vertical coordinates
      real(dp), intent(in) :: rho(:)       ! Density values
      real(dp) :: drho_dy                  ! Density derivative

      integer :: i, n
      ! Check that rho_a and y have the same size
      if (size(rho) /= size(y)) then
         error stop "Error: rho and y must have the same size"
      end if

      n = size(y)

      ! Find the interval [y(i), y(i+1)] that contains yi
      i = binary_search(yi, y(1:n))

      if (i == n) i = n - 1  ! Handle edge case where yi is exactly y(n)

      drho_dy = (rho(i+1) - rho(i)) / (y(i+1) - y(i))
   end function calc_dump_drho_dy

   elemental pure function calc_dump_dP_dt(S_i) result(dP_dt)
      !! Ref(1) - Eqn. 3.4 - Calculate the time derivative of the solid volume of the ith component in the cloud
      ! dP_i/dt = - S_i
      real(dp), intent(in) :: S_i    !! Volume rate of solids settling out of the cloud (m^3/s)
      real(dp)             :: dP_dt  !! Result: Time derivative of the solid volume of each component in the cloud (m^3/s)

      dP_dt = -S_i
   end function calc_dump_dP_dt

   pure function calc_dump_dB_dt(E, surf_rho_a, rho_a, S_i, rho_p) result(dB_dt)
      !! Eqn. 3.3 - Calculate the time derivative of the buoyancy of the cloud
      ! dB/dt = E * surf_rho_a - rho_a - sum(S_i * (surf_rho_a - rho_p))
      real(dp), intent(in) :: E            ! Entrainment rate in m^3/s
      real(dp), intent(in) :: surf_rho_a   ! Ambient density at the surface of the cloud
      real(dp), intent(in) :: rho_a        ! Density of the ambient fluid (kg/m^3)
      real(dp), intent(in) :: S_i(:)       ! Volume rate of solids settling out of the cloud (m^3/s)
      real(dp), intent(in) :: rho_p(:)     ! Density of particles in cloud (kg/m^3)
      real(dp)             :: dB_dt        ! Result: Time derivative of the buoyancy of the cloud in kg/s^2

      dB_dt = E * (surf_rho_a - rho_a) - sum(S_i * (surf_rho_a - rho_p))
   end function calc_dump_dB_dt

   pure function calc_dump_dMass_dt(E, rho_a, S_i, rho_p) result(dMass_dt)
      !! Ref. (1) - Eqn. 3.1, 3.22 - Calculate time derivative of the mass of the cloud
      real(dp), intent(in) :: E        !! Volume Entrainment rate
      real(dp), intent(in) :: rho_a    !! Ambient Density
      real(dp), intent(in) :: S_i(:)   !! Volume rate of solids setting out of the cloud
      real(dp), intent(in) :: rho_p(:) !! Density of particles in cloud
      real(dp)             :: dMass_dt !! Rate of the cloud's mass change

      dMass_dt = E * rho_a - sum(S_i * rho_p)
   end function calc_dump_dMass_dt

end module mod_dump_gen_deriv
