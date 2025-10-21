module mod_dump_descent
   !! Modulue defines functions related to instant descent of dredged material
   !! Use double comments after to document functions and modules for automatic
   !! documentation generation tools.
   !! dump_descent shortened to "dd" for function names
   use stdlib_kinds, only: dp
   use mod_constants, only: pi, g
   use mod_dump_gen, only: calc_dump_F_drag
   use mod_dump_gen_deriv, only: calc_dump_drho_dy

   implicit none(type, external)

   private
   public :: calc_dump_des_E, calc_dump_des_F_drag, calc_dump_des_A, calc_dump_des_vorticity, &
      dump_dump_des_alpha, calc_dump_des_beta, calc_dump_des_epsilon

contains

   pure function calc_dump_des_E(vert_radius, alpha, U_c, U_a) result(E)
      !! Eqn. 3.5 - Calculate the entrainment rate based on mean radius and entrainment coefficient
      real(dp), intent(in) :: vert_radius    !! Vertical radius of the cloud, [L]
      real(dp), intent(in) :: alpha          !! Entrainment coefficient (dimensionless), [-]
      real(dp), intent(in) :: U_c(3), U_a(3) !! Velocity of the cloud, []
      real(dp)             :: E              !! Entrainment rate, $[L^3 / t]$

      ! Local variables
      real(dp) :: rel_vel_mag

      rel_vel_mag = norm2( U_c - U_a )

      E = 2.0_dp * pi * vert_radius**2 * alpha * rel_vel_mag
   end function calc_dump_des_E

   pure function calc_dump_des_F_drag(rho_a, vert_radius, U, U_a, C_d) result(F_drag)
      !! Eqn. 3.9-3.11 - Calculate the drag force acting on the cloud
      real(dp), intent(in) :: rho_a          !! Density of the ambient fluid, []
      real(dp), intent(in) :: vert_radius    !! Mean radius of the cloud, []
      real(dp), intent(in) :: U(3), U_a(3)   !! Velocity of the cloud and ambient current, []
      real(dp)             :: F_drag(3)      !! Drag force acting on the cloud, []
      real(dp), intent(in) :: C_d            !! Drag coefficient (dimensionless), [-]

      ! Local variables
      real(dp) :: rel_vel_mag
      real(dp), dimension(3) :: shape_effect

      rel_vel_mag = norm2(U - U_a)

      ! Due to the objects shape need to calculate the drag force vectorially
      shape_effect = [0.5_dp, 1.0_dp, 0.5_dp]  ! Shape effect due to the oblate spheroid shape of the cloud

      F_drag = calc_dump_F_drag(rho_a, vert_radius, vert_radius, U, U_a, C_d, shape_effect)
   end function calc_dump_des_F_drag

   elemental pure function calc_dump_des_A(vert_radius, surf_rho_a, C) result(A)
      !! Ref(1) - Eqn. 3.15 - Calculate the dissipation parameter of the cloud, []
      real(dp), intent(in) :: vert_radius  !! Mean radius of the cloud, []
      real(dp), intent(in) :: C            !! Empirical constant (dimensionless), recommended value is 3.0, []
      real(dp), intent(in) :: surf_rho_a   !! Density of the ambient fluid at the surface, []
      real(dp)             :: A            !! Dissipation parameter, []

      A = C * (vert_radius)**2 * g / surf_rho_a
   end function calc_dump_des_A

   elemental pure function calc_dump_des_vorticity(v_vel, b) result(vorticity)
      !! Calculate the vorticity of the cloud
      ! TODO: Look into this more. I'm not sure where this equation comes from
      !       or if it's correct.
      real(dp), intent(in) :: v_vel      !! Vertical velocity of the cloud, []
      real(dp), intent(in) :: b          !! Lateral radius of the cloud, []
      real(dp)             :: vorticity  !! Vorticity of the cloud , [1/t]
      vorticity = v_vel * b
   end function calc_dump_des_vorticity

   elemental pure function dump_dump_des_alpha(B, K, alpha_0, C_1) result(alpha)
      !! Ref. (1) - Eqn. 3.18 - Calculate the entrainment coefficient.
      !! This is a superset of Ref. (1) - Eqn. 3.17
      real(dp), intent(in) :: B       !! Buoyancy of the cloud, []
      real(dp), intent(in) :: alpha_0 !! entrainmen coefficient for turbulent thermals, []
      real(dp), intent(in) :: C_1     !! Empirical constant, []
      real(dp)             :: alpha   !! Entrainment coefficient (dimensionless), []

      ! Local variables
      real(dp) :: tanh_arg
      real(dp), intent(in) :: K            ! Empirical constant, suggested between 1 and 2, [-]

      tanh_arg = B / (2.0_dp * pi * g * C_1 * K**2 * alpha_0)
      ! Squaring then square is equivalent to taking the absolute value. Left this way to match paper.
      alpha = alpha_0 * sqrt( tanh(tanh_arg)**2 )
   end function dump_dump_des_alpha

   elemental pure function calc_dump_des_beta(v, v_fi, beta_0) result(beta)
      !! Ref. (1) - Eqn. 3.20 -  Calculate the empirical constant beta (setttling coefficient) - bottom of page 1-34
      real(dp), intent(in) :: v        !! Velocity of the cloud, []
      real(dp), intent(in) :: v_fi     !! Fall velocity of individual particles in m/s, []
      real(dp), intent(in) :: beta_0   !! Empirical constant (dimensionless), assumed to be known, []
      real(dp)             :: beta     !! Empirical constant (dimensionless), []

      ! Local variables
      real(dp) :: ratio ! Ratio of cloud vertical velocity to particle fall velocity

      ratio = abs(v / v_fi)

      if (ratio < 1.0_dp) then
         beta = 0.0_dp
      else if (ratio >= 1.0_dp) then
         beta = beta_0
      end if
   end function calc_dump_des_beta

   pure function calc_dump_des_epsilon(yi, y, rho_a) result(epsilon)
      !! Ref. (1) - Eqn. 3.16 - Calculate the vertical gradient of the turbulent dissipation rate
      ! dε/dy = (d rho_a / dy)
      !!NOTE: Enter the y-coordinate for the clouds centroid here
      !! For the descent case, ε is the same as drho/dy
      real(dp), intent(in) :: yi          !! Vertical position  - y-coordinate of the clouds centroid, []
      real(dp), intent(in) :: rho_a(:)    !! Density of the ambient fluid, []
      real(dp), intent(in) :: y(:)        !! 1D array of y-coordinates (must be sorted), []
      real(dp)             :: epsilon     !! Vertical gradient of the turbulent dissipation rate, [L^2 / t^3]

      ! Calculate the finite difference approximation of the gradient
      epsilon = calc_dump_drho_dy(yi, y, rho_a)

      ! Could implement more accurate complicated methods, but this should be sufficient for now
   end function calc_dump_des_epsilon

end module mod_dump_descent
