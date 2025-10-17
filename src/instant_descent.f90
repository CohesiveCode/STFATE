module mod_instant_descent
   !! Modulue defines functions related to instant descent of dredged material
   !! Use double comments after to document functions and modules for automatic
   !! documentation generation tools.
   use stdlib_kinds, only: dp
   use constants, only: g, pi
   use stdlib_linalg, only: norm
   use mod_set_default, only: set_default
   implicit none
   private
   public :: calc_cloud_vol, calc_E, calc_S_i, calc_momentum, calc_F_b, calc_drag, &
             calc_B, calc_P_i, calc_A_param, calc_vorticity, calc_alpha, calc_beta
contains

   elemental pure function calc_cloud_vol(vert_radius) result(volume)
      !! Calc the volume of a cloud given its mean radius
      real(dp), intent(in) :: vert_radius  ! Mean radius of the cloud in meters
      real(dp)             :: volume       ! Result: Volume of the cloud in cubic meters
      volume = (2.0_dp / 3.0_dp) * pi * vert_radius**3.0_dp
   end function calc_cloud_vol

   pure function calc_E(vert_radius, alpha, U_c, U_a) result(E)
      !! Eqn. 3.5 - Calculate the entrainment rate based on mean radius and entrainment coefficient
      real(dp), intent(in) :: vert_radius   ! Mean radius of the cloud in meters
      real(dp), intent(in) :: alpha         ! Entrainment coefficient (dimensionless)
      real(dp), intent(in) :: U_c(3), U_a(3)    ! Velocity of the cloud in m/s
      real(dp)             :: E             ! Result: Entrainment rate in m^3/s

      ! Local variables
      real(dp) :: rel_vel_mag

      ! Calc the norm of the velocity difference between the ambient current and the cloud
      rel_vel_mag = norm(U_c-U_a,2)
      E = 2.0_dp * pi * vert_radius**2 * alpha * rel_vel_mag
   end function calc_E

   pure elemental function calc_S_i(vert_radius, v_fi, C_s, beta) result(S_i)
      !! Eqn. 3.6 - Calculate the volume rate of solids settling out of the cloud
      real(dp), intent(in) :: vert_radius  ! Mean radius of the cloud in meters
      real(dp), intent(in) :: v_fi         ! Fall velocity of individual particles in m/s
      real(dp), intent(in) :: C_s          ! Concentration of solids in the cloud (kg/m^3)
      real(dp), intent(in) :: beta         ! Empirical constant (dimensionless)
      real(dp)             :: S_i          ! Result: Volue rate of solids settling out of the cloud (m^3/s)

      S_i = pi * vert_radius**2 * abs(v_fi) * C_s * (1.0_dp - beta)
   end function

   pure function calc_momentum(C_m, rho_c, vol_c, U) result(M)
      !! Eqn. 3.7 - Calculate the momentum of the cloud
      real(dp), intent(in) :: C_m          ! Apparent mass coefficient (dimensionless), suggested between 0.5 and 1.5
      real(dp), intent(in) :: rho_c        ! Density of the cloud (kg/m^3)
      real(dp), intent(in) :: vol_c        ! Volume of the cloud
      real(dp), intent(in) :: U(3)         ! Velocity of the cloud in m/s
      real(dp)             :: M(3)         ! Result: Momentum of the cloud in kg·m/s

      M = C_m * rho_c * vol_c * U
   end function calc_momentum

   pure function calc_F_b(rho_a, rho_c, g, vert_radius) result(F_b)
      !! Eqn. 3.8 - Calculate the buoyancy force acting on the cloud
      real(dp), intent(in) :: rho_a        ! Density of the ambient fluid (kg/m^3)
      real(dp), intent(in) :: rho_c        ! Density of the cloud (kg/m^3)
      real(dp), intent(in) :: g            ! Acceleration due to gravity (m/s^2)
      real(dp), intent(in) :: vert_radius  ! Mean radius of the cloud in meters
      real(dp)             :: F_b            ! Result: Buoyancy force acting on the cloud (N)

      F_b = (rho_c - rho_a) * g * calc_cloud_vol(vert_radius)
   end function calc_F_b

   pure function calc_drag(rho_a, vert_radius, U, U_a, C_d) result(F_d)
      !! Eqn. 3.9-3.11 - Calculate the drag force acting on the cloud
      !! C_d is optional and if provided uses the suggested value of 0.5 - page 1-32 paragraph 2
      real(dp), intent(in) :: rho_a        ! Density of the ambient fluid (kg/m^3)
      real(dp), intent(in) :: vert_radius  ! Mean radius of the cloud in meters
      real(dp), intent(in) :: U(3), U_a(3)   ! Velocity of the cloud and ambient current in m/s
      real(dp)             :: F_d(3)       ! Result: Drag force acting on the cloud (N)
      real(dp),  intent(in), optional:: C_d         ! Drag coefficient (dimensionless)
      
      ! Local variables
      real(dp) :: C_d_local
      real(dp) :: rel_vel_mag
      real(dp), dimension(3) :: shape_effect

      if (present(C_d)) then
         C_d_local = C_d
      else
         C_d_local = 0.5_dp  ! Default value according to paper suggestion
      end if

      rel_vel_mag = norm(U - U_a, 2)

      ! Due to the objects shape need to calculate the drag force vectorially
      shape_effect = [0.5_dp, 1.0_dp, 0.5_dp]  ! Shape effect due to the oblate spheroid shape of the cloud

      F_d = 0.5_dp * rho_a * C_d * shape_effect * pi * vert_radius**2 * rel_vel_mag * (U - U_a)
   end function calc_drag

   pure function calc_B(surf_rho_a, rho_c, vert_radius) result(B)
      !! Eqn. 3.12 - Calculate the buoyancy of the cloud
      !! TODO: Look into this more. I'm not sure why this is different from force_buoyancy
      real(dp), intent(in) :: surf_rho_a   ! Density of the ambient fluid at the surface (kg/m^3)
      real(dp), intent(in) :: rho_c        ! Density of the cloud (kg/m^3)
      real(dp), intent(in) :: vert_radius  ! Mean radius of the cloud in meters
      real(dp)             :: B            ! Result: Buoyancy of the cloud (kg)

      B = (surf_rho_a - rho_c)  * calc_cloud_vol(vert_radius)

   end function calc_B

   pure function calc_P_i(vert_radius, C_si) result(P_i)
      !! eqn. 3.13 - Calculate the volume of solids in the cloud
      real(dp), intent(in) :: vert_radius  ! Mean radius of the cloud in meters
      real(dp), intent(in) :: C_si         ! Concentration of solids in the cloud (kg/m^3),
      real(dp)             :: P_i          ! Result: Volume of solids in the cloud (m^3)

      P_i = calc_cloud_vol(vert_radius) * C_si
   end function calc_P_i

   pure function calc_A_param(vert_radius, surf_rho_a, g, C) result(A)
      !! Eqn. 3.15 - Calculate the dissipation parameter of the cloud
      real(dp), intent(in) :: vert_radius  ! Mean radius of the cloud in meters
      real(dp), intent(in), optional :: C  ! Empirical constant (dimensionless)
      real(dp), intent(in) :: surf_rho_a   ! Density of the ambient fluid at the surface (kg/m^3)
      real(dp), intent(in) :: g            ! Acceleration due to gravity (m/s^2)
      real(dp)             :: A            ! Result: Dissipation parameter (m^2/s^3)

      ! Local variables
      real(dp) :: C_local
      if (present(C)) then
         C_local = C
      else
         C_local = 3.0_dp  ! Equal to 3 according to Turner (1960)
      end if

      A = C_local * (vert_radius)**2 * g / surf_rho_a
   end function calc_A_param

   pure function calc_vorticity(v_vel, b) result(vorticity)
      !! Calculate the vorticity of the cloud
      ! TODO: Look into this more. I'm not sure where this equation comes from
      !       or if it's correct.
      real(dp), intent(in) :: v_vel      ! Vertical velocity of the cloud in m/s
      real(dp), intent(in) :: b          ! Lateral radius of the cloud in meters
      real(dp)             :: vorticity  ! Result: Vorticity of the cloud (1/s)
      vorticity = v_vel * b
   end function calc_vorticity

   pure function calc_alpha(B, g, K, alpha_0, C_1) result(alpha)
      !! Eqn. 3.18 - Calculate the entrainment coefficient
      real(dp), intent(in) :: B                 ! Buoyancy of the cloud (kg)
      real(dp)             :: alpha             ! Result: Entrainment coefficient (dimensionless)
      real(dp), intent(in), optional :: alpha_0 ! entrainmen coefficient for turbulent thermals
      real(dp), intent(in), optional :: C_1     ! Empirical constant (dimensionless), suggested 0.16

      ! Local variables
      real(dp) :: alpha_0_loc, C_1_loc, tanh_arg
      real(dp), intent(in) :: g            ! Acceleration due to gravity (m/s^2)
      real(dp), intent(in) :: K            ! Empirical constant (dimensionless), suggested between 1 and 2

      alpha_0_loc = set_default(alpha_0, 0.25_dp)  ! Default value according to paper suggestion
      C_1_loc = set_default(C_1, 0.16_dp)  ! Default value according to paper suggestion

      tanh_arg = B / (2 * pi * g *C_1_loc * K**2 * alpha_0_loc)
      ! Squaring then square is equivalent to taking the absolute value. Left this way to match paper.
      alpha = alpha_0_loc * sqrt( tanh(tanh_arg)**2 )
   end function calc_alpha

   pure elemental function calc_beta(v, v_fi, beta_0) result(beta)
      !! Calculate the empirical constant beta (setttling coefficient) - bottom of page 1-34
      real(dp), intent(in) :: v        ! Velocity of the cloud in m/s
      real(dp), intent(in) :: v_fi     ! Fall velocity of individual particles in m/s
      real(dp), intent(in) :: beta_0   ! Empirical constant (dimensionless), assumed to be known
      real(dp)             :: beta     ! Result: Empirical constant (dimensionless)

      ! Local variables
      real(dp) :: ratio ! Ratio of cloud vertical velocity to particle fall velocity
      
      ratio = abs(v / v_fi)

      if (ratio < 1.0_dp) then
         beta = 0.0_dp
      else if (ratio >= 1.0_dp) then
        beta = beta_0
      end if
   end function calc_beta
end module mod_instant_descent
