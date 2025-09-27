module instant_descent
   !! Modulue defines functions related to instant descent of dredged material
   !! Use double comments after to document functions and modules for automatic
   !! documentation generation tools.
   use stdlib_kinds, only: dp
   use stdlib_constants, only: pi_dp
   use stdlib_linalg, only: norm
   implicit none

contains

   elemental pure function calc_V_c(mean_radius) result(volume)
      !! Calc the volume of a cloud given its mean radius
      real(dp), intent(in) :: mean_radius  ! Mean radius of the cloud in meters
      real(dp)             :: volume       ! Result: Volume of the cloud in cubic meters
      volume = (2.0_dp / 3.0_dp) * pi_dp * mean_radius**3.0_dp
   end function calc_V_c

   pure function calc_E(mean_radius, alpha, U, U_a) result(E)
      !! Calculate the entrainment rate based on mean radius and entrainment coefficient
      real(dp), intent(in) :: mean_radius   ! Mean radius of the cloud in meters
      real(dp), intent(in) :: alpha         ! Entrainment coefficient (dimensionless)
      real(dp), intent(in) :: U(3), U_a(3)    ! Velocity of the cloud in m/s
      real(dp)             :: E             ! Result: Entrainment rate in m/s

      ! Local variables
      real(dp) :: rel_vel_mag

      ! Calc the norm of the velocity difference between the ambient current and the cloud
      rel_vel_mag = norm(U-U_a,2)
      E = 2.0_dp * pi_dp * mean_radius**2 * alpha * rel_vel_mag
   end function calc_E

   pure function calc_S_i(mean_radius, v_f, C_s, beta) result(S)
      real(dp), intent(in) :: mean_radius  ! Mean radius of the cloud in meters
      real(dp), intent(in) :: v_f(3)          ! Fall velocity of individual particles in m/s
      real(dp), intent(in) :: C_s          ! Concentration of solids in the cloud (kg/m^3)
      real(dp), intent(in) :: beta         ! Empirical constant (dimensionless)
      real(dp)             :: S            ! Result: Rate of solids passing through the cloud boundary (kg/s)

      S = beta * mean_radius**2 * norm(v_f,2) * C_s
   end function

   pure function calc_M(C_m, rho_c, mean_radius, U) result(M)
      real(dp), intent(in) :: C_m          ! Apparent mass coefficient (dimensionless), suggested between 0.5 and 1.5
      real(dp), intent(in) :: rho_c        ! Density of the cloud (kg/m^3)
      real(dp), intent(in) :: mean_radius  ! Mean radius of the cloud in meters
      real(dp), intent(in) :: U(3)         ! Velocity of the cloud in m/s
      real(dp)             :: M(3)         ! Result: Momentum of the cloud in kg·m/s

      M = C_m * rho_c * calc_V_c(mean_radius) * U
   end function calc_M

   pure function calc_F_b(rho_a, rho_c, g, mean_radius) result(F_b)
      real(dp), intent(in) :: rho_a        ! Density of the ambient fluid (kg/m^3)
      real(dp), intent(in) :: rho_c        ! Density of the cloud (kg/m^3)
      real(dp), intent(in) :: g            ! Acceleration due to gravity (m/s^2)
      real(dp), intent(in) :: mean_radius  ! Mean radius of the cloud in meters
      real(dp)             :: F_b            ! Result: Buoyancy force acting on the cloud (N)

      F_b = (rho_a - rho_c) * g * calc_V_c(mean_radius)
   end function calc_F_b

   pure function calc_drag(C_d, rho_a, mean_radius, U, U_a) result(F_d)
      real(dp),  intent(in) :: C_d         ! Drag coefficient (dimensionless), good default value is 0.5 from paper suggestion
      real(dp), intent(in) :: rho_a        ! Density of the ambient fluid (kg/m^3)
      real(dp), intent(in) :: mean_radius  ! Mean radius of the cloud in meters
      real(dp), intent(in) :: U(3), U_a(3)   ! Velocity of the cloud and ambient current in m/s
      real(dp)             :: F_d(3)       ! Result: Drag force acting on the cloud (N)
      ! Local variables
      real(dp) :: rel_vel_mag
      real(dp), dimension(3) :: shape_effect
    
      rel_vel_mag = norm(U - U_a, 2)

      ! Due to the objects shape need to calculate the drag force vectorially
      shape_effect = [0.5_dp, 1.0_dp, 0.5_dp]  ! Shape effect due to the oblate spheroid shape of the cloud

      F_d = 0.5_dp * rho_a * C_d * shape_effect * pi_dp * mean_radius**2 * rel_vel_mag * (U - U_a)
   end function calc_drag

   pure function calc_B(surf_rho_a, rho_c, mean_radius) result(B)
      !! Calculate the buoyancy of the cloud
      !! TODO: Look into this more. I'm not sure why this is different from force_buoyancy
      real(dp), intent(in) :: surf_rho_a   ! Density of the ambient fluid at the surface (kg/m^3)
      real(dp), intent(in) :: rho_c        ! Density of the cloud (kg/m^3)
      real(dp), intent(in) :: mean_radius  ! Mean radius of the cloud in meters
      real(dp)             :: B            ! Result: Buoyancy of the cloud (kg)

      B = (surf_rho_a - rho_c)  * calc_V_c(mean_radius)

   end function calc_B

   pure function calc_P_i(mean_radius, C_s) result(P_i)
        !! Calculate the volume of solids in the cloud
        real(dp), intent(in) :: mean_radius  ! Mean radius of the cloud in meters
        real(dp), intent(in) :: C_s          ! Concentration of solids in the cloud (kg/m^3),
        real(dp)             :: P_i          ! Result: Volume of solids in the cloud (m^3)
    
        P_i = calc_V_c(mean_radius) * C_s
     end function calc_P_i

     pure function calc_diss_param(mean_radius, surf_rho_a, g, C) result(A)
        !! Calculate the dissipation parameter of the cloud
        real(dp), intent(in) :: mean_radius  ! Mean radius of the cloud in meters
        real(dp), optional, intent(in) :: C            ! Empirical constant (dimensionless)
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

        A = C_local * (mean_radius)**2 * g / surf_rho_a
     end function calc_diss_param

end module instant_descent
