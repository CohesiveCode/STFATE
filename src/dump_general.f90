module mod_dump_gen
   !! Module contains procedures that are common across all three phases of the dump descent
   !! Descent, Collapse in water, Collapse on bottom
   !! using _d_ to represent that the procedure is across the entire dump process
   use stdlib_kinds, only: dp
   use mod_constants, only: pi
   implicit none(type, external)
   private
   public :: calc_dump_momentum, calc_dump_S_i, calc_dump_B, calc_dump_P_i, calc_dump_F_b, calc_dump_F_drag

contains
   pure function calc_dump_momentum(C_m, rho_c, vol_c, U) result(M)
      !! Ref(1) - Eqn(3.7, 3.43, 3.65) - Calculate the momentum of the cloud
      real(dp), intent(in) :: C_m     !! Apparent mass coefficient (dimensionless), suggested between 0.5 and 1.5, , []
      real(dp), intent(in) :: rho_c   !! Density of the cloud, []
      real(dp), intent(in) :: vol_c   !! Volume of the cloud, []
      real(dp), intent(in) :: U(3)    !! Velocity of the cloud, []
      real(dp)             :: M(3)    !! Momentum of the cloud, []

      M = C_m * rho_c * vol_c * U
   end function calc_dump_momentum

   elemental pure function calc_dump_S_i(horiz_radius, v_fi, C_si, beta) result(S_i)
      !! Ref(1) - Eqn(3.6, 3.28, 3.55) - Calculate the volume rate of solids settling out of the cloud
      !! Using the horizontal radius (b) instead of vertical radius (a) as in eqaution 3.6 since in descent, where a is used
      !! vertical and horizontal radi are equal
      real(dp), intent(in) :: horiz_radius !! Horizontal radius of the cloud, []
      real(dp), intent(in) :: v_fi         !! Fall velocity of individual particles in the cloud, []
      real(dp), intent(in) :: C_si         !! Concentration of solids in the cloud, []
      real(dp), intent(in) :: beta         !!  Empirical constant (dimensionless), []
      real(dp)             :: S_i          !! Volume rate of solids settling out of the cloud, []

      S_i = pi * horiz_radius**2 * abs(v_fi) * C_si * (1.0_dp - beta)
   end function calc_dump_S_i

   elemental pure function calc_dump_B(vol_c, surf_rho_a, rho_c) result(B)
      !! Ref(1) - Eqn(3.12, 3.48, 3.74) - Calculate the buoyancy of the cloud
      !! TODO: Look into this more. I'm not sure why this is different from force_buoyancy
      real(dp), intent(in) :: vol_c        !! Volume of the cloud
      real(dp), intent(in) :: surf_rho_a   !! Density of the ambient fluid at the surface
      real(dp), intent(in) :: rho_c        !! Density of the cloud
      real(dp)             :: B            !! Result: Buoyancy of the cloud

      B = vol_c * (surf_rho_a - rho_c)
   end function calc_dump_B

   elemental pure function calc_dump_P_i(vol_c, C_si) result(P_i)
      !! Ref(1) - Eqn(3.13, 3.49, 3.75) - Calculate the volume of solids in the cloud
      real(dp), intent(in) :: vol_c        !! [], Volume of the cloud
      real(dp), intent(in) :: C_si         !! [], Concentration of solids in the cloud
      real(dp)             :: P_i          !! [], Volume of solids in the cloud

      P_i = vol_c * C_si
   end function calc_dump_P_i

   elemental pure function calc_dump_F_b(rho_a, rho_c, g, vol_c) result(F_b)
      !! Ref(1) - Eqn(3.8, 3.44, 3.66) - Calculate the buoyancy force acting on the cloud
      real(dp), intent(in) :: rho_a        !! [], Density of the ambient fluid
      real(dp), intent(in) :: rho_c        !! [], Density of the cloud
      real(dp), intent(in) :: g            !! [], Acceleration due to gravity
      real(dp), intent(in) :: vol_c        !! [], Volume of the cloud
      real(dp)             :: F_b          !! [], Buoyancy force acting on the cloud

      F_b = (rho_c - rho_a) * g * vol_c
   end function calc_dump_F_b

   pure function calc_dump_F_drag(rho_a, vert_radius, horiz_radius, U, U_a, C_d, shape_effect) result(F_drag)
      !! Ref(1) - Base drag function for Eqn(3.9-3.11, 3.45-3.47, 3.67-3.69)
      !! Wrapper functions are provided for descent, collapse, and bottom to account for shape and shape effects
      !! Calculate the drag force acting on the cloud

      real(dp), intent(in) :: rho_a           !! [], Density of the ambient fluid
      real(dp), intent(in) :: vert_radius     !! [], Vertical radius of the cloud
      real(dp), intent(in) :: horiz_radius    !! [], Horizontal radius of the cloud
      real(dp), intent(in) :: U(3), U_a(3)    !! [], Velocity of the cloud and ambient current
      real(dp), intent(in) :: C_d             !! [-], Drag coefficient (dimensionless)
      real(dp), intent(in) :: shape_effect(3) !! [], Array of shape effects for the individual velocity components
      real(dp)             :: F_drag(3)       !! [], Result: Drag force acting on the cloud

      ! Local variables
      real(dp) :: rel_vel_mag

      rel_vel_mag = norm2(U - U_a)

      F_drag = 0.5_dp * rho_a * C_d * shape_effect * pi * vert_radius * horiz_radius * rel_vel_mag * (U - U_a)
   end function calc_dump_F_drag

end module mod_dump_gen
