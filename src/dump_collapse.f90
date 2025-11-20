module mod_dump_collapse
   !! Module contains procedures for the collapse portion of the calculation
   use stdlib_kinds, only: dp
   use mod_constants, only: pi
   use mod_dump_gen_deriv, only: calc_dump_drho_dy
   use mod_dump_gen, only: calc_dump_F_drag
   implicit none(type, external)
   private
   public :: calc_dump_col_E, calc_dump_col_alpha, calc_dump_col_epsilon, calc_dump_col_rho_a, &
      calc_dump_col_rho_cloud, calc_dump_col_F_radial, calc_dump_col_v_1, calc_dump_col_D_D,   &
      calc_dump_col_F_f, calc_dump_col_I_deriv_version, calc_dump_col_I, calc_dump_col_db_dt,  &
      calc_dump_col_v_2, calc_dump_col_F_drag
      
contains

   pure function calc_dump_col_E(vert_radius, horiz_radius, alpha, alpha_c, &
      U_c, U_a, db_dt) result(E)
      !! Ref(1) - Eqn(3.26) - Calc the volume entrainment rate (E)
      real(dp), intent(in) :: vert_radius    ! Vertical radius
      real(dp), intent(in) :: horiz_radius   ! Horizontal radius
      real(dp), intent(in) :: alpha          ! Entrainment coefficient
      real(dp), intent(in) :: alpha_c        ! Entrainment coefficient due to collapse
      real(dp), intent(in) :: U_c(3)         ! [], Velocity of the cloud
      real(dp), intent(in) :: U_a(3)         ! [], Ambient velocity
      real(dp), intent(in) :: db_dt          ! [], Velocity at the top of the collapsing cloud
      real(dp)             :: E

      ! Local variables
      real(dp) :: R, paren_1, paren_2, rel_vel_mag

      rel_vel_mag = norm2(U_c - U_a)

      R = sqrt(horiz_radius**2 - vert_radius**2)

      paren_1 = 2.0_dp * pi * horiz_radius**2 + pi * vert_radius**2 * horiz_radius / R * log( (horiz_radius + R) / (horiz_radius - R))

      paren_2 = alpha * rel_vel_mag + alpha_c * db_dt

      E = paren_1 * paren_2
   end function calc_dump_col_E

   pure function calc_dump_col_alpha(vert_radius, horiz_radius, alpha_0) result(alpha)
      !! Ref(1) - Eqn(3.27) - Calc the entrainment coefficient for the collapse phase
      real(dp), intent(in) :: vert_radius     ! Vertical radius
      real(dp), intent(in) :: horiz_radius    ! Horizontal radius
      real(dp), intent(in) :: alpha_0         ! Entrainment coefficient for a turbulent thermal
      real(dp) :: alpha

      alpha = (vert_radius / horiz_radius)**2 * alpha_0
   end function calc_dump_col_alpha

   pure function calc_dump_col_epsilon(yi, y, rho_0, rho_a) result(epsilon)
      !! Ref(1) - Eqn(3.29) - Normalized ambient density gradient
      real(dp), intent(in) :: yi
      real(dp), intent(in) :: y(:)
      real(dp), intent(in) :: rho_0
      real(dp), intent(in) :: rho_a(:)
      real(dp)             :: epsilon

      epsilon = 1.0_dp / rho_0 * calc_dump_drho_dy(yi, y, rho_a)
   end function calc_dump_col_epsilon

   elemental pure function calc_dump_col_rho_a(rho_0, epsilon, vert_radius, y_loc) result(rho_a)
      !! Ref(1) - Eqn(3.30) - Ambient density in the region of the collapsed cloud
      real(dp), intent(in) :: rho_0         !! Density at the clouds neutrally buoyant location
      real(dp), intent(in) :: epsilon       !! Normalized ambient density gradient
      real(dp), intent(in) :: vert_radius   !! Vertical radius of the collapsed cloud
      real(dp), intent(in) :: y_loc         !! Local y-dist from the top of the cloud, See Ref(1) - Fig. 3.5b
      real(dp)             :: rho_a         !! Ambient density around the cloud
      rho_a  = rho_0 * (1 - epsilon * (vert_radius - y_loc))
   end function calc_dump_col_rho_a

   elemental pure function calc_dump_col_rho_cloud(rho_0, epsilon, vert_radius, y_loc, gamma, final_des_radius) result(rho_cloud)
      !! Ref(1) - Eqn(3.31) - Calc the density inside of the collapsed cloud
      real(dp), intent(in) :: rho_0            !! Density at the clouds neutrally buoyant location
      real(dp), intent(in) :: epsilon          !! Normalized ambient density gradient
      real(dp), intent(in) :: gamma            !! Density scaling coefficient
      real(dp), intent(in) :: final_des_radius !! Final radius of the convective descent cloud
      real(dp), intent(in) :: vert_radius      !! Vertical radius of the collapsed cloud
      real(dp), intent(in) :: y_loc            !! Local y-dist from the top of the cloud, See Ref(1) - Fig. 3.5b
      real(dp) :: rho_cloud

      ! Local variables
      real(dp) :: a_0

      a_0 = final_des_radius / 2.0_dp  ! See Ref(1), Figure 3.4a and second paragraph on Ref(1) - pg(40)

      rho_cloud = rho_0 * (1 - gamma * a_0 / vert_radius * epsilon * (vert_radius - y_loc))
   end function calc_dump_col_rho_cloud

   elemental pure function calc_dump_col_F_radial(rho_0, gamma, epsilon, final_des_radius, vert_radius, horiz_radius, g, dtheta) result(F_radial)
      !! Ref(1) - Eqn(3.34, 3.56) - Calc the radial force driving the collapse
      real(dp), intent(in) :: rho_0
      real(dp), intent(in) :: gamma
      real(dp), intent(in) :: epsilon
      real(dp), intent(in) :: final_des_radius
      real(dp), intent(in) :: vert_radius
      real(dp), intent(in) :: horiz_radius
      real(dp), intent(in) :: g
      real(dp), intent(in) :: dtheta
      real(dp)              :: F_radial

      ! Local variables
      real(dp) :: a_0, paren_1

      a_0 = final_des_radius / 2.0_dp  ! See Ref(1), Figure 3.4a and second paragraph on Ref(1) - pg(40)

      paren_1 = 1.0_dp - gamma * a_0 / vert_radius

      F_radial = pi * rho_0 / 16.0_dp  * paren_1 * epsilon * g * vert_radius**3 * horiz_radius * dtheta
   end function calc_dump_col_F_radial

   elemental pure function calc_dump_col_v_1(dc_dt) result(v_1)
      !! Ref(1) - Eqn(3.36, 3.57) - Calc the cloud tip velocity
      real(dp), intent(in) :: dc_dt
      real(dp) :: v_1

      v_1 = 16.0_dp / (3.0_dp * pi) * dc_dt
   end function calc_dump_col_v_1

   pure function calc_dump_col_D_D(c_drag, rho_a, vert_radius, horiz_radius, v1, dtheta) result(D_D)
      !! Ref(1) - Eqn(3.37, 3.58) - Calc the form drag of the slice
      real(dp), intent(in) :: c_drag
      real(dp), intent(in) :: rho_a
      real(dp), intent(in) :: vert_radius
      real(dp), intent(in) :: horiz_radius
      real(dp), intent(in) :: v1
      real(dp), intent(in) :: dtheta
      real(dp) :: D_D

      D_D = c_drag * rho_a * vert_radius * horiz_radius / 4.0_dp * abs(v1) * v1 * dtheta
   end function calc_dump_col_D_D

   pure function calc_dump_col_F_f(c_fric, rho_a, vert_radius, horiz_radius, v_1, dtheta) result(F_f)
      !! Ref(1) - Eqn(3.38) - Calc the skin friction drag on slice
      real(dp), intent(in) :: c_fric
      real(dp), intent(in) :: rho_a
      real(dp), intent(in) :: vert_radius
      real(dp), intent(in) :: horiz_radius
      real(dp), intent(in) :: v_1
      real(dp), intent(in) :: dtheta
      real(dp) :: F_f

      F_f = c_fric * rho_a * horiz_radius**2 / (2.0_dp * vert_radius) * v_1 * dtheta
   end function calc_dump_col_F_f

   pure function calc_dump_col_I_deriv_version(rho_c, vert_radius, horiz_radius, v_1, dtheta) result(I)
      !! Ref(1) - Eqn(3.39, 3.61) - Calc horizontal intertia of the slice
      real(dp), intent(in) :: rho_c
      real(dp), intent(in) :: vert_radius
      real(dp), intent(in) :: horiz_radius
      real(dp), intent(in) :: v_1          ! Velocity of the slice centroid
      real(dp), intent(in) :: dtheta
      real(dp) :: I

      print *, "Function isn't implemented yet. Not sure how to integrate this."
      print *, "calc_dump_col_I_deriv_version"
   end function calc_dump_col_I_deriv_version

   pure function calc_dump_col_I(F_radial, D_D, F_f) result(I)
      !! Ref(1) - Eqn(3.40) - Cals the horizontal intertia using a force balance. Used with equation 3.39
      real(dp), intent(in) :: F_radial   ! Radial force
      real(dp), intent(in) :: D_D     ! Form drag
      real(dp), intent(in) :: F_f     ! Skin friction drag
      real(dp) :: I

      I = F_radial - D_D - F_f
   end function calc_dump_col_I

   pure function calc_dump_col_db_dt(v_1, v_2) result(db_dt)
      !! Ref(1) - Eqn(3.41) - Calc the velocity of the slice tip
      real(dp), intent(in) :: v_1
      real(dp), intent(in) :: v_2
      real(dp) :: db_dt

      db_dt = v_1 + v_2
   end function calc_dump_col_db_dt

   pure function calc_dump_col_v_2(dMass_dt, rho_c, vert_radius, horiz_radius) result(v_2)
      !! Ref(1) - Eqn(3.42) - Calc the e
      real(dp), intent(in) :: dMass_dt
      real(dp), intent(in) :: rho_c
      real(dp), intent(in) :: vert_radius
      real(dp), intent(in) :: horiz_radius
      real(dp) :: v_2

      v_2 = dMass_dt / (rho_c * 8.0_dp / 3.0_dp * pi * vert_radius * horiz_radius)
   end function calc_dump_col_v_2

   pure function calc_dump_col_F_drag(rho_a, vert_radius, horiz_radius, U, U_a, C_d) result(F_drag)
      !! Eqn. 3.45-3.47 - Calculate the drag force acting on the cloud
      real(dp), intent(in) :: rho_a          ! Density of the ambient fluid (kg/m^3)
      real(dp), intent(in) :: vert_radius    ! Vertical radius of the cloud
      real(dp), intent(in) :: horiz_radius   ! Horizontal radius of the cloud
      real(dp), intent(in) :: U(3), U_a(3)   ! Velocity of the cloud and ambient current in m/s
      real(dp)             :: F_drag(3)      ! Result: Drag force acting on the cloud (N)
      real(dp), intent(in) :: C_d            ! Drag coefficient (dimensionless)

      ! Local variables
      real(dp) :: rel_vel_mag
      real(dp), dimension(3) :: shape_effect

      rel_vel_mag = norm2(U - U_a)


      ! Due to the objects shape need to calculate the drag force vectorially
      shape_effect = [1.0_dp, 1.0_dp, 1.0_dp]  ! Shape effect due to the oblate spheroid shape of the cloud

      F_drag = calc_dump_F_drag(rho_a, vert_radius, horiz_radius, U, U_a, C_d, shape_effect)
   end function calc_dump_col_F_drag

end module mod_dump_collapse
