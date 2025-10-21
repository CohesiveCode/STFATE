module mod_dump_bottom
   !! Module contains procedures for calculating cloud properties when it's collapsing on the bottom.
   use stdlib_kinds, only: dp
   use mod_constants, only: pi
   use mod_dump_gen, only: calc_dump_F_drag

   !! Module for procedures related to the dynamic collapse of the cloud on the bottom
   implicit none(type, external)
   private
   public :: calc_dump_bot_E, calc_dump_bot_F_bf, calc_dump_bot_I, calc_dump_bot_v_2, calc_dump_bot_F_reac
contains


   pure function calc_dump_bot_E(vert_radius, horiz_radius, alpha_c, db_dt) result(E)
      !! Ref(1) - Eqn(3.54) - Calc the volume entrainment rate (E) when the cloud collapses on the bottom
      real(dp), intent(in) :: vert_radius    !! Vertical radius
      real(dp), intent(in) :: horiz_radius   !! Horizontal radius
      real(dp), intent(in) :: alpha_c        !! Entrainment coefficient due to collapse
      real(dp), intent(in) :: db_dt          !! [], Velocity at the top of the collapsing cloud
      real(dp)             :: E              !! Entrainment rate

      ! Local variables
      real(dp) :: R        !! Squared and square rooted difference between horizontal velocities 
      real(dp) :: paren_1  !! 1st parenthesis term

      R = sqrt(horiz_radius**2 - vert_radius**2)

      paren_1 = pi * horiz_radius**2 + 0.5_dp * pi * vert_radius**2 * horiz_radius / R * log( (horiz_radius + R) / (horiz_radius - R))

      E = paren_1 * alpha_c * db_dt
   end function calc_dump_bot_E

   pure function calc_dump_bot_F_bf(F_bed, F_frictn, F_1) result(F_bf)
      !! Ref(1) - Eqn. 3.60 - Calc the bottom friction force
      real(dp), intent(in) :: F_bed    !! Force from impacting the bed
      real(dp), intent(in) :: F_frictn !! Bottom cloud interface friction coeffieint
      real(dp), intent(in) :: F_1      !! Modification factor used in computing the resistance of the
      real(dp)             :: F_bf     !! Bottom friction force

      F_bf = F_bed * F_frictn * F_1 / (2.0_dp * pi)
   end function calc_dump_bot_F_bf

   pure function calc_dump_bot_I(F_radial, D_D, F_skin_frictn, F_bf) result(inertia)
      !! Ref(1) - Eqn(3.62) - Calcs the horizontal intertia using a force balance. Used with equation 3.39
      real(dp), intent(in) :: F_radial          !! Radial force
      real(dp), intent(in) :: D_D            !! Form drag
      real(dp), intent(in) :: F_skin_frictn  !! Skin friction drag, F_f in Ref(1)
      real(dp), intent(in) :: F_bf           !! Bottom friction force
      real(dp) :: inertia                    !! Intertia of the slice

      inertia = F_radial - D_D - F_skin_frictn - F_bf
   end function calc_dump_bot_I

   pure function calc_dump_bot_v_2(dMass_dt, rho_c, vert_radius, horiz_radius) result(v_2)
      !! Ref(1) - Eqn(3.64) - Calc tip velocity change due to entrainment
      real(dp), intent(in) :: dMass_dt      !! time derivative of the mass of the cloud
      real(dp), intent(in) :: rho_c         !! Bulk density of the cloud
      real(dp), intent(in) :: vert_radius   !! Vertical radius of the cloud
      real(dp), intent(in) :: horiz_radius  !! horizontal radius of the cloud
      real(dp)             :: v_2           !! Cloud tip velocity due to entrainment

      v_2 = dMass_dt / (rho_c * 4.0_dp / 3.0_dp * pi * vert_radius * horiz_radius)
   end function calc_dump_bot_v_2

   pure function calc_dump_bot_F_drag(rho_a, vert_radius, horiz_radius, U, U_a, C_d) result(F_drag)
      !! Ref(1) - Eqn. 3.67-3.69 - Calculate the drag force acting on the cloud
      real(dp), intent(in) :: rho_a          !! Density of the ambient fluid (kg/m^3)
      real(dp), intent(in) :: vert_radius    !! Vertical radius of the cloud
      real(dp), intent(in) :: horiz_radius   !! Horizontal radius of the cloud
      real(dp), intent(in) :: U(3), U_a(3)   !! Velocity of the cloud and ambient current in m/s
      real(dp), intent(in) :: C_d            !! Drag coefficient (dimensionless)
      real(dp)             :: F_drag(3)      !! Drag force acting on the cloud (N)

      ! Local variables
      real(dp) :: rel_vel_mag
      real(dp), dimension(3) :: shape_effect

      rel_vel_mag = norm2(U - U_a)

      ! Due to the objects shape need to calculate the drag force vectorially
      shape_effect = [0.5_dp, 0.0_dp, 0.5_dp]  ! Shape effect and am abusing this to set the y-term to zero. Should be zero anyway.

      F_drag = calc_dump_F_drag(rho_a, vert_radius, horiz_radius, U, U_a, C_d, shape_effect)
   end function calc_dump_bot_F_drag

   pure function calc_dump_bot_F_reac(F_bed, F_frictn, dM_dt, S_i, rho_i, U_c) result(F_reac)
        !! Ref1) - Eqn(3.70-3.73) - Calc the reaction force at the bed. This variable is called F_f in Ref(1). 
        !! See top of page 49 in Ref(1) for information
        real(dp), intent(in) :: F_bed    !! Pretty Sure this is the bed force. TODO: Check this
        real(dp), intent(in) :: F_frictn !! Bottom cloud interface friction coeffieint
        real(dp), intent(in) :: dM_dt(3) !! Cloud momentum derivative
        real(dp), intent(in) :: S_i(:)   !! Volume rate of solids setting out of cloud
        real(dp), intent(in) :: rho_i(:) !! Dry density of solids in cloud
        real(dp), intent(in) :: U_c(3)   !! Velocity of the cloud
        real(dp) :: F_reac(3)            !! Reaction force at the bottom
        
        ! Local variables
        real(dp) :: horiz_vel_mag

        horiz_vel_mag = sqrt( U_c(1)**2 + U_c(3)**2 )
        
        F_reac(1) = F_bed * F_frictn * U_c(1) / horiz_vel_mag

        F_reac(2) = -F_bed

        F_reac(3) = F_bed * F_frictn * U_c(3) / horiz_vel_mag

    end function calc_dump_bot_F_reac

   !  pure function calc_dump_bot_F_bed()



end module mod_dump_bottom
