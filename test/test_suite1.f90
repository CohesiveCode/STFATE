module test_dump_collapse
   use stdlib_kinds, only: dp
   use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
   use mod_dump_collapse
   implicit none
   private

   public :: collect_dump_collapse

contains

!> Collect all exported unit tests
   subroutine collect_dump_collapse(testsuite)
      !> Collection of tests
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      print *, "Collapse Tests"
      testsuite = [ &
         new_unittest("Entrain.", test_entrain), &
         new_unitests("Alpha", test_alpha), &
         new_unitests("Epsilon", test_epsilon), &
         new_unitests("Ambient Density", test_ambient_density), &
         new_unitests("Cloud Density", test_cloud_density), &
         new_unitests("Radial Force", test_radial_force), &
         new_unitests("Collapse tip v_1", test_tip_v1), &
         new_unitests("Form Drag", test_form_drag)v, &
         new_unitests("Skin Friction Drag", test_skin_drag), &
         new_unitests("Inertia deriv. version", test_I_deriv), &
         new_unitests("Inertia", test_intertia), &
         new_unitests("Collapse tip v_2", test_tip_v2), &
         new_unitests("Drag Force", test_drag_force)&
         ]

   end subroutine collect_dump_collapse

   subroutine test_entrain(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: E
      real(dp), parameter :: E_truth =

      E = calc_dump_col_E(vert_radius = 1.0_dp,&
         horiz_radius = 1.0_dp,&
         alpha = 1.0_dp,&
         alpha_c = 1.0_dp,&
         U_c = 1.0_dp,&
         U_a = 1.0_dp,&
         db_dt = 1.0_dp,&
         )

      call check(error, E, E_truth)

   end subroutine test_entrain

   subroutine test_alpha(error)
      type(error_type), intent(inout) :: error
      real(dp) :: alpha
      real(dp), parameter :: alpha_truth

      alpha = calc_dump_col_alpha(vert_radius=,&
         horiz_radius=,&
         alpha_0 = ,&
         )

      call check(error, alpha, alpha_truth)

   end subroutine test_alpha

   subroutine test_epsilon(error)
      type(error_type), intent(inout) :: error
      real(dp) :: epsilon
      real(dp), parameter :: epsilon_truth

      epsilon = calc_dump_col_epsilon(&
         yi = ,&
         y = ,&
         rho_0 = ,&
         rho_a = &
         )

      call check(error, epsilon, epsilon_truth)
   end subroutine test_epsilon

   subroutine test_ambient_density(error)
      type(error_type), intent(inout) :: error
      real(dp) :: rho_ambient
      real(dp), parameter :: rho_ambient_truth

      ambient_density = calc_dump_col_rho_a(&
         rho_0 = ,&
         epilon = ,&
         vert_radius = ,&
         y_loc = &
         )

      call check(error, rho_ambient, rho_ambient_truth)

   end subroutine test_ambient_density

   subroutine test_cloud_density(error)
      type(error_type), intent(inout) :: error
      real(dp) :: rho_cloud
      real(dp), parameter :: rho_cloud_truth
      
      rho_cloud = calc_dump_col_rho_cloud(&
      rho_0 = ,&
      epsilon=,&
      vert_radius = ,&
      y_loc = ,&
      gamma= ,&
      final_des_radius= &
      )
   end subroutine test_cloud_density

   subroutine test_radial_force(error)
      type(error_type), intent(inout) :: error
      real(dp) :: radial_force
      real(dp), parameter :: radial_force_truth
      radial_force = calc_dump_col_F_radial(&
      rho_0 = 0.0_dp, &
      gamma = 0.0_dp,&
      epsilon = 0.0_dp,&
      final_des_radius = 0.0_dp,&
      vert_radius = 0.0_dp,&
      horiz_radius = 0.0_dp,&
      g = 0.0_dp,&
      dtheta = 0.0_dp&
      )
   end subroutine test_radial_force

   subroutine test_tip_v1(error)
      type(error_type), intent(inout) :: error
      real(dp) :: tip_v1
      real(dp), parameter :: tip_v1_truth

   end subroutine test_tip_v1

   subroutine test_skin_drag(error)
      type(error_type), intent(inout) :: error
      real(dp) :: skin_drag
      real(dp), parameter :: skin_drag_truth

   end subroutine test_skin_drag

   subroutine test_I_deriv(error)
      type(error_type), intent(inout) :: error
      real(dp) :: I_deriv
      real(dp), parameter :: I_deriv_truth

      call skip_test(error, "Test is skipped. Function is not implemented")

   end subroutine test_I_deriv

   subroutine test_intertia(error)
      type(error_type), intent(inout) :: error
      real(dp) :: intertia
      real(dp), parameter :: intertia_truth

   end subroutine test_intertia

   subroutine test_tip_v2(error)
      type(error_type), intent(inout) :: error
      real(dp) :: tip_v2
      real(dp), parameter :: tip_v2_truth

   end subroutine test_tip_v2

   subroutine test_drag_force(error)
      type(error_type), intent(inout) :: error
      real(dp) :: drag_force
      real(dp), parameter :: drag_force_truth

   end subroutine test_drag_force

end module test_suite1
