
!!                       z , w
!!                     ╱
!!                   ╱
!!                 ╱
!! ───────────────●──────────────────→ x , u
!!                │
!!        g       │
!!        |       │
!!        |       │
!!        ↓       │
!!                │_   ┌ b(t) ┐
!!                │|   \           /  |
!!           a(t) │|    \ (cloud) /   | U(t)
!!                │|     \_______/    ↓
!!                │-
!!                ↓ y , v

!> Paper that this code is based on:
!> 1) Development of Models for Prediction of Short-term Fate of dredged Material Discharged in the Estuarine Environment
!>   Model Coordinate system
! @todo
!> - Add in the interpolation of ambient density and velocity fields using fitpack
!> - Add in the time-stepping loop to update the cloud properties over time
!> - Add in the calculation of the cloud properties using the instant_descent module
!> - Add in the output of the cloud properties over time to a file for later analysis
!> - Allow bathymetry, max vertical y-coordinate, to vary across the domain. Need to think about how this would effect the differencing methods
!> - Have multiple clouds in the domain at once
!> - Have interaction between the clouds and the bottom of the channel
!> - Check that the clouds don't travel outside the water surface
!> - Add coordinate system for barge and convert between barge and ambient field coordinates
!> Steps to make sure the program works:
!> 1) Allow the cloud to settle like a particle with zero ambient field
!> 2) Check the program when the cloud reaches the bottom
!> 3) Move
!> Things that need to happen to init a cloud
!> 6. Define the initial concentration of solids in the cloud
!> 7. Define the initial fall velocity of individual particles in the cloud
!> 8. Define the initial entrainment coefficient
!> 9. Define the initial apparent mass coefficient
!> 10. Define the initial empirical constant for solids passing through the cloud boundary
!> 11. Define the initial drag coefficient

program STFATE

   ! At this point the purpose of this program is to calculate the transport of a single cloud within a 3D ambient flow field
   ! use stdlib_math,    only: meshgrid, linspace
   use stdlib_kinds,   only: dp, int32
   ! use mod_instant_descent, only: calc_B, calc_A_param, calc_drag, calc_E, calc_F_b, calc_M, calc_P_i,&
   !                            calc_S_i, calc_vol_cloud, calc_alpha, calc_vorticity
   ! use mod_cloud, only: calc_y_centroid, check_cloud_in_domain
   ! use mod_warn, only: warn_bounds
   ! use mod_search, only: binary_search
   ! use mod_scalar_interp, only: interp1d
   ! use mod_vec_interp, only: interp3d_vec_field
   use mod_bounds, only: t_bounds
   use mod_cloud, only: t_dump_des_cloud
   use mod_model_coeffs, only: t_model_coeffs
   use mod_constants, only: g
!    use mod_instant_descent, only:calc_hemisphere_vol, calc_E, calc_S_i, calc_momentum, calc_F_b, calc_drag, &
!       calc_B, calc_P_i, calc_A_param, calc_vorticity, calc_alpha, calc_beta

   implicit none(type, external)

   integer(int32), parameter :: nx = 10, ny = 10, nz = 10
   real(dp) :: x(nx), y(ny), z(nz)
   real(dp), parameter :: x_start = 0.0_dp, x_end = 10.0_dp
   real(dp), parameter :: y_start = 0.0_dp, y_end = 10.0_dp
   real(dp), parameter :: z_start = 0.0_dp, z_end = 10.0_dp
   real(dp), parameter :: dx=(x_end - x_start) / real(nx - 1, dp)
   real(dp), parameter :: dy=(y_end - y_start) / real(ny - 1, dp)
   real(dp), parameter :: dz=(z_end - z_start) / real(nz - 1, dp)
   real(dp) :: xm(nx, ny, nz), ym(nx, ny, nz), zm(nx, ny, nz)

   integer , parameter:: num_partls = 2
   real(dp) :: init_partl_concen(num_partls)
   real(dp) :: init_partl_rho(num_partls)

   real(dp) :: init_tracer_concen = 0.01 ! Random number for the time being
   real(dp) :: bg_tracer_concen = 0.0    ! background tracer concentration

   type(t_bounds) :: bounds
   type(t_dump_des_cloud) :: cloud, clouds(2)
   type(t_model_coeffs) :: coeffs
   integer :: i

   ! Init the size of the domain
   call bounds%init([x_start, x_end], [y_start, y_end], [z_start, z_end])
   ! call bounds%print()

   init_partl_concen = [0.1, 0.2] ! Imaginary particle

   call cloud%init(id=1, rho=100.0_dp, mass_coeff=0.3_dp, vert_radius=1.0_dp, horiz_radius=1.0_dp, &
      top_surf_pos=[5.0_dp, 0.0_dp, 5.0_dp], init_vel=[0.0_dp, 1.0_dp, 0.0_dp],   &
      init_partl_concen = init_partl_concen, init_partl_rho=init_partl_rho,      &
      init_tracer_concen = init_tracer_concen, domain_bounds=bounds)

   call cloud%print()

   call cloud%partl%print()

   call coeffs%set_tetra_tech()
   call coeffs%print()
   print *, coeffs%akyo

   ! What are the next st




end program STFATE


