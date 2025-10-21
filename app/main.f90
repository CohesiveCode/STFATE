
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







   ! ! Variables for the ambient flow field
   ! real(dp) :: U_a(nx, ny, nz), V_a(nx, ny, nz), W_a(nx, ny, nz)
   ! real(dp) :: rho_a(ny)! Ambient density field (Only varies in y direction for now)

   ! ! init cloud properties
   ! real(dp) :: cloud_pos(3)    ! Initial position of the cloud (x, y, z) in meters
   ! real(dp) :: a=1.0_dp        ! Initial vertical radius of the cloud in meters
   ! real(dp) :: b=1.0_dp        ! Initial lateral radius of the cloud in meters
   ! real(dp) :: rho_c=2000.0_dp ! Initial mean density of the cloud
   ! real(dp) :: U_c(3)          ! Velocity of the cloud in m/s
   ! real(dp) :: Vol_c           ! Initial volume of the cloud in m^3
   ! real(dp) :: rho_ac          ! Ambient density at the location of the cloud
   ! real(dp) :: U_ac(3)         ! Ambient velocity at the location of the cloud
   ! real(dp) :: alpha_c         ! Entrainment coefficient
   ! real(dp) :: vorticity       ! Vorticity of the ambient flow field

   ! ! Init particles in cloud
   ! integer(int32), parameter :: num_particles = 0 ! Number of different particle types in the cloud
   ! real(dp) :: rho_p(num_particles)               ! Density of particles in cloud
   ! real(dp) :: V_p(num_particles)                 ! Init? Fall velocities of particles in cloud
   ! real(dp) :: C_p(num_particles)                 ! Concentrations of particles in cloud
   ! real(dp) :: P_p(num_particles)                 ! Solid volume of the ith component in cloud

   ! ! Everything else
   ! real(dp) :: M_c(3) ! Initial momentum of the cloud in kg·m/s
   ! real(dp) :: E_c    ! Entrainment rate in m^3/s
   ! real(dp) :: F_b    ! Buoyancy force
   ! real(dp) :: m      ! Mass of the cloud
   ! real(dp) :: D(3)   ! Drag force on the cloud
   ! real(dp) :: B_f    ! Buoyancy force

   ! ! Loop indices
   ! integer :: i, j, k

   ! ! Generate the grid coordinates
   ! x = linspace(x_start, x_end, nx)
   ! y = linspace(y_start, y_end, ny)
   ! z = linspace(z_start, z_end, nz)

   ! call meshgrid(x, y, z, xm, ym, zm)

   ! ! Init the initial ambient velocity field (For now make the field zero)
   ! U_a = 0.0_dp
   ! V_a = 0.0_dp
   ! W_a = 0.0_dp

   ! ! Init the ambient density field
   ! rho_a = 1025.0_dp

   ! ! Init cloud properties
   ! ! Calc the initial properties of the cloud
   ! ! The cloud position is the centroid of the hemisphere
   ! ! Init the cloud in the center of x, z and at the surface in y (plus the distance to the centroid)
   ! cloud_pos = [(x(nx) - x(1))/2.0_dp, y(1)+calc_y_centroid(a), (z(nx) - z(1))/2.0_dp]

   ! ! Interpolate ambient density and velocity fields at the location of the cloud
   ! ! TODO: Make interpolation functions
   ! ! For the time being just take the ambient density at the top of the domain
   ! !TODO: Might have to find the bounding elements for the interpolation outside of the interpolation function
   ! ! These interpolation functions assume that the grid is a box. To have more complex bathymetry need to do the search differently
   ! ! Work on this in the future when the code is more established.

   ! rho_ac = interp1d(cloud_pos(2), y, rho_a)
   ! U_ac   = interp3d_vec_field(cloud_pos, x, y, z, U_a, V_a, W_a)
   ! vorticity = calc_vorticity(U_c(2), b)
   ! alpha_c = calc_alpha(F_b, g, vorticity)
   ! E_c = calc_E(a, 0.1_dp, U_c, U_ac) ! Rate of entrainment in m/s
   ! M_c = calc_M(Cm, rho_c, a, U_c)    ! TODO: Need to make sure that U_c is in correct coordinate system
   ! Vol_c = calc_cloud_vol(a)                ! Volume of the cloud assuming a hemisphere

   ! ! Fb = calc_F_b(rho_a(ny/2), rho_c, g, a)
   ! ! P_p = 0.0_dp ! Initial solid volume of particles in cloud will be zero for the time being

   ! ! Do initial checks
   ! call check_cloud_in_domain(cloud_pos, a, b, x, y, z)
   ! call warn_bounds(Cm, 1.0_dp, 1.5_dp) ! Recommendation for Cm is between 1 and 1.5 - page 1-32

   ! ! Print initial conditions
   ! print *, "Cloud initial position (x,y,z): ", cloud_pos
   ! print *, "Cloud initial radius (a,b)    : ", a, b
   ! print *, "Cloud initial density (rho_c) : ", rho_c
   ! print *, "Ambient Density at Cloud (rho_ac): ", rho_ac
   ! print *, "Ambient Velocity at Cloud (U_ac)  : ", U_ac
   ! print *, "Cloud initial velocity (U_c)  : ", U_c
   ! print *, "Cloud initial volume (Vol_c)  : ", Vol_c

   ! Might need to interpolate the ambient density and velocity fields to the location of the cloud

end program STFATE


