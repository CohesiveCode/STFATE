module mod_cloud
   use stdlib_kinds, only: dp, int32
   use mod_instant_descent, only: calc_B, calc_A_param, calc_drag, calc_E, calc_F_b, calc_momentum, calc_P_i,&
      calc_S_i, calc_cloud_vol, calc_alpha, calc_vorticity
   use mod_cloud_helper, only: calc_clouds_bounds
   use mod_bounds, only: t_bounds

   implicit none
   private

   type, public :: d_cloud

      real(dp)        :: top_surf_pos(3)   ! Global posiiton of the top surface of the cloud
      real(dp)        :: pos(3)            ! Centroid position of the cloud (x, y, z)
      real(dp)        :: y_centroid        ! y-distance from the top_surf_pos to the y-centroid of the cloud
      real(dp)        :: vert_radius       ! Vertical radius
      real(dp)        :: horiz_radius      ! horizontal radius

      real(dp)        :: mass              ! Total mass of the cloud
      real(dp)        :: rho               ! Density of the cloud
      real(dp)        :: vol               ! Volume of the cloud
      real(dp)        :: vorticity         ! Vorticity of the cloud
      real(dp)        :: vel(3)            ! Velocity of the cloud (u, v, w)
      real(dp)        :: momentum(3)       ! Momementum of the cloud

      real(dp), allocatable :: vol_class(:) ! Volume of each class
      
      class(t_bounds) :: bounds            ! Rectangular box bound around the cloud in global coordinates
   
   contains
      procedure, public :: init => init_dump_cloud
      procedure, public :: set_vol
      procedure, public :: set_mass
      procedure, public :: set_momentum
      procedure, public :: set_vorticity
      procedure, public :: set_y_centroid
      procedure, public :: set_bounds => set_cloud_bounds
      
      procedure, private :: set_cloud_bounds

   end type d_cloud
   
   public :: d_cloud


contains

   pure subroutine set_vol(self)
      !! Set the colume of the cloud given the vertical radius
      class(d_cloud), intent(inout) :: self

      self%vol = calc_cloud_vol(self%vert_radius)
   end subroutine set_vol

   pure subroutine set_mass(self)
      !! Set the mass of the cloud
      class(d_cloud), intent(inout) :: self

      self%mass = self%rho * self%vol
   end subroutine set_mass

   subroutine set_momentum(self)
      !! Set the momentum of the cloud
      class(d_cloud), intent(inout) :: self

      self%momentum = calc_momentum(self%mass_coeff, self%rho, self%vert_radius, self%vel)
   end subroutine set_momentum

   subroutine set_vorticity(self)
      !! Set the vorticity
      class(d_cloud), intent(inout) :: self

      ! self%vel(2) is the velocity in the y-direction
      self%vorticity = calc_vorticity(self%vel(2), self%horiz_radius)
   end subroutine set_vorticity

   pure subroutine set_y_centroid(self)
      !! Set the y_centroid of the cloud
      class(d_cloud), intent(inout) :: self

      self%y_centroid = calc_y_centroid(self%vert_radius)
   end subroutine set_y_centroid

   pure subroutine set_cloud_bounds(self, pos)
      !! Sets the bounds for the cloud
      class(d_cloud), intent(inout) :: self
      real(dp), intent(in) :: pos(3)

      self%bounds = calc_clouds_bounds(pos, self%y_centroid, self%vert_radius, self%horiz_radius)
   end subroutine set_cloud_bounds

   pure subroutine set_pos(self, pos, domain_bounds)
      class(d_cloud), intent(inout) :: self
      class(t_bounds), intent(inout) :: domain_bounds
      real(dp), intent(in) :: pos
      !! Subroutine checks that the updated position is in the domain,
      ! Set the position of the cloud, but check that it's in the domain first

      call check_cloud_in_domain(self%bounds, domain_bounds, stat)

      if (stat .eq. 1) then
         ! print *, cloud_pos(1) - b, cloud_pos(1) + b, x(1), x(nx)
         print *, "Error: Cloud does not fit within the x domain"
         error stop
      else if (stat .eq. 2) then
         print *, "Error: Cloud does not fit within the y domain"
         error stop
      else if (stat .eq. 3) then
         print *, "Error: Cloud does not fit within the z domain"
         error stop
      end if

      ! Update the position
      self%pos = pos

   end subroutine set_pos

   pure subroutine init_dump_cloud(self, rho, mass_coeff, vert_radius, horiz_radius, &
      top_surf_pos, vel, domain_bounds)
      ! Initialize a cloud with given properties

      class(d_cloud)       :: self            ! Resulting cloud object
      real(dp), intent(in) :: rho             ! Density of the cloud
      real(dp), intent(in) :: mass_coeff      ! Apparent mass coeff
      real(dp), intent(in) :: vert_radius     ! Vertical radius of the cloud
      real(dp), intent(in) :: horiz_radius    ! Horizontal radius of the cloud
      real(dp), intent(in) :: top_surf_pos(3) ! Initial position of the center of the top surface of the cloud (x, y, z)
      real(dp), intent(in) :: vel(3)          ! Initial velocity of the cloud (u, v, w)
      type(t_bounds)       :: domain_bounds

      ! Local variables
      real(dp) :: pos(3)
      real(dp) :: y_centroid

      self%rho = rho
      self%vert_radius = vert_radius
      self%horiz_radius = horiz_radius

      self%vel = vel
      self%mass_coeff = mass_coeff

      call self%set_vol()
      call self%set_mass()
      call self%set_momentum()
      call self%set_vorticity()

      ! Store pos in a dummy variable for the time being
      pos = top_surf_pos
      pos(2) = pos(2) + y_centroid

      y_centroid = calc_y_centroid(self%vert_radius)

      call self%set_pos(top_surf_pos, pos)

      ! Now that the position is updated, update the other variables
      self%y_centroid = y_centroid

   end subroutine init_dump_cloud

end module mod_cloud
