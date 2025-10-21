module mod_cloud
   use stdlib_kinds, only: dp, int32
   use stdlib_optval, only: optval
   use mod_dump_descent,  only: calc_dump_des_vorticity
   use mod_dump_gen   ,   only: calc_dump_momentum
   use mod_geometry   ,   only: calc_y_centroid_hemisphere, calc_y_centroid_spheriod, &
      calc_half_ellipsoid_vol, calc_oblate_spheriod_vol
   use mod_cloud_helper,  only: calc_clouds_bounds, check_cloud_in_domain
   use mod_bounds, only: t_bounds
   use mod_particles, only: t_particles


   implicit none(type, external)
   private

   !> Private parameters to the module for checking what phase the cloud is in
   integer, private, parameter :: descentPhase        = 1 !! Phase val during cloud descent phase
   integer, private, parameter :: waterCollapsePhase  = 2 !! Phase val during the water collapse phase
   integer, private, parameter :: bottomCollapsePhase = 3 !! Phase val during the bottom collapse phase

   type, public :: t_dump_des_cloud
      !! Cloud to represent clouds
      integer         :: id                !! id of the cloud
      real(dp)        :: top_surf_pos(3)   !! Global posiiton of the top surface of the cloud
      real(dp)        :: pos(3)            !! Centroid position of the cloud (x, y, z)
      real(dp)        :: y_centroid        !! y-distance from the top_surf_pos to the y-centroid of the cloud
      real(dp)        :: vert_radius       !! Vertical radius
      real(dp)        :: horiz_radius      !! horizontal radius

      real(dp)        :: mass              !! Total mass of the cloud
      real(dp)        :: mass_coeff        !! Apparent mass coefficient
      real(dp)        :: rho               !! Density of the cloud
      real(dp)        :: vol               !! Volume of the cloud
      real(dp)        :: vorticity         !! Vorticity of the cloud
      real(dp)        :: vel(3)            !! Velocity of the cloud (u, v, w)
      real(dp)        :: momentum(3)       !! Momementum of the cloud

      real(dp)          :: init_tracer_concen !! Init tracer concentart
      type(t_particles) :: partl              !! Particles properties of the cloud
      type(t_bounds)    :: bounds             !! Rectangular box bound around the cloud in global coordinates
      integer           :: phase              !! Phase that the cloud is in: Descent, Water Collapse, Bottom Collapse

   contains
      procedure, public :: set_vol                 !! Set the cloud volume
      procedure, public :: set_mass                !! Set the cloud mass
      procedure, public :: set_momentum            !! Set the cloud momentum
      procedure, public :: set_vorticity           !! Set the cloud's vorticity
      procedure, public :: set_y_centroid          !! Set the y-centroid of the cloud
      procedure, public :: set_bounds              !! Set the rectangular bounds around the cloud
      procedure, public :: check_bounds            !! Check that the cloud bounds are inside the domain bounds
      procedure, public :: init => init_dump_cloud !! Set do housekeeping for the cloud's initial values
      procedure, public :: print => print_cloud    !! Print the metadata about the cloud (Useful for debugging)
   end type t_dump_des_cloud

contains

   pure subroutine set_vol(self)
      !! For the current phase set the volume of the cloud
      class(t_dump_des_cloud), intent(inout) :: self

      select case (self%phase)

       case(descentPhase)
         self%vol = calc_half_ellipsoid_vol(self%vert_radius, self%horiz_radius)

       case(waterCollapsePhase)
         self%vol = calc_oblate_spheriod_vol(self%vert_radius, self%horiz_radius)

       case(bottomCollapsePhase)
         self%vol = calc_half_ellipsoid_vol(self%vert_radius, self%horiz_radius)

       case default
         error stop "The elected phase isn't allowed. Selected is: "! self%phase

      end select

   end subroutine set_vol

   pure subroutine set_mass(self)
      !! Set the mass of the cloud
      class(t_dump_des_cloud), intent(inout) :: self

      self%mass = self%rho * self%vol
   end subroutine set_mass

   pure subroutine set_momentum(self)
      !! Set the momentum of the cloud
      class(t_dump_des_cloud), intent(inout) :: self

      self%momentum = calc_dump_momentum(self%mass_coeff, self%rho, self%vol, self%vel)
   end subroutine set_momentum

   pure subroutine set_vorticity(self)
      !! Set the vorticity
      class(t_dump_des_cloud), intent(inout) :: self

      select case(self%phase)

       case(descentPhase)
         ! self%vel(2) is the velocity in the y-direction
         self%vorticity = calc_dump_des_vorticity(self%vel(2), self%horiz_radius)
      
       case default
         error stop "Voriticity should only be calculated during descent phase"
      end select
   end subroutine set_vorticity

   pure subroutine set_y_centroid(self)
      !! Set the y_centroid of the cloud
      class(t_dump_des_cloud), intent(inout) :: self

      select case(self%phase)

       case(descentPhase)
         self%y_centroid = calc_y_centroid_hemisphere(self%vert_radius)

       case(waterCollapsePhase)
         self%y_centroid = calc_y_centroid_spheriod(self%vert_radius)

       case(bottomCollapsePhase)
         self%y_centroid = calc_y_centroid_hemisphere(self%vert_radius)

       case default
         error stop "Phase is incorrect"

      end select
   end subroutine set_y_centroid

   pure subroutine set_bounds(self, pos)
      !! Sets the bounds for the cloud
      !! TODO: Generalize for different phases
      class(t_dump_des_cloud), intent(inout) :: self
      real(dp), intent(in) :: pos(3)

      self%bounds = calc_clouds_bounds(pos, self%y_centroid, self%vert_radius, self%horiz_radius)
   end subroutine set_bounds

   subroutine check_bounds(self, domain_bounds)
      !! Subroutine checks that the updated position is in the domain,
      class(t_dump_des_cloud), intent(inout) :: self
      class(t_bounds), intent(in) :: domain_bounds

      ! Local variables
      integer :: stat

      call check_cloud_in_domain(self%bounds, domain_bounds, stat)

      if (stat .eq. 1) then
         call self%bounds%print("Cloud")
         call domain_bounds%print("Domain")
         error stop "Error: Cloud does not fit within the x domain"

      else if (stat .eq. 2) then
         call self%bounds%print("Cloud")
         call domain_bounds%print("Domain")
         error stop "Error: Cloud does not fit within the y domain"

      else if (stat .eq. 3) then
         call self%bounds%print("Cloud")
         call domain_bounds%print("Domain")
         error stop "Error: Cloud does not fit within the z domain"
      end if

   end subroutine check_bounds

   subroutine init_dump_cloud(self, id, rho, mass_coeff, vert_radius, horiz_radius, &
      top_surf_pos, init_vel, init_partl_concen, init_partl_rho, init_tracer_concen, &
      domain_bounds, ext_phase)
      !! Initialize a cloud with given properties

      ! TODO: Need to go through this and update it so that the location is properly updated.
      class(t_dump_des_cloud), intent(inout) :: self         !! Resulting cloud object
      integer , intent(in)          :: id                    !! id of the cloud
      real(dp), intent(in)          :: rho                   !! Density of the cloud
      real(dp), intent(in)          :: mass_coeff            !! Apparent mass coeff
      real(dp), intent(in)          :: vert_radius           !! Vertical radius of the cloud
      real(dp), intent(in)          :: horiz_radius          !! Horizontal radius of the cloud
      real(dp), intent(in)          :: top_surf_pos(3)       !! Initial position of the center of the top surface of the cloud (x, y, z)
      real(dp), intent(in)          :: init_vel(3)           !! Initial velocity of the cloud (u, v, w)

      real(dp), intent(in)          :: init_partl_concen(:)  !! initial particle concentration
      real(dp), intent(in)          :: init_partl_rho(:)     !! Initial particle dry density
      real(dp), intent(in)          :: init_tracer_concen    !! initial tracer concentration

      class(t_bounds), intent(in)   :: domain_bounds         !! Rectangular bounds of the domain

      integer, intent(in), optional:: ext_phase             !! Phase that the cloud is in. Descent, Water Collapse, or Bottom collapse

      ! Local variables
      real(dp) :: pos(3)

      self%id = id

      self%phase = optval(ext_phase, default=descentPhase)  ! If no value is provided assume that the cloud starts in the descent phase
      self%rho = rho ! Bulk density of the
      self%vert_radius = vert_radius
      self%horiz_radius = horiz_radius

      self%vel = init_vel
      self%mass_coeff = mass_coeff

      call self%set_vol()
      call self%set_mass()
      call self%set_momentum()
      call self%set_vorticity()


      call self%set_y_centroid()
      pos = top_surf_pos
      pos(2) = pos(2) + self%y_centroid
      call self%set_bounds(pos)
      call self%check_bounds(domain_bounds) ! Check that the cloud is inside of the domain

      ! Update positions
      self%top_surf_pos = top_surf_pos ! Set the top surf position now that we've checked that the
      self%pos = pos

      ! Init the particles in the cloud
      call self%partl%init(init_partl_concen, init_partl_rho, self%mass, self%vol)



   end subroutine init_dump_cloud

   subroutine print_cloud(self)
      class(t_dump_des_cloud), intent(in) :: self

      print *, "id", self%id
      print *, "centroid position", self%pos
   end subroutine print_cloud
end module mod_cloud
