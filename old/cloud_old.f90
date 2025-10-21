module mod_cloud
   use stdlib_kinds, only: dp
   use mod_dump_descent !! TODO add in only statement
   use mod_cloud_helper, only: calc_y_centroid_hemisphere, calc_clouds_bounds, check_cloud_in_domain 
   use mod_bounds, only: t_bounds

   implicit none(type, external)

   private

   type, public :: t_particles
      integer :: num_class               ! number of particles classes
      
      real(dp), allocatable :: concen(:) ! Concentration of each particle class
      real(dp), allocatable :: vol(:)    ! Volume of each particle class
      real(dp), allocatable :: mass(:)   ! Mass of each particle class

      real(dp) :: total_mass             ! Total mass of all particles 
      real(dp) :: total_vol              ! Total volume of all particles
   end type t_particles
   
   type, public :: t_dump_des_cloud

      integer         :: id                ! id of the cloud

      real(dp)        :: top_surf_pos(3)   ! Global posiiton of the top surface of the cloud
      real(dp)        :: pos(3)            ! Centroid position of the cloud (x, y, z)
      real(dp)        :: y_centroid        ! y-distance from the top_surf_pos to the y-centroid of the cloud
      real(dp)        :: vert_radius       ! Vertical radius
      real(dp)        :: horiz_radius      ! horizontal radius

      real(dp)        :: mass              ! Total mass of the cloud
      real(dp)        :: mass_coeff       ! Apparent mass coefficient
      real(dp)        :: rho               ! Density of the cloud
      real(dp)        :: vol               ! Volume of the cloud
      real(dp)        :: vorticity         ! Vorticity of the cloud
      real(dp)        :: vel(3)            ! Velocity of the cloud (u, v, w)
      real(dp)        :: momentum(3)       ! Momementum of the cloud

      real(dp), allocatable :: vol_class(:) ! Volume of each class
      type(t_particles) :: part             ! Particles properties of the cloud
      type(t_bounds) :: bounds            ! Rectangular box bound around the cloud in global coordinates
   
   contains
      procedure, public :: set_vol
      procedure, public :: set_mass
      procedure, public :: set_momentum
      procedure, public :: set_vorticity
      procedure, public :: set_y_centroid
      procedure, public :: set_bounds
      procedure, public :: check_bounds
      procedure, public :: init => init_dump_cloud
      procedure, public :: print => print_cloud
   end type t_dump_des_cloud

contains

   elemental pure subroutine set_vol(self, ext_vert_radius)
      !! Set the colume of the cloud given the vertical radius
      class(t_dump_des_cloud), intent(inout) :: self
      real(dp), intent(in), optional :: ext_vert_radius

      !Local variables
      real(dp) :: vert_radius

      if(present(ext_vert_radius)) then
         vert_radius = ext_vert_radius
      else 
         vert_radius = self%vert_radius
      end if

      self%vol = calc_hemisphere_vol(vert_radius)

   end subroutine set_vol

   elemental pure subroutine set_mass(self)
      !! Set the mass of the cloud
      class(t_dump_des_cloud), intent(inout) :: self

      self%mass = self%rho * self%vol
   end subroutine set_mass

   elemental pure subroutine set_momentum(self)
      !! Set the momentum of the cloud 
      
      class(t_dump_des_cloud), intent(inout) :: self

      self%momentum = calc_dd_momentum(self%mass_coeff, self%rho, self%vert_radius, self%vel)
   end subroutine set_momentum

   elemental pure subroutine set_vorticity(self)
      !! Set the vorticity
      class(t_dump_des_cloud), intent(inout) :: self

      ! self%vel(2) is the velocity in the y-direction
      self%vorticity = calc_dd_vorticity(self%vel(2), self%horiz_radius)
   end subroutine set_vorticity

   elemental pure subroutine set_y_centroid(self)
      !! Set the y_centroid of the cloud
      class(t_dump_des_cloud), intent(inout) :: self

      self%y_centroid = calc_y_centroid_hemisphere(self%vert_radius)
   end subroutine set_y_centroid

   pure subroutine set_bounds(self, pos)
      !! Sets the bounds for the cloud
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
      top_surf_pos, init_vel, domain_bounds)
      ! Initialize a cloud with given properties

      ! TODO: Need to go through this and update it so that the location is properly updated.
      class(t_dump_des_cloud), intent(inout) :: self            ! Resulting cloud object
      integer , intent(in)          :: id              ! id of the cloud
      real(dp), intent(in)          :: rho             ! Density of the cloud
      real(dp), intent(in)          :: mass_coeff      ! Apparent mass coeff
      real(dp), intent(in)          :: vert_radius     ! Vertical radius of the cloud
      real(dp), intent(in)          :: horiz_radius    ! Horizontal radius of the cloud
      real(dp), intent(in)          :: top_surf_pos(3) ! Initial position of the center of the top surface of the cloud (x, y, z)
      real(dp), intent(in)          :: init_vel(3)          ! Initial velocity of the cloud (u, v, w)
      class(t_bounds), intent(in)   :: domain_bounds

      ! Local variables
      real(dp) :: pos(3)

      self%id = id

      self%rho = rho
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

   end subroutine init_dump_cloud

   subroutine print_cloud(self)
      class(t_dump_des_cloud), intent(in) :: self

      print *, "id", self%id
      print *, "centroid position", self%pos
   end subroutine print_cloud
end module mod_cloud
