module mod_particles
   use stdlib_kinds, only: dp
   use stdlib_string_type
   use stdlib_strings
   
   use mod_utils, only: alloc_message
   use mod_constants, only: min_allow_fluid_density

   implicit none(type, external)
   private

   type, public :: t_particles
      integer :: num_class               ! number of particles classes

      integer(dp)           :: num       ! Number of particles
      real(dp), allocatable :: concen(:) ! Concentration of each particle class
      real(dp), allocatable :: rho(:)    ! Dry density of the particles
      real(dp), allocatable :: vol(:)    ! Volume of each particle class
      real(dp), allocatable :: mass(:)   ! Mass of each particle class
   contains
      procedure, public, pass :: init => init_particles
      procedure, public, pass :: total_vol => calc_total_vol
      procedure, public, pass :: total_mass => calc_total_vol
      procedure, public, pass :: print => print_particle
   end type t_particles

contains
   subroutine init_particles(self, concentration, partl_rho, cloud_mass, cloud_vol)
      !! Init the particles. Only setting the concentration, setting
      class(t_particles), intent(inout) :: self
      real(dp), intent(in) :: concentration(:), partl_rho(:)
      real(dp), intent(in) :: cloud_mass, cloud_vol

      ! Local
      real(dp) :: cloud_fluid_vol, cloud_fluid_mass, fluid_density
      character(len=*), parameter :: FMT = '(A16, " : ", T19, F12.5)'

      self%concen = concentration
      self%rho    = partl_rho
      self%vol    = self%concen * cloud_vol
      self%mass   = self%vol    * self%rho
      self%num    = size(self%concen)

      ! TODO: Check that all the arrays have the same length

      cloud_fluid_vol  = cloud_vol  - self%total_vol()
      cloud_fluid_mass = cloud_mass  - self%total_mass()

      fluid_density = cloud_fluid_vol / cloud_fluid_mass
            
      if (fluid_density < min_allow_fluid_density .or. &
         cloud_fluid_vol < 0.0_dp .or.&
         cloud_fluid_mass < 0.0_dp) then

         print *, "Check particle and cloud properties"
         print FMT, "Cloud Mass: "   , cloud_mass
         print FMT, "Cloud Volume: " , cloud_vol 
         call self%print()

         error stop
      end if 

   end subroutine init_particles

   pure function calc_total_vol(self) result(total_vol)
      class(t_particles), intent(in) :: self
      real(dp) :: total_vol

      if (alloc_message(self%vol, "Part. Volume")) then
         total_vol = sum(self%vol)
      end if

   end function calc_total_vol

   pure function calc_total_mass(self) result(total_mass)
      class(t_particles), intent(in) :: self
      real(dp) :: total_mass

      if (alloc_message(self%mass, "Part. Mass")) then
        total_mass = sum(self%mass)
      end if
      
   end function calc_total_mass

   subroutine print_particle(self)
      class(t_particles), intent(in) :: self
      type(string_type) :: header, footer
      character(len=*), parameter :: FMT = '(A16, " : ", T19, F12.5)'

      header = "------ Particle values: ------"
      footer = repeat("-", len_trim(header))

      ! Add blank lines for better separation from other program output
      print *, ""
      print *, header

      ! 3. Print all parameters using the new, consistent format
      print *, "Concentration"
      print *, self%concen

      print *,  "Volume"
      print *, self%vol

      print *, "Mass"
      print *, self%mass
      
      print FMT, "Total mass:", self%total_mass()
      
      print FMT, "Total Volume:", self%total_vol()
      
      print *, ""
      print *, footer

   end subroutine print_particle

end module mod_particles
